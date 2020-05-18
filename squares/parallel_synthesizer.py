import multiprocessing
import select
import signal
import time
import traceback
from collections import defaultdict
from enum import Enum
from logging import getLogger
from multiprocessing import Process, Pipe, Queue
from threading import Thread

import cProfile

from . import util, results
from .config import Config
from .decider import InstrumentedDecider
from .dsl.dc import CubeGenerator, map_node, StatisticCubeGenerator
from .dsl.interpreter import SquaresInterpreter
from .dsl.specification import Specification
from .exceptions import REvaluationError
from .results import ResultsHolder
from .statistics import BigramStatistics
from .tyrell.decider import Example
from .tyrell.enumerator import LinesEnumerator
from .tyrell.interpreter import InterpreterError
from .tyrell.spec import TyrellSpec
from .util import pipe_write, pipe_read

logger = getLogger('squares.synthesizer')


class Message(Enum):
    INIT = 1
    SOLVE = 2


def synthesize(enumerator, decider, blacklist_queue):
    attempts = 0
    current_attempts = 0
    rejected = 0
    failed = 0
    start = time.time()
    prog = enumerator.next()
    ResultsHolder().enum_time += time.time() - start
    while prog is not None:
        ResultsHolder().n_attempts += 1
        attempts += 1
        current_attempts += 1
        if current_attempts == 1:
            util.get_program_queue().put((current_attempts, None))
            current_attempts = 0

        try:
            start = time.time()
            res = decider.analyze(prog, enumerator.current_roots)
            ResultsHolder().analysis_time += time.time() - start
            if res.is_ok():
                util.get_program_queue().put((current_attempts, None))
                return prog, attempts, rejected, failed

            else:
                rejected += 1
                info = res.why()

        except REvaluationError as e:
            if all(map(lambda arg: arg.children == [], e.args[0][0].args)):
                blacklist_queue.put(e.args[0][0])
            failed += 1
            info = decider.analyze_interpreter_error(e)

        except InterpreterError as e:
            failed += 1
            info = decider.analyze_interpreter_error(e)

        print(prog)
        enumerator.update(info)
        start = time.time()
        prog = enumerator.next()
        ResultsHolder().enum_time += time.time() - start

    util.get_program_queue().put((current_attempts, None))
    return None, attempts, rejected, failed


def run_process(pipe: Pipe, config: Config, specification: Specification, blacklist_queue: Queue, program_queue: Queue, logger_level):
    signal.signal(signal.SIGINT, results.handler_subprocess)
    signal.signal(signal.SIGTERM, results.handler_subprocess)

    logger.setLevel(logger_level)

    util.store_config(config)
    util.set_program_queue(program_queue)
    specification.generate_r_init()  # must initialize R
    tyrell_specification = specification.generate_dsl()

    decider = InstrumentedDecider(
        interpreter=SquaresInterpreter(specification, False),
        examples=[
            Example(input=specification.tables, output='expected_output'),
            ]
        )

    while True:
        action, data = pipe.recv()

        if action == Message.INIT:
            logger.debug('Initialising process for %d lines of code.', data)
            start = time.time()
            enumerator = LinesEnumerator(tyrell_specification, data, sym_breaker=False)
            ResultsHolder().init_time += time.time() - start
            # enumerator = cProfile.runctx('LinesEnumerator(tyrell_specification, 2, sym_breaker=False)', globals(), locals(), f'output_2.cProfile')
            pipe_write(pipe, (None, 0, 0, 0))

        elif action == Message.SOLVE:
            logger.debug('Solving cube %s', repr(data))

            try:
                enumerator.z3_solver.push()

                for constraint in data:
                    enumerator.z3_solver.add(constraint.realize_constraint(tyrell_specification, enumerator))

                ret, attempts, rejected, failed = synthesize(enumerator, decider, blacklist_queue)
                if ret:
                    logger.debug('Found solution with cube %s', repr(data))
                pipe_write(pipe, (ret, attempts, rejected, failed))

            except:
                logger.error('Exception while enumerating cube with hash %d', hash(data))
                print(traceback.format_exc())

            finally:
                enumerator.z3_solver.pop()

        else:
            logger.error('Unrecognized action %s', action)


class ParallelSynthesizer:

    def __init__(self, tyrell_specification: TyrellSpec, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j
        self.alternate_j = round(self.j * util.get_config().advance_percentage)

    def synthesize(self):
        processes = {}
        locs = {}
        pipes = {}
        alternates = []
        probers = []
        stopped = 0

        blacklist_queue = multiprocessing.Queue()
        poll = select.epoll()

        program_queue = multiprocessing.Queue()

        statistics = BigramStatistics(self.tyrell_specification)

        def collect_programs():
            while True:
                packet = program_queue.get()
                statistics.update(*packet)

        t = Thread(target=collect_programs, daemon=True)
        t.start()

        logger.info('Creating %d processes', self.j)
        for i in range(self.j):
            pipe, pipe_child = Pipe()
            process = Process(target=run_process, name=f'cube-solver-{i}',
                              args=(pipe_child, util.get_config(), self.specification, blacklist_queue, program_queue,
                                    logger.getEffectiveLevel()), daemon=True)
            process.start()

            processes[pipe.fileno()] = process
            locs[pipe.fileno()] = -1
            pipes[pipe.fileno()] = pipe
            poll.register(pipe)

            if len(probers) < util.get_config().probing_threads:
                probers.append(pipe.fileno())

        blacklist = defaultdict(set)
        queue = {}

        ResultsHolder().blacklist = blacklist

        solution_loc = None
        solution = None

        GeneratorClass = CubeGenerator if util.get_config().static_search else lambda a, b, c, d, e: StatisticCubeGenerator(statistics, a,
                                                                                                                            b, c, d, e)

        loc = self.specification.min_loc
        generator = GeneratorClass(self.specification, self.tyrell_specification, loc, loc, blacklist)
        alternate_generator = None
        alternated = False
        left_to_assign = None
        while True and stopped < self.j:
            if solution_loc and solution_loc <= min(locs.values()):
                ResultsHolder().store_solution(solution, solution_loc, optimal=True)
                return solution

            events = poll.poll()

            for fd, event in events:
                pipe = pipes[fd]
                process_loc = locs[fd]

                if event & select.EPOLLIN:
                    try:
                        program, att, rjc, fld = pipe_read(pipe)
                    except EOFError:  # this is needed because runsolver kills processes bottom-up
                        continue
                    poll.modify(fd, select.EPOLLIN | select.EPOLLOUT)

                    if util.get_config().advance_processes and att >= util.get_config().programs_per_cube_threshold and fd not in probers:
                        if not alternated:
                            logger.info('Hard problem!')
                            alternated = True
                            left_to_assign = self.alternate_j

                    ResultsHolder().increment_attempts(att, rjc, fld)

                    if program:
                        if process_loc <= min(locs.values()) or not util.get_config().optimal:
                            ResultsHolder().store_solution(program, process_loc, optimal=True)
                            return program

                        logger.info('Waiting for loc %d to finish before returning solution of loc %d', min(locs.values()), process_loc)
                        if solution_loc is None or process_loc < solution_loc:
                            solution = program
                            ResultsHolder().store_solution(solution, process_loc, optimal=False)
                            solution_loc = process_loc

                            for fd in processes.keys():  # TODO not tested
                                if locs[fd] >= solution_loc:
                                    processes[fd].terminate()
                                    poll.unregister(fd)

                if event & select.EPOLLOUT:
                    if solution is not None:
                        poll.unregister(pipe)
                        locs[fd] = solution_loc  # TODO hack
                        continue

                    fails = util.get_all(blacklist_queue)
                    for fail in fails:
                        blacklist[fail.name].add(tuple(map(map_node, fail.children)))

                    if fd in queue:  # a cube was previously generated for this process but not used
                        cube = queue[fd]
                        queue.pop(fd)

                        pipe.send((Message.SOLVE, cube))
                        poll.modify(fd, select.EPOLLIN)
                    else:

                        if alternated and left_to_assign > 0:
                            alternates.append(fd)
                            left_to_assign -= 1

                        if fd in alternates:
                            cube = None
                            while cube is None:
                                try:
                                    # logger.debug('Current scores:\n%s', repr(statistics))
                                    cube = alternate_generator.next(False)
                                    ResultsHolder().increment_cubes()
                                except (StopIteration, AttributeError):
                                    logger.debug('Increasing alternate_generator loc to %d', loc + 1)
                                    alternate_generator = GeneratorClass(self.specification, self.tyrell_specification, loc + 1,
                                                                         loc + 1,
                                                                         blacklist)
                        else:
                            cube = None
                            while cube is None:
                                try:
                                    if util.get_config().verbosity >= 3:
                                        if fd not in probers:
                                            pass
                                            # logger.debug('Current scores:\n%s', repr(statistics))
                                        else:
                                            logger.debug('Sending random probe!')
                                    cube = generator.next(fd in probers)
                                    ResultsHolder().increment_cubes()
                                except StopIteration:
                                    if loc + 1 > util.get_config().maximum_loc:
                                        stopped += 1
                                        poll.unregister(fd)
                                        logger.info('Reached maximum loc (%d). Giving up...', util.get_config().maximum_loc)
                                        break
                                    loc += 1
                                    logger.debug('Increasing generator loc to %d', loc)
                                    if not alternated:
                                        generator = GeneratorClass(self.specification, self.tyrell_specification, loc, loc,
                                                                   blacklist)
                                    else:
                                        generator = alternate_generator
                                        alternate_generator = GeneratorClass(self.specification, self.tyrell_specification, loc + 1,
                                                                             loc + 1,
                                                                             blacklist)

                        if cube is None:
                            break

                        this_loc = loc if fd not in alternates else loc + 1

                        if process_loc < this_loc:
                            locs[fd] = this_loc
                            pipe.send((Message.INIT, this_loc))
                            poll.modify(fd, select.EPOLLIN)
                            assert fd not in queue
                            queue[fd] = cube  # save this cube for later
                        else:
                            pipe.send((Message.SOLVE, cube))
                            poll.modify(fd, select.EPOLLIN)

        ResultsHolder().exceeded_max_loc = True
