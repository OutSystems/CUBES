import multiprocessing
import select
import signal
import traceback
from collections import defaultdict
from enum import Enum
from logging import getLogger
from multiprocessing import Process, Pipe, Queue
from threading import Thread

import logging

from . import util
from .config import Config
from .decider import InstrumentedDecider
from .dsl.dc import StatisticCubeGenerator, map_node
from .dsl.interpreter import SquaresInterpreter
from .dsl.specification import Specification
from .exceptions import REvaluationError
from .results import ResultsHolder
from .tyrell import spec as S
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
    rejected = 0
    failed = 0
    prog = enumerator.next()
    while prog is not None:
        attempts += 1
        if 'ID == ID.other & Company_group == Name.other' in str(prog):
            print(prog)
        try:
            res = decider.analyze(prog, enumerator.current_roots)
            if res.is_ok():
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

        enumerator.update(info)
        prog = enumerator.next()

    return None, attempts, rejected, failed


def run_process(pipe: Pipe, config: Config, specification: Specification, blacklist_queue: Queue, program_queue: Queue, logger_level):
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    signal.signal(signal.SIGTERM, signal.SIG_DFL)

    logger.setLevel(logger_level)

    util.store_config(config)
    util.set_program_queue(program_queue)
    specification.generate_r_init()  # must initialize R
    tyrell_specification = S.parse(repr(specification.dsl))

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
            enumerator = LinesEnumerator(tyrell_specification, data, sym_breaker=False)
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

        blacklist_queue = multiprocessing.Queue()
        poll = select.epoll()

        program_queue = multiprocessing.Queue()
        if logger.isEnabledFor(logging.DEBUG):
            def collect_programs():
                while True:
                    p = program_queue.get()
                    ResultsHolder().store_program(p)

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

        blacklist = defaultdict(set)
        queue = {}

        ResultsHolder().blacklist = blacklist

        solution_loc = None
        solution = None

        loc = self.specification.min_loc
        generator = StatisticCubeGenerator(self.specification, self.tyrell_specification, loc, loc, blacklist)
        alternate_generator = None
        alternated = False
        left_to_assign = None
        while True:
            if solution_loc and solution_loc <= min(locs.values()):
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

                    if util.get_config().advance_processes and att >= util.get_config().programs_per_cube_threshold:
                        if not alternated:
                            logger.info('Hard problem!')
                            alternated = True
                            left_to_assign = self.alternate_j

                    ResultsHolder().increment_attempts(att, rjc, fld)

                    if program:
                        if process_loc <= min(locs.values()) or not util.get_config().optimal:
                            return program

                        logger.info('Waiting for loc %d to finish before returning solution of loc %d', min(locs.values()), process_loc)
                        if solution_loc is None or process_loc < solution_loc:
                            solution = program
                            ResultsHolder().store_solution(solution)
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
                                    logger.debug('%d programs saved so far', ResultsHolder().program_counter)
                                    cube = next(alternate_generator)
                                    ResultsHolder().increment_cubes()
                                except (StopIteration, TypeError):
                                    logger.debug('Increasing alternate_generator loc to %d', loc + 1)
                                    alternate_generator = StatisticCubeGenerator(self.specification, self.tyrell_specification, loc + 1, loc + 1,
                                                                        blacklist)
                        else:
                            cube = None
                            while cube is None:
                                try:
                                    logger.debug('%d programs saved so far', ResultsHolder().program_counter)
                                    cube = next(generator)
                                    ResultsHolder().increment_cubes()
                                except StopIteration:
                                    if loc + 1 > util.get_config().maximum_loc:
                                        break
                                    loc += 1
                                    logger.debug('Increasing generator loc to %d', loc)
                                    if not alternated:
                                        generator = StatisticCubeGenerator(self.specification, self.tyrell_specification, loc, loc, blacklist)
                                    else:
                                        generator = alternate_generator
                                        alternate_generator = StatisticCubeGenerator(self.specification, self.tyrell_specification, loc + 1, loc + 1,
                                                                            blacklist)

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
