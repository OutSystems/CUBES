import logging
import multiprocessing
import select
import signal
import traceback
from collections import defaultdict, Counter
from enum import Enum
from multiprocessing import Process, Pipe, Queue

from . import util
from .config import Config
from .dc import CubeGenerator, map_node
from .decider import InstrumentedDecider
from .exceptions import REvaluationError
from .interpreter import SquaresInterpreter, eq_r
from .specification import Specification
from .tyrell import spec as S
from .tyrell.decider import Example
from .tyrell.enumerator import LinesEnumerator
from .tyrell.interpreter import InterpreterError
from .tyrell.logger import get_logger
from .tyrell.spec import TyrellSpec
from .util import pipe_write, pipe_read

logger = get_logger('squares.synthesizer')


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


def run_process(pipe: Pipe, config: Config, specification: Specification, blacklist_queue: Queue, program_queue: Queue):
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    signal.signal(signal.SIGTERM, signal.SIG_DFL)

    logger.handlers[0].set_identifier(multiprocessing.current_process().name)
    logger.setLevel('DEBUG')

    util.store_config(config)
    util.set_program_queue(program_queue)
    specification.generate_r_init()  # must initialize R
    tyrell_specification = S.parse(repr(specification.dsl))

    decider = InstrumentedDecider(
        interpreter=SquaresInterpreter(specification, False),
        examples=[
            Example(input=specification.tables, output='expected_output'),
        ],
        equal_output=eq_r
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

    def synthesize(self):
        cubes = 0
        programs = 0
        rejected = 0
        failed = 0

        program_dict = defaultdict(list)

        def log_statistics():
            logger.info('Statistics:\n'
                        '\tGenerated cubes: %d\n'
                        '\tAttempted programs: %d\n'
                        '\t\tRejected: %d\n'
                        '\t\tFailed: %d\n'
                        '\tBlacklist clauses: %d',
                        cubes, programs, rejected, failed, sum(map(len, blacklist.values())))

            for l in program_dict.keys():
                logger.info('Priting statistics for good programs of size %d', l)
                for i in range(l):
                    distribution = Counter(map(lambda prog: prog[i],program_dict[l]))
                    logger.info('\t%d: %s', i, str(distribution))

        processes = {}
        locs = {}
        pipes = {}

        blacklist_queue = multiprocessing.Queue()
        poll = select.epoll()

        program_queue = multiprocessing.Queue()

        logger.info('Creating %d processes', self.j)
        for i in range(self.j):
            pipe, pipe_child = Pipe()
            process = Process(target=run_process, name=f'cube-solver-{i}',
                              args=(pipe_child, util.get_config(), self.specification, blacklist_queue, program_queue), daemon=True)
            process.start()

            processes[pipe.fileno()] = process
            locs[pipe.fileno()] = -1
            pipes[pipe.fileno()] = pipe
            poll.register(pipe)

        blacklist = defaultdict(set)
        queue = {}

        solution_loc = None
        solution = None

        loc = self.specification.min_loc
        generator = CubeGenerator(self.specification, self.tyrell_specification, loc, loc - 1, blacklist)
        while True:
            if solution_loc and solution_loc <= min(locs.values()):
                log_statistics()
                return solution

            events = poll.poll()

            for fd, event in events:
                pipe = pipes[fd]
                process_loc = locs[fd]

                if event & select.EPOLLIN:
                    program, att, rjc, fld = pipe_read(pipe)
                    poll.modify(fd, select.EPOLLIN | select.EPOLLOUT)

                    programs += att
                    rejected += rjc
                    failed += fld

                    if logger.isEnabledFor(logging.DEBUG):
                        good_programs = util.get_all(program_queue)
                        for prog in good_programs:
                            program_dict[len(prog)].append(prog)

                    if program:
                        if process_loc <= min(locs.values()) or not util.get_config().optimal:
                            log_statistics()
                            return program

                        logger.info('Waiting for loc %d to finish before returning solution of loc %d', min(locs.values()), process_loc)
                        if solution_loc is None or process_loc < solution_loc:
                            solution = program
                            util.store_solution(solution)
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
                        cube = None
                        while cube is None:
                            try:
                                cube = next(generator)
                                cubes += 1
                            except StopIteration:
                                loc += 1
                                logger.debug('Increasing generator loc to %d', loc)
                                generator = CubeGenerator(self.specification, self.tyrell_specification, loc, loc - 1, blacklist)

                        if process_loc < loc:
                            locs[fd] = loc
                            pipe.send((Message.INIT, loc))
                            poll.modify(fd, select.EPOLLIN)
                            queue[fd] = cube  # save this cube for later
                        else:
                            pipe.send((Message.SOLVE, cube))
                            poll.modify(fd, select.EPOLLIN)
