import gc
import multiprocessing
import select
import signal
import time
import traceback
from enum import Enum
from logging import getLogger
from multiprocessing import Process, Pipe, Queue
from multiprocessing.connection import Connection
from threading import Thread
from typing import Dict, Collection, List

from . import util, results
from .config import Config
from .decider import LinesDecider
from .dsl.dc import CubeGenerator, StatisticCubeGenerator, CubeConstraint
from .dsl.interpreter import SquaresInterpreter
from .dsl.specification import Specification
from .statistics import BigramStatistics
from .tyrell.decider import Example
from .tyrell.enumerator.bitenum import BitEnumerator
from .tyrell.interpreter import InterpreterError
from .tyrell.spec import TyrellSpec
from .util import pipe_write, pipe_read

logger = getLogger('squares.synthesizer')


class Message(Enum):
    INIT = 1
    SOLVE = 2


def synthesize(enumerator, decider, current_cube):
    attempts = 0
    current_attempts = 0
    rejected = 0
    failed = 0
    start = time.time()
    prog = enumerator.next()
    results.enum_time += time.time() - start
    while prog is not None:
        results.n_attempts += 1
        attempts += 1
        current_attempts += 1
        if current_attempts == 100:
            util.get_program_queue().put((current_attempts, None))
            current_attempts = 0

        try:
            start = time.time()
            res = decider.analyze(prog, current_cube)
            results.analysis_time += time.time() - start
            if res.is_ok():
                util.get_program_queue().put((current_attempts, None))
                return prog, attempts, rejected, failed

            else:
                rejected += 1
                info = res.why()

        except InterpreterError as e:
            logger.error('Failed program %s', str(prog))
            logger.error('%s', str(e))
            failed += 1
            info = decider.analyze_interpreter_error(e)

        enumerator.update(info)
        start = time.time()
        prog = enumerator.next()
        results.enum_time += time.time() - start

    util.get_program_queue().put((current_attempts, None))
    return None, attempts, rejected, failed


def run_process(pipe: Pipe, config: Config, specification: Specification, program_queue: Queue, logger_level):
    signal.signal(signal.SIGINT, results.handler_subprocess)
    signal.signal(signal.SIGTERM, results.handler_subprocess)

    logger.setLevel(logger_level)

    util.store_config(config)
    util.set_program_queue(program_queue)
    specification.generate_r_init()  # must initialize R
    tyrell_specification = specification.generate_dsl()

    decider = LinesDecider(interpreter=SquaresInterpreter(specification),
                           examples=[Example(input=specification.tables, output='expected_output')])

    while True:
        action, data = pipe.recv()

        if action == Message.INIT:
            logger.debug('Initialising process for %d lines of code.', data)
            start = time.time()
            enumerator = BitEnumerator(tyrell_specification, specification, data)
            results.init_time += time.time() - start
            # enumerator = cProfile.runctx('enumerator = BitEnumerator(tyrell_specification, specification, 3, sym_breaker=False)', globals(), locals(), f'output_3.cProfile')
            pipe_write(pipe, (None, 0, 0, 0, None))
            gc.collect()

        elif action == Message.SOLVE:
            logger.debug('Solving cube %s', repr(data))

            try:
                enumerator.z3_solver.push()

                for i, constraint in enumerate(data):
                    enumerator.assert_expr(constraint.realize_constraint(tyrell_specification, enumerator), f'cube_line_{i}')

                ret, attempts, rejected, failed = synthesize(enumerator, decider, tuple(constraint.production for constraint in data))

                core = None
                if attempts == 0:
                    logger.warning('Cube generated 0 programs')
                    if util.get_program_queue():
                        util.get_program_queue().put((tuple(line.production for line in data), -5))

                    unsat_core = [str(clause) for clause in enumerator.unsat_core]

                    core = []
                    for i, constraint in enumerate(data):
                        if f'cube_line_{i}' in unsat_core:
                            core.append(constraint.production)
                        else:
                            core.append(None)

                if ret:
                    logger.debug('Found solution with cube %s', repr(data))
                pipe_write(pipe, (ret, attempts, rejected, failed, core))
                gc.collect()

            except:
                logger.error('Exception while enumerating cube with hash %d', hash(data))
                print(traceback.format_exc())

            finally:
                enumerator.z3_solver.pop()

        else:
            logger.error('Unrecognized action %s', action)


class ProcessSet:

    def __init__(self, generator_params, cube_generator=None) -> None:
        self.processes = {}
        self.locs = {}
        self.cube_generator = cube_generator
        self.generator_params = generator_params

    def register_process(self, process: Process, pipe: Connection):
        self.processes[pipe] = process
        self.locs[pipe] = -1

    def unregister_process(self, pipe: Connection) -> Process:
        process = self.processes[pipe]
        self.processes.pop(pipe)
        self.locs.pop(pipe)
        return process

    def set_generator(self, generator: CubeGenerator):
        self.cube_generator = generator

    def generate_cube(self, pipe: Connection) -> Collection[CubeConstraint]:
        try:
            return self.cube_generator.next(*self.generator_params)
        except StopIteration:
            raise StopIteration

    def should_reinit(self, pipe: Connection) -> bool:
        return self.locs[pipe] < self.cube_generator.loc

    def init(self, pipe: Connection):
        self.locs[pipe] = self.cube_generator.loc
        pipe.send((Message.INIT, self.cube_generator.loc))

    def min_loc(self) -> int:
        return min(self.locs.values())

    def get_loc(self, pipe: Connection) -> int:
        return self.locs[pipe]

    def set_loc(self, pipe: Connection, loc: int):
        self.locs[pipe] = loc

    def __len__(self):
        return len(self.processes)

    def __repr__(self) -> str:
        return f'ProcessSet(loc={self.cube_generator.loc}, params={self.generator_params}, n={len(self)})'

    def kill_above(self, loc_limit) -> List[Connection]:
        result = []
        for pipe, process in self.processes.items():
            loc = self.locs[pipe]
            if loc >= loc_limit:
                try:
                    process.terminate()
                    process.terminate()

                    def _kill(process):
                        time.sleep(0.25)
                        if process.is_alive():
                            logger.debug('Process %s did not terminate. Killing', process.name)
                            process.kill()

                    t = Thread(target=_kill, args=(process,))
                    t.start()
                except ProcessLookupError:
                    pass
                result.append(pipe)
        return result


class ProcessSetManager:

    def __init__(self, poll, generator_constructor, alternate_j):
        self.probing_process_set = None
        self.process_sets: Dict[Connection, ProcessSet] = {}
        self.waiting_list = {}  # cubes that have been generated but are waiting for a INIT to finish
        self.poll = poll
        self.process_set_stack: List[ProcessSet] = []
        self.generator_constructor = generator_constructor
        self.alternated = False
        self.left_to_switch = 0
        self.alternate_j = alternate_j
        self.current_cube = {}
        self.current_cube_generator = {}

    def register_process(self, process_set: ProcessSet, process: Process, pipe: Connection):
        self.process_sets[pipe] = process_set
        process_set.register_process(process, pipe)

    def receive(self, pipe: Connection, program, att, rjc, fld, core):
        if core and any(map(lambda x: x is None, core)):
            old = results.blocked_cubes
            self.current_cube_generator[pipe].block(core)
            if results.blocked_cubes - old > 0:
                logger.info('Blocked %d cubes in %s due to %s', results.blocked_cubes - old, self.current_cube_generator[pipe], self.current_cube[pipe])

        if util.get_config().advance_processes and att >= util.get_config().programs_per_cube_threshold:
            if not self.alternated:
                logger.info('Hard problem!')
                self.alternated = True
                self.left_to_switch = self.alternate_j
                self.process_set_stack.append(
                    ProcessSet(self.process_set_stack[-1].generator_params,
                               self.generator_constructor(self.process_set_stack[-1].cube_generator.loc + 1)))

        results.increment_attempts(att, rjc, fld)

        process_loc = self.get_loc(pipe)
        return program, process_loc

    def send(self, pipe: Connection):
        if pipe in self.waiting_list:
            cube = self.waiting_list[pipe]
            self.waiting_list.pop(pipe)

            self.current_cube[pipe] = cube
            pipe.send((Message.SOLVE, cube))
            self.poll.modify(pipe.fileno(), select.EPOLLIN)
        else:
            if self.alternated and self.left_to_switch > 0 and \
                    self.process_sets[pipe] in self.process_set_stack and self.process_sets[pipe] != self.process_set_stack[-1]:
                process = self.process_sets[pipe].unregister_process(pipe)
                self.register_process(self.process_set_stack[-1], process, pipe)
                self.left_to_switch -= 1
                logger.info('Generator configuration has changed: %s', str(set(self.process_sets.values())))

            cube = None
            while cube is None:
                try:
                    cube = self.process_sets[pipe].generate_cube(pipe)
                    results.increment_cubes()
                except StopIteration:
                    loc = self.process_sets[pipe].cube_generator.loc
                    old_generator = self.process_sets[pipe].cube_generator
                    logger.warning('Generator for loc %d is exhausted!', loc)
                    if self.process_sets[pipe] in self.process_set_stack:
                        idx = self.process_set_stack.index(self.process_sets[pipe])
                    else:
                        idx = 0
                    for i in range(idx, len(self.process_set_stack) - 1):
                        self.process_set_stack[i].set_generator(self.process_set_stack[i + 1].cube_generator)
                    if self.process_sets[pipe] not in self.process_set_stack:
                        self.process_sets[pipe].cube_generator = self.process_set_stack[0].cube_generator
                    self.process_set_stack[-1].set_generator(self.generator_constructor(loc + 1))
                    for process_set in set(self.process_sets.values()):
                        if process_set.cube_generator == old_generator:
                            process_set.cube_generator = self.process_set_stack[0].cube_generator
                    logger.info('New generator configuration: %s', str(set(self.process_sets.values())))

            self.current_cube_generator[pipe] = self.process_sets[pipe].cube_generator
            if self.should_reinit(pipe):
                self.init(pipe)
                self.poll.modify(pipe.fileno(), select.EPOLLIN)
                assert pipe not in self.waiting_list
                self.waiting_list[pipe] = cube
            else:
                self.current_cube[pipe] = cube
                pipe.send((Message.SOLVE, cube))
                self.poll.modify(pipe.fileno(), select.EPOLLIN)

    def min_loc(self) -> int:
        return min(map(lambda x: x.min_loc(), self.process_sets.values()))

    def get_loc(self, pipe: Connection) -> int:
        return self.process_sets[pipe].get_loc(pipe)

    def should_reinit(self, pipe: Connection) -> bool:
        return self.process_sets[pipe].should_reinit(pipe)

    def init(self, pipe: Connection):
        self.process_sets[pipe].init(pipe)

    def set_loc(self, pipe: Connection, loc: int):
        self.process_sets[pipe].set_loc(pipe, loc)

    def kill_above(self, loc):
        for process_set in self.process_sets.values():
            for pipe in process_set.kill_above(loc):
                try:
                    self.poll.unregister(pipe.fileno())
                except FileNotFoundError:
                    pass


class ParallelSynthesizer:

    def __init__(self, tyrell_specification: TyrellSpec, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j
        # dont alternate so many processes such that there are no longer any processes searching the previous loc
        self.alternate_j = min(round(self.j * util.get_config().advance_percentage), j - 1 - util.get_config().probing_threads)

    def synthesize(self):
        pipes = {}
        stopped = 0

        poll = select.epoll()

        program_queue = multiprocessing.Queue()
        statistics = BigramStatistics(self.tyrell_specification)

        def collect_programs():
            while True:
                packet = program_queue.get()
                statistics.update(*packet)

        t = Thread(target=collect_programs, daemon=True)
        t.start()

        process_manager = ProcessSetManager(poll,
                                            (lambda loc: CubeGenerator(self.specification, self.tyrell_specification, loc,
                                                                       loc - util.get_config().cube_freedom))
                                            if util.get_config().static_search else
                                            (lambda loc: StatisticCubeGenerator(statistics, self.specification, self.tyrell_specification,
                                                                                loc, loc - util.get_config().cube_freedom)),
                                            self.alternate_j)

        generator = process_manager.generator_constructor(self.specification.min_loc)

        probers = ProcessSet((True,), generator)
        main_set = ProcessSet((False,), generator)
        process_manager.process_set_stack.append(main_set)
        process_manager.probing_process_set = probers

        logger.info('Creating %d processes', self.j)
        for i in range(self.j):
            pipe, pipe_child = Pipe()
            process = Process(target=run_process,
                              name=f'cube-solver-{i}',
                              args=(pipe_child, util.get_config(), self.specification, program_queue, logger.getEffectiveLevel()),
                              daemon=True)
            process.start()

            pipes[pipe.fileno()] = pipe
            poll.register(pipe)

            if len(probers) < util.get_config().probing_threads:
                process_manager.register_process(probers, process, pipe)
            else:
                process_manager.register_process(main_set, process, pipe)

        solution_loc = None
        solution = None

        while True and stopped < self.j:
            if solution_loc and solution_loc <= process_manager.min_loc():
                results.store_solution(solution, solution_loc, optimal=True)
                process_manager.kill_above(0)
                return solution

            events = poll.poll()

            for fd, event in events:
                pipe = pipes[fd]

                if event & select.EPOLLIN:
                    try:
                        message = pipe_read(pipe)
                    except EOFError:  # this is needed because runsolver kills processes bottom-up
                        continue
                    poll.modify(fd, select.EPOLLIN | select.EPOLLOUT)

                    program, loc = process_manager.receive(pipe, *message)

                    if program:
                        if loc <= process_manager.min_loc() or not util.get_config().optimal:
                            results.store_solution(program, loc, optimal=True)
                            process_manager.kill_above(0)
                            return program

                        logger.info('Waiting for loc %d to finish before returning solution of loc %d', process_manager.min_loc(), loc)
                        if solution_loc is None or loc < solution_loc:
                            solution = program
                            results.store_solution(solution, loc, optimal=False)
                            solution_loc = loc

                            process_manager.kill_above(solution_loc)

                if event & select.EPOLLOUT:
                    if solution is not None:
                        poll.unregister(pipe)
                        process_manager.set_loc(pipe, solution_loc)
                        continue

                    process_manager.send(pipe)

        results.exceeded_max_loc = True
