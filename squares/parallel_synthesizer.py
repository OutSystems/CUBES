import copy
import gc
import multiprocessing
import signal
import time
import traceback
from logging import getLogger
from math import ceil, floor
from multiprocessing import Process, Pipe, Queue
from threading import Thread

import select

import random

from . import util, results
from .config import Config
from .decider import LinesDecider
from .dsl.cube_generators import StaticCubeGenerator, StatisticCubeGenerator, BlockStatisticCubeGenerator, ForceStatisticCubeGenerator
from .dsl.interpreter import SquaresInterpreter
from .dsl.specification import Specification
from .process_manager import ProcessSet, ProcessSetManager, MaximumLinesOfCodeReached
from .statistics import BigramStatistics
from .tyrell.decider import Example
from .tyrell.enumerator.bitenum import BitEnumerator
from .tyrell.enumerator.input_enumerator import InputEnumerator
from .tyrell.spec import TyrellSpec
from .tyrell.synthesizer.synthesizer import Synthesizer, AbstractSynthesizer
from .util import pipe_write, pipe_read, Message

logger = getLogger('squares.synthesizer')


class ChildSynthesizer(Process, Synthesizer):

    def __init__(self, id: int, pipe: Pipe, config: Config, specification: Specification, program_queue: Queue, logger_level):
        super().__init__(name=f'cube-solver-{id}', daemon=True)
        self.pipe = pipe
        self.config = config
        self.specification = specification
        self.tyrell_specification = None
        self.program_queue = program_queue
        self.logger_level = logger_level

    def run(self) -> None:
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        signal.signal(signal.SIGTERM, signal.SIG_DFL)

        random.seed(self.config.seed)
        logger.setLevel(self.logger_level)
        util.store_config(copy.copy(self.config))
        util.set_program_queue(self.program_queue)

        self.specification.generate_r_init()  # must initialize R
        self.tyrell_specification = self.specification.generate_dsl()

        self._decider = LinesDecider(interpreter=SquaresInterpreter(self.specification),
                                     examples=[Example(input=self.specification.input_table_names, output='expected_output')])

        while True:
            msg = self.pipe.recv()
            if msg[0] == Message.INIT:
                self.init(*msg[1:])
            elif msg[0] == Message.SOLVE:
                self.solve(*msg[1:])
            elif msg[0] == Message.RESUME_SOLVE:
                self.resume_solve(*msg[1:])
            else:
                logger.error('Unrecognized action %s', msg[0])

    def init(self, loc, config_change=None):
        logger.debug('Initialising process for %d lines of code.', loc)
        if config_change:
            util.store_config(copy.copy(self.config))
            for key, value in config_change.items():
                util.get_config().__setattr__(key, value)
            self.tyrell_specification = self.specification.generate_dsl(discard=True)

        start = time.time()
        if loc > 0:
            self._enumerator = BitEnumerator(self.tyrell_specification, self.specification, loc)
        else:
            self._enumerator = InputEnumerator(self.tyrell_specification, self.specification)
        util.get_program_queue().put((Message.DEBUG_STATS, 0, 0, 0, 0, 0, 0, 0, time.time() - start, 0, 0, 0, 0))
        pipe_write(self.pipe, (None, None, 0))

    def solve(self, cube):
        logger.debug('Solving cube %s', repr(cube))

        core = None
        try:
            if isinstance(self._enumerator, BitEnumerator):
                self._enumerator.z3_solver.push()

                for i, constraint in enumerate(cube):
                    self._enumerator.assert_expr(constraint.realize_constraint(self.tyrell_specification, self._enumerator), f'cube_line_{i}',
                                                 track=True)

            ret, attempts = next(self.synthesize())

            if isinstance(self._enumerator, BitEnumerator):
                core = None
                if attempts == 0:
                    # logger.warning('Cube generated 0 programs')
                    unsat_core = [str(clause) for clause in self._enumerator.unsat_core]

                    core = []
                    for i, constraint in enumerate(cube):
                        if f'cube_line_{i}' in unsat_core:
                            core.append(constraint.production)
                        else:
                            core.append(None)

            if ret:
                logger.debug('Found solution with cube %s', repr(cube))
            pipe_write(self.pipe, (ret, core if isinstance(self._enumerator, BitEnumerator) else None, attempts))

        except:
            logger.error('Exception while enumerating cube with hash %s', repr(cube))
            print(traceback.format_exc())

        finally:
            self._enumerator.blocked_models = {}
            if isinstance(self._enumerator, BitEnumerator) and not ret:
                self._enumerator.z3_solver.pop()

    def resume_solve(self):
        logger.debug('Resuming solving previous cube')

        core = None
        try:
            ret, attempts = next(self.synthesize())

            if isinstance(self._enumerator, BitEnumerator):
                core = None
                if attempts == 0:
                    # logger.warning('Cube generated 0 programs')
                    unsat_core = [str(clause) for clause in self._enumerator.unsat_core]

            if ret:
                logger.debug('Found solution with cube')
            pipe_write(self.pipe, (ret, core, attempts))

        except:
            logger.error('Exception while enumerating cube')
            print(traceback.format_exc())

        finally:
            self._enumerator.blocked_models = {}
            if isinstance(self._enumerator, BitEnumerator) and not ret:
                self._enumerator.z3_solver.pop()


class ParallelSynthesizer(AbstractSynthesizer):

    def __init__(self, tyrell_specification: TyrellSpec, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j
        # dont alternate so many processes such that there are no longer any processes searching the previous loc
        self.alternate_j = min(round(self.j * util.get_config().advance_percentage), j - 1 - util.get_config().probing_threads)

    def synthesize(self, top_n: int = 1, enum_all=False):
        pipes = {}
        stopped = 0

        poll = select.epoll()

        program_queue = multiprocessing.Queue()
        statistics = BigramStatistics(self.tyrell_specification)

        def collect_programs():
            while True:
                packet = program_queue.get()
                if packet[0] == Message.DEBUG_STATS:
                    results.update_stats(*packet[1:])
                    statistics.update(packet[1], None)
                elif packet[0] == Message.EVAL_INFO:
                    statistics.update(*packet[1:])
                else:
                    raise NotImplementedError

        t = Thread(target=collect_programs, daemon=True)
        t.start()

        process_manager = ProcessSetManager(self.tyrell_specification, poll, self.alternate_j)

        if util.get_config().static_search:
            generator_f = (generator_b := StaticCubeGenerator(self.specification, self.tyrell_specification, self.specification.min_loc,
                                                              self.specification.min_loc - util.get_config().cube_freedom))

        else:
            if util.get_config().split_complex_joins:
                generator_f = ForceStatisticCubeGenerator(self.specification, self.tyrell_specification, self.specification.min_loc,
                                                          self.specification.min_loc - util.get_config().cube_freedom, statistics,
                                                          ['cross_join', 'inner_join'])
                generator_b = BlockStatisticCubeGenerator(self.specification, self.tyrell_specification, self.specification.min_loc,
                                                          self.specification.min_loc - util.get_config().cube_freedom, statistics,
                                                          ['cross_join', 'inner_join'])
            else:
                generator_f = (
                    generator_b := StatisticCubeGenerator(self.specification, self.tyrell_specification, self.specification.min_loc,
                                                          self.specification.min_loc - util.get_config().cube_freedom, statistics))

        probers_f = ProcessSet((True,), generator_f)
        probers_b = ProcessSet((True,), generator_b)
        main_set_f = ProcessSet((False,), generator_f)
        main_set_b = ProcessSet((False,), generator_b)

        logger.info('Creating %d processes', self.j)
        for i in range(self.j):
            pipe, pipe_child = Pipe()
            process = ChildSynthesizer(i, pipe_child, util.get_config(), self.specification, program_queue, logger.getEffectiveLevel())
            process.start()

            pipes[pipe.fileno()] = pipe
            poll.register(pipe)

            if len(probers_f) < ceil(util.get_config().probing_threads / 2):
                process_manager.register_process(probers_f, process, pipe)
            elif len(probers_b) < floor(util.get_config().probing_threads / 2):
                process_manager.register_process(probers_b, process, pipe)
            elif len(main_set_f) < (self.j - util.get_config().probing_threads) * util.get_config().split_complex_joins_ratio:
                process_manager.register_process(main_set_f, process, pipe)
            else:
                process_manager.register_process(main_set_b, process, pipe)

        solution_loc = None
        solution = None
        solution_n = 0
        found_solutions = set()

        while True and stopped < self.j:
            if (not enum_all) and (solution_n >= top_n):
                process_manager.kill_above(0)
                return

            if solution_loc and solution_loc <= process_manager.min_loc():
                results.store_solution(solution, solution_loc, optimal=True)
                yield solution, solution_loc, True
                solution_n += 1

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
                            if not enum_all:
                                stopped += process_manager.kill_above(loc+1)
                            process_manager.stop_incrementing = True
                            found_solutions.add(pipe)
                            yield program, loc, True
                            solution_n += 1
                            if util.get_config().enum_until is None:
                                logger.info('Found program of loc %d. %d programs to go.', loc, top_n - solution_n)
                        else:
                            logger.info('Waiting for loc %d to finish before returning solution of loc %d', process_manager.min_loc(), loc)
                            if solution_loc is None or loc < solution_loc:
                                solution = program
                                results.store_solution(solution, loc, optimal=False)
                                solution_loc = loc
                                if not enum_all:
                                    stopped += process_manager.kill_above(solution_loc)

                if event & select.EPOLLOUT:
                    if solution is not None and not enum_all:
                        poll.unregister(pipe)
                        process_manager.set_loc(pipe, solution_loc)
                        continue

                    try:
                        if pipe not in found_solutions:
                            process_manager.send(pipe)
                        else:
                            found_solutions.remove(pipe)
                            process_manager.send(pipe, True)
                    except MaximumLinesOfCodeReached:
                        stopped += 1

        if not solution:
            results.exceeded_max_loc = True
