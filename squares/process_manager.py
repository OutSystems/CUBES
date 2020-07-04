import select
import time
from logging import getLogger
from multiprocessing.connection import Connection
from multiprocessing.context import Process
from typing import Collection, List, Dict

from . import results, util
from .dsl.cube_generators import StaticCubeGenerator, CubeConstraint
from .tyrell.spec import TyrellSpec
from .util import Message

logger = getLogger('squares.synthesizer')


class MaximumLinesOfCodeReached(Exception):

    def __init__(self, *args: object) -> None:
        super().__init__(*args)


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

    def set_generator(self, generator: StaticCubeGenerator):
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
        pipe.send((Message.INIT, *self.cube_generator.init_info()))

    def min_loc(self) -> int:
        return min(self.locs.values())

    def get_loc(self, pipe: Connection) -> int:
        return self.locs[pipe]

    def set_loc(self, pipe: Connection, loc: int):
        self.locs[pipe] = loc

    def __len__(self):
        return len(self.processes)

    def __repr__(self) -> str:
        return f'ProcessSet(generator={self.cube_generator}, params={self.generator_params}, n={len(self)})'

    def kill_above(self, loc_limit) -> List[Connection]:
        result = []
        for pipe, process in self.processes.items():
            loc = self.locs[pipe]
            if loc >= loc_limit:
                if process.is_alive():
                    logger.debug('Killing process %s with loc %d', process.name, loc)
                process.kill()
                result.append(pipe)
        return result


class ProcessSetManager:

    def __init__(self, tyrell_specification: TyrellSpec, poll, alternate_j):
        self.tyrell_specification = tyrell_specification
        self.process_sets: Dict[Connection, ProcessSet] = {}
        self.waiting_list = {}  # cubes that have been generated but are waiting for a INIT to finish
        self.poll = poll
        self.process_set_stack: List[ProcessSet] = []
        self.alternated = False
        self.left_to_switch = 0
        self.alternate_j = alternate_j
        self.current_cube = {}
        self.current_cube_generator = {}

    def register_process(self, process_set: ProcessSet, process: Process, pipe: Connection):
        self.process_sets[pipe] = process_set
        process_set.register_process(process, pipe)

    def receive(self, pipe: Connection, program, core, att):
        if core and any(map(lambda x: x is None, core)):
            old = results.blocked_cubes
            start = time.time()
            self.current_cube_generator[pipe].block(core)
            results.block_time += time.time() - start
            if results.blocked_cubes - old > 0:
                logger.info('Blocked %d cubes in %s due to %s', results.blocked_cubes - old, self.current_cube_generator[pipe],
                            self.current_cube[pipe])

        if util.get_config().advance_processes and \
                att >= util.get_config().programs_per_cube_threshold * \
                (len(self.tyrell_specification.get_productions_with_lhs('Table')) ** (util.get_config().cube_freedom / 2 + .5)):
            if not self.alternated:
                logger.info('Hard problem!')
                self.alternated = True
                self.left_to_switch = self.alternate_j
                self.process_set_stack.append(
                    ProcessSet(self.process_set_stack[-1].generator_params,
                               self.generator_constructor(self.process_set_stack[-1].cube_generator.loc + 1)))

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
                    if loc + 1 > util.get_config().maximum_loc:
                        self.poll.unregister(pipe.fileno())
                        raise MaximumLinesOfCodeReached
                    if self.process_sets[pipe].cube_generator.next_generator is None:
                        logger.info('Generator for loc %d is exhausted!', loc)
                    self.process_sets[pipe].cube_generator = self.process_sets[pipe].cube_generator.get_next_generator()
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
        n = 0
        for process_set in set(self.process_sets.values()):
            for pipe in process_set.kill_above(loc):
                try:
                    self.poll.unregister(pipe.fileno())
                    n += 1
                except FileNotFoundError:
                    pass
        return n
