import multiprocessing
from collections import defaultdict
from multiprocessing import Process, Pipe, Queue
from select import select

from . import util
from .config import Config
from .dc import CubeGenerator, map_node
from .exceptions import REvaluationError
from .interpreter import SquaresInterpreter, eq_r
from .specification import Specification
from .tyrell import spec as S
from .tyrell.decider import Example, ExampleDecider
from .tyrell.enumerator import LinesEnumerator
from .tyrell.interpreter import InterpreterError
from .tyrell.logger import get_logger

logger = get_logger('squares.synthesizer')


def synthesize(enumerator, decider, blacklist_queue):
    num_attempts = 0
    num_rejected = 0
    num_failed = 0
    prog = enumerator.next()
    while prog is not None:
        num_attempts += 1
        if num_attempts % 500 == 0:
            enumerator.closeLattices()
        try:
            res = decider.analyze(prog)
            if res.is_ok():
                enumerator.closeLattices()
                return prog

            else:
                num_rejected += 1
                info = res.why()
                enumerator.update(info)
                prog = enumerator.next()

        except REvaluationError as e:
            if all(map(lambda arg: arg.children == [], e.args[0][0].args)):
                blacklist_queue.put(e.args[0][0])
            num_failed += 1
            info = decider.analyze_interpreter_error(e)
            enumerator.update(info)
            prog = enumerator.next()

        except InterpreterError as e:
            num_failed += 1
            info = decider.analyze_interpreter_error(e)
            enumerator.update(info)
            prog = enumerator.next()

    enumerator.closeLattices()
    return None


def run_process(pipe: Pipe, config: Config, specification: Specification, blacklist_queue: Queue):
    logger.handlers[0].set_identifier(multiprocessing.current_process().name)
    logger.setLevel('DEBUG')

    util.store_config(config)
    specification.generate_r_init()  # must initialize R
    tyrell_specification = S.parse(repr(specification.dsl))

    decider = ExampleDecider(
        interpreter=SquaresInterpreter(specification, False),
        examples=[
            Example(input=specification.tables, output='expected_output'),
        ],
        equal_output=eq_r
    )

    pipe.send(None)

    while True:
        action, object = pipe.recv()

        if action == 'init':
            logger.debug('Initialising process for %d lines of code.', object)
            enumerator = LinesEnumerator(tyrell_specification, object, sym_breaker=False)
            pipe.send(None)

        elif action == 'solve':
            logger.debug('Solving cube %s', repr(object))

            enumerator.z3_solver.push()

            for constraint in object:
                enumerator.z3_solver.add(constraint.realize_constraint(tyrell_specification, enumerator))

            try:
                prog = synthesize(enumerator, decider, blacklist_queue)
            except Exception:
                prog = None
            enumerator.z3_solver.pop()
            pipe.send(prog)

        else:
            raise NotImplementedError()


class ParallelSynthesizer:

    def __init__(self, tyrell_specification, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j

    def synthesize(self):
        tries = 0
        cubes = 0

        processes = {}
        locs = {}
        pipes = []

        blacklist_queue = multiprocessing.Queue()

        logger.info('Creating %d processes', self.j)
        for i in range(self.j):
            pipe, pipe_child = Pipe()
            process = Process(target=run_process, name=f'cube-solver-{i}', args=(pipe_child, util.get_config(), self.specification,
                                                                                 blacklist_queue), daemon=True)
            process.start()

            processes[pipe] = process
            locs[pipe] = -1
            pipes.append(pipe)

        blacklist = defaultdict(set)

        solution_loc = None
        solution = None

        loc = self.specification.min_loc
        generator = CubeGenerator(self.specification, self.tyrell_specification, loc, loc - 1, blacklist)
        while True:
            ready_pipes, _, _ = select(pipes, [], [])

            for pipe in ready_pipes:
                process_loc = locs[pipe]

                program = pipe.recv()
                if solution_loc and solution_loc <= min(locs.values()):
                    return solution

                if solution_loc:
                    locs[pipe] = solution_loc

                if program:
                    if process_loc <= min(locs.values()) or not util.get_config().optimal:
                        return program

                    logger.info('Waiting for loc %d to finish before returning solution of loc %d', min(locs.values()), process_loc)
                    if solution_loc is None or process_loc < solution_loc:
                        solution = program
                        solution_loc = process_loc

                if solution is None:
                    fails = util.get_all(blacklist_queue)
                    for fail in fails:
                        blacklist[fail.name].add(tuple(map(map_node, fail.children)))

                    cube = None
                    while cube is None:
                        try:
                            cube = next(generator)
                        except StopIteration:
                            loc += 1
                            generator = CubeGenerator(self.specification, self.tyrell_specification, loc, loc - 1, blacklist)

                    if process_loc < loc:
                        locs[pipe] = loc
                        pipe.send(('init', loc))

                    pipe.send(('solve', cube))
