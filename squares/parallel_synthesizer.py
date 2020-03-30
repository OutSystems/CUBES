import base64
import multiprocessing
import pickle
from collections import defaultdict
from multiprocessing import Process, Pipe, Queue
from select import select

import setproctitle

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
from .util import pipe_write, pipe_read

logger = get_logger('squares.synthesizer')


def synthesize(enumerator, decider, blacklist_queue):
    num_attempts = 0
    num_rejected = 0
    num_failed = 0
    prog = enumerator.next()
    while prog is not None:
        num_attempts += 1
        try:
            res = decider.analyze(prog)
            if res.is_ok():
                return prog

            else:
                num_rejected += 1
                info = res.why()
                enumerator.update(info)
                prog = enumerator.next()

        except REvaluationError as e:
            if all(map(lambda arg: arg.children == [], e.args[0][0].args)):
                blacklist_queue.put(e.args[0][0], timeout=1)
            num_failed += 1
            info = decider.analyze_interpreter_error(e)
            enumerator.update(info)
            prog = enumerator.next()

        except InterpreterError as e:
            num_failed += 1
            info = decider.analyze_interpreter_error(e)
            enumerator.update(info)
            prog = enumerator.next()

    return None


def run_process(pipe: Pipe, config: Config, specification: Specification, blacklist_queue: Queue):
    setproctitle.setproctitle(multiprocessing.current_process().name)
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

    while True:
        action, object = pipe.recv()

        if action == 'init':
            logger.debug('Initialising process for %d lines of code.', object)
            enumerator = LinesEnumerator(tyrell_specification, object, sym_breaker=False)

        elif action == 'solve':
            logger.debug('Solving cube %s', repr(object))

            try:
                enumerator.z3_solver.push()

                for constraint in object:
                    enumerator.z3_solver.add(constraint.realize_constraint(tyrell_specification, enumerator))

                ret = synthesize(enumerator, decider, blacklist_queue)
                if ret:
                    print(ret)
                pipe_write(pipe, ret)

            except:
                logger.error('Exception while enumerating cube %s', repr(object))

            finally:
                enumerator.z3_solver.pop()

        else:
            logger.error('Unrecognized action %s', action)


class ParallelSynthesizer:

    def __init__(self, tyrell_specification, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j

    def synthesize(self):
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
            if solution_loc and solution_loc <= min(locs.values()):
                return solution

            readable, writable, _ = select(pipes, pipes, [])

            for pipe in readable:
                process_loc = locs[pipe]
                program = pipe_read(pipe)

                fails = util.get_all(blacklist_queue)
                for fail in fails:
                    blacklist[fail.name].add(tuple(map(map_node, fail.children)))

                if program is None:
                    continue

                if process_loc <= min(locs.values()) or not util.get_config().optimal:
                    return program

                logger.info('Waiting for loc %d to finish before returning solution of loc %d', min(locs.values()), process_loc)
                if solution_loc is None or process_loc < solution_loc:
                    solution = program
                    solution_loc = process_loc

            for pipe in writable:
                if solution is not None:
                    pipes.remove(pipe)
                    locs[pipe] = solution_loc  # TODO hack
                    continue

                process_loc = locs[pipe]

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
