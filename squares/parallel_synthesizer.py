import multiprocessing
from itertools import count
from multiprocessing.pool import Pool
from typing import List

from . import util
from .dc import CubeConstraint, generate_cubes
from .interpreter import SquaresInterpreter, eq_r
from .specification import Specification
from .tyrell import spec as S
from .tyrell.decider import Example, ExampleDecider
from .tyrell.enumerator import LinesEnumerator
from .tyrell.interpreter import InterpreterError
from .tyrell.logger import get_logger

logger = get_logger('squares.synthesizer')


def process_start(loc_, config, speci: Specification):
    global tyrell_spec, specification, loc, decider, enumerator, n

    logger.handlers[0].set_identifier(multiprocessing.current_process().name)
    logger.setLevel('DEBUG')

    logger.debug('Initialising process for %d lines of code.', loc_)

    util.store_config(config)
    speci.generate_r_init()  # must initialize R
    specification = speci
    tyrell_spec = S.parse(repr(specification.dsl))
    loc = loc_

    decider = ExampleDecider(
        interpreter=SquaresInterpreter(specification, False),
        examples=[
            Example(input=specification.tables, output='expected_output'),
        ],
        equal_output=eq_r
    )
    enumerator = LinesEnumerator(tyrell_spec, loc + 1, loc, sym_breaker=False)

    n = 0


def solve_cube(cube: List[CubeConstraint]):
    global tyrell_spec, specification, enumerator

    logger.debug('Solving cube %s', repr(cube))

    enumerator.z3_solver.push()

    for constraint in cube:
        enumerator.z3_solver.add(constraint.realize_constraint(tyrell_spec, enumerator))

    prog = synthesize(enumerator, decider)
    enumerator.z3_solver.pop()
    return prog


def synthesize(enumerator, decider):
    global n

    num_attempts = 0
    num_rejected = 0
    num_failed = 0
    prog = enumerator.next()
    while prog is not None:
        num_attempts += 1
        n += 1
        if num_attempts % 500 == 0:
            enumerator.closeLattices()
        try:
            res = decider.analyze(prog)
            if res.is_ok():
                enumerator.closeLattices()
                return prog, n

            else:
                num_rejected += 1
                info = res.why()
                enumerator.update(info)
                prog = enumerator.next()

        except InterpreterError as e:
            num_failed += 1
            info = decider.analyze_interpreter_error(e)
            enumerator.update(info)
            prog = enumerator.next()

    enumerator.closeLattices()
    return None, n


class ParallelSynthesizer:

    def __init__(self, tyrell_specification, specification: Specification, j: int):
        self.tyrell_specification = tyrell_specification
        self.specification = specification
        self.j = j

    def synthesize(self):
        tries = 0
        cubes = 0

        for loc in count(start=1):
            with Pool(self.j, initializer=process_start, initargs=(loc, util.get_config(), self.specification)) as pool:
                for program, n in pool.imap_unordered(solve_cube,
                                                      generate_cubes(self.specification, self.tyrell_specification, loc, loc - 1)):
                    tries += n
                    cubes += 1
                    if program:
                        pool.terminate()
                        logger.info('Program accepted after %d cubes (%d programs)', cubes, tries)
                        return program
