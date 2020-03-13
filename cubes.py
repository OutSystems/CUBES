#!/usr/bin/env python
import cProfile
import multiprocessing
import random
import re
from itertools import count
from multiprocessing import Pool
from typing import List

import sqlparse as sp
from rpy2 import robjects

import tyrell.spec as S
from squares import util
from squares.Specification import Specification
from squares.config import Config
from squares.dc import generate_cubes, CubeConstraint
from squares.interpreter import SquaresInterpreter, eq_r
from squares.util import create_argparser, parse_specification
from tyrell.decider import Example, ExampleConstraintPruningDecider
from tyrell.enumerator import LinesEnumerator
from tyrell.logger import get_logger
from tyrell.synthesizer import Synthesizer

robjects.r('''
zz <- file("r_output.log", open = "wt")
sink(zz)
sink(zz, type = "message")
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
options(warn=-1)''')

logger = get_logger('squares')


def process_start(loc_, config, speci: Specification):
    global tyrell_spec, specification, loc, decider, enumerator

    logger.setLevel('DEBUG')
    get_logger('tyrell').setLevel('DEBUG')

    util.store_config(config)
    speci.generate_r_init()  # must initialize R
    specification = speci
    tyrell_spec = S.parse(repr(specification.dsl))
    loc = loc_
    logger.handlers[0].set_identifier(multiprocessing.current_process().name)
    decider = ExampleConstraintPruningDecider(
        spec=tyrell_spec,
        interpreter=SquaresInterpreter(specification, False),
        examples=[
            Example(input=specification.tables, output='expected_output'),
        ],
        equal_output=eq_r
    )
    logger.debug('Creating enumerator instance...')
    enumerator = LinesEnumerator(tyrell_spec, loc + 1, loc, sym_breaker=False)


def profile_process_start(loc_, config, speci):
    cProfile.runctx('process_start(loc_, config, speci)', globals(), locals(),
                    'profile-%s.out' % multiprocessing.current_process().name)


def solve_cube(cube: List[CubeConstraint]):
    global tyrell_spec, specification, enumerator

    enumerator.z3_solver.push()

    for constraint in cube:
        enumerator.z3_solver.add(constraint.realize_constraint(tyrell_spec, enumerator))

    synthesizer = Synthesizer(
        enumerator=enumerator,
        decider=decider
    )

    logger.info('Synthesizing programs...')
    prog = synthesizer.synthesize()
    enumerator.z3_solver.pop()
    return prog


def beautifier(sql):
    # parsed = sp.parse(sql)
    # new_sql = beautifier_aux(parsed[0])
    sql = re.sub('`TBL_LEFT`\.`[^,`]*` AS |`LHS`\.`[^,`]*` AS ', "", sql)
    sql = re.sub('`TBL_RIGHT`\.`[^,`]*` AS |`RHS`\.`[^,`]*` AS ', "", sql)
    return sp.format(sql, reindent=True, keyword_case='upper')


if __name__ == '__main__':
    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    config = Config(seed=seed, ignore_aggrs=False, disabled=['semi_join'], force_summarise=True,
                    z3_QF_FD=True, z3_sat_phase='random')
    util.store_config(config)

    specification = Specification(spec)
    tyrell_spec = S.parse(repr(specification.dsl))

    for loc in count(start=1):
        with Pool(initializer=process_start, initargs=(loc, config, specification)) as pool:
            for program in pool.imap_unordered(solve_cube, generate_cubes(tyrell_spec, loc, loc - 1), chunksize=5):
                if program is not None:
                    pool.terminate()

                    logger.info(f'Solution found: {program}')
                    interpreter = SquaresInterpreter(specification, True)
                    evaluation = interpreter.eval(program, specification.tables)

                    try:
                        program = specification.r_init + interpreter.final_program
                        robjects.r(program)
                        sql_query = robjects.r(f'sink(); sql_render({evaluation})')
                    except Exception:
                        logger.error('Error while trying to convert R code to SQL.')
                        sql_query = None

                    print()
                    if args.r:
                        print(
                            "------------------------------------- R Solution ---------------------------------------\n")
                        print(specification.r_init + '\n' + interpreter.final_program)

                    if sql_query is not None:
                        print()
                        print(
                            "+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
                        print(beautifier(str(sql_query)[6:]))
                        exit()
                    else:
                        print('Failed to generate SQL query')
                        exit(2)

    print("No solution found")
    exit(1)
