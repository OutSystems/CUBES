#!/usr/bin/env python
import os
import random
import re
import signal
from time import time

import sqlparse as sp
from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.interpreter import SquaresInterpreter
from squares.parallel_synthesizer import ParallelSynthesizer
from squares.specification import Specification
from squares.tyrell.logger import get_logger
from squares.tyrell import spec as S
from squares.util import create_argparser, parse_specification

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


def handle_sigint(signal, stackframe):
    print_solution(util.get_current_solution())


def beautifier(sql):
    # parsed = sp.parse(sql)
    # new_sql = beautifier_aux(parsed[0])
    sql = re.sub('`TBL_LEFT`\.`[^,`]*` AS |`LHS`\.`[^,`]*` AS ', "", sql)
    sql = re.sub('`TBL_RIGHT`\.`[^,`]*` AS |`RHS`\.`[^,`]*` AS ', "", sql)
    return sp.format(sql, reindent=True, keyword_case='upper')


def print_solution(program):
    if program is not None:
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

        print('Time: ', time() - start)
        print()
        if args.r:
            pass
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


def main():
    global specification, args, start
    start = time()

    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        logger.setLevel('DEBUG')
        # get_logger('tyrell').setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    config = Config(seed=seed, z3_QF_FD=True, z3_sat_phase='random', disabled=['inner_join', 'semi_join'], optimal=True)
    util.store_config(config)

    specification = Specification(spec)
    tyrell_spec = S.parse(repr(specification.dsl))

    if args.j > 0:
        processes = args.j
    else:
        processes = os.cpu_count() + args.j

    signal.signal(signal.SIGINT, handle_sigint)
    signal.signal(signal.SIGTERM, handle_sigint)

    synthesizer = ParallelSynthesizer(tyrell_spec, specification, processes)
    program = synthesizer.synthesize()

    print_solution(program)


if __name__ == '__main__':
    main()
