# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squaresEnumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import logging
import random
import re
from multiprocessing import Queue
from multiprocessing import SimpleQueue

import rpy2.robjects as robjects
import sqlparse as sp

import tyrell.spec as S
from squares import util
from squares.Specification import Specification
from squares.config import Config
from squares.interpreter import SquaresInterpreter, eq_r
from squares.util import create_argparser
from tyrell.decider import Example, ExampleConstraintDecider
from tyrell.enumerator import LinesEnumerator, SmtEnumerator
from tyrell.logger import get_logger
from tyrell.synthesizer import Synthesizer

# warnings.filterwarnings("ignore", category=RRuntimeWarning)

logger = get_logger('squares')

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


def beautifier(sql):
    # parsed = sp.parse(sql)
    # new_sql = beautifier_aux(parsed[0])
    sql = re.sub('`TBL_LEFT`\.`[^,`]*` AS |`LHS`\.`[^,`]*` AS ', "", sql)
    sql = re.sub('`TBL_RIGHT`\.`[^,`]*` AS |`RHS`\.`[^,`]*` AS ', "", sql)
    return sp.format(sql, reindent=True, keyword_case='upper')


def main(args, spec, id: int, conf: Config, queue: Queue, limit: int):
    util.seed(conf.seed)
    util.store_config(conf)

    if args.debug:
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    logger.handlers[0].set_identifier(f'prc{id}')

    logger.info('Creating specification instance...')
    specification = Specification(spec)

    if logger.isEnabledFor(logging.DEBUG):
        with open(f'dsl{id}.tyrell', 'w') as f:
            f.write(repr(specification.dsl))

    spec = S.parse(repr(specification.dsl))
    logger.info('Parsing succeeded')

    logger.info('Building synthesizer...')
    loc = conf.starting_loc
    while loc <= limit:
        logger.info("Lines of Code: " + str(loc))
        if args.tree:
            enumerator = SmtEnumerator(spec, depth=loc + 1, loc=loc)
        else:
            if args.symm_off:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc)
            elif args.symm_on:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc, break_sym_online=True)
            else:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc, sym_breaker=False)

        # enumerator = ExhaustiveEnumerator(spec, loc)

        synthesizer = Synthesizer(
            # loc: # of function productions
            enumerator=enumerator,
            decider=ExampleConstraintDecider(
                # decider=ExampleConstraintPruningDecider(
                spec=spec,
                interpreter=SquaresInterpreter(specification, False),
                examples=[
                    Example(input=specification.tables, output='expected_output'),
                ],
                equal_output=eq_r
            )
        )
        logger.info('Synthesizing programs...')

        prog = synthesizer.synthesize()
        if prog is not None:
            logger.info(f'Solution found: {prog}')
            interpreter = SquaresInterpreter(specification, True)
            evaluation = interpreter.eval(prog, specification.tables)

            try:
                program = specification.r_init + interpreter.final_program
                robjects.r(program)
                sql_query = robjects.r(f'sink(); sql_render({evaluation})')
            except Exception:
                logger.error('Error while trying to convert R code to SQL.')
                sql_query = None

            queue.put((specification.r_init + '\n' + interpreter.final_program,
                       None if sql_query is None else beautifier(str(sql_query)[6:]), id))
            return

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1

    logger.error('Process %d reached the maximum number of lines (%d). Giving up...', id, limit)


if __name__ == '__main__':
    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    main(args, 1,
         Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'bind_rows', 'semi_join'], force_summarise=True,
                z3_QF_FD=True, z3_sat_phase='random'), SimpleQueue(), 7)
