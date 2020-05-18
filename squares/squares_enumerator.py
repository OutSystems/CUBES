# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squares_enumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import logging
from multiprocessing import Queue

import rpy2.robjects as robjects

from . import util
from .config import Config
from squares.dsl.interpreter import SquaresInterpreter
from .results import ResultsHolder
from squares.dsl.specification import Specification
from .tyrell import spec as S
from .tyrell.decider import Example, ExampleConstraintDecider
from .tyrell.enumerator import LinesEnumerator, SmtEnumerator
from .tyrell.synthesizer import Synthesizer

logger = logging.getLogger('squares')

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


def main(args, spec, id: int, conf: Config, queue: Queue):
    util.seed(conf.seed)
    util.store_config(conf)

    if args.verbose >= 1:
        logger.setLevel('INFO')
    if args.verbose >= 2:
        logger.setLevel('DEBUG')
    if args.verbose >= 3:
        logging.getLogger('tyrell').setLevel('DEBUG')

    logger.info('Creating specification instance...')
    specification = Specification(spec)

    ResultsHolder().specification = specification

    if logger.isEnabledFor(logging.DEBUG):
        with open(f'dsl{id}.tyrell', 'w') as f:
            f.write(repr(specification.dsl))

    spec = S.parse(repr(specification.dsl))
    logger.info('Parsing succeeded')

    decider = ExampleConstraintDecider(spec=spec,
                                       interpreter=SquaresInterpreter(specification, False),
                                       examples=[Example(input=specification.tables, output='expected_output')],
                                       )

    logger.info('Building synthesizer...')
    loc = max(specification.min_loc, conf.minimum_loc)
    while loc <= util.get_config().maximum_loc:
        logger.info("Lines of Code: " + str(loc))
        if args.tree:
            enumerator = SmtEnumerator(spec, depth=loc + 1, loc=loc)
        else:
            if args.symm_off:
                enumerator = LinesEnumerator(spec, loc=loc)
            elif args.symm_on:
                enumerator = LinesEnumerator(spec, loc=loc, break_sym_online=True)
            else:
                enumerator = LinesEnumerator(spec, loc=loc, sym_breaker=False)

        synthesizer = Synthesizer(enumerator=enumerator, decider=decider)

        logger.info('Synthesizing programs...')
        prog = synthesizer.synthesize()
        if prog:
            logger.info(f'Solution found: {prog}')
            ResultsHolder().store_solution(prog, loc, True)
            ResultsHolder().print()
            queue.put(ResultsHolder().exit_code)
            return

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1

    logger.error('Process %d reached the maximum number of lines (%d). Giving up...', id, util.get_config().maximum_loc)
