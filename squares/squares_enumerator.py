# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squares_enumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import logging
import signal
import time
from multiprocessing import Queue

import rpy2.robjects as robjects

from squares.dsl.interpreter import SquaresInterpreter
from squares.dsl.specification import Specification
from . import util, results
from .config import Config
from .tyrell.decider import Example, ExampleDecider
from .tyrell.enumerator.bitenum import BitEnumerator
from .tyrell.synthesizer import Synthesizer

logger = logging.getLogger('squares')

robjects.r('''
zz <- file("r_output.log", open = "wt")
sink(zz)
sink(zz, type = "message")
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(dplyr)
library(dbplyr)
options(warn=-1)''')


def handle_sigint(signal, stackframe):
    print()
    results.print_results()
    results.handler_subprocess(signal, stackframe)
    exit(results.exit_code)


def main(args, spec, id: int, conf: Config, queue: Queue):
    signal.signal(signal.SIGINT, handle_sigint)
    signal.signal(signal.SIGTERM, handle_sigint)

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

    results.specification = specification

    # if logger.isEnabledFor(logging.DEBUG):
    #     with open(f'dsl{id}.tyrell', 'w') as f:
    #         f.write(repr(specification.dsl))

    spec = specification.generate_dsl()
    logger.info('Parsing succeeded')

    decider = ExampleDecider(interpreter=SquaresInterpreter(specification, False),
                             examples=[Example(input=specification.tables, output='expected_output')],
                             )

    logger.info('Building synthesizer...')
    loc = max(specification.min_loc, conf.minimum_loc)
    while loc <= util.get_config().maximum_loc:
        logger.info("Lines of Code: " + str(loc))
        # if args.tree:
        #     enumerator = SmtEnumerator(spec, depth=loc + 1, loc=loc)
        # else:
        #     if args.symm_off:
        #         enumerator = LinesEnumerator(spec, loc=loc)
        #     elif args.symm_on:
        #         enumerator = LinesEnumerator(spec, loc=loc, break_sym_online=True)
        #     else:
        #         enumerator = LinesEnumerator(spec, loc=loc, sym_breaker=False)
        start = time.time()
        enumerator = BitEnumerator(spec, specification, loc=loc)
        results.init_time += time.time() - start

        synthesizer = Synthesizer(enumerator=enumerator, decider=decider)

        logger.info('Synthesizing programs...')
        prog = synthesizer.synthesize()
        if prog:
            logger.info(f'Solution found: {prog}')
            results.store_solution(prog, loc, True)
            results.print_results()
            queue.put(results.exit_code)
            results.handler_subprocess(None, None)
            return

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1

    results.exceeded_max_loc = True
    logger.error('Process %d reached the maximum number of lines (%d). Giving up...', id, util.get_config().maximum_loc)
    results.handler_subprocess(None, None)
