#!/usr/bin/env python
import os
import random
import signal
from logging import getLogger
from time import time

import logging
from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.parallel_synthesizer import ParallelSynthesizer
from squares.results import ResultsHolder
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

logger = getLogger('squares')


def handle_sigint(signal, stackframe):
    print()
    ResultsHolder().print()
    exit(ResultsHolder().exit_code)


def main():
    global specification, args, start
    start = time()

    parser = create_argparser()
    args = parser.parse_args()

    if args.verbose >= 1:
        logger.setLevel('INFO')
    if args.verbose >= 2:
        logger.setLevel('DEBUG')
    if args.verbose >= 4:
        getLogger('tyrell').setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations, optimal=args.optimal,
                    solution_use_lines=args.use_lines, solution_use_last_line=args.use_last, advance_processes=args.split_search,
                    static_search=args.static_search, programs_per_cube_threshold=args.split_search_threshold, minimum_loc=args.min_lines,
                    maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo, max_column_combinations=args.max_cols_combo,
                    max_join_combinations=args.max_join_combo, good_program_weight=args.good_program_weight,
                    strictly_good_program_weight=args.strictly_good_program_weight, program_weigth_decay_rate=args.decay_rate,
                    probing_threads=args.probing_threads,
                    z3_QF_FD=True, z3_sat_phase='random', disabled=args.disable)
    util.store_config(config)

    specification = Specification(spec)

    logger.debug("Generating DSL...")
    tyrell_spec = specification.generate_dsl()

    if logger.isEnabledFor(logging.DEBUG):
        with open('dsl.tyrell', 'w') as f:
            f.write(repr(tyrell_spec))

    ResultsHolder().specification = specification

    if args.jobs > 0:
        processes = args.jobs
    else:
        processes = os.cpu_count() + args.jobs

    signal.signal(signal.SIGINT, handle_sigint)
    signal.signal(signal.SIGTERM, handle_sigint)

    synthesizer = ParallelSynthesizer(tyrell_spec, specification, processes)
    program = synthesizer.synthesize()  # program is stored in the results holder

    ResultsHolder().print()
    exit(ResultsHolder().exit_code)


if __name__ == '__main__':
    main()
