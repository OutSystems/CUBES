#!/usr/bin/env python
import os
import random
import signal
from logging import getLogger
from time import time

import logging
from rpy2 import robjects

from squares import util, results
from squares.config import Config
from squares.dsl.specification import Specification
from squares.parallel_synthesizer import ParallelSynthesizer
from squares.util import create_argparser, parse_specification

robjects.r('''
sink("/dev/null")
options(warn=-1)
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))''')

logger = getLogger('squares')


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
                    max_join_combinations=args.max_join_combo, program_weigth_decay_rate=args.decay_rate,
                    block_commutative_ops=args.block_commutative_ops, subsume_conditions=args.subsume_conditions,
                    probing_threads=args.probing_threads, cube_freedom=args.cube_freedom,
                    z3_QF_FD=args.qffd, z3_sat_phase='random', disabled=args.disable)
    util.store_config(config)

    specification = Specification(spec)

    logger.debug("Generating DSL...")
    tyrell_spec = specification.generate_dsl()

    if util.get_config().verbosity >= 3:
        with open('dump.dsl', 'w') as f:
            f.write(str(specification))

    results.specification = specification

    if args.jobs > 0:
        processes = args.jobs
    else:
        processes = os.cpu_count() + args.jobs

    signal.signal(signal.SIGINT, results.handle_sigint)
    signal.signal(signal.SIGTERM, results.handle_sigint)

    synthesizer = ParallelSynthesizer(tyrell_spec, specification, processes)
    program = synthesizer.synthesize()  # program is stored in the results holder

    results.print_results()
    exit(results.exit_code)


if __name__ == '__main__':
    main()
