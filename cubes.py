#!/usr/bin/env python
import logging
import os
import random
import signal
from time import time

from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.parallel_synthesizer import ParallelSynthesizer
from squares.results import ResultsHolder
from squares.dsl.specification import Specification
from squares.tyrell import spec as S
from squares.tyrell.logger import get_logger
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
    print()
    ResultsHolder().print()
    exit(ResultsHolder().exit_code)


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

    config = Config(seed=seed, print_r=args.r, cache_ops=args.cache_ops, optimal=args.optimal, solution_use_first_line=args.use_first,
                    solution_use_last_line=args.use_last, advance_processes=args.split_search,
                    programs_per_cube_threshold=args.split_search_h, minimum_loc=args.min_lines, maximum_loc=args.max_lines,
                    max_filter_combinations=args.max_filter_combo, max_column_combinations=args.max_cols_combo,
                    max_join_combinations=args.max_join_combo,
                    z3_QF_FD=True, z3_sat_phase='random', disabled=args.disable)
    util.store_config(config)

    specification = Specification(spec)

    if logger.isEnabledFor(logging.DEBUG):
        with open('dsl.tyrell', 'w') as f:
            f.write(repr(specification.dsl))

    tyrell_spec = S.parse(repr(specification.dsl))

    ResultsHolder().specification = specification

    if args.j > 0:
        processes = args.j
    else:
        processes = os.cpu_count() + args.j

    signal.signal(signal.SIGINT, handle_sigint)
    signal.signal(signal.SIGTERM, handle_sigint)

    synthesizer = ParallelSynthesizer(tyrell_spec, specification, processes)
    program = synthesizer.synthesize()

    ResultsHolder().store_solution(program, True)
    ResultsHolder().print()
    exit(ResultsHolder().exit_code)


if __name__ == '__main__':
    main()
