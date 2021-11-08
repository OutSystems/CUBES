#!/usr/bin/env python
import os
import random
import resource
import signal
from logging import getLogger
from time import time

import logging

import psutil
import rpy2
import yaml
from rpy2 import robjects

from squares import util, results
from squares.config import Config
from squares.dsl import interpreter
from squares.dsl.specification import Specification
from squares.exceptions import SquaresException
from squares.parallel_synthesizer import ParallelSynthesizer
from squares.util import create_argparser, parse_specification, write_specification


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print

robjects.r('''
sink("/dev/null")
options(warn=-1)
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(purrr))''')

logger = getLogger('squares')


def main():
    global specification, args, start
    start = time()

    parser = create_argparser()

    g = parser.add_argument_group('CUBES arguments')

    g.add_argument('-j', '--jobs', type=int, default=0, help='number of processes to use')

    g.add_argument('--optimal', action='store_true', help='make sure that returned solutions are as short as possible')
    g.add_argument('--static-search', action='store_true', help='search for solutions using a static cube generator')

    g.add_argument('--cube-freedom', type=int, default=0, help='number of free lines when generating cubes')
    g.add_argument('--no-deduce-cubes', dest='deduce_cubes', action='store_false', help='use unsat cores to deduce unsat cubes')

    g.add_argument('--no-split-complex-joins', dest='split_complex_joins', action='store_false',
                   help='use separate threads to test programs containing complex joins')
    g.add_argument('--split-ratio', type=float, default=1 / 3, help='percentage of threads that are forced to use complex joins')

    g.add_argument('--decay-rate', type=float, default=0.99999, help='rate at which old information is forgotten')
    g.add_argument('--probing-threads', type=int, default=2,
                   help='number of threads that should be used to randomly explore the search space')

    g.add_argument('--split-search', action='store_true',
                   help='use an heuristic to determine if search should be split among multiple lines of code')
    g.add_argument('--split-search-threshold', type=int, default=500, help='instance hardness threshold')

    args = parser.parse_args()

    if args.heap_limit:
        rsrc = resource.RLIMIT_DATA
        soft, hard = resource.getrlimit(rsrc)
        resource.setrlimit(rsrc, (args.heap_limit, hard))

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
                    advance_processes=args.split_search, static_search=args.static_search,
                    programs_per_cube_threshold=args.split_search_threshold, minimum_loc=args.min_lines,
                    maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo, max_column_combinations=args.max_cols_combo,
                    max_join_combinations=args.max_join_combo, program_weigth_decay_rate=args.decay_rate,
                    subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking, use_solution_dsl=args.use_dsl,
                    use_solution_cube=args.use_cube, probing_threads=args.probing_threads, cube_freedom=args.cube_freedom,
                    split_complex_joins=args.split_complex_joins, bitenum_enabled=args.bitenum, split_complex_joins_ratio=args.split_ratio,
                    deduce_cubes=args.deduce_cubes, z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable, top_programs=args.top,
                    use_beam_info=args.beam_info, beam_threshold=args.beam_threshold, enum_until=args.under, max_min_gen_cols=args.max_min_use_gen_cols,
                    beam_name=args.beam_name)
    util.store_config(config)

    specification = Specification(spec)

    logger.debug("Generating DSL...")
    tyrell_spec = specification.generate_dsl()

    if util.get_config().verbosity >= 3:
        with open('dump.dsl', 'w') as f:
            f.write(args.input)
            f.write('\n')
            f.write(str(specification))

    results.specification = specification

    if args.jobs > 0:
        processes = args.jobs
    else:
        processes = psutil.cpu_count(logical=False) + args.jobs

    signal.signal(signal.SIGINT, results.handle_sigint)
    signal.signal(signal.SIGTERM, results.handle_sigint)

    if util.get_config().enum_until is not None:
        signal.signal(signal.SIGALRM, results.handle_timeout)
        signal.alarm(util.get_config().enum_until)

    synthesizer = ParallelSynthesizer(tyrell_spec, specification, processes)
    first = True
    for program, loc, optimal in synthesizer.synthesize(top_n=util.get_config().top_programs, enum_all=util.get_config().enum_until is not None):
        if args.append:
            spec = parse_specification(args.input)
            if 'comment' not in spec:
                spec['comment'] = ''
            interp = interpreter.SquaresInterpreter(specification, True)
            evaluation = interp.eval(program, specification.input_tables)
            assert interp.equals(evaluation, 'expected_output')[0]  # this call makes it so that the select() appears in the output
            spec['comment'] += '\n\n' + interp.program
            if 'solution' not in spec:
                spec['solution'] = [line.production.name for line in program]
            with open(args.input, 'w') as f:
                write_specification(spec, f)

        if not first:
            specification.generate_r_init()
            # results.specification = Specification(spec)
        else:
            first = False

        results.store_solution(program, loc, optimal)
        results.print_results()
        results.solution = None

    if util.get_config().enum_until is not None:
        print()
        print('All solutions found')

    exit(results.exit_code)


if __name__ == '__main__':
    try:
        main()
    except SquaresException as e:
        logger.error(e.args[0])
        results.print_results()
        exit(e.exit_status)
