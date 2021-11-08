#!/usr/bin/env python

import os
import random
import signal
from logging import getLogger
from multiprocessing import Process, SimpleQueue

from squares import squares_enumerator, results, util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.exceptions import SquaresException
from squares.util import create_argparser, parse_specification

logger = getLogger('squares')


def main():
    parser = create_argparser()
    args = parser.parse_args()

    if args.verbose >= 1:
        logger.setLevel('INFO')
    if args.verbose >= 2:
        logger.setLevel('DEBUG')
    if args.verbose >= 3:
        getLogger('tyrell').setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    base_config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                         minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                         max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                         subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                         use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                         z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable, top_programs=args.top,
                         use_beam_info=args.beam_info, use_solution_loc=args.use_loc, beam_threshold=args.beam_threshold,
                         enum_until=args.under, max_min_gen_cols=args.max_min_use_gen_cols, beam_name=args.beam_name)
    util.store_config(base_config)

    specification = Specification(spec)
    results.specification = specification

    if util.get_config().verbosity >= 3:
        with open('dump.dsl', 'w') as f:
            f.write(args.input)
            f.write('\n')
            f.write(str(specification))

    configs = [base_config]

    if os.name == 'nt':
        logger.warning('Running on Windows is currently untested.')

    else:
        if len(configs) > len(os.sched_getaffinity(0)):
            logger.warning('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    processes = []
    for i in range(len(configs)):
        process = Process(target=squares_enumerator.main, name=f'process{i}',
                          args=(args, specification, i, configs[i], queue),
                          daemon=True)
        process.start()
        processes.append(process)

    signal.signal(signal.SIGINT, results.handle_sigint)
    signal.signal(signal.SIGTERM, results.handle_sigint)

    if util.get_config().enum_until is not None:
        signal.signal(signal.SIGALRM, results.handle_timeout)
        signal.alarm(util.get_config().enum_until)

    first = True
    while True:
        packet = queue.get()
        if packet[0] == util.Message.DONE:
            print()
            if util.get_config().enum_until is not None:
                print('All solutions of length', packet[1], 'found')
            break
        elif packet[0] == util.Message.DEBUG_STATS:
            results.update_stats(*packet[1:])
        elif packet[0] == util.Message.SOLUTION:
            if not first:
                results.specification = Specification(spec)
            else:
                first = False
            logger.debug('Solution found using process %d', packet[1])
            results.store_solution(*packet[2:])
            results.print_results()
        elif packet[0] == util.Message.EVAL_INFO:
            pass
        elif packet[0] == util.Message.NO_SOLUTION:
            results.exceeded_max_loc = True
            results.print_results()
            break
        else:
            logger.error('Unexpected message %s', packet[0])
            raise NotImplementedError

    for p in processes:
        p.kill()

    exit(results.exit_code)


if __name__ == '__main__':
    try:
        main()
    except SquaresException as e:
        logger.error(e.args[0])
        results.print_results()
        exit(e.exit_status)
