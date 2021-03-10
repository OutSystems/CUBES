#!/usr/bin/env python
import copy
import os
import random
from itertools import product
from logging import getLogger
from multiprocessing import Process, SimpleQueue
from time import sleep

import signal

from squares import squares_enumerator, results, util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import create_argparser, parse_specification

logger = getLogger('squares')

default_config_1 = [{'z3_sat_phase': 'caching', 'bitenum_enabled': True},
                    {'z3_sat_phase': 'caching', 'bitenum_enabled': False},
                    {'z3_sat_phase': 'random', 'bitenum_enabled': True},
                    {'z3_sat_phase': 'random', 'bitenum_enabled': False}]


default_config_2 = [{'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': True},
                    {'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': False},
                    {'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': True},
                    {'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': False},
                    {'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': True},
                    {'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': False},
                    {'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': True},
                    {'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': False}]

default_config_3 = [{'subsume_conditions': False, 'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': True},
                    {'subsume_conditions': False, 'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': False},
                    {'subsume_conditions': False, 'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': True},
                    {'subsume_conditions': False, 'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': False},
                    {'subsume_conditions': False, 'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': True},
                    {'subsume_conditions': False, 'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': False},
                    {'subsume_conditions': False, 'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': True},
                    {'subsume_conditions': False, 'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': False},
                    {'subsume_conditions': True, 'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': True},
                    {'subsume_conditions': True, 'z3_QF_FD': True, 'z3_sat_phase': 'caching', 'bitenum_enabled': False},
                    {'subsume_conditions': True, 'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': True},
                    {'subsume_conditions': True, 'z3_QF_FD': True, 'z3_sat_phase': 'random', 'bitenum_enabled': False},
                    {'subsume_conditions': True, 'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': True},
                    {'subsume_conditions': True, 'z3_QF_FD': False, 'z3_smt_phase': 3, 'bitenum_enabled': False},
                    {'subsume_conditions': True, 'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': True},
                    {'subsume_conditions': True, 'z3_QF_FD': False, 'z3_smt_phase': 5, 'bitenum_enabled': False}]

presets = [default_config_1, default_config_2, default_config_3]

def main():
    parser = create_argparser()

    g = parser.add_argument_group('Portfolio arguments')
    g.add_argument('--preset', choices=map(str, range(len(presets))), default='0', help='one of the preset portfolio configurations')

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
                         z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable, top_programs=args.top)
    util.store_config(base_config)

    specification = Specification(spec)
    results.specification = specification

    configs = []
    for config in presets[int(args.preset)]:
        new_config = copy.copy(base_config)
        for key, val in config.items():
            new_config.__setattr__(key, val)
        configs.append(new_config)

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

    while True:
        packet = queue.get()
        if packet[0] == util.Message.DEBUG_STATS:
            results.update_stats(*packet[1:])
        elif packet[0] == util.Message.SOLUTION:
            logger.debug('Solution found using process %d', packet[1])
            results.store_solution(*packet[2:])
            break
        elif packet[0] == util.Message.EVAL_INFO:
            pass
        else:
            logger.error('Unexpected message %s', packet[0])
            raise NotImplementedError

    for p in processes:
        p.kill()

    results.print_results()
    exit(results.exit_code)


if __name__ == '__main__':
    main()
