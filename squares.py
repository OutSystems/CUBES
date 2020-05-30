#!/usr/bin/env python

import os
import random
from logging import getLogger
from multiprocessing import Process, SimpleQueue
from time import sleep

import signal

from squares import squares_enumerator
from squares.config import Config
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

    configs = [
        Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations, optimal=args.optimal,
               solution_use_lines=args.use_lines, solution_use_last_line=args.use_last, advance_processes=args.split_search,
               static_search=args.static_search,
               programs_per_cube_threshold=args.split_search_threshold, minimum_loc=args.min_lines, maximum_loc=args.max_lines,
               max_filter_combinations=args.max_filter_combo, max_column_combinations=args.max_cols_combo,
               max_join_combinations=args.max_join_combo, good_program_weight=args.good_program_weight,
               strictly_good_program_weight=args.strictly_good_program_weight, program_weigth_decay_rate=args.decay_rate,
               probing_threads=args.probing_threads, cube_freedom=args.cube_freedom,
               z3_QF_FD=True, z3_sat_phase='random', disabled=args.disable)
        ]

    if os.name == 'nt':
        logger.warning('Running on Windows is currently untested.')

    else:
        if len(configs) > len(os.sched_getaffinity(0)):
            logger.warning('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    processes = []
    for i in range(len(configs)):
        process = Process(target=squares_enumerator.main, name=f'process{i}',
                          args=(args, spec, i, configs[i], queue),
                          daemon=True)
        process.start()
        processes.append(process)

    def handle_sigint(signal, stackframe):
        for process in processes:
            process.join()

    signal.signal(signal.SIGINT, handle_sigint)
    signal.signal(signal.SIGTERM, handle_sigint)

    done = False
    while not done and processes:
        sleep(.5)
        for p in processes:
            if not p.is_alive():
                if not queue.empty():
                    done = True
                    break
                processes.remove(p)

    for p in processes:
        p.terminate()

    if not queue.empty():
        exit(queue.get())

    exit(1)


if __name__ == '__main__':
    main()
