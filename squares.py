#!/usr/bin/env python

import argparse
import os
from multiprocessing import Process, SimpleQueue
import random
from time import sleep

import squaresEnumerator

from squares.config import Config
from squares.util import create_argparser
from tyrell.logger import get_logger

logger = get_logger('squares')

if __name__ == '__main__':
    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    configs = [
        Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'semi_join'], force_summarise=True),
        # Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='caching'),
    ]

    if os.name == 'nt':
        logger.warning('Running on Windows is currently untested.')

    else:
        if len(configs) > len(os.sched_getaffinity(0)):
            logger.warning('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    Ps = []
    for i in range(len(configs)):
        P = Process(target=squaresEnumerator.main, name=str(configs[i]), args=(args, i, configs[i], queue, args.limit),
                    daemon=True)
        P.start()
        Ps.append(P)

    done = False
    while not done and Ps:
        sleep(.5)
        for p in Ps:
            if not p.is_alive():
                if not queue.empty():
                    done = True
                    break
                Ps.remove(p)

    for p in Ps:
        p.terminate()

    if not queue.empty():
        r, sql, process_id = queue.get()

        print(f'Solution found using process {process_id}')
        print()
        if args.r:
            print("------------------------------------- R Solution ---------------------------------------\n")
            print(r)

        if sql is not None:
            print()
            print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
            print(sql)
        else:
            print('Failed to generate SQL query')
            exit(2)
    else:
        print("No solution found")
        exit(1)
