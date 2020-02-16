#!/usr/bin/env python

import argparse
import os
from multiprocessing import Process, SimpleQueue
import random
from time import sleep

import squaresEnumerator

from squares.config import Config
from tyrell.logger import get_logger

logger = get_logger('squares')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A SQL Synthesizer Using Query Reverse Engineering')

    parser.add_argument('input', metavar='SPECIFICATION', type=str, help='specification file')

    parser.add_argument('-d', '--debug', action='store_true', help="Print debug info.")

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--symm-on', dest='symm_on', action='store_true', help="compute symmetries online")
    g.add_argument('--symm-off', dest='symm_off', action='store_true', help="compute symmetries offline")

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--r', dest='r', action='store_true', help="output R program")
    g.add_argument('--no-r', dest='r', action='store_false', help="don't output R program")
    parser.set_defaults(r=True)

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--tree', dest='tree', action='store_true', help="use tree encoding")
    g.add_argument('--lines', dest='tree', action='store_false', help="use line encoding")
    parser.set_defaults(tree=False)

    parser.add_argument('--limit', type=int, default=7, help='maximum program size')

    parser.add_argument('--seed', default='squares')

    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')
    else:
        logger.setLevel('CRITICAL')

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    configs = [
        Config(seed=seed, disabled=['semi_join']),  # original squares
        Config(seed=seed, disabled=['inner_join4'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, disabled=['inner_join3'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, disabled=['semi_join', 'inner_join4', 'anti_join', 'left_join', 'bind_rows', 'intersect'],
               z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, disabled=['semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True,
               z3_sat_phase='random'),
        Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    ]

    if len(configs) > len(os.sched_getaffinity(0)):
        logger.warn('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    Ps = []
    for i in range(len(configs)):
        P = Process(target=squaresEnumerator.main, name=str(configs[i]), args=(args, i, configs[i], queue, args.limit), daemon=True)
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
        r, sql = queue.get()

        print()
        if args.r:
            print("------------------------------------- R Solution ---------------------------------------\n")
            print(r + '\n')

        print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
        print(sql)
    else:
        print("No solution found")
        exit(1)
