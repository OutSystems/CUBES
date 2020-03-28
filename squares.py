#!/usr/bin/env python

import os
import random
from multiprocessing import Process, SimpleQueue
from time import sleep, time

from squares import squares_enumerator
from squares.config import Config
from squares.tyrell.logger import get_logger
from squares.util import create_argparser, parse_specification

logger = get_logger('squares')


def main():
    start = time()
    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    configs = [
        Config(seed=seed, disabled=['inner_join', 'semi_join'], ignore_aggrs=True, force_summarise=False, z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=[], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=["max"], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=["min"], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=["mean"], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=["n"], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
        Config(seed=seed, aggregation_functions=["n", "max"], ignore_aggrs=True, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    ]

    if os.name == 'nt':
        logger.warning('Running on Windows is currently untested.')

    else:
        if len(configs) > len(os.sched_getaffinity(0)):
            logger.warning('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    Ps = []
    for i in range(len(configs)):
        P = Process(target=squares_enumerator.main, name=str(configs[i]),
                    args=(args, spec, i, configs[i], queue, args.limit),
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

        print('Time: ', time() - start)
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


if __name__ == '__main__':
    main()
