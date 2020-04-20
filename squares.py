#!/usr/bin/env python

import os
import random
from multiprocessing import Process, SimpleQueue
from time import sleep

from squares import squares_enumerator
from squares.config import Config
from squares.tyrell.logger import get_logger
from squares.util import create_argparser, parse_specification

logger = get_logger('squares')


def main():
    parser = create_argparser()
    args = parser.parse_args()

    if args.debug:
        logger.setLevel('DEBUG')

    logger.info('Parsing specification...')
    spec = parse_specification(args.input)

    random.seed(args.seed)
    seed = random.randrange(2 ** 16)

    configs = [
        Config(seed=seed, print_r=args.r, cache_ops=args.cache_ops, z3_QF_FD=True, z3_sat_phase='random', disabled=['inner_join', 'semi_join'])
    ]

    if os.name == 'nt':
        logger.warning('Running on Windows is currently untested.')

    else:
        if len(configs) > len(os.sched_getaffinity(0)):
            logger.warning('Starting more processes than available CPU cores!')

    queue = SimpleQueue()

    processes = []
    for i in range(len(configs)):
        process = Process(target=squares_enumerator.main, name=str(configs[i]),
                          args=(args, spec, i, configs[i], queue, args.limit),
                          daemon=True)
        process.start()
        processes.append(process)

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
