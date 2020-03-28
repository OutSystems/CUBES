# NOTE: this file should be the only one allowed to use 'global'
import argparse
import multiprocessing
from itertools import permutations, combinations
from multiprocessing import Queue
from random import Random
from typing import List, Dict, Any, Iterable

import yaml
from ordered_set import OrderedSet

from .config import Config
from .tyrell.logger import get_logger

logger = get_logger('squares')

counter = 0
random = None
config = None


def seed(s):
    global random
    random = Random(s)


def next_counter():
    global counter  # FIXME
    counter += 1
    return counter


def get_fresh_name():
    return 'df' + str(next_counter())


def get_combinations(cols, num):
    if num == 0:
        return []
    return [", ".join(a) for a in combinations(cols, num)] + get_combinations(cols, num - 1)


def get_permutations(cols, num):
    if num == 0:
        return []
    return [", ".join(a) for a in permutations(cols, num)] + get_permutations(cols, num - 1)


def store_config(conf):
    global config
    config = conf


def get_config() -> Config:
    global config
    return config


def boolvec2int(bools: List[bool]) -> int:
    result = 0
    for i in range(len(bools)):
        if bools[i]:
            result += 2 ** i
    return result


def add_osdict(d: Dict[Any, OrderedSet], key: Any, value: Any):
    if key not in d:
        d[key] = OrderedSet()
    d[key].add(value)


def create_argparser():
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

    parser.add_argument('-j', type=int, default=-2, help='number of processes to use')
    parser.add_argument('--limit', type=int, default=7, help='maximum program size')
    parser.add_argument('--seed', default='squares')
    return parser


def parse_specification(filename):
    f = open(filename)

    spec = yaml.safe_load(f)

    if "inputs" not in spec:
        logger.error('Field "inputs" is required in spec')
        exit()

    if "output" not in spec:
        logger.error('Field "output" is required in spec')
        exit()

    for field in ["const", "aggrs", "attrs", "bools", 'filters']:
        if field not in spec:
            spec[field] = []

    if 'dateorder' not in spec:
        spec['dateorder'] = 'dmy'

    if 'loc' not in spec:
        spec['loc'] = 1

    return spec


def quote_str(string: str) -> str:
    return f'"{string}"'


def count(iter: Iterable) -> int:
    try:
        return len(iter)
    except TypeError:
        return sum(1 for _ in iter)


def get_all(queue: Queue) -> List:
    acum = []
    while not queue.empty():
        acum.append(queue.get())
    return acum
