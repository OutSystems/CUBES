# NOTE: this file should be the only one allowed to use 'global'
import argparse
from itertools import permutations, combinations
from random import Random
from typing import List, Dict, Any, Iterable, Sequence

from ordered_set import OrderedSet

from .config import Config

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


def current_counter():
    global counter
    return counter


def get_fresh_name():
    return 'df' + str(next_counter())


def get_combinations(cols, num):
    if num == 0:
        return []
    return [", ".join(a) for a in combinations(cols, num)] + get_combinations(cols, num - 1)


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
    parser.add_argument('--limit', type=int, default=7, help='maximum program size')
    parser.add_argument('--seed', default='squares')

    return parser
