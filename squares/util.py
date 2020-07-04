import argparse
import pickle
import time
from collections import OrderedDict
from enum import Enum
from itertools import permutations, combinations, tee, chain
from logging import getLogger
from multiprocessing.connection import Connection
from random import Random
from typing import List, Any, Iterable

import yaml
import z3

from .config import Config
from .tyrell.logger import setup_logger

setup_logger('squares')
setup_logger('tyrell')
logger = getLogger('squares')

z3.Z3_DEBUG = False

counter = 0
random = None
config = None
solution = None
program_queue = None

BUFFER_SIZE = 8 * 4 * 1024


class literal(str):
    pass


def literal_presenter(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')


yaml.add_representer(literal, literal_presenter)


def seed(s):
    global random
    random = Random(s)


def next_counter():
    global counter  # FIXME
    counter += 1
    return counter


def get_fresh_name():
    return 'df' + str(next_counter())


def powerset_except_empty(cols, num=None):
    if num is None:
        num = len(cols)
    if num == 0:
        return []
    return powerset_except_empty(cols, num - 1) + [a for a in combinations(cols, num)]


def all_permutations(cols, num=None):
    if num is None:
        num = len(cols)
    if num == 0:
        return []
    return all_permutations(cols, num - 1) + [a for a in permutations(cols, num)]


def get_combinations(cols, num):
    if num == 0:
        return []
    return get_combinations(cols, num - 1) + [", ".join(a) for a in combinations(cols, num)]


def get_permutations(cols, num):
    if num == 0:
        return []
    return get_permutations(cols, num - 1) + [", ".join(a) for a in permutations(cols, num)]


def store_config(conf):
    global config
    config = conf


def get_config() -> Config:
    global config
    return config


def set_program_queue(q):
    global program_queue
    program_queue = q


def get_program_queue():
    return program_queue


def boolvec2int(bools: List[bool]) -> int:
    result = 0
    for i in range(len(bools)):
        if bools[i]:
            result += 2 ** i
    return result


def create_argparser(all_inputs=False):
    parser = argparse.ArgumentParser(description='A SQL Synthesizer Using Query Reverse Engineering',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    if not all_inputs:
        parser.add_argument('input', metavar='SPECIFICATION', type=str, help='specification file')
    parser.add_argument('-v', '--verbose', action='count', default=0, help='using this flag multiple times further increases verbosity')

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--symm-on', dest='symm_on', action='store_true', help="enable online symmetry breaking")
    g.add_argument('--symm-off', dest='symm_off', action='store_true', help="enable offline symmetry breaking")

    parser.add_argument('--no-r', action='store_true', help="don't output R program")

    parser.add_argument('--optimal', action='store_true', help='make sure that returned solutions are as short as possible')
    parser.add_argument('--no-cache', dest='cache_operations', action='store_false',
                        help='increased memory usage, but possibly faster results')
    parser.add_argument('--static-search', action='store_true', help='search for solutions using a static ordering')
    parser.add_argument('--cube-freedom', type=int, default=0, help='number of free lines when generating cubes')
    parser.add_argument('--no-block-commutative-ops', dest='block_commutative_ops', action='store_false',
                        help='block commutative operations')
    parser.add_argument('--no-subsume-conditions', dest='subsume_conditions', action='store_false', help='subsume conditions')
    parser.add_argument('--no-transitive-blocking', dest='transitive_blocking', action='store_false', help='subsume conditions transitively')

    g = parser.add_argument_group('heuristics')

    g.add_argument('--split-search', action='store_true',
                   help='use an heuristic to determine if search should be split among multiple lines of code')
    g.add_argument('--split-search-threshold', type=int, default=500, help='instance hardness threshold')
    g.add_argument('--decay-rate', type=float, default=0.99999, help='rate at which old information is forgotten')
    g.add_argument('--probing-threads', type=int, default=2,
                   help='number of threads that should be used to randomly explore the search space')

    parser.add_argument('--disable', nargs='+', default=[])

    parser.add_argument('--max-filter-combo', type=int, default=2, help='maximum size of filter conditions')
    parser.add_argument('--max-cols-combo', type=int, default=2, help='maximum size of column combinations')
    parser.add_argument('--max-join-combo', type=int, default=2, help='maximum size of join column combinations')

    parser.add_argument('--use-solution-line', dest='use_lines', type=int, action='append', default=[])
    parser.add_argument('--use-solution-last-line', dest='use_last', action='store_true')

    parser.add_argument('-j', '--jobs', type=int, default=-2, help='number of processes to use')
    parser.add_argument('--max-lines', type=int, default=7, help='maximum program size')
    parser.add_argument('--min-lines', type=int, default=1, help='minimum program size')
    parser.add_argument('--no-fd', dest='qffd', action='store_false', help='use this flag to disable QF_FD theory')
    parser.add_argument('--seed', default='squares')
    parser.add_argument('--append', action='store_true')
    return parser


def parse_specification(filename):
    f = open(filename)

    spec = yaml.safe_load(f)

    if 'inputs' not in spec:
        logger.error('Field "inputs" is required in spec')
        exit()

    if 'output' not in spec:
        logger.error('Field "output" is required in spec')
        exit()

    if 'attrs' in spec:
        logger.warning('"attrs" field is deprecated. Please use "columns"')
        spec['columns'] = spec['attrs']

    if 'aggrs' in spec:
        logger.warning('"aggrs" field is deprecated. Please use "functions"')
        spec['functions'] = spec['aggrs']

    if 'const' in spec:
        logger.warning('"const" field is deprecated. Please use "constants"')
        spec['constants'] = spec['const']

    for field in ['constants', 'functions', 'columns', 'filters']:
        if field not in spec:
            spec[field] = None

    if 'dateorder' not in spec:
        spec['dateorder'] = None

    return spec


def write_specification(spec, file):
    spec = OrderedDict(spec)

    for key in list(spec.keys()):
        if spec[key] is None:
            spec.pop(key)

    if 'comment' in spec:
        spec['comment'] = literal(spec['comment'])

    spec.move_to_end('inputs')
    spec.move_to_end('output')
    if 'functions' in spec:
        spec.move_to_end('functions')
    if 'filters' in spec:
        spec.move_to_end('filters')
    if 'constants' in spec:
        spec.move_to_end('constants')
    if 'columns' in spec:
        spec.move_to_end('columns')
    if 'foreign-keys' in spec:
        spec.move_to_end('foreign-keys')
    if 'dateorder' in spec:
        spec.move_to_end('dateorder')
    if 'comment' in spec:
        spec.move_to_end('comment')

    yaml.dump(dict(spec), file, default_flow_style=False, sort_keys=False)


def single_quote_str(string: str) -> str:
    return f"'{string}'"


def count(iter: Iterable) -> int:
    try:
        return len(iter)
    except TypeError:
        return sum(1 for _ in iter)


def pipe_write(pipe: Connection, ret: Any):
    data = pickle.dumps(ret, protocol=-1)
    size = len(data)
    pipe.send(size)
    counter = 0
    while counter < size:
        while not pipe.writable:
            time.sleep(0.1)
        pipe.send_bytes(data, counter, min(size - counter, BUFFER_SIZE))
        counter += min(size - counter, BUFFER_SIZE)


def pipe_read(pipe: Connection) -> Any:
    size = pipe.recv()
    data = bytearray(size)
    counter = 0
    while counter < size:
        counter += pipe.recv_bytes_into(data, counter)

    return pickle.loads(data)


def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)


def has_duplicates(iterable):
    seen = set()
    for x in iterable:
        if x in seen:
            return True
        seen.add(x)
    return False


def flatten(list_of_lists):
    return chain.from_iterable(list_of_lists)


class Message(Enum):
    INIT = 1
    SOLVE = 2
    DEBUG_STATS = 3
    EVAL_INFO = 4
    SOLUTION = 5
