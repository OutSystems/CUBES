import argparse
import itertools
import pickle
import re
import time
from argparse import _StoreAction
from collections import OrderedDict, Counter
from enum import Enum
from itertools import permutations, combinations, tee, chain
from logging import getLogger
from multiprocessing.connection import Connection
from random import Random
from typing import List, Any, Iterable

import yaml
import z3
from ordered_set import OrderedSet

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

units = {"B": 1, "KB": 2 ** 10, "MB": 2 ** 20, "GB": 2 ** 30, "TB": 2 ** 40}


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


def parse_size(size):
    size = size.upper()
    if not re.match(r' ', size):
        size = re.sub(r'([KMGT]?B)', r' \1', size)
    number, unit = [string.strip() for string in size.split()]
    return int(float(number) * units[unit])


def sizeof_fmt(num, suffix='B'):
    for unit in ['', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi']:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f%s%s" % (num, 'Yi', suffix)


class CubesHelpFormatter(argparse.HelpFormatter):
    """Help message formatter which adds default values to argument help.

    Only the name of this class is considered a public API. All the methods
    provided by the class are considered an implementation detail.
    """

    def _get_help_string(self, action):
        help = action.help
        if '%(default)' not in action.help:
            if action.default is not argparse.SUPPRESS:
                defaulting_nargs = [argparse.OPTIONAL, argparse.ZERO_OR_MORE]
                if action.option_strings or action.nargs in defaulting_nargs:
                    if isinstance(action, _StoreAction):
                        help += ' (default: %(default)s)'
        return help


def create_argparser(all_inputs=False):
    parser = argparse.ArgumentParser(description='A SQL Synthesizer Using Query Reverse Engineering',
                                     formatter_class=CubesHelpFormatter)
    if not all_inputs:
        parser.add_argument('input', metavar='SPECIFICATION', type=str, help='specification file')

    g = parser.add_argument_group('Core arguments')
    g.add_argument('-v', '--verbose', action='count', default=0, help='using this flag multiple times further increases verbosity')
    g.add_argument('--seed', default='squares')
    g.add_argument('-M', '--heap-limit', type=parse_size, help='sets a memory usage limit')
    g.add_argument('--no-r', action='store_true', help="don't output R program")
    g.add_argument('--append', action='store_true', help='store the solution in the specification file')
    g.add_argument('--top', type=int, default=1, help='number of programs that should be returned')
    g.add_argument('--under', type=int, default=None, help='enumerate all programs found in under x seconds')
    g.add_argument('--no-beam-info', dest='beam_info', action='store_false', help='disable beam info usage')
    g.add_argument('--beam-threshold', type=float, default=0.9, help='minimum percentage of beams that must agree for it to be considered a majority')
    g.add_argument('--beam-name', type=str, help='beam name')

    g = parser.add_argument_group('Program space arguments')
    g.add_argument('--disable', nargs='+', default=[], help='disable DSL components')

    g.add_argument('--max-filter-combo', type=int, default=2, help='maximum size of filter conditions')
    g.add_argument('--max-cols-combo', type=int, default=2, help='maximum size of column combinations')
    g.add_argument('--max-join-combo', type=int, default=2, help='maximum size of join column combinations')

    g.add_argument('--no-max-min-use-gen-cols', dest='max_min_use_gen_cols', action='store_false', help='allow max and min functions to use results from other generated columns')

    g.add_argument('--max-lines', type=int, default=7, help='maximum program size')
    g.add_argument('--min-lines', type=int, default=0, help='minimum program size')

    g = parser.add_argument_group('Synthesizer arguments')
    g.add_argument('--no-fd', dest='qffd', action='store_false', help='use this flag to disable QF_FD theory')
    g.add_argument('--no-bitenum', dest='bitenum', action='store_false', help='use bitvector restrictions')
    g.add_argument('--subsume-conditions', dest='subsume_conditions', action='store_true', help='subsume conditions')
    g.add_argument('--no-transitive-blocking', dest='transitive_blocking', action='store_false',
                   help='disable transitive condition blocking')
    g.add_argument('--no-cache', dest='cache_operations', action='store_false',
                   help='increased memory usage, but possibly faster results')

    g = parser.add_argument_group('Debug arguments')
    g.add_argument('--use-solution-dsl', dest='use_dsl', action='store_true')
    g.add_argument('--use-solution-cube', dest='use_cube', action='store_true')
    g.add_argument('--use-solution-loc', dest='use_loc', action='store_true')

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

    for field in ['constants', 'functions', 'columns', 'filters', 'join_columns', 'groupby_columns']:
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


def all_equal(iterable):
    g = itertools.groupby(iterable)
    return next(g, True) and not next(g, False)


def at_least(iterable, percentage=0.75):
    c = Counter(map(lambda x: frozenset(x) if isinstance(x, OrderedSet) else x, iterable))
    for key, count in c.items():
        if count >= len(iterable) * percentage:
            return True, key
    return False, None


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
    """s -> (s0,s1), (s1,s2), (s2, s3), ..."""
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


def summing_dict(key_values):
    result = dict()
    for key, value in key_values:
        if key not in result:
            result[key] = value
        else:
            result[key] += value
    return result


def sum_dicts(a, b):
    if a is None:
        return b
    if len(a) < len(b):
        a, b = b, a
    result = a
    for key, value in b.items():
        if key not in result:
            result[key] = value
        else:
            result[key] += value
    return result


class Message(Enum):
    INIT = 1
    SOLVE = 2
    DEBUG_STATS = 3
    EVAL_INFO = 4
    SOLUTION = 5
    DONE = 6
    RESUME_SOLVE = 7
    NO_SOLUTION = 8
