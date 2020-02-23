# NOTE: this file should be the only one allowed to use 'global'

from itertools import permutations
from random import Random
from typing import List

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
    return 'RET_DF' + str(next_counter())


def get_fresh_col():
    return 'COL' + str(next_counter())


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
