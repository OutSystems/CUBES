from itertools import permutations
from random import Random

counter = 0
random = None


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
