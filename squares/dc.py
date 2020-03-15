from abc import ABC
from itertools import product
from typing import List

from .tyrell.enumerator.lines import LinesEnumerator
from .tyrell.spec import TyrellSpec


class CubeConstraint(ABC):
    def realize_constraint(self, spec: TyrellSpec, enumerator: LinesEnumerator):
        raise NotImplemented()


class LineConstraint(CubeConstraint):

    def __init__(self, line: int, production: str):
        self.line = line
        self.production = production

    def realize_constraint(self, spec, enumerator):
        return enumerator.roots[self.line].var == spec.get_function_production(self.production).id

    def __repr__(self) -> str:
        return f'lines[{self.line}] = {self.production}'


def find_production(production_list, name):
    for p in production_list:
        if p.name == name:
            return p
    return None


def move(production_list, name, position):
    e = find_production(production_list, name)
    production_list.remove(e)
    production_list.insert(position, e)


def generate_cubes(spec, loc: int, prefilled: int):
    table_productions = list(
        filter(lambda x: not isinstance(x.rhs[0], int), spec._prod_spec._get_productions_with_lhs('Table')))

    if find_production(table_productions, 'filter'):
        move(table_productions, 'filter', 3)
    if find_production(table_productions, 'summariseGrouped'):
        move(table_productions, 'summariseGrouped', 3)

    yield from sequential_generator(table_productions, prefilled)


def sequential_generator(production_list, n: int, i: int = 0, lines: List = None):
    if lines is None:
        lines = []

    if n == 0:
        yield lines
        return

    for production in production_list:
        yield from sequential_generator(production_list, n - 1, i + 1, lines + [LineConstraint(i, production.name)])
