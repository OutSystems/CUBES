from abc import ABC
from itertools import product
from typing import List, Tuple, Iterable

from z3 import And

from .tyrell.enumerator.lines import LinesEnumerator
from .tyrell.spec import TyrellSpec
from .util import count


class CubeConstraint(ABC):
    def realize_constraint(self, spec: TyrellSpec, enumerator: LinesEnumerator):
        raise NotImplemented()


class LineConstraint(CubeConstraint):

    def __init__(self, line: int, production: str, arguments: Tuple):
        self.line = line
        self.production = production
        self.arguments = arguments

    def realize_constraint(self, spec, enumerator):
        return And(enumerator.roots[self.line].var == spec.get_function_production(self.production).id,
                   *(enumerator.roots[self.line].children[i].var == a.id for i, a in enumerate(self.arguments) if a is not None))

    def __repr__(self) -> str:
        return f'lines[{self.line}] = {self.production}({",".join(map(str, self.arguments))})'


def find_production(production_list, name):
    for p in production_list:
        if p.name == name:
            return p
    return None


def move(production_list, name, position):
    try:
        e = find_production(production_list, name)
        production_list.remove(e)
        production_list.insert(position, e)
    except:
        pass


def generate_cubes(spec, tyrell_spec, loc: int, prefilled: int):
    table_productions = list(filter(lambda p: p.is_function(), tyrell_spec.get_productions_with_lhs('Table')))

    if find_production(table_productions, 'filter'):
        move(table_productions, 'filter', 3)
    if find_production(table_productions, 'summariseGrouped'):
        move(table_productions, 'summariseGrouped', 3)
    if find_production(table_productions, 'inner_join'):
        move(table_productions, 'inner_join', len(table_productions))

    yield from sequential_generator(spec, tyrell_spec, table_productions, loc, prefilled, 1)


def sequential_generator(spec, tyrell_spec: TyrellSpec, production_list, loc, n_lines: int, full_lines: int, current_line: int = 0, lines: List = None):
    if lines is None:
        lines = []

    if n_lines == 0:
        yield lines
        return

    production_list = production_list.copy()

    if len(spec.aggrs) - count(l for l in lines if l.production == 'summariseGrouped') >= loc - current_line - 1:
        production_list = list(filter(lambda p: p.name == 'summariseGrouped', production_list))

    if (spec.consts or spec.filters) and count(l for l in lines if l.production == 'filter') == 0 and current_line >= loc - 2:
        production_list = list(filter(lambda p: p.name == 'filter', production_list))

    if len(lines) > 0 and (lines[-1].production == 'natural_join' or lines[-1].production == 'natural_join3'):
        production_list = list(filter(lambda x: 'natural_join' not in x.name, production_list))

    if len(lines) > 0 and lines[-1].production == 'natural_join4':
        move(production_list, 'natural_join', len(production_list))
        move(production_list, 'natural_join3', len(production_list))
        move(production_list, 'natural_join4', len(production_list))

    for production in production_list:
        combos = []

        if full_lines > 0:
            for arg in production.rhs:
                if arg.name == 'Table':
                    combos.append([None])
                else:
                    combos.append([p for p in tyrell_spec.get_productions_with_lhs(arg.name) if p.is_param() or p.is_enum()])

        new_full_lines = full_lines - 1 if combos and any(map(lambda x: any(map(lambda y: y is not None, x)), combos)) else full_lines

        for args in product(*combos):
            yield from sequential_generator(spec, tyrell_spec, production_list, loc, n_lines - 1, new_full_lines, current_line + 1,
                                            lines + [LineConstraint(current_line, production.name, args)])
