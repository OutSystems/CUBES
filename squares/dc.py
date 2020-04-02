from abc import ABC
from collections import deque
from typing import List, Tuple, Collection, Iterator, Iterable

from z3 import And, Or

from . import util
from .tyrell.dsl import ParamNode, AtomNode
from .tyrell.enumerator.lines import LinesEnumerator
from .tyrell.spec import TyrellSpec, FunctionProduction
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
                   *(Or(*(enumerator.roots[self.line].children[i].var != arg.get_id(spec) for i, arg in enumerate(args))) for args in
                     self.arguments))

    def __repr__(self) -> str:
        return f'l{self.line} = {self.production}({",".join(map(str, self.arguments))})'


class Param:

    def __init__(self, n) -> None:
        self.n = n

    def __eq__(self, o: object) -> bool:
        if isinstance(o, Param):
            return self.n == o.n
        else:
            return False

    def __hash__(self) -> int:
        return hash(self.n)

    def get_id(self, spec: TyrellSpec):
        return spec.get_param_production(self.n).id

    def __repr__(self) -> str:
        return f'@{self.n}'


class Atom:

    def __init__(self, lhs, rhs) -> None:
        self.lhs = lhs
        self.rhs = rhs

    def __eq__(self, o: object) -> bool:
        if isinstance(o, Atom):
            return self.lhs == o.lhs and self.rhs == o.rhs
        else:
            return False

    def __hash__(self) -> int:
        return hash((self.lhs, self.rhs))

    def get_id(self, spec: TyrellSpec):
        return spec.get_enum_production(self.lhs, self.rhs).id

    def __repr__(self) -> str:
        return f'{self.rhs}'


def map_node(node):
    if isinstance(node, ParamNode):
        return Param(node.index)
    elif isinstance(node, AtomNode):
        return Atom(node.type, node.data)
    else:
        raise NotImplementedError()


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


class CubeGenerator(Iterable):

    def __init__(self, specification, tyrell_specification, loc, n_lines, blacklist) -> None:
        self.line_stack = list()
        self.specification = specification
        self.tyrell_specification: TyrellSpec = tyrell_specification
        self.loc = loc
        self.n_lines = n_lines
        self.fill_stack()
        self.exahusted = False
        self.blacklist = blacklist

    def fill_stack(self):
        while len(self.line_stack) < self.n_lines:
            self.line_stack.append(self.filtered_productions())

    def __iter__(self) -> Iterator[Collection[CubeConstraint]]:
        return self

    def __next__(self) -> Collection[CubeConstraint]:
        self.update_blacklist()

        if self.n_lines == 0 and self.exahusted == False:
            self.exahusted = True
            return tuple()
        elif self.n_lines == 0:
            raise StopIteration

        while not self.line_stack[-1]:
            while self.line_stack and not self.line_stack[-1]:
                self.line_stack.pop()
            if not self.line_stack:
                raise StopIteration
            self.line_stack[-1].popleft()
            if not self.line_stack[-1]:
                continue
            self.fill_stack()

        cube = self.collect_current()
        self.line_stack[-1].popleft()

        return cube

    def collect_current(self) -> Tuple[LineConstraint]:
        cube = tuple(LineConstraint(i, line[0].name, self.blacklist[line[0].name]) for i, line in enumerate(self.line_stack))
        return cube

    def current(self) -> List[FunctionProduction]:
        cube = [line[0] for line in self.line_stack if line]
        return cube

    def filtered_productions(self):
        productions = [p for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]
        # return deque(productions)
        lines = self.current()

        if util.get_config().solution_use_last_line and self.specification.solution and len(lines) == self.n_lines - 1:
            productions = [p for p in productions if p.name == self.specification.solution[-2]]

        # if util.get_config().solution_use_last_line and self.specification.solution and len(lines) == self.n_lines - 2:
        #     productions = [p for p in productions if p.name == self.specification.solution[-3]]

        if find_production(productions, 'filter'):
            move(productions, 'filter', 3)
        if find_production(productions, 'summariseGrouped'):
            move(productions, 'summariseGrouped', 3)
        if find_production(productions, 'inner_join'):
            move(productions, 'inner_join', len(productions))

        if util.get_config().force_summarise and len(self.specification.aggrs) - count(
                l for l in lines if l.name == 'summariseGrouped') >= self.loc - len(self.line_stack) - 1:
            productions = list(filter(lambda p: p.name == 'summariseGrouped', productions))

        if (self.specification.consts or self.specification.filters) and count(l for l in lines if l.name == 'filter') == 0 and len(
                self.line_stack) >= self.loc - 2:
            productions = list(filter(lambda p: p.name == 'filter', productions))

        if len(lines) > 0 and (lines[-1].name == 'natural_join' or lines[-1].name == 'natural_join3'):
            productions = list(filter(lambda x: 'natural_join' not in x.name, productions))

        if len(lines) > 0 and lines[-1].name == 'natural_join4':
            move(productions, 'natural_join', len(productions))
            move(productions, 'natural_join3', len(productions))
            move(productions, 'natural_join4', len(productions))

        return deque(productions)

    def update_blacklist(self):
        pass
