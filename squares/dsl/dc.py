from abc import ABC
from collections import deque, namedtuple
from random import shuffle, sample
from typing import List, Tuple, Collection, Iterator, Iterable

from ordered_set import OrderedSet
from z3 import And, Or

from squares import util
from squares.results import ResultsHolder
from squares.statistics import Statistics
from squares.tyrell.dsl import ParamNode, AtomNode
from squares.tyrell.enumerator.lines import LinesEnumerator
from squares.tyrell.spec import TyrellSpec, FunctionProduction
from squares.util import count


class CubeConstraint(ABC):
    def realize_constraint(self, spec: TyrellSpec, enumerator: LinesEnumerator):
        raise NotImplemented()


class LineConstraint(CubeConstraint):

    def __init__(self, line: int, production: str, arguments: Tuple):
        self.line = line
        self.production = production
        self.arguments = arguments

    def realize_constraint(self, spec, enumerator):
        return And(
            enumerator.roots[self.line].var == spec.get_function_production_or_raise(self.production).id,
            *(Or(*(enumerator.roots[self.line].children[i].var != arg.get_id(spec) for i, arg in enumerate(args))) for args in
              self.arguments)
            )

    def __repr__(self) -> str:
        return f'l{self.line} = {self.production}({"[" + str(len(self.arguments)) + "]" if self.arguments else ""})'


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


class CubeGenerator:

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

    def next(self, dummy: bool) -> Collection[CubeConstraint]:
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
        lines = self.current()

        if self.specification.solution and len(lines) + 1 in util.get_config().solution_use_lines and len(lines) < len(
                self.specification.solution):
            productions = [p for p in productions if p.name == self.specification.solution[len(lines)]]

        if util.get_config().solution_use_last_line and self.specification.solution and len(lines) == self.n_lines - 1:
            productions = [p for p in productions if p.name == self.specification.solution[-1]]

        if find_production(productions, 'filter'):
            move(productions, 'filter', 3)
        if find_production(productions, 'summarise'):
            move(productions, 'summarise', 3)
        if find_production(productions, 'mutate'):
            move(productions, 'mutate', len(productions))
        if find_production(productions, 'inner_join'):
            move(productions, 'inner_join', len(productions))

        if util.get_config().force_summarise and \
                len(self.specification.aggrs) - count(l for l in lines if l.name == 'summarise' or l.name == 'mutate') >= self.loc - len(
            self.line_stack):
            productions = list(filter(lambda p: p.name == 'summarise' or p.name == 'mutate', productions))

        if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters) and \
                count(l for l in lines if l.name == 'filter') == 0 and \
                len(self.line_stack) == self.loc - 1:
            productions = list(filter(lambda p: p.name == 'filter', productions))

        if util.get_config().h_unlikely_two_natural_joins and len(lines) > 0 and 'natural_join' in lines[-1].name:
            move(productions, 'natural_join', len(productions))
            move(productions, 'natural_join3', len(productions))
            move(productions, 'natural_join4', len(productions))

        return deque(productions)


class Node:

    def __init__(self, head, children, parent):
        self.head = head
        self.children: List[Node] = children
        self.parent: Node = parent

    def __repr__(self) -> str:
        return repr(self.head)


# production_limit = {
#     'anti_join': 1,
#     'semi_join': 1,
#     'left_join': 1,
#     'cross_join': 1,
#     'union': 2,
#     'intersect': 1,
#     }


class StatisticCubeGenerator(CubeGenerator):

    def __init__(self, statistics: Statistics, specification, tyrell_specification, loc, n_lines, blacklist) -> None:
        super().__init__(specification, tyrell_specification, loc, n_lines, blacklist)
        self.root = Node(None, None, None)
        self.statistics = statistics

    def next(self, probe: bool) -> Collection[CubeConstraint]:
        node = self.root
        depth = 0
        cube = []
        failed = False
        while depth < self.n_lines:
            if node.children is None:
                node.children = [Node(p, None, node) for p in self.filter_productions(cube)]

            try:
                name = self.statistics.choose_production([c.head.name for c in node.children], [c.head.name for c in cube], probe, self.loc)
                node = next(filter(lambda x: x.head.name == name, node.children))
                depth += 1
                cube.append(node)
            except:
                failed = True
                break

        node_ = node
        while not node_.children:
            tmp = node_.parent
            if not tmp and failed:
                raise StopIteration
            elif not tmp:
                break
            else:
                tmp.children.remove(node_)
                node_ = tmp

        if not failed:
            return list(map(lambda x: LineConstraint(x[0], x[1].head.name, self.blacklist[x[1].head.name]), enumerate(cube)))
        return None

    def filter_productions(self, cube):
        productions = [p for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]

        # for production, max_count in production_limit.items():
        #     if count(l for l in cube if l.head.name == production) >= max_count:
        #         productions = list(filter(lambda p: p.name != production, productions))

        if util.get_config().force_summarise and len(self.specification.aggrs) - count(
                l for l in cube if l.head.name == 'summarise' or l.head.name == 'mutate') >= self.loc - len(cube):
            productions = list(filter(lambda p: p.name == 'summarise' or p.name == 'mutate', productions))

        if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters) and count(
                l for l in cube if l.head.name == 'filter') == 0 and len(cube) == self.loc - 1:
            productions = list(filter(lambda p: p.name == 'filter', productions))

        return productions
