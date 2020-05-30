from abc import ABC
from collections import deque
from logging import getLogger
from typing import List, Tuple, Collection

from z3 import And

from squares import util, results
from squares.statistics import Statistics
from squares.tyrell.enumerator.lines import LinesEnumerator
from squares.tyrell.spec import TyrellSpec, FunctionProduction
from squares.util import count

logger = getLogger('squares.synthesizer')


class CubeConstraint(ABC):
    def realize_constraint(self, spec: TyrellSpec, enumerator: LinesEnumerator):
        raise NotImplemented()


class LineConstraint(CubeConstraint):

    def __init__(self, line: int, production: str):
        self.line = line
        self.production = production

    def realize_constraint(self, spec, enumerator):
        return enumerator.roots[self.line].var == spec.get_function_production_or_raise(self.production).id

    def __repr__(self) -> str:
        return f'l{self.line} = {self.production}'


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

    def __init__(self, specification, tyrell_specification, loc, n_lines) -> None:
        self.line_stack = list()
        self.specification = specification
        self.tyrell_specification: TyrellSpec = tyrell_specification
        self.loc = loc
        self.n_lines = n_lines
        self.fill_stack()
        self.exahusted = False

    def fill_stack(self):
        while len(self.line_stack) < self.n_lines:
            self.line_stack.append(self.filtered_productions())

    def block(self, core):
        logger.warning('Block is noop for CubeGenerator.')

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
        cube = tuple(LineConstraint(i, line[0].name) for i, line in enumerate(self.line_stack))
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
        return f'Node({self.head.name if self.head else None}, children={str(self.children)})'


class StatisticCubeGenerator(CubeGenerator):

    def __init__(self, statistics: Statistics, specification, tyrell_specification, loc, n_lines) -> None:
        super().__init__(specification, tyrell_specification, loc, n_lines)
        self.root = Node(None, None, None)
        self.statistics = statistics
        self.force_stop = False

    def block(self, core, node=None, path=None):
        if node is None:
            node = self.root
        if path is None:
            path = []

        if node.children is None:
            node.children = [Node(p, None, node) for p in self.filter_productions(path)]

        if len(core) == 1:
            if core[0] is None:
                for child in node.children:
                    # logger.debug('Blocking %s', str(list(map(lambda x: x.head.name, path + [child]))))
                    results.blocked_cubes += 1
                node.children = []
                self.clean_branch(node)
            else:
                try:
                    # logger.debug('Blocking %s', str(list(map(lambda x: x.head.name,path + [next(filter(lambda x: x.head.name == core[0], node.children))]))))
                    node.children = [child for child in node.children if child.head.name != core[0]]
                    self.clean_branch(node)
                    results.blocked_cubes += 1
                except StopIteration:
                    pass  # trying to block already blocked cube
            return

        if core[0] is None:
            for child in node.children:
                self.block(core[1:], child, path + [child])
        else:
            try:
                child = next(filter(lambda x: x.head.name == core[0], node.children))
                self.block(core[1:], child, path + [child])
            except StopIteration:
                pass  # trying to block already blocked cube

    def next(self, probe: bool, node=None, cube=None):
        if node is None:
            node = self.root
        if cube is None:
            cube = []

        if self.force_stop:
            raise StopIteration

        if len(cube) >= self.n_lines and not self.force_stop:
            parent = node.parent
            if parent:
                parent.children.remove(node)
                self.clean_branch(parent)
            if self.n_lines < 1:
                self.force_stop = True
            return tuple(map(lambda x: LineConstraint(x[0], x[1].head.name), enumerate(cube)))

        if node.children is None:
            node.children = [Node(p, None, node) for p in self.filter_productions(cube)]

        for child in self.statistics.sort_productions([c.head.name for c in node.children], [c.head.name for c in cube], probe, self.loc):
            child_node = next(filter(lambda x: x.head.name == child, node.children))
            result = self.next(probe, child_node, cube + [child_node])
            if result:
                return result

        if not cube:
            raise StopIteration
        else:
            return None

    def clean_branch(self, node):
        node_ = node
        while not node_.children:
            tmp = node_.parent
            if not tmp:
                break
            else:
                tmp.children.remove(node_)
                node_ = tmp

    def filter_productions(self, cube):
        productions = [p for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]

        if util.get_config().force_summarise and len(self.specification.aggrs) - count(
                l for l in cube if l.head.name == 'summarise' or l.head.name == 'mutate') >= self.loc - len(cube):
            productions = list(filter(lambda p: p.name == 'summarise' or p.name == 'mutate', productions))

        if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters) and count(
                l for l in cube if l.head.name == 'filter') == 0 and len(cube) == self.loc - 1:
            productions = list(filter(lambda p: p.name == 'filter', productions))

        return productions

    def __str__(self) -> str:
        return f'CubeGenerator(loc={self.loc}, n_lines={self.n_lines})'


