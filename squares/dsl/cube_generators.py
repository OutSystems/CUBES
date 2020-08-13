import random
from abc import ABC
from functools import cached_property, reduce
from itertools import accumulate
from logging import getLogger
from typing import List, Any, Sequence, Tuple, Union, Dict, Optional

import operator

from ordered_set import OrderedSet

from squares import util, results
from squares.dsl.specification import Specification
from squares.statistics import Statistics
from squares.tyrell.enumerator import Enumerator
from squares.tyrell.enumerator.bitenum import BitEnumerator
from squares.tyrell.spec import TyrellSpec, Production
from squares.util import count

logger = getLogger('squares.synthesizer')


class CubeConstraint(ABC):
    def realize_constraint(self, spec: TyrellSpec, enumerator: BitEnumerator):
        raise NotImplemented()


class LineConstraint(CubeConstraint):

    def __init__(self, line: int, production: str):
        self.line = line
        self.production = production

    def realize_constraint(self, spec: TyrellSpec, enumerator: BitEnumerator):
        return enumerator.roots[self.line].var == spec.get_function_production_or_raise(self.production).id

    def __repr__(self) -> str:
        return f'l{self.line} = {self.production}'


class Node:

    def __init__(self, head: Any = None, children: List['Node'] = None, parent: 'Node' = None) -> None:
        self.head = head
        self.children: Optional[List[Node]] = children
        self.parent: Optional[Node] = parent

    def ancestors(self) -> List['Node']:
        result = []
        node = self
        while node.head:
            result.insert(0, node)
            node = node.parent
        return result

    def __repr__(self) -> str:
        return f'Node({self.head.name if self.head else None}, children={str(self.children)})'


class TreeBasedCubeGenerator:

    def __init__(self, specification: Specification, tyrell_specification: TyrellSpec, loc: int, max_lines: int) -> None:
        self.specification = specification
        self.tyrell_specification = tyrell_specification
        self.root = Node()
        self.loc = loc
        self.max_lines = max_lines
        self.force_stop = False
        self.next_generator = None

    def get_next_generator(self) -> 'TreeBasedCubeGenerator':
        """
        The same cube generator instance can be used in several process sets.
        As such the result must be cached so that the next line generator is the same for all process sets.
        """
        if self.next_generator:
            return self.next_generator
        self.next_generator = self._next_generator()
        return self.next_generator

    def _next_generator(self) -> 'TreeBasedCubeGenerator':
        """
        Actual next generator function.
        Child classes should implement this function so that it returns a new generator of the same type where
        only the loc differs.
        """
        raise NotImplementedError

    def init_info(self) -> tuple:
        return self.loc,

    def expand_node(self, node: Node) -> None:
        assert node.children is None, "Cannot expand a node that has already been expanded"
        node.children = [Node(p, None, node) for p in self.filter_productions(node.ancestors())]

    def block(self, core: Sequence[str], node: Node = None, path: Sequence[Node] = None) -> None:
        if node is None:
            node = self.root
        if path is None:
            path = []

        if node.children is None:
            self.expand_node(node)

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

    def next(self, probe: bool, node: Node = None, cube: List[Node] = None) -> Union[Tuple[CubeConstraint, ...], None]:
        if node is None:
            node = self.root
        if cube is None:
            cube = []

        if self.force_stop:
            raise StopIteration

        if util.get_config().use_solution_cube and self.specification.solution:
            if len(self.specification.solution) == self.loc:
                self.force_stop = True
                return tuple(map(lambda x: LineConstraint(x[0], x[1]), enumerate(self.specification.solution)))
            else:
                raise StopIteration

        if len(cube) >= self.max_lines and not self.force_stop:
            parent = node.parent
            if parent:
                parent.children.remove(node)
                self.clean_branch(parent)
            if self.max_lines < 1:
                self.force_stop = True
            return tuple(map(lambda x: LineConstraint(x[0], x[1].head.name), enumerate(cube)))

        if node.children is None:
            self.expand_node(node)

        sorted_productions = self.sort_productions([c.head.name for c in node.children], [c.head.name for c in cube], probe)
        while sorted_productions:
            child = self.choose_production(sorted_productions)
            child_node = next(filter(lambda x: x.head.name == child, node.children))
            result = self.next(probe, child_node, cube + [child_node])
            if result:
                return result
            sorted_productions.pop(child)

        if not cube:
            raise StopIteration
        else:
            return None

    def clean_branch(self, node: Node) -> None:
        node_ = node
        while not node_.children:
            tmp = node_.parent
            if not tmp:
                break
            else:
                tmp.children.remove(node_)
                node_ = tmp

    @cached_property
    def productions(self) -> List[Production]:
        return [p for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]

    def filter_productions(self, current_path: List[Node]) -> List[Production]:
        productions = self.productions

        if util.get_config().force_summarise and len(set(self.specification.aggrs) - {'concat'}) - \
                count(l for l in current_path if l.head.name == 'summarise' or l.head.name == 'mutate') >= self.loc - len(current_path):
            productions = list(filter(lambda p: p.name == 'summarise' or p.name == 'mutate', productions))

        if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters) and \
                count(l for l in current_path if l.head.name == 'filter') == 0 and len(current_path) == self.loc - 1:
            productions = list(filter(lambda p: p.name == 'filter', productions))

        return productions

    def sort_productions(self, allowed_productions: List[str], current_path: List[str], is_probe: bool) -> Any:
        raise NotImplementedError

    def choose_production(self, sorted_productions: Any) -> str:
        raise NotImplementedError

    def __str__(self) -> str:
        return f'{type(self).__name__}(loc={self.loc}, max_lines={self.max_lines})'


class StaticCubeGenerator(TreeBasedCubeGenerator):

    def __init__(self, specification, tyrell_specification, loc, max_lines) -> None:
        super().__init__(specification, tyrell_specification, loc, max_lines)

    def _next_generator(self) -> 'StaticCubeGenerator':
        return StaticCubeGenerator(self.specification, self.tyrell_specification, self.loc + 1, self.max_lines + 1)

    def sort_productions(self, allowed_productions: List[str], current_path: List[str], is_probe: bool) -> List[str]:
        productions = allowed_productions.copy()
        if 'filter' in productions:
            productions.remove('filter')
            productions.insert(3, 'filter')
        if 'summarise' in productions:
            productions.remove('summarise')
            productions.insert(3, 'summarise')
        if 'mutate' in productions:
            productions.remove('mutate')
            productions.insert(3, 'mutate')
        if 'inner_join' in productions:
            productions.remove('inner_join')
            productions.append('inner_join')
        if 'cross_join' in productions:
            productions.remove('cross_join')
            productions.append('cross_join')

        if util.get_config().h_unlikely_two_natural_joins and len(current_path) > 0 and 'natural_join' == current_path[-1]:
            if 'natural_join' in productions:
                productions.remove('natural_join')
                productions.append('natural_join')
            if 'natural_join3' in productions:
                productions.remove('natural_join3')
                productions.append('natural_join3')
            if 'natural_join4' in productions:
                productions.remove('natural_join4')
                productions.append('natural_join4')
        return productions

    def choose_production(self, sorted_productions: List[str]) -> str:
        return sorted_productions[0]


class StatisticCubeGenerator(TreeBasedCubeGenerator):

    def __init__(self, specification, tyrell_specification, loc, max_lines, statistics: Statistics) -> None:
        super().__init__(specification, tyrell_specification, loc, max_lines)
        self.statistics = statistics
        self.random = random.Random(util.get_config().seed)
        # logger.warning('First line for %s generated %d possibilities', self, self.estimated_programs())

    def _next_generator(self) -> 'StatisticCubeGenerator':
        return StatisticCubeGenerator(self.specification, self.tyrell_specification, self.loc + 1, self.max_lines + 1, self.statistics)

    def sort_productions(self, allowed_productions: List[str], current_path: List[str], is_probe: bool) -> Dict[str, float]:
        return self.statistics.sort_productions(allowed_productions, current_path, is_probe)

    def choose_production(self, sorted_productions: Dict[str, float]) -> str:
        return self.random.choices(list(sorted_productions.keys()), list(sorted_productions.values()))[0]


class BlockStatisticCubeGenerator(StatisticCubeGenerator):

    def __init__(self, specification, tyrell_specification, loc, max_lines, statistics: Statistics, disabled: list) -> None:
        self.disabled = disabled
        super().__init__(specification, tyrell_specification, loc, max_lines, statistics)

    # def estimated_programs(self) -> int:
    #     lines_left = self.loc
    #     programs = 1
    #     if util.get_config().force_summarise and len(set(self.specification.aggrs) - {'concat'}) != 0:
    #         if lines_left >= len(set(self.specification.aggrs) - {'concat'}):
    #             programs *= reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in self.tyrell_specification.get_function_production_or_raise('summarise').rhs), 1) ** len(set(self.specification.aggrs) - {'concat'})
    #             lines_left -= len(set(self.specification.aggrs) - {'concat'})
    #         else:
    #             programs = 0
    #     if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters):
    #         if lines_left >= 1:
    #             programs *= reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in self.tyrell_specification.get_function_production_or_raise('filter').rhs), 1)
    #             lines_left -= 1
    #         else:
    #             programs = 0
    #     programs *= sum(reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in prod.rhs), 1) for prod in self.productions) ** lines_left
    #     return programs

    def _next_generator(self) -> 'BlockStatisticCubeGenerator':
        return BlockStatisticCubeGenerator(self.specification, self.tyrell_specification, self.loc + 1, self.max_lines + 1, self.statistics,
                                           self.disabled)

    def init_info(self) -> tuple:
        return self.loc, {'disabled': self.disabled}

    @cached_property
    def productions(self) -> List[Production]:
        return [p for p in super().productions if p.name not in self.disabled]


class ForceStatisticCubeGenerator(StatisticCubeGenerator):

    def __init__(self, specification, tyrell_specification, loc, max_lines, statistics: Statistics, forced: list) -> None:
        self.forced = forced
        super().__init__(specification, tyrell_specification, loc, max_lines, statistics)

    # def estimated_programs(self) -> int:
    #     lines_left = self.loc
    #     programs = 1
    #     programs *= sum(reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in self.tyrell_specification.get_function_production_or_raise(prod).rhs), 1) for prod in self.forced)
    #     lines_left -= 1
    #     if util.get_config().force_summarise and len(set(self.specification.aggrs) - {'concat'}) != 0:
    #         if lines_left >= len(set(self.specification.aggrs) - {'concat'}):
    #             programs *= reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in self.tyrell_specification.get_function_production_or_raise('summarise').rhs), 1) ** len(set(self.specification.aggrs) - {'concat'})
    #             lines_left -= len(set(self.specification.aggrs) - {'concat'})
    #         else:
    #             programs = 0
    #     if not self.specification.aggrs_use_const and (self.specification.consts or self.specification.filters):
    #         if lines_left >= 1:
    #             programs *= reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in self.tyrell_specification.get_function_production_or_raise('filter').rhs), 1)
    #             lines_left -= 1
    #         else:
    #             programs = 0
    #     programs *= sum(reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else 1 for rhs in prod.rhs), 1) for prod in self.productions) ** lines_left
    #     return programs

    def _next_generator(self) -> 'ForceStatisticCubeGenerator':
        return ForceStatisticCubeGenerator(self.specification, self.tyrell_specification, self.loc + 1, self.max_lines + 1, self.statistics,
                                           self.forced)

    def filter_productions(self, current_path: List[Node]) -> List[Production]:
        productions = super().filter_productions(current_path)

        if count(l for l in current_path if l.head.name in self.forced) == 0 and len(current_path) == self.loc - 1:
            productions = list(filter(lambda p: p.name in self.forced, productions))

        return productions
