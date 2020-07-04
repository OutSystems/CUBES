from collections import defaultdict
from logging import getLogger
from typing import Dict, Mapping

from ordered_set import OrderedSet

from squares.tyrell.spec import Type, TyrellSpec

logger = getLogger('squares.conditions')


class ConditionTable:

    def __init__(self) -> None:
        self.graphs = defaultdict(lambda: defaultdict(OrderedSet))

    def append(self, t: Type, origin: str, destination: str):
        self.graphs[t][origin].append(destination)

    def dfs(self, t: Type, key: str, visited: OrderedSet[str] = None) -> OrderedSet[str]:
        if visited is None:
            visited = OrderedSet()
        if key not in visited:
            visited.add(key)
            for neighbour in self.graphs[t][key]:
                self.dfs(t, neighbour, visited)
        return visited - OrderedSet([key])

    def compile(self, spec: TyrellSpec) -> Mapping[int, OrderedSet[int]]:
        return ConditionTableJIT(self, spec)


class ConditionTableJIT:

    def __init__(self, base_conditions: ConditionTable, spec: TyrellSpec) -> None:
        self.base_conditions = base_conditions
        self.spec: TyrellSpec = spec
        self.compiled: Dict[int, OrderedSet[int]] = {}

    def dfs(self, key: int) -> OrderedSet[str]:
        if key not in self.compiled.keys():
            self.compiled[key] = OrderedSet()
            production = self.spec.get_production(key)
            if production and production.is_enum():
                for neighbour in self.base_conditions.graphs[production.lhs][production.rhs]:
                    n_production = self.spec.get_enum_production(production.lhs, neighbour)
                    if n_production:
                        tmp = self.dfs(n_production.id)
                        self.compiled[key].update(tmp)
                    else:
                        logger.warning('Unknown production "%s" in type %s', neighbour, production.lhs)
        return self.compiled[key] | {key}

    def __getitem__(self, item: int) -> OrderedSet[int]:
        if item not in self.compiled:
            self.dfs(item)
        return self.compiled[item] - {item}
