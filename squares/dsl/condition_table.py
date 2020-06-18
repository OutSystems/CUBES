from collections import defaultdict
from logging import getLogger
from typing import Dict

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

    def compile(self, spec: TyrellSpec) -> Dict[int, OrderedSet[int]]:
        result = defaultdict(OrderedSet)
        missing_values = False
        for type, graph in self.graphs.items():
            for key in list(graph.keys()):
                prod0 = spec.get_enum_production(type, key)
                if prod0 is None:
                    missing_values = True
                    continue
                for condition in self.dfs(type, key):
                    prod1 = spec.get_enum_production(type, condition)
                    if prod1 is None:
                        missing_values = True
                        continue
                    result[prod0.id].append(prod1.id)
        if missing_values:
            logger.warning('There were some missing values while compiling condition table.')
        return result
