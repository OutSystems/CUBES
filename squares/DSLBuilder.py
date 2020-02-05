from abc import ABC
from typing import List, Tuple

from squares import util


class DSLElement(ABC):
    pass


class DSLEnum(DSLElement):

    def __init__(self, name: str, values: List[str]):
        self._name = name
        self._values = values
        self._wrapped = map(lambda x: f'"{x}"', values)

    def __repr__(self) -> str:
        return f'enum {self._name} {{\n' \
               f'\t{", ".join(self._wrapped)}\n' \
               f'}}\n'


class DSLValue(DSLElement):

    def __init__(self, name: str, properties: List[Tuple[str, str]]):
        self._name = name
        self._properties = properties

    def __repr__(self) -> str:
        if self._properties:
            tmp = map(lambda x: f'\t{x[0]}: {x[1]};\n', self._properties)
            return f'value {self._name} {{\n' \
                   f'{"".join(tmp)}' \
                   f'}}\n'
        else:
            return f'value {self._name};\n'


class DSLFunction(DSLElement):

    def __init__(self, name: str, output: str, inputs: List[str], constraints: List[str]):
        self._name = name
        self._output = output
        self._inputs = inputs
        self._constraints = constraints

    def __repr__(self) -> str:
        if self._constraints:
            tmp = ' {\n' + ''.join(map(lambda x: f'\t{x};\n', self._constraints)) + '}'
        else:
            tmp = ';'

        return f'func {self._name}: {self._output} -> {", ".join(self._inputs)}{tmp}\n'


class DSLPredicate(DSLElement):

    def __init__(self, name: str, arguments: List[str]):
        self._name = name
        self._arguments = arguments

    def __repr__(self) -> str:
        return f'predicate {self._name}({", ".join(self._arguments)});\n'


class DSLBuilder:

    def __init__(self, name, inputs, output):
        self._name = name
        self._inputs = inputs
        self._output = output
        self._enums = []
        self._values = []
        self._functions = []
        self._predicates = []

        self._repr = None  # for caching the string representation

    def add_enum(self, e: DSLEnum):
        self._enums.append(e)

    def add_value(self, v: DSLValue):
        self._values.append(v)

    def add_function(self, f: DSLFunction):
        self._functions.append(f)

    def add_predicate(self, p: DSLPredicate):
        self._predicates.append(p)

    def __repr__(self) -> str:
        if self._repr:
            return self._repr

        else:
            t = "\n".join(map(repr, self._enums)) + '\n'

            if util.get_config().alt_empty_pos:
                t += 'value Empty;\n\n'

            t += "\n".join(map(repr, self._values)) + '\n'

            if not util.get_config().alt_empty_pos:
                t += 'value Empty;\n\n'

            t += f'program {self._name}({", ".join(self._inputs)}) -> {self._output};\n\n'
            t += 'func empty: Empty -> Empty;\n\n'

            t += '\n'.join(map(repr, self._functions)) + '\n'
            t += ''.join(map(repr, self._predicates))

            self._repr = t

            return self._repr
