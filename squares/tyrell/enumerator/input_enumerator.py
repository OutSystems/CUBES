from typing import Any, Optional

from squares.dsl.specification import Specification
from squares.tyrell.dsl import Node
from squares.tyrell.enumerator import Enumerator
from squares.tyrell.spec import TyrellSpec


class InputEnumerator(Enumerator):

    def __init__(self, tyrell_spec: TyrellSpec, spec: Specification):
        super().__init__()
        self.inputs = spec.input_table_names.copy()

    def next(self) -> Optional[Node]:
        if self.inputs:
            r = self.inputs[0]
            self.inputs = self.inputs[1:]
            return r
        return None

    def update(self, info: Any = None) -> None:
        return 1

