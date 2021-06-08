from enum import Enum
from logging import getLogger
from typing import NamedTuple, List, Any

from rpy2 import robjects

from .tyrell.decider import ExampleDecider, ok, bad


logger = getLogger('squares.decider')


class RowNumberInfo(Enum):
    MORE_ROWS = 1
    LESS_ROWS = 2
    UNKNOWN = 3


class RejectionInfo(NamedTuple):
    row_info: RowNumberInfo
    score: float


class LinesDecider(ExampleDecider):

    def get_failed_examples(self, prog) -> List[Any]:
        fails = []
        for example in self._examples:
            output = self.interpreter.eval(prog, example.input)
            result = self.interpreter.equals(output, example.output, prog)
            # for var in self.interpreter.current_vars:
            #     robjects.r(f'rm({var})')
            # self.interpreter.current_vars = set()
            if not result[0]:
                fails.append((example, output, result[1], result[2]))
        return fails

    def analyze(self, prog):
        failed_examples = self.get_failed_examples(prog)
        if len(failed_examples) == 0:
            return ok()
        else:
            for example, output, score, row_info in failed_examples:
                return bad(RejectionInfo(row_info, score))
            return bad()
