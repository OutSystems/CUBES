from enum import Enum
from typing import NamedTuple, List, Any

from rpy2 import robjects

from .tyrell.decider import ExampleDecider, ok, bad


class RowNumberInfo(Enum):
    MORE_ROWS = 1
    LESS_ROWS = 2


class RejectionInfo(NamedTuple):
    row_info: RowNumberInfo
    score: float


class LinesDecider(ExampleDecider):

    def get_failed_examples(self, prog) -> List[Any]:
        fails = []
        for example in self._examples:
            output = self.interpreter.eval(prog, example.input)
            result = self.interpreter.equals(output, example.output, prog)
            if not result[0]:
                fails.append((example, output, result[1]))
        return fails

    def analyze(self, prog):
        failed_examples = self.get_failed_examples(prog)
        if len(failed_examples) == 0:
            return ok()
        else:
            for example, output, score in failed_examples:
                actual_n = int(robjects.r(f'nrow({output})')[0])
                expected_n = int(robjects.r(f'nrow({example.output})')[0])
                if actual_n > expected_n:
                    return bad(RejectionInfo(RowNumberInfo.LESS_ROWS, score))
                if actual_n < expected_n:
                    return bad(RejectionInfo(RowNumberInfo.MORE_ROWS, score))
            return bad()
