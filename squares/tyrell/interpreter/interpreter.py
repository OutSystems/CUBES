from abc import ABC, abstractmethod
from typing import Callable, Iterable, List, Any

from .error import AssertionViolation
from ..dsl import Node


class Interpreter(ABC):

    @abstractmethod
    def eval(self, prog: Node, inputs: List[Any]) -> Any:
        '''
        Evaluate a DSL `prog` on input `inputs`. The output is returned.
        This is a covenient wrapper over `eval_step` that repeatedly invoke the generator until we get the final result.
        '''
        raise NotImplementedError

    @abstractmethod
    def equals(self, actual, expected, *args) -> bool:
        """
        Tests if a given output is equal to the expected output
        """
        raise NotImplementedError

    def assertArg(
            self,
            node: Node,
            args: List[Any],
            index: int,
            cond: Callable[[Any], bool],
            capture_indices: Iterable[int] = []) -> None:
        '''
        Check the value of `index`-th argument against `cond`. If `cond` is not met, raise AssertionViolation.
        If the assertion refers to the value(s) of other arugments, the indices of those arguments should be passed to `capture_indices` as a list of integer. By default, this list is empty.
        '''
        if node.is_leaf():
            raise RuntimeError(
                'assertArg() cannot be called within a leaf node: {}'.format(node))
        if not cond(args[index]):
            raise AssertionViolation(node, index, cond, capture_indices)
