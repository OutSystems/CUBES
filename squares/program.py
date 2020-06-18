from logging import getLogger
from typing import List, Any

from rpy2 import robjects

from . import util
from .tyrell.interpreter import Interpreter, InterpreterError
from .tyrell.spec import Production, EnumProduction, ParamProduction, FunctionProduction, LineProduction

logger = getLogger('squares')


class LimitReachedException(Exception):
    pass


def make_argument(arg: Production):
    if isinstance(arg, EnumProduction):
        return arg.rhs
    elif isinstance(arg, ParamProduction):
        return ProgramInput(arg.rhs)
    elif isinstance(arg, FunctionProduction):
        return None
    elif isinstance(arg, LineProduction):
        return PreviousLine(arg.line)
    else:
        raise NotImplementedError


class ProgramInput:

    def __init__(self, index: int) -> None:
        self.index = index

    def __repr__(self) -> str:
        return f'input{self.index}'


class PreviousLine:

    def __init__(self, index: int) -> None:
        self.index = index

    def __repr__(self) -> str:
        return f'line{self.index}'


class Line:

    def __init__(self, production, arguments):
        self.production = production
        self.arguments = tuple(make_argument(arg) for arg in arguments)

    def __repr__(self) -> str:
        return f'{self.production.name}({", ".join(map(repr, filter(lambda arg: arg is not None, self.arguments)))})'


Program = List[Line]


class LineInterpreter(Interpreter):

    def eval(self, prog: Program, inputs: List[Any]) -> Any:
        self.program = ''
        vars = []
        for line in prog:
            method_name = self._eval_method_name(line.production.name)
            method = getattr(self, method_name)
            new_var = method(tuple(self.transform_arg(arg, inputs, vars) for arg in line.arguments), self.filled_program(prog, line))
            vars.append(new_var)
        return vars[-1]

    def transform_arg(self, arg, inputs, vars):
        if isinstance(arg, ProgramInput):
            return inputs[arg.index]
        elif isinstance(arg, PreviousLine):
            return vars[arg.index]
        else:
            return arg

    def transform_arg_recursive(self, prog, limit, arg):
        if isinstance(arg, PreviousLine):
            return self.filled_program(prog, prog[arg.index], limit - 1)
        elif isinstance(arg, ProgramInput):
            return arg.index
        else:
            return arg

    def filled_program(self, prog, line, limit=2):
        if limit <= 0:
            return None

        args = tuple(self.transform_arg_recursive(prog, limit, arg) for arg in line.arguments if arg is not None)
        if not any(map(lambda arg: arg is None, args)):
            return line.production.name, *args
        else:
            return None

    @staticmethod
    def _eval_method_name(name):
        return 'eval_' + name
