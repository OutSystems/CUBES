from logging import getLogger
from typing import List, Any

from rpy2 import robjects

from . import util
from .tyrell.interpreter import Interpreter, InterpreterError
from .tyrell.spec import Production, EnumProduction, ParamProduction, FunctionProduction, LineProduction

logger = getLogger('squares')


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
        return f'{self.production.name}({", ".join(map(repr, self.arguments))})'


Program = List[Line]


class LineInterpreter(Interpreter):

    def eval(self, prog: Program, inputs: List[Any]) -> Any:
        self.program = ''
        vars = []
        for line in prog:
            new_var = util.get_fresh_name()
            method_name = self._eval_method_name(line.production.name)
            method = getattr(self, method_name)
            method(new_var, tuple(self.transform_arg(arg, inputs, vars) for arg in line.arguments))
            vars.append(new_var)
        try:
            robjects.r(self.program)
        except Exception as e:
            logger.error("Error while evaluating program %s", str(prog))
            logger.error("%s", str(e))
            raise InterpreterError(e)
        return vars[-1]

    def transform_arg(self, arg, inputs, vars):
        if isinstance(arg, ProgramInput):
            return inputs[arg.index]
        elif isinstance(arg, PreviousLine):
            return vars[arg.index]
        else:
            return arg

    @staticmethod
    def _eval_method_name(name):
        return 'eval_' + name
