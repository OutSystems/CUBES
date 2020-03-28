from squares.tyrell.interpreter import InterpreterError


class SquaresException(Exception):

    def __init__(self, *args: object) -> None:
        super().__init__(*args)


class REvaluationError(InterpreterError):

    def __init__(self, *args: object) -> None:
        super().__init__(*args)