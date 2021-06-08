from squares.tyrell.interpreter import InterpreterError


class SquaresException(Exception):

    def __init__(self, *args: object, exit_status=1) -> None:
        super().__init__(*args)
        self.exit_status = exit_status


class REvaluationError(InterpreterError):

    def __init__(self, *args: object) -> None:
        super().__init__(*args)