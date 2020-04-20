from squares.tyrell.decider import ExampleDecider, bad, ok


class InstrumentedDecider(ExampleDecider):
    def has_failed_examples(self, prog, roots=None):
        return any(
            not self.interpreter.equals(
                self.interpreter.eval(prog, x.input), x.output, roots)
            for x in self.examples
        )

    def analyze(self, prog, roots=None):
        if self.has_failed_examples(prog, roots):
            return bad()
        else:
            return ok()
