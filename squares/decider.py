from .tyrell.decider import ExampleDecider, bad, ok, ExampleConstraintPruningDecider
from .tyrell.decider.example_constraint_pruning import BlameFinder


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


class InstrumentedPruningDecider(ExampleConstraintPruningDecider):

    def analyze(self, prog, roots=None):
        blame_finder = BlameFinder(self.interpreter, prog)
        return blame_finder.process_examples(self.examples, lambda x, y: self.interpreter.equals(x, y, roots))
