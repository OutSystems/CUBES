from .tyrell.decider import ExampleDecider, bad, ok, ExampleConstraintPruningDecider
from .tyrell.decider.example_constraint_pruning import BlameFinder


class LinesDecider(ExampleDecider):
    def has_failed_examples(self, prog):
        return any(
            not self.interpreter.equals(
                self.interpreter.eval(prog, x.input), x.output, prog)
            for x in self.examples
            )

    def analyze(self, prog, roots=None):
        if self.has_failed_examples(prog):
            return bad()
        else:
            return ok()


class InstrumentedPruningDecider(ExampleConstraintPruningDecider):

    def analyze(self, prog, roots=None):
        blame_finder = BlameFinder(self.interpreter, prog)
        return blame_finder.process_examples(self.examples, lambda x, y: self.interpreter.equals(x, y, roots))
