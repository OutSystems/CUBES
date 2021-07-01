from logging import getLogger

from squares.disambiguator import Disambiguator
from squares.tyrell.synthesizer.synthesizer import AbstractSynthesizer

logger = getLogger('squares')


class DisambiguatingSynthesizer(AbstractSynthesizer):

    def __init__(self, synthesizer: AbstractSynthesizer, disambiguator: Disambiguator):
        self.synthesizer = synthesizer
        self.disambiguator = disambiguator

    def synthesize(self, top_programs=None, enum_all=None):
        prog = None

        for program, loc in self.synthesizer.synthesize(enum_all=True):
            if program is None:
                continue

            if prog is None:
                logger.info('First solution found. Waiting for next solution to perform disambiguation.')
                prog = program
                continue

            prog = self.disambiguator.disambiguate(prog, program)

        yield prog
