from abc import ABC

from squares.dsl.specification import Specification


class Disambiguator(ABC):

    def disambiguate(self, prog1, prog2):
        raise NotImplementedError


class FuzzingDisambiguator(Disambiguator):

    def __init__(self, specification: Specification, max_rounds: int):
        self.specification = specification
        self.max_rounds = max_rounds

    def disambiguate(self, prog1, prog2):


        raise NotImplementedError
