import random
from abc import ABC
from collections import defaultdict, deque
from functools import singledispatchmethod
from logging import getLogger
from random import sample, choices

from ordered_set import OrderedSet

from . import util
from .tyrell.spec import TyrellSpec
from .util import pairwise, ProgramInfo

logger = getLogger('squares')


class Statistics(ABC):

    @singledispatchmethod
    def update(self, arg, *args):
        raise NotImplementedError

    def choose_production(self, allowed_productions, cube, is_probe, loc):
        raise NotImplementedError


class BottomAlignedLinesStatistics(Statistics):

    def __init__(self, tyrell_specification: TyrellSpec) -> None:
        self.programs = defaultdict(lambda: defaultdict(int))
        self.tyrell_specification = tyrell_specification

    @singledispatchmethod
    def update(self, arg, *args):
        raise NotImplementedError

    @update.register
    def _(self, arg: int, *args):
        for key in self.programs:
            for key2 in self.programs[key]:
                self.programs[key][key2] *= util.get_config().program_weigth_decay_rate ** arg

    @update.register
    def _(self, program: tuple, *args):
        for i, production in enumerate(program):
            if args[0].strict_good:
                self.programs[i - len(program)][production] += util.get_config().strictly_good_program_weight
            else:
                self.programs[i - len(program)][production] += util.get_config().good_program_weight

    def choose_production(self, allowed_productions, cube, is_probe, loc):
        productions = OrderedSet()

        if not is_probe:
            try:
                for production in self.programs[len(cube) - loc]:
                    production = self.tyrell_specification.get_function_production_or_raise(production)
                    if production in map(lambda x: x.head, allowed_productions):
                        productions.append(next(filter(lambda x: x.head == production, allowed_productions)))
            except:
                pass

        for p in sample(allowed_productions, len(allowed_productions)):
            productions.append(p)

        return deque(productions)

    def __repr__(self) -> str:
        result = ''
        for line in sorted(self.programs):
            result += str(line) + ': {\n'
            for production in sorted(self.programs[line], key=lambda x: self.programs[line][x], reverse=True):
                result += f'\t{production.ljust(16)}: {self.programs[line][production]:.3f}\n'
            result += '}\n'
        return result[:-1]


class BigramStatistics(Statistics):

    def __init__(self, tyrell_specification) -> None:
        self.base_scores = defaultdict(int)
        self.bigram_scores = defaultdict(lambda: defaultdict(int))
        self.tyrell_specification = tyrell_specification
        self.productions = [p.name for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]

    def base_probabilities(self, base_productions):
        scores = {production: score for (production, score) in self.base_scores.items() if production in base_productions}
        for prod in base_productions:
            if prod not in scores:
                scores[prod] = util.get_config().smoothing_bias
        return {production: score / sum(scores.values()) for (production, score) in scores.items()}

    def bigram_probabilities(self, base_productions, prior_productions):
        scores = {prod0: {prod1: score for (prod1, score) in submap.items() if prod1 in base_productions} for (prod0, submap) in
                  self.bigram_scores.items() if prod0 in prior_productions}
        for prod0 in prior_productions:
            if prod0 not in scores:
                scores[prod0] = {}
            for prod1 in base_productions:
                if prod1 not in scores[prod0]:
                    scores[prod0][prod1] = util.get_config().smoothing_bias
        return {prod0: {prod1: score / sum(scores[prod0].values()) for (prod1, score) in submap.items()} for (prod0, submap) in
                scores.items()}

    @singledispatchmethod
    def update(self, arg, *args):
        raise NotImplementedError

    @update.register
    def _(self, arg: int, *args):
        for key in self.base_scores:
            self.base_scores[key] *= util.get_config().program_weigth_decay_rate ** arg
        for key0 in self.bigram_scores:
            for key1 in self.bigram_scores[key0]:
                self.bigram_scores[key0][key1] *= util.get_config().program_weigth_decay_rate ** arg

    @update.register
    def _(self, program: tuple, score):
        for bigram in pairwise(program):
            self.bigram_scores[bigram[0]][bigram[1]] += score
        self.base_scores[program[0]] += score
        # for i, prod in enumerate(program):
        #     self.base_scores[prod] += score / ((i + 1) ** 2)

    def choose_production(self, allowed_productions, cube, is_probe, loc):
        if is_probe:
            return random.choice(allowed_productions)
        if len(cube) == 0:
            probabilities = self.base_probabilities(allowed_productions)
            choice = choices(list(probabilities.keys()), list(probabilities.values()))[0]
            if util.get_config().verbosity >= 3:
                logger.debug('First line %s: %s', str(sorted(self.base_probabilities(self.productions).items(), key=lambda x: x[1], reverse=True)), choice)
            return choice
        else:
            probabilities = self.bigram_probabilities(allowed_productions, [cube[-1]])
            choice = choices(list(probabilities[cube[-1]].keys()), list(probabilities[cube[-1]].values()))[0]
            if util.get_config().verbosity >= 3:
                logger.debug('2-gram for %s: %s: %s', cube[-1],
                             str(sorted(self.bigram_probabilities(self.productions, [cube[-1]])[cube[-1]].items(), key=lambda x: x[1], reverse=True)), choice)
            return choice
