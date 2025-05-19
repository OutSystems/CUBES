import random
from abc import ABC
from collections import defaultdict
from functools import singledispatchmethod
from logging import getLogger

from . import util
from .util import pairwise
from . import config

logger = getLogger('squares')


class Statistics(ABC):

    @singledispatchmethod
    def update(self, arg, *args):
        raise NotImplementedError

    def sort_productions(self, allowed_productions, cube, is_probe):
        raise NotImplementedError


class BigramStatistics(Statistics):

    def __init__(self, tyrell_specification) -> None:
        self.tyrell_specification = tyrell_specification
        self.productions = [p.name for p in self.tyrell_specification.get_productions_with_lhs('Table') if p.is_function()]
        self.base_scores = defaultdict(int, {
                'natural_join': 100,
                'natural_join3': 100,
                'natural_join4': 100,
                'summarise': 60,
                'filter': 60,
            })
        self.bigram_scores = defaultdict(lambda: defaultdict(int))

    def base_probabilities(self, base_productions):
        scores = {production: score for (production, score) in self.base_scores.items() if production in base_productions}
        for prod in base_productions:
            if prod not in scores or scores[prod] <= 0:
                scores[prod] = util.get_config().smoothing_bias
        return {production: score / sum(scores.values()) for (production, score) in scores.items()}

    def bigram_probabilities(self, base_productions, prior_productions):
        scores = {prod0: {prod1: score for (prod1, score) in submap.items() if prod1 in base_productions} for (prod0, submap) in
                  self.bigram_scores.items() if prod0 in prior_productions}
        for prod0 in prior_productions:
            if prod0 not in scores:
                scores[prod0] = {}
            for prod1 in base_productions:
                if prod1 not in scores[prod0] or scores[prod0][prod1] <= 0:
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
        if program:
            self.base_scores[program[0]] += score
        if util.get_config().score_extra_update_reordering:
            for i, prod in enumerate(program):
                self.base_scores[prod] += score / ((i + 1) ** 2)

    def sort_productions(self, allowed_productions, cube, is_probe):
        if is_probe:
            return {prod: 1 for prod in allowed_productions}
        if len(cube) == 0:
            probabilities = self.base_probabilities(allowed_productions)
            if util.get_config().verbosity >= 4:
                logger.debug('First line %s',
                             str(sorted(self.base_probabilities(self.productions).items(), key=lambda x: x[1], reverse=True)))
            return probabilities
        else:
            probabilities = self.bigram_probabilities(allowed_productions, [cube[-1]])
            if util.get_config().verbosity >= 4:
                logger.debug('2-gram for %s: %s', cube[-1],
                             str(sorted(self.bigram_probabilities(self.productions, [cube[-1]])[cube[-1]].items(), key=lambda x: x[1],
                                        reverse=True)))
            return probabilities[cube[-1]]
