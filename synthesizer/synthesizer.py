from abc import ABC, abstractmethod
from typing import Any
from .result import Result
from interpreter import Interpreter
from enumerator import Enumerator
from dsl import Node
import logger

logger = logger.get('tyrell.synthesizer')


class Synthesizer(ABC):

    _enumerator: Enumerator
    _interpreter: Interpreter

    @abstractmethod
    def __init__(self, enumerator: Enumerator, interpreter: Interpreter):
        self._enumerator = enumerator
        self._interpreter = interpreter

    @abstractmethod
    def analyze(self, ast: Node) -> Result:
        '''
        The main API of this class.
        It is expected to analyze the given AST and check if it is valid. If not, optionally returns why, which is used to update the enumerator.
        '''
        raise NotImplementedError

    @property
    def enumerator(self):
        return self._enumerator

    @property
    def interpreter(self):
        return self._interpreter

    def synthesize(self):
        '''
        A convenient method to enumerate ASTs until the result passes the analysis.
        Returns the synthesized program, or `None` if the synthesis failed.
        '''
        num_attempts = 0
        prog = self._enumerator.next()
        while prog is not None:
            num_attempts += 1
            logger.debug('Enumerator generated: {}'.format(prog))
            res = self.analyze(prog)
            if res.is_ok():
                logger.debug(
                    'Program accepted after {} attempts'.format(num_attempts))
                return prog
            else:
                info = res.why()
                logger.debug('Program rejected. Reason: {}'.format(info))
                self._enumerator.update(info)
                prog = self._enumerator.next()
        logger.debug(
            'Enumerator is exhausted after {} attempts'.format(num_attempts))
        return None