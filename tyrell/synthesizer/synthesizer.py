from abc import ABC, abstractmethod
from typing import Any
from ..interpreter import InterpreterError
from ..enumerator import Enumerator
from ..decider import Decider
from ..dsl import Node
from ..logger import get_logger
import time

logger = get_logger('tyrell.synthesizer')


class Synthesizer(ABC):

    def __init__(self, enumerator: Enumerator, decider: Decider):
        self._enumerator = enumerator
        self._decider = decider

    @property
    def enumerator(self):
        return self._enumerator

    @property
    def decider(self):
        return self._decider

    def synthesize(self):
        '''
        A convenient method to enumerate ASTs until the result passes the analysis.
        Returns the synthesized program, or `None` if the synthesis failed.
        '''
        start_time = time.time()
        num_attempts = 0
        num_rejected = 0
        num_failed = 0
        prog = self._enumerator.next()
        while prog is not None:
            num_attempts += 1
            if num_attempts % 500 == 0:
                self._enumerator.closeLattices()
                logger.info('Attempts: %d. Rejected: %d. Failed: %d.', num_attempts, num_rejected, num_failed)
            # logger.debug('Attempt %s: %s', str(num_attempts), str(prog))
            try:
                res = self._decider.analyze(prog)
                if res.is_ok():
                    self._enumerator.closeLattices()
                    logger.info('Program accepted after %d attempts (%d rejected, %d failed)', num_attempts, num_rejected, num_failed)
                    logger.debug('Total Time: %f', time.time() - start_time)
                    return prog

                else:
                    num_rejected += 1
                    info = res.why()
                    # logger.debug('Attempt {}: Program rejected. Reason: {}'.format(num_attempts, info))
                    self._enumerator.update(info)
                    prog = self._enumerator.next()

            except InterpreterError as e:
                num_failed += 1
                info = self._decider.analyze_interpreter_error(e)
                # logger.debug('Attempt {}: Interpreter failed. Reason: {}'.format(num_attempts, e))
                self._enumerator.update(info)
                prog = self._enumerator.next()

        logger.debug('Enumerator is exhausted after %d attempts', num_attempts)
        logger.debug('Total Time: %f', time.time() - start_time)
        self._enumerator.closeLattices()
        return None
