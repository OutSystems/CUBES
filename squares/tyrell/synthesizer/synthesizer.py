import time
from abc import ABC
from logging import getLogger

from ..decider import Decider
from ..enumerator import Enumerator
from ..interpreter import InterpreterError
from ... import results

logger = getLogger('tyrell.synthesizer')


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
        start = time.time()
        prog = self._enumerator.next()
        results.enum_time += time.time() - start
        while prog is not None:
            results.n_attempts += 1
            num_attempts += 1
            if num_attempts % 500 == 0:
                self._enumerator.close_lattices()
                logger.info('Attempts: %d. Rejected: %d. Failed: %d.', num_attempts, num_rejected, num_failed)
            # logger.debug('Attempt %s: %s', str(num_attempts), str(prog))
            try:
                start = time.time()
                res = self._decider.analyze(prog)
                results.analysis_time += time.time() - start
                if res.is_ok():
                    self._enumerator.close_lattices()
                    logger.info('Program accepted after %d attempts (%d rejected, %d failed)', num_attempts,
                                num_rejected, num_failed)
                    logger.debug('Total Time: %f', time.time() - start_time)
                    return prog

                else:
                    num_rejected += 1
                    results.n_rejects += 1
                    info = res.why()
                    # logger.debug('Attempt {}: Program rejected. Reason: {}'.format(num_attempts, info))
                    self._enumerator.update(info)
                    start = time.time()
                    prog = self._enumerator.next()
                    results.enum_time += time.time() - start

            except InterpreterError as e:
                num_failed += 1
                results.n_fails += 1
                info = self._decider.analyze_interpreter_error(e)
                # logger.debug('Attempt {}: Interpreter failed. Reason: {}'.format(num_attempts, e))
                self._enumerator.update(info)
                start = time.time()
                prog = self._enumerator.next()
                results.enum_time += time.time() - start

        logger.debug('Enumerator is exhausted after %d attempts', num_attempts)
        logger.debug('Total Time: %f', time.time() - start_time)
        self._enumerator.close_lattices()
        return None
