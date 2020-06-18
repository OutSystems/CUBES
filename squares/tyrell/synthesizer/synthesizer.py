import cProfile
import time
from abc import ABC
from logging import getLogger

from ..decider import Decider
from ..enumerator import Enumerator
from ..interpreter import InterpreterError
from ... import results, util

logger = getLogger('squares.synthesizer')

# counter = 0


class AbstractSynthesizer(ABC):

    def synthesize(self):
        raise NotImplementedError


class Synthesizer(AbstractSynthesizer):

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
        global counter
        total_attempts = 0
        attempts = 0
        rejected = 0
        failed = 0
        blocked = 0
        enum_time = 0
        analysis_time = 0
        block_time = 0
        start = time.time()
        prog = self.enumerator.next()
        enum_time += time.time() - start
        # pr = cProfile.Profile()
        while prog is not None:
            # logger.debug('Testing program %s', prog)
            total_attempts += 1
            attempts += 1
            if attempts == 50:
                util.get_program_queue().put(
                    (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0,
                     block_time))
                attempts = 0
                rejected = 0
                failed = 0
                blocked = 0
                results.empty_output = 0
                enum_time = 0
                analysis_time = 0
                block_time = 0

            try:
                start = time.time()
                res = self.decider.analyze(prog)
                analysis_time += time.time() - start
                if res.is_ok():
                    util.get_program_queue().put(
                        (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0,
                         block_time))
                    results.empty_output = 0
                    # pr.dump_stats(f'profiles/{counter}.cProfile')
                    # counter += 1
                    return prog, attempts

                else:
                    rejected += 1
                    info = res.why()

            except InterpreterError as e:
                logger.error('Failed program %s', str(prog))
                logger.error('%s', str(e))
                failed += 1
                info = self.decider.analyze_interpreter_error(e)

            start = time.time()
            # pr.enable()
            blocked_ = self.enumerator.update(info)
            # pr.disable()
            blocked += blocked_
            total_attempts += blocked_
            block_time += time.time() - start
            start = time.time()
            prog = self.enumerator.next()
            enum_time += time.time() - start

        util.get_program_queue().put(
            (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0, block_time))
        results.empty_output = 0
        # pr.dump_stats(f'profiles/{counter}.cProfile')
        # counter += 1
        return None, total_attempts
