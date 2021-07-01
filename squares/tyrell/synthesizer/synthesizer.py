import time
from abc import ABC
from logging import getLogger

from ..decider import Decider
from ..enumerator import Enumerator
from ..interpreter import InterpreterError
from ... import results, util
from ...dsl.interpreter import RedudantError

logger = getLogger('squares.synthesizer')


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

    def synthesize(self, n=1, enum_all=False):
        '''
        A convenient method to enumerate ASTs until the result passes the analysis.
        Returns the synthesized program, or `None` if the synthesis failed.
        '''
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
        n_progs = 0
        while prog is not None and (n_progs < n or enum_all):
            # logger.debug('Testing program %s', prog)
            total_attempts += 1
            attempts += 1
            if attempts == 50:
                util.get_program_queue().put(
                    (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0,
                     block_time, results.redundant_lines, results.cache_hit, results.cache_miss))
                attempts = 0
                rejected = 0
                failed = 0
                blocked = 0
                results.empty_output = 0
                enum_time = 0
                analysis_time = 0
                block_time = 0
                results.redundant_lines = 0

            try:
                start = time.time()
                res = self.decider.analyze(prog)
                analysis_time += time.time() - start
                if res.is_ok():
                    util.get_program_queue().put(
                        (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0,
                         block_time, results.redundant_lines, results.cache_hit, results.cache_miss))
                    results.empty_output = 0
                    results.redundant_lines = 0

                    if n == 1 and not enum_all:
                        self.enumerator.update(None)
                        yield prog, attempts
                        return
                    else:
                        yield prog, attempts
                        info = None
                        n_progs += 1

                else:
                    rejected += 1
                    info = res.why()

            except RedudantError as e:
                info = None

            except InterpreterError as e:
                # logger.error('Failed program %s', str(prog))
                # logger.error('%s', str(e))
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
            (util.Message.DEBUG_STATS, attempts, rejected, failed, blocked, results.empty_output, enum_time, analysis_time, 0, block_time, results.redundant_lines,
             results.cache_hit, results.cache_miss))
        results.empty_output = 0
        results.redundant_lines = 0
        if n == 1 and not enum_all:
            yield None, total_attempts
            return
        else:
            yield None, total_attempts
