import re
import sys
from enum import IntEnum
from logging import getLogger

import os
import sqlparse
from rpy2 import robjects

from . import util
from .dsl import interpreter
from .util import Singleton

logger = getLogger('squares')


def handler_subprocess(signal, stackframe):
    logger.debug('Time spent in equality testing: %f', ResultsHolder().equality_time)
    logger.debug('Time spent in analysis: %f', ResultsHolder().analysis_time)
    logger.debug('Time spent in enumerator: %f', ResultsHolder().enum_time)
    logger.debug('Time spent in enumerator init: %f', ResultsHolder().init_time)
    logger.debug('Enumerarted %f programs/second', ResultsHolder().n_attempts / ResultsHolder().enum_time)
    os._exit(os.EX_OK)


class ExitCode(IntEnum):
    OK = 0
    NON_OPTIMAL = 3
    ERROR = 1
    SQL_FAILED = 2
    SQL_FAILED_NON_OPTIMAL = 4
    END_SEARCH_SPACE = 5


def beautifier(sql):
    sql = re.sub(r"""`(?=([^"'\\]*(\\.|"([^"'\\]*\\.)*[^"'\\]*"))*[^"']*$)""", '', sql)  # remove backticks if not inside strings
    return sqlparse.format(sql, reindent=True, keyword_case='upper')


class ResultsHolder(metaclass=Singleton):

    def __init__(self) -> None:
        self.specification = None
        self.solution = None
        self.solution_size = None
        self.blacklist = None
        self.n_cubes = 0
        self.n_attempts = 0
        self.n_rejects = 0
        self.n_fails = 0
        self.exit_code = ExitCode.ERROR
        self.exceeded_max_loc = False
        self.equality_time = 0
        self.analysis_time = 0
        self.enum_time = 0
        self.init_time = 0

    def print(self):
        logger.info('Statistics:')
        if self.n_cubes:
            logger.info('\tGenerated cubes: %d', self.n_cubes)
        logger.info('\tAttempted programs: %d', self.n_attempts)
        logger.info('\t\tRejected: %d', self.n_rejects)
        logger.info('\t\tFailed: %d', self.n_fails)
        if self.blacklist:
            logger.info('\tBlacklist clauses: %d', sum(map(len, self.blacklist.values())))

        if self.solution is not None:
            logger.info(f'Solution found: {self.solution}')
            logger.info(f'Solution size: {self.solution_size}')
            util.get_config().cache_ops = True
            interp = interpreter.SquaresInterpreter(self.specification, True)
            evaluation = interp.eval(self.solution, self.specification.tables)
            assert interp.equals(evaluation, 'expected_output')  # this call makes it so that the select() appears in the output

            try:
                program = self.specification.r_init + interp.final_program
                robjects.r(program)
                sql_query = robjects.r(f'sink(); sql_render(out, bare_identifier_ok=T)')
            except:
                logger.error('Error while trying to convert R code to SQL.')
                sql_query = None
                self.exit_code = ExitCode.SQL_FAILED if self.exit_code != ExitCode.NON_OPTIMAL else ExitCode.SQL_FAILED_NON_OPTIMAL

            print()
            if util.get_config().print_r:
                pass
                print("------------------------------------- R Solution ---------------------------------------\n")
                print(self.specification.r_init + '\n' + interp.final_program)

            if sql_query is not None:
                print()
                print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
                print(beautifier(str(sql_query)[6:]))
            else:
                print('Failed to generate SQL query')
        else:
            if self.exceeded_max_loc:
                self.exit_code = ExitCode.END_SEARCH_SPACE

            print("No solution found")

    def increment_attempts(self, attempts, rejects, fails):
        self.n_attempts += attempts
        self.n_rejects += rejects
        self.n_fails += fails

    def increment_cubes(self):
        self.n_cubes += 1

    def store_solution(self, solution, size: int, optimal: bool):
        self.solution = solution
        self.solution_size = size
        self.exit_code = ExitCode.OK if optimal else ExitCode.NON_OPTIMAL
