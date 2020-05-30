import os
import re
from enum import IntEnum
from logging import getLogger

import sqlparse
from rpy2 import robjects

from . import util
from .dsl import interpreter

logger = getLogger('squares')


class ExitCode(IntEnum):
    OK = 0
    NON_OPTIMAL = 3
    ERROR = 1
    SQL_FAILED = 2
    SQL_FAILED_NON_OPTIMAL = 4
    END_SEARCH_SPACE = 5


specification = None
solution = None
solution_size = None
blacklist = None
n_cubes = 0
blocked_cubes = 0
n_attempts = 0
n_rejects = 0
n_fails = 0
exit_code = ExitCode.ERROR
exceeded_max_loc = False
equality_time = 0
analysis_time = 0
enum_time = 0
init_time = 0


def handler_subprocess(signal, stackframe):
    logger.debug('Time spent in equality testing: %f', equality_time)
    logger.debug('Time spent in analysis: %f', analysis_time)
    logger.debug('Time spent in enumerator: %f', enum_time)
    logger.debug('Time spent in enumerator init: %f', init_time)
    if enum_time != 0:
        logger.debug('Enumerated %f programs/second', n_attempts / enum_time)
    os._exit(os.EX_OK)


def beautifier(sql):
    sql = re.sub(r"""`(?=([^"'\\]*(\\.|"([^"'\\]*\\.)*[^"'\\]*"))*[^"']*$)""", '', sql)  # remove backticks if not inside strings
    return sqlparse.format(sql, reindent=True, keyword_case='upper')


def print_results():
    global exit_code
    logger.info('Statistics:')
    if n_cubes:
        logger.info('\tGenerated cubes: %d', n_cubes)
        logger.info('\tBlocked cubes: %d', blocked_cubes)
    logger.info('\tAttempted programs: %d', n_attempts)
    logger.info('\t\tRejected: %d', n_rejects)
    logger.info('\t\tFailed: %d', n_fails)
    if blacklist:
        logger.info('\tBlacklist clauses: %d', sum(map(len, blacklist.values())))

    if solution is not None:
        logger.info(f'Solution found: {solution}')
        logger.info(f'Solution size: {solution_size}')
        util.get_config().cache_ops = True
        interp = interpreter.SquaresInterpreter(specification, True)
        evaluation = interp.eval(solution, specification.tables)
        assert interp.equals(evaluation, 'expected_output')  # this call makes it so that the select() appears in the output

        try:
            program = specification.r_init + interp.program
            robjects.r(program)
            sql_query = robjects.r(f'sink(); sql_render(out, bare_identifier_ok=T)')
        except:
            logger.error('Error while trying to convert R code to SQL.')
            sql_query = None
            exit_code = ExitCode.SQL_FAILED if exit_code != ExitCode.NON_OPTIMAL else ExitCode.SQL_FAILED_NON_OPTIMAL

        print()
        if util.get_config().print_r:
            pass
            print("------------------------------------- R Solution ---------------------------------------\n")
            print(specification.r_init + '\n' + interp.program)

        if sql_query is not None:
            print()
            print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
            print(beautifier(str(sql_query)[6:]))
        else:
            print('Failed to generate SQL query')
    else:
        if exceeded_max_loc:
            exit_code = ExitCode.END_SEARCH_SPACE

        print("No solution found")


def increment_attempts(attempts, rejects, fails):
    global n_attempts, n_rejects, n_fails
    n_attempts += attempts
    n_rejects += rejects
    n_fails += fails


def increment_cubes():
    global n_cubes
    n_cubes += 1


def store_solution(sol, size: int, optimal: bool):
    global solution, solution_size, exit_code
    solution = sol
    solution_size = size
    exit_code = ExitCode.OK if optimal else ExitCode.NON_OPTIMAL
