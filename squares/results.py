import re
import time
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


start_time = time.time()
specification = None
solution = None
solution_size = None
n_cubes = 0
blocked_cubes = 0
n_attempts = 0
n_rejects = 0
n_fails = 0
n_blocks = 0
exit_code = ExitCode.ERROR
exceeded_max_loc = False
analysis_time = 0
enum_time = 0
init_time = 0
block_time = 0
empty_output = 0


def handle_sigint(signal, stackframe):
    print()
    print_results()
    exit(exit_code)


def beautifier(sql):
    sql = re.sub(r"""`(?=([^"'\\]*(\\.|"([^"'\\]*\\.)*[^"'\\]*"))*[^"']*$)""", '', sql)  # remove backticks if not inside strings
    return sqlparse.format(sql, reindent=True, keyword_case='upper')


def print_results():
    global exit_code
    logger.info('Statistics:')
    if n_cubes:
        logger.info('\tGenerated cubes: %d', n_cubes)
        logger.info('\tBlocked cubes: %d (%f / generated avg.)', blocked_cubes, blocked_cubes / n_cubes if n_cubes else 0)
    logger.info('\tAttempted programs: %d (approx)', n_attempts)
    logger.info('\t\tRejected: %d (approx)', n_rejects)
    logger.info('\t\tFailed: %d (approx)', n_fails)
    logger.info('\t\tEmpty outputs: %d (%.1f%%) (approx)', empty_output, empty_output / n_attempts * 100 if n_attempts else 0)
    logger.info('\tBlocked programs: %d (%f / attempted avg.) (approx)', n_blocks, n_blocks / n_attempts if n_attempts else 0)
    logger.info('\tTotal time spent in enumerator init: %f (approx)', init_time)
    logger.info('\tTotal time spent in enumerator: %f (approx)', enum_time)
    if enum_time != 0:
        logger.info('\t\tEnumerated %f programs/s avg. (just enumeration time)', n_attempts / enum_time)
    logger.info('\t\tEnumerated %f programs/s avg. (overall)', n_attempts / (time.time() - start_time))
    logger.info('\tTotal time spent in evaluation & testing: %f (approx)', analysis_time)
    logger.info('\tTotal time spent blocking cubes/programs: %f (approx)', block_time)

    if solution is not None:
        logger.info(f'Solution found: {solution}')
        logger.info(f'Solution size: {solution_size}')
        util.get_config().cache_ops = True
        interp = interpreter.SquaresInterpreter(specification, True)
        evaluation = interp.eval(solution, specification.tables)
        assert interp.equals(evaluation, 'expected_output')[0]  # this call makes it so that the select() appears in the output

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


def update_stats(attempts, rejects, fails, blocks, emptys, enum_t, analysis_t, init_t, block_t):
    global n_attempts, n_rejects, n_fails, n_blocks, empty_output, enum_time, analysis_time, init_time, block_time
    n_attempts += attempts
    n_rejects += rejects
    n_fails += fails
    n_blocks += blocks
    empty_output += emptys
    enum_time += enum_t
    analysis_time += analysis_t
    init_time += init_t
    block_time += block_t


def increment_cubes():
    global n_cubes
    n_cubes += 1


def store_solution(sol, size: int, optimal: bool):
    global solution, solution_size, exit_code
    solution = sol
    solution_size = size
    exit_code = ExitCode.OK if optimal else ExitCode.NON_OPTIMAL
