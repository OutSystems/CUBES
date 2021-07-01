import csv
import glob
import logging
import multiprocessing
import os
import pathlib
import random
import re
import resource
import signal
import sys
from collections import Counter, defaultdict
from contextlib import contextmanager
from enum import Enum
from itertools import permutations, count, repeat
from logging import getLogger, FileHandler
from concurrent.futures import TimeoutError

import chromalog
import pandas
import rpy2
import sqlalchemy
from pebble import ProcessPool
from TestSuiteEval.fuzz import fuzz

from squares import util
from squares.config import Config
from squares.dsl.table import Table
from squares.tyrell.logger import TyrellLogFormatter
from squares.util import parse_specification, create_argparser


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print


@contextmanager
def suppress_stdout():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout


getLogger('squares').setLevel(50)
logger = getLogger('fuzzer')

formatter = TyrellLogFormatter(fmt='[%(seconds)s][%(processName)s][%(levelname)s] %(message)s')
handler = chromalog.ColorizingStreamHandler()
handler.setFormatter(formatter)
logger.addHandler(handler)

parser = create_argparser(all_inputs=True)
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-m', default=65536, type=int, help='memout')
parser.add_argument('-p', default=1, type=int, help='#processes')
parser.add_argument('--timeout', default=10, type=int, help='#processes')
parser.add_argument('--run', help='run')
parser.add_argument('--ratsql', action='store_true')
parser.add_argument('--smbop', action='store_true')
parser.add_argument('--force-pool', action='store_true')
parser.add_argument('--check-gt', action='store_true')
parser.add_argument('--save-alt', action='store_true')
parser.add_argument('--fuzzies', type=int, default=16)

args = parser.parse_args()

if args.run:
    run = args.run
elif args.ratsql:
    run = 'ratsql'
elif args.smbop:
    run = 'smbop'
else:
    raise ValueError()

is_gt = run == 'gt'
is_patsql = run.startswith('patsql')
is_scythe = run.startswith('scythe')
is_squares = run.startswith('squares')
is_cubes = not is_patsql and not is_scythe and not is_squares
is_from_spec = run == 'ratsql' or run == 'smbop'

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'(?:------------------------------------- R Solution ---------------------------------------)|(?:All solutions of length \d+ found)|(?:Timeout reached)'
scythe_sep = r'\[Query No\.\d]==============================='

random.seed(args.seed)
seed = random.randrange(2 ** 16)

config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable)
util.store_config(config)

fuzzies = args.fuzzies


class ReturnCodes(Enum):
    CORRECT = 1
    WRONG = 0
    NO_SOLUTION = -2
    NO_LOG = -3
    FUZZER_ERROR = -4
    GT_MISMATCH = -5
    NO_DATABASE = -6
    GT_ERROR = -8
    FUZZIED_ERROR = -9
    NO_GT = -10
    BASE_ERROR = -12


def shuffled(lst):
    lst = list(lst)
    random.shuffle(lst)
    return lst


def removesuffix(str1, str2):
    if str1.endswith(str2):
        return str1[:-len(str2)]
    return str1


def sanitize_sql(result_sql, spec_in):
    if is_from_spec:
        return result_sql

    if not is_patsql:
        result_sql = result_sql.replace('df_', '')
        # result_sql = re.sub(r'\bOVER \(\)', '', result_sql, re.IGNORECASE)
        # print(result_sql)

        if is_squares:
            result_sql = '\n'.join(filter(lambda x: not x.startswith('Joining, by'), result_sql.split('\n')))

        if is_cubes:
            for table_name in map(lambda x: pathlib.Path(x).stem, spec_in['inputs']):
                result_sql = re.sub(table_name.replace('-', '[-_]'), table_name, result_sql)

        if not is_scythe:
            for i, input in enumerate(spec_in['inputs']):
                result_sql = result_sql.replace(f'input{i}', pathlib.Path(input).stem)
        else:
            for scythe_name, table_name in zip(
                    sorted(set(re.findall(r'\binput[0-9]?\b', result_sql))),
                    map(lambda x: pathlib.Path(x).stem, spec_in['inputs'])):
                result_sql = re.sub(fr'\b{scythe_name}\b', table_name, result_sql)

    else:
        for i, input in enumerate(spec_in['inputs']):
            result_sql = result_sql.replace(f'input{i + 1}', pathlib.Path(input).stem)

    if is_scythe:
        result_sql = re.sub(r'(?:As\s+t[0-9]+\s+)+(As\s+t[0-9]+)', r'\1', result_sql).replace(', From', ' From')
        result_sql = re.sub(r'\A\(((?:.|\n)*)\)\s+As\s+t[0-9]+\s*;?\s*\Z', r'\1', result_sql).replace('Count_distinct(',
                                                                                                      'Count(distinct ')

    return result_sql


def execute(connection, sql):
    df = pandas.read_sql_query(sql, connection)
    df = df.sort_values(by=sorted(list(df.columns))).reset_index(drop=True)
    df.columns = df.columns.str.lower()
    return df


def check(connection, expected_sql, actual_sql, instance='', verbose=False, base=False, n: int = -1):
    if isinstance(expected_sql, str):
        try:
            expected_df = execute(connection, expected_sql)
        except Exception as e:
            if verbose:
                logger.error('Error while executing ground truth for instance %s (%d)', instance, n)
                logger.error('\n%s', str(e))
            return ReturnCodes.GT_ERROR
    else:
        expected_df = expected_sql

    try:
        actual_df = execute(connection, actual_sql)
    except Exception as e:
        if verbose:
            logger.error('Error while executing solution for instance %s (%d)', instance, n)
            logger.error('\n%s', str(e))
        return ReturnCodes.BASE_ERROR if base else ReturnCodes.FUZZIED_ERROR

    expected_df = expected_df.sort_values(by=sorted(list(expected_df.columns))).reset_index(drop=True)

    for perm in permutations(list(expected_df.columns)):
        actual_df_try = actual_df.copy()
        actual_df_try.columns = perm
        try:
            pandas.testing.assert_frame_equal(expected_df, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=True)
            return ReturnCodes.CORRECT
        except:
            actual_df_try = actual_df_try.sort_values(by=sorted(list(actual_df_try.columns))).reset_index(drop=True)
            try:
                pandas.testing.assert_frame_equal(expected_df, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=True)
                return ReturnCodes.CORRECT
            except:
                pass

    if verbose:
        if base:
            logger.error('Wrong output for base solution in instance %s (%d)', instance, n)
        else:
            logger.warning('Wrong output for fuzzied solution in instance %s (%d)', instance, n)

        logger.warning('\tEXPECTED OUTPUT ====')
        if isinstance(expected_sql, str):
            logger.warning('\n\t\t%s', str(expected_sql).replace('\n', '\n\t\t'))
        logger.warning('\n\t%s', str(expected_df).replace('\n', '\n\t'))

        logger.warning('\tACTUAL OUTPUT ====')
        logger.warning('\n\t\t%s', str(actual_sql).replace('\n', '\n\t\t'))
        logger.warning('\n\t%s', str(actual_df).replace('\n', '\n\t'))

    return ReturnCodes.WRONG


def compare(instance_file: str, n: int):
    instance = re.sub(r'^tests/', '', instance_file).replace('.yaml', '')
    spec_in = parse_specification(instance_file)

    if not is_from_spec:
        log = f'analysis/data/{run}/{instance}_0.log'
        if not os.path.isfile(log):
            log = f'analysis/data/{run}/{instance}.log'
        if not os.path.isfile(log):
            logger.info('No log for %s (%d)', instance, n)
            return instance, ReturnCodes.NO_LOG, []

        with open(log, 'r') as inst_log_file:
            if is_cubes or is_squares:
                result = re.findall(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_r_sep})', inst_log_file.read())
            elif is_scythe:
                result = re.findall(rf'(?:{scythe_sep})\n((?:.|\n)*?)(?:(?:{scythe_sep})|$)', inst_log_file.read())
            else:
                result = inst_log_file.read().strip()
    elif run == 'ratsql':
        if 'ratsql_beam_inferred_code_w_terminals' in spec_in:
            result = spec_in['ratsql_beam_inferred_code_w_terminals']
        else:
            logger.info('No log for %s (%d)', instance, n)
            return instance, ReturnCodes.NO_LOG, []
    elif run == 'smbop':
        if 'smbop_beam_inferred_code' in spec_in:
            result = spec_in['smbop_beam_inferred_code']
        else:
            logger.info('No log for %s (%d)', instance, n)
            return instance, ReturnCodes.NO_LOG, []

    if 'sql' in spec_in and 'db' in spec_in:
        engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': 60})

        if args.check_gt:
            try:
                with engine.begin() as connection:
                    output_df = Table(spec_in['output']).df
                    tmp = check(connection, output_df, spec_in['sql'], instance, True, True, n)
                    if tmp == ReturnCodes.WRONG:
                        logger.error('Wrong output for ground truth in instance %s (%d)', instance, n)
                        return instance, ReturnCodes.GT_MISMATCH, []
                    elif tmp == ReturnCodes.GT_ERROR or tmp == ReturnCodes.BASE_ERROR:
                        logger.error('Error while checking ground truth in instance %s (%d)', instance, n)
                        return instance, ReturnCodes.GT_ERROR, []
            except Exception as e:
                logger.error('Error while checking ground truth in instance %s (%d)', instance, n)
                logger.error('\n%s', str(e))
                return instance, ReturnCodes.GT_ERROR, []

    if result is not None and result != [] and ((not is_patsql) or ('Exception in thread' not in result)):

        if not isinstance(result, list):
            result = [result]

        if 'sql' in spec_in:
            if 'db' not in spec_in or not os.path.isfile(spec_in['db']):
                logger.warning('No database for instance %s (%d)', instance, n)
                return instance, ReturnCodes.NO_DATABASE, []

            verbose = True

            result_list = []

            for result_i, result_sql in enumerate(result):
                result_sql = sanitize_sql(result_sql, spec_in)

                for table_name in spec_in['inputs']:
                    table_name = pathlib.Path(table_name).stem
                    if re.search('^[0-9]', table_name):
                        result_sql = re.sub(f'([^`]){table_name}([^`])', fr'\1`{table_name}`\2', result_sql)
                        spec_in['sql'] = re.sub(f'[^`]{table_name}[^`]', f'`{table_name}`', spec_in['sql'])

                engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': args.timeout})

                try:
                    with engine.begin() as connection:
                        result_orig = check(connection, spec_in['sql'], result_sql, instance, verbose, True, n)
                except Exception as e:
                    logger.error('Error while checking base solution in instance %s (%d)', instance, n)
                    logger.error('\n%s', str(e))
                    return instance, ReturnCodes.BASE_ERROR, []

                if result_orig != ReturnCodes.CORRECT:
                    verbose = False

                random.seed(1)

                fuzzy_results = []

                for i in range(fuzzies):
                    fuzzy_file = f'/tmp/fuzzy_{multiprocessing.current_process().ident}.sqlite3'
                    try:
                        with suppress_stdout():
                            fuzz.generate_random_db_with_queries_wrapper((spec_in['db'], fuzzy_file, [spec_in['sql']], {}))

                        fuzzied_engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{fuzzy_file}", connect_args={'timeout': args.timeout})
                    except Exception as e:
                        os.remove(f'fuzzy_{multiprocessing.current_process().ident}.sqlite3')
                        logger.error('Error while fuzzing instance %s (%d)', instance, n)
                        logger.error('%s', str(e))
                        if os.path.isfile(fuzzy_file):
                            os.remove(fuzzy_file)
                        return instance, ReturnCodes.FUZZER_ERROR, []

                    connection = fuzzied_engine.connect()
                    try:
                        fuzzy_results.append(check(connection, spec_in['sql'], result_sql, instance, verbose, False, n))
                    except Exception as e:
                        logger.warning('Error while checking fuzzied solution in instance %s (%d)', instance, n)
                        logger.error('%s', str(e))
                        if os.path.isfile(fuzzy_file):
                            os.remove(fuzzy_file)
                        return instance, ReturnCodes.FUZZIED_ERROR, []

                    if os.path.isfile(fuzzy_file):
                        os.remove(fuzzy_file)

                    if fuzzy_results[-1] != ReturnCodes.CORRECT:
                        verbose = False

                verbose = False
                result_list.append((instance, result_orig, fuzzy_results))

                counter = Counter(fuzzy_results)
                if result_orig == ReturnCodes.CORRECT and counter[ReturnCodes.CORRECT] == fuzzies:
                    break

            return result_list

        else:
            logger.warning('No ground truth for instance %s (%d)', instance, n)
            return instance, ReturnCodes.NO_GT, []
    else:
        logger.info('No solution for instance %s (%d)', instance, n)
        return instance, ReturnCodes.NO_SOLUTION, []


def initializer():
    """Set maximum amount of memory each worker process can allocate."""
    soft, hard = resource.getrlimit(resource.RLIMIT_DATA)
    resource.setrlimit(resource.RLIMIT_DATA, (args.m * 1024 * 1024, hard))


if __name__ == '__main__':

    instances = list(glob.glob('tests/**/*.yaml', recursive=True))
    print(instances)
    # instances = list(glob.glob('tests/spider/club_1/*.yaml', recursive=True))

    output_file = f'analysis/fuzzy/{run}{"_" if args.save_alt else ""}.csv'
    log_file = f'analysis/fuzzy/{run}{"_" if args.save_alt else ""}.log'

    if os.path.exists(log_file):
        os.remove(log_file)

    logger.addHandler(FileHandler(log_file))
    logger.setLevel(logging.DEBUG)

    logger.info('Starting log for run %s', run)

    with ProcessPool(max_workers=args.p, initializer=initializer) as pool:
        with open(output_file, 'w') as results_f:
            result_writer = csv.writer(results_f)
            result_writer.writerow(('name', 'fuzzies', 'base_eq', 'fuzzy_eq', 'fuzzy_neq', 'fuzzy_err', 'top_i'))

            if not args.force_pool or args.p != 1:
                future = pool.map(compare, shuffled(instances), count(1), chunksize=1, timeout=args.timeout * fuzzies)
            else:
                future = pool.map(compare, instances, count(1), chunksize=1, timeout=args.timeout * fuzzies)

            iterator = future.result()

            while True:
                try:
                    comp_result = next(iterator)
                    if isinstance(comp_result, list):
                        top_i = 0
                        for i, elem in enumerate(comp_result):
                            if elem[1] == ReturnCodes.CORRECT and all(map(lambda x: x == ReturnCodes.CORRECT, elem[2])):
                                top_i = i
                                break
                            elif elem[1] == ReturnCodes.CORRECT:
                                top_i = i
                        instance, res, results = comp_result[top_i]
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res.value, counter[ReturnCodes.CORRECT], counter[ReturnCodes.WRONG], counter[None], top_i + 1 if counter[ReturnCodes.CORRECT] == fuzzies else -1))
                    else:
                        instance, res, results = comp_result
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res.value, counter[ReturnCodes.CORRECT], counter[ReturnCodes.WRONG], counter[None], None))
                except StopIteration:
                    break
                except TimeoutError as error:
                    logger.error('Timeout while getting results...')
                    logger.error("\n%s", error)
                except MemoryError as error:
                    logger.error('Memout while getting results...')
                    logger.error("\n%s", error)
                except Exception as error:
                    logger.error('Error while getting results...')
                    logger.error("\n%s", error)
                finally:
                    results_f.flush()
