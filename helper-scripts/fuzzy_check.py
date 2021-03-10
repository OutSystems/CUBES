import csv
import glob
import multiprocessing
import os
import pathlib
import random
import re
import threading
from collections import Counter
from itertools import permutations
from logging import getLogger

import pandas
import rpy2
import sqlalchemy
from pebble import ProcessPool
from rpy2 import robjects
from TestSuiteEval.fuzz import fuzz

from squares import util
from squares.config import Config
from squares.util import parse_specification, create_argparser


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print

robjects.r('''
sink("/dev/null")
options(warn=-1)
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))''')

getLogger('squares').setLevel(50)

parser = create_argparser(all_inputs=True)
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-m', default=57344, type=int, help='memout')
parser.add_argument('-p', default=1, type=int, help='#processes')
parser.add_argument('--run', help='run')
parser.add_argument('--force-pool', action='store_true')
parser.add_argument('--check-gt', action='store_true')
parser.add_argument('--save-alt', action='store_true')
parser.add_argument('--fuzzies', type=int, default=16)

args = parser.parse_args()

run = args.run

is_gt = run == 'gt'
is_patsql = run.startswith('patsql')
is_scythe = run.startswith('scythe')
is_squares = run.startswith('squares')
is_cubes = not is_patsql and not is_scythe and not is_squares

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'------------------------------------- R Solution ---------------------------------------'
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


def removesuffix(str1, str2):
    if str1.endswith(str2):
        return str1[:-len(str2)]
    return str1


def execute(connection, sql):
    df = pandas.read_sql_query(sql, connection)
    df = df.sort_values(by=sorted(list(df.columns))).reset_index(drop=True)
    df.columns = df.columns.str.lower()
    return df


def check(connection, expected_sql, actual_sql, instance='', verbose=False, base=False, file_total=None, file_i=None):
    if isinstance(expected_sql, str):
        try:
            expected_df = execute(connection, expected_sql)
        except Exception as e:
            if verbose:
                print('Error while executing ground truth for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                print('\t' + str(e).replace('\n', '\n\t'))
            return None
    else:
        expected_df = expected_sql
    try:
        actual_df = execute(connection, actual_sql)
    except Exception as e:
        if verbose:
            print('Error while executing solution for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
            print('\t' + str(e).replace('\n', '\n\t'))
        return None

    expected_df = expected_df.sort_values(by=sorted(list(expected_df.columns))).reset_index(drop=True)

    for perm in permutations(list(expected_df.columns)):
        actual_df_try = actual_df.copy()
        # print(actual_df_try.columns, perm)
        actual_df_try.columns = perm
        try:
            # print(actual_df_try)
            pandas.testing.assert_frame_equal(expected_df, actual_df_try, check_names=False, check_like=True)
            return True
        except:
            # print(sorted(list(actual_df_try.columns)))
            actual_df_try = actual_df_try.sort_values(by=sorted(list(actual_df_try.columns))).reset_index(drop=True)
            try:
                # print(actual_df_try)
                pandas.testing.assert_frame_equal(expected_df, actual_df_try, check_names=False, check_like=True)
                return True
            except:
                pass

    if base:
        print('Wrong output for base solution in', instance, f'{file_i}/{file_total}' if file_i is not None else '')
        print('\tEXPECTED')
        print('\t\t' + str(expected_sql).replace('\n', '\n\t\t'))
        print('\t' + str(expected_df).replace('\n', '\n\t'))
        print('\tACTUAL')
        print('\t\t' + str(actual_sql).replace('\n', '\n\t\t'))
        print('\t' + str(actual_df).replace('\n', '\n\t'))

    return False


def compare(instance_file: str, file_total=None, file_i=None):
    instance = re.sub(r'^tests/', '', instance_file).replace('.yaml', '')
    spec_in = parse_specification(instance_file)

    print(instance)

    log = f'analysis/data/{run}/{instance}_0.log'
    if not os.path.isfile(log):
        log = f'analysis/data/{run}/{instance}.log'
    if not os.path.isfile(log):
        print('No log for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
        return instance, -3, []

    with open(log, 'r') as log_file:
        if is_cubes or is_squares:
            result = re.findall(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_r_sep})', log_file.read())
        elif is_scythe:
            result = re.findall(rf'(?:{scythe_sep})\n((?:.|\n)*?)(?:(?:{scythe_sep})|$)', log_file.read())
        else:
            result = log_file.read().strip()

    if 'sql' in spec_in and 'db' in spec_in:
        engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': 60})

        if args.check_gt:
            try:
                with engine.begin() as connection:
                    if check(connection, pandas.read_csv(spec_in['output']), spec_in['sql'], instance, True, True, file_total, file_i) is False:
                        print('ERROR ERROR! Wrong output for gt', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                        return instance, -5, []
            except ValueError:
                print('Incorrect number of rows for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                return instance, -7, []

    if result != [] and ((not is_patsql) or ('Exception in thread' not in result)):

        if not isinstance(result, list):
            result = [result]

        if 'sql' in spec_in:
            if 'db' not in spec_in:
                print('No database', instance_file, f'{file_i}/{file_total}' if file_i is not None else '')
                return instance, -6, []

            verbose = True

            resulsss = []

            for result_i, result_sql in enumerate(result):
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

                for table_name in spec_in['inputs']:
                    table_name = pathlib.Path(table_name).stem
                    if re.search('^[0-9]', table_name):
                        result_sql = re.sub(f'([^`]){table_name}([^`])', fr'\1`{table_name}`\2', result_sql)
                        spec_in['sql'] = re.sub(f'[^`]{table_name}[^`]', f'`{table_name}`', spec_in['sql'])

                engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': 60})

                try:
                    with engine.begin() as connection:
                        result_orig = check(connection, spec_in['sql'], result_sql, instance, verbose, True, file_total, file_i)
                except ValueError:
                    print('Incorrect number of rows for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                    return instance, -8, []

                if result_orig is None:
                    verbose = False

                random.seed(1)

                results = []

                for i in range(fuzzies):
                    try:
                        fuzz.generate_random_db_with_queries_wrapper(
                            (spec_in['db'], f'test{multiprocessing.current_process().ident}.sqlite3', [spec_in['sql']], {}))
                        fuzzied_engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///test{multiprocessing.current_process().ident}.sqlite3",
                                                                  connect_args={'timeout': 60})
                    except:
                        print('Error while fuzzing for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                        return instance, -4, []
                    connection = fuzzied_engine.connect()
                    try:
                        results.append(check(connection, spec_in['sql'], result_sql, instance, verbose, False, file_total, file_i))
                    except ValueError:
                        print('Incorrect number of rows for', instance, f'{file_i}/{file_total}' if file_i is not None else '')
                        return instance, -9, []

                    if results[-1] is None:
                        verbose = False

                resulsss.append((instance, result_orig, results))

            return resulsss

        else:
            print('No ground truth', instance_file, f'{file_i}/{file_total}' if file_i is not None else '')
            return instance, -10, []
    else:
        print('No solution', instance_file, f'{file_i}/{file_total}' if file_i is not None else '')
        return instance, -2, []


instances = list(glob.glob('tests/**/*.yaml', recursive=True))
# instances = list(glob.glob('tests/spider/club_1/*.yaml', recursive=True))


def shuffled(lst):
    lst = list(lst)
    random.shuffle(lst)
    return lst


output_file = f'analysis/fuzzy/{run}{"_" if args.save_alt else ""}.csv'

if args.p == 1 and not args.force_pool:
    with open(output_file, 'w') as results_f:
        result_writer = csv.writer(results_f)
        result_writer.writerow(('name', 'fuzzies', 'base_eq', 'fuzzy_eq', 'fuzzy_neq', 'fuzzy_err', 'top_i'))
        for i, file in enumerate(instances):
            comp_result = compare(file, len(instances), i)
            if isinstance(comp_result, list):
                try:
                    top_i = list(map(lambda x: all(x[2]), comp_result)).index(True)
                except:
                    top_i = 0
                instance, res, results = comp_result[top_i]
                counter = Counter(results)
                result_writer.writerow(
                    (instance, fuzzies, res, counter[True], counter[False], counter[None], top_i + 1 if counter[True] == fuzzies else -1))
            else:
                instance, res, results = comp_result
                counter = Counter(results)
                result_writer.writerow((instance, fuzzies, res, counter[True], counter[False], counter[None], None))

else:
    with ProcessPool(max_workers=args.p) as pool:
        with open(output_file, 'w') as results_f:
            result_writer = csv.writer(results_f)
            result_writer.writerow(('name', 'fuzzies', 'base_eq', 'fuzzy_eq', 'fuzzy_neq', 'fuzzy_err', 'top_i'))

            if not args.force_pool or args.p != 1:
                future = pool.map(compare, shuffled(instances), chunksize=1, timeout=10*fuzzies)
            else:
                future = pool.map(compare, instances, chunksize=1, timeout=10*fuzzies)

            iterator = future.result()

            while True:
                try:
                    comp_result = next(iterator)
                    if isinstance(comp_result, list):
                        try:
                            top_i = list(map(lambda x: all(x[2]), comp_result)).index(True)
                        except:
                            top_i = 0
                        instance, res, results = comp_result[top_i]
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res, counter[True], counter[False], counter[None],
                                                top_i + 1 if counter[True] == fuzzies else -1))
                    else:
                        instance, res, results = comp_result
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res, counter[True], counter[False], counter[None], None))
                except StopIteration:
                    break
                except Exception as error:
                    pass
                    # result_writer.writerow((instance, fuzzies, -1, None, None, None, None))
