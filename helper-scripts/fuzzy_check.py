import csv
import glob
import os
import pathlib
import random
import re
from collections import Counter
from itertools import permutations
from logging import getLogger

import pandas
import rpy2
import sqlalchemy
from pebble import ProcessPool
from rpy2 import robjects

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

args = parser.parse_args()

run = 'sequential_3'

random.seed(args.seed)
seed = random.randrange(2 ** 16)

config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable)
util.store_config(config)

fuzzies = 20


def removesuffix(str1, str2):
    if str1.endswith(str2):
        return str1[:-len(str2)]
    return str1


def execute(connection, sql):
    df = pandas.read_sql_query(sql, connection)
    df = df.sort_values(by=sorted(list(df.columns))).reset_index(drop=True)
    df.columns = df.columns.str.lower()
    return df


def check(connection, expected_sql, actual_sql, instance='', verbose=False, base=False):
    try:
        expected_df = execute(connection, expected_sql)
    except Exception as e:
        if verbose:
            print('Error while executing ground truth for', instance)
            print('\t' + str(e).replace('\n', '\n\t'))
        return None
    try:
        actual_df = execute(connection, actual_sql)
    except Exception as e:
        if verbose:
            print('Error while executing solution for', instance)
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
        print('Wrong output for base solution in', instance)
        print('\tEXPECTED')
        print('\t\t' + str(expected_sql).replace('\n', '\n\t\t'))
        print('\t' + str(expected_df).replace('\n', '\n\t'))
        print('\tACTUAL')
        print('\t\t' + str(actual_sql).replace('\n', '\n\t\t'))
        print('\t' + str(actual_df).replace('\n', '\n\t'))

    return False


def compare(instance_file: str):
    instance = re.sub(r'^tests/', '', instance_file).replace('.yaml', '')
    spec_in = parse_specification(instance_file)

    log = f'analysis/data/{run}/{instance}_0.log'
    if not os.path.isfile(log):
        log = f'analysis/data/{run}/{instance}.log'
    if not os.path.isfile(log):
        print('No log for', instance)
        return instance, -3, []

    with open(log, 'r') as log_file:
        if run != 'patsql':
            result = re.search(
                r'(?:\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution '
                r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\n((?:.|\n)*))|'
                r'(?:\[Query No\.1]===============================\n((?:.|\n)*?)'
                r'(?:(?:\[Query No\.2]===============================)|$))',
                log_file.read())
        else:
            result = log_file.read().strip()

    if result:

        if 'sql' in spec_in:
            if 'db' not in spec_in:
                print('No database', instance_file)
                return instance, None, []

            verbose = True



            if run != 'patsql':
                result_sql = result[1] or result[2]
                result_sql = result_sql.replace('df_', '')
                # result_sql = re.sub(r'\bOVER \(\)', '', result_sql, re.IGNORECASE)

                if run.startswith('sequential') or run.startswith('c'):
                    for table_name in map(lambda x: pathlib.Path(x).stem, spec_in['inputs']):
                        result_sql = re.sub(table_name.replace('-', '[-_]'), table_name, result_sql)

                if not run.startswith('scythe'):
                    for i, input in enumerate(spec_in['inputs']):
                        result_sql = result_sql.replace(f'input{i}', pathlib.Path(input).stem)
                else:
                    for scythe_name, table_name in zip(
                            sorted(set(re.findall(r'\binput[0-9]?\b', result_sql))),
                            map(lambda x: pathlib.Path(x).stem, spec_in['inputs'])):
                        result_sql = re.sub(fr'\b{scythe_name}\b', table_name, result_sql)

            else:
                result_sql = result

                for i, input in enumerate(spec_in['inputs']):
                    result_sql = result_sql.replace(f'input{i + 1}', pathlib.Path(input).stem)

            if run.startswith('scythe'):
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
                    result_orig = check(connection, spec_in['sql'], result_sql, instance, verbose, True)
            except ValueError:
                print('Incorrect number of rows for', instance)
                return instance, None, []

            if result_orig is None:
                verbose = False

            random_generator = random.Random()
            random_generator.seed(1)

            results = []
            for i in range(fuzzies):
                connection = engine.connect()
                trans = connection.begin()
                for table in engine.table_names():
                    rows = list(pandas.read_sql_query(f'SELECT ROWID as rowid from `{table}`', connection)['rowid'])
                    drop = random_generator.sample(rows, k=random_generator.randint(0, len(rows)))

                    sql_tmp = f'DELETE FROM `{table}` WHERE ROWID IN ({",".join(map(str, drop))})'
                    connection.execute(sql_tmp)
                try:
                    results.append(check(connection, spec_in['sql'], result_sql, instance, verbose))
                except ValueError:
                    trans.rollback()
                    print('Incorrect number of rows for', instance)
                    return instance, None, []

                if results[-1] is None:
                    verbose = False

                trans.rollback()

            return instance, result_orig, results

        else:
            print('No ground truth', instance_file)
            return instance, None, []
    else:
        print('No solution', instance_file)
        return instance, -2, []


instances = list(glob.glob('tests/**/*.yaml', recursive=True))


def shuffled(lst):
    lst = list(lst)
    random.shuffle(lst)
    return lst


if args.p == 1:
    with open(f'analysis/fuzzy/{run}.csv', 'w') as results_f:
        result_writer = csv.writer(results_f)
        result_writer.writerow(('name', 'fuzzies', 'base_eq', 'fuzzy_eq', 'fuzzy_neq', 'fuzzy_err'))
        for file in instances:
            instance, res, results = compare(file)
            counter = Counter(results)
            result_writer.writerow((instance, fuzzies, res, counter[True], counter[False], counter[None]))
else:
    with ProcessPool(max_workers=args.p) as pool:
        with open(f'analysis/fuzzy/{run}.csv', 'w') as results_f:
            result_writer = csv.writer(results_f)
            result_writer.writerow(('name', 'fuzzies', 'base_eq', 'fuzzy_eq', 'fuzzy_neq', 'fuzzy_err'))

            future = pool.map(compare, shuffled(instances), chunksize=3, timeout=50)

            iterator = future.result()

            while True:
                try:
                    instance, res, results = next(iterator)
                    counter = Counter(results)
                    result_writer.writerow((instance, fuzzies, res, counter[True], counter[False], counter[None]))
                except StopIteration:
                    break
                except Exception as error:
                    result_writer.writerow((instance, fuzzies, -1, None, None, None))
