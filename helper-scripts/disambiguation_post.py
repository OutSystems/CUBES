import argparse
import csv
import ctypes
import glob
import logging
import multiprocessing
import os.path
import pathlib
import re
import resource
import signal
import sys
from collections import defaultdict, Counter
from contextlib import contextmanager
from itertools import permutations
from logging import getLogger
from random import shuffle

import chromalog
import pandas
import rpy2
import sqlalchemy
import yaml
from TestSuiteEval.fuzz import fuzz
from ordered_set import OrderedSet
from pandas.core.util.hashing import hash_pandas_object
from pebble import ProcessPool

from squares.dsl import table
from squares.dsl.table import Table
from squares.tyrell.logger import TyrellLogFormatter
from squares.util import pairwise

getLogger('squares').setLevel(50)

logger = getLogger('disambiguator')

formatter = TyrellLogFormatter(fmt='[%(seconds)s][%(processName)s][%(levelname)s] %(message)s')
handler = chromalog.ColorizingStreamHandler()
handler.setFormatter(formatter)
logger.addHandler(handler)

logger.setLevel(logging.DEBUG)

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'(?:------------------------------------- R Solution ---------------------------------------)'
cubes_end_sep = r'(?:------------------------------------- R Solution ---------------------------------------)|(?:All solutions of length \d+ found)|(?:Timeout reached)|(?:\[.*?\])'

# dsl_regex = r'\[MainProcess\]\[INFO\] Solution found: \[(.*)\](?!\n.*\n.*\[ERROR\])'
dsl_regex = r'\[MainProcess\]\[INFO\] Solution found: \[?(.*?)\]?'
sql_regex = rf'{cubes_sql_sep}((?:.|\n)*){cubes_r_sep}'

skipped = 0

STATUS_OK = 0
STATUS_NO_YAML = 1
STATUS_NO_SQL = 2
STATUS_MISMATCH_LENGTH = 3


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print


class OutputGroup:

    def __init__(self):
        self.outputs = []
        self.occurrences = []
        self.mapping = defaultdict(list)

    def add(self, sql, output_df):
        for i in range(len(self.outputs)):
            if df_soft_equal(self.outputs[i], output_df):
                self.occurrences[i] += 1
                self.mapping[i].append(sql)
                return

        self.outputs.append(output_df)
        self.occurrences.append(1)
        self.mapping[len(self.outputs) - 1].append(sql)

    def closests(self):
        target = sum(self.occurrences) / 2
        best_i = 0
        closest = None
        distance = None
        closest_sqls = None
        best_output = None
        for i in range(len(self.occurrences)):
            if closest is None or abs(self.occurrences[i] - target) < distance:
                best_i = i
                closest = self.occurrences[i]
                distance = abs(closest - target)
                closest_sqls = self.mapping[i]
                best_output = self.outputs[i]

        other_sqls = []
        for i in range(len(self.occurrences)):
            if i != best_i:
                other_sqls += self.mapping[i]

        return closest, best_output, closest_sqls, other_sqls

    def __repr__(self):
        return repr(self.occurrences)


def askbool(message):
    while True:
        i = input(message + ' (y/n) ')
        if i.lower() == 'y':
            return True
        if i.lower() == 'n':
            return False


@contextmanager
def suppress_stdout():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout


def sanitize_sql(result_sql, spec_in):
    result_sql = result_sql.replace('df_', '')

    for table_name in map(lambda x: pathlib.Path(x).stem, spec_in['inputs']):
        result_sql = re.sub(table_name.replace('-', '[-_]'), table_name, result_sql)
        result_sql = re.sub(fr'(?<!`)\b{table_name}\b(?!`)', f'`{table_name}`', result_sql)

    for i, input in enumerate(spec_in['inputs']):
        result_sql = result_sql.replace(f'input{i}', f'`{pathlib.Path(input).stem}`')

    return result_sql


def execute(connection, sql):
    df = pandas.read_sql_query(sql, connection)
    df = df.sort_values(by=sorted(list(df.columns))).reset_index(drop=True)
    df.columns = df.columns.str.lower()
    return df


def print_df_diff(df1, df2):
    logger.warning('\tEXPECTED OUTPUT ====')
    logger.warning('\n\t%s', str(df1).replace('\n', '\n\t'))

    logger.warning('\tACTUAL OUTPUT ====')
    logger.warning('\n\t%s', str(df2).replace('\n', '\n\t'))


def df_soft_equal(df1, df2):
    for perm in permutations(list(df1.columns)):
        actual_df_try = df2.copy()
        actual_df_try.columns = perm
        try:
            pandas.testing.assert_frame_equal(df1, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=True)
            return True
        except:
            actual_df_try = actual_df_try.sort_values(by=sorted(list(actual_df_try.columns))).reset_index(drop=True)
            try:
                pandas.testing.assert_frame_equal(df1, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=True)
                return True
            except:
                pass

    return False


def ask_user(df, instance, engine):
    print('Is the following output correct for instance', instance, '?')
    print(df)
    if args.interactive:
        return askbool('> ')

    with engine.begin() as connection:
        gt_df = execute(connection, sql)
        print('Automatic execution resulted in:')
        print(gt_df)
        return df_soft_equal(gt_df, df)


def disambiguate(instance, spec, sql_list, splits=None, fuzzy_level=1, results=None):
    if splits is None:
        splits = []
    if results is None:
        results = []
    logger.debug('Fuzzing disambiguation for %s part %d (%d queries)', instance, fuzzy_level, len(sql_list))

    if 'db' not in spec:
        logger.error('No database for instance %s', instance)
        return
    else:
        db_file = spec['db']

    output_table = Table(spec['output'])
    output_df = output_table.df.sort_values(by=sorted(list(output_table.df.columns))).reset_index(drop=True)

    engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{db_file}", connect_args={'timeout': 60})

    sqls = []
    dfs = []

    with engine.begin() as connection:
        for i, sql in enumerate(sql_list):
            try:
                df = execute(connection, sql)

                if not df_soft_equal(output_df, df):
                    logger.error('Wrong output for solution %d of instance %s', i, instance)
                    print_df_diff(output_df, df)
                else:
                    sqls.append(sql)
                    dfs.append(df)

            except Exception as e:
                print(e)

    outputs = defaultdict(OutputGroup)

    fuzzied_engines = []

    for round_i in range(args.rounds):
        fuzzy_file = f'/tmp/fuzzy_{multiprocessing.current_process().ident}.sqlite3'
        try:
            with suppress_stdout():
                fuzz.generate_random_db_with_queries_wrapper((db_file, fuzzy_file, sqls, {}))

            fuzzied_engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{fuzzy_file}", connect_args={'timeout': args.timeout})
            fuzzied_engines.append(fuzzied_engine)
        except Exception as e:
            logger.error('Error while fuzzing instance %s', instance)
            logger.error('%s', str(e))
        else:
            connection = fuzzied_engine.connect()
            for sql in sqls:
                df = execute(connection, sql)

                outputs[round_i].add(sql, df)
            connection.close()

    bests = [x.closests() for x in outputs.values()]

    target = len(sqls) / 2
    best_i = None
    distance = None
    for i in range(len(bests)):
        if best_i is None or abs(bests[i][0] - target) < distance:
            best_i = i
            distance = abs(bests[i][0] - target)

    logger.debug('Best split found was %s', outputs[best_i].occurrences)

    if bests[best_i][0] == len(sqls):
        logger.debug('Disambiguated! (%d queries)', len(sqls))
        return sqls, fuzzy_level, splits, results

    if ask_user(bests[best_i][1], instance, fuzzied_engines[best_i]):
        logger.debug('Output accepted. Reject %d queries. Continuing with %d.', len(bests[best_i][3]), len(bests[best_i][2]))
        return disambiguate(instance, spec, bests[best_i][2], splits + [outputs[best_i].occurrences], fuzzy_level + 1, results + [True])
    else:
        logger.debug('Output rejected. Reject %d queries. Continuing with %d.', len(bests[best_i][2]), len(bests[best_i][3]))
        return disambiguate(instance, spec, bests[best_i][3], splits + [outputs[best_i].occurrences], fuzzy_level + 1, results + [False])


def print_stats():
    logger.debug('Time spent on {fuzz: %f, exec: %f}', fuzz_time.value, exec_time.value)


def compute(file):
    with open(file) as f:
        content = f.read()

    instance = file.replace(f'analysis/data/{args.run}/', '').replace('_0.log', '')
    yaml_file = f'tests-examples/{instance}.yaml'

    if not os.path.isfile(yaml_file):
        logger.error('Instance %s skipped due to instance file not existing', instance)
        return instance, 0, None, None

    with open(yaml_file) as f:
        spec = yaml.safe_load(f)

    tmp1 = re.findall(dsl_regex, content)
    tmp2 = re.findall(rf'{cubes_r_sep}((?:.|\n)*?)(?:$|(?={cubes_end_sep}))', content)

    if len(tmp1) == len(tmp2):
        dsl_list = []
        sql_list = []

        for dsl, cubes_sol in zip(tmp1, tmp2):
            tmp3 = re.search(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_end_sep})', cubes_sol)

            if tmp3 is None:
                skipped += 1
                return

            else:
                for i, input_f in enumerate(spec['inputs']):
                    table_name = table.get_table_name(input_f)
                    dsl = re.sub(rf'\binput{i}', table_name, dsl)

                sql = tmp3[1]
                sql = re.sub(r'(\s|\n)+', ' ', sql).strip()
                sql = sanitize_sql(sql, spec)

                dsl_list.append(dsl)
                sql_list.append(sql)

        if sql_list:
            disambiguate(instance, spec, sql_list)
        else:
            logger.info('No solutions found for %s', instance)

    else:
        logger.warning('Instance %s skipped due to mismatched lenghts: %d != %d', instance, len(tmp1), len(tmp2))
        skipped += len(tmp2)


def initializer():
    """Set maximum amount of memory each worker process can allocate."""
    soft, hard = resource.getrlimit(resource.RLIMIT_DATA)
    resource.setrlimit(resource.RLIMIT_DATA, (args.m * 1024 * 1024, hard))


if __name__ == '__main__':
    fuzz_time = multiprocessing.Value(ctypes.c_double)
    exec_time = multiprocessing.Value(ctypes.c_double)

    signal.signal(signal.SIGALRM, print_stats)
    signal.setitimer(signal.ITIMER_REAL, 0, 60)

    parser = argparse.parser = argparse.ArgumentParser()
    parser.add_argument('run', metavar='RUN')
    parser.add_argument('--rounds', type=int, default=16)
    parser.add_argument('--timeout', type=int, default=60)
    parser.add_argument('--interactive', action='store_true')
    args = parser.parse_args()

    output_file = f'analysis/fuzzy/{args.run}_dis.csv'
    instances = list(glob.glob(f'analysis/data/{args.run}/**/*.log', recursive=True))
    shuffle(instances)

    with ProcessPool(max_workers=args.p, initializer=initializer) as pool:
        with open(output_file, 'w') as results_f:
            result_writer = csv.writer(results_f)
            result_writer.writerow(('name', 'status', 'total_queries', 'n_questions', 'final_queries', 'splits', 'results'))

            future = pool.map(compute, instances, chunksize=1, timeout=args.timeout * args.rounds)

            iterator = future.result()

            while True:
                try:
                    comp_result = next(iterator)
                    result_writer.writerow(comp_result)
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
