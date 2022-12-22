import argparse
import csv
import ctypes
import glob
import multiprocessing
import os.path
import resource
import signal
import traceback
from random import shuffle

import sqlalchemy
import yaml
from TestSuiteEval.fuzz import fuzz
from pebble import ProcessPool

from fuzzing_utils import *
from squares.dsl.table import Table

dsl_regex = r'\[MainProcess\]\[INFO\] Solution found: \[?(.*?)\]?'
sql_regex = rf'{cubes_sql_sep}((?:.|\n)*){cubes_r_sep}'

skipped = 0

STATUS_OK = 0
STATUS_NO_YAML = 1
STATUS_NO_SQL = 2
STATUS_MISMATCH_LENGTH = 3
STATUS_NO_DB = 4
STATUS_FUZZER_ERROR = 5


class OutputGroup:

    def __init__(self):
        self.outputs = []
        self.occurrences = []
        self.mapping = []

    def add(self, sql, output_df):
        for i in range(len(self.outputs)):
            if df_soft_equal(self.outputs[i], output_df):
                self.occurrences[i] += 1
                self.mapping[i].append(sql)
                return

        self.outputs.append(output_df)
        self.occurrences.append(1)
        self.mapping.append([sql])

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


def ask_user(df, sql, instance, engine):
    print('Is the following output correct for instance', instance, '?')
    print(df)
    if args.interactive:
        return askbool('> ')

    with engine.begin() as connection:
        print(sql)
        gt_df = execute(connection, sql)
        print('Automatic execution resulted in:')
        print(gt_df)
        return df_soft_equal(gt_df, df)


def disambiguate(instance, spec, sql_list, total_sql=None, splits=None, fuzzy_level=1, results=None):
    if splits is None:
        splits = []
    if results is None:
        results = []
    if total_sql is None:
        total_sql = sql_list
    logger.debug('Fuzzing disambiguation for %s part %d (%d queries)', instance, fuzzy_level, len(sql_list))

    if 'db' not in spec:
        logger.error('No database for instance %s', instance)
        return instance, STATUS_NO_DB, 0, 0, None, None, None
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

    if len(sqls) == 0:
        return instance, STATUS_NO_SQL, 0, 0, None, None, None

    outputs = []
    fuzzied_engines = []

    random.seed(1)

    for round_i in range(args.rounds):
        fuzzy_file = f'/tmp/fuzzy_{multiprocessing.current_process().ident}_{round_i}.sqlite3'
        try:
            with suppress_stdout():
                fuzz.generate_random_db_with_queries_wrapper((db_file, fuzzy_file, sqls, {}))

            fuzzied_engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{fuzzy_file}", connect_args={'timeout': args.timeout})
        except Exception as e:
            logger.error('Error while fuzzing instance %s', instance)
            logger.error('%s', str(e))
            return instance, STATUS_FUZZER_ERROR, 0, 0, None, None, None
        else:
            fuzzied_engines.append(fuzzied_engine)
            connection = fuzzied_engine.connect()
            outputs.append(OutputGroup())
            for sql in sqls:
                df = execute(connection, sql)
                outputs[-1].add(sql, df)
            connection.close()

    bests = [x.closests() for x in outputs]

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
        return instance, STATUS_OK, len(total_sql), fuzzy_level, sqls, splits, results

    if ask_user(bests[best_i][1], spec['sql'], instance, fuzzied_engines[best_i]):
        logger.debug('Output accepted. Reject %d queries. Continuing with %d.', len(bests[best_i][3]), len(bests[best_i][2]))
        return disambiguate(instance, spec, bests[best_i][2], total_sql, splits + [outputs[best_i].occurrences], fuzzy_level + 1, results + [True])
    else:
        logger.debug('Output rejected. Reject %d queries. Continuing with %d.', len(bests[best_i][2]), len(bests[best_i][3]))
        return disambiguate(instance, spec, bests[best_i][3], total_sql, splits + [outputs[best_i].occurrences], fuzzy_level + 1, results + [False])


def process_worker_sighandler(signum, frame):
    print("ABC")
    print(frame)
    signame = signal.Signals(signum).name
    print(f'Signal handler called with signal {signame} ({signum})')


def compute(file):
    signal.signal(signal.SIGTERM, process_worker_sighandler)

    random.seed('squares')

    with open(file) as f:
        content = f.read()

    instance = file.replace(f'analysis/data/{args.run}/', '').replace('_0.log', '').replace('.log', '')
    yaml_file = f'tests-examples/{instance}.yaml'

    if not os.path.isfile(yaml_file):
        logger.error('Instance %s skipped due to instance file not existing', instance)
        return instance, STATUS_NO_YAML, 0, 0, None, None, None

    with open(yaml_file) as f:
        spec = yaml.safe_load(f)

    tmp2 = re.findall(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_r_sep})', content)

    sql_list = []

    for sql_sol in tmp2:
        # if tmp3 is None:
        #     return instance, STATUS_NO_SQL, 0, 0, None, None, None

        sql = re.sub(r'(\s|\n)+', ' ', sql_sol).strip()
        sql = sanitize_sql(sql, spec)

        sql_list.append(sql)

    if sql_list:
        return disambiguate(instance, spec, sql_list)
    else:
        logger.info('No solutions found for %s', instance)
        return instance, STATUS_NO_SQL, 0, 0, None, None, None

    # else:
    #     logger.warning('Instance %s skipped due to mismatched lenghts: %d != %d', instance, len(tmp1), len(tmp2))
    #     return instance, STATUS_MISMATCH_LENGTH, 0, 0, None, None, None


def initializer():
    """Set maximum amount of memory each worker process can allocate."""
    soft, hard = resource.getrlimit(resource.RLIMIT_DATA)
    resource.setrlimit(resource.RLIMIT_DATA, (args.m * 1024 * 1024, hard))


if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser()
    parser.add_argument('run', metavar='RUN')
    parser.add_argument('-m', default=65536, type=int, help='memout')
    parser.add_argument('-p', default=1, type=int, help='number of processes')
    parser.add_argument('--rounds', type=int, default=16)
    parser.add_argument('--timeout', type=int, default=60)
    parser.add_argument('--interactive', action='store_true')
    parser.add_argument('--save-alt', action='store_true')
    parser.add_argument('--suffix')
    parser.add_argument('--instances')
    args = parser.parse_args()

    random.seed('squares')

    output_file = f'analysis/fuzzy/{args.run}_dis{"_" + args.suffix if args.suffix else ""}{"_" if args.save_alt else ""}.csv'
    if not args.instances:
        instances = list(glob.glob(f'analysis/data/{args.run}/**/*.log', recursive=True))
    else:
        all_instances = list(glob.glob(f'analysis/data/{args.run}/**/*.log', recursive=True))
        allowed_instances = []
        with open(args.instances) as inst_list:
            for inst in inst_list.readlines():
                if inst[:-1]:
                    allowed_instances.append(inst[:-1])
        instances = []
        for instance in all_instances:
            for allowed in allowed_instances:
                if allowed in instance:
                    instances.append(instance)
                    break

    print(instances)
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
                    print(traceback.format_exc())
                finally:
                    results_f.flush()
