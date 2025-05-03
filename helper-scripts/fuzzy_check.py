import csv
import glob
import multiprocessing
import resource
from collections import Counter
from concurrent.futures import TimeoutError
from enum import Enum
from itertools import count
from logging import FileHandler

import sqlalchemy
from TestSuiteEval.fuzz import fuzz
from pebble import ProcessPool

from fuzzing_utils import *
from squares.dsl.table import Table
from squares.util import parse_specification, create_argparser

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
parser.add_argument('--simulate', type=str, default=None)
parser.add_argument('--from-dis', action='store_true')
parser.add_argument('--dis-suffix')

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

scythe_sep = r'\[Query No\.\d+]==============================='

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
    PRE_CORRECT = 2
    PRE_BASE_ERROR = -13
    PRE_FUZZER_ERROR = -14
    PRE_FUZZIED_ERROR = -15
    PRE_WRONG = -16


def check(connection, expected_sql, actual_sql, instance='', verbose=False, base=False, n: int = -1, correct_code=ReturnCodes.CORRECT, base_error_code=ReturnCodes.BASE_ERROR, fuzzied_error_code=ReturnCodes.FUZZIED_ERROR, wrong_code=ReturnCodes.WRONG):
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
        expected_sql = None

    try:
        actual_df = execute(connection, actual_sql)
    except Exception as e:
        if verbose:
            logger.error(actual_sql)
            logger.error('Error while executing solution for instance %s (%d)', instance, n)
            logger.error('\n%s', str(e))
        return base_error_code if base else fuzzied_error_code

    expected_df = expected_df.sort_values(by=sorted(list(expected_df.columns))).reset_index(drop=True)

    if df_soft_equal(expected_df, actual_df):
        return correct_code

    if verbose:
        if base:
            logger.error('Wrong output for base solution in instance %s (%d)', instance, n)
        else:
            logger.warning('Wrong output for fuzzied solution in instance %s (%d)', instance, n)

        print_df_diff(expected_df, actual_df, expected_sql, actual_sql)

    return wrong_code


def check_beam(sqls, instance, spec_in, n,
               correct_code=ReturnCodes.CORRECT,
               base_error_code=ReturnCodes.BASE_ERROR,
               fuzzer_error_code=ReturnCodes.FUZZER_ERROR,
               fuzzied_error_code=ReturnCodes.FUZZIED_ERROR,
               wrong_code=ReturnCodes.WRONG):
    verbose = True
    result_list = []

    for result_i, result_sql in enumerate(sqls):
        if result_sql is None:
            continue

        result_sql = sanitize_sql(result_sql, spec_in, run='squares' if is_squares else 'scythe' if is_scythe else 'patsql' if is_patsql else 'ratsql' if is_from_spec else 'cubes')

        if not is_squares:
            for table_name in spec_in['inputs']:
                table_name = pathlib.Path(table_name).stem
                if re.search('^[0-9]', table_name):
                    result_sql = re.sub(f'([^`]){table_name}([^`])', fr'\1`{table_name}`\2', result_sql)
                    spec_in['sql'] = re.sub(f'[^`]{table_name}[^`]', f'`{table_name}`', spec_in['sql'])

        engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': args.timeout})

        try:
            with engine.begin() as connection:
                result_orig = check(connection, spec_in['sql'], result_sql, instance, verbose, True, n, correct_code, base_error_code, fuzzied_error_code, wrong_code)
        except Exception as e:
            logger.error('Error while checking base solution in instance %s (%d)', instance, n)
            logger.error('\n%s', str(e))
            result_list.append((instance, base_error_code, []))
            continue

        if result_orig != correct_code:
            verbose = False

        random.seed(1)

        fuzzy_results = []

        skip_rest = False
        for i in range(fuzzies):
            fuzzy_file = f'/tmp/fuzzy_{multiprocessing.current_process().ident}.sqlite3'
            try:
                with suppress_stdout():
                    fuzz.generate_random_db_with_queries_wrapper((spec_in['db'], fuzzy_file, [spec_in['sql']], {}))

                if not os.path.isfile(fuzzy_file):
                    result_list.append((instance, fuzzer_error_code, []))
                    skip_rest = True
                    break

                fuzzied_engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{fuzzy_file}", connect_args={'timeout': args.timeout})
            except Exception as e:
                os.remove(f'fuzzy_{multiprocessing.current_process().ident}.sqlite3')
                logger.error('Error while fuzzing instance %s (%d)', instance, n)
                logger.error('%s', str(e))
                if os.path.isfile(fuzzy_file):
                    os.remove(fuzzy_file)
                result_list.append((instance, fuzzer_error_code, []))
                skip_rest = True
                break

            connection = fuzzied_engine.connect()
            try:
                fuzzy_results.append(check(connection, spec_in['sql'], result_sql, instance, verbose, False, n, correct_code, base_error_code, fuzzied_error_code, wrong_code))
            except Exception as e:
                logger.warning('Error while checking fuzzied solution in instance %s (%d)', instance, n)
                logger.error('%s', str(e))
                if os.path.isfile(fuzzy_file):
                    os.remove(fuzzy_file)
                result_list.append((instance, fuzzied_error_code, []))
                skip_rest = True
                break

            if os.path.isfile(fuzzy_file):
                os.remove(fuzzy_file)

            if fuzzy_results[-1] != correct_code:
                verbose = False

        if skip_rest:
            continue

        verbose = False
        result_list.append((instance, result_orig, fuzzy_results))

        counter = Counter(fuzzy_results)
        if result_orig == correct_code and counter[correct_code] == fuzzies:
            break

    return result_list


def load_sql_from_log(run, instance, execution, n=None) -> list:
    log = f'analysis/data/{run}/{instance}_{execution}.log'
    if not os.path.isfile(log):
        log = f'analysis/data/{run}/{instance}.log'
    if not os.path.isfile(log):
        logger.info('No log for %s (%d)', instance, n)
        raise FileNotFoundError

    with open(log, 'r') as inst_log_file:
        if is_cubes or is_squares:
            return re.findall(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_r_sep})', inst_log_file.read())
        elif is_scythe:
            return re.findall(rf'(?:{scythe_sep})\n((?:.|\n)*?)(?:(?:{scythe_sep})|$)', inst_log_file.read())
        else:
            log_file_content = inst_log_file.read()
            if re.search(scythe_sep, log_file_content) is not None:
                return re.findall(rf'(?:{scythe_sep})\n((?:.|\n)*?)(?:(?:{scythe_sep})|$)', log_file_content)
            else:
                return [log_file_content.strip()]


def compare(instance_file: str, n: int):
    instance = re.sub(r'^tests/', '', instance_file).replace('.yaml', '')
    spec_in = parse_specification(instance_file)

    if not is_from_spec and not args.from_dis:
        try:
            sqls = load_sql_from_log(run, instance, 0, n)
        except FileNotFoundError:
            return instance, ReturnCodes.NO_LOG, []

    elif args.from_dis:
        try:
            sqls = dis_sqls[instance]
        except (FileNotFoundError, KeyError):
            return instance, ReturnCodes.NO_LOG, []

    elif run == 'ratsql' or run == 'smbop':
        if f'{run}_beam_inferred_code_w_terminals' in spec_in:
            sqls = spec_in[f'{run}_beam_inferred_code_w_terminals']
        else:
            logger.info('No log for %s (%d)', instance, n)
            return instance, ReturnCodes.NO_LOG, []

    if 'sql' in spec_in and 'db' in spec_in:
        engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{spec_in['db']}", connect_args={'timeout': args.timeout})

        if args.check_gt:
            try:
                with engine.begin() as connection:
                    output_df = Table(spec_in['output']).df
                    tmp = check(connection, output_df, spec_in['sql'], instance, False, True, n)
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

    if args.simulate and f'{args.simulate}_beam_inferred_code_w_terminals' in spec_in:
        pre_sqls = spec_in[f'{args.simulate}_beam_inferred_code_w_terminals']

        logger.info('Checking solutions from NLP for instance%s (%d)', instance, n)
        tmp = check_beam(pre_sqls, instance, spec_in, n, ReturnCodes.PRE_CORRECT, ReturnCodes.PRE_BASE_ERROR, ReturnCodes.PRE_FUZZER_ERROR, ReturnCodes.PRE_FUZZIED_ERROR, ReturnCodes.PRE_WRONG)

        for result in tmp:
            if all(map(lambda x: x == ReturnCodes.PRE_CORRECT or x == ReturnCodes.PRE_WRONG, result[2])) and result[1] == ReturnCodes.PRE_CORRECT:
                return tmp

        logger.info('Solutions from NLP failed for instance%s (%d)', instance, n)

    if sqls and ((not is_patsql) or ('Exception in thread' not in sqls[0])):

        if 'sql' in spec_in:
            if 'db' not in spec_in or not os.path.isfile(spec_in['db']):
                logger.warning('No database for instance %s (%d)', instance, n)
                return instance, ReturnCodes.NO_DATABASE, []

            return check_beam(sqls, instance, spec_in, n)

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

    output_file = f'analysis/fuzzy/{run}{"_dis_fuzz" if args.from_dis else ""}{"_" + args.dis_suffix if args.dis_suffix else ""}{"_" if args.save_alt else ""}.csv'
    log_file = f'analysis/fuzzy/{run}{"_dis_fuzz" if args.from_dis else ""}{"_" + args.dis_suffix if args.dis_suffix else ""}{"_" if args.save_alt else ""}.log'

    if os.path.exists(log_file):
        os.remove(log_file)

    logger.addHandler(FileHandler(log_file))
    logger.setLevel(logging.DEBUG)

    logger.info('Starting log for run %s', run)

    dis_sqls = {}
    if args.from_dis:
        csv.field_size_limit(sys.maxsize)
        dis_file = f'analysis/fuzzy/{run}_dis{"_" + args.dis_suffix if args.dis_suffix else ""}.csv'
        with open(dis_file) as dis_f:
            reader = csv.reader(dis_f)
            next(reader) # skip header
            for line in reader:
                dis_sqls[line[0]] = eval(line[4]) if line[4] else None

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
                            if (elem[1] == ReturnCodes.CORRECT or elem[1] == ReturnCodes.PRE_CORRECT) and (all(map(lambda x: x == ReturnCodes.CORRECT, elem[2])) or all(map(lambda x: x == ReturnCodes.PRE_CORRECT, elem[2]))):
                                top_i = i
                                break
                            elif elem[1] == ReturnCodes.CORRECT or elem[1] == ReturnCodes.PRE_CORRECT:
                                top_i = i
                        instance, res, results = comp_result[top_i]
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res.value, counter[ReturnCodes.CORRECT] + counter[ReturnCodes.PRE_CORRECT], counter[ReturnCodes.WRONG], counter[None], top_i + 1 if counter[ReturnCodes.CORRECT] == len(results) or counter[ReturnCodes.PRE_CORRECT] == len(results) else -1))
                    else:
                        instance, res, results = comp_result
                        counter = Counter(results)
                        result_writer.writerow((instance, fuzzies, res.value, counter[ReturnCodes.CORRECT] + counter[ReturnCodes.PRE_CORRECT], counter[ReturnCodes.WRONG], counter[None], None))
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
