import argparse
import glob
import multiprocessing
import os
import pathlib
import re
import subprocess
import json
import tempfile
from logging import getLogger
from multiprocessing import Pool
import random

import pandas
import pglast
import rpy2
from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
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

run = 'scythe'

random.seed(args.seed)
seed = random.randrange(2 ** 16)

config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable)
util.store_config(config)


def removesuffix(str1, str2):
    if str1.endswith(str2):
        return str1[:-len(str2)]
    return str1


def type_convert(type):
    if pandas.api.types.is_integer_dtype(type):
        return 'int'
    else:
        return 'str'


def compare(instance_file: str):
    spec_in = parse_specification(instance_file)

    if 'sql' in spec_in:
        instance = re.sub(r'^tests/', '', instance_file).replace('.yaml', '')
        log = f'analysis/data/{run}/{instance}_0.log'
        if not os.path.isfile(log):
            log = f'analysis/data/{run}/{instance}.log'
        if not os.path.isfile(log):
            print('No log for', instance)
            return

        with open(log, 'r') as log_file:
            result = re.search(
                r'(?:\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution '
                r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\n((?:.|\n)*))|'
                r'(?:\[Query No\.1]===============================\n((?:.|\n)*?)'
                r'(?:(?:\[Query No\.2]===============================)|$))',
                log_file.read())

        if result:
            result_sql = result[1] or result[2]
            ground_truth = re.sub(r'count\s*\(\s*\)', 'count(*)', re.sub('order by .*', '', removesuffix(spec_in['sql']
                                                                                                         .lower()
                                                                                                         .strip(), ';'),
                                                                         flags=re.IGNORECASE), flags=re.IGNORECASE) \
                .replace('`', '') \
                .strip()
            actual_query = re.sub(r'count\s*\(\s*\)', 'count(*)', re.sub('order by .*', '', removesuffix(result_sql
                                                                                                         .lower()
                                                                                                         .strip(), ';'),
                                                                         flags=re.IGNORECASE), flags=re.IGNORECASE) \
                .replace('`', '') \
                .strip()

            cosette = ''

            specification = Specification(spec_in)
            for table in specification.input_tables:
                cosette += f'schema {table}('
                cols = []
                for column in specification.data_frames[table]:
                    cols.append(f'{column}:{type_convert(specification.data_frames[table].dtypes[column])}')
                cosette += ', '.join(cols) + ');\n'

            cosette += '\n'
            for table in specification.input_tables:
                ground_truth = re.sub(fr'\b{table[3:]}\b', f'{table}', ground_truth)
                ground_truth = re.sub(fr'\bfrom {table}(?! as)\b', f'from {table} {table}', ground_truth)
                actual_query = re.sub(fr'\bfrom {table}(?! as)\b', f'from {table} {table}', actual_query)
                cosette += f'table {table}({table});\n'

            cosette += f'\nquery q1\n`{ground_truth}`;\n'
            cosette += f'\nquery q2\n`{actual_query}`;\n'
            cosette += '\nverify q1 q2;'

            # print('\n'.join(map(str, enumerate(cosette.split('\n')))))

            out_file = f'analysis/cosette/{run}/{instance}.cos'
            pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)
            with open(out_file, 'w') as f:
                f.write(cosette)
                f.flush()

        else:
            print('No solution', instance_file)
    else:
        print('No ground truth', instance_file)


instances = list(glob.glob('tests/**/*.yaml', recursive=True))

if args.p == 1:
    for file in instances:
        compare(file)
else:
    with Pool(processes=args.p) as pool:
        pool.map(compare, instances, chunksize=1)
