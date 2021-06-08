#!/usr/bin/python3
import argparse
import csv

import glob

import os
import random
from copy import deepcopy
from logging import getLogger

import pandas
import rpy2
import yaml
from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import create_argparser, parse_specification


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


def convert_line(table_name, line, corrected):
    line2 = []
    for i, cell in enumerate(line):
        if isinstance(cell, str) and cell == '':
            if pandas.api.types.is_integer_dtype(specification.data_frames[table_name].dtypes[i]) or \
                    pandas.api.types.is_float_dtype(specification.data_frames[table_name].dtypes[i]):
                line2.append('NULL[num]')
            elif pandas.api.types.is_string_dtype(specification.data_frames[table_name].dtypes[i]):
                line2.append('NULL[str]')
            elif pandas.api.types.is_datetime64_dtype(specification.data_frames[table_name].dtypes[i]):
                line2.append('NULL[date]')
            elif pandas.api.types.is_timedelta64_dtype(specification.data_frames[table_name].dtypes[i]):
                line2.append('NULL[time]')
            else:
                print(specification.data_frames[table_name].dtypes[i])
                raise NotImplementedError
        elif isinstance(cell, str) and ',' in cell:
            corrected = True
            line2.append(cell.replace(',', ';'))
        else:
            line2.append(cell)
    return line2, corrected


if __name__ == '__main__':

    parser = create_argparser(all_inputs=True)
    parser.add_argument('output', metavar='OUTPUT')

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

    for file in glob.glob('tests-examples/kaggle/*.yaml', recursive=True):
        if 'schema.yaml' in file:
            continue

        print(file)

        try:
            corrected = False

            with open(file) as f:
                spec = yaml.safe_load(f)

            specification = Specification(parse_specification(file))

            result = ''

            for input_name, table_name in zip(spec['inputs'], specification.input_tables):
                result += '#input\n\n'
                with open(input_name) as f:
                    reader = csv.reader(f)
                    for line in reader:
                        line2, corrected = convert_line(table_name, line, corrected)
                        line2 = ','.join(map(str, line2)).replace('\n', '\\n')
                        result += line2 + '\n'
                result += '\n'

            result += '#output\n\n'
            with open(spec['output']) as f:
                reader = csv.reader(f)
                for line in reader:
                    line2, corrected = convert_line('expected_output', line, corrected)
                    line2 = ','.join(map(str, line2)).replace('\n', '\\n')
                    result += line2 + '\n'
            result += '\n'

            result += '#constraint\n{\n'

            if 'const' in spec:
                spec['constants'] = spec['const']
            if 'functions' in spec:
                spec['aggregation_functions'] = spec['functions']
            if 'aggrs' in spec:
                spec['aggregation_functions'] = spec['aggrs']

            if 'constants' not in spec:
                spec['constants'] = []

            if 'aggregation_functions' not in spec:
                spec['aggregation_functions'] = []

            if 'n' in spec['aggregation_functions']:
                spec['aggregation_functions'].remove('n')
                spec['aggregation_functions'].append('count')

            result += '"constants": ' + repr(list(map(str, spec['constants']))) + ',\n'
            result += '"aggregation_functions": ' + repr(spec['aggregation_functions'])

            result += '\n}\n'

            output_file = file.replace('tests-examples', args.output).replace('.yaml', '')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

            if corrected:
                print('\tCORRECTED')

        except Exception as e:
            print(f'Failed to convert {file}!')
            raise e
            print(e)
