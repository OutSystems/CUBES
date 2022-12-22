#!/usr/bin/python
import csv
import glob
import itertools
import os
import random
import traceback
from collections import defaultdict
from logging import getLogger

import pathlib

import re
import rpy2
from TestSuiteEval.sql_util.process_sql import parse_sql, Schema, get_schema, get_sql
from rpy2 import robjects
import pglast

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import parse_specification, pairwise, create_argparser

from spider_eval import Evaluator


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
args = parser.parse_args()

random.seed(args.seed)
seed = random.randrange(2 ** 16)

base_config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                     minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                     max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                     subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                     use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                     z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable, top_programs=args.top,
                     use_beam_info=False, use_solution_loc=args.use_loc, beam_threshold=args.beam_threshold,
                     enum_until=args.under, max_min_gen_cols=args.max_min_use_gen_cols, beam_name=args.beam_name)
util.store_config(base_config)

base_scores = defaultdict(int)
bigrams = defaultdict(lambda: defaultdict(int))

evaluator = Evaluator()

with open('instances.csv', 'w') as output:
    writer = csv.writer(output)
    writer.writerow(('name', 'loc', 'sql_size', 'sql_length', 'sql_hardness', 'tables', 'total_cols', 'unique_cols', 'total_rows', 'total_cells', 'total_bytes', 'input_cols', 'unique_input_cols', 'input_rows', 'input_cells', 'input_bytes', 'output_cols', 'output_rows', 'output_cells', 'output_bytes'))
    for file in glob.glob('tests/**/*.yaml', recursive=True):
        test_name = file.replace('tests/', '', 1).replace('.yaml', '')
        try:
            spec_in = parse_specification(file)
            specification = Specification(spec_in)
            # tyrell_spec = specification.generate_dsl()
            tables = len(specification.input_tables)
            unique_cols = len(specification.columns)
            total_cols = sum([len(table.df.columns) for table in specification.input_tables + [specification.output_table]])
            total_rows = sum([len(table.df) for table in specification.input_tables + [specification.output_table]])
            total_cells = sum([len(table.df) * len(table.df.columns) for table in specification.input_tables + [specification.output_table]])
            unique_input_cols = len(set(itertools.chain.from_iterable([table.df.columns for table in specification.input_tables])))
            input_cols = sum([len(table.df.columns) for table in specification.input_tables])
            input_rows = sum([len(table.df) for table in specification.input_tables])
            input_cells = sum([len(table.df) * len(table.df.columns) for table in specification.input_tables])
            output_cols = len(specification.output_table.df.columns)
            output_rows = len(specification.output_table.df)
            output_cells = output_cols * output_rows

            input_bytes = sum(map(lambda n: os.path.getsize(n), spec_in['inputs']))
            output_bytes = os.path.getsize(spec_in['output'])
            total_bytes = input_bytes + output_bytes

            nodes = None
            length = None
            hardness = None
            if 'sql' in spec_in:
                try:
                    root = pglast.Node(pglast.parse_sql(spec_in['sql']))
                    nodes = len(list(filter(lambda x: isinstance(x, pglast.Node), root.traverse())))
                except Exception as e:
                    print('Error parsing instance', test_name, 'using pglast')
                    print(e)
                length = len(spec_in['sql'])
                try:
                    result_sql = spec_in['sql']
                    for i, table_name in enumerate(map(lambda x: pathlib.Path(x).stem, spec_in['inputs'])):
                        result_sql = re.sub(f'"{table_name}"', table_name, result_sql)
                    hardness = evaluator.eval_hardness(get_sql(Schema(get_schema(spec_in['db'])), result_sql))
                except Exception as e:
                    print('Error parsing instance', test_name, 'using spider')
                    # traceback.print_exc()
                    print(e)
            else:
                print('Could not find SQL query for', test_name)
            # if 'solution' in spec_in:
            #     base_scores[spec_in['solution'][0]] += 1
            #     for p0, p1 in pairwise(spec_in['solution']):
            #         bigrams[p0][p1] += 1
            loc = spec_in['loc'] if 'loc' in spec_in else None
            writer.writerow((test_name, loc, nodes, length, hardness, tables, total_cols, unique_cols, total_rows, total_cells, total_bytes, input_cols, unique_input_cols, input_rows, input_cells, input_bytes, output_cols, output_rows, output_cells, output_bytes))
        except Exception as e:
            print(file, e)
            pass

print(base_scores)
print(bigrams)
