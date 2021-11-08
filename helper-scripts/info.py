#!/usr/bin/python
import csv
import glob
import random
from collections import defaultdict
from logging import getLogger

import rpy2
from rpy2 import robjects
import pglast

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import parse_specification, pairwise, create_argparser

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

config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations,
                minimum_loc=args.min_lines, maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo,
                max_column_combinations=args.max_cols_combo, max_join_combinations=args.max_join_combo,
                subsume_conditions=args.subsume_conditions, transitive_blocking=args.transitive_blocking,
                use_solution_dsl=args.use_dsl, use_solution_cube=args.use_cube, bitenum_enabled=args.bitenum,
                z3_QF_FD=args.qffd, z3_sat_phase='caching', disabled=args.disable)
util.store_config(config)

base_scores = defaultdict(int)
bigrams = defaultdict(lambda: defaultdict(int))

with open('instances.csv', 'w') as output:
    writer = csv.writer(output)
    writer.writerow(('name', 'loc', 'sql_size', 'cols', 'tables', 'total_rows', 'total_cells'))
    for file in glob.glob('tests/**/*.yaml', recursive=True):
        test_name = file.replace('tests/', '', 1).replace('.yaml', '')
        try:
            spec_in = parse_specification(file)
            specification = Specification(spec_in)
            # tyrell_spec = specification.generate_dsl()
            cols = len(specification.columns)
            tables = len(specification.input_tables)
            total_rows = sum(map(len, specification.data_frames.values()))
            total_cells = sum(map(lambda df: len(df.columns) * len(df), specification.data_frames.values()))
            length = None
            if 'sql' in spec_in:
                root = pglast.Node(pglast.parse_sql(spec_in['sql']))
                length = len(list(filter(lambda x: isinstance(x, pglast.Node), root.traverse())))
            elif 'comment' in spec_in:
                try:
                    root = pglast.Node(pglast.parse_sql(spec_in['comment']))
                    length = len(list(filter(lambda x: isinstance(x, pglast.Node), root.traverse())))
                    with open(file, 'a') as f:
                        tmp = spec_in["comment"].replace("\n", "\n\t")
                        f.write(f'\nsql: |-\n\t{tmp}')
                except:
                    print('Could not find SQL query for', test_name)
            # if 'solution' in spec_in:
            #     base_scores[spec_in['solution'][0]] += 1
            #     for p0, p1 in pairwise(spec_in['solution']):
            #         bigrams[p0][p1] += 1
            loc = spec_in['loc'] if 'loc' in spec_in else None
            writer.writerow((test_name, loc, length, cols, tables, total_rows, total_cells))
        except Exception as e:
            print(file, e)
            pass

print(base_scores)
print(bigrams)
