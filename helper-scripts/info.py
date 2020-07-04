#!/usr/bin/python
import csv
import glob
import random
from collections import defaultdict

from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import parse_specification, pairwise, create_argparser

robjects.r('''
sink("/dev/null")
options(warn=-1)
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))''')

parser = create_argparser(all_inputs=True)
args = parser.parse_args()

random.seed(args.seed)
seed = random.randrange(2 ** 16)

config = Config(seed=seed, verbosity=args.verbose, print_r=not args.no_r, cache_ops=args.cache_operations, optimal=args.optimal,
                solution_use_lines=args.use_lines, solution_use_last_line=args.use_last, advance_processes=args.split_search,
                static_search=args.static_search, programs_per_cube_threshold=args.split_search_threshold, minimum_loc=args.min_lines,
                maximum_loc=args.max_lines, max_filter_combinations=args.max_filter_combo, max_column_combinations=args.max_cols_combo,
                max_join_combinations=args.max_join_combo, program_weigth_decay_rate=args.decay_rate,
                block_commutative_ops=args.block_commutative_ops, subsume_conditions=args.subsume_conditions,
                probing_threads=args.probing_threads, cube_freedom=args.cube_freedom,
                z3_QF_FD=args.qffd, z3_sat_phase='random', disabled=args.disable)
util.store_config(config)

base_scores = defaultdict(int)
bigrams = defaultdict(lambda: defaultdict(int))

with open('instances.csv', 'w') as output:
    writer = csv.writer(output)
    writer.writerow(('name', 'loc'))
    for file in glob.glob('tests/**/*.yaml', recursive=True):
        test_name = file.replace('tests/', '', 1).replace('.yaml', '')
        try:
            spec_in = parse_specification(file)
            specification = Specification(spec_in)
            tyrell_spec = specification.generate_dsl()
            if 'solution' in spec_in:
                base_scores[spec_in['solution'][0]] += 1
                for p0, p1 in pairwise(spec_in['solution']):
                    bigrams[p0][p1] += 1
            loc = spec_in['loc'] if 'loc' in spec_in else None
            writer.writerow((test_name, loc))
        except:
            pass

print(base_scores)
print(bigrams)
