import csv
import random
from itertools import permutations, combinations
from typing import List, Tuple

import pandas
import yaml
import logging

from rpy2 import robjects

from squares import util
from squares.DSLBuilder import DSLFunction, DSLPredicate, DSLEnum, DSLBuilder, DSLValue
from tyrell.logger import get_logger

from squares.util import next_counter, get_permutations

logger = get_logger('squares')


def find_consts(consts):
    for c in consts:
        try:
            int(c)
            return True
        except:
            pass

    return False


def exec_and_return(r_script):
    robjects.r(r_script)
    return r_script


def parse_specification(filename):
    f = open(filename)

    spec = yaml.safe_load(f)

    if "inputs" not in spec:
        logger.error('Field "inputs" is required in spec')
        exit()

    if "output" not in spec:
        logger.error('Field "output" is required in spec')
        exit()

    for field in ["const", "aggrs", "attrs", "bools"]:
        if field not in spec:
            spec[field] = []

    if 'loc' not in spec:
        spec['loc'] = 1

    return Specification(spec["inputs"], spec["output"], spec["const"], spec["aggrs"], spec["attrs"], spec["bools"],
                         spec["loc"])


def add_is_not_parent_if_enabled(dsl, a, b):
    if a not in util.get_config().disabled and b not in util.get_config().disabled:
        dsl.add_predicate(DSLPredicate('is_not_parent', [a, b, '100']))


def symm_op(op: str):
    if op == '>':
        return '<'
    elif op == '<':
        return '>'
    elif op == '>=':
        return '<='
    elif op == '<=':
        return '>='
    elif op == '==':
        return '=='


def find_pairs(cols: List[str]) -> List[Tuple[List, List]]:
    cols = list(map(lambda x: list(map(lambda y: y.strip(), x.split(','))), cols))  # TODO assumes col doesn't have withspace
    for c1, c2 in combinations(cols, 2):
        if len(c1) == len(c2) and c1 != c2:
            yield c1, c2


def generate_on_conditions(conditions: List[Tuple[List, List]]) -> List[str]:
    return list(map(lambda t: ','.join(map(lambda t_: f"'{t_[0]}' = '{t_[1]}'", zip(t[0], t[1]))), conditions))


class Specification:

    def __init__(self, inputs, output, consts, aggrs, attrs, bools, loc):
        self.inputs = inputs
        self.output = output
        self.consts = consts
        if util.get_config().ignore_aggrs:
            self.aggrs = util.get_config().aggregation_functions
        else:
            self.aggrs = aggrs
        self.attrs = attrs
        self.has_int_consts = find_consts(consts)
        self.bools = bools
        self.loc = loc

        self.tables = []

        self.data_frames = []

        self._tables = {}
        self.columns = set()

        logger.debug("Reading input files...")
        for input in inputs:
            id = next_counter()

            self.tables.append(f'input{id}')
            self._tables[self.tables[-1]] = id

            df = pandas.read_csv(input)

            self.data_frames.append(df)
            self.columns |= set(df.columns)

        self.columns = list(self.columns)
        self.columns.sort()
        if util.get_config().shuffle_cols:
            util.random.shuffle(self.columns)

        self.all_columns = self.columns.copy()

        self._tables['expected_output'] = next_counter()

        self.generate_r_init()

        logger.debug("Generating DSL...")
        self.generate_dsl()

    def generate_r_init(self):  # TODO dirty: initializes R for the inputs
        self.r_init = 'con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")\n'

        for table, file in zip(self.tables, self.inputs):
            self.r_init += exec_and_return(f'{table} <- read.csv("{file}")\n')
            self.r_init += f'{table} <- copy_to(con, {table})\n'

        self.r_init += exec_and_return(f'expected_output <- read.table("{self.output}", sep =",", header=T)\n')

    def generate_dsl(self):
        filters_f_one = [DSLFunction('filter', 'Table r', ['Table a', 'FilterCondition f'], [
            'row(r) <= row(a)',
            'col(r) == col(a)',
            # 'columns(r) == columns(a)'
        ])]
        filters_f = filters_f_one
        filters_f_and_or = [
            DSLFunction('filters', 'Table r', ['Table a', 'FilterCondition f', 'FilterCondition g', 'Op o'], [
                'row(r) <= row(a)',
                'col(r) == col(a)',
                # 'columns(r) == columns(a)'
            ])]

        filters_p_one = [DSLPredicate('is_not_parent', ['filter', 'filter', '100']),
                         DSLPredicate('distinct_inputs', ['filter'])]
        if 'natural_join4' not in util.get_config().disabled:
            filters_p_one.insert(0, DSLPredicate('is_not_parent', ['natural_join4', 'filter', '100']))
        if 'natural_join3' not in util.get_config().disabled:
            filters_p_one.insert(0, DSLPredicate('is_not_parent', ['natural_join3', 'filter', '100']))
        filters_p = filters_p_one
        filters_p_two = [DSLPredicate('distinct_filters', ['filters', '1', '2']),
                         DSLPredicate('is_not_parent', ['filters', 'filters', '100']),
                         DSLPredicate('distinct_inputs', ['filters'])]

        if 'natural_join4' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join4', 'filter', '100']))
        if 'natural_join3' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join3', 'filter', '100']))
        if 'natural_join' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join', 'filter', '100']))

        summarise_f = [DSLFunction('summariseGrouped', 'Table r', ['Table a', 'SummariseCondition s', 'Cols b'], [
            'row(r) <= row(a)',
            'col(r) <= 3'
        ])]
        summarise_p = [DSLPredicate('is_not_parent', ['summariseGrouped', 'summariseGrouped', '100'])]
        if 'natural_join4' not in util.get_config().disabled:
            summarise_p.insert(0, DSLPredicate('is_not_parent', ['natural_join4', 'summariseGrouped', '100']))

        operators = None
        concat = None

        if self.consts:
            if len(self.consts) > 1:
                filters_f = filters_f_and_or
                filters_p = filters_p_two
                operators = DSLEnum('Op', ['|', '&'])
        else:
            filters_p, filters_f = [], []

        if self.aggrs:
            for a in self.aggrs:
                if a == 'concat':
                    self.aggrs.remove(a)
                    concat = DSLFunction('unite', 'Table r', ['Table a', 'Col c', 'Col d'], [
                        'row(r) <= row(a)',
                        'col(r) <= col(a)'
                    ])
            if len(self.aggrs) == 1 and "like" in self.aggrs:
                summarise_f, summarise_p = [], []
        else:
            summarise_f, summarise_p = [], []

        if ['max(n)'] == self.aggrs:
            self.consts.append('max(n)')
            self.aggrs.remove('max(n)')

        if self.attrs:
            self.attrs = (self.attrs + ["n"]) if "n" in self.aggrs and self.has_int_consts else self.attrs
        elif 'n' in self.aggrs:
            self.attrs.append('n')

        filter_conditions, summarise_conditions, necessary_conditions, happens_before = self.find_conditions()

        if filters_f == [] and filter_conditions != []:
            filters_f = filters_f_one
            filters_p = [DSLPredicate('is_not_parent', ['filter', 'filter', '100'])]

        if len(necessary_conditions) > 1:
            filters_f = filters_f_one + filters_f_and_or
            filters_p = [DSLPredicate('distinct_filters', ['filters', '1', '2']),
                         DSLPredicate('is_not_parent', ['filters', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['filter', 'filters', '100']),
                         DSLPredicate('is_not_parent', ['filter', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['filters', 'filters', '100'])]
            operators = DSLEnum('Op', ['|', '&'])

        necessary_conditions = self.find_necessary_conditions(necessary_conditions)
        necessary_conditions += self.happens_before(happens_before)

        with open(self.output) as f:
            reader = csv.reader(f)
            output_attrs = next(reader)

        cols = get_permutations(self.columns, 2)
        one_column = get_permutations(self.columns, 1)

        dsl = DSLBuilder('Squares', ['Table'] * len(self.inputs), 'TableSelect')
        dsl.add_enum(DSLEnum('Cols', cols))
        dsl.add_enum(DSLEnum('OnCondition', generate_on_conditions(find_pairs(get_permutations(self.all_columns, 2)))))
        dsl.add_enum(DSLEnum('Col', one_column))
        dsl.add_enum(DSLEnum('SelectCols', [','.join(output_attrs)]))
        dsl.add_enum(DSLEnum('Distinct', ['distinct', '']))

        if filter_conditions:
            dsl.add_enum(DSLEnum('FilterCondition', filter_conditions))

        if summarise_conditions:
            dsl.add_enum(DSLEnum('SummariseCondition', summarise_conditions))

        if operators:
            dsl.add_enum(operators)

        dsl.add_value(DSLValue('Table', [('col', 'int'), ('row', 'int'),
                                         # ('columns', 'bv')
                                         ]))
        dsl.add_value(DSLValue('TableSelect', [('col', 'int'), ('row', 'int'),
                                               # ('columns', 'bv')
                                               ]))

        if 'natural_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('natural_join', 'Table r', ['Table a', 'Table b'], ["col(r) <= col(a) + col(b)",
                                                                                # "columns(r) == columns(a) | columns(b)",
                                                                                # f"columns(a) & columns(b) != *0"
                                                                                ]))

        if 'natural_join3' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('natural_join3', 'Table r', ['Table a', 'Table b', 'Table c'],
                            ["col(r) < col(a) + col(b) + col(c)",
                             # "columns(r) == columns(a) | columns(b) | columns(c)",
                             # f"columns(a) & columns(b) != *0",
                             # f"(columns(a) | columns(b)) & columns(c) != *0"
                             ]))

        if 'natural_join4' not in util.get_config().disabled:
            dsl.add_function(DSLFunction('natural_join4', 'Table r', ['Table a', 'Table b', 'Table c', 'Table d'],
                                         ["col(r) < col(a) + col(b) + col(c) + col(d)",
                                          # "columns(r) == columns(a) | columns(b) | columns(c) | columns(d)",
                                          # f"columns(a) & columns(b) != *0",
                                          # f"(columns(a) | columns(b)) & columns(c) != *0",
                                          # f"(columns(a) | columns(b) | columns(c)) & columns(d) != *0"
                                          ]))

        if 'inner_join' not in util.get_config().disabled:
            dsl.add_function(DSLFunction('inner_join', 'Table r', ['Table a', 'Table b', 'OnCondition c'],
                                         ["col(r) <= col(a) + col(b)"]))

        if 'anti_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('anti_join', 'Table r', ['Table a', 'Table b', 'Col c'],
                            ["col(r) == 1", 'row(r) <= row(a)']))

        if 'left_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('left_join', 'Table r', ['Table a', 'Table b'],
                            ['col(r) <= col(a) + col(b)', 'row(r) == row(a)',
                             # "columns(r) == columns(a) | columns(b)"
                             ]))

        if 'bind_rows' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('bind_rows', 'Table r', ['Table a', 'Table b'],
                            ['col(r) <= col(a) + col(b)', 'row(r) == row(a) + row(b)',
                             # "columns(r) == columns(a) | columns(b)"
                             ]))

        if 'intersect' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('intersect', 'Table r', ['Table a', 'Table b', 'Col c'],
                            ['col(r) == 1', 'row(r) <= row(a)']))

        if 'semi_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('semi_join', 'Table r', ['Table a', 'Table b'], ['col(r) == col(a)', 'row(r) <= row(a)']))

        dsl.add_function(DSLFunction('select', 'TableSelect r', ['Table a', 'SelectCols c', 'Distinct d'],
                                     ['row(r) <= row(a)', 'col(r) <= col(a)',
                                      # f"columns(r) & columns(a) != *0",
                                      # f"columns(r) & ~columns(a) == *0",
                                      ]))

        if concat:
            dsl.add_function(concat)

        for f in filters_f + summarise_f:
            dsl.add_function(f)

        for p in summarise_p + filters_p:
            dsl.add_predicate(p)

        for p in necessary_conditions:
            dsl.add_predicate(p)

        add_is_not_parent_if_enabled(dsl, 'natural_join', 'natural_join3')
        add_is_not_parent_if_enabled(dsl, 'natural_join', 'natural_join4')
        # add_is_not_parent_if_enabled(dsl, self._config, 'natural_join', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'natural_join3', 'natural_join')
        add_is_not_parent_if_enabled(dsl, 'natural_join3', 'natural_join3')
        add_is_not_parent_if_enabled(dsl, 'natural_join3', 'natural_join4')
        add_is_not_parent_if_enabled(dsl, 'natural_join3', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'natural_join4', 'natural_join')
        add_is_not_parent_if_enabled(dsl, 'natural_join4', 'natural_join3')
        add_is_not_parent_if_enabled(dsl, 'natural_join4', 'natural_join4')
        # add_is_not_parent_if_enabled(dsl, self._config, 'natural_join4', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'natural_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'natural_join4')

        for join in ['natural_join4', 'natural_join3', 'natural_join', 'anti_join']:
            if join not in util.get_config().disabled:
                dsl.add_predicate(DSLPredicate('distinct_inputs', [join]))

        self.dsl = dsl

    def find_filter_conditions(self, str_const, int_const, str_attr, int_attr, new_int_attr,
                               necessary_conditions: List[List[str]],
                               summarise_conditions):
        conditions = []
        int_ops = ["==", ">", "<", ">=", "<="]
        str_ops = ["==", "!="]
        happens_before = []

        for constant in str_const + int_const:
            necessary_conditions.append([])

            for str_attribute in str_attr:

                if 'like' in util.get_config().aggregation_functions:
                    conditions.append(f'str_detect({str_attribute}|{constant})')
                    necessary_conditions[-1].append(conditions[-1])

                att = False
                for data_frame in self.data_frames:  # check if constant appears in column 'str_attribute' in some data_frame
                    if att:
                        break

                    if str_attribute in data_frame.columns:
                        for row in data_frame.itertuples():
                            if getattr(row, str_attribute) == constant:
                                att = True
                                break
                    else:
                        continue

                if att:
                    for op in str_ops:
                        conditions.append(f'{str_attribute} {op} {constant}')
                        necessary_conditions[-1].append(conditions[-1])

        for int_constant in int_const:
            necessary_conditions.append([])

            for int_attribute in int_attr + new_int_attr:
                if int_constant == int_attribute:
                    continue

                for op in int_ops:
                    conditions.append(f'{int_attribute} {op} {int_constant}')
                    necessary_conditions[-1].append(conditions[-1])
                    if int_attribute == "n":
                        happens_before.append((conditions[-1], 'n = n()'))

        bc = set()

        for attr1 in new_int_attr:
            for attr2 in int_attr + new_int_attr:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symm_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue

                    bc.add((attr2, op, attr1))
                    conditions.append('{ia} {io} {ic}'.format(ia=attr2, io=op, ic=attr1))
                    for constant in summarise_conditions:
                        if attr1 in constant:
                            happens_before.append((conditions[-1], constant))

        necessary_conditions = list(filter(lambda a: a != [], necessary_conditions))
        # if "max" in aggrs and "n" in aggrs or "max(n)" in aggrs:
        if 'max(n)' in self.aggrs:
            conditions.append('n == max(n)')
            happens_before.append((conditions[-1], 'n = n()'))
            if util.get_config().force_occurs_maxn:
                necessary_conditions.append([conditions[-1]])

        return conditions, necessary_conditions, happens_before

    def find_summarise_conditions(self, int_attr, str_attr, necessary_conditions: List[List[str]]):
        conditions = []
        new_int_attr = []

        for a in self.aggrs:
            if a == "like":
                continue

            necessary_conditions.append([])

            if "n" == a:
                conditions.append(f'{a} = {a}()')
                necessary_conditions[-1].append(conditions[-1])
                continue

            if 'concat' in a:
                for at in int_attr + str_attr:
                    conditions.append(f'paste|{at}')
                    necessary_conditions[-1].append(conditions[-1])
                continue

            if "max(n)" == a:  # special case: where max(n) == something without grouping
                continue

            for ia in int_attr:
                conditions.append(f'{a}{ia} = {a}({ia})')
                necessary_conditions[-1].append(conditions[-1])
                new_int_attr.append(f'{a}{ia}')
                self.all_columns.append(f'{a}{ia}')

        return list(filter(lambda x: x != [], necessary_conditions)), new_int_attr, conditions

    def find_conditions(self):
        necessary_conditions = []
        str_const, int_const = self.divide_int_str_constants()
        str_attr, int_attr = self.divide_int_str_attributes()

        necessary_conditions, new_int_attr, sum_cond = self.find_summarise_conditions(int_attr, str_attr,
                                                                                      necessary_conditions)

        if not util.get_config().force_summarise:
            necessary_conditions = []

        filt_cond, necessary_conditions, happens_before = self.find_filter_conditions(str_const, int_const, str_attr,
                                                                                      int_attr,
                                                                                      new_int_attr,
                                                                                      necessary_conditions, sum_cond)
        self.attributes = int_attr + new_int_attr
        return filt_cond, sum_cond, necessary_conditions, happens_before

    def divide_int_str_constants(self):
        str_const, int_const = [], []
        for c in self.consts:
            try:
                int(c)
                int_const.append(c)
            except:
                str_const.append(c)
        return str_const, int_const

    def divide_int_str_attributes(self):
        str_attr, int_attr = [], []
        for a in self.attrs:
            if a == "n":
                if a not in int_attr:
                    int_attr.append(a)
            for input in self.inputs:
                with open(input) as f:
                    reader = csv.reader(f)
                    columns = next(reader)
                    if a in columns:
                        ind = columns.index(a)
                        line = next(reader)
                        try:  # if int
                            int(line[ind])
                            if a not in int_attr:
                                int_attr.append(a)
                        except:
                            if a not in str_attr:
                                str_attr.append(a)
        return str_attr, int_attr

    @staticmethod
    def find_necessary_conditions(conds):
        if not util.get_config().force_constants:
            return []

        predicates = []
        for c in conds:
            if c == []:
                break
            predicates.append(DSLPredicate('constant_occurs', ['"' + ','.join(map(str, c)) + '"']))
        return predicates

    @staticmethod
    def happens_before(conds):
        predicates = []
        for c in conds:
            if c == ():
                break
            predicates.append(DSLPredicate('happens_before', ['"' + str(c[0]) + '"', '"' + str(c[1]) + '"']))
        return predicates
