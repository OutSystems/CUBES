import csv
import random
from itertools import permutations

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


def parse_specification(filename, config):
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

    return Specification(spec["inputs"], spec["output"], spec["const"], spec["aggrs"], spec["attrs"], spec["bools"],
                         spec["loc"])


def add_is_not_parent_if_enabled(dsl, a, b):
    if a not in util.get_config().disabled and b not in util.get_config().disabled:
        dsl.add_predicate(DSLPredicate('is_not_parent', [a, b, '100']))


class Specification:

    def __init__(self, inputs, output, consts, aggrs, attrs, bools, loc):
        self.inputs = inputs
        self.output = output
        self.consts = consts
        self.aggrs = aggrs
        self.attrs = attrs
        self.has_int_consts = find_consts(consts)
        self.bools = bools
        self.loc = loc

        self.tables = []

        self._tables = {}
        self.columns = set()

        logger.debug("Reading input files...")
        for input in inputs:
            id = next_counter()
            self.tables.append(f'input{id}')
            self._tables[self.tables[-1]] = id

            with open(input) as f:
                reader = csv.reader(f)
                self.columns = self.columns.union(next(reader))

        self.columns = list(self.columns)
        self.columns.sort()
        if util.get_config().shuffle_cols:
            util.random.shuffle(self.columns)

        self._tables['expected_output'] = next_counter()

        self.generate_r_init()

        logger.debug("Generating DSL...")
        self.generate_dsl()

    def generate_r_init(self):  # TODO dirty: initializes R for the inputs
        self.r_init = 'con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")\n'

        for table, file in zip(self.tables, self.inputs):
            self.r_init += exec_and_return(f'{table} <- read.table("{file}", sep=",", header=T)\n')
            self.r_init += f'{table} <- copy_to(con, {table})\n'

        self.r_init += exec_and_return(f'expected_output <- read.table("{self.output}", sep =",", header=T)\n')

    def generate_dsl(self):
        filters_f_one = [DSLFunction('filter', 'Table r', ['Table a', 'FilterCondition f'], [
            'row(r) <= row(a)',
            'col(r) == col(a)'
        ])]
        filters_f = filters_f_one
        filters_f_and_or = [
            DSLFunction('filters', 'Table r', ['Table a', 'FilterCondition f', 'FilterCondition g', 'Op o'], [
                'row(r) <= row(a)',
                'col(r) == col(a)'
            ])]

        filters_p_one = [DSLPredicate('is_not_parent', ['inner_join3', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['inner_join4', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['filter', 'filter', '100']),
                         DSLPredicate('distinct_inputs', ['filter'])]
        filters_p = filters_p_one
        filters_p_two = [DSLPredicate('distinct_filters', ['filters', '1', '2']),
                         DSLPredicate('is_not_parent', ['filters', 'filters', '100']),
                         DSLPredicate('is_not_parent', ['inner_join', 'filters', '100']),
                         DSLPredicate('is_not_parent', ['inner_join3', 'filters', '100']),
                         DSLPredicate('is_not_parent', ['inner_join4', 'filters', '100']),
                         DSLPredicate('distinct_inputs', ['filters'])]

        summarise_f = [DSLFunction('summariseGrouped', 'Table r', ['Table a', 'SummariseCondition s', 'Cols b'], [
            'row(r) <= row(a)',
            'col(r) <= 3'
        ])]
        summarise_p = [DSLPredicate('is_not_parent', ['summariseGrouped', 'summariseGrouped', '100'])]
        if 'inner_join4' not in util.get_config().disabled:
            summarise_p.insert(0, DSLPredicate('is_not_parent', ['inner_join4', 'summariseGrouped', '100']))

        operators = None
        concat = None

        if self.consts:
            if len(self.consts) > 1:
                filters_f = filters_f_and_or
                filters_p = filters_p_two
                operators = DSLEnum('Op', ['|', '&'])
        else:
            filters_p, filters_f, self.consts = [], [], []

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
        dsl.add_enum(DSLEnum('Col', one_column))
        dsl.add_enum(DSLEnum('SelectCols', [','.join(output_attrs)]))
        dsl.add_enum(DSLEnum('Distinct', ['distinct', '']))

        if filter_conditions:
            dsl.add_enum(DSLEnum('FilterCondition', filter_conditions))

        if summarise_conditions:
            dsl.add_enum(DSLEnum('SummariseCondition', summarise_conditions))

        if operators:
            dsl.add_enum(operators)

        dsl.add_value(DSLValue('Table', [('col', 'int'), ('row', 'int')]))
        dsl.add_value(DSLValue('TableSelect', [('col', 'int'), ('row', 'int')]))

        if 'inner_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('inner_join', 'Table r', ['Table a', 'Table b'], ["col(r) <= col(a) + col(b)"]))

        if 'inner_join3' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('inner_join3', 'Table r', ['Table a', 'Table b', 'Table c'],
                            ["col(r) < col(a) + col(b) + col(c)"]))

        if 'inner_join4' not in util.get_config().disabled:
            dsl.add_function(DSLFunction('inner_join4', 'Table r', ['Table a', 'Table b', 'Table c', 'Table d'],
                                         ["col(r) < col(a) + col(b) + col(c) + col(d)"]))

        dsl.add_function(
            DSLFunction('anti_join', 'Table r', ['Table a', 'Table b', 'Col c'], ["col(r) == 1", 'row(r) <= row(a)']))
        dsl.add_function(
            DSLFunction('left_join', 'Table r', ['Table a', 'Table b'],
                        ['col(r) <= col(a) + col(b)', 'row(r) == row(a)']))

        dsl.add_function(
            DSLFunction('bind_rows', 'Table r', ['Table a', 'Table b'],
                        ['col(r) <= col(a) + col(b)', 'row(r) == row(a) + row(b)']))

        dsl.add_function(
            DSLFunction('intersect', 'Table r', ['Table a', 'Table b', 'Col c'], ['col(r) == 1', 'row(r) <= row(a)']))

        if 'semi_join' not in util.get_config().disabled:
            dsl.add_function(DSLFunction('semi_join', 'Table r', ['Table a', 'Table b'], ['col(r) == col(a)', 'row(r) <= row(a)']))

        dsl.add_function(DSLFunction('select', 'TableSelect r', ['Table a', 'SelectCols c', 'Distinct d'],
                                     ['row(r) <= row(a)', 'col(r) <= col(a)']))

        if concat:
            dsl.add_function(concat)

        for f in filters_f + summarise_f:
            dsl.add_function(f)

        for p in summarise_p + filters_p + necessary_conditions:
            dsl.add_predicate(p)

        add_is_not_parent_if_enabled(dsl, 'inner_join', 'inner_join3')
        add_is_not_parent_if_enabled(dsl, 'inner_join', 'inner_join4')
        # add_is_not_parent_if_enabled(dsl, self._config, 'inner_join', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'inner_join3', 'inner_join')
        add_is_not_parent_if_enabled(dsl, 'inner_join3', 'inner_join3')
        add_is_not_parent_if_enabled(dsl, 'inner_join3', 'inner_join4')
        add_is_not_parent_if_enabled(dsl, 'inner_join3', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'inner_join4', 'inner_join')
        add_is_not_parent_if_enabled(dsl, 'inner_join4', 'inner_join3')
        add_is_not_parent_if_enabled(dsl, 'inner_join4', 'inner_join4')
        # add_is_not_parent_if_enabled(dsl, self._config, 'inner_join4', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'anti_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'inner_join')
        add_is_not_parent_if_enabled(dsl, 'anti_join', 'inner_join4')

        for join in ['inner_join4', 'inner_join3', 'inner_join', 'anti_join']:
            if join not in util.get_config().disabled:
                dsl.add_predicate(DSLPredicate('distinct_inputs', [join]))

        self.dsl = dsl

    def find_filter_conditions(self, str_const, int_const, str_attr, int_attr, new_int_attr,
                               necessary_conditions,
                               summarise_conditions):
        conditions = []
        int_ops = ["==", ">", "<", ">=", "<="]
        str_ops = ["==", "!="]
        happens_before = []

        for constant in str_const + int_const:
            necessary_conditions.append([])
            for str_attribute in str_attr:
                att = False
                for input in self.inputs:
                    if att:
                        break
                    with open(input) as f:
                        reader = csv.reader(f)
                        columns = next(reader)
                        if str_attribute in columns:
                            ind = columns.index(str_attribute)
                            for row in reader:
                                if row[ind] == constant:
                                    att = True
                                    break
                        else:
                            continue

                if 'like' in self.aggrs:
                    conditions.append(f'str_detect({str_attribute}|{constant})')
                    necessary_conditions[-1].append(conditions[-1])

                if not att:
                    continue
                for so in str_ops:
                    conditions.append(f'{str_attribute} {so} {constant}')
                    necessary_conditions[-1].append(conditions[-1])

        for int_constant in int_const:
            necessary_conditions.append([])
            for int_attribute in int_attr + new_int_attr:
                if int_constant == int_attribute:
                    continue
                for io in int_ops:
                    conditions.append(f'{int_attribute} {io} {int_constant}')
                    necessary_conditions[-1].append(conditions[-1])
                    if int_attribute == "n":
                        happens_before.append((conditions[-1], 'n = n()'))

        for int_constant in new_int_attr:
            for int_attribute in int_attr + new_int_attr:
                if int_constant == int_attribute:
                    continue
                for io in int_ops:
                    conditions.append('{ia} {io} {ic}'.format(ia=int_attribute, io=io, ic=int_constant))
                    for constant in summarise_conditions:
                        if int_constant in constant:
                            happens_before.append((conditions[-1], constant))

        necessary_conditions = list(filter(lambda a: a != [], necessary_conditions))
        # if "max" in aggrs and "n" in aggrs or "max(n)" in aggrs:
        if 'max(n)' in self.aggrs:
            conditions.append('n == max(n)')
            happens_before.append((conditions[-1], 'n = n()'))
            necessary_conditions.append([conditions[-1]])

        return conditions, necessary_conditions, happens_before

    def find_summarise_conditions(self, int_attr, str_attr, necessary_conditions):
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
            if "max(n)" == a:
                continue
            for ia in int_attr:
                conditions.append(f'{a}{ia} = {a}({ia})')
                necessary_conditions[-1].append(conditions[-1])
                new_int_attr.append(f'{a}{ia}')
        return list(filter(lambda x: x != [], necessary_conditions)), new_int_attr, conditions

    def find_conditions(self):
        necessary_conditions = []
        str_const, int_const = self.divide_int_str_constants()
        str_attr, int_attr = self.divide_int_str_attributes()

        necessary_conditions, new_int_attr, sum_cond = self.find_summarise_conditions(int_attr, str_attr,
                                                                                      necessary_conditions)

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
