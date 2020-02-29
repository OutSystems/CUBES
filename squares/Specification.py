import csv
import re
from itertools import combinations, product
from typing import List, Tuple, Dict

import pandas
import yaml
from ordered_set import OrderedSet
from rpy2 import robjects

import squares.types
from squares import util, types
from squares.DSLBuilder import DSLFunction, DSLPredicate, DSLEnum, DSLBuilder, DSLValue
from squares.exceptions import SquaresException
from squares.util import next_counter, get_combinations
from tyrell.logger import get_logger

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


def read_table(path):
    df = pandas.read_csv(path)
    df = df.convert_dtypes(convert_integer=False)

    for col in df:  # try to coerce columns to datetime
        if all(squares.types.is_date(elem) for elem in df[col]):
            try:
                df[col] = pandas.to_datetime(df[col])
            except Exception:
                pass

    logger.info('Inferred data types for table %s: %s', path, str(list(map(str, df.dtypes.values))))

    return df


def parse_specification(filename):
    f = open(filename)

    spec = yaml.safe_load(f)

    if "inputs" not in spec:
        logger.error('Field "inputs" is required in spec')
        exit()

    if "output" not in spec:
        logger.error('Field "output" is required in spec')
        exit()

    for field in ["const", "aggrs", "attrs", "bools", 'filters']:
        if field not in spec:
            spec[field] = []

    if 'dateorder' not in spec:
        spec['dateorder'] = 'dmy'

    if 'loc' not in spec:
        spec['loc'] = 1

    return Specification(spec)


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


class Specification:

    def __init__(self, spec):
        self.inputs = spec['inputs']
        self.output = spec['output']
        self.consts = spec['const']
        if util.get_config().ignore_aggrs:
            self.aggrs = util.get_config().aggregation_functions
        else:
            self.aggrs = spec['aggrs']
        self.attrs = spec['attrs']
        self.has_int_consts = find_consts(self.consts)
        self.bools = spec['bools']
        self.dateorder = spec['dateorder']
        self.loc = spec['loc']
        self.filters = spec['filters']

        self.tables = []
        self.data_frames = {}

        self.columns = OrderedSet()
        self.generated_columns = {}
        self.columns_by_type = types.empty_type_map()
        self.types_by_const = {}
        self.consts_by_type = types.empty_type_map()

        logger.debug("Reading input files...")
        for input in self.inputs:
            id = next_counter()
            table_name = f'input{id}'
            self.tables.append(table_name)
            df = read_table(input)

            for column, type in df.dtypes.items():
                type = types.get_type(type)
                self.columns_by_type[type].add(column)

            self.data_frames[table_name] = df
            self.columns |= df.columns

        self.all_columns = self.columns.copy()

        self.data_frames['expected_output'] = read_table(self.output)

        for const in self.consts:
            self.types_by_const[const] = []

            for type in types.Type:
                if types.is_type(const, type):
                    self.types_by_const[const].append(type)
                    self.consts_by_type[type].append(const)

        self.generate_r_init()

        logger.debug("Generating DSL...")
        self.generate_dsl()

    def generate_r_init(self):  # TODO dirty: initializes R for the inputs
        self.r_init = 'con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")\n'

        for table, file in zip(self.tables, self.inputs):
            df = self.data_frames[table]
            self.r_init += exec_and_return(
                f'{table} <- read_csv("{file}", col_types = cols({types.get_r_types(df.dtypes)}))\n')

            for col, dtype in zip(df.columns, df.dtypes):  # parse dates
                if types.get_type(dtype) == types.DATETIME:
                    self.r_init += exec_and_return(f'{table}${col} <- {self.dateorder}({table}${col})\n')

            self.r_init += f'{table} <- copy_to(con, {table})\n'

        output_df = self.data_frames['expected_output']
        self.r_init += exec_and_return(
            f'expected_output <- read_csv("{self.output}", col_types = cols({types.get_r_types(output_df.dtypes)}))\n')
        for col, dtype in zip(output_df.columns, output_df.dtypes):  # parse dates
            if types.get_type(dtype) == types.DATETIME:
                self.r_init += exec_and_return(f'expected_output${col} <- {self.dateorder}(expected_output${col})\n')

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
        if 'natural_join' not in util.get_config().disabled:
            filters_p_one.insert(0, DSLPredicate('is_not_parent', ['natural_join', 'filter', '100']))
        if 'inner_join' not in util.get_config().disabled:
            filters_p_one.insert(0, DSLPredicate('is_not_parent', ['inner_join', 'filter', '100']))
        filters_p = filters_p_one
        filters_p_two = [DSLPredicate('distinct_filters', ['filters', '1', '2']),
                         DSLPredicate('is_not_parent', ['filters', 'filters', '100']),
                         DSLPredicate('distinct_inputs', ['filters'])]

        if 'natural_join4' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join4', 'filters', '100']))
        if 'natural_join3' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join3', 'filters', '100']))
        if 'natural_join' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['natural_join', 'filters', '100']))
        if 'inner_join' not in util.get_config().disabled:
            filters_p_two.insert(2, DSLPredicate('is_not_parent', ['inner_join', 'filters', '100']))

        summarise_f = [DSLFunction('summariseGrouped', 'Table r', ['Table a', 'SummariseCondition s', 'Cols b'], [
            'row(r) <= row(a)',
            'col(r) <= 3'
        ])]
        # summarise_p = []
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

        filter_conditions, summarise_conditions, on_conditions, predicates = self.find_conditions()

        if filters_f == [] and filter_conditions != []:
            filters_f = filters_f_one
            filters_p = [DSLPredicate('is_not_parent', ['filter', 'filter', '100'])]

        if len(filter_conditions) > 1:
            filters_f = filters_f_one + filters_f_and_or
            filters_p = [DSLPredicate('distinct_filters', ['filters', '1', '2']),
                         DSLPredicate('is_not_parent', ['filters', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['filter', 'filters', '100']),
                         DSLPredicate('is_not_parent', ['filter', 'filter', '100']),
                         DSLPredicate('is_not_parent', ['filters', 'filters', '100'])]
            operators = DSLEnum('Op', ['|', '&'])

        with open(self.output) as f:
            reader = csv.reader(f)
            output_attrs = next(reader)

        dsl = DSLBuilder('Squares', ['Table'] * len(self.inputs), 'TableSelect')
        dsl.add_enum(DSLEnum('Cols', get_combinations(self.columns, util.get_config().max_column_combinations)))
        dsl.add_enum(DSLEnum('Col', list(self.columns)))
        dsl.add_enum(DSLEnum('SelectCols', [', '.join(output_attrs)]))
        dsl.add_enum(DSLEnum('Distinct', ['distinct', '']))

        if 'inner_join' not in util.get_config().disabled:
            dsl.add_enum(DSLEnum('JoinCondition', on_conditions))

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
            dsl.add_function(DSLFunction('inner_join', 'Table r', ['Table a', 'Table b', 'JoinCondition c'],
                                         ["col(r) <= col(a) + col(b)"]))

        if 'anti_join' not in util.get_config().disabled:
            dsl.add_function(
                DSLFunction('anti_join', 'Table r', ['Table a', 'Table b'],
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

        for p in predicates:
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

    def find_filter_conditions(self, predicates=None):
        if predicates is None:
            predicates = []

        int_ops = ["==", ">", "<", ">=", "<="]
        str_ops = ["==", "!="]

        conditions = []
        frozen_columns = self.filter_columns(self.columns_by_type)

        for constant in self.consts_by_type[types.STRING]:
            current_predicate = []

            for column in frozen_columns[types.STRING]:
                if util.get_config().like_enabled:
                    conditions.append(f"str_detect({column}, '{constant}')")
                    current_predicate.append(f'"str_detect({column}, \'{constant}\')"')

                if self.constant_occurs(column, constant):
                    for op in str_ops:
                        conditions.append(f"{column} {op} '{constant}'")
                        current_predicate.append(f'"{column} {op} \'{constant}\'"')

            if current_predicate:
                predicates.append(DSLPredicate('constant_occurs', current_predicate))

        for constant in self.consts_by_type[types.INT] | self.consts_by_type[types.FLOAT]:
            current_predicate = []

            for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                for op in int_ops:
                    conditions.append(f'{column} {op} {constant}')
                    current_predicate.append(f'"{column} {op} {constant}"')
                    if column in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before',
                                         [f'"{column} {op} {constant}"', self.generated_columns[column]]))

            if current_predicate:
                predicates.append(DSLPredicate('constant_occurs', current_predicate))

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
            for attr2 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symm_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    conditions.append(f'{attr2} {op} {attr1}')
                    if attr1 in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before', [f'"{attr2} {op} {attr1}"', self.generated_columns[attr1]]))
                    if attr2 in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before', [f'"{attr2} {op} {attr1}"', self.generated_columns[attr2]]))

        if 'max(n)' in self.aggrs:
            raise SquaresException('max(n) is not a valid aggregator. Use a filter instead.')

        for filter in self.filters:
            current_predicate = []

            match = re.match('[a-zA-Z_][a-zA-Z_]*\(([a-zA-Z_][a-zA-Z_]*)\)', filter)
            if match:
                col = match[1]
                matching_types = []
                for type in types.Type:
                    if col in self.columns_by_type[type]:
                        matching_types.append(type)
                for type in matching_types:
                    for other_col in self.columns_by_type[type]:
                        conditions.append(f'{other_col} == {filter}')
                        current_predicate.append(f'"{other_col} == {filter}"')
                        if col in self.generated_columns.keys():
                            predicates.append(DSLPredicate('happens_before',
                                                           [f'"{other_col} == {filter}"', self.generated_columns[col]]))
                        if other_col in self.generated_columns.keys():
                            predicates.append(DSLPredicate('happens_before', [f'"{other_col} == {filter}"',
                                                                              self.generated_columns[other_col]]))

            if current_predicate:
                predicates.append(DSLPredicate('constant_occurs', current_predicate))

        return conditions, predicates

    def constant_occurs(self, column, constant):
        att = False
        for input in self.tables:
            data_frame = self.data_frames[input]
            if att:
                break

            if column in data_frame.columns:
                if constant in data_frame[column].values:
                    att = True
                    break
            else:
                continue
        return att

    def find_summarise_conditions(self, predicates=None):
        if predicates is None:
            predicates = []

        conditions = []
        frozen_columns = self.filter_columns(self.columns_by_type)

        for aggr in self.aggrs:
            current_predicate = []

            if "n" == aggr:
                conditions.append('n = n()')
                current_predicate.append('"n = n()"')
                self.all_columns.append('n')
                self.columns_by_type[types.INT].append('n')
                self.generated_columns['n'] = '"n = n()"'

            if aggr in ['concat']:
                for column in frozen_columns[types.STRING]:
                    conditions.append(f'paste|{column}')
                    current_predicate.append(f'"paste|{column}"')

            if aggr in ['min', 'max', 'mean', 'sum']:
                for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                    conditions.append(f'{aggr}{column} = {aggr}({column})')
                    current_predicate.append(f'"{aggr}{column} = {aggr}({column})"')
                    self.all_columns.append(f'{aggr}{column}')
                    self.columns_by_type[types.INT].append(f'{aggr}{column}')
                    self.columns_by_type[types.FLOAT].append(f'{aggr}{column}')  # todo
                    self.generated_columns[f'{aggr}{column}'] = f'"{aggr}{column} = {aggr}({column})"'
                    if column in self.generated_columns.keys():
                        predicates.append(DSLPredicate('happens_before', [f'"{aggr}{column} = {aggr}({column})"',
                                                                          self.generated_columns[column]]))

            if aggr in ['min', 'max']:
                for column in frozen_columns[types.DATETIME]:
                    conditions.append(f'{aggr}{column} = {aggr}({column})')
                    current_predicate.append(f'"{aggr}{column} = {aggr}({column})"')
                    self.all_columns.append(f'{aggr}{column}')
                    self.columns_by_type[types.DATETIME].append(f'{aggr}{column}')
                    self.generated_columns[f'{aggr}{column}'] = f'"{aggr}{column} = {aggr}({column})"'

            if util.get_config().force_summarise and current_predicate:
                predicates.append(DSLPredicate('constant_occurs', current_predicate))

        return conditions, predicates

    def find_inner_join_conditions(self, predicates=None) -> Tuple[List[str], List[Tuple]]:
        if predicates is None:
            predicates = []

        column_pairs = []
        for cols in self.columns_by_type.values():
            column_pairs += product(cols, repeat=2)

        on_conditions = list(combinations(column_pairs, 2)) + list(combinations(column_pairs, 1))
        on_conditions = list(filter(lambda t: any(map(lambda cond: cond[0] != cond[1], t)), on_conditions))

        str_conditions = []
        for on_condition in on_conditions:
            str_conditions.append(','.join(map(lambda x: f"'{x[0]}' = '{x[1]}'", on_condition)))

            for col_pairs in on_condition:  # we can only inner join after columns have been generated (by summarise)
                for col in col_pairs:
                    if col in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before', [f'"{str_conditions[-1]}"', self.generated_columns[col]]))

        return str_conditions, predicates

    def find_conditions(self):
        sum_cond, predicates = self.find_summarise_conditions()
        filt_cond, predicates = self.find_filter_conditions(predicates)

        if 'inner_join' not in util.get_config().disabled:
            on_conditions, predicates = self.find_inner_join_conditions(predicates)
        else:
            on_conditions = []

        return filt_cond, sum_cond, on_conditions, predicates

    def filter_columns(self, columns_map: Dict[types.Type, OrderedSet]) -> Dict[types.Type, OrderedSet]:
        d = {}

        for type in columns_map.keys():
            d[type] = OrderedSet()
            for column in columns_map[type]:
                if column in self.attrs or util.get_config().ignore_attrs or column in self.generated_columns.keys():
                    d[type].add(column)

        return d
