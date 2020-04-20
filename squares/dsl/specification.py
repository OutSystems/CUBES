import re
from collections import defaultdict
from itertools import combinations, product
from typing import List, Tuple, Dict

import pandas
from ordered_set import OrderedSet
from rpy2 import robjects

from squares import util, types
from squares.dsl import dsl_library
from squares.dsl.dsl_builder import DSLPredicate, DSLEnum, DSLBuilder
from squares.exceptions import SquaresException
from squares.tyrell.logger import get_logger
from squares.util import next_counter, get_combinations, powerset_except_empty

logger = get_logger('squares')


def exec_and_return(r_script):
    robjects.r(r_script)
    return r_script


def read_table(path):
    df = pandas.read_csv(path)
    df = df.convert_dtypes(convert_integer=False)

    for col in df:  # try to coerce columns to datetime
        if types.get_type(df[col].dtype) == types.INT:
            for elem in df[col]:
                if elem >= 2 ** 32 - 1:
                    logger.warning('Using integers larger than 32 bits! Converting column %s to string.', col)
                    df[col] = df[col].astype(str)
                    break

        if '-' in col:
            logger.error('Column names should be valid identifiers.')
            raise ValueError(f'Columns names should be valid identifiers. Column {col} in table {path}.')

        if all(types.is_time(elem) or pandas.isna(elem) for elem in df[col]) and any(types.is_time(elem) for elem in df[col]):
            try:
                df[col] = pandas.to_timedelta(df[col], errors='coerce')
            except Exception:
                pass

        elif all(types.is_date(elem) or pandas.isna(elem) for elem in df[col]) and any(types.is_date(elem) for elem in df[col]):
            try:
                df[col] = pandas.to_datetime(df[col], errors='coerce')
            except Exception:
                pass

    logger.info('Inferred data types for table %s: %s', path, str(list(map(str, df.dtypes.values))))

    return df


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
        self.dateorder = spec['dateorder']
        self.filters = spec['filters']
        if 'solution' in spec:
            self.solution = spec['solution']
        else:
            self.solution = None

        self.min_loc = max((len(self.aggrs) if util.get_config().force_summarise else 0) + (1 if self.filters or self.consts else 0),
                           util.get_config().minimum_loc)  # TODO

        self.tables = []
        self.data_frames = {}

        self.columns = OrderedSet()
        self.generated_columns = {}
        self.columns_by_type = types.empty_type_map()
        self.types_by_const = {}
        self.consts_by_type = types.empty_type_map()

        if 'max(n)' in self.aggrs:
            raise SquaresException('max(n) is not a valid aggregator. Use a filter instead.')

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

        self.columns = OrderedSet(sorted(self.columns))
        self.all_columns = self.columns.copy()

        self.data_frames['expected_output'] = read_table(self.output)
        self.output_cols = self.data_frames['expected_output'].columns

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

        if 'concat' in self.aggrs:
            self.r_init += exec_and_return(
                '\nstring_agg <- function(v,s) {Reduce(function(x, y) paste(x, y, sep = s), v)}\n')
        if 'mode' in self.aggrs:
            self.r_init += exec_and_return(
                '\nmode <- function(x) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}\n')

    def generate_dsl(self):
        filter_conditions, summarise_conditions, join_conditions, predicates = self.find_conditions()

        dsl = DSLBuilder('Squares', ['Table'] * len(self.inputs), 'Table')

        if summarise_conditions:
            dsl.add_enum(DSLEnum('Cols', get_combinations(self.columns, util.get_config().max_column_combinations)))

        if 'intersect' not in util.get_config().disabled:
            dsl.add_enum(DSLEnum('Col', list(self.columns)))

        if 'anti_join' not in util.get_config().disabled:
            dsl.add_enum(DSLEnum('JoinCols',
                                 [''] + get_combinations([f"'{col}'" for col in self.columns], util.get_config().max_column_combinations)))

        if 'inner_join' not in util.get_config().disabled and join_conditions:
            dsl.add_enum(DSLEnum('JoinCondition', join_conditions))

        if filter_conditions:
            dsl.add_enum(DSLEnum('FilterCondition', filter_conditions))

        if summarise_conditions:
            dsl.add_enum(DSLEnum('SummariseCondition', summarise_conditions))

        if util.get_config().filters_function_enabled and len(filter_conditions) > 1:
            dsl.add_enum(DSLEnum('Op', ['|', '&']))

        dsl.add_value(dsl_library.table_value)

        if 'natural_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join_function)

        if 'natural_join3' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join3_function)

        if 'natural_join4' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join4_function)

        if 'inner_join' not in util.get_config().disabled and join_conditions:
            dsl.add_function(dsl_library.inner_join_function)

        if 'anti_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.anti_join_function)

        if 'left_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.left_join_function)

        if 'union' not in util.get_config().disabled:
            dsl.add_function(dsl_library.union_function)

        if 'intersect' not in util.get_config().disabled:
            dsl.add_function(dsl_library.intersect_function)

        if 'semi_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.semi_join_function)

        if 'concat' in self.aggrs:
            dsl.add_function(dsl_library.unite_function)

        if filter_conditions:
            dsl.add_function(dsl_library.filter_function)

            if util.get_config().filters_function_enabled and len(filter_conditions) > 1:
                dsl.add_function(dsl_library.filters_function)

        if summarise_conditions:
            dsl.add_function(dsl_library.summarise_grouped_function)

        if summarise_conditions:
            add_is_not_parent_if_enabled(dsl, 'natural_join4', 'summarise')
            dsl.add_predicate(DSLPredicate('is_not_parent', ['summarise', 'summarise', '100']))

        if len(filter_conditions) == 1:
            add_is_not_parent_if_enabled(dsl, 'inner_join', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join3', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join4', 'filter')
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('distinct_inputs', ['filter']))
        elif len(filter_conditions) > 1 and util.get_config().filters_function_enabled:
            dsl.add_predicate(DSLPredicate('distinct_filters', ['filters', '1', '2']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filters', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filters', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filters', 'filters', '100']))

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
                # frozen_columns[types.INT].append('n')
                self.generated_columns['n'] = '"n = n()"'

            if aggr == 'concat':
                for column in frozen_columns[types.STRING]:
                    for separator in ['', ' ', ',', ', ']:
                        conditions.append(f"{aggr}{column} = string_agg({column}, '{separator}')")
                        current_predicate.append(f"\"{aggr}{column} = string_agg({column}, '{separator}')\"")
                    self.all_columns.append(f'{aggr}{column}')
                    self.columns_by_type[types.STRING].append(f'{aggr}{column}')
                    self.generated_columns[f'{aggr}{column}'] = f"\"{aggr}{column} = string_agg({column}, '{separator}')\""
                    # TODO generated columns (both ways). as it is it overrides

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
                for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                    conditions.append(f'{column} = {aggr}({column})')
                    current_predicate.append(f'"{column} = {aggr}({column})"')
                    self.all_columns.append(f'{column}')
                    self.columns_by_type[types.INT].append(f'{column}')
                    self.columns_by_type[types.FLOAT].append(f'{column}')  # todo
                    # self.generated_columns[f'{column}'] = f'"{column} = {aggr}({column})"'  # TODO
                    if column in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before', [f'"{column} = {aggr}({column})"', self.generated_columns[column]]))

            if aggr in ['mode']:
                for column in self.attrs:
                    conditions.append(f'{aggr}{column} = {aggr}({column})')
                    current_predicate.append(f'"{aggr}{column} = {aggr}({column})"')
                    self.all_columns.append(f'{aggr}{column}')
                    # self.columns_by_type[].append(f'{aggr}{column}') # todo
                    # self.columns_by_type[types.FLOAT].append(f'{aggr}{column}')
                    self.generated_columns[f'{aggr}{column}'] = f'"{aggr}{column} = {aggr}({column})"'
                    if column in self.generated_columns.keys():
                        predicates.append(
                            DSLPredicate('happens_before', [f'"{aggr}{column} = {aggr}({column})"', self.generated_columns[column]]))

            if aggr in ['min', 'max']:
                for column in frozen_columns[types.DATETIME] | frozen_columns[types.TIME]:
                    conditions.append(f'{aggr}{column} = {aggr}({column})')
                    current_predicate.append(f'"{aggr}{column} = {aggr}({column})"')
                    self.all_columns.append(f'{aggr}{column}')
                    self.columns_by_type[types.DATETIME].append(f'{aggr}{column}')
                    self.generated_columns[f'{aggr}{column}'] = f'"{aggr}{column} = {aggr}({column})"'

            if aggr == 'first':
                conditions.append('all$first')
                current_predicate.append('"all$first"')

            if util.get_config().force_summarise and current_predicate:
                predicates.append(DSLPredicate('constant_occurs', current_predicate))

        return conditions, predicates

    def find_filter_conditions(self, predicates=None):
        if predicates is None:
            predicates = []

        int_ops = ['==', '>', '<', '>=', '<=']
        str_ops = ['==', '!=']

        filter_parts = defaultdict(OrderedSet)
        frozen_columns = self.filter_columns(self.columns_by_type)

        for constant in self.consts_by_type[types.NONE]:
            for column in frozen_columns[types.STRING] | frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                filter_parts[frozenset((column, constant))].add(f"is.na({column})")
                filter_parts[frozenset((column, constant))].add(f"!is.na({column})")

        for constant in self.consts_by_type[types.STRING]:
            for column in frozen_columns[types.STRING]:
                if 'str_detect' in self.filters or 'like' in self.filters:
                    filter_parts[frozenset((column, constant))].add(f"str_detect({column}, {types.to_r_repr(constant)})")

                if self.constant_occurs(column, constant):
                    for op in str_ops:
                        filter_parts[frozenset((column, constant))].add(f"{column} {op} {types.to_r_repr(constant)}")

        for constant in self.consts_by_type[types.INT] | self.consts_by_type[types.FLOAT]:
            for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                for op in int_ops:
                    filter_parts[frozenset((column, constant))].add(f'{column} {op} {constant}')

        for constant in self.consts_by_type[types.DATETIME]:
            for column in frozen_columns[types.DATETIME]:
                for op in int_ops:
                    filter_parts[frozenset((column, constant))].add(f"{column} {op} {self.dateorder}('{constant}')")

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
            for attr2 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symm_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.DATETIME]:
            for attr2 in frozen_columns[types.DATETIME]:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symm_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')

        for filter in self.filters:
            match = re.match(r'[a-zA-Z_][a-zA-Z_]*\(([a-zA-Z_][a-zA-Z_]*)\)', filter)
            if match:
                col = match[1]
                matching_types = []
                for type in types.Type:
                    if col in self.columns_by_type[type]:
                        matching_types.append(type)
                for type in matching_types:
                    for other_col in self.columns_by_type[type]:
                        filter_parts[frozenset((col, other_col, filter))].add(f'{other_col} == {filter}')
                        filter_parts[frozenset((col, other_col, filter))].add(f'{other_col} != {filter}')

        conditions = []
        condition_map = defaultdict(OrderedSet)

        for n in range(1, util.get_config().max_filter_combinations + 1):
            for keys in combinations(filter_parts.keys(), n):
                parts = list(map(lambda k: filter_parts[k], keys))
                keyss = list(map(lambda k: [k] * len(filter_parts[k]), keys))

                for part_combo, k in zip(product(*parts), product(*keyss)):
                    cols = frozenset([col for cols in k for col in cols])

                    if len(part_combo) == 1:
                        conditions.append(part_combo[0])
                        condition_map[cols].add(conditions[-1])
                        for col in cols:
                            if col in self.generated_columns.keys():
                                predicates.append(DSLPredicate('happens_before', [f'"{conditions[-1]}"', self.generated_columns[col]]))

                    else:
                        conditions.append(' & '.join(part_combo))
                        conditions.append(' | '.join(part_combo))
                        condition_map[cols].add(conditions[-1])
                        condition_map[cols].add(conditions[-2])

                        for col in cols:
                            if col in self.generated_columns.keys():
                                predicates.append(DSLPredicate('happens_before', [f'"{conditions[-1]}"', self.generated_columns[col]]))
                                predicates.append(DSLPredicate('happens_before', [f'"{conditions[-2]}"', self.generated_columns[col]]))

        for spec_part in [self.consts, self.filters]:
            for constant in spec_part:
                current_predicate = []
                for key, value in condition_map.items():
                    if constant in key:
                        current_predicate += list(map(lambda v: f'"{v}"', value))
                if current_predicate:
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
                        predicates.append(DSLPredicate('happens_before', [f'"{str_conditions[-1]}"', self.generated_columns[col]]))

        for subset in powerset_except_empty(self.columns, util.get_config().max_join_combinations):
            str_conditions.append(','.join(map(util.single_quote_str, subset)))

            for col in subset:
                if col in self.generated_columns.keys():
                    predicates.append(DSLPredicate('happens_before', [f'"{str_conditions[-1]}"', self.generated_columns[col]]))

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

    def constant_occurs(self, column, constant):
        att = False
        for input in self.tables:
            data_frame = self.data_frames[input]
            if att:
                break

            if column in data_frame.columns:
                if constant is None:
                    if any(pandas.isna(data_frame[column])):
                        att = True
                        break
                else:
                    if constant in data_frame[column].values:
                        att = True
                        break
            else:
                continue
        return att
