from logging import getLogger
from typing import Dict

import pandas
from ordered_set import OrderedSet
from rpy2 import robjects

from . import dsl_library
from .conditions import ConditionGenerator
from .dsl_builder import DSLPredicate, DSLEnum, DSLBuilder
from .. import util, types
from ..exceptions import SquaresException
from ..util import next_counter, get_combinations

logger = getLogger('squares')


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


class Specification:

    def __init__(self, spec):
        self.inputs = spec['inputs']
        self.output = spec['output']
        self.consts = spec['constants']
        if util.get_config().ignore_aggrs:
            self.aggrs = util.get_config().aggregation_functions
        else:
            self.aggrs = spec['functions']
        self.attrs = spec['columns']
        self.dateorder = spec['dateorder']
        self.filters = spec['filters']
        if 'solution' in spec:
            self.solution = spec['solution']
        else:
            self.solution = None

        self.min_loc = max((len(self.aggrs) if util.get_config().force_summarise else 0) + (1 if self.filters or self.consts else 0),
                           util.get_config().minimum_loc)  # TODO

        self.aggrs_use_const = False

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
        condition_generator = ConditionGenerator(self)
        condition_generator.generate_summarise()
        condition_generator.generate_filter()
        condition_generator.generate_inner_join()
        condition_generator.generate_cross_join()

        dsl = DSLBuilder('Squares', ['Table'] * len(self.inputs), 'Table')

        if condition_generator.summarise_conditions:
            dsl.add_enum(DSLEnum('Cols', get_combinations(self.columns, util.get_config().max_column_combinations)))

        if 'intersect' not in util.get_config().disabled:
            dsl.add_enum(DSLEnum('Col', list(self.columns)))

        if 'anti_join' not in util.get_config().disabled:
            dsl.add_enum(DSLEnum('JoinCols',
                                 [''] + get_combinations([f"'{col}'" for col in self.columns], util.get_config().max_column_combinations)))

        if 'inner_join' not in util.get_config().disabled and condition_generator.inner_join_conditions:
            dsl.add_enum(DSLEnum('JoinCondition', condition_generator.inner_join_conditions))

        if 'cross_join' not in util.get_config().disabled and condition_generator.cross_join_conditions:
            dsl.add_enum(DSLEnum('CrossJoinCondition', condition_generator.cross_join_conditions))

        if condition_generator.filter_conditions:
            dsl.add_enum(DSLEnum('FilterCondition', condition_generator.filter_conditions))

        if condition_generator.summarise_conditions:
            dsl.add_enum(DSLEnum('SummariseCondition', condition_generator.summarise_conditions))

        if condition_generator.summarise_conditions:
            dsl.add_enum(DSLEnum('MutateCondition', condition_generator.summarise_conditions))

        if util.get_config().filters_function_enabled and len(condition_generator.filter_conditions) > 1:
            dsl.add_enum(DSLEnum('Op', ['|', '&']))

        dsl.add_value(dsl_library.table_value)

        if 'natural_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join_function)

        if 'natural_join3' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join3_function)

        if 'natural_join4' not in util.get_config().disabled:
            dsl.add_function(dsl_library.natural_join4_function)

        if 'inner_join' not in util.get_config().disabled and condition_generator.inner_join_conditions:
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

        if 'cross_join' not in util.get_config().disabled:
            dsl.add_function(dsl_library.cross_join_function)

        if 'concat' in self.aggrs:
            dsl.add_function(dsl_library.unite_function)

        if condition_generator.filter_conditions:
            dsl.add_function(dsl_library.filter_function)

            if util.get_config().filters_function_enabled and len(condition_generator.filter_conditions) > 1:
                dsl.add_function(dsl_library.filters_function)

        if condition_generator.summarise_conditions and 'summarise' not in util.get_config().disabled:
            dsl.add_function(dsl_library.summarise_function)

        if condition_generator.summarise_conditions:
            dsl.add_function(dsl_library.mutate_function)

        if condition_generator.summarise_conditions and 'summarise' not in util.get_config().disabled:
            add_is_not_parent_if_enabled(dsl, 'natural_join4', 'summarise')
            dsl.add_predicate(DSLPredicate('is_not_parent', ['summarise', 'summarise', '100']))

        if len(condition_generator.filter_conditions) == 1:
            add_is_not_parent_if_enabled(dsl, 'inner_join', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join3', 'filter')
            add_is_not_parent_if_enabled(dsl, 'natural_join4', 'filter')
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('distinct_inputs', ['filter']))
        elif len(condition_generator.filter_conditions) > 1 and util.get_config().filters_function_enabled:
            dsl.add_predicate(DSLPredicate('distinct_filters', ['filters', '1', '2']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filters', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filters', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filter', 'filter', '100']))
            dsl.add_predicate(DSLPredicate('is_not_parent', ['filters', 'filters', '100']))

        for p in condition_generator.predicates:
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

    def filter_columns(self, columns_map: Dict[types.Type, OrderedSet]) -> Dict[types.Type, OrderedSet]:
        d = {}
        for type in columns_map.keys():
            d[type] = OrderedSet()
            for column in columns_map[type]:
                if column in self.attrs or util.get_config().ignore_attrs or column in self.generated_columns.keys():
                    d[type].add(column)
        return d

    def constant_occurs(self, column, constant):
        for data_frame in self.data_frames.values():
            if column in data_frame.columns:
                if constant is None:
                    if any(pandas.isna(data_frame[column])):
                        return True
                else:
                    if constant in data_frame[column].values:
                        return True

        return False
