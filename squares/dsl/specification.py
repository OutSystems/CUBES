import io
import operator
from functools import reduce
from logging import getLogger
from typing import Dict, Sequence, Any

import moz_sql_parser
import pandas
from ordered_set import OrderedSet
from rpy2 import robjects

from . import dsl
from .conditions import ConditionGenerator
from .sql2dsl import SQL2R
from .table import Table
from .. import util, types
from ..exceptions import SquaresException
from ..tyrell.spec import TyrellSpec, TypeSpec, ProductionSpec, ProgramSpec, ValueType
from ..tyrell.spec.spec import PredicateSpec
from ..util import powerset_except_empty

# from rpy2.rinterface_lib.embedded import RRuntimeError

logger = getLogger('squares')


def exec_and_return(r_script: str) -> str:
    # print(r_script)
    robjects.r(r_script)
    return r_script


def exec_and_return_try(r_script: str) -> str:
    try:
        return exec_and_return(r_script)
    except:
        return ''


def add_is_not_parent_if_enabled(pred_spec: PredicateSpec, a: Any, b: Any) -> None:
    if a not in util.get_config().disabled and b not in util.get_config().disabled:
        pred_spec.add_predicate('is_not_parent', [a, b])


class Specification:

    def __init__(self, spec) -> None:
        self.inputs = OrderedSet(spec['inputs'])
        self.output = spec['output']
        self.consts = OrderedSet(spec['constants']) or OrderedSet()
        if util.get_config().ignore_aggrs:
            self.aggrs = util.get_config().aggregation_functions
        else:
            self.aggrs = OrderedSet(spec['functions']) or OrderedSet()
        self.attrs = OrderedSet(spec['columns']) or OrderedSet()
        self.join_columns = OrderedSet(spec['join_columns']) or None
        self.group_columns = OrderedSet(spec['groupby_columns']) or None
        self.dateorder = spec['dateorder'] or 'parse_datetime'
        self.filters = OrderedSet(spec['filters']) or OrderedSet()
        if 'solution' in spec:
            self.solution = spec['solution']
        else:
            self.solution = None
        self.sql_visitor = None

        if util.get_config().use_beam_info:

            if util.get_config().beam_name is None:
                raise SquaresException('You must provide a valid beam name.')

            beam_name = util.get_config().beam_name

            # if 'sql' in spec:
            #     self.sql = spec['sql']
            # try:
            #     self.sql_visitor = SQL2R()
            #     self.sql_visitor.visit(moz_sql_parser.parse(self.sql))
            #     logger.debug(self.sql_visitor)
            # except Exception as e:
            #     self.sql_visitor = None
            #     logger.error('Error while parsing SQL query.')
            #     logger.error('%s', str(e))

            # self.scores_actions = list(map(lambda x: util.summing_dict(zip(x[1], x[0])), zip(spec["beam_score_history"], spec["beam_actions"])))
            # self.scores_actions_r = sorted(list(reduce(util.sum_dicts, self.scores_actions).items()), key=lambda x: x[1])

            def beam_helper(iter, regular, noun, callback=None):
                majority, majorant = util.at_least(list(map(OrderedSet, iter)), percentage=util.get_config().beam_threshold)
                if majority:
                    if regular != majorant:
                        logger.debug('Beam %s differ from regular %s! Beam: %s Regular: %s', noun, noun, majorant, regular)
                    return majorant
                else:
                    new = OrderedSet().union(*map(OrderedSet, iter))
                    if regular != new:
                        logger.debug('Beam %s differ from regular %s! Beam: %s Regular: %s', noun, noun, new, regular)
                    if callback:
                        callback()
                    return new

            if f'{beam_name}_beam_tables' in spec:
                def table_callback():
                    logger.warning('Uncertainty in beam tables! Disabling forced usage of all inputs.')
                    util.get_config().lines_force_all_inputs = False

                self.inputs = beam_helper(spec[f'{beam_name}_beam_tables'], self.inputs, 'tables', table_callback)

            if f'{beam_name}_beam_constants' in spec:
                def constants_callback():
                    logger.warning('Uncertainty in beam constants! Disabling forced usage of all consts.')
                    util.get_config().force_constants = False  # TODO this disables the requirement for all filters

                self.consts = beam_helper(spec[f'{beam_name}_beam_constants'], self.consts, 'constants', constants_callback)

            if f'{beam_name}_beam_functions' in spec:
                def functions_callback():
                    logger.warning('Uncertainty in beam functions! Disabling forced usage of all funcs.')
                    util.get_config().force_summarise = False

                self.aggrs = beam_helper(spec[f'{beam_name}_beam_functions'], self.aggrs, 'functions', functions_callback)

            if f'{beam_name}_beam_columns' in spec:
                self.attrs = beam_helper(spec[f'{beam_name}_beam_columns'], self.attrs, 'columns')

            if f'{beam_name}_beam_filters' in spec:
                def filters_callback():
                    logger.warning('Uncertainty in beam filters! Disabling forced usage of all filters')
                    util.get_config().force_constants = False  # TODO this disables the requirement for all filters

                self.filters = beam_helper(spec[f'{beam_name}_beam_filters'], self.filters, 'filters', filters_callback)

            if f'{beam_name}_beam_join_columns' in spec:
                self.join_columns = beam_helper(spec[f'{beam_name}_beam_join_columns'], self.join_columns, 'join columns')

            if f'{beam_name}_beam_groupby_columns' in spec:
                self.group_columns = beam_helper(spec[f'{beam_name}_beam_groupby_columns'], self.group_columns, 'group_by columns')

        if util.get_config().use_solution_dsl and self.solution:
            util.get_config().disabled = OrderedSet(util.get_config().disabled) | (dsl.functions - OrderedSet(self.solution))

        if util.get_config().force_constants:
            self.min_loc = max((len(self.aggrs) if util.get_config().force_summarise else 0) + (1 if self.filters or self.consts else 0),
                               util.get_config().minimum_loc)  # TODO
        else:
            self.min_loc = max((len(self.aggrs) if util.get_config().force_summarise else 0), util.get_config().minimum_loc)

        if util.get_config().use_solution_loc and self.solution:
            self.min_loc = max(self.min_loc, len(self.solution))
            util.get_config().maximum_loc = min(util.get_config().maximum_loc, len(self.solution))

        self.attrs = list(map(lambda s: s.lower(), self.attrs))
        if self.join_columns:
            self.join_columns = list(map(lambda s: s.lower(), self.join_columns))
        if self.group_columns:
            self.group_columns = list(map(lambda s: s.lower(), self.group_columns))

        self.aggrs_use_const = False

        self.input_tables = []
        self.data_frames = {}

        self.columns = OrderedSet()
        self.generated_columns = {}
        self.columns_by_type = types.empty_type_map()
        self.generated_columns_by_type = types.empty_type_map()
        self.join_columns_by_type = types.empty_type_map()
        self.types_by_const = {}
        self.consts_by_type = types.empty_type_map()

        if 'max(n)' in self.aggrs:
            raise SquaresException('max(n) is not a valid aggregator. Use a filter instead.')

        logger.debug("Reading input files...")
        for input in self.inputs:

            table = Table(input)

            self.input_tables.append(table)

            for column, type in table.col_types.items():
                self.columns_by_type[type].add(column)
                if self.join_columns is not None and column.lower() in self.join_columns:
                    self.join_columns_by_type[type].add(column)

            self.columns |= table.col_names

        self.input_table_names = [table.name for table in self.input_tables]

        self.columns = OrderedSet(sorted(self.columns))
        self.all_columns = self.columns.copy()

        self.output_table = Table(self.output, name='expected_output')
        self.output_cols = self.output_table.col_names

        if self.output_table.df.shape[0] == 0:
            raise SquaresException('Output table has 0 rows...', exit_status=6)

        for const in self.consts:
            self.types_by_const[const] = []

            for type in types.Type:
                if types.is_type(const, type):
                    self.types_by_const[const].append(type)
                    self.consts_by_type[type].append(const)

        self.condition_generator = ConditionGenerator(self)
        self.condition_generator.generate()

        if self.group_columns is None:
            self.group_columns = self.columns

        self.n_columns = len(self.all_columns)

        self.generate_r_init()

        self.tyrell_spec = None

    def generate_r_init(self) -> None:  # TODO dirty: initializes R for the inputs
        self.r_init = 'con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")\n'

        for table in self.input_tables:
            self.r_init += exec_and_return(table.gen_r_read_code())
            self.r_init += f'{table.name} <- copy_to(con, {table.name})\n'

        self.r_init += exec_and_return(self.output_table.gen_r_read_code())

        if 'concat' in self.aggrs:
            self.r_init += exec_and_return('\nstring_agg <- function(v,s) {paste0("", Reduce(function(x, y) paste(x, y, sep = s), v))}\n')
        if 'mode' in self.aggrs:
            self.r_init += exec_and_return('\nmode <- function(x) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}\n')

    def generate_dsl(self, discard: bool = False) -> TyrellSpec:
        if self.tyrell_spec and not discard:
            return self.tyrell_spec

        type_spec = TypeSpec()

        Empty = ValueType('Empty')
        type_spec.define_type(Empty)

        type_spec.define_type(dsl.Table)

        if 'intersect' not in util.get_config().disabled:
            dsl.Col.set_domain([(column, self.get_bitvecnum([column])) for column in self.columns])
            type_spec.define_type(dsl.Col)

        if 'anti_join' not in util.get_config().disabled:
            dsl.Cols.set_domain([('', 0)] + [(','.join(map(util.single_quote_str, cols)), self.get_bitvecnum(cols)) for cols in
                                             powerset_except_empty(self.columns, util.get_config().max_column_combinations)])
            type_spec.define_type(dsl.Cols)

        if 'summarise' not in util.get_config().disabled or self.condition_generator.summarise_conditions:
            dsl.GroupCols.set_domain([('', 0)] + [(','.join(map(util.single_quote_str, cols)), self.get_bitvecnum(cols)) for cols in
                                                  powerset_except_empty(self.group_columns, util.get_config().max_column_combinations)])
            type_spec.define_type(dsl.GroupCols)

        if 'inner_join' not in util.get_config().disabled and self.condition_generator.inner_join_conditions:
            dsl.JoinCondition.set_domain(self.condition_generator.inner_join_conditions)
            type_spec.define_type(dsl.JoinCondition)

        if 'cross_join' not in util.get_config().disabled and self.condition_generator.cross_join_conditions:
            dsl.CrossJoinCondition.set_domain(self.condition_generator.cross_join_conditions)
            type_spec.define_type(dsl.CrossJoinCondition)

        if self.condition_generator.filter_conditions:
            dsl.FilterCondition.set_domain(self.condition_generator.filter_conditions)
            type_spec.define_type(dsl.FilterCondition)

        if self.condition_generator.summarise_conditions:
            dsl.SummariseCondition.set_domain(self.condition_generator.summarise_conditions)
            type_spec.define_type(dsl.SummariseCondition)

        if util.get_config().filters_function_enabled and len(self.condition_generator.filter_conditions) > 1:
            dsl.Op.set_domain(['|', '&'])
            type_spec.define_type(dsl.Op)

        prod_spec = ProductionSpec()

        prod_spec.add_func_production('empty', Empty, [Empty])

        if 'natural_join' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.natural_join)

        if 'natural_join3' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.natural_join3)

        if 'natural_join4' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.natural_join4)

        if 'inner_join' not in util.get_config().disabled and self.condition_generator.inner_join_conditions:
            prod_spec.add_func_production(*dsl.inner_join)

        if 'anti_join' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.anti_join)

        if 'left_join' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.left_join)

        if 'union' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.union)

        if 'intersect' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.intersect)

        if 'semi_join' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.semi_join)

        if 'cross_join' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.cross_join)

        if 'concat' in self.aggrs:
            prod_spec.add_func_production(*dsl.unite)

        if self.condition_generator.filter_conditions:
            prod_spec.add_func_production(*dsl.filter)

            if util.get_config().filters_function_enabled and len(self.condition_generator.filter_conditions) > 1:
                prod_spec.add_func_production(*dsl.filters)

        if self.condition_generator.summarise_conditions and 'summarise' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.summarise)

        if self.condition_generator.summarise_conditions and 'mutate' not in util.get_config().disabled:
            prod_spec.add_func_production(*dsl.mutate)

        pred_spec = PredicateSpec()

        # if self.condition_generator.summarise_conditions and 'summarise' not in util.get_config().disabled:
        #     pred_spec.add_predicate('is_not_parent', ['summarise', 'summarise'])

        if len(self.condition_generator.filter_conditions) >= 1:
            # add_is_not_parent_if_enabled(pred_spec, 'filter', 'inner_join')
            # add_is_not_parent_if_enabled(pred_spec, 'filter', 'natural_join')
            # add_is_not_parent_if_enabled(pred_spec, 'filter', 'natural_join3')
            # add_is_not_parent_if_enabled(pred_spec, 'filter', 'natural_join4')
            # add_is_not_parent_if_enabled(pred_spec, 'inner_join', 'filter')
            # add_is_not_parent_if_enabled(pred_spec, 'natural_join', 'filter')
            # add_is_not_parent_if_enabled(pred_spec, 'natural_join3', 'filter')
            # add_is_not_parent_if_enabled(pred_spec, 'natural_join4', 'filter')
            pass
        elif len(self.condition_generator.filter_conditions) > 1 and util.get_config().filters_function_enabled:
            pred_spec.add_predicate('distinct_filters', ['filters', 1, 2])
            pred_spec.add_predicate('is_not_parent', ['filters', 'filter'])
            # pred_spec.add_predicate('is_not_parent', ['filter', 'filters'])
            pred_spec.add_predicate('is_not_parent', ['filter', 'filter'])
            # pred_spec.add_predicate('is_not_parent', ['filters', 'filters'])

        for p in self.condition_generator.predicates:
            pred_spec.add_predicate(*p)

        add_is_not_parent_if_enabled(pred_spec, 'natural_join', 'natural_join')
        add_is_not_parent_if_enabled(pred_spec, 'natural_join', 'natural_join3')
        add_is_not_parent_if_enabled(pred_spec, 'natural_join', 'natural_join4')
        add_is_not_parent_if_enabled(pred_spec, 'natural_join3', 'natural_join')
        add_is_not_parent_if_enabled(pred_spec, 'natural_join3', 'natural_join4')
        add_is_not_parent_if_enabled(pred_spec, 'anti_join', 'anti_join')
        add_is_not_parent_if_enabled(pred_spec, 'anti_join', 'natural_join')
        add_is_not_parent_if_enabled(pred_spec, 'anti_join', 'natural_join3')
        add_is_not_parent_if_enabled(pred_spec, 'anti_join', 'natural_join4')

        for join in ['natural_join4', 'natural_join3', 'natural_join', 'anti_join', 'semi_join', 'left_join']:
            if join not in util.get_config().disabled:
                pred_spec.add_predicate('distinct_inputs', [join])

        program_spec = ProgramSpec('squares', [(dsl.Table, self.get_bitvecnum(input.df)) for input in self.input_tables],
                                   (dsl.Table, self.get_bitvecnum(self.output_table.df)))

        self.tyrell_spec = TyrellSpec(type_spec, program_spec, prod_spec, pred_spec)
        function_difficulty = {
            prod.name: reduce(operator.mul, (len(rhs.domain) if rhs.is_enum() else len(self.inputs) for rhs in prod.rhs), 1) for prod
            in self.tyrell_spec.get_function_productions() if prod.name != 'empty'}
        logger.debug(function_difficulty)
        return self.tyrell_spec

    def get_bitvecnum(self, columns: Sequence[str]) -> int:
        return util.boolvec2int([column in columns for column in self.all_columns])

    def filter_columns(self, columns_map: Dict[types.Type, OrderedSet[str]]) -> Dict[types.Type, OrderedSet[str]]:
        d = {}
        for type in columns_map.keys():
            d[type] = OrderedSet()
            for column in columns_map[type]:
                if column in self.attrs or util.get_config().ignore_attrs or column in self.generated_columns.keys():
                    d[type].add(column)
        return d

    def constant_occurs(self, column: str, constant: str) -> bool:
        for table in self.input_tables:
            if column in table.df.columns:
                if constant is None:
                    if any(pandas.isna(table.df[column])):
                        return True
                else:
                    if constant in table.df[column].dropna().values:
                        return True
        return False

    def __str__(self) -> str:
        buffer = io.StringIO()
        buffer.write(repr(self.generate_dsl()))
        # buffer.write('\nMore restrictive:\n')
        # for type in self.condition_generator.more_restrictive.graphs.keys():
        #     max_length = max(map(len, self.condition_generator.more_restrictive.graphs[type])) + 1
        #     buffer.write(f'\t{type}:\n')
        #     for key in list(self.condition_generator.more_restrictive.graphs[type]):
        #         buffer.write(f'\t\t{key.ljust(max_length)}: {self.condition_generator.more_restrictive.dfs(type, key).items}\n')
        # buffer.write('Less restrictive:\n')
        # for type in self.condition_generator.less_restrictive.graphs.keys():
        #     max_length = max(map(len, self.condition_generator.less_restrictive.graphs[type])) + 1
        #     buffer.write(f'\t{type}:\n')
        #     for key in list(self.condition_generator.less_restrictive.graphs[type]):
        #         buffer.write(f'\t\t{key.ljust(max_length)}: {self.condition_generator.less_restrictive.dfs(type, key).items}\n')
        return buffer.getvalue()
