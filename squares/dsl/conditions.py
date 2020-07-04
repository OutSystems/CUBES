import re
from collections import defaultdict
from itertools import combinations, product, chain, permutations

from ordered_set import OrderedSet

from . import dsl
from .condition_table import ConditionTable
from .. import types, util
from ..util import powerset_except_empty, all_permutations


def symmetric_op(op: str):
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
    elif op == '!=':
        return '!='


def more_restrictive_op(op: str) -> OrderedSet:
    if op == '>':
        return OrderedSet()
    elif op == '<':
        return OrderedSet()
    elif op == '>=':
        return OrderedSet(['>', '=='])
    elif op == '<=':
        return OrderedSet(['<', '=='])
    elif op == '==':
        return OrderedSet()
    elif op == '!=':
        return OrderedSet(['>', '<'])


def less_restrictive_op(op: str) -> OrderedSet:
    if op == '>':
        return OrderedSet(['>=', '!='])
    elif op == '<':
        return OrderedSet(['<=', '!='])
    elif op == '>=':
        return OrderedSet()
    elif op == '<=':
        return OrderedSet()
    elif op == '==':
        return OrderedSet(['>=', '<='])
    elif op == '!=':
        return OrderedSet()


class ConditionGenerator:

    def __init__(self, specification) -> None:
        self.specification = specification
        self.predicates = []
        self.summarise_conditions = []
        self.filter_conditions = []
        self.inner_join_conditions = []
        self.cross_join_conditions = []
        self.cols = []
        self.more_restrictive = ConditionTable()
        self.less_restrictive = ConditionTable()

    def generate(self) -> None:
        self.generate_summarise()
        self.generate_filter()
        self.generate_inner_join()
        self.generate_cross_join()
        self.generate_cols()

    def generate_summarise(self) -> None:
        frozen_columns = self.specification.filter_columns(self.specification.columns_by_type)

        for aggr in self.specification.aggrs:
            current_predicate = []

            def add_condition(cond, new_col, new_col_types, dependencies, save_generated=True) -> None:
                if new_col:
                    self.specification.all_columns.add(new_col)
                    if save_generated:
                        self.specification.generated_columns[new_col] = cond
                self.summarise_conditions.append((cond, (dependencies, self.specification.get_bitvecnum([new_col]))))
                current_predicate.append(cond)
                for type in new_col_types:
                    self.specification.columns_by_type[type].append(new_col)

            if aggr == 'n' or aggr == 'count':
                add_condition('n = n()', 'n', [types.INT], 0)

            if aggr == 'n_distinct' or aggr == 'count':
                for columns in powerset_except_empty(chain(*frozen_columns.values()), util.get_config().max_column_combinations):
                    add_condition(f'n_distinct = n_distinct({",".join(columns)})', 'n_distinct', [types.INT],
                                  self.specification.get_bitvecnum(columns))

            if aggr == 'concat':
                for column in frozen_columns[types.STRING]:
                    for separator in ['', ' ', ',', ', ']:  # TODO generated columns (both ways). as it is it overrides
                        add_condition(f"{aggr}{column} = string_agg({column}, '{separator}')", f'{aggr}{column}', [types.STRING],
                                      self.specification.get_bitvecnum([column]))

            if aggr == 'str_count':
                self.specification.aggrs_use_const = True
                for column in frozen_columns[types.STRING]:
                    for constant in self.specification.consts_by_type[types.STRING]:
                        add_condition(f"{aggr}{column} = str_count({column}, '{constant}')", f'{aggr}{column}', [types.INT],
                                      self.specification.get_bitvecnum([column]))

            if aggr == 'mean':
                for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [types.FLOAT],
                                  self.specification.get_bitvecnum([column]))

            if aggr in ['sum', 'cumsum']:
                for type, column in [(t, col) for t in [types.INT, types.FLOAT] for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type],
                                  self.specification.get_bitvecnum([column]))

            if aggr in ['min', 'max']:
                for type, column in [(t, col) for t in [types.INT, types.FLOAT] for col in frozen_columns[t]]:
                    add_condition(f'{column} = {aggr}({column})', column, [type], self.specification.get_bitvecnum([column]),
                                  save_generated=False)
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type],
                                  self.specification.get_bitvecnum([column]))

            if aggr in ['pmin', 'pmax']:
                for t in [types.INT, types.FLOAT]:
                    for cols in powerset_except_empty(frozen_columns[t]):
                        add_condition(f'{aggr}_ = {aggr}({",".join(cols)})', f'{aggr}_', [t], self.specification.get_bitvecnum(cols))

            if aggr in ['coalesce']:
                for type, columns in frozen_columns.items():
                    for cols in all_permutations(columns, len(columns)):
                        if len(cols) <= 1:
                            continue
                        add_condition(f'{aggr}_ = {aggr}({",".join(cols)})', f'{aggr}_', [type], self.specification.get_bitvecnum(cols))

            if aggr in ['mode', 'lead', 'lag', 'median']:
                for type, column in [(t, col) for t in types.Type for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type],
                                  self.specification.get_bitvecnum([column]))

            if aggr == 'rank':
                for type, column in [(t, col) for t in types.Type for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = dense_rank({column})', f'{aggr}{column}', [types.INT],
                                  self.specification.get_bitvecnum([column]))
                    add_condition(f'{aggr}d{column} = dense_rank(desc({column}))', f'{aggr}d{column}', [types.INT],
                                  self.specification.get_bitvecnum([column]))

            if aggr == 'row_number':
                add_condition(f'{aggr}_ = {aggr}()', f'{aggr}_', [types.INT], 0)

            if aggr in ['min', 'max']:
                for type, column in [(t, col) for t in [types.DATETIME, types.TIME] for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type],
                                  self.specification.get_bitvecnum([column]))

            if aggr == 'first':
                add_condition('all$first', None, [], 0)

            if util.get_config().force_summarise and current_predicate:
                self.predicates.append(('constant_occurs', current_predicate))

    def generate_filter(self) -> None:
        filter_parts = defaultdict(OrderedSet)
        parts_2_cols = defaultdict(OrderedSet)
        frozen_columns = self.specification.filter_columns(self.specification.columns_by_type)

        for constant in self.specification.consts_by_type[types.NONE]:
            for column in frozen_columns[types.STRING] | frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                filter_parts[frozenset((column, constant))].add(f"is.na({column})")
                parts_2_cols[f"is.na({column})"].add(column)
                filter_parts[frozenset((column, constant))].add(f"!is.na({column})")
                parts_2_cols[f"!is.na({column})"].add(column)

        for constant in self.specification.consts_by_type[types.STRING]:
            for column in frozen_columns[types.STRING]:
                if 'str_detect' in self.specification.filters or 'like' in self.specification.filters:
                    filter_parts[frozenset((column, constant))].add(f"str_detect({column}, {types.to_r_repr(constant)})")
                    parts_2_cols[f"str_detect({column}, {types.to_r_repr(constant)})"].add(column)

                if self.specification.constant_occurs(column, constant):
                    for op in types.operators_by_type[types.STRING]:
                        filter_parts[frozenset((column, constant))].add(f"{column} {op} {types.to_r_repr(constant)}")
                        parts_2_cols[f"{column} {op} {types.to_r_repr(constant)}"].add(column)

        for constant in self.specification.consts_by_type[types.INT] | self.specification.consts_by_type[types.FLOAT]:
            for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                for op in types.operators_by_type[types.INT]:
                    filter_parts[frozenset((column, constant))].add(f'{column} {op} {constant}')
                    parts_2_cols[f'{column} {op} {constant}'].add(column)
                    for op2 in more_restrictive_op(op):
                        self.more_restrictive.append(dsl.FilterCondition, f'{column} {op} {constant}', f'{column} {op2} {constant}')
                    for op2 in less_restrictive_op(op):
                        self.less_restrictive.append(dsl.FilterCondition, f'{column} {op} {constant}', f'{column} {op2} {constant}')

        for constant in self.specification.consts_by_type[types.DATETIME]:
            for column in frozen_columns[types.DATETIME]:
                for op in types.operators_by_type[types.INT]:
                    filter_parts[frozenset((column, constant))].add(f"{column} {op} {self.specification.dateorder}('{constant}')")
                    parts_2_cols[f"{column} {op} {self.specification.dateorder}('{constant}')"].add(column)

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
            for attr2 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                if attr1 == attr2:
                    continue
                for op in types.operators_by_type[types.INT]:
                    if (attr2, op, attr1) in bc or (attr1, symmetric_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')
                    parts_2_cols[f'{attr2} {op} {attr1}'].add(attr2)
                    parts_2_cols[f'{attr2} {op} {attr1}'].add(attr1)
                    for op2 in more_restrictive_op(op):
                        self.more_restrictive.append(dsl.FilterCondition, f'{attr2} {op} {attr1}', f'{attr2} {op2} {attr1}')
                    for op2 in less_restrictive_op(op):
                        self.less_restrictive.append(dsl.FilterCondition, f'{attr2} {op} {attr1}', f'{attr2} {op2} {attr1}')

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.DATETIME]:
            for attr2 in frozen_columns[types.DATETIME]:
                if attr1 == attr2:
                    continue
                for op in types.operators_by_type[types.INT]:
                    if (attr2, op, attr1) in bc or (attr1, symmetric_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')
                    parts_2_cols[f'{attr2} {op} {attr1}'].add(attr2)
                    parts_2_cols[f'{attr2} {op} {attr1}'].add(attr1)
                    for op2 in more_restrictive_op(op):
                        self.more_restrictive.append(dsl.FilterCondition, f'{attr2} {op} {attr1}', f'{attr2} {op2} {attr1}')
                    for op2 in less_restrictive_op(op):
                        self.less_restrictive.append(dsl.FilterCondition, f'{attr2} {op} {attr1}', f'{attr2} {op2} {attr1}')

        for filter in self.specification.filters:
            match = re.match(r'[a-zA-Z_][a-zA-Z_]*\(([a-zA-Z_][a-zA-Z_]*)\)', filter)
            if match:
                col = match[1]
                matching_types = []
                for type in types.Type:
                    if col in self.specification.columns_by_type[type]:
                        matching_types.append(type)
                for type in matching_types:
                    for other_col in self.specification.columns_by_type[type]:
                        filter_parts[frozenset((col, other_col, filter))].add(f'{other_col} == {filter}')
                        filter_parts[frozenset((col, other_col, filter))].add(f'{other_col} != {filter}')
                        parts_2_cols[f'{other_col} == {filter}'].add(col)
                        parts_2_cols[f'{other_col} == {filter}'].add(other_col)
                        parts_2_cols[f'{other_col} != {filter}'].add(col)
                        parts_2_cols[f'{other_col} != {filter}'].add(other_col)

        conditions = []
        condition_map = defaultdict(OrderedSet)

        for n in range(1, util.get_config().max_filter_combinations + 1):
            for keys in combinations(filter_parts.keys(), n):
                parts = list(map(lambda k: filter_parts[k], keys))
                keyss = list(map(lambda k: [k] * len(filter_parts[k]), keys))

                for part_combo, k in zip(product(*parts), product(*keyss)):
                    cols = frozenset([col for cols in k for col in cols])

                    if len(part_combo) == 1:
                        conditions.append((part_combo[0], self.specification.get_bitvecnum(parts_2_cols[part_combo[0]])))
                        condition_map[cols].add(conditions[-1][0])

                    else:
                        conditions.append((' & '.join(part_combo), self.specification.get_bitvecnum(
                            list(chain.from_iterable(map(lambda x: parts_2_cols[x], part_combo))))))
                        conditions.append((' | '.join(part_combo), self.specification.get_bitvecnum(
                            list(chain.from_iterable(map(lambda x: parts_2_cols[x], part_combo))))))
                        condition_map[cols].add(conditions[-1][0])
                        condition_map[cols].add(conditions[-2][0])
                        self.less_restrictive.append(dsl.FilterCondition, conditions[-2][0], part_combo[0])
                        self.less_restrictive.append(dsl.FilterCondition, conditions[-2][0], part_combo[1])
                        self.more_restrictive.append(dsl.FilterCondition, conditions[-1][0], part_combo[0])
                        self.more_restrictive.append(dsl.FilterCondition, conditions[-1][0], part_combo[1])
                        self.less_restrictive.append(dsl.FilterCondition, part_combo[0], conditions[-1][0])
                        self.less_restrictive.append(dsl.FilterCondition, part_combo[1], conditions[-1][0])
                        self.more_restrictive.append(dsl.FilterCondition, part_combo[0], conditions[-2][0])
                        self.more_restrictive.append(dsl.FilterCondition, part_combo[1], conditions[-2][0])

        for spec_part in [self.specification.consts, self.specification.filters]:
            for constant in spec_part:
                current_predicate = []
                for key, value in condition_map.items():
                    if constant in key:
                        current_predicate += list(map(lambda v: f'{v}', value))
                if current_predicate:
                    self.predicates.append(('constant_occurs', current_predicate))

        self.filter_conditions += conditions

    def generate_inner_join(self) -> None:
        column_pairs = []
        for cols in self.specification.columns_by_type.values():
            column_pairs += product(cols, repeat=2)
        simple_column_pairs = []
        for cols in self.specification.columns_by_type.values():
            simple_column_pairs += combinations(cols, 2)

        on_conditions = list(combinations(column_pairs, 2)) + list(combinations(simple_column_pairs, 1))
        on_conditions = list(filter(lambda t: any(map(lambda cond: cond[0] != cond[1], t)), on_conditions))

        for on_condition in on_conditions:
            if util.has_duplicates(map(lambda x: x[0], on_condition)) or util.has_duplicates(map(lambda x: x[1], on_condition)):
                continue
            self.inner_join_conditions.append((','.join(map(lambda x: f"'{x[0]}' = '{x[1]}'", on_condition)), (
                self.specification.get_bitvecnum([pair[0] for pair in on_condition]),
                self.specification.get_bitvecnum([pair[1] for pair in on_condition])
                )))

        for subset in powerset_except_empty(self.specification.columns, util.get_config().max_join_combinations):
            self.inner_join_conditions.append((','.join(map(util.single_quote_str, subset)),
                                               (self.specification.get_bitvecnum(subset), self.specification.get_bitvecnum(subset))))

    def generate_cross_join(self) -> None:
        if util.get_config().full_cross_join:
            all_columns = self.specification.filter_columns(self.specification.columns_by_type)

            for type in all_columns:
                all_columns[type] |= [col + '.other' for col in all_columns[type]]

            column_pairs = []
            for type, cols in all_columns.items():
                for col_pair in combinations(cols, 2):
                    column_pairs.append((type, col_pair))

            on_conditions = list(combinations(column_pairs, 2)) + list(combinations(column_pairs, 1))
            on_conditions = list(filter(lambda t: all(map(lambda cond: cond[1][0] != cond[1][1], t)) and
                                                  any(map(lambda cond: '.other' in cond[1][0] or '.other' in cond[1][1], t)),
                                        on_conditions))

            for on_condition in on_conditions:
                if len(on_condition) == 1:
                    for op1 in types.operators_by_type[on_condition[0][0]]:
                        cols = [on_condition[0][1][0], on_condition[0][1][1]]
                        main_cols = [col for col in cols if '.other' not in col]
                        second_cols = [col.replace('.other', '') for col in cols if '.other' in col]
                        self.cross_join_conditions.append((f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}',
                                                           (self.specification.get_bitvecnum(main_cols),
                                                            self.specification.get_bitvecnum(second_cols))))

                        for op2 in more_restrictive_op(op1) & types.operators_by_type[on_condition[0][0]]:
                            self.more_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}',
                                                         f'{on_condition[0][1][0]} {op2} {on_condition[0][1][1]}')
                        for op2 in less_restrictive_op(op1) & types.operators_by_type[on_condition[0][0]]:
                            self.less_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}',
                                                         f'{on_condition[0][1][0]} {op2} {on_condition[0][1][1]}')

                if len(on_condition) == 2:
                    for op1 in types.operators_by_type[on_condition[0][0]]:
                        for op2 in types.operators_by_type[on_condition[1][0]]:
                            for op3 in ['&', '|']:
                                cols = [on_condition[0][1][0], on_condition[1][1][0], on_condition[0][1][1], on_condition[1][1][1]]
                                main_cols = [col for col in cols if '.other' not in col]
                                second_cols = [col.replace('.other', '') for col in cols if '.other' in col]
                                self.cross_join_conditions.append((
                                    f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]} {op3} {on_condition[1][1][0]} {op2} {on_condition[1][1][1]}',
                                    (self.specification.get_bitvecnum(main_cols),
                                     self.specification.get_bitvecnum(second_cols))))

                            self.less_restrictive.append(dsl.CrossJoinCondition,
                                                         self.cross_join_conditions[-2][0],
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}')
                            self.less_restrictive.append(dsl.CrossJoinCondition,
                                                         self.cross_join_conditions[-2][0],
                                                         f'{on_condition[1][1][0]} {op2} {on_condition[1][1][1]}')
                            self.more_restrictive.append(dsl.CrossJoinCondition,
                                                         self.cross_join_conditions[-1][0],
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}')
                            self.more_restrictive.append(dsl.CrossJoinCondition,
                                                         self.cross_join_conditions[-1][0],
                                                         f'{on_condition[1][1][0]} {op2} {on_condition[1][1][1]}')
                            self.less_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}',
                                                         self.cross_join_conditions[-1][0])
                            self.less_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[1][1][0]} {op2} {on_condition[1][1][1]}',
                                                         self.cross_join_conditions[-1][0])
                            self.more_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[0][1][0]} {op1} {on_condition[0][1][1]}',
                                                         self.cross_join_conditions[-2][0])
                            self.more_restrictive.append(dsl.CrossJoinCondition,
                                                         f'{on_condition[1][1][0]} {op2} {on_condition[1][1][1]}',
                                                         self.cross_join_conditions[-2][0])

        # self.cross_join_conditions.append(('', 0))

    def generate_cols(self) -> None:
        cols_tmp = []
        for cols in powerset_except_empty(self.specification.columns, util.get_config().max_column_combinations):
            cols_tmp.append((set(cols), (','.join(cols), self.specification.get_bitvecnum(cols))))

        for cols1, cols2 in permutations(cols_tmp, 2):
            if cols1[0] <= cols2[0]:
                self.less_restrictive.append(dsl.Cols, cols1[1][0], cols2[1][0])
                self.more_restrictive.append(dsl.Cols, cols2[1][0], cols1[1][0])
            if cols2[0] <= cols1[0]:
                self.less_restrictive.append(dsl.Cols, cols2[1][0], cols1[1][0])
                self.more_restrictive.append(dsl.Cols, cols1[1][0], cols2[1][0])

        self.cols = [cols[1] for cols in cols_tmp]
