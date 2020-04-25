import re
from collections import defaultdict
from itertools import combinations, product, chain

from ordered_set import OrderedSet

from .dsl_builder import DSLPredicate
from .. import types, util
from ..util import powerset_except_empty, get_permutations, all_permutations


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


class ConditionGenerator():

    def __init__(self, specification):
        self.specification = specification
        self.predicates = []
        self.summarise_conditions = []
        self.filter_conditions = []
        self.inner_join_conditions = []

    def generate_summarise(self):
        frozen_columns = self.specification.filter_columns(self.specification.columns_by_type)

        for aggr in self.specification.aggrs:
            current_predicate = []

            def add_condition(cond, new_col, new_col_types, save_generated=True):
                self.summarise_conditions.append(cond)
                current_predicate.append(util.quote_str(cond))
                if new_col:
                    self.specification.all_columns.add(new_col)
                    if save_generated:
                        self.specification.generated_columns[new_col] = util.quote_str(cond)
                for type in new_col_types:
                    self.specification.columns_by_type[type].append(new_col)

            if aggr == 'n':
                add_condition('n = n()', 'n', [types.INT])

            if aggr == 'concat':
                for column in frozen_columns[types.STRING]:
                    for separator in ['', ' ', ',', ', ']:  # TODO generated columns (both ways). as it is it overrides
                        add_condition(f"{aggr}{column} = string_agg({column}, '{separator}')", f'{aggr}{column}', [types.STRING])

            if aggr == 'str_count':
                self.specification.aggrs_use_const = True
                for column in frozen_columns[types.STRING]:
                    for constant in self.specification.consts_by_type[types.STRING]:
                        add_condition(f"{aggr}{column} = str_count({column}, '{constant}')", f'{aggr}{column}', [types.INT])

            if aggr == 'mean':
                for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [types.FLOAT])
                    if column in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{aggr}{column} = {aggr}({column})"',
                                                                               self.specification.generated_columns[column]]))

            if aggr in ['sum', 'cumsum']:
                for type, column in [(t, col) for t in [types.INT, types.FLOAT] for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type])
                    if column in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{aggr}{column} = {aggr}({column})"',
                                                                               self.specification.generated_columns[column]]))

            if aggr in ['min', 'max']:
                for type, column in [(t, col) for t in [types.INT, types.FLOAT] for col in frozen_columns[t]]:
                    add_condition(f'{column} = {aggr}({column})', column, [type], save_generated=False)
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type])
                    if column in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{column} = {aggr}({column})"',
                                                                               self.specification.generated_columns[column]]))

            if aggr in ['pmin', 'pmax']:
                for t in [types.INT, types.FLOAT]:
                    for cols in powerset_except_empty(frozen_columns[t]):
                        add_condition(f'{aggr} = {aggr}({",".join(cols)})', f'{aggr}', [t])
                        for column in cols:
                            if column in self.specification.generated_columns.keys():
                                self.predicates.append(DSLPredicate('happens_before', [f'{aggr} = {aggr}({",".join(cols)})',
                                                                                       self.specification.generated_columns[column]]))

            if aggr in ['coalesce']:
                for cols in all_permutations(set(chain.from_iterable(frozen_columns.values())), len(set(chain.from_iterable(frozen_columns.values())))):
                    add_condition(f'{aggr} = {aggr}({",".join(cols)})', f'{aggr}', types.Type)
                    for column in cols:
                        if column in self.specification.generated_columns.keys():
                            self.predicates.append(DSLPredicate('happens_before', [f'{aggr} = {aggr}({",".join(cols)})',
                                                                                   self.specification.generated_columns[column]]))

            if aggr in ['mode', 'lead', 'lag', 'median']:
                for type, column in [(t, col) for t in types.Type for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type])
                    if column in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{aggr}{column} = {aggr}({column})"',
                                                                               self.specification.generated_columns[column]]))

            if aggr == 'rank':
                for type, column in [(t, col) for t in types.Type for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = dense_rank({column})', f'{aggr}{column}', [types.INT])
                    add_condition(f'{aggr}d{column} = dense_rank(desc({column}))', f'{aggr}d{column}', [types.INT])
                    if column in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{aggr}{column} = dense_rank({column})"',
                                                                               self.specification.generated_columns[column]]))
                        self.predicates.append(DSLPredicate('happens_before', [f'"{aggr}d{column} = dense_rank(desc({column})"',
                                                                               self.specification.generated_columns[column]]))

            if aggr == 'row_number':
                add_condition(f'{aggr} = {aggr}()', f'{aggr}', [types.INT])

            if aggr in ['min', 'max']:
                for type, column in [(t, col) for t in [types.DATETIME, types.TIME] for col in frozen_columns[t]]:
                    add_condition(f'{aggr}{column} = {aggr}({column})', f'{aggr}{column}', [type])

            if aggr == 'first':
                add_condition('all$first', None, [])

            if util.get_config().force_summarise and current_predicate:
                self.predicates.append(DSLPredicate('constant_occurs', current_predicate))

    def generate_filter(self):
        int_ops = ['==', '>', '<', '>=', '<=']
        str_ops = ['==', '!=']

        filter_parts = defaultdict(OrderedSet)
        frozen_columns = self.specification.filter_columns(self.specification.columns_by_type)

        for constant in self.specification.consts_by_type[types.NONE]:
            for column in frozen_columns[types.STRING] | frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                filter_parts[frozenset((column, constant))].add(f"is.na({column})")
                filter_parts[frozenset((column, constant))].add(f"!is.na({column})")

        for constant in self.specification.consts_by_type[types.STRING]:
            for column in frozen_columns[types.STRING]:
                if 'str_detect' in self.specification.filters or 'like' in self.specification.filters:
                    filter_parts[frozenset((column, constant))].add(f"str_detect({column}, {types.to_r_repr(constant)})")

                if self.specification.constant_occurs(column, constant):
                    for op in str_ops:
                        filter_parts[frozenset((column, constant))].add(f"{column} {op} {types.to_r_repr(constant)}")

        for constant in self.specification.consts_by_type[types.INT] | self.specification.consts_by_type[types.FLOAT]:
            for column in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                for op in int_ops:
                    filter_parts[frozenset((column, constant))].add(f'{column} {op} {constant}')

        for constant in self.specification.consts_by_type[types.DATETIME]:
            for column in frozen_columns[types.DATETIME]:
                for op in int_ops:
                    filter_parts[frozenset((column, constant))].add(f"{column} {op} {self.specification.dateorder}('{constant}')")

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
            for attr2 in frozen_columns[types.INT] | frozen_columns[types.FLOAT]:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symmetric_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')

        bc = set()  # this set is used to ensure no redundant operations are created
        for attr1 in frozen_columns[types.DATETIME]:
            for attr2 in frozen_columns[types.DATETIME]:
                if attr1 == attr2:
                    continue
                for op in int_ops:
                    if (attr2, op, attr1) in bc or (attr1, symmetric_op(op), attr2) in bc:  # don't add symmetric conditions
                        continue
                    bc.add((attr2, op, attr1))
                    filter_parts[frozenset((attr2, attr1))].add(f'{attr2} {op} {attr1}')

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
                            if col in self.specification.generated_columns.keys():
                                self.predicates.append(
                                    DSLPredicate('happens_before', [f'"{conditions[-1]}"', self.specification.generated_columns[col]]))

                    else:
                        conditions.append(' & '.join(part_combo))
                        conditions.append(' | '.join(part_combo))
                        condition_map[cols].add(conditions[-1])
                        condition_map[cols].add(conditions[-2])

                        for col in cols:
                            if col in self.specification.generated_columns.keys():
                                self.predicates.append(
                                    DSLPredicate('happens_before', [f'"{conditions[-1]}"', self.specification.generated_columns[col]]))
                                self.predicates.append(
                                    DSLPredicate('happens_before', [f'"{conditions[-2]}"', self.specification.generated_columns[col]]))

        for spec_part in [self.specification.consts, self.specification.filters]:
            for constant in spec_part:
                current_predicate = []
                for key, value in condition_map.items():
                    if constant in key:
                        current_predicate += list(map(lambda v: f'"{v}"', value))
                if current_predicate:
                    self.predicates.append(DSLPredicate('constant_occurs', current_predicate))

        self.filter_conditions += conditions

    def generate_inner_join(self):
        column_pairs = []
        for cols in self.specification.columns_by_type.values():
            column_pairs += product(cols, repeat=2)

        on_conditions = list(combinations(column_pairs, 2)) + list(combinations(column_pairs, 1))
        on_conditions = list(filter(lambda t: any(map(lambda cond: cond[0] != cond[1], t)), on_conditions))

        for on_condition in on_conditions:
            self.inner_join_conditions.append(','.join(map(lambda x: f"'{x[0]}' = '{x[1]}'", on_condition)))

            for col_pairs in on_condition:  # we can only inner join after columns have been generated (by summarise)
                for col in col_pairs:
                    if col in self.specification.generated_columns.keys():
                        self.predicates.append(DSLPredicate('happens_before', [f'"{self.inner_join_conditions[-1]}"',
                                                                               self.specification.generated_columns[col]]))

        for subset in powerset_except_empty(self.specification.columns, util.get_config().max_join_combinations):
            self.inner_join_conditions.append(','.join(map(util.single_quote_str, subset)))

            for col in subset:
                if col in self.specification.generated_columns.keys():
                    self.predicates.append(
                        DSLPredicate('happens_before', [f'"{self.inner_join_conditions[-1]}"', self.specification.generated_columns[col]]))
