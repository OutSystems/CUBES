import logging
from itertools import combinations, permutations

import re
from rpy2 import robjects as robjects
from z3 import BitVecVal

from squares import util
from squares.exceptions import REvaluationError
from squares.tyrell.interpreter import PostOrderInterpreter, InterpreterError
from squares.tyrell.logger import get_logger
from squares.util import get_fresh_name, get_permutations

logger = get_logger('squares.interpreter')


def get_type(df, index):
    _script = f'sapply({df}, class)[{index}]'
    ret_val = robjects.r(_script)
    return ret_val[0]


def cached(func):
    def wrap(*args, **kwargs):
        if not util.get_config().cache_ops:
            return func(*args, **kwargs)
        if (args[1], tuple(args[2])) in args[0].op_cache:
            tmp = args[0].op_cache[(args[1], tuple(args[2]))]
            if tmp is InterpreterError:
                raise tmp
            else:
                return tmp
        else:
            try:
                res = func(*args, **kwargs)
                args[0].op_cache[(args[1], tuple(args[2]))] = res
                return res
            except InterpreterError as e:
                args[0].op_cache[(args[1], tuple(args[2]))] = e
                raise e
    return wrap


class SquaresInterpreter(PostOrderInterpreter):

    def __init__(self, problem, store_program):
        self.problem = problem
        self.store_program = store_program
        self.final_program = ''
        self.op_cache = {}

    @staticmethod
    def fresh_table():
        return get_fresh_name()

    def save_and_try_execute(self, script, name, node):
        if self.store_program:
            self.final_program += script + '\n'
        try:
            robjects.r(script)
            return name
        except Exception as e:
            raise REvaluationError(node, e)

    @cached
    def eval_filter(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- {args[0]} %>% filter({args[1]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_filters(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- {args[0]} %>% filter({args[1]} {args[3]} {args[2]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_summarise(self, node, args):
        name = self.fresh_table()

        re_object = re.fullmatch(r'([A-Za-z_]+)\$([A-Za-z_]+)', args[1])
        if re_object:
            _script = f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise_{re_object.groups()[0]}({re_object.groups()[1]}) %>% ungroup()'
        else:
            _script = f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise({args[1]}) %>% ungroup()'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_inner_join(self, node, args):
        name = self.fresh_table()
        _script = f"{name} <- inner_join({args[0]}, {args[1]}, by=c({args[2]}), suffix = c('', '.other'))"
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_natural_join(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- inner_join({args[0]}, {args[1]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_natural_join3(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_natural_join4(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]}) %>% inner_join({args[3]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_anti_join(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- anti_join({args[0]}, {args[1]}, by=c({args[2]}))'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_left_join(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- left_join({args[0]}, {args[1]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_union(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- bind_rows({args[0]}, {args[1]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_intersect(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- intersect(select({args[0]},{args[2]}), select({args[1]}, {args[2]}))'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_semi_join(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- semi_join({args[0]}, {args[1]})'
        return self.save_and_try_execute(_script, name, node)

    @cached
    def eval_unite(self, node, args):
        name = self.fresh_table()
        _script = f'{name} <- unite({args[0]}, {args[1]}, {args[1]}, {args[2]}, sep=":")'
        return self.save_and_try_execute(_script, name, node)

    def apply_row(self, val):
        df = robjects.r(val)
        return df.nrow

    def apply_col(self, val):
        df = robjects.r(val)
        return df.ncol

    def apply_columns(self, val):
        a = list(robjects.r(f'colnames({val})'))
        bools = list(map(lambda c: c in a, self.problem.all_columns))
        return BitVecVal(util.boolvec2int(bools), util.get_config().bitvector_size)

    def equals(self, actual: str, expect: str, *args) -> bool:
        a_cols = list(robjects.r(f'colnames({actual})'))
        e_cols = list(robjects.r(f'colnames({expect})'))

        for combination in permutations(a_cols, len(e_cols)):
            for d in ['', ' %>% distinct()']:
                _script = f'out <- {actual} %>% select({", ".join(map(lambda pair: f"{pair[0]} = {pair[1]}" if pair[0] != pair[1] else pair[0], zip(e_cols, combination)))}){d}'
                try:
                    robjects.r(_script)
                    if self.test_equality('out', expect, False, *args):
                        if self.store_program:
                            for perm in get_permutations(e_cols, len(e_cols)):
                                name = get_fresh_name()
                                new_script = f'{name} <- out %>% arrange({perm})'
                                robjects.r(new_script)
                                if self.test_equality(name, expect, True, *args):
                                    _script += f' %>% arrange({perm})'
                                    break

                            self.final_program += _script + '\n'
                        return True
                except:
                    continue
        return False

    def test_equality(self, actual: str, expect: str, keep_order: bool = False, *args) -> bool:
        if logger.isEnabledFor(logging.DEBUG):
            a_dim = tuple(robjects.r(f'dim({actual})'))
            e_dim = tuple(robjects.r(f'dim({expect})'))
            # if a_dim[0] == e_dim[0] or a_dim[1] == e_dim[1]:
            #     if util.get_program_queue():
            #         util.get_program_queue().put(tuple(r.production.name for r in args[0]))

        if not keep_order:
            _script = f'all_equal({actual}, {expect}, convert=T)'
        else:
            _script = f'all_equal({actual}, {expect}, convert=T, ignore_row_order=T)'
        try:
            return True == robjects.r(_script)[0]  # TODO this tests if the result is actually True, and not just True-like
        except:
            return False