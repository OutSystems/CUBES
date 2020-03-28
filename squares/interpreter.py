from rpy2 import robjects as robjects
from z3 import BitVecVal

from . import util
from .exceptions import REvaluationError
from .tyrell.interpreter import PostOrderInterpreter, GeneralError
from .util import get_fresh_name


def get_type(df, index):
    _rscript = f'sapply({df}, class)[{index}]'
    ret_val = robjects.r(_rscript)
    return ret_val[0]


def cols_to_c_vector(col_list):
    return 'c(' + ','.join(map(lambda x: f'"{x}"', col_list.split(','))) + ')'


class SquaresInterpreter(PostOrderInterpreter):

    def __init__(self, problem, store_program):
        self.problem = problem
        self.store_program = store_program
        self.final_program = ''

    def fresh_table(self):
        name = get_fresh_name()
        return name

    def save_and_try_execute(self, script, name, node):
        if self.store_program:
            self.final_program += script + '\n'
        try:
            robjects.r(script)
            return name
        except Exception as e:
            raise REvaluationError(node, e)

    def eval_select(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- {args[0]} %>% select({args[1]})'

        if args[2] == 'distinct':
            _script += ' %>% distinct()'

        return self.save_and_try_execute(_script, name, node)

    def eval_filter(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- {args[0]} %>% filter({args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_filters(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- {args[0]} %>% filter({args[1]} {args[3]} {args[2]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_summariseGrouped(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise({args[1]}) %>% ungroup()'

        return self.save_and_try_execute(_script, name, node)

    def eval_inner_join(self, node, args):
        name = self.fresh_table()

        _script = f"{name} <- inner_join({args[0]}, {args[1]}, by=c({args[2]}), suffix = c('', '.other'))"

        return self.save_and_try_execute(_script, name, node)

    def eval_natural_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join({args[0]}, {args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_natural_join3(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join(inner_join({args[0]}, {args[1]}), {args[2]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_natural_join4(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join(inner_join(inner_join({args[0]}, {args[1]}), {args[2]}), {args[3]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_anti_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- anti_join({args[0]}, {args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_left_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- left_join({args[0]}, {args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_bind_rows(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- bind_rows({args[0]}, {args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_intersect(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- intersect(select({args[0]},{args[2]}), select({args[1]}, {args[2]}))'

        return self.save_and_try_execute(_script, name, node)

    def eval_semi_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- semi_join({args[0]}, {args[1]})'

        return self.save_and_try_execute(_script, name, node)

    def eval_unite(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- unite({args[0]}, {args[1]}, which(colnames({args[0]})=="{args[1]}"), {args[2]}, which(colnames({args[0]})=="{args[2]}"), sep=":")'

        return self.save_and_try_execute(_script, name, node)

    def apply_row(self, val):
        df = val
        if isinstance(val, str):
            df = robjects.r(val)
        return df.nrow

    def apply_col(self, val):
        df = val
        if isinstance(val, str):
            df = robjects.r(val)
        return df.ncol

    def apply_columns(self, val):
        a = list(robjects.r(f'tbl_vars({val})'))
        bools = list(map(lambda c: c in a, self.problem.all_columns))
        return BitVecVal(util.boolvec2int(bools), util.get_config().bitvector_size)


def eq_r(actual, expect):
    _rscript = f'all_equal(lapply({actual}, as.character), lapply({expect}, as.character))'
    try:
        ret_val = robjects.r(_rscript)
    except:
        return False
    return True == ret_val[0]
