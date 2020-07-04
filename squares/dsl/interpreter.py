import math
import re
from itertools import permutations
from logging import getLogger
from typing import Tuple

from rpy2 import robjects
from z3 import BitVecVal

from .. import util, results
from ..program import LineInterpreter
from ..tyrell.interpreter import InterpreterError

logger = getLogger('squares.interpreter')


def get_type(df, index):
    _script = f'sapply({df}, class)[{index}]'
    ret_val = robjects.r(_script)
    return ret_val[0]


class RedudantError(InterpreterError):

    def __init__(self, *args):
        pass


def eval_decorator(func):
    def wrapper(self, args, key):
        if key and not self.final_interpretation and util.get_config().cache_ops:
            if not key in self.cache:
                name = util.get_fresh_name()
                self.try_execute(func(self, name, args))
                # if robjects.r(f'all_equal({name}, {args[0]}, convert=T, ignore_row_order=T)')[0] is True:
                #     results.redundant_lines += 1
                #     raise RedudantError()
                self.cache[key] = name
            return self.cache[key]
        name = util.get_fresh_name()
        script = func(self, name, args)
        if self.final_interpretation:
            self.program += script
        self.try_execute(script)
        return name

    return wrapper


class SquaresInterpreter(LineInterpreter):

    def __init__(self, problem, final_interpretation=False):
        self.problem = problem
        self.program = ''
        self.final_interpretation = final_interpretation
        self.cache = {}

    def try_execute(self, script):
        try:
            robjects.r(script)
        except Exception as e:
            logger.error("Error while evaluating program")
            logger.error("%s", str(e))
            raise InterpreterError(e)

    @eval_decorator
    def eval_filter(self, name, args):
        return f'{name} <- {args[0]} %>% filter({args[1]})\n'

    @eval_decorator
    def eval_filters(self, name, args):
        return f'{name} <- {args[0]} %>% filter({args[1]} {args[3]} {args[2]})\n'

    @eval_decorator
    def eval_summarise(self, name, args):
        re_object = re.fullmatch(r'([A-Za-z_]+)\$([A-Za-z_]+)', args[1])
        if re_object:
            return f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise_{re_object.groups()[0]}({re_object.groups()[1]}) %>% ungroup()\n'
        else:
            return f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise({args[1]}) %>% ungroup()\n'

    @eval_decorator
    def eval_mutate(self, name, args):
        return f'{name} <- {args[0]} %>% mutate({args[1]})\n'

    @eval_decorator
    def eval_inner_join(self, name, args):
        _script = f"{name} <- inner_join({args[0]}, {args[1]}, by=c({args[2]}), suffix = c('', '.other'))"
        for pair in args[2].split(','):
            if '=' in pair:
                A, B = pair.split('=')
                A = A.strip()[1:-1]
                B = B.strip()[1:-1]
                if A.strip() != B.strip():
                    _script += f' %>% mutate({B} = {A})'
        return _script + '\n'

    @eval_decorator
    def eval_natural_join(self, name, args):
        return f'{name} <- inner_join({args[0]}, {args[1]})\n'

    @eval_decorator
    def eval_natural_join3(self, name, args):
        return f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]})\n'

    @eval_decorator
    def eval_natural_join4(self, name, args):
        return f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]}) %>% inner_join({args[3]})\n'

    @eval_decorator
    def eval_anti_join(self, name, args):
        return f'{name} <- anti_join({args[0]}, {args[1]}, by=c({args[2]}))\n'

    @eval_decorator
    def eval_left_join(self, name, args):
        return f'{name} <- left_join({args[0]}, {args[1]})\n'

    @eval_decorator
    def eval_union(self, name, args):
        return f'{name} <- bind_rows({args[0]}, {args[1]})\n'

    @eval_decorator
    def eval_intersect(self, name, args):
        return f'{name} <- intersect(select({args[0]},{args[2]}), select({args[1]}, {args[2]}))\n'

    @eval_decorator
    def eval_semi_join(self, name, args):
        return f'{name} <- semi_join({args[0]}, {args[1]})\n'

    @eval_decorator
    def eval_cross_join(self, name, args):
        _script = f'{name} <- full_join({args[0]}, {args[1]}, by=character(), suffix = c("", ".other"))'

        if args[2] != '':
            _script += f' %>% filter({args[2]})'
        return _script + '\n'

    @eval_decorator
    def eval_unite(self, name, args):
        return f'{name} <- unite({args[0]}, {args[1]}, {args[1]}, {args[2]}, sep=":", remove=F)\n'

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

    def equals(self, actual: str, expect: str, *args) -> Tuple[bool, float]:
        if robjects.r(f'nrow({actual})')[0] == 0:
            results.empty_output += 1

        # with rpy2.robjects.conversion.localconverter(robjects.default_converter + pandas2ri.converter):
        #     print(robjects.conversion.rpy2py(robjects.r(actual)))

        score = robjects.r(f'ue <- {expect} %>% unlist %>% unique;length(intersect({actual} %>% unlist %>% unique, ue)) / length(ue)')[0]
        if math.isnan(score):
            score = 0

        if score < 1:
            return False, score

        a_cols = list(robjects.r(f'colnames({actual})'))
        e_cols = list(robjects.r(f'colnames({expect})'))
        for combination in permutations(a_cols, len(e_cols)):
            for d in ['', ' %>% distinct()']:
                _script = f'out <- {actual} %>% select({", ".join(map(lambda pair: f"{pair[0]} = {pair[1]}" if pair[0] != pair[1] else pair[0], zip(e_cols, combination)))}){d}'
                try:
                    robjects.r(_script)
                    if self.test_equality('out', expect, False):
                        if self.final_interpretation:
                            for perm in util.get_permutations(e_cols, len(e_cols)):
                                name = util.get_fresh_name()
                                new_script = f'{name} <- out %>% arrange({perm})'
                                robjects.r(new_script)
                                if self.test_equality(name, expect, True):
                                    _script += f' %>% arrange({perm})'
                                    break

                            self.program += _script + '\n'
                        return True, score
                except:
                    continue
        return False, score

    def test_equality(self, actual: str, expect: str, keep_order: bool = False) -> bool:
        if not keep_order:
            _script = f'all_equal({actual}, {expect}, convert=T)'
        else:
            _script = f'all_equal({actual}, {expect}, convert=T, ignore_row_order=T)'
        try:
            return robjects.r(_script)[0] is True
        except:
            return False
