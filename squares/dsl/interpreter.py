import math
import re
import time
from itertools import permutations
from logging import getLogger

from rpy2 import robjects as robjects
from z3 import BitVecVal

from .. import util, results
from ..program import LineInterpreter
from ..util import get_fresh_name, get_permutations

logger = getLogger('squares.interpreter')


def get_type(df, index):
    _script = f'sapply({df}, class)[{index}]'
    ret_val = robjects.r(_script)
    return ret_val[0]


class SquaresInterpreter(LineInterpreter):

    def __init__(self, problem, sort_columns = False):
        self.problem = problem
        self.program = ''
        self.sort_columns = sort_columns

    def eval_filter(self, name, args):
        self.program += f'{name} <- {args[0]} %>% filter({args[1]})\n'

    def eval_filters(self, name, args):
        self.program += f'{name} <- {args[0]} %>% filter({args[1]} {args[3]} {args[2]})\n'

    def eval_summarise(self, name, args):
        re_object = re.fullmatch(r'([A-Za-z_]+)\$([A-Za-z_]+)', args[1])
        if re_object:
            self.program += f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise_{re_object.groups()[0]}({re_object.groups()[1]}) %>% ungroup()\n'
        else:
            self.program += f'{name} <- {args[0]} %>% group_by({args[2]}) %>% summarise({args[1]}) %>% ungroup()\n'

    def eval_mutate(self, name, args):
        self.program += f'{name} <- {args[0]} %>% mutate({args[1]})\n'

    def eval_inner_join(self, name, args):
        _script = f"{name} <- inner_join({args[0]}, {args[1]}, by=c({args[2]}), suffix = c('', '.other'))"
        for pair in args[2].split(','):
            if '=' in pair:
                A, B = pair.split('=')
                A = A.strip()[1:-1]
                B = B.strip()[1:-1]
                if A.strip() != B.strip():
                    _script += f' %>% mutate({B} = {A})'
        self.program += _script + '\n'

    def eval_natural_join(self, name, args):
        self.program += f'{name} <- inner_join({args[0]}, {args[1]})\n'

    def eval_natural_join3(self, name, args):
        self.program += f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]})\n'

    def eval_natural_join4(self, name, args):
        self.program += f'{name} <- inner_join({args[0]}, {args[1]}) %>% inner_join({args[2]}) %>% inner_join({args[3]})\n'

    def eval_anti_join(self, name, args):
        self.program += f'{name} <- anti_join({args[0]}, {args[1]}, by=c({args[2]}))\n'

    def eval_left_join(self, name, args):
        self.program += f'{name} <- left_join({args[0]}, {args[1]})\n'

    def eval_union(self, name, args):
        self.program += f'{name} <- bind_rows({args[0]}, {args[1]})\n'

    def eval_intersect(self, name, args):
        self.program += f'{name} <- intersect(select({args[0]},{args[2]}), select({args[1]}, {args[2]}))\n'

    def eval_semi_join(self, name, args):
        self.program += f'{name} <- semi_join({args[0]}, {args[1]})\n'

    def eval_cross_join(self, name, args):
        if args[2] == '':
            _script = f'{name} <- full_join({args[0]} %>% mutate(tmp.col=1), {args[1]} %>% mutate(tmp.col=1), by="tmp.col", suffix = c("", ".other")) %>% select(-tmp.col))'
        else:
            _script = f'{name} <- full_join({args[0]} %>% mutate(tmp.col=1), {args[1]} %>% mutate(tmp.col=1), by="tmp.col", suffix = c("", ".other")) %>% select(-tmp.col) %>% filter({args[2]})'
        self.program += _script + '\n'

    def eval_unite(self, name, args):
        self.program += f'{name} <- unite({args[0]}, {args[1]}, {args[1]}, {args[2]}, sep=":")\n'

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
        start = time.time()
        score = \
        tuple(robjects.r(f'ue <- {expect} %>% unlist %>% unique;length(intersect({actual} %>% unlist %>% unique, ue)) / length(ue)'))[0]
        if math.isnan(score):
            score = 0
        # score -= .5
        if score > 0 and util.get_program_queue():
            util.get_program_queue().put((tuple(line.production.name for line in args[0]), score * 10))

        if score < 1:
            results.ResultsHolder().equality_time += time.time() - start
            return False

        a_cols = list(robjects.r(f'colnames({actual})'))
        e_cols = list(robjects.r(f'colnames({expect})'))
        for combination in permutations(a_cols, len(e_cols)):
            for d in ['', ' %>% distinct()']:
                _script = f'out <- {actual} %>% select({", ".join(map(lambda pair: f"{pair[0]} = {pair[1]}" if pair[0] != pair[1] else pair[0], zip(e_cols, combination)))}){d}'
                try:
                    robjects.r(_script)
                    if self.test_equality('out', expect, False):
                        if self.sort_columns:
                            for perm in get_permutations(e_cols, len(e_cols)):
                                name = get_fresh_name()
                                new_script = f'{name} <- out %>% arrange({perm})'
                                robjects.r(new_script)
                                if self.test_equality(name, expect, True):
                                    _script += f' %>% arrange({perm})'
                                    break

                            self.program += _script + '\n'
                        results.ResultsHolder().equality_time += time.time() - start
                        return True
                except:
                    continue
        results.ResultsHolder().equality_time += time.time() - start
        return False

    def test_equality(self, actual: str, expect: str, keep_order: bool = False) -> bool:
        if not keep_order:
            _script = f'all_equal({actual}, {expect}, convert=T)'
        else:
            _script = f'all_equal({actual}, {expect}, convert=T, ignore_row_order=T)'
        try:
            return robjects.r(_script)[0] is True
        except:
            return False
