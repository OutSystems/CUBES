import math
import re
from itertools import permutations
from logging import getLogger
from typing import Tuple, Union

import pylru
from rpy2 import robjects
from rpy2.rinterface_lib.embedded import RRuntimeError
from z3 import BitVecVal

import rpy2
from rpy2.robjects.packages import importr
import pandas
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter

from .. import util, results
from ..decider import RowNumberInfo
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
    def wrapper(self, args, key, line_index):
        if key and not self.final_interpretation and util.get_config().cache_ops:
            # self._cache_counter += 1
            # if self._cache_counter >= 25:
            #     self._cache_counter = 0
            #     total_size = sum(map(lambda x: list(robjects.r(f'object.size({x})'))[0], self.cache.values()))
            #     total_size_est = (total_size / len(self.cache)) * self.cache.size()
            #     print('Estimated cache size if it were full: ', util.sizeof_fmt(total_size_est))
            #     if total_size_est >= 78643200:
            #         new_size = max(int(52428800 / (total_size / len(self.cache))), 1)
            #         print('Updating max number of elements to', new_size)
            #         self.cache.size(new_size)
            #     elif total_size_est <= 26214400:
            #         new_size = max(int(52428800 / (total_size / len(self.cache))), 1)
            #         print('Updating max number of elements to', new_size)
            #         self.cache.size(new_size)
            if not key in self.cache:
                results.cache_miss += 1
                name = util.get_fresh_name()
                self.try_execute(func(self, name, args))
                # if robjects.r(f'all_equal({name}, {args[0]}, convert=T, ignore_row_order=T)')[0] is True:
                #     results.redundant_lines += 1
                #     raise RedudantError()
                # print(list(robjects.r(f'object.size({name})'))[0])
                self.cache[key] = name
            else:
                results.cache_hit += 1
            return self.cache[key]
        name = util.get_fresh_name()
        # self.current_vars.add(name)
        script = func(self, name, args)
        if self.final_interpretation:
            self.program += script
        self.try_execute(script)
        return name

    return wrapper


def cache_evict(key, val):
    # print(list(robjects.r(f'object.size({val})'))[0])
    robjects.r(f'rm({val})')


class SquaresInterpreter(LineInterpreter):

    def __init__(self, problem, final_interpretation=False):
        self.problem = problem
        self.program = ''
        self.final_interpretation = final_interpretation
        self.cache = pylru.lrucache(40, cache_evict)
        self._cache_counter = 0
        # self.current_vars = set()

    def try_execute(self, script):
        try:
            # print(script, end='')
            robjects.r(script)
        except (Exception, RRuntimeError) as e:
            # logger.error("Error while evaluating program")
            # logger.error("%s", str(e))
            raise InterpreterError(e)

    @eval_decorator
    def eval_filter(self, name, args):
        return f'{name} <- {args[0]} %>% filter({args[1]})\n'

    @eval_decorator
    def eval_filters(self, name, args):
        return f'{name} <- {args[0]} %>% filter({args[1]} {args[3]} {args[2]})\n'

    @eval_decorator
    def eval_summarise(self, name, args):
        args2 = args[2].replace("'", "")
        re_object = re.fullmatch(r'([A-Za-z_]+)\$([A-Za-z_]+)', args[1])
        if re_object:
            return f'{name} <- {args[0]} %>% group_by({args2}) %>% summarise_{re_object.groups()[0]}({re_object.groups()[1]}) %>% ungroup()\n'
        else:
            return f'{name} <- {args[0]} %>% group_by({args2}) %>% summarise({args[1]}) %>% ungroup()\n'

    @eval_decorator
    def eval_mutate(self, name, args):
        re_object = re.fullmatch(r'([A-Za-z_]+)\$([A-Za-z_]+)', args[1])
        if re_object:
            return f'{name} <- {args[0]} %>% mutate_{re_object.groups()[0]}({re_object.groups()[1]})\n'
        else:
            return f'{name} <- {args[0]} %>% mutate({args[1]})\n'

    @eval_decorator
    def eval_inner_join(self, name, args):
        _script = f"{name} <- inner_join({args[0]}, {args[1]}, by=c({args[2]}), suffix = c('', '.other'), na_matches='{util.get_config().na_matches}')"
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
        if robjects.r(f'length(intersect(colnames({args[0]}), colnames({args[1]})))')[0] > 0:
            return f'{name} <- inner_join({args[0]}, {args[1]}, na_matches="{util.get_config().na_matches}")\n'
        else:
            return f'{name} <- full_join({args[0]}, {args[1]}, by=character(), na_matches="{util.get_config().na_matches}")\n'

    @eval_decorator
    def eval_natural_join3(self, name, args):
        _script = f'{name} <- '
        if robjects.r(f'length(intersect(colnames({args[0]}), colnames({args[1]})))')[0] > 0:
            _script += f'inner_join({args[0]}, {args[1]}, na_matches="{util.get_config().na_matches}") '
        else:
            _script += f'full_join({args[0]}, {args[1]}, by=character(), na_matches="{util.get_config().na_matches}") '
        if robjects.r(f'length(intersect(union(colnames({args[0]}), colnames({args[1]})), colnames({args[2]})))')[0] > 0:
            _script += f'%>% inner_join({args[2]}, na_matches="{util.get_config().na_matches}")\n'
        else:
            _script += f'%>% full_join({args[2]}, by=character(), na_matches="{util.get_config().na_matches}")\n'
        return _script

    @eval_decorator
    def eval_natural_join4(self, name, args):
        _script = f'{name} <- '
        if robjects.r(f'length(intersect(colnames({args[0]}), colnames({args[1]})))')[0] > 0:
            _script += f'inner_join({args[0]}, {args[1]}, na_matches="{util.get_config().na_matches}") '
        else:
            _script += f'full_join({args[0]}, {args[1]}, by=character(), na_matches="{util.get_config().na_matches}") '
        if robjects.r(f'length(intersect(union(colnames({args[0]}), colnames({args[1]})), colnames({args[2]})))')[0] > 0:
            _script += f'%>% inner_join({args[2]}, na_matches="{util.get_config().na_matches}") '
        else:
            _script += f'%>% full_join({args[2]}, by=character(), na_matches="{util.get_config().na_matches}") '
        if robjects.r(f'length(intersect(union(union(colnames({args[0]}), colnames({args[1]})), colnames({args[2]})), colnames({args[3]})))')[0] > 0:
            _script += f'%>% inner_join({args[3]}, na_matches="{util.get_config().na_matches}")\n'
        else:
            _script += f'%>% full_join({args[3]}, by=character(), na_matches="{util.get_config().na_matches}")\n'
        return _script

    @eval_decorator
    def eval_anti_join(self, name, args):
        return f'{name} <- anti_join({args[0]}, {args[1]}, by=c({args[2]}), na_matches="{util.get_config().na_matches}")\n'

    @eval_decorator
    def eval_left_join(self, name, args):
        return f'{name} <- left_join({args[0]}, {args[1]}, na_matches="{util.get_config().na_matches}")\n'

    @eval_decorator
    def eval_union(self, name, args):
        return f'{name} <- bind_rows({args[0]}, {args[1]})\n'

    @eval_decorator
    def eval_intersect(self, name, args):
        return f'{name} <- intersect(select({args[0]},{args[2]}), select({args[1]}, {args[2]}))\n'

    @eval_decorator
    def eval_semi_join(self, name, args):
        return f'{name} <- semi_join({args[0]}, {args[1]}, na_matches="{util.get_config().na_matches}")\n'

    @eval_decorator
    def eval_cross_join(self, name, args):
        _script = f'{name} <- full_join({args[0]}, {args[1]}, by=character(), suffix = c("", ".other"), na_matches="{util.get_config().na_matches}")'

        if args[2] != '':
            _script += f' %>% filter({args[2]})'
        return _script + '\n'

    @eval_decorator
    def eval_unite(self, name, args):
        return f'{name} <- unite({args[0]}, {args[1]}, {args[1]}, {args[2]}, sep=":", remove=F)\n'

    @eval_decorator
    def eval_limit(self, name, args):
        self.has_limit = True
        return f'{name} <- {args[0]} %>% arrange({args[1]}) %>% head({self.problem.output_table.df.shape[0]})\n'

    def apply_row(self, val):
        df = robjects.r(val)
        return df.nrow

    def apply_col(self, val):
        df = robjects.r(val)
        return df.ncol

    def apply_columns(self, val):
        a = list(robjects.r(f'colnames({val})'))
        bools = list(map(lambda c: c in a, self.problem.all_columns))
        raise NotImplementedError()

    def equals(self, actual: str, expect: str, *args) -> Tuple[bool, float, Union[RowNumberInfo, None]]:
        if robjects.r(f'nrow({actual})')[0] == 0:
            results.empty_output += 1

        # with rpy2.robjects.conversion.localconverter(robjects.default_converter + pandas2ri.converter):
        #     print(robjects.conversion.rpy2py(robjects.r(actual)))

        score = robjects.r(f'ue <- unique(reduce(map({expect}, as.list), c)) %>% {{intersect(.,.)}};length(intersect(unique(reduce(map({actual}, as.list), c)) %>% {{intersect(.,.)}}, ue)) / length(ue)')[0]
        if math.isnan(score):
            score = 0
        # print(score)

        # with localconverter(robjects.default_converter + pandas2ri.converter):
        #     pandas.set_option('display.max_columns', None)
        #     print('EXPECTED')
        #     print(robjects.conversion.rpy2py(robjects.r[expect]))
        #     print(list(map(list, robjects.r(f'unique(reduce(map({expect}, as.list), c)) %>% {{intersect(.,.)}}'))))
        #     print('ACTUAL')
        #     print(robjects.conversion.rpy2py(robjects.r[actual]))
        #     print(list(map(list, robjects.r(f'unique(reduce(map({actual}, as.list), c)) %>% {{intersect(.,.)}}'))))
        #     print('INTERSECTION')
        #     print(list(map(list, robjects.r(f'intersect(unique(reduce(map({expect}, as.list), c)) %>% {{intersect(.,.)}}, unique(reduce(map({actual}, as.list), c)) %>% {{intersect(.,.)}})'))))

        if not util.get_config().subsume_conditions and score < 1:
            return False, score, None
        # elif len(args) > 0:
        #     logger.info('Promising program found: %s', args[0])

        a_cols = list(robjects.r(f'colnames({actual})'))
        e_cols = list(robjects.r(f'colnames({expect})'))
        expected_n = int(robjects.r(f'nrow({expect})')[0])
        result = None
        for combination in permutations(a_cols, len(e_cols)):
            for d in ['', ' %>% distinct()']:
                _script = f'out <- {actual} %>% select({", ".join(map(lambda pair: f"{pair[0]} = {pair[1]}" if pair[0] != pair[1] else pair[0], zip(e_cols, combination)))}){d}'
                try:
                    robjects.r(_script)
                    if self.test_equality('out', expect, False):
                        if self.final_interpretation:
                            if not self.has_limit:
                                for perm in util.get_permutations(e_cols, len(e_cols)):
                                    name = util.get_fresh_name()
                                    new_script = f'{name} <- out %>% arrange({perm})'
                                    robjects.r(new_script)
                                    if self.test_equality(name, expect, True):
                                        _script += f' %>% arrange({perm})'
                                        break

                            self.program += _script + '\n'
                        return True, score, None
                except Exception as e:
                    logger.error("Error while testing program")
                    logger.error("%s", str(e))
                finally:
                    if util.get_config().subsume_conditions and result != RowNumberInfo.UNKNOWN:
                        actual_n = int(robjects.r(f'nrow(out)')[0])
                        if actual_n > expected_n:
                            if result is None or result == RowNumberInfo.LESS_ROWS:
                                result = RowNumberInfo.LESS_ROWS
                            else:
                                result = RowNumberInfo.UNKNOWN
                        if actual_n < expected_n:
                            if result is None or result == RowNumberInfo.MORE_ROWS:
                                result = RowNumberInfo.MORE_ROWS
                            else:
                                result = RowNumberInfo.UNKNOWN
        return False, score, result

    def test_equality(self, actual: str, expect: str, keep_order: bool = False) -> bool:
        if not keep_order:
            _script = f'all_equal({actual} %>% mutate(across(where(function(x) {{is.numeric(x) & is.double(x)}}), round, {util.get_config().fp_comparison_digits})), {expect} %>% mutate(across(where(function(x) {{is.numeric(x) & is.double(x)}}), round, {util.get_config().fp_comparison_digits})), convert=T)'
        else:
            _script = f'all_equal({actual} %>% mutate(across(where(function(x) {{is.numeric(x) & is.double(x)}}), round, {util.get_config().fp_comparison_digits})), {expect} %>% mutate(across(where(function(x) {{is.numeric(x) & is.double(x)}}), round, {util.get_config().fp_comparison_digits})), convert=T, ignore_row_order=T)'
        # if not keep_order:
        #     _script = f'all_equal({actual}, {expect}, convert=T)'
        # else:
        #     _script = f'all_equal({actual}, {expect}, convert=T, ignore_row_order=T)'
        try:
            return robjects.r(_script)[0] is True
        except:
            return False
