from rpy2 import robjects as robjects

from squares.util import get_fresh_name, current_counter
from tyrell.interpreter import PostOrderInterpreter, GeneralError


def get_collist(sel):
    return sel


def get_type(df, index):
    _rscript = f'sapply({df}, class)[{index}]'
    ret_val = robjects.r(_rscript)
    return ret_val[0]


class SquaresInterpreter(PostOrderInterpreter):

    def __init__(self, problem, store_program):
        self.problem = problem
        self.store_program = store_program
        self.final_program = ""

    # get the string format to be used in filter
    def transform_const(self, cons):
        try:
            int(cons)
            return str(cons)
        except:
            if str(cons) == "max(n)" or cons in self.problem.attributes:
                return str(cons)
            else:
                return '"' + str(cons) + '"'

    def fresh_table(self):
        name = get_fresh_name()
        self.problem._tables[name] = current_counter()
        return name

    def eval_ColInt(self, v):
        return v

    def eval_ColList(self, v):
        return v

    def eval_const(self, node, args):
        return args[0]

    def eval_unused(self, node, args):
        return get_fresh_name()

    def eval_select(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- {args[0]} %>% ungroup() %>% select({get_collist(args[1])})'

        if args[2] == "distinct":
            _script += ' %>% distinct()'
        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_filter(self, node, args):
        name = self.fresh_table()

        if 'str_detect' not in args[1]:
            col, op, const = args[1].split(" ", 2)
            if const != "max(n)":
                _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({col} {op} {self.transform_const(const)})'
            else:
                _script = f'{name} <- filter({args[0]}, {col} {op} max(n))'
        else:
            col, string = args[1].split("|")
            _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({col}, "{string[:-1]}"))'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_filters(self, node, args):
        name = self.fresh_table()

        if "str_detect" not in args[1]:
            col, op, const = args[1].split(" ", 2)
            const = self.transform_const(const) if const != "max(n)" else "max(n)"
            arg1 = col + " " + op + " " + const
        else:
            col, string = args[1].split("|")
            arg1 = col + ", " + "\"" + string[:-1] + "\")"

        if "str_detect" not in args[2]:
            col, op, const = args[2].split(" ", 2)
            const = self.transform_const(const) if const != "max(n)" else "max(n)"
            arg2 = col + " " + op + " " + const
        else:
            col, string = args[2].split("|")
            arg2 = col + ", " + "\"" + string[:-1] + "\")"

        _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({arg1} {args[3]} {arg2})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_summariseGrouped(self, node, args):
        name = self.fresh_table()

        if "paste" in args[1]:
            args[1] = '{at} = paste({at}, collapse=:)'.format(at=args[1].split("|")[1])
        args[1] = args[1].replace(':', '":"')

        _script = f'{name} <- {args[0]} %>% group_by({get_collist(args[2])}) %>% summarise({args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_summarise(self, node, args):
        name = self.fresh_table()

        if "paste" in args[1]:
            args[1] = '{at} = paste({at}, collapse=":")'.format(at=args[1].split("|")[1])

        _script = f'{name} <- {args[0]} %>% summarise({args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_inner_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join({args[0]}, {args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_inner_join3(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join(inner_join({args[0]}, {args[1]}), {args[2]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_inner_join4(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- inner_join(inner_join(inner_join({args[0]}, {args[1]}), {args[2]}), {args[3]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_anti_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- anti_join(select({args[0]},{get_collist(args[2])}), select({args[1]}, {get_collist(args[2])}))'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_left_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- left_join({args[0]}, {args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_bind_rows(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- bind_rows({args[0]}, {args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_intersect(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- intersect(select({args[0]},{get_collist(args[2])}), select({args[1]}, {get_collist(args[2])}))'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_semi_join(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- semi_join({args[0]}, {args[1]})'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

    def eval_unite(self, node, args):
        name = self.fresh_table()

        _script = f'{name} <- unite({args[0]}, {get_collist(args[1])}, which(colnames({args[0]})=="{get_collist(args[1])}"), {get_collist(args[2])}, which(colnames({args[0]})=="{get_collist(args[2])}"), sep=":")'

        if self.store_program:
            self.final_program += _script + "\n"
        try:
            robjects.r(_script)
            return name
        except Exception as e:
            raise GeneralError(str(e))

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

    def apply_name(self, val):
        return self.problem._tables[val]
