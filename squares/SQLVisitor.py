from tyrell.interpreter import PostOrderInterpreter, GeneralError


class SQLVisitor(PostOrderInterpreter):

    def transform_col(self, col: str):
        if ' ' in col:
            return '`' + col + '`'

        return col

    def transform_const(self, const: str):
        if const == 'max(n)':
            return 'MAX(n)'
        else:
            try:
                float(const)
                return const
            except:
                return '"' + const.replace('\\', '\\\\').replace('"', '\\"') + '"'

    def transform_bool_op(selfs, op):
        if op == '&':
            return 'AND'
        elif op == '|':
            return 'OR'
        else:
            raise GeneralError()

    def transform_summarise(self, param):
        if param == 'n = n()':
            return 'COUNT() AS n'
        else:
            raise GeneralError()

    def transform_filter_condition(self, cond):
        col, op, const = cond.split(' ', 3)  # FIXME assumes col doesn't have whitespace

        return self.transform_col(col) + ' ' + op + ' ' + self.transform_const(const)

    def eval_ColInt(self, v):
        return v

    def eval_ColList(self, v):
        return v

    def eval_const(self, node, args):
        return args[0]

    def eval_unused(self, node, args):
        return ""

    def eval_select(self, node, args):
        command = 'SELECT DISTINCT' if args[2] == 'distinct' else 'SELECT'

        return f'{command} {args[1]} FROM {args[0]}'

    def eval_filter(self, node, args):
        return f'{args[0]} WHERE {self.transform_filter_condition(args[1])}'
        # if 'str_detect' not in args[1]:
        #     col, op, const = args[1].split(" ")
        #     if const != "max(n)":
        #         _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({col} {op} {self.getConst(const)})'
        #     else:
        #         _script = f'{name} <- filter({args[0]}, {col} {op} max(n))'
        # else:
        #     col, string = args[1].split("|")
        #     _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({col}, "{string[:-1]}"))'

    def eval_filters(self, node, args):
        return f'{args[0]} WHERE ({self.transform_filter_condition(args[1])} {self.transform_bool_op(args[3])} {self.transform_filter_condition(args[2])})'
        # if "str_detect" not in args[1]:
        #     col, op, const = args[1].split(" ")
        #     const = self.getConst(const) if const != "max(n)" else "max(n)"
        #     arg1 = col + " " + op + " " + const
        # else:
        #     col, string = args[1].split("|")
        #     arg1 = col + ", " + "\"" + string[:-1] + "\")"
        # if "str_detect" not in args[2]:
        #     col, op, const = args[2].split(" ")
        #     const = self.getConst(const) if const != "max(n)" else "max(n)"
        #     arg2 = col + " " + op + " " + const
        # else:
        #     col, string = args[2].split("|")
        #     arg2 = col + ", " + "\"" + string[:-1] + "\")"
        #
        # _script = f'{name} <- {args[0]} %>% ungroup() %>% filter({arg1} {args[3]} {arg2})'

    def eval_summariseGrouped(self, node, args):
        return f'(SELECT *, {self.transform_summarise(args[1])} FROM {args[0]} GROUP BY {self.transform_col(args[2])})'
        # if "paste" in args[1]:
        #     args[1] = '{at} = paste({at}, collapse=:)'.format(at=args[1].split("|")[1])
        # args[1] = args[1].replace(':', '":"')
        #
        # _script = f'{name} <- {args[0]} %>% group_by({get_collist(args[2])}) %>% summarise({args[1]})'

    def eval_summarise(self, node, args):
        return ""
        # if "paste" in args[1]:
        #     args[1] = '{at} = paste({at}, collapse=":")'.format(at=args[1].split("|")[1])
        #
        # _script = f'{name} <- {args[0]} %>% summarise({args[1]})'

    def eval_inner_join(self, node, args):
        return f'{args[0]} NATURAL JOIN {args[1]}'

    def eval_inner_join3(self, node, args):
        return f'{args[0]} NATURAL JOIN {args[1]} NATURAL JOIN {args[2]}'

    def eval_inner_join4(self, node, args):
        return f'{args[0]} NATURAL JOIN {args[1]} NATURAL JOIN {args[2]} NATURAL JOIN {args[3]}'

    def eval_anti_join(self, node, args):
        return ""
        # _script = f'{name} <- anti_join(select({args[0]},{get_collist(args[2])}), select({args[1]}, {get_collist(args[2])}))'

    def eval_left_join(self, node, args):
        return f'{args[0]} LEFT JOIN {args[1]}'

    def eval_bind_rows(self, node, args):
        return f'{args[0]} UNION {args[1]}'

    def eval_intersect(self, node, args):
        return ""
        # _script = f'{name} <- intersect(select({args[0]},{get_collist(args[2])}), select({args[1]}, {get_collist(args[2])}))'

    def eval_unite(self, node, args):
        return ""
        # _script = f'{name} <- unite({args[0]}, {get_collist(args[1])}, which(colnames({args[0]})=="{get_collist(args[1])}"), {get_collist(args[2])}, which(colnames({args[0]})=="{get_collist(args[2])}"), sep=":")'

    def apply_row(self, val):
        return 0

    def apply_col(self, val):
        return 0

    def apply_name(self, val):
        return ""
