from ordered_set import OrderedSet


class SQL2R:

    def __init__(self):
        self.functions = OrderedSet()
        self.tables = OrderedSet()
        self.groupby_cols = []
        self.function_usages = 0
        self.filter_usages = 0
        self.join_usages = 0
        self.union_usages = 0
        self.join_conditions = []
        self.filter_ops = OrderedSet()

    def visit(self, it):
        for key, val in it.items():
            method = getattr(self, f'visit_{key}')
            method(val)

    def visit_select(self, it):
        if isinstance(it, str):
            pass
        elif isinstance(it, list):
            for i in it:
                self.visit_select(i)
        elif isinstance(it, dict) and set(it.keys()).issubset({'value', 'name'}):
            if isinstance(it['value'], dict):
                self.functions.append(next(iter(it['value'])))
                self.function_usages += 1
        else:
            raise NotImplementedError

    def visit_from(self, it):
        if isinstance(it, str):
            self.tables.append(it)
        elif isinstance(it, dict) and set(it.keys()).issubset({'value', 'name'}):
            self.visit_from(it['value'])
        elif isinstance(it, dict) and {'select', 'from'}.issubset(set(it.keys())):
            self.visit(it)
        elif isinstance(it, dict) and set(it.keys()).issubset({'inner join', 'on'}):
            self.join_usages += 1
            self.join_conditions.append(it['on'])
            self.visit_from(it['inner join'])
        elif isinstance(it, dict) and set(it.keys()).issubset({'join', 'on'}):
            self.join_usages += 1
            self.join_conditions.append(it['on'])
            self.visit_from(it['join'])
        elif isinstance(it, dict) and set(it.keys()).issubset({'left join', 'on'}):
            self.join_usages += 1
            self.join_conditions.append(it['on'])
            self.visit_from(it['left join'])
        elif isinstance(it, list):
            for from_elem in it:
                self.visit_from(from_elem)
        else:
            raise NotImplementedError

    def visit_groupby(self, it):
        if isinstance(it, dict):
            for col in it.values():
                self.groupby_cols.append(col)
        else:
            raise NotImplementedError

    def visit_having(self, it):
        for op in ['like', 'gt', 'ge', 'eq', 'ne', 'le', 'lt']:
            if set(it.keys()) == {op}:
                self.filter_usages += 1
                self.filter_ops.append(op)
                if isinstance(it[op][0], dict):
                    self.functions.append(next(iter(it[op][0])))
                    self.function_usages += 1
                return
        if set(it.keys()) == {'or'}:
            assert len(it['or']) == 2
            self.visit_having(it['or'][0])
            self.visit_having(it['or'][1])
        elif set(it.keys()) == {'and'}:
            assert len(it['and']) == 2
            self.visit_having(it['and'][0])
            self.visit_having(it['and'][1])
        else:
            raise NotImplementedError

    def visit_where(self, it):
        for op in ['like', 'gt', 'gte', 'eq', 'neq', 'lt', 'lte']:
            if set(it.keys()) == {op}:
                self.filter_usages += 1
                self.filter_ops.append(op)
                return
        if set(it.keys()) == {'or'}:
            assert len(it['or']) == 2
            self.visit_where(it['or'][0])
            self.visit_where(it['or'][1])
        elif set(it.keys()) == {'and'}:
            assert len(it['and']) == 2
            self.visit_where(it['and'][0])
            self.visit_where(it['and'][1])
        else:
            raise NotImplementedError

    def visit_union(self, it):
        self.union_usages += len(it) - 1
        for elem in it:
            self.visit(elem)

    def visit_orderby(self, it):
        pass

    def __repr__(self) -> str:
        return f'joins: {self.join_usages}, filters: {self.filter_usages}, functions: {self.function_usages}'

