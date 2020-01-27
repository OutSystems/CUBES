import logging

from rpy2 import robjects

from tyrell.logger import get_logger

from squares.DSL import find_consts
from squares.util import next_counter, get_permutations

logger = get_logger('tyrell')


def exec_and_return(r_script):
    robjects.r(r_script)
    return r_script


def parse_problem(filename):
    f = open(filename)
    inputs = f.readline()[:-1].split(":")[1].replace(" ", "").split(",")
    output = f.readline()[:-1].split(":")[1].replace(" ", "")
    consts = list(filter(lambda s: s != '', map(lambda s: s[1:-2], f.readline()[:-1].split(":", 1)[1].split(","))))
    aggrs = list(filter(lambda s: s != '', map(lambda s: s[1:-2], f.readline()[:-1].split(":", 1)[1].split(","))))
    attrs = list(filter(lambda s: s != '', map(lambda s: s[1:-2], f.readline()[:-1].split(":", 1)[1].split(","))))
    bools = list(filter(lambda s: s != '', map(lambda s: s[1:-2], f.readline()[:-1].split(":", 1)[1].split(","))))
    loc = int(f.readline()[:-1].replace(" ", "").split(":")[1])

    return Problem(inputs, output, consts, aggrs, attrs, bools, loc)


class Problem:

    def __init__(self, inputs, output, consts, aggrs, attrs, bools, loc):
        self.inputs = inputs
        self.output = output
        self.consts = consts
        self.aggrs = aggrs
        self.attrs = attrs
        self.has_int_consts = find_consts(consts)
        self.bools = bools
        self.loc = loc

        self.tables = []

        self._tables = {}
        self.columns = set()

        for input in inputs:
            id = next_counter()
            self.tables.append(f'input{id}')
            self._tables[self.tables[-1]] = id

            with open(input) as f:
                self.columns = self.columns.union(set(f.readline()[:-1].split(",")))

        self._tables['expected_output'] = next_counter()

        self.generate_r_init()
        self.generate_dsl()

        if logger.isEnabledFor(logging.DEBUG):
            with open('dsl.log', 'w') as f:
                f.write(self.dsl)

    def generate_r_init(self):
        self.r_init = 'con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")\n'

        for table, file in zip(self.tables, self.inputs):
            self.r_init += exec_and_return(f'{table} <- read.table("{file}", sep=",", header=T)\n')
            self.r_init += f'{table} <- copy_to(con, {table})\n'

        self.r_init += exec_and_return(f'expected_output <- read.table("{self.output}", sep =",", header=T)\n')

    def generate_dsl(self):
        filtersOne = 'func filter: Table r -> Table a, FilterCondition f {\n' \
                     '\trow(r) <= row(a);\n' \
                     '\tcol(r) == col(a);\n' \
                     '}\n'
        filters = filtersOne
        filterAndOr = 'func filters: Table r -> Table a, FilterCondition f, FilterCondition g, Op o {\n' \
                      '\trow(r) <= row(a);\n' \
                      '\tcol(r) == col(a);\n' \
                      '}\n'
        filterPredicateOne = "predicate is_not_parent(inner_join3, filter, 100);\n" \
                             "predicate is_not_parent(inner_join4, filter, 100);\n" \
                             "predicate is_not_parent(filter, filter, 100);\n" \
                             "predicate distinct_inputs(filter);\n"
        filterPredicate = filterPredicateOne
        filterPredicateTwo = "predicate distinct_filters(filters, 1, 2);\n" \
                             "predicate is_not_parent(filters, filters, 100);\n" \
                             "predicate is_not_parent(inner_join, filters, 100);\n" \
                             "predicate is_not_parent(inner_join3, filters, 100);\n" \
                             "predicate is_not_parent(inner_join4, filters, 100);\n" \
                             "predicate distinct_inputs(filters);\n"
        summarise = "func summariseGrouped: Table r -> Table a, SummariseCondition s, Cols b {\n" \
                    "\trow(r) <= row(a);\n" \
                    "\tcol(r) <= 3;\n" \
                    "}\n" \
                    "predicate is_not_parent(inner_join4, summariseGrouped, 100);\n" \
                    "predicate is_not_parent(summariseGrouped, summariseGrouped, 100);\n"

        operators = ""
        concat = ""

        if self.consts:
            if len(self.consts) > 1:
                filters = filterAndOr
                filterPredicate = filterPredicateTwo
                operators = 'enum Op{\n "|", "&"\n}\n'
        else:
            filterPredicate, filters, consts = "", "", ""

        if self.aggrs:
            for a in self.aggrs:
                if a == 'concat':
                    self.aggrs.remove(a)
                    concat = 'func unite: Table r -> Table a, Col c, Col d {\n' \
                             '\trow(r) <= row(a);\n' \
                             '\tcol(r) < col(a);\n' \
                             '}\n'
            if len(self.aggrs) == 1 and "like" in self.aggrs:
                summarise = ""
        else:
            summarise = ""

        if 'max(n)' in self.aggrs:
            self.consts.append('max(n)')
            self.aggrs.remove('max(n)')

        if self.attrs:
            self.attrs = (self.attrs + ["n"]) if "n" in self.aggrs and self.has_int_consts else self.attrs
        elif 'n' in self.aggrs:
            self.attrs.append('n')

        filterConditions, summariseConditions, necessary_conditions, happens_before = self.find_conditions()

        if filters == "" and filterConditions != []:
            filters = filtersOne
            filterPredicate = "predicate is_not_parent(filter, filter, 100);\n"

        if len(necessary_conditions) > 1:
            filters = filtersOne + filterAndOr
            filterPredicate = 'predicate distinct_filters(filters, 1, 2);\n' \
                              'predicate is_not_parent(filters, filter, 100);\n' \
                              'predicate is_not_parent(filter, filters, 100);\n' \
                              'predicate is_not_parent(filter, filter, 100);\n' \
                              'predicate is_not_parent(filters, filters, 100);\n'
            operators = 'enum Op{\n "|", "&"\n}\n'

        necessary_conditions = self.find_necessary_conditions(necessary_conditions)
        necessary_conditions += self.happens_before(happens_before)

        with open(self.output) as f:
            output_attrs = f.readline()[:-1]

        cols = str(get_permutations(str(self.columns)[1:-1].replace("'", "").replace(" ", "").split(","), 2))[
               1:-1].replace("'", "\"")
        oneColumn = str(get_permutations(str(self.columns)[1:-1].replace("'", "").replace(" ", "").split(","), 1))[
                    1:-1].replace("'", "\"")

        fil_conditions = "enum FilterCondition{\n" + str(filterConditions)[1:-1].replace("'",
                                                                                         "\"") + "\n}\n" if filterConditions != [] else ""
        sum_conditions = "enum SummariseCondition{\n" + str(summariseConditions)[1:-1].replace("'",
                                                                                               "\"") + "\n}\n" if summariseConditions != [] else ""

        file_path = 'example/squares.tyrell'

        with open(file_path) as f:
            spec_str = f.read()

        self.dsl = spec_str.format(cols=cols, Tables=str("Table, " * len(self.inputs))[:-2], summarise=summarise,
                                   filters=filters,
                                   filterPred=filterPredicate, FilterConditions=fil_conditions,
                                   SummariseConditions=sum_conditions, Op=operators,
                                   necessaryConditions=necessary_conditions,
                                   SelectCols=str("\"" + output_attrs + "\""), col=oneColumn,
                                   concat=concat)

    def find_filter_conditions(self, str_const, int_const, str_attr, int_attr, new_int_attr,
                               necessary_conditions,
                               summarise_conditions):
        conditions = []
        int_ops = ["==", ">", "<", ">=", "<="]
        str_ops = ["==", "!="]
        happens_before = []

        for sc in str_const + int_const:
            necessary_conditions.append([])
            for sa in str_attr:
                att = False
                for i in self.inputs:
                    if att:
                        break
                    with open(i) as f:
                        columns = f.readline()[:-1].split(",")
                        if sa in columns:
                            ind = columns.index(sa)
                            for l in f:
                                if l[:-1].split(",")[ind] == sc:
                                    att = True
                                    break
                        else:
                            continue
                if 'like' in self.aggrs:
                    conditions.append(f'str_detect({sa}|{sc})')
                    necessary_conditions[-1].append(conditions[-1])

                if not att:
                    continue
                for so in str_ops:
                    conditions.append(f'{sa} {so} {sc}')
                    necessary_conditions[-1].append(conditions[-1])

        for ic in int_const:
            necessary_conditions.append([])
            for ia in int_attr + new_int_attr:
                if ic == ia:
                    continue
                for io in int_ops:
                    conditions.append(f'{ia} {io} {ic}')
                    necessary_conditions[-1].append(conditions[-1])
                    if ia == "n":
                        happens_before.append((conditions[-1], 'n = n()'))

        for ic in new_int_attr:
            for ia in int_attr + new_int_attr:
                if ic == ia:
                    continue
                for io in int_ops:
                    conditions.append('{ia} {io} {ic}'.format(ia=ia, io=io, ic=ic))
                    for sc in summarise_conditions:
                        if ic in sc:
                            happens_before.append((conditions[-1], sc))

        necessary_conditions = list(filter(lambda a: a != [], necessary_conditions))
        # if "max" in aggrs and "n" in aggrs or "max(n)" in aggrs:
        if 'max(n)' in self.aggrs:
            conditions.append('n == max(n)')
            happens_before.append((conditions[-1], 'n = n()'))
            necessary_conditions.append([conditions[-1]])

        # print("filter conditions "+str(conditions))
        return conditions, necessary_conditions, happens_before

    def find_summarise_conditions(self, int_attr, str_attr, necessary_conditions):
        conditions = []
        new_int_attr = []
        for a in self.aggrs:
            if a == "like":
                continue
            necessary_conditions.append([])
            if "n" == a:
                conditions.append('{a} = {a}()'.format(a=a))
                necessary_conditions[-1].append(conditions[-1])
                continue
            if 'concat' in a:
                for at in int_attr + str_attr:
                    conditions.append('paste|{at}'.format(at=at))
                    necessary_conditions[-1].append(conditions[-1])
                continue
            if "max(n)" == a:
                continue
            for ia in int_attr:
                conditions.append('{a}{ia} = {a}({ia})'.format(ia=ia, a=a))
                necessary_conditions[-1].append(conditions[-1])
                new_int_attr.append('{a}{ia}'.format(ia=ia, a=a))
        return list(filter(lambda a: a != [], necessary_conditions)), new_int_attr, conditions

    def find_conditions(self):
        necessary_conditions = []
        str_const, int_const = self.divide_int_str_constants()
        str_attr, int_attr = self.divide_int_str_attributes()

        necessary_conditions, new_int_attr, sum_cond = self.find_summarise_conditions(int_attr, str_attr,
                                                                                      necessary_conditions)

        filt_cond, necessary_conditions, happens_before = self.find_filter_conditions(str_const, int_const, str_attr,
                                                                                      int_attr,
                                                                                      new_int_attr,
                                                                                      necessary_conditions, sum_cond)
        attributes = int_attr + new_int_attr
        return filt_cond, sum_cond, necessary_conditions, happens_before

    @staticmethod
    def find_necessary_conditions(conds):
        predicates = ""
        for c in conds:
            if c == []:
                break
            predicate = "\npredicate constant_occurs(\""
            for i in c:
                predicate += i + ","
            predicates += predicate[:-1] + "\");"
        return predicates

    def divide_int_str_constants(self):
        str_const, int_const = [], []
        for c in self.consts:
            try:
                if c == '0' or int(c):
                    int_const.append(c)
            except:
                str_const.append(c)
        return str_const, int_const

    def divide_int_str_attributes(self):
        str_attr, int_attr = [], []
        for a in self.attrs:
            if a == "n":
                if a not in int_attr:
                    int_attr.append(a)
            for i in self.inputs:
                with open(i) as f:
                    columns = f.readline()[:-1].split(",")
                    if a in columns:
                        ind = columns.index(a)
                        try:
                            if f.readline()[:-1].split(",")[ind] == '0' or int(f.readline()[:-1].split(",")[ind]):
                                if a not in int_attr:
                                    int_attr.append(a)
                        except:
                            if a not in str_attr:
                                str_attr.append(a)
        return str_attr, int_attr

    @staticmethod
    def happens_before(conds):
        predicates = ""
        for c in conds:
            if c == ():
                break
            predicates += 'predicate happens_before("' + c[0] + '","' + c[1] + '");\n'
        return predicates
