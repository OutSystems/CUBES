import argparse
import csv
import sqlite3
from collections import defaultdict
from difflib import ndiff
from functools import singledispatchmethod
from itertools import chain
from logging import getLogger
from os.path import isfile
from pathlib import Path

import sqlparse
import yaml
from colorama import Fore
from ordered_set import OrderedSet
from rpy2 import robjects
from sqlparse.sql import Statement, Token, Function, Identifier, Where, Comparison, IdentifierList, Parenthesis
from sqlparse.tokens import Operator, Number, Punctuation, Wildcard, String, Name

robjects.r('library(vctrs)')

vec_as_names = robjects.r('vec_as_names')

allowed_keywords_as_identifiers = ['events', 'length', 'month', 'location', 'position', 'uid', 'source', 'year', 'section', 'connection', 'type', 'roles', 'host', 'match', 'class', 'block', 'characteristics']


def askbool(message):
    while True:
        i = input(message + ' (y/n) ')
        if i.lower() == 'y':
            return True
        if i.lower() == 'n':
            return False


class EmptyOutputException(Exception):
    pass


class literal(str):
    pass


def literal_presenter(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')


class blocklist(list):
    pass


def blocklist_presenter(dumper, data):
    return dumper.represent_sequence(u'tag:yaml.org,2002:seq', data, flow_style=True)


yaml.add_representer(literal, literal_presenter)
yaml.add_representer(blocklist, blocklist_presenter)

parser = argparse.ArgumentParser(description='Util for parsing text2sql benchmarks.')
parser.add_argument('--force', action='store_true')

args, other_args = parser.parse_known_args()

logger = getLogger('spider-parser')

connections = {}
tables = {}
column_map = defaultdict(OrderedSet)
invalid_instance_sets = set()

counters = defaultdict(lambda: 1)
failed = 0


def color_diff(diff):
    for line in diff:
        if line.startswith('+'):
            yield Fore.GREEN + line + Fore.RESET
        elif line.startswith('-'):
            yield Fore.RED + line + Fore.RESET
        elif line.startswith('^'):
            yield Fore.BLUE + line + Fore.RESET
        else:
            yield line


class InstanceCollector:

    def __init__(self) -> None:
        self.functions = OrderedSet()
        self.identifiers = OrderedSet()
        self.columns = OrderedSet()
        self.constants = OrderedSet()
        self.filters = OrderedSet()
        self.in_limit = False
        self.limit = None
        self.limit_func = 'min'
        self.in_order = False
        self.order_col = OrderedSet()
        self.in_like = False

    @singledispatchmethod
    def visit(self, node):
        raise NotImplementedError(node, type(node))

    @visit.register
    def _(self, node: Statement):
        for elem in node.tokens:
            self.visit(elem)

    @visit.register
    def _(self, node: Token):
        if not node.is_keyword and not node.is_whitespace and not node.ttype in Punctuation and not node.ttype in Operator and not node.ttype in Wildcard and not node.ttype in Name.Builtin:
            if node.ttype in Number:
                if self.in_limit:
                    self.limit = node.value
                    self.in_limit = False
                    if int(self.limit) == 1:
                        self.functions.add(self.limit_func)
                        self.columns.update(self.order_col)
                else:
                    self.constants.add(node.value)
            elif node.ttype in String:
                self.constants.add(node.value[1:-1])
            else:
                raise NotImplementedError(node, type(node))
        elif node.is_keyword:
            if node.value.lower() == 'limit':
                self.in_limit = True
            elif node.value.lower() == 'count' or node.value.lower() == 'avg':
                self.functions.add(node.value.lower())
            elif node.value.lower() == 'desc':
                self.limit_func = 'max'
            elif node.value.lower() == 'asc':
                self.limit_func = 'min'
            elif node.value.lower() == 'order by':
                self.in_order = True
            elif node.value.lower() in allowed_keywords_as_identifiers:
                self.identifiers.add(node.value.lower())
        elif node.ttype in Name.Builtin:
            self.identifiers.add(node.value.lower())

    @visit.register
    def _(self, node: IdentifierList):
        for elem in node.tokens:
            self.visit(elem)

    @visit.register
    def _(self, node: Identifier):
        if not self.in_order:
            if len(node.tokens) == 1:
                self.identifiers.add(node.value.lower())
            elif len(node.tokens) == 3:
                self.identifiers.add(node.tokens[2].value.lower())
            elif len(node.tokens) == 5:
                self.identifiers.add(node.tokens[0].value.lower())
            else:
                raise NotImplementedError
        else:
            self.in_order = False
            if len(node.tokens) == 1:
                self.order_col.add(node.value.lower())
            elif len(node.tokens) == 3:
                arg = node.tokens[0]
                if len(arg.tokens) == 1:
                    self.order_col.add(arg.value.lower())
                elif len(arg.tokens) == 3:
                    self.order_col.add(arg.tokens[2].value.lower())
                elif len(arg.tokens) == 5:
                    self.order_col.add(arg.tokens[0].value.lower())
                else:
                    raise NotImplementedError
                self.visit(node.tokens[2])
            else:
                raise NotImplementedError

    @visit.register
    def _(self, node: Function):
        self.functions.add(node.tokens[0].value.lower())
        if len(node.tokens[1].tokens) == 3:
            self.visit_where(node.tokens[1].tokens[1])
        elif len(node.tokens[1].tokens) == 5:
            self.visit_where(node.tokens[1].tokens[3])
        else:
            raise NotImplementedError

    @visit.register
    def _(self, node: Where):
        for i, elem in enumerate(node.tokens):
            if elem.value.lower() == 'intersect':
                break
            self.visit_where(elem)
        if node.tokens[i+1:]:
            self.visit(sqlparse.parse(''.join(map(lambda x: x.value, node.tokens[i+1:])))[0])

    @visit.register
    def _(self, node: Parenthesis):
        if node.tokens[1].value.lower() == 'select':
            self.visit(sqlparse.parse(''.join(map(lambda x: x.value, node.tokens[1:-1])))[0])
            return
        for elem in node.tokens[1:-1]:
            self.visit_where(elem)

    @visit.register
    def _(self, node: Comparison):
        for elem in node.tokens:
            self.visit(elem)

    @singledispatchmethod
    def visit_where(self, node):
        raise NotImplementedError(node, type(node))

    @visit_where.register
    def _(self, node: Token):
        if not node.is_keyword and not node.is_whitespace and not node.ttype in Operator and not node.ttype in Punctuation and not node.ttype in Name.Builtin and not node.ttype in Wildcard:
            if node.ttype in Number:
                self.constants.add(node.value)
            elif node.ttype in String:
                const = node.value[1:-1]
                if self.in_like:
                    if const[0] == '%':
                        const = const[1:]
                    if const[-1] == '%':
                        const = const[:-1]
                    self.in_like = False
                self.constants.add(const)
            else:
                raise NotImplementedError(node, type(node))
        elif node.ttype in Operator:
            if node.value.lower() == 'like':
                self.filters.add('like')
                self.in_like = True
        elif node.ttype in Name.Builtin or node.value.lower() in allowed_keywords_as_identifiers:
            self.columns.add(node.value.lower())

    @visit_where.register
    def _(self, node: Parenthesis):
        for elem in node.tokens[1:-1]:
            self.visit(elem)

    @visit_where.register
    def _(self, node: Identifier):
        if len(node.tokens) == 1:
            self.columns.add(node.value.lower())
        elif len(node.tokens) == 3:
            self.columns.add(node.tokens[2].value.lower())
        else:
            raise NotImplementedError

    @visit_where.register
    def _(self, node: Comparison):
        for elem in node.tokens:
            self.visit_where(elem)

    @visit_where.register
    def _(self, node: Function):
        self.functions.add(node.tokens[0].value.lower())
        if len(node.tokens[1].tokens) == 3:
            self.visit_where(node.tokens[1].tokens[1])
        elif len(node.tokens[1].tokens) == 5:
            arg = node.tokens[1].tokens[3]
            if arg.ttype not in Wildcard:
                self.columns.add(arg.value.lower())
        else:
            raise NotImplementedError


with open('spider/train_gold.sql') as f:
    for line in OrderedSet(f):
        orig_query, instance_set = line.strip().split('\t')
        query = orig_query.replace('"', "'")

        folder_ = Path(f'tests-examples/spider/{instance_set}')
        folder_.mkdir(parents=True, exist_ok=True)
        Path(f'tests-examples/spider/{instance_set}/tables').mkdir(exist_ok=True)

        if instance_set not in connections:
            connections[instance_set] = sqlite3.connect(f'spider/database/{instance_set}/{instance_set}.sqlite')

            c = connections[instance_set].cursor()
            c.execute("SELECT name FROM sqlite_master WHERE type='table';")
            tables[instance_set] = list(map(lambda x: x.lower(), chain.from_iterable(c.fetchall())))
            print(f'Found new instance set {instance_set} with tables {tables[instance_set]}')
            conn = connections[instance_set]
            c = conn.cursor()
            try:
                for table in tables[instance_set]:
                    columns = OrderedSet()
                    with open(f'tests-examples/spider/{instance_set}/tables/{table}.csv', 'w', newline='') as out_f:
                        writer = csv.writer(out_f)
                        c.execute(f"SELECT * FROM {table};")
                        writer.writerow([desc[0].lower() for desc in c.description])
                        columns.update([desc[0].lower() for desc in c.description])
                        row = c.fetchone()
                        if row is None:
                            raise EmptyOutputException(f'{instance_set}/{table}')
                        while row:
                            writer.writerow(row)
                            row = c.fetchone()
                    column_map[instance_set].update(columns)
            except EmptyOutputException as e:
                print(f"\tTable {e.args[0]} is empty!! Skipping instance set.")
                invalid_instance_sets.add(instance_set)
            finally:
                c.close()

        if instance_set in invalid_instance_sets:
            continue

        conn = connections[instance_set]
        columns = column_map[instance_set]

        print(f'Executing query {counters[instance_set]} of {instance_set} (total {sum(counters.values())})')
        cur = conn.cursor()
        try:
            with open(f'tests-examples/spider/{instance_set}/tables/{str(counters[instance_set]).rjust(4, "0")}.csv', 'w',
                      newline='') as out_f:
                writer = csv.writer(out_f)
                cur.execute(query)
                cols = [desc[0].lower() for desc in cur.description]
                writer.writerow(list(vec_as_names(robjects.StrVector(cols), repair='unique')))
                row = cur.fetchone()
                if row is None:
                    raise EmptyOutputException
                while row:
                    writer.writerow(row)
                    row = cur.fetchone()

            stmt = sqlparse.parse(query)[0]

            collector = InstanceCollector()
            collector.visit(stmt)

            instance = {
                'inputs': [f'tests-examples/spider/{instance_set}/tables/{table}.csv' for table in tables[instance_set] if
                           table in collector.identifiers],
                'output': f'tests-examples/spider/{instance_set}/tables/{str(counters[instance_set]).rjust(4, "0")}.csv',
                }

            if collector.constants:
                instance['constants'] = list(collector.constants)
            if collector.functions:
                instance['functions'] = list(collector.functions - {'distinct'})
            if collector.columns:
                instance['columns'] = list(collector.columns)
            if collector.filters:
                instance['filters'] = list(collector.filters)

            instance['comment'] = literal(sqlparse.format(orig_query, reindent=True, keyword_case='upper'))

            output = yaml.dump(instance, default_flow_style=False, sort_keys=False)

            file_path = f'tests-examples/spider/{instance_set}/{str(counters[instance_set]).rjust(4, "0")}.yaml'
            if isfile(file_path):
                with open(file_path) as f:
                    current_content = f.read()
                    if output == current_content:
                        continue
                print(''.join(color_diff(ndiff(current_content.splitlines(keepends=True), output.splitlines(keepends=True)))))
                if args.force or askbool(
                        f'Instance {instance_set}/{str(counters[instance_set]).rjust(4, "0")} has changed. Do you wish to replace it?'):
                    with open(file_path, 'w') as f:
                        f.write(output)
            else:
                with open(file_path, 'x') as f:
                    f.write(output)

        except EmptyOutputException:
            print(f'Empty output while executing query {counters[instance_set]} of {instance_set} (total {sum(counters.values())})')
        except Exception as e:
            print(f'{Fore.RED}Error while executing query {counters[instance_set]} of {instance_set} (total {sum(counters.values())})')
            print(f"\t{query}")
            print(f'\t{e}{Fore.RESET}')
            failed += 1
        finally:
            cur.close()
            counters[instance_set] += 1

print(f'Failed to produce {failed} instances!')
print(f'Invalid instance sets: {invalid_instance_sets}')