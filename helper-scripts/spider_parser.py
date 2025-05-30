import argparse
import csv
import logging
import os.path
import re
import shutil
import sqlite3
import traceback
from collections import defaultdict
from difflib import ndiff
from functools import singledispatchmethod
from itertools import chain
from os.path import isfile
from pathlib import Path
from typing import List, Tuple

import sqlparse
import yaml
from colorama import Fore
from ordered_set import OrderedSet
from rpy2 import robjects
from sqlparse.sql import Statement, Token, Function, Identifier, Where, Comparison, IdentifierList, Parenthesis
from sqlparse.tokens import Operator, Number, Punctuation, Wildcard, String, Name

import squares.util
from squares import types
import squares.tyrell.logger

robjects.r('library(vctrs)')

vec_as_names = robjects.r('vec_as_names')

allowed_keywords_as_identifiers = ['events', 'length', 'month', 'location', 'position', 'uid', 'source', 'year',
                                   'section', 'connection',
                                   'type', 'roles', 'host', 'match', 'class', 'block', 'characteristics', 'result', 'title', 'user']


def removesuffix(self: str, suffix: str, /) -> str:
    # suffix='' should not call self[:-0].
    if suffix and self.endswith(suffix):
        return self[:-len(suffix)]
    else:
        return self[:]


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

squares.tyrell.logger.setup_logger('spider-parser')
logger = logging.getLogger('spider-parser')
logger.setLevel(logging.INFO)

connections = {}
tables = {}
empty_tables = defaultdict(set)
column_map = defaultdict(OrderedSet)
column_types = defaultdict(dict)

counters = defaultdict(lambda: 1)
failed = 0
skipped = 0


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
                # if self.in_limit:
                #     self.limit = node.value
                #     self.in_limit = False
                #     if int(self.limit) == 1:
                #         self.functions.add(self.limit_func)
                #         self.columns.update(self.order_col)
                # else:
                self.constants.add(node.value)
            elif node.ttype in String:
                self.constants.add(node.value[1:-1])
            else:
                logger.error('Unknown token found %s (of type %s)', node, type(node))
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
                if not self.in_order:
                    self.identifiers.add(node.value.lower())
                else:
                    self.order_col.add(node.value.lower())
                    self.in_order = False
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
                if not arg.is_group:
                    pass
                elif len(arg.tokens) == 1:
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
        if node.tokens[i + 1:]:
            self.visit(sqlparse.parse(''.join(map(lambda x: x.value, node.tokens[i + 1:])))[0])

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
                logger.error('Unknown token found %s (of type %s)', node, type(node))
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


def map_type(t: str) -> str:
    t = t.lower()
    if t == 'integer' or t == 'int' or t == 'bigint' or t == 'smallint' or t == 'smallint unsigned' or t == 'tinyint unsigned' or t == 'mediumint unsigned' or re.match(
            r'(big)?int\(\d+\)', t) or t == 'year':
        return 'int'
    elif t == 'text' or re.match(r'(character )?(var)?char(2)?\(\d+\)', t) or t == 'blob':
        return 'str'
    elif t == 'real' or t == 'numeric' or t == 'decimal' or t == 'double' or t == 'float' or re.match(
            r'(number)|(numeric)|(decimal)\(\d+,\d+\)', t) or re.match(r'float\(\d+\)', t):
        return 'real'
    elif t == 'datetime' or t == 'timestamp':
        return 'datetime'
    elif t == 'date':
        return 'datetime'
    elif t == 'bit' or t == 'bool' or t == 'boolean':
        return 'bool'
    elif t == '' or t == 'null':
        return 'guess'
    else:
        raise NotImplementedError(f'Unknown type {t}')


def coalesce_type(current, new):
    logger.warning('Column with mixed types found: %s and %s', current, new)
    if {current, new} == {'int', 'real'}:
        return 'real'
    if {current, new} == {'str', 'int'}:
        return 'str'
    if {current, new} == {'str', 'real'}:
        return 'str'
    if {'guess'}.issubset({current, new}):
        return 'guess'
    else:
        raise NotImplementedError(f'Unknown type mixing: {current} with {new}')


def coalesce_types(cell_types: List[Tuple[str]]) -> List[str]:
    result = None
    for row in cell_types:
        if result is None:
            result = list(map(map_type, row))
            continue

        for i, (current, new) in enumerate(zip(result, map(map_type, row))):
            if current != new:
                result[i] = coalesce_type(current, new)

    return result


with open('spider/train_gold.sql') as f:
    for line in OrderedSet(f):
        orig_query, instance_set = line.strip().split('\t')
        query = orig_query.replace('"', "'")

        folder_ = Path(f'tests-examples/spider/{instance_set}')
        folder_.mkdir(parents=True, exist_ok=True)
        Path(f'tests-examples/spider/{instance_set}/tables').mkdir(exist_ok=True)

        if instance_set not in connections:
            shutil.copyfile(f'spider/database/{instance_set}/{instance_set}.sqlite', f'tests-examples/spider/{instance_set}/tables/db.sqlite')
            connections[instance_set] = sqlite3.connect(f'spider/database/{instance_set}/{instance_set}.sqlite')

            c = connections[instance_set].cursor()
            c.execute("SELECT name FROM sqlite_master WHERE type='table';")
            tables[instance_set] = [c.lower() for c in chain.from_iterable(c.fetchall()) if c != 'sqlite_sequence']
            print(f'Found new instance set {instance_set} with tables {tables[instance_set]}')
            conn = connections[instance_set]
            c = conn.cursor()

            for table in tables[instance_set]:
                try:
                    columns = OrderedSet()
                    with open(f'tests-examples/spider/{instance_set}/tables/{table}.csv', 'w', newline='') as out_f:
                        writer = csv.writer(out_f)

                        c.execute(f"PRAGMA table_info({table});")
                        csv_cols = []
                        cols = []
                        row = c.fetchone()
                        while row:
                            cols.append(row[1].lower())
                            csv_cols.append(f'{cols[-1]}:{map_type(row[2])}')
                            if cols[-1] not in column_types[instance_set]:
                                column_types[instance_set][cols[-1]] = map_type(row[2])
                                logger.debug('Mapped column %s to type %s', cols[-1], column_types[instance_set][cols[-1]])
                            elif column_types[instance_set][cols[-1]] == map_type(row[2]):
                                pass
                            else:
                                column_types[instance_set][cols[-1]] = types.UNKNOWN
                            row = c.fetchone()
                        writer.writerow(csv_cols)
                        columns.update(cols)

                        c.execute(f"SELECT * FROM {table};")
                        row = c.fetchone()
                        if row is None:
                            raise EmptyOutputException(table)
                        while row:
                            writer.writerow(row)
                            row = c.fetchone()
                    column_map[instance_set].update(columns)
                except EmptyOutputException as e:
                    empty_tables[instance_set].add(e.args[0])
                    logger.error('Table %s/%s is empty...', instance_set, e.args[0])
            c.close()

        conn = connections[instance_set]
        columns = column_map[instance_set]

        logger.info('Executing query %s of %s (total %d)', counters[instance_set], instance_set, sum(counters.values()))
        cur = conn.cursor()
        cur2 = conn.cursor()
        output_filename = f'tests-examples/spider/{instance_set}/tables/{str(counters[instance_set]).rjust(4, "0")}.csv'
        try:
            with open(output_filename, 'w', newline='') as out_f:
                writer = csv.writer(out_f)
                cur.execute(query)
                cols = [desc[0].lower() for desc in cur.description]
                row = cur.fetchone()
                if row is None:
                    raise EmptyOutputException
                tmp = ', '.join(map(lambda x: f'typeof("{x}")', cols))
                cur2.execute(f'SELECT {tmp} FROM ({removesuffix(query, ";")})')
                cols_types = coalesce_types(cur2.fetchall())
                cols_types2 = list()
                for col, col_type in zip(cols, cols_types):
                    if col == 'count(*)':
                        cols_types2.append('int')
                        continue
                    tmp = re.match(rf'(\w+)\s*\((?:distinct\s+)?(.+)\)', col)
                    if tmp is not None:
                        col = tmp[2]
                        if tmp[1] == 'avg' or tmp[1] == 'mean':
                            cols_types2.append('real')
                            continue
                        elif tmp[1] == 'count':
                            cols_types2.append('int')
                            continue

                    tmp = re.match(rf'.+\.(.+)', col)
                    if tmp is not None:
                        col = tmp[1]

                    if col in column_types[instance_set]:
                        if column_types[instance_set][col] != types.UNKNOWN:
                            cols_types2.append(column_types[instance_set][col])
                            if column_types[instance_set][col] != col_type:
                                logger.warning('Column %s has type %s in inputs for data is stored as %s', col,
                                               column_types[instance_set][col], col_type)
                            continue
                    logger.warning('No column named %s in inputs, using data store type %s', col, col_type)
                    cols_types2.append(col_type)
                writer.writerow(map(lambda x: f'{x[0]}:{x[1]}',
                                    zip(list(vec_as_names(robjects.StrVector(cols), repair='unique')), cols_types2)))
                while row:
                    writer.writerow(row)
                    row = cur.fetchone()

            stmt = sqlparse.parse(query)[0]

            collector = InstanceCollector()
            collector.visit(stmt)

            has_empty_input = False
            for table in tables[instance_set]:
                if table in collector.identifiers:
                    if table in empty_tables[instance_set]:
                        logger.error('Skipping instance %d of %s because table %s is empty', counters[instance_set], instance_set, table)
                        has_empty_input = True
                        break
            if has_empty_input:
                if os.path.isfile(output_filename):
                    os.remove(output_filename)
                skipped += 1
                continue

            instance = {
                'db': f'tests-examples/spider/{instance_set}/tables/db.sqlite',
                'inputs': [f'tests-examples/spider/{instance_set}/tables/{table}.csv' for table in tables[instance_set]
                           if
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

            instance['sql'] = literal(sqlparse.format(orig_query, reindent=True, keyword_case='upper'))

            output = yaml.dump(instance, default_flow_style=False, sort_keys=False)

            file_path = f'tests-examples/spider/{instance_set}/{str(counters[instance_set]).rjust(4, "0")}.yaml'
            if isfile(file_path):
                with open(file_path) as f:
                    current_content = f.read()
                    if output == current_content:
                        continue
                print(''.join(
                    color_diff(ndiff(current_content.splitlines(keepends=True), output.splitlines(keepends=True)))))
                if args.force or askbool(
                        f'Instance {instance_set}/{str(counters[instance_set]).rjust(4, "0")} has changed. Do you wish to replace it?'):
                    with open(file_path, 'w') as f:
                        f.write(output)
            else:
                with open(file_path, 'x') as f:
                    f.write(output)

        except EmptyOutputException:
            print(f'Empty output while executing query {counters[instance_set]} of {instance_set} (total {sum(counters.values())})')
            skipped += 1
            if os.path.isfile(output_filename):
                os.remove(output_filename)
        except Exception as e:
            logger.exception('Error while executing query %s of %s (total %d)\n%s', counters[instance_set], instance_set, sum(counters.values()), query)
            failed += 1
        finally:
            cur.close()
            counters[instance_set] += 1

print(f'Failed to produce {failed} instances!')
print(f'Skipped {skipped} instances due to empty inputs/outputs!')

