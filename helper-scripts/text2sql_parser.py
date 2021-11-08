import argparse
import csv
import os
import re
from logging import getLogger
from pathlib import Path

import mysql.connector
import sqlparse

import yaml
from ordered_set import OrderedSet


class literal(str):
    pass


def literal_presenter(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')


class blocklist(list): pass


def blocklist_presenter(dumper, data):
    return dumper.represent_sequence(u'tag:yaml.org,2002:seq', data, flow_style=True)


yaml.add_representer(literal, literal_presenter)
yaml.add_representer(blocklist, blocklist_presenter)

parser = argparse.ArgumentParser(description='Util for parsing text2sql benchmarks.')
parser.add_argument('input', metavar='INPUT', help="name of the input file")
parser.add_argument('output', metavar='OUTPUT', help="name of the output folder")

args, other_args = parser.parse_known_args()

logger = getLogger('text2sql-parser')

password = input('Database password for user squares_u: ')
conn = mysql.connector.connect(user='squares_u', host='localhost', database='squares', password=password)

COUNTER = 1

folder = Path(f'tests-examples/text2sql/{args.output}')
folder.mkdir(parents=True, exist_ok=True)
Path(f'tests-examples/text2sql/{args.output}/tables').mkdir(exist_ok=True)

tables = ['state',
          'border_info',
          'city',
          'highlow',
          'river',
          'mountain',
          'road',
          'lake'
          ]

columns = []

with open(args.input) as f:
    data = yaml.safe_load(f)

    cur = conn.cursor()
    try:
        cur.execute('SELECT COLUMN_NAME, REFERENCED_COLUMN_NAME\n'
                    'FROM information_schema.KEY_COLUMN_USAGE\n'
                    'WHERE COLUMN_NAME != REFERENCED_COLUMN_NAME\n'
                    f'''  AND lower(TABLE_NAME) IN ({", ".join(map(lambda x: "'" + x + "'", tables))})\n'''
                    f'''  AND lower(REFERENCED_TABLE_NAME) IN ({", ".join(map(lambda x: "'" + x + "'", tables))});''')
        foreign_keys = list(map(blocklist, cur.fetchall()))
    finally:
        cur.close()

    for table in tables:
        cur = conn.cursor()
        try:
            cur.execute('SELECT column_name\n'
                        'FROM INFORMATION_SCHEMA.COLUMNS\n'
                        f'''WHERE TABLE_NAME = N'{table}';''')
            columns += list(map(lambda x: x[0], cur.fetchall()))
        finally:
            cur.close()

    for obj in data:

        if 'sql' not in obj:
            print('Found object without SQL query!')

        for query in obj['sql']:
            for variable in obj['variables']:
                query = query.replace(variable['name'], variable['example'])

            print(f'Executing query {COUNTER}')

            cur = conn.cursor()
            try:
                with open(f'tests-examples/text2sql/{args.output}/tables/{str(COUNTER).rjust(4, "0")}.csv', 'w', newline='') as out_f:
                    writer = csv.writer(out_f)
                    cur.execute(query)
                    writer.writerow([desc[0] for desc in cur.description])
                    row = cur.fetchone()
                    if row is None:
                        raise Exception('Empty output!')
                    while row:
                        writer.writerow(row)
                        row = cur.fetchone()

                with open(f'tests-examples/text2sql/{args.output}/{str(COUNTER).rjust(4, "0")}.yaml', 'w') as out_f:
                    yaml.dump({
                        'inputs': [f'tests-examples/text2sql/{args.output}/tables/{table}.csv' for table in tables if
                                   re.search(fr'\b{table}\b', query.lower())],
                        'output': f'tests-examples/text2sql/{args.output}/tables/{str(COUNTER).rjust(4, "0")}.csv',
                        'constants': [variable['example'] for variable in obj['variables']],
                        'functions': [m[1] for m in re.finditer(fr'(\w+)\s*\(\s*\w+\.\w+\s*\)', query.lower()) if m],
                        'columns': list(OrderedSet([column for column in columns if re.search(fr'\w+\s*\(\s*\w+\.{column.lower()}\s*\)', query.lower()) is not None or re.search(fr'\w+\.{column.lower()}\s*[=<>]|<=|>=|<>|!=|like\s*.*', query.lower()) is not None])),
                        'foreign-keys': foreign_keys,
                        'comment': literal(sqlparse.format(query, reindent=True, keyword_case='upper'))
                        }, out_f, default_flow_style=False, sort_keys=False)

            except Exception as e:
                print(f'Error while executing query {COUNTER}')
                print(e)
                if os.path.exists(f'tests-examples/text2sql/{args.output}/tables/{str(COUNTER).rjust(4, "0")}.csv'):
                    os.remove(f'tests-examples/text2sql/{args.output}/tables/{str(COUNTER).rjust(4, "0")}.csv')
                if os.path.exists(f'tests-examples/text2sql/{args.output}/{str(COUNTER).rjust(4, "0")}.yaml'):
                    os.remove(f'tests-examples/text2sql/{args.output}/{str(COUNTER).rjust(4, "0")}.yaml')
            finally:
                conn.rollback()
                cur.close()
                COUNTER += 1
