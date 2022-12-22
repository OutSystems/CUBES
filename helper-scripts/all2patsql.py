#!/usr/bin/python3
import argparse
import csv
import glob
import os

import pandas
import yaml

from squares import types
from squares.dsl.table import Table


def map_type(t) -> str:
    if t == types.INT:
        return 'Int'

    elif t == types.FLOAT:
        return 'Dbl'

    elif t == types.STRING:
        return 'Str'

    elif types == types.DATETIME:
        return 'Date'

    elif types == types.TIME:
        return 'Date'

    else:
        return 'Str'


if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert all yaml spec file to scythe format.')
    parser.add_argument('output', metavar='OUTPUT')
    args = parser.parse_args()

    for file in glob.glob('tests/db2csv/**/*.yaml', recursive=True):
        if 'schema.yaml' in file:
            continue

        print(file)

        try:
            with open(file) as f:
                spec = yaml.safe_load(f)

            result = ''

            for input_name in spec['inputs']:
                result += '#input\n\n'
                table = Table(input_name)
                result += ','.join(map(lambda t: f'{t}:{map_type(table.col_types[t])}', table.col_names)) + '\n'
                with open(input_name) as f:
                    reader = csv.reader(f)
                    next(reader)  # skip header
                    for line in reader:
                        line2 = ','.join(map(lambda x: x if x != '' else 'NULL', map(str, line))).replace('\n', '\\n')
                        result += line2 + '\n'
                result += '\n'

            result += '#output\n\n'
            table = Table(spec['output'])
            result += ','.join(map(lambda t: f'{t}:{map_type(table.col_types[t])}', table.col_names)) + '\n'
            with open(spec['output']) as f:
                reader = csv.reader(f)
                next(reader)  # skip header
                for line in reader:
                    line2 = ','.join(map(lambda x: x if x != '' else 'NULL', map(str, line))).replace('\n', '\\n')
                    result += line2 + '\n'
            result += '\n'

            result += '#constraint\n{\n'

            if 'const' in spec:
                spec['constants'] = spec['const']
            if 'functions' in spec:
                spec['aggregation_functions'] = spec['functions']
            if 'aggrs' in spec:
                spec['aggregation_functions'] = spec['aggrs']

            if 'constants' not in spec:
                spec['constants'] = []

            if 'aggregation_functions' not in spec:
                spec['aggregation_functions'] = []

            if 'n' in spec['aggregation_functions']:
                spec['aggregation_functions'].remove('n')
                spec['aggregation_functions'].append('count')

            result += '"constants": ' + repr(list(map(str, spec['constants']))) + ',\n'
            result += '"aggregation_functions": ' + repr(spec['aggregation_functions'])

            result += '\n}\n'

            print("G")
            output_file = file.replace('tests/', args.output + '/').replace('.yaml', '')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

        except Exception as e:
            print(f'Failed to convert {file}!')
            print(e)
