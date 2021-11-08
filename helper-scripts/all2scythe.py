#!/usr/bin/python3
import argparse
import csv
import glob
import os
from logging import getLogger

import yaml

from squares import types
from squares.dsl.table import Table

getLogger('squares').setLevel(50)


def convert_line(table:Table, line, corrected):
    line2 = []
    for i, cell in enumerate(line):
        if isinstance(cell, str) and cell == '':
            if table.col_types[table.col_names[i]] == types.INT or table.col_types[table.col_names[i]] == types.FLOAT:
                line2.append('NULL[num]')
            elif table.col_types[table.col_names[i]] == types.STRING or table.col_types[table.col_names[i]] == types.BOOL:
                line2.append('NULL[str]')
            elif table.col_types[table.col_names[i]] == types.DATETIME:
                line2.append('NULL[date]')
            elif table.col_types[table.col_names[i]] == types.TIME:
                line2.append('NULL[time]')
            else:
                print(table.col_types[table.col_names[i]])
                raise NotImplementedError
        elif isinstance(cell, str) and ',' in cell:
            corrected = True
            line2.append(cell.replace(',', ';'))
        else:
            line2.append(cell)
    return line2, corrected


if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert all yaml spec file to scythe format.')
    parser.add_argument('output', metavar='OUTPUT')
    args = parser.parse_args()

    for file in glob.glob('tests/**/*.yaml', recursive=True):
        if 'schema.yaml' in file:
            continue

        print(file)

        try:
            corrected = False

            with open(file) as f:
                spec = yaml.safe_load(f)

            result = ''

            for input_name in spec['inputs']:
                table = Table(input_name)
                result += '#input\n\n'
                result += ','.join(map(lambda t: f'{t}', table.col_names)) + '\n'
                with open(input_name) as f:
                    reader = csv.reader(f)
                    next(reader)  # skip header
                    for line in reader:
                        line2, corrected = convert_line(table, line, corrected)
                        line2 = ','.join(map(str, line2)).replace('\n', '\\n')
                        result += line2 + '\n'
                result += '\n'

            result += '#output\n\n'
            table = Table(spec['output'])
            result += ','.join(map(lambda t: f'{t}', table.col_names)) + '\n'
            with open(spec['output']) as f:
                reader = csv.reader(f)
                next(reader)  # skip header
                for line in reader:
                    line2, corrected = convert_line(table, line, corrected)
                    line2 = ','.join(map(str, line2)).replace('\n', '\\n')
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

            output_file = file.replace('tests/', args.output + '/').replace('.yaml', '')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

            if corrected:
                print('\tCORRECTED')

        except Exception as e:
            print(f'Failed to convert {file}!')
            print(e)
