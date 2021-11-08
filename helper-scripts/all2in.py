#!/usr/bin/python3
import argparse
import csv

import glob

import os
import re

import yaml

from squares.dsl.table import Table

if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert all yaml spec files to old \'.in\' file.')
    parser.add_argument('output', metavar='OUTPUT')
    args = parser.parse_args()

    for file in glob.glob('tests/**/*.yaml', recursive=True):
        if 'schema.yaml' in file:
            continue

        print(file)

        try:
            with open(file) as f:
                spec = yaml.safe_load(f)

            result = ''

            for input in spec['inputs']:
                table = Table(input)
                os.makedirs(os.path.dirname(re.sub('^tests', args.output, input)), exist_ok=True)
                with open(input) as in_f, open(re.sub('^tests', args.output, input), 'w') as out_f:
                    out_f.write(','.join(map(lambda t: f'{t}', table.col_names)) + '\n')
                    reader = csv.reader(in_f)
                    next(reader)  # skip header
                    writer = csv.writer(out_f)
                    for line in reader:
                        writer.writerow((map(lambda x: x.replace('\n', '\\n'), line)))

            table = Table(spec['output'])
            with open(spec['output']) as in_f, open(re.sub('^tests', args.output, spec['output']), 'w') as out_f:
                out_f.write(','.join(map(lambda t: f'{t}', table.col_names)) + '\n')
                reader = csv.reader(in_f)
                next(reader)  # skip header
                writer = csv.writer(out_f)
                for line in reader:
                    writer.writerow((map(lambda x: x.replace('\n', '\\n'), line)))

            result += 'inputs: ' + ', '.join(spec['inputs']) + '\n'
            result += 'output: ' + spec['output'] + '\n'

            if 'constants' in spec:
                spec['const'] = spec['constants']
            if 'functions' in spec:
                spec['aggrs'] = spec['functions']
            if 'columns' in spec:
                spec['attrs'] = spec['columns']
            if 'filters' in spec:
                if 'aggrs' not in spec:
                    spec['aggrs'] = []
                spec['aggrs'] += spec['filters']

            if 'aggrs' in spec and 'count' in spec['aggrs']:
                spec['aggrs'].remove('count')
                spec['aggrs'].append('n')

            if 'aggrs' in spec and 'avg' in spec['aggrs']:
                spec['aggrs'].remove('avg')
                spec['aggrs'].append('mean')

            if 'attrs' in spec:
                spec['attrs'] = [a.lower() for a in spec['attrs']]

            for a in ['const', 'aggrs', 'attrs', 'bools']:
                if a in spec:
                    result += a + ': ' + ', '.join(map(lambda x: f'"{x}"', spec[a])) + '\n'
                else:
                    result += a + ':\n'

            if 'loc' in spec:
                result += 'loc: ' + str(spec['loc']) + '\n'
            else:
                result += 'loc: 1\n'

            output_file = re.sub('^tests', args.output, file).replace('.yaml', '.in')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

        except Exception as e:
            print(e)
            print(f'Failed to convert {file}!')
