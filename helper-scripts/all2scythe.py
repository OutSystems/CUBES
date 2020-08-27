#!/usr/bin/python3
import argparse

import glob

import os
import yaml

if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert all yaml spec file to scythe format.')
    parser.add_argument('output', metavar='OUTPUT')
    args = parser.parse_args()

    for file in glob.glob('tests-examples/**/*.yaml', recursive=True):
        if 'schema.yaml' in file:
            continue

        print(file)

        try:

            with open(file) as f:
                spec = yaml.safe_load(f)

            result = ''

            for input_name in spec['inputs']:
                result += '#input\n\n'
                with open(input_name) as f:
                    result += f.read()
                result += '\n'

            result += '#output\n\n'
            with open(spec['output']) as f:
                result += f.read()
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

            output_file = file.replace('tests-examples', args.output).replace('.yaml', '')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

        except:
            print(f'Failed to convert {file}!')
