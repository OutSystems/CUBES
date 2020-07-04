#!/usr/bin/python3
import argparse

import yaml

if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert yaml spec file to old \'.in\' file.')
    parser.add_argument('input', metavar='INPUT')
    parser.add_argument('output', metavar='OUTPUT')
    args = parser.parse_args()

    with open(args.input) as f:
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

    result += '"constants": ' + repr(spec['constants']) + ',\n'
    result += '"aggregation_functions": ' + repr(spec['aggregation_functions'])

    result += '\n}\n'

    with open(args.output, 'w') as f:
        f.write(result)
