#!/usr/bin/python3
import argparse
import datetime

import yaml

if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert yaml spec file to old \'.in\' file.')
    parser.add_argument('input', metavar='INPUT')
    args = parser.parse_args()

    with open(args.input) as f:
        spec = yaml.safe_load(f)

    result = ''

    for input_name in spec['inputs']:
        result += '#input\n\n'
        with open(input_name) as f:
            result += f.read()
        result += '\n'

    result += '\n#output\n\n'
    with open(spec['output']) as f:
        result += f.read()
    result += '\n'

    result += '\n#constraint\n{\n'

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

    result += '"constants": ' + repr(list(map(lambda x: str(x) if isinstance(x, datetime.date) else x, spec['constants']))) + ',\n'
    result += '"aggregation_functions": ' + repr(spec['aggregation_functions'])

    result += '\n}\n'

    print(result)
