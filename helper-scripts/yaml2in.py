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

    result += 'inputs: ' + ', '.join(spec['inputs']) + '\n'
    result += 'output: ' + spec['output'] + '\n'

    for a in ['const', 'aggrs', 'attrs', 'bools']:
        if a in spec:
            result += a + ': ' + ', '.join(map(lambda x: f'"{x}"', spec[a])) + '\n'
        else:
            result += a + ':\n'

    if 'loc' in spec:
        result += 'loc: ' + str(spec['loc']) + '\n'
    else:
        result += 'loc: 1\n'

    with open(args.output, 'w') as f:
        f.write(result)
