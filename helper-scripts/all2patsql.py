#!/usr/bin/python3
import argparse

import glob

import os
import re

import pandas
import yaml

from squares import types


def read_table(path: str):
    df = pandas.read_csv(path)
    df = df.convert_dtypes(convert_integer=False, convert_boolean=False)

    replacements = {}

    for col in df.columns:
        new_col, n = re.subn('[^a-zA-Z0-9._]', '.', col)
        new_col, n2 = re.subn('^([0-9])', r'col_\1', new_col)
        if n + n2:
            replacements[col] = new_col

    df = df.rename(columns=replacements)

    for col in df:  # try to coerce columns to datetime
        if types.get_type(df[col].dtype) == types.INT:
            for elem in df[col]:
                if elem >= 2 ** 32 - 1:
                    df[col] = df[col].astype(str)
                    break

        if all(types.is_time(elem) or pandas.isna(elem) for elem in df[col]) and any(types.is_time(elem) for elem in df[col]):
            try:
                df[col] = pandas.to_timedelta(df[col], errors='coerce')
            except Exception:
                pass

        elif all(types.is_date(elem) or pandas.isna(elem) for elem in df[col]) and any(types.is_date(elem) for elem in df[col]):
            try:
                df[col] = pandas.to_datetime(df[col], errors='coerce')
            except Exception:
                pass

    return df


def map_type(dtype) -> str:
    if pandas.api.types.is_integer_dtype(dtype):
        return 'Int'

    elif pandas.api.types.is_float_dtype(dtype):
        return 'Dbl'

    elif pandas.api.types.is_bool_dtype(dtype):
        return 'Str'

    elif pandas.api.types.is_datetime64_any_dtype(dtype):
        return 'Date'

    elif pandas.api.types.is_timedelta64_dtype(dtype):
        return 'Date'

    else:
        return 'Str'


if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert all yaml spec file to scythe format.')
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

            for input_name in spec['inputs']:
                result += '#input\n\n'
                df = read_table(input_name)
                result += ','.join(map(lambda t: f'{t[0]}:{map_type(t[1])}', zip(list(df.columns), list(df.dtypes)))) + '\n'
                with open(input_name) as f:
                    result += f.read().split('\n', 1)[1].replace(',,', ',"",').replace(',\n', ',""\n')
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

            output_file = file.replace('tests/', args.output + '/').replace('.yaml', '')
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            with open(output_file, 'w') as f:
                f.write(result)

        except Exception as e:
            print(f'Failed to convert {file}!')
