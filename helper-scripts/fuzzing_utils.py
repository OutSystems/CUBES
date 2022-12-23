import logging
import os
import pathlib
import random
import re
import sys
from contextlib import contextmanager
from itertools import permutations
from logging import getLogger

import chromalog
import pandas
import rpy2

from squares.tyrell.logger import TyrellLogFormatter

getLogger('squares').setLevel(50)
logger = getLogger('fuzzer')

formatter = TyrellLogFormatter(fmt='[%(seconds)s][%(processName)s][%(levelname)s] %(message)s')
handler = chromalog.ColorizingStreamHandler()
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.DEBUG)


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'(?:------------------------------------- R Solution ---------------------------------------)|(?:All solutions of length \d+ found)|(?:Timeout reached)|(?:\n\n)|(?:\[.*?\])'


@contextmanager
def suppress_stdout():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout


def shuffled(lst):
    lst = list(lst)
    random.shuffle(lst)
    return lst


def removesuffix(str1, str2):
    if str1.endswith(str2):
        return str1[:-len(str2)]
    return str1


def sanitize_sql(result_sql, spec_in, run='cubes'):

    if run == 'smbop' or run == 'ratsql':
        return result_sql

    if run != 'patsql':
        result_sql = result_sql.replace('df_', '')
        # result_sql = re.sub(r'\bOVER \(\)', '', result_sql, re.IGNORECASE)
        # print(result_sql)

        if run == 'squares':
            result_sql = '\n'.join(filter(lambda x: not x.startswith('Joining, by'), result_sql.split('\n')))

        if run == 'cubes':
            for table_name in map(lambda x: pathlib.Path(x).stem, spec_in['inputs']):
                result_sql = re.sub(table_name.replace('-', '[-_]'), table_name, result_sql)
                result_sql = re.sub(fr'(?<!`)\b{table_name}\b(?!`)', f'`{table_name}`', result_sql)

        if run != 'scythe':
            for i, input in enumerate(spec_in['inputs']):
                result_sql = re.sub(f'`?input{i}`?', f'`{pathlib.Path(input).stem}`', result_sql)
        else:
            for scythe_name, table_name in zip(
                    sorted(set(re.findall(r'\binput[0-9]?\b', result_sql))),
                    map(lambda x: pathlib.Path(x).stem, spec_in['inputs'])):
                result_sql = re.sub(fr'\b{scythe_name}\b', table_name, result_sql)

    else:
        for i, input in enumerate(spec_in['inputs']):
            result_sql = result_sql.replace(f'input{i + 1}', pathlib.Path(input).stem)

    if run == 'scythe':
        result_sql = re.sub(r'(?:As\s+t[0-9]+\s+)+(As\s+t[0-9]+)', r'\1', result_sql).replace(', From', ' From')
        result_sql = re.sub(r'\A\(((?:.|\n)*)\)\s+As\s+t[0-9]+\s*;?\s*\Z', r'\1', result_sql).replace('Count_distinct(',
                                                                                                      'Count(distinct ')

    return result_sql


def execute(connection, sql):
    df = pandas.read_sql_query(sql, connection)
    df.columns = df.columns.str.lower()
    df = df.sort_values(by=sorted(list(df.columns))).reset_index(drop=True)
    return df


def print_df_diff(df1, df2, sql1=None, sql2=None):
    logger.warning('\tEXPECTED OUTPUT ====')
    if sql1:
        logger.warning('\n\t\t%s', str(sql1).replace('\n', '\n\t\t'))
    logger.warning('\n\t%s', str(df1).replace('\n', '\n\t'))

    logger.warning('\tACTUAL OUTPUT ====')
    if sql2:
        logger.warning('\n\t\t%s', str(sql2).replace('\n', '\n\t\t'))
    logger.warning('\n\t%s', str(df2).replace('\n', '\n\t'))


def df_soft_equal(df1, df2):
    for perm in permutations(list(df1.columns)):
        actual_df_try = df2.copy()
        actual_df_try.columns = perm
        try:
            pandas.testing.assert_frame_equal(df1, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=False)
            return True
        except:
            actual_df_try = actual_df_try.sort_values(by=sorted(list(actual_df_try.columns))).reset_index(drop=True)
            try:
                pandas.testing.assert_frame_equal(df1, actual_df_try, check_dtype=False, check_names=False, check_like=True, check_datetimelike_compat=False)
                return True
            except:
                pass

    return False
