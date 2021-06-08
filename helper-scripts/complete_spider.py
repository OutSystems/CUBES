import argparse
import glob
import multiprocessing
import os
import pathlib
import re
import shutil
import subprocess
import json
import tempfile
from logging import getLogger
from multiprocessing import Pool
import random
import sqlite3

import pandas
import pglast
import rpy2
from rpy2 import robjects

from squares import util
from squares.config import Config
from squares.dsl.specification import Specification
from squares.util import parse_specification, create_argparser


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print

parser = create_argparser(all_inputs=True)
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-m', default=57344, type=int, help='memout')
parser.add_argument('-p', default=1, type=int, help='#processes')

args = parser.parse_args()


def compare(instance_file: str):
    instance = re.sub('^tests/', '', instance_file).replace('.yaml', '')
    bench = instance.split('/')[1]

    spec_in = parse_specification(instance_file)
    shutil.copyfile(f'/home/ricardo/Downloads/spider/database/{bench}/{bench}.sqlite', f'tests-examples/spider/{bench}/tables/db.sqlite')
    if 'db' not in spec_in:
        if not os.path.isfile(f'tests-examples/spider/{bench}/tables/db.sqlite'):
            shutil.copyfile(f'/home/ricardo/Downloads/spider/database/{bench}/{bench}.sqlite', f'tests-examples/spider/{bench}/tables/db.sqlite')

        with open(instance_file, 'r+') as f:
            lines = f.readlines()
            lines.insert(0, f'db: tests-examples/spider/{bench}/tables/db.sqlite\n')
            f.seek(0)
            f.writelines(lines)


instances = list(glob.glob('tests/spider/**/*.yaml', recursive=True))

if args.p == 1:
    for file in instances:
        compare(file)
else:
    with Pool(processes=args.p) as pool:
        pool.map(compare, instances, chunksize=1)
