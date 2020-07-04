#!/usr/bin/python

import argparse
import csv
import glob
import os
import pathlib
import re
import subprocess
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Util for benchmarking the SQUARES program synthesizer.')
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-p', default=1, type=int, help='#processes')
parser.add_argument('--append', action='store_true', help='append to file')
parser.add_argument('--cubes', action='store_true', help='use cubes')
parser.add_argument('name', metavar='NAME', help="name of the result file")

args, other_args = parser.parse_known_args()


def test_file(filename: str):
    test_name = filename.replace('tests/', '', 1).replace('.yaml', '')
    out_file = f'data-treatment/{args.name}/{test_name}.log'
    pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)

    if not args.cubes:
        command = ['./squares.py', filename]
    else:
        command = ['./cubes.py', filename]

    command += other_args

    print(' '.join(command))
    p = subprocess.run(command, capture_output=True, encoding='utf8')

    info = p.stdout[:-1].split(',')

    with open('data-treatment/' + args.name + '.csv', 'a') as f:
        writer = csv.writer(f)
        writer.writerow((test_name, *info))
        f.flush()


with open('data-treatment/' + args.name + '.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(('name',))
    f.flush()

if args.p == 1:
    for file in glob.glob('tests/**/*.yaml', recursive=True):
        test_file(file)
else:
    with Pool(processes=args.p) as pool:
        pool.map(test_file, glob.glob('tests/**/*.yaml', recursive=True), chunksize=1)
