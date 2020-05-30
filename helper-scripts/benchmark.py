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
        command = ['runsolver', '-W', str(args.t), '--rss-swap-limit', '57344', '-d', '5', '-o', out_file, './squares.py', '-vv', filename]
    else:
        command = ['runsolver', '-W', str(args.t), '--rss-swap-limit', '57344', '-d', '5', '-o', out_file, './cubes.py', '-vv', filename]

    command += other_args

    print(' '.join(command))
    p = subprocess.run(command, capture_output=True, encoding='utf8')

    timeout = re.search('Maximum wall clock time exceeded: sending SIGTERM then SIGKILL', p.stdout) is not None
    memout = re.search('Maximum memory exceeded: sending SIGTERM then SIGKILL', p.stdout) is not None

    try:
        status = re.search('Child status: (.*)', p.stdout)[1]
    except:
        status = None if timeout or memout else 0

    process = None
    # if not timeout and not memout and not args.cubes:
    #     with open(out_file) as f:
    #         log = f.read()
    #         process = int(re.search('Solution found using process (.*)', log)[1])

    real = float(re.search('Real time \(s\): (.*)', p.stdout)[1])
    cpu = float(re.search('CPU time \(s\): (.*)', p.stdout)[1])
    ram = int(re.search('Max. memory \(cumulated for all children\) \(KiB\): (.*)', p.stdout)[1])

    with open('data-treatment/' + args.name + '.csv',
              'a') as f:  # TODO use a queue so that only one process needs to have the file open
        writer = csv.writer(f)
        writer.writerow((test_name, timeout, real, cpu, ram, process, status, memout))
        f.flush()


if not args.append:
    os.mkdir(f'data-treatment/{args.name}')
    with open('data-treatment/' + args.name + '.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(('name', 'timeout', 'real', 'cpu', 'ram', 'process', 'status', 'memout'))
        f.flush()

if args.p == 1:
    for file in glob.glob('tests/**/*.yaml', recursive=True):
        test_file(file)
else:
    with Pool(processes=args.p) as pool:
        pool.map(test_file, glob.glob('tests/**/*.yaml', recursive=True), chunksize=1)
