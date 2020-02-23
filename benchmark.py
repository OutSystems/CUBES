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
parser.add_argument('name', metavar='NAME', help="name of the result file")

args = parser.parse_args()


def test_file(filename: str):
    test_name = filename.replace('tests/', '', 1).replace('.yaml', '')
    out_file = f'data-treatment/{args.name}/{test_name}.log'
    pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)

    command = ['runsolver', '-W', str(args.t), '-o', out_file, './squares.py', filename]

    print(' '.join(command))
    p = subprocess.run(command, capture_output=True, encoding='utf8')

    timeout = re.search('Maximum wall clock time exceeded: sending SIGTERM then SIGKILL', p.stdout) is not None

    try:
        status = re.search('Child status: (.*)', p.stdout)[1]
    except:
        status = None if timeout else 0

    process = None
    if not timeout:
        with open(out_file) as f:
            log = f.read()
            process = int(re.search('Solution found using process (.*)', log)[1])

    real = float(re.search('Real time \(s\): (.*)', p.stdout)[1])
    cpu = float(re.search('CPU time \(s\): (.*)', p.stdout)[1])
    ram = int(re.search('Max. memory \(cumulated for all children\) \(KiB\): (.*)', p.stdout)[1])

    with open('data-treatment/' + args.name + '.csv',
              'a') as f:  # TODO use a queue so that only one process needs to have the file open
        writer = csv.writer(f)
        writer.writerow((test_name, timeout, real, cpu, ram, process, status))
        f.flush()


os.mkdir(f'data-treatment/{args.name}')
with open('data-treatment/' + args.name + '.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(('name', 'timeout', 'real', 'cpu', 'ram', 'process', 'status'))
    f.flush()

with Pool(processes=16) as pool:
    pool.map(test_file, glob.glob('tests/**/*.yaml'), chunksize=1)
