#!/usr/bin/python

import argparse
import csv
import glob
import os
import pathlib
import re
import subprocess
import random
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Util for benchmarking the SQUARES program synthesizer.')
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-m', default=57344, type=int, help='memout')
parser.add_argument('-n', default=1, type=int, help='number of times to run each instance')
parser.add_argument('-p', default=1, type=int, help='#processes')
parser.add_argument('--append', action='store_true', help='append to file')
parser.add_argument('--cubes', action='store_true', help='use cubes')
parser.add_argument('--portfolio', action='store_true', help='use portfolio')
parser.add_argument('--portfolio2', action='store_true', help='use portfolio 2')
parser.add_argument('--sequential2', action='store_true', help='use sequential 2')
parser.add_argument('--resume', action='store_true', help='resume previous run')
parser.add_argument('--sample', type=float)
parser.add_argument('--instances')
parser.add_argument('name', metavar='NAME', help="name of the result file")

args, other_args = parser.parse_known_args()


def test_file(filename: str, run: str = ''):
    test_name = filename.replace('tests/', '', 1).replace('.yaml', '')
    out_file = f'analysis/data/{args.name}/{test_name}{run}.log'
    pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)

    if args.cubes:
        command = ['helper-scripts/runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, './cubes.py', '-vv', filename]
    elif args.portfolio:
        command = ['helper-scripts/runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, './portfolio.py', '-vv', filename]
    elif args.portfolio2:
        command = ['helper-scripts/runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, './portfolio2.py', '-vv', filename]
    elif args.sequential2:
        command = ['helper-scripts/runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, './sequential2.py', '-vv', filename]
    else:
        command = ['helper-scripts/runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, './sequential.py', '-vv', filename]

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
    if not timeout and not memout and not args.cubes:
        with open(out_file) as f:
            log = f.read()
            try:
                process = int(re.search('Solution found using process (.*)', log)[1])
            except:
                process = None

    real = float(re.search('Real time \(s\): (.*)', p.stdout)[1])
    cpu = float(re.search('CPU time \(s\): (.*)', p.stdout)[1])
    ram = int(re.search('Max. memory \(cumulated for all children\) \(KiB\): (.*)', p.stdout)[1])

    with open('analysis/data/' + args.name + '.csv', 'a') as f:  # TODO use a queue so that only one process needs to have the file open
        writer = csv.writer(f)
        writer.writerow((test_name, timeout, real, cpu, ram, process, status, memout))
        f.flush()


if __name__ == '__main__':
    if not args.append and not args.resume:
        os.mkdir(f'analysis/data/{args.name}')
        with open('analysis/data/' + args.name + '.csv', 'w') as f:
            writer = csv.writer(f)
            writer.writerow(('name', 'timeout', 'real', 'cpu', 'ram', 'process', 'status', 'memout'))
            f.flush()

    if not args.instances:
        instances = glob.glob('tests/**/*.yaml', recursive=True)
    else:
        instances = []
        with open(args.instances) as inst_list:
            for inst in inst_list.readlines():
                instances += list(glob.glob(inst[:-1], recursive=True))

    if args.sample:
        instances = random.sample(instances, int(len(instances) * args.sample))

    if args.resume:
        with open('analysis/data/' + args.name + '.csv', 'r') as f:
            reader = csv.reader(f)
            existing_instances = []
            for row in reader:
                existing_instances.append('tests/' + row[0] + '.yaml')
                print('Skipping', 'tests/' + row[0] + '.yaml')

        instances = filter(lambda x: x not in existing_instances, instances)

    if args.p == 1:
        for i in range(args.n):
            for file in instances:
                test_file(file, f'_{i}')
    else:
        with Pool(processes=args.p) as pool:
            for i in range(args.n):
                pool.map(test_file, instances, chunksize=1)
