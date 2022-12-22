#!/usr/bin/python

import argparse
import csv
import glob
import os
import os.path
import pathlib
import random
import re
import subprocess
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Util for benchmarking the SQUARES program synthesizer.')
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('-p', default=1, type=int, help='#processes')
parser.add_argument('-m', default=57344, type=int, help='memout')
parser.add_argument('--resume', action='store_true', help='resume previous run')
parser.add_argument('--instances')
parser.add_argument('--n-sols', default=1, type=int)
parser.add_argument('name', metavar='NAME', help="name of the result file")

args = parser.parse_args()


def test_file(filename: str):
    if not os.path.isfile(filename):
        return

    test_name = filename.replace('data/', '', 1)
    out_file = f'data-treatment/{args.name}/{test_name}.log'
    pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)

    command = ['runsolver', '-W', str(args.t), '--rss-swap-limit', str(args.m), '-d', '5', '-o', out_file, 'java', '-Xmx62g', '-jar', 'out/artifacts/patsql/patsql-engine.jar',
               filename]

    if args.n_sols != 1:
        command += [str(args.n_sols)]

    print(' '.join(command))
    p = subprocess.run(command, capture_output=True, encoding='utf8')

    timeout = re.search('Maximum wall clock time exceeded: sending SIGTERM then SIGKILL', p.stdout) is not None
    memout = re.search('Maximum memory exceeded: sending SIGTERM then SIGKILL', p.stdout) is not None

    try:
        status = re.search('Child status: (.*)', p.stdout)[1]
    except:
        status = None if timeout or memout else 0

    real = float(re.search('Real time \(s\): (.*)', p.stdout)[1])
    cpu = float(re.search('CPU time \(s\): (.*)', p.stdout)[1])
    ram = int(re.search('Max. memory \(cumulated for all children\) \(KiB\): (.*)', p.stdout)[1])

    with open('data-treatment/' + args.name + '.csv',
              'a') as f:  # TODO use a queue so that only one process needs to have the file open
        writer = csv.writer(f)
        writer.writerow((test_name, timeout, real, cpu, ram, status, memout))
        f.flush()


if __name__ == '__main__':

    if not args.instances:
        instances = list(glob.glob('data/**/*', recursive=True))
        random.shuffle(instances)  # reduce chances of two memory intensive instances running at the same time
    else:
        instances = []
        with open(args.instances) as inst_list:
            for inst in inst_list.readlines():
                instances += list(glob.glob(inst[:-1], recursive=True))

    print(instances)

    if not args.resume:
        os.mkdir(f'data-treatment/{args.name}')
        with open('data-treatment/' + args.name + '.csv', 'w') as f:
            writer = csv.writer(f)
            writer.writerow(('name', 'timeout', 'real', 'cpu', 'ram', 'status', 'memout'))
            f.flush()
    else:
        with open('data-treatment/' + args.name + '.csv', 'r') as f:
            reader = csv.reader(f)
            existing_instances = []
            for row in reader:
                existing_instances.append('data/' + row[0])
                print('Skipping', 'data/' + row[0])

        instances = filter(lambda x: x not in existing_instances, instances)

    with Pool(processes=args.p) as pool:
        pool.map(test_file, instances, chunksize=1)
