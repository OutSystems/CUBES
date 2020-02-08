#!/usr/bin/python

import argparse
import csv
import glob
import os
import pathlib
import subprocess
from multiprocessing import Pool

parser = argparse.ArgumentParser(description='Util for benchmarking the SQUARES program synthesizer.')
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('name', metavar='NAME', help="name of the result file")

args = parser.parse_args()


def test_file(filename:str):
    out_file = filename.replace('tests/', f'data-treatment/{args.name}/').replace('.yaml', '.log')
    pathlib.Path(os.path.dirname(out_file)).mkdir(parents=True, exist_ok=True)
    command = ['timeout', str(args.t), 'mytime', out_file, './squaresEnumerator.py', '-d', filename]
    print(' '.join(command))
    p = subprocess.run(command, capture_output=True)
    time = -1
    system = -1
    user = -1
    ram = -1
    if p.returncode == 0:
        out = p.stdout.split()
        try:
            time = float(out[0])
            system = float(out[1])
            user = float(out[2])
            ram = int(out[3])
        except:
            pass

    elif p.returncode == 124:
        time = args.t

    with open('data-treatment/' + args.name + '.csv', 'a') as f:
        writer = csv.writer(f)
        writer.writerow((filename, time, system, user, ram))
        f.flush()


os.mkdir(f'data-treatment/{args.name}')
with open('data-treatment/' + args.name + '.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(('name', 'real', 'system', 'user', 'ram'))
    f.flush()

with Pool(processes=1) as pool:
    pool.map(test_file, glob.glob('tests/**/*.yaml'))
