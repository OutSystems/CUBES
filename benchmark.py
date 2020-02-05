#!/usr/bin/python

import argparse
import csv
import glob
import os
import subprocess

parser = argparse.ArgumentParser(description='Util for benchmarking the SQUARES program synthesizer.')
parser.add_argument('-t', default=600, type=int, help='timeout')
parser.add_argument('name', metavar='NAME', help="name of the result file")

args = parser.parse_args()

with open('results_' + args.name + '.csv', 'w') as f:
    writer = csv.writer(f)
    for filename in glob.glob('tests/**/*.yaml'):
        print(' '.join(['timeout', str(args.t), 'mytime', './squaresEnumerator.py', filename]))
        p = subprocess.Popen(['timeout', str(args.t), 'mytime', './squaresEnumerator.py', filename], stdout=subprocess.PIPE)
        p.wait()

        if p.returncode != 124:
            out = p.stdout.read().split()
            try:
                time = float(out[0])
                system = float(out[1])
                user = float(out[2])
                ram = int(out[3])
            except:
                time = -1
                system = -1
                user = -1
                ram = -1
        else:
            time = -1
            system = -1
            user = -1
            ram = -1

        writer.writerow((filename, time, system, user, ram))
        f.flush()
