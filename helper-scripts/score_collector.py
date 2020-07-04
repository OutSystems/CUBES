import argparse

import re

parser = argparse.ArgumentParser()

parser.add_argument('input')

args = parser.parse_args()

input = args.input
output = re.sub('.log', '.csv', input)

with open(input) as f:
    lines = f.readlines()

t = 0
current_line = 0
with open(output, 'w') as f:
    f.write('t,line,production,score\n')
    for line in lines:
        match = re.search(r'^\[([0-9. ]+)\].*Current score', line)
        if match:
            t = match[1]

        match = re.match(r'(.*): \{', line)
        if match:
            current_line = match[1]

        match = re.search(r'\s+(.*)\s+:\s+(.*)\s+', line)
        if match:
            f.write(f'{t},{current_line},{match[1].strip()},{match[2]}\n')
