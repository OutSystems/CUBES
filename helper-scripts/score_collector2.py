import argparse

import re

parser = argparse.ArgumentParser()

parser.add_argument('input')

args = parser.parse_args()

input = args.input
output = input + '.csv'

with open(input) as f:
    lines = f.readlines()

t = 0
current_line = 0
with open(output, 'w') as f:
    f.write('t,prod1,prod2,probability\n')

    for line in lines:
        match = re.search(r'^\[([0-9. ]+)\].*First line (.*)', line)
        if match:
            for prod, prob in eval(match[2]):
                f.write(f'{match[1].strip()},_,{prod},{prob}\n')

        match = re.search(r'^\[([0-9. ]+)\].*2-gram for (.*): (.*)', line)
        if match:
            for prod, prob in eval(match[3]):
                f.write(f'{match[1].strip()},{match[2]},{prod},{prob}\n')
