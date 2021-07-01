#!/usr/bin/python

import csv
import glob
import itertools

import re
from collections import defaultdict

from statistics import median, mean

from squares.util import parse_specification


def remove_text_between_parens(text):
    n = 1  # run at least once
    while n:
        text, n = re.subn(r'\([^()]*\)', '', text)  # remove non-nested/flat balanced parts
    return text


instance_sets = defaultdict(int)
solutions = defaultdict(int)
solution_sizes = defaultdict(list)
new_solution = {}

log_path = 'analysis/data/cubes49_16_0f_fake/'

with open('analysis/instances.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(('name',))
    f.flush()

    for file_name in glob.glob('tests/**/*.yaml', recursive=True):
        test_name = file_name.replace('tests/', '', 1).replace('.yaml', '')
        instance_set = re.search(r'(.*)/', test_name)[0] if 'spider' not in test_name else 'spider'
        instance_sets[instance_set] += 1

        spec = parse_specification(file_name)
        if 'solution' in spec:
            solutions[instance_set] += 1
            solution_sizes[instance_set].append(len(spec['solution']))
        else:
            found_one = False
            for log_file in glob.glob(log_path + test_name + '_*.log'):
                with open(log_file) as log:
                    match = re.search(r'Solution found: \[(.*)]', log.read())
                    if match:
                        if not found_one:
                            print('New solution for instance', test_name)
                            solutions[instance_set] += 1
                            found_one = True

                        sol = list(map(lambda x: x.strip(), remove_text_between_parens(match[1]).split(',')))
                        if test_name not in new_solution or len(sol) < len(new_solution[test_name]):
                            if test_name in new_solution:
                                solution_sizes[instance_set].remove(len(new_solution[test_name]))
                            solution_sizes[instance_set].append(len(sol))
                            new_solution[test_name] = sol

        writer.writerow((test_name,))

for instance_set in instance_sets.keys():
    print(instance_set)
    print(f'\t#instances: {instance_sets[instance_set]}')
    print(f'\t#solutions: {solutions[instance_set]}')
    try:
        print(f'\t#mean solution size: {mean(solution_sizes[instance_set])}')
        print(f'\t#median solution size: {median(solution_sizes[instance_set])}')
    except:
        pass

    print()

print('TOTAL')
print(f'\t#instances: {sum(instance_sets.values())}')
print(f'\t#solutions: {sum(solutions.values())}')
try:
    print(f'\t#mean solution size: {mean(itertools.chain.from_iterable(solution_sizes.values()))}')
    print(f'\t#median solution size: {median(itertools.chain.from_iterable(solution_sizes.values()))}')
except:
    pass

print()
