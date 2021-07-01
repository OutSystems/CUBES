import argparse
import glob
import pathlib
import re

import yaml
from ordered_set import OrderedSet

from squares.dsl import table

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'(?:------------------------------------- R Solution ---------------------------------------)'
cubes_end_sep = r'(?:------------------------------------- R Solution ---------------------------------------)|(?:All solutions of length \d+ found)|(?:Timeout reached)|(?:\[.*?\])'

dsl_regex = r'\[MainProcess\]\[INFO\] Solution found: \[(.*)\]'
sql_regex = rf'{cubes_sql_sep}((?:.|\n)*){cubes_r_sep}'

skipped = 0

dsl_list = []
sql_list = []

if __name__ == '__main__':

    parser = argparse.parser = argparse.ArgumentParser()
    parser.add_argument('run', metavar='RUN')
    args = parser.parse_args()

    for file in glob.glob(f'analysis/data/{args.run}/**/*.log', recursive=True):
        with open(file) as f:
            content = f.read()

        instance = file.replace(f'analysis/data/{args.run}/', '').replace('_0.log', '')
        yaml_file = f'tests-examples/{instance}.yaml'
        with open(yaml_file) as f:
            spec = yaml.safe_load(f)

        tmp1 = re.findall(dsl_regex, content)
        tmp2 = re.findall(rf'{cubes_r_sep}((?:.|\n)*?)(?:$|(?={cubes_end_sep}))', content)

        if len(tmp1) == len(tmp2):
            for dsl, cubes_sol in zip(tmp1, tmp2):
                tmp3 = re.search(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_end_sep})', cubes_sol)

                if tmp3 is None:
                    skipped += 1
                    continue

                else:
                    for i, input_f in enumerate(spec['inputs']):
                        table_name = table.get_table_name(input_f)
                        dsl = re.sub(rf'\binput{i}', table_name, dsl)

                    if ' ' not in dsl:
                        skipped += 1
                        continue

                    sql = tmp3[1]
                    sql = re.sub(r'(\s|\n)+', ' ', sql).strip()
                    sql = re.sub(r'SELECT ([^()]*?) FROM', 'SELECT * FROM', sql)

                    dsl_list.append(dsl)
                    sql_list.append(sql)

        else:
            skipped += len(tmp2)

    print('DSL programs collected:', len(dsl_list))
    print('SQL programs collected:', len(sql_list))
    print('Skipped:', skipped)

    tmp = OrderedSet(zip(dsl_list, sql_list))

    print('Examples after duplicate removal:', len(tmp))

    dsl_list, sql_list = zip(*tmp)

    with open('dsl_sols.txt', 'w') as f:
        f.write('\n'.join(dsl_list))

    with open('sql_sols.txt', 'w') as f:
        f.write('\n'.join(sql_list))


