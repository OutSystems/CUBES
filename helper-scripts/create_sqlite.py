import argparse
import os
from pathlib import Path

import pandas
import sqlalchemy

from squares.util import parse_specification

if __name__ == '__main__':
    parser = argparse.parser = argparse.ArgumentParser(description='Convert yaml spec file to old \'.in\' file.')
    parser.add_argument('input', metavar='INPUT')
    args = parser.parse_args()

    spec = parse_specification(args.input)

    sqlite_name = os.path.join(os.path.dirname(args.input), 'tables', Path(args.input).stem + '.sqlite3')

    print(sqlite_name)

    engine = sqlalchemy.create_engine(f"sqlite+pysqlite:///{sqlite_name}")

    for input in spec['inputs']:
        df = pandas.read_csv(input)
        df.to_sql(Path(input).stem, engine, index=False, if_exists='replace')

    if 'db' not in spec:
        with open(args.input, 'r+') as f:
            lines = f.readlines()
            lines.insert(0, f'db: {sqlite_name}\n')
            f.seek(0)
            f.writelines(lines)
