import csv
import os
import pathlib
import re
from logging import getLogger

import pandas

from squares import types, util

logger = getLogger('squares')


def sanitize_col(col, path=''):
    new_col = col.lower()
    new_col, n = re.subn('[^a-zA-Z0-9_]', '_', new_col)
    new_col, n2 = re.subn('^([0-9_])', r'col_\1', new_col)
    if n + n2:
        logger.warning('Column names should be valid R identifiers. Trying to fix names. Conflicts may arise!')
        logger.warning('Replacing column "%s" in table %s with %s', col, path, new_col)
    return new_col


class Table:

    def __init__(self, filename: str, name: str = None):
        if not pathlib.Path(filename).is_file():
            raise FileNotFoundError()

        self.path = filename

        if name is not None:
            self.name = name
        else:
            self.name = 'df_' + os.path.splitext(os.path.basename(filename))[0]
        self.name = re.sub('[^a-zA-Z0-9._]', '_', self.name)

        with open(filename, 'r') as file:
            reader = csv.reader(file)
            header = next(reader)

        self.get_types_from_header(header)

        give_up = False
        while not give_up:
            try:
                self.df = pandas.read_csv(filename, header=0, names=self.col_names, dtype=types.map_to_pandas(self.col_types), parse_dates=types.get_date_cols(self.col_types))
                break
            except ValueError as e:
                tmp = re.match(r'Unable to parse string "(.*)" at position (\d+)', e.args[0])
                if tmp is not None:
                    failing_string, line = tmp.groups()
                    with open(filename) as f:
                        r = csv.reader(f)
                        for i, l in enumerate(r):
                            if i == int(line) + 1:
                                col = self.col_names[l.index(failing_string)]
                                if self.col_types[col] == types.STRING:
                                    give_up = True
                                else:
                                    logger.error('Column %s in table %s was labeled as %s but parsing failed due to entry "%s". Attempting to use column as string...', col, self.name, self.col_types[col], failing_string)
                                    self.col_types[col] = types.STRING
                                break
                else:
                    tmp = re.match(r'cannot safely convert passed user dtype of .* for object dtyped data in column (\d+)', e.args[0])
                    if tmp is not None:
                        col = self.col_names[int(tmp.group(1))]
                        if self.col_types[col] == types.STRING:
                            give_up = True
                        else:
                            logger.error('Column %s in table %s was labeled as %s but parsing failed. Attempting to use column as string...', col, self.name, self.col_types[col])
                            self.col_types[col] = types.STRING

        for col, col_type in self.col_types.items():
            if col_type == types.UNKNOWN:
                if all(types.is_time(elem) or pandas.isna(elem) for elem in self.df[col]) and any(
                        types.is_time(elem) for elem in self.df[col]):
                    try:
                        self.df[col] = pandas.to_timedelta(self.df[col], errors='coerce')
                        self.col_types[col] = types.TIME
                    except Exception:
                        pass

                elif all(types.is_date(elem) or pandas.isna(elem) for elem in self.df[col]) and any(
                        types.is_date(elem) for elem in self.df[col]):
                    try:
                        self.df[col] = pandas.to_datetime(self.df[col], errors='coerce')
                        self.col_types[col] = types.DATETIME
                    except Exception:
                        pass

                else:
                    self.col_types[col] = types.get_type(self.df.dtypes[col])

    def get_types_from_header(self, header):
        self.col_names = []
        self.col_types = {}

        for col in header:
            if ':' not in col:
                col_name = sanitize_col(col, path=self.path)
                self.col_names.append(col_name)
                self.col_types[col_name] = types.UNKNOWN
            else:
                col_name, col_type = col.rsplit(':', 1)
                col_name = sanitize_col(col_name, path=self.path)
                self.col_names.append(col_name)
                self.col_types[col_name] = types.map_type(col_type)

    def gen_r_read_code(self):
        code = f'{self.name} <- read_csv("{self.path}", skip=1, ' \
               f'col_names=c({",".join(map(util.single_quote_str, self.col_names))}), ' \
               f'col_types=cols({types.get_r_types([self.col_types[col] for col in self.col_names])}))\n'

        return code
