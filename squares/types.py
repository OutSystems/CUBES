from enum import Enum
from typing import Union, Dict

import dateutil
import pandas
from numpy import dtype
from ordered_set import OrderedSet
from pandas.core.dtypes.base import ExtensionDtype


class Type(Enum):
    INT = 1
    STRING = 2
    FLOAT = 3
    DATETIME = 4
    BOOL = 5

INT = Type.INT
STRING = Type.STRING
FLOAT = Type.FLOAT
DATETIME = Type.DATETIME
BOOL = Type.BOOL


def get_type(dtype: Union[ExtensionDtype, dtype]) -> Type:
    if pandas.api.types.is_integer_dtype(dtype):
        return Type.INT

    elif pandas.api.types.is_float_dtype(dtype):
        return Type.FLOAT

    elif pandas.api.types.is_bool_dtype(dtype):
        return Type.Bool

    elif pandas.api.types.is_datetime64_any_dtype(dtype):
        return Type.DATETIME

    else:
        return Type.STRING


def is_date(o):
    if not isinstance(o, str):
        return False

    try:
        dateutil.parser.parse(o)
        return True

    except ValueError:
        return False


def is_integer(o):
    try:
        int(o)
        return True
    except ValueError:
        return False


def is_float(o):
    try:
        float(o)
        return True
    except ValueError:
        return False


def is_bool(o):
    return isinstance(o, str) and o.lower() in ['t', 'f', 'true', 'false']


def is_type(o, type):
    if type == Type.INT:
        return is_integer(o)

    elif type == Type.FLOAT:
        return is_float(o)

    elif type == Type.DATETIME:
        return is_date(o) and not is_integer(o)

    elif type == Type.BOOL:
        return is_bool(o)

    elif type == Type.STRING:
        return isinstance(o, str)

    else:
        raise NotImplemented


def empty_type_map() -> Dict[Type, OrderedSet]:
    d = {}
    for type in Type:
        d[type] = OrderedSet()
    return d


def _get_r_type(type):
    if type == Type.INT:
        return 'col_integer()'
    elif type == Type.FLOAT:
        return 'col_double()'
    elif type == Type.BOOL:
        return 'col_logical()'
    elif type == Type.DATETIME:
        return 'col_character()'  # dates are parsed later (because of different date formats)
    elif type == Type.STRING:
        return 'col_character()'
    else:
        raise NotImplemented


def get_r_types(dtypes):
    result = []
    for col, type in zip(dtypes.index, map(get_type, dtypes)):
        result.append(f'{col} = {_get_r_type(type)}')
    return ','.join(result)
