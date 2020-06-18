from enum import Enum
from typing import Union, Dict

import datetime
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
    NONE = 6
    TIME = 7


INT = Type.INT
STRING = Type.STRING
FLOAT = Type.FLOAT
DATETIME = Type.DATETIME
BOOL = Type.BOOL
NONE = Type.NONE
TIME = Type.TIME

operators_by_type = {
    INT: OrderedSet(['==', '!=', '<=', '>=', '<', '>']),
    FLOAT: OrderedSet(['==', '!=', '<=', '>=', '<', '>']),
    DATETIME: OrderedSet(['==', '!=', '<=', '>=', '<', '>']),
    TIME: OrderedSet(['==', '!=', '<=', '>=', '<', '>']),
    BOOL: OrderedSet(['==', '!=']),
    STRING: OrderedSet(['==', '!='])
    }


def get_type(dtype: Union[ExtensionDtype, dtype]) -> Type:
    if pandas.api.types.is_integer_dtype(dtype):
        return Type.INT

    elif pandas.api.types.is_float_dtype(dtype):
        return Type.FLOAT

    elif pandas.api.types.is_bool_dtype(dtype):
        return Type.BOOL

    elif pandas.api.types.is_datetime64_any_dtype(dtype):
        return Type.DATETIME

    elif pandas.api.types.is_timedelta64_dtype(dtype):
        return Type.TIME

    else:
        return Type.STRING


dflt_1 = datetime.datetime(1, 1, 1)
dflt_2 = datetime.datetime(2, 2, 2)


def is_time(o):
    if not isinstance(o, str) or o.startswith('T') or not any(i.isdigit() for i in o) or '-' in o or 'a' in o:
        return False

    try:
        dt1 = dateutil.parser.parse(o, default=dflt_1)
        dt2 = dateutil.parser.parse(o, default=dflt_2)

        if dt2 != dt1:
            return True

        return False
    except (ValueError, OverflowError):
        return False


def is_date(o):
    if not isinstance(o, str) or o.startswith('T') or not any(i.isdigit() for i in o):
        return False

    try:
        dateutil.parser.parse(o)
        return True

    except (ValueError, OverflowError):
        return False


def is_integer(o):
    try:
        i = int(o)
        return True
    except (ValueError, TypeError):
        return False


def is_float(o):
    try:
        float(o)
        return True
    except (ValueError, TypeError):
        return False


def is_bool(o):
    return isinstance(o, str) and o.lower() in ['t', 'f', 'true', 'false']


def is_none(o):
    return o is None


def is_type(o, type):
    if type == Type.NONE:
        return o is None

    if type == Type.INT:
        return is_integer(o)

    elif type == Type.FLOAT:
        return is_float(o)

    elif type == type.TIME:
        return is_time(o) and not is_integer(o)

    elif type == Type.DATETIME:
        return is_date(o) and not is_integer(o)

    elif type == Type.BOOL:
        return is_bool(o)

    elif type == Type.STRING:
        return isinstance(o, str)

    else:
        raise NotImplementedError


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
    elif type == Type.TIME:
        return 'col_time()'
    elif type == Type.STRING:
        return 'col_character()'
    else:
        raise NotImplementedError


def get_r_types(dtypes):
    result = []
    for col, type in zip(dtypes.index, map(get_type, dtypes)):
        result.append(f'{col} = {_get_r_type(type)}')
    return ','.join(result)


def to_r_repr(o):
    if o is None:
        return 'NA'
    elif is_type(o, Type.DATETIME):
        return f"'{o}'"
    elif is_type(o, Type.STRING):
        return f"'{o}'"
    elif is_type(o, Type.INT) or is_type(o, Type.FLOAT):
        return str(o)  # TODO should use R integers for ints but it needs to be tested first
    else:
        raise NotImplementedError  # TODO what about times and booleans??
