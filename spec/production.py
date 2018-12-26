from typing import List, Union
from abc import ABC, abstractmethod
from .type import Type, EnumType, ValueType


class Production(ABC):
    '''
    This class represent a CFG production rule for our DSL.
    Each production rule is uniquely identified by its ID in a given spec.
    '''

    _id: int
    _lhs: Type

    @abstractmethod
    def __init__(self, id: int, lhs: Type):
        self._id = id
        self._lhs = lhs

    @property
    def id(self):
        return self._id

    @property
    def lhs(self):
        return self._lhs

    @property
    @abstractmethod
    def rhs(self) -> List[Union[str, int, Type]]:
        raise NotImplementedError

    @abstractmethod
    def is_function(self) -> bool:
        raise NotImplementedError


class EnumProduction(Production):
    _choice: int

    def __init__(self, id: int, lhs: EnumType, choice: int):
        super().__init__(id, lhs)
        if choice >= len(lhs.domain):
            msg = 'Cannot create a EnumProduction with choice {} for a domain with {} elements.'.format(
                choice, len(lhs.domain))
            raise ValueError(msg)
        self._choice = choice

    def _get_rhs(self) -> str:
        return self._lhs.domain[self._choice]

    @property
    def rhs(self) -> List[str]:
        return [self._get_rhs()]

    def is_function(self) -> bool:
        return False

    def __repr__(self) -> str:
        return 'EnumProduction(id={}, lhs={!r}, choice={})'.format(
            self._id, self._lhs, self._choice)

    def __str__(self) -> str:
        return 'Production {}: {} -> {}'.format(
            self._id, self._lhs, self._get_rhs())


class ParamProduction(Production):
    _param_id: int

    def __init__(self, id: int, lhs: ValueType, param_id: int):
        super().__init__(id, lhs)
        if not isinstance(lhs, ValueType):
            raise ValueError('LHS of ParamProduction must be a value type')
        self._param_id = param_id

    @property
    def rhs(self) -> List[int]:
        return [self._param_id]

    def is_function(self) -> bool:
        return False

    def __repr__(self) -> str:
        return 'ParamProduction(id={}, lhs={!r}, param_id={})'.format(
            self._id, self._lhs, self._param_id)

    def __str__(self) -> str:
        return 'Production {}: {} -> <param {}>'.format(
            self._id, self._lhs, self._param_id)


class FunctionProduction(Production):
    _name: str
    _rhs: List[Type]

    def __init__(self, id: int, name: str, lhs: ValueType, rhs: List[Type]):
        super().__init__(id, lhs)
        if not isinstance(lhs, ValueType):
            raise ValueError('LHS of FunctionProduction must be a value type')
        if len(rhs) == 0:
            raise ValueError(
                'Cannot construct a FunctionProduction with empty RHS')
        self._name = name
        self._rhs = rhs

    @property
    def rhs(self):
        return self._rhs

    @property
    def name(self):
        return self._name

    def is_function(self) -> bool:
        return True

    def __repr__(self) -> str:
        return 'FunctionProduction(id={}, lhs={!r}, name={}, rhs={})'.format(
            self._id, self._lhs, self._name, self._rhs)

    def __str__(self) -> str:
        return 'Production {}: {} -> {}({})'.format(
            self._id, self._lhs, self._name,
            ', '.join([str(x) for x in self._rhs]))