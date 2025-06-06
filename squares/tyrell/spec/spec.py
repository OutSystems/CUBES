from collections import defaultdict
from functools import cached_property
from typing import Iterable, List, Dict, DefaultDict, Optional, Union, Any, Tuple

from .expr import Expr
from .predicate import Predicate
from .production import EnumProduction, ParamProduction, FunctionProduction, Production
from .type import Type, EnumType, ValueType


class TypeSpec:
    _types: Dict[str, Type]

    def __init__(self):
        self._types = dict()

    def get_type(self, name: str) -> Optional[Type]:
        '''
        Return the type associated with `name`, if it is defined.
        If the type has not been defined, return `None`
        '''
        return self._types.get(name)

    def get_type_or_raise(self, name: str) -> Type:
        '''
        Return the type associated with `name`, if it is defined.
        If the type has not been defined, raise `KeyError`
        '''
        return self._types[name]

    def define_type(self, ty: Type) -> Type:
        '''
        Add the type `ty` to this spec. Return `ty` itself.
        Raise `ValueError` if another type with duplicated name is found.
        '''
        name = ty.name
        if name in self._types:
            raise ValueError(
                'The type has already been defined in the Tyrell spec: {}'
                    .format(ty))
        else:
            self._types[name] = ty
        return ty

    def types(self) -> Iterable[Type]:
        '''Return an iterator for all defined types'''
        return self._types.values()

    def num_types(self) -> int:
        '''Return the total number of defined types'''
        return len(self._types)

    def __repr__(self) -> str:
        new_line = '\n'
        return f'TypeSpec(\n{new_line.join(map(str, self._types.values()))}\n)'


class ProductionSpec:
    _productions: List[Production]
    _lhs_map: DefaultDict[str, List[Production]]
    _param_map: Dict[int, Production]
    _func_map: Dict[str, Production]

    def __init__(self):
        self._productions = list()
        self._lhs_map = defaultdict(list)
        self._param_map = dict()
        self._func_map = dict()
        self._enum_map = dict()

    def get_production(self, id: int) -> Optional[Production]:
        '''
        Return the production associated with `id`.
        If the id does not correspond to any production, return `None`
        '''
        return self._productions[id] if id < len(self._productions) else None

    def get_production_or_raise(self, id: int) -> Production:
        '''
        Return the production associated with `id`.
        If the id does not correspond to any production, return `KeyError`
        '''
        try:
            return self._productions[id]
        except IndexError:
            msg = 'Cannot find production with given id: {}'.format(id)
            raise KeyError(msg)

    def _get_productions_with_lhs(self, lhs: str) -> List[Production]:
        return self._lhs_map.get(lhs, [])

    def get_productions_with_lhs(self, ty: Union[str, Type]) -> List[Production]:
        '''
        Return the productions whose LHS is `ty`, where `ty` can be a Type or a string representing the name of the type
        If no production is found, or `ty` is not a string or a Type, return an empty list
        '''
        if isinstance(ty, Type):
            return self._get_productions_with_lhs(ty.name)
        elif isinstance(ty, str):
            return self._get_productions_with_lhs(ty)
        else:
            return []

    def get_function_production(self, name: str) -> Optional[Production]:
        '''
        Return the function production whose name is `name`.
        If no production is found, return `None`
        '''
        return self._func_map.get(name)

    def get_function_production_or_raise(self, name: str) -> Production:
        '''
        Return the function production whose name is `name`.
        If no production is found, raise `KeyError`
        '''
        return self._func_map[name]

    def get_function_productions(self) -> List[Production]:
        '''
        Return all function productions.
        '''
        return list(self._func_map.values())

    def get_param_production(self, index: int) -> Optional[Production]:
        '''
        Return the param production whose index is `index`.
        If no production is found, return `None`
        '''
        return self._param_map.get(index)

    def get_param_productions(self) -> List[Production]:
        '''
        Return all param productions
        If no production is found, return an empty list
        '''
        return list(self._param_map.values())

    def get_param_production_or_raise(self, index: int) -> Production:
        '''
        Return the function production whose name is `name`.
        If no production is found, raise `KeyError`
        '''
        return self._param_map[index]

    def get_enum_production(self, ty: EnumType, value: str) -> Optional[Production]:
        '''
        Return the enum production whose type is `type` and value is `value`.
        If no production is found, return `None`
        '''
        return self._enum_map.get((ty, value))

    def get_enum_production_or_raise(self, ty: EnumType, value: str) -> Optional[Production]:
        '''
        Return the enum production whose type is `type` and value is `value`.
        If no production is found, raise `KeyError`
        '''
        return self._enum_map[(ty, value)]

    def _get_next_id(self) -> int:
        return len(self._productions)

    def _add_production(self, prod: Production) -> None:
        self._productions.append(prod)
        self._lhs_map[prod.lhs.name].append(prod)

    def add_enum_production(self, lhs: EnumType, choice: int) -> EnumProduction:
        '''
        Create a new enum production. Return the created production.
        Raise `ValueError` if `choice` is out of bound.
        '''
        prod = EnumProduction(self._get_next_id(), lhs, choice)
        self._add_production(prod)
        self._enum_map[(lhs, prod.rhs)] = prod
        return prod

    def add_param_production(self, lhs: ValueType, index: int, value: Any = None) -> ParamProduction:
        '''
        Create new param production. Return the created production.
        Raise `ValueError` if a production with the same `index` has already been created.
        '''
        if index in self._param_map:
            raise ValueError(
                'Parameter Production with index {} has already been created'.format(index))
        prod = ParamProduction(self._get_next_id(), lhs, index, value)
        self._param_map[index] = prod
        self._add_production(prod)
        return prod

    def add_func_production(self, name: str, lhs: ValueType, rhs: List[Type],
                            constraints: List[Expr] = []) -> FunctionProduction:
        '''
        Create a new function production with the given `name`, `lhs`, and `rhs`. Return the created production.
        Raise `ValueError` if a production with the same `name` has already been created
        '''
        if name in self._func_map:
            raise ValueError(
                'Function Production with name {} has already been created'.format(name))
        prod = FunctionProduction(
            self._get_next_id(), name, lhs, rhs, constraints)
        self._func_map[name] = prod
        self._add_production(prod)
        return prod

    def productions(self) -> Iterable[Production]:
        '''Return all productions'''
        return self._productions

    def num_productions(self) -> int:
        '''Return the number of defined productions'''
        return len(self._productions)

    def __repr__(self):
        new_line = '\n'
        return f'ProductionSpec(\n{new_line.join(map(str, self._productions))}\n)'


class ProgramSpec:
    _name: str
    _inputs: List[Tuple[Type, Any]]
    _output: Tuple[Type, Any]

    def __init__(self, name: str, inputs: List[Tuple[Type, Any]], output: Tuple[Type, Any]):
        self._name = name
        self._inputs = inputs
        self._input_types = [type for type, _ in inputs]
        self._output = output
        self._output_type = output[0]

    @property
    def name(self) -> str:
        return self._name

    @property
    def inputs(self) -> List[Tuple[Type, Any]]:
        return self._inputs

    @property
    def input_types(self) -> List[Type]:
        return self._input_types

    def num_input(self) -> int:
        return len(self._inputs)

    @property
    def output(self) -> Tuple[Type, Any]:
        return self._output

    @property
    def output_type(self) -> Type:
        return self._output_type


class PredicateSpec:
    _preds: List[Predicate]
    _name_map: DefaultDict[str, List[Predicate]]

    def __init__(self):
        self._preds = list()
        self._name_map = defaultdict(list)

    def add_predicate(self, name: str, args: List[Any] = []) -> Predicate:
        pred = Predicate(name, args)
        self._preds.append(pred)
        self._name_map[name].append(pred)
        return pred

    def get_predicates_with_name(self, name: str) -> List[Predicate]:
        return self._name_map.get(name, [])

    def predicates(self) -> Iterable[Predicate]:
        return self._preds

    def num_predicates(self) -> int:
        '''Return the number of predicates'''
        return len(self._preds)

    def __repr__(self):
        new_line = '\n'
        return f'PredicateSpec(\n{new_line.join(map(str, self._preds))}\n)'


class TyrellSpec:
    _type_spec: TypeSpec
    _prog_spec: ProgramSpec
    _prod_spec: ProductionSpec
    _pred_spec: PredicateSpec

    def __init__(self,
                 type_spec,
                 prog_spec,
                 prod_spec=ProductionSpec(),
                 pred_spec=PredicateSpec()):
        # Generate all enum productions
        self._add_enum_productions(
            prod_spec,
            filter(lambda ty: isinstance(ty, EnumType),
                   type_spec.types()))
        # Generate all param productions
        self._add_param_productions(prod_spec, prog_spec.inputs)

        self._type_spec = type_spec
        self._prog_spec = prog_spec
        self._prod_spec = prod_spec
        self._pred_spec = pred_spec

    @staticmethod
    def _add_enum_productions(prod_spec, enum_tys):
        for ty in enum_tys:
            for i in range(len(ty.domain)):
                prod_spec.add_enum_production(ty, i)

    @staticmethod
    def _add_param_productions(prod_spec, input_tys):
        for i, ty in enumerate(input_tys):
            prod_spec.add_param_production(ty[0], i, ty[1])

    @cached_property
    def max_rhs(self):
        max_rhs = 0
        for production in self.productions():
            if production.is_function() and len(production.rhs) > max_rhs:
                max_rhs = len(production.rhs)
        return max_rhs

    # Delegate methods for TypeSpec
    def get_type(self, name: str) -> Optional[Type]:
        return self._type_spec.get_type(name)

    def get_type_or_raise(self, name: str) -> Type:
        return self._type_spec.get_type_or_raise(name)

    def types(self):
        return self._type_spec.types()

    def num_types(self) -> int:
        return self._type_spec.num_types()

    # Delegate methods for ProductionSpec
    def get_production(self, id: int) -> Optional[Production]:
        return self._prod_spec.get_production(id)

    def get_production_or_raise(self, id: int) -> Production:
        return self._prod_spec.get_production_or_raise(id)

    def get_productions_with_lhs(self, ty: Union[str, Type]) -> List[Production]:
        return self._prod_spec.get_productions_with_lhs(ty)

    def get_function_production(self, name: str) -> Optional[Production]:
        return self._prod_spec.get_function_production(name)

    def get_function_production_or_raise(self, name: str) -> Production:
        return self._prod_spec.get_function_production_or_raise(name)

    def get_function_productions(self) -> List[Production]:
        return self._prod_spec.get_function_productions()

    def get_param_production(self, index: int) -> Optional[Production]:
        return self._prod_spec.get_param_production(index)

    def get_param_production_or_raise(self, index: int) -> Production:
        return self._prod_spec.get_param_production_or_raise(index)

    def get_param_productions(self) -> List[Production]:
        return self._prod_spec.get_param_productions()

    def get_enum_production(self, ty: EnumType, value: str) -> Optional[Production]:
        return self._prod_spec.get_enum_production(ty, value)

    def get_enum_production_or_raise(self, ty: EnumType, value: str) -> Optional[Production]:
        return self._prod_spec.get_enum_production_or_raise(ty, value)

    def productions(self) -> Iterable[Production]:
        return self._prod_spec.productions()

    def num_productions(self) -> int:
        return self._prod_spec.num_productions()

    # Delegate methods for ProgramSpec
    @property
    def name(self) -> str:
        return self._prog_spec.name

    @property
    def input(self) -> List[Type]:
        return self._prog_spec.input_types

    def num_input(self) -> int:
        return self._prog_spec.num_input()

    @property
    def output(self) -> Type:
        return self._prog_spec.output_type

    # Delegate methods for PredicateSpec
    def get_predicates_with_name(self, name: str) -> List[Predicate]:
        return self._pred_spec.get_predicates_with_name(name)

    def predicates(self) -> Iterable[Predicate]:
        return self._pred_spec.predicates()

    def num_predicates(self) -> int:
        '''Return the number of predicates'''
        return self._pred_spec.num_predicates()

    def __repr__(self) -> str:
        result = ''
        result += repr(self._type_spec) + '\n'
        result += repr(self._prod_spec) + '\n'
        result += repr(self._pred_spec) + '\n'
        return result
