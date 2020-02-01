# find if there is one integer constant in the list of constants
from typing import List

from tyrell.spec import TyrellSpec, TypeSpec, ProgramSpec, Type, ProductionSpec, ValueType
from tyrell.spec.expr import ExprType, Expr, BinaryExpr, BinaryOperator, PropertyExpr, ParamExpr
from tyrell.spec.spec import PredicateSpec

Empty_T = ValueType("Empty")
Table_T = ValueType("Table", [("col", ExprType.INT), ("row", ExprType.INT)])
TableSelect_T = ValueType("TableSelect", [("col", ExprType.INT), ("row", ExprType.INT)])


class DSL:

    def __init__(self):
        self.type_spec = TypeSpec()
        self.prog_spec = ProgramSpec("squares", [Table_T, Table_T], TableSelect_T)
        self.prod_spec = ProductionSpec()
        self.pred_spec = PredicateSpec()

        self.add_type(Empty_T)
        self.add_type(Table_T)
        self.add_type(TableSelect_T)

        self.add_function("empty", Empty_T, [Empty_T], [])

    def add_type(self, t: Type):
        self.type_spec.define_type(t)

    def add_function(self, name: str, lhs: ValueType, rhs: List[Type], expr: List[Expr]):
        self.prod_spec.add_func_production(name, lhs, rhs, expr)

    def get_spec(self):
        return TyrellSpec(self.type_spec, self.prog_spec, self.prod_spec, self.pred_spec)


dsl = DSL()
dsl.add_function("inner_join", Table_T, [Table_T, Table_T], [
    BinaryExpr(BinaryOperator.LE, PropertyExpr("col", ExprType.INT, ParamExpr(0)),
               BinaryExpr(BinaryOperator.ADD, PropertyExpr("col", ExprType.INT, ParamExpr(1)),
                          PropertyExpr("col", ExprType.INT, ParamExpr(2))
                          )
               )
])
