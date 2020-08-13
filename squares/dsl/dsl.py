from typing import Any

from ordered_set import OrderedSet

from squares.tyrell.spec import ValueType, EnumType
from squares.tyrell.spec.expr import *


class Helper:
    def __getattr__(self, name: str) -> Any:
        if name in ['col', 'row']:
            return lambda i: PropertyExpr(name, ExprType.INT, ParamExpr(i))

        elif name in BinaryOperator.__members__:
            return lambda x, y: BinaryExpr(BinaryOperator[name], x, y)

        elif name in UnaryOperator.__members__:
            return lambda x: UnaryExpr(UnaryOperator[name], x)

        raise AttributeError


h = Helper()

Table = ValueType('Table',
                  [('col', ExprType.INT),
                   ('row', ExprType.INT)
                   ])

JoinCondition = EnumType('JoinCondition')
JoinCols = EnumType('JoinCols')
Col = EnumType('Col')
Cols = EnumType('Cols')
CrossJoinCondition = EnumType('CrossJoinCondition')
FilterCondition = EnumType('FilterCondition')
Op = EnumType('Op')
SummariseCondition = EnumType('SummariseCondition')

functions = OrderedSet(['natural_join', 'natural_join3', 'natural_join4', 'inner_join', 'anti_join', 'left_join', 'union', 'intersect', 'semi_join', 'cross_join', 'unite', 'filter', 'filters', 'summarise', 'mutate'])

natural_join = ('natural_join',
                Table,
                [Table, Table],
                [h.LE(h.col(0), h.ADD(h.col(1), h.col(2))),
                 h.LE(h.row(0), h.MUL(h.row(1), h.row(2)))])

natural_join3 = ('natural_join3',
                 Table,
                 [Table, Table, Table],
                 [h.LE(h.col(0), h.ADD(h.ADD(h.col(1), h.col(2)), h.col(3))),
                  h.LE(h.row(0), h.MUL(h.MUL(h.row(1), h.row(2)), h.row(3)))])

natural_join4 = ('natural_join4',
                 Table,
                 [Table, Table, Table, Table],
                 [h.LE(h.col(0), h.ADD(h.ADD(h.ADD(h.col(1), h.col(2)), h.col(3)), h.col(4))),
                  h.LE(h.row(0), h.MUL(h.MUL(h.MUL(h.row(1), h.row(2)), h.row(3)), h.row(4)))])

inner_join = ('inner_join',
              Table,
              [Table, Table, JoinCondition],
              [h.LE(h.col(0), h.ADD(h.col(1), h.col(2))),
               h.LE(h.row(0), h.MUL(h.row(1), h.row(2)))])

anti_join = ('anti_join',
             Table,
             [Table, Table, JoinCols],
             [h.EQ(h.col(0), ConstExpr(1)),
              h.LE(h.row(0), h.row(1))])

left_join = ('left_join',
             Table,
             [Table, Table],
             [h.LE(h.col(0), h.ADD(h.col(1), h.col(2))),
              h.EQ(h.row(0), h.row(1))])

union = ('union',
         Table,
         [Table, Table],
         [h.LE(h.col(0), h.ADD(h.col(1), h.col(2))),
          h.EQ(h.row(0), h.ADD(h.row(1), h.row(2)))])

intersect = ('intersect',
             Table,
             [Table, Table, Col],
             [h.EQ(h.col(0), ConstExpr(1)),
              h.LE(h.row(0), h.row(1)),
              h.LE(h.row(0), h.row(2))])

semi_join = ('semi_join',
             Table,
             [Table, Table],
             [h.EQ(h.col(0), h.col(1)),
              h.LE(h.row(0), h.row(1))])

cross_join = ('cross_join',
              Table,
              [Table, Table, CrossJoinCondition],
              [h.LE(h.col(0), h.ADD(h.col(1), h.col(2))),
               h.LE(h.row(0), h.MUL(h.row(1), h.row(2)))])

unite = ('unite',
         Table,
         [Table, Col, Col],
         [h.LE(h.col(0), h.col(1)),
          h.LE(h.row(0), h.row(1))])

filter = ('filter',
          Table,
          [Table, FilterCondition],
          [h.EQ(h.col(0), h.col(1)),
           h.LE(h.row(0), h.row(1))])

filters = ('filters',
           Table,
           [Table, FilterCondition, FilterCondition, Op],
           [h.EQ(h.col(0), h.col(1)),
            h.LE(h.row(0), h.row(1))])

summarise = ('summarise',
             Table,
             [Table, SummariseCondition, Cols],
             [h.LE(h.col(0), ConstExpr(3)),  # TODO should depend on max_cols
              h.LE(h.row(0), h.row(1))])

mutate = ('mutate',
          Table,
          [Table, SummariseCondition],
          [h.GE(h.col(0), h.col(1)),
           h.EQ(h.row(0), h.row(1))])
