from collections import defaultdict
from itertools import permutations
from logging import getLogger
from typing import (
    cast,
    Tuple,
    Set,
    FrozenSet,
    Mapping,
    MutableMapping,
    Iterator
    )

import z3

from .assert_violation_handler import AssertionViolationHandler
from .blame import Blame
from .constraint_encoder import ConstraintEncoder
from .eval_expr import eval_expr
from .example_base import Example, ExampleDecider
from .result import ok, bad
from ..dsl import Node, AtomNode, ParamNode, ApplyNode, NodeIndexer
from ..interpreter import Interpreter, InterpreterError
from ..spec import Production, ValueType, TyrellSpec
from ..spec.expr import *
from ..visitor import GenericVisitor
from ... import util

logger = getLogger('tyrell.synthesizer.constraint')
ImplyMap = Mapping[Tuple[Production, Expr], List[Production]]
MutableImplyMap = MutableMapping[Tuple[Production, Expr], List[Production]]

COUNTER = 0

class Z3Encoder(GenericVisitor):
    _interp: Interpreter
    _indexer: NodeIndexer
    _example: Example
    _unsat_map: Dict[str, Tuple[Node, int]]
    _solver: z3.Solver

    def __init__(self, interp: Interpreter, indexer: NodeIndexer, example: Example):
        self._interp = interp
        self._indexer = indexer
        self._example = example
        self._unsat_map = dict()
        self._solver = z3.Solver()
        self._solver.set('random_seed', util.get_config().seed)
        self._solver.set('sat.random_seed', util.get_config().seed)

    def get_z3_var(self, node: Node, pname: str, ptype: ExprType):
        node_id = self._indexer.get_id(node)
        var_name = '{}_n{}'.format(pname, node_id)
        if ptype is ExprType.INT:
            return z3.Int(var_name)
        elif ptype is ExprType.BOOL:
            return z3.Bool(var_name)
        else:
            raise RuntimeError('Unrecognized ExprType: {}'.format(ptype))

    def _get_constraint_var(self, node: Node, index: int):
        global COUNTER
        node_id = self._indexer.get_id(node)
        var_name = '@n{}_c{}_{}'.format(node_id, index, COUNTER)
        COUNTER += 1
        return var_name

    def encode_param_alignment(self, node: Node, ty: ValueType, index: int):
        if not isinstance(ty, ValueType):
            raise RuntimeError(
                'Unexpected program output type: {}'.format(ty))
        for pname, pty in ty.properties:
            actual = self.get_z3_var(node, pname, pty)
            expected_expr = PropertyExpr(pname, pty, ParamExpr(index))
            expected = eval_expr(
                self._interp, self._example.input, self._example.output, expected_expr)
            self._solver.add(actual == expected)

    def encode_output_alignment(self, prog: Node):
        out_ty = cast(ValueType, prog.type)
        self.encode_param_alignment(prog, out_ty, 0)

    def visit_param_node(self, param_node: ParamNode):
        param_ty = cast(ValueType, param_node.type)
        self.encode_param_alignment(
            param_node, param_ty, param_node.index + 1)

    def visit_atom_node(self, atom_node: AtomNode):
        pass

    def visit_apply_node(self, apply_node: ApplyNode):
        def encode_property(prop_expr):
            param_expr = cast(ParamExpr, prop_expr.operand)
            if param_expr.index == 0:
                node = apply_node
            else:
                node = apply_node.args[param_expr.index - 1]
            pname = prop_expr.name
            pty = prop_expr.type
            return self.get_z3_var(node, pname, pty)

        constraint_visitor = ConstraintEncoder(encode_property)
        print(apply_node)
        for index, constraint in enumerate(apply_node.production.constraints):
            cname = self._get_constraint_var(apply_node, index)
            z3_clause = constraint_visitor.visit(constraint)
            self._unsat_map[cname] = (apply_node, index)
            self._solver.assert_and_track(z3_clause, cname)
        for arg in apply_node.args:
            self.visit(arg)

    def get_blame_nodes(self):
        if self._solver.check() != z3.unsat:
            # Abstract semantics is satisfiable or unknown. Cannot learn anything.
            return None
        unsat_core = self._solver.unsat_core()
        if len(unsat_core) == 0:
            return None

        unsat_dict = defaultdict(list)
        for v in unsat_core:
            node, cidx = self._unsat_map[str(v)]
            unsat_dict[node].append(node.production.constraints[cidx])
        return unsat_dict


class BlameFinder:
    _interp: Interpreter
    _imply_map: ImplyMap
    _prog: Node
    _indexer: NodeIndexer
    _blames_collection: Set[FrozenSet[Blame]]

    def __init__(self, interp: Interpreter, imply_map: ImplyMap, prog: Node):
        self._interp = interp
        self._imply_map = imply_map
        self._prog = prog
        self._indexer = NodeIndexer(prog)
        self._blames_collection = set()

    def _get_raw_blames(self) -> List[List[Blame]]:
        return [list(x) for x in self._blames_collection]

    def _expand_blame(self, base_nodes: List[Node], node: Node, exprs: List[Expr]) -> Iterator[FrozenSet[Blame]]:
        def gen_blame(prod):
            return frozenset(
                [Blame(node=n, production=(prod if n is node else n.production))
                 for n in base_nodes]
            )

        for expr in exprs:
            for other_prod in self._imply_map.get((node.production, expr), []):
                yield gen_blame(other_prod)

    def get_blames(self) -> List[List[Blame]]:
        return [list(x) for x in self._blames_collection]

    def process_examples(self, examples: List[Example]):
        for example in examples:
            self.process_example(example)

    def process_example(self, example: Example):
        z3_encoder = Z3Encoder(self._interp, self._indexer, example)
        z3_encoder.encode_output_alignment(self._prog)
        z3_encoder.visit(self._prog)
        blame_nodes = z3_encoder.get_blame_nodes()
        if blame_nodes is not None:
            base_nodes = list(blame_nodes.keys())
            for node, exprs in blame_nodes.items():
                for blame in self._expand_blame(base_nodes, node, exprs):
                    self._blames_collection.add(blame)
            self._blames_collection.add(
                frozenset([(n, n.production) for n in base_nodes])
            )


class ExampleConstraintDecider(ExampleDecider):
    _imply_map: ImplyMap
    _assert_handler: AssertionViolationHandler

    def __init__(self,
                 spec: TyrellSpec,
                 interpreter: Interpreter,
                 examples: List[Example]):
        super().__init__(interpreter, examples)
        self._imply_map = self._build_imply_map(spec)
        self._assert_handler = AssertionViolationHandler(spec, interpreter)

    def _check_implies(self, pre, post) -> bool:
        def encode_property(prop_expr: PropertyExpr):
            param_expr = cast(ParamExpr, prop_expr.operand)
            var_name = '{}_p{}'.format(prop_expr.name, param_expr.index)
            ptype = prop_expr.type
            if ptype is ExprType.INT:
                return z3.Int(var_name)
            elif ptype is ExprType.BOOL:
                return z3.Bool(var_name)
            else:
                raise RuntimeError('Unrecognized ExprType: {}'.format(ptype))

        constraint_visitor = ConstraintEncoder(encode_property)

        z3_solver = z3.Solver()
        z3_solver.set('random_seed', util.get_config().seed)
        z3_solver.set('sat.random_seed', util.get_config().seed)
        z3_pre = constraint_visitor.visit(pre)
        z3_post = constraint_visitor.visit(post)
        z3_solver.add(z3.Not(z3.Implies(z3_pre, z3_post)))
        return z3_solver.check() == z3.unsat

    def _build_imply_map(self, spec: TyrellSpec) -> ImplyMap:
        ret: MutableImplyMap = defaultdict(list)
        constrained_prods = filter(
            lambda prod: prod.is_function() and len(prod.constraints) > 0,
            spec.productions())
        for prod0, prod1 in permutations(constrained_prods, r=2):
            if len(prod0.rhs) != len(prod1.rhs):
                continue
            for c0 in prod0.constraints:
                for c1 in prod1.constraints:
                    if self._check_implies(c1, c0):
                        ret[(prod0, c0)].append(prod1)
                        break
        return ret

    def analyze(self, prog):
        '''
        This version of analyze() tries to analyze the reason why a synthesized program fails, if it does not pass all the tests.
        '''
        failed_examples = self.get_failed_examples(prog)
        if len(failed_examples) == 0:
            return ok()
        else:
            blame_finder = BlameFinder(self.interpreter, self._imply_map, prog)
            blame_finder.process_examples(failed_examples)
            blames = blame_finder.get_blames()
            if len(blames) == 0:
                return bad()
            else:
                return bad(why=blames)

    def analyze_interpreter_error(self, error: InterpreterError):
        return self._assert_handler.handle_interpreter_error(error)
