from collections import defaultdict
from functools import cached_property
from itertools import permutations, islice
from logging import getLogger
from typing import Optional, List, Callable, TypeVar, Tuple, Type, Any, Dict

import z3
from ordered_set import OrderedSet

from .enumerator import Enumerator
from ..spec import TyrellSpec, Predicate
from ..spec.production import LineProduction, Production
from ... import util, program
from ...program import Program
from ...decider import RejectionInfo, RowNumberInfo
from ...dsl.specification import Specification
from ...util import flatten

logger = getLogger('squares.enumerator')

ExprType = TypeVar('ExprType')


class Node:
    def __init__(self) -> None:
        self.var: Optional[z3.ExprRef] = None
        self.bitvec: Optional[z3.ExprRef] = None


class Root(Node):
    def __init__(self, enumerator: 'BitEnumerator', id: int) -> None:
        super().__init__()
        self.id = id
        self.children = []
        self.type = self._create_type_variable(enumerator)
        self.var = self._create_root_variable(enumerator)
        self.bitvec = enumerator.create_variable(f'bv_{id}', z3.BitVec, enumerator.specification.n_columns)

    def _create_root_variable(self, enumerator: 'BitEnumerator') -> z3.ExprRef:
        var = enumerator.create_variable(f'root_{self.id}', z3.Int)
        enumerator.variables.append(var)
        ctr = []
        for p in enumerator.spec.productions():
            if p.is_function() and p.lhs.name != 'Empty':
                ctr.append(var == p.id)
        enumerator.assert_expr(z3.Or(ctr), f'root_{self.id}_domain')
        return var

    def _create_type_variable(self, enumerator: 'BitEnumerator') -> z3.ExprRef:
        var = enumerator.create_variable(f'type_{self.id}', z3.Int)
        # variable range constraints
        enumerator.assert_expr(z3.And(var >= 0, var < len(enumerator.types)), f'type_{self.id}_domain')
        return var

    def __repr__(self) -> str:
        return f'Root({self.id}, children={len(self.children)})'


class Leaf(Node):
    def __init__(self, enumerator: 'BitEnumerator', parent: Root, child_id: int):
        super().__init__()
        self.parent = parent
        self.child_id = child_id
        self.var = self._create_leaf_variable(enumerator)
        self.lines = self._create_lines_variables(enumerator)
        self.bitvec = enumerator.create_variable(f'bv_{parent.id}_{child_id}_a', z3.BitVec, enumerator.specification.n_columns)
        self.bitvec2 = enumerator.create_variable(f'bv_{parent.id}_{child_id}_b', z3.BitVec, enumerator.specification.n_columns)

    def _create_leaf_variable(self, enumerator: 'BitEnumerator') -> z3.ExprRef:
        var = enumerator.create_variable(f'leaf_{self.parent.id}_{self.child_id}', z3.Int)
        enumerator.variables.append(var)

        ctr = []
        for p in enumerator.spec.productions():
            if not p.is_function() or p.lhs.name == 'Empty':  # FIXME: improve empty integration
                ctr.append(var == p.id)

        for l in range(0, self.parent.id - 1):
            line_productions = enumerator.line_productions[l]
            for line_production in line_productions:
                ctr.append(var == line_production.id)

        enumerator.assert_expr(z3.Or(ctr), f'leaf_{self.parent.id}_{self.child_id}_domain')
        return var

    def _create_lines_variables(self, enumerator: 'BitEnumerator') -> List[z3.ExprRef]:
        lines = []
        for x in range(1, self.parent.id):
            var = enumerator.create_variable(f'leaf_{self.parent.id}_{self.child_id}_is_line_{x}', z3.Bool)
            enumerator.line_vars_by_line[x].append(var)
            lines.append(var)
        return lines

    def __repr__(self) -> str:
        return f'Leaf(parent={self.parent}, child_id={self.child_id})'


class BitEnumerator(Enumerator):

    def __init__(self, tyrell_spec: TyrellSpec, spec: Specification, loc: int = None, debug: bool = True):
        super().__init__()

        if util.get_config().z3_QF_FD:
            # self.z3_solver = Then('lia2card', 'card2bv', 'dt2bv', 'simplify', 'bit-blast', 'sat').solver()
            self.z3_solver = z3.SolverFor('QF_FD')

        else:
            self.z3_solver = z3.Solver()
            self.z3_solver.set('phase_selection', util.get_config().z3_smt_phase)
            self.z3_solver.set('case_split', util.get_config().z3_smt_case_split)

        self.z3_solver.set('unsat_core', True)
        self.z3_solver.set('core.minimize', True)
        self.z3_solver.set('random_seed', util.get_config().seed)

        self.z3_solver.set('sat.random_seed', util.get_config().seed)
        self.z3_solver.set('sat.phase', util.get_config().z3_sat_phase)
        self.z3_solver.set('sat.restart', util.get_config().z3_sat_restart)
        self.z3_solver.set('sat.branching.heuristic', util.get_config().z3_sat_branching)
        # self.z3_solver.set("sat.cardinality.solver", True)

        self.spec = tyrell_spec
        self.specification = spec
        self.loc = loc
        self.debug = debug

        if loc <= 0:
            raise ValueError(f'LOC cannot be non-positive: {loc}')

        self.line_productions = []
        self.line_productions_by_id = {}

        self.bitvec_cache = {}

        # z3 variables for each production node
        self.variables = []

        self.num_constraints = 0
        self.num_variables = 0

        self.clean_model = {}
        self.num_prods = self.spec.num_productions()
        self.max_children = self.spec.max_rhs
        self.create_line_productions()
        self.line_vars_by_line = defaultdict(list)
        self.roots, self.leaves = self.build_trees()
        self.model = None
        self.current_program = None
        self.unsat_core = None

        if util.get_config().lines_force_all_inputs:
            self.create_input_constraints()
        self.create_output_constraints()
        self.create_lines_constraints()
        self.create_type_constraints()
        self.create_children_constraints()
        self._production_id_cache = defaultdict(OrderedSet)
        for p in self.spec.productions():
            if p.is_enum():
                self._production_id_cache[p.rhs].append(p.id)
        self._production_id_cache.default_factory = lambda: None  # from now on throw errors id invalid keys are accessed
        self.resolve_predicates()
        self.create_bitvector_constraints()

        self.more_restrictive = self.specification.condition_generator.more_restrictive.compile(tyrell_spec)
        self.less_restrictive = self.specification.condition_generator.less_restrictive.compile(tyrell_spec)

        self.natural_join = self.spec.get_function_production('natural_join')
        self.natural_join3 = self.spec.get_function_production('natural_join3')
        self.natural_join4 = self.spec.get_function_production('natural_join4')

        logger.debug('Enumerator for loc %d constructed using %d variables and %d constraints', self.loc, self.num_variables,
                     self.num_constraints)

        res = self.z3_solver.check()
        if res != z3.sat:
            logger.debug(f"There is no solution for current loc ({self.loc}).")
        else:
            self.model = self.z3_solver.model()

    @cached_property
    def types(self) -> List[str]:
        types = []
        for t in self.spec.types():
            if t.name == 'Empty':
                continue
            for p in self.spec.get_productions_with_lhs(t):  # type must be the lhs of some production
                if p.is_function():
                    types.append(t.name)
                    break
        return types

    def create_variable(self, name: str, type: 'Callable[[str, ...], ExprType]', *args) -> ExprType:
        self.num_variables += 1
        return type(name, *args)

    def assert_expr(self, expr: z3.ExprRef, name: str = None, track: bool = False) -> None:
        self.num_constraints += 1
        if self.debug and track and name is not None:
            self.z3_solver.assert_and_track(expr, name)
        else:
            self.z3_solver.add(expr)

    def get_production(self, prod_id: int) -> Production:
        if prod_id in self.line_productions_by_id:
            return self.line_productions_by_id[prod_id]
        else:
            return self.spec.get_production(prod_id)

    def create_line_productions(self) -> None:
        for l in range(0, self.loc - 1):
            line_productions = []
            for t in self.types:
                self.num_prods += 1
                line_production = LineProduction(self.num_prods, self.spec.get_type(t), l)
                self.line_productions_by_id[self.num_prods] = line_production
                line_productions.append(line_production)
            self.line_productions.append(line_productions)

    def build_trees(self) -> Tuple[List[Root], List[Leaf]]:
        """Builds a loc trees, each tree will be a line of the program"""
        nodes = []
        leaves = []
        for i in range(1, self.loc + 1):
            n = Root(self, i)
            for x in range(self.max_children):
                child = Leaf(self, n, x)
                n.children.append(child)
                leaves.append(child)
            nodes.append(n)
        return nodes, leaves

    def create_output_constraints(self) -> None:
        """The output production matches the output type"""
        ctr = []
        var = self.roots[-1].var  # last line corresponds to the output line
        for p in self.spec.get_productions_with_lhs(self.spec.output):
            ctr.append(var == p.id)
        self.assert_expr(z3.Or(ctr), 'output_has_correct_type')
        # self.assert_expr(z3.AtLeast(*(z3.Extract(i, i, self.roots[-1].bitvec) == z3.BitVecVal(1, 1) for i in range(self.specification.n_columns)),
        #                          len(self.specification.output_cols)),
        #                  'output_has_at_least_k_columns')

    def create_lines_constraints(self) -> None:
        """Each line is used at least once in the program"""
        for r in range(1, len(self.roots)):
            self.assert_expr(z3.Or(self.line_vars_by_line[r]), f'line_{r}_is_used')

    def create_input_constraints(self) -> None:
        """Each input will appear at least once in the program"""
        input_productions = self.spec.get_param_productions()
        for i, prod in enumerate(input_productions):
            ctr = []
            for y in self.leaves:
                ctr.append(y.var == prod.id)
            self.assert_expr(z3.Or(ctr), f'input_{i}_is_used')

    def create_type_constraints(self) -> None:
        """If a production is used in a node, then the nodes' type is equal to the production's type"""
        for r in self.roots:
            for t in range(len(self.types)):
                if self.types[t] == 'Empty':
                    continue
                for p in self.spec.get_productions_with_lhs(self.types[t]):
                    if p.is_function():
                        self.assert_expr(z3.Implies(r.var == p.id, r.type == t))

    def create_children_constraints(self) -> None:
        for r in self.roots:
            for p in self.spec.productions():
                if not p.is_function() or p.lhs.name == 'Empty':
                    continue
                aux = r.var == p.id
                for c in range(len(r.children)):
                    ctr = []
                    if c >= len(p.rhs):
                        self.assert_expr(z3.Implies(aux, z3.And(r.children[c].var == 0,
                                                                r.children[c].bitvec == self.mk_bitvec(0),
                                                                r.children[c].bitvec2 == self.mk_bitvec(0))))
                        continue

                    for leaf_p in self.spec.get_productions_with_lhs(p.rhs[c]):
                        if not leaf_p.is_function():
                            bv1, bv2 = leaf_p.value if isinstance(leaf_p.value, tuple) else (leaf_p.value, 0)
                            ctr.append(z3.And(r.children[c].var == leaf_p.id,
                                              r.children[c].bitvec == self.mk_bitvec(bv1),
                                              r.children[c].bitvec2 == self.mk_bitvec(bv2)))

                    for l in range(r.id - 1):
                        for line_production in self.line_productions[l]:
                            if line_production.lhs.name == p.rhs[c].name:
                                ctr.append(z3.And(r.children[c].var == line_production.id,
                                                  r.children[c].bitvec == self.roots[l].bitvec,
                                                  r.children[c].bitvec2 == self.mk_bitvec(0)))
                                # if a previous line is used, then its flag must be true
                                line_var = r.children[c].lines[l]
                                self.assert_expr(line_var == (r.children[c].var == line_production.id))

                    self.assert_expr(z3.Implies(aux, z3.Or(ctr)))

    def create_bitvector_constraints(self) -> None:
        bv0 = z3.BitVecVal(0, self.specification.n_columns)

        for i, root in enumerate(self.roots):
            natural_join = self.spec.get_function_production('natural_join')
            if natural_join:
                self.assert_expr(z3.Implies(root.var == natural_join.id,
                                            z3.And(root.children[0].var < root.children[1].var,
                                                   (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec)),
                                 f'natural_join_bv_{i}')

            natural_join3 = self.spec.get_function_production('natural_join3')
            if natural_join3:
                self.assert_expr(z3.Implies(root.var == natural_join3.id,
                                            z3.And(root.children[0].var < root.children[1].var,
                                                   root.children[1].var < root.children[2].var,
                                                   (root.children[0].bitvec | root.children[1].bitvec | root.children[
                                                       2].bitvec) == root.bitvec)), f'natural_join3_bv_{i}')

            natural_join4 = self.spec.get_function_production('natural_join4')
            if natural_join4:
                self.assert_expr(z3.Implies(root.var == natural_join4.id,
                                            z3.And(root.children[0].var < root.children[1].var,
                                                   root.children[1].var < root.children[2].var,
                                                   root.children[2].var < root.children[3].var,
                                                   (root.children[0].bitvec | root.children[1].bitvec | root.children[2].bitvec |
                                                    root.children[3].bitvec) == root.bitvec)), f'natural_join4_bv_{i}')

            inner_join = self.spec.get_function_production('inner_join')
            if inner_join:
                self.assert_expr(z3.Implies(root.var == inner_join.id,
                                            z3.And((root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   (root.children[1].bitvec & root.children[2].bitvec2) == root.children[2].bitvec2,
                                                   (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec)),
                                 f'inner_join_bv_{i}')

            anti_join = self.spec.get_function_production('anti_join')
            if anti_join:
                self.assert_expr(z3.Implies(root.var == anti_join.id,
                                            z3.And((root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   (root.children[1].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   z3.Implies(root.children[2].bitvec == bv0,
                                                              (root.children[0].bitvec & root.children[1].bitvec) != bv0),
                                                   root.children[0].bitvec == root.bitvec)), f'anti_join_bv_{i}')

            left_join = self.spec.get_function_production('left_join')
            if left_join:
                self.assert_expr(z3.Implies(root.var == left_join.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) != bv0,
                                                   (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec)),
                                 f'left_join_bv_{i}')

            union = self.spec.get_function_production('union')
            if union:
                self.assert_expr(z3.Implies(root.var == union.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec),
                                 f'union_bv_{i}')

            intersect = self.spec.get_function_production('intersect')
            if intersect:
                self.assert_expr(z3.Implies(root.var == intersect.id,
                                            z3.And((root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   (root.children[1].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   root.children[2].bitvec == root.bitvec)), f'intersect_bv_{i}')

            semi_join = self.spec.get_function_production('semi_join')
            if semi_join:
                self.assert_expr(z3.Implies(root.var == semi_join.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) != bv0,
                                                   root.children[0].bitvec == root.bitvec)), f'semi_join_bv_{i}')

            cross_join = self.spec.get_function_production('cross_join')
            if cross_join:
                self.assert_expr(z3.Implies(root.var == cross_join.id,
                                            z3.And((root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   ((root.children[0].bitvec & root.children[1].bitvec) & root.children[2].bitvec2) ==
                                                   root.children[2].bitvec2,
                                                   (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec)),
                                 f'cross_join_bv_{i}')

            filter = self.spec.get_function_production('filter')
            if filter:
                self.assert_expr(z3.Implies(root.var == filter.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec,
                                                   root.children[0].bitvec == root.bitvec)), f'filter_bv_{i}')

            summarise = self.spec.get_function_production('summarise')
            if summarise:
                self.assert_expr(z3.Implies(root.var == summarise.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec,
                                                   (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   (root.children[1].bitvec2 & root.children[2].bitvec) == bv0,
                                                   (root.children[1].bitvec2 | root.children[2].bitvec) == root.bitvec)),
                                 f'summarise_bv_{i}')

            mutate = self.spec.get_function_production('mutate')
            if mutate:
                self.assert_expr(z3.Implies(root.var == mutate.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec,
                                                   (root.children[0].bitvec | root.children[1].bitvec2) == root.bitvec)), f'mutate_bv_{i}')

            unite = self.spec.get_function_production('unite')
            if unite:
                self.assert_expr(z3.Implies(root.var == unite.id,
                                            z3.And((root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec,
                                                   (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec,
                                                   root.children[0].bitvec == root.bitvec)), f'unite_bv_{i}')

    @staticmethod
    def _check_arg_types(pred: Predicate, python_tys: List[Type]):
        if pred.num_args() < len(python_tys):
            msg = 'Predicate "{}" must have at least {} arugments. Only {} is found.'.format(pred.name, len(python_tys), pred.num_args())
            raise ValueError(msg)
        for index, (arg, python_ty) in enumerate(zip(pred.args, python_tys)):
            if not isinstance(arg, python_ty):
                msg = 'Argument {} of predicate {} has unexpected type.'.format(index, pred.name)
                raise ValueError(msg)

    def _resolve_is_not_parent_predicate(self, pred: Predicate):
        if not util.get_config().is_not_parent_enabled:
            return

        self._check_arg_types(pred, [str, str])
        prod0 = self.spec.get_function_production_or_raise(pred.args[0])
        prod1 = self.spec.get_function_production_or_raise(pred.args[1])

        for r in self.roots:
            for s in range(len(r.children[0].lines)):
                children = []
                for c in r.children:
                    children.append(c.lines[s])
                self.assert_expr(z3.Implies(z3.And(z3.Or(children), self.roots[s].var == prod1.id), r.var != prod0.id), f'{prod0.id}_is_not_parent_of_{prod1.id}_r{r.id}_l{s}')

    def _resolve_distinct_inputs_predicate(self, pred: Predicate):
        self._check_arg_types(pred, [str])
        production = self.spec.get_function_production_or_raise(pred.args[0])
        for r in self.roots:
            ctr = []
            for c1 in range(len(r.children)):
                child1 = r.children[c1]
                for c2 in range(c1 + 1, len(r.children)):
                    child2 = r.children[c2]
                    # this works because even a inner_join between two filters, the children will have different values for the variables because of the lines produtions
                    ctr.append(z3.Or(child1.var != child2.var,
                                     z3.And(child1.var == 0, child2.var == 0)))

            self.assert_expr(z3.Implies(r.var == production.id, z3.And(ctr)), f'{production.id}_has_distinct_inputs_r{r.id}')

    def _resolve_distinct_filters_predicate(self, pred: Predicate):
        self._check_arg_types(pred, [str])
        prod0 = self.spec.get_function_production_or_raise(pred.args[0])
        for r in self.roots:
            self.z3_solver.add(z3.Implies(r.var == prod0.id, r.children[int(pred.args[1])].var != r.children[int(pred.args[2])].var))

    def _resolve_constant_occurs_predicate(self, pred: Predicate):
        conditions = pred.args
        lst = []
        ids = ''
        for c in conditions:
            for id in self._production_id_cache[c]:
                ids += f'{id}_'
                for l in self.leaves:
                    lst.append(l.var == id)
        self.assert_expr(z3.Or(lst), f'constant_occurs_{ids[:-1]}')

    def _resolve_happens_before_predicate(self, pred: Predicate):
        return

    def resolve_predicates(self) -> None:
        try:
            for pred in self.spec.predicates():
                if pred.name == 'is_not_parent':
                    self._resolve_is_not_parent_predicate(pred)
                elif pred.name == 'distinct_inputs':
                    self._resolve_distinct_inputs_predicate(pred)
                elif pred.name == 'constant_occurs':
                    self._resolve_constant_occurs_predicate(pred)
                elif pred.name == 'happens_before':
                    self._resolve_happens_before_predicate(pred)
                elif pred.name == 'distinct_filters':
                    self._resolve_distinct_filters_predicate(pred)
                else:
                    logger.warning('Predicate not handled: {}'.format(pred))
        except (KeyError, ValueError) as e:
            msg = 'Failed to resolve predicates. {}'.format(e)
            raise RuntimeError(msg) from None

    @cached_property
    def blocking_template(self) -> z3.ExprRef:
        ctr = []
        counter = 0
        for root in self.roots:
            ctr.append(root.var != z3.Var(counter, z3.IntSort()))
            counter += 1
            for child in root.children:
                ctr.append(child.var != z3.Var(counter, z3.IntSort()))
                counter += 1
        return z3.Or(ctr)

    def block_model(self, model: Dict[z3.ExprRef, Any]) -> None:
        values = []
        for root in self.roots:
            values.append(model[root.var])
            for child in root.children:
                values.append(model[child.var])
        self.z3_solver.add(z3.substitute_vars(self.blocking_template, *values))

    def line_uses_line(self, x, y):
        vars = list(flatten([set(child.lines) & set(self.line_vars_by_line[y + 1]) for child in self.roots[x].children]))
        return any(map(lambda var: self.model[var], vars))

    def lines_used_by(self, x):
        return [i for i, v in enumerate([self.line_uses_line(j, x) for j in range(len(self.roots))]) if v]

    @cached_property
    def monotonic_ids(self):
        return [self.spec.get_function_production(pn).id for pn in
                ['natural_join', 'natural_join3', 'natural_join4', 'mutate', 'filter', 'summarise', 'inner_join', 'left_join', 'union'] if
                self.spec.get_function_production(pn)]

    def all_recursive(self, model, i):
        tmp = list(map(lambda line: (line, model[self.roots[line].var].as_long() in self.monotonic_ids), self.lines_used_by(i)))
        if not all(map(lambda x: x[1], tmp)):
            return False
        for j, elem in tmp:
            if not self.all_recursive(model, j):
                return False
        return True

    def update(self, info: RejectionInfo = None):
        models = [{x: self.model[x] for x in self.variables}]

        if info and util.get_config().subsume_conditions:
            restriction_table = None
            if info.row_info == RowNumberInfo.MORE_ROWS:
                restriction_table = self.more_restrictive
            elif info.row_info == RowNumberInfo.LESS_ROWS:
                restriction_table = self.less_restrictive

            for model in islice(models, len(models)):
                for i, root in enumerate(self.roots):
                    if util.get_config().transitive_blocking and not self.all_recursive(models[0], i):
                        continue
                    elif not util.get_config().transitive_blocking and i > 0:
                        continue
                    for child in root.children:
                        for replacement in restriction_table[model[child.var].as_long()]:
                            new_model = model.copy()
                            new_model[child.var] = z3.IntVal(replacement)
                            models.append(new_model)

        # if info and util.get_config().block_commutative_ops:
        #     for root in self.roots:
        #         model = models[0]
        #         # for model in islice(models, len(models)):
        #         if self.natural_join and model[root.var].as_long() == self.natural_join.id:
        #             for c0, c1 in permutations(root.children[0:2]):
        #                 if c0 != root.children[0] or c1 != root.children[1]:
        #                     new_model = model.copy()
        #                     new_model[root.children[0].var] = model[c0.var]
        #                     new_model[root.children[1].var] = model[c1.var]
        #                     models.append(new_model)
        #         if self.natural_join3 and model[root.var].as_long() == self.natural_join3.id:
        #             for c0, c1, c2 in permutations(root.children[0:3]):
        #                 if c0 != root.children[0] or c1 != root.children[1] or c2 != root.children[2]:
        #                     new_model = model.copy()
        #                     new_model[root.children[0].var] = model[c0.var]
        #                     new_model[root.children[1].var] = model[c1.var]
        #                     new_model[root.children[2].var] = model[c2.var]
        #                     models.append(new_model)
        #         if self.natural_join4 and model[root.var].as_long() == self.natural_join4.id:
        #             for c0, c1, c2, c3 in permutations(root.children[0:4]):
        #                 if c0 != root.children[0] or c1 != root.children[1] or c2 != root.children[2] or c3 != root.children[3]:
        #                     new_model = model.copy()
        #                     new_model[root.children[0].var] = model[c0.var]
        #                     new_model[root.children[1].var] = model[c1.var]
        #                     new_model[root.children[2].var] = model[c2.var]
        #                     new_model[root.children[3].var] = model[c3.var]
        #                     models.append(new_model)

        # if len(models) > 1:
        #     logger.warning('Blocked %d programs!', len(models) - 1)
        #     logger.warning('Due to %s:', self.construct_program(models[0]))
        for model in models:
            # if len(models) > 1:
            #     logger.warning('Blocking %s', self.construct_program(model))
            self.block_model(model)

        if info and info.score != 0:
            util.get_program_queue().put((util.Message.EVAL_INFO,
                                          tuple(line.production.name for line in self.current_program),
                                          info.score * len(models)))
        return len(models) - 1

    def construct_program(self, model) -> Program:
        prog = [program.Line(self.get_production(model[root.var].as_long()),
                             tuple(self.get_production(model[child.var].as_long()) for child in root.children)) for root in
                self.roots]
        self.current_program = prog
        return prog

    def next(self) -> Optional[Program]:
        res = self.z3_solver.check()

        if res == z3.unsat:
            self.unsat_core = self.z3_solver.unsat_core()
            return None
        elif res == z3.unknown:
            logger.error('Z3 failed to produce an answer: %s', self.z3_solver.reason_unknown())
            raise RuntimeError()

        self.model = self.z3_solver.model()

        if self.model is not None:
            return self.construct_program(self.model)
        else:
            return None

    def mk_bitvec(self, bitvec_val: int) -> z3.BitVecNumRef:
        if bitvec_val not in self.bitvec_cache:
            self.bitvec_cache[bitvec_val] = z3.BitVecVal(bitvec_val, self.specification.n_columns)
        return self.bitvec_cache[bitvec_val]
