import time
from collections import defaultdict
from logging import getLogger

from ordered_set import OrderedSet
from z3 import *

from .enumerator import Enumerator
from .gen_lattices import SymmetryFinder
from ..spec import TyrellSpec
from ..spec.production import LineProduction
from ... import util, program
from ...dsl.specification import Specification

logger = getLogger('tyrell.enumerator.smt')


class Node(object):
    def __init__(self, nb=None):
        self.nb = nb
        self.var = None
        self.children = None
        self.h = None
        self.bitvec = None
        self.bitvec2 = None

    def __repr__(self) -> str:
        return f'Node({self.nb})'


class Root(Node):
    def __init__(self, id=None, nb=None, depth=None, children=None, type=None):
        super().__init__(nb)
        self.id = id
        self.depth = depth  # num of children
        self.children = children
        self.type = type

    def __repr__(self) -> str:
        return 'Root({}, children={})'.format(self.id, len(self.children))


class Leaf(Node):
    def __init__(self, nb=None, parent=None, lines=None):
        super().__init__(nb)
        self.parent = parent  # parent id
        self.lines = lines

    def __repr__(self) -> str:
        return 'Leaf({}, parent={})'.format(self.nb, self.parent)


class BitEnumerator(Enumerator):

    def __init__(self, tyrell_spec: TyrellSpec, spec: Specification, loc=None, debug=True):

        if util.get_config().z3_QF_FD:
            # self.z3_solver = OrElse('qffd', 'smt').solver()  # TODO SMTFD might be better when it eventually gets released
            self.z3_solver = SolverFor('QF_FD')  # TODO SMTFD might be better when it eventually gets released
            # self.z3_solver = Solver()  # TODO SMTFD might be better when it eventually gets released

        else:
            self.z3_solver = Solver()
            # self.z3_solver.set('phase_selection', util.get_config().z3_smt_phase)
            # self.z3_solver.set('case_split', util.get_config().z3_smt_case_split)

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

        self.bivector_length = spec.n_columns

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
        self.find_types()
        self.create_leaf_enum()
        # self.init_leaf_productions()
        # self.init_line_productions()
        self.linesVars = []
        self.typeVars = []
        self.line_vars_by_line = defaultdict(list)
        self.roots, self.leafs = self.build_trees()
        self.model = None
        # Times
        self.symTime = 0
        self.totalSymTime = 0
        self.solverTime = 0
        self.blockedModels = 0
        self.totalBlockedModels = 0

        self.modelConstraint = 0
        if util.get_config().lines_force_all_inputs:
            self.create_input_constraints()
        self.create_output_constraints()
        self.create_lines_constraints()
        self.create_type_constraints()
        self.create_children_constraints()
        self._production_id_cache = defaultdict(OrderedSet)
        for p in self.spec.productions():
            if p.is_enum():
                self._production_id_cache[p._get_rhs()].append(p.id)
        self._production_id_cache.default_factory = lambda: None
        self.resolve_predicates()
        self.constraint_functions()
        logger.info('Number of Nodes: {} '.format(len(self.roots + self.leafs)))
        logger.info('Number of Variables: {}'.format(len(self.variables + self.typeVars + self.linesVars)))
        logger.info('Number of Constraints: {}'.format(self.num_constraints))

        with open(f'formula_{loc}.smt', 'w') as f:
            f.write(self.z3_solver.sexpr())

        res = self.z3_solver.check()
        if res != sat:
            logger.warning(f"There is no solution for current loc ({self.loc}).")
        else:
            self.model = self.z3_solver.model()

    def assert_expr(self, expr, name):
        if self.debug:
            self.z3_solver.assert_and_track(expr, name)
        else:
            self.z3_solver.add(expr)

    def find_types(self):
        types = []
        for t in self.spec.types():
            for p in self.spec.productions():
                if p.is_function() and p.lhs.name != 'Empty' and p.lhs == t:
                    types.append(t.name)
                    break
        self.types = types
        self.num_types = len(self.types)

    def create_leaf_enum(self):
        productions = []

        true_leafs_number = 0
        for p in self.spec.productions():
            if not p.is_function() or p.lhs.name == 'Empty':  # FIXME: improve empty integration
                true_leafs_number += 1
                productions.append(f'{p.id}')

        for l in range(0, self.loc - 1):
            line_productions = []
            for t in self.types:
                self.num_prods += 1
                line_production = LineProduction(self.num_prods, self.spec.get_type(t), l)
                self.line_productions_by_id[self.num_prods] = line_production
                productions.append(f'{self.num_prods}')
                line_productions.append(line_production)
            self.line_productions.append(line_productions)

        sort, values = EnumSort('Leaf', productions)
        self.leaf_enum = sort
        self.leaf_enum_values = {int(value.decl().name()): value for value in values}
        self.leaf_enum_refs = {value: int(value.decl().name()) for value in values}

    def build_trees(self):
        """Builds a loc trees, each tree will be a line of the program"""
        nodes = []
        nb = 1
        leafs = []
        for i in range(1, self.loc + 1):
            n = Root(i, nb, self.max_children)
            n.var = self.create_root_variables(nb)
            children = []
            for x in range(self.max_children):
                nb += 1
                child = Leaf(nb, n)
                child.lines = self.create_lines_variables(nb, n.id)
                child.var = self.create_leaf_variables(nb, n.id)
                child.bitvec = BitVec(f'bv_{nb}_{n.id}', self.specification.n_columns)
                child.bitvec2 = BitVec(f'bv_{nb}_{n.id}_', self.specification.n_columns)
                children.append(child)
                leafs.append(child)
            n.children = children
            n.type = self.create_type_variables(n.id)
            n.bitvec = BitVec(f'bv_{nb}', self.specification.n_columns)
            nodes.append(n)
            nb += 1
        return nodes, leafs

    def create_lines_variables(self, nb, parent):
        lines = []
        for x in range(1, parent):
            name = f'line_{nb}_{x}'
            var = Int(name)
            self.linesVars.append(var)
            self.line_vars_by_line[x].append(var)
            lines.append(var)
            # variable range constraints
            self.assert_expr(Or(var == 0, var == 1), f'line_{nb}_{x}_domain')
            self.num_constraints += 1
        return lines

    def create_type_variables(self, nb):
        var = Int(f'type_{nb}')
        # variable range constraints
        self.typeVars.append(var)
        self.assert_expr(And(var >= 0, var < self.num_types), f'type_{nb}_domain')
        self.num_constraints += 1
        return var

    def create_root_variables(self, nb):
        v = Int(f'root_{nb}')
        self.variables.append(v)
        ctr = []
        for p in self.spec.productions():
            if p.is_function() and p.name != 'Empty':
                ctr.append(v == p.id)
        self.assert_expr(Or(ctr), f'root_{nb}_domain')
        self.num_constraints += 1
        return v

    def create_leaf_variables(self, nb, parent):
        var = Const(f'leaf_{nb}', self.leaf_enum)
        self.variables.append(var)

        ctr = []
        for a in range(parent, self.loc - 1):
            for p in self.line_productions[a]:
                ctr.append(var != self.leaf_enum_values[p.id])

        self.assert_expr(And(ctr), f'leaf_{nb}_domain')
        self.num_constraints += 1
        return var

    def create_output_constraints(self):
        """The output production matches the output type"""
        ctr = []
        var = self.roots[-1].var  # last line corresponds to the output line
        for p in self.spec.get_productions_with_lhs(self.spec.output):
            ctr.append(var == p.id)
        self.assert_expr(Or(ctr), 'output_has_correct_type')
        self.assert_expr(AtLeast(*(Extract(i, i, self.roots[-1].bitvec) == BitVecVal(1, 1) for i in range(self.specification.n_columns)),
                                 len(self.specification.output_cols)),
                         'output_has_at_least_k_columns')
        self.z3_solver.add()
        self.num_constraints += 1

    def create_lines_constraints(self):
        """Each line is used at least once in the program"""
        for r in range(1, len(self.roots)):
            ctr = None
            for line_var in self.line_vars_by_line[r]:
                if ctr is None:
                    ctr = line_var
                else:
                    ctr += line_var

            self.z3_solver.add(ctr >= 1)
            self.num_constraints += 1

    def create_input_constraints(self):
        """Each input will appear at least once in the program"""
        input_productions = self.spec.get_param_productions()
        for prod in input_productions:
            ctr = []
            for y in self.leafs:
                ctr.append(y.var == self.leaf_enum_values[prod.id])
            self.z3_solver.add(Or(ctr))
            self.num_constraints += 1

    def create_type_constraints(self):
        """If a production is used in a node, then the nodes' type is equal to the production's type"""
        for r in self.roots:
            for t in range(len(self.types)):  # todo one of the fors can be removed
                if self.types[t] == 'Empty':
                    continue
                for p in self.spec.productions():
                    if p.is_function() and p.lhs.name == self.types[t]:
                        self.z3_solver.add(Implies(r.var == p.id, r.type == t))
                        self.num_constraints += 1

    def create_children_constraints(self):
        for r in self.roots:
            for p in self.spec.productions():
                if not p.is_function() or p.lhs.name == 'Empty':
                    continue
                aux = r.var == p.id
                for c in range(len(r.children)):
                    ctr = []
                    if c >= len(p.rhs):
                        self.num_constraints += 1
                        self.z3_solver.add(Implies(aux, And(r.children[c].var == self.leaf_enum_values[0],
                                                            r.children[c].bitvec == self.mk_bitvec(0),
                                                            r.children[c].bitvec2 == self.mk_bitvec(0))))
                        continue

                    for leaf_p in self.spec.productions():
                        if not leaf_p.is_function() or leaf_p.lhs.name == 'Empty':
                            if leaf_p.lhs.name == p.rhs[c].name:
                                if not isinstance(leaf_p.value, tuple):
                                    ctr.append(And(r.children[c].var == self.leaf_enum_values[leaf_p.id],
                                                   r.children[c].bitvec == self.mk_bitvec(leaf_p.value),
                                                   r.children[c].bitvec2 == self.mk_bitvec(0)))
                                else:
                                    ctr.append(And(r.children[c].var == self.leaf_enum_values[leaf_p.id],
                                                   r.children[c].bitvec == self.mk_bitvec(leaf_p.value[0]),
                                                   r.children[c].bitvec2 == self.mk_bitvec(leaf_p.value[1])))

                    for l in range(r.id - 1):
                        for line_production in self.line_productions[l]:
                            if line_production.lhs.name == p.rhs[c].name:
                                ctr.append(And(r.children[c].var == self.leaf_enum_values[line_production.id],
                                               r.children[c].bitvec == self.roots[l].bitvec,
                                               r.children[c].bitvec2 == self.mk_bitvec(0)))
                                # if a previous line is used, then its flag must be true
                                line_var = r.children[c].lines[l]
                                self.z3_solver.add(Implies(line_var == 1, r.children[c].var == self.leaf_enum_values[line_production.id]))
                                self.z3_solver.add(Implies(r.children[c].var == self.leaf_enum_values[line_production.id], line_var == 1))
                                self.num_constraints += 2

                    self.num_constraints += 1
                    self.z3_solver.add(Implies(aux, Or(ctr)))

    @staticmethod
    def _check_arg_types(pred, python_tys):
        if pred.num_args() < len(python_tys):
            msg = 'Predicate "{}" must have at least {} arugments. Only {} is found.'.format(pred.name, len(python_tys), pred.num_args())
            raise ValueError(msg)
        for index, (arg, python_ty) in enumerate(zip(pred.args, python_tys)):
            if not isinstance(arg, python_ty):
                msg = 'Argument {} of predicate {} has unexpected type.'.format(index, pred.name)
                raise ValueError(msg)

    def _resolve_is_not_parent_predicate(self, pred):
        if not util.get_config().is_not_parent_enabled:
            return

        self._check_arg_types(pred, [str, str])
        prod0 = self.spec.get_function_production_or_raise(pred.args[0])
        prod1 = self.spec.get_function_production_or_raise(pred.args[1])

        for r in self.roots:
            for s in range(len(r.children[0].lines)):
                children = []
                for c in r.children:
                    children.append(c.lines[s] == 1)
                self.z3_solver.add(Implies(And(Or(children), self.roots[s].var == prod1.id), r.var != prod0.id))

    def _resolve_distinct_inputs_predicate(self, pred):
        self._check_arg_types(pred, [str])
        production = self.spec.get_function_production_or_raise(pred.args[0])
        for r in self.roots:
            for c1 in range(len(r.children)):
                child1 = r.children[c1]
                for c2 in range(c1 + 1, len(r.children)):
                    child2 = r.children[c2]
                    # this works because even a inner_join between two filters, the children will have different values for the variables because of the lines produtions
                    self.z3_solver.add(Implies(r.var == production.id, Or(child1.var != child2.var,
                                                                          And(child1.var == self.leaf_enum_values[0],
                                                                              child2.var == self.leaf_enum_values[0]))))

    def _resolve_distinct_filters_predicate(self, pred):
        self._check_arg_types(pred, [str])
        prod0 = self.spec.get_function_production_or_raise(pred.args[0])
        for r in self.roots:
            self.z3_solver.add(Implies(r.var == prod0.id, r.children[int(pred.args[1])].var != r.children[int(pred.args[2])].var))

    def _resolve_constant_occurs_predicate(self, pred):
        conditions = pred.args
        lst = []
        for c in conditions:
            for id in self._production_id_cache[c]:
                for l in self.leafs:
                    lst.append(l.var == self.leaf_enum_values[id])
        self.z3_solver.add(Or(lst))

    def _resolve_happens_before_predicate(self, pred):
        return

    def resolve_predicates(self):
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

    def block_model_aux(self, model):
        self.z3_solver.add(Or(*(x != model[x] for x in self.variables)))

    def block_model(self):
        assert (self.model is not None)
        # in order to find symmetric programs
        self.block_model_aux(self.model)

    def update(self, info=None, id=None):
        if info is not None and not isinstance(info, str):
            for core in info:
                ctr = []
                for constraint in core:
                    ctr.append(self.program2tree[constraint[0]] != constraint[1].id)
                self.z3_solver.add(Or(ctr))
        else:
            self.blockedModels = 0
            self.block_model()
            self.totalBlockedModels += self.blockedModels
            if self.blockedModels != 0:
                logger.error('Total Blocked Models: {}'.format(self.totalBlockedModels))
                logger.error('Total Time Symmetries: {}'.format(self.totalSymTime))

    def get_production(self, prod_id):
        if prod_id in self.line_productions_by_id:
            return self.line_productions_by_id[prod_id]
        else:
            return self.spec.get_production(prod_id)

    def construct_program(self) -> program.Program:
        lines = []
        for r in self.roots:
            lines.append(
                program.Line(self.get_production(self.model[r.var].as_long()),
                             tuple(self.get_production(self.leaf_enum_refs[self.model[child.var]]) for child in r.children)))
        return lines

    def print_bitvec_prog(self):
        for i, r in enumerate(self.roots):
            if self.model[r.bitvec] is not None:
                print(i, format(self.model[r.bitvec].as_long(), f'0{self.specification.n_columns}b'), '=', end='(')
                for child in r.children:
                    print('(', format(self.model[child.bitvec].as_long(), f'0{self.specification.n_columns}b'), ',',
                          format(self.model[child.bitvec2].as_long(), f'0{self.specification.n_columns}b'), ')', end=';')
                print()

    def next(self):
        start_time = time.time()
        res = self.z3_solver.check()

        self.solverTime += time.time() - start_time
        if res != sat:
            self.unsat_core = self.z3_solver.unsat_core()
            # print(self.unsat_core)
            return None

        self.model = self.z3_solver.model()

        if self.model is not None:
            return self.construct_program()
        else:
            return None

    # @formatter:off
    def constraint_functions(self):
        bv0 = BitVecVal(0, self.specification.n_columns)

        ctr = []
        for root in self.roots:
            natural_join = self.spec.get_function_production('natural_join')
            if natural_join:
                ctr.append(Implies(root.var == natural_join.id, (root.children[0].bitvec & root.children[1].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec))

            natural_join3 = self.spec.get_function_production('natural_join3')
            if natural_join3:
                ctr.append(Implies(root.var == natural_join3.id, (root.children[0].bitvec & root.children[1].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join3.id, ((root.children[0].bitvec | root.children[1].bitvec) & root.children[2].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join3.id, (root.children[0].bitvec | root.children[1].bitvec | root.children[2].bitvec) == root.bitvec))

            natural_join4 = self.spec.get_function_production('natural_join4')
            if natural_join4:
                ctr.append(Implies(root.var == natural_join4.id, (root.children[0].bitvec & root.children[1].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join4.id, ((root.children[0].bitvec | root.children[1].bitvec) & root.children[2].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join4.id, ((root.children[0].bitvec | root.children[1].bitvec | root.children[2].bitvec) & root.children[3].bitvec) != bv0))
                ctr.append(Implies(root.var == natural_join4.id, (root.children[0].bitvec | root.children[1].bitvec | root.children[2].bitvec | root.children[3].bitvec) == root.bitvec))

            inner_join = self.spec.get_function_production('inner_join')
            if inner_join:
                ctr.append(Implies(root.var == inner_join.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == inner_join.id, (root.children[1].bitvec & root.children[2].bitvec2) == root.children[2].bitvec2))
                ctr.append(Implies(root.var == inner_join.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec))

            anti_join = self.spec.get_function_production('anti_join')
            if anti_join:
                ctr.append(Implies(root.var == anti_join.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == anti_join.id, (root.children[1].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == anti_join.id, Implies(root.children[2].bitvec == bv0, (root.children[0].bitvec & root.children[1].bitvec) != bv0)))
                ctr.append(Implies(root.var == anti_join.id, root.children[0].bitvec == root.bitvec))

            left_join = self.spec.get_function_production('left_join')
            if left_join:
                ctr.append(Implies(root.var == left_join.id, (root.children[0].bitvec & root.children[1].bitvec) != bv0))
                ctr.append(Implies(root.var == left_join.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec))

            union = self.spec.get_function_production('union')
            if union:
                ctr.append(Implies(root.var == union.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec))

            intersect = self.spec.get_function_production('intersect')
            if intersect:
                ctr.append(Implies(root.var == intersect.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == intersect.id, (root.children[1].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == intersect.id, root.children[2].bitvec == root.bitvec))

            semi_join = self.spec.get_function_production('semi_join')
            if semi_join:
                ctr.append(Implies(root.var == semi_join.id, (root.children[0].bitvec & root.children[1].bitvec) != bv0))
                ctr.append(Implies(root.var == semi_join.id, root.children[0].bitvec == root.bitvec))

            cross_join = self.spec.get_function_production('cross_join')
            if cross_join:
                ctr.append(Implies(root.var == cross_join.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == cross_join.id, ((root.children[0].bitvec & root.children[1].bitvec) & root.children[2].bitvec2) == root.children[2].bitvec2))
                ctr.append(Implies(root.var == cross_join.id, (root.children[0].bitvec | root.children[1].bitvec) == root.bitvec))

            filter = self.spec.get_function_production('filter')
            if filter:
                ctr.append(Implies(root.var == filter.id, (root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec))
                ctr.append(Implies(root.var == filter.id, root.children[0].bitvec == root.bitvec))

            summarise = self.spec.get_function_production('summarise')
            if summarise:
                ctr.append(Implies(root.var == summarise.id, (root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec))
                ctr.append(Implies(root.var == summarise.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == summarise.id, (root.children[1].bitvec2 & root.children[2].bitvec) == bv0))
                ctr.append(Implies(root.var == summarise.id, (root.children[1].bitvec2 | root.children[2].bitvec) == root.bitvec))

            mutate = self.spec.get_function_production('mutate')
            if mutate:
                ctr.append(Implies(root.var == mutate.id, (root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec))
                ctr.append(Implies(root.var == mutate.id, (root.children[0].bitvec | root.children[1].bitvec2) == root.bitvec))

            unite = self.spec.get_function_production('unite')
            if unite:
                ctr.append(Implies(root.var == unite.id, (root.children[0].bitvec & root.children[1].bitvec) == root.children[1].bitvec))
                ctr.append(Implies(root.var == unite.id, (root.children[0].bitvec & root.children[2].bitvec) == root.children[2].bitvec))
                ctr.append(Implies(root.var == unite.id, root.children[0].bitvec == root.bitvec))

        self.z3_solver.add(And(*ctr))

    def mk_bitvec(self, bitvec_val):
        if bitvec_val not in self.bitvec_cache:
            self.bitvec_cache[bitvec_val] = BitVecVal(bitvec_val, self.specification.n_columns)
        return self.bitvec_cache[bitvec_val]
