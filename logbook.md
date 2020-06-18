##Week 1

###Configuration attempts

####try1
    Config(seed=seed, disabled=['inner_join', semi_join']),
    Config(seed=seed, disabled=['inner_join', semi_join'], alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['inner_join', semi_join'], alt_empty_pos=False, shuffle_cols=True),
    Config(seed=seed + 1, disabled=['inner_join', semi_join'], alt_empty_pos=True, shuffle_cols=True),

####try2
    Config(seed=seed, disabled=['inner_join', 'semi_join'], filters_function_enabled=True, max_filter_combinations=1),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

####try3
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='random'),

####try4:
    Config(seed=seed, disabled=['inner_join', 'semi_join'], max_filter_combinations=1, filters_function_enabled=True),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='always_false'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='always_true'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

####try5:
    Config(seed=seed, disabled=['inner_join', 'semi_join'], max_filter_combinations=1, filters_function_enabled=True),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_smt_phase=6),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

##Week 2

###Parameters
- pb.solver: no effect at all (no PB clauses?)
- cardinality.solver: no effect at all (no card clauses?)
- unit_walk: much slower, can't enumerate programs with 3 lines

###Interesting instances
- 55-tests/54: susceptible to bind_rows
- 55-tests/12
- 55-tests/10
- 55-tests/14
- 55-tests/5

###Subject to non-determinism in original squares
- 55-tests/10

###Configuration attempts

####try6
    Config(seed=seed, disabled=['inner_join', 'semi_join'], filters_function_enabled=True, max_filter_combinations=1),
    Config(seed=seed, disabled=['inner_join', 'natural_join4'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'natural_join3'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join', 'natural_join4', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
##Week 3


###Reasons why instances fail
- Dates are not supported
- concat as an aggregation function (!= from concatenating two different columns, ie, unite)
- first as an aggregation function?? (only supported by some DBMS)
- needs INNER JOIN ON instead of NATURAL JOIN
- a column that is a max of 'something' must be called 'maxsomething' in the io example


###Trying to remove aggrs requirement
- Removed redundancy from filter conditions (eg: A==B and B==A, A<B and B\>A)
- Problem: SQUARES forced the generated filter conditions to appear on the program. Since we cannot do that anymore synthesis becomes much much much slower. Use multiple threads as a workaround?
  - eg: 55-tests/2 takes ages to go through 3 line programs, while forcing the constants makes it so that no 3 line programs are generated

###Interesting instances
- 55-tests/40

###Misc.
- Found out why running a configuration similar to original squares gave different results. The second Z3 solver (used for conflict analysis) was not being seeded.
  - However there is still some variation in the results...

###Configuration attempts
    
####try7 - first try without knowing aggrs
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=[], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["min"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["mean"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max(n)"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
## Week 4
###Reasons why instances fail
- Extracting parts of the date
- mutate (eg. SELECT sqrt(a-b) AS c) is needed sometimes (eg. cumulative sums)
- renaming columns (some very simple cases are already supported by using the new inner join)

###New things
- Inner join is finished
- Dates are now supported (kind of? too many formats...)
- concat as an aggregation function is now supported
- 'max(n)' is no longer a special case with lots of exceptions. Most R expressions can be used as filter conditions
- Removed redundant group by conditions (a, b) and (b, a)
- Removed redundant filter conditions (eg. n > 5 | n >= 5)   \[might be slightly slower for simple programs?\]

###Different solutions
- 55-tests/2
- 55-tests/4

###Configuration attempts
From this point onward the new filter, summarise and join condition code is used.

####try8 - knows args
    Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, disabled=['semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='random'),
    
## Week 5

### Notes
- Starting one of the processes with higher loc helps solve most very hard instances. However, that makes it so that we no longer find the optimal solution to problems.

### Configuration attempts

#### try9 - knows args
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'semi_join']),
    # original squares
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join3'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True,
           disabled=['inner_join', 'semi_join', 'natural_join4', 'anti_join', 'left_join', 'bind_rows',
                     'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True,
           disabled=['inner_join', 'semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random', max_column_combinations=1, max_filter_combinations=1,
           starting_loc=6),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random', max_column_combinations=1, starting_loc=5),
    Config(seed=seed, ignore_aggrs=False, disabled=['semi_join'], force_summarise=True, z3_QF_FD=True,
           z3_sat_phase='random'),
           
## Week 6

###New
- Added very basic space splitting. Using the first n-1 lines and only fixing the function (not the arguments)

### Configuration attempts

#### cubes1
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random', is_not_parent_enabled=False)
    
## Week 8

### 55-tests/22
- last line: 41m56 (30 processes)
- 2 last lines: 2m40 (30 processes)

### 55-tests/43
- last line: 1m20 (just 5 lines, weird solution) (30 processes)

### 55-tests/46
- last line: 2h+ timeout (30 processes)

## Week 9

### 55-tests/22
Stats after 300s (16 threads):

    [  299.2467][main][info] Statistics:
    [  299.2467][main][info]        Generated cubes: 224
    [  299.2467][main][info]        Attempted programs: 187316
    [  299.2467][main][info]                Rejected: 18104
    [  299.2467][main][info]                Failed: 169212
    [  299.2467][main][info]        Blacklist clauses: 62
    [  299.2469][main][info] Priting statistics for good programs of size 4
    [  299.2484][main][info]        0: Counter({'summariseGrouped': 1624, 'filter': 232, 'natural_join': 16, 'left_join': 16, 'bind_rows': 16})
    [  299.2500][main][info]        1: Counter({'natural_join3': 1344, 'filter': 208, 'summariseGrouped': 208, 'bind_rows': 112, 'left_join': 16, 'natural_join': 16})
    [  299.2506][main][info]        2: Counter({'filter': 1464, 'natural_join3': 288, 'summariseGrouped': 72, 'bind_rows': 48, 'natural_join': 16, 'left_join': 16})
    [  299.2512][main][info]        3: Counter({'select': 1904})
    [  299.2512][main][info] Priting statistics for good programs of size 5
    [  299.2579][main][info]        0: Counter({'natural_join': 15714, 'summariseGrouped': 4309})
    [  299.2627][main][info]        1: Counter({'summariseGrouped': 12986, 'natural_join': 4091, 'filter': 2568, 'natural_join3': 218, 'left_join': 64, 'bind_rows': 64, 'semi_join': 32})
    [  299.2670][main][info]        2: Counter({'filter': 5189, 'natural_join3': 4624, 'left_join': 3274, 'bind_rows': 2571, 'summariseGrouped': 2402, 'natural_join': 1664, 'anti_join': 192, 'semi_join': 107})
    [  299.2710][main][info]        3: Counter({'filter': 12320, 'natural_join3': 3711, 'natural_join': 1208, 'bind_rows': 1061, 'left_join': 977, 'summariseGrouped': 328, 'semi_join': 210, 'anti_join': 208})
    [  299.2747][main][info]        4: Counter({'select': 20023})
    No solution found

### 55-tests/46
Stats after 300s (16 threads):

    [  299.1345][main][info] Statistics:
    [  299.1345][main][info]        Generated cubes: 72
    [  299.1345][main][info]        Attempted programs: 0
    [  299.1345][main][info]                Rejected: 0
    [  299.1345][main][info]                Failed: 0
    [  299.1345][main][info]        Blacklist clauses: 86
    [  299.1348][main][info] Priting statistics for good programs of size 4
    [  299.1365][main][info]        0: Counter({'summariseGrouped': 1616, 'natural_join3': 907})
    [  299.1376][main][info]        1: Counter({'natural_join3': 1081, 'summariseGrouped': 907, 'filter': 535})
    [  299.1383][main][info]        2: Counter({'filter': 1988, 'natural_join4': 451, 'natural_join3': 84})
    [  299.1390][main][info]        3: Counter({'select': 2523})
    [  299.1391][main][info] Priting statistics for good programs of size 5
    [  299.1409][main][info]        0: Counter({'natural_join': 7995})
    [  299.1426][main][info]        1: Counter({'summariseGrouped': 7995})
    [  299.1443][main][info]        2: Counter({'filter': 2754, 'natural_join3': 1811, 'left_join': 1392, 'natural_join': 1300, 'anti_join': 738})
    [  299.1460][main][info]        3: Counter({'filter': 5241, 'bind_rows': 916, 'natural_join4': 880, 'natural_join': 291, 'left_join': 263, 'natural_join3': 201, 'semi_join': 123, 'anti_join': 80})
    [  299.1477][main][info]        4: Counter({'select': 7995})
    No solution found
    
## Week 10

### Unsupported
- Union with selects (scythe/recent_posts/001)
- Complex join (scythe/recent_posts/013, scythe/recent_posts/036, scythe/recent_posts/048, leetcode/181)
- Left join with const (scythe/recent_posts/021)
- Top n (scythe/recent_posts/022, leetcode/185)
- Float comparison? (scythe/recent_posts/24)
- Mutate / summarise with mutate (scythe/recent_posts/24, scythe/recent_posts/33, scythe/recent_posts/35, leetcode/262)
- Mutate (scythe/recent_posts/26, leetcode/178)
- Date part + date arithmetic (scythe/recent_posts/27)
- Join with different names (scythe/recent_posts/28)
- full outer join? (scythe/recent_posts/029)
- self filter ?? (scythe/recent_posts/043)

#### Unknown reason
- scythe/recent_posts/007
- scythe/recent_posts/023
- scythe/recent_posts/037 (evil)

### Requires inner_join
- scythe/recent_posts/003
- others

### Should sort
- scythe/recent_posts/003
- scythe/recent_posts/014
- scythe/recent_posts/028
- scythe/recent_posts/032
- scythe/recent_posts/042
- scythe/recent_posts/048
- leetcode/178

### Pivots
- scythe/recent_posts/008
- scythe/recent_posts/015

### Hard but possibly solvable
- scythe/recent_posts/046

## Week 11

### Possible extension
- union distinct

## Week 12

## Week something??
https://github.com/Z3Prover/z3/issues/1044
>We don't expose ways to print internal state. You could perhaps interrupt the solver, then clone it using the "translate" methods and access the translated solver state using internal print utilities. You would have to change the code a bit to get to this state.
 The print features on solvers don't access the internal state of any of the solvers, instead they look at the asserted formulas and print them.
 I don't translate learned lemmas. For example, the code in smt_context.cpp line 176 is disabled because it didn't help with any performance enhancements. Similarly, the copy code in sat_solver does not copy learned clauses even though it retains unit literals and binary clauses that are learned.

Open issue: https://github.com/Z3Prover/z3/issues/2095
>I take there are two parts to your post:
>
>1. Expose hooks to make saving state usable.
>2. The format and fidelity of the state that is saved.
>
>Regarding 1, I wonder if adding a configuration option to
 save state to a file if a solver is interrupted will be a way
 to go.
>
>Regarding 2, much of what is below is also discussed in the earlier
>post, #1044. A few things changed since
>
>So far z3 can persist state by serializing solver state to smtlib or dimacs.
>Text files in this format are much easier to work with for debugging.
>This isn't wrapped in a way that is as usable as you suggest (with commands).
>You are also bound to lose lemmas.
>
>For the SAT solver I include lemmas of low glue level
>and have configuration flags to control this. The SAT
>solver serializes to either dimacs or SMT-LIB. Using SMT-LIB
>has the advantage that the format includes model transformations
>so that a solution to the saved formula can be translated to
>a solution to the original formula.
>
>I use this serialization for distributed parallelization and
>I think the text file-based approach is preferrable for debuggability
>and portability.
>
>Saving state that is produced from SMT core is much cruder
>(= limited to asserted formulas after pre-processing, excluding lemmas)
>and other kernels (nlsat) simply non-existent. It possibly could be
>improved if there is a clear use case, such as yours.
>This would be for text-based state snapshots.
>Since the text based formats dont' expose a notion of redundant
>clauses they would be limited to learned clauses with low glue level,
>which is not much of a restriction (because it amounts to running
>agressive GC).
>
>A caveat with the text-based format is that it doesn't handle
>internal identifiers created during pre-processing correctly:
>you have to rename them (e.g., rename k!10 to k!_10) before
>using them again so to avoid that a new internal identifier k!10
>gets created.
>
>A binary format would be tougher to get right, have to address all
>questions as the textual formats, but potentially be more flexible
>and could address the internal identifier issue systematically.
>Some of this was engineered around 2010 for MPI, by @cwintersteiger,
>but subsequently bit-rot.
>Features that would have to be serializable:
>
>model and proof transformers
>smt-context state
>sat solver state
>maybe nlsat, qsat, other solver states
>ast-manager state
>An OS-based approach, with native snapshots would be nicer to
>administrate. This would be outside of Z3.

Nikolaj on StackOverflow:
>Yes, there are essentially two incremental modes.
>
>Stack based: using push(), pop() you create a local context, that follows a stack discipline. Assertions added under a push() are removed after a matching pop(). Furthermore, any lemmas that are derived under a push are removed. Use push()/pop() to emulate freezing a state and adding additional constraints over the frozen state, then resume to the frozen state. It has the advantage that any additional memory overhead (such as learned lemmas) built up within the scope of a push() is released. The working assumption is that learned lemmas under a push would not be useful any longer.
>
>Assumption based: using additional assumption literals passed to check()/check_sat() you can (1) extract unsatisfiable cores over the assumption literals, (2) gain local incrementality without garbage collecting lemmas that get derived independently of the assumptions. In other words, if Z3 learns a lemma that does not contain any of the assumption literals it expects to not garbage collect them. To use assumption literals effectively, you would have to add them to formulas too. So the tradeoff is that clauses used with assumptions contain some amount of bloat. For example if you want to locally assume some formula (<= x y), then you add a clause (=> p (<= x y)), and assume p when calling check_sat(). Note that the original assumption was a unit. Z3 propagates units efficiently. With the formulation that uses assumption literals it is no longer a unit at the base level of search. This incurs some extra overhead. Units become binary clauses, binary clauses become ternary clauses, etc.
>
>The differentiation between push/pop functionality holds for Z3's default SMT engine. This is the engine most formulas will be using. Z3 contains some portfolio of engines. For example, for pure bit-vector problems, Z3 may end up using the sat based engine. Incrementality in the sat based engine is implemented differently from the default engine. Here incrementality is implemented using assumption literals. Any assertion you add within the scope of a push is asserted as an implication (=> scope_literals formula). check_sat() within such a scope will have to deal with assumption literals. On the flip-side, any consequence (lemma) that does not depend on the current scope is not garbage collected on pop().

## Week of 10/6

#### c27
- Added blocking of programs based on condition subsumption of Cols, FilterConditions and CrossJoinConditions

#### c28
- Multiply scores by number of programs solved
- Re-enabled h-splitting
- Optimized cube blocking

#### c29
- split threshold changed from 200 to 500
- now uses substitute_vars in model blocking

#### c30
- made it so that max, min and mean do not create new columns

#### c31
- fixed a bug in condition subsumption and added transitivity

#### c32
- reversed "made it so that max, min and mean do not create new columns"

#### c33
- added operation transitivity   ---   UNSOUND

#### c34
- reverted "added operation transitivity"

#### c35
- disabled condition subsumption

#### c36
- enabled condition subsumption
- split inner and cross join   ---    FLAWED IMPLEMENTATION

#### t11
    'subsume_conditions': [True, False],
    'static_search': [True, False],
    'z3_QF_FD': [True, False]
- cache enabled

#### c37
- fixed a few instances (csv format errors / type detection mistakes)
- fixed a bug where the dsl for the block case wasn't actually being blocked
- small optimizations

#### c38
- disabled condition subsumption
