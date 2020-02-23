##Week 1

###Configuration attempts

####try1
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'], alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'], alt_empty_pos=False, shuffle_cols=True),
    Config(seed=seed + 1, disabled=['semi_join'], alt_empty_pos=True, shuffle_cols=True),

####try2
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='random')

####try3
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='random'),

####try4:
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='always_false'),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='always_true'),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='random'),

####try5:
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'], z3_smt_phase=6),
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

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
    Config(seed=seed, disabled=['semi_join']),  # original squares
    Config(seed=seed, disabled=['inner_join4'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join3'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join', 'inner_join4', 'anti_join', 'left_join', 'bind_rows', 'intersect'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True,
           z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
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

####try6_1
    Config(seed=seed, disabled=['semi_join']),  # original squares
    Config(seed=seed, disabled=['inner_join4'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join', 'inner_join4', 'anti_join', 'left_join', 'bind_rows', 'intersect'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True,
           z3_sat_phase='random'),
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
####try7 - first try without knowing aggrs
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=[], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["min"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["mean"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max(n)"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),