##Week 1

###Configuration attempts

####try1
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=False, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=True, shuffle_cols=True),

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