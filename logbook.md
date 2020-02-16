##Week 1

###Configuration attempts

####try1
    Config(),
    Config(alt_empty_pos=True, shuffle_cols=True),
    Config(alt_empty_pos=False, shuffle_cols=True),
    Config(alt_empty_pos=True, shuffle_cols=True),

####try2
    Config(),
    Config(z3_QF_FD=True, z3_sat_phase='caching'),
    Config(z3_QF_FD=True, z3_sat_phase='random')

####try3
    Config(),
    Config(alt_empty_pos=True, shuffle_cols=True),
    Config(z3_QF_FD=True, z3_sat_phase='random'),

####try4:
    Config(),
    Config(z3_QF_FD=True, z3_sat_phase='always_false'),
    Config(z3_QF_FD=True, z3_sat_phase='caching'),
    Config(z3_QF_FD=True, z3_sat_phase='always_true'),
    Config(z3_QF_FD=True, z3_sat_phase='random'),

####try5:
    Config(disabled=['semi_join']),
    Config(disabled=['semi_join'], z3_smt_phase=6),
    Config(disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    

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