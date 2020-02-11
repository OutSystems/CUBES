##Week 1

###try1
    Config(),
    Config(alt_empty_pos=True, shuffle_cols=True),
    Config(alt_empty_pos=False, shuffle_cols=True),
    Config(alt_empty_pos=True, shuffle_cols=True),

###try2
    Config(),
    Config(z3_QF_FD=True, z3_sat_phase='caching'),
    Config(z3_QF_FD=True, z3_sat_phase='random')

###try3
    Config(),
    Config(alt_empty_pos=True, shuffle_cols=True),
    Config(z3_QF_FD=True, z3_sat_phase='random'),

###try4:
    Config(),
    Config(z3_QF_FD=True, z3_sat_phase='always_false'),
    Config(z3_QF_FD=True, z3_sat_phase='caching'),
    Config(z3_QF_FD=True, z3_sat_phase='always_true'),
    Config(z3_QF_FD=True, z3_sat_phase='random'),

###try5:
    Config(disabled=['semi_join']),
    Config(disabled=['semi_join'], z3_smt_phase=6),
    Config(disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    

##Week 2