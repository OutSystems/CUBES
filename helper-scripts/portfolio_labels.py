from itertools import product

config_map = {
        'subsume_conditions': [True, False],
        'bitenum_enabled': [True, False],
        'z3_QF_FD': [True, False]
        }

for i, config in enumerate(map(lambda vals: {key: val for key, val in zip(config_map.keys(), vals)}, product(*config_map.values()))):
    print(i, config)
