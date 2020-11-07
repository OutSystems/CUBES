from itertools import product

config_map = {
    'z3_sat_phase': ['caching', 'random'],
    'bitenum_enabled': [True, False]
    }

configs = []
for config in map(lambda vals: {key: val for key, val in zip(config_map.keys(), vals)}, product(*config_map.values())):
    configs.append(config)

print(configs)
