import glob

runs = ['scythe', 'squares', 'sequential_2']

instances = {}

for run in runs:
    instances[run] = set(map(lambda f: f.replace(f'analysis/cosette/{run}/', ''), glob.glob(f'analysis/cosette/{run}/**/*.cos', recursive=True)))

# print(instances)

instance_intersect = None
for run in runs:
    if instance_intersect is None:
        instance_intersect = instances[run]
    else:
        instance_intersect = instance_intersect & instances[run]

print(sorted(instance_intersect))
