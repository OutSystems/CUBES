import glob
import json
import re
from collections import OrderedDict

import yaml


def ordered_dump(data, stream=None, Dumper=yaml.Dumper, **kwds):
    class OrderedDumper(Dumper):
        pass

    def _dict_representer(dumper, data):
        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
            data.items())

    OrderedDumper.add_representer(OrderedDict, _dict_representer)
    return yaml.dump(data, stream, OrderedDumper, **kwds)


for file in glob.glob("**/*.json"):
    j = {}

    with open(file) as f:
        j = json.load(f, object_pairs_hook=OrderedDict)

    out = file.replace('.json', '.yaml')
    with open(out, 'w') as f:
        ordered_dump(j, f, Dumper=yaml.SafeDumper)
