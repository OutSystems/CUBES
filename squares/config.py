from dataclasses import dataclass, field
from typing import List, Tuple


@dataclass
class Config:
    seed: int

    lines_force_all_inputs: bool = False

    bitvector_size: int = 16  # TODO should not be a fixed number

    max_column_combinations: int = 2
    max_filter_combinations: int = 2

    disabled: List[str] = field(default_factory=list)
    aggregation_functions: List[str] = field(default_factory=lambda: ['max', 'min', 'mean', 'n', 'sum'])
    ignore_aggrs: bool = True
    ignore_attrs: bool = False
    force_constants: bool = True
    force_summarise: bool = False
    force_occurs_maxn: bool = False
    like_enabled: bool = False

    alt_empty_pos: bool = False
    shuffle_cols: bool = False
    z3_smt_phase: int = 3
    z3_smt_case_split: int = 1
    z3_sat_phase: str = 'caching'
    z3_sat_restart: str = 'ema'
    z3_sat_branching: str = 'vsids'
    z3_QF_FD: bool = False
