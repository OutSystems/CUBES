from dataclasses import dataclass, field
from typing import List, Tuple


@dataclass
class Config:
    seed: int

    lines_force_all_inputs: bool = True

    disabled: List[str] = field(default_factory=list)
    aggregation_functions: List[str] = field(default_factory=lambda: ['max', 'min', 'avg', 'n', 'sum', 'max(n)'])

    like_comparison_enabled: bool = True

    alt_empty_pos: bool = False
    shuffle_cols: bool = False
    z3_smt_phase: int = 3
    z3_smt_case_split: int = 1
    z3_sat_phase: str = 'caching'
    z3_sat_restart: str = 'ema'
    z3_sat_branching: str = 'vsids'
    z3_QF_FD: bool = False
