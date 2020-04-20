from dataclasses import dataclass, field
from typing import List


@dataclass
class Config:
    seed: int

    print_r: bool
    cache_ops: bool

    minimum_loc: int = 1
    maximum_loc: int = 256

    optimal: bool = False
    advance_processes: bool = False
    advance_percentage = .4

    programs_per_cube_threshold: int = 3500

    lines_force_all_inputs: bool = True
    is_not_parent_enabled: bool = True

    bitvector_size: int = 16  # TODO should not be a fixed number

    max_column_combinations: int = 2
    max_filter_combinations: int = 2
    max_join_combinations: int = 2
    max_columns: int = 6

    filters_function_enabled: bool = False

    disabled: List[str] = field(default_factory=list)
    aggregation_functions: List[str] = field(default_factory=lambda: ['max', 'min', 'mean', 'n', 'sum', 'concat'])

    ignore_aggrs: bool = False
    ignore_attrs: bool = False
    force_constants: bool = True
    force_summarise: bool = True

    solution_use_first_line: bool = False
    solution_use_last_line: bool = False

    alt_empty_pos: bool = False

    z3_smt_phase: int = 3
    z3_smt_case_split: int = 1
    z3_sat_phase: str = 'caching'
    z3_sat_restart: str = 'ema'
    z3_sat_branching: str = 'vsids'
    z3_QF_FD: bool = True

    h_unlikely_two_natural_joins: bool = True
