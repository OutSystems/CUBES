from dataclasses import dataclass, field
from typing import List


@dataclass
class Config:
    seed: int
    verbosity: int

    print_r: bool
    cache_ops: bool

    minimum_loc: int
    maximum_loc: int

    subsume_conditions: bool
    transitive_blocking: bool

    advance_percentage = .6
    smoothing_bias: float = 1

    optimal: bool = False
    top_programs: int = 1
    advance_processes: bool = False
    programs_per_cube_threshold: int = 500
    bitenum_enabled: bool = True

    static_search: bool = False
    program_weigth_decay_rate: float = 0.99999

    probing_threads: int = 2
    cube_freedom: int = 0
    split_complex_joins: bool = True
    split_complex_joins_ratio: float = 1/3
    deduce_cubes: bool = True

    use_solution_dsl: bool = False
    use_solution_cube: bool = False

    bitvector_size: int = 16  # TODO should not be a fixed number # NOTE: not used anymore

    disabled: List[str] = field(default_factory=list)
    max_column_combinations: int = 2
    max_filter_combinations: int = 2
    max_join_combinations: int = 2
    na_matches: str = 'never'

    lines_force_all_inputs: bool = True
    is_not_parent_enabled: bool = True

    full_cross_join: bool = True

    filters_function_enabled: bool = False

    ignore_aggrs: bool = False
    aggregation_functions: List[str] = field(default_factory=lambda: ['max', 'min', 'mean', 'n', 'sum', 'concat'])

    ignore_attrs: bool = False
    force_constants: bool = True
    force_summarise: bool = True

    solution_use_lines: List[int] = field(default_factory=list)
    solution_use_last_line: bool = False

    z3_smt_phase: int = 3
    z3_smt_case_split: int = 1
    z3_sat_phase: str = 'caching'
    z3_sat_restart: str = 'ema'
    z3_sat_branching: str = 'vsids'
    z3_QF_FD: bool = True

    h_unlikely_two_natural_joins: bool = True
