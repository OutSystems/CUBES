from dataclasses import dataclass, field
from typing import List


@dataclass
class Config:
    disabled: List[str] = field(default_factory=list)
    alt_empty_pos: bool = False
    shuffle_cols: bool = False
