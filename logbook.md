##Week 1

###Configuration attempts

####try1
    Config(seed=seed, disabled=['inner_join', semi_join']),
    Config(seed=seed, disabled=['inner_join', semi_join'], alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['inner_join', semi_join'], alt_empty_pos=False, shuffle_cols=True),
    Config(seed=seed + 1, disabled=['inner_join', semi_join'], alt_empty_pos=True, shuffle_cols=True),

####try2
    Config(seed=seed, disabled=['inner_join', 'semi_join'], filters_function_enabled=True, max_filter_combinations=1),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

####try3
    Config(seed=seed, disabled=['semi_join']),
    Config(seed=seed, disabled=['semi_join'],alt_empty_pos=True, shuffle_cols=True),
    Config(seed=seed, disabled=['semi_join'],z3_QF_FD=True, z3_sat_phase='random'),

####try4:
    Config(seed=seed, disabled=['inner_join', 'semi_join'], max_filter_combinations=1, filters_function_enabled=True),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='always_false'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='always_true'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

####try5:
    Config(seed=seed, disabled=['inner_join', 'semi_join'], max_filter_combinations=1, filters_function_enabled=True),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_smt_phase=6),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='caching'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),

##Week 2

###Parameters
- pb.solver: no effect at all (no PB clauses?)
- cardinality.solver: no effect at all (no card clauses?)
- unit_walk: much slower, can't enumerate programs with 3 lines

###Interesting instances
- 55-tests/54: susceptible to bind_rows
- 55-tests/12
- 55-tests/10
- 55-tests/14
- 55-tests/5

###Subject to non-determinism in original squares
- 55-tests/10

###Configuration attempts

####try6
    Config(seed=seed, disabled=['inner_join', 'semi_join'], filters_function_enabled=True, max_filter_combinations=1),
    Config(seed=seed, disabled=['inner_join', 'natural_join4'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'natural_join3'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join', 'natural_join4', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
##Week 3


###Reasons why instances fail
- Dates are not supported
- concat as an aggregation function (!= from concatenating two different columns, ie, unite)
- first as an aggregation function?? (only supported by some DBMS)
- needs INNER JOIN ON instead of NATURAL JOIN
- a column that is a max of 'something' must be called 'maxsomething' in the io example


###Trying to remove aggrs requirement
- Removed redundancy from filter conditions (eg: A==B and B==A, A<B and B\>A)
- Problem: SQUARES forced the generated filter conditions to appear on the program. Since we cannot do that anymore synthesis becomes much much much slower. Use multiple threads as a workaround?
  - eg: 55-tests/2 takes ages to go through 3 line programs, while forcing the constants makes it so that no 3 line programs are generated

###Interesting instances
- 55-tests/40

###Misc.
- Found out why running a configuration similar to original squares gave different results. The second Z3 solver (used for conflict analysis) was not being seeded.
  - However there is still some variation in the results...

###Configuration attempts
    
####try7 - first try without knowing aggrs
    Config(seed=seed, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=[], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["min"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["mean"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max(n)"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, aggregation_functions=["n", "max"], force_summarise=True, disabled=['semi_join'], z3_QF_FD=True, z3_sat_phase='random'),
    
## Week 4
###Reasons why instances fail
- Extracting parts of the date
- mutate (eg. SELECT sqrt(a-b) AS c) is needed sometimes (eg. cumulative sums)
- renaming columns (some very simple cases are already supported by using the new inner join)

###New things
- Inner join is finished
- Dates are now supported (kind of? too many formats...)
- concat as an aggregation function is now supported
- 'max(n)' is no longer a special case with lots of exceptions. Most R expressions can be used as filter conditions
- Removed redundant group by conditions (a, b) and (b, a)
- Removed redundant filter conditions (eg. n > 5 | n >= 5)   \[might be slightly slower for simple programs?\]

###Different solutions
- 55-tests/2
- 55-tests/4

###Configuration attempts
From this point onward the new filter, summarise and join condition code is used.

####try8 - knows args
    Config(seed=seed, ignore_aggrs=False, disabled=['inner_join', 'semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, disabled=['semi_join'], force_summarise=True, z3_QF_FD=True, z3_sat_phase='random'),
    
## Week 5

### Notes
- Starting one of the processes with higher loc helps solve most very hard instances. However, that makes it so that we no longer find the optimal solution to problems.

### Configuration attempts

#### try9 - knows args
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'semi_join']),
    # original squares
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join3'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True,
           disabled=['inner_join', 'semi_join', 'natural_join4', 'anti_join', 'left_join', 'bind_rows',
                     'intersect'], z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True,
           disabled=['inner_join', 'semi_join', 'anti_join', 'left_join', 'bind_rows', 'intersect'],
           z3_QF_FD=True, z3_sat_phase='random'),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random', max_column_combinations=1, max_filter_combinations=1,
           starting_loc=6),
    Config(seed=seed, ignore_aggrs=False, force_summarise=True, disabled=['inner_join', 'natural_join4'],
           z3_QF_FD=True, z3_sat_phase='random', max_column_combinations=1, starting_loc=5),
    Config(seed=seed, ignore_aggrs=False, disabled=['semi_join'], force_summarise=True, z3_QF_FD=True,
           z3_sat_phase='random'),
           
## Week 6

###New
- Added very basic space splitting. Using the first n-1 lines and only fixing the function (not the arguments)

### Configuration attempts

#### cubes1
    Config(seed=seed, disabled=['inner_join', 'semi_join'], z3_QF_FD=True, z3_sat_phase='random', is_not_parent_enabled=False)
    
## Week 8

### 55-tests/22
- last line: 41m56 (30 processes)
- 2 last lines: 2m40 (30 processes)

### 55-tests/43
- last line: 1m20 (just 5 lines, weird solution) (30 processes)

### 55-tests/46
- last line: 2h+ timeout (30 processes)

## Week 9

### 55-tests/22
Stats after 300s (16 threads):

    [  299.2467][main][info] Statistics:
    [  299.2467][main][info]        Generated cubes: 224
    [  299.2467][main][info]        Attempted programs: 187316
    [  299.2467][main][info]                Rejected: 18104
    [  299.2467][main][info]                Failed: 169212
    [  299.2467][main][info]        Blacklist clauses: 62
    [  299.2469][main][info] Priting statistics for good programs of size 4
    [  299.2484][main][info]        0: Counter({'summariseGrouped': 1624, 'filter': 232, 'natural_join': 16, 'left_join': 16, 'bind_rows': 16})
    [  299.2500][main][info]        1: Counter({'natural_join3': 1344, 'filter': 208, 'summariseGrouped': 208, 'bind_rows': 112, 'left_join': 16, 'natural_join': 16})
    [  299.2506][main][info]        2: Counter({'filter': 1464, 'natural_join3': 288, 'summariseGrouped': 72, 'bind_rows': 48, 'natural_join': 16, 'left_join': 16})
    [  299.2512][main][info]        3: Counter({'select': 1904})
    [  299.2512][main][info] Priting statistics for good programs of size 5
    [  299.2579][main][info]        0: Counter({'natural_join': 15714, 'summariseGrouped': 4309})
    [  299.2627][main][info]        1: Counter({'summariseGrouped': 12986, 'natural_join': 4091, 'filter': 2568, 'natural_join3': 218, 'left_join': 64, 'bind_rows': 64, 'semi_join': 32})
    [  299.2670][main][info]        2: Counter({'filter': 5189, 'natural_join3': 4624, 'left_join': 3274, 'bind_rows': 2571, 'summariseGrouped': 2402, 'natural_join': 1664, 'anti_join': 192, 'semi_join': 107})
    [  299.2710][main][info]        3: Counter({'filter': 12320, 'natural_join3': 3711, 'natural_join': 1208, 'bind_rows': 1061, 'left_join': 977, 'summariseGrouped': 328, 'semi_join': 210, 'anti_join': 208})
    [  299.2747][main][info]        4: Counter({'select': 20023})
    No solution found

### 55-tests/46
Stats after 300s (16 threads):

    [  299.1345][main][info] Statistics:
    [  299.1345][main][info]        Generated cubes: 72
    [  299.1345][main][info]        Attempted programs: 0
    [  299.1345][main][info]                Rejected: 0
    [  299.1345][main][info]                Failed: 0
    [  299.1345][main][info]        Blacklist clauses: 86
    [  299.1348][main][info] Priting statistics for good programs of size 4
    [  299.1365][main][info]        0: Counter({'summariseGrouped': 1616, 'natural_join3': 907})
    [  299.1376][main][info]        1: Counter({'natural_join3': 1081, 'summariseGrouped': 907, 'filter': 535})
    [  299.1383][main][info]        2: Counter({'filter': 1988, 'natural_join4': 451, 'natural_join3': 84})
    [  299.1390][main][info]        3: Counter({'select': 2523})
    [  299.1391][main][info] Priting statistics for good programs of size 5
    [  299.1409][main][info]        0: Counter({'natural_join': 7995})
    [  299.1426][main][info]        1: Counter({'summariseGrouped': 7995})
    [  299.1443][main][info]        2: Counter({'filter': 2754, 'natural_join3': 1811, 'left_join': 1392, 'natural_join': 1300, 'anti_join': 738})
    [  299.1460][main][info]        3: Counter({'filter': 5241, 'bind_rows': 916, 'natural_join4': 880, 'natural_join': 291, 'left_join': 263, 'natural_join3': 201, 'semi_join': 123, 'anti_join': 80})
    [  299.1477][main][info]        4: Counter({'select': 7995})
    No solution found
    
## Week 10

### Unsupported
- Union with selects (scythe/recent_posts/001)
- Complex join (scythe/recent_posts/013, scythe/recent_posts/036, scythe/recent_posts/048, leetcode/181)
- Left join with const (scythe/recent_posts/021)
- Top n (scythe/recent_posts/022, leetcode/185)
- Float comparison? (scythe/recent_posts/24)
- Mutate / summarise with mutate (scythe/recent_posts/24, scythe/recent_posts/33, scythe/recent_posts/35, leetcode/262)
- Mutate (scythe/recent_posts/26, leetcode/178)
- Date part + date arithmetic (scythe/recent_posts/27)
- Join with different names (scythe/recent_posts/28)
- full outer join? (scythe/recent_posts/029)
- self filter ?? (scythe/recent_posts/043)

#### Unknown reason
- scythe/recent_posts/007
- scythe/recent_posts/023
- scythe/recent_posts/037 (evil)

### Requires inner_join
- scythe/recent_posts/003
- others

### Should sort
- scythe/recent_posts/003
- scythe/recent_posts/014
- scythe/recent_posts/028
- scythe/recent_posts/032
- scythe/recent_posts/042
- scythe/recent_posts/048
- leetcode/178

### Pivots
- scythe/recent_posts/008
- scythe/recent_posts/015

### Hard but possibly solvable
- scythe/recent_posts/046

## Weeks 11, 12

### Possible extension
- union distinct

### Underspecified instances
- leetcode/180

### Unsolved instances

#### Supported
- 55-tests/22
- 55-tests/46
- scythe/recent_posts/006
- scythe/recent_posts/012
- scythe/recent_posts/019
- scythe/recent_posts/036
- scythe/recent_posts/046
- scythe/recent_posts/046
- textbook/20
- textbook/22

#### Unsure if supported?
- scythe/recent_posts/001
- scythe/recent_posts/039
- leetcode/185
- textbook/23
- textbook/26
- textbook/29
- textbook/32
- textbook/34

#### Not supported

##### Mutate
- scythe/recent_posts/020 (concat, array_agg)
- scythe/recent_posts/024 (arithmetic)
- scythe/recent_posts/026 (predicates to bool cols)
- scythe/recent_posts/027 (date arithmetic: hour(ts)*2 + minute(ts) %/% 30)
- scythe/recent_posts/033 (arithmetic)
- scythe/top_rated_posts/024 (year(date), month(date, label=T))
- scythe/top_rated_posts/052 (arithmetic)
- scythe/top_rated_posts/053 (case when)
- leetcode/197 (arithmetic)
- leetcode/262 (replace_na(n.y, 0) / n.x)
- textbook/33 (arithmetic)

##### Grouped mutate
- scythe/recent_posts/024
- scythe/recent_posts/035 (n / sum(n) * 100)

##### Grouped filter
- textbook/36 (all)
- textbook/37 (all, any)

##### Complex Join
- scythe/recent_posts/013
- scythe/recent_posts/021
- scythe/recent_posts/023 (chained left join)
- scythe/recent_posts/028
- scythe/recent_posts/043 (self join)
- scythe/recent_posts/048 (self join)
- scythe/recent_posts/050
- scythe/top_rated_posts/018
- scythe/top_rated_posts/030
- scythe/top_rated_posts/054
- leetcode/181
- leetcode/197
- textbook/27

##### Complex Constant
- scythe/recent_posts/033 (ymd('2016-10-13') - weeks(1))

##### Distinct
- scythe/recent_posts/017 (distinct with .keep=T) (would still be 6 lines + select) (scythe supports because instance is underspecified)

##### Union select
- scythe/recent_posts/002

##### Float comparison
- scythe/recent_posts/024
- scythe/recent_posts/035
- leetcode/262

##### Gather / Spread
- scythe/recent_posts/008
- scythe/recent_posts/015
- scythe/top_rated_posts/015
- scythe/top_rated_posts/026
- scythe/top_rated_posts/033
- scythe/top_rated_posts/035
- scythe/top_rated_posts/042 (transpose)

##### Others
- scythe/recent_posts/010 (???)
- scythe/recent_posts/029 (full outer join?)
- scythe/top_rated_posts/041 (recursive query)
- scythe/top_rated_posts/046 (custom order, multiple filters)
- scythe/top_rated_posts/056 (rotate table 45 degrees?????????????????????????)

##### Too complex to ever support?
- scythe/recent_posts/030 (IN sets)
- scythe/recent_posts/037 (SQL windows??)
