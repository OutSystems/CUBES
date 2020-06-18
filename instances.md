# Underspecified instances
- scythe/recent_posts/017
- leetcode/180

# Unsolved instances

## Doesn't finish init
- scythe/recent_posts/007 (supported by scythe)

## Supported
- scythe/recent_posts/004
- scythe/recent_posts/006
- scythe/recent_posts/012
- scythe/recent_posts/013
- scythe/recent_posts/017
- scythe/recent_posts/019
- scythe/recent_posts/021
- scythe/recent_posts/028
- scythe/recent_posts/036
- scythe/recent_posts/046
- scythe/recent_posts/049
- leetcode/184
- textbook/20
- textbook/22
- textbook/26
- textbook/29
- textbook/34

## Unsure if supported?
- scythe/recent_posts/001
- scythe/recent_posts/023 (chained left join)
- scythe/recent_posts/039
- scythe/recent_posts/043 (self join)
- scythe/recent_posts/048 (self join)
- scythe/recent_posts/050
- scythe/top_rated_posts/018
- scythe/top_rated_posts/030
- leetcode/181
- leetcode/185
- leetcode/197
- textbook/27
- textbook/32

## Not supported

### Mutate
- scythe/recent_posts/020 (concat, array_agg)
- scythe/recent_posts/024 (arithmetic)
- scythe/recent_posts/026 (predicates to bool cols)
- scythe/recent_posts/027 (date arithmetic: hour(ts)*2 + minute(ts) %/% 30)           hour(??) * ?c + minute(??) %>% ?c
- scythe/recent_posts/033 (arithmetic)
- scythe/top_rated_posts/024 (year(date), month(date, label=T))
- scythe/top_rated_posts/052 (arithmetic)
- scythe/top_rated_posts/053 (case when)
- leetcode/197 (arithmetic)
- leetcode/262 (replace_na(n.y, 0) / n.x)
- textbook/33 (arithmetic)

### Grouped mutate
- scythe/recent_posts/024
- scythe/recent_posts/035 (n / sum(n) * 100)

### Grouped filter
- textbook/23 (any)
- textbook/36 (all)
- textbook/37 (all, any)

### Complex Constant
- scythe/recent_posts/033 (ymd('2016-10-13') - weeks(1))

### Union select
- scythe/recent_posts/002

### Float comparison
- scythe/recent_posts/024
- scythe/recent_posts/035
- leetcode/262

### Gather / Spread
- scythe/recent_posts/008
- scythe/recent_posts/015
- scythe/top_rated_posts/015
- scythe/top_rated_posts/026
- scythe/top_rated_posts/033
- scythe/top_rated_posts/035
- scythe/top_rated_posts/042

### Others
- scythe/recent_posts/010 (???)
- scythe/recent_posts/029 (full outer join?)
- scythe/top_rated_posts/041 (recursive query)
- scythe/top_rated_posts/046 (custom order, multiple filters)
- scythe/top_rated_posts/056 (rotate table 45 degrees?????????????????????????)
- textbook/12 (???)

### Too complex to ever support?
- scythe/recent_posts/030 (IN sets)
- scythe/recent_posts/037 (SQL windows??)