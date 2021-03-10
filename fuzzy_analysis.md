|                           | Scythe | SQUARES | Sequential | Seq (new sem.) | PatSQL |
|---------------------------|--------|---------|------------|----------------|--------|
| Exec Error Solution       | 687    | 519     | 104        | 95             | 245    |
| -- Syntax                 | 44     | 0       | 8          | 3              | 106    |
| -- Ambiguous column name  | 0      | 444     | 0          | 0              | 0      |
| -- DISTINCT not supported | 0      | 0       | 68         | 64             | 0      |
| -- doesn't return rows    | 0      | 75      | 0          | 0              | 133    |
| -- no such column         | 536    | 0       | 12         | 12             | 0      |
| -- no such function       | 107    | 0       | 16         | 16             | 0      |
| ---- first                | 97     | 0       | 0          | 0              | 0      |
| ---- concat               | 10     | 0       | 0          | 0              | 0      |
| ---- parse_datetime       | 0      | 0       | 14         | 14             | 0      |
| ---- mdy                  | 0      | 0       | 1          | 1              | 0      |
| ---- string_agg           | 0      | 0       | 1          | 1              | 0      |
| Exec Error gt             | 38     | 10      | 44         | 35             | 55     |
| No database               | 17     | 14      | 16         | 16             | 19     |
| No ground truth           | 32     | 10      | 47         | 47             | 63     |
| Incorrect number of cols  | 1      | 0       | 0          | 0              | 0      |


    You can optionally request that NAs don't match, giving a
    a result that more closely resembles SQL joins
    left_join(df1, df2, na_matches = "never")