# Benchmarking

The benchmarking strategy for this library is to compare the full implementation against the simplest possible parser that does an acceptable job (it just splits rows by the row separator, and column by the column separator. This wouldn't work for real data because of escaping issues.)

The idea here is that if we can get the benchmarks for the real thing to be anything like the naive-but-fast thing, we're doign pretty well.

Numbers are runs per second on Brian's MacBook Pro (2017, 3.1 Ghz Quad-Core Intel Core i7 w/ 16GB memory) in Chrome (latest at time of writing.)

## January 22, 2021 (1.0.1)

| Size   | Naive     | Real    |
|--------|-----------|---------|
| 0 rows | 1,800,185 | 104,775 |
| 1 row  | 1,108,333 | 53,309  |
| 2 rows | 803,773   | 35,380  |
| 4 rows | 505,313   | 21,408  |
| 8 rows | 295,464   | 11,983  |
