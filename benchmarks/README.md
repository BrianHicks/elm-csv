# Benchmarking

The benchmarking strategy for this library is to compare the full implementation against the simplest possible parser that does an acceptable job (it just splits rows by the row separator, and column by the column separator. This wouldn't work for real data because of escaping issues.)

The idea here is that if we can get the benchmarks for the real thing to be anything like the naive-but-fast thing, we're doign pretty well.

Numbers are runs per second on Brian's MacBook Pro (2017, 3.1 Ghz Quad-Core Intel Core i7 w/ 16GB memory) in Chrome (latest at time of writing.)

## January 23, 2021 (1.0.1)

| Size   | Naive     | Real      | % Change |
|--------|----------:|----------:|---------:|
| 0 rows | 3,133,535 | 2,863,423 |   -8.62% |
| 1 row  | 1,956,732 |   101,973 |  -94.79% |
| 2 rows | 1,116,813 |    51,576 |  -95.56% |
| 4 rows |   623,706 |    26,050 |  -95.82% |
| 8 rows |   330,512 |    13,279 |  -95.98% |

So around two orders of magnitude slower across the board.
That's where we're starting!
