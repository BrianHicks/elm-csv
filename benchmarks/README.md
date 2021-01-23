# Benchmarking

The benchmarking strategy for this library is to compare the full implementation against the simplest possible parser that does an acceptable job (it just splits rows by the row separator, and column by the column separator. This wouldn't work for real data because of escaping issues.)

The idea here is that if we can get the benchmarks for the real thing to be anything like the naive-but-fast thing, we're doign pretty well.

Numbers are runs per second on Brian's MacBook Pro (2017, 3.1 Ghz Quad-Core Intel Core i7 w/ 16GB memory) in Chrome (latest at time of writing.)

## Hand-Rolled Parser, January 24, 2021 (1.0.1)

I can keep (mostly) the same API, but probably get a big speedup by rolling my own parser function on `String` directly.

## Bail Early, January 23, 2021 (1.0.1)

I can get the 0 rows edge case way down by checking for that instead of using the `elm/parser` machinery.

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,099,488 | 42,873,148 | +1283.23% |
| 1 row  | 1,766,812 |     67,496 |   -96.18% |
| 2 rows |   967,385 |     33,915 |   -96.49% |
| 4 rows |   531,227 |     17,209 |   -96.76% |
| 8 rows |   285,386 |      8,491 |   -97.02% |

... yeah, that works.

## Initial Measurement, January 23, 2021 (1.0.1)

| Size   | Naive     | Real      | % Change |
|--------|----------:|----------:|---------:|
| 0 rows | 2,985,148 | 2,834,003 |   -5.06% |
| 1 row  | 1,704,020 |    64,626 |  -96.21% |
| 2 rows |   939,422 |    32,939 |  -96.49% |
| 4 rows |   530,027 |    16,667 |  -96.86% |
| 8 rows |   286,299 |     8,261 |  -97.11% |

So around two orders of magnitude slower across the board.
That's where we're starting!
