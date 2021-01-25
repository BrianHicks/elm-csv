# Benchmarking

The benchmarking strategy for this library is to compare the full implementation against the simplest possible parser that does an acceptable job (it just splits rows by the row separator, and column by the column separator. This wouldn't work for real data because of escaping issues.)

The idea here is that if we can get the benchmarks for the real thing to be anything like the naive-but-fast thing, we're doign pretty well.

Numbers are runs per second on Brian's MacBook Pro (2017, 3.1 Ghz Quad-Core Intel Core i7 with 16GB memory) in Chrome (latest at time of writing.)

Ideas I haven't tried yet:

- parse into an `Array String` directly
- defer slicing until the decoding step

## Avoiding passing next source slice, January 24, 2021 (1.0.1)

We can definitely just keep the slice indexes around instead of truncating the string.
Ok, let's slice into the full source!

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,140,522 | 29,442,281 |  +837.50% |
| 1 row  | 1,781,778 |  1,014,435 |   -43.07% |
| 2 rows |   993,161 |    519,795 |   -47.66% |
| 4 rows |   543,486 |    258,108 |   -52.51% |
| 8 rows |   290,018 |    129,659 |   -55.29% |

Well, that's an improvement from before, especially in the longer strings.
From the last time I compared benchmarks to benchmarks:

| Size   | Hand-Rolled | Source Slicing | % Change  |
|--------|------------:|---------------:|----------:|
| 1 row  |   1,062,082 |      1,014,435 | -4.49%    |
| 2 rows |     500,770 |        519,795 | +3.80%    |
| 4 rows |     245,318 |        258,108 | +5.21%    |

Huh!
Not quite the slam dunk I was expecting, but the effect seems to grow with longer CSVs, so I'll take it.

## Avoiding tuple allocation, January 24, 2021 (1.0.1)

Andrey (w0rm) pointed out that `String.uncons` is allocating a `Maybe ( Char, String )` on every iteration.
He suggested that getting the length of the string on each iteration ([constant time](https://jsbench.me/0dkkb3th3a/1)) or simply checking prefixes ([`slice` and `startsWith` should be equivalent](https://jsbench.me/mikkb4dm2s/1)) may be faster.
Let's try!

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,183,463 | 30,584,921 |  +860.74% |
| 1 row  | 1,800,687 |    944,412 |   -44.78% |
| 2 rows |   998,618 |    463,075 |   -53.63% |
| 4 rows |   540,625 |    227,136 |   -57.99% |
| 8 rows |   296,402 |    111,209 |   -62,48% |

Hmm, this didn't actually make things faster!
But I think this is required for further improvements, so I'm going to keep it (for now.)

## Hand-Rolled Parser, January 24, 2021 (1.0.1)

I can keep (mostly) the same API, but probably get a big speedup by rolling my own parser function on `String` directly.
(nb. it's not done yet; I haven't done quoted values.
However, the benchmark doesn't use quoted values and I added in a do-nothing branch so the compiled output includes the conditional check.)

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,209,593 | 30,841,837 |  +860.93% |
| 1 row  | 1,822,683 |  1,062,082 |   -41.73% |
| 2 rows |   988,819 |    500,770 |   -49.36% |
| 4 rows |   530,179 |    245,318 |   -53.73% |
| 8 rows |   240,949 |    124,999 |   -57.04% |

Much better!
It's not always reasonable to compare speed across revisions (that's why we test against a known target), but the naive numbers look reasonably close.
They're mostly within a couple percent of each other (except for 8, which is ~16%,) so I'm going to compare 1, 2, and 4:

| Size   | Bail Early | Hand-Rolled | % Change  |
|--------|-----------:|------------:|----------:|
| 1 row  |     67,496 |   1,062,082 | +1473.55% |
| 2 rows |     33,915 |     500,770 | +1376.54% |
| 4 rows |     17,209 |     245,318 | +1321.45% |

Seems like something like a 13x speedup.
Works for me!

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
