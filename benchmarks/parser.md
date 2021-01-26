# Benchmarking

The benchmarking strategy for this library is to compare the full implementation against the simplest possible parser that does an acceptable job (it just splits rows by the row separator, and column by the column separator. This wouldn't work for real data because of escaping issues.)

The idea here is that if we can get the benchmarks for the real thing to be anything like the naive-but-fast thing, we're doing pretty well.

Numbers are runs per second on Brian's MacBook Pro (2017, 3.1 Ghz Quad-Core Intel Core i7 with 16GB memory) in Chrome (latest at time of writing.)

After this document was finalized, I had to make some changes to the parser that knock about 7% off the speed, but make the parser way more robust by being more accepting of different kinds of newlines.
It's still about 25x faster than the 1.0.1 parser.

## Final parser speedup for this document (January 26, 2021)

As of 00ac997c:

| Size    | Naive     | Real       | % Change  |
|---------|----------:|-----------:|----------:|
| 0 rows  | 3,077,611 | 19,439,037 |  +531.63% |
| 1 row   | 1,747,415 |  1,853,376 |    +6.06% |
| 2 rows  |   979,352 |    931,390 |    -4.90% |
| 4 rows  |   535,850 |    469,160 |   -12.45% |
| 8 rows  |   287,293 |    235,588 |   -18.00% |
| 16 rows |   146,172 |    118,980 |   -18.60% |

| Size   | Before    | After     | Adjustment | After, Adjusted | % Change  |
|--------|----------:|----------:|-----------:|----------------:|----------:|
| 1 row  |    64,626 | 1,853,376 |     -2.55% |       1,806,115 | 2,694.72% |
| 2 rows |    32,939 |   931,390 |     -4.25% |         891,806 | 2,607.45% |
| 4 rows |    16,667 |   469,160 |     -1.10% |         463,999 | 2,683.94% |
| 8 rows |     8,261 |   235,588 |     -0.35% |         234,763 | 2,741.82% |

Overall, a 26x speedup!

## Don't duplicate slices (January 25, 2021)

I was slicing off the first charcter of the string three times.
That only needs to be done once.
(Thanks again again, Andrey!)

| Size    | Naive     | Real       | % Change  |
|---------|----------:|-----------:|----------:|
| 0 rows  | 3,063,892 | 19,598,562 |  +539.66% |
| 1 row   | 1,708,921 |  1,830,877 |    +7.14% |
| 2 rows  |   983,645 |    922,945 |    -1.67% |
| 4 rows  |   526,378 |    465,208 |   -11.62% |
| 8 rows  |   279,097 |    465,208 |   -17.43% |
| 16 rows |   143,799 |    115,259 |   -19.85% |
| 32 rows |    73,565 |     58.084 |   -21.04% |

I also added a 16-row test because the function is fast enough now that doing so completes in a reasonable time, and a 32-row test just for this instance.

## Subtraction instead of comparison (January 25, 2021)

If you've got integers, doing `(x - y) >= 0` is faster than `x < y` because the compiler generates literally that code instead of using a comparator function.
(Thanks again, Andrey!)

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 2,982,748 | 18,602,065 |  +523.66% |
| 1 row  | 1,630,630 |  1,323,911 |   -18.11% |
| 2 rows |   905,553 |    660,299 |   -27.08% |
| 4 rows |   498,260 |    333,485 |   -33.07% |
| 8 rows |   266,176 |    165,825 |   -37.70% |

For 8 rows: +10.54% to the new values (I changed monitor configs again between last run and now.)
That means that 8 rows adjusted is like 183,302 times per second, a +6.13% improvement.

## Measuring progres on parser speedup (January 25, 2021)

I think I've optimized the parser as much as I possibly can (or at least as much as I want to right now.)
I've also made a bunch of bug fixes (and added the same inlining optimization for `;`-separated values, common in Europe.)

Let's get a final number to see how far we've come:

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,203,108 | 19,949,199 |  +522.81% |
| 1 row  | 1,811,039 |  1,311,312 |   -27.59% |
| 2 rows |   998,932 |    681,188 |   -31.81% |
| 4 rows |   551,327 |    341,781 |   -38.01% |
| 8 rows |   297,552 |    172,720 |   -41.95% |

We were down like 100% before, so this is a big improvement!
Let's make that percentage adjustment using the naive implementation and see where we're at:

| Size   | Before | After     | Adjustment | After, Adjusted | % Change  |
|--------|--------|-----------|------------|-----------------|----------:|
| 1 row  | 64,626 | 1,311,312 |     -6.28% |       1,228,961 | +1,801.65 |
| 2 rows | 32,939 |   681,188 |     -6.40% |         637,591 | +1,835.67 |
| 4 rows | 16,667 |   341,781 |     -4.01% |         328,076 | +1,868.42 |
| 8 rows |  8,261 |   172,720 |     -3.96% |         165,880 | +1,907.99 |

So I'm going to claim it's **19x faster**, since most CSVs I've worked with are 8 rows or longer.

What's that mean in characters per second?
The benchmark has five fields per row, plus four commas, and a `\r\n` on every line except the end: `rows * 31 - 2`.
For the benchmark with 8 rows, that's 246 characters.

So previously, we could process that 8,261 times per second, or 2,032,206 characters per second.
Now we can do it 165,880 (adjusted) times per second, or 40,806,480 characters per second!

## Inline "," and "\r\n" (January 25, 2021)

I'm going to copy the whole parser function and make a version that tests literals instead of references.
That should make the compiler use direct JS `===` instead of `_Utils_eq`.
Maybe it makes things faster, at the cost of more generated code?

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,152,392 | 20,269,870 |  +543.00% |
| 1 row  | 1,818,376 |  1,327,727 |   -26.98% |
| 2 rows | 1,003,826 |    678,638 |   -32.39% |
| 4 rows |   552,580 |    342,057 |   -38.10% |
| 8 rows |   293,287 |    175,085 |   -40.30% |

Looks like the naive implementation differs more than I'd like (~6%, vs ~2% before) so I'm going to adjust the new numbers down by the percentage the naive numbers differ to arrive at a conclusion:

| Size   | Baseline  | Inline    | Adjustment | Inline, Adjusted | % Change |
|--------|----------:|----------:|-----------:|-----------------:|---------:|
| 1 row  | 1,062,197 | 1,327,727 |     -6.05% |        1,247,399 |  +17.44% |
| 2 rows |   524,645 |   678,638 |     -6.35% |          635,544 |  +21.14% |
| 4 rows |   264,076 |   342,057 |     -5.45% |          323,415 |  +22.47% |
| 8 rows |   132,918 |   175,085 |     -5.16% |          166,050 |  +24.93% |

Ok, wow, that's probably worth keeping!

## Re-baseline once quoted field parser is finished (January 25, 2021)

I finished the quoted field parser.
It shouldn't have slowed down *too* much (it's doing the same comparison as before.)
But let's see:

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,171,828 | 20,880,023 |  +558.30% |
| 1 row  | 1,714,612 |  1,062,197 |   -38.05% |
| 2 rows |   944,000 |    524,645 |   -44.45% |
| 4 rows |   524,009 |    264,076 |   -49.60% |
| 8 rows |   278,901 |    132,918 |   -52.34% |

There's some weird noise here: the 0 row benchmark went down by a lot.
I tried quieting my computer down (had some additional chat apps open and a different monitor configuration) but the difference persists.

I don't see a big difference in the generated code, so I think this might be a fluke.
Everything seems to be within a couple percentage points of where I want it to be, so I'm going to move ahead.

## Defer slicing until the decoding step (January 25, 2021)

We won't necessarily use all the fields we parse, so we can just keep track of offsets instead of slicing directly.
This will cause a little issue for quoted strings, but we could get around it by returning `(Int, Int, Bool)` where the bool indicates whether the field is quoted or not and replacing `""` with `"` after slicing if it is.

Anyway, benchmarks:

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,154,649 | 29,144,858 |  +823.87% |
| 1 row  | 1,808,216 |  1,175,177 |   -35.01% |
| 2 rows |   997,407 |    576,939 |   -42.16% |
| 4 rows |   553,081 |    288,789 |   -47.79% |
| 8 rows |   290,929 |    142,389 |   -51.06% |

Let's see how that changed:

| Size   | Source Slicing | Int Tuples | % Change  |
|--------|---------------:|-----------:|----------:|
| 1 row  |      1,014,435 |  1,175,177 |   +15.85% |
| 2 rows |        519,795 |    576,939 |   +10.99% |
| 4 rows |        258,108 |    288,789 |   +11.89% |

Hmm!
That's nothing to scoff at, but there's the complication above.
And another thing, I bet people use all of the fields most of the time, so this may actually be adding allocations that we wouldn't have to be doing, just moving the string slicing to the decoder.
For both reasons, I'm going to back this change out, finish the quoting implementation, and then benchmark decoding.

## Avoiding passing next source slice (January 24, 2021)

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
| 1 row  |   1,062,082 |      1,014,435 |    -4.49% |
| 2 rows |     500,770 |        519,795 |    +3.80% |
| 4 rows |     245,318 |        258,108 |    +5.21% |

Huh!
Not quite the slam dunk I was expecting, but the effect seems to grow with longer CSVs, so I'll take it.

## Avoiding tuple allocation (January 24, 2021)

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

## Hand-Rolled Parser (January 24, 2021)

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

## Bail Early (January 23, 2021)

I can get the 0 rows edge case way down by checking for that instead of using the `elm/parser` machinery.

| Size   | Naive     | Real       | % Change  |
|--------|----------:|-----------:|----------:|
| 0 rows | 3,099,488 | 42,873,148 | +1283.23% |
| 1 row  | 1,766,812 |     67,496 |   -96.18% |
| 2 rows |   967,385 |     33,915 |   -96.49% |
| 4 rows |   531,227 |     17,209 |   -96.76% |
| 8 rows |   285,386 |      8,491 |   -97.02% |

... yeah, that works.

## Initial Measurement (January 23, 2021)

| Size   | Naive     | Real      | % Change |
|--------|----------:|----------:|---------:|
| 0 rows | 2,985,148 | 2,834,003 |   -5.06% |
| 1 row  | 1,704,020 |    64,626 |  -96.21% |
| 2 rows |   939,422 |    32,939 |  -96.49% |
| 4 rows |   530,027 |    16,667 |  -96.86% |
| 8 rows |   286,299 |     8,261 |  -97.11% |

So around two orders of magnitude slower across the board.
That's where we're starting!
