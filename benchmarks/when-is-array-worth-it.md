# when is array worth it?

I was planning to optimize the decoders by creating an array from the row list and then do all lookups on that instead of doing `List.drop index |> List.head`, thinking that lookups would be faster.
Turns out, that may only be accurate above 8 items!

For each size, we're benchmarking getting each item in the list individually.
For arrays, we create the array inside the benchmark (but even if we don't do that, we only get a modest speedup at 8.)

| Size     | List       | Array     | % Change |
|----------|-----------:|----------:|---------:|
| 1 item   | 10,862,604 | 4,803,727 |  -55.78% |
| 2 items  |  7,160,442 | 3,740,035 |  -47.77% |
| 4 items  |  3,966,711 | 2,560,377 |  -35.45% |
| 8 items  |  1,939,120 | 1,613,071 |  -16.81% |
| 16 items |    869,669 |   940,014 |   +8.09% |
| 32 items |    329,673 |   422,594 |  +28.19% |

Of course, this effect will increase at larger sizes, but I haven't worked with a lot of CSVs that have more than 10 items.
If someone complains about it, this is maybe worth implementing, but I don't think it's worth doing proactively (especially for the large cost to smaller CSVs.)
