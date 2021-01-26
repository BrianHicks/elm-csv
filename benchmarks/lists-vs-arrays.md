# Is it faster to cons onto a list, append to an array, or assign into an array with preallocated size?

The parser has to construct a lot of lists, and there may be a faster way.
There may be wins here, so I'm going to set up a new benchmark to measure.

| Test                                 | Speed      | % Change |
|--------------------------------------|-----------:|---------:|
| `(::)` directly                      | 17,996,283 |          |
| `(::)` into `List.reverse`           |  9,425,834 |  -90.93% |
| `(::)` into `Array.fromList`         |  5,745,229 |  -64.06% |
| `Array.push`                         |  3,737,409 |  -53.72% |
| `Array.set`, sharing initial `Array` |  2,029,485 |  -84.16% |
| `Array.set` without sharing          |  1,884,629 |   -7.69% |

Well, that's that; guess I should keep using `List`!
I probably could have predicted that reversing the array would roughly double the time, but I wouldn't have predicted that `set` on an `Array` initialized to the right length would be slower than `push`.
It'd be the opposite if we were mutating the array directly, but I guess since we aren't it's faster to keep all the null values and create a new reference or something.
