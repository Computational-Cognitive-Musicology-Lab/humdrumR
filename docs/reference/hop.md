# Generate regular sequence "along" input

`hop()` is similar to base R's
[`seq()`](https://rdrr.io/r/base/seq.html), but with some additional
features, including special sugar when used with humdrumR's
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
command. `hop()` is used to create customizable sequences of indices for
a vector; for example, if you want to index every third value from a
vector. This is useful for, as when used with
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md),
defining the start points of "rolling-window" analyses along a vector;
the "hop size" is the gap between the start of each window, defined by
`hop()`'s `by` argument.

## Usage

``` r
hop(
  along.with,
  by = 1,
  from = 1L,
  to = NULL,
  value = FALSE,
  logical = FALSE,
  groupby = list()
)
```

## Arguments

- along.with:

  ***The vector you want indices to "hop" along.***

  Must be a vector (either atomic, or a
  [`list()`](https://rdrr.io/r/base/list.html)).

- by:

  ***The pattern of "hops" to to use.***

  Defaults to `1`: returning all indices `from:to`.

  Must be one or more whole numbers.

  `sum(by)` must non-zero.

- from:

  ***Where to start the sequence.***

  Defaults to `1`: starting from the first index.

  Must be either a single natural number, a single `character` string,
  or a `logical` vector the same length as `along.with`.

  A `character`-string input is treated as a regular expression, which
  is matched against `along.with` using
  [`grepl()`](https://rdrr.io/r/base/grep.html) to generate a `logical`
  vector. The index of the first `TRUE` is used.

- to:

  ***Where to end the sequence.***

  Defaults to `NULL`.

  Must be either `NULL`, a single natural number, a single `character`
  string, or a `logical` vector the same length as `along.with`.

  If `NULL`, `to` is set to the last index of `along.with` (or of each
  group in `groupby`). A `character`-string input is treated as a
  regular expression, which is matched against `along.with` using
  [`grepl()`](https://rdrr.io/r/base/grep.html) to generate a `logical`
  vector. The index of the last `TRUE` is used.

- value:

  ***Should indices be returned as logical `TRUE`s?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value; an on/off switch.

- groupby:

  ***Optional vectors to group hop sequences within.***

  Defaults to empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a [`list()`](https://rdrr.io/r/base/list.html), which is
  either empty or contains vectors which are all the same length as
  `along.with`. In calls to
  [with/within.humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
  `groupby` is passed `list(Piece, Spine, Path)` by default.

## Value

By default, `hop()` returns an `integer` vector, appropriates indices
for the `along.with` vector. However, there are two other options:

- If `logical = TRUE`, the indices are returned as a logical vector, the
  same length as `along.with`, with `TRUE` values indicating indices.
  Note that any ordering in the output (due to a mix of positive and
  negative values in the `by` argument) is lost.

- If `value = TRUE`, the actual indixed elements of the `along.with`
  vector are returned: Thus, `hop(myvector, ..., value = TRUE)` is
  simply the same as `myvector[hop(myvector, ...)]`. If `value = TRUE`,
  the `logical` argument is ignored.

## Details

`hop()` has similar arguments to
[`base::seq()`](https://rdrr.io/r/base/seq.html), but focused on the
`along.with` argument, a vector which you'd like to generate indices
for. If you simply call `hop(myvector)`, the output will be the same as
`1:length(myvector)`. The `by` argument can be used to specify a
different "hop" pattern: `by = 2` will get you *every other* index, `1`,
`3`, `5`, `7`, etc. Unlike
[`base::seq()`](https://rdrr.io/r/base/seq.html), `hop()`'s `by`
argument can be a vector of numbers, allowing you to specify a pattern
of hops. For example, `by = c(2, 3)` will first hop `2`, then hop `3`,
then repeat—so the output would be `1`, `3`, `6`, `8`, `11`, `13`, etc.

The by pattern can be comprised of negative numbers, or a mix of
negative and positive numbers. If `by` mixes negative and positive
numbers, the pattern can hop up and down, as it climbs. For example, you
could go up two, then down one, then repeat using `by = c(2,-1)`. If the
pattern is *overall* (sums) negative, the `from` argument must be
greater than the `to` argument (see next section); if the pattern sums
to zero, an error occurs because the pattern would never end! If a
pattern changes directions, it is possible for pattern to hop outside
the bounds of the vector; if this happens, the outside indices return
`NA`.

## Starting and Ending

By default, `hop()` builds indices from `1` to the end of the
`along.with` vector. The `from` and `to` arguments can be used to
control this. Either argument can simply be a natural number, indicating
where to start and end the output sequences. (If `to` is `NULL`, it is
set to `length(along.with)`.) An alternate approach is to provide either
argument a single `character` string, which is treated as a regular
expression and matched against `along.with`, or a `logical` vector the
same length as `along.with`. The first match/`TRUE` is used for the
`from` index and the last match/`TRUE` for the `to` index. This means
you can say things like `from = Record == 33` in a
[within()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
call.

If the `by` argument is *overall* (sums) positive, `from` must be less
than `to`. If the `by` argument is *overall* (sums) negative, `from`
must be greater than `to`.

If the `by` pattern doesn't ever actually the actual `to` index—perhaps
jumping over it— the output stops when it would *pass* the `to`.

## Grouping

In many cases we want to how along vectors, but *not across certain
boundaries*. For example, if we want all even numbered indices, we can
set `by = 2` and `from = 2`. However, if your vector includes data from
multiple pieces, and some of the pieces have an odd number of data
points, our "even" sequence would end up hitting odd numbers in some
pieces. To get around this, the `groupby` argument indicates one, or
more, grouping vectors, which break the `x` (input) argument into
groups. If more than `groupby` vectors are given, a change in *any*
vector indicates a boundary. Each grouped segement of `along.with` is
treated just like a separate call to `hop()`; for example, if
`from = 2`, the hop sequence will start on the second index of *each*
group. However, the output indices still represent the original
`along.with` indices.

Since `hop()` is usually used with
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
to create rolling windows within musical parts, we want typically want
to apply `hop()` using `groupby = list(Piece, Spine, Path)`. In fact,
`humdrumR`
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
calls will *automatically* feed these three fields as `groupby`
arguments to `hop()`. So any use of `hop()` in a call to
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
will automatically generate the hop sequence in a "melodic" way, within
each spine path of each piece.

## Examples

``` r
# use the built-in 'letters' vector

hop(letters)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26

hop(letters, by = 3)
#> [1]  1  4  7 10 13 16 19 22 25

hop(letters, by = 2, from = 4)
#>  [1]  4  6  8 10 12 14 16 18 20 22 24 26

hop(letters, by = 2, from = 'e', to = 'x')
#>  [1]  5  7  9 11 13 15 17 19 21 23

hop(letters, by = c(-1, 2), from = 'e', to = 'w', value = TRUE)
#>  [1] "e" "d" "f" "e" "g" "f" "h" "g" "i" "h" "j" "i" "k" "j" "l" "k" "m" "l" "n"
#> [20] "m" "o" "n" "p" "o" "q" "p" "r" "q" "s" "r" "t" "s" "u" "t" "v" "u" "w"

hop(letters, by = -1, from = 'z', to = 3)
#>  [1] 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3
```
