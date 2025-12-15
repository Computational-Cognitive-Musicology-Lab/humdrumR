# Shift data within a vector/matrix/data.frame

The `lag` and `lead` functions take input vectors, matrices, or
data.frames and shifts their data by `n` indices. They are similar to
the
[`data.table::shift()`](https://rdatatable.gitlab.io/data.table/reference/shift.html)
function, but with a few additional options.

## Usage

``` r
lag(x, n = 1, fill, wrap, groupby, ...)

lead(x, n = 1, ...)
```

## Arguments

- x:

  ***The input argument.***

  Should be `list`, `atomic`, `matrix`, or `data.frame`.

- n:

  ***The amount to lag/lead the data.***

  Defaults to `0`.

  Must be a natural number.

  If `n == 0`, `x` is returned unchanged.

- fill:

  ***Tokens used to pad the outputs.***

  Defaults to `NA`.

  Should be the same class as `x`.

  If `wrap = FALSE` parts of the output are padded with the `fill`
  argument.

- wrap:

  ***Whether to wrap the data.***

  Defaults to `FALSE`.

  Must be `logical`. Must be length `1`.

  If `wrap = TRUE`, data from the end (head or tail) is copied to the
  other end of the output, "wrapping" the data within the data
  structure.

- groupby:

  ***How to group the data.***

  Should be `vector` or `list` of `vectors`; must be length `length(x)`.

  Each segment of `x` delineated by the `groupby` vector(s) is treated
  separately.

- margin:

  ***Which dimension to shift.***

  Must be `numeric`.

  Arrays and data.frames can be lagged lead in multiple dimensions using
  the `margin` argument: `margin == 1` shifts across rows while
  `margin == 2` shifts across columns.

## Details

A lagged vector has the same values as the original vector, except
offset by `n` indices. `lag` moves each value to a high index (if
`n > 0`); `lead` does the opposite, moving each value to a lower index
(if `n > 0`). `n` can be positive or negative—negative lags are
equivalent to leads, and vice versa. Values near the end/beginning are
either "wrapped" to the opposite end of the vector, or replaced/padded
with the value of the `fill` argument.

The vector a, b, c, d, e, f, g can be lagged by `n==1` is NA, a, b, c,
d, e, f. If we set `wrap == TRUE`, the `"g"` moved to the beginning of
the output: is g, a, b, c, d, e, f.

## Grouping

In many cases we want to perform lagged calculations in a vector, but
*not across certain boundaries*. For example, if your vector includes
data from multiple pieces, we wouldn't want to calculate melodic
intervals between pieces, only within pieces. The `groupby` argument
indicates one, or more, grouping vectors, which break the `x` (input)
argument into groups. If more than `groupby` vectors are given, a change
in *any* vector indicates a boundary.

Value pairs which cross between groups are treated as if they were at
the beginning. Basically, using the `groupby` argument to a function
should be similar or identical to using
`tapply(x, groupby, laggedFunction, ...)` or using a `groupby` expession
in a call to
[with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).
However, using a `groupby` argument directly is usually much faster, as
they have been specially optimized for this functions.

The most common use case in humdrum data, is looking at "melodies"
within spines. For this, we want `groupby = list(Piece, Spine, Path)`.
In fact, `humdrumR`
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
calls will *automatically* feed these three fields as `groupby`
arguments to certain functions: mint, delta, sigma, lag, ditto, ioi,
sumTies, hop, wort, or wort.character. So any use of `delta` in a call
to
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
will automatically calculate the `delta` in a "melodic" way, within each
spine path of each piece. However, if you wanted, for instance, to
calculate differences across spines (like harmonic intervals) you could
manually set `groupby = list(Piece, Record)`.

## See also

[`data.table::shift()`](https://rdatatable.gitlab.io/data.table/reference/shift.html)

Other Lagged vector functions:
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`int()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md),
[`sigma()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sigma.md)
