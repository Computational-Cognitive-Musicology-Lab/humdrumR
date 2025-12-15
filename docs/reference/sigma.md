# Cumulative sum of numeric vector

Calculate sequential cummulative sum of values in numeric vectors.

## Usage

``` r
sigma(x, lag, skip = is.na, init, groupby = list(), ...)

# Default S3 method
sigma(
  x,
  lag = 1,
  skip = is.na,
  init = 0,
  groupby = list(),
  orderby = list(),
  ...
)

# S3 method for class 'matrix'
sigma(x, margin = 2L, ...)
```

## Arguments

- x:

  ***The input vector.***

  Must be `atomic` numbers.

  `NULL` values are returned `NULL`.

- lag:

  ***Which lag to use.***

  Defaults to `1`.

  Must be a natural number. (See *Greater lags* section, below.)

- skip:

  ***A function to indicate which values to skip.***

  Defaults to `is.na`.

  This must be a `function` which can be applied to `x` to return a
  `logical` vector of the same length. `TRUE` values are skipped over in
  the calculations. By default, the `skip` function is `is.na`, so `NA`
  values in the input (`x` argument) are skipped. The skipped values are
  returned as is in the output vector.

- init:

  ***Initial value to fill the beginning for calculation.***

  Defaults to `0`.

  Should be the same class as `x`; length must be not longer than `lag`.

  `NA` values at the beginning (or end of `right == TRUE`) are filled
  with these values *before* summing.

- groupby:

  ***How to group the data.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Should be `vector` or `list` of `vectors`; must be length `length(x)`.

  Differences are not calculated across groups indicated by the
  `groupby` vector(s).

- orderby:

  ***The order for calculating the difference.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Should be `vector` or `list` of `vectors`; must be length `length(x)`.

  Differences in `x` are calculated based on the order of `orderby`
  vector(s), as determined by
  [`base::order()`](https://rdrr.io/r/base/order.html).

- right:

  ***Should the `init` padding be at the "right" (end of the vector)?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  By default, `right == FALSE` so the `init` padding is at the beginning
  of the output.

## Details

`sigma` is very similar base-`R`
[`cumsum()`](https://rdrr.io/r/base/cumsum.html). However, `sigma`
should be favored in
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
use because:

1.  It has a `groupby` argument, which is *automatically* used by
    `humdrumR`
    [with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
    commands to constrain the differences within pieces/spines/paths of
    `humdrum` data. Using the `groupby` argument to a function (details
    below) is generally faster than using a `groupby` argument to
    [`withinHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).

2.  They (can) automatically skip `NA` (or other) values.

3.  `sigma` also has a `init` argument which can be used to ensure full
    invertability with
    [`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md).
    See the "Invertability" section below.

If applied to a matrix, `sigma` is applied separately to each column,
unless `margin` is set to `1` (rows) or, if you have a
higher-dimensional array, a higher value.

## Invertability

The `sigma` and `delta` functions are inverses of each other, meaning
that with the right arguments set, `sigma(delta(x)) == x` and
`delta(sigma(x)) == x`. In other words, the two functions "reverse" each
other. The key is that the `init` argument needs to be set to `0`, and
all other arguments (`lag`, `skip`, `groupby`, etc.) need to match. So
*actually*, `sigma(delta(x, init = 0, ...)) == x` and
`delta(sigma(x), init = 0)) == x`.

When we take the differences between values (`delta(x)`), the resulting
differences can't tell us fully how to reconstruct the original unless
we know where to "start" (a constant offset). For example,

- `delta(c(5, 7, 5, 6)) == c(NA, 2, -2, 1)`

We know our input goes up 2, back down 2, then up 1, but the starting
value (the first `5`) is lost. If we call sigma on this, we'll get:

- `sigma(c(NA, 2, -2, 1)) == c(0, 2,0, 1)`

We get the right contour, but we're offset by that constant `5`.

If we call `delta(x, init = 0)` the necessary constant (the first value)
is kept at the beginning of the vector

- `delta(c(5, 7, 5, 6), init = 0) == c(5, 2, -2, 1)`

so `sigma` gets what we want, full invertability:

- `sigma(delta(c(5, 7, 5, 6), init = 0)) == c(5, 7, 5, 6)`

Alternatively, we could specify the necessary constant as the `init`
argument of `sigma`:

- `sigma(delta(c(5, 7, 5, 6)), init = 5) == c(5, 7, 5, 6)`

so the `init` arguments of the two functions are complementary.

Currently, the `right` argument of `delta` has no complement in `sigma`,
so invertability only holds true if `right = FALSE` (the default).

## Greater lags

The behavior of `sigma` when `abs(lag) > 1` is easiest to understand as
the inverse of the behavior of [delta(abs(lag) \>
1)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md), which is
more intuitive. (`sigma` is the inverse of
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
see the *Invertability* section above).

Generally, if `abs(lag) > 1`, `x` is grouped by its indices modulo
`lag`, and the cumulative sum is calculated separately for each set of
modulo indices. For example, consider `lag == 2` for the following
input:

|     |       |                |
|-----|-------|----------------|
| `x` | index | index modulo 2 |
| 1   | 1     | 1              |
| 3   | 2     | 0              |
| 2   | 3     | 1              |
| 2   | 4     | 0              |
| 5   | 2     | 1              |

The cumulative sum of the `1` and `0` modulo-index groups are:

- Index `1`: `cumsum(c(1,2,5)) == c(1, 3, 8)`.

- Index `0`: `cumsum(c(3,2)) == c(3, 5)`

Interleaved back into order, the result is `c(1,3,3,5,8)`. This may not
be very clear, but sure enough
`delta(c(1, 3, 3, 5, 8), lag = 2, init = 0)` returns the original
`c(1,3,2,2,5)` vector! Again, understanding [delta(..., lag =
n)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md) is easier
than `sigma(..., lag = n)` (see the *Invtertability* section below.)

## Negative lag

If `lag` is negative, the output is the same as the equivalent positive
lag, except the sign is reversed (`output * -1`). This behavior is
easiest to understand as the inverse of the behavior of [delta(lag \<
0)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md), which is
more intuitive. (`sigma` is the inverse of
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
see the *Invertability* section above).

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

## Order

When performing lagged calculations, we typically assume that the order
of the values in the input vector (`x`) is the order we want to "lag"
across. E.g., the first element is "before" the second element, which is
"before" the third element, etc. [Humdrum
tables](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
are always ordered `Piece > Piece > Spine > Path > Record > Stop`. Thus,
any lagged calculations across fields of the humtable will be, by
default, "melodic": the *next* element is the next element in the spine
path. For example, consider this data:

    **kern  **kern
    a       d
    b       e
    c       f
    *-      *-

The default order of these tokens (in the `Token` field) would be
`a b c d e f`. If we wanted to instead lag across our tokens
*harmonically* (across records) we'd need to specifiy a different order
For example, we could say `orderby = list(Piece, Record, Spine)`—the
lagged function would interpret the `Token` field above as
`a d b e c f`.

For another example, note `Stop` comes last in the order. Let's consider
what happens then if here are stops in our data:

    **kern  **kern
    a       d
    b D     e g
    c A     f a
    *-      *-

The default ordering here (`Piece > Spine > Record > Stop`) "sees" this
in the order `a b D c A d e g f a`. That may or may not be what you
want! If we wanted, we could reorder such that `Stop` takes precedence
over `Record`: `orderby = list(Piece, Spine, Stop, Record)`. The
resulting order would be `a b c d e f D G g a`.

## See also

This function's inverse is
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md).

Other Lagged vector functions:
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`int()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md),
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)
