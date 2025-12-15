# Propagate data points to "fill" null data.

`ditto` is a function that allow you to "fill" null values in a vector
with non-null values from earlier/later in the same vector. The default,
"forward," behavior fills each null value with the previous (lower
index) non-null value, if there are any. The `reverse` argument can be
used to cause "backward" filling, where the *next* (higher index)
non-null value is used. If the input begins (or ends if
`reverse == TRUE`) with a null value, the `initial` argument is filled
instead; defaults to `NA`.

## Usage

``` r
ditto(x, ...)

# Default S3 method
ditto(
  x,
  null = function(x) is.na(x) | x == ".",
  initial = NA,
  reverse = FALSE,
  groupby = list(),
  orderby = list()
)

# S3 method for class 'data.frame'
ditto(x, ...)

# S3 method for class 'matrix'
ditto(x, margin = 2, ...)

# S3 method for class 'humdrumR'
ditto(x, ..., initial = NA, reverse = FALSE)
```

## Arguments

- x:

  ***A vector.***

  Should be `list`, `atomic`, `matrix`, or `data.frame`.

- null:

  ***Defines which elements needs to be filled.***

  Defaults to `function(x) is.na(x) | x == "."`.

  Should be either a logical vector where (`length(x) == length(null)`),
  a numeric vector of positive indices, or a function which, when
  applied to `x` returns an appropriate logical/numeric vector.

- initial:

  ***Padder for the beginning (or end, if `reverse == TRUE`) of the
  output, if needed.***

  Defaults to `NA`.

  Should be the same class as `x`; must be length `1`.

- reverse:

  ***Whether the excecution order is reversed.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `reverse == TRUE`, the "non-null" values are coped to overwrite
  null values *earlier* (lower indices) in the vector.

- groupby:

  ***How to group the data.***

  Should be `vector` or `list` of `vectors`; must be length `length(x)`.

  Each segment of `x` delineated by the `groupby` vector(s) is treated
  separately.

- margin:

  ***A vector giving the dimensions which the function will be applied
  over.***

  Defaults to `2` (across columns) for `matrix` inputs.

  Must be natural number(s).

  E.g., for a matrix `1` indicates rows, `2` indicates columns. Where
  `x` has named dimnames, it can be a character vector selecting
  dimension names.

  Must be a single `character` string.

## Details

Which values are considered "null" can be controlled using the `null`
argument. The `null` argument can either be a logical vector which is
the same length as the input (`x`) argument, a numeric vector of
positive indices, or a function which, when applied to `x` returns an
appropriate logical/numeric vector. The values of `x` where
`null == FALSE` are copied forward/backwards to replace any adjacent
vales where `null == TRUE`. By default, `null` is the function
`\(x) is.na(x) | x == '.'`, which means that `NA` values and the string
`"."` are "null", and are overwritten by adjacent values.

`ditto` methods are defined for data.frames and matrices. The
`data.frame` method simply applies `ditto` to each column of the
`data.frame` separately. For matrices, ditto can be applied across
columns (`margin == 2`), rows (`margin == 1`), or other dimensions.

The `ditto` method for a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
simply applies `ditto` to the, by default, the selected field; thus
`ditto(humData)` is equivalent to
`within(humData, newField <- ditto(.), dataTypes = 'Dd')`. The `field`
argument can be used to indicated a different field to apply to. The
result of the dittoing is saved to a new field—the `newField` argument
can be used to control what to name the new field.

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

Other Lagged vector functions:
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
[`int()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md),
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md),
[`sigma()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sigma.md)
