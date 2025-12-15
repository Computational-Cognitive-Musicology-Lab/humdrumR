# Identify contiguous segments of data in a vector

`segments()` and `changes()` are extremely useful functions for finding
contiguous "segments" indicated in a vector. It can be particularly
useful to use `segments()` to create [grouping
factors](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md).

## Usage

``` r
segments(..., first = TRUE, any = TRUE, reverse = FALSE)

changes(..., first = TRUE, value = FALSE, any = TRUE, reverse = FALSE)
```

## Arguments

- ...:

  ***A list of atomic vectors.***

  If the vectors differ in length, they are all recycled to match the
  length of the longest vector.

- first:

  ***Is the first index (or last index if `reverse == TRUE`) marked as a
  "change."***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- any:

  ***Whether to mark changes any or all input vectors.***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, a change in *any* input vector is marked a change. If
  `FALSE`, changes must occur in *all* input vectors to be marked as a
  change.

- reverse:

  ***Whether the excecution order is reversed.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE` the function is excecuted backwards through the input
  vector(s).

- value:

  ***Whether to return the changed value matrix.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the input values where changes occur are returned in a
  matrix, with each row matching a change and each column containing the
  value from the associated input vector.

## Changes

`changes` takes an input vector and finds all indices `i` where the
value of `x[i] != x[i-1]`—i.e., where the value at one index has
"changed" since the last index. By default, `changes` returns a
`logical` vector the same length as the input, with `TRUE` only at
indices where a change occured. The `first` argument indicates whether
the first index (`i == 1`) is marked `TRUE`.

`changes` can accept more than one input vector. If the `any` argument
is set to `TRUE` (the default), a change in *any* input is marked as a
change (`TRUE`) in the output. If `any == FALSE`, changes must happen in
*all* vectors to be marked in the output.

Finally, the `reverse` argument reverses the behavior of `changes`,
checkig instead if `x[i] != x[i + 1]`.

### Values

By default, the values of the input vector(s) where a change occurs are
placed in a matrix and put in the `values` attribute of the `logical`
output. However, if the `value` argument is set to `TRUE`, the values
themselves are returned.

## Segments

The `segments` builds off of the `changes` function. The segments
function takes a `logical` input and *cummulatively* tallies each `TRUE`
value in the vector, from left to right (or right to left, if
`reverse == TRUE`). Thus, the input
`c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)` would return
`c(1, 1, 2, 2, 2, 3, 4, 4)`. This creates contiguous blocks of values
which can be used for a `groupby` argument in a call to
[`within.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
or similar functions like
[`base::tapply()`](https://rdrr.io/r/base/tapply.html).

Any input vector(s) to `segments` which are not `logical`, are first fed
to `changes` to create a `logical` input.

## Examples

``` r
segments(letters %~% '[aeiou]')
#>  [1] 1 1 1 1 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 5 5 5 5 5 5

changes(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), 
        c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
        any = TRUE)
#>  [1]  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE
#> attr(,"values")
#>    [,1] [,2]
#> 1     1    1
#> 4     2    1
#> 5     2    2
#> 7     3    2
#> 9     3    3
#> 10    4    3
# result is T,F,F,T,T,F,T,F,T,T,F,F
```
