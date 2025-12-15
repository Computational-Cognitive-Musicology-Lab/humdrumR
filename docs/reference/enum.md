# Enumerate vector

This function enumerates the values of an input vector `x`, counting
along the length of the vector from 1 to `length(x)`.

## Usage

``` r
enum(x, inPlace = TRUE, sep = ":")
```

## Arguments

- x:

  ***The input vector to enumrate.***

  Must be a vector (either atomic, or a
  [`list()`](https://rdrr.io/r/base/list.html)).

- inPlace:

  ***Should the numbers be pasted onto the original vector?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- sep:

  ***Separator between numbers and vector.***

  Defaults to `":"`.

  Can be empty string (`""`), if no separator is desired.

## Details

If `inPlace = TRUE` (and `x` is atomic), the original vector is returned
with the counts pasted to the front of each value, separated by `sep`.
If `inPlace = FALSE`, a new `integer` vector is returned, identical to
calling [seq_along(x)](https://rdrr.io/r/base/seq.html).

## Examples

``` r
enum(letters)
#> Error in enum(letters): could not find function "enum"
enum(letters, inPlace = FALSE)
#> Error in enum(letters, inPlace = FALSE): could not find function "enum"
```
