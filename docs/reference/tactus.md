# Extract levels from meters

These functions take
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
objects—or values parseable as meters—and return specific levels from
the meter. `tactus()` extracts the tactus of a meter; `measure()`
extracts the length of the full measure of a meter.
[`nbeats()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/nbeats.md)
counts the number of tactus beats in the meter. These functions are
particularly useful as arguments to the [timecount and
subpos](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
functions.

## Usage

``` r
tactus(x, deparser, ...)

# S3 method for class 'meter'
tactus(x, deparser = recip, sep = "+", ...)

# S3 method for class 'character'
tactus(x, deparser = recip)

# S3 method for class '`NULL`'
tactus(x)

measure(x, deparser, ...)

# S3 method for class 'meter'
measure(x, deparser = recip)

# S3 method for class 'character'
measure(x, deparser = recip)

# S3 method for class '`NULL`'
measure(x)
```

## Arguments

- x:

  ***The input to compute the desired duration from.***

  Must be a
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
  object or a `character` vector.

  A `character` input is parsed using
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md);
  failures to parse result in errors.

- deparser:

  ***What output format is desired?***

  The default is
  [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md).

  Must be a [rhythm
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
  or `NULL`.

- sep:

  ***Seperator between irregular beat patterns.***

  Defaults to `"+"`.

  A singleton `character` value.

  If the tactus is a pattern of irregular beats, they are pasted
  together using this separator.

## Details

By default, `tactus()` and `measure()` deparse their output as
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md);
an alternative deparser (output format) can be chosen using the
`deparser` argument.

## See also

Other Metric functions:
[`meter`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md),
[`tatum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tatum.md)

## Examples

``` r
tactus("M4/4")
#> [1] "4"

tactus("M6/8")
#> [1] "4."

measure("M4/4")
#> **recip (character)
#> [1] 1

measure("M6/8")
#> **recip (character)
#> [1] 2.

measure("M6/8", deparser = duration)
#> **duration (numeric)
#> [1] 0.75
 
```
