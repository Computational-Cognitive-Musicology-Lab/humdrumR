# Find common denominator of beats

In `humdrumR`, we define a *tatum* as the greatest common denominator of
a set of durations. In other words, given a set of durations, the
largest duration that divides all the given beats is the tatum—a common
unit which can measure any of the other durations

## Usage

``` r
tatum(x, ...)

# S3 method for class 'meter'
tatum(x, deparser = recip)

# S3 method for class 'character'
tatum(x, deparser = recip)

# S3 method for class 'numeric'
tatum(x, deparser = duration)

# S3 method for class 'rational'
tatum(x)

# S3 method for class 'token'
tatum(x)

# S3 method for class '`NULL`'
tatum(x)
```

## Arguments

- x:

  \***The input to compute the tatum of.**

  Must be a
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
  object, a singleton `character` value, or vector of either
  `character`, `numeric`, or
  [`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  values.

  For `character` input, valuest that match the regular expression
  `"^\*?M"` are parsed as a time signature using
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md),
  while other strings are parsed as durations using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).
  `numeric` input is also parsed using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  parse failures result in errors.

- deparser:

  ***What output format is desired?***

  For `character` or `meter` input, the default is
  [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md);
  for `numeric` input, the default is
  [`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md).

  Must be a [rhythm
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
  or `NULL`.

## Details

`tatum()` is a generic function; it can read any input which can be
parsed by the [rhythm
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).
If can also take a
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
object or `character` string of the form `"MX/Y"`.

The tatum of a
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
is the tatum of all that meters metric levels. If meters *and* durations
are provided—like `tatum(c('M4/4', '6')`— the tatum of all the meters'
levels *and* all the durations is computed.

The `deparser` argument is a [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
which controls the output format. If `deparser` is `NULL`, the tatum is
returned as a
[`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
value.

## See also

Other Metric functions:
[`meter`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md),
[`tactus()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tactus.md)

## Examples

``` r
tatum(c("4", "8", "12"))
#> **recip (character)
#> [1] 24

tatum(c("M4/4"))
#> [1] "16"
#> attr(,"Exclusive")
#> [1] "recip"

tatum(c('M4/4', '6'))
#> **recip (character)
#> [1] 48
```
