# Numeric (double) representation of durations

Output is `numeric` (real number).

If `duration()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `quarters()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
duration(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

humData |> select(Token) |> duration() 
humData |> duration(Token)
humData |> duration(simple = TRUE)

duration(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

# Default S3 method
quarters(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

humData |> select(Token) |> quarters() 
humData |> quarters(Token)
humData |> quarters(simple = TRUE)

quarters(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)
```

## Arguments

- x:

  ***An input vector.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a [rational
  (rhythmInterval)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md),
  or `NULL`.

  Must be [parsable as rhythm
  infromation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- ...:

  ***Arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  There are also two hidden (advanced) arguments you can specify:
  `memoize` and `deparse` (see the details below).

- scale:

  ***A `numeric` or
  [rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  value which is used as the output unit of measurement.***

  Defaults to `rational(1, 1)`.

  Must be `numeric` or
  [rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md).

- inPlace:

  ***Should non-rhythm information be retained in the output string?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  Defaults to `FALSE`.

  A singleton `logical` value, or `NA`.

  See "Grace notes" section below.

## See also

To better understand how this function works, read about the [family of
rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
or how rhythms are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).

Other rhythm functions:
[`grid()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/grid.md),
[`notehead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md),
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)

## Examples

``` r
tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')

duration(tokens)
#> **duration (numeric)
#> [1] 0.3750 0.1250 0.0625 0.0625 0.0625 0.0625 0.0000 0.2500
quarters(tokens, parse(grace = NA))
#> **quarters (numeric)
#> [1] 1.50 0.50 0.25 0.25 0.25 0.25    . 1.00
quarters(tokens, inPlace = TRUE)
#> [1] "1.5GG"  "0.5G"   "0.25E"  "0.25F#" "0.25G"  "0.25D"  "0D#"    "1E"    

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!

with(B075, subset = Spine > 2, duration(Token)) 
#> **duration (numeric)
#>  [1]     .     .     .     .     .     .     . 0.250 0.500 0.250 0.250 0.250
#> [13] 0.250 0.500 0.250 0.500 0.500 0.250 0.250 0.250 0.250 0.125 0.125 0.125
#> [25] 0.125 0.125 0.125 0.125 0.125 0.250 0.250 0.250 0.250 0.125 0.125 0.125
#> [37] 0.125 0.125 0.125 0.125 0.125 0.000 0.000 0.250 0.250 0.250 0.250 0.125
#> [49] 0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.250 0.250 0.250
with(B075, subset = Spine > 2, quarters(Token)) 
#> **quarters (numeric)
#>  [1]   .   .   .   .   .   .   . 1.0 2.0 1.0 1.0 1.0 1.0 2.0 1.0 2.0 2.0 1.0 1.0
#> [20] 1.0 1.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5
#> [39] 0.5 0.5 0.5 0.0 0.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1.0 1.0
#> [58] 1.0
```
