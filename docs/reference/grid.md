# Drum-machine grid representation of rhythmic durations.

These functions read and write a sequencer-like representation of
rhythm. Rhythms are represented as either strings or vectors of
"on"/"off" values, which indicate where rhythmic onsets occur in a
regular time grid. For example, `"X00X00X0"` or
`c(1, 0, 0, 1, 0, 0, 1, 0)`.

If `grid()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
grid(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

humData |> select(Token) |> grid() 
humData |> grid(Token)

grid(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

togrid(
  x,
  tick = tatum(x),
  measure = "1",
  on = "X",
  off = "O",
  collapse = TRUE,
  sep = ""
)

fromgrid(x, tick, meter)

# S3 method for class 'matrix'
fromgrid(x, tick = "16")

# S3 method for class 'character'
fromgrid(x, tick = "16", on = "X", off = "O", sep = "", deparser = recip, ...)

# S3 method for class 'logical'
fromgrid(x, tick = "16", deparser = rint2recip, ...)

# S3 method for class 'numeric'
fromgrid(x, tick = "16", deparser = recip, ...)

# S3 method for class 'integer'
fromgrid(x, tick = "16", deparser = recip, ...)
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

  ***Argments passed to the
  [deparser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).***

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

- tick:

  ***What is the unit of the grid?***

  Defaults to sixteenth-note (`fromgrid()`) or the
  [`tatum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tatum.md)
  of the `x` argument.

  Must be parsed as a rhythm by
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- on, off:

  ***What represents onsets (attacks) and rests in the grid?***

  Default to `"X"` and `"O"` respectively.

  Must be singleton atomic values.

- collapse:

  ***Should the output be collapsed to a single string per measure?***

  Defaults to `TRUE`.

  Must be singleton `logical` value: an on/off switch.

- sep:

  ***Separator between ticks in collapsed output.***

  Defaults to an empty string (no separator).

  Must be a singleton `character` string.

- deparser:

  ***What output representation should be returned?***

  Defaults to
  [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md).

  Must be a function which accepts
  [`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  numbers.

## Details

The `grid()` function, is a fully vectorized [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
which translates *individual* durations to a grid-representation
strings. For example, on a 16th-note grid, a dotted eighth-note would be
represented `"XOO"`. The `fromgrid()` and `togrid()` functions
create/read fuller grid representations, representing whole rhythms at
once: in their case, the length of input and output will not be the
same.

## See also

To better understand how `grid()` works, read about the [family of
rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
or how rhythms are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).

Other rhythm functions:
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md),
[`notehead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md),
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)

## Examples

``` r
rhythm <- c('8.', '8.', '8', '8.', '8', '16', '8')

grid(rhythm)
#> **grid (character)
#> [1] XOO XOO XO  XOO XO  X   XO 

togrid(rhythm, on = '1', off = '0')
#> [1] "1001001010010110"

togrid(rhythm, collapse = FALSE)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "X"  "O"  "O"  "X"  "O"  "O"  "X"  "O"  "X"  "O"   "O"   "X"   "O"   "X"  
#>      [,15] [,16]
#> [1,] "X"   "O"  

fromgrid('XOXOXOOXXOXOXXOO', tick = '8')
#> **recip (character)
#> [1] 4  4  4. 8  4  4  8  4.

```
