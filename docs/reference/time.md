# Clock-time representations of duration

These functions convert duration values to clock-time representations.
`seconds()` and `ms()` output `numeric` values. `dur()` outputs a
`character` string encoding the humdrum
[\*\*dur](https://www.humdrum.org/rep/dur/index.html) representation of
time.

If `seconds()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `ms()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `dur()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
seconds(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> seconds() 
humData |> seconds(Token)

seconds(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

# Default S3 method
ms(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> ms() 
humData |> ms(Token)

ms(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

# Default S3 method
dur(
  x,
  ...,
  BPM = "*M60",
  minutes = FALSE,
  hours = FALSE,
  days = FALSE,
  months = FALSE,
  years = FALSE,
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> dur() 
humData |> dur(Token)

dur(
  x,
  ...,
  BPM = "*M60",
  minutes = FALSE,
  hours = FALSE,
  days = FALSE,
  months = FALSE,
  years = FALSE,
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)
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

- minutes:

  (`logical`, T/F) Should the `dur` output include minutes?

- hours:

  (`logical`, T/F) Should the `dur` output include hours?

- days:

  (`logical`, T/F) Should the `dur` output include days?

- months:

  (`logical`, T/F) Should the `dur` output include months?

- years:

  (`logical`, T/F) Should the `dur` output include years?

## Details

These functions require a `BPM` (beats-per-minute) argument to be
specified. By default, the value is 60 bpm.

## dur

The [\*\*dur](https://www.humdrum.org/rep/dur/index.html) output can be
modified to include different clock-time units: The `minutes`, `hours`,
`days`, `months`, and `years` arguments are all true/false `logical`
arguments, indicating whether to use that unit in the output (all
default to `FALSE`). For example, if `minutes = FALSE`, an input of 90
seconds will return `":90"` (90 seconds!), but if `minutes = TRUE`, the
output will be `:1:30` (one minute and thirty seconds).

## See also

To better understand how this function works, read about the [family of
rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
or how rhythms are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).

Other time functions:
[`bpm2sec()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bpm2sec.md)

## Examples

``` r
tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')

seconds(tokens)
#> **seconds (numeric)
#> [1] 1.50 0.50 0.25 0.25 0.25 0.25 0.00 1.00
seconds(tokens, BPM = '90')
#> **seconds (numeric)
#> [1] 1.0000000 0.3333333 0.1666667 0.1666667 0.1666667 0.1666667 0.0000000
#> [8] 0.6666667
ms(tokens, BPM = '90')
#> **ms (numeric)
#> [1] 1000.0000  333.3333  166.6667  166.6667  166.6667  166.6667    0.0000
#> [8]  666.6667
dur(tokens)
#> **dur (character)
#> [1] :1.500 :0.500 :0.250 :0.250 :0.250 :0.250 :0     :1    

wholenotes <- c(10, 30, 90, 310, 5000)
dur(wholenotes)
#> **dur (character)
#> [1] :40    :120   :360   :1240  :20000
dur(wholenotes, minutes = TRUE)
#> **dur (character)
#> [1] :40     :2:0    :6:0    :20:40  :333:20
dur(wholenotes, minutes = TRUE, hours = TRUE)
#> **dur (character)
#> [1] :40     :2:0    :6:0    :20:40  5:33:20

quarternotes <- c(10, 30, 90, 310, 5000)
dur(quarternotes, minutes = TRUE, BPM = '120', parse(unit = '4'))
#> **dur (character)
#> [1] :5     :15    :45    :2:35  :41:40

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpEuDntc/temp_libpathb11a7693ac44/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!

with(B075[[,3:4]], seconds(Token))
#> **seconds (numeric)
#>  [1] 1.0 2.0 2.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1.0 1.0 1.0 1.0
#> [20] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.0 0.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5
#> [39] 0.5 0.5 0.5 1.0 1.0 1.0

```
