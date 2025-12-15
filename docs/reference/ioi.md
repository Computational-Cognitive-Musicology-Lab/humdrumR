# Sum "connected" durations

These functions are used to sum (melodically) adjacent rhythmic duration
values which are not associated with new onsets/attacks. `ioi()` adds
the duration of [rests](https://en.wikipedia.org/wiki/Rest_(music)) to
the previous non-rest (onset) duration, to create [interonset
intervals](https://en.wikipedia.org/wiki/Time_point#Interonset_interval)
(IOIs). `sumTies()` sums
[tied](https://en.wikipedia.org/wiki/Tie_(music)) durations.

If `ioi()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `sumTies()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
ioi(
  x,
  onsets = !grepl("r", x) & !is.na(x) & x != ".",
  ...,
  finalOnset = FALSE,
  groupby = list(),
  parseArgs = list(),
  Exclusive = NULL,
  inPlace = TRUE
)

humData |> select(Token) |> ioi() 
humData |> ioi(Token)

ioi(
  x,
  onsets = !grepl("r", x) & !is.na(x) & x != ".",
  ...,
  finalOnset = FALSE,
  groupby = list(),
  parseArgs = list(),
  Exclusive = NULL,
  inPlace = TRUE
)

# Default S3 method
sumTies(x, open = "[", close = "]", ..., groupby = list(), inPlace = TRUE)

humData |> select(Token) |> sumTies() 
humData |> sumTies(Token)

sumTies(x, open = "[", close = "]", ..., groupby = list(), inPlace = TRUE)
```

## Arguments

- x:

  ***Input rhythm information.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or `NULL`.

  Must be [parsable as rhythm
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- onsets:

  ***A `logical` vector denotes the onsets.***

  Defaults to a `logical` vector with `TRUE` wherever there are rests,
  indicated by the presence of an `"r"` character, in the input `x`.

  Must be `logical`; must be length `length(x)`.

  All durations in `x` where `onsets == FALSE` are added to the previous
  value where `onsets == TRUE`.

- finalOnset:

  ***Whether to count the last onset.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the last IOI is computed between the last onset and the end
  of the input vector. Otherwise, this last IOI is undefined (`NA`).

- groupby:

  ***A `list` of vectors to group `x`.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

- parseArgs:

  ***An optional list of arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- inPlace:

  ***Should non-rhythm information be retained in the output string?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- open:

  ***How are the beginnings of ties indicated in `x`?***

  Defaults to `[`.

  Must be a single, non-empty `character` string, interpreted as a
  regular expression, or a `logical` of the same length as `x`.

- close:

  ***How are the ends of ties indicated in `x`?***

  Defaults to `]`.

  Must be a single, non-empty `character` string, interpreted as a
  regular expression, or a `logical` of the same length as `x`.

## Details

Both functions return "collapsed" durations as null data tokens. For
example, `sumTies(c('[4a', '4a]', '2g'))` returns `c('2a', '.', '2g')`,
with the second (tied) duration null (`"."`).

For interonset intervals, the last duration in a string of durations is
undefined—there is a final onset, but no *next* onset, so there can't
really be a "interonset" interval. Thus, by default, `ioi()` will return
`NA` at the location of the final duration. However, if the `finalOnset`
argument is set to `TRUE`, the function will act like there is one
additional onset *after* the end of the sequence: the last "IOI" is
calculated between the last onset and this fictional "final onset." For
example, if we run `ioi(c('4.a','8r', '4.a','8r','2a', '2r'))` the
result is `c("2a", ".", "2a", ".", NA, ".")`, with the last onset (`2a`)
returning `NA`. However, if we run
`ioi(c('4.a','8r', '4.a','8r','2a', '2r'), finalOnset = TRUE)` the
result is `c("2a", ".", "2a", ".", "1a", ".")`—the last onset's whole
duration through the end is returned!

Non-onsets (rests) that occur *before* the first onset are returned as
null.

## Examples

``` r
tokens_withrests <- c('2c', '4d', '4e', '4r', '8f','8d#', '4r', '4e')

ioi(tokens_withrests)
#> [1] "2c"   "4d"   "2e"   "."    "8f"   "4.d#" "."    NA    
ioi(tokens_withrests, finalOnset = TRUE)
#> [1] "2c"   "4d"   "2e"   "."    "8f"   "4.d#" "."    "4e"  

tokens_withties <- c('2c', '4d', '[4e', '4e]', '8f','[8d#', '4d#]', '4e')

sumTies(tokens_withties) 
#> [1] "2c"   "4d"   "2e"   "."    "8f"   "4.d#" "."    "4e"  
```
