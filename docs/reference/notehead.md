# Note value representation of duration

This function outputs duration information in as traditional [note
value](https://en.wikipedia.org/wiki/Note_value). symbols, as in Western
notation.

If `notehead()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
notehead(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)

humData |> select(Token) |> notehead() 
humData |> notehead(Token)
humData |> notehead(simple = TRUE)

notehead(x, ..., parseArgs = list(), scale = 1, unit = 1, inPlace = FALSE)
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

## Details

Note-value symbols are simply encoded in `character` vectors, since the
[unicode character
table](https://unicode-table.com/en/blocks/musical-symbols/) includes
these musical symbols. Of course, this depends on your system having a
unicode font installed and working: the symbols might not show up
properly on your machine! In fact, the symbols always print a bit
strangely (out of alignment) and can be hard to manipulate like "normal"
`character` strings.

The note-value symbols are most useful for making the labels of plots.
For example, if you tabulate note values and use
[`barplot()`](https://rdrr.io/r/graphics/barplot.html), you get nice bar
labels:

    chorales <- readHumdrum(humdrumRroot, 'HumdrumData/Chorales/.*krn')
    with(chorales, barplot(table(notehead(Token)), cex.names = 2))

## See also

To better understand how this function works, read about the [family of
rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
or how rhythms are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).

Other rhythm functions:
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md),
[`grid()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/grid.md),
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)

## Examples

``` r
tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
notehead(tokens)
#> **notehead (character)
#> [1] 𝅘𝅥 𝅭  𝅘𝅥𝅮    𝅘𝅥𝅯    𝅘𝅥𝅯    𝅘𝅥𝅯    𝅘𝅥𝅯    .    𝅘𝅥   

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpL03I08/temp_libpath1dfb3eb89ac20/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!

with(B075[[,3:4]], notehead(Token)) |> table()
#> 
#> 𝅘𝅥𝅮  𝅘𝅥  𝅗𝅥  
#> 24 16  2 
```
