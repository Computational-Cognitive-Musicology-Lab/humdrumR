# Reciprocal representation of duration

The standard approach to representing conventional note values in
humdrum is the "reciprocal"
[\*\*recip](https://www.humdrum.org/rep/recip/index.html)
representation. The `**recip` rhythmic values are often used as a part
of `**kern` representation, which also includes
[pitch](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
information and notation details.

If `recip()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
recip(
  x,
  ...,
  sep = "%",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

## humdrumR S3 method:
humData |> select(Token) |> recip() 
humData |> recip(simple = TRUE)
humData |> recip(Token, Key = Key)

recip(
  x,
  ...,
  sep = "%",
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

- sep:

  ***The separator printed between numerator and denominator.***

  A single `character` string.

  Must be a single `character` string.

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

`**recip` values are literally the reciprocal of a duration value. Since
most note values in conventional music notation are simple fractions,
the reciprocal approach is highly concise and very similar to
conventional western notation and terminology. A "quarter note" is
represented as the reciprocal of 1/4: simply `"4"`. Full reciprocal
fractions can be specified: `"2%3"` to indicate 3/2. The `%` separator
can be changed using the `sep` argument.

As in conventional [note
values](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md),
"dots" can be added after a value to increase the duration by the ratio
of `(2 - (2^{-n}))`, where `n` is the number of dots. (One dot is 3/2;
two dots is 7/4; etc.).

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
[`notehead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md)

## Examples

``` r
tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')

recip(tokens)
#> **recip (character)
#> [1] 4.  8   16  16  16  16  1%0 4  
recip(tokens, parse(grace = NA))
#> **recip (character)
#> [1] 4. 8  16 16 16 16 .  4 
recip(tokens, scale = 2)
#> **recip (character)
#> [1] 2.  4   8   8   8   8   1%0 2  

recip(c(.25, .25, .25, .25, .5, 1))
#> **recip (character)
#> [1] 4 4 4 4 2 1
recip(c(.25, .25, .25, .25, .5, 1), parse(unit = '4'))
#> **recip (character)
#> [1] 16 16 16 16 8  4 

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/Rtmpjdf00d/temp_libpath10ef5c6bd813c9/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
within(B075, subset = Spine > 2,  recip(Token))
#> ###################### vvv B075_00_05_a.krn vvv ######################
#>    1:  !!!COM: Beethoven
#>    2:  !!!OTL: 7 Variations on a Quartet by Winter
#>    3:  !!!Variation: Theme e
#>    4:      ****recip    ****recip       ****recip           ****recip
#>    5:              .            .               .                   .
#>    6:              .            .               .                   .
#>    7:          *M2/4        *M2/4           *M2/4               *M2/4
#>    8:              .            .               .                   .
#>    9:          FALSE       FALSE4           TRUE4        TRUE TRUE4 4
#>   10:            =37          =37             =37                 =37
#>   11:          FALSE       FALSE2    TRUE TRUE2 2        TRUE TRUE8 8
#>   12:              .            .               .        TRUE TRUE8 8
#>   13:              .            .               .        TRUE TRUE8 8
#>   14:              .            .               .        TRUE TRUE8 8
#>   15:            =38          =38             =38                 =38
#>   16:              .            .               .    TRUE TRUE1%0 1%0
#>   17:          FALSE       FALSE4    TRUE TRUE4 4        TRUE TRUE4 4
#>   18:          FALSE       FALSE4    TRUE TRUE4 4        TRUE TRUE4 4
#>   19:            =39          =39             =39                 =39
#>   20:          FALSE       FALSE4    TRUE TRUE8 8        TRUE TRUE8 8
#>   21:              .            .    TRUE TRUE8 8        TRUE TRUE8 8
#>   22:          FALSE       FALSE4    TRUE TRUE8 8        TRUE TRUE8 8
#>   23:              .            .    TRUE TRUE8 8        TRUE TRUE8 8
#>   24:            =40          =40             =40                 =40
#>   25:          FALSE       FALSE2           TRUE4        TRUE TRUE4 4
#>   26:              .            .           TRUE4               TRUE4
#>   27:             *-           *-              *-                  *-
#> ###################### ^^^ B075_00_05_a.krn ^^^ ######################
#> 
#>    Data fields: 
#>           Token                   :: character
#>          *humdrumR:::recip(Token) :: character (**recip tokens)
#>          *subset                  :: logical
#> 

with(B075, subset = Spine > 2, recip(Token)) |> table()
#> 
#> 1%0   8   4   2   1 
#>   2  24  21   4   0 
```
