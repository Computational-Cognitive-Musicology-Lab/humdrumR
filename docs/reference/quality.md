# Extract quality from pitch

Use this if you want to extract *only* the tonal qualities from pitch
data, discarding octave and step information.

## Usage

``` r
quality(
  x,
  ...,
  generic = FALSE,
  simple = FALSE,
  octave.relative = FALSE,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  gamutArgs = list(),
  inPlace = FALSE
)
```

## Arguments

- x:

  ***Input data to parse as pitch information.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  or `NULL`.

- ...:

  ***Arguments passed to the [pitch
  deparser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).***

  There are also two hidden (advanced) arguments you can specify:
  `memoize` and `deparse` (see the details below).

- generic:

  ***Should "specific" pitch information (accidentals and qualites) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- simple:

  ***Should "compound" pitch information (octave/contour) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- Key:

  ***The input `Key` used by the parser, deparser, and transposer.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

- transposeArgs:

  ***An optional list of arguments passed to a special
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  call.***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).

- parseArgs:

  ***An optional list of arguments passed to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- inPlace:

  ***Should non-pitch information be retained in the output string.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  This argument only has an effect if the input (the `x` argument) is
  `character` strings, *and* there is extra, non-pitch information in
  the input strings "besides" the pitch information. If so, and
  `inPlace = TRUE`, the output will be placed into an output string
  beside the original non-pitch information. If `inPlace = FALSE`, only
  the pitch output information will be returned (details below).

## See also

Other pitch functions:
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md),
[`bhatk()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bhatk.md),
[`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md),
[`freq()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/freq.md),
[`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md),
[`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md),
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
[`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md),
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`pc()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pc.md),
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md),
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md),
[`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md),
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md),
[`tonh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonh.md)

Other partial pitch functions:
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md),
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md)

## Examples

``` r
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpEuDntc/temp_libpathb11a7693ac44/humdrumR/HumdrumData/BachChorales/chor001.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!

within(chorale[[20:30,]], quality(Token))
#> ######################## vvv chor001.krn vvv #########################
#>     8:        **quality      **quality      **quality      **quality
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:                P              M              P              P
#>    21:               =1             =1             =1             =1
#>    22:                P              M              P              P
#>    23:                M              P              M              .
#>    24:                .              M              .              .
#>    25:                M              M              P              P
#>    26:               =2             =2             =2             =2
#>    27:                P              P              P              M
#>    28:                P              M              .              .
#>    29:                .              .              .              M
#>    30:                M              P              M              P
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>           Token                     :: character
#>          *humdrumR:::quality(Token) :: character (**quality tokens)
#> 

# Harmonic interval qualities:

within(chorale[[20:30,]], hint(Token, deparser = quality))
#> ######################## vvv chor001.krn vvv #########################
#>     8:        **quality      **quality      **quality      **quality
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:             [GG]              M              m              P
#>    21:               =1             =1             =1             =1
#>    22:              [G]              M              m              P
#>    23:              [E]              m              M              .
#>    24:                .            [B]              .              .
#>    25:             [F#]              m              P              P
#>    26:               =2             =2             =2             =2
#>    27:              [G]              P              P              M
#>    28:              [D]              M              .              .
#>    29:                .              .              .            [a]
#>    30:              [E]              m              M              m
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>           Token                                      :: character
#>          *humdrumR:::hint(Token, deparser = quality) :: character (**quality tokens)
#> 
with(chorale[[20:30,]], hint(Token, deparser = quality, incomplete = NA, bracket = FALSE)) |> table()
#> 
#> P A M m 
#> 6 0 6 6 
```
