# Extract scale step.

This is equivalent to using any [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
with the arguments `generic = TRUE`, `simple = TRUE`, and
`step.labels = NULL`. By default, `step()` will returns steps relative
to the key—set `Key = NULL` if you don't want this.

## Usage

``` r
step(
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
[`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md),
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md),
[`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md),
[`tonh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonh.md)

Other partial pitch functions:
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md),
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md)

## Examples

``` r
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/BachChorales/chor001.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
chorale[[20:30,]]
#> ######################## vvv chor001.krn vvv #########################
#>     8:           **kern         **kern         **kern         **kern
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:              4GG             4B             4d             4g
#>    21:               =1             =1             =1             =1
#>    22:               4G             4B             4d             2g
#>    23:               4E            8cL             4e              .
#>    24:                .            8BJ              .              .
#>    25:              4F#             4A             4d            4dd
#>    26:               =2             =2             =2             =2
#>    27:               4G             4G             2d            4.b
#>    28:               4D            4F#              .              .
#>    29:                .              .              .             8a
#>    30:               4E             4G             4B             4g
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>          *Token :: character
#> 

within(chorale[[20:30,]], step(Token))
#> ######################## vvv chor001.krn vvv #########################
#>     8:           **step         **step         **step         **step
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:                1              3              5              1
#>    21:               =1             =1             =1             =1
#>    22:                1              3              5              1
#>    23:                6              4              6              .
#>    24:                .              3              .              .
#>    25:                7              2              5              5
#>    26:               =2             =2             =2             =2
#>    27:                1              1              5              3
#>    28:                5              7              .              .
#>    29:                .              .              .              2
#>    30:                6              1              3              1
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>           Token                  :: character
#>          *humdrumR:::step(Token) :: integer (**step tokens)
#> 

within(chorale[[20:30,]], step(Token, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B')))
#> ######################## vvv chor001.krn vvv #########################
#>     8:           **step         **step         **step         **step
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:                C              E              G              C
#>    21:               =1             =1             =1             =1
#>    22:                C              E              G              C
#>    23:                A              F              A              .
#>    24:                .              E              .              .
#>    25:                B              D              G              G
#>    26:               =2             =2             =2             =2
#>    27:                C              C              G              E
#>    28:                G              B              .              .
#>    29:                .              .              .              D
#>    30:                A              C              E              C
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>           Token                :: character
#>          *humdrumR:::step(...) :: character (**step tokens)
#> 
```
