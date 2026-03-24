# Extract accidental from pitch.

Use this if you want to extract *only* the accidentals from pitch data,
discarding octave and step information. Set `explicitNaturals = FALSE`
if you don't want explicit naturals.

## Usage

``` r
accidental(
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
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md),
[`tonh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonh.md)

Other partial pitch functions:
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md),
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md)

## Examples

``` r
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
chorale[[100:110, ]]
#> ######################## vvv chor001.krn vvv #########################
#>     8:           **kern         **kern         **kern         **kern
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    52:              *>B            *>B            *>B            *>B
#>   100:              =17            =17            =17            =17
#>   101:              8EL             4B          8f#L]            4.g
#>   102:               8D              .            8eJ              .
#>   103:               8C             4c            8eL              .
#>   104:              8BB              .           8f#J             8a
#>   105:              8AA             4d             4g             4b
#>   106:             8GGJ              .              .              .
#>   107:              =18            =18            =18            =18
#>   108:              2D;            2d;           2f#;            2a;
#>   109:              [4G             4d             4g             4b
#>   110:              =19            =19            =19            =19
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>          *Token :: character
#> 

within(chorale[[100:110, ]], accidental(Token))
#> ######################## vvv chor001.krn vvv #########################
#>     8:     **accidental   **accidental   **accidental   **accidental
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    52:              *>B            *>B            *>B            *>B
#>   100:              =17            =17            =17            =17
#>   101:                n              n              n              n
#>   102:                n              .              n              .
#>   103:                n              n              n              .
#>   104:                n              .              n              n
#>   105:                n              n              n              n
#>   106:                n              .              .              .
#>   107:              =18            =18            =18            =18
#>   108:                n              n              n              n
#>   109:                n              n              n              n
#>   110:              =19            =19            =19            =19
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>           Token                        :: character
#>          *humdrumR:::accidental(Token) :: character (**accidental tokens)
#> 
```
