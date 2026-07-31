# "Pop/Jazz" chord symbols

These functions outputs jazz/pop-style chord symbols. There is no
universal standard for how to notate such chord symbols, in particular
in plain text. The `chord()` function outputs a chord symbol
representation roughly consistent with "standard practices."

## Usage

``` r
# Default S3 method
chord(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)

# Default S3 method
harte(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)
```

## Arguments

- x:

  ***An `atomic` vector.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vectors

- Key:

  ***The diatonic key used by the parser, deparser, and transposer.***

  Defaults to `NULL`, which is interpreted as C major.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`.

- transposeArgs:

  ***An optional list of arguments passed to a
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  call.***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).

- parseArgs:

  ***An optional list of arguments to the [chord
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [chord
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md).

- inPlace:

  ***Should non-chord information be retained in the output string.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

## Details

For more rigorous, consistent work, we recommend the
[Harte](https://github.com/Computational-Cognitive-Musicology-Lab/Star-Wars-Thematic-Corpus)
notation, which is the standard used by MIREX, etc. The
[`harte()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harte.md)
function will output standard Harte symbols.

## Examples

``` r
romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')

chord(romanNumerals)
#> [1] "C"      "Fmaj7"  "G"      "Amin"   "Ab"     "Ddim7"  "Gb7/Fb"
chord(romanNumerals, Key = 'A:')
#> [1] "A"      "Dmaj7"  "E"      "F#min"  "F"      "Bdim7"  "Eb7/Db"

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/Rtmpjdf00d/temp_libpath10ef5c6bd813c9/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
with(B075[[ , 2]], chord(Token))
#> [1] "F"    "F"    "C7/E" "F"    "Bb"   "C7"   "F"   
```
