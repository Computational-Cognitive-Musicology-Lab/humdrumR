# Tertian set

`tertianSet` is one of
[humdrumR's](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
types of tonal data, representing Western tertian harmonies.
`tertianSet` is a subclass of `diatonicSet` (and hence, `struct`).

This functions a generic form of tertian harmony representation,
commonly used in music theory. In this representation, the root of a
chord is indicated as \*\*kern, followed by one or more quality
indicators, like `"CMM"` (C major seventh).

## Usage

``` r
tset(
  root = 0L,
  signature = 0L,
  alterations = 0L,
  cardinality = 3L,
  extension = NULL,
  inversion = 0L
)

is.tertianSet(x)

# Default S3 method
tertian(
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

The only structural addition, compared to `diatonicSet`, is the
`Extensions` slot. This slot indicates which tertian chord members are
active in the chord. There are seven possible chord members: the root,
third, fifth, seventh, ninth, eleventh, and thirteenth. Every possible
combination of these seven degrees is represented by a single integer,
corresponding to the 7-bit representation of on/offs on the seven
degrees in reverse order (13, 11, 9, 7, 5, 3, 1). For example, the
integer `15` corresponds to a seventh chord: in binary, 15 is `0001111`.
The initial three zeros indicate that the 13th, 11th, and 9th are *not*
part of the harmony, while the four ones indicate that the root, third,
fifth, and seventh *are* part of the harmony. Ultimately, adding or
removing a chord degree from a harmony can be achieved by adding the
power of two associated with that degree:

- **Root**: \\\pm 1\\

- **Third**: \\\pm 2\\

- **Fifth**: \\\pm 4\\

- **Seventh**: \\\pm 8\\

- **Ninth**: \\\pm 16\\

- **Eleventh**: \\\pm 32\\

- **Thirteenth**: \\\pm 64\\

`tertianSet` has many specific methods defined for reading/writing
harmonic information.

The first quality after the root indicates the quality of the triad.
Subsequent qualities, if present, indicate the quality of the 7th, 9th,
11th, and 13th respectively. Some examples:

- `M`: major triad

- `Mm`: dominant-seventh chord

- `MM`: major-seventh chord

- `Mmm`: dominant-seventh-with-flat-9 chord.

- `oo`: fully-diminished-seventh chord.

Missing extensions can be indicated in their position using `.`. For
example, `E-Mm.P` indicates a E-flat dominant-11th chord with no 9th.
Missing members of the triad can be indicated by specifying either `5`
or `3` immediately after the root, but before any quality indicators.
For example, `C5M` indicates a C major chord with no 3rd, while `G3mm`
indicates a G-minor-seventh chord with missing 5th.

The default quality indicators are `P` (perfect), `M` (major), `m`
(minor), `o` (diminished), or `+` (augmented), but these can be
overridden by calls to their respective arguments: for example,
`tertian('Cdim', diminish = 'd')`.

## Inversions

Inversions are indicated with slash notation, with the scale degree to
the right of the slash. For example, a first-inversion A major chord
would be `AM/3`.

## See also

The main way to create `tertianSet` S4 objects is with the
[`tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
pitch parser.

Other Tonal S4 classes:
[`diatonicSetS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md),
[`tonalIntervalS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)

## Examples

``` r
romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')

tertian(romanNumerals)
#> [1] "CM"   "FMM"  "GM"   "Am"   "AbM"  "Dom"  "GbMm"
tertian(romanNumerals, Key = 'A:')
#> [1] "AM"   "DMM"  "EM"   "F#m"  "FM"   "Bom"  "EbMm"

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
with(B075[[,2]], tertian(Token))
#> [1] "FM"  "FM"  "CMm" "FM"  "BbM" "CMm" "FM" 
```
