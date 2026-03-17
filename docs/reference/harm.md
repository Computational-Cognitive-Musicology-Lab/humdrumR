# Roman numeral representations of harmony

These functions output [roman
numeral](https://en.wikipedia.org/wiki/Roman_numeral_analysis)
representations of a tertian harmony. The `**harm` representation is the
most widely used standard for roman numeral notation in humdrum data.
Unlike traditional roman numerals, `**harm` does not indicate inversions
with figuration. It uses lowercase letters (`a`, `b`, `c`, etc.)
instead. The `roman` function however does output (relatively)
traditional figures.

The output format of `roman()` is very similar to `**harm`. The main
difference is that inversions are indicated using traditional figures ,
like `653`, instead of `**harm`'s simpler system (using letters). So,
for example, if we take the input `E7/B` in the key of A major, we'll
get:

## Usage

``` r
# Default S3 method
harm(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)

# Default S3 method
roman(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)

# S3 method for class 'humdrumR'
roman(x, ...)

roman(
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

`harm('E7/B', Key = 'A:')` =\> `"V7c"` `roman('E7/B', Key = 'A:')` =\>
`"V643"`

## Examples

``` r
tertian <- c('AM', 'AMm/3', 'DM', 'Dm', 'B-MM', 'AM/5', 'EMmm')

harm(tertian, Key = 'A:')
#> [1] "I"    "Im7b" "IV"   "iv"   "-II7" "Ic"   "Vm9" 
roman(tertian, Key = 'A:')
#> [1] "I"     "I6m53" "IV"    "iv"    "-II7"  "I641"  "Vm9"  

B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpEuDntc/temp_libpathb11a7693ac44/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!

with(B075[[ , 2]], harm(Token))
#> [1] "I"   "I"   "V7b" "I"   "IV"  "V7"  "I"  
with(B075[[ , 2]], roman(Token))
#> [1] "I"    "I"    "V653" "I"    "IV"   "V7"   "I"   
```
