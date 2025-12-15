# Test the major/minor modality of a set

These functions test the majorness/minorness of a tertian or diatonic
set, a logical `TRUE`/`FALSE`. These functions are not testing whether a
chord is strictly a major or minor chord, but rather a "broad"
major/minorness: generally, the presence of a minor third degree makes a
set "minor"; thus, a diminished chord is "minor" and the lydian key is
"major."

## Usage

``` r
is.major(x, ...)

is.minor(x, ...)

# S3 method for class 'diatonicSet'
is.major(x)

# S3 method for class 'diatonicSet'
is.minor(x)
```

## Arguments

- x:

  ***Input data, interpreted as diatonic keys or chords.***

  Must be a `diatonicSet` or `tertianSet` or something that can be
  parsed as one.

- ...:

  ***Parameters passed to the parsers
  ([`tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
  and
  [`diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html)).***

## Details

Either function can be called directly on
[tertian](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
or
[diatonic](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
sets. If called on anything else, the functions first call the
[`tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
parser. If any values fail to parse (returning `NA`), the
[`diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html)
parser is called on them.

## See also

Other Tonal feature functions:
[`is.simple()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.simple.md)
