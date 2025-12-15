# Test the properties of tonal information

These functions test basic properties of pitch information. `is.simple`
returns `TRUE` if pitch information is constrained in one octave.
`is.generic` returns `TRUE` if pitch information is "natural" to the key
(`Key`) argument.

## Usage

``` r
is.simple(x, ...)

# S3 method for class 'tonalInterval'
is.simple(x, octave.round = floor, ...)

# Default S3 method
is.simple(x, ...)

is.generic(x, Key, ...)

# S3 method for class 'tonalInterval'
is.generic(x, Key = NULL)
```

## Arguments

- x:

  ***Pitch information.***

  Must be something that can be [parsed as pitch
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- ...:

  ***Parameters passed to
  [`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).***

- octave.round:

  ***The rounding function.***

  Must be a [rouding
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expand.md).

  Controls how simple intervals are interpreted relative to C.

- Key:

  ***The onic key used to defined generic pitches.***

  Defaults to `NULL`.

  Must be something that can be [parsed as a onic
  key](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md);
  must be either length `1` or `length(x)`.

## Details

These functions can be called directly on
[tonalIntervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md);
If called on anything else, the functions first calls the
[`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
parser. If any values fail to parse `NA` is returned.

"Simple" intervals fall in a particular octave relative to
middle-C/unison. The `octave.floor` argument can be used to change how
this works: The default option, `floor`, interprets the (\*\*kern)
pitches `c`, `d`, `e`, `f`, `g`, `a`, and `b` to be "simple." The most
common alternative, `round`, identifies `G`, `A`, `B`, `c`, `d`, `e`,
and `f` as "simple." See the [pitch deparsing
docs](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)
for a more detailed explanation.

"Generic" intervals belong to a key.

## See also

Other Tonal feature functions:
[`is.major()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.major.md)
