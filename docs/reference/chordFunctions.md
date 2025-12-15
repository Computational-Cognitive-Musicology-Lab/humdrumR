# Parsing and deparsing chord information

These functions can be used to extract and "translate," or otherwise
modify, data representing tertian harmony information. The functions
are:

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

- parseArgs:

  ***An optional list of arguments to the [chord
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [chord
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md).

- transposeArgs:

  ***An optional list of arguments passed to a
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  call.***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).

- inPlace:

  ***Should non-chord information be retained in the output string.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

## Details

- Jazz/Pop

  - [`chord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md)

  - [`harte()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harte.md)

- Classical

  - [`figuredBass()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/figuredBass.md)

  - [`tertian()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertian.md)

  - *Roman Numerals*

    - [`harm()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harm.md)

    - `roman()`

## See also

To better understand how these functions work, read about how tertian
harmonies are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordDeparsing.md).
