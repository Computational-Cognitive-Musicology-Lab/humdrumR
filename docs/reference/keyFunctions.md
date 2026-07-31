# Parsing and deparsing key information

These functions can be used to extract and "translate," or otherwise
modify, data representing diatonic key information. The functions are:

## Arguments

- x:

  ***Input data, interpreted as diatonic keys.***

  Must be an `atomic` vector.

- Key:

  ***The key used by the parser, deparser, and transposer.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

- parseArgs:

  ***An optional list of arguments passed to the [key
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [key
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md).

## Details

- [`key()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/key.md)

- [`romanKey()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/romanKey.md)

- [`signature()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/signature.md)

## See also

To better understand how these functions work, read about how diatonic
keys are
[represented](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html),
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md),
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyDeparsing.md).
