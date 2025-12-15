# Generating ("deparsing") key representations

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes an easy-to-use system for generating a variety of diatonic key
representations, which can be flexibly modified by users. "Under the
hood" `humdrumR` represents all tonal chord information using the [same
underlying
representation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md),
which is typically extracted from input data using the [key
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md).
This representation can then be "deparsed" into a variety of predefined
output formats, or into new formats that you create!

## Details

Deparsing is the second step in the [key
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
processing pipeline:

- **Input** representation `|>`

  - *Parsing* `|>`

    - **Intermediate**
      ([diatonicSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md))
      representation `|>`

    - **Transformation** `|>`

  - *Deparsing* (DEPARSING ARGS GO HERE) `|>`

- **Output** representation

Various pitch representations can be generated using predefined [key
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
like [`key()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/key.md)
[`signature()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/signature.md),
and
[`romanKey()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/romanKey.md).
All of these functions use a common deparsing framework, and are
specified using different combinations of arguments to the deparser. By
modifying these *"deparsing" arguments*, you can exercise fine control
over how you want pitch information to be represented in your output.

## See also

All `humdrumR` [key
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
make use of the deparsing functionality.
