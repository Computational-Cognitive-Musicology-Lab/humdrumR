# Generating ("deparsing") chord representations

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes an easy-to-use system for generating a variety of tertian
harmony (chord) representations, which can be flexibly modified by
users. "Under the hood" `humdrumR` represents all tonal chord
information using the [same underlying tertian set
representation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md),
which is typically extracted from input data using the [chord
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md).
This representation can then be "deparsed" into a variety of predefined
output formats (like `**harm`), or into new formats that you create!

## Details

Deparsing is the second step in the [chord
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md)
processing pipeline:

- **Input** representation `|>`

  - *Parsing* `|>`

    - **Intermediate**
      ([tertianSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md))
      representation `|>`

    - **Transformation** `|>`

  - *Deparsing* (DEPARSING ARGS GO HERE) `|>`

- **Output** representation

Various pitch representations can be generated using predefined [chord
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md)
like
[`chord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md)
[`tertian()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertian.md),
and `roman()`. All of these functions use a common deparsing framework,
and are specified using different combinations of arguments to the
deparser. By modifying these *"deparsing" arguments*, you can exercise
fine control over how you want pitch information to be represented in
your output.

## See also

All `humdrumR` [chord
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md)
make use of the deparsing functionality.
