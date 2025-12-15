# Parsing chord information

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes an easy-to-use but powerful system for *parsing* tertian
harmony information: various basic chord representations (including
`numeric` and `character`-string representations) can be "parsed"—read
and interpreted by `humdrumR`. For the most part, parsing automatically
happens "behind the scenes" whenever you use any humdrumR [chord
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md),
like
[`harm()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harm.md)
`roman()`, or
[`chord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md).

## Usage

``` r
tertianSet(...)

# S3 method for class 'tertianSet'
tertianSet(x, ...)

# S3 method for class 'logical'
tertianSet(x, ...)

# S3 method for class '`NULL`'
tertianSet(x, ...)

# S3 method for class 'numeric'
tertianSet(x)

# S3 method for class 'integer'
tertianSet(x)

# S3 method for class 'character'
tertianSet(x, ..., Exclusive = NULL, multiDispatch = FALSE)

# S3 method for class 'factor'
tertianSet(x, Exclusive = NULL, ...)

# S3 method for class 'token'
tertianSet(x, Exclusive = NULL, ...)
```

## See also

All `humdrumR` [chord
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md)
make use of the deparsing functionality.
