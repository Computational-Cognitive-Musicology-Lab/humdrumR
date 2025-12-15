# Parsing key information

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes an easy-to-use but powerful system for *parsing* diatonic key
information: various basic key representations (including `numeric` and
`character`-string representations) can be "parsed"—read and interpreted
by `humdrumR`. For the most part, parsing automatically happens "behind
the scenes" whenever you use any humdrumR [key
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md),
like [`key()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/key.md)
or
[`signature()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/signature.md).

## Usage

``` r
diatonicSet(...)

# S3 method for class 'diatonicSet'
diatonicSet(x, ...)

# S3 method for class 'logical'
diatonicSet(x, ...)

# S3 method for class '`NULL`'
diatonicSet(x, ...)

# S3 method for class 'numeric'
diatonicSet(x)

# S3 method for class 'integer'
diatonicSet(x)

# S3 method for class 'character'
diatonicSet(x, ..., Key = NULL, Exclusive = NULL, multiDispatch = FALSE)
```

## See also

All `humdrumR` [key
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
make use of the deparsing functionality.
