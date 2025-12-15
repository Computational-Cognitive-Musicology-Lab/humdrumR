# Line of Fifths

The function `LO5th` is a S3-generic function with methods to extract
the "line-of-fifths" value from various pitch objects and
representations.

## Usage

``` r
LO5th(x, generic = FALSE, ...)
```

## Value

Returns an integer vector or array, matching the input.

## Details

### The Line of Fifths

Every interval in Western music is associated with an integer on the
line of fifths:

- Bb = m7 = -2

- F = P4 = -1

- C = P1 = 0

- G = P5 = 1

- D = M2 = 2

- A = M6 = 3

- E = M3 = 4

- B = M7 = 5

- F# = A4 = 6

- etc.

The natural notes of (C) major scale—which we also call the *generic
intervals*—fall in the range `-1:5`. In fact, any onic key is a block of
seven consecutive numbers of the line-of-fifths: for example, Eb major
is `-4:2`. "Sharps" and "flats" represent `+7` or `-7` on the
line-of-fifths respectively.

## See also

`LO5th`s are a core component of the
[tonalIntervalS4](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
representation, and are thus an argument to the `tonalInterval`
constructor: [tint(octave,
LO5th)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md).
Also, see the
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
function, which is used by many `LO5th` methods.

Other core pitch representations:
[`tonalIntervalS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
