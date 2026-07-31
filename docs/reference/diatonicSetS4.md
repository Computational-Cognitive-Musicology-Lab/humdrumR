# Tonal (diatonic) sets

`diatonicSet` is one of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)'s
types of tonal data, representing Western diatonic keys. For the most
part, users should not need to interact with diatonicSets
directly—rather, diatonicSets work behind the scene in numerous
`humdrumR` pitch functions. See the keyRepresentations and
[keyFunctions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
documentation for details of usage and functionality or the *Tonality in
humdrumR* vignette for a detailed explanation of the theory and
specifics of diatonicSets.

## Usage

``` r
diatonicSet(...)

dset(root = 0L, signature = root, alterations = 0L)

is.diatonicSet(x)

order.diatonicSet(
  x,
  ...,
  parallel = TRUE,
  na.last = TRUE,
  decreasing = FALSE,
  method = c("auto", "shell", "radix")
)

# S4 method for class 'diatonicSet,diatonicSet'
e1 == e2

# S4 method for class 'diatonicSet,diatonicSet'
Compare(e1, e2)

# S4 method for class 'tertianSet,tertianSet'
e1 == e2
```

## Details

`diatonicSet` is a [S4](https://adv-r.had.co.nz/S4.html) subclass of
`humdrumR`'s virtual class
[struct](https://humdrumR.ccml.gtcmt.gatech.edu/reference/struct.md),
from which it inherits a lot of useful "vector-like"
behaviors/functionality.

The constructor function `dset` can be used to create `diatonicSets`
directly. The three arguments corespond to the three slots: `root`,
`mode`, and `alteration`. All inputs will be coerced to match in length.
The `root` argument will attempt to coerce character strings to
[tonalIntervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md),
and use their `LO5th` value as the root.

By default, the [as.character](https://rdrr.io/r/base/character.html)
method, and thus (via
[struct](https://humdrumR.ccml.gtcmt.gatech.edu/reference/struct.md))
the [show](https://rdrr.io/r/methods/show.html) method, for diatonicSets
call
[key()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md).
Thus, if you return a `diatonicSet` on the command line (or
[print](https://rdrr.io/r/base/print.html) one), you'll see the [key
interpretation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
representation printed.

## Slots

- `Root`:

  integers representing the root of the key on the line-of-fifths

- `Signature`:

  integers representing the signature (number of accidentals) of the
  key.

  A key is represented by two integers, `Root` and `Signature`. Root is
  simply the tonic note of the key on the circle of fifths. Signature is
  a value on the circle of fifths, indicating the diatonic mode. You can
  think of the `Signature` value as indicating the number of
  accidentals, with negative numbers for flats and positive numbers for
  sharps. You can also think of the signature as indicating how much the
  "natural key" (C major) is slid up and down the line-of-fifths. The
  [traditional diatonic
  modes](https://en.wikipedia.org/wiki/Mode_(music)) of Western music
  occur wherever `Signature - Tonic` is in the range `-5:1`:

  - Signature - Tonic = +1 =\> Lydian

  - Signature - Tonic = +0 =\> Major (Ionian)

  - Signature - Tonic = -1 =\> Mixolydian

  - Signature - Tonic = -2 =\> Dorian

  - Signature - Tonic = -3 =\> Minor (Aeolian)

  - Signature - Tonic = -5 =\> Locrian

  - Signature - Tonic = -4 =\> Phyrgian

  *Note that you can make diatonicSets where the `Root` is outside the
  `Key`. This is unusual, and may result in sets you wouldn't predict.*

- `Alteration`:

  integers representing alterations of the diatonic set (details below).

## Alterations

The `Alteration` slots (also integer) can be used to represent various
"altered" scales. The integer values are interpreted as a seven-trit
[balanced ternary](https://en.wikipedia.org/wiki/Balanced_ternary)
string. ("trits" are the ternary equivalent of binary "bits.") Balanced
ternary allows for three digits, `0` (unaltered degree), `1` (sharpened
degree), and `-1` (flattened degree). The seven trits correspond to the
seven scale degrees on the line-of-fifth indicated by the
*signature*—i.e., ordered from lowest to hightest on the line-of-fifths,
not relative to the root. (For instance, when `Signature == 0`, the
degrees are `c(-1, 0, 1, 2, 3, 4, 5)`.)

The ternary arrangement maps powers of three to each scale degree, as so
that in the `Alteration` integer:

- ± 1: raise or flatten the **7th** scale degree.

- ± 3: raise or flatten the **3rd** scale degree.

- ± 9: raise or flatten the **6th** scale degree.

- ± 27: raise or flatten the **2nd** scale degree.

- ± 81: raise or flatten the **5th** scale degree.

- ± 243: raise or flatten the **1st** scale degree.

- ± 729: raise or flatten the **4th** scale degree.

For example, consider `Alteration == 26`: In a balanced ternary
representation, the decimal integer 26 is represented as
`1 0 0 1 0 -1 0`. (In other words 1 in the "27s place" and -1 in the
"ones place"—i.e., 27 - 1). This represents a raised 2nd (the 27) and a
lowered 7th (the -1).

The `Alteration` integer allows us to concisely represent all the 2,187
possible combinations of raised and lowered diatonic scale degrees!
However, combined with the `Signature` slot, there is some redundancy in
scale representation. For example, a melodic minor scale can be
represented as a major scale (`Signature - Root == 0`) with a lowered
third degree (`Alteration == -3`) *or* as minor scale
(`Signature - Root == -3`) with raised 6ths and 7ths
(`Alteration == 10`). However, though these two representations result
in the same set on the line-of-fifths, some might consider them to be
conceptually different in some contexts, so we consider the redundancy
acceptable. Another case of encoding redundancy *is* that
`Alteration - 1` (flatten the 7th) is exactly equivalent to
`Signature - 1`. Similarly, `Alteration + 729` (raise the 4th) is
exactly equivalent to `Signature + 1`.

Double-flat and double-sharp degrees are **not** encodable in
`diatonicSet`. However, in combination with the `Signature` slot, sets
with double-flat/sharps (like doubly-diminished 7ths) can be encoded.

## Arithmetic

Arithmetic between diatonicSets is not defined. However, a number of
useful arithmetic operations between diatonicSets and other data types
*are* defined:

XXXX Elaborate XXXX Need to implement special logic for adding
Alterations! (Taking into account Signature addition.)

## Relational Operators

diatonicSets can be compared using the standard [relational
operations](https://rdrr.io/r/base/Comparison.html) `==`, and `!=`. Two
diatonicSets are equal (according to `==`) only if all their slots
(`Root`, `Signature`, and `Alteration`) are exactly identical. Ordinal
comparisons (e.g., `>`, `<=`) between diatonicSets are on their
`Signature` only.

## Coercion

`humdrumR` knows how to
[coerce](https://en.wikipedia.org/wiki/Type_conversion) several [base-R
atomic types](https://rdrr.io/r/base/vector.html) into diatonicSets.
This can be done using the [as](https://rdrr.io/r/methods/as.html)
function—e.g., `as(3, "diatonicSet")`—or more intuitively using the
function
[`diatonicSet()`](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html).
Coercision methods are defined for

- [integer](https://rdrr.io/r/base/integer.html): interpreted as root of
  major key

- [numeric](https://rdrr.io/r/base/numeric.html): rounded to nearest
  integer and intepreted as root of major key

- [character](https://rdrr.io/r/base/character.html): interpreted using
  `humdrumR`'s [regular expression dispatch
  system](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md),
  as explained fully
  [here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md).

## See also

The main way to create `diatonicSet` S4 objects is with the
[`diatonicSet()`](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html)
pitch parser.

Other Tonal S4 classes:
[`tertianSetS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md),
[`tonalIntervalS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
