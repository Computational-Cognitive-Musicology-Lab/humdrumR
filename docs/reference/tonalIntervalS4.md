# Representation of tonal pitch information

The `tonalInterval` is the core tonal pitch representation in
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md).
A `tonalInterval` is an abstract representation of tonal pitch, which
can be translated to/from all standard "concrete" pitch representations:
solfege, scientific pitch, semitones, frequencies, scale degrees,
intervals, etc. For the most part, users should not need to interact
with `tonalInterval`s directly—rather, `tonalInterval`s work behind the
scene in numerous `humdrumR` pitch functions. See the [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
and [pitch
parsing](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
documentation for details on how `tonalIntervals` are used by
`humdrumR`.

## Usage

``` r
tint(
  octave,
  LO5th = 0L,
  cent = numeric(length(octave)),
  partition = FALSE,
  Key = NULL
)

is.tonalInterval(x)

order.tonalInterval(
  x,
  ...,
  na.last = TRUE,
  decreasing = FALSE,
  method = c("auto", "shell", "radix")
)
```

## Slots

- `Octave`:

  integers representing the octave offset.

- `Fifth`:

  integers representing the "line-of-fifths" value.

- `Cent`:

  numeric values representing cents (1200th of an octave).

  The `tonalInterval` is a [S4](http://adv-r.had.co.nz/S4.md) subclass
  of `humdrumR`'s virtual class
  [struct](https://humdrumR.ccml.gtcmt.gatech.edu/reference/struct.md),
  from which it inherits a lot of useful "vector-like"
  behaviors/functionality.

## Creating tonal intervals

Generally, `tonalIntervals` are created using the
[`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
function, and its various methods. The `tonalInterval` *function* is
primarily a parser, [documented
elsewhere](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md),
which interprets various input representations and generates
`tonalInterval` S4 *objects* [documented
here](http://adv-r.had.co.nz/S4.md).

Alternatively, the constructor function `tint` can be used to directly
create `tonalInterval` objects. The three arguments to `tint` correspond
to the three slots: `octave`, `LO5th` (Fifth), and `cent`. All inputs
will be coerced to match in length. The `octave` argument can be left
blank, in which case the appropriate octave will automatically be
computed to place the interval in the octave above .

By default, the [as.character](https://rdrr.io/r/base/character.html)
method, and thus (via
[struct](https://humdrumR.ccml.gtcmt.gatech.edu/reference/struct.md))
the [show](https://rdrr.io/r/methods/show.html) method, for
`tonalInterval`s call
[`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md).
Thus, if you return a `tonalInterval` on the command line you'll see the
`**interval` representation printed.

### Predefined Intervals:

`humdrumR` automatically exports a bunch of `tonalInterval`s, named by
their musical interval representation. Every generic interval from 1 to
15 is combined with every interval quality `dd` (doubly diminished), `d`
(diminished), `m` (minor), `M` (major), `A` (augumented) `AA` (doubly
augmented). Thus, after loading `humdrumR`, you can type things like
`M3 + M3` and get `A5`. In addition, the variables `unison`
(`= P1 = tint(0, 0)`) and `pythagorean.comma` (`= d2 = tint(-19,12)`)
are exported as well.

### Arithmetic:

Technically, `tonalInterval`s are examples of algebraic [modules over
integers](https://en.wikipedia.org/wiki/Module_(mathematics)). This
means that certain arithmetic operations are defined for
`tonalIntervals` and can be called using standard arithmetic operators
(`+`, `-`, etc.):

- Addition: `tonalIntervals` can be added together, acting exactly as
  you'd expect (i.e., \\M3 + m3 = P5\\).

- Subtraction: `tonalIntervals` can be subtracted just as they are
  added. Also, they can be negated with a single `-` operator (like
  `-M3`).

- Multiplication: `tonalInterval`s can *not* be multiplied together.
  However, [scalar (integer)
  multiplication](https://en.wikipedia.org/wiki/Scalar_multiplication)
  is defined: thus, `tonalIntervals` can be multiplied by integers to
  create new `tonalInterval`s: e.g., \\M2 \* 3 = A4\\.

- Division: as the natural inverse of scale multiplication, [Euclidean
  division](https://en.wikipedia.org/wiki/Euclidean_division) is defined
  for `tonalIntervals`—i.e., division by/into whole (integer) pieces,
  often with leftover "remainders" (modulo). In R, Euclidean division is
  achieved with the [%/%](https://rdrr.io/r/base/Arithmetic.html)
  operator—*not* `/`—, with the associated
  [%%](https://rdrr.io/r/base/Arithmetic.html) used for the
  remainder/modulo. Two `tonalInterval`s can be divided to produced an
  integer; Conversely, a `tonalInterval` can be divided by an integer to
  produce a `tonalInterval`.

Take note that the way `humdrumR` defines Euclidean division is based in
*tonal space*—i.e., the line-of-fifths—not frequency or atonal-semitone
space. For example, an augmented-fourth divided by a major-second *is*
`3L`, but a diminished-fifth divided by a major-second is *not*
3L—`d5 %/% M2` equals `-3L` with a remainder of `P8` (plus an octave)!
The division algorithm works by applying standard Euclidean division to
the `@Fifth` slot (line-of-fifths tonal space), and shifting the
`@Octave` value in the remainder to the match the appropriate octave.

If you attempt to do addition between a `tonalInterval` and
non-`tonalInterval` atomic vector (e.g., `integer`, or `character`),
`humdrumR` will attempt to
[coerce](https://en.wikipedia.org/wiki/Type_conversion) the other input
to a `tonalInterval`, using the
[`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
parser, do the math and then output the answer in the original format
(non-`tonalInterval`) format. For instance, `M3 + 2` will interpret `2`
as two semitones and add a major-second to the major-third, resulting in
`6` semitones.
["In-place"](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)
parsing/deparsing will be used, so "extra" characters in the input will
be passed through. For example, `M3 + 4.ee-` will return `4.gg`.

### Relational Operators

`tonalInterval`s can be compared using the standard [relational
operations](https://rdrr.io/r/base/Comparison.html), like `==`, `!=`,
`>`, and `>=`. Two `tonalInterval`s are equal (according to `==`) only
if all their slots (`Octave`, `Fifth`, and `Cent`) are exactly
identical. Thus, enharmonic notes (like C and Db) are *not* equal. In
contrast, ordinal comparisons (e.g., `>`, `<=`) between `tonalInterval`s
are based on their semitone (equal temperament) size, so enharmonicity
is irrelevant. Thus, `m3 >= A2` and `A2 >= m3` are both `TRUE`, even
though `m3 == A2` is not.

## See also

The main way to create `tonalInterval` S4 objects is with the
[`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
pitch parser.

Other core pitch representations:
[`LO5th()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/LO5th.md)

Other Tonal S4 classes:
[`diatonicSetS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md),
[`tertianSetS4`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)

## Examples

``` r
M3 <- tint(   , 4L)

M2 <- tint(   , 2L)
M9 <- tint(-1L, 2L)

M9 - M2 
#> tonalInterval
#> [1] +P15
# = octave
M9 - 2L
#> [1] 24
# = 12

M3 %/% M2 
#> [1] 2
# = 2

"cc#]" + M6
#> [1] "aa#]"
# = "aa#]"

###

cMajor <- sort(tint( , -1:5))
eMajor <- cMajor + M3
eMajor + 2L 
#> [1]  6  8 10 11 13 15 17
# 6 8 10 11 13 15 17

eMajor[4:5] - P8
#> tonalInterval
#> [1] -m3 -m2
# = -m3 -m2


```
