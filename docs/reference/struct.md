# struct

Virtual class to help create atomic-vector-like composite data objects.

## Usage

``` r
is.struct(x)
```

## Details

`humdrumR` defines a number of [S4
classes](http://adv-r.had.co.nz/S4.md) which are, underneath the
surface, [composite data
types](https://en.wikipedia.org/wiki/Composite_data_type) made up of
collections of [base-R atomic
vectors](https://rdrr.io/r/base/vector.html), stuck together. The
"vectorized" nature of R's atomic types is one of R's key strengths, so
in `humdrumR` we try to A) mostly use the standard atomic types B) make
all the new types we *do* define act as much like atomic vectors as
possible. `struct` is a *virtual* S4 class which serves this purpose:
creating composite atomic vectors which act (mostly) like base-R atomic
vectors.

As a "virtual class" `struct`s themselves don't really exist as
independent objects, but the `struct` class defines (abstractly) all the
necessarry methods to treat a collection of atomic vectors as a single
vector/matrix-like object—simply make your new subclass
[inherit](https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming))
`struct` and it is all taken care of. (To do this, specify
`contains = "struct"` in your call to
[setClass](https://rdrr.io/r/methods/setClass.html).)

Important `humdrumR` classes which inherit from `struct` include:

- [tonal
  intervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)

- [diatonicSet](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html)

- [tertianSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)

- [`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)

Be warned, R's S4 object-system is limited in this regard: you can't
really define S4 classes that act *fully* like R atomics, as many of
their features are hard-coded into R itself and can't be replicated. The
most important limitation of `struct` that you may encounter is that,
though `struct` classes work (ok) in
[data.frames](https://rdrr.io/r/base/data.frame.html),
[data.tables](https://rdrr.io/pkg/data.table/man/data.table.html) and
[tibbles](https://tibble.tidyverse.org/reference/tibble.html) will
either not work or give strange behaviors if you put `struct`s into
them.

## Slots

- `dim`:

  Either `NULL` or a non-negative
  [integer-vector](https://rdrr.io/r/base/integer.html) of
  `length == 2L`, representing the number of rows and columns
  respectively. Dimensions *can* be zero.

- `rownames`:

  Either `NULL` or a
  [integer](https://rdrr.io/r/base/integer.html)/[character](https://rdrr.io/r/base/character.html)-vector
  which is the same length as either A) if `dim == NULL`, the length of
  the `struct` B) if `dim != NULL`, the number of rows in the `struct`.

- `colnames`:

  Either `NULL` (it *must* be `NULL` if `dim == NULL`) or a
  [integer](https://rdrr.io/r/base/integer.html)/[character](https://rdrr.io/r/base/character.html)-vector
  of length equal to the number of columns in the `struct`.

## Behavior

`struct` subclasses (i.e., classes which inherit from `struct`) behave
very similarly to normal [R atomic
vectors/matrices](https://rdrr.io/r/base/vector.html). However, they do
differ in a few respects, mostly in ways that are intended to avoid some
of the quirky behaviors of R matrices: In general, the distinction
between dimensionless vectors and dimensioned vectors
([matrices](https://rdrr.io/r/base/matrix.html)) is slightly weaker in
`structs` than with normal R atomic vectors/matrices. Most importantly,
dimensioned `struct`s won't drop their dimensions under various common
operations ([c](https://rdrr.io/r/base/c.html), `[]`, etc.), the way
base-R matrices do. In general, it is easier to interact with a
multi-column (matrix-)`struct` in the same way as a dimensionless
(vector-)`struct`. For example, if the struct has dimensions then
`length(struct) == nrow(struct)`, instead of
`length(matrix) == nrow(matrix) * ncol(matrix)`—i.e., the "height" of
the `struct` (the number of rows) is its length. Another big difference
is in the behaviors of [c](https://rdrr.io/r/base/c.html): `c` doesn't
always cause `struct`s to lose their dimensions and `c` can be used to
concatinated multi-column `struct`s, or even mixes of dimensionless and
dimensioned `struct`s: If any `struct` arguments to `c` have dimensions,
the `struct`s are concatinated via a call to
[rbind](https://rdrr.io/r/base/cbind.html), with any dimensionless
vectors coerced to 1-column matrices. Of course, the (resulting) number
of columns must all be the same or an error will occur!

Other differences:

- `struct`s can have either no dimensions (`dim(struct) == NULL`) or two
  dimensions. Higher dimensional `struct`s are not supported (yet).

- `rowSums` and `colSums` will coerce a dimensionless struct to a column
  matrix.

- `struct`s always throw an error if you try to index them with an index
  value that is greater than the length/nrow of the `struct`. This is
  different than atomic vectors, which will pad the vector up to the
  length of the index you give—a sometimes useful but quirky behavior.

- `struct`s with two dimensions have a `cartesian` indexing argument. If
  `cartesian = TRUE`, the `i` and `j` arguments are treated as cartesian
  coordinates. (This behavior can be achieved with base R matrices (or
  `struct`s) by inputing a matrix with two columns.)

## Requirements

To work, `struct` makes a few assumptions about your class. Your class
must have one or more slots which are vectors, all of which are the same
length. `struct`'s indexing method will cause all of these vectors to be
indexed as one. When you define a new subclass of `struct`, it will
inherit a [validObject](https://rdrr.io/r/methods/validObject.html)
method which assures that all elements are the same dimension. Thus, if
you are writing your own `validObject` method (using
[setValidity](https://rdrr.io/r/methods/validObject.html)) you just have
to worry specifically about the validity of the information in your
slots, not that the slots are all the same length.

## Initialize

An initialize method which automatically makes all slots the same length
is predefined for `structs`. If you want to make a more specialized
[initialize](https://rdrr.io/r/methods/new.html) method, you can still
take advantage of the inherited method by using callNextMethod at the
beginning of your method.

## Predefined methods

The main purpose of the `struct` virtual class is that it defines many
of the basic methods you need to manipulate subclass objects. Most
importantly, [indexing](https://rdrr.io/r/base/Extract.html) methods are
fully defined (that mimic base-R atomic vector/matrix indexing), as well
as basic "structural" methods like
[(col/row)names](https://rdrr.io/r/base/colnames.html),
[dim](https://rdrr.io/r/base/dim.html),
[length](https://rdrr.io/r/base/length.html), [ncol,
nrow](https://rdrr.io/r/base/nrow.html), etc. In addition:

- If you define [\> and \>=](https://rdrr.io/r/base/Comparison.html),
  `<` and `<=` will be automatically defined.

- If you define [as.character](https://rdrr.io/r/base/character.html)
  for your subclass, [show](https://rdrr.io/r/methods/show.html) and
  [format](https://rdrr.io/r/base/format.html) methods are automatically
  defined.

What's more, default arithmetic methods for addition, subtraction,
(scalar-integer) multiplication, and negation (`-x`) are defined. The
default addition behavior is that each numeric
([base::integer](https://rdrr.io/r/base/integer.html) or
[base::numeric](https://rdrr.io/r/base/numeric.html)) slot from your
subclasses will be added together. Thus, `struct1 + struct2` will
extract each numeric/integer slot from each `struct`, add them together
and create a new `struct` from the result. `-struct` will negate all
numeric fields, and subtraction is simply defined as adding the
negation. Since *scalar* multiplication is defined, two `struct`s cannot
be multiplied, but a struct can be multiplied by an integer (all numeric
fields are multiplied by the integer(s)). If these definitions don't
work for your subclass, you'll need to create your own, more specific,
method!

## See also

Examples of `struct` subclasses:
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
[rhythmInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
[diatonicSet](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html)
[tertianSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)

## Examples

``` r
setClass('mynewsubclass', contains = 'struct', slots = c(X= 'numeric', Y = 'numeric'))

test <- new('mynewsubclass', X = 1:10, Y = 10:1)

# all of these should work:
test[1:5]
#> mynewsubclass
#> [1] 1,10 2,9  3,8  4,7  5,6 
rev(test)  == test
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
cbind(test, test)
#> mynewsubclass[10 , 2]
#>       [,1] [,2]
#>  [1,] 1,10 1,10
#>  [2,] 2,9  2,9 
#>  [3,] 3,8  3,8 
#>  [4,] 4,7  4,7 
#>  [5,] 5,6  5,6 
#>  [6,] 6,5  6,5 
#>  [7,] 7,4  7,4 
#>  [8,] 8,3  8,3 
#>  [9,] 9,2  9,2 
#> [10,] 10,1 10,1
c(test, test)
#> mynewsubclass
#>  [1] 1,10 2,9  3,8  4,7  5,6  6,5  7,4  8,3  9,2  10,1 1,10 2,9  3,8  4,7  5,6 
#> [16] 6,5  7,4  8,3  9,2  10,1
test * 3
#> mynewsubclass
#>  [1] 3,30  6,27  9,24  12,21 15,18 18,15 21,12 24,9  27,6  30,3 
test - test
#> mynewsubclass
#>  [1] 0,0 0,0 0,0 0,0 0,0 0,0 0,0 0,0 0,0 0,0

```
