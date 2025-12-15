# Generating ("deparsing") rhythm representations

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a easy-to-use system for generating a variety of rhythm (time
duration) representations, which can be flexibly modified by users.
"Under the hood" `humdrumR` represents all rhythmic duration information
as [rational
numbers](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md),
which is typically extracted from input data using the [rhythm
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).
This
[rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
representation can then be "deparsed" into a variety of predefined
output formats (like `**recip`), or into new formats that you create!

## Details

Deparsing is the second step in the [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
processing pipeline:

- **Input** representation `|>`

  - *Parsing* `|>`

    - **Intermediate**
      ([rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md))
      representation `|>`

  - *Deparsing* (DEPARSING ARGS GO HERE) `|>`

- **Output** representation

Various rhythm representations like `**recip`, `**dur`, and `**duration`
can be generated using predefined [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
like
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
[`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md), and
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
respectively. All of these functions use a common deparsing framework.
*This* documentation talks about this deparsing step. For an overview of
the parsing process, look
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

## Basic rhythm arguments

Different rhythms share a few standard arguments which control details
of the output. The most important is the `scale` argument.

### Scalar unit

The `scale` argument is a `numeric` or
[rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
value which indicates the reference unit used for duration values: what
is "1" duration? By default, the unit is a "whole note" or duration. By
changing the unit, you can rescale your output. For example, a recip
value represents a fraction of the unit: e.g., `"2"` equals 1/2 of the
unit. If we call `recip('2', scale = 1/16)` this is telling us to get
half of a sixteenth: which in this case would be `'32'`.

### In-place parsing

In humdrum data, character strings are often encoded with multiple
pieces of musical information right besides each other: for example,
`**recip` data might include tokens like `"4.ee-[`. The `humdrumR`
parser (`rhythmInterval`) will automatically "pull out" rhythm
information from within strings, if it can find any using the
appropriate known regular expressions. For example, `duration('4.ee-[')`
returns 0.375. However, all the pitch functions (like
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
and [`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md))
have an option to keep the "extra" information and return the result "in
place"—i.e., embedded right where it was found in the input string. This
is controlled with the `inPlace` argument, which is `FALSE` by default.
So, `duration('4.ee-[', inPlace = TRUE)` will return 0.375ee-\[. It
keeps the `"ee-["` part. Note that `inPlace = TRUE` will force functions
like `duration`, which normally return `numeric` values, to return
`character` strings *if* their input is a `character` string.

## See also

All `humdrumR` [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
make use of the deparsing functionality.
