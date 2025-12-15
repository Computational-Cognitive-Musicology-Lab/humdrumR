# Translate between rhythm representations.

These functions are used to extract and translate between different
representations of rhythmic (time duration) information.

## Arguments

- x:

  ***An input vector.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a [rational
  (rhythmInterval)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md),
  or `NULL`.

  Must be [parsable as rhythm
  infromation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- ...:

  ***Arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  There are also two hidden (advanced) arguments you can specify:
  `memoize` and `deparse` (see the details below).

- scale:

  ***A `numeric` or
  [rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  value which is used as the output unit of measurement.***

  Defaults to `rational(1, 1)`.

  Must be `numeric` or
  [rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md).

- Exclusive, parseArgs:

  ***An vector of exclusive interpretations and/or an optional list of
  arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  Default to `NULL` and an empty
  [`list()`](https://rdrr.io/r/base/list.html) respectively.

  `Exclusive` must be `NULL`, or a `character` vector of either length 1
  or length(x); `parseArgs` must be a `list` of named arguments to the
  [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- inPlace:

  ***Should non-rhythm information be retained in the output string?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  Defaults to `FALSE`.

  A singleton `logical` value, or `NA`.

  See "Grace notes" section below.

## Details

The full list of rhythm functions is:

- **Metric rhythm representations**

  - *Symbolic rhythm representations*

    - [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
      (reciprocal note values)

    - [`notehead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md)
      (traditional note-value symbols)

  - *Numeric rhythm representations*

    - [`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
      (Whole notes)

    - [`quarters()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
      (quarter notes/crotchets)

- **Ametric rhythm representations**

  - *Symbolic rhythm representations*

    - [`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)
      (durations of time)

  - *Numeric rhythm representations*

    - [`seconds()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)

    - [`ms()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)
      (milliseconds)

These rhythm functions all work in similar ways, with similar arguments
and functionality. Each function takes an input rhythm (time duration)
representation (which can be anything) and outputs *its* own rhythm
representation. For example,
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
takes any input representation and outputs `**recip` ([reciprocal
durations](https://www.humdrum.org/rep/recip/index.html)) data.
Underneath the hood, the full processing of each function looks like
this:

- **Input** representation (e.g., `**recip` or `**dur`) `|>`

  - *Parsing* (done by
    [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md))
    `|>`

    - **Intermediate**
      ([rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md))
      representation `|>`

  - *Deparsing* `|>`

- **Output** representation (e.g. `**recip` or `**duration`)

To read the details of the parsing step, read
[this](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).
To read the details of the "deparsing" step, read
[this](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).
To read more details about each specific function, click on the links in
the list above, or type `?func` in the R command line: for example,
[`?notehead`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md).

### Grace notes

`**recip` and `**kern` data sometimes include tokens indicating [grace
notes](https://en.wikipedia.org/wiki/Grace_note)—a special category of
duration, usually used to indicate "freely" a-metric notes in an
otherwise metric context. In humdrum data, grace notes are marked with
`"q"` or `"Q"`; `q` should be reserved for tokens with no (other)
duration information, while `Q` should be marked along with duration
information: for example, `aa-q` or `16aa-Q`. In practice, this
distinction is not always made, and is rarely important.

By default, the `**recip` parser treats input marked as grace notes as
having a duration of zero. However, if you pass a `grace` argument to
the [rhythm
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md),
you can control this behavior. If `parse(grace = TRUE)`, grace-note
durations (like the `16` in `"16aa-Q"`) are parsed like any other
duration. If `grace = NA`, grace-notes return as `NA`. If
`grace = FALSE`, the duration returns as zero (the default behavior).

## See also

To better understand how these functions work, read about how rhythms
are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).
