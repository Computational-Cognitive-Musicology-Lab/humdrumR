# Translate between pitch representations.

These functions are used to extract and translate between different
representations of pitch information. The functions can also do things
like transposing and simplifying pitches.

## Arguments

- x:

  ***Input data to parse as pitch information.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  or `NULL`.

- ...:

  ***Arguments passed to the [pitch
  deparser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).***

  There are also two hidden (advanced) arguments you can specify:
  `memoize` and `deparse` (see the details below).

- generic:

  ***Should "specific" pitch information (accidentals and qualites) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- simple:

  ***Should "compound" pitch information (octave/contour) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- Key:

  ***The input `Key` used by the parser, deparser, and transposer.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

- parseArgs:

  ***An optional list of arguments passed to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- transposeArgs:

  ***An optional list of arguments passed to a special
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  call.***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).

- inPlace:

  ***Should non-pitch information be retained in the output string.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  This argument only has an effect if the input (the `x` argument) is
  `character` strings, *and* there is extra, non-pitch information in
  the input strings "besides" the pitch information. If so, and
  `inPlace = TRUE`, the output will be placed into an output string
  beside the original non-pitch information. If `inPlace = FALSE`, only
  the pitch output information will be returned (details below).

## Value

`NULL` inputs (`x` argument) return a `NULL` output. Otherwise, returns
a vector/matrix of the same length/dimension as `x`. `NA` values in the
input `x` are propagated to the output.

## Details

The full list of pitch functions is:

- **Tonal pitch representations**

  - *Absolute pitch representations*

    - [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)

    - [`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)

    - [`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md)

    - [`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md)

    - [`tonh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonh.md)
      (German-style notation)

  - *Relative pitch representations*

    - [`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md)

    - [`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md)
      (relative-do solfege)

    - [`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md)
      (French-style fixed-do solfege)

    - [`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md)
      (absolute scale degrees)

    - [`deg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md)
      (melodic scale degrees)

    - [`bhatk()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bhatk.md)
      (hindustani swara)

  - *Partial pitch representations*

    - [`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md)

    - [`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md)

    - [`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md)

    - [`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md)

- **Atonal pitch representations**

  - *Musical pitch representations*

    - [`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)

    - [`midi()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)

    - [`cents()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)

    - [`pc()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pc.md)
      (pitch classes)

  - *Physical pitch representations*

    - [`freq()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/freq.md)

These pitch functions all work in similar ways, with similar arguments
and functionality. Each function takes an input pitch representation
(which can be anything) and outputs *its* own pitch representation. For
example,
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
takes any input representation and outputs `**kern` (pitch) data.
Underneath the hood, the full processing of each function looks like
this:

- **Input** representation (e.g., `**pitch` or `**semits`) `|>`

  - *Parsing* (done by
    [`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md))
    `|>`

    - **Intermee**
      ([tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md))
      representation `|>`

    - **Transformation** (e.g.,
      [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md))
      `|>`

  - *Deparsing* `|>`

- **Output** representation (e.g. `**kern` or `**solfa`)

To read the details of the parsing step, read
[this](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
To read the details of the "deparsing" step, read
[this](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).
To read more details about each specific function, click on the links in
the list above, or type `?func` in the R command line: for example,
[`?kern`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).

The "partial" pitch functions
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md),
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md),
and
[`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md)
are so-called because they each only return one part/aspect of pitch
information, and only that part. For example,
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md)
only returns the accidentals (if any) of pitches.

## See also

To better understand how these functions work, read about how pitches
are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).
