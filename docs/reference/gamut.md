# Make a pitch gamut

This function generates a [gamut](https://en.wikipedia.org/wiki/Gamut):
an ordered range of notes used in music. It is used to generate
[factor](https://rdrr.io/r/base/factor.html) levels for [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md).
The output format of the gamut is controlled by the `deparser` argument
(a function) and any `deparseArgs` that are passed to it, defaulting to
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).

## Usage

``` r
gamut(
  generic = FALSE,
  simple = FALSE,
  min.octave,
  max.octave,
  min.lof,
  max.lof,
  reference = NULL,
  deparser = tint2kern,
  deparseArgs = list(),
  ...
)
```

## Arguments

- generic:

  \*\*\*Should the gamut include only generic intervals?

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- simple:

  ***Should the gamut be constrained to one octave?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- reference:

  ***An optional reference vector to base the gamut on.***

  Defaults to `NULL`.

  Must be either `NULL`, or a
  [`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md),
  `integer`, or `character` vector.

  This vector is [parsed as
  pitch](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
  If it can't be parsed as pitch, it will be ignored.

- deparser:

  ***A [pitch
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
  to format the output.***

  Defaults to
  [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).

  Must be a [pitch
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md).

## Details

A gamut is produced based on two criteria: what range on the
line-of-fifths to include, and what range of octaves to include? These
ranges can be controlled directly with the `min.octave`, `max.octave`,
`min.lof`, and `max.lof` arguments, with the corresponding ranges being
`min.octave:max.octave` and `min.log:max.log` respectively. If any of
these arguments are missing (by default), the ranges default values
based on the `simple/compound` and `generic/specific` arguments. These
default ranges are:

- **Line-of-fifths**:

- If `generic = TRUE`, `-1:5` (F to B)

- If `generic = FALSE`, `-4:7` (Ab to C#)

- **Octaves**:

- If `simple = TRUE`, `0:0` (one octave only).

- If `simple = FALSE`, `-1:1`.

If a `tints` argument is provide (not `NULL`), `tints` is
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
as pitch data and the line-of-fifth and octave ranges of this data is
used to set the gamut ranges. This assures that all values that appear
in `tint` always make it into the gamut. However, if `min.octave`,
`max.octave`, `min.lof`, or `max.lof` are present, they override the
ranges of `tint`; this can be used to exclude values, even if they
appear in `tint`.
