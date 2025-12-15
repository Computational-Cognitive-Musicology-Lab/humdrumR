# Identify syncopated rhythms

The `syncopation()` function takes a vector of rhythmic duration values
and a meter and identifies which durations are syncopated, return `TRUE`
for synocopations and `FALSE` otherwise. The output syncopation depends
a lot on how meter is specified/interpreted so check out the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
documentation if you are looking for more control of the output.

## Usage

``` r
# Default S3 method
syncopation(dur, meter = duple(5), levels = "all", groupby = list())

syncopation(dur, meter = duple(5), levels = "all", groupby = list())
```

## Arguments

- dur:

  ***An input vector of rhythmic durations.***

  Must be a `character` or `numeric` vector.

  Is parsed using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  Wherever the input can't be parsed as a duration, that element is
  treated as a duration of zero.

- meter:

  ***The meter(s) to compute levels from.***

  Defaults to a standard, five-level duple (4/4) meter.

  Must be a
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
  object or a `character` vector.

  For `character` input, the string is parsed using
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md);
  a failure to parse will result in an error.

- levels:

  ***On which metrics levels should we identify syncopations?***

  Defaults to `"all"`.

  Must be a non-empty `character` or `numeric` vector.

  If `levels` is simply the singleton string `"all"`, syncopations at
  any metric level are identified. Otherwise, the `levels` are parsed
  with
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  fail to parse may lead to an error. The parsed levels must be levels
  of the given
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md).

- groupby:

  ***A `list` of vectors to group `x`.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

  To function as a by-record timeline, the `groupby` list music include
  a *named* `Piece` and `Record` fields. Luckily, these are
  automatically passed by
  [with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
  so you won't need to worry about it!

## Details

A syncopation occurs whenever a rhythmic duration is longer than the
highest metric level that it lands on.

In some cases, we might want to restrict our attention to syncopations
that occur at a specific metric level: for example, "eighth-note
syncpations." We can provide a set of metric levels to the `levels`
argument to enforce this restriction. The `levels` must be parsable as
durations which match the levels of the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md).
