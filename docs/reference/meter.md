# Musical Meter in humdrumR

`HumdrumR` represents musical meter, internally, using the S4-class
`meter`. A `meter` is simply a collection of regular or irregular beat
"levels," with each level represented as [musical
durations](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md).
For example, the meter 4/4 could be represented as the
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
beat-levels `c("1", "2", "4", "8", "16")`—that is, whole-note,
half-note, quater-note, eighth-note, and sixteenth note. In addition,
each meter has a *tactus*—the "main" beat level.

## Usage

``` r
meter(x, ..., measure, tactus, tick, fill.levels, subdiv, hyper, tatum)

# S3 method for class 'meter'
meter(x, ...)

# S3 method for class 'rational'
meter(
  x,
  ...,
  measure = NULL,
  tactus = NULL,
  tick = "16",
  fill.levels = "neither",
  subdiv = NULL,
  hyper = NULL,
  tatum = FALSE
)

# S3 method for class 'list'
meter(
  x,
  ...,
  measure = NULL,
  tactus = NULL,
  tick = "16",
  fill.levels = "neither",
  hyper = NULL,
  subdiv = NULL,
  tatum = FALSE
)

# S3 method for class 'character'
meter(
  x,
  ...,
  sep = "/",
  compound = TRUE,
  Exclusive = NULL,
  multiDispatch = FALSE
)
```

## Arguments

- x:

  ***A time signature or list of beats used to construct a meter.***

  Must be a `character` string or a `list` of either `numeric` or
  `character` values.

  A `character` input is parsed as a time signature, which is used to
  extract tactus and measure levels. The contents of `list` are parsed
  as durations using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md):
  these durations become the levels of the meter. Failures to parse the
  input will result in an error.

- measure, tick, tactus:

  ***Durations to use longest (measure), shortest (tick), or tactus
  levels.***

  Must be a singleton `character` or `numeric` value, or a `list`
  containing a single vector of such values.

  These are parsed as durations using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  parse failures lead to errors.

- fill.levels:

  ***Should "gaps" between specified levels be added to the meter?***

  Must be a singleton `logical` or `character` value; `character` values
  must be `'above'`, `'below'`, `'both'`, or `'neither'`.

  `TRUE` is shorthand for `'both'`; `FALSE` is shorthand for
  `'neither'`.

- subdiv:

  ***Subdivisions (fractions) of the `tactus` to add to the meter.***

  Must be positive natural numbers.

- hyper:

  ***Multiples of the `measure` duration to add to the meter.***

  Must be positive natural numbers.

- tatum:

  ***Should the least common denominator of all levels be added to the
  meter?***

  Must be a singleton `logical` value: an on/off switch.

## Details

Generating meters in `humdrumR` can be done easily with the `meter()`
function. If you pass a `character` string of a humdrum time signature,
you'll get a meter object: for example, `meter("M4/4")` or
`meter("M12/8")`. Additive time signatures, like `meter(M2+3/8)` are
also parseable.

### Adding or removing levels

Time signatures leave a lot of room for interpretation. Even in
"standard" 4/4 time, there are a number of questions you need to
consider when analyzing meter:

- What is the fastest level you want to count? 16ths? 32nds? 64ths?

- Do you want to count the half-note level, which is "in between" the
  full measure (whole-note) and the tactus quarter-note?

- How do you want to treat triplets or other tuplets? What is a piece
  uses a lot of triplet eighth-notes?

- Do you want to consider hypermeter, *above* the measure level?

Fortunately, `humdrumR` and the `meter()` function give you options to
precisely specify metric levels. The most transparent way is to simply
pass `meter()` a `list` of duration values, like
`meter(list("1", "4", "8"))`. However, `meter()` includes a number of
helpful arguments that can be used to quickly streamline the process of
defining a meter. To understand these arguments, lets first clarify some
metric definitions used in `humdrumR`:

- *Measure*: The duration of the "measure" of the meter. I.e., the
  highest metric level, or the least common multiple of all levels.

- *Hypermeter*: Metric levels above the measure indicated by the time
  signature.

- *Tactus*: The "main," usually central, level.

- *Subdivision*: The level directly below the tactus.

- *Tick*: The smallest/fastest metric level. (The "ticks" in the grid.)

- *Tatum*: The smallest common denominator between a set of
  beats/duration. Note that a fully specified metric grid *should* have
  the tick and the tatum be identical. However, we don't require that
  the tick be the tatum.

#### Meter construction arguments

The *measure* (largest) and the *tick* (smallest) levels capture the
full range of a meter. The tactus is typically somewhere in the middle
between these two, but this is not required. The `fill.levels` argument
can be used to 'fill in' levels between the measure, tactus, and tick.
This means that if there is room to "fit" (geometrically) duple or
triple subdivisions between the higher and lower level, those
subdivisions are added to the meter. The `fill.levels` argument must be
a single `character` string, [partially
matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
either `'above'`, `'below'`, `'both'`, or `'neither'`. "Above" means
fill in any gap between the tactus and the measure; "Below" means fill
in any gap between the tactus and the tick. As a shortcut, `logical`
`TRUE` or `FALSE` can be used as an alternative way of specifiying
`'both'` or `'neither'`, respectively. For example, if you start with
levels measure-tactus-tick combination of `c("1", "4", "16")`,
`fill.levels = TRUE`, will fill in levels at `"2"` and `"8"`.

The `tick` argument can be used to directly specify a tick for the
meter. This is especially useful if you are parsing datasets with
multiple meters, but want to force them all to use the same tick value.
For example, `meter(TimeSignature, tick = '32')`. The `tick` argument
must be a single value which can be
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
as a rhythmic duration.

The `subdiv` argument can be used to explicitly control how the tactus
is subdivided. `subdiv` should be a natural number (greater than 1), to
divide the tactus by. Similarly, the `hyper` argument is a natural
number to explicitly multiply the measure by. Thus, `hyper = 2` adds a
two-measure hyper meter.

The `tatum` argument, if `TRUE`, causes the tatum of all other metric
levels to be added to the meter (if not already present). This is
useful, for example, if you specify a meter with a mix of duple and
triple levels and want to make sure the *tick* of the meter is the tatum
of the levels. For example, if you have levels `c("4", "6", "8")`,
`tatum = TRUE` will add the level `"24"` to the meter.

#### Constructing meters from time signatures

The "meter construction arguments" above can *also* be used when reading
time signatures. This allows you to use time signatures from your
datasets, while maintaining precise control of the metric levels used.
For example, the command `meter("M4/4", fill.levels = FALSE)` generates
a meter with levels `c("1", "4", "16")`. We could add an eighth-note
level by adding `subdiv = 2`, or triple-eighth-notes with `subdiv = 3`.
If we *did* add triplet-eighth-notes (`subdiv = 3`), we might want to
add `tatum = TRUE`, which would automatically calculate the common tatum
of our levels—in this case, adding forty-eighth notes to the meter. On
the opposite end, `hyper = 4` could add a four-measure hyper meter on
top of our 4/4 bar. Finally, the `tactus` argument could be used to
choose another tactus, like `tactus = "2"`.

Any additional, unnamed arguments to `meter()` will be parsed as
durations, and added as levels to the meter.

## See also

Other Metric functions:
[`tactus()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tactus.md),
[`tatum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tatum.md)
