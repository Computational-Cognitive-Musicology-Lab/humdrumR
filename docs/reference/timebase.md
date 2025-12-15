# Represent time on a regular grid

The `timebase()` function takes a
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
dataset and converts rhythmic information in the data into a
step-sequencer like representation, with each humdrum data record
representing one step. The duration of each step is the the "timebase",
which can be controlled with the `tb` argument. The `timebase()`
function is currently in a beta-draft, so may not work well.

## Usage

``` r
timebase(humdrumR, tb = "16")
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  This data must have at least one spine with rhythmic
  ([duration](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md))
  encoded.

- timebase:

  ***The duration of each step in the output sequence.***

  Defaults to a sixteenth-note.

  Must be a single atomic value, which can be [parsed as a
  duration](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
  or `NULL`. If `NULL`, the
  [`tatum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tatum.md)
  of the dataset if automatically used as the time base.
