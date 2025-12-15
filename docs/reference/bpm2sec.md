# Translate between durations and tempos

Functions for translating between durations (in seconds) and
*tempos*—expressed as BPM (beats-per-minute). The "beats" in
beats-per-minute are specified using the `unit` argument; `unit`
defaults to `.25` (a quarter-note), as is conventional.

## Usage

``` r
bpm2sec(BPM, unit = 0.25)

sec2bpm(sec, unit = 0.25)

bpm2ms(BPM, unit = 0.25)

ms2bpm(ms, unit = 0.25)
```

## Arguments

- BPM:

  ***The tempo.***

  Defaults to `60`.

  Must be a number or a `character` string in the format `"MM120"` (for
  120 bpm).

  By default,
  [with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  passes the `BPM`
  [field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
  if present.

- unit:

  ***The "Beat" in BPM.***

  Defaults to a quarter-note.

  Must be a value which can be interpreted as [rhythmic
  duration](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

## Details

The pairs of functions involving `ms` (milliseconds) and `sec`
(seconds), are identical except for the change of scale between seconds
and milliseconds.

## See also

Other time functions:
[`time()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)
