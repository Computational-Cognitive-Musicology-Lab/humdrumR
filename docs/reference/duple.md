# Generate duple meters

This function generates a
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
objects representing duple meters. The desired number of duple levels is
controlled by the `nlevels` argument. The span of the meter (i.e., the
highest level) is indicated by the `measure` argument. Finally, the
`tactus` argument indicates which level (indexed from highest to lowest)
is the tactus. The default arguments build a 4/4 meter with levels
ranging from whole-notes down to sixteenth-notes, and a quarter-note
tactus.

## Usage

``` r
duple(nlevels = 4, measure = 1, tactus = 3L)
```

## Arguments

- nlevels:

  ***The number of duple levels.***

  Must be a singleton, positive natural number

- measure:

  ***The duration of the top level of the meter.***

  Must be a singleton `numeric` or `character` value.

  Is parsed as a duration by
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  a failure to parse leads to an error.

- tactus:

  ***Which level is the tactus?***

  Must be a singleton, positive natural number; must be less than or
  equal to `nlevels`.

## See also

Use the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
function to create abitrary meters.
