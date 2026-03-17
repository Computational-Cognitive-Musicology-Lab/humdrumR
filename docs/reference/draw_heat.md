# Draw a "heatmap" of 2d data

This function draws a heat map, with color indicating value.

## Usage

``` r
draw_heat(
  tab,
  log = "",
  xlim = NULL,
  ylim = NULL,
  showCounts = FALSE,
  minColor = flatly[5],
  maxColor = flatly[4]
)
```

## Arguments

- showCounts:

  ***Should the counts of values in cell be printed on the bar?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- minColor:

  ***What color should be used to represent 0?***

  Defaults to `"#2C3E50"`.

  Must be a single `character` (color name or hexcode) or `integer`
  (index of flatly palette).

- maxColor:

  ***What color should be used to represent the maximum value?***

  Defaults to `"#E74C3C""`.

  Must be a single `character` (color name or hexcode) or `integer`
  (index of flatly palette).
