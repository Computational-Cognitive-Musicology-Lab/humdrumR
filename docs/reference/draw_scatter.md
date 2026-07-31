# Draw a scatter plot

variables, `x` and `y`, as a scatter plot. If `line = TRUE`, points are
not drawn at all, and instead a line connecting the points is draw from
leftmost point to rightmost.

## Usage

``` r
draw(x, y , col = NA) 
       # where x is numeric
       # and   y is numeric
```

## Arguments

- log:

  ***Should X and/or Y axes be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale on both axes).

  Must be a single `character` string; options are `"x"` (X axis on log
  scale), `"y"` (Y axis on log scale), and `"xy"` (both axes on log
  scale).

- jitter:

  ***Should random jitter be added to the X and/or Y positions?***

  Defaults to `""` (no jitter).

  Must be a single `character` string; options are `"x"` (jitter on X
  axis), `"y"` (jitter on Y axis), and `"xy"` (both axes).

  Useful if `x` or `y` values are not really continuous, so that many
  data points fall on top of each other.

- line:

  ***Should a line be drawn through the `x`/`y` coordinates, instead of
  points?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- normalReference:

  ***Should a Gaussian reference distribution be drawn?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the 95% and 50% density regions of a bivariate normal
  (Gaussian) distribution are drawn under the scatter plot, using the
  means variances of, and the covariance between, the input vectors `x`
  and `y`.

- mean:

  ***Should the mean of input vectors `x` and `y` be marked on the
  plot?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- quantiles:

  ***Should distribution quantiles of `x` and `y` be marked?***

  Defaults to [`c()`](https://rdrr.io/r/base/c.html), so no quantiles
  are drawn.

  Must be a vector of numbers between 0 and 1 (inclusive), or an empty
  vector (the default).

  If any quantiles are specified, each quantile is drawn on the plot and
  labeled appropriately. Quantiles are computed separately for input
  vectors `x` and `y` and drawn using vertical and horizontal lines
  respectively. For example, `quantiles = .5` will draw a lines which
  converge at the medians of `x` and `y`; `quantiles = c(.25, .5, .75)`
  will draw vertical and horizontal lines marking the four quartiles of
  `x` and `y` (creating a grid with 16 cells).

- lm:

  ***Should the regression line, predicting `y` from `x`, be drawn?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  The simple regression line is estimated using [lm(y ~
  x)](https://rdrr.io/r/stats/lm.html). The 95% confidence
  limits—estimated using
  [`predict.lm()`](https://rdrr.io/r/stats/predict.lm.html)—are also
  drawn (as dashed lines). The regression coefficients are printed in a
  legend at the top left corner of the plot.

- conditional:

  ***Should normal reference, regression slope, means, and/or quantiles
  be computed separately for each color/pointStyle group?***

  Defaults to `TRUE`.

  Must be either a singleton `logical` value (an on/off switch), a named
  list of singleton logicals, or a character vector of names. Legal
  names can be `"normalReference"`, `"lm"`, `"mean"`, or `"quantiles"`.

  If no `color` or `pointStyle` groups are provided, this argument has
  no effect.

- ...:

  ***Additional parameters to pass to par().***

  Any base-R graphing parameters that can be set using the
  [`par()`](https://rdrr.io/r/graphics/par.html) function may be passed
  to
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md).
  These parameters are set using
  [`par()`](https://rdrr.io/r/graphics/par.html) (overriding humdrumR's
  defaults), but only for the duration of the
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
  call—i.e., the global [`par()`](https://rdrr.io/r/graphics/par.html)
  settings are not changed.

- pointSize:

  ***What size of points should be drawn?***

  Must be either a single positive number, or a vector of numeric values
  of the same length as `x`/`y`.

- pointStyle:

  ***What shape should points be drawn?***

  Must be a single whole number from 1 to 16, or a vector of discrete
  values of the same length as `x`/`y`.

## Details

The input vectors `x` and `y` must be of equal length, unless one of the
pair is a single scalar, in which case that scalar is recycled to match
the length of the other input. (Thus, providing a scalar `x` or `y`
causes the other variable to be drawn on a straight line.) Up to three
additional dimensions can be visually added to the plot using `color`
(color), `pointSize` (point size), and `pointStyle` (point style)
arguments.

## Color

If you pass a single color value to `color`, the whole graph is drawn
that color. However, if you pass `color` a vector of values which is the
exact same length as the input vectors `x`/`y`, and `line = FALSE`, the
unique values of this vector will be used to color the points of the
scatter plot. (A color legend will be drawn automatically.) The colors
for each group will be chosen automatically, unless the entire `color`
vector is valid color values. (Use `alpha` independently to change the
transparency.) If the grouping `color` vector is numeric and there are
more than ten unique values, a continuum of colors is created to
represent that numeric space.

## Point size

By default,
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
chooses an appropriate size to draw data points based on the density of
the X and Y points in the window—the more data on the screen, smaller
the points are drawn. You can override this by passing a single numeric
value to `pointSize`; values between about `.2` and `1.5` are pretty
reasonable, typically. However, if you pass `pointSize` vector of
positive numeric values which is the same length as the input vectors
`x`/`y`, the point sizes are scaled so that the relative *area* of drawn
points matches the relative magnitude of numbers in the `pointSize`
vector—A point-size legend is also drawn. Thus, if you draw two points
with `pointSize = c(3, 6)`, the second point will be drawn twice the
size (twice the area) of the first. If the range of values is too great,
it is not feasible to represent them using points, because the points
would either get too small to see or too big (covering the whole plot).
Thus, if the largest `pointSize` value is more than 100 times greater
than the smallest, the scaling will be changed to accommodate this. When
this happens, a message will be printed, explaining how the relative
area of drawn points relates to the relative magnitude of `pointSize`
values. For example, you might see a message like: "When comparing the
points in this plot, a doubling of area corresponds to multiplying the
value by three."

## Point shape

By default, each data point is represented by a solid circle. This can
be overridden by passing a `pointStyle` argument. There are sixteen
possible shapes, which are specified by the natural numbers from 1 to
16—try calling `plot(1:16, pch = 1:16)` to see them all. If a single
value is passed to `pointStyle`, all points are drawn with the
corresponding shape. However, if you pass `pointStyle` a vector which is
the same length as the input vectors `x`/`y`, the data is grouped
relative to the unique values of this vector, and a shape is used to
represent each group. (A point-shape legend is automatically drawn.) If
the `pointStyle` vector is numeric, with more than four unique values,
the numeric range is divided into four groups automatically. If the
`pointStyle` vector is discrete (`character`, `logical`, or `factor`),
each unique value is mapped to a point-shape; This only works up to
sixteen unique values—if there are more than 16 unique values in a
discrete `pointStyle` vector, an error will occur.

## Conditional features

If `color` or `pointStyle` arguments are provided to group the data, you
can use the `conditional` argument to control how information is drawn
for each group. For example, if `conditional = list(lm = TRUE)`, a
separate regerssion slope is estimated and drawn within each group, but
if `density = FALSE`, only one overall linear model is estimated using
the whole `x`/`y` input vectors. Other `conditional` options can be
paired with the `normalReference`, `mean`, and `quantiles` arguments,
controlling whether these descriptive values are computed separately in
each group, or across the entire input vectors `x`/`y`. (Note that the
`lm` currently only computes separate models across `color` groups, not
`pointStyle`.)

The `conditional` argument can be specified be either a list of named
`logical` values, with valid names being `normalReference`, `mean`,
`quantiles`, or `lm`. Alternatively, a `character` vector of these names
can be provided. If a singleton `logical` value is provided, the
provided value (`TRUE` or `FALSE`) is used for all the conditional
arguments.
