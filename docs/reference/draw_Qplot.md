# Draw quantile plot

This method draws the distribution of a numeric (continuous) `y`
argument by drawing the values, sorted from lowest to highest, across
the screen from left to right. Position on the X axis corresponds to the
empirical quantiles of the data; for example, the median point (50%
quantile) is exactly in the middle of the X axis. As an alternative, a
single
[violin](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_violins.md)
diagram can be drawn by setting `violin = TRUE`.

## Usage

``` r
draw( , y, col = NA)
      # where y is numeric
```

## Arguments

- log:

  ***Should Y axis be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale).

  Must be a single `character` string, either `""` (linear scale) of
  `"y"` (draw Y on a logarithmic scale).

- line:

  ***Should a line be drawn through the `x`/`y` coordinates, instead of
  points?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- violin:

  ***Should a [violin
  plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_violins.md)
  be drawn instead?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  See
  [draw_violins](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_violins.md).

- normalReference:

  ***Should a Gaussian reference distribution be drawn?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, a normal (Gaussian) distribution is drawn as a dashed black
  line. The mean and standard deviation of this distribution is taken
  from the input vector `y`, within each group. This gives a sense of
  how close to normally distributed `y` is.

- mean:

  ***Should the mean of input vector `y` be marked at the center of the
  plot?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- quantiles:

  ***Should distribution quantiles of `y` be marked?***

  Defaults to `c(.25, .5, .75)`, so the quartiles are drawn.

  Must be a vector of numbers between 0 and 1 (inclusive), or an empty
  vector (the default).

  If any quantiles are specified, each quantile is drawn as a horizontal
  line on the plot, labeled appropriately. For example, `quantiles = .5`
  will draw a line at the median of input vector `y`;
  `quantiles = c(.25, .5, .75)` will draw lines marking the four
  quartiles of `y`.

- conditional:

  ***Should normal reference, density, means, and/or quantiles be
  computed separately for each color/pointStyle group?***

  Defaults to `TRUE`.

  Must be either a singleton `logical` value (an on/off switch), a named
  list of singleton logicals, or a character vector of names. Legal
  names can be `"normalReference"`, `"density"`, `"mean"`, or
  `"quantiles"`.

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
  of the same length as `y`.

- pointStyle:

  ***What shape should points be drawn?***

  Must be a single whole number from 1 to 16, or a vector of discrete
  values of the same length as `y`.

## Details

Up to three additional dimensions can be visually added to the plot
using `color` (color), `pointSize` (point size), and `pointStyle` (point
style) arguments.

## Color

If you pass a single color value to `color`, the whole graph is drawn
that color. However, if you pass `color` a vector of values which is the
exact same length as the input vector `y`, the unique values of this
vector will be used to color the points of the quantile plot. (A color
legend will be drawn automatically.) The colors for each group will be
chosen automatically, unless the entire `color` vector is valid color
values. (Use `alpha` independently to change the transparency.) If the
grouping `color` vector is numeric and there are more than ten unique
values, a continuum of colors is created to represent that numeric
space.

## Point size

By default,
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
chooses an appropriate size to draw data points based on the size of the
input vector `y` and the window size—the more data on the screen the
smaller the points are drawn. You can override this by passing a single
numeric value to `pointSize`; values between about `.2` and `1.5` are
pretty reasonable, typically. However, if you pass `pointSize` vector of
positive numeric values which is the same length as the input vector
`y`, the point sizes are scaled so that the relative *area* of drawn
points matches the relative magnitude of numbers in the `pointSize`
vector. (A point-size legend will be drawn automatically.) Thus, if you
draw two points with `pointSize = c(3, 6)`, the second point will be
drawn twice the size (twice the area) of the first.

If the range of values is too great, it is not feasible to represent
them using points, because the points would either get too small to see
or too big (covering the whole plot). Thus, if the largest `pointSize`
value is more than 100 times greater than the smallest, the scaling will
be changed to accommodate this. When this happens, a message will be
printed, explaining how the relative area of drawn points relates to the
relative magnitude of `pointSize` values. For example, you might see a
message like: "When comparing the points in this plot, a doubling of
area corresponds to multiplying the value by three."

## Point shape

By default, each data point is represented by a solid circle; This can
be overridden by passing a `pointStyle` argument. There are sixteen
possible shapes, which are specified by the natural numbers from 1 to
16—try calling `plot(1:16, pch = 1:16)` to see them all.

If a single value is passed to `pointStyle`, all points are drawn with
the corresponding shape. However, if you pass `pointStyle` a vector
which is the same length as the input vector `y`, the data is grouped
relative to the unique values of this vector, and a shape is used to
represent each group. (A point-shape legend will be drawn
automatically.)

If the `pointStyle` vector is `numeric`, and with more than four unique
values, the numeric range is divided into four groups automatically. If
the `pointStyle` vector is discrete (`character`, `logical`, or
`factor`), each unique value is mapped to a point-shape; This only works
up to sixteen unique values—if there are more than 16 unique values in a
discrete `pointStyle` vector, an error will occur.

## Conditional features

If `color` or `pointStyle` arguments are provided to group the data, you
can use the `conditional` argument to control how information is drawn
for each group. If `conditional = list(density = TRUE)`, data is drawn
to represent the quantiles within each group separately; If
`density = FALSE`, the data points are drawn at their quantile position
within the entire input vector `y`. Other `conditional` options can be
paired with the `normalReference`, `mean`, and `quantiles` arguments,
controlling whether these descriptive values are computed separately in
each group, or across the entire input vector `y`.

The `conditional` argument can be specified be either a list of named
`logical` values, with valid names being `normalReference`, `mean`,
`quantiles`, or `density`. Alternatively, a `character` vector of these
names can be provided. If a singleton `logical` value is provided, the
provided value (`TRUE` or `FALSE`) is used for all the conditional
arguments.

## General Draw Arguments

[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md) is
built on top of R's base plotting system. This means that all the
standard arguments to base-R plots can be used to customize
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots (see [`par()`](https://rdrr.io/r/graphics/par.html) for a full
list). You can also add stuff to
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots using base functions like
[`points()`](https://rdrr.io/r/graphics/points.html),
[`mtext()`](https://rdrr.io/r/graphics/mtext.html), or
[`abline()`](https://rdrr.io/r/graphics/abline.html).
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
also has a number of unique features, including easily dividing data
into multiple plots ("facets").

### Plot Text

Every
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plot can have a title, subtitle, X-axis label, and Y-axis label.

The [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
function will automatically generate X and Y labels for every plot,
usually just using the expression you passed; for example, if you say
`draw(rnorm(100))`, the X label will be "rnorm(100)." This can be
overridden using the `xlabel` and/or `ylabel` arguments, which can be
provided a single string each—to suppress a label, provide an empty
string, like `ylabel = ""`.

Titles and subtitles are specified using the `title` and `subtitle`
arguments, respectively. (Alternatively, you can use the standard `main`
and `sub` arguments.) No title or subtitle is drawn by default.

You can use the base-R
[`mtext()`](https://rdrr.io/r/graphics/mtext.html) function to draw
additional text on plot axes.

### Axes control

The [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
function will select reasonable X and Y axes ranges automatically. If
you want to override the defaults, you can use `xlimit` or `ylimit` to
control the range of values shown on each axis. Each of these must be
passed a vector of two numbers, representing the left and right X-axis
extremes (`xlimit`) or the bottom and top Y-axis extremes (`ylimit`).
For example, to show data in the range \\\[10, 50\]\\ on the X axis,
specify `xlimit = c(10, 50)`.

For `numeric` axes, you can also plot data on a logarithmic scale using
the `log` argument. This is set by providing a single `character` string
containing a lower-case `"x"`, `"y"`, or both (`"xy"`). Note that
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
will throw an error if you try to plot negative values on a logarithmic
scale. Also note that some plots will not allow logarithmic scaling on
some axes, and will simply ignore attempts to do that.

### Window control

Normally, a plot is drawn to fill the available graphical device using
base-R's normal algorithm. However, the `aspect` argument can override
this, controlling the aspect ratio of the plot. The `aspect` argument
must be a single numeric value between `0.5` and `5`. If `aspect = 1`
the plot is drawn as a square. Try settings like `aspect = 4/3` or
`aspect = 16/9`.

The `margin` argument controls the portion of the screen used for the
plot margins, with legal values ranging from `0.4` to `0.1`. The margins
are used to draw all text (axis labels, title, etc.), so they are
needed. The default value is `0.2`—using other margins may result in
less optimal placement of plot text and legends.
