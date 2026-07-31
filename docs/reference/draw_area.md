# Draw area plot

This method draws the distribution of a categorical (discrete) `y`
variable across the range of a numeric (continuous) `x` variable,
creating an "[area plot](https://en.wikipedia.org/wiki/Area_chart)."
Specifically, colored areas representing the estimated probability
density of each unique category in `y`, depending on the value of `x`,
are stacked on top of each other. The result is like stacking multiple
histograms on top of each other.

## Usage

``` r
draw(x, y, col = NA) 
     # where x is numeric
     # and   y is discrete data
```

## Arguments

- log:

  ***Should X axis be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale).

  Must be a single `character` string, either `""` (linear scale) of
  `"x"` (draw X on a logarithmic scale ).

- center:

  ***Should the stacked areas be centered on the screen?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the areas are centered in the Y-axis, expanding outward (up
  and down) when the density increases. If `FALSE`, the areas are
  stacked up from the bottom of the screen.

- smooth:

  ***Should a smoothed density curve be estimated for each group?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, a smooth density contour is estimated from each group of
  `x` values, using [`density()`](https://rdrr.io/r/stats/density.html).
  If `FALSE`, `x` values are binned using
  [`hist()`](https://rdrr.io/r/graphics/hist.html).

- conditional:

  ***Should density, means, and/or quantiles be computed separately for
  each color area?***

  Defaults to `TRUE`.

  Must be either a singleton `logical` value (an on/off switch), a named
  list of singleton logicals, or a character vector of names. Legal
  names can be `"density"`, `"mean"`, or `"quantiles"`.

- mean:

  ***Should the mean of input vector `x` be marked below the plot?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- quantiles:

  ***Should distribution quantiles of `y` be marked?***

  Defaults to [`c()`](https://rdrr.io/r/base/c.html), so no quantiles
  are drawn.

  Must be a vector of numbers between 0 and 1 (inclusive), or an empty
  vector.

  If any quantiles are specified, each quantile is drawn as a vetical
  line on the plot, labeled appropriately. For example, `quantiles = .5`
  will draw a line at the median of input vector `x`;
  `quantiles = c(.25, .5, .75)` will draw lines marking the four
  quartiles of `x`.

- showPoints:

  ***Should individual points from `x` be shown above the density
  plot?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, individual data points from the `x` input vector are
  plotted in a "cloud" above the area plot. The X-position of each point
  is matched to its actual value; The Y-position of each point is
  randomly (uniformly) selected in a range at the top of the plot
  window. This spaces out points that are close together, so it is
  easier to see how dense they are.

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

## Details

Like
[`draw_density()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_density.md),
the area-plot density shape(s) can be generated using either R's
[`density()`](https://rdrr.io/r/stats/density.html) or
[`hist()`](https://rdrr.io/r/graphics/hist.html) algorithms, for smooth
and binned plots respectively. We can pass arguments directly through to
these functions: For example, the `breaks` argument can be passed
through to [`hist()`](https://rdrr.io/r/graphics/hist.html), or the `bw`
and `kernel` arguments to
[`density()`](https://rdrr.io/r/stats/density.html). The smooth density
contour is drawn by default; to draw a binned plot, use
`smooth = FALSE`.

### Density vs Mass

Whether smoothed or binned, the height of each color at each X
coordinate represents the probability **density** of each category. The
height of bars (when `smooth = FALSE`) does *not* correspond exactly to
the the probability **mass** in each bin, because that actually depends
on the size of the bins; For narrow bins (or continuous contours),
density can even be greater than 1. If bin sizes are all equal, then the
*relative* width of the density bars *does* map exactly to the relative
probability mass of each bin. If bins are *not* equal width—which can
only happen if you manually specify unequal bins using the `breaks`
argument—the widths of bars *don't* map to probability mass. However,
using the density assures that the relative **area** of each bin *does*
match the probability mass associated with that bin, even if the bins
are of unequal size. The scale of the densities in the plot is shown
through the width of a set of lines to the left of the plot.

## Color

By default, colors are automatically chosen to represent the categories
in the `y` variable. (A color legend will be drawn automatically.)
However, these colors can be controlled using the `color` argument; you
can provide a vector of unique colors exactly the same length as the
number of categories (unique values in `y`). These colors are mapped to
the areas, from bottom up. (Use `alpha` independently to change the
transparency.)

### Conditional features

You can use the `conditional` argument to control how information is
drawn for each violin. If `conditional = list(density = TRUE)`, each
color area is scaled to sum to 1—this is good to see the details of the
distribution within each group. If `density = FALSE`, each color area is
drawn in its true proportion in the data. Other `conditional` options
can be paired with the `mean`, and `quantiles` arguments, controlling
whether these descriptive values are computed separately for each area,
or across the entire input vector `x`.

The `conditional` argument can be specified be either a list of named
`logical` values, with valid names being `mean`, `quantiles`, or
`density`. Alternatively, a `character` vector of these names can be
provided. If a singleton `logical` value is provided, the provided value
(`TRUE` or `FALSE`) is used for all the conditional arguments.

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
