# Draw "violin" plot

This method draws the distribution of a numeric (continuous) `y`
variable within groups defined by a categorical (discrete) `x` variable,
creating a "[violin
plot](https://en.wikipedia.org/wiki/Violin_plot)"—so-called for its
characteristic shape that can (sometimes) resemble the shape of a
violin. If there are too many violins (categories), the plot would be
unreadable, so
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
will throw an error if the `x` argument has more than 25 unique values.

## Usage

``` r
draw(x, y)
     # where x is discrete data
     # and   y is numeric
```

## Arguments

- smooth:

  ***Should a smoothed density curve be estimated for each group?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, a smooth density contour is estimated from each group of
  `y` values, using [`density()`](https://rdrr.io/r/stats/density.html).
  If `FALSE`, `y` values are binned using
  [`hist()`](https://rdrr.io/r/graphics/hist.html).

- conditional:

  ***Should normal reference, density, means, and/or quantiles be
  computed separately for each violin?***

  Defaults to `TRUE`.

  Must be either a singleton `logical` value (an on/off switch), a named
  list of singleton logicals, or a character vector of names. Legal
  names can be `"normalReference"`, `"density"`, `"mean"`, or
  `"quantiles"`.

- mean:

  ***Should the mean of input vector `y` be marked?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  The X-position of the mean mark is determined by the relative
  frequency of the three groups, as arranged on the screen. Groups with
  more data will pull the mean-X position toward them.

- quantiles:

  ***Should distribution quantiles of `y` be marked?***

  Defaults to `c(.25, .75)`, so the inter-quartile interval is shown.

  Must be a vector of numbers between 0 and 1 (inclusive), or an empty
  vector.

  If any quantiles are specified, each quantile is drawn as a horizontal
  line on the plot, labeled appropriately. For example, `quantiles = .5`
  will draw a line at the median of input vector `y`;
  `quantiles = c(.25, .5, .75)` will draw lines marking the four
  quartiles of `y`.

- normalReference:

  ***Should a Gaussian reference distribution be drawn?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, a normal (Gaussian) distribution is drawn as a dashed black
  line. The mean and standard deviation of this distribution is taken
  from the input vector `y`. This gives a sense of how close to normally
  distributed `y` is within.

- showPoints:

  ***Should individual points from `y` be overlaid on the violin(s)?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, individual data points from the `y` input vector are
  plotted in a "cloud" over each violin. The Y-position of each point is
  matched to its actual value; The X-position of each point is randomly
  (uniformly) spread across the area of each violin. The points will
  look consistent, regardless of what histogram- or density-algorithm
  parameters are used.

- log:

  ***Should Y axis be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale).

  Must be a single `character` string, either `""` (linear scale) of
  `"y"` (draw Y on a logarithmic scale ).

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

A violin plot is much like a density
[histogram/contour](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_density.md)
plot turned on its side, with the shape mirrored left to right. Like
[`draw_density()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_density.md),
the violin-plot density shape can be generated using either R's
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

Whether smoothed or binned, the width of each "violin" represents the
probability **density**. The width of bars (when `smooth = FALSE`) does
*not* correspond exactly to the the probability **mass** in each bin,
because that actually depends on the size of the bins; For narrow bins
(or continuous contours), density can even be greater than 1. If bin
sizes are all equal, then the *relative* width of the density bars
*does* map exactly to the relative probability mass of each bin. If bins
are *not* equal width—which can only happen if you manually specify
unequal bins using the `breaks` argument—the widths of bars *don't* map
to probability mass. However, using the density assures that the
relative **area** of each bin *does* match the probability mass
associated with that bin, even if the bins are of unequal size. The
scale of the densities in the plot is shown through the width of a set
of lines above each violin.

## Color

By default, each violin is automatically drawn a different color, with a
color legend drawn as well. (A color legend will be drawn
automatically.) However, these colors can be controlled using the
`color` argument; you can provide a single color for all the violins or
a vector of unique colors exactly the same length as the number of
categories (unique values in `x`). These colors are mapped to the
violins, from left to right. (Use `alpha` independently to change the
transparency.)

### Conditional features

You can use the `conditional` argument to control how information is
drawn for each violin. If `conditional = list(density = TRUE)`, each
violin is scaled to sum to 1—this is good to see the details of the
distribution within each group. If `density = FALSE`, each violin is
drawn in its true proportion in the data. Other `conditional` options
can be paired with the `normalReference`, `mean`, and `quantiles`
arguments, controlling whether these descriptive values are computed
separately for each violin, or across the entire input vector `y`.

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
