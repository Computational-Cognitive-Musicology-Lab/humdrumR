# Draw bar plot

This method draws [tabulated
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md) as a
bar plot. If the data is passed to the first (`x`) argument, the bar
plot is oriented vertically; If the data is instead passed to the second
(`y`) argument—-with `x` missing—, the bar plot is oriented
horizontally, from left to right. The input table can have one or two
dimensions; if more dimensions are provided the third and fourth
dimension are split across draw facets.

## Usage

``` r
draw(x, col = NA)
           # where x is table/count data
```

## Arguments

- log:

  ***Should X and/or Y axes be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale on both axes).

  Must be a single `character` string; options are `"x"` (X axis on log
  scale), `"y"` (Y axis on log scale), and `"xy"` (both axes on log
  scale).

- horizontal:

  ***Should bars be drawn horizontally?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- stacked:

  ***Should bars be stacked on top of each other?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value, or `NULL`. If `NULL`, both
  stacked *and* side-by-side bars are drawn.

- heat:

  ***Should a heat map be drawn, instead of a barplot?***

  Defaults to `TRUE`, if there are two dimensions and the total number
  of bars is greater than `80`. Otherwise, defaults to `FALSE`.

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

- mean:

  ***Should the mean of input vectors `x` and `y` be marked on the
  plot?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- showCounts:

  ***Should the counts of values in each bar be printed above the
  bar?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

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

The `draw_barplot()` method will accept tabular data created `humdrumR`
functions
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
or equivalent base-R functions
[`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)/[`proportions()`](https://rdrr.io/r/base/proportions.html).
If a single atomic vector of discrete values is passed to either `x` or
`y`, the discrete values are automatically
[counted](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
and the resulting table is passed to `draw_barplot()`. Thus, if
`mydiscrete` is a vector of discrete values (like `character`), calling
`draw(mydiscrete)` is the same as calling `draw(count(mydiscrete))`.

For count data (natural numbers), Y-axis labels for counts and
proportion of total are shown; For proportion data (real numbers between
0 and 1) only a proportion key is shown; If the table includes negative
numbers, the Y axis is simply labeled "value."

### Dimensions

A single-dimensional table (representing one variable) is simply drawn
as a set of bars. However, if the input table is two dimensional, a
double bar plot is drawn, with bars representing every combination of
levels across the two dimensions; Bars representing values of the first
dimension are drawn in groups representing each level of second
dimension. However, if the total number of bars to draw is greater than
80, [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
will dispatch
[`draw_heat()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_heat.md)
instead. This behavior can be overridden using the `heat` argument.

### Barplot types

By default, bars are drawn side by side, so their relative height is
easiest to compare. However, is `stacked = TRUE`, a "stacked" bar plot
will be drawn, with bars stacked on top of each other. If
`stacked = NULL`, side-by-side *and* stacked plots are drawn—the bars
are drawn side by side, but then redrawn (with more transparency)
stacked on top of the right-most bar.

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
