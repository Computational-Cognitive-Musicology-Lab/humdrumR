# Visualize data

The `draw()` function is humdrumR's go-to plotting function, which can
make a variety of graphs depending on the type of data you give it. For
the most part, `draw()` is simply a easy-to-use wrapper around the
base-R graphics functions—Anything that `draw()` does can be done using
normal base-R functions
([`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`barplot()`](https://rdrr.io/r/graphics/barplot.html), etc.), but
`draw()` makes making good looking plots faster and easier.

## Usage

``` r
draw(
  x,
  y,
  facets = list(),
  ...,
  xlab = NULL,
  ylab = NULL,
  axes = 1:4,
  legend = TRUE,
  aspect = NULL,
  margin = 0.2,
  main = "",
  sub = "",
  col = 1,
  cex = NULL,
  pch = NULL
)
```

## Arguments

- ...:

  ***Additional parameters to pass to par().***

  Any base-R graphing parameters that can be set using the
  [`par()`](https://rdrr.io/r/graphics/par.html) function may be passed
  to `draw()`. These parameters are set using
  [`par()`](https://rdrr.io/r/graphics/par.html) (overriding humdrumR's
  defaults), but only for the duration of the `draw()` call—i.e., the
  global [`par()`](https://rdrr.io/r/graphics/par.html) settings are not
  changed.

- xlab, ylab:

  ***What X and/or Y axis labels should be used?***

  Must be single `character` strings.

  See "Plot Text" section below.

- legend:

  ***Should legends be drawn?***

  Defaults to `TRUE`.

- margin:

  ***How big should plot margins be?***

  Defaults to `0.2`.

  Must be as single numeric value \\0.4 \geq margin \geq 0.1\\.

  This controls the proportion of the plotting area used for margins. A
  value of `0.2` means that 20% of the plotting area is used for the
  margins.

- col:

  ***What colors should be used in plots?***

  See "Color" section below.

- cex:

  ***What size of points should be plotted?***

  See "Point size" section below.

- xlim, ylim:

  ***What range of X/Y values should drawn on the plot?***

  By default, X and Y limits are automatically selected.

  Must be a `numeric` vector of length two.

  The first number of each `xlim`/`ylim` vector specifies the
  left/bottom edge of the X/Y axis. The second number specifies the
  right/top edge.

- \`aspect\`:

  Defaults to `NULL`.

  Must be a single numeric value \\5 \geq aspect \geq 0.2\\, or `NULL`.

  This controls the aspect ratio of the plot: `1` (square), `4/3`,
  `16/9`, etc. If `aspect` is `NULL` , R \#' automatically uses the
  current aspect of your current plotting device.

- heat:

  ***Should a heatmap be drawn?***

  Defaults to `FALSE`, unless the input has two or more dimensions *and*
  at least 80 conditions.

  Must be a singleton `logical` value: an on/off switch.

- normalReference:

  ***Should a reference Normal distribution by overlayed?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- showCounts:

  ***Should the number of observations in each category be drawn on the
  plot?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- showPoints:

  ***Should individual data points be plotted above the histogram?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- log:

  ***Should X and/or Y axes be drawn on a logarithmic scale?***

  Defaults to `""` (linear scale on both axes).

  Must be a single `character` string; options are `"x"` (X axis on log
  scale), `"y"` (Y axis on log scale), and `"xy"` (both axes on log
  scale).

- smooth:

  ***Should continuous values be grouped using density estimation or
  binning?***

  Defaults to `TRUE` for violin and area plots, but `FALSE` for
  histograms.

  Must be a singleton `logical` value: an on/off switch.

## Details

`draw()` is a generic function, which does different plots depending on
the data you pass to its `x` and `y` arguments. The following table
indicates the seven possibilities, with separate section below
explaining how each plot can be modified.

|  |  |  |
|----|----|----|
| `x` | `y` | Plot type |
| `numeric` | (missing) | Density Histogram/Contour |
| (missing) | `numeric` | Quantile plot |
|  |  | (or Violin plot) |
| `numeric` | `numeric` | Scatter plot |
| 1 or 2 dimensional [table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md) or [distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md) |  |  |
| `character` or `factor` | (additional `character`/`factor`) | Barplot or Heatmap |
| `character` or `factor` | `numeric` | Violin plot |
| `numeric` | `character` or `factor` | Area chart |

For purely categorical data, the default behavior is to draw barplots
for 1D distributions, or 2D distributions with 80 or fewer conditions,
and heatmaps otherwise. This default behavior can be overridden using
either `heat = TRUE` or `heat = FALSE`. Similarly, `violin = TRUE` can
be used to force `draw( , y)` to draw a violin plot.

Note that, if you pass one or two `character`/`factor` vectors to
`draw()`, it will pass these vectors to
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
then pass the resulting
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
to `draw()`, creating a barplot. Thus, `draw(charvec1, charvec2)` is
equivalent to `draw(count(charvec1, charvec2))`.

### Drawing dimensions of data

`draw()` is equipped to visualize data in up to four dimensions in one
plot. The main dimensions are, of course, the X and Y axes, controlled
by the `x` and `y` arguments. The other two dimensions are color (`col`)
and point-size (`cex`). An another approach is to draw multiple plots at
the same time in a grid, each sub plot called a "facet." Details for all
these options can be found below.

## Common Arguments

`draw()` is built on top of R's "base" plotting system. This means that
all the standard arguments to base-R plots can be used to customize
plots (See [`par()`](https://rdrr.io/r/graphics/par.html) for a full
list) or add to them (for example, using
[`points()`](https://rdrr.io/r/graphics/points.html) or
[`mtext()`](https://rdrr.io/r/graphics/mtext.html)). However, `draw()`
has a number of special additional features, including easily plotting
"facets" (dividing data into multiple plots).

### Plot Text

Every `draw()` plot can have a title, subtitle, X-axis label, and Y-axis
label.

The `draw()` function will automatically generate X and Y labels for
every plot, usually just using the expression you passed; for example,
if you say `draw(rnorm(100))`, the X label will be "rnorm(100)." This
can be overriden using the `xlab` and/or `ylab` arguments, which can be
provided a single string each—to surpress a label, provide an empty
string, like `ylab = ""`.

Titles and subtitles are specified using the `main` and `sub` arguments,
respectively. No title or subtitle is drawn by default.

You can use the base-R
[`mtext()`](https://rdrr.io/r/graphics/mtext.html) function to draw
additional text on plot axes.

### Axes control

The `draw()` function will select reasonable X and Y axes ranges
automatically. If you want to override the defaults, you can use `xlim`
or `ylim` to control the range of values shown on each axis. Each of
these must be passed a vector of two numbers, representing the left and
right X-axis extremes (`xlim`) and the bottom and top Y-axis extremes
(`ylim`). For example, to show data in the range \\\[10, 50\]\\ on the X
axis, specifiy `xlim = c(10, 50)`.

For `numeric` axes, you can also plot data on a logarithmic scale using
the `log` argument. This set by providing a single `character` string
containing lower-case `"x"`, `"y"`, or both (`"xy"`). Note that `draw()`
will throw an error if you try to plot negative values on a logarithmic
scale. Note that some plots will not allow logarithmic scaling on some
axes, and will simply ignore attempts to do that.

### Color

Colors can be specified in all `draw()` plots using the `col` argument,
along with the `alpha` argument which controls the transparency of
colors. Note that color control can used for entirely aesthetic purposes
(picking a color scheme you want) *or* to represent an additional
dimension of data.

Colors can be specified several ways:

- As names (e.g., `"red"` or `"darkgreen'`)

- As hex codes (e.g., `"#ff0000"` or `"#00ff00`)

- Using the [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) function.

  - If a single `col` value is provided, all points are drawn this
    color.

  - If the `col` value is the same length as `x` and `y`, a scale of
    colors (either discrete of continuous) is generated to match the
    values this variable takes, and a legend is drawn.

- As natural numbers, indexing `humdrumR`'s flatly palette, based on the
  colors `'#18BC9C'`, `'#3498DB'`, `'#F39C12'`, `'#E74C3C'`, and
  `'#2C3E50'`.

The `alpha` argument must be a `numeric` value \\1 \geq alpha \geq 0\\,
where `alpha = 0` is totally transparent and `alpha = 1` is totally
opaque.

It is always possible to specify a single color value for a plot.
However, `draw()` can also (generally) accept more color values,
depending on the type of plot. For some plots, multiple colors are used
(aesthetically) by default; For other types of plots, its possible to
use color to represent an additional dimension of information. In the
"Specific Plot Types" subsections below, the details of how each plot
type interprets the `col` argument are explained.

## Specific Plot Types

### Density Histogram or Contour (x = numeric, y =)

To draw a single numeric variable on the X axis, `draw()` either bins
the numbers to create a histogram, or estimates a smooth density contour
to draw. To choose which approach, use the `smooth` argument:
`smooth = FALSE` (default) for histogram, and `smooth = TRUE` for
density contour. The `draw()` function relies on the algorithms used by
base-R's [`hist()`](https://rdrr.io/r/graphics/hist.html) and
[`density()`](https://rdrr.io/r/stats/density.html) functions,
respectively, for these two tasks. In many cases, we can pass arguments
directly through to these functions. For example, the `breaks` argument
can be passed through to
[`hist()`](https://rdrr.io/r/graphics/hist.html), or the `bw` and
`kernel` arguments to
[`density()`](https://rdrr.io/r/stats/density.html).

Whether smoothed or binned, the Y axis represents the probability
density. This means that the height of histogram bars does not
correspond exactly to the the probability mass in each bin, because it
depends on the width of the bins; For narrow bins, density can even be
greater than 1. If bin sizes are all equal, then the relative height of
he density bars does map exactly to the relative probability mass of
each bin. However, if bins are not equal width—which can only happen if
you manually specify unequal bins using the `breaks` argument—the
heights of bars *don't* map to probability mass. However, using the
density assures that the relative **area** of each bin does match the
probability mass associated with that bin, even if the bins are of
unequal width. This is ideal for a plot.

A second dimension can be visually added to the plot using the `col`
(color) argument. If you pass a single color, the whole graph is drawn
that color. However, if you pass `col` a vector of values which is the
exact same length as the input vector `x`, the unique values of this
vector will be used to group the `x` data and a separate density-graph
will be drawn for each group, with its own color. (A color legend will
be drawn automatically.) The colors for each group will be chosen
automatically, unless the entire `col` vector is valid color values.
(Use `alpha` independently to change the transparency.) If the grouping
`col` vector is numeric and there are more than five unique values, the
numbers are automatically divided into (at most) five bins. Use the
`conditional` argument (see below) to control how the densities of each
group are scaled relative to each other.

#### Other histogram arguments

In addition to Plot Text and Axes Control parameters (listed above), as
well as the dimensional `col` argument, arguments understood by
histogram/density plots are listed below. The following arguments are
all singleton `logical` on/off switches (`TRUE` or `FALSE`), unless
otherwise indicated.

- `smooth` — Controls whether histogram is binned or smooth.

  - Defaults to `FALSE`.

- `showPoints` — Controls whether individual data points from the `x`
  input vector are plotted in a "cloud" above the density plot.

  - Defaults to `TRUE`.

  - The X-position of each point is matched to its actual value.

  - The Y-position of each point is randomly (uniformly) selected in a
    range at the top of the plot window. This spaces out points that are
    close together, so it is easier to see how dense they are.

- `showCounts` — Controls whether the actual count of values associated
  with each bin (or contour) is printed above the each bar/contour.

  - Defaults to `TRUE`.

  - Note that, if the data is grouped into multiple draws by `col` (see
    above), there is no guarantee the counts won't be drawn on top of
    each other.

- `normalReference` — If `TRUE`, a normal (Gaussian) distribution is
  drawn as a dashed black line. The mean and standard deviation of this
  distribution is taken from the input vector `x`. This gives a sense of
  how close to normally distributed `x` is.

  - Defaults to `FALSE`.

- `conditional` — If the data is grouped in multiple draws by `col` (see
  above), should the density of each color match it's global share in
  the distribution of input variable `x`, or be rescaled in each group
  to sum/integrate to 1? I.e., should probabilities be conditioned on
  the grouping factor?

  - Setting `conditional = TRUE` is useful if you want to see the
    details of how each group is distributed.

  - `conditional = FALSE` (the default) is useful when you want to see
    the actual proportion of data in each group (if they are different
    sizes).

- `quantiles` — Must be a vector of numbers between 0 and 1 (inclusive),
  or an empty vector (the default).

  - By default, `quantiles = c()` and no quantiles are drawn.

  - If any quantiles are specified, each quantile is drawn as a vertical
    line on the plot, labeled appropriately. For example,

    - `quantiles = .5` will draw a line at the median of input vector
      `x`.

    - `quantiles = c(.25, .5, .75)` will draw lines marking the four
      quartiles of `x`.

- `mean` — If `TRUE`, the mean of input vector `x` is marked on the X
  axis below the plot, using a cross-hair symbol.

  - Defaults to `FALSE`.

- `global_quantiles` — If the data is grouped into multiple draws by
  `col` (see above), and if `quantiles` or the `mean` are going to be
  drawn, should they be computed separately for each group or for the
  whole (global) distribution of the input variable `x`?

  - Defaults to `TRUE`.

  - If `global_quantiles = TRUE`, the overall quantiles and/or mean of
    input vector `x` are drawn. If the data is grouped by `col` and
    `global_quantiles = FALSE`, quantiles and/or means are instead
    computed and drawn separately for each group. This can get very
    messy very quickly!

### Quantile plot (x = , y = numeric)

To draw a single numeric variable on the Y axis, `draw()` sorts the
input vector `y` and draws each point on the Y axis, from lowest to
highest, equally spaced across the X axis. This means that the position
on the X axis corresponds to the empirical quantiles of the data; for
example, the median point (50% quantile) is exactly in the middle of the
X axis. Up to three additional dimensions can be visually added to the
plot using `col` (color), `cex` (point size), and `pch` (point style)
arguments.

If you pass a single color value to `col`, the whole graph is drawn that
color. However, if you pass `col` a vector of values which is the exact
same length as the input vector `x`, the unique values of this vector
will be used to color the points of the quantile plot. (A color legend
will be drawn automatically.) The colors for each group will be chosen
automatically, unless the entire `col` vector is valid color values.
(Use `alpha` independently to change the transparency.) If the grouping
`col` vector is numeric and there are more than ten unique values, a
continuum of colors is created to represent that numeric space.

By default, `draw()` chooses an appropriate size to draw data points
based on the size of the input vector `y` and the window size—the more
data on the screen, smaller the points are drawn. You can override this
by passing a single numeric value to `cex`; values between about `.2`
and `1.5` are pretty reasonable, typically. However, if you pass `cex`
vector of positive numeric values which is the same length as the input
vector `y`, the point sizes are scaled so that the relative *area* of
drawn points matches the relative magnitude of numbers in the `cex`
vector—A point-size legend is also drawn. Thus, if you draw two points
with `cex = c(3, 6)`, the second point will be drawn twice the size
(twice the area) of the first. If the range of values is too great, it
is not feasible to represent them using points, because the points would
either get too small to see or too big (covering the whole plot). Thus,
if the largest `cex` value is more than 100 times greater than the
smallest, the scaling will be changed to accommodate this. When this
happens, a message will be printed, explaining how the relative area of
drawn points relates to the relative magnitude of `cex` values For
example, you might see a message like: "When comparing the point in this
plot, a doubling of area corresponds to multiplying the value by three."

By default, each data point is represented by a solid circle. This can
be overriden by passing a `pch` argument. There are sixteen possible
shapes, which are specified by the natural numbers from 1 to 16—try
calling `plot(1:16, pch = 1:16)` to see them all. If a single value is
passed to `pch`, all points are drawn with the corresponding shape.
However, if you pass `pch` a vector which is the same length as the
input vector `y`, the data is grouped relative to the unique values of
this vector, with a shape associated with each group. A point-shape
legend is automatically drawn. If the `pch` vector is numeric, with more
than four unique values, the numeric range is divided into four groups
automatically. If the `pch` vector is discrete (`character`, `logical`,
or `factor`), each unique value is mapped to a point-shape; This only
works up to sixteen unique values—if there are more unique values in a
discrete `pch` vector, an error will occur.

In addition to Plot Text and Axes Control parameters (listed above), and
dimensional arguments `y`, `col`, `cex`, and `pch`, arguments understood
by quantile plots are listed below. The following arguments are all
singleton `logical` on/off switches (`TRUE` or `FALSE`), unless
otherwise indicated.

- `normalReference` — If `TRUE`, a normal (Gaussian) distribution is
  drawn as a dashed black line. The mean and standard deviation of this
  distribution is taken from the input vector `y`. This gives a sense of
  how close to normally distributed `y` is.

  - Defaults to `FALSE`.

- `quantiles` — Must be a vector of numbers between 0 and 1 (inclusive),
  or an empty vector.

  - If any quantiles are specified, each quantile is drawn as a
    horizontal line on the plot, labeled appropriately. For example,

    - `quantiles = .5` will draw a line at the median of input vector
      `y`.

    - `quantiles = c(.25, .5, .75)` will draw lines marking the four
      quartiles of `y` (this is the default for quantile plots).

- `mean` — If `TRUE`, the mean of input vector `y` is marked on the Y
  axis at the center of plot, using a cross-hair symbol.

  - Defaults to `FALSE`.

- `violin` — If `TRUE`, a single violin plot is drawn instead of a
  quantile plot. (See details about these plots below.)

### Scatter plot (x = numeric, y = numeric)

To draw a pair of numeric variables, `draw()` creates a scatter plot
with the `x` and `y` input variables mapped to position on the X and Y
axes respectively. The input vectors must be equal length, unless one of
the pair is a single scalar, in which case that scalar is recycled to
match the length of the other input. (Thus, providing a scalar `x` or
`y` causes the other variable to be drawn on a straight line.) Up to
three additional dimensions can be visually added to the plot using
`col` (color), `cex` (point size), and `pch` (point style) arguments.

If you pass a single color value to `col`, the whole graph is drawn that
color. However, if you pass `col` a vector of values which is the exact
same length as the input vectors `x`/`y`, the unique values of this
vector will be used to color the points of the scatter plot. (A color
legend will be drawn automatically.) The colors for each group will be
chosen automatically, unless the entire `col` vector is valid color
values. (Use `alpha` independently to change the transparency.) If the
grouping `col` vector is numeric and there are more than ten unique
values, a continuum of colors is created to represent that numeric
space.

By default, `draw()` chooses an appropriate size to draw data points
based on the density of the X and Y points in the window—the more data
on the screen, smaller the points are drawn. You can override this by
passing a single numeric value to `cex`; values between about `.2` and
`1.5` are pretty reasonable, typically. However, if you pass `cex`
vector of positive numeric values which is the same length as the input
vector `y`, the point sizes are scaled so that the relative *area* of
drawn points matches the relative magnitude of numbers in the `cex`
vector—A point-size legend is also drawn. Thus, if you draw two points
with `cex = c(3, 6)`, the second point will be drawn twice the size
(twice the area) of the first. If the range of values is too great, it
is not feasible to represent them using points, because the points would
either get too small to see or too big (covering the whole plot). Thus,
if the largest `cex` value is more than 100 times greater than the
smallest, the scaling will be changed to accommodate this. When this
happens, a message will be printed, explaining how the relative area of
drawn points relates to the relative magnitude of `cex` values For
example, you might see a message like: "When comparing the point in this
plot, a doubling of area corresponds to multiplying the value by three."

By default, each data point is represented by a solid circle. This can
be overridden by passing a `pch` argument. There are sixteen possible
shapes, which are specified by the natural numbers from 1 to 16—try
calling `plot(1:16, pch = 1:16)` to see them all. If a single value is
passed to `pch`, all points are drawn with the corresponding shape.
However, if you pass `pch` a vector which is the same length as the
input vector `y`, the data is grouped relative to the unique values of
this vector, with a shape associated with each group. A point-shape
legend is automatically drawn. If the `pch` vector is numeric, with more
than four unique values, the numeric range is divided into four groups
automatically. If the `pch` vector is discrete (`character`, `logical`,
or `factor`), each unique value is mapped to a point-shape; This only
works up to sixteen unique values—if there are more unique values in a
discrete `pch` vector, an error will occur.

In addition to Plot Text and Axes Control parameters (listed above), and
dimensional arguments `y`, `col`, `cex`, and `pch`, arguments understood
by scatter plots are listed below. The following arguments are all
singleton `logical` on/off switches (`TRUE` or `FALSE`), unless
otherwise indicated.

- `normalReference` — If `TRUE`, a bivariate normal (Gaussian)
  distribution is drawn under the scatter plot, using the means and
  variances of, and the covariance between, the input vectors `x` and
  `y`. To visualize this two dimensional distribution, a random sample
  of points from this bivariate distribution is drawn in large black,
  but mostly transparent points. This gives a sense of how close to
  jointly-normally distributed `x` and `y` are.

  - Defaults to `FALSE`.

- `lm` — If `TRUE`, the simple regression line is estimated using [lm(y
  ~ x)](https://rdrr.io/r/stats/lm.html) and this regression line and
  its 95% confidence limits—estimated using
  [`predict.lm()`](https://rdrr.io/r/stats/predict.lm.html)—are drawn
  (as solid lines and dashed lines respectively). The regression
  coefficients themselves are also drawn in a legend at the top left
  corner of the plot.

- `quantiles` — Must be a vector of numbers between 0 and 1 (inclusive),
  or an empty vector.

  - By default, `quantiles = c()` and no quantiles are drawn.

  - If any quantiles are specified, each quantile is drawn as on the
    plot and labeled appropriately. Quantiles are computed separately
    for input vectors `x` and `y` drawn using vertical and horizontal
    lines respectively. For example,

    - `quantiles = .5` will draw a lines which converge at the medians
      of `x` and `y`.

    - `quantiles = c(.25, .5, .75)` will draw vertical and horizontal
      lines marking the four quartiles of `x` and `y` (creating a grid
      with 16 cells).

- `mean` — If `TRUE`, a cross-hair symbol is drawn marking at the point
  marking the means of `x` and `y`.

  - Defaults to `FALSE`.

## Facets
