# Visualize data

The `draw()` function is humdrumR's go-to plotting function, which can
make a variety of graphs depending on the type of data you give it. For
the most part, `draw()` is just an easy-to-use wrapper around the base-R
graphics functions—Anything that `draw()` does can be done using normal
base-R functions
([`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`barplot()`](https://rdrr.io/r/graphics/barplot.html), etc.). However,
`draw()` makes making good looking plots faster and easier.

## Usage

``` r
draw(x, y, ...)
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

- xlabel, ylabel:

  ***What X and/or Y axis labels should be used?***

  Must be single `character` strings.

  See "Plot Text" section below.

- xlimit, ylimit:

  ***What range of X/Y values should drawn on the plot?***

  By default, X and Y limits are automatically selected.

  Must be a `numeric` vector of length two.

  The first number of each `xlimit`/`ylimit` vector specifies the
  left/bottom edge of the X/Y axis. The second number specifies the
  right/top edge.

- legend:

  ***Should legends be drawn?***

  Defaults to `TRUE`.

- aspect:

  ***Control the aspect ratio of the plot.***

  Defaults to `NULL`.

  Must be a single numeric value \\5 \geq aspect \geq 0.2\\, or `NULL`.

  This controls the aspect ratio of the plot: `1` (square), `4/3`,
  `16/9`, etc. If `aspect` is `NULL` , R automatically uses the current
  aspect of your current plotting device.

- margin:

  ***How big should plot margins be?***

  Defaults to `0.2`.

  Must be as single numeric value \\0.4 \geq margin \geq 0.1\\.

  This controls the proportion of the plotting area used for margins. A
  value of `0.2` means that 20% of the plotting area is used for the
  margins.

- heat:

  ***Should a heatmap be drawn?***

  Defaults to `FALSE`, unless the input has two or more dimensions *and*
  at least 80 conditions.

  Must be a singleton `logical` value: an on/off switch.

## Value

Returns a `draw_object`, which renders the plot when printed, or can be
passed to other functions.

## Details

`draw()` is a generic function, which does different sorts of plots
depending on the data you pass to its `x` and `y` arguments. The
following table indicates the seven possible plot types, and which
combinations of `x` and `y` variable classes result in each type. The
links in the table lead to detailed documentation for each type of plot.

|                       |                         |                                                                                               |
|-----------------------|-------------------------|-----------------------------------------------------------------------------------------------|
| `x`                   | `y`                     | Plot type                                                                                     |
| `numeric`             | (missing)               | [Density Histogram/Contour](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_density.md) |
| (missing)             | `numeric`               | [Quantile plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_Qplot.md)               |
|                       |                         | (or Violin plot)                                                                              |
| `numeric`             | `numeric`               | [Scatter/line plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_scatter.md)         |
| `character`/`factor`  | `numeric`               | [Violin plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_violins.md)               |
| `numeric`             | `character` or `factor` | [Area chart](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_area.md)                   |
| `character`/ `factor` | (missing)               | [Bar plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_barplot.md)                  |
| (missing)             | `character`/`factor`    | [Bar plot](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_barplot.md)                  |
| `character`/`factor`  | `character`/`factor`    | [Heat map](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_heat.md)                     |

### Drawing dimensions of data

`draw()` is equipped to visualize data in up to five dimensions in one
plot. The main dimensions are, of course, the X and Y axes, controlled
by the `x` and `y` arguments. The other two dimensions are color
(`color`), point size (`pointSize`), and point shape (`pointStyle`).
Another approach is to draw multiple plots at the same time in a grid,
with each sub plot called a "facet." Details for all these options can
be found below.

## General Draw Arguments

`draw()` is built on top of R's base plotting system. This means that
all the standard arguments to base-R plots can be used to customize
`draw()` plots (see [`par()`](https://rdrr.io/r/graphics/par.html) for a
full list). You can also add stuff to `draw()` plots using base
functions like [`points()`](https://rdrr.io/r/graphics/points.html),
[`mtext()`](https://rdrr.io/r/graphics/mtext.html), or
[`abline()`](https://rdrr.io/r/graphics/abline.html). `draw()` also has
a number of unique features, including easily dividing data into
multiple plots ("facets").

### Plot Text

Every `draw()` plot can have a title, subtitle, X-axis label, and Y-axis
label.

The `draw()` function will automatically generate X and Y labels for
every plot, usually just using the expression you passed; for example,
if you say `draw(rnorm(100))`, the X label will be "rnorm(100)." This
can be overridden using the `xlabel` and/or `ylabel` arguments, which
can be provided a single string each—to suppress a label, provide an
empty string, like `ylabel = ""`.

Titles and subtitles are specified using the `title` and `subtitle`
arguments, respectively. (Alternatively, you can use the standard `main`
and `sub` arguments.) No title or subtitle is drawn by default.

You can use the base-R
[`mtext()`](https://rdrr.io/r/graphics/mtext.html) function to draw
additional text on plot axes.

### Axes control

The `draw()` function will select reasonable X and Y axes ranges
automatically. If you want to override the defaults, you can use
`xlimit` or `ylimit` to control the range of values shown on each axis.
Each of these must be passed a vector of two numbers, representing the
left and right X-axis extremes (`xlimit`) or the bottom and top Y-axis
extremes (`ylimit`). For example, to show data in the range \\\[10,
50\]\\ on the X axis, specify `xlimit = c(10, 50)`.

For `numeric` axes, you can also plot data on a logarithmic scale using
the `log` argument. This is set by providing a single `character` string
containing a lower-case `"x"`, `"y"`, or both (`"xy"`). Note that
`draw()` will throw an error if you try to plot negative values on a
logarithmic scale. Also note that some plots will not allow logarithmic
scaling on some axes, and will simply ignore attempts to do that.

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

## Color

Colors can be specified in all `draw()` plots using the `color`
argument, along with the `alpha` argument which controls the
transparency of colors. Note that color control can used for entirely
aesthetic purposes (picking a color scheme you want) *or* to represent
an additional dimension of data.

Colors can be specified several ways:

- As names (e.g., `"red"` or `"darkgreen"`).

- As [hex codes](https://en.wikipedia.org/wiki/Web_colors#hex-triplet)
  (e.g., `"#ff0000"` or `"#00ff00`).

- Using the [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) function.

- As natural number (1,2,3, etc.), indexing `humdrumR`'s flatly palette,
  based on the colors `'#18BC9C'`, `'#3498DB'`, `'#F39C12'`,
  `'#E74C3C'`, and `'#2C3E50'`.

The `alpha` argument must be a `numeric` value \\1 \geq alpha \geq 0\\,
where `alpha = 0` is totally transparent and `alpha = 1` is totally
opaque.

It is always possible to specify a single color value for a plot.
However, `draw()` can also (generally) accept more color values,
depending on the type of plot. For some plots, multiple colors are used
(aesthetically) by default; For other types of plots, its possible to
use color to represent an additional dimension of information. The
details of how each plot type interprets the `color` argument are
explained in their respective documentation pages (see links in table
above).

## Facets

## See also

Use
[`drawToFile()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawToFile.md)
to render these plots to files. To add to plots,
[drawMore](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMore.md).
To draw multiple plots,
[drawMultiple](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMultiple.md).

## Examples

``` r
x <- rnorm(1000, 30, 4)

draw(x)
#> new("draw_object", .Data = function (...) 
#> {
#>     list2env(list(...), envir = environment())
#>     dotpars <- list(...)[intersect(names(list(...)), names(par()))]
#>     par_draw[names(dotpars)] <- dotpars
#>     oldpar <- do.call("par", par_draw)
#>     on.exit({
#>         par(oldpar, no.readonly = TRUE)
#>     })
#>     if (!output$faceted) {
#>         plot.new()
#>         marginLines <- setMargins(margin, aspect)
#>         output$canvas()
#>     }
#>     else {
#>         marginLines <- setMargins(margin, aspect)
#>     }
#>     output$drawer()
#>     marginLab(marginLines, title, 3, 3, col = par("col.main"), 
#>         cex = par("cex.main"), font = 2)
#>     marginLab(marginLines, subtitle, 3, 2, font = 2)
#>     humaxes(output$axes, output$axisNames, axes, marginLines)
#>     if (length(legend)) {
#>         sides <- c(4, 3, 2)
#>         side_i <- 1
#>         if (!is.null(legend$color) && !is.null(output$col$legend)) {
#>             output$col$legend(side = sides[side_i], marginLines = marginLines, 
#>                 col.legend = legend$color)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointSize) && !is.null(output$cex$legend)) {
#>             output$cex$legend(side = sides[side_i], marginLines = marginLines, 
#>                 cex.legend = legend$pointSize)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointStyle) && !is.null(output$pch$legend)) {
#>             output$pch$legend(side = sides[side_i], marginLines = marginLines, 
#>                 pch.legend = legend$pointStyle)
#>         }
#>     }
#> }, add = expression(), layout = list(layout = 1L, layout_heights = 1, 
#>     layout_widths = 1), aspect = 1.33333333333333)
#> <bytecode: 0x566d40b18390>
#> <environment: 0x566d40b0ed20>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#> [1] 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1
#> 
#> attr(,"layout")$layout_widths
#> [1] 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"
draw( , x)
#> new("draw_object", .Data = function (...) 
#> {
#>     list2env(list(...), envir = environment())
#>     dotpars <- list(...)[intersect(names(list(...)), names(par()))]
#>     par_draw[names(dotpars)] <- dotpars
#>     oldpar <- do.call("par", par_draw)
#>     on.exit({
#>         par(oldpar, no.readonly = TRUE)
#>     })
#>     if (!output$faceted) {
#>         plot.new()
#>         marginLines <- setMargins(margin, aspect)
#>         output$canvas()
#>     }
#>     else {
#>         marginLines <- setMargins(margin, aspect)
#>     }
#>     output$drawer()
#>     marginLab(marginLines, title, 3, 3, col = par("col.main"), 
#>         cex = par("cex.main"), font = 2)
#>     marginLab(marginLines, subtitle, 3, 2, font = 2)
#>     humaxes(output$axes, output$axisNames, axes, marginLines)
#>     if (length(legend)) {
#>         sides <- c(4, 3, 2)
#>         side_i <- 1
#>         if (!is.null(legend$color) && !is.null(output$col$legend)) {
#>             output$col$legend(side = sides[side_i], marginLines = marginLines, 
#>                 col.legend = legend$color)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointSize) && !is.null(output$cex$legend)) {
#>             output$cex$legend(side = sides[side_i], marginLines = marginLines, 
#>                 cex.legend = legend$pointSize)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointStyle) && !is.null(output$pch$legend)) {
#>             output$pch$legend(side = sides[side_i], marginLines = marginLines, 
#>                 pch.legend = legend$pointStyle)
#>         }
#>     }
#> }, add = expression(), layout = list(layout = 1L, layout_heights = 1, 
#>     layout_widths = 1), aspect = 1.33333333333333)
#> <bytecode: 0x566d40b18390>
#> <environment: 0x566d406da210>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#> [1] 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1
#> 
#> attr(,"layout")$layout_widths
#> [1] 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

y <- x * 1.2 + rnorm(1000, 0, 4)

draw(x, y, title = "Linear regression", lm = TRUE)
#> new("draw_object", .Data = function (...) 
#> {
#>     list2env(list(...), envir = environment())
#>     dotpars <- list(...)[intersect(names(list(...)), names(par()))]
#>     par_draw[names(dotpars)] <- dotpars
#>     oldpar <- do.call("par", par_draw)
#>     on.exit({
#>         par(oldpar, no.readonly = TRUE)
#>     })
#>     if (!output$faceted) {
#>         plot.new()
#>         marginLines <- setMargins(margin, aspect)
#>         output$canvas()
#>     }
#>     else {
#>         marginLines <- setMargins(margin, aspect)
#>     }
#>     output$drawer()
#>     marginLab(marginLines, title, 3, 3, col = par("col.main"), 
#>         cex = par("cex.main"), font = 2)
#>     marginLab(marginLines, subtitle, 3, 2, font = 2)
#>     humaxes(output$axes, output$axisNames, axes, marginLines)
#>     if (length(legend)) {
#>         sides <- c(4, 3, 2)
#>         side_i <- 1
#>         if (!is.null(legend$color) && !is.null(output$col$legend)) {
#>             output$col$legend(side = sides[side_i], marginLines = marginLines, 
#>                 col.legend = legend$color)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointSize) && !is.null(output$cex$legend)) {
#>             output$cex$legend(side = sides[side_i], marginLines = marginLines, 
#>                 cex.legend = legend$pointSize)
#>             side_i <- side_i + 1
#>         }
#>         if (!is.null(legend$pointStyle) && !is.null(output$pch$legend)) {
#>             output$pch$legend(side = sides[side_i], marginLines = marginLines, 
#>                 pch.legend = legend$pointStyle)
#>         }
#>     }
#> }, add = expression(), layout = list(layout = 1L, layout_heights = 1, 
#>     layout_widths = 1), aspect = 1.33333333333333)
#> <bytecode: 0x566d40b18390>
#> <environment: 0x566d404a20f0>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#> [1] 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1
#> 
#> attr(,"layout")$layout_widths
#> [1] 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

```
