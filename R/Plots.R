

# plot class ----

# This class allows us to save the code to generate a plot
# into an object, which can be saved, and regenerated time again.
# Most importantly, it makes it composable.

setClass('plot', contains = 'function', slots = c(add = 'expression', layout = 'list'))


setMethod('show', 'plot',
          function(object) {
            
            on.exit(layout(1L))
            layout <- object@layout
            if (length(layout$layout) != 1L) {
              omi <- min(par('fin')) * .05
              oldpar <- par(omi = c(omi, omi, omi, omi))
              on.exit(par(oldpar), add = TRUE)
            }
            
            layout(layout$layout, 
                   widths = layout$layout_widths,
                   heights = layout$layout_heights)
            
            .drawSelf(object)
            
            
            invisible(NULL)
            
          })


.drawSelf <- function(object) {
  object@.Data()
  envir <- environment(object)
  for (expr in object@add) eval(expr, envir = envir)
}


plot_object <- function(plotfunc, layout = list(layout = cbind(1L), layout_widths = 1, layout_heights = 1)) new('plot', plotfunc, layout = layout)

#' @export
drawMore <- function(plot, ...) {
  exprs <- rlang::enexprs(...) |> as.expression()
  plot@add <- c(plot@add, exprs)
  plot
}

#' @export
drawNothing <- function() {
  
  plot_object(function() plot.new())
  
}


.drawSet <- function(..., sizes = NULL, binder, funcName, argName) {
  fs <- list(...)
  
  isfuncs <- sapply(fs, inherits, what = 'function')
  if (!all(isfuncs)) .stop("Plot arguments to {funcName}() must be zero-argument functions.") 
  
  if (is.null(sizes)) sizes <- rep(1, length(fs))
  checks(sizes, xpositive, argname = argName)
  
  # layout
  layouts <- lapply(fs, \(f) if (class(f) == 'plot') f@layout$layout else cbind(1L))
  newlayout <- Reduce(\(a, b) combineLayouts(a, b, binder = binder), layouts) 
  newfunc <- function()  lapply(fs, \(f) if (class(f) == 'plot') .drawSelf(f) else f())
  
  plot_object(newfunc, layout = list(layout = newlayout,
                                     layout_widths = sizes, layout_heights = 1))
  
}

#' @export
drawBeside <- function(..., widths = NULL) {
  .drawSet(..., sizes = widths, binder = 'cbind', 
           funcName = 'drawBeside', argName = 'widths')
}

#' @export
drawAbove <- function(..., heights = NULL) {
  .drawSet(..., sizes = heights, binder = 'rbind', 
           funcName = 'drawAbove', argName = 'heights')
}


combineLayouts <- function(layout1, layout2, binder = 'cbind') {
  layout2 <- layout2 + max(layout1)
  
  if (!hasdim(layout1)) layout1 <- cbind(layout1)
  if (!hasdim(layout2)) layout2 <- cbind(layout2)
  
  newdim <- pmax(dim(layout1), dim(layout2))
  
  layout1 <- array(layout1, newdim)
  layout2 <- array(layout2, newdim)
  
  do.call(binder, list(layout1 = layout1, layout2 = layout2))
  
}

# draw() ----


#' Visualize data
#' 
#' The `draw()` function is humdrumR's go-to plotting function,
#' which can make a variety of graphs depending on the type of data you give it.
#' For the most part, `draw()` is just an easy-to-use wrapper around
#' the base-R graphics functions---Anything that `draw()` does can be done
#' using normal base-R functions ([plot()], [barplot()], etc.).
#' However, `draw()` makes making good looking plots faster and easier.
#' 
#' @details
#' 
#' `draw()` is a generic function, which does different sorts of plots depending on the data you pass to its
#' `x` and `y` arguments.
#' The following table indicates the seven possible plot types, and which combinations of `x` and `y` variable classes
#' result in each type.
#' Separate sections below explaining the detailed features of each type of plot.
#' 
#' 
#' | `x`                                                      | `y`                                   | Plot type                                           |
#' |----------------------------------------------------------|---------------------------------------|-----------------------------------------------------|
#' | `numeric`                                                | (missing)                             | [Density Histogram/Contour][draw_density()]         |
#' | (missing)                                                | `numeric`                             | [Quantile plot][draw_Qplot()]                       |
#' |                                                          |                                       | (or Violin plot)                                    |
#' | `numeric`                                                | `numeric`                             | [Scatter/line plot][draw_scatter()]                 |                  
#' | `character`/`factor`                                     | `numeric`                             | [Violin plot][draw_violins]                         |
#' | `numeric`                                                | `character` or `factor`               | [Area chart][draw_area()]                           |
#' | `character`/ `factor`                                    | (missing)                             | [barplot][draw_barplot()]                           |
#' | (missing)                                                | `character`/`factor`                  | [barplot][draw_barplot()]                           |
#' | `character`/`factor`                                     | `character`/`factor`                  | [Heat map][draw_heat()]                             |
#' 
#' 
#' 
#' ### Drawing dimensions of data
#' 
#' `draw()` is equipped to visualize data in up to five dimensions in one plot.
#' The main dimensions are, of course, the X and Y axes, controlled by the `x` and `y` arguments.
#' The other two dimensions are color (`col`), point size (`cex`), and point shape (`pch`).
#' Another approach is to draw multiple plots at the same time in a grid, with each sub
#' plot called a "facet."
#' Details for all these options can be found below.
#' 
#' @section General Draw Arguments:
#' 
#' 
#' `draw()` is built on top of R's base plotting system.
#' This means that all the standard arguments to base-R plots can be used to customize `draw()` plots 
#' (see [par()] for a full list).
#' You can also add stuff to `draw()` plots using base functions like [points()], [mtext()], or [abline()].
#' `draw()` also has a number of unique features, including easily dividing data into multiple plots ("facets").
#' 
#'  
#' ### Plot Text
#' 
#' Every `draw()` plot can have a title, subtitle, X-axis label, and Y-axis label.
#' 
#' The `draw()` function will automatically generate X and Y labels for every plot,
#' usually just using the expression you passed; for example, if you say `draw(rnorm(100))`, the
#' X label will be "rnorm(100)."
#' This can be overridden using the `xlab` and/or `ylab` arguments, which can be provided a single string
#' each---to suppress a label, provide an empty string, like `ylab = ""`.
#' 
#' Titles and subtitles are specified using the `main` and `sub` arguments, respectively.
#' No title or subtitle is drawn by default.
#' 
#' You can use the base-R [mtext()] function to draw additional text on plot axes.
#' 
#' ### Axes control
#' 
#' The `draw()` function will select reasonable X and Y axes ranges automatically.
#' If you want to override the defaults, you can use `xlim` or `ylim` to control
#' the range of values shown on each axis.
#' Each of these must be passed a vector of two numbers, representing the left and right
#' X-axis extremes (`xlim`) and the bottom and top Y-axis extremes (`ylim`).
#' For example, to show data in the range \eqn{[10, 50]} on the X axis,
#' specify `xlim = c(10, 50)`.
#' 
#' For `numeric` axes, you can also plot data on a logarithmic scale
#' using the `log` argument.
#' This is set by providing a single `character` string containing a lower-case
#' `"x"`, `"y"`, or both (`"xy"`).
#' Note that `draw()` will throw an error if you try to plot negative values 
#' on a logarithmic scale.
#' Also note that some plots will not allow logarithmic scaling on some axes,
#' and will simply ignore attempts to do that.
#' 
#' ### Window control
#' 
#' Normally, when you use `draw()`, the plot is drawn to fill the available graphical device,
#' using base-R's normal algorithm.
#' However, the `aspect` argument can override this, controlling the aspect 
#' ratio of the plot.
#' The `aspect` argument must be a single numeric value between `0.5` and `5`.
#' If `aspect = 1` the plot is drawn as a square.
#' Try settings like `aspect = 4/3` or `aspect = 16/9`.
#' 
#' The `margin` argument controls the portion of the screen used for the plot margins,
#' with legal values ranging from `0.4` to `0.1`.
#' The default value is `0.2`---using other margins may result in less optimal placement
#' of plot text and legends.
#' 
#' @section Color:
#' 
#' 
#' Colors can be specified in all `draw()` plots using the `col` argument, along with the 
#' `alpha` argument which controls the transparency of colors.
#' Note that color control can used for entirely aesthetic purposes (picking 
#' a color scheme you want) *or* to represent an additional dimension of data.
#'
#' Colors can be specified several ways:
#' 
#' + As names (e.g., `"red"` or `"darkgreen"`).
#' + As [hex codes](https://en.wikipedia.org/wiki/Web_colors#hex-triplet) (e.g., `"#ff0000"` or `"#00ff00`).
#' + Using the [rgb()] function.
#' + As natural number (1,2,3, etc.), indexing `humdrumR`'s flatly palette,
#'   based on the colors `'#18BC9C'`, `'#3498DB'`, `'#F39C12'`, `'#E74C3C'`, and `'#2C3E50'`.
#'
#' The `alpha` argument must be a `numeric` value \eqn{1 \geq alpha \geq 0},
#' where `alpha = 0` is totally transparent and `alpha = 1` is totally opaque.
#'
#' It is always possible to specify a single color value for a plot.
#' However, `draw()` can also (generally) accept more color values, depending on the type of plot.
#' For some plots, multiple colors are used (aesthetically) by default;
#' For other types of plots, its possible to use color to represent an additional dimension of information.
#' In the "Specific Plot Types" subsections below, the details of how each plot type interprets
#' the `col` argument are explained.
#' 
#'
#'
#' @section Facets:
#' 
#'
#' @param xlab,ylab ***What X and/or Y axis labels should be used?***
#' 
#' Must be single `character` strings. 
#' 
#' See "Plot Text" section below.
#' 
#' @param xlim,ylim ***What range of X/Y values should drawn on the plot?***
#' 
#' By default, X and Y limits are automatically selected.
#' 
#' Must be a `numeric` vector of length two.
#' 
#' The first number of each `xlim`/`ylim` vector specifies the left/bottom
#' edge of the X/Y axis. The second number specifies the right/top edge.
#' 
#' 
#' @param legend ***Should legends be drawn?***
#' 
#' Defaults to `TRUE`.
#' 
#' @param `aspect` 
#' 
#' Defaults to `NULL`.
#' 
#' Must be a single numeric value \eqn{5 \geq aspect \geq 0.2}, or `NULL`.
#' 
#' This controls the aspect ratio of the plot:
#' `1` (square), `4/3`, `16/9`, etc. 
#' If `aspect` is `NULL` , R automatically uses the current aspect of 
#' your current plotting device.
#' 
#' @param margin ***How big should plot margins be?***
#' 
#' Defaults to `0.2`.
#' 
#' Must be as single numeric value \eqn{0.4 \geq margin \geq 0.1}.
#' 
#' This controls the proportion of the plotting area used for margins.
#' A value of `0.2` means that 20% of the plotting area is used for the margins.
#' 
#'
#' @param heat ***Should a heatmap be drawn?***
#' 
#' Defaults to `FALSE`, unless the input has two or more dimensions
#' *and* at least 80 conditions.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' 
#' @param ... ***Additional parameters to pass to par().***
#' 
#' Any base-R graphing parameters that can be set using the [par()] function
#' may be passed to `draw()`. 
#' These parameters are set using `par()` (overriding humdrumR's defaults), but only for the duration of the
#' `draw()` call---i.e., the global `par()` settings are not changed.
#' 
#' 
#' @export
draw <- function(x, y, ...) {
  UseMethod('draw')
}

#' @export
draw.humdrumR <- function(x, ...) {
  call <- match.call()
  
  groups <- getGroupingFields(x)
  x <- ungroup(x)
  
  if (!pmatch('facets', names(call[-1]), nomatch = 0) &&
      length(groups))  {
    
     call[['facets']] <- as.list(groups)
  }
  
  
  call[['x']] <- NULL
  call[[1]] <- quote(draw.default)
  
  if (!any(.names(call[-1]) %in% c('x', 'y', ''))) {
    fields <- rlang::syms(selectedFields(x))
    for (field in fields) call[[length(call) + 1L]] <- field
  }
  rlang::eval_tidy(rlang::expr(with(x, !!call)))
}

#' @export
draw.default <- function(x, y, facets = list(), ..., 
                         xlab = NULL, ylab = NULL, 
                         axes = 1:4, legend = TRUE, aspect = NULL, margin = .2,
                         main = '', sub = '', col = 1, cex = NULL, pch = 16) {
  
  checks(aspect, xnull | (xlen1 & xnumeric & xmin(.2) & xmax(5)))
  checks(margin, xlen1 & xnumeric & xmin(.1) & xmax(.4))
  checks(legend, xTF | (xcharacter & xminlength(1) & xmaxlength(2)))
  checks(axes, xwholenum & xmaxlength(4L) & xmax(4) & xmin(1))
  checks(xlab, xnull | (xlen1 & xatomic))
  checks(ylab, xnull | (xlen1 & xatomic))
  checks(main, xatomic & xlen1)
  checks(sub, xatomic & xlen1)

  # this sets default par(...) values for for draw(), 
  # but overrides them with args from list(...)
  par_draw <- list(family = 'Helvetica',   col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2, pty = 'm')
  dotpars <- list(...)[intersect(names(list(...)), names(par()))]
  par_draw[names(dotpars)] <- dotpars
  oldpalette <- palette(flatly)
  on.exit(palette(oldpalette))

  # xlab and ylab
  xexpr <- deparse1(substitute(x)) 
  yexpr <- deparse1(substitute(y)) 
  if (xexpr == '') xexpr <- 'x'
  if (yexpr == '') yexpr <- 'y'
  
  # change missing to NULL
  x <- if (!missing(x)) token2atomic(x)
  y <- if (!missing(y)) token2atomic(y)
  
  if (rlang::is_formula(x)) {
    formula <- xy_formula(x)
    x <- formula$x
    y <- formula$y
    xlab <- xlab %||% formula$xlab
    ylab <- ylab %||% formula$ylab
  } 

  if (length(facets)) {
    facets <- prep_facets(x, y, facets)
    
    args <- list(x = x, y = y, col = col, cex = cex, pch = pch)
    facets <- by(as.data.frame(args[lengths(args) == length(facets[[1]])]), 
                 facets, simplify = FALSE, 
                 FUN = \(df) {
                   c(as.list(df), args[lengths(args) != length(facets[[1]])])
                 })
    
    output <- draw_facets(args, facets, ...,
                          xexpr = xexpr, yexpr = yexpr,
                          xlab = xlab, ylab = ylab, 
                          axes = axes, legend = legend,
                          aspect = aspect, margin = margin)
  } else {
    output <- .draw(x, y, ..., col = col, cex = cex, pch = pch, aspect = aspect)
    output$layout <- output$layout %||% 1L
    output$faceted <- output$faceted %||% FALSE
    output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
    output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  }
  
  plot_object(function(...) {
    list2env(list(...), envir = environment())
    
    dotpars <- list(...)[intersect(names(list(...)), names(par()))]
    par_draw[names(dotpars)] <- dotpars
    
    oldpar <- do.call('par', par_draw)
    on.exit({par(oldpar, no.readonly = TRUE)})
    
    marginLines <- setMargins(margin, aspect)
    if (!output$faceted) {
      plot.new()
      output$canvas()
    }
    output$drawer()
    
    # title and subtitle
    marginLab(marginLines, stringr::str_to_title(main), 3, 3,
              col = par('col.main'), cex = par('cex.main'), font = 2)
    marginLab(marginLines, stringr::str_to_title(sub), 3, 2,
              font = 2)

    # axes labels
    humaxes(output$axes, output$axisNames, axes, marginLines)

    # legends
    if (is.character(legend) || legend) {

      sides <- c(4, 2, 3)
      side_i <- 1
      if (is.logical(legend)) legend <- ''
      legend <- rep(legend, length.out = 3)

      if (!is.null(output$col$legend)) {
        output$col$legend(side = sides[side_i], marginLines = marginLines, col.legend = legend[2])
        side_i <- side_i + 1
      }
      if (!is.null(output$cex$legend)) {
        output$cex$legend(side = sides[side_i], marginLines = marginLines, cex.legend = legend[1])
        side_i <- side_i + 1
      }
      if (!is.null(output$pch$legend)) output$pch$legend(side = sides[side_i], marginLines = marginLines)
    }
    },
    layout = list(layout = output$layout, layout_heights = 1, layout_widths = 1))
}
  

## specific draw_functions ----


### draw_scatter ----

#' Draw a scatter plot
#' 
#" This method draws the relationship between a pair of (continuous) numeric 
#' variables, `x` and `y`, as a scatter plot.
#' If `line = TRUE`, points are not drawn at all, and instead a line connecting the 
#' points is draw from leftmost point to rightmost.
#' 
#' @details
#' 
#' The input vectors `x` and `y` must be of equal length, unless one of the pair is a single scalar,
#' in which case that scalar is recycled to match the length of the other input.
#' (Thus, providing a scalar `x` or `y` causes the other variable to be drawn on a straight line.)
#' Up to three additional dimensions can be visually added to the plot using `col` (color),
#' `cex` (point size), and `pch` (point style) arguments.
#' 
#' @section Color:
#' 
#' If you pass a single color value to `col`, the whole graph is drawn that color.
#' However, if you pass `col` a vector of values which is the exact same length as 
#' the input vectors `x`/`y`, and `line = FALSE`, the unique values of this vector will be used to color
#' the points of the scatter plot.
#' (A color legend will be drawn automatically.)
#' The colors for each group will be chosen automatically, unless the entire `col` vector is
#' valid color values. (Use `alpha` independently to change the transparency.)
#' If the grouping `col` vector is numeric and there are more than ten unique values,
#' a continuum of colors is created to represent that numeric space.
#' 
#' @section Point size:
#' 
#' By default, `draw()` chooses an appropriate size to draw data points based on the 
#' density of the X and Y points in the window---the more data on the screen, smaller the points are drawn.
#' You can override this by passing a single numeric value to `cex`; values between about
#' `.2` and `1.5` are pretty reasonable, typically.
#' However, if you pass `cex` vector of positive numeric values which is the same length
#' as the input vectors `x`/`y`, the point sizes are scaled so that the relative *area* of drawn points
#' matches the relative magnitude of numbers in the `cex` vector---A point-size legend is also drawn.
#' Thus, if you draw two points with `cex = c(3, 6)`, the second point will be drawn twice
#' the size (twice the area) of the first.
#' If the range of values is too great, it is not feasible to represent them using points,
#' because the points would either get too small to see or too big (covering the whole plot).
#' Thus, if the largest `cex` value is more than 100 times greater than the smallest,
#' the scaling will be changed to accommodate this.
#' When this happens, a message will be printed, explaining how the relative area of drawn
#' points relates to the relative magnitude of `cex` values.
#' For example, you might see a message like: "When comparing the points in this plot, 
#' a doubling of area corresponds to multiplying the value by three."
#'
#' @section Point shape:
#' 
#' By default, each data point is represented by a solid circle.
#' This can be overridden by passing a `pch` argument.
#' There are sixteen possible shapes, which are specified by the natural
#' numbers from 1 to 16---try calling `plot(1:16, pch = 1:16)` to see them all.
#' If a single value is passed to `pch`, all points are drawn with the corresponding shape.
#' However, if you pass `pch` a vector which is the same length as the input
#' vectors `x`/`y`, the data is grouped relative to the unique values of this vector,
#' and a shape is used to represent each group.
#' (A point-shape legend is automatically drawn.)
#' If the `pch` vector is numeric, with more than four unique values, the 
#' numeric range is divided into four groups automatically.
#' If the `pch` vector is discrete (`character`, `logical`, or `factor`),
#' each unique value is mapped to a point-shape;
#' This only works up to sixteen unique values---if there are more than 16
#' unique values in a discrete `pch` vector, an error will occur.
#'
#' @param cex ***What size of points should be drawn?***
#'
#' Must be either a single positive number, or a vector
#' of numbers or color `character` values of the same length as `x`/`y`.
#' 
#' @param pch ***What shape should points be drawn?***
#' 
#' Must be a single whole number from 1 to 16, or a vector of 
#' discrete values of the same length as `x`/`y`.
#' 
#' @param line ***Should a line be drawn through the `x`/`y` coordinates, instead of points?***
#' 
#' Defaults to `FALSE`. 
#'
#' Must be a singleton `logical` value: an on/off switch.
#'   
#' @param normalReference ***Should a Gaussian reference distribution be drawn?***
#' 
#' Defaults to `FALSE`. 
#'
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, a bivariate normal (Gaussian) distribution is drawn under the scatter 
#' plot, using the means and variances of, and the covariance between, the input vectors `x` and `y`.
#' To visualize this two dimensional distribution, a random sample of points from this bivariate
#' distribution is drawn in large black, but mostly transparent, points.
#' This gives a sense of how close to jointly-normally distributed `x` and `y` are.
#'
#' @param quantiles ***Should distribution quantiles of `x` and `y` be marked?***
#'
#' Defaults to `c()`, so no quantiles are drawn.
#' 
#' Must be a vector of numbers between 0 and 1 (inclusive), or an empty vector (the default).
#' 
#' If any quantiles are specified, each quantile is drawn on the plot and labeled appropriately.
#' Quantiles are computed separately for input vectors `x` and `y` and drawn using vertical and horizontal
#' lines respectively.
#' For example, `quantiles = .5` will draw a lines which converge at the medians of `x` and `y`;
#' `quantiles = c(.25, .5, .75)` will draw vertical and horizontal lines marking the four 
#' quartiles of `x` and `y` (creating a grid with 16 cells).
#' 
#' @param mean ***Should the mean of input vectors `x` and `y` be marked on the plot?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param lm ***Should the regression line, predicting `y` from `x`, be drawn?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' The simple regression line is estimated using [lm(y ~ x)][lm()].
#' The 95% confidence limits---estimated using [predict.lm()]---are
#' also drawn (as dashed lines).
#' The regression coefficients are printed in a legend at the top left corner of the plot.
#'
#' @param log ***Should X and/or Y axes be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale on both axes).
#' 
#' Must be a single `character` string; options are `"x"` (X axis on log scale), 
#' `"y"` (Y axis on log scale), and `"xy"` (both axes on log scale).
#' 
#'

#'   
#' @usage draw(x # numeric,
#'      y # numeric,  
#'      col = NA)
#' @inheritParams draw
draw_scatter <- function(x, y, log = '', jitter = '', line = FALSE,
                         normalReference = FALSE, mean = FALSE, quantiles = c(), lm = FALSE,
                         xlim = NULL, ylim = NULL, 
                         col = 1, alpha = .5, cex = NULL, pch = NULL, marginLines, ...) {
  checks(jitter, xcharacter & xlen1 & xlegal(c('', 'x', 'y', 'xy', 'yx')), seealso = '?draw_scatter')
  checks(lm, xTF, seealso = '?draw_scatter')
  checks(line, xTF, seealso = '?draw_scatter')
  checks(mean, xTF, seealso = '?draw_scatter')
  checks(normalReference, xTF, seealso = '?draw_scatter')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_scatter')
  
  if (length(x) != 1L && length(x) != length(y) && length(y) != 1L) {
    .stop("You can't draw two numeric vectors if they are different lengths.",
          "In your call, length(x) = {length(x)} and length(y) = {length(y)}.")
  }
  
  match_size(x = x, y = y, toEnv = TRUE)
  
  output <- canvas(x = x, xlim = xlim, 
                   y = y, ylim = ylim,
                   log = log)
  
  output$col <- prep_col(col, y, alpha = alpha, log = log, ...)
  output$cex <- prep_cex(x, y, cex = cex, col = output$col$col, log = log, ...)
  output$pch <- prep_pch(x, y, pch = pch, log = log, col = output$col$col)
  
  if (grepl('x', jitter)) x <- smartjitter(x)
  if (grepl('y', jitter)) y <- smartjitter(y)
  
  output$drawer <- function() {
    if (normalReference) draw_mvnorm(x, y)
    
    if (line) {
      y <- y[order(x)]
      x <- x[order(x)]
      points(x, y, col = output$col$col[1], type = 'l', ...)
      
    } else {
      points(x, y, col = output$col$col, cex = output$cex$cex, pch = output$pch$pch, ...)
    }
    
    
    # extra stuff
    draw_quantiles(1, x, quantiles)
    draw_quantiles(2, y, quantiles)
    if (mean)  draw_mean(mean(x), mean(y)) 
    
    if (lm) {
      fit <- stats::lm(y ~ x)
      
      xseq <-  seq(output$window$xlim[[1]][1], 
                   output$window$xlim[[1]][2], length.out = 300L)
      conf <- predict(fit,  newdata = data.frame(x = xseq), interval = 'confidence', ...)
      
      lmcol <- 'grey30'
      points(xseq, conf[ , 1], type = 'l', lwd = .5, col = lmcol)
      points(xseq, conf[ , 2], type = 'l', lwd = .3, lty = 'longdash', col = lmcol)
      points(xseq, conf[ , 3], type = 'l', lwd = .3, lty = 'longdash', col = lmcol)
      
      legend('topleft', bty = 'n', lwd = .5, col = lmcol, text.col = 'black', cex = .8,
             legend = bquote(list(a == .(format(coef(fit)[1], big.mark = ',', digits = 3)),
                                  b == .(format(coef(fit)[2], big.mark = ',', digits = 3)))))
    }
  }
  output
}


### draw_density  ----


#' Draw density plot (histogram or contour)
#' 
#' This method draws the density distribution of a numeric (continuous) `x` argument,
#' either as a binned histogram (binned) or a smoothed contour plot.
#' To choose which approach, use the `smooth` argument: `smooth = FALSE` (default) for histogram,
#' and `smooth = TRUE` for contour.
#' An additional dimension can be added to the plot using the `col` argument,
#' drawing multiple density graphs in different colors.
#' 
#' @details
#'
#' The `draw_density()` method relies on the algorithms used by base-R's [hist()] and [density()] functions
#' for estimating histogram bins and density contours respectively.
#' We can pass arguments directly through to these functions:
#' For example, the `breaks` argument can be passed through to [hist()],
#' or the `bw` and `kernel` arguments to [density()].
#' 
#' ### Density vs Mass
#' 
#' Whether smoothed or binned, the Y axis represents the probability **density**.
#' The height of histogram bars does *not* correspond exactly to the 
#' the probability **mass** in each bin, because that actually depends on the width of the bins;
#' For narrow bins (or contious contours), density can even be greater than 1.
#' If bin sizes are all equal, then the *relative* height of the density bars *does*
#' map exactly to the relative probability mass of each bin.
#' If bins are not equal width---which can only happen if you manually specify
#' unequal bins using the `breaks` argument---the heights of bars *don't* map to probability mass.
#' However, using the density assures that the relative **area** of each bin *does* match the probability mass 
#' associated with that bin, even if the bins are of unequal width.
#' 
#' @section Color:
#' 
#' A second dimension can be added to the density plot using the `col` (color) argument.
#' If you pass a single color, the whole graph is drawn that color.
#' However, if you pass `col` a vector of values which is the exact same length as 
#' the input vector `x`, the unique values of this vector will be used to group 
#' the `x` data, and a separate density plot will be drawn for each group, with its own color.
#' (A color legend will be drawn automatically.)
#' 
#' The colors for each group will be chosen automatically, unless the entire `col` vector is
#' valid color values. (Use `alpha` independently to change the transparency.)
#' If the grouping `col` vector is numeric and there are more than five unique values,
#' the numbers are automatically divided into (at most) five bins.
#' Use the `conditional` argument to control how the densities of each group 
#' are scaled relative to each other.
#' 
#' @inheritSection draw General Draw Arguments
#' 
#' @param smooth ***Should a smoothed density curve be estimated for each group?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, a smooth density contour is estimated for `x`, using [density()].
#' If `FALSE`, `x` values are binned using [hist()].
#' 
#' @param showPoints ***Should individual points from `x` be shown above the density plot?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, individual data points from the `x` input vector
#' are plotted in a "cloud" above the density plot.
#' The X-position of each point is matched to its actual value;
#' The Y-position of each point is randomly (uniformly) selected in a range at the top of the plot window.
#' This spaces out points that are close together, so it is easier to see how dense they are.
#' 
#' @param showCounts ***Should the counts of values in each bin/count be printed above the bar/contour?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' Note that, if the data is grouped into multiple draws by `col` (see above), there is no guarantee
#' the counts won't be drawn on top of each other.
#' 
#' @param normalReference ***Should a Gaussian reference distribution be drawn?***
#' 
#' Defaults to `FALSE`. 
#'
#' Must be a singleton `logical` value: an on/off switch.
#'
#' If `TRUE`, a normal (Gaussian) distribution is drawn as a dashed
#' black line. The mean and standard deviation of this distribution is taken from the input vector `x`,
#' within each group.
#' This gives a sense of how close to normally distributed `x` is.
#'
#' @param conditional ***Should probability density be calculated separately within each color group?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If the `col` argument is used to draw multiply density graphs,
#' this controls whether the density
#' of each color matches its global share in the distribution of input variable `x`, or is rescaled in each
#' group to sum/integrate to 1. I.e., should probabilities be conditioned on the grouping factor?
#' Setting `conditional = TRUE` is useful if you want to see the details of how each group is distributed.
#' `conditional = FALSE` (the default) is useful when you want to see the actual proportion of
#' data in each group (i.e., if the groups are different sizes).
#'
#' @param quantiles ***Should distribution quantiles of `x` be marked?***
#'
#' Defaults to `c()`, so no quantiles are drawn.
#' 
#' Must be a vector of numbers between 0 and 1 (inclusive), or an empty vector (the default).
#'
#' If any quantiles are specified, each quantile is drawn as a vertical line on the plot, labeled appropriately.
#' For example, `quantiles = .5` will draw a line at the median of input vector `x`;
#' `quantiles = c(.25, .5, .75)` will draw lines marking the four quartiles of `x`.
#' (Depends on `global_stats` argument.)
#' 
#' @param mean ***Should the mean of input vector `x` be marked at the center of the plot?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param global_stats ***Should quantiles and/or means be drawn within each color group?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If the `col` argument is used to draw multiply density graphs,
#' and `global_stats = FALSE`, quantiles and/or means are drawn separately for each group. 
#' This can get very messy very quickly!
#'
#' @param log ***Should X axis be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale).
#' 
#' Must be a single `character` string, either `""` (linear scale)
#' of `"x"` (draw X on a logarithmic scale). 
#' 
#'
#' @usage draw(x # numeric,  
#'      col = NA)
#' @inheritParams draw
draw_density <- function(x, y, log = '', 
                           breaks = 'Sturges', bw = 'SJ', normalReference = FALSE, 
                           smooth = FALSE, conditional = FALSE, showCounts = FALSE, showPoints = FALSE,
                           mean = FALSE, quantiles = c(), global_stats = FALSE,
                           xlim = NULL, ylim = NULL,
                           col = 3, alpha = .4, cex = .7, pch = NULL, ...) {
  # pch is used only to stop it being passed to hist_coor, which causes a warning
  checks(conditional, xTF, seealso = '?draw_density')
  checks(global_stats, xTF, seealso = '?draw_density')
  checks(mean, xTF, seealso = '?draw_density')
  checks(normalReference, xTF, seealso = '?draw_density')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_density')
  checks(smooth, xTF, seealso = '?draw_density')
  checks(showCounts, xTF, seealso = '?draw_density')
  checks(showPoints, xTF, seealso = '?draw_density')
  
  cols <- prep_col(col, x, alpha = alpha, log = log, ncontinuous = 5, ...)
  
  breaks <- if (!smooth && length(breaks) == 1L && pmatch(breaks, 'quantiles', 0)) {
    quantile(x, c(0, if (length(quantiles)) sort(unique(quantiles)) else c(.25, .5, .75), 1))
  } else {
    hist.default(x, breaks = breaks, plot = FALSE)$breaks
  }
  names(breaks) <- format(breaks, digits = 3)
  
  # This does the actual density stuff:
  col <- rep(cols$col, length.out = length(x)) # col may be a grouping factor
  coordinates <- multihist_coor(x, col, conditional = conditional, bw = bw,
                                smooth = smooth, breaks = breaks, ...)
  
  allDens <- unlist(lapply(coordinates, '[[', 'Density'))
  ylim <- ylim %||% c(0, 2^(ceiling(log( max(allDens), 2)))) # 1, .5, .25, .125, etc.
  
  output <- canvas(x = x, xlim = xlim %||% range(breaks), 
                   y =  allDens[allDens > 0], ylim = ylim, 
                   log = gsub('y', '', log))
  
  
  # prepare ticks
  ## x
  output$breaks <- x.ticks <- breaks
  output$draw_type <- 'histogram'
  while(length(x.ticks) > 20L) {
    x.ticks <- x.ticks[seq(1, length(x.ticks), by = 2)]
  }
  output$axes[side == 1, ticks := x.ticks]
  
  ## y
  output$axes[side == 2, ticks := setNames(ticks[[1]], format(ticks[[1]]))]
  output$axisNames[[2]] <- 'Probability density'
  
  output$col <- cols
  
  output$drawer <- function() {
    
    # actual plot of polygons
    ymin <- min(output$window$ylim[[1]])
    Map(\(coor, color) {
      coor[ , {
        polygon(c(X, rev(X)), c(Density, rep(ymin, length(Density))), col = color, border = NA, xpd = NA)
        
        points(type = 'l', X, Density, col = color, lwd = 2)
        
      }]
      
      # lines and counts
      if (!smooth) {
        #horixontal bars
        coor[!duplicated(X), { 
          graphics::segments(x0 = X, x1 = X, y0 = ymin, y1 = Density, col = color)
        }]}
      
      if (showCounts) {
        if (!smooth) {
          coor[!duplicated(X) & Counts > 0, draw_counts(Mids, Density, Counts, color, min(Delta))]
        } else {
          coor[ , draw_counts(X[which.max(Density)], max(Density), sum(col == color), color, diff(range(X)))]
        }
      }
    }, coordinates, names(coordinates))
    
    # extra stuff
    
    ## dots
    if (showPoints) draw_points(x, col, allDens, output$window$ylim)
    
    if (global_stats || length(coordinates) == 1L) {
      means <- mean(x)
      draw_quantiles(1, x, quantiles)
    } else {
      means <- if (global_stats) mean(x) else tapply(x, col, mean)
      lapply(unique(col), \(color) {
        draw_quantiles(1, x[col == color], quantiles, col = color)
      })
    }
    if (mean) draw_mean(means, grconvertY(0.02, 'npc', 'user'), col = output$col$col)
    
    if (normalReference) {
      xpoints <- seq(output$window$xlim[[1]][1], output$window$xlim[[1]][2], length.out = 100)
      points(xpoints, dnorm(xpoints, mean(x), sd(x)), type = 'l',
             lwd = .5, lty = 'dashed')
    }
  } 
  
  output
}


### draw_Qplot ----


#' Draw quantile plot 
#' 
#' This method draws the distribution of a numeric (continuous) `y` argument
#' by drawing the values, sorted from lowest to highest,
#' across the screen from left to right.
#' Position on the X axis corresponds to the empirical quantiles 
#' of the data; for example, the median point (50% quantile) is exactly in the middle of the 
#' X axis.
#' As an alternative, a single [violin][violin plot()] diagram can be drawn by setting `violin = TRUE`.
#' 
#' @details
#' 
#' Up to three additional dimensions can be visually added to the plot using `col` (color),
#' `cex` (point size), and `pch` (point style) arguments.
#' 
#' @section Color:
#' 
#' If you pass a single color value to `col`, the whole graph is drawn that color.
#' However, if you pass `col` a vector of values which is the exact same length as 
#' the input vector `y`, the unique values of this vector will be used to color
#' the points of the quantile plot.
#' (A color legend will be drawn automatically.)
#' The colors for each group will be chosen automatically, unless the entire `col` vector is
#' valid color values. (Use `alpha` independently to change the transparency.)
#' If the grouping `col` vector is numeric and there are more than ten unique values,
#' a continuum of colors is created to represent that numeric space.
#' 
#' @section Point size:
#' 
#' By default, `draw()` chooses an appropriate size to draw data points based on the 
#' size of the input vector `y` and the window size---the more data on the screen 
#' the smaller the points are drawn.
#' You can override this by passing a single numeric value to `cex`; values between about
#' `.2` and `1.5` are pretty reasonable, typically.
#' However, if you pass `cex` vector of positive numeric values which is the same length
#' as the input vector `y`, the point sizes are scaled so that the relative *area* of drawn points
#' matches the relative magnitude of numbers in the `cex` vector.
#' (A point-size legend will be drawn automatically.)
#' Thus, if you draw two points with `cex = c(3, 6)`, the second point will be drawn twice
#' the size (twice the area) of the first.
#' 
#' If the range of values is too great, it is not feasible to represent them using points,
#' because the points would either get too small to see or too big (covering the whole plot).
#' Thus, if the largest `cex` value is more than 100 times greater than the smallest,
#' the scaling will be changed to accommodate this.
#' When this happens, a message will be printed, explaining how the relative area of drawn
#' points relates to the relative magnitude of `cex` values.
#' For example, you might see a message like: "When comparing the points in this plot, 
#' a doubling of area corresponds to multiplying the value by three."
#' 
#' @section Point shape:
#' 
#' By default, each data point is represented by a solid circle;
#' This can be overridden by passing a `pch` argument.
#' There are sixteen possible shapes, which are specified by the natural
#' numbers from 1 to 16---try calling `plot(1:16, pch = 1:16)` to see them all.
#' 
#' If a single value is passed to `pch`, all points are drawn with the corresponding shape.
#' However, if you pass `pch` a vector which is the same length as the input
#' vector `y`, the data is grouped relative to the unique values of this vector,
#' and a shape is used to represent each group.
#' (A point-shape legend will be drawn automatically.)
#' 
#' If the `pch` vector is `numeric`, and with more than four unique values, the 
#' numeric range is divided into four groups automatically.
#' If the `pch` vector is discrete (`character`, `logical`, or `factor`),
#' each unique value is mapped to a point-shape;
#' This only works up to sixteen unique values---if there are more than 16
#' unique values in a discrete `pch` vector, an error will occur.
#'
#'   
#' @inheritSection draw General Draw Arguments
#' 
#' @param normalReference ***Should a Gaussian reference distribution be drawn?***
#' 
#' Defaults to `FALSE`. 
#'
#' Must be a singleton `logical` value: an on/off switch.
#'
#' If `TRUE`, a normal (Gaussian) distribution is drawn as a dashed
#' black line. The mean and standard deviation of this distribution is taken from the input vector `y`,
#' within each group.
#' This gives a sense of how close to normally distributed `y` is.
#'
#' @param quantiles ***Should distribution quantiles of `x` be marked?***
#'
#' Defaults to `c(.25, .5, .75)`, so the quartiles are drawn.
#' 
#' Must be a vector of numbers between 0 and 1 (inclusive), or an empty vector (the default).
#'
#' If any quantiles are specified, each quantile is drawn as a horizontal line on the plot, labeled appropriately.
#' For example, `quantiles = .5` will draw a line at the median of input vector `y`;
#' `quantiles = c(.25, .5, .75)` will draw lines marking the four quartiles of `y`.
#' 
#' @param mean ***Should the mean of input vector `y` be marked at the center of the plot?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' @param log ***Should Y axis be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale).
#' 
#' Must be a single `character` string, either `""` (linear scale)
#' of `""` (draw Y on a logarithmic scale). 
#' 
#' @param violin ***Should a [violin plot][violin_plot()] be drawn instead?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'   
#' @usage draw( , y # numeric,  
#'      col = NA)
#' @inheritParams draw
#' @inheritParams draw_scatter
draw_Qplot <- function(x, y, log = '', 
                       violin = FALSE, normalReference = FALSE, 
                       mean = FALSE, quantiles = c(.25, .5, .75),
                       xlim = NULL, ylim = NULL, 
                       col = 1, alpha = .8, cex = NULL, pch = NULL, 
                       ...) {
  
  checks(mean, xTF, seealso = '?draw_Qplot')
  checks(normalReference, xTF, seealso = '?draw_Qplot')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_Qplot')
  checks(violin, xTF, seealso = '?draw_Qplot')
  
  if (violin) return(draw_violins(integer(length(y)), y,  mean = mean,
                                  xlim = xlim, ylim = ylim, alpha = alpha, 
                                  normalReference = normalReference, 
                                  ..., col = col, quantiles = quantiles))
  
  
  output <- canvas(x = if (violin) c(.5, 1.5) else c(0, 1), 
                   xlim = xlim, 
                   y = y, ylim = ylim , 
                   log = gsub('x', '', log))
  output$col <- prep_col(col, y, ..., alpha = alpha, pch = pch, log = log)
  output$cex <- prep_cex(x, y, cex = cex, col = output$col$col, log = log, ...)
  output$pch <- prep_pch(x, y, pch = pch, log = log, col = output$col$col)
  output$axisNames[[1]] <- 'Quantile'
  
  output$drawer <- function() {
    if (length(output$col$col) == length(y)) output$col$col <- output$col$col[order(y)]
    if (length(output$cex$cex) == length(y)) output$cex$cex <- output$cex$cex[order(y)]
    if (length(output$pch$pch) == length(y)) output$pch$pch <- output$pch$pch[order(y)]
    
    y <- sort(y)
    x <- seq(0, 1, length.out = length(y))
    points(x = x, y = y, col = output$col$col, cex = output$cex$cex, 
           pch = output$pch$pch, ...)
    
    # extra stuff
    draw_quantiles(2, y, quantiles = quantiles)
    if (mean)  draw_mean(0.5, mean(y))
    
    if (normalReference) {
      points(x, qnorm(x, mean(y), sd(y)), type = 'l', col = 'black',
             lwd = .5, lty = 'dashed', xpd = TRUE)
      
      legend('topleft', bty = 'n', lty = 'dashed', lwd = .5, 
             col = 'black', text.col = 'black', cex = .8, 
             legend = quote(N(mu[y], sigma[y])) )
    }
    
    
  }
  
  output
}


### draw_barplot ----


#' Draw bar plot
#' 
#' This method draws [tabulated data][count()] as a bar plot.
#' If the data is passed to the first (`x`) argument, the bar plot is oriented vertically;
#' If the data is instead passed to the second (`y`) argument----with `x` missing---,
#' the bar plot is oriented horizontally, from left to right.
#' The input table can have one or two dimensions; if more dimensions are provided
#' the third and fourth dimension are split across draw [facets][draw()#facets].
#' 
#' @details
#' 
#' The `draw_barplot()` method will accept tabular data created 
#' `humdrumR` functions [count()] and [pdist()], or equivalent base-R functions [table()]/[proportions()].
#' If a single atomic vector of discrete values is passed to either `x` or `y`, the discrete
#' values are automatically [counted][count()], and the resulting table is passed to `draw_barplot()`.
#' Thus, if `mydiscrete` is a vector of discrete values (like `character`), 
#' calling `draw(mydiscrete)` is the same as calling `draw(count(mydiscrete))`.
#'
#' For count data (natural numbers), Y-axis labels for counts and proportion of total are shown;
#' For proportion data (real numbers between 0 and 1) only a proportion key is shown;
#' If the table includes negative numbers, the Y axis is simply labeled "value."
#'
#' ### Dimensions
#' 
#' A single-dimensional table (representing one variable) is simply drawn as a set of bars.
#' However, if the input table is two dimensional, a double bar plot is drawn, with bars
#' representing every combination of levels across the two dimensions;
#' Bars representing values of the first dimension are drawn in groups representing 
#' each level of second dimension.
#' However, if the total number of bars to draw is greater than 80, 
#' [draw()] will dispatch [draw_heatmap()] instead.
#' This behavior can be overridden using the `heat` argument.
#'
#' ### Barplot types
#' 
#' By default, bars are drawn side by side, so their relative height is easiest to compare.
#' However, is `beside = FALSE`, a "stacked" bar plot will be drawn, with bars stacked on top of each other.
#' If `beside = NULL`, side-by-side *and* stacked plots are drawn---the bars are drawn side by side,
#' but then redrawn (with more transparency) stacked on top of the right-most bar.
#'
#' @section Color
#' 
#' @inheritSection draw General Draw Arguments
#' 
#' @usage draw(x # table/counts, 
#'      col = NA)
#'      
#' draw( , y # table/counts, 
#'      col = NA)
#' @inheritParams draw
#' @inheritParams draw_scatter
draw_barplot <- function(counts, log = '', 
                         horizontal = FALSE, beside = TRUE, heat = length(dim(counts) == 2L) && length(counts) > 80L,
                         xlim = NULL, ylim = NULL, 
                         quantiles = c(), mean = FALSE, showCounts = FALSE,
                         col = NULL,  alpha = .9, ...) { 
  
  checks(beside, xTF, seealso = '?draw_barplot')
  checks(heat, xTF, seealso = '?draw_barplot')
  checks(horizontal, xTF, seealso = '?draw_barplot')
  checks(mean, xTF, seealso = '?draw_barplot')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_barplot')
  checks(showCounts, xTF, seealso = '?draw_barplot')
  
  # counts should be a table object
  if (!is.numeric(c(counts))) .stop("No draw() method for a matrix/table of class '{class(x[1, 1])}.'")
  dimnames(counts) <- lapply(dimnames(counts), \(dn) ifelse(is.na(dn), "NA", dn))
  
  # if table is one dimensional, add col dimension
  if (length(dim(counts)) == 1L)  {
    dn <- dimnames(counts)
    dim(counts) <- c(dim(counts), 1L)
    
    dimnames(counts) <- c(dn, list(''))
  }

  if (heat) return(draw_heat(counts, log = log, ...))
  
  if (dim(counts)[1] == 1L) counts <- t(counts)
  
  type <- if (is.null(beside)) 'both' else { if (beside) 'beside' else 'stacked'}
  space <- if (type == 'stacked') .5 else c(0, 1 + nrow(counts) %/% 8) 
  
  ylim <- ylim %||% c(0, if (is.null(beside) || type == 'beside') max(counts) else max(colSums(counts)))
  
  col <- prep_col_categories(col %||% rownames(counts), rownames(counts), alpha = alpha, log = log, ...)
  col$legend <- NULL
  output <- list(col = col)
  
  barx <-  barplot(plot = FALSE, counts, col = if (type == 'stacked' ) rev(col$col) else col$col, log = gsub('x', '', log), 
                   space = space, beside = type != 'stacked')
  
  xlim <- xlim %||% c(1, ceiling(max(barx)))
  output$window <- data.table(layout = 1L,
                              xlim = list(xlim, ylim)[[horizontal + 1L]], 
                              ylim = list(ylim, xlim)[[horizontal + 1L]],
                              log = log)
  
  # axes
  proportions <- pretty(ylim / sum(counts), n = 10L, min.n = 5L)
  proportions <- setNames(proportions * sum(counts), proportions)
  axes <- data.table(side = if (horizontal) c(1, 3) else c(2, 4),
                     ticks = list(proportions,
                                  unique(round(axTicks((!horizontal) + 1, 
                                                       log = grepl(if (horizontal) 'x' else 'y', log, fixed = TRUE))))),
                     line = 1L)
  
  if (ncol(counts) > 1) axes <- rbind(axes,
                                      data.table(side = if (horizontal) 2 else 1,
                                                 ticks = list(setNames(if (type == 'stacked') barx else colMeans(barx), colnames(counts))),
                                                 line = 1 + as.integer(type != 'stacked')))
  if (type != 'stacked' && nrow(counts) > 1L && length(counts) < 100) axes <- rbind(axes, fill = TRUE,
                                                                                    data.table(side = if (horizontal) 2 else 1,
                                                                                               ticks = list(setNames(c(barx), rownames(counts)[row(barx)])),
                                                                                               line = 1,
                                                                                               col = list(col$col)))
  output$axes <- axes
  
  # legend_col_discrete(rownames(x), col$col, pch = 15, side = 4, marginLines = marginLines)
  
  # axis Names
  axisNames <- vector('list', 4L)
  axisNames[if (horizontal) c(1, 3) else c(2,4)] <- c('Proportion', if (is.integer(counts)) 'Count' else 'N')
  
  axisNames[if (horizontal) 2 else 1] <- paste(Filter(\(counts) counts != '', names(dimnames(counts))), collapse = ' × ')
  output$axisNames <- axisNames
  
  output$canvas <- function() plot.window(xlim = xlim, ylim = ylim)
  output$drawer <- function() {
    
    barx <- barplot(counts, col = if (type == 'stacked' ) rev(col$col) else col$col, 
                    log = gsub(if (horizontal) 'y' else 'x', '', log), space = space,
                    axisnames = FALSE, 
                    horiz = horizontal,
                    add = TRUE,
                    ylab = '', xlab = '',
                    beside = type != 'stacked', axes = FALSE, 
                    ylim =  if (horizontal) xlim else ylim,
                    xlim = if (horizontal) ylim else xlim,
                    border = rgb(.2,.2,.2,.2))
    
    if (type == 'both') {
      barplot(counts[nrow(counts):1, ], col = setalpha(rev(col$col), alpha / 4), border = rgb(.2,.2,.2, alpha / 3),
              names.arg = logical(ncol(counts)), axes = FALSE,
              horiz = horizontal,
              add = TRUE, beside = FALSE, space = nrow(counts) + space[2] - 1)
    }
    
    # draw extra stuff
    draw_quantiles(if (horizontal) 1 else 2, counts, quantiles = quantiles, limits = grconvertX(c(-.01, 1.01), 'nfc', 'user'))
    if (mean) draw_mean(colMeans(barx), colMeans(counts))
    if (showCounts) draw_counts(barx, counts, counts, col =col$col,min(diff(counts)))
    
  }
  
  output
}

### draw_heat ----

draw_heat <- function(tab, log = '', xlim = NULL, ylim = NULL, ...) {
  # cex/pch aren't used obviously, but it gets passed in ... above, causing warnings below
  xlim <- c(0L, ncol(tab))
  ylim <- c(0L, nrow(tab))
  
  
  col <- prep_col(c(tab), c(tab), log = log, pch = NULL)
  colarray <- array(col$col, dim = dim(tab))
  
 
  output <- canvas(seq_along(tab), xlim = xlim %||% xlim, 
                   seq_along(tab), ylim = ylim %||% ylim)
  output$axes <- data.table(side = 1:2,
                            ticks = list(setNames(1:ncol(tab) - .5, colnames(tab)),
                                         setNames(1:nrow(tab) - .5, rev(rownames(tab)))),
                            line = 1)
  
  output$col <- col
  
  window <- data.table(layout,
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  axisNames <-  vector('list', 4L)
  if (names(dimnames(tab))[1] != '') axisNames[[1]] <- names(dimnames(tab))[1]
  if (names(dimnames(tab))[2] != '') axisNames[[2]] <- names(dimnames(tab))[2]
  output$axisNames <- axisNames
  
  output$drawer <- function() {
    Map(\(i, j, c) {
      polygon(c(i, i, i - 1, i - 1), 
              c(j, j - 1, j - 1, j), 
              col = c,
              border = rgb(.1, .1, .1, .1), lwd = .3)
      
    }, col(tab), nrow(tab) + 1L - row(tab), colarray)
    
  }
  output
}








### draw_violin ----



#' Draw "violin" plot
#' 
#' This method draws the distribution of a numeric (continuous) `y` variable
#' within groups defined by a categorical (discrete) `x` variable,
#' creating a "[violin plot](https://en.wikipedia.org/wiki/Violin_plot)"---so-called
#' for its characteristic shape that can (sometimes) resemble the shape of a violin.
#' If there are too many violins (categories), the plot would be unreadable,
#' so `draw()` will throw an error if the `x` argument has more than 25 unique values.
#'
#' @details
#' 
#' 
#' A violin plot is much like a density [histogram/contour][draw_density()] plot turned on its
#' side, with the shape mirrored left to right.
#' Like [draw_density()], the violin-plot density shape can be generated using either R's [density()] or
#' [hist()] algorithms, for smooth and binned plots respectively.
#' We can pass arguments directly through to these functions:
#' For example, the `breaks` argument can be passed through to [hist()],
#' or the `bw` and `kernel` arguments to [density()].
#' The smooth density contour is drawn by default; to draw a binned plot,
#' use `smooth = FALSE`.
#' 
#' ### Density vs Mass
#' 
#' Whether smoothed or binned, the width of each "violin" represents the probability **density**.
#' The width of bars (when `smooth = FALSE`) does *not* correspond exactly to the 
#' the probability **mass** in each bin, because that actually depends on the size of the bins;
#' For narrow bins (or continuous contours), density can even be greater than 1.
#' If bin sizes are all equal, then the *relative* width of the density bars *does*
#' map exactly to the relative probability mass of each bin.
#' If bins are *not* equal width---which can only happen if you manually specify
#' unequal bins using the `breaks` argument---the widths of bars *don't* map to probability mass.
#' However, using the density assures that the relative **area** of each bin *does* match the probability mass 
#' associated with that bin, even if the bins are of unequal size.
#' The scale of the densities in the plot is shown through the width of a set of lines above each violin.
#' 
#' @section Color:
#' 
#' By default, each violin is automatically drawn a different color, with
#' a color legend drawn as well.
#' (A color legend will be drawn automatically.)
#' However, these colors can be controlled using the `col` argument;
#' you can provide a single color for all the violins or 
#' a vector of unique colors exactly the same length as the number of categories 
#' (unique values in `x`).
#' These colors are mapped to the violins, from left to right.
#' (Use `alpha` independently to change the transparency.)
#' 
#' @inheritSection draw General Draw Arguments
#' 
#' @param smooth ***Should a smoothed density curve be estimated for each group?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, a smooth density contour is estimated from each group of `y` values, using [density()].
#' If `FALSE`, `y` values are binned using [hist()].
#' 
#' @param showPoints ***Should individual points from `y` be overlaid on the violin(s)?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, individual data points from the `y` input vector
#' are plotted in a "cloud" over each violin.
#' The Y-position of each point is matched to its actual value;
#' The X-position of each point is randomly (uniformly) spread across the area of each violin.
#' The points will look consistent, regardless of what histogram- or density-algorithm parameters are used.
#'
#' @param normalReference ***Should a Gaussian reference distribution be drawn?***
#' 
#' Defaults to `FALSE`. 
#'
#' Must be a singleton `logical` value: an on/off switch.
#'
#' If `TRUE`, a normal (Gaussian) distribution is drawn as a dashed
#' black line. The mean and standard deviation of this distribution is taken from the input vector `y`,
#' within each group.
#' This gives a sense of how close to normally distributed `y` is within each group.
#'
#' @param conditional ***Should probability density be calculated separately within each group?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' This controls whether the density
#' of each violin matches its global share in the distribution of input variable `y`, or is rescaled in each
#' group to sum/integrate to 1. I.e., should probabilities be conditioned on the grouping factor?
#' Setting `conditional = TRUE` is useful if you want to see the details of how each group is distributed.
#' `conditional = FALSE` (the default) is useful when you want to see the actual proportion of
#' data in each group (i.e., if the groups are different sizes).
#'
#' @param quantiles ***Should distribution quantiles of `y` be marked?***
#'
#' Defaults to `c(.25, .75)`, so the inter-quartile interval is shown.
#' 
#' Must be a vector of numbers between 0 and 1 (inclusive), or an empty vector.
#'
#' If any quantiles are specified, each quantile is drawn as a horizontal line on the plot, labeled appropriately.
#' For example, `quantiles = .5` will draw a line at the median of input vector `y`;
#' `quantiles = c(.25, .5, .75)` will draw lines marking the four quartiles of `y`.
#' (Depends on `global_stats` argument.)
#' 
#' @param mean ***Should the mean of input vector `y` be marked at the center of each violin?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param global_stats ***Should quantiles and/or means be drawn within each group?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `global_stats = FALSE`, quantiles and/or means are drawn separately for each group. 
#' This can get very messy very quickly!
#'
#' @param log ***Should Y axis be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale).
#' 
#' Must be a single `character` string, either `""` (linear scale)
#' of `"y"` (draw Y on a logarithmic scale ).
#'  
#' @usage draw(x # discrete,  
#'      y # numeric,
#'      col = NA)
#' @inheritParams draw
draw_violins <- function(x, y, smooth = TRUE, conditional = FALSE, 
                         mean = TRUE, quantiles = c(.25, .75), global_stats = FALSE, 
                         breaks = "Sturges", bw = 'SJ', normalReference = FALSE, showPoints = FALSE,
                         xlim = NULL, ylim = NULL, log = '',
                         col = 1, ...) {
  checks(conditional, xTF, seealso = '?draw_violins')
  checks(global_stats, xTF, seealso = '?draw_violins')
  checks(mean, xTF, seealso = '?draw_violins')
  checks(normalReference, xTF, seealso = '?draw_violins')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_violins')
  checks(smooth, xTF, seealso = '?draw_violins')
  checks(showPoints, xTF, seealso = '?draw_violins')
  
  
  groups <- x
  
  groups <- groups[!is.na(y)]
  y <- y[!is.na(y)]
  categories <- sort(unique(groups))
  if (length(categories) > 25L) {
    .stop("You can't draw a violin plot with more than 25 categories---it would result in the world's ",
          "smallest violins. ",
          "You have provided an grouping argument with {num2word(length(categories))} unique values.")
  }
  
  col <- prep_col_categories(col %||% categories, categories, ...)
  
  breaks <- if (!smooth && length(breaks) == 1L && pmatch(breaks, 'quantiles', 0)) {
    quantile(y, c(0, if (length(quantiles)) sort(unique(quantiles)) else c(.25, .5, .75), 1))
  } else {
    hist.default(y, breaks = breaks, plot = FALSE)$breaks
  }
  names(breaks) <- format(breaks, digits = 3)
  
  values <- tapply(y, groups, list)
  ptable <- proportions(table(groups))
  coordinates <- multihist_coor(y, groups, vardim = 'Y', bw = bw,
                                conditional = conditional, smooth = smooth, breaks = breaks)
  
  # need to figure out x-limit width
  allDens <- unlist(lapply(coordinates, '[[', 'Density'))
  xkeyWidths <- unique(2^(ceiling(log(max(allDens), 2)):floor(log(median(allDens), 2)))) |> head(4)
  xlim <- xlim %||% c(0, xkeyWidths[1]) # 1, .5, .25, .125, etc.
  
  ## each violin will draw across a x range of N - . 5: (N + .5), centered on N
  ## need to scale density to this range, based on xlim
  coordinates <- lapply(coordinates, \(coor) {
    coor[ , X := (Density / xlim[2]) / 2]
    coor
  })
  xkeyWidths_scaled <- (xkeyWidths[-1] / xkeyWidths[1]) / 2
  
  violinN <- setNames(seq_along(coordinates), categories)
  
  output <- canvas(x = violinN, xlim = c(0.5, max(violinN) + .5), 
                   y =  y, #ylim = ylim %||% range(breaks), 
                   ylim = ylim %||% range(unlist(lapply(coordinates, '[[', 'Y'))), #this stops density from going off plot
                   log = gsub('x', '', log))
  
  # prepare ticks
  ## y
  output$breaks <- y.ticks <- breaks
  output$draw_type <- 'violin'
  while(length(y.ticks) > 20L) {
    y.ticks <- y.ticks[seq(1, length(y.ticks), by = 2)]
  }
  output$axes[side == 2, ticks := y.ticks]
  
  ## x
  # output$axisNames[[1]] <- 'Probability density'
  output$axes[side == 1, ticks := list(violinN)]
  
  output$col <- col
  output$drawer <- function() {
    
    Map(\(coor, col, N, vals, proportion) {
      
      coor[ , {
        # polygon(c(X, rev(X)), c(Density, rep(ymin, length(Density))), col = color, border = NA, xpd = NA)
        if (!smooth) {
          X <- c(X, rep(0, length(X)))
          Y <- c(Y, rev(Y))
        }
        polygon(N + X, Y, border = NA, col = col, xpd = NA)
        polygon(N - X, Y, border = NA, col = col, xpd = NA)
        
        
        ## Draw density Key
        ykey <- grconvertY(seq(.97, 1.00, length.out = length(xkeyWidths_scaled)), 'npc', 'user')
        graphics::segments(N - xkeyWidths_scaled, x1 = N + xkeyWidths_scaled, lwd = .5, lty = 'solid',
                           ykey, ykey, xpd = NA)
        
        text(N, grconvertY(1.02, 'npc', 'user'), 'Probability density', xpd = NA,
             cex = .5, col = par('cex.lab'))
        text(N + xkeyWidths_scaled, ykey, 
             format(xkeyWidths[-1], drop0trailing = T) |> stringr::str_remove('^0'),
             cex = .4, pos = 4, xpd = NA)
        
        if (normalReference) {
          ypoints <- seq(output$window$ylim[[1]][1], output$window$ylim[[1]][2], length.out = 100)
          norm <- dnorm(ypoints, mean(vals), sd(vals)) / (xkeyWidths[1] * 2)
          if (!conditional) norm <- norm * proportion
          points(N + norm, ypoints, type = 'l',
                 lwd = .5, lty = 'dashed')
          points(N - norm, ypoints, type = 'l',
                 lwd = .5, lty = 'dashed')
        }
        
        if (showPoints) {
          ysamp <- if (length(vals) >= 10^5) sample(vals, 10^5) else vals
          samp_dens <- X[X > 0][order(Y[X > 0])][findInterval(ysamp, sort(Y[X > 0]))]
          xsamp <- runif(length(ysamp), -samp_dens, samp_dens)
          dotAlpha <- cex_density(xsamp, ysamp, .3)
          points(xsamp + N , ysamp,  cex = .3, col = setalpha('black', dotAlpha), pch = 16, xpd = NA)
        }
        
        if (!global_stats && length(coordinates) > 1L) {
          draw_quantiles(2, vals, quantiles, limits = c(N - .5, N + .5)) 
          if (mean) draw_mean(N, mean(vals), col = 'black')
        }
      }]
      
    }, coordinates, col$col, violinN, values, ptable)
    if (global_stats || length(coordinates) == 1L) {
      draw_quantiles(2, y, quantiles)
      draw_mean(violinN, mean(y))
      
    }
  }
  
  output
  
}




### draw_area ----

#' Draw area plot
#' 
#' This method draws the distribution of a categorical (discrete) `y` variable
#' across the range of a numeric (continuous) `x` variable, creating an 
#' "[area plot](https://en.wikipedia.org/wiki/Area_chart)."
#' Specifically, colored areas representing the estimated probability density of each
#' unique category in `y`, depending on the value of `x`, are stacked on top of each
#' other. 
#' The result is like stacking multiple histograms on top of each other.
#'
#' @details
#' 
#' 
#' Like [draw_density()], the area-plot density shape(s) can be 
#' generated using either R's [density()] or
#' [hist()] algorithms, for smooth and binned plots respectively.
#' We can pass arguments directly through to these functions:
#' For example, the `breaks` argument can be passed through to [hist()],
#' or the `bw` and `kernel` arguments to [density()].
#' The smooth density contour is drawn by default; to draw a binned plot,
#' use `smooth = FALSE`.
#' 
#' ### Density vs Mass
#' 
#' Whether smoothed or binned, the height of each color at each
#' X coordinate represents the probability **density** of each category.
#' The height of bars (when `smooth = FALSE`) does *not* correspond exactly to the 
#' the probability **mass** in each bin, because that actually depends on the size of the bins;
#' For narrow bins (or continuous contours), density can even be greater than 1.
#' If bin sizes are all equal, then the *relative* width of the density bars *does*
#' map exactly to the relative probability mass of each bin.
#' If bins are *not* equal width---which can only happen if you manually specify
#' unequal bins using the `breaks` argument---the widths of bars *don't* map to probability mass.
#' However, using the density assures that the relative **area** of each bin *does* match the probability mass 
#' associated with that bin, even if the bins are of unequal size.
#' The scale of the densities in the plot is shown through the width of a set of lines 
#' to the left of the plot.
#' 
#' @section Color:
#' 
#' By default, colors are automatically chosen to represent
#' the categories in the `y` variable.
#' (A color legend will be drawn automatically.)
#' However, these colors can be controlled using the `col` argument;
#' you can provide a vector of unique colors exactly the same length as
#'  the number of categories (unique values in `y`).
#' These colors are mapped to the areas, from bottom up.
#' (Use `alpha` independently to change the transparency.)
#' 
#' @inheritSection draw General Draw Arguments
#' 
#' @param smooth ***Should a smoothed density curve be estimated for each group?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, a smooth density contour is estimated from each group of `x` values, using [density()].
#' If `FALSE`, `x` values are binned using [hist()].
#' 
#' @param showPoints ***Should individual points from `x` be shown above the density plot?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, individual data points from the `x` input vector
#' are plotted in a "cloud" above the area plot.
#' The X-position of each point is matched to its actual value;
#' The Y-position of each point is randomly (uniformly) selected in a range at the top of the plot window.
#' This spaces out points that are close together, so it is easier to see how dense they are.
#' 
#' @param conditional ***Should probability density be calculated separately within each group?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' This controls whether the density
#' of color area matches its global share in the distribution of input variable `x`, or is rescaled in each
#' group to sum/integrate to 1. I.e., should probabilities be conditioned on the grouping factor?
#' Setting `conditional = TRUE` is useful if you want to see the details of how each group is distributed.
#' `conditional = FALSE` (the default) is useful when you want to see the actual proportion of
#' data in each group (i.e., if the groups are different sizes).
#'
#' @param center ***Should the stacked areas be centered on the screen?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the areas are centered in the Y-axis, expanding outward (up and down)
#' when the density increases.
#' If `FALSE`, the areas are stacked up from the bottom of the screen.
#' 
#' @param quantiles ***Should distribution quantiles of `y` be marked?***
#'
#' Defaults to `c()`, so no quantiles are drawn.
#' 
#' Must be a vector of numbers between 0 and 1 (inclusive), or an empty vector.
#'
#' If any quantiles are specified, each quantile is drawn as a vetical line on the plot, labeled appropriately.
#' For example, `quantiles = .5` will draw a line at the median of input vector `x`;
#' `quantiles = c(.25, .5, .75)` will draw lines marking the four quartiles of `x`.
#' (Depends on `global_stats` argument.)
#' 
#' @param mean ***Should the mean of input vector `x` be marked below the plot?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param global_stats ***Should quantiles and/or means be drawn within each group?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `global_stats = FALSE`, quantiles and/or means are drawn separately for each group. 
#' This can get very messy very quickly!
#'
#' @param log ***Should X axis be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale).
#' 
#' Must be a single `character` string, either `""` (linear scale)
#' of `"x"` (draw X on a logarithmic scale ).
#'  
#' @usage draw(x # numeric,  
#'      y # discrete,
#'      col = NA # colors chosen automatically)
#' @inheritParams draw
draw_area <- function(x, y, log = '', 
                      center = TRUE, smooth = TRUE, conditional = FALSE, 
                      breaks = 40, bw = 'SJ', 
                      mean = TRUE, quantiles = c(), global_stats = TRUE,
                      showPoints = FALSE,
                      xlim = NULL, ylim = NULL, 
                      col = NULL, alpha = .7, ...) {
  checks(center, xTF, seealso = '?draw_area')
  checks(conditional, xTF, seealso = '?draw_area')
  checks(global_stats, xTF, seealso = '?draw_area')
  checks(mean, xTF, seealso = '?draw_area')
  checks(quantiles, xnull | (xnumeric & xrange(0, 1)), seealso = '?draw_area')
  checks(smooth, xTF, seealso = '?draw_area')
  checks(showPoints, xTF, seealso = '?draw_area')
  
  categories <- sort(unique(y), decreasing = TRUE)
  
  breaks <- hist.default(x, breaks = breaks, plot = FALSE)$breaks 
  
  coordinates <- area_coor(x, y, smooth = smooth, conditional = conditional, 
                           center = center, bw = bw, breaks = breaks, ...)
  
  output <- canvas(x, xlim, range(coordinates$Y), ylim, log = gsub('y', '', log))
  output$col <- prep_col_categories(col %||% categories, rev(categories), 
                                    alpha = alpha, ...)
  if (!conditional && center)  output$axes <- output$axes[side == 1]
  output$axisNames[[2]] <-'Probability density' 
  # if (center) output$axes[ , ticks := lapply(ticks, \(t) {names(t) <- abs(t) ; t})]
  
  X <- coordinates$X
  output$drawer <- function() {
    for (j in 1:(ncol(coordinates$Y) - 1L)) {
      polygon(c(X, rev(X)), 
              c(coordinates$Y[ , j], rev(coordinates$Y[ , j + 1])), 
              col = output$col$col[j],
              border = FALSE, xpd = NA)
      if (showPoints) draw_points(x, output$col$col[match(y, categories)], 
                                  coordinates$Y, output$window$ylim)
      
      
      
      
      if (!global_stats && length(categories) > 1) {
        draw_quantiles(1, x[y == categories[j]], 
                       quantiles,
                       limits = NULL, 
                       col =  setalpha(output$col$col[j], 1))
        if (mean) draw_mean(mean(x[y == categories[j]]), 
                            grconvertY(0.01, 'npc', 'user'), 
                            col = rev(output$col$col)[j])
      }
    }
    
    ## Draw density Key
    
    if (!conditional && center) {
      xkey <- grconvertX(seq(-.04, 0.0, length.out = length(coordinates$DensityKey)), 'npc', 'user')
      ykey <- coordinates$DensityKey / 2
      
      graphics::segments(x0 = xkey, x1 = xkey, 
                         -ykey, ykey, lwd = .5, lty = 'solid', xpd = NA)
      text(xkey,  ykey, srt = 90,
           format(coordinates$DensityKey, drop0trailing = T) |> stringr::str_remove('^0'),
           adj = c(0, 1), 
           cex = .4, xpd = NA)
    }
    
    
    
    
    if (global_stats || length(coordinates) == 1L) {
      if (mean) draw_mean(mean(x), grconvertY(0.01, 'npc', 'user'))
      draw_quantiles(1, x, quantiles, limits = NULL)
    } 
    
  }
  
  output
  
}



## draw_facets ----


draw_facets <- function(full_data, faceted_data,  
                        xexpr = '', yexpr = '', 
                        xlab = NULL, ylab = NULL,
                        axes = 1:4, legend = TRUE,
                        aspect = NULL,
                        ...) {
  
  # overall xlim ylim etc (output) is determined by plotting the full data
  output <- with(full_data, .draw(x = x,  y = y, ...,  
                                  col = col, cex = cex, pch = pch))
  
  output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
  output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  
  # args <- list(x = x, y = y,  log = output$window$log, 
               # col = output$col$col, cex = output$cex$cex,
               # xlim = output$window$xlim[[1]], ylim = output$window$ylim[[1]], ...)
  facet_dimnames <- dimnames(faceted_data)
  facet_names <- .names(facet_dimnames)
  
  facet_sizes <- array(0L, dim = dim(faceted_data), dimnames = dimnames(faceted_data))
  facet_sizes[] <- sapply(faceted_data, \(fdata) length(fdata[[1]]))
  
  if ('breaks' %in% names(output)) {
    faceted_data[] <- Map(faceted_data, prop.table(facet_sizes),  
                          f = \(fdata, p) {  
                            fdata$breaks <- output$breaks  
                            fdata$hist_scale <- p 
                            fdata 
                            })
  }
  # args <- args[!duplicated(names(args))]
  
  # Determine layout
  lay <- array(seq_along(facet_sizes), dim = dim(facet_sizes))
  output$layout <-  if (length(dim(lay)) == 1L) cbind(lay) else lay
  
  
  if (length(dim(lay)) == 1L) {
    left.side   <- right.side <- facet_sizes > 0L
    top.side    <- seq_along(facet_sizes) == 1L
    bottom.side <- seq_along(facet_sizes) == length(facet_sizes)
  } else {
    .facet_sizes <- facet_sizes > 0
    left.side   <- leftmost(.facet_sizes)
    right.side  <- rightmost(.facet_sizes)
    top.side    <- topmost(.facet_sizes)
    bottom.side <- bottommost(.facet_sizes)
    
  }
               
  axisNames <- output$axisNames
  axes <- output$axes
  
  output$drawer <- function() {
    for (n in lay) {
      
      # if (n > min(lay)) plot.new()
      plot.new()
      
      # prepare args and draw
      if (facet_sizes[n] > 0) {
        facet <- do.call('.draw', c(faceted_data[[n]], list(...))) # actual draw of plot
        marginLines <- setMargins(.1, aspect = aspect)
        
        output$canvas()
        facet$drawer()
        
        sides <- c(bottom.side[n], left.side[n], top.side[n], right.side[n])
        lapply(which(!sides), border)
        
        # # axes 
        curaxes <- intersect(which(sides), axes$side)
        
        humaxes(axes,
                ifelse(1:4 %in% curaxes, axisNames, vector('list', 4L)),
                curaxes, marginLines)
      } 
     
      coor <- which(cbind(lay == n), arr.ind = TRUE)
      
      # draw facet levels
      if (facet_sizes[n] > 0) {
        if (nrow(facet_sizes) > 1L && sides[4]) {
          text(grconvertX(marginLines[[4]][3], 'inches', 'user'), 
               grconvertY(.5, 'npc', 'user'), 
               facet_dimnames[[1]][coor[ , 'row']],
               cex = 1.5, xpd = NA, col = par('col.axis'))
        }
        if (length(dim(facet_sizes)) > 1L && ncol(facet_sizes) > 1L && sides[3]) {
          
          text(grconvertX(.5, 'npc', 'user'),
               grconvertY(marginLines[[3]][3], 'inches', 'user'), 
               collevel <- facet_dimnames[[2]][coor[ , 'col']],
               cex = 1.5, xpd = NA, col = par('col.axis'))
        }
      }
      
      # If we are in the middle row, draw the facet (dimension level) label
      if (facet_names[1] != '' && sides[4] &&
          coor[ , 'row'] == ceiling(nrow(facet_sizes) / 2)) {
        
        lab <- facet_names[1]
        text(grconvertX(marginLines[[4]][5], 'inches', 'user'), 
             if (is.whole(nrow(facet_sizes) / 2)) {
               grconvertY(marginLines[[1]][5], 'inches', 'user')
             } else {
               grconvertY(.5, 'npc', 'user')
             },
             lab, xpd = NA, col = par('col.lab'), 
             cex = 2, srt = if (nchar(lab) > 3L) -90 else 0)
      }
      
      # If we are in the middle col, draw the facet label
      if (length(dim(facet_sizes)) > 1 && facet_names[2] != '' && sides[3] &&
        coor[ , 'col'] == ceiling(ncol(facet_sizes) / 2)) {
        
        lab <- facet_names[2]
        text(if (is.whole(ncol(facet_sizes) / 2)) {
               grconvertX( 1, 'nfc', 'user')
             } else {
               grconvertX(.5, 'npc', 'user')
             },
             grconvertY(marginLines[[3]][4], 'inches', 'user'),
             lab, xpd = NA, col = par('col.lab'), 
             cex = 2)
      }
      
    }
    
  }
  
  # axis names for facet(s)
  output$axisNames <- vector('list', 4L)
  output$axes <- output$axes[0]
  output$faceted <- TRUE
  
  output
}

prep_facets <- function(x, y, facets) {
  if (!is.list(facets)) facets <- list(facets)
  
  if (length(facets) > 2L) .stop("The draw() function can't handle more than two faceting variables.",
                                 "You have provided {num2print(length(facet))}.")
  
  vecsize <- max(length(x), length(y))
  if (!all(lengths(facets) == vecsize)) {
    .stop('Facets variables must be vectors of the same length as the x/y plotting variables.')
  }
  
  facets <- lapply(facets, \(facet) {
    if (is.numeric(facet) && length(unique(facet)) > 8) {
      cut(facet, breaks = 4) 
    } else {
      facet
    }
  })
  
  facets
}

table_dimtolist <- function(tab) {
  
  higherdim <- dim(tab)[-1:-2]
  
  listmat <- array(apply(tab, seq_along(dim(tab))[-1:-2], as.table, simplify = FALSE),
                   dim = dim(tab)[-1:-2], dimnames = dimnames(tab)[-1:-2])
  
  listmat
}

### draw adders ---



## Dispatch for draw() ----

setGeneric('.draw', def =  \(x, y,  ...) standardGeneric('.draw'))

### atomic ----

#### numeric ----

setMethod('.draw', c('numeric', 'NULL'), draw_density)
setMethod('.draw', c('NULL', 'numeric'), draw_Qplot)
setMethod('.draw', c('numeric', 'numeric'), draw_scatter)

#### numeric X discrete ----

setMethod('.draw', c('discrete', 'numeric'), draw_violins)
setMethod('.draw', c('numeric', 'discrete'), draw_area)

#### discrete only ----
setMethod('.draw', c('discrete', 'NULL'),
          function(x, y, ...) draw_barplot(table(x), ...))

setMethod('.draw', c('NULL', 'discrete'),
          function(x, y, ...) draw_barplot(table(y), horizontal = TRUE, ...))

setMethod('.draw', c('discrete', 'discrete'),
          function(x, y, ...) draw_heat(table(x, y), ...))

### tables ----



setMethod('.draw', c('table', 'NULL'), 
          function(x, y, ..., col = NA, pch = NA, cex = NA) {
            if (length(dim(x)) > 2) {
              
              full <- list(x = apply(x, 1:2, sum) |> as.table(),
                           y = NULL, col = col, cex = NA, pch = NA)
              faceted_tables <- table_dimtolist(x)
              faceted_tables[] <- lapply(faceted_tables, \(tab) list(x = tab, y = NULL,
                                                                     col = col, cex = NA, pch = NA))
              draw_facets(full, faceted_tables, ...)
              
            } else {
              draw_barplot(x, ...)
              
            }
            })
setMethod('.draw', c('NULL', 'table'), function(x, y, ...) draw_barplot(y, horizontal = TRUE, ...))


setMethod('.draw', c('count', 'NULL'),
          function(x, y, ...) {
            draw_barplot(as.table(x), ...)
            })

setMethod('.draw', c('NULL', 'count'),
          function(x, y, ...) {
            draw_barplot(as.table(y), horizontal = TRUE, ...)
            
            })

setMethod('.draw', c('probability', 'NULL'),
          function(x, y, ...) .draw(count(x), NULL, ...))

setMethod('.draw', c('NULL', 'probability'),
          function(x, y, ...) .draw(NULL, count(y), ...))


### humdrumR and formula ----

setMethod('.draw', c('humdrumR.table', 'NULL'),
          function(x, y, ...) {
            class(x) <- class(x)[-1]
            .draw(x, NULL, ...)
          })

setMethod('.draw', c('humdrumR'),
          function(x, facet = NULL, ...) {
            selected <- pullSelectedField(x, null = 'asis')
            fields <- fields(x)
            groupFields <- if (length(facet)) {
              fieldMatch(x, unlist(facet), callfun = 'draw')
            } else {
              fields[GroupedBy == TRUE]$Name 
            }
            if (length(groupFields)) {
              facet <- pullFields(x, groupFields)
            }
            .draw(selected, facet = facet, ...)
            
          })

setMethod('.draw', c('formula'),
          function(x, y, col = 2, xlab = NULL, ylab = NULL, data = NULL, ...) {
            
            vars <- model.frame(x, data = data)
            
            if (ncol(vars) == 1L) {
              .draw(vars[[1]], col = col, ..., xlab = xlab %||% names(vars), ylab = ylab)
            } else {
              
              if (ncol(vars) > 2) {
                
              }
              
              .draw(vars[[2]], vars[[1]], col = col, ...,
                   xlab = xlab %||% names(vars)[2],
                   ylab = ylab %||% names(vars)[1])
            } 
            
            
            list(xlab = '')
            
          })
# 
# setMethod('.draw', c('token', 'NULL'),
#           function(x, y, ...) {
#             x <- token2atomic(x)
#             .draw(x, NULL, ...)
#           })
# 
# setMethod('.draw', c('NULL', 'token'),
#           function(x, y, ...) {
#             y <- token2atomic(y)
#             .draw(NULL, y, ...)
#           })
# 
# setMethod('.draw', c('token', 'token'),
#           function(x, y, ...) {
#             x <- token2atomic(x)
#             y <- token2atomic(y)
#             .draw(x, y, ...)
#           })
# setMethod('.draw', c('discrete', 'numeric'),
#           function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
#             .draw(list(1, factor(x)), y, col = col, log = log, breaks = breaks, ..., yat = yat)
#             list(xlab = NULL, ylab = NULL)
#           })

# setMethod('.draw', c('list', 'numeric'),
#           function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
#             
#             layout <- prep_layout(x)
#             oldpar <- par(oma = par('mar'), mar = c(0, 0, 0, 0))
#             on.exit({
#               layout(cbind(1)) 
#               par(oldpar)
#               
#             })
#             
#             y.ticks <- auto_ticks(y, log = grepl('y', log), at = yat)
#             ylim <- range(y.ticks)
#             y <- split(y, f = x)
#             
#             x.ticks <- seq(0, 1, .1)
#             x.labels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
#             
#             xuniq <- unique(as.data.frame(x))
#             xuniq <- xuniq[sapply(xuniq, \(val) length(unique(val)) > 1L)]
#             grouplabels <- do.call('paste', xuniq)
#             for (k in c(layout)) {
#               ytick <- if (k %in% layout[, 1]) y.ticks 
#               if (k %in% layout[nrow(layout), ]) {
#                 xtick <- x.ticks 
#                 xlabel <- x.labels
#               } else {
#                 xtick <- xlabel <- NULL
#               }
#               
#               canvas(log = gsub('x', '', log),  xlim = c(0, 1),  ylim = ylim)
#               
#               if (length(layout) > 1L) text(0.2, ylim[1] + (diff(ylim) * .75), grouplabels[k])
#               draw_violin(y[[k]], breaks = breaks)
#             }
#             
#             
#             list(oma = TRUE, xlab = if (length(layout) == 1L) 'Proportion' else "", ylab = "")
#           })


## draw() helpers ----



draw_quantiles <- function(side, var, quantiles = c(.025, .25, .5, .75, .975), limits = NULL, col = 'black', ...) {
  
  if (length(quantiles)) {
    col <- setalpha(col, 1)
    quantiles <- unique(quantiles)
    
    sides <- side %% 2 == 0
    quants <- quantile(var, prob = quantiles)
    
    
    if (is.null(limits)) limits <- if (sides) grconvertX(c(0, 1), 'npc', 'user') else grconvertY(c(0, 1), 'npc', 'user')
    
    
    # labels
    q <- paste0(round(quantiles       * 100, 1), '%')
    p <- paste0(round((1 - quantiles) * 100, 1), '%')
    
    if (sides) {
      text(limits[1], quants, as.expression(lapply(q, \(q) bquote('' %down% .(q)))), 
           cex = .4, xpd = NA, adj = c(0, .5), col = col)
      text(limits[2], quants, as.expression(lapply(p, \(q) bquote(.(q) %up% ''))),  
           cex = .4, xpd = NA, adj = c(1, .5), col = col)
    } else {
      text(quants, limits[1], as.expression(lapply(q, \(q) bquote('' %<-% .(q)))), 
           cex = .4, xpd = NA, adj = c(.5, 1), col = col)
      text(quants, limits[2], as.expression(lapply(p, \(q) bquote(.(q) %->% ''))), 
           cex = .4, xpd = NA, adj = c(.5, 0), col = col)
    }
    
    # lines
    strwidth <- if (sides) {
      max(strwidth(paste0('|', names(quants)), cex = .4) )
    } else {
      max(strheight(names(quants), cex = .4))
    }
    lineArgs <- list(limits[1] + strwidth,
                     limits[2] - strwidth, quants, quants, lty = 'dashed', 
                     lwd = .5, col = col)
    names(lineArgs)[1:4] <- if (sides) {
      c('x0', 'x1', 'y0', 'y1')
    } else {
      c('y0', 'y1', 'x0', 'x1')
    }
    do.call(graphics::segments, lineArgs)
    
    
  }
  
}


draw_mean <- function(x, y, col = 'black') {
  points(x, rep(y, length.out = length(x)), 
         pch = 3, cex = 1.4, lwd = 1.5, xpd = TRUE, col = setalpha(col, 1))
}

draw_counts <- function(x, y, counts, col, width, cex = .8) {
  counts <- prettyN(counts, expr = TRUE)
  
  text(x, y, counts, xpd = NA,
       cex = cex_scale(counts, targetWidth = width * .8, cex = cex), 
       col = setalpha(col, 1), pos = 3)
}

draw_points <- function(x, col, allDens, ylim) {
  xsamp <- if (length(x) >= 10^5) sample(x, 10^5) else x
  ysamp <- runif(length(xsamp), min(max(allDens * 1.1, mean(ylim[[1]]) * 1.5), 
                                    grconvertY(.95, 'npc', 'user')), 
                 grconvertY(1, 'npc', 'user'))
  dotAlpha <- cex_density(xsamp, ysamp, .3)
  points(xsamp, ysamp,  cex = .4, col = setalpha(col, dotAlpha), pch = 16, xpd = NA)
}

draw_mvnorm <- function(x, y) {
  
  sigma <- matrix(cov(x, y), nrow = 2, ncol = 2)
  sigma[1, 1] <- var(x)
  sigma[2, 2] <- var(y)
  
  
  sample <- MASS::mvrnorm(min(10000, length(x) * 10), c(mean(x), mean(y)), sigma)
  
  points(sample[,1], sample[,2], pch = 8, col = rgb(0,0,0, alpha=.02), xpd = NA)
  
  
}


draw_lines <- function(n = 10, outer = FALSE) {
  for (side in 1:4) {
    for (line in 0:n) {
      mtext(paste0('___', line, '___'), side = side, line = line, outer = outer)
    }
  }
}
### creating stable margins ----

# lines
# 0 -> quantile labels
# 1 -> axis labels
# 2 -> second axis ticks
# 3 -> xaxis labels
# 4 -> legend / sub title
# 5 -> title


setMargins <- function(margin.percent = .2, aspect = NULL) {
  

  devsize <- par('fin')
  figsize <- devsize * (1 - margin.percent*2)
  figmar <- devsize * margin.percent 
  
  figasp <- figsize[1] / figsize[2]
  
  if (!is.null(aspect)) {
    if (aspect >= figasp) {
      figsize[2] <- figsize[1] / aspect 
    } else {
      figsize[1] <- figsize[2] * aspect
    }
    
  }
  
  fullmar <- (devsize - figsize) / 2
  
  par(mai = fullmar[c(2, 1, 2, 1)]) #, omi = fullmar[c(2, 1, 2, 1)])
  
  # scale cex to size of device
  # xarea <- prod(devsize)
  xarea <- prod(figsize)
  
  magic <- .1488 # this seems to be the linear slope between cex and strheight('M')
  unity <- 36 # arbitrary. For example, a 6in x 8in plot
  
  cex <- sqrt(xarea /  (unity / (magic^2))) / magic
  par(cex = cex)
  
  # everything is currently inches
  lines <- c(0, .2, .5, .8, 1) * min(figmar)
  
  marginLines <- list(grconvertY(0, 'npc', 'inches') - lines,
                      grconvertX(0, 'npc', 'inches') - lines,
                      grconvertY(1, 'npc', 'inches') + lines,
                      grconvertX(1, 'npc', 'inches') + lines) 
  marginLines
  
}
facetMargins <- function(margin.percent = .15) {
  figsize <- par('fin')
  figmar <- figsize * margin.percent
  
  lines <- c(0, .75, 1.5) * min(figmar)
  
  mai <- par('mai')
  maiX <- mai[2]
  maiY <- mai[1]
  
  marginLines <- list(grconvertY(0, 'nfc', 'inches') - lines + maiY,
                      grconvertX(0, 'nfc', 'inches') - lines + maiX,
                      grconvertY(1, 'nfc', 'inches') + lines - maiY,
                      grconvertX(1, 'nfc', 'inches') + lines - maiX) 
  
  marginLines
}


marginLab <- function(marginLines, text, side, marginLine = 3, las = 0, ...) {
  
  marginLine <- marginLines[[side]][marginLine]
  
  if (side %in% c(1, 3)) {
    x <- grconvertX(.5, 'npc', 'user')
    y <- grconvertY(marginLine, 'inches', 'user')
  } else {
    x <- grconvertX(marginLine, 'inches', 'user')
    y <- grconvertY(.5, 'npc', 'user')
  }
  
  srt <- switch(las + 1,
                c(0, 90, 0, 270)[side],
                0,
                c(90, 0, 90, 0)[side],
                90)
  text(x, y, text, srt = srt, xpd = NA, adj = c(.5, if (side == 1) 0 else 1), offset = 0, ...)
  
}

shrinklim <- function(lim, scale = .8) {
  ((lim - mean(lim)) * scale) + mean(lim)
}

### axes ----


logcheck <- function(log, x = '', y = '') {
  checks(log, xlen1 & xcharacter & xlegal(c('x', 'y', 'xy', 'yx', '')), seealso = '?draw()')
  
  badx <- grepl('x', log, fixed = TRUE) && any(x <= 0)
  bady <- grepl('y', log, fixed = TRUE) && any(y <= 0)
  
  if (badx || bady) {
    bad <- .paste(if (badx) 'x', if (bady) 'y', sep = ' and ')
    .stop("You've specified draw(..., log = '{log}') but your {bad} numbers include zero or negative numbers.",
          "These can't be drawn on a log scale.")
    
  }
  
}

humaxes <- function(axesframe, axisNames, axes = 1:4, marginLines) {
  if (length(axesframe)) do.call('mapply', c(list(humaxis, MoreArgs = list(marginLines = marginLines)),
                                             axesframe[side %in% axes]))
  
  
  Map(axisNames, 1:4, f = \(label, side) {
    if (!is.null(label)) {
      marginLab(marginLines, label, marginLine = 3L, 
                side, col = par('col.lab'), 
                las = if (is.character(label) && nchar(label) > 3 && side %% 2 == 0) 0  else  1)
    } })
  
  
}

humaxis <- function(side, ticks, line = 1, lab = 0, 
                    col = par('col.axis'), cex = par('cex.axis'), marginLines) {
  # this function attempts to draw axis labels that always fit on the screen
  # but never overlap
  las <- 1
  sides <- side %% 2 == 0L
  labels <- if (is.null(names(ticks))) {
    format(ticks, big.mark = ',')
  } else {
    names(ticks)
  }
  
  slotSize <- abs(diff(marginLines[[side]][c(1, 2)])) # in inches
  fits <- checkStrFit(side, slotSize, ticks, labels, cex)
  while(!fits) {
    cex <- cex * .95
    if ( cex < .1) break
    fits <- checkStrFit(side, slotSize, ticks, labels, cex)
  }
  
  marginLine <- marginLines[[side]][line]
  line <- if (sides) {
    marginLine <- grconvertX(marginLine, 'inches', 'user')
    text(marginLine, ticks, pos = side,
         labels, cex = cex, xpd = NA, col = col)
  } else {
    marginLine <- grconvertY(marginLine, 'inches', 'user')
    text(ticks, marginLine, pos = side,
         labels, cex = cex, xpd = NA, col = col)
    
  }
  # axis(side, ticks, labels, line = line, las = las, tick = FALSE, cex.axis = cex, gap.axis = .1)
  
}

checkStrFit  <- function(side, slotSize, ticks, labels, cex) {
  list(checkStrFit_13, checkStrFit_24, checkStrFit_13, checkStrFit_24)[[side]](slotSize, ticks, labels, cex)
}

checkStrFit_13 <- function(slotSize, ticks, labels, cex) {
  
  strWidth  <- strwidth(labels, units = 'inches', cex = cex)
  strHeight <- strheight(labels, units = 'inches', cex = cex)
  
  strStart <- grconvertX(ticks, 'user', 'inches') - (strWidth / 2)
  strEnd   <- grconvertX(ticks, 'user', 'inches') + (strWidth / 2)
  
  tootall <- max(strHeight) >= slotSize
  overlap <- tail(strStart, -1) <= head(strEnd, -1)
  
  
  !(any(overlap) || any(tootall))
  
}

checkStrFit_24 <- function(slotSize, ticks, labels, cex) {
  
  strWidth  <- strwidth(labels, units = 'inches', cex = cex)
  strHeight <- strheight(labels, units = 'inches', cex = cex)
  
  strTop    <- grconvertY(ticks, 'user', 'inches') + (strHeight / 2)
  strBottom <- grconvertY(ticks, 'user', 'inches') - (strHeight / 2)
  
  toowide <- max(strWidth) >= slotSize
  overlap <- tail(strBottom, -1) <= head(strTop, -1)
  
  !(any(overlap) || any(toowide))
  
}


### color ----


setalpha <- function(col, alpha = 1) {
  rgba <- col2rgb(col, alpha = TRUE) / 255
  
  rgb(rgba['red', ], rgba['green', ], rgba['blue', ], alpha)
}

### coordinates ----


bar_coor <- function(x, type) {
  dim <- dim(x)
}

multihist_coor <- function(x, groups, conditional = TRUE, ...) {
  coor_grouped <- tapply(x, groups, hist_coor, ..., simplify = FALSE)
  
  if (length(coor_grouped) > 1L && !conditional) {
    coor_grouped <- Map(\(coor, prop) {
      coor$Density <- coor$Density * prop
      coor
    }, coor_grouped, prop.table(table(groups)))
  } 
  coor_grouped
}

hist_coor <- function(x, smooth = FALSE, breaks = "Sturges", ..., groups = NULL, hist_scale = 1, vardim = 'X') {
  # gets x/density/counts for a numeric distribution, using either density() or hist()
  # but returning the same format either way
  if (smooth) {
    dens <- stats::density.default(x, ...)
    output <- data.table(Dim = dens$x, Density = dens$y)
  } else {
    hist <- graphics::hist.default(x, breaks = breaks, plot = FALSE)
    
    output <- data.table(Density = hist$density, Counts = hist$counts, Mids = hist$mids, 
                         Delta = diff(hist$breaks))
    output <- output[rep(1:nrow(output), each = 2)]
    i <- c(1, rep(2:(length(hist$breaks) - 1), each = 2), length(hist$breaks))
    output[ , Dim := hist$breaks[i]]
    
  }
  output[, Density := Density * hist_scale]
  colnames(output)[colnames(output) == 'Dim'] <- vardim
  output[]
}


area_coor <- function(x, groups,  smooth = TRUE, conditional = FALSE, center = TRUE, bw = 'SJ', breaks = 40, ...) {
  range <- range(x)
  
  if (smooth) {
    densities <- tapply(x, groups, density, bw = bw, from = range[1], to = range[2], simplify = FALSE)
    
    X <- densities[[1]]$x
    Y <- lapply(densities, \(dens) dens$y)
    if (!conditional) Y <- Map(\(dens, prop) dens * prop, Y, prop.table(table(groups)))
    
    
  } else {
    
    coordinates <- multihist_coor(x, groups, conditional = conditional, breaks = breaks, vardim = 'X')
    X <- coordinates[[1]]$X
    Y <- lapply(coordinates, \(coor) coor$Density)
  }
  
  if (conditional) {
    margin <- Reduce('+', Y)
    Y <- lapply(Y, \(y) y / ifelse(margin == 0, 1, margin))
  } 
  
  # for density key
  densKey <- unique(2^(ceiling(log(max(unlist(Y)), 2)):floor(log(median(unlist(Y)), 2)))) |> head(4)
  
  Y <- do.call('cbind', Reduce('+', Y, accumulate = TRUE))
  Y <- cbind(axis = 0, Y)
  if (center && !conditional) Y <- sweep(Y, 1, rowMeans(Y), '-')  
  
  
  list(X = X, Y = Y, DensityKey = densKey)
  
  
}


xy_formula <- function(form) {
  lhs <- rlang::f_lhs(form)
  rhs <- rlang::f_rhs(form)
  env <- rlang::f_env(form)
  y <- rlang::eval_tidy(lhs, env = env)
  x <- rlang::eval_tidy(rhs, env = env)
  
  list(x = x, y = y, xlab = rlang::as_label(rhs), ylab = rlang::as_label(lhs))
}


### other ----




border <- function(side, scale = .8) {
  coor <- par('usr')
  coor <- c(grconvertX(c(0,1),'nfc','user'), grconvertY(c(0, 1), 'nfc', 'user'))
  
  x <- switch(as.character(side),
              "1" = , "3" = shrinklim(coor[1:2], scale),
              "2" = coor[c(1, 1)],
              "4" = coor[c(2, 2)])
  
  y <- switch(as.character(side),
              "1" = coor[c(3, 3)],
              "3" = coor[c(4, 4)],
              "2" = , "4" = shrinklim(coor[3:4], scale))
  
  graphics::segments(x0 = x[1], x1 = x[2],
                     y0 = y[1], y1 = y[2], col = setalpha('grey50', .35),
                     lwd = .5, lty = 'longdash', xpd = NA)
  
}





canvas <- function(x, xlim = NULL, y, ylim = NULL, log = '') {
  logcheck(log, x, y)
  
  xlim <- xlim %||% range(x) 
  ylim <- ylim %||% range(y) 
  
  
  xlog <- grepl('x', log, fixed = TRUE)
  ylog <- grepl('y', log, fixed = TRUE)
  
  if (xlog && xlim[1] <= 0) xlim[1] <- min(x) / 2
  if (ylog && ylim[1] <= 0) ylim[1] <- min(y) / 2
  
  axes <- data.table(side = 1:2,
                     ticks = list(axisTicks(if (xlog) log10(xlim) else xlim, log = xlog),
                                  axisTicks(if (ylog) log10(ylim) else ylim, log = ylog)),
                     line = 1L)
  
  window <- data.table(layout = 1L,
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  list(window = window, axes = axes, axisNames = vector('list', 4L), 
       canvas = function() {
         plot.window(xlim = xlim, ylim = ylim, log = log)
       })
}

smartjitter <- function(x) {
  .x <- x[!is.na(x)]
  
  ord <- order(.x)
  sorted <- .x[ord]
  
  range <- if (length(unique(sorted)) == 1) 1 else diff(range(sorted))
  diff <- c(range, diff(sorted))
  
  close <- diff == 0 
  if (!any(close)) return(x)
  smallest <- min(diff[!close], range / 10)
  
  shift <- (rbeta(sum(close), 3, 3) - .5) * smallest * .5
  
  
  sorted[close] <- sorted[close] + shift
  
  .x <- sorted[match(seq_along(sorted), ord)] # back to original order
  
  x[!is.na(x)] <- .x
  x
}




cutter <- function(value, reference, maxUnique = 4, Ncuts = 4) {
  
  # value is dimension that may need to be cut,
  # reference is another data vector to match length
  
  if (is.numeric(value) && length(col) == length(x)) {
    
    value <- if (length(unique(value)) <= 8) {
      as.factor(value) 
    } else {
      cut(value, breaks = Ncuts)
    }
  }
  rep(value, length.out = length(reference))
}
### argument preppers ----


#### prep_col ----

  


setGeneric('prep_col', 
           useAsDefault = function(col, var, pch, alpha, contrast, ncontinuous, log, marginLines, ...) rep(col, length.out = n), # if there is no method
           function(col, var, pch = 16, alpha = 1, contrast = FALSE, ncontinuous = 100, log = '', marginLines, ...) { 
             if (is.list(col) && names(col)[1] == 'col') col <- col$col
             
             checks(col, xlen1 | xmatch(var), seealso = c('?draw'))
             checks(alpha, xlen1 & xnumber & xrange(0, 1), seealso = c('?draw'))
             checks(contrast, xTF, seealso = c('?draw'))
             checks(ncontinuous, xlen1 & xnatural & xmin(2), seealso = c('?draw')) 
             
             pch <- if (length(unique(pch)) > 1) 16 else unique(pch)
             
             if (length(col) == 1L || any(isColor(as.character(col)))) return(list(col = setalpha(col, alpha)))
             if (length(unique(col)) == 1L) return(list(col = setalpha(flatly[1], alpha)))
             
             standardGeneric('prep_col')
             
           })


setMethod('prep_col', c('discrete'),
          function(col, var, pch = 16, alpha = 1, contrast = FALSE, ...) {
            categories <- sort(unique(col))
            if (is.integer(col) && length(categories) > 10L) return(prep_col(as.numeric(col), var, 
                                                                             alpha = alpha, pch, contrast = FALSE, log = log))
            
            palette <- flatly_scale(length(categories), alpha = alpha, contrast = contrast)
            col <- palette[match(col, categories)]
            
            list(col = col,
                 legend = \(side = 3, marginLines, col.legend = '')  legend_col_discrete(categories, palette, pch, col.legend = col.legend,
                                                                                         side = side, marginLines = marginLines))
          })

setMethod('prep_col', c('numeric'),
          function(col, var, pch = NULL, alpha = 1, ncontinuous = 100L, log = '', ...) {
            if (length(unique(col)) <= 10 || ncontinuous <= 10) return(prep_col(cut(col, breaks = min(ncontinuous, length(unique(col)))),
                                                                                var, alpha = alpha, contrast = FALSE))
            breaks <- seq(min(col), max(col), length.out = ncontinuous)
            
            palette <- flatly_scale(ncontinuous, alpha = alpha)
            cols <- palette[as.integer(cut(col, breaks = breaks, include.lowest = TRUE))]
            
            list(col = cols,
                 legend = \(side = 3, marginLines, col.legend = '') legend_col_continuous(col, palette, ..., col.legend = col.legend,
                                                                                          side = side, marginLines = marginLines))
          })



legend_col_discrete <- function(categories, palette, pch, side, marginLines, col.legend = '') {
  
  xpos <- grconvertX(marginLines[[side]][3:4], 'inches', 'user')
  
  y <- grconvertY(seq(.2, .8, along = categories), 'ndc', 'user')
  
  points(rep(xpos[2], length(y)), y, pch = pch, xpd = NA, col = palette, cex = 2)
  text(xpos[2], y, categories, cex = .6, xpd = NA, pos = 4)

  text(xpos[2], grconvertY(.81, 'ndc', 'user'), pos = 3, col.legend, col = par('col.lab'), xpd = NA)
}

legend_col_continuous <- function(var, palette, pch = NULL, smooth_legend = TRUE, side, marginLines, col.legend = '') {
  
  xpos <- grconvertX(marginLines[[side]][3:4], 'inches', 'user')
  col.labs <- pretty(var, min.n = 5, n = 10) |> format(big.mark = ',', digits = 2)
  
  ylabs <- grconvertY(seq(.2, .8, along = col.labs), 'ndc', 'user')
  ycols <- grconvertY(seq(.2, .8, along = palette), 'ndc', 'user')
  
  
  if (smooth_legend) {
    ydiff <- diff(ycols) / 2
    for(i in seq_along(ycols)) {
      polygon(c(xpos[1], xpos[2], xpos[2], xpos[1]),
              ycols[i] + c(ydiff[i], ydiff[i], -ydiff[max(1L, i - 1)], -ydiff[max(1L, i - 1)]),
              col = palette[i], xpd = NA, border = NA)
    } 
  } else {
    points(rep(xpos[1], length(ycols)), ycols, pch = pch, col = palette, xpd = TRUE)
  }
  
  text(xpos[2], ylabs, col.labs, pos = side, cex = .6, xpd = NA)
  
  text(xpos[2], grconvertY(.81, 'ndc', 'user'), pos = 3, col.legend, col = par('col.lab'), xpd = NA)
}


prep_col_categories <- function(col, categories, pch = 16, alpha = 1, contrast = FALSE, ...) {
  checks(col, xlen1 | xmatch(categories))
  checks(contrast, xTF, seealso = c('?draw'))
  checks(alpha, xlen1 & xnumber & xrange(0, 1), seealso = c('?draw'))
  
  col <- if (all(isColor(col))) {
    setalpha(col, alpha)
  } else {
    if (is.integer(col)) {
      flatly_scale(max(col), alpha = alpha, contrast = contrast)[col]
    } else {
      flatly_scale(length(categories), alpha = alpha, contrast = contrast)
    }
  }
  if (length(col) == 1L) return(list(col = rep(setalpha(col, alpha), length(categories))))
  
  list(col = col,
       legend = \(side = 3, marginLines, col.legend = '') legend_col_discrete(categories, col, pch, col.legend = col.legend,
                                                                              side = side, marginLines = marginLines))
}


#### prep_pch ----

prep_pch <- function(x, y, pch = NULL, col, ...) {
  size <- max(length(x), length(y))
  checks(pch, xnull | (xlen1 & (xwholenum & xrange(1, 16L))) | (xatomic & xlength(size)))
  
  col <- if (length(unique(col)) > 1) 'black' else unique(col)
  
  if (is.null(pch) || length(unique(pch)) == 1L) return(list(pch = 16))
  pch <- if (is.numeric(pch) & length(unique(pch)) > 4) cut(pch, breaks = 4) else factor(pch)
  if (length(unique(pch)) > 16) .stop("You can only draw at most 16 distinct groups using the pch (point type) argument.")
  categories <- levels(pch)
  pchfav <- c(16, 1, 3,2, 8, 13, 4, 5, 15, 6, 7, 9, 10, 11,14, 12)
  pch <- pchfav[as.integer(pch)]
  list(pch = pch,
       legend = \(side, marginLines) legend_pch_discrete(categories, pchfav[seq_along(categories)], side, marginLines, col))
}

legend_pch_discrete <- function(categories, pch, side, marginLines, col = 'black') {
  if (side == 3) {
    ypos <- grconvertY(marginLines[[side]][1:2], 'inches', 'user')
    xpos <- grconvertX(seq(.2, .8, along = categories), 'ndc', 'user')
    
    points(xpos, rep(ypos[2], length(xpos)), pch = pch, xpd = NA, cex = 1, col = col)
    text(xpos, ypos[2], categories, cex = .6, xpd = NA, pos = 3)
  } else {
    xpos <- grconvertX(marginLines[[side]][3:4], 'inches', 'user')
    ypos <- grconvertY(seq(.2, .8, along = categories), 'ndc', 'user')
    
    points(rep(xpos[2], length(ypos)), ypos, pch = pch, xpd = NA, cex = 1, col = col)
    text(xpos[2], ypos, categories, cex = .6, xpd = NA, pos = 4)
  }
  
  
  
  # text(xpos[2], grconvertY(.81, 'ndc', 'user'), pos = 3, col.legend, col = par('col.lab'), xpd = NA)
}

#### prep_cex ----

prep_cex <- function(x, y, cex = NULL, col, pch = 16, ...) {
  size <- max(length(x), length(y))
  checks(cex, xnull | (xpositive & (xlen1 | xlength(size))), seealso = '?draw')
  
  col <- if (length(unique(col)) > 1) 'black' else unique(col)
  
  output <- list(cex = cex)
  if (is.null(cex)) {
    output$cex <- cex_density(x, y)
    
  } else {
    if (length(cex) == size) {
      # val_legend <- 2^seq(log(min(cex), 2), log(max(cex), 2), length.out = 9) 
      val_legend <- if (length(unique(cex)) > 20) {
        2^pretty(log(range(cex), 2), min.n = 5, n = 10) # this must be calculated before cex is altered
      } else {
        sort(unique(cex))
        
      }
      
      logrange <- diff(range(log10(cex)))
      
      cex <- sqrt(cex) 
      if (logrange > 2) {
        maxPowers <- 6
        power <- ceiling(10^(logrange / maxPowers))
        .message("In draw(cex = ), your largest cex value is {round(10^logrange)} times greater than the smallest value.",
                 "To plot this, we must understate the differences between points.",
                 "When comparing the point in this plot, a doubling of area corresponds to multiplying the value",
                 "by {num2print(power)}.")
        cex <- 2^log(cex, power)
        cex_legend <- sqrt(2 ^ log(val_legend, power))
      } else {
        cex_legend <- sqrt(val_legend)
      }
      
      # scale to center on 
      scale <- exp(mean(log(cex))) 
      cex_legend <- cex_legend / scale
      cex <- cex / scale
      
      output$cex <- cex 
      output$legend <- \(side = 3, marginLines, cex.legend = '') legend_cex_continuous(val_legend, cex_legend, col, pch, 
                                                                                       cex.legend = cex.legend,
                                                                                       side = side, marginLines = marginLines)
    } 
  }
  
  output
  
}

cex_scale <- function(str, targetWidth, cex = .8) {
  mx <- max(strwidth(str, cex = cex))
  min(targetWidth / mx, 1) * cex
  
  
}

cex_density <- function(x, y, scalar = .225) {
  vars <- Filter(length, list(x, y))
  
  cuts <- lapply(vars, cut, breaks = if (length(vars) == 1L) 100 else 10)
  
  # forced to be between 1 and 250
  maxdensity <- min(floor(log10(max(do.call('table', cuts)))), 4)

  1 - (maxdensity * scalar)
  
}

legend_cex_continuous <- function(val, cex, col, pch, side, marginLines, cex.legend = '') {
  
  lab <- format(val, big.mark = ',', digits = 2)
  
  ypos <- grconvertY(seq(.2, .8, along = val), 'ndc', 'user')
  
  xpos <- grconvertX(marginLines[[side]][3:4], 'inches', 'user') 
  
  points(rep(xpos[2], length(ypos)), ypos, pch = pch, col = col[1], cex = cex, xpd = NA)
  
  text(xpos[2], ypos, lab, pos = side, cex = .6, xpd = NA)
  
  text(xpos[2], grconvertY(.81, 'ndc', 'user'), cex.legend, pos = 3, col = par('col.lab'), xpd = NA)
}
  
#### prep_layout ----

prep_layout <- function(facets) {
  
  if (length(facets) > 2) {
    facets[[2]] <- squashGroupby(facets[-1])
    facets <- facets[1:2]
  }
  facets <- unique(as.data.frame(facets))
  
  
  mat <- matrix(1:nrow(facets), nrow = length(unique(facets[[1]])))
  
  layout(mat)
  
  mat
}





# Notation viewer ----

toHNP <- function(lines, message, render = TRUE, header = 'HumdrumR viewer') {
  output <- paste(lines, collapse = '\n')
  
  randomID <- paste0(sample(letters, 100, replace = TRUE), collapse = '')
  message <- gsub("PLUGIN", '<a href="https://plugin.humdrum.org/">humdrum notation plugin</a>', message)
  header <- if (!is.null(header)) paste0('<h1>', header,'</h1>') else ''
  doctype <- if(render) '<!DOCTYPE html>' else '' 
  
  html <- .glue(.open = '[[', .close = ']]',
  '[[doctype]]
    <html lang="en">
    <head>
    <script src="https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"></script>
    <script>displayHumdrum({source: "[[randomID]]", autoResize: "true"});</script>
    </head>
    <body>
    [[header]]
    <p>[[message]]</p>
    <script id="[[randomID]]" type="text/x-humdrum">[[output]]</script>
    </body>
    </html>')
  
  if (render) {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, 'index.html')
    
    writeLines(strsplit(html, split = '\n')[[1]],  htmlFile)
    
    getOption('viewer', default = utils::browseURL)(htmlFile)
  } 

  invisible(html)
}

#' @export
viewKernTable <- function(table) {
  df <- as.data.frame(table)
  df <- df[order(df[[length(df)]], decreasing = TRUE), ]
  
  df <- subset(df, df[[length(df)]] > 0)
  
  
  
  kern <- lapply(as.list(df[1:(ncol(df) -1)]), as.character)
  # if (length(kern) > 1) {
    # kern[[1]] <- paste0('(', kern[[1]])
    # kern[[length(kern)]] <- paste0(kern[[length(kern)]], ')')
  # }
  N    <- num2str(df[[length(df)]])
  
  kernspine <- do.call('rbind', c(kern, list('=||')))
  kernspine <- c('**kern', kernspine, '*-')
  
  Nspine <- c(do.call('rbind', c(list(N), 
                                 replicate(length(kern) - 1, list('.'), simplify = T), 
                                 list('=||'))))
  Nspine <- c('**cdata', Nspine, '*-')
  
  lines <- paste(kernspine, Nspine, sep = '\t')
  
  toHNP(lines, "Tabulating kern data and viewing using the PLUGIN.")
}

# ggplot2 ----



#' @rdname withinHumdrum
#' @export
ggplot.humdrumR <- function(data = NULL, mapping = aes(), ..., dataTypes = 'D') {
  humtab <- getHumtab(data, dataTypes = dataTypes)
  
  ggplot(as.data.frame(data), mapping = mapping, ...) + theme_humdrum()
}




### Treatment of token ----

#' @export
scale_type.token <- function(x) if (class(x@.Data) %in% c('integer', 'numeric', 'integer64')) 'continuous' else 'discrete'


#' @export
scale_x_token <- function(..., expand = waiver(), guide = waiver(), position = "bottom") {
  sc <- ggplot2::discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                                # limits = c("c", "c#", "d-", "d", "d#", "e-", "e", "e#", "f", "f#", "f##", "g-", "g", "g#", "a-", "a", "a#", "b-", "b", "b#"),
                                expand = expand, guide = guide, position = position, super = ScaleDiscretePosition)
  
  sc$range_c <- scales::ContinuousRange$new()
  sc
}



### humdrumR plot style ----

#### Colors ----

scale_color_humdrum <- ggplot2::scale_fill_manual(values = flatly)
# scale_color_continuous(type = colorRamp(flatly[2:3]))

options(ggplot2.continuous.fill = ggplot2::scale_color_gradientn(colors = flatly_scale(100)))
options(ggplot2.continuous.color = ggplot2::scale_color_gradientn(colours = flatly_scale(100)))
options(ggplot2.continuous.colour = ggplot2::scale_color_gradientn(colours = flatly_scale(100)))

# options(ggplot2.continuous.colour = 'humdrum')

#### Theme ----


theme_humdrum <- function() {
  ggplot2::update_geom_defaults("point", list(size = .5, color = flatly[1], fill = flatly[2]))
  ggplot2::update_geom_defaults("line", list(size = .5, color = flatly[4], fill = flatly[3]))
  ggplot2::update_geom_defaults("rect", list(fill = flatly[1]))
  
  theme(panel.background = element_blank(), axis.ticks = element_blank(),
        strip.background = element_blank(), 
        # panel.border = element_rect(linetype = 'dashed', fill = NA),
        legend.key = element_rect(fill = NA),
        title = element_text(family = 'Helvetica', color = flatly[5], size = 16),
        plot.title.position = 'plot', plot.title = element_text(hjust = .5),
        line = element_line(color = flatly[1]),
        rect = element_rect(color = flatly[2]),
        text = element_text(family = 'Helvetica', color = flatly[4]),
        axis.text = element_text(color = flatly[5], size = 7),
        axis.title = element_text(color = flatly[4], size = 11)
        )
}


  


 

          
