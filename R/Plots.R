



# draw() ----


#' Visualize data
#' 
#' The `draw()` function is humdrumR's go-to plotting function.
#' `draw()` will make a variety of graphs, depending on the type of data you give it.
#' For the most part, `draw()` is simply a stylish, easy to use wrapper around
#' the base-R graphics functions [plot()], [barplot()], and [hist()].
#' 
#' 
#' @details
#' 
#' `draw()` is a generic function, which does different plots depending on the data you pass to its
#' `x` and `y` arguments.
#' 
#' 
#' | `x`                                                      | `y`                                   | Plot type           |
#' |----------------------------------------------------------|---------------------------------------|---------------------|
#' | `numeric`                                                | (missing)                             | Histogram           |
#' | (missing)                                                | `numeric`                             | Quantile plot       |
#' |                                                          |                                       | (or Violin plot)    |
#' | `numeric`                                                | `numeric`                             | Scatter plot        |
#' | 1 or 2 dimensional [table][table()] or [distribution]    |                                       |                     |
#' | `character` or `factor`                                  | (additional `character`/`factor`)     | Barplot or Heatmap  |
#' | `character` or `factor`                                  | `numeric`                             | Violin plot         |
#' | `numeric`                                                | `character` or `factor`               | Area char           |
#' 
#' For purely categorical data, the default behavior is to draw barplots for 1D distributions, or
#' 2D distributions with 80 or fewer conditions, and heatmaps otherwise.
#' This default behavior can be overridden using either `heat = TRUE` or `heat = FALSE`.
#' Similarly, `violin = TRUE` can be used to force `draw( , y)` to draw a violin plot.
#' 
#' Note that, if you pass one or two `character`/`factor` vectors to `draw()`, it will pass these vectors to [count()],
#' then pass the resulting [distribution] to `draw()`, creating a barplot.
#' Thus, `draw(charvec1, charvec2)` is equivalent to `draw(count(charvec1, charvec2))`.
#' 
#' 
#' ### Drawing dimensions of data
#' 
#' `draw()` is equipped to visualize data in up to four dimensions in one plot.
#' The main dimensions are, of course, the X and Y axes, controlled by the `x` and `y` arguments.
#' The other two dimensions are color (`col`) and point-size (`cex`).
#' An another approach is to draw multiple plots at the same time in a grid, each sub
#' plot called a "facet."
#' Details for all these options can be found below.
#' 
#' @section General Arguments:
#' 
#' 
#' `draw()` is built on top of R's "base" plotting system.
#' This means that all the standard arguments to base-R plots can be used to customize plots 
#' (See [par()] for a full list) or add to them (for example, using [points()] or [mtext()]).
#' However, `draw()` has a number of special additional features, including easily plotting "facets" 
#' (dividing data into multiple plots).
#' Anything that `draw()` does can be done using normal base-R plotting functions ([plot()], [barplot()], etc.),
#' but `draw()` makes making good looking plots faster and easier.
#' 
#'  
#' ### Plot Text
#' 
#' Every `draw()` plot can have a title, subtitle, X-axis label, and Y-axis label.
#' 
#' The `draw()` function will automatically generate X and Y labels for every plot,
#' usually just using the expression you passed; for example, if you say `draw(rnorm(100))`, the
#' X label will be "rnorm(100)."
#' This can be overriden using the `xlab` and/or `ylab` arguments, which can be provided a single string
#' each---to surpress a label, provide an empty string, like `ylab = ""`.
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
#' specifiy `xlim = c(10, 50)`.
#' 
#' For `numeric` axes, you can also plot data on a logarithmic scale
#' using the `log` argument.
#' This set by providing a single `character` string containing lower-case
#' `"x"`, `"y"`, or both (`"xy"`).
#' Note that `draw()` will throw an error if you try to plot negative values 
#' on a logarithmic scale.
#' Note that some plots will not allow logarithmic scaling on some axes,
#' and will simply ignore attempts to do that.
#' 
#' ### Color
#' 
#' 
#' Colors can be specified in all `draw()` plots using the `col` argument, along with the 
#' `alpha` argument which controls the transparency of colors.
#' Note that color control can used for entirely aesthetic purposes (picking 
#' a color scheme you want) *or* to represent an additional dimension of data.
#'
#' Colors can be specified as either:
#' 
#' + Names (e.g., `"red"` or `"darkgreen'`)
#' + Hex codes (e.g., `"#ff0000"` or `"#00ff00`)
#' + Using the [rgb()] function.
#'   + If a single `col` value is provided, all points are drawn this color.
#'   + If the `col` value is the same length as `x` and `y`, a scale of colors (either discrete of continuous)
#'     is generated to match the values this variable takes, and a legend is drawn.
#' + Or as natural numbers, indexing `humdrumR`'s flatly palette,
#'   based on the colors `'#18BC9C'`, `'#3498DB'`, `'#F39C12'`, `'#E74C3C'`, and `'#2C3E50'`.
#'
#' The `alpha` argument must be a `numeric` value \eqn{1 \geq alpha \geq 0},
#' where `alpha = 0` is totally transparent and `alpha = 1` is totally opaque.
#'
#' It is always possible to specify a single color value for a plot.
#' However, `draw()` can also (generally) accept more color values, depending on the type of plot.
#' For some plots, multiple colors are used (aesthetically) by default;
#' For other types of plots, its possible to use color to represent an additional dimension of information.
#' 
#' + Scatter (`x = y = 'numeric'`) and Quantile plots (`y = 'numeric'`)
#'   + `col` may be an atomic vector the same length as `x`/`y`.
#'   + A color scheme is automatically computed to cover the range of values in `col`
#'     and a legend is added to the plot.
#'   + For example, if the `col` vector contains five unique values, each of these five values
#'     will be assigned a color in the plot.
#'     If the `col` values are continuous and numeric, the range of values will placed on a continuum
#'     of colors.
#' + Histograms (`x = 'numeric'`)
#'   + `col` may be an atomic vector the same length as `x`.
#'     + The histogram is divided into a separate histogram
#'       for each unique color value, they are plotted overlayed on top of each other,
#'       and a legend is added to the plot.
#'     + For example, if the `col` vector contains five unique values, five separate
#'       histograms will be overlayed on top of each other, each with their own color.
#'     + If the color argument contains is `numeric` with more than three unique values,
#'       these values will be divided into four ranges, each assigned a color.
#' + Barplots, Violinplots, and Area plots
#'   + `col` may be an atomic vector of the same length as the number of levels in the first 
#'     (X) dimension of the plotted distribution (or the Y dimension for area plots).
#'   + These colors are used to select the colors of each bar and a legend is added to the plot.
#'     Note that `draw()` will color levels by default (this currently can't be turned off).
#' + Heatmaps
#'   + Heatmaps use colors to represent values, and there is currently no control of this process.
#'     Using `col` will have no effect.
#'     
#' ### Point size
#' 
#' Point size in `draw()` scatter and quantile plots can be controlled using the `cex` argument.
#' Note that point-size control can used for entirely aesthetic purposes *or* to represent an 
#' additional dimension of data.
#' Generally, `draw()` automatically picks an aesthetic point size based on the number of data points,
#' and the size of the plotting window:
#' the more data points, the smaller the points.
#' 
#' If a single `cex` value is provided, all points are drawn the same size.
#' If the `cex` value is numeric and the same length as `x` and `y`, a scale of sizes is generated to match 
#' the range of values this variable takes, and a legend is added to the plot.
#' The generated scale matches the range of `cex` values to the *area* of drawn points.
#' If the range of values is too great, it is not feasible to represent them using points,
#' because the points would either get too small to see, or too big (covering the whole plot).
#' Thus, if the largest `cex` value is more than 100 times greater than the smallest,
#' the scaling will be changed to accomodate this.
#' When this happens, a message will be printed, explaining how the relative area of drawn
#' points relates to the relative magnitude of `cex` values.
#' For example, in the two calls below, the first will work with no message
#' but the second one will print the message shown:
#' 
#' ```
#' draw(rnorm(100)|>sort(),rnorm(100), cex=(1:100))
#' 
#' draw(rnorm(100)|>sort(),rnorm(100), cex=(1:100)^2)
#' # In draw(cex = ), your largest cex value is 10000 times greater than the smallest value.
#' # To plot this, we must understate the differences between points. 
#' # When comparing the point in this plot, a doubling of area corresponds to multiplying the value by three.
#' 
#' ```
#' 
#' ### Drawing descriptive/reference statistics
#' 
#' Most `draw()` plots have options for drawing (overlaying) additional useful information and the
#' data distribution. 
#' For example, the arithmetic mean of numeric distributions can be marked (with a black cross hair) by specifying
#' `mean = TRUE`.
#' (Only violin plots do this by default.)
#' For scatter plots, the cross hair mark is placed at the mean of both `x` and `y`.
#' 
#' All `draw()` plots (except heatmaps) also have an option to overlay lines marking data quantiles.
#' (Only quantile plots do this by default.)
#' To do this, specify your desired quantiles as a vector of numbers \eqn{1 \geq qs \geq 0},
#' providde to the `quantiles` argument.
#' For example, `draw(rnorm(100), quantiles = c(.05, .25, .5, .75 ,.95))`.
#' For scatter plots, quantiles are drawn for both `x` and `y` variables.
#' For violin plots, if `global_quantiles = TRUE`, the quantiles of the marginal `y`
#' distribution (ignoring the grouping by `x` categories) are drawn; however,
#' if `global_quantiles = FALSE` (the default), separate quantiles are drawn for each group.
#' 
#' Note that, when drawing [density()] objects, the mean and quantiles of the density estimate are
#' shown, not the raw data. 
#' 
#' Another option is to compare numeric distributions to a normal (Gaussian) distribution.
#' If `normalReference = TRUE`, a normal distribution, with mean and standard deviation estimated 
#' from the data, is overlayed on the plot as a dashed, black line.
#' If the data distribution is approximately normal, this line should approximate the data.
#' For scatter plots, the bivariate normal distribution of the two variables is estimated
#' (i.e., using their individual variances and their covariance).
#' A sample ten times the length of `x`/`y` (up to 10,000 at the most) is drawn
#' and these sample points are drawn as a transparent underlay of the actual scatter plot.
#' This conveys a sense of what the bivariate normal would look like, compared to the actual data.
#
#' A final option for scatter plots, is to automatically overline the simple linear regression line
#' between the two variables.
#' When `lm = TRUE`, the simple regression line is estimated using [lm(y ~ x)][lm()];
#' the regression line and 95% confidence limits on the regression line---estimated using [predict.lm()]---are
#' drawn (as solid lines and dashed lines respectively).
#' The regression coefficients are also drawn in a legend at the top left corner of the plot.
#' 
#'
#' 
#' ### Binning and Smoothing
#' 
#' Histograms, violin plots, and area plots, all involve drawing `numeric` values into
#' areas representing probability mass.
#' There are two basic approaches 1) density estimation and 2) binning.
#' The `draw()` function relies on the algorithms used by base-R's [density()] and [hist()] functions for 
#' these two tasks.
#' In many cases, we can pass arguments directly through to these functions.
#' For example, the `bw` and `kernel` arguments can be passed through to [density()],
#' or the `breaks` argument tp [hist()].
#' 
#' By default, `draw()` draws histograms using bins, while violin plots and area plots use density smoothing.
#' However, this can be overridden using the `smooth` argument: `smooth = TRUE` for density, and `smooth = FALSE` for binning.
#' If you create a violin plot with `smooth = FALSE`, the result is blocky, cubist violins.
#' 
#' #### Density vs counts
#' 
#' In histograms the Y-axis represents the probability density, which is not the same
#' as the probability mass in each bin, because it depends on the widths of the bins.
#' In some cases, density can even be greater than 1.
#' Using the density assures that the relative *area* of each bin matches the probability mass 
#' associated with that bin, even if the bins are of unequal width.
#' However, if the bins are of unequal width, the actual height of the bins won't actually match
#' the proportion of data.
#' 
#' 
#' ### Group proportions
#' 
#' Violin plots, area plots, and multi-color histograms draw probability mass grouped across categories.
#' A consideration is whether to draw this mass proportioned to the overall mass, or proportioned within each group.
#' These two possibilities can be set using the `conditional` argument: when `conditional = FALSE`,
#' each group's probability mass is drawn as a proportion of the total; when `conditional = TRUE`, each group's
#' probability mass is scaled up to the size of the group (i.e., so it should sum to 1).
#' For illustrate, consider the following data and associated drawings:
#' 
#' ```
#' X <- c(A = rnorm(1000, mean = 0), B = rnorm(5000, mean = 1), C = rnorm(2000, mean = 1))
#' Categories <- rep(c('A', 'B', 'C'), c(1000, 5000, 2000))
#' 
#' # violin plot
#' draw(Categories, X) 
#' draw(Categories, X, conditional = TRUE) 
#' 
#' # area plot
#' draw(X, Categories) 
#' draw(X, Categories, conditional = TRUE) 
#' 
#' # multi-color histogram
#' draw(X, col = Categories) 
#' draw(X, col = Categories, conditional = TRUE) 
#' 
#' ```
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
#' @param col ***What colors should be used in plots?***
#' 
#' See "Color" section below.
#' 
#' @param cex ***What size of points should be plotted?***
#' 
#' See "Point size" section below.
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
#' If `aspect` is `NULL` , R #' automatically uses the current aspect of 
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
#' @param heat ***Should a heatmap be drawn?***
#' 
#' Defaults to `FALSE`, unless the input has two or more dimensions
#' *and* at least 80 conditions.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param normalReference ***Should a reference Normal distribution by overlayed?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' @param showCounts ***Should the number of observations in each category be drawn on the plot?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param showPoints ***Should individual data points be plotted above the histogram?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param log ***Should X and/or Y axes be drawn on a logarithmic scale?***
#' 
#' Defaults to `""` (linear scale on both axes).
#' 
#' Must be a single `character` string; options are `"x"` (X axis on log scale), 
#' `"y"` (Y-axis on log scale), and `"xy"` (both axes on log scale).
#'  
#'  
#' @param smooth ***Should continuous values be grouped using density estimation or binning?***
#' 
#' Defaults to `TRUE` for violin and area plots, but `FALSE` for histograms.
#' 
#' Must be a singleton `logical` value: an on/off switch.
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
draw <- function(x, y, facets = list(), ..., 
                 xlab = NULL, ylab = NULL, 
                 axes = 1:4, legend = TRUE, aspect = NULL, margin = .2,
                 main = '', sub = '', col = 1, cex = NULL) {
  

  # this sets default par(...) values for for draw(), but these defaults can be overrode by ...
  oldpar <- par(family = 'Helvetica',  pch = 16,  col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2, pty = 'm')
  oldpar$mar <- oldpar$omi <- NULL
  

  marginLines <- setMargins(margin, aspect)
  
  
  do.call('par', list(...)[match(names(list(...)), names(par()), nomatch = 0L) > 0])
  oldpalette <- palette(flatly)
  on.exit({par(oldpar) ; palette(oldpalette)})
  
  checks(xlab, xnull | (xlen1 & xatomic))
  checks(ylab, xnull | (xlen1 & xatomic))
  checks(legend, xTF | (xcharacter & xminlength(1) & xmaxlength(2)))
  checks(axes, xwholenum & xmaxlength(4L) & xmax(4) & xmin(1))
  checks(main, xatomic & xlen1)
  checks(sub, xatomic & xlen1)
  

  # xlab and ylab
  xexpr <- deparse1(substitute(x)) 
  yexpr <- deparse1(substitute(y)) 
  if (xexpr == '') xexpr <- 'x'
  if (yexpr == '') yexpr <- 'y'
  
  # change missing to NULL
  x <- if (!missing(x)) x
  y <- if (!missing(y)) y
  
  if (rlang::is_formula(x)) {
    formula <- xy_formula(x)
    x <- formula$x
    y <- formula$y
    xlab <- xlab %||% formula$xlab
    ylab <- ylab %||% formula$ylab
  } 

  if (length(facets)) {
    if (!is.list(facets)) facets <- list(facets)
    
    output <- draw_facets(x, y, facets, xlab = xlab, ylab = ylab, ..., 
                col = col, cex = cex, marginLines = marginLines, 
                axes = axes, legend = legend,
                xexpr = xexpr, yexpr = yexpr)
  } else {
    output <- .draw(x, y, ..., col = col, cex = cex, marginLines = marginLines)
    
    output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
    output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  }
  
  output$marginLines <- marginLines
  
  # title and subtitle
  marginLab(marginLines, stringr::str_to_title(main), 3, 3, 
            col = par('col.main'), cex = par('cex.main'), font = 2)
  marginLab(marginLines, stringr::str_to_title(sub), 3, 2, 
            font = 2)
  
  
  humaxes(output$axes, output$axisNames, axes, marginLines)
  
 
  if (is.character(legend) || legend) {
    side <- 4
    if (is.logical(legend)) legend <- ''
    legend <- rep(legend, length.out = 2)
    if (!is.null(output$col$legend)) {
      output$col$legend(side = side, marginLines = marginLines, col.legend = legend[2])
      side <- side - 2
    }
    if (!is.null(output$cex$legend)) output$cex$legend(side = side, marginLines = marginLines, cex.legend = legend[1])
  } 
  
  return(invisible(output))
}
  
## the .draw() function ----

setGeneric('.draw', def =  \(x, y,  ...) standardGeneric('.draw'))


### draw() numeric ----

setMethod('.draw', c('numeric', 'numeric'), 
          \(x, y, log = '', jitter = '', 
            normalReference = FALSE, mean = FALSE, quantiles = c(), lm = FALSE,
            xlim = NULL, ylim = NULL, 
            col = 1, alpha = .5, cex = NULL, marginLines, ...) {
            
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
            
           
            
            if (grepl('x', jitter)) x <- smartjitter(x)
            if (grepl('y', jitter)) y <- smartjitter(y)
            
            if (normalReference) showmvnorm(x, y)
            
            points(x, y, col = output$col$col, cex = output$cex$cex, ...)
            
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
            

            output
            
          })

#### histogram ----

setMethod('.draw', c('numeric', 'NULL'), 
          \(x, y, log = '', jitter = '', 
            breaks = 'Sturges', normalReference = FALSE, 
            smooth = FALSE, conditional = FALSE, showCounts = TRUE, showPoints = TRUE,
            mean = FALSE, quantiles = c(), global_quantiles = TRUE,
            xlim = NULL, ylim = NULL,
            col = 3, alpha = .2, cex = .7, marginLines, ...) {
            
            cols <- prep_col(col, x, alpha = alpha, log = log, ncontinuous = 5, ...)
          
            breaks <- if (!smooth && length(breaks) == 1L && pmatch(breaks, 'quantiles', 0)) {
              quantile(x, c(0, if (length(quantiles)) sort(unique(quantiles)) else c(.25, .5, .75), 1))
            } else {
              hist.default(x, breaks = breaks, plot = FALSE)$breaks
            }
            names(breaks) <- format(breaks, digits = 3)
            
            col <- rep(cols$col, length.out = length(x))
            coordinates <- tapply(x, col, hist.coor, smooth = smooth, breaks = breaks, ..., simplify = FALSE)
            if (length(coordinates) > 1L && !conditional) {
              coordinates <- Map(\(coor, prop) {
                coor$Density <- coor$Density * prop
                coor
                }, coordinates, prop.table(table(col)))
            } 
            
            allDens <- unlist(lapply(coordinates, '[[', 'Density'))
            
            ylim <- ylim %||% c(0, 2^(ceiling(log( max(allDens), 2)))) # 1, .5, .25, .125, etc.
            output <- canvas(x = x, xlim = xlim %||%range(breaks), 
                             y =  allDens[allDens > 0], ylim = ylim, 
                             log = gsub('y', '', log))
            
            # actual plot of polygons
            ymin <- min(output$window$ylim[[1]])
            Map(\(coor, color) {
              coor[ , {
                polygon(c(X, rev(X)), c(Density, rep(ymin, length(Density))), col = color, border = NA)
                
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
                 coor[!duplicated(X) & Counts > 0, draw_counts(Mids, Density, Counts, col, min(Delta))]
               } else {
                 coor[ , draw_counts(X[which.max(Density)], max(Density), sum(col == color), col, diff(range(X)))]
               }
             }
            }, coordinates, names(coordinates))
            
            # extra stuff
            
            ## dots
            if (showPoints) {
              xsamp <- if (length(x) >= 10^5) sample(x, 10^5) else x
              ysamp <- runif(length(xsamp), max(allDens * 1.1, mean(output$window$ylim[[1]]) * 1.5), output$window$ylim[[1]][2])
              dotAlpha <- cex_density(xsamp, ysamp, .3)
              points(xsamp, ysamp,  cex = .3, col = setalpha(col, dotAlpha), pch = 16, xpd = TRUE)
            }
            
            if (global_quantiles || length(coordinates) == 1L) {
              draw_quantiles(1, x, quantiles, limits = grconvertY(c(.02, 1.01), 'nfc', 'user'))
            } else {
              lapply(unique(col), \(color) {
                draw_quantiles(1, x[col == color], quantiles, limits = grconvertY(c(.02, 1.01), 'nfc', 'user'), col = color)
              })
              
            }
            if (normalReference) {
              xpoints <- seq(output$window$xlim[[1]][1], output$window$xlim[[1]][2], length.out = 100)
              points(xpoints, dnorm(xpoints, mean(x), sd(x)), type = 'l',
                     lwd = .5, lty = 'dashed')
            }
            if (mean) draw_mean(x, grconvertY(0.02, 'nfc', 'user'))
            
            
            # prepare ticks
            ## x
            x.ticks <- breaks
            while(length(x.ticks) > 20L) {
              x.ticks <- x.ticks[seq(1, length(x.ticks), by = 2)]
            }
            output$axes[side == 1, ticks := x.ticks]
            
            ## y
            output$axes[side == 2, ticks := setNames(ticks[[1]], format(paste0(ticks[[1]] * 100, '%')))]
              
            #   
            # ## counts (side 4)
            # count.ticks <- unique(round(pretty(c(0, sum(histogram$counts) * output$axes[side == 2, ticks[[1]]]), n = 10L, min.n = 5L)))
            # count.ticks <- structure(count.ticks / sum(histogram$counts), names = format(count.ticks, big.mark = ','))
            # output$axes <- rbind(output$axes,
            #                      data.table(side = 4, ticks = list(count.ticks), line = 1))
        
            
            output$axisNames[[2]] <- 'Probability density'
            
            output$col <- cols
            output
            
          })



setMethod('.draw', c('NULL', 'numeric'),
          function(x, y, log = '', 
                   violin = FALSE, normalReference = FALSE,
                   mean = FALSE, quantiles = c(.25, .5, .75),
                   xlim = NULL, ylim = NULL, 
                   col = 1, alpha = .8, cex = NULL, pch = 16, 
                   marginLines, ...) {
            
            checks(violin, xTF)
            output <- canvas(x = if (violin) c(.5, 1.5) else c(0, 1), 
                             xlim = xlim, 
                             y = y, ylim = ylim , 
                             log = gsub('x', '', log))
            
            if (violin) {
              
              output$col <- prep_col(col, 1, alpha = alpha, log = log, ...)
              draw_violins(list(y), horiz = FALSE, mean = mean, ..., col = output$col$col, quantiles = quantiles)
              output$axisNames[[1]] <- 'Density'
              output$axes <- output$axes[side == 2L]
              
            } else {
              
              output$col <- prep_col(col, y, ..., alpha = alpha, pch = pch, log = log)
              output$cex <- prep_cex(x, y, cex = cex, col = output$col$col, log = log, ...)
              if (length(output$col$col) == length(y)) output$col$col <- output$col$col[order(y)]
               
              
              y <- sort(y)
              x <- seq(0, 1, length.out = length(y))
              points(x = x, y = y, col = output$col$col, cex = output$cex$cex, ...)
              
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
              
              output$axisNames[[1]] <- 'Quantile'
            }
           
            output
          })



### draw() discrete ----



setMethod('.draw', c('table', 'NULL'),
          function(x, y, log = '', 
                   beside = TRUE, heat = length(dim(x) == 2L) && length(x) > 80L,
                   ylim = NULL, marginLines, 
                   quantiles = c(), mean = FALSE, showCounts = FALSE,
                   col = NULL,  alpha = .9, ...) { 
            if (!is.numeric(c(x))) .stop("No draw() method for a matrix/table of class '{class(x[1, 1])}.'")
            dimnames(x) <- lapply(dimnames(x), \(dn) ifelse(is.na(dn), "NA", dn))
            
            # if table is one dimensional, add col dimension
            if (length(dim(x)) == 1L)  {
              dn <- dimnames(x)
              dim(x) <- c(dim(x), 1L)
              
              dimnames(x) <- c(dn, list(''))
            }
            
            
            if (heat) return(draw_heat(x, log = log, ...))
              
            if (dim(x)[1] == 1L) x <- t(x)
            
            type <- if (is.null(beside)) 'both' else { if (beside) 'beside' else 'stacked'}
            space <- if (type == 'stacked') .5 else c(0, 1 + nrow(x) %/% 8) 
            
            ylim <- ylim %||% c(0, if (is.null(beside) || type == 'beside') max(x) else max(colSums(x)))
            
            col <- prep_col_categories(col %||% rownames(x), rownames(x), alpha = alpha, log = log, ...)
            
            barx <- barplot(x, col = if (type == 'stacked' ) rev(col$col) else col$col, log = gsub('x', '', log), space = space,
                            axisnames = FALSE,
                            ylab = '', xlab = '',
                            beside = type != 'stacked', axes = FALSE, 
                            ylim = ylim,
                            border = rgb(.2,.2,.2,.2))
            
            if (type == 'both') {
              barplot(x[nrow(x):1, ], col = setalpha(rev(col$col), alpha / 4), border = rgb(.2,.2,.2, alpha / 3),
                      names.arg = logical(ncol(x)), axes = FALSE,
                      add = TRUE, beside = FALSE, space = nrow(x) + space[2] - 1)
            }
            
            
            # draw extra stuff
            draw_quantiles(2, x, quantiles = quantiles, limits = grconvertX(c(-.01, 1.01), 'nfc', 'user'))
            if (mean) draw_mean(colMeans(barx), colMeans(x))
            if (showCounts) draw_counts(barx, x, x, col =col$col,min(diff(x)))
            
            legend_col_discrete(rownames(x), col$col, pch = 15, side = 4, marginLines = marginLines)
            # axes
            proportions <- pretty(ylim / sum(x), n = 10L, min.n = 5L)
            proportions <- setNames(proportions * sum(x), proportions)
            
            axes <- data.table(side = c(2, 4),
                               ticks = list(proportions,
                                            unique(round(axTicks(2, log = grepl('y', log, fixed = TRUE))))),
                               line = 1L)
            
            if (ncol(x) > 1) axes <- rbind(axes,
                                           data.table(side = 1,
                                                      ticks = list(setNames(if (type == 'stacked') barx else colMeans(barx), colnames(x))),
                                                      line = 1 + as.integer(type != 'stacked')))
            if (type != 'stacked' && nrow(x) > 1L && length(x) < 100) axes <- rbind(axes,
                                                              data.table(side = c(1),
                                                                         ticks = list(setNames(c(barx), rownames(x)[row(barx)])),
                                                                         line = 1))
            
            
            window <- data.table(Screen = as.integer(screen()),
                                 xlim = list(c(0, ceiling(max(barx)))), 
                                 ylim = list(ylim),
                                 log = log)
            
            
            axisNames <- vector('list', 4L)
            axisNames[c(2,4)] <- c('Proportion', if (is.integer(x)) 'Count' else 'N')
            
            axisNames[1] <- paste(Filter(\(x) x != '', names(dimnames(x))), collapse = ' × ')
            
            list(axes = axes, window = window, axisNames = axisNames, col = col)
          })

setMethod('.draw', c('NULL', 'table'),
          function(x, y, ...) {
            .draw(t(y), NULL, ...)
          })


setMethod('.draw', c('count', 'NULL'),
          function(x, y, ...) {
            .draw(as.table(x), NULL, ...)
          })

setMethod('.draw', c('NULL', 'count'),
          function(x, y, ...) {
            .draw(as.table(x) |> t(), NULL, ...)
          })


setMethod('.draw', c('probability', 'NULL'),
          function(x, y, ...) {
            .draw(count(x), NULL, ...)
          })


setMethod('.draw', c('NULL', 'probability'),
          function(x, y, ...) {
            .draw(NULL, count(y), ...)
          })


setMethod('.draw', c('humdrumR.table', 'NULL'),
          function(x, y, ...) {
            class(x) <- class(x)[-1]
            .draw(x, NULL, ...)
          })

setMethod('.draw', c('discrete', 'NULL'),
          function(x, y, ...){ 
            .draw(table(x, deparse.level = 2L), NULL, ...)
            })


setMethod('.draw', c('NULL', 'discrete'),
          function(x, y, ...){ 
            .draw(table(y, deparse.level = 2L) |> t(), NULL, ...)
          })
setMethod('.draw', c('discrete', 'discrete'),
          function(x, y, ...){ 
            .draw(table(x, y, deparse.level = 2L), NULL, ...)
          })





# setMethod('.draw', c('discrete', 'numeric'),
#           function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
#             .draw(list(1, factor(x)), y, col = col, log = log, breaks = breaks, ..., yat = yat)
#             list(xlab = NULL, ylab = NULL)
#           })

setMethod('.draw', c('list', 'numeric'),
          function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
            
            layout <- prep_layout(x)
            oldpar <- par(oma = par('mar'), mar = c(0, 0, 0, 0))
            on.exit({
              layout(cbind(1)) 
              par(oldpar)
              
            })
            
            y.ticks <- auto_ticks(y, log = grepl('y', log), at = yat)
            ylim <- range(y.ticks)
            y <- split(y, f = x)
            
            x.ticks <- seq(0, 1, .1)
            x.labels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
            
            xuniq <- unique(as.data.frame(x))
            xuniq <- xuniq[sapply(xuniq, \(val) length(unique(val)) > 1L)]
            grouplabels <- do.call('paste', xuniq)
            for (k in c(layout)) {
              ytick <- if (k %in% layout[, 1]) y.ticks 
              if (k %in% layout[nrow(layout), ]) {
                xtick <- x.ticks 
                xlabel <- x.labels
              } else {
                xtick <- xlabel <- NULL
              }
              
              canvas(log = gsub('x', '', log),  xlim = c(0, 1),  ylim = ylim)
              
              if (length(layout) > 1L) text(0.2, ylim[1] + (diff(ylim) * .75), grouplabels[k])
              draw_violin(y[[k]], breaks = breaks)
            }
            
            
            list(oma = TRUE, xlab = if (length(layout) == 1L) 'Proportion' else "", ylab = "")
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


### draw() numeric ~ discrete ----


setMethod('.draw', c('discrete', 'numeric'),
          function(x, y, log = '', 
                   smooth = TRUE, conditional = FALSE,
                   mean = TRUE, quantiles = c(.25, .75), global_quantiles = FALSE, 
                   xlim = NULL, ylim = NULL, ...,
                   col = NULL) {
            
            categories <- sort(unique(x))
            if (is.integer(x) && length(categories) > 25L) {
              return(.draw(as.numeric(x), y, log = log, 
                           xlim = xlim, ylim = ylim, ..., quantiles = quantiles))
            }
            
            xlim <- xlim %||% c(.5, length(categories) + .5)
            output <- canvas(x = seq_along(categories), xlim = xlim, 
                             y = y, ylim = ylim , 
                             log = gsub('y', '', log))
            
            output$col <- prep_col_categories(col %||% categories, categories, log = log, ...)
            
            draw_violins(tapply(y, x, list), smooth = smooth, conditional = conditional, col = output$col$col, ...,
                         mean = mean, quantiles = quantiles, global_quantiles = global_quantiles)
            
            # output$axisNames[[1]] <- 'Density'
            output$axes[side == 1, ticks := list(setNames(seq_along(categories), categories))]
            output
            
          })



### draw() discrete ~ numeric ----


setMethod('.draw', c('numeric', 'discrete'),
          function(x, y, log = '', 
                   center = TRUE, conditional = FALSE, breaks = 40,
                   mean = TRUE, quantiles = c(.25, .75),
                   xlim = NULL, ylim = NULL, 
                   col = NULL, alpha = .7, ...) {
            
            categories <- sort(unique(y), decreasing = TRUE)
            
            breaks <- hist.default(x, breaks = breaks)$breaks #seq(min(x), max(x), length.out = n)
            xcuts <- cut(x, breaks = breaks, include.lowest = TRUE)
            tab <- table(xcuts, factor(y, levels = categories))
            tab <- cbind(0, tab)
            
            if (conditional) tab <- proportions(tab, margin = 1)
            tab[is.na(tab)] <- 0
            tab <- do.call('cbind', Reduce('+', accumulate = TRUE, lapply(1:ncol(tab), \(j) tab[, j])))
            if (center) tab <- sweep(tab, 1, rowMeans(tab), '-')
            
            output <- canvas(x, xlim, range(tab), ylim, log = gsub('y', '', log))
            output$col <- prep_col_categories(col %||% categories, categories, alpha = alpha, log = log, ...)
            
            if (center) output$axes[ , ticks := lapply(ticks, \(t) {names(t) <- abs(t) ; t})]
            
            for (j in 1:(ncol(tab) - 1L)) {
              polygon(c(breaks, rev(breaks)), 
                      c(0, tab[ , j], rev(tab[ , j + 1]), 0), 
                      col = output$col$col[j],
                      border = FALSE)
            }
            
            
            output
            
          })

## draw_facets ----


draw_facets <- function(x = NULL, y = NULL, facets,  ..., xexpr = '', yexpr = '', 
                        xlab = NULL, ylab = NULL,
                        axes = 1:4, legend = TRUE,
                        col = 1, cex = NULL,
                        main = '', sub = '') {
  
  if (length(facets) > 2L) .stop("The draw() functon can't handle more than two faceting variables.",
                                 "You have provided {num2print(length(facet))}.")
  
  vecsize <- max(length(x), length(y))
  if (!all(lengths(facets) == vecsize)) {
    .stop('Facets variables must be vectors of the same length as the x/y plotting variables.')
  }
  

  
  
  # determine overall xlim ylim etc (output)
  output <- .draw(x, y, ..., col = col, cex = cex)
  
  output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
  output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  
  args <- list(x = x, y = y,  log = output$window$log,
               ..., col = output$col$col, cex = output$cex$cex,
               xlim = output$window$xlim[[1]], ylim = output$window$ylim[[1]])
  args <- args[!duplicated(names(args))]
  
  
  # Determine layout
  table <- do.call('table', facets)
  lay <- array(seq_along(table), dim = dim(table))
  layout(lay)
  
  
  if (length(facets) == 1L) {
    left.side   <- right.side <- table > 0L
    top.side    <- seq_along(table) == 1L
    bottom.side <- seq_along(table) == length(table)
  } else {
    .table <- table > 0
    left.side   <- leftmost(.table)
    right.side  <- rightmost(.table)
    top.side    <- topmost(.table)
    bottom.side <- bottommost(.table)
    
  }
               
  
  # mar <- c(outside = 5, inside = .5)
  mar <- 1
  facetX <- facetY <- c()
  # plot each screen
  for (n in lay) {
  
    
      cur <- lay == n
      curlevels <- Map('[', dimnames(table), which(cur, arr.ind = TRUE))
      
      # set margins
      par(mar = c(mar, mar, mar, mar))
      
      # prepare args and draw
      if (table[cur] > 0) {
        facet_ind <- Reduce('&', Map('==', curlevels, facets))
        curargs <- lapply(args, \(arg) if (length(arg) == vecsize) arg[facet_ind] else arg)
        
        do.call('.draw', curargs) # actual draw of plot
        
        sides <- c(bottom.side[cur], left.side[cur], top.side[cur], right.side[cur])
        lapply(which(!sides), border)
        
        # # axes 
        curaxes <- intersect(which(sides), axes)
        
        marginLines <- facetMargins()
        humaxes(output$axes,
                ifelse(1:4 %in% curaxes, output$axisNames, vector('list', 4L)),
                curaxes, marginLines)
      } else {
        # empty facet
        plot.new() # only needed for writing facet levels in margins
      }
      
      #coordinates of facets-
      if (sides[1]) facetX <- c(facetX, grconvertX(.5, 'nfc', 'ndc'))
      if (sides[2]) facetY <- c(facetY, grconvertY(.5, 'nfc', 'ndc'))
      
      
  }
  
  
  # reset layout
  layout(1)
  par(mar = c(0, 0, 0, 0))
  
  # axes for facet(s)
  output$axisNames[2] <-  list(if (.names(facets)[1] != '') names(facets)[1])
  axes <- data.table(side = 2L, 
                     ticks = list(sort(setNames(grconvertY(unique(facetY), 'ndc', 'user'), 
                                                dimnames(table)[[1]]))),
                     line = 2L)
  output$axisNames[1] <- list(if (length(dim(table)) > 1L) {
    axes <- rbind(axes, 
                  data.table(side = 1L, 
                             ticks = list(sort(setNames(grconvertX(unique(facetX), 'ndc', 'user'),
                                                        dimnames(table)[[2]]))),
                             line = 2L))
    if (.names(facets)[2] != '') names(facets)[2]
  } )
  output$axes <- axes
  
  output
}

## draw_x ----


draw_quantiles <- function(side, var, quantiles = c(.025, .25, .5, .75, .975), limits = NULL, col = 'black', ...) {
  
  if (length(quantiles)) {
    quantiles <- unique(quantiles)
    checks(quantiles, xnumeric & xrange(0, 1))
    
    sides <- side %% 2 == 0
    quants <- quantile(var, prob = quantiles)
    
    
    if (is.null(limits)) limits <- if (sides) grconvertX(c(0, 1), 'nfc', 'user') else grconvertY(c(0, 1), 'nfc', 'user')
  
    
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
                    lwd = .3, col = setalpha(col, .5))
   names(lineArgs)[1:4] <- if (sides) {
     c('x0', 'x1', 'y0', 'y1')
   } else {
     c('y0', 'y1', 'x0', 'x1')
   }
   do.call(graphics::segments, lineArgs)
    
    
  }
  
}




draw_violins <- function(vars, smooth = TRUE, conditional = FALSE, 
                         mean = TRUE, quantiles = c(), global_quantiles = FALSE, 
                         breaks = "Sturges", kernel = 'gaussian', bw = 'nrd0', ...,
                         col = 1) {
  vars <- lapply(vars, \(v) v[!is.na(v)])
  
  if (length(breaks) == 1L && pmatch(breaks, 'quantiles', 0) == 1 && length(quantiles)) {
    breaks <- quantile(unlist(vars), sort(unique(c(0, quantiles, 1))))
    names(breaks) <- format(breaks, digits = 3)
  }  
  
  if (!smooth) x <- hist.default(unlist(vars), breaks = breaks, plot = FALSE)$breaks
  
  
  lapply(seq_along(vars), 
         \(i) {
           var <- vars[[i]]
           if (smooth) {
             dens <- density(var, bw = bw, kernel = kernel)
             x <- dens$x
             y <- dens$y
             dx <- mean(diff(x))
             y <- y * dx
             
             x <- c(x[1], x,x[length(x)])
             y <- c(0, y, 0)
           } else {
             hist <- hist.default(var, breaks = x, plot = FALSE)
             y <- hist$density
             dx <- mean(diff(x))
             y <- y * dx
             
             x <- c(x[1], x[1], rep(x[-1], each = 2))
             y <- c(0, rep(y, each = 2), 0)
             # x <- c(x[1], x, x[length(x)])
             # y <- c(0, y, 0)
           }
           
           data.table(I = i, X = x, Y = y)
           
         }) |> data.table::rbindlist() -> Coor
  
  varprop <- proportions(lengths(vars))
  
  if (conditional) {
    Coor[, Yscale := 2^(ceiling(log(max(Y), 2))), by = I]
    
  } else {
    Coor[ , Y := Y * varprop[I]]
    Coor[ , Yscale := 2^(ceiling(log(max(Y), 2)))]
  }
  
  Coor[ , Y := Y / Yscale]
  
  for (i in seq_along(vars)) {
    Coor[I == i, {

      polygon(I + Y * .5, X, border = NA, col = col[i])
      polygon(I - Y * .5, X, border = NA, col = col[i])

      if (mean) points(i, mean(vars[[i]]), pch = 3, cex = 1.4, lwd = 1.5, col = 'black')
      
      minx <- par('usr')[3]
      arrows(i - .5, x1 = i + .5, minx, minx, 
             code = 3, length = .1, angle = 90, lty = 'dashed')
      text(i, minx, unique(Yscale), pos = 3, cex = .5, xpd = TRUE, col = 'black')

      
    }]
    if (!global_quantiles) draw_quantiles(2, vars[[i]], quantiles, limits = c(i - .5, i + .5))
  }
      
  if (global_quantiles) draw_quantiles(2, unlist(vars), quantiles)
  
  
}



draw_heat <- function(tab, log = '', cex = NULL, ...) {
  # cex isn't used obviously, but it gets passed in ... above, causing warnings below
  xlim <- c(0L, ncol(tab))
  ylim <- c(0L, nrow(tab))
  
  plot.new()
  plot.window(xlim, ylim, log = log)
  
  col <- prep_col(c(tab), c(tab), log = log, ..., pch = NULL)
  colarray <- array(col$col, dim = dim(tab))
  
  Map(\(i, j, c) {
    polygon(c(i, i, i - 1, i - 1), 
            c(j, j - 1, j - 1, j), 
            col = c,
            border = rgb(.1, .1, .1, .1), lwd = .3)
    
  }, col(tab), nrow(tab) + 1L - row(tab), colarray)
  
  axes <- data.table(side = 1:2,
                     ticks = list(setNames(1:ncol(tab) - .5, colnames(tab)),
                                  setNames(1:nrow(tab) - .5, rev(rownames(tab)))),
                     line = 1)
  
  window <- data.table(Screen = as.integer(screen()),
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  axisNames <-  vector('list', 4L)
  if (names(dimnames(tab))[1] != '') axisNames[[1]] <- names(dimnames(tab))[1]
  if (names(dimnames(tab))[2] != '') axisNames[[2]] <- names(dimnames(tab))[2]
  list(window = window, axes = axes, axisNames = axisNames, col = col)
}



## draw()'s helpers ----


### creating stable margins ----

setMargins <- function(margin.percent = .2, aspect = NULL) {
  
  checks(margin.percent, xlen1 & xnumeric & xmin(.1) & xmax(.4), argname = 'margin', seealso = '?draw()')
  checks(aspect, xnull | (xlen1 & xnumeric & xmin(.2) & xmax(5)), seealso = '?draw()')
  

  devsize <- par('din')
  
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
  
  par(omi = fullmar[c(2, 1, 2, 1)], mar = c(0, 0, 0, 0))
  
  # scale cex to size of device
  # xarea <- prod(devsize)
  xarea <- prod(figsize)
  
  magic <- .1488 # this seems to be the linear slope between cex and strheight('M')
  unity <- 36 # arbitrary. For example, a 6in x 8in plot
  
  cex <- sqrt(xarea /  (unity / (magic^2))) / magic
  par(cex = cex)
  
  # everything is currently inches
  lines <- c(0, .3, .6, .75, 1) * min(figmar)
  
  marginLines <- list(grconvertY(0, 'nfc', 'inches') - lines,
                      grconvertX(0, 'nfc', 'inches') - lines,
                      grconvertY(1, 'nfc', 'inches') + lines,
                      grconvertX(1, 'nfc', 'inches') + lines) 
  
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
    x <- grconvertX(.5, 'nfc', 'user')
    y <- grconvertY(marginLine, 'inches', 'user')
  } else {
    x <- grconvertX(marginLine, 'inches', 'user')
    y <- grconvertY(.5, 'nfc', 'user')
  }
  
  srt <- switch(las + 1,
                c(0, 90, 0, 270)[side],
                0,
                c(90, 0, 90, 0)[side],
                90)
  text(x, y, text, srt = srt, xpd = NA, adj = c(.5, if (side == 1) 0 else 1), offset = 0, ...)
  
}


### other ----



showmvnorm <- function(x, y) {
  
  sigma <- matrix(cov(x, y), nrow = 2, ncol = 2)
  sigma[1, 1] <- var(x)
  sigma[2, 2] <- var(y)
  
  
  sample <- MASS::mvrnorm(min(10000, length(x) * 10), c(mean(x), mean(y)), sigma)
  
  points(sample[,1], sample[,2], pch = 16, pch = 8, col = rgb(0,0,0, alpha=.02), xpd = TRUE)
  
# 
#   
#   xbr <- seq(min(x), max(x), length.out = nblocks)
#   ybr <- seq(min(y), max(y), length.out = nblocks)
#   
#   tab <- table(x = cut(sample[,1], xbr), y = cut(sample[,2], ybr))
#   
#   xopen <- strsplit(rownames(tab), split = ',') |> do.call(what = 'rbind')
#   xopen[] <- gsub('[^-0-9.]', '', xopen)
#   xopen <- array(as.numeric(xopen), dim = dim(xopen))
#   
#   yopen <- strsplit(colnames(tab), split = ',') |> do.call(what = 'rbind')
#   yopen[] <- gsub('[^-0-9.]', '', yopen)
#   yopen <- array(as.numeric(yopen), dim = dim(yopen))
#   for (i in 1:nrow(tab)) {
#     for(j in 1:ncol(tab)) {
#       if (tab[i,j] == 0) next
#       polygon(x = c(xopen[i, 1:2], xopen[i, 2:1]),
#               y = c(yopen[j, c(1,1)], yopen[j, c(2,2)]),
#               border = NA,
#               col = rgb(0,0,0, alpha = tab[i,j] / max(tab)))
#     }
#   }
  
}


outerInches <- function(inches, side) {
  
  grconvert <- if (side %in% c(1, 3)) grconvertY else grconvertX
  edge <- if (side %in% c(1, 2)) -1 else 1
  
  edgeinches <- grconvert(max(edge, 0), 'nic', 'inches')
  
  
  inches <- edgeinches + inches * edge
  
  grconvert(inches, 'inches', 'user')
  
  
}

line2user <- function(line, side, outer = FALSE) {
  # gets coordinates of axis lines
  if (outer) line <- line + par('mar')[side]
  
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call. = FALSE))
}

shrinklim <- function(lim, scale = .8) {
  ((lim - mean(lim)) * scale) + mean(lim)
}

border <- function(side, scale = .8) {
  coor <- par('usr')
  
  
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
                     lwd = .5, lty = 'longdash')
  
}

xy_formula <- function(form) {
  lhs <- rlang::f_lhs(form)
  rhs <- rlang::f_rhs(form)
  env <- rlang::f_env(form)
  y <- rlang::eval_tidy(lhs, env = env)
  x <- rlang::eval_tidy(rhs, env = env)
  
  list(x = x, y = y, xlab = rlang::as_label(rhs), ylab = rlang::as_label(lhs))
}


setalpha <- function(col, alpha = 1) {
  rgba <- col2rgb(col, alpha = TRUE) / 255
  
  rgb(rgba['red', ], rgba['green', ], rgba['blue', ], alpha)
}



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

humaxis <- function(side, ticks, line = 1, lab = 0, cex = par('cex.axis'), marginLines) {
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
         labels, cex = cex, xpd = NA, col = par('col.axis'))
  } else {
    marginLine <- grconvertY(marginLine, 'inches', 'user')
    text(ticks, marginLine, pos = side,
         labels, cex = cex, xpd = NA, col = par('col.axis'))
    
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

canvas <- function(x, xlim = NULL, y, ylim = NULL, log = '') {
 logcheck(log, x, y)
  
  xlim <- xlim %||% range(x) 
  ylim <- ylim %||% range(y) 
  
  if (grepl('x', log, fixed = TRUE) && xlim[1] <= 0) xlim[1] <- min(x) / 2
  if (grepl('y', log, fixed = TRUE) && ylim[1] <= 0) ylim[1] <- min(y) / 2
  
  plot.new()
  plot.window(xlim = xlim, ylim = ylim, log = log)
  
  axes <- data.table(side = 1:2,
                     ticks = list(axTicks(1, log = grepl('x', log)),
                                  axTicks(2, log = grepl('y', log))),
                     line = 1L)
  
  window <- data.table(Screen = as.integer(screen()),
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  list(window = window, axes = axes, axisNames = vector('list', 4L))
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


# lines
# 0 -> quantile labels
# 1 -> axis labels
# 2 -> second axis ticks
# 3 -> xaxis labels
# 4 -> legend / sub title
# 5 -> title

drawlines <- function(n = 10, outer = FALSE) {
  for (side in 1:4) {
    for (line in 0:n) {
      mtext(paste0('___', line, '___'), side = side, line = line, outer = outer)
    }
  }
}

draw_mean <- function(x, y) {
  points(x, y, pch = 3, cex = 1.4, lwd = 1.5, col = 'black', xpd = TRUE)
}

draw_counts <- function(x, y, counts, col, width, cex = .8) {
  counts <- prettyN(counts, expr = TRUE)
  
  text(x, y, counts, xpd = NA,
       cex = cex_scale(counts, targetWidth = width * .8, cex = cex), 
       col = setalpha(col, 1), pos = 3)
}


axis.lines <- function() {
  cexs <- par(c('cex.axis', 'cex.lab', 'cex.sub'))
  
  lines <- cumsum(c(0.5, unlist(cexs))) 
  names(lines) <- c(names(lines)[-1], 'mar')
  lines <- as.list(lines)
  
  par(mar = rep(lines$mar, 4))
  plot(1:10, type='n', axes= FALSE, xlab='', ylab='')
  box()
  mtext(1:10, 1, at = 1:10, line = lines$cex.axis, cex = cexs$cex.axis, padj = 1)
  mtext(1:10, 2, at = 1:10, line = lines$cex.axis, cex = cexs$cex.axis)
  mtext('Y', side = 2, line = lines$cex.lab, cex = cexs$cex.lab)
  mtext('X', side = 1, line = lines$cex.lab, cex = cexs$cex.lab, padj = 1)
  
  mtext('Main', line = lines$cex.sub, cex = cexs$cex.sub)
  mtext('Sub', side = 1, line = lines$cex.sub, cex = cexs$cex.sub, padj=1)
lines
}


hist.coor <- function(x, smooth = FALSE, breaks = "Sturges", ..., groups = NULL) {
  # gets x/density/counts for a numeric distribution, using either density() or hist()
  # but returning the same format either way
  if (smooth) {
    dens <- stats::density.default(x, ...)
    output <- data.table(X = dens$x, Density = dens$y)
  } else {
    hist <- graphics::hist.default(x, breaks = breaks, ..., plot = FALSE)
    
    output <- data.table(Density = hist$density, Counts = hist$counts, Mids = hist$mids, 
                         Delta = diff(hist$breaks))
    output <- output[rep(1:nrow(output), each = 2)]
    i <- c(1, rep(2:(length(hist$breaks) - 1), each = 2), length(hist$breaks))
    output[ , X := hist$breaks[i]]
    
  }
  output[]
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


prep_col_categories <- function(col, categories, pch = 16, alpha = 1, contrast = FALSE, log = '', ...) {
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
  
  list(col = col,
       legend = \(side = 3, marginLines, col.legend = '') legend_col_discrete(categories, col, pch, col.legend = col.legend,
                                                                      side = side, marginLines = marginLines))
}

setGeneric('prep_col', 
           useAsDefault = function(col, var, pch, alpha, contrast, ncontinuous, log, marginLines, ...) rep(col, length.out = n), # if there is no method
           function(col, var, pch = 16, alpha = 1, contrast = FALSE, ncontinuous = 100, log = '', marginLines, ...) { 
             if (is.list(col) && names(col)[1] == 'col') col <- col$col
             
             checks(col, xlen1 | xmatch(var), seealso = c('?draw'))
             checks(contrast, xTF, seealso = c('?draw'))
             checks(alpha, xlen1 & xnumber & xrange(0, 1), seealso = c('?draw'))
             checks(ncontinuous, xlen1 & xnatural & xmin(2), seealso = c('?draw')) 
             
             if (length(col) == 1L || any(isColor(as.character(col)))) return(list(col = setalpha(col, alpha)))
             
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
            if (length(unique(col)) < 6) return(prep_col(factor(col),
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


#### prep_cex ----



prep_cex <- function(x, y, cex = NULL, col, pch = 16, ...) {
  size <- max(length(x), length(y))
  checks(cex, xnull | (xpositive & (xlen1 | xlength(size))), seealso = '?draw')
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
      
      cex <- sqrt(cex) 
      
      maxPowers <- 6
      logrange <- diff(range(log(cex)))
      power <- max(2, ceiling(exp(logrange / maxPowers)))
      
      if (power > 2) {
        .message("In draw(cex = ), your largest cex value is {round(exp(logrange)^2)} times greater than the smallest value.",
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

toHNP <- function(lines, message, render = TRUE) {
  output <- paste(lines, collapse = '\n')
  
  randomID <- paste0(sample(letters, 100, replace = TRUE), collapse = '')
  message <- gsub("PLUGIN", '<a href="https://plugin.humdrum.org/">humdrum notation plugin</a>', message)
  
  html <- .glue(.open = '[[', .close = ']]',
  '<!DOCTYPE html>
    <html lang="en">
    <head>
    <script src="https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"></script>
    <script>displayHumdrum({source: "[[randomID]]", autoResize: "true"});</script>
    </head>
    <body>
    <h1>HumdrumR viewer</h1>
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
  } else {
    cat(html, sep = '\n')
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


  


 

          
