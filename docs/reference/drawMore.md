# Add content to [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md) plots.

Add content to
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots.

## Usage

``` r
drawMore(drawPlot, ...)
```

## Arguments

- drawPlot:

  ***A plot to add to.***

  Must be a `plot_object`, created by
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md).

## Details

Use this function to add arbitrary content to a plot created by
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md).
The `...` arguments can be one or more expressions that are evaluated to
add to the existing plot. These expressions should make use of base-R
graphics functions like
[`mtext()`](https://rdrr.io/r/graphics/mtext.html),
[`abline()`](https://rdrr.io/r/graphics/abline.html),
[`points()`](https://rdrr.io/r/graphics/points.html),
[`graphics::arrows()`](https://rdrr.io/r/graphics/arrows.html), or
[`graphics::segments()`](https://rdrr.io/r/graphics/segments.html).
These expressions can reference variables from the original plot,
notably the `x` and/or `y` variables.

## Examples

``` r
x <- rnorm(200, mean = 10, sd = 10)

draw(x) |> drawMore(abline(v = mean(x)))
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
#> }, add = expression(abline(v = mean(x))), layout = list(layout = 1L, 
#>     layout_heights = 1, layout_widths = 1), aspect = 1.33333333333333)
#> <bytecode: 0x5bbea8011868>
#> <environment: 0x5bbeabf3dac8>
#> attr(,"add")
#> expression(abline(v = mean(x)))
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

draw( , y = x) |> drawMore(mtext(paste0('SD: ', sd(y) |> round(2)), side = 3, cex = 1.2),
                           abline(h = mean(y), col = 'red'))
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
#> }, add = expression(mtext(paste0("SD: ", round(sd(y), 2)), side = 3, 
#>     cex = 1.2), abline(h = mean(y), col = "red")), layout = list(
#>     layout = 1L, layout_heights = 1, layout_widths = 1), aspect = 1.33333333333333)
#> <bytecode: 0x5bbea8011868>
#> <environment: 0x5bbeac9dacf8>
#> attr(,"add")
#> expression(mtext(paste0("SD: ", round(sd(y), 2)), side = 3, cex = 1.2), 
#>     abline(h = mean(y), col = "red"))
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
