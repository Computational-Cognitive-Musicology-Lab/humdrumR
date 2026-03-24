# Draw multiple plots next to each other.

Draw multiple plots next to each other.

## Usage

``` r
drawBeside(..., widths = NULL)

drawBelow(..., heights = NULL)

drawNothing()
```

## Arguments

- ...:

  ***Plots to combine.***

  Must be `plot_object`s, created by
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md).

- widths:

  ***How should horizontal space be divided up between plots?***

  Defaults to `NULL`, leading to equal spacing.

- heights:

  ***How should vertical space be divided up between plots?***

  Defaults to `NULL`, leading to equal spacing.

## Details

Use this function to draw multiple, aribitrary
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots. The `...` arguments can be one or more
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots, which are placed beside (or below) each other.

The `heights` or `widths` arguments can be used control the relative
height (`drawBelow`) or width (`drawBeside`) of the plots.

`drawNothing()` can be used to put an empty space in a layout.

## See also

Combines plots created with
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md).
Use
[`drawMore()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMore.md)
to *add* to existing plots.

## Examples

``` r
x1 <- rnorm(200, mean = 10, sd = 10)
x2 <- rnorm(200, mean = 10, sd = 10)

draw(x1) |> drawBeside(x2)
#> Error: Failed to parse glue component
#> Caused by error in `parse()`:
#> ! <text>:1:11: unexpected symbol
#> 1:    NULL   base
#>               ^
draw(x1) |> drawBelow(x2)
#> Error: Failed to parse glue component
#> Caused by error in `parse()`:
#> ! <text>:1:11: unexpected symbol
#> 1:    NULL   base
#>               ^

draw(x1, title = 'Histogram of x1') |> drawBeside(draw( ,x1, title = 'Q-plot of x1'))
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = 1:2, layout_widths = c(1, 
#>     1), layout_heights = c(1, 1)), aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d479249a8>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2]
#> [1,]    1    2
#> 
#> attr(,"layout")$layout_widths
#> [1] 1 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

draw(x1, title = 'Histogram of x1') |> drawBeside(draw( ,x1, title = 'Thinner Q-plot of x1'), widths = c(2,1))
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = 1:2, layout_widths = c(2, 
#>     1), layout_heights = c(1, 1)), aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d43b7b3b8>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2]
#> [1,]    1    2
#> 
#> attr(,"layout")$layout_widths
#> [1] 2 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

draw(x1) |> drawBeside(draw(x2)) |> drawBeside(draw(x1, x2))
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = c(1L, 2L, 3L, 
#>     3L), layout_widths = c(1, 1), layout_heights = c(1, 1)), 
#>     aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d4720ec78>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    2    3    3
#> 
#> attr(,"layout")$layout_widths
#> [1] 1 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

draw(x1) |> drawBeside(draw(x2), draw(x1, x2)) 
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = c(1L, 2L, 3L, 
#>     3L), layout_widths = c(1, 1, 1), layout_heights = c(1, 1, 
#>     1)), aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d453dfa78>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    2    3    3
#> 
#> attr(,"layout")$layout_widths
#> [1] 1 1 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

draw(x1) |> drawBeside(draw(x2)) |> drawBelow(draw(x1, x2))
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = c(1L, 3L, 2L, 
#>     3L), layout_widths = c(1, 1), layout_heights = c(1, 1)), 
#>     aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d43b09ce8>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    3    3
#> 
#> attr(,"layout")$layout_widths
#> [1] 1 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"

# Using draw nothing

drawBeside(draw(x1), draw(, x1)) |> drawBelow(drawBeside(draw(x2), drawNothing() |> drawMore(text(.5, .5, "Blank space"))))
#> new("draw_object", .Data = function () 
#> lapply(fs, function(f) if (class(f) == "draw_object") .drawSelf(f) else f()), 
#>     add = expression(), layout = list(layout = c(1L, 3L, 2L, 
#>     4L), layout_widths = c(1, 1), layout_heights = c(1, 1)), 
#>     aspect = 1.33333333333333)
#> <bytecode: 0x566d3c752b90>
#> <environment: 0x566d45fe57f0>
#> attr(,"add")
#> expression()
#> attr(,"layout")
#> attr(,"layout")$layout
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    3    4
#> 
#> attr(,"layout")$layout_widths
#> [1] 1 1
#> 
#> attr(,"layout")$layout_heights
#> [1] 1 1
#> 
#> attr(,"aspect")
#> [1] 1.333333
#> attr(,"class")
#> [1] "draw_object"
#> attr(,"class")attr(,"package")
#> [1] "humdrumR"
```
