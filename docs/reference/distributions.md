# Distributions

HUmdrumR has ways to...

The `count()` function is exactly like R's fundamental
[table()](https://rdrr.io/r/base/table.html) function, except that 1)
will give special treatment to humdrumR
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
data 2) has more intuitive/simple argument names 3) makes it easier to
combine/manipulate disparate output tables.

## Usage

``` r
# S4 method for class 'distribution'
show(object)

# S3 method for class 'distribution'
print(
  dist,
  digits = if (inherits(dist, "probability")) 3 else 1,
  syntaxHighlight = humdrumRoption("syntaxHighlight"),
  zeros = "."
)

# S4 method for class 'distribution'
sort(x, decreasing = TRUE)

# Default S3 method
count(..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list())

# S3 method for class 'humdrumR'
count(x, ..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list())

# S3 method for class 'table'
count(..., sort = FALSE, na.rm = FALSE, .drop = FALSE)

# S3 method for class 'count'
pdist(
  x,
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)

# S3 method for class 'probability'
pdist(
  x,
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)

# Default S3 method
pdist(
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)

# S3 method for class 'data.frame'
pdist(
  x,
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)

# S3 method for class 'humdrumR'
pdist(
  x,
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)

# S3 method for class 'table'
pdist(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, binArgs = list())

table(
  x,
  ...,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = "no",
  dnn = NULL,
  deparse.level = 1
)

# S4 method for class 'humdrumR'
table(
  x,
  ...,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = "no",
  dnn = NULL,
  deparse.level = 1
)

# S4 method for class 'distribution'
table(x)
```

## Details

The `count()` function is essentially a wrapper around
[base::table()](https://rdrr.io/r/base/table.html) function. However,
any
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
class arguments are treated like `factors()`, calling generating their
own levels. This assures that, for example, pitch data is tabulated in
order of pitch height, and "missing" pitches are counted as zero.

`count()` will, by default, count `NA` values if they are present—if you
don't want to count `NA`s, specify `na.rm = TRUE`. You can also tell
`count()` to exclude (not count) any other arbitrary values you provide
as a vector to the `exclude` argument.

`count()` will always give names to the dimensions of the table it
creates. You can specify these names directly as argument names, like
`count(Kern = kern(Token))`; if you don't specify a name, `count()` will
make up a name(s) based on expression(s) it is tallying. (Note that
`count()` does not copy
[`base::table()`](https://rdrr.io/r/base/table.html)'s obtusely-named
`dnn` or `deparse.level` arguments.)

## Manipulating humdrum tables

The output of `count()` is a special form of R `table`, a
`humdrumR.table`. Given two or more `humdrumR.table`s, if you apply
basic R operators (e.g., arithmetic, comparisons) or row/column binding
(`cbind`/`rbind`) `humdrumR` will align the tables by their
dimension-names before doing the operation. This means, that if you have
two tables of pitch data, but one table includes specific pitch and
other doesn't, you can still add them together or bind them into a
matrix. See the examples!

## Examples

``` r
generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b')
complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a')

genericTable   <- count(generic)
complexTable <- count(complex)

genericTable
#> humdrumR count distribution 
#> generic  n
#>       a  1
#>       b  3
#>       c  2
#>       e  1
#>       g  1
#> generic  n
#> humdrumR count distribution 
complexTable
#> humdrumR count distribution 
#> complex  n
#>       a  1
#>       c  1
#>      c#  1
#>       e  1
#>       f  1
#>       g  1
#>      g#  2
#> complex  n
#> humdrumR count distribution 

genericTable + complexTable
#> Error: Failed to parse glue component
#> Caused by error in `parse()`:
#> ! <text>:1:11: unexpected symbol
#> 1:    NULL   base
#>               ^

cbind(genericTable, complexTable)
#> Error: Failed to parse glue component
#> Caused by error in `parse()`:
#> ! <text>:1:11: unexpected symbol
#> 1:    NULL   base
#>               ^
```
