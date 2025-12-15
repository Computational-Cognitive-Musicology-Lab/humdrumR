# Tabulate and/or cross-tabulate data

The `count()` function is exactly like R's fundamental
[table()](https://rdrr.io/r/base/table.html) function, except that 1)
will give special treatment to humdrumR
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
data 2) has more intuitive/simple argument names 3) makes it easier to
combine/manipulate disparate output tables.

## Usage

``` r
# S3 method for humdrumR
count(x, ..., sort = FALSE, na.rm = FALSE, exclude = NULL)

# S3 method for default
count(..., sort = FALSE, na.rm = FALSE, exclude = NULL)

# S4 method for humdrum.table,humdrum.table
Ops(e1, e2)

# S3 method for humdrum.table
cbind(...)

# S3 method for humdrum.table
rbind(...)

as.data.frame.humdrum.table(x, ..., responseName = "n")

as.data.frame.probabilityDistribution(x, ...)
```

## Details

The `count()` function is essentially a wrapper around
[base::table()](https://rdrr.io/r/base/table.html) function. However,
any
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
class arguments are treated like `factors()`, calling generating their
own levels. This assures that, for example, pitch data is tabulated in
order of pitch height, and "missing" pitches are counted as zero.

`count()` will, by default, count `NA` values if they are present---if
you don't want to count `NA`s, specify `na.rm = TRUE`. You can also tell
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
`humdrum.table`. Given two or more `humdrum.table`s, if you apply basic
R operators (e.g., arithmetic, comparisons) or row/column binding
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
#> generic
#> a b c e g 
#> 1 3 2 1 1 
complexTable
#> complex
#>  a  c c#  e  f  g g# 
#>  1  1  1  1  1  1  2 

genericTable + complexTable
#> generic/complex
#>  a  b  c  e  g c#  f g# 
#>  2  3  3  2  2  1  1  2 

cbind(genericTable, complexTable)
#>    [,1] [,2]
#> a     1    1
#> b     3    0
#> c     2    1
#> e     1    1
#> g     1    1
#> c#    0    1
#> f     0    1
#> g#    0    2
```
