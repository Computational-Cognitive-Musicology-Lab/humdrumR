# Tabulate and/or cross-tabulate data

The `count()` function is exactly like R's fundamental
[table()](https://rdrr.io/r/base/table.html) function, except that 1)
will give special treatment to humdrumR
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
data 2) has more intuitive/simple argument names 3) makes it easier to
combine/manipulate disparate output tables.

## Usage

    # S4 method for counts
    show(object)

    # S3 method for counts
    merge(x, y)

    # S4 method for counts,counts
    +(e1, e2)

    # S4 method for counts,counts
    -(e1, e2)

    # S4 method for counts,counts
    Ops(e1, e2)

    sort.counts(x, decreasing = TRUE)

    # S3 method for counts
    as.data.table(x)

    as.data.frame.counts(x)

    # S3 method for humdrumR
    count(x, ..., sort = FALSE, na.rm = FALSE, exclude = NULL, .drop = FALSE)

    # S3 method for default
    count(..., sort = FALSE, na.rm = FALSE, exclude = NULL, .drop = FALSE)

    # S3 method for table
    count(..., sort = FALSE, na.rm = FALSE, exclude = NULL, .drop = FALSE)

    table(
      ...,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = c("no", "ifany", "always"),
      dnn = list.names(...),
      deparse.level = 1
    )

    # S4 method for token
    table(
      ...,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = "no",
      dnn = names(list(...)),
      deparse.level = 1
    )

    # S4 method for humdrumR
    table(
      ...,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = "no",
      dnn = names(list(...)),
      deparse.level = 1
    )

    # S3 method for counts
    as.table(x)

    # S4 method for counts
    table(
      ...,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = c("no", "ifany", "always"),
      dnn = list.names(...),
      deparse.level = 1
    )

    as.data.frame.probabilityDistribution(x, ...)

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

The output of `count()` is a special form of R `table`, a `counts`.
Given two or more `counts`s, if you apply basic R operators (e.g.,
arithmetic, comparisons) or row/column binding (`cbind`/`rbind`)
`humdrumR` will align the tables by their dimension-names before doing
the operation. This means, that if you have two tables of pitch data,
but one table includes specific pitch and other doesn't, you can still
add them together or bind them into a matrix. See the examples!

## Examples

``` r
generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b')
complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a')

genericTable   <- count(generic)
complexTable <- count(complex)

genericTable
#>    generic Count
#> 1:       a     1
#> 2:       b     3
#> 3:       c     2
#> 4:       e     1
#> 5:       g     1
complexTable
#>    complex Count
#> 1:       a     1
#> 2:       c     1
#> 3:      c#     1
#> 4:       e     1
#> 5:       f     1
#> 6:       g     1
#> 7:      g#     2

genericTable + complexTable
#>     generic complex Count
#>  1:       a    <NA>     1
#>  2:       b    <NA>     3
#>  3:       c    <NA>     2
#>  4:       e    <NA>     1
#>  5:       g    <NA>     1
#>  6:    <NA>       a     1
#>  7:    <NA>       c     1
#>  8:    <NA>      c#     1
#>  9:    <NA>       e     1
#> 10:    <NA>       f     1
#> 11:    <NA>       g     1
#> 12:    <NA>      g#     2

cbind(genericTable, complexTable)
#> Warning: Item 1 has 5 rows but longest item has 7; recycled with remainder.
#>    generic Count complex Count
#> 1:       a     1       a     1
#> 2:       b     3       c     1
#> 3:       c     2      c#     1
#> 4:       e     1       e     1
#> 5:       g     1       f     1
#> 6:       a     1       g     1
#> 7:       b     3      g#     2
```
