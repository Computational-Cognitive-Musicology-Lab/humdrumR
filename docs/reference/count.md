# Tabulate and/or cross-tabulate data

The `count()` function can be used to tabulate unique values in a
vector, or cross-tabulate combinations of values across multiple vectors
of the same length. This is similar to R's fundamental
[table()](https://rdrr.io/r/base/table.html) function, except it returns
a specialized `data.frame`
([distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
object) instead of an `array`. HumdrumR `count()` methods also give
special treatment to
[`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
data and to numeric data.

The `pdist()` function is identical to `count()`, except it produces
(empirical) probability
[distributions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md),
simply by dividing by the sum of all counts or, if a `condition` is
indicated, the marginal sums across one or more dimensions. The later
case produces a conditional probability distribution. The function of
`pdist()` is similar to using
[`base::prop.table()`](https://rdrr.io/r/base/proportions.html).

## Usage

``` r
# Default S3 method
count(..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list())

# S3 method for class 'humdrumR'
count(x, ..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list())

pdist(
  x,
  ...,
  condition = NULL,
  na.rm = FALSE,
  sort = FALSE,
  .drop = FALSE,
  binArgs = list()
)
```

## Arguments

- ...:

  ***Values to count.***

  Either one or more vectors of equal length, a [humdrumR
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
  or a
  [table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md).

- sort:

  ***Should the output table be sorted?***

  Defaults to `FALSE`.

  Either a single `logical` value (on or off), or a single numeric
  value.

  Positive values (or `TRUE`) lead to decreasing sort (top to bottom);
  Negative values lead to increasing sort; Zero or `FALSE` lead to no
  sort.

- na.rm:

  ***Should `NA` values be removed (not counted)?***

  Defaults to `FALSE`.

  Must be singleton `logical` value: an on/off switch.

  If `TRUE`, `NA` values are not counted.

- .drop:

  ***Should missing levels be dropped (not counted as zeros)?***

  Defaults to `FALSE`. (This is opposite of
  [dplyr's](https://dplyr.tidyverse.org/reference/count.html) default.)

  Must be singleton `logical` value: an on/off switch.

  If `TRUE`, missing factor levels and/or missing combinations of values
  are *not* counted in the output table. If `FALSE`, these values are
  included in the output table (as zeros).

- binArgs:

  ***List of arguments to pass to numeric binning algorithm.***

  Defaults to empty [`list()`](https://rdrr.io/r/base/list.html).

  Possible list arguments include any arguments to
  [`hist()`](https://rdrr.io/r/graphics/hist.html), as well as:

  - `maxUnique` (single whole number), defaulting to `20`.

  - `right` (single `logical`), defaulting to `TRUE`,

  - `quantiles` (single whole number), defaulting to `0` (no quantiles).

  Note that the `binArgs` argument has no effect if the input (`...`)
  are not numbers.

- condition:

  ***Compute conditional entropy/information, conditioned on this
  variable.***

  Defaults to `NULL` (no condition), so the joint entropy is calculated.

  Must be a non-empty `character` string, which matches the name of one
  or of the named variables in the distribution, or a positive whole
  number which indexes the variables.

## Details

The `count()` function is defined in the
[dplyr](https://dplyr.tidyverse.org/reference/count.html) package, but
only for working with
[tibbles](https://tibble.tidyverse.org/reference/tibble.html). In
`humdrumR`, we extend the `count()` function to work with atomic data
(like [`base::table()`](https://rdrr.io/r/base/table.html)) as well as
[humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
When `humdrumR` is attached,
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
will only be called for
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
objects specifically. Note that
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
doesn't have all the same arguments or behaviors as `humdrumR`'s
`count()` methods, which are described in *this* documentation.

HumdrumR `count()` and `pdist()` methods return special
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
objects, with each input vector creating one dimension in the
distribution. When applied to atomic vectors, `humdrumR::count()` will
use the deparsed expression(s) provided to it as the name for each
vector/dimension (similar to `base::table(..., dnn = 2)`). You can
override this by specifying dimension names directly as argument names,
like `count(Kern = kern(Token))`; if you don't specify a name, `count()`
will make up a name(s) based on expression(s) it is tallying.

The `sort` argument can be used to sort the output distribution, just
like passing it to the [`sort()`](https://rdrr.io/r/base/sort.html)
function. Thus, `count(x) |> sort()` is identical to
`count(x, sort = TRUE)`. If you want to sort the output in reverse
(ascending), specify `sort = -1`. Thus,
`count(x) |> sort(decreasing = FALSE)` is also identical to
`count(x, sort = -1)`.

If `count()` or `pdist()` are applied directly to a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
you can specify any fields in the data as arguments. If you don't
specify any fields, the
[selected](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
field(s) will be passed and tabulated.

## NAs and zeros

HumdrumR `count()` and `pdist()` methods will, by default, count `NA`
values if they are present—if you don't want to count `NA`s, specify
`na.rm = TRUE`.

By default, `count()` and `pdist()` will include all known levels of
input variables, even if those levels don't occur (they are counted as
zero). This can happen if the input includes
[factors](https://rdrr.io/r/base/factor.html) or
[tokens](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md),
which have their known levels attached to them. Zeros can also occur for
any atomic type when cross-tabulating multiple vectors, if certain
combinations of values never occur. To drop zeros from the output
distribution, specify .drop =
TRUE`. (Note that `dplyr::count()`drops levels by default, but`r hm\`
functions do not.=)

## Tabulating numeric values

For numeric values, especially real numbers, it is often the case that
there are few (or no) exact values that occur more than once, so
tabulating unique values is pointless. In these cases, we might prefer
to count numbers into corresponding to ranges of numbers, like in a
histogram, and this is exactly what `count()`/`pdist()` (can) do.

By default, if you pass a vector of numbers which has more than `20`
unique values, the numbers will be binned using the same algorithm as
[`graphics::hist()`](https://rdrr.io/r/graphics/hist.html). This process
can be controlled using the `binArgs` argument, which is itself a list
of control arguments. `binArgs = list(maxUnique = N)` controls the
number of unique numbers needed before binning occurs (`20` by default).
`binArgs = list(right = FALSE)` (default is `TRUE`) controls whether are
closed on the right (larger numbers) instead of the left. Finally,
additional arguments to
[`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) can be passed
via `binArgs`, controlling how binning occurs: notably, you can use the
`binArgs = list(breaks = _)` to control exactly where boundaries should
occur, or the number of bins you want. For example,
`binArgs = list(breaks = 10)` will make `count()` bin the input numbers
into twelve bins (see [`hist()`](https://rdrr.io/r/graphics/hist.html)
for details).

Alternatively, you can tell `count()` to divy up (bin) the input numbers
into quantiles by passing `binArgs = list(quantiles = N)`. For example,
`binArgs = list(quantiles = 4)` will divide the data into four equal
quantiles (0%-25%, 25%-50%, 50%-75%, 75%-100%). In the resulting tables,
all the counts/proportions will be the same, but you can see what the
quantile ranges would be.

Note that this binning process will also be applied to integer values,
if there are more than `maxUnique` unique integers. If you ever want to
force `count()`/`pdist()` to (not) do numeric binning, coerce your input
to `character`. For example, `count(as.character(myNumbers))`.

## Coersion/conversion

Count and probability distributions, as well as base R
[tables](https://rdrr.io/r/base/table.html) can be freely converted
between using the `count()`, `pdist()`, and
[`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
functions. What this means is that, for example:

- `count(x) |> table()` is the same as `table(x)`

- `table(x) |> count()` is the same as `count(x, na.rm = TRUE)`

- `count(x) |> pdist()` is the same as `pdist(x)`

- `pdist(x) |> count() |> table()` is the same as `count(x) |> table()`

- etc.

## Conditional probability

By default, `pdist(x)` produces a table which is essentially identical
to `count(x) / length(x)`, or `count(x, y, ...) / length(x)` for
multi-dimensional arrays. This means the default is the [marginal
probability](https://en.wikipedia.org/wiki/Marginal_distribution) (for
one variable) or the [joint
probability](https://en.wikipedia.org/wiki/Joint_probability_distribution)
(for more than one variables).

If more than variables are present, `pdist()` can also the [conditional
probabilities](https://en.wikipedia.org/wiki/Conditional_probability_distribution),
conditioned on one or more of the variable. (There can be `K - 1`
conditions, where `K` is the total number of variables.) Conditions can
be expressed as either natural numbers (indicating which variable(s) to
condition on in their input order) or as character strings exactly
matching dimension names. Thus, if we call something like
`pdist(X = x, Y = y)`, we could condition on the `y` variable *either*
by saying `pdist(X = x, Y = y, condition = 2)` or
`pdist(X = x, Y = y, condition = "Y")`.

## See also

These functions create
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
objects.

## Examples

``` r
generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b', NA)
complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a', 'a##')

count(generic)
#> humdrumR count distribution 
#> generic  n
#>       a  1
#>       b  3
#>       c  2
#>       e  1
#>       g  1
#>      NA  1
#> generic  n
#> humdrumR count distribution 
count(generic, na.rm = TRUE)
#> humdrumR count distribution 
#> generic  n
#>       a  1
#>       b  3
#>       c  2
#>       e  1
#>       g  1
#> generic  n
#> humdrumR count distribution 
count(complex)
#> humdrumR count distribution 
#> complex  n
#>       a  1
#>     a##  1
#>       c  1
#>      c#  1
#>       e  1
#>       f  1
#>       g  1
#>      g#  2
#> complex  n
#> humdrumR count distribution 



count(generic, complex)
#> humdrumR count distribution 
#> generic  complex                         
#>                a  a##  c  c#  e  f  g  g#
#>      NA        .    1  .   .  .  .  .   .
#>       a        .    .  .   .  .  .  1   .
#>       b        1    .  .   .  .  .  .   2
#>       c        .    .  1   1  .  .  .   .
#>       e        .    .  .   .  1  .  .   .
#>       g        .    .  .   .  .  1  .   .
#>                a  a##  c  c#  e  f  g  g#
#> generic  complex                         
#> humdrumR count distribution 
count(generic, complex, sort = TRUE)
#> humdrumR count distribution 
#> Rank  generic  complex  n
#> 1           b       g#  2
#> 2           b        a  1
#> 3          NA      a##  1
#> 4           c        c  1
#> 5           c       c#  1
#> 6           e        e  1
#> 7           g        f  1
#> 8           a        g  1
#> 9           a        a  .
#> 10          c        a  .
#> 11          e        a  .
#> 12          g        a  .
#> 13         NA        a  .
#> 14          a      a##  .
#> 15          b      a##  .
#> 16          c      a##  .
#> 17          e      a##  .
#> 18          g      a##  .
#> 19          a        c  .
#> 20          b        c  .
#> 21          e        c  .
#> 22          g        c  .
#> 23         NA        c  .
#> 24          a       c#  .
#> 25          b       c#  .
#> 26          e       c#  .
#> 27          g       c#  .
#> 28         NA       c#  .
#> 29          a        e  .
#> 30          b        e  .
#> 31          c        e  .
#> 32          g        e  .
#> 33         NA        e  .
#> 34          a        f  .
#> 35          b        f  .
#> 36          c        f  .
#> 37          e        f  .
#> 38         NA        f  .
#> 39          b        g  .
#> 40          c        g  .
#> 41          e        g  .
#> 42          g        g  .
#> 43         NA        g  .
#> 44          a       g#  .
#> 45          c       g#  .
#> 46          e       g#  .
#> 47          g       g#  .
#> 48         NA       g#  .
#> Rank  generic  complex  n
#> humdrumR count distribution 
count(generic, complex, sort = -1)
#> humdrumR count distribution 
#> Rank  generic  complex  n
#> 48          a        a  .
#> 47          c        a  .
#> 46          e        a  .
#> 45          g        a  .
#> 44         NA        a  .
#> 43          a      a##  .
#> 42          b      a##  .
#> 41          c      a##  .
#> 40          e      a##  .
#> 39          g      a##  .
#> 38          a        c  .
#> 37          b        c  .
#> 36          e        c  .
#> 35          g        c  .
#> 34         NA        c  .
#> 33          a       c#  .
#> 32          b       c#  .
#> 31          e       c#  .
#> 30          g       c#  .
#> 29         NA       c#  .
#> 28          a        e  .
#> 27          b        e  .
#> 26          c        e  .
#> 25          g        e  .
#> 24         NA        e  .
#> 23          a        f  .
#> 22          b        f  .
#> 21          c        f  .
#> 20          e        f  .
#> 19         NA        f  .
#> 18          b        g  .
#> 17          c        g  .
#> 16          e        g  .
#> 15          g        g  .
#> 14         NA        g  .
#> 13          a       g#  .
#> 12          c       g#  .
#> 11          e       g#  .
#> 10          g       g#  .
#> 9          NA       g#  .
#> 8           b        a  1
#> 7          NA      a##  1
#> 6           c        c  1
#> 5           c       c#  1
#> 4           e        e  1
#> 3           g        f  1
#> 2           a        g  1
#> 1           b       g#  2
#> Rank  generic  complex  n
#> humdrumR count distribution 
count(generic, complex, sort = -1, na.rm = TRUE)
#> humdrumR count distribution 
#> Rank  generic  complex  n
#> 40          a        a  .
#> 39          c        a  .
#> 38          e        a  .
#> 37          g        a  .
#> 36          a      a##  .
#> 35          b      a##  .
#> 34          c      a##  .
#> 33          e      a##  .
#> 32          g      a##  .
#> 31          a        c  .
#> 30          b        c  .
#> 29          e        c  .
#> 28          g        c  .
#> 27          a       c#  .
#> 26          b       c#  .
#> 25          e       c#  .
#> 24          g       c#  .
#> 23          a        e  .
#> 22          b        e  .
#> 21          c        e  .
#> 20          g        e  .
#> 19          a        f  .
#> 18          b        f  .
#> 17          c        f  .
#> 16          e        f  .
#> 15          b        g  .
#> 14          c        g  .
#> 13          e        g  .
#> 12          g        g  .
#> 11          a       g#  .
#> 10          c       g#  .
#> 9           e       g#  .
#> 8           g       g#  .
#> 7           b        a  1
#> 6           c        c  1
#> 5           c       c#  1
#> 4           e        e  1
#> 3           g        f  1
#> 2           a        g  1
#> 1           b       g#  2
#> Rank  generic  complex  n
#> humdrumR count distribution 
count(generic, complex, sort = -1, .drop = TRUE)
#> humdrumR count distribution 
#> Rank  generic  complex  n
#> 8           b        a  1
#> 7          NA      a##  1
#> 6           c        c  1
#> 5           c       c#  1
#> 4           e        e  1
#> 3           g        f  1
#> 2           a        g  1
#> 1           b       g#  2
#> Rank  generic  complex  n
#> humdrumR count distribution 

# Dimension names
count(Generic = generic, X = complex)
#> humdrumR count distribution 
#> Generic  X                         
#>          a  a##  c  c#  e  f  g  g#
#>      NA  .    1  .   .  .  .  .   .
#>       a  .    .  .   .  .  .  1   .
#>       b  1    .  .   .  .  .  .   2
#>       c  .    .  1   1  .  .  .   .
#>       e  .    .  .   .  1  .  .   .
#>       g  .    .  .   .  .  1  .   .
#>          a  a##  c  c#  e  f  g  g#
#> Generic  X                         
#> humdrumR count distribution 

# HumdrumR data
if (FALSE) { # \dontrun{
  humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")

  humData |> kern() |> count()
  humData |> mutate(Kern = kern(Token), Recip = recip(Token)) |> count()
  humData |> mutate(Kern = kern(Token),  Recip = recip(Token)) |> count(Recip, sort = TRUE)
  humData |> mutate(Kern = kern(Token),  Recip = recip(Token)) |> select(Recip) |> count()
} # }

# Numeric values

real <- rnorm(1000)

count(real)
#> humdrumR count distribution 
#>      real    n
#> [-4,-3.5]    1
#> (-3.5,-3]    2
#> (-3,-2.5]    3
#> (-2.5,-2]    9
#> (-2,-1.5]   43
#> (-1.5,-1]   92
#> (-1,-0.5]  165
#>  (-0.5,0]  174
#>   (0,0.5]  203
#>   (0.5,1]  158
#>   (1,1.5]   86
#>   (1.5,2]   44
#>   (2,2.5]   14
#>   (2.5,3]    5
#>   (3,3.5]    1
#>      real    n
#> humdrumR count distribution 
count(real, binArgs = list(breaks = 40))
#> humdrumR count distribution 
#>        real   n
#> [-3.6,-3.4]   1
#> (-3.4,-3.2]   2
#>   (-3.2,-3]   .
#>   (-3,-2.8]   1
#> (-2.8,-2.6]   .
#> (-2.6,-2.4]   2
#> (-2.4,-2.2]   2
#>   (-2.2,-2]   7
#>   (-2,-1.8]  10
#> (-1.8,-1.6]  16
#> (-1.6,-1.4]  33
#> (-1.4,-1.2]  40
#>   (-1.2,-1]  36
#>   (-1,-0.8]  56
#> (-0.8,-0.6]  65
#> (-0.6,-0.4]  70
#> (-0.4,-0.2]  71
#>    (-0.2,0]  77
#>     (0,0.2]  69
#>   (0.2,0.4]  97
#>   (0.4,0.6]  78
#>   (0.6,0.8]  63
#>     (0.8,1]  54
#>     (1,1.2]  40
#>   (1.2,1.4]  36
#>   (1.4,1.6]  22
#>   (1.6,1.8]  21
#>     (1.8,2]  11
#>     (2,2.2]   6
#>   (2.2,2.4]   3
#>   (2.4,2.6]   6
#>   (2.6,2.8]   3
#>     (2.8,3]   1
#>     (3,3.2]   1
#>        real   n
#> humdrumR count distribution 
count(real, binArgs = list(breaks = 40, right = FALSE))
#> humdrumR count distribution 
#>        real   n
#> [-3.6,-3.4)   1
#> [-3.4,-3.2)   2
#>   [-3.2,-3)   .
#>   [-3,-2.8)   1
#> [-2.8,-2.6)   .
#> [-2.6,-2.4)   2
#> [-2.4,-2.2)   2
#>   [-2.2,-2)   7
#>   [-2,-1.8)  10
#> [-1.8,-1.6)  16
#> [-1.6,-1.4)  33
#> [-1.4,-1.2)  40
#>   [-1.2,-1)  36
#>   [-1,-0.8)  56
#> [-0.8,-0.6)  65
#> [-0.6,-0.4)  70
#> [-0.4,-0.2)  71
#>    [-0.2,0)  77
#>     [0,0.2)  69
#>   [0.2,0.4)  97
#>   [0.4,0.6)  78
#>   [0.6,0.8)  63
#>     [0.8,1)  54
#>     [1,1.2)  40
#>   [1.2,1.4)  36
#>   [1.4,1.6)  22
#>   [1.6,1.8)  21
#>     [1.8,2)  11
#>     [2,2.2)   6
#>   [2.2,2.4)   3
#>   [2.4,2.6)   6
#>   [2.6,2.8)   3
#>     [2.8,3)   1
#>     [3,3.2]   1
#>        real   n
#> humdrumR count distribution 
count(real, binArgs = list(quantiles = 4))
#> humdrumR count distribution 
#>            real    n
#>  [-3.52,-0.664]  250
#> (-0.664,0.0456]  250
#>   (0.0456,0.65]  250
#>     (0.65,3.08]  250
#>            real    n
#> humdrumR count distribution 

int <- sample(100, 30, replace = TRUE)

count(int)
#> humdrumR count distribution 
#>      int  n
#>   [0,20]  3
#>  (20,40]  7
#>  (40,60]  7
#>  (60,80]  4
#> (80,100]  9
#>      int  n
#> humdrumR count distribution 
count(int, binArgs = list(maxUnique = 50))
#> humdrumR count distribution 
#> int  n
#>   4  1
#>  11  1
#>  18  1
#>  25  1
#>  30  1
#>  33  1
#>  34  1
#>  35  1
#>  37  1
#>  40  1
#>  45  1
#>  52  1
#>  55  1
#>  58  1
#>  59  3
#>  63  1
#>  64  1
#>  70  1
#>  71  1
#>  83  1
#>  84  1
#>  86  2
#>  87  1
#>  89  1
#>  93  1
#>  94  1
#>  99  1
#> int  n
#> humdrumR count distribution 
count(int, binArgs = list(maxUnique = 5))
#> humdrumR count distribution 
#>      int  n
#>   [0,20]  3
#>  (20,40]  7
#>  (40,60]  7
#>  (60,80]  4
#> (80,100]  9
#>      int  n
#> humdrumR count distribution 


chord <- c('I', 'I', 'I', 'I', 'V', 'V', 'V', 'V', 'IV', 'IV', 'IV', 'IV')
note  <- c('1', '3', '5', '5', '5', '7', '4', '2', '6',  '1',   '4',  '4')

entropy(chord, note) # joint entropy
#> H(chord,note) 
#>      3.251629 
entropy(chord, note, condition = 'note') # conditional entropy
#> H(chord|note) 
#>     0.6258146 

entropy_by(chord, note, condition = 'note') # conditional entropy, by condition
#> humdrumR entropy distribution H(chord|note) 
#> note  H(chord)
#>    1     1.000
#>    2     0.000
#>    3     0.000
#>    4     0.918
#>    5     0.918
#>    6     0.000
#>    7     0.000
#> note  H(chord)
#> humdrumR entropy distribution H(chord|note) 

```
