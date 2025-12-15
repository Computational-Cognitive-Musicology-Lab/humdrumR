# Distributions

HumdrumR represents data distributions in a special `distribution`
class, which is a simple extension of a standard `data.frame`.
Distributions may have one or more dimensions. There are two subtypes of
distributions: count distributions (created by
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md))
and probability distributions (created by
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)).

## Usage

``` r
varnames(x)

varnames(x) <- value

# S3 method for class 'distribution'
print(
  dist,
  digits = 3,
  syntaxHighlight = humdrumRoption("syntaxHighlight"),
  wide = TRUE,
  printZeros = TRUE,
  zeros = "."
)

# S4 method for class 'distribution'
sort(x, decreasing = TRUE)

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

## Arguments

- wide:

  ***Should the second dimension in distributions be printed "wide"?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- printZeros:

  ***Should zero counts/probabilities be printed?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- zeros:

  ***How should zeros be represented in tables?***

  Defaults to `"."`.

  Must be a single atomic value, which will be coerced to a `character`.

- syntaxHighight:

  ***Should syntax highlighting be used (in Rstudio)?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

## Details

The `distribution` class is essentially just a normal `data.frame`
except with a special methods for printing, indexing, and combining
distributions. These features all make the object look and behave more
intuitively like a like a distribution of data. You can always use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html),
or `as_tibble()` (if you've attached
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)) to
get rid of the humdrumR features.

Each `distribution` frame has \\k + 1\\ columns, where \\k\\ is the
number of dimensions (variables). In a `distribution` data.frame, each
row represents a level, or combination of levels, in the distribution.
The first \$k\$ columns indicate the levels of each variable in the
distribution (each dimension). Each of these \$k\$ dimensions always has
a unique (non-empty) name, which can be accessed with the `varnames()`
command, or modified with `varnames(myDist) <- newnames`. These columns
can be accessed directly using the
[`levels()`](https://rdrr.io/r/base/levels.html) command.

The last (rightmost) column contains the count or proportion associated
with each level. In the case of count distributions, this last column is
named `n`; in probability distributions, it is named `p`. The `n`
(count) column is always an `integer` vector, where are \\\forall n, n
\geq 0\\. The `p` (proportion) column is always a `numeric` vector,
where \\\forall p, 1 \geq p \geq 0\\.

We define
[`as.matrix()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
and [`as.array()`](https://rdrr.io/r/base/array.html) methods for
`distribution` objects. The
[`as.array()`](https://rdrr.io/r/base/array.html) method results in an
[`array()`](https://rdrr.io/r/base/array.html) with the same
dimensionality as the distribution. In contrast, the
[`as.matrix()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
function will force distributions with more than two dimensions into two
dimensions, by collapsing combining all dimensions greater than two with
the first dimension: each combination of levels will appear as a row
(with `.` in between the names of levels).

## Printing distributions

The [`print()`](https://rdrr.io/r/base/print.html) method for
distribution objects has some nice features, including syntax
highlighting (which can be turned off with the `syntaxHighlight = FALSE`
argument to [`print()`](https://rdrr.io/r/base/print.html), or by
setting the global humdrumR option with
`humdrumR(syntaxHighlight = FALSE)`.

HumdrumR distributions will print rounded versions of numbers (marked
with `~`), and use appropriate [SI
prefixes](https://en.wikipedia.org/wiki/Metric_prefix) for large and
small numbers. Specifically, count distributions will affix large
numbers with the characters `k` (kilo), `M` (mega), `G` (giga), or `P`
(peta), while probability distributions will use `m` (milli), `𝜇`
(micro), `n` (nano), and `p` (pico). For example, humdrumR will print
`12321` as `~12.3k`. The number of digits printed can be controlled with
the `digits` argument to [`print()`](https://rdrr.io/r/base/print.html),
defaulting to `digits = 3`. For example, if `digits = 4`, `12321` would
print as `~12.32k`. If *all* numbers in a distribution are of the same
order of magnitude, the scale is printed with the name of the
distribution instead of adding prefixes to all the numbers. For example,
it might print "humdrumR count distribution (thousands)."

The `distribution` print method also includes an argument `printZeros`,
to control whether zero values are printed, and the argument `zeros` to
control *how* zeros are printed. By default, `zeros = '.'`, so zeros are
printed as `.`, which makes tables easier to read. Note that the
`printZeros` function only determines if zeros are printed; the
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
functions also have the `.drop` argument to actually *remove* those
levels from a distribution.

A final printing feature is controlled by the `wide` print argument.
When `wide = TRUE` (which is the default), the second dimension (if
present) of the a distribution is printed "wide", with each level in its
own column. This creates a contingency table like visual, which is
easier to read. This wide format is only used when the resulting
printout will fit on your screen.

Note that the wide printing does not change the structure of the
underlying `data.frame`, which is still a "tall" data.frame, with each
levels of dimension two represented in different rows. This may make
indexing unintuitive, i.e., if you expect the (apparent) two-dimensional
table to be indexed like a 2d matrix/table would be in R. It won't work
that way! This is why we provide the `wide = FALSE` option, so you can
see what the table "really" looks like if you need to.

## Indexing distributions

Subsets of humdrumR `distribution` objects can be extracted using
indexing (`[]` or `[[]]`) or the (equivalent)
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)/[`subset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
methods. In either case, a `distribution` of the same class (either
`count` or `probability`) if returned unless the `drop` argument is set
to `TRUE`, which will cause a `data.frame` to return. Distributions
cannot be empty, so any attempt to index that returns nothing will
result in an error (unless `drop = TRUE`). You can use
`myDist[drop = TRUE]` to transform a `distribution` into a normal
`data.frame`, without indexing it.

In calls to
[`subset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)/[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
you can refer to either the distribution's variable names, or to the `n`
(count) or `p` (proportion) column. For example, you can find all
variables where the count is greater than one hundred with
`myDist |> filter(n > 100)`.

### Single-bracket \[i , \]

With single-bracket indexing, the `i` argument is matched to rows of the
underlying data.table. (As mentioned above, this may conflict visually
with the `wide` printing option, for distributions with two or more
dimensions.) If `i` is either `logical` or `numeric`, indexing is
exactly like conventional `data.frames`, except there are more strict
checks for valid indexes—for example, non-whole values of `i` are not
allowed, and an error will throw if `any(i > nrow(myDist))`,

If `i` is `character`, the strings are matched against the level names
of *all* the distributions' dimensions. An exact match with a level in
any dimension will result in that level being returned.

### Single-bracket \[ , j\]

With single-bracket indexing, the `j` argument is used to index the
dimensions of the distribution. If `j` is either `logical` or `numeric`,
indexing is exactly like conventional `data.frames`, except 1) there are
more strict checks for valid indexes—for example, non-whole values of
`j` are not allowed, and an error will throw if
`any(j > length(varnames(myDist)))`; and 2) the `n`/`p` is not counted
as a column to index—this column is always retained. If `j` is
`character`, the strings are matched exactly against the distribution
names.

### Double-bracket \[i, j, ...\]

Double-bracket indexing can be used to index specific combinations of
levels, across more than one distribution. Named index arguments are
matched (exactly) to dimension names; unnamed arguments are matched in
order to any dimensions not given a named argument. Each index argument
can be a `logical` (same length as number of levels for corresponding
dimension), whole numbers (indexing dimension levels in order), or
`character` strings (exactly) matching level names.

Normally, each dimension is indexed separately based on any indexes
applied to it. However, if `cartesian = TRUE`, specific combinations of
levels are indexed based on which values in each of the index vectors
align with each other. (This approach cannot be used with `logical`
indices.) For example, `myDist[[c(1, 1, 2, 2), c(1, 2, 3, 4)]]` will
return only the counts/proportions from the level-pairs (1,1), (1,2),
(2,3), and (2,4). The count for (1,3)–for example—would not be returned.
This behavior is similar to indexing a
[`base::matrix()`](https://rdrr.io/r/base/matrix.html) using
matrix—indeed, you can get the same behavior by indexing with a matrix.
For example, `myDist[[cbind(c(1,1,2,2), c(1,2,3,4))]]`.

### Probability sums

When indexing/filtering a `probability` distribution, the probabilities
of any remaining levels after filtering/indexing are recomputed so as to
sum to 1. If you index out dimensions, the levels of the removed
dimensions are summed across the levels of the remaining dimensions. If
you want to keep the values as is (not summing to 1), use
`drop = TRUE`—the output will no longer be a probability distribution.

## Conditional probability

`probability` `distribution` objects, by default, represent the joint
probability of all their dimensions. (This means that the whole
distribution sums to one). However, they may also have their
probabilities conditioned on one or more of their variables, so that the
probabilities with each level (or combinations of levels) of the
conditions sum to one. These conditions can be set when a distribution
is created, or modified, by the
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function, using the `condition` argument. When a `condition` is not
specified, the joint probability is computed. Thus, you can remove
conditions from any `probability` distribution by passing it to
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
with no `condition` argument—or can also change the condition to other
variables. For example:

    pdist(X = x, Y = y, condition = 'X') -> condition_X

    condition_X |> pdist() -> joint
    condition_X |> pdist(condition = 'Y') -> condition_y

When indexing levels, conditional probabilities are resummed as usual.
If you are indexing out variables, any conditions remaining in the
distribution (after indexing) will be kept (and recomputed) if it makes
sense. However, if you index out all the conditions, or index out all
variables that weren't conditioned on, the conditions will be removed
during indexing.

## Combining distributions

HumdrumR's `distribution` objects can be concatenatated (combined)
together to form new distributions. Only `distribution` objects with
*identical* dimension names can be combined (an error will be thrown,
otherwise). (If you need to change dimension names to make the match,
use `varnames()<-`. When combined, the shared levels of each dimension
are aligned. Levels that are not shared are simply copied from their
origin `distribution`. This means that the levels of each dimension of
the resulting `distribution` will be the union of the levels of that
dimension between the two originating distributions.

The [`c()`](https://rdrr.io/r/base/c.html) function can be used to
combine two or more `count` distributions which have identical dimension
names. The resulting distribution has a new variable added, indexing the
original distribution source. This new variable/dimension is named
`"concat"` by default, but can be changed to another name using the
`varname` argument.

## Distribution math

HumdrumR defines some mathematical operations for `distribution`
objects, including arithmetic and comparison operations between two
distributions; operations between a distributions and positive,
whole-number scalar values, and mathematical summaries of distributions.

When doing arithmetic or comparison with `distribution` objects, some
operations make sense as "closed" operations: meaning that the result of
the operation is still a `distribution` of the same type. In contrast,
other arithmetic operations only make sense if we view the result as
"just" numbers, not a new distribution. In these cases, humdrumR simply
returns an `atomic` vector corresponding to the `p`/`n` column of the
distribution, with names corresponding to the levels of the
distribution. These vectors can be used to, for example, index the
original distribution; for example: `myCounts[myCounts > 50]`.

### Arithmetic with count distributions

Only addition (`+`) between two `count` distributions results in new
distribution: all other operations *between* distributions result in an
vector return value. (Subtraction is not allowed, so as to avoid
negative counts.) However, `count` distributions can be scaled by
positive whole numbers using either `*` (multiplication) or `%/%`
(Euclidean division), while remaining a count distribution. The scaling
value must either be length 1, or be the same length as the number of
levels in the distribution (`nrow(levels(myDist))`). All other
arithmetic with count distributions will result in vector output.

### Arithmetic with probability distributions

For `probability` distributions, all arithmetic involving distributions
inevitably destroys their structure (e.g., make it so the total
probability no longer sums to 1), so if arithmetic is done between
`probability` distributions (which share dimension names), or between
`probability` distributions and scalar values (length 1, or same length
as the number of levels in the distribution), the result is always
always a vector.

There is one other special arithmetic operation between `probability`
distributions which *don't* share dimensions. The outer product function
`%o%` can be used to produce the empirical, independent joint
probability between two probability distributions. The joint probability
of all levels is calculated, assuming all dimensions/variables are
independent—in other words, the joint product of each condition is just
the product of all the levels. This outer-product operation (`%o%`) will
strip any/all conditions from the `probability` distributions.

### Mathematical summaries of distributions

The R "group generic functions"
[Math](https://rdrr.io/r/base/groupGeneric.html) and
[Summary](https://rdrr.io/r/base/groupGeneric.html) are defined for
`distribution` objects. This includes, functions like
[`log()`](https://rdrr.io/r/base/Log.html),
[`round()`](https://rdrr.io/r/base/Round.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`range()`](https://rdrr.io/r/base/range.html), and
[`sum()`](https://rdrr.io/r/base/sum.html). Methods for
[`mean()`](https://rdrr.io/r/base/mean.html) and
[`median()`](https://rdrr.io/r/stats/median.html) are also defined. All
of these functions, when applied to a `distribution`, return an atomic
vector.

For more general operations with `distribution` values, use
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
or
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
(or [with/within](https://rdrr.io/r/base/with.html)), and refer to the
`n` (count) or `p` (probability) field. For example:
`myDist |> filter(n > 70)`.

## See also

Use the
[count()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md) and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
functions to create distribution objects.
