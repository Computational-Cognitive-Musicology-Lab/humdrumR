# Working *with* humdrum data fields

These functions are the primary means of working with humdrumR data.
They allow us to perform arbitrary (free form) manipulation of data
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
held within a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
with convenient functionality for ignoring null data,
[lagging](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) data,
[grouping](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
data,
[windowing](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md),
and more. The `with()` and `within()` functions, which come from [base
R](https://rdrr.io/r/base/base-package.html), are the core functions.
However, the
[dplyr::dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
"verbs" `mutate()`, `summarize()`, and `reframe()` can be used as
well—they are equivalent to using `with()`/`within()` with particular
arguments.

## Usage

``` r
# S3 method for class 'humdrumR'
with(
  data,
  ...,
  dataTypes = "D",
  recycle = "no",
  alignLeft = TRUE,
  expandPaths = FALSE,
  drop = TRUE,
  .by = NULL,
  variables = list()
)

# S3 method for class 'humdrumR'
within(
  data,
  ...,
  dataTypes = "D",
  alignLeft = TRUE,
  expandPaths = FALSE,
  recycle = "pad",
  .by = NULL,
  variables = list()
)

# S3 method for class 'humdrumR'
mutate(
  .data,
  ...,
  dataTypes = "D",
  recycle = "ifscalar",
  alignLeft = TRUE,
  expandPaths = FALSE,
  .by = NULL
)

# S3 method for class 'humdrumR'
summarise(
  .data,
  ...,
  dataTypes = "D",
  expandPaths = FALSE,
  drop = FALSE,
  .by = NULL
)

# S3 method for class 'humdrumR'
reframe(
  .data,
  ...,
  dataTypes = "D",
  alignLeft = TRUE,
  expandPaths = FALSE,
  recycle = "pad",
  .by = NULL
)

# S3 method for class 'humdrumR'
ggplot(data = NULL, mapping = aes(), ..., dataTypes = "D")
```

## Arguments

- data:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Any number of expressions to evaluate.***

  These expressions can reference
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the data by name, as well as variables outside the data.

  If the expressions are named, the names are used to name the new
  fields (or column names for `with(..., drop = FALSE)`.

- dataTypes:

  ***Which types of humdrum records to include.***

  Defaults to `"D"`.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.)

- recycle:

  ***How should results be "recycled" (or padded) to relative to the
  input length?***

  `within()` and `reframe()` default to `"pad"`; `mutate()` defaults to
  `"ifscalar"`; `with()` defaults to `"no"`.

  Must be a single `character` string. The full list of options are
  `"no"`, `"yes"`, `"pad"`, `"ifscalar"`, `"ifeven"`, `"never"`, and
  `"summarize"`, though not all functions accept all options. See the
  *Parsing expression results* section below.

- alignLeft:

  ***Should output that is shorter than input be aligned to the left?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- expandPaths:

  ***Should spine paths be expanded before evaluating expressions?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch. If `TRUE`, the
  [`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)
  function is run on the data before evaluating the expressions. After
  evaluation, the expanded locations are removed from the output.

- drop:

  ***Whether to return a simplified data structure.***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  This argument is conceptually similar to the `drop` argument in R
  matrices. If `drop = TRUE`, the output of `with()`/`summarize()` is
  simplified as much as possible (trying to return the "raw" vector,
  list, table, etc. within it). If `drop = FALSE`, the result is
  *always* a
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

- .by:

  ***Optional grouping fields; an alternative to using
  [group_by()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md).***

  Defaults to `NULL`.

  Must be `NULL`, or `character` strings which [partially
  match](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  one or more
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the `data`.

  If not `NULL`, these fields are used to group the data. If grouping
  fields have already been set by a call to
  [group_by()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md),
  the `.by` argument overrides them.

- variables:

  ***A named `list` of values, to interpolate into your expressions.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a named `list`. These values are interpolated into the `...`
  expression arguments wherever a variable name matches a name from the
  list.

## Overview

These functions are the primary means of working with [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
They all allow you to write code that accesses and manipulates the raw
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
in our data. The main differences between them are what they do with the
*results* of your code: `with()` and `summarize()` return results in
normal, "raw" R formats, **removed** from the [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md);
In contrast, `within()`, `mutate()`, and `reframe()` always insert the
results of your code into new
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
**within** your humdrum data. The other distinctions between these
functions have to do with how they recycle/pad results (see below).

## Expression evaluation

The `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()`
methods for [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
all perform "[non-standard
evalation](http://adv-r.had.co.nz/Computing-on-the-language.md)" of any
[expressions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
you provide them as arguments. Basically, when you use a function like
`with(...)` or `mutate(...)`, the expressions you write inside the
function call aren't
[evaluated](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
right then and there—instead, R takes those expressions into the
"[environment](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)"
of your [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
where all your fields are "visible" to the expression. This means you
can write code (expressions) that refer to your
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
like `Token` or `Spine`. For example:

    with(humData,
         ifelse(Spine > 2,
                kern(Token),
                recip(Token)))

Since all the fields in a [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md) are
the same length, the expressions you write can be, and generally should
be,
[vectorized](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md).

By default, `with()`, `within()`, etc. don't use the whole [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
but instead only evaluate their expressions using rows containing
non-null data tokens (`Type == "D"`). This means that interpretations,
comments, barlines, and null data tokens are automatically ignored for
you! This feature is controlled by the `dataTypes` argument: you can
choose to work with the other token types by providing a `character`
string containing combinations of the characters `G` (global comments),
`L` (local comments), `I` (interpretations), `M` (barlines), `D`
(non-null data), or `d` (null data). For example, `dataTypes = 'MDd'`
will evaluate your expressions on barline tokens (`=`), non-null data,
and null data. See the
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md)
manual for an example application of using `dataTypes = 'Dd'`. Keep in
mind that `humdrumR` dynamically updates what tokens are considered
"null" (`"d"`) based on what fields are
[selected](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).

If multiple expression arguments are provided, each expression is
evaluated in order, from left to right. Each expression can refer to
variables assigned in the previous expression (examples below).

*Note*: Within any of these expressions, the humdrumR namespace takes
priority. This means that, for example, if you use
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)
within an expression, the humdrumR version of
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) will
be used, even if you have loaded other packages which have their own
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)
function. To use another package's function, you'll have to specify
`package::function()`—for example,
[`dplyr::lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html).
This is only an issue when functions have the exact same name as a
humdrumR function.

### Expression pre-processing

These functions all do some pre-processing of expressions arguments
before evaluating them. This pre-processing provides some convenient
"[syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar)" for
working with humdrum data. There are currently five pre-processing
steps:

1.  Explicit variable interpolation.

2.  The `.` placeholder for selected fields.

3.  Automatic argument insertion.

4.  "Lagged"-vectors shorthand.

5.  "Splatted" arguments.

Each of these is explained below.

#### Explicit variable interpolation

The `variable` argument can be provided as an (option) `list` of named
values. If any of the names in the `variable` list appear as symbols
(variable names) in any expression argument, their value is interpolated
in place of that symbol. For example, in

    within(humData, kern(Token, simple = x), variables = list(x = TRUE))

the variable `x` will be changed to `TRUE`, resulting in:

    within(humData, kern(Token, simple = TRUE))

This feature is most useful for programmatic purposes, like if you'd
like to run the same expression many times but with slightly different
parameters.

#### The . placeholder

The `.` variable can be used as a special placeholder representing the
data's first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).
For example, in

    humData |>
      select(Token) |>
      with(count(.))

will run
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
on the `Token` field.

Because new fields created by `within()`/`mutate()`/`reframe()` become
the [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
(details below), the `.` makes it easy to refer to the *last* new field
in pipes. For example, in

    humData |>
       mutate(kern(Token, simple = TRUE)) |>
       with(count(.))

the
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function is run on the output of the `mutate(kern(Token, simpe = TRUE))`
expression.

#### Automatic argument insertion

Many
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
functions are designed to work with certain common fields in [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
For example, many [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
have a `Key` argument which (can) take the content of the `Key` which
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
creates when there are key interpretations, like `*G:`, in the data.
When an expression argument uses one of these functions, but doesn't
explicitly set the argument, humdrumR will *automatically* insert the
appropriate field into the call (if the field is present). So, for
example, if you run

    humData |>
       mutate(Solfa = solfa(Token))

on a data set that includes a `Key` field, the expression will be
changed to:

    humData |>
       mutate(Solfa = solfa(Token, Key = Key))

If you *don't* want this to happen, you need to explicitly give a
different `Key` argument, like:

    humData |>
       mutate(Solfa = solfa(Token, Key = 'F:'))

(The `Key` argument can also be set to `NULL`).

Another common/important automatic argument insertion is for functions
with a `groupby` argument. These functions will automatically have
appropriate grouping fields inserted into them. For example, the
[`mint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md)
(melodic intervals) command will *automatically* use `groupby`
`groupby = list(Piece, Spine, Path)`, which makes sure that melodic
intervals are only calculated within spine paths...not between
pieces/spines/paths (which wouldn't make sense!).

All `humdrumR` functions which use automatic argument interpolation will
mention it in their own documentation. For example, the
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md)
documentation mentions the treatment of `Key` in its "Key" section.

#### Lagged vectors

In music analysis, we very often want to work with
"[lagged](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)"
vectors of data. For example, we want to look at the relationship
between a vector and the previous values of the same vector—e.g., the
vector offset or "lagged" by one index. The
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) and
[`lead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)
functions are useful for this, always keeping them the same length so
vectorization is never hindered.

In expression arguments, we can use a convenient shorthand to call
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) (or
`lead`). In an expression, any vector can be indexed with an `integer`
argument named `lag` or `lead` (case insensitive), causing it to be
lagged/led by that integer amount. (A vector indexed with `lag = 0`
returns the unchanged vector.) For example, the following two calls are
the same:

    humData |> with(Token[lag = 1])
    humData |> with(lag(Token, 1))

This is most useful if the `lag`/`lead` index has *multiple* values: if
the indexed object appears within a higher function call, each lag is
inserted as a *separate* argument to that call. Thus, *these* two calls
are also the same:

    humData |> with(count(Token[lag = 1:2]))
    humData |> with(count(lag(Token, 1), lag(Token, 2)))

Note that the lagging will also be automatically grouped within the
fields `list(Piece, Spine, Path)`, which is the default "melodic"
structure in most data. This assures that a vector is "lagged" from one
piece to another, or from one spine to the next. If you'd like to turn
this off or change the grouping, you need to override it by adding a
`groupby` argument to the lagged index, like
`Token[lag = 1, groupby = list(...)]`.

Using lagged vectors, since they are vectorized, is the fastest
(computationally) and easiest way of working with n-grams. For example,
if you want to create `character`-string 5-grams of your data, you could
call:

    humData |> with(paste(Token[lag = 0:5], sep = '-'))

Since the lagging is grouped by `list(Piece, Spine, Path)`, these are
true "melodic" n-grams, only created within spine-paths within each
piece.

#### Splatted arguments

"Splatting" refers to feeding a function a list/vector of arguments.
Sometimes we want to divide our data into pieces (with
[group_by()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)),
but rather than applying the same expression to each piece, we want to
feed the separate pieces as separate arguments to the same function. You
can use some [syntactic
sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) to do just this.
We can index any field in our call with a `splat` argument, which must
be some `x %in% Field`. For example,

    humData |> with(list(Token[splat = Spine %in% 1:2]))

In this call, the `Token` field will be divided into two groups, one
where `Spine == 1` and the other where `Spine == 2`; the first group
(`Spine == 1`) will be used as the first argument to `list`, and the
second group (`Spine == 2`) as the second argument. Thus, `within`
translates the previous expression to this:

    humData |> within(list(Token[Spine == 1], Token[Spine == 2]))

Splatting can be a little weird, because there is nothing to assure that
the splatted arguments are all the same length, which we usually want
([vectorization](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md)).
For example, in the previous example, there is no guarantee that
`Token[Spine == 1]` and `Token[Spine == 2]` are the same length. This
just means we should only use splatting if we really understand the
groups we are splatting. For example, *if* there are [no spine paths or
stops in our
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md),
*then* we can know that all spines have the same number of data records,
but only including **all** data records (null *and* non-null). So, if I
know there are no stops/paths in our data, we can run something like
this:

    humData |> within(dataTypes = 'Dd',
                      count(Token[splat = Spine %in% 1:2]))

### Saving expressions for later

In some cases you may find that there are certain arguments expressions
that you use repeatedly. You can store expressions as variables by
"quoting" them: the most common way to quote an expression in R is using
the [~](https://rdrr.io/r/base/tilde.html), which creates what is called
a "formula"—essentially a quoted expression. You can also quote
expressions, using [`quote()`](https://rdrr.io/r/base/substitute.html).
Once you've quoted an expression you can pass it to `with()`,
`within()`, `mutate()`, `summarize()`, and `reframe()`.

Imagine that you have three different datasets (`humData1`, `humData2`,
and `humData3`), and you'd like to evaluate the expression
`count(kern(Token, simple = TRUE))` in all three. Use the `~` operator
to quote and save that expression to variable, then use it with
`with()`:

    countKern <- ~count(kern(Token, simple = TRUE))

    humData1 |> with(countKern)
    humData2 |> with(countKern)
    humData3 |> with(countKern)

### Expanding paths

For data that includes spine paths (which you can check with
[`anyPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)),
some analyses may require that spine paths are treated as contiguous
"melodies." The
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)
function can be used to "expand" spine paths into new spines. The
`expandPaths` *argument* to `with()`/`within()` will cause
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)
to be run on your data before evaluating your argument expressions.
After evaluation, the expanded parts of the data are then removed from
the output.

## Parsing expression results

The only differences between the `with()`, `within()`, `mutate()`,
`summarize()`, and `reframe()` humdrumR methods are what they do with
the *results* of expressions passed to them. The major difference is
that `within()`, `mutate()`, and `reframe()` put results into new
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
in a [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
while `with()` and `summarize()` just return their results in "normal"
R. Other differences between the functions simply relate to how the
`recycle` and `drop` arguments are used (details below).

The `recycle` argument controls how the results of your code are, or
aren't, [recycled (or
padded)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md).
When you write code using your [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)'s
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as input, your results are inspected to see how long they are compared
to the length of the input field(s). If any of your results are *longer*
than the input, you'll get an error message—`humdrumR` can't (yet)
handle that case. If any of your results are *shorter* than the input,
the `recycle` argument controls what happens to that result. There are
seven options:

- `"no"`: The result is not recycled or padded. For calls to `within()`,
  `mutate`, or `reframe()`, this option is not allowed.

- `"yes"`: the result is recycled, no matter how long it is.

- `"pad"`: the result is padded with `NA` values.

- `"ifscalar"`: if the result is scalar (length 1), it is recycled;
  otherwise you see an error.

- `"ifeven"`: if the result length evenly divides the input length, it
  is recycled; otherwise you see an error.

- `"never"`: The result can never be recycled. Unlike "no", "never"
  treats differing input and output lengths as an error.

- `"summarize"`: if the result is not scalar, *even if it matches the
  input length*, you see an error. The result is not recycled.

The result of padding/recycling also depends on the `alignLeft`
argument: If `alignLeft = TRUE`, results are padded on the right: like
`c(result, NA, NA, ...)`; If `alignLeft = FALSE`, results are padded on
the left: like `c(..., NA, NA, results)`. Recycling is also affected if
the result's length does not evenly divide the input length. For
example, consider a result `c(1, 2, 3)` which needs to be recycled to
length `10`: If `alignLeft = TRUE`, the result is recycled
`c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)`; If `alignLeft = FALSE`, the result is
recycled `c(3, 1, 2, 3, 1, 2, 3, 1, 2, 3)`.

### with() and summarize()

The humdrumR `with()` and `summarize()` methods return "normal" R data
objects. The only difference between the `with()` and `summarize()`
methods is their default `drop` and `recycle` arguments:

- `with(..., drop = TRUE, recycle = 'no')`

- `summarize(..., drop = FALSE, recycle = 'summarize')`

If `drop = TRUE`, these methods return whatever your code's result is
with no parsing. This can be *any* kind of R data, including
[vectors](https://rdrr.io/r/base/vector.html) or objects like [lm
fits](https://rdrr.io/r/stats/lm.html) or
[tables](https://rdrr.io/r/base/table.html). If `drop = FALSE`, the
results will instead be returned in a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

If you are working with [grouped
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md),
the `drop = FALSE` output (`data.table`) will include all grouping
columns as well as the results of your expressions. If `drop = TRUE`
*and* there is only one result per group, the grouping fields will be
used to generate names for the output vector.

### within(), mutate(), and reframe().

The humdrumR `within()`, `mutate()`, and `reframe()` methods always
return a new [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
with new
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
created from your code results. The only differences between these
methods is their default `recycle` argument and the types of `recycle`
argument they allow:

- `within(..., recycle = 'pad')`

  - Can accept any `recycle` option except `"no"`.

- `mutate(..., recycle = 'ifscalar')`

  - Can only accept `"ifscalar"` or `"never"`.

- `reframe(..., recycle = 'pad')`

  - Can only accept `"pad"` or `"yes"`.

### Creating new humdrumR fields

When running `within()`, `mutate()`, or `reframe()`, new
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
are added to the output [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
These new fields become the [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
in the output. You can explicitly name newly created fields
(recommended), or allow `humdrumR` to automatically name them (details
below). When using `with(..., drop = FALSE)` or
`summarize(..., drop = FALSE)`, the column names of the output
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
are determined in the same way.

Note that `within()`, `mutate()`, and `reframe()` will (attempt to) put
*any* result back into your [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)...even
if it doesn't make much sense. Things will work well with
[vectors](https://rdrr.io/r/base/vector.html). Atomic vectors are
usually the best to work with (i.e., numbers, `character` strings, or
`logical` values), but `list`s will work well too—just remember that
you'll need to treat those fields as lists (e.g., you might need to use
[`lapply()`](https://rdrr.io/r/base/lapply.html) or
[`Map()`](https://rdrr.io/r/base/funprog.html) to work with `list`
fields.) Any *non-vector* result will be put into a list as well, padded
as needed. For example, if you use
[`lm()`](https://rdrr.io/r/stats/lm.html) to compute a linear-regression
in a call to `within()` the result will be a new field containing a
`list`, with first element in the list being a single `lm` fit object,
and the rest of the list empty (padded to the length of the field).

#### Naming new fields

If you don't explicitly name the code expressions you provide, the new
fields are named by capturing the expression code itself as a
`character` string. However, it is generally a better idea to explicitly
name your new fields. This can be done in two ways:

- Base-R [within()](https://rdrr.io/r/base/with.html) style: Use the
  `<-` assignment operator inside your expression.

  - Example: `within(humData, Kern <- kern(Token))`.

- Tidyverse
  [mutate()](https://dplyr.tidyverse.org/reference/mutate.html) style:
  provide the expression as a named argument with `=`.

  - Example: `mutate(humData, Kern = kern(Token))`.

Either style can be used with any of the `humdrumR` methods. When using
`<-`, only top-level assignment will create a new field, which means
only one field can be assigned per expression. For example,

    within(humData,
           Semits <- semits(Token),
           Recip <- recip(Token))

will create two fields (`Semits` and `Recip`). However,

    within(humData,
           {
             Semits <- semits(Token)
             Recip <- recip(Token)
            })

will not. The [result of
expressions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
grouped by [`{}`](https://rdrr.io/r/base/Paren.html) is always the
*last* expression in the brackets. Thus, the last example above will
only create one new field, corresponding to the result of
`recip(Token)`. However, the resulting field won't be called `Recip`!
This is because only *top-level* assignments are used to name an
expression: To name a multi-expression expression (using
[`{}`](https://rdrr.io/r/base/Paren.html)), you could do something like
this:

    within(humData,
           Recip <- {
             Semits <- semits(Token)
             recip(Token)
            })

Of course, only the result of `recip(Token)` would be saved to `Recip`,
so the `Semits <- semits(Token)` expression is doing nothing useful
here.

### Returning multiple fields

Sometimes, you might need to create a single expression which returns
two or more fields. To do this, simply return a `data.frame` (or
`tibble` or `data.table`). Each column of the `data.frame` will return
as it's own field. If the `data.frame` columns are named, those names
will be used as field names. If the `data.frame` columns are named *and*
the whole expression is named, the expression name is pasted to the
front of the column names.

The following example will create two fields, named `Pitch.Simple` and
`Pitch.Complex`.

    within(humData, Pitch = data.frame(Simple = kern(Token, simple = TRUE),
                                       Complex = kern(Token, simple = FALSE)))

### Piped references

All argument expressions passed to the `with()`/`within()` methods are
evaluated in order, from left to right, so any assignments in a previous
expression will be visible to the next expression. This means we can,
for example, do this:

    within(humData,
           Kern <- kern(Token),
           KernN <- paste0(Kern, nchar(Kern)))

the use of `Kern` in the second expression will refer to the `Kern`
assigned in the previous expression.

Sometimes you might want to save the result of an expression *only* to
use in a later expression, and *not* to save into a field. You can do
this be assigning to any field name that begins with `.`. For example,
if you only wanted the kern token with pasted length (`KernN` above),
you could do this:

    within(humData,
           .Kern <- kern(Token),
           KernN <- paste0(.Kern, nchar(.Kern)))

The object `.Kern` is visible in the second expression, but doesn't get
saved into a field.

## Evaluating expressions in groups or windows

The `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()`
functions all work with
[grouped](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
data, or data with [contextual
windows](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
defined. When groups or windows are defined, all argument expressions
are evaluated independently within each and every group/window. Results
are then processed (including recycling/padding) within each
group/window. Finally, the results are then pieced back together in
locations corresponding to the original data locations. Since
[groups](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
are necessarily exhaustive and non-overlapping, the results location are
easy to understand. On the other hand [contextual
windows](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
may overlap, which means any non-scalar results could potentially
overlap as well; in these cases, it may be hard to predict which data
lands where.

## See also

These functions are most useful in combination with the
[subset()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
[group_by()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md),
and
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
commands.

## Examples

``` r

# with/within style:

humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpGCgWwF/temp_libpath2869ab26c00287/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

humData |> with(count(kern(Token, simple = TRUE), Spine))
#> humdrumR count distribution 
#> humdrumR:::kern(Token, simple = TRUE, Exclusive = Exclusive,   Spine   n
#>                                                             c      1  12
#>                                                            c#      1  17
#>                                                             d      1  25
#>                                                            d#      1   7
#>                                                            e-      1   .
#>                                                             e      1  43
#>                                                            e#      1   1
#>                                                             f      1   3
#>                                                            f#      1  20
#>                                                             g      1  23
#>                                                            g#      1  11
#>                                                            a-      1   .
#>                                                             a      1  29
#>                                                            a#      1   1
#>                                                            b-      1   .
#>                                                             b      1  33
#>                                                             c      2  22
#>                                                            c#      2  17
#>                                                             d      2  42
#>                                                            d#      2   4
#>                                                            e-      2   .
#>                                                             e      2  37
#>                                                            e#      2   .
#>                                                             f      2   1
#>                                                            f#      2  18
#>                                                             g      2   4
#>                                                            g#      2  13
#>                                                            a-      2   .
#>                                                             a      2  17
#>                                                            a#      2   3
#>                                                            b-      2   1
#>                                                             b      2  37
#>                                                             c      3   .
#>                                                            c#      3   5
#>                                                             d      3  12
#>                                                            d#      3  10
#>                                                            e-      3   .
#>                                                             e      3  46
#>                                                            e#      3   1
#>                                                             f      3   3
#>                                                            f#      3  41
#>                                                             g      3  24
#>                                                            g#      3  25
#>                                                            a-      3   .
#>                                                             a      3  27
#>                                                            a#      3   3
#>                                                            b-      3   .
#>                                                             b      3  15
#>                                                             c      4  16
#>                                                            c#      4  16
#>                                                             d      4  19
#>                                                            d#      4   4
#>                                                            e-      4   .
#>                                                             e      4  16
#>                                                            e#      4   .
#>                                                             f      4   .
#>                                                            f#      4   8
#>                                                             g      4  13
#>                                                            g#      4   9
#>                                                            a-      4   .
#>                                                             a      4  33
#>                                                            a#      4   2
#>                                                            b-      4   .
#>                                                             b      4  53
#> humdrumR:::kern(Token, simple = TRUE, Exclusive = Exclusive,   Spine   n
#> humdrumR count distribution 

humData |> within(Kern <- kern(Token), 
                  Recip <- recip(Token),
                  Semits <- semits(Token)) -> humData
                  
humData |> 
    group_by(Spine) |>
    with(mean(Semits))
#>     Spine1     Spine2     Spine3     Spine4 
#> -9.2844444  0.3425926  5.7500000 10.5661376 
    
humData |> 
    group_by(Piece, Spine) |>
    with(mean(Semits), drop = FALSE)
#>     humdrumR:::mean(Semits) Piece Spine
#>                       <num> <int> <int>
#>  1:             -12.4126984     1     1
#>  2:               0.2881356     1     2
#>  3:               5.5737705     1     3
#>  4:              10.4347826     1     4
#>  5:              -8.1639344     2     1
#>  6:               0.7903226     2     2
#>  7:               6.3818182     2     3
#>  8:              10.7358491     2     4
#>  9:              -7.7115385     3     1
#> 10:              -0.1200000     3     2
#> 11:               5.6170213     3     3
#> 12:              10.8297872     3     4
#> 13:              -8.3265306     4     1
#> 14:               0.3111111     4     2
#> 15:               5.3877551     4     3
#> 16:              10.2093023     4     4
    
# tidyverse (dplyr) style:

humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpGCgWwF/temp_libpath2869ab26c00287/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

humData |> mutate(Kern = kern(Token), 
                  Recip = recip(Token),
                  Semits = semits(Token)) -> humData
                  
humData |> 
    group_by(Spine, Bar) |>
    summarize(mean(Semits))
#>     humdrumR:::mean(Semits)   Bar Spine
#>                       <num> <int> <int>
#>  1:             -8.00000000     0     1
#>  2:             -6.05882353     1     1
#>  3:             -9.00000000     2     1
#>  4:             -7.71428571     3     1
#>  5:            -10.33333333     4     1
#>  6:            -12.50000000     5     1
#>  7:             -7.19047619     6     1
#>  8:             -9.81250000     7     1
#>  9:             -8.35294118     8     1
#> 10:            -10.64285714     9     1
#> 11:             -9.75000000    10     1
#> 12:            -11.57142857    11     1
#> 13:             -6.44444444    12     1
#> 14:             -8.22222222    13     1
#> 15:            -15.40000000    14     1
#> 16:            -14.50000000    15     1
#> 17:            -16.66666667    16     1
#> 18:            -13.33333333    17     1
#> 19:            -12.50000000    18     1
#> 20:             -7.50000000    19     1
#> 21:             -6.33333333    20     1
#> 22:            -10.00000000    21     1
#> 23:            -17.00000000    22     1
#> 24:              2.00000000     0     2
#> 25:              0.38095238     1     2
#> 26:             -0.05555556     2     2
#> 27:              0.43750000     3     2
#> 28:             -2.30769231     4     2
#> 29:             -2.50000000     5     2
#> 30:              0.40000000     6     2
#> 31:              1.14285714     7     2
#> 32:              1.81250000     8     2
#> 33:             -0.14285714     9     2
#> 34:             -0.63157895    10     2
#> 35:             -0.21428571    11     2
#> 36:              1.30000000    12     2
#> 37:              2.44444444    13     2
#> 38:              1.50000000    14     2
#> 39:              1.00000000    15     2
#> 40:              0.25000000    16     2
#> 41:              1.33333333    17     2
#> 42:              0.33333333    18     2
#> 43:              2.00000000    19     2
#> 44:              3.00000000    20     2
#> 45:              2.00000000    21     2
#> 46:             -1.00000000    22     2
#> 47:              5.50000000     0     3
#> 48:              6.47058824     1     3
#> 49:              5.18750000     2     3
#> 50:              5.47368421     3     3
#> 51:              3.91666667     4     3
#> 52:              6.00000000     5     3
#> 53:              6.88888889     6     3
#> 54:              6.06250000     7     3
#> 55:              6.47058824     8     3
#> 56:              5.06250000     9     3
#> 57:              4.38888889    10     3
#> 58:              5.00000000    11     3
#> 59:              6.70000000    12     3
#> 60:              8.00000000    13     3
#> 61:              5.33333333    14     3
#> 62:              5.50000000    15     3
#> 63:              7.33333333    16     3
#> 64:              6.50000000    17     3
#> 65:              5.40000000    18     3
#> 66:              6.50000000    19     3
#> 67:              7.33333333    20     3
#> 68:              6.50000000    21     3
#> 69:              2.00000000    22     3
#> 70:              9.50000000     0     4
#> 71:             11.00000000     1     4
#> 72:             10.37500000     2     4
#> 73:              9.42105263     3     4
#> 74:              9.54545455     4     4
#> 75:             12.40000000     5     4
#> 76:             10.56250000     6     4
#> 77:             10.93333333     7     4
#> 78:             11.37500000     8     4
#> 79:              9.46153846     9     4
#> 80:             11.40000000    10     4
#> 81:              9.92307692    11     4
#> 82:             12.83333333    12     4
#> 83:             11.75000000    13     4
#> 84:             10.00000000    14     4
#> 85:              9.00000000    15     4
#> 86:             13.00000000    16     4
#> 87:             10.00000000    17     4
#> 88:              9.00000000    18     4
#> 89:             10.00000000    19     4
#> 90:             13.00000000    20     4
#> 91:             10.00000000    21     4
#> 92:              7.00000000    22     4
#>     humdrumR:::mean(Semits)   Bar Spine
#>                       <num> <int> <int>
      
# dataTypes argument

humData |>
   group_by(Piece, Spine) |>
   within(paste(Token, seq_along(Token)))
#> ######################## vvv chor001.krn vvv #########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:           **kern         **kern         **kern         **kern
#>             9:           *ICvox         *ICvox         *ICvox         *ICvox
#>            10:           *Ibass        *Itenor         *Ialto        *Isoprn
#>            11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
#>            12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>            13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>            14:              *>A            *>A            *>A            *>A
#>            15:          *clefF4       *clefGv2        *clefG2        *clefG2
#>            16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
#>            17:              *G:            *G:            *G:            *G:
#>            18:            *M3/4          *M3/4          *M3/4          *M3/4
#>            19:           *MM100         *MM100         *MM100         *MM100
#>            20:            4GG 1           4B 1           4d 1           4g 1
#>            21:               =1             =1             =1             =1
#>            22:             4G 2           4B 2           4d 2           2g 2
#>            23:             4E 3          8cL 3           4e 3              .
#>            24:                .          8BJ 4              .              .
#>            25:            4F# 4           4A 5           4d 4          4dd 3
#>            26:               =2             =2             =2             =2
#>            27:             4G 5           4G 6           2d 5          4.b 4
#>            28:             4D 6          4F# 7              .              .
#>            29:                .              .              .           8a 5
#>            30:             4E 7           4G 8           4B 6           4g 6
#>            31:               =3             =3             =3             =3
#>            32:             4C 8          8cL 9          8eL 7          4.g 7
#>            33:                .         8BJ 10           8d 8              .
#>            34:           8BBL 9          4c 11           8e 9              .
#>            35:          8AAJ 10              .        8f#J 10           8a 8
#>            36:           4GG 11          4d 12          4g 11           4b 9
#>            37:               =4             =4             =4             =4
#>            38:           2D; 12         2d; 13        2f#; 12         2a; 10
#>            39:           4GG 13          4d 14          4g 13          4b 11
#>            40:               =5             =5             =5             =5
#>            41:          4FF# 14          4A 15          4d 14         2dd 12
#>            42:           4GG 15          4B 16          4e 15              .
#>            43:           4AA 16          4c 17         4f# 16         4cc 13
#>            44:               =6             =6             =6             =6
#>            45:           4BB 17          4d 18          2g 17          4b 14
#>            46:            4C 18          4e 19              .          2a 15
#>            47:            4D 19         8dL 20         4f# 18              .
#>            48:                .         8cJ 21              .              .
#>            49:               =7             =7             =7             =7
#>            50:          2GG; 20         2B; 22         2d; 19         2g; 16
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:            4E 22         4G# 22         8eL 22          4b 18
#>            55:                .              .        8f#J 23              .
#>            56:               =5             =5             =5             =5
#>            57:           4C# 23         4c# 23        8g#L 24         4ee 19
#>            58:                .              .        8a#J 25              .
#>            59:          8D#L 24         4F# 24          4b 26        4dd# 20
#>            60:           8EJ 25              .              .              .
#>            61:           4F# 26         4f# 25         4a# 27        4cc# 21
#>            62:            4B 27         4f# 26          4b 28        4dd# 22
#>            63:               =6             =6             =6             =6
#>            64:          8G#L 28          4B 27          4b 29        8eeL 23
#>            65:           8EJ 29              .              .       8dd#J 24
#>            66:           4F# 30         4A# 28         4f# 30        4cc# 25
#>            67:          4BB; 31        4d#; 29        4f#; 31         4b; 26
#>            68:            4E 32          4e 30         4g# 32          4b 27
#>            69:               =7             =7             =7             =7
#>            70:           4G# 33          4B 31         8eL 33         4ee 28
#>            71:                .              .        8f#J 34              .
#>            72:            4E 34          4e 32         4g# 35          4b 29
#>            73:         8AnXL 35          4e 33          4a 36        4cc# 30
#>            74:           8BJ 36              .              .              .
#>            75:           4c# 37         4c# 34         8eL 37        8g#L 31
#>            76:                .              .        8f#J 38         8aJ 32
#>            77:               =8             =8             =8             =8
#>            78:           4E# 38          4d 35         4g# 39          4b 33
#>            79:           4F# 39         4c# 36         4f# 40          4a 34
#>            80:          4C#; 40        4c#; 37        4e#; 41        4g#; 35
#>            81:            4E 41         [4B 38          4e 42         4g# 36
#>            82:               =9             =9             =9             =9
#>            83:           4BB 42         4B] 39         4d# 43         4f# 37
#>            84:          8C#L 43          4e 40         4c# 44          4a 38
#>            85:          8D#J 44              .              .              .
#>            86:            4E 45          4e 41          4B 45         4g# 39
#>            87:           4BB 46        8d#L 42          4B 46         4f# 40
#>            88:                .         8BJ 43              .              .
#>            89:              =10            =10            =10            =10
#>            90:          4AA# 47         2F# 44         4c# 47         4c# 41
#>            91:           4BB 48              .          4B 48         4d# 42
#>            92:          4EE; 49        4G#; 45         4B; 49         4e; 43
#>            93:               ==             ==             ==             ==
#>            94:               *-             *-             *-             *-
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ #########################
#>               (***four global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>           Kern                           :: character (**kern tokens)
#>           Recip                          :: character (**recip tokens)
#>           Semits                         :: integer (**semits tokens)
#>           Token                          :: character
#>          *paste(Token, seq_along(Token)) :: character
#> 
#>    Grouping fields: (eight groups)
#>           Piece                          :: integer
#>           Spine                          :: integer
#> 
   
humData |>
   group_by(Piece, Spine) |>
   mutate(Enumerated = paste(Token, seq_along(Token)),
          dataTypes = 'Dd')
#> ######################## vvv chor001.krn vvv #########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:           **kern         **kern         **kern         **kern
#>             9:           *ICvox         *ICvox         *ICvox         *ICvox
#>            10:           *Ibass        *Itenor         *Ialto        *Isoprn
#>            11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
#>            12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>            13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>            14:              *>A            *>A            *>A            *>A
#>            15:          *clefF4       *clefGv2        *clefG2        *clefG2
#>            16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
#>            17:              *G:            *G:            *G:            *G:
#>            18:            *M3/4          *M3/4          *M3/4          *M3/4
#>            19:           *MM100         *MM100         *MM100         *MM100
#>            20:            4GG 1           4B 1           4d 1           4g 1
#>            21:               =1             =1             =1             =1
#>            22:             4G 2           4B 2           4d 2           2g 2
#>            23:             4E 3          8cL 3           4e 3            . 3
#>            24:              . 4          8BJ 4            . 4            . 4
#>            25:            4F# 5           4A 5           4d 5          4dd 5
#>            26:               =2             =2             =2             =2
#>            27:             4G 6           4G 6           2d 6          4.b 6
#>            28:             4D 7          4F# 7            . 7            . 7
#>            29:              . 8            . 8            . 8           8a 8
#>            30:             4E 9           4G 9           4B 9           4g 9
#>            31:               =3             =3             =3             =3
#>            32:            4C 10         8cL 10         8eL 10         4.g 10
#>            33:             . 11         8BJ 11          8d 11           . 11
#>            34:          8BBL 12          4c 12          8e 12           . 12
#>            35:          8AAJ 13           . 13        8f#J 13          8a 13
#>            36:           4GG 14          4d 14          4g 14          4b 14
#>            37:               =4             =4             =4             =4
#>            38:           2D; 15         2d; 15        2f#; 15         2a; 15
#>            39:           4GG 16          4d 16          4g 16          4b 16
#>            40:               =5             =5             =5             =5
#>            41:          4FF# 17          4A 17          4d 17         2dd 17
#>            42:           4GG 18          4B 18          4e 18           . 18
#>            43:           4AA 19          4c 19         4f# 19         4cc 19
#>            44:               =6             =6             =6             =6
#>            45:           4BB 20          4d 20          2g 20          4b 20
#>            46:            4C 21          4e 21           . 21          2a 21
#>            47:            4D 22         8dL 22         4f# 22           . 22
#>            48:             . 23         8cJ 23           . 23           . 23
#>            49:               =7             =7             =7             =7
#>            50:          2GG; 24         2B; 24         2d; 24         2g; 24
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:            4E 29         4G# 29         8eL 29          4b 29
#>            55:             . 30           . 30        8f#J 30           . 30
#>            56:               =5             =5             =5             =5
#>            57:           4C# 31         4c# 31        8g#L 31         4ee 31
#>            58:             . 32           . 32        8a#J 32           . 32
#>            59:          8D#L 33         4F# 33          4b 33        4dd# 33
#>            60:           8EJ 34           . 34           . 34           . 34
#>            61:           4F# 35         4f# 35         4a# 35        4cc# 35
#>            62:            4B 36         4f# 36          4b 36        4dd# 36
#>            63:               =6             =6             =6             =6
#>            64:          8G#L 37          4B 37          4b 37        8eeL 37
#>            65:           8EJ 38           . 38           . 38       8dd#J 38
#>            66:           4F# 39         4A# 39         4f# 39        4cc# 39
#>            67:          4BB; 40        4d#; 40        4f#; 40         4b; 40
#>            68:            4E 41          4e 41         4g# 41          4b 41
#>            69:               =7             =7             =7             =7
#>            70:           4G# 42          4B 42         8eL 42         4ee 42
#>            71:             . 43           . 43        8f#J 43           . 43
#>            72:            4E 44          4e 44         4g# 44          4b 44
#>            73:         8AnXL 45          4e 45          4a 45        4cc# 45
#>            74:           8BJ 46           . 46           . 46           . 46
#>            75:           4c# 47         4c# 47         8eL 47        8g#L 47
#>            76:             . 48           . 48        8f#J 48         8aJ 48
#>            77:               =8             =8             =8             =8
#>            78:           4E# 49          4d 49         4g# 49          4b 49
#>            79:           4F# 50         4c# 50         4f# 50          4a 50
#>            80:          4C#; 51        4c#; 51        4e#; 51        4g#; 51
#>            81:            4E 52         [4B 52          4e 52         4g# 52
#>            82:               =9             =9             =9             =9
#>            83:           4BB 53         4B] 53         4d# 53         4f# 53
#>            84:          8C#L 54          4e 54         4c# 54          4a 54
#>            85:          8D#J 55           . 55           . 55           . 55
#>            86:            4E 56          4e 56          4B 56         4g# 56
#>            87:           4BB 57        8d#L 57          4B 57         4f# 57
#>            88:             . 58         8BJ 58           . 58           . 58
#>            89:              =10            =10            =10            =10
#>            90:          4AA# 59         2F# 59         4c# 59         4c# 59
#>            91:           4BB 60           . 60          4B 60         4d# 60
#>            92:          4EE; 61        4G#; 61         4B; 61         4e; 61
#>            93:               ==             ==             ==             ==
#>            94:               *-             *-             *-             *-
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ #########################
#>               (***four global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Enumerated :: character
#>           Kern       :: character (**kern tokens)
#>           Recip      :: character (**recip tokens)
#>           Semits     :: integer (**semits tokens)
#>           Token      :: character
#> 
#>    Grouping fields: (eight groups)
#>           Piece      :: integer
#>           Spine      :: integer
#> 
          
# recycle argument

humData |>
   group_by(Piece, Bar, Spine) |>
   mutate(BarMean = mean(Semits), recycle = 'ifscalar')
#> ######################## vvv chor001.krn vvv ########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:               **kern             **kern              **kern    ***
#>             9:               *ICvox             *ICvox              *ICvox    ***
#>            10:               *Ibass            *Itenor              *Ialto    ***
#>            11:              *I"Bass           *I"Tenor             *I"Alto    ***
#>            12:            *>[A,A,B]          *>[A,A,B]           *>[A,A,B]    ***
#>            13:         *>norep[A,B]       *>norep[A,B]        *>norep[A,B]    ***
#>            14:                  *>A                *>A                 *>A    ***
#>            15:              *clefF4           *clefGv2             *clefG2    ***
#>            16:               *k[f#]             *k[f#]              *k[f#]    ***
#>            17:                  *G:                *G:                 *G:    ***
#>            18:                *M3/4              *M3/4               *M3/4    ***
#>            19:               *MM100             *MM100              *MM100    ***
#>            20:                  -17                 -1                   2    ***
#>            21:                   =1                 =1                  =1    ***
#>            22:    -6.33333333333333              -1.25    2.66666666666667    ***
#>            23:    -6.33333333333333              -1.25    2.66666666666667    ***
#>            24:                    .              -1.25                   .    ***
#>            25:    -6.33333333333333              -1.25    2.66666666666667    ***
#>            26:                   =2                 =2                  =2    ***
#>            27:    -7.66666666666667  -5.33333333333333                 0.5    ***
#>            28:    -7.66666666666667  -5.33333333333333                   .    ***
#>            29:                    .                  .                   .    ***
#>            30:    -7.66666666666667  -5.33333333333333                 0.5    ***
#>            31:                   =3                 =3                  =3    ***
#>            32:               -14.25               0.25                 4.6    ***
#>            33:                    .               0.25                 4.6    ***
#>            34:               -14.25               0.25                 4.6    ***
#>            35:               -14.25                  .                 4.6    ***
#>            36:               -14.25               0.25                 4.6    ***
#>            37:                   =4                 =4                  =4    ***
#>            38:                -13.5                  2                 6.5    ***
#>            39:                -13.5                  2                 6.5    ***
#>            40:                   =5                 =5                  =5    ***
#>            41:    -16.6666666666667  -1.33333333333333                   4    ***
#>            42:    -16.6666666666667  -1.33333333333333                   4    ***
#>            43:    -16.6666666666667  -1.33333333333333                   4    ***
#>            44:                   =6                 =6                  =6    ***
#>            45:    -11.6666666666667                  2                 6.5    ***
#>            46:    -11.6666666666667                  2                   .    ***
#>            47:    -11.6666666666667                  2                 6.5    ***
#>            48:                    .                  2                   .    ***
#>            49:                   =7                 =7                  =7    ***
#>            50:                  -17                 -1                   2    ***
#> 51-133:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ ########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv ########################
#>   1-53:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:                   -8                 -4                   5    ***
#>            55:                    .                  .                   5    ***
#>            56:                   =5                 =5                  =5    ***
#>            57:                   -7               1.75                  10    ***
#>            58:                    .                  .                  10    ***
#>            59:                   -7               1.75                  10    ***
#>            60:                   -7                  .                   .    ***
#>            61:                   -7               1.75                  10    ***
#>            62:                   -7               1.75                  10    ***
#>            63:                   =6                 =6                  =6    ***
#>            64:                 -7.8                  1                7.75    ***
#>            65:                 -7.8                  .                   .    ***
#>            66:                 -7.8                  1                7.75    ***
#>            67:                 -7.8                  1                7.75    ***
#>            68:                 -7.8                  1                7.75    ***
#>            69:                   =7                 =7                  =7    ***
#>            70:                   -3                  2    6.16666666666667    ***
#>            71:                    .                  .    6.16666666666667    ***
#>            72:                   -3                  2    6.16666666666667    ***
#>            73:                   -3                  2    6.16666666666667    ***
#>            74:                   -3                  .                   .    ***
#>            75:                   -3                  2    6.16666666666667    ***
#>            76:                    .                  .    6.16666666666667    ***
#>            77:                   =8                 =8                  =8    ***
#>            78:                   -8               0.75                5.75    ***
#>            79:                   -8               0.75                5.75    ***
#>            80:                   -8               0.75                5.75    ***
#>            81:                   -8               0.75                5.75    ***
#>            82:                   =9                 =9                  =9    ***
#>            83:                -10.8                1.8                 0.5    ***
#>            84:                -10.8                1.8                 0.5    ***
#>            85:                -10.8                  .                   .    ***
#>            86:                -10.8                1.8                 0.5    ***
#>            87:                -10.8                1.8                 0.5    ***
#>            88:                    .                1.8                   .    ***
#>            89:                  =10                =10                 =10    ***
#>            90:    -15.6666666666667                 -5  -0.333333333333333    ***
#>            91:    -15.6666666666667                  .  -0.333333333333333    ***
#>            92:    -15.6666666666667                 -5  -0.333333333333333    ***
#>            93:                   ==                 ==                  ==    ***
#>            94:                   *-                 *-                  *-    ***
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebast***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf un***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach,***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc.***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ ########################
#>                (***one spine/path not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *BarMean :: numeric
#>           Kern    :: character (**kern tokens)
#>           Recip   :: character (**recip tokens)
#>           Semits  :: integer (**semits tokens)
#>           Token   :: character
#> 
#>    Grouping fields: (140 groups)
#>           Bar     :: integer
#>           Piece   :: integer
#>           Spine   :: integer
#> 
   
humData |>
   group_by(Piece, Bar, Spine) |>
   within(BarMean = mean(Semits), recycle = 'pad')  
#> ######################## vvv chor001.krn vvv ########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:               **kern             **kern              **kern    ***
#>             9:               *ICvox             *ICvox              *ICvox    ***
#>            10:               *Ibass            *Itenor              *Ialto    ***
#>            11:              *I"Bass           *I"Tenor             *I"Alto    ***
#>            12:            *>[A,A,B]          *>[A,A,B]           *>[A,A,B]    ***
#>            13:         *>norep[A,B]       *>norep[A,B]        *>norep[A,B]    ***
#>            14:                  *>A                *>A                 *>A    ***
#>            15:              *clefF4           *clefGv2             *clefG2    ***
#>            16:               *k[f#]             *k[f#]              *k[f#]    ***
#>            17:                  *G:                *G:                 *G:    ***
#>            18:                *M3/4              *M3/4               *M3/4    ***
#>            19:               *MM100             *MM100              *MM100    ***
#>            20:                  -17                 -1                   2    ***
#>            21:                   =1                 =1                  =1    ***
#>            22:    -6.33333333333333              -1.25    2.66666666666667    ***
#>            23:                    .                  .                   .    ***
#>            24:                    .                  .                   .    ***
#>            25:                    .                  .                   .    ***
#>            26:                   =2                 =2                  =2    ***
#>            27:    -7.66666666666667  -5.33333333333333                 0.5    ***
#>            28:                    .                  .                   .    ***
#>            29:                    .                  .                   .    ***
#>            30:                    .                  .                   .    ***
#>            31:                   =3                 =3                  =3    ***
#>            32:               -14.25               0.25                 4.6    ***
#>            33:                    .                  .                   .    ***
#>            34:                    .                  .                   .    ***
#>            35:                    .                  .                   .    ***
#>            36:                    .                  .                   .    ***
#>            37:                   =4                 =4                  =4    ***
#>            38:                -13.5                  2                 6.5    ***
#>            39:                    .                  .                   .    ***
#>            40:                   =5                 =5                  =5    ***
#>            41:    -16.6666666666667  -1.33333333333333                   4    ***
#>            42:                    .                  .                   .    ***
#>            43:                    .                  .                   .    ***
#>            44:                   =6                 =6                  =6    ***
#>            45:    -11.6666666666667                  2                 6.5    ***
#>            46:                    .                  .                   .    ***
#>            47:                    .                  .                   .    ***
#>            48:                    .                  .                   .    ***
#>            49:                   =7                 =7                  =7    ***
#>            50:                  -17                 -1                   2    ***
#> 51-133:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ ########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv ########################
#>   1-53:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:                   -8                 -4                   5    ***
#>            55:                    .                  .                   .    ***
#>            56:                   =5                 =5                  =5    ***
#>            57:                   -7               1.75                  10    ***
#>            58:                    .                  .                   .    ***
#>            59:                    .                  .                   .    ***
#>            60:                    .                  .                   .    ***
#>            61:                    .                  .                   .    ***
#>            62:                    .                  .                   .    ***
#>            63:                   =6                 =6                  =6    ***
#>            64:                 -7.8                  1                7.75    ***
#>            65:                    .                  .                   .    ***
#>            66:                    .                  .                   .    ***
#>            67:                    .                  .                   .    ***
#>            68:                    .                  .                   .    ***
#>            69:                   =7                 =7                  =7    ***
#>            70:                   -3                  2    6.16666666666667    ***
#>            71:                    .                  .                   .    ***
#>            72:                    .                  .                   .    ***
#>            73:                    .                  .                   .    ***
#>            74:                    .                  .                   .    ***
#>            75:                    .                  .                   .    ***
#>            76:                    .                  .                   .    ***
#>            77:                   =8                 =8                  =8    ***
#>            78:                   -8               0.75                5.75    ***
#>            79:                    .                  .                   .    ***
#>            80:                    .                  .                   .    ***
#>            81:                    .                  .                   .    ***
#>            82:                   =9                 =9                  =9    ***
#>            83:                -10.8                1.8                 0.5    ***
#>            84:                    .                  .                   .    ***
#>            85:                    .                  .                   .    ***
#>            86:                    .                  .                   .    ***
#>            87:                    .                  .                   .    ***
#>            88:                    .                  .                   .    ***
#>            89:                  =10                =10                 =10    ***
#>            90:    -15.6666666666667                 -5  -0.333333333333333    ***
#>            91:                    .                  .                   .    ***
#>            92:                    .                  .                   .    ***
#>            93:                   ==                 ==                  ==    ***
#>            94:                   *-                 *-                  *-    ***
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebast***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf un***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach,***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc.***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ ########################
#>                (***one spine/path not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *BarMean :: numeric
#>           Kern    :: character (**kern tokens)
#>           Recip   :: character (**recip tokens)
#>           Semits  :: integer (**semits tokens)
#>           Token   :: character
#> 
#>    Grouping fields: (140 groups)
#>           Bar     :: integer
#>           Piece   :: integer
#>           Spine   :: integer
#> 



```
