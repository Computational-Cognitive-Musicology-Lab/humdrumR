# Regular expression method dispatch and function application

The
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
**regular-expression method dispatch system** is a simple system for
making new functions which can be smartly applied to a variety of
character strings. Humdrum dispatch works like normal R method dispatch,
but instead of dispatching specific methods based on their class
(`integer`, `character`, etc.) it dispatches based on regular
expressions. In addition, exclusive interpretations can be used to guide
dispatch.

## Usage

``` r
humdrumDispatch(
  x,
  dispatchDF,
  Exclusive = NULL,
  funcName = NULL,
  regexApply = TRUE,
  multiDispatch = FALSE,
  ...,
  outputClass = "character"
)

exclusiveDispatch(
  x,
  dispatchDF,
  Exclusive,
  regexApply = TRUE,
  outputClass = "character",
  inPlace = FALSE,
  ...
)

makeDispatchDF(...)

makeHumdrumDispatcher(
  ...,
  funcName = "humdrum-dispatch",
  outputClass = "character",
  args = alist(),
  memoize = TRUE,
  dispatch.attr = TRUE
)

# S3 method for class 'humdrumDispatch'
print(x)
```

## Arguments

- dispatchDF:

  ***A data.frame which describes what function should be called for
  which regex input. (See details).***

  Must be a `data.frame`.

- Exclusive:

  ***Exclusive interpretations to dispatch.***

  Defaults to `NULL`.

  If `NULL`, only the regexes are used for dispatch.

- multiDispatch:

  ***Whether to use multiple dispatch function for each
  interpretation.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `FALSE` the "best" regex/exclusive match is dispatched for each
  Exclusive segment. If `TRUE`, differenet functions can be dispatched
  within the same input vector.

- ...:

  ***Arguments to pass to dispatch functions.***

- outputClass:

  ***The default output class which the function should return.***

  Defaults to `"character"`.

  Must be single `character` string.

  Generally, to make sense, all dispatched functions should return the
  same type, which you should explicitly indicate with the `outputClass`
  argument. Dispatch functions should also be
  [vectorized](https://rdrr.io/r/base/Vectorize.html).

- str:

  ***The input strings, on which dispatch is called.***

  Must be `character`.

## Details

Many `humdrumR` functions are in fact, humdrum-dispatch functions: for
example,
[`tonalInterval.character()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
If you call `tonalInterval('ee-')`, the function will recognize that the
input string is a token in the `**kern` representation, and call the
appropriate parser. If you instead call `tonalInterval('me')`, the
function will recognize that the input string is a token in the
`**solfa` representation, and call the appropriate parser for that.

### dispatchDF

The `dispatchDF` must be a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
created using the `makeDispatchDF` function. `makeDispatchDF` takes one
or more arguments, each a list with three components (ordered, not
nameed):

1.  A character vector of exclusive interpretations. (Specify `"any"` if
    you don't want exclusive dispatch).

2.  A regular expression (character string) or a function which can
    generate a regular expression, which accepts `...` arguments at the
    time of dispatch.

3.  A function to dispatch.

## makeHumdrumDispatcher

`makeHumdrumDispatcher` is a function which creates a new function which
automatically performs humdrum-dispatch. A number of important
`humdrumR` functions are created with `makeHumdrumDispatcher`:

- `tonalInterval.character`

- `diatonicSet.character`

- `tertianSet.character`

- `rhythmInterval.character`

## Examples

``` r
u <- c('A', 'B', 'CD', 'E', 'F', 'gh', 'L', 'KX')
l <- c('a', 'b', 'cd', 'e', 'f', 'gh', 'l', 'kx')

lowercasefunc <- \(x) 5L - nchar(x)

humdrumDispatch(l, outputClass = 'integer',
                makeDispatchDF(list('any', '[a-z]+',  lowercasefunc),
                               list('any', '[A-Z]+',  nchar)))
#> [1] 4 4 3 4 4 3 4 3
#> attr(,"dispatch")
#> attr(,"dispatch")$Original
#> [1] "a"  "b"  "cd" "e"  "f"  "gh" "l"  "kx"
#> 
#> attr(,"dispatch")$Regexes
#>   [a-z]+ 
#> "[a-z]+" 
#> 
#> attr(,"dispatch")$Segments
#> [1] 1
#> 
#> attr(,"dispatch")$Exclusives
#> [1] "any"
#> 
 # lowercasefunc will be called on l, nchar on u
```
