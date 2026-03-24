# Parsing rhythm information

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a easy-to-use but powerful system for *parsing* rhythm (time
duration) information: various basic rhythm representations (including
`numeric` and `character`-string representations) can be "parsed"—read
and interpreted by `humdrumR`. For the most part, parsing automatically
happens "behind the scenes" whenever you use any humdrumR [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
like
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md),
[`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md), or
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md).

## Usage

``` r
rhythmInterval(x, ...)

# Default S3 method
rhythmInterval(x, ...)

# S3 method for class 'integer'
rhythmInterval(x, ...)

# S3 method for class '`NULL`'
rhythmInterval(x, ...)

# S3 method for class 'numeric'
rhythmInterval(x, ..., Exclusive = NULL, multiDispatch = FALSE)

# S3 method for class 'character'
rhythmInterval(x, ..., Exclusive = NULL, multiDispatch = FALSE)

# S3 method for class 'factor'
rhythmInterval(x, Exclusive = NULL, ...)

# S3 method for class 'token'
rhythmInterval(x, Exclusive = NULL, ...)
```

## Details

The underlying parser used by all `humdrumR` [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
can be called explicitly using the function `rhythmInterval()`. The
`rhythmInterval` parser will attempt to parse any input information into
a [rational
number](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
object. When you use one of the main [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md),
like
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
or [`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md),
the input is parsed into a
[rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
object, then immediately
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md)
to the representation you asked for (e.g., `**recip` or `**dur`). Thus,
the underlying pipeline for `humdrumR` [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
looks something like:

- **Input** representation (e.g., `**recip` or `**dur`) `|>`

  - *Parsing* (done by `rhythmInterval()`) `|>`

    - **Intermediate**
      ([rational](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md))
      representation `|>`

  - *Deparsing* `|>`

- **Output** representation (e.g. `**recip` or `**duration`)

*This* documentation talks about the parsing step. For an overview of
the "deparsing" process, look
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md).
To learn about the "deparsing" of specific representations, [start
here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
or go straight to the docs for specific functions— for example, call
[`?recip`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md) to
learn about
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md).

## Dispatch

The rhythm parser (`rhythmInterval()`) is a generic function, meaning it
accepts a variety of inputs and automatically "dispatches" the
appropriate method for the input. R's standard `S3` system is used to
dispatch for either `numeric` or `character`-string input: Though most
rhythmic representations are essentially numbers, several standard
representations included a mix of numeric and non-numeric symbols. Given
either a `character` string or a number, `humdrumR` then uses either
regular-expression matching or humdrum exclusive interpretation matching
to dispatch specific parsing methods.

## Symbolic Parsing

Since humdrum data is inherently string-based, all our input data
ultimately starts as `character` strings. (This includes character
tokens with rhythm information embedded alongside other information;
Details below.) The rhythm parser (`rhythmInterval()`) uses a
combination of regular-expressions and exclusive interpretations to
decide how to parse an input string. There are three regular-expression
patterns for rhythm that `rhythmInterval()` knows how to parse
automatically:

|                                                              |               |         |
|--------------------------------------------------------------|---------------|---------|
| Representation                                               | Exclusive     | Example |
| [Recip](https://www.humdrum.org/rep/recip/index.html)        | \*\*recip     | `4.`    |
| [Note values](https://en.wikipedia.org/wiki/Note_value)      | \*\*notevalue | `𝅘𝅥 𝅭`   |
| [Time durations](https://www.humdrum.org/rep/dur/index.html) | \*\*dur       | `/1.5`  |

### Exclusive Dispatch

If you call `rhythmInterval()` (or *any* [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md))
on a `character`-string vector, with a non-`NULL` `Exclusive` argument,
that `Exclusive` argument will be used to choose the input
interpretation you want, based on the "Exclusive" column in the table
above. For example, `seconds(x, Exclusive = 'recip')` will force the
parser to interpret `x` as `**recip` data. Similarly,
`recip(x, Exclusive = 'dur')` will force the parser to interpret `x` as
`**dur` data. If you use any [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
within a special call to
[withinHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
`humdrumR` will automatically pass the `Exclusive` field from the
humdrum data to the function—this means that, in most cases, you don't
need to explicitly do anything with the `Exclusive` argument! (If you
want this *not* to happen, you need to explicitly specify your own
`Exclusive` argument, or `Exclusive = NULL`.)

### Regex Dispatch

If you call `rhythmInterval()` (or *any* [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md))
on a `character`-string vector, but the `Exclusive` argument is missing
or `NULL`, `humdrumR` will instead use regular-expression patterns to
select a known interpretation. For example, `seconds('4.')` will
automatically recognize that `'4.'` is a `**recip` token, and will
interpret the data accordingly (the output should be 1.5). If there are
more than one matches, `humdrumR` will use the longest match, and if
they tie, pick based on the order in the table above (topmost first).

If there is no match, `rhythmInterval()` (and all other [rhythm
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md))
return `NA` values. Remember, if `Exclusive` is specified, it overrides
the regex-based dispatch, which means that
`pitch('4.', Exclusive = 'notevalue')` will return `NA`, because `'4.'`
can't be interpreted as a `**notevalue`.

#### "In place" parsing

In lots of humdrum data, character strings are encoded with multiple
pieces of musical information right besides each other: for example,
`**kern` data might include tokens like `"4.ee-[`. The `humdrumR` rhythm
parser (`rhythmInterval()`) will automatically "pull out" rhythm
information from within strings, if it can find any, using the
appropriate known regular expressions. Various [rhythm parsing
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
have an option to keep the original "extra" data, using their `inPlace`
argument.

## See also

All `humdrumR` [rhythm
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
make use of the parsing functionality.
