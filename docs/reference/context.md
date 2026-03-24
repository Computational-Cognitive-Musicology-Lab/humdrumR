# Group vectors into contextual windows

The `context()` command can be used to group input data (vectors or
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md))
into arbitrary contextual windows. Unlike [grouping
vectors](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md),
`context()` windows 1) are always contiguous relative to the reference
vector(s)/field(s) (which can depend on
[order](https://dplyr.tidyverse.org/reference/order_by.html)); 2) can
*overlap*; and 3) don't necessarily exhaustively divide the data. The
`context()` function should generally be called on [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
but it can also be called directly on vectors.

The `uncontext()` function removes contextual windows from a [humdrumR
data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

Once contextual windows are created, the `windows()` function can be
used to view a
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
representing these windows. The `Open` and `Close` columns indicate row
indices in the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

## Usage

``` r
context(x, open, close, ...)

# Default S3 method
context(
  x,
  open,
  close,
  reference = x,
  overlap = "paired",
  depth = NULL,
  rightward = TRUE,
  duplicate_indices = TRUE,
  min_length = 2L,
  max_length = Inf,
  inPlace = FALSE,
  complement = FALSE,
  alignToOpen = TRUE,
  collapse = TRUE,
  sep = ",",
  stripRegex = FALSE,
  groupby = list()
)

# S3 method for class 'humdrumR'
context(
  humdrumR,
  open,
  close,
  dataTypes = "D",
  overlap = "paired",
  depth = NULL,
  rightward = TRUE,
  duplicate_indices = TRUE,
  min_length = 2L,
  max_length = Inf,
  groupby
)

uncontext(humdrumR, complement = NULL)

windows(humdrumR)
```

## Arguments

- x:

  ***Input data to group into windows.***

  Must be an atomic vector.

- open:

  ***Where to "open" (start) windows.***

  Can be natural numbers, `logical` vectors (of the same length as `x`),
  a single `character` string (interpreted as a regular expression). May
  also be an arbitrary expression which returns natural numbers; the
  expression can refer to named elements of `reference`, to `end` (last
  index), to `close`, or to `prevclose` (the previous close).

- close:

  ***Where to "close" (end) windows.***

  Can be natural numbers, `logical` vectors (of the same length as `x`),
  a single `character` string (interpreted as a regular expression). May
  also be an arbitrary expression which returns natural numbers; the
  expression can refer to named elements of `reference`, to `end`
  (previous index), to `open`, or to `nextopen` (the next open).

- reference:

  ***Vector(s) to use to identify window open/closes.***

  Defaults to `x`.

  Must be either an atomic vector of the same length as `x`, or a
  [`list()`](https://rdrr.io/r/base/list.html)/`data.frame` of such
  vectors, all [named](https://rdrr.io/r/base/names.html).

  If `context()` is applied to a [humdrumR
  dataset](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
  all the fields of the data's [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  are used as `reference`.

- overlap:

  ***How are overlapping windows treated/created?***

  Defaults to `'paired'`.

  Must be a single `character`, [partially
  matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  either `"paired"`, `"nested"`, `"edge"`, or `"none"`.

- depth:

  ***How "deep" can windows overlap?***

  Defaults to `NULL`.

  Must be `NULL`, or a vector of non-zero whole numbers.

- rightward:

  ***Should window alignment/overlap be determined from left to
  right?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- duplicate_indices:

  ***Can the same index open/close multiple windows?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- min_length, max_length:

  ***The minimum/maximum lengths of output windows.***

  Default to two and infinity (no maximum) respectively.

  Must be single, positive whole numbers.

- inPlace:

  ***Should output be padded to same length as input?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- complement:

  ***Should input "outside" any windows, be output?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- alignToOpen:

  ***Should '`inPlace`' output be aligned to the open of each window?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- collapse:

  ***Should output windows be collapsed to single `character`
  strings?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- sep:

  ***Separator for collapsed output.***

  Defaults to a comma (`","`).

  Must be a single `character` string.

- stripRegex:

  ***Should regular expressions matched by the `open`/`close` arguments
  be removed from the output?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- groupby:

  ***Optional vectors to group windows within.***

  Defaults to empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a [`list()`](https://rdrr.io/r/base/list.html), which is
  either empty or contains vectors which are all the same length as `x`.
  In calls to
  [with/within.humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
  `groupby` is passed `list(Piece, Spine, Path)` by default.

  Windows cannot cross group boundaries.

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Details

The `context()` function determines where contextual windows will begin
and end based on
[expressions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
in its `open` and `close` arguments. These `open` and `close`
expressions are evaluated using a **reference**
[vector](https://rdrr.io/r/base/vector.html), or set of
vectors/[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
that are all the same length.

In most cases, we'll apply `context()` to a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
so windows are defined by evaluating the `open` and `close` arguments
using the
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
of the humdrum table as the reference. Once this has been done, the
humdrumR object will
[show](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
how many windows have been identified when printed in the console. If we
then use
[with/within/mutate/summarize/reframe](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
on our data, these methods will evaluate their argument expressions
within each and every contextual window defined by `context()`. This
means we can do basically anything we want to/with our contextual
windows.

We can also apply `context()` directly to a single input vector `x`,
providing a [vector](https://rdrr.io/r/base/vector.html) or
[list](https://rdrr.io/r/base/list.html)/[data.frame](https://rdrr.io/r/base/data.frame.html)
of equal-length vectors as the `reference` for the `open` and `close`
arguments. By default, `x` itself is reused as the `reference`, so
windows are based on the input `x` itself. When applied to a vector,
`context()` will simply group the elements of `x` into the windows we
defined, with a few arguments to control how this is done:

- `complement`: The "complement" refers to elements of the input vector
  that don't fall inside any indicated windows: if `complement = FALSE`
  (the default), these "outside" values are dropped; if
  `complement = TRUE`, they are retained.

- `inPlace`: If `inPlace = TRUE`, windows are output in a vector of the
  same length as the input, padded with `NA` as needed—otherwise (the
  default), only the windows are returned

- `collapse`: If `collapse = TRUE`, the windows are collapsed to strings
  (separated by `sep`), otherwise, a
  [`list()`](https://rdrr.io/r/base/list.html) of windows is returned.

  - `sep` the separator used if `collapse = TRUE`.

- `alignToOpen`: Should padded output (`inPlace = TRUE`) by aligned to
  the openning (left-side) of each window?

- `stripRegex`: Should regular expressions used to identify windows
  (details below) be stripped from the output?

In the rest of this man page, we will apply `context()` to simple
vectors (like the [letters](https://rdrr.io/r/base/Constants.html)
vector) to illustrate how windows are defined. In actual analyses,
you'll be more likely to apply `context()` to [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

Note that, when using `context()` inside [with, within,
etc.](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
the `alignToOpen` argument will have no effect. Instead, use
`alignLeft = FALSE` as an argument to
[`with()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)/[`within()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
not as an argument to `context()`.

### groupby

The `groupby` argument is an optional
[list](https://rdrr.io/r/base/list.html) of grouping vectors, all the
same length as `x`/`reference`. Contextual windows cannot cross
boundaries indicated in `groupby`. When applying `context()` to
[humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
`groupby` is automatically passed `list(Piece, Spine, Path)`, which
prevents windows from crossing normal "melodic" boundaries in the data.

## Defining windows

The system `context()` uses to define/identify windows in the data is
quite sophisticated, and can take some time to master! The basic idea is
that you must indicate where you want windows to start ("*open*") and
where you want them to end ("*close*"): you indicate this using the
`open` and `close` arguments. To introduce their usage, we will first do
some simple examples applying `context()` to the built-in
[letters](https://rdrr.io/r/base/Constants.html) vector, which (by
default) will act as our `reference` vector *and* the target vector `x`
to contextualize. We will then show how these techniques can be used
with multiple
vectors/[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

The `open` and `close` arguments are
[expressions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
which, when evaluated, must indicate indices in the `reference`
vector(s)/[field(s)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md);
For example, if we want a window to open at the 4th and 11th indices,
and close at the 15th and 24th index, we can write:

    context(letters, open = c(4, 11), close = c(15, 24))

This is quite trivial. However, the `open` and `close` expressions can
do a number of special tricks, including refering to each other. For
example, if either argument includes a call to
[`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md),
[`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md) will
automatically be applied along the input vector. Consider this example:

    context(letters, open = hop(2), close = open + 3)

In this example, the
[`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md)
command generates `open` indices for every odd number from `1` to `25`.
The `close` argument then references these `open` indices, and adds `3`
to each— the result is pairs like `1:4`, `2:5`, `3:6`, `4:7`, etc. If we
give [`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md)
different arguments (like `by` or `from`), we can modify this process.
In fact, if we use the default `by` value for
[`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md)
(`1`), we can use this approach to create standard N-grams.

We can also indicate open/closes by providing `logical` vectors (the
same length as `x`/`reference`). For example:

    context(letters, open = letters %in% c('e', 'j', 'l'), close = open + 2)

### Regular Expressions

If either `open` or `close` are provided a `character` string, this
string is treated as a regular expression and is matched against the
`reference` vector. For example, we could make windows in the alphabet
starting or ending on each vowel:

    context(letters, open = '[aeiou]', close = open + 4)
    context(letters, open = close - 4, close = '[aeiou]', alignToOpen = FALSE)

If the `stripRegex = TRUE` (not the default), the matching `open` or
`close` regular expressions are removed from the output. This can be
useful if the character/tokens used to indicate windows are no longer
needed once windowing is done.

### Special variables

The `open` and `close` expressions will understand a few special
variable names:

- `nextopen`: represents the index of the *next* open—can only be used
  in the `close` argument.

- `prevclose`: represents the index of the *previous* close—can only be
  used in the `open` argument.

- `end`: represents the last index of the `reference` vector(s).

- `|`: As in "OR"—specify alternative window `open`/`close` criteria.

What if we'd like each of our windows to close right before the next
window opens? We can do this by making the `close` argument refer to the
*next* `open`, by referring to the `nextopen` variable:

    context(letters, open = '[aeiou]', close = nextopen - 1L)

Conversely, `open` can refer to the `prevclose` close:

    context(letters, open = prevclose + 1, close = '[aeiou]', alignToOpen = FALSE)

Notice that when we called
`context(letters, open = '[aeiou]', close = nextopen - 1L)`, the window
opening on `"u"` is not returned. This is because there is no
"`nextopen`" open to close on. We can instead provide `context()` an
alternative, using `|` (or):

    context(letters, open = '[aeiou]', close = nextopen - 1L | 26)

Here we are saying, close a window 1 index before the next open *OR* at
index 26. What if we don't know exactly how long our input vector is?
Refer to the `end` variable:

    context(letters, open = '[aeiou]', close = nextopen - 1L | end)

### Separating context reference from application

The previous examples illustrate the basic concepts of using
`open`/`close`; to grasp how they work, study these examples and play
around with them. We can also define `open` and `close` expressions that
reference more than one
vector(s)/[field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/s),
and which aren't necessarily the thing we want to apply the windowing
to. To illustrate this last point, let's take the last command from the
previous section and make it so the `x` argument is different than the
`reference` argument:

    context(LETTERS, reference = letters, open = '[aeiou]', close = nextopen - 1L | 26)

Now, `letters` is still being used as the windowing reference, but the
contextual windowing is being applied to `LETTERS`. When we use
`context()` on a [humdrumR
dataset](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
the data's
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
can be used as the reference, then [with(), within(),
mutate()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
can be used to manipulate other fields.

### Multi-vector/field reference

The `open` and `close` arguments can reference more than one reference
vector. When applying `context()` to a vector `x`, we can provide a
named [`list()`](https://rdrr.io/r/base/list.html) or
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) as the
`reference` argument—so long as all the vectors they contain are the
same length as `x`. We can then refer to these vectors by name:

    reference.frame <- data.frame(Threes = rep(1:3, length.out = 26),
                                   Fours = rep(4:1, length.out = 26))

    context(letters,
            reference = reference.frame,
            open = Threes == Fours, close = Fours == 1)

So we created a data.frame with columns `Threes` and `Fours`. We
referenced *both* of these columns when defining where windows `open`
and `close`.

#### Using humdrumR data

When we apply `context()` to [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
we can refer to *any* of the data's
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
in `open` or `close`. We can also use all of `open`/`close`'s special
tricks (described above), like
[`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md),
`nextopen`, `prevclose`, and `end`. For example, to create 4-grams in a
humdrum dataset:

    humData |> context(open = hop(), open + 3)

As mentioned above, when we apply `context()` to [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
`groupby` is automatically passed `list(Piece, Spine, Path)`, which
prevents windows from crossing normal "melodic" boundaries in the data.
This can be overrriden by providing your own explicit `groupby`
argument. [Grouping
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
that have already been defined in the data, are *also* used.

## Filtering windows

Once `open` and `close` have identified where windows can start and end,
there is still some options for open and close indices to associate with
each other to create a window. For example, as mentioned above, the
`groupby` argument can be used to make sure windows do not cross
grouping boundaries—even if one group has an extra open index and the
next group has an extra close index. The minimum and maximum length of
windows can also be controlled using the `min_length` and `max_length`
arguments. The `overlap`, `depth`, `rightward`, and `duplicate_indices`
arguments provide a number of additional options, which are useful for
some use cases (details below).

### Nested windows

A common use-case for `context()` is analyzing phrases indicated in
music. In `**kern`, phrases are indicated with opening (`(`) and close
(`)`) parentheses, which we can capture with regular expressions for
`open` and `close`. Here is an example:

    nesting1 <- c('(a', 'b)', '(c', 'd', 'e)', '(d', 'e', 'f)', '(e', 'f', 'f#', 'g', 'g#', 'a)')
    context(nesting1, open = '(', close = ')')

    #> [1] "(a,b)"           "(c,d,e)"         "(d,e,f)"         "(e,f,f#,g,g#,a)"

Perfect. However, what if there are nested phrasing indicators?

    nesting2 <- c('(a', 'b)', '(c', '(d', 'e)',  '(d', 'e)', 'f)', '(e', '(f', '(f#', 'g)', 'g#)', 'a)')
    context(nesting2, open = '(', close = ')')

    #> [1] "(a,b)"         "(c,(d,e)"      "(d,e),(d,e)"   "(d,e),f)"      "(e,(f,(f#,g)"  "(f,(f#,g),g#)" "(f#,g),g#),a)"

That's not what we want! By default, `context()` "pairs" each `open`
with the next `close`, which often makes the most sense. But in this
case, we want different behavior. We can get what we want by specifying
`overlap = 'nested'`:

    context(nesting2, open = '(', close = ')', overlap = 'nested')

    #> [1] "(a,b)"               "(c,(d,e),(d,e),f)"   "(d,e)"               "(d,e)"
    #> [5] "(e,(f,(f#,g),g#),a)" "(f,(f#,g),g#)"       "(f#,g)"

Now context aligns each `open` with the corresponding `close` at the
same *nesting level*. What if we are only interested in the highest (or
lowest) level of nesting? Use the `depth` argument, which can be
non-zero integers: the highest level is `1`, with "deeper" levels
incrementing up.

    context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 1)

    #> [1] "(a,b)"               "(c,(d,e),(d,e),f)"   "(e,(f,(f#,g),g#),a)"

    context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2)

    #> [1] "(d,e)"         "(d,e)"         "(f,(f#,g),g#)"

    context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2:3)

    #> [1] "(d,e)"         "(d,e)"         "(f,(f#,g),g#)" "(f#,g)"

You can also use negative `depth` to specify from the deepest levels
outward. For example, in this case `depth == -1` should get us that
deepest level:

    context(nesting2, open = '(', close = ')', overlap = 'nested', depth = -1)

    #> [1] "(f#,g)"

If `depth` is `NULL` (the default), all depths are returned.

### Controlling overlap

There are some other options for controlling how windows can, or cannot,
overlap. Perhaps we'd like to look at every melodic phrase moving from
*So* (dominant) to *Do* (tonic).

    melody <- c('so', 'la', 'ti', 'do', 'so', 'fi', 'so', 'la', 'ti', 're', 'do', 'so', 'la', 're', 'do')

    context(melody, open = 'so', close = 'do')

This output is probably not what we want. Again, `context()` (by
default) pairs each opening with the next close *which hasn't already
been paired*. In this case, that means the third *So* is getting pairs
with the third *Do*, even though there is another *Do* in between! We
might want to try either the `"edge"` or `"none"` options for the
`overlap` argument:

    context(melody, open = 'so', close = 'do', overlap = 'edge')
    context(melody, open = 'so', close = 'do', overlap = 'none')

The `"edge"` option allows the closing edge of windows to share a
`close`—in this case, the second and third *So* (`open`) are paired with
the same *Do*. On the other hand, with `overlap = "none"`, overlapping
windows are simply not allowed, so the third `open` simply doesn't get
paired with anything.

What if you would like to pair windows on their left (opening) edge? If
you specify `rightward = FALSE`, the overlap argument works backwards
(right-to-left) through the input vector, starting on each `close` and
ending on each `open`. By combining `righward = FALSE` with various
`overlap` options, you can achieve a lot of windowing options you might
need.

### Repeated indices

Note that if `duplicates_indices = TRUE` (the default) the `open` and
`close` arguments can incorporate repeated indices, including multiple
matches to a regular expression in the same index. This is useful with,
for example, nested phrases:

    nesting3 <- c('(a', 'b)', '((c', 'd', 'e)',  '(d', 'e', 'f))', '(e', 'f', '((f#', 'g)', 'g#)', 'a)')

    context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 1)
    context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 2)

In some cases, you might want to turn `duplicate_indices = FALSE`.

## Complements (removing context)

The `uncontext()` command, like the
[`ungroup()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
command, is needed to remove contextual windows from [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
so that further calls to
[within()/mutate()/etc.](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
are not applied in context.

The `uncontext()` command can also be used to access data *outside* of
contextual windows by using a `complement` argument, similar to the
[`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
function. The `complement` must be an existing field in the data. If
`uncontext()` is used with a given complement field, the currently
selected data field (unless `Token` is selected) has the contents of the
complement field inserted into it at all points outside the contextual
windows. This can be used to preserve non-contextual data while
displaying it alongside contextual transformations.

## See also

Other Contextual grouping functions.:
[`groupHumdrum`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)

## Examples

``` r
# use the built-in 'letters' vector

context(letters, open = hop(4), close = open + 3)
#> [1] "a,b,c,d" "e,f,g,h" "i,j,k,l" "m,n,o,p" "q,r,s,t" "u,v,w,x"

context(letters, open = "[aeiou]", close = nextopen - 1 | end)
#> [1] "a,b,c,d"     "e,f,g,h"     "i,j,k,l,m,n" "o,p,q,r,s,t" "u,v,w,x,y,z"
context(letters, open = "[aeiou]", close = nextopen - 1 | end, inPlace = TRUE)
#>  [1] "a,b,c,d"     NA            NA            NA            "e,f,g,h"    
#>  [6] NA            NA            NA            "i,j,k,l,m,n" NA           
#> [11] NA            NA            NA            NA            "o,p,q,r,s,t"
#> [16] NA            NA            NA            NA            NA           
#> [21] "u,v,w,x,y,z" NA            NA            NA            NA           
#> [26] NA           
context(letters, open = "[aeiou]", close = nextopen - 1 | end, collapse = FALSE)
#> [[1]]
#> [1] "a" "b" "c" "d"
#> 
#> [[2]]
#> [1] "e" "f" "g" "h"
#> 
#> [[3]]
#> [1] "i" "j" "k" "l" "m" "n"
#> 
#> [[4]]
#> [1] "o" "p" "q" "r" "s" "t"
#> 
#> [[5]]
#> [1] "u" "v" "w" "x" "y" "z"
#> 



humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

humData |> context(hop(6), open + 2) |> within(paste(Token, collapse = '|')) |> uncontext(complement = 'Token')
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
#>            20:        4GG|4G|4E      4B|4B|8cL       4d|4d|4e      4g|2g|4dd
#>            21:               =1             =1             =1             =1
#>            22:                .              .              .              .
#>            23:                .              .              .              .
#>            24:                .            8BJ              .              .
#>            25:              4F#             4A             4d              .
#>            26:               =2             =2             =2             =2
#>            27:               4G             4G             2d            4.b
#>            28:               4D     4F#|4G|8cL              .              .
#>            29:                .              .              .             8a
#>            30:       4E|4C|8BBL              .             4B             4g
#>            31:               =3             =3             =3             =3
#>            32:                .              .      8eL|8d|8e      4.g|8a|4b
#>            33:                .            8BJ              .              .
#>            34:                .             4c              .              .
#>            35:             8AAJ              .           8f#J              .
#>            36:              4GG             4d             4g              .
#>            37:               =4             =4             =4             =4
#>            38:              2D;      2d;|4d|4A           2f#;            2a;
#>            39:     4GG|4FF#|4GG              .       4g|4d|4e             4b
#>            40:               =5             =5             =5             =5
#> 41-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-63::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            64:             8G#L              .             4b           8eeL
#>            65:              8EJ              .              .          8dd#J
#>            66:              4F#            4A#            4f#    4cc#|4b;|4b
#>            67:      4BB;|4E|4G#           4d#;   4f#;|4g#|8eL              .
#>            68:                .             4e              .              .
#>            69:               =7             =7             =7             =7
#>            70:                .       4B|4e|4e              .            4ee
#>            71:                .              .           8f#J              .
#>            72:               4E              .            4g#             4b
#>            73:            8AnXL              .             4a           4cc#
#>            74:              8BJ              .              .              .
#>            75:      4c#|4E#|4F#            4c#   8eL|8f#J|4g#    8g#L|8aJ|4b
#>            76:                .              .              .              .
#>            77:               =8             =8             =8             =8
#>            78:                .             4d              .              .
#>            79:                .            4c#            4f#             4a
#>            80:             4C#;   4c#;|[4B|4B]           4e#;           4g#;
#>            81:               4E              .             4e            4g#
#>            82:               =9             =9             =9             =9
#>            83:              4BB              .     4d#|4c#|4B     4f#|4a|4g#
#>            84:     8C#L|8D#J|4E             4e              .              .
#>            85:                .              .              .              .
#>            86:                .             4e              .              .
#>            87:              4BB           8d#L             4B            4f#
#>            88:                .   8BJ|2F#|4G#;              .              .
#>            89:              =10            =10            =10            =10
#>            90:             4AA#              .            4c#            4c#
#>            91:              4BB              .             4B            4d#
#>            92:             4EE;              .            4B;            4e;
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
#>           Token                        :: character
#>          *paste(Token, collapse = "|") :: character
#> 
```
