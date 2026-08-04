# Calculate intervals between pitches

These functions allow us to calculate intervals between pitches. `int()`
is the most basic form, calculating the interval(s) between two input
vectors. `mint()` and `hint()` are special forms for calculating
intervals "melodically" or "harmonically," respectively.

If `mint()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `hint()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
int(
  x,
  from = tint(0L, 0L),
  deparser = interval,
  incomplete = NULL,
  bracket = is.function(incomplete),
  classify = FALSE,
  ...,
  Exclusive = NULL,
  Key = NULL,
  parseArgs = list()
)

# Default S3 method
mint(
  x,
  lag = 1,
  deparser = interval,
  incomplete = kern,
  bracket = is.function(incomplete),
  classify = FALSE,
  ...,
  parseArgs = list(),
  Exclusive = NULL,
  Key = NULL,
  groupby = list(),
  orderby = list()
)

humData |> select(Token) |> mint() 
humData |> mint(simple = TRUE)
humData |> mint(Token, Key = Key)

mint(
  x,
  lag = 1,
  deparser = interval,
  incomplete = kern,
  bracket = is.function(incomplete),
  classify = FALSE,
  ...,
  parseArgs = list(),
  Exclusive = NULL,
  Key = NULL,
  groupby = list(),
  orderby = list()
)

# Default S3 method
hint(
  x,
  lag = 1,
  deparser = interval,
  incomplete = kern,
  bracket = is.function(incomplete),
  ...,
  parseArgs = list(),
  Exclusive = NULL,
  Key = NULL,
  groupby = list(),
  orderby = list()
)

humData |> select(Token) |> hint() 
humData |> hint(simple = TRUE)
humData |> hint(Token, Key = Key)

hint(
  x,
  lag = 1,
  deparser = interval,
  incomplete = kern,
  bracket = is.function(incomplete),
  ...,
  parseArgs = list(),
  Exclusive = NULL,
  Key = NULL,
  groupby = list(),
  orderby = list()
)
```

## Arguments

- x:

  ***Input pitch information.***

  Can be any ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  or `NULL`. Must be [parsable as pitch
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- from:

  ***Pitches to calculate the intervals from.***

  Defaults to middle C / unison.

  Can be any ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  or `NULL`. Must be [parsable as pitch
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- deparser:

  ***What output representation do you want?***

  Defaults to
  [interval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md).

  Must be a [pitch
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
  like
  [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).

- incomplete:

  ***How to pad incomplete intervals (e.g., the start/end of input).***

  Defaults to `NULL` for `int()`, `kern` for `mint()` and `hint()`.

  Must `NULL`, a [pitch
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
  or an atomic value of `length(incomplete) == abs(lag)`.

- bracket:

  ***Whether to print brackets around `incomplete` output.***

  Defaults to `TRUE` if `incomplete` is a function, `FALSE` otherwise.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, square brackets (`"[]"`) are printed around `incomplete`
  observations.

- classify:

  ***Should intervals be classified as step/skip/leaps?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the `deparser` is ignored and the output is classified as
  `Unison`, `Step`, `Skip`, or `Leap`.

- parseArgs:

  ***An optional list of arguments passed to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- lag:

  ***The
  [`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) to
  calculate harmonic/melodic intervals between.***

  Defaults to `1`, which means intervals between immee successors in
  `x`.

  Must be either a single number, or a `logical` of `length(x)` (see
  "Logical lags" section in manual).

- groupby:

  ***A `list` of vectors to group `x`.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

- orderby:

  ***A `list` of vectors to group `x`.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

  Used to interpret the order of elements in `x`. Lagged computations
  are done in the indicated order, but the output is returned in the
  original order.

## Details

Input vectors `x` (and `from`) are [parsed as
pitches](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
([tonal interval
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)),
if possible. (Parsing arguments can be passed via the `parseArgs` list,
or `parse(...)` sugar. `Key` and `Exclusive` arguments are also passed
to the parser.) Any inputs that fail to parse will show up as `NA` in
the output.

Once parsed, the intervals between the pitches are calculated as
`x - from`. The resulting intervals are then
"[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)"
into a standard representation; by default, the
[`lubridate::intervals()`](https://lubridate.tidyverse.org/reference/Interval-class.html)
representation is used, but you can set the `deparser` argument to any
[pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md).
However, the only alternative deparser that would be *commonly* used
(besides
[`lubridate::intervals()`](https://lubridate.tidyverse.org/reference/Interval-class.html))
would be
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md).
If `deparser` is `NULL`, the raw
[tonalIntervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
are returned.

## Melodic and Harmonic intervals

`mint` and `hint` calculate "melodic" and "harmonic" intervals
respectively. In this context, "melodies" are sequences of notes within
a **spine path**, while "harmonies" are intervals between notes
occurring in the same **record** (at the same time). Outside of a
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
call, `mint` or `hint` are exactly the same; It is only when used in a
call to
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
that you will see them have different behaviors, as
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
will automatically apply them across spine paths (`mint()`) or records
(`hint()`). This is achieved by modifying the `groupby` and `orderby`
arguments to
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)—you
can manually achieve the default behaviors, or other behaviors, by
setting these arguments yourself.

When used in a
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
expression, `mint()` will (by default) calculate the melodic interval
*approaching* each note from the previous note: for example,
`mint('C4', 'D4', 'Eb4')` fill return `c('[c]', '+M2', '+m2')`, because
D4 is approached by ascending whole step *from* C4, and Eb4 is
approached by ascending half step *from* D4. Similarly, the default
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
behavior of `hint()` is to calculate successive intervals in the same
record (*across* spine paths), from left to right. So the record
`C3 G4 C5` will return values `[CC] +P12 +P4`, because the G4 is a
perfect 12th above C3, and C5 is a perfect fourth above G4.

`mint()` and `hint()` work by passing
[lagged](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) and/or
[dittoed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md)
versions of `x` as the `from` argument to `int()`. Basically, `mint()`
is equivalent to
`int(x, lag(x, lag = lag, groupby = list(Piece, Spine, Path)), ...)` and
`hint()` is equivalent to
`int(x, lag(x, lag = lag, groupby = list(Piece, Record), orderby = list(Piece, Record, Spine, Path)), ...)`.
In either case, the parsed pitch vector is copied and lagged using
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md), with
pairs crossing outside `groupby` groups ignored. The `lag` argument
controls how far apart in the melody intervals are calculated. For
instance, a lag of `2` will calculate intervals between *every other*
note in the vector. Positive lags (the default) will calculate
**approaching** intervals: each token represents the interval between
the current note and the *previous* note. Negative lags will calculate
**departing** intervals: each token represents the interval between the
current note and the *next* note. Note that, by passing
`directed = FALSE` through the
[deparser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md),
the undirected (absolute value) of the melodic intervals can be
returned.

## Incomplete value padding

By default, `int` will return `NA` whereever `x` **or** `from` is `NA`.
However, if `from` is `NA` but `x` is *not* `NA`, we can ask for
different output for these "incomplete" pairs. using the `incomplete`
argument. If `incomplete` is an atomic value, incomplete outputs indices
are filled with this value. If the incomplete argument is a [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
(like the `deparser` argument), this function is used to (re)parse the
values of `x` where `from` is missing. If `bracket == TRUE`, incomplete
output values are surrounded with `[]`, so they are easier to
distinguish from the actual intervals.

The main use of the `incomplete` argument is in `mint()` and `hint()`.
The lagged `from` arguments used in `mint()`/`hint()` (see previous
section) are necessarily padded by `abs(lag)` `NA` values at the
beginning (positive lag) or end (negative lag). These are thus
"incomplete" pairs passed to `int()`, and can be controlled using the
`incomplete` argument. By default, both `mint()` and `hint()` set
`incomplete = kern(), bracket = TRUE` which cause these notes to show up
as bracketed kern, like `[ee-]` or `[C#]`. If `incomplete` is `NULL`,
the incomplete values are simply padded with `NA`.

## Interval classification

If the `classify` argument is set to `TRUE`, intervals are classified as
either `"Unison"`, `"Step"`, `"Skip"`, or `"Leap"`. Alternatively, skips
can be interpreted as leaps by setting `skips = FALSE`.
(`classify = TRUE` overrides the `deparser` argument.)

By default, intervals are categorized tonally, meaning that the interval
in tonal *steps* is used as the basis of classification. For example, an
augmented 2nd is a step, and a diminished 3rd is a skip/leap. This means
that augmented and diminished unisons are marked `"Unison"` as well!
However, if `directed = TRUE`, augmented/diminished unisons will be
marked with `+` or `-` to indicate direction, whereas perfect unisons
are never marked with `+`/`-`.

Alternatively, you may choose to categorize intervals *atonally* by
setting `atonal = TRUE`. If so, intervals are categorized based only on
semitone (enharmonic) intervals: D# and Eb are classified the same.

## Logical (ditto) lags

For calls to `hint()` and `mint()` the default behavior is a `numeric`
`lag` argument passed to
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md). An
alternate option is to specify the `lag` argument as `logical` vector
the same length as the input (`x` argument). Rather than calculating the
interval between a pitch and another pitch separated by a regular lag, a
`logical` `lag` argument "lags" each pitch back to the previous value
where `lag == TRUE`. This means that more than one interval can be
calculated from those same `TRUE` indices.

The canonic use of this "logical lag" feature is to calculate harmonic
intervals relative to the same voice, like the bass voice. For example,
consider this file:

     **kern        **kern        **kern        **kern
    *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
          C             e             g            cc
          G             d             f             b
          C             c             e            cc
         *-            *-            *-            *-

If we
[read](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
this file and applied `hint()` to the `Token` field (with default
arguments) the result would be:

     **kern        **kern        **kern        **kern
    *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
        [C]          +M10           +m3           +P4
        [G]           +P5           +m3           +A4
        [C]           +P8           +M3           +m6
         *-            *-            *-            *-

In each record, we see the intervals as lagged (`lag == 1`) from left
right: we see the intervals between the bass and the tenoir, the tenor
and the alto, and the alto and the soprano. What if we wanted to see all
the intervals with the bass? Well, we can use a `logical` `lag`
argument, where we would specify that `Spine == 1`:
`with(humData, hint(Token, lag = Spine == 1)`. This means that all
`from` values are "lagged" back to the previous value where
`Spine == 1`. The result would be:

     **kern        **kern        **kern        **kern
    *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
        [C]          +M10          +P12          +P14
        [G]           +P5           +m7          +M10
        [C]           +P8          +M10          +P14
         *-            *-            *-            *-

Now we see all the intervals relative to the bass.

The `logical` `lag` only takes place within the `groupby` groups.
However, note that any values *before* the first index where
`lag == TRUE` are calculated relative to that first value.

## Grouping

In many cases we want to perform lagged calculations in a vector, but
*not across certain boundaries*. For example, if your vector includes
data from multiple pieces, we wouldn't want to calculate melodic
intervals between pieces, only within pieces. The `groupby` argument
indicates one, or more, grouping vectors, which break the `x` (input)
argument into groups. If more than `groupby` vectors are given, a change
in *any* vector indicates a boundary.

Value pairs which cross between groups are treated as if they were at
the beginning. Basically, using the `groupby` argument to a function
should be similar or identical to using
`tapply(x, groupby, laggedFunction, ...)` or using a `groupby` expession
in a call to
[with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).
However, using a `groupby` argument directly is usually much faster, as
they have been specially optimized for this functions.

The most common use case in humdrum data, is looking at "melodies"
within spines. For this, we want `groupby = list(Piece, Spine, Path)`.
In fact, `humdrumR`
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
calls will *automatically* feed these three fields as `groupby`
arguments to certain functions: mint, delta, sigma, lag, ditto, ioi,
sumTies, hop, wort, or wort.character. So any use of `delta` in a call
to
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
will automatically calculate the `delta` in a "melodic" way, within each
spine path of each piece. However, if you wanted, for instance, to
calculate differences across spines (like harmonic intervals) you could
manually set `groupby = list(Piece, Record)`.

## See also

`mint` uses
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) to
"lag" the input pitches, and also makes use of [pitch
parsers](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
and [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md).

Other relative pitch functions:
[`bhatk()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bhatk.md),
[`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md),
[`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md),
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md)

Other Lagged vector functions:
[`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md),
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md),
[`sigma()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sigma.md)

## Examples

``` r
exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
results <- mint(exampleToken)
results
#> **interval (character)
#> [1] [GG] +P8  -m3  +M2  +m2  -P4  +M2 
results <- hint(exampleToken)
results
#> **interval (character)
#> [1] [GG] +P8  -m3  +M2  +m2  -P4  +M2 

exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
results <- with(exampleHumdrum[[,3:4]], mint(Token))
results
#> **interval (character)
#>  [1] .   [F] +P5 -m6 +m6 -P5 +P5 -M9 +P8 -P8 +P8 -m7 +M6 -M6 +P5 -M2 .   [a] +m3
#> [20] -P5 +M3 -M6 +P4 P1  +M3 P1  +m3 -m3 +m3 -P4 +m3 -P4 +M3 -M3 +M6 -M6 +M6 -M6
#> [39] +P5 -M9 +A4 -P5 +m6 .  
results <- with(exampleHumdrum[[,3:4]], hint(Token))
results
#> **interval (character)
#>  [1] .     [F]   +P5   [E]   +m6   [F]   +P5   [BB-] +P8   [BB-] +P8   [C]  
#> [13] +M6   [C]   +P5   [F]   .     [a]   +m3   +P4   +M3   [c]   +P4   [f]  
#> [25] +M3   [a]   +m3   [a]   +m3   +P5   +m3   +P4   +M3   +P5   +M6   +P5  
#> [37] +M6   +m6   +P5   +m3   +A4   +M3   +m6   .    
```
