# Rhythmic timeline of a piece

These functions calculate the amount of time (either in beats, or
seconds) that have unfolded since the beginning of a piece, giving a
sense of the timeline in which events unfold. In `music21` this
information is described as "offsets"—however, we prefer to reserve the
words "onset" and "offset" to refer to the beginning (attack) and end
(release) of rhythmic events.

If `timeline()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

If `timestamp()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
timeline(
  x,
  start = 0,
  pickup = NULL,
  ...,
  Exclusive = NULL,
  threadNA = TRUE,
  total = FALSE,
  parseArgs = list(),
  groupby = list()
)

humData |> select(Token) |> timeline() 
humData |> timeline(Token)

timeline(
  x,
  start = 0,
  pickup = NULL,
  ...,
  Exclusive = NULL,
  threadNA = TRUE,
  total = FALSE,
  parseArgs = list(),
  groupby = list()
)

# Default S3 method
timestamp(
  x,
  BPM = 60,
  start = 0,
  pickup = NULL,
  minutes = TRUE,
  ...,
  Exclusive = NULL,
  threadNA = TRUE,
  total = FALSE,
  parseArgs = list(),
  groupby = list()
)

humData |> select(Token) |> timestamp() 
humData |> timestamp(Token)

timestamp(
  x,
  BPM = 60,
  start = 0,
  pickup = NULL,
  minutes = TRUE,
  ...,
  Exclusive = NULL,
  threadNA = TRUE,
  total = FALSE,
  parseArgs = list(),
  groupby = list()
)
```

## Arguments

- x:

  ***Input rhythm information.***

  Must be `atomic`, or `NULL`.

  Is [parsed as duration
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- start:

  ***Where does timeline begin?***

  Defaults to `0`.

  Must be a single number.

- pickup:

  ***Where is there a pickup (anacrusis)?***

  Defaults to `NULL`

  Must be `logical` of same `length(x)`, Or `NULL`. See "Pickups"
  section below.

- threadNA:

  ***Should rhythm-less tokens return `NA`?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- total:

  ***Should timeline propagate to all records in all spines?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- parseArgs:

  ***An optional list of arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- groupby:

  ***A `list` of vectors to group `x`.***

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

  To function as a by-record timeline, the `groupby` list music include
  a *named* `Piece` and `Record` fields. Luckily, these are
  automatically passed by
  [with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
  so you won't need to worry about it!

- BPM:

  ***The tempo.***

  Defaults to `60`.

  Must be a single number or a `character` string in the format
  `"MM120"` (for 120 bpm).

  By default,
  [with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  passes the `BPM`
  [field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
  if present.

- minutes:

  ***Should minutes be counted in output?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, output seconds are converted to a character string encoding
  minutes, seconds, and milliseconds in the format `MM.SS.ms`.

## Details

Music unfolds over time, and humdrum data typically represents this by
placing simultaneous events in the same record, with successive events
in ever higher records—progressing "top down" through the file. In some
humdrum data, only this (implicit) ordering of data over time is
present. The `Record` and `DataRecord`
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
capture this ordering in all data parsed by `humdrumR`. However, many
(probably most) humdrum data files contain at least some information
about the relative duration of events, representing more detailed
information about timing and rhythm.

`timeline()` parses an input vector `x` as
[durations](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md),
computes the [cumulative
sum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sigma.md) of the
durations, with the `start` argument appended to the beginning. The
result is a `numeric` vector representing the total duration since the
beginning of the vector (plus the value of `start`, which defaults to
zero). The cumulative durations of `timeline()` represent musical
duration units, where `1` equals a whole note. `timestamp()` converts
these durations to seconds, either using the `BPM` argument/field to
determine the tempo or using the default tempo of 60 beats per minute.
If `minutes == TRUE`, the output is formatted into
`"minute:seconds.milliseconds"` character strings.

When applying `timeline()` to a [humdrumR
dataset](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
the timeline of all spines with rhythmic information (e.g., `**kern`,
`**harm`) is computed separately. (Note that `timeline()` can't
guarantee that your data spines contain consistent rhythmic information!
In other words, if one of your spines has (for example) an extra
eighth-note token, the timelines in each spine will diverge.) By
default, the timeline is output only in spines/tokens where rhythmic
information is encoded. However, sometimes we want to know the timing of
every datapoint. We can then set `total = TRUE`, which will propagate
timeline information to *all* data tokens in all spines.

Note that, `timeline()` and `timestamp()` follow the default behavior of
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
by treating grace-notes as duration `0`. This means that their position
on the timeline is simply inherited from the previous event on the
timeline, as if they occur at the same time. If you want to use the
specified duration(s) of grace notes, specify `grace = TRUE`. By
default, any *other* tokens without (parsable) rhythm information are
returned a `NA`. However, if `threadNA = FALSE`, rhythm-less tokens will
be treated as if they have a duration of `0` as well, and thus have a
(shared) position on the timeline.

## Pickups

Another option is to pass the `pickup` argument a logical vector of the
same length as the input `x`. Within each piece/group, any block of
`TRUE` values at the *beginning* of the `pickup` vector indicate a
pickup. The *first* index where the `pickup` logical is `FALSE` is used
as the starting point of the timeline/timecount; All the earlier
(`pickup == TRUE`) points will be negative numbers, measured backwards
from the start index. In `humdrumR`, any datapoints before the first
barline record (`=`) are labeled `Bar == 0` in the `Bar`
[field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
Thus, a common use for the `pickup` argument is
`within(humData, timeline(Token, pickup = Bar < 1)`, which makes the
downbeat of the first complete bar `1` the starting point of the
timeline—any notes in pickup bars are negative on the timeline.

## See also

The
[`timecount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
and
[`metcount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/metlev.md)
functions provide "higher level" musical interpretations of timeline
information.

## Examples

``` r
tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')

timeline(tokens)
#> [1] 0.0000 0.3750 0.5000 0.5625 0.6250 0.6875 0.6875 0.7500
timestamp(tokens, BPM = '90')
#> [1] ":0"     ":1"     ":1.333" ":1.500" ":1.667" ":1.833" ":1.833" ":2"    

B075 <- readHumsrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_0._a.krn")
#> Error in readHumsrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_0._a.krn"): could not find function "readHumsrum"
within(B075, timeline(Token))
#> Error: object 'B075' not found
```
