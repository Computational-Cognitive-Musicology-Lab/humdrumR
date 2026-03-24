# Count or measure metric position

These functions take vectors of rhythmic duration values and compute the
metric position of each rhythmic onset. `metlev()` identifies the metric
*level* of each onset; `metcount()` counts beats within a measure;
`metsubpos()` measures the distance between an onset and the nearest
metric beat. `metcount()` and `metsubpos()` parallel the more general
[`timecount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
and
[`subpos()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
functions.

## Usage

``` r
# Default S3 method
metlev(
  dur,
  meter = duple(5),
  pickup = NULL,
  value = TRUE,
  offBeats = TRUE,
  remainderSubdivides = FALSE,
  deparser = recip,
  groupby = list(),
  ...,
  parseArgs = list()
)

metlev(
  dur,
  meter = duple(5),
  pickup = NULL,
  value = TRUE,
  offBeats = TRUE,
  remainderSubdivides = FALSE,
  deparser = recip,
  groupby = list(),
  ...,
  parseArgs = list()
)

# Default S3 method
metcount(
  dur,
  meter = duple(5),
  level = tactus(meter),
  pickup = NULL,
  ...,
  offBeats = TRUE,
  withinNext = FALSE,
  remainderSubdivides = FALSE,
  groupby = list(),
  parseArgs = list(),
  Exclusive = NULL
)

metcount(
  dur,
  meter = duple(5),
  level = tactus(meter),
  pickup = NULL,
  ...,
  offBeats = TRUE,
  withinNext = FALSE,
  remainderSubdivides = FALSE,
  groupby = list(),
  parseArgs = list(),
  Exclusive = NULL
)

# Default S3 method
metsubpos(
  dur,
  meter = duple(5),
  pickup = NULL,
  deparser = duration,
  ...,
  remainderSubdivides = TRUE,
  groupby = list(),
  parseArgs = list()
)

metsubpos(
  dur,
  meter = duple(5),
  pickup = NULL,
  deparser = duration,
  ...,
  remainderSubdivides = TRUE,
  groupby = list(),
  parseArgs = list()
)
```

## Arguments

- dur:

  ***An input vector of rhythmic durations.***

  Must be a `character` or `numeric` vector.

  Is parsed using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  Wherever the input can't be parsed as a duration, that element is
  treated as a duration of zero.

- meter:

  ***The meter(s) to compute levels from.***

  Defaults to a standard, five-level duple (4/4) meter.

  Must be a
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
  object or a `character` vector.

  For `character` input, the string is parsed using
  [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md);
  a failure to parse will result in an error.

- pickup:

  ***Where is there a pickup (anacrusis)?***

  Defaults to `NULL`

  Must be `logical` of same `length(x)`, Or `NULL`. See "Pickups"
  section below.

- value:

  ***Should the output levels be represented as rhythmic duration
  values?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- offBeats:

  ***Should off-beat onsets be numbered in the output, or `NA`?***

  Defaults to `TRUE`.

  Must be a single `logical` value: an on/off switch.

- remainderSubdivides:

  ***Should off-beat onsets only be associated with beat levels that
  they evenly subdivide?***

  Defaults to `FALSE`.

  A singleton `logical` value: an on/off switch.

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

- parseArgs:

  ***An optional list of arguments passed to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [rhythm
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md).

- level:

  ***Which metric level should be counted?***

  Defaults to the tactus of the `meter`.

  A single `character` value or positive natural number.

  A `character` string input must be a
  [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
  value, matching a beat level in the meter. A numeric input directly
  indicates a level in the meter, starting from the highest level (`1`).

- withinNext:

  ***Should beats be counted within the next highest beat of the
  meter?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

## Details

Watch out! These `met...()` functions require *meter* information and
their output is highly dependent on how you interpret meter from scores.
For a full discussion of how meter can be represented, parsed, and
created in `humdrumR`, see the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
manual. Effective use of the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
function is essential to use of `metlev()`, `metcount()`, and
`metsubpos()`.

### Metric levels

`metlev()` identifies the "highest" (longest) metric level that each
onset lands in/on: For example, in 4/4 time:

- An onset on the downbeat is at the highest level of all, the
  whole-note level;

- An onset on beat three of the 4/4 measure is on the half-note level;

- Onsets on the backbeats (beats two and four) fall on the quarter-note
  level;

- The next level down is the eighth-note level, in between each
  quarter-note beat;

- etc.

The `metlev()` output expresses beat levels as the duration of the the
level, in
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
format by default. So the whole-note level is `"1"` and the quarter-note
level (backbeats) is `"4"`. You can specify a different
[deparsing](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md)
function (like
[`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md))
using the `deparser` argument. (If `deparser` is `NULL`,
[`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
numbers are returned.)

Another option is to express the metric levels simply as natural
numbers, which you can achieve with the argument `value = FALSE`. In
this case, the top level of the meter is `1`, which each next
lower-level incrementing up: i.e., the quarter-note level (of 4/4) would
be `3`, while the sixteenth-note level would be `5`.

#### (Full) 4/4 meter levels

|                                                                                |       |       |       |       |       |       |       |       |
|--------------------------------------------------------------------------------|-------|-------|-------|-------|-------|-------|-------|-------|
|                                                                                | 1     | &     | 2     | &     | 3     | &     | 4     | &     |
| Level ([`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)) | `"1"` | `"8"` | `"4"` | `"8"` | `"2"` | `"8"` | `"4"` | `"8"` |
| Level (`value = FALSE`)                                                        | `1`   | `4`   | `3`   | `4`   | `2`   | `4`   | `3`   | `4`   |

#### 3/4 meter levels

|                                                                                |       |       |       |       |       |       |
|--------------------------------------------------------------------------------|-------|-------|-------|-------|-------|-------|
|                                                                                | 1     | &     | 2     | &     | 3     | &     |
| Level ([`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)) | `"1"` | `"8"` | `"4"` | `"8"` | `"4"` | `"8"` |
| Level (`value = FALSE`)                                                        | `1`   | `3`   | `2`   | `3`   | `2`   | `3`   |

#### 6/8 meter levels

|                                                                                |       |       |       |        |       |       |
|--------------------------------------------------------------------------------|-------|-------|-------|--------|-------|-------|
|                                                                                | 1     | &     | a     | 2      | &     | a     |
| Level ([`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)) | `"1"` | `"8"` | `"8"` | `"4."` | `"8"` | `"8"` |
| Level (`value = FALSE`)                                                        | `1`   | `3`   | `3`   | `2`    | `3`   | `3`   |

### Metric counts

The `metcount()` function counts one beat level in a metric hierarchy
within the span of highest ('measure') level (by default). Which level
you want to count is controlled by the `level` argument, which can be
either be a `character` string in
[`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
format or a natural number (`1` is top level, `2` is next lowest level,
etc.). If you tell `metcount()` to count the highest (measure) level in
the meter, it will count bars.

An additional option is to count beats within the next *highest* beat
(which is not necessarily the full bar). For example, if we count
eighth-notes in a 6/8 meter, the default behavior is to count 1, 2, 3,
4, 5, 6. However, if we specify `withinNext = TRUE`, the eighth-notes
will be counted within the next highest 6/8 beat level, which is the
dotted-quarter note. So the eighth notes in a bar would be counted 1, 2,
3, 1, 2, 3. The exact counting behavior can then be further controlled
by changing exactly how the specifying
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
is specified.

#### (Full) 4/4 meter counts:

If `withinNext = FALSE`:

|                 |     |     |     |     |     |     |     |     |
|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|
|                 | 1   | &   | 2   | &   | 3   | &   | 4   | &   |
| `"1"` (whole)   | 1   | 1   | 1   | 1   | 1   | 1   | 1   | 1   |
| `"2"` (half)    | 1   | 1   | 1   | 1   | 2   | 2   | 2   | 2   |
| `"4"` (quarter) | 1   | 1   | 2   | 2   | 3   | 3   | 4   | 4   |
| `"8"` (eighth)  | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   |

If `withinNext = TRUE`:

|                 |     |     |     |     |     |     |     |     |
|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|
|                 | 1   | &   | 2   | &   | 3   | &   | 4   | &   |
| `"1"` (whole)   | 1   | 1   | 1   | 1   | 1   | 1   | 1   | 1   |
| `"2"` (half)    | 1   | 1   | 1   | 1   | 2   | 2   | 2   | 2   |
| `"4"` (quarter) | 1   | 1   | 2   | 2   | 1   | 1   | 2   | 2   |
| `"8"` (eighth)  | 1   | 2   | 1   | 2   | 1   | 2   | 1   | 2   |

#### 3/4 meter counts:

If `withinNext = FALSE`:

|                      |     |     |     |     |     |     |
|----------------------|-----|-----|-----|-----|-----|-----|
|                      | 1   | &   | 2   | &   | 3   | &   |
| `"2."` (dotted-half) | 1   | 1   | 1   | 1   | 1   | 1   |
| `"4"` (quarter)      | 1   | 1   | 2   | 2   | 3   | 3   |
| `"8"` (eighth)       | 1   | 2   | 3   | 4   | 5   | 6   |

If `withinNext = TRUE`:

|                      |     |     |     |     |     |     |
|----------------------|-----|-----|-----|-----|-----|-----|
|                      | 1   | &   | 2   | &   | 3   | &   |
| `"2."` (dotted-half) | 1   | 1   | 1   | 1   | 1   | 1   |
| `"4"` (quarter)      | 1   | 1   | 2   | 2   | 3   | 3   |
| `"8"` (eighth)       | 1   | 2   | 1   | 2   | 1   | 2   |

#### 6/8 meter counts:

If `withinNext = FALSE`:

|                         |     |     |     |     |     |     |
|-------------------------|-----|-----|-----|-----|-----|-----|
|                         | 1   | &   | a   | 2   | &   | a   |
| `"2."` (dotted-half)    | 1   | 1   | 1   | 1   | 1   | 1   |
| `"4."` (dotted-quarter) | 1   | 1   | 1   | 2   | 2   | 2   |
| `"8"` (eighth)          | 1   | 2   | 3   | 4   | 5   | 6   |

If `withinNext = TRUE`:

|                         |     |     |     |     |     |     |
|-------------------------|-----|-----|-----|-----|-----|-----|
|                         | 1   | &   | a   | 2   | &   | a   |
| `"2."` (dotted-half)    | 1   | 1   | 1   | 1   | 1   | 1   |
| `"4."` (dotted-quarter) | 1   | 1   | 1   | 2   | 2   | 2   |
| `"8"` (eighth)          | 1   | 2   | 3   | 1   | 2   | 3   |

#### 4/4 meter with no half-note level:

|                 |     |     |     |     |     |     |     |     |
|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|
|                 | 1   | &   | 2   | &   | 3   | &   | 4   | &   |
| `"1"` (whole)   | 1   | 1   | 1   | 1   | 1   | 1   | 1   | 1   |
| `"4"` (quarter) | 1   | 1   | 2   | 2   | 3   | 3   | 4   | 4   |
| `"8"` (eighth)  | 1   | 2   | 1   | 2   | 1   | 2   | 1   | 2   |

You can do this with `meter('M4/4', fill.levels = 'below')`.

### Metric subpositions

In some cases, onsets may occur which do not land on any beat specified
in the meter. This could be very fast beat levels (e.g., 32nd notes),
triplets, or other tuplets. In these cases, you might consider adding
these levels to the
[`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md);
for example, if you want to have a 32nd-note level in 4/4, you could use
`meter('M4/4', tick = '32')`. For `metlev()` and `metcount()`, the
`offBeats` argument can be set to `FALSE` to cause offbeat onsets to
return `NA`. Another option is to use `metsubpos()`, which measures how
far an onset is from the nearest associated beat in the meter.

By default, off-beat onsets are always associated with the closets
previous position in any level in the meter. If the
`remainderSubdivides` argument is `TRUE`, off-beat onsets are associated
with the previous metric level which the subposition makes an even
subdivision of.

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
[`subpos()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
functions are more basic versions of `metcount()` and `metsubpos()`,
based only on counting a *single* beat level, rather then a hierarchy of
beat levels.

## Examples

``` r
rhythm <- c('4', '8', '8', '4', '8', '16', '16','4.', '8','2')

metlev(rhythm)
#>  [1] "1"  "4"  "8"  "2"  "4"  "8"  "16" "1"  "8"  "2" 
metlev(rhythm, meter = 'M6/8')
#>  [1] "2." "8"  "4." "8"  "2." "8"  "16" "8"  "8"  "2."

metcount(rhythm)
#>  [1] 1 2 2 3 4 4 4 1 2 3
metcount(rhythm, offBeats = FALSE)
#>  [1]  1  2 NA  3  4 NA NA  1 NA  3
metcount(rhythm, meter = 'M6/8', offBeats = FALSE)
#>  [1]  1 NA  2 NA  1 NA NA NA NA  1

# chorales
chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn') 
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/.*krn' matches 10 text files in 1 directory.
#> Ten files read from disk.
#> Validating ten files...
#> all valid.
#> Parsing ten files...
#> Assembling corpus...
#> Done!

within(chorales, metlev(Token, pickup = Bar < 1))
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
#>            20:                4              4              4              4
#>            21:               =1             =1             =1             =1
#>            22:               2.             2.             2.             2.
#>            23:                4              4              4              .
#>            24:                .             16              .              .
#>            25:                4              4              4              4
#>            26:               =2             =2             =2             =2
#>            27:               2.             2.             2.             2.
#>            28:                4              4              .              .
#>            29:                .              .              .             16
#>            30:                4              4              4              4
#>            31:               =3             =3             =3             =3
#>            32:               2.             2.             2.             2.
#>            33:                .             16             16              .
#>            34:                4              4              4              .
#>            35:               16              .             16             16
#>            36:                4              4              4              4
#>            37:               =4             =4             =4             =4
#>            38:               2.             2.             2.             2.
#>            39:                4              4              4              4
#>            40:               =5             =5             =5             =5
#>            41:               2.             2.             2.             2.
#>            42:                4              4              4              .
#>            43:                4              4              4              4
#>            44:               =6             =6             =6             =6
#>            45:               2.             2.             2.             2.
#>            46:                4              4              .              4
#>            47:                4              4              4              .
#>            48:                .             16              .              .
#>            49:               =7             =7             =7             =7
#>            50:               2.             2.             2.             2.
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (eight more pieces...)
#> 
#> ######################## vvv chor010.krn vvv #########################
#>   1-50::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            51:                4              4              4              4
#>            52:                .              .             16              .
#>            53:               =7             =7             =7             =7
#>            54:                1              1              1              1
#>            55:                4              4              4              4
#>            56:               16             16              .              .
#>            57:                4              4              4              4
#>            58:                .              .             16             16
#>            59:                4              4              4              4
#>            60:               =8             =8             =8             =8
#>            61:                1              1              1              1
#>            62:                4              4              4              4
#>            63:               =9             =9             =9             =9
#>            64:                1              1              1              1
#>            65:                4              4              4              4
#>            66:               16              .              .              .
#>            67:                4              4              4              4
#>            68:                .              .             16              .
#>            69:                4              4              4              4
#>            70:              =10            =10            =10            =10
#>            71:                1              1              1              1
#>            72:                .             16              .              .
#>            73:                4              .              4              4
#>            74:                .             16              .              .
#>            75:                4              4              4              4
#>            76:              =11            =11            =11            =11
#>            77:                1              1              1              1
#>            78:                4              4              4              4
#>            79:                4              4              4              4
#>            80:                .              .             16              .
#>            81:              =12            =12            =12            =12
#>            82:                1              1              1              1
#>            83:                4              4              4              4
#>            84:                4              4              4              4
#>            85:                4              4              .              4
#>            86:              =13            =13            =13            =13
#>            87:                1              1              1              1
#>            88:                4              4              4              .
#>            89:                4              4              4              .
#>            90:               ==             ==             ==             ==
#>            91:               *-             *-             *-             *-
#>            92:  !!!hum2abc: -Q ''
#>            93:  !!!title: @{PC#}. @{OTL@@DE}
#>            94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
#>            95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
#>            96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
#>            97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
#>            98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>            99:  !!!EED:  Craig Stuart Sapp
#>           100:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor010.krn ^^^ #########################
#>               (***four global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of ten pieces.
#> 
#>    Data fields: 
#>           Token                                      :: character
#>          *humdrumR:::metlev(Token, pickup = Bar < 1) :: character
#> 

within(chorales, metcount(Token, pickup = Bar < 1, fill.levels = 'below'))
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
#>            20:                3              3              3              3
#>            21:               =1             =1             =1             =1
#>            22:                1              1              1              1
#>            23:                2              2              2              .
#>            24:                .              2              .              .
#>            25:                3              3              3              3
#>            26:               =2             =2             =2             =2
#>            27:                1              1              1              1
#>            28:                2              2              .              .
#>            29:                .              .              .              2
#>            30:                3              3              3              3
#>            31:               =3             =3             =3             =3
#>            32:                1              1              1              1
#>            33:                .              1              1              .
#>            34:                2              2              2              .
#>            35:                2              .              2              2
#>            36:                3              3              3              3
#>            37:               =4             =4             =4             =4
#>            38:                1              1              1              1
#>            39:                3              3              3              3
#>            40:               =5             =5             =5             =5
#>            41:                1              1              1              1
#>            42:                2              2              2              .
#>            43:                3              3              3              3
#>            44:               =6             =6             =6             =6
#>            45:                1              1              1              1
#>            46:                2              2              .              2
#>            47:                3              3              3              .
#>            48:                .              3              .              .
#>            49:               =7             =7             =7             =7
#>            50:                1              1              1              1
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (eight more pieces...)
#> 
#> ######################## vvv chor010.krn vvv #########################
#>   1-50::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            51:                4              4              4              4
#>            52:                .              .              4              .
#>            53:               =7             =7             =7             =7
#>            54:                1              1              1              1
#>            55:                2              2              2              2
#>            56:                2              2              .              .
#>            57:                3              3              3              3
#>            58:                .              .              3              3
#>            59:                4              4              4              4
#>            60:               =8             =8             =8             =8
#>            61:                1              1              1              1
#>            62:                3              3              3              3
#>            63:               =9             =9             =9             =9
#>            64:                1              1              1              1
#>            65:                2              2              2              2
#>            66:                2              .              .              .
#>            67:                3              3              3              3
#>            68:                .              .              3              .
#>            69:                4              4              4              4
#>            70:              =10            =10            =10            =10
#>            71:                1              1              1              1
#>            72:                .              1              .              .
#>            73:                2              .              2              2
#>            74:                .              2              .              .
#>            75:                3              3              3              3
#>            76:              =11            =11            =11            =11
#>            77:                1              1              1              1
#>            78:                3              3              3              3
#>            79:                4              4              4              4
#>            80:                .              .              4              .
#>            81:              =12            =12            =12            =12
#>            82:                1              1              1              1
#>            83:                2              2              2              2
#>            84:                3              3              3              3
#>            85:                4              4              .              4
#>            86:              =13            =13            =13            =13
#>            87:                1              1              1              1
#>            88:                2              2              2              .
#>            89:                3              3              3              .
#>            90:               ==             ==             ==             ==
#>            91:               *-             *-             *-             *-
#>            92:  !!!hum2abc: -Q ''
#>            93:  !!!title: @{PC#}. @{OTL@@DE}
#>            94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
#>            95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
#>            96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
#>            97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
#>            98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>            99:  !!!EED:  Craig Stuart Sapp
#>           100:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor010.krn ^^^ #########################
#>               (***four global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of ten pieces.
#> 
#>    Data fields: 
#>           Token                                                               :: character
#>          *humdrumR:::metcount(Token, pickup = Bar < 1, fill.levels = "below") :: numeric
#> 
```
