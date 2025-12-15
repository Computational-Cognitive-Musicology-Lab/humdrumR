# Count beats or measures

The `timecount()` function takes a vector of rhythmic duration values
and counts (in the musical sense) the number of *beats* (or *measures*)
which have occurred since the starting point, associating each rhythmic
onsets with a beat. The `subpos()` function is paired with
`timecount()`, computing how far (in rhythmic time) each onset is from
its associated beat; if `subpos()` returns `0`, this means that an onset
is *on* the beat. Finally, `onbeat()` is simply a convenient shorthand
for `subpos() == 0`, returning a `logical` vector for indicating where
onsets fall on or off beat.

## Usage

``` r
# Default S3 method
timecount(
  dur,
  unit = rational(1L),
  start = 1L,
  phase = 0,
  pickup = NULL,
  offBeats = TRUE,
  groupby = list(),
  ...
)

timecount(
  dur,
  unit = rational(1L),
  start = 1L,
  phase = 0,
  pickup = NULL,
  offBeats = TRUE,
  groupby = list(),
  ...
)

# Default S3 method
subpos(
  dur,
  unit = rational(1L),
  phase = 0,
  pickup = NULL,
  deparser = duration,
  ...,
  groupby = list()
)

subpos(
  dur,
  unit = rational(1L),
  phase = 0,
  pickup = NULL,
  deparser = duration,
  ...,
  groupby = list()
)

# Default S3 method
onbeat(dur, unit = rational(1L), groupby = list(), ...)

onbeat(dur, unit = rational(1L), groupby = list(), ...)
```

## Arguments

- dur:

  ***An input vector of rhythmic durations.***

  Must be a `character` or `numeric` vector.

  Is parsed using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  Wherever the input can't be parsed as a duration, that element is
  treated as a duration of zero.

- unit:

  ***The size of "beat" (or measure) to count.***

  Defaults to a whole-note (one measure of 4/4 time).

  Must be a `character` or `numeric` vector, or a list of such vectors;
  must be a singleton or the same length as `dur`.

  Is parsed as a duration using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  If the input can't be parsed as a duration, the output will be all
  `NA`.

- start:

  ***The number to start counting from.***

  Must be a single whole-number value (either `numeric` or `integer`).

- phase:

  ***The phase offset between onsets and beats.***

  Defaults to `0`.

  Must be a `character` or `numeric` vector; must be length `1` or the
  same length as `dur`; The duration of `phase` must be smaller than the
  smallest duration value in `unit`.

  Is parsed as a duration using
  [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md);
  If the input can't be parsed as a duration, an error occurs.

- pickup:

  ***Indicates which leading values in the input are pickups, if any.***

  Defaults to `NULL`.

  Must be `NULL`, or a `logical` vector of the same length as `dur`.

- offBeats:

  ***Should off-beat onsets be numbered in the output, or `NA`?***

  Defaults to `TRUE`.

  Must be a single `logical` value: an on/off switch.

- groupby:

  ***Optional vectors to group by and count within.***

  Defaults to empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a [`list()`](https://rdrr.io/r/base/list.html), which is
  either empty or contains vectors which are all the same length as
  `dur`. To function as a by-record timeline, the `groupby` list must
  include a *named* `Piece` and `Record` vectors. Luckily, these are
  automatically passed by
  [with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
  so you won't need to worry about it!

## Details

In many basic use cases, using `timecount()` is essentially the same as
using `floor(timeline())`. However, `timecount()` gives us a few
additional options which add musicological power compared to
[`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md).
(`timecount()` also starts from `1` not `0`, as
[`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)
does.)

The first beat in an input vector is assigned the value of the `start`
argument, which defaults to `start = 1L`. There is no 'zeroth' count, as
the first beat occurs at the instant of the starting time—i.e., the
first onset in the input vector. Every rhythmic onset is associated with
one beat, but multiple onsets may occur within the same beat—thus the
output of `timecount()` assigns (rounds) each onset to the previous beat
onset. However, if `offBeats = FALSE`, only onsets that *land* on a beat
are counted, with offbeat values returning `NA`.

The `phase` controls how offbeat onsets are associated with nearby
beats. `phase` is
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
as a rhythmic value and must be smaller than the smallest `beat` value.
The `phase` argument shifts the "boundary" between beats backwards,
before the beat onset. By default, `phase = 0` so the beat-association
boundary lands on the beat: only onsets on or after each beat "belong"
to that beat. If `phase = '8'`, the beat boundary is pushed back to
capture one eighth-note *before* the beat itself. This can be used to,
for example, associate the last 3/8s of a measure with the next measure
(like pick ups); This could be achieved with a command like
`timecount(dur, beat = '1', phase = 3/8)`.

## "Beats"

The `unit` argument is used to indicate what size of beat you want to
count. The default `unit` is a whole note, equivalent to a measure of
`M4/4` time. The `unit` argument uses the [rhythm
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md),
so it can understand unit values input in a variety of formats: thus,
you could specify quarter-note units as either `unit = '4'` or
`unit = 0.25`. The parser also understands how to parse the (full)
duration of time signature: for example, `unit = 'M3/4'` would use a
dotted-half-note unit (`'2.'`).

### Changing meter

If your data has changing meters (either between pieces, or within
pieces), you can specify the `unit` argument as a vector which is the
same length as `dur`, indicating the beat size at each moment/index.
This feature is very easy to use with any dataset that includes time
signature interpretations, like `"*M4/4"`; these interpetations, if
present, are automatically [read
into](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md) a
field called `TimeSignature`. For such a dataset, you can simply pass
the `TimeSignature` field to the `unit` argument of `timecount()`, and
the measures of the piece will be correctly counted (even when
changing!): `timecount(x, unit = TimeSignature)`. Alternatively, you can
use the
[`tactus()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tactus.md)
command to extract the tactus beat from a time signature, like
`timecount(x, unit = tactus(TimeSignature))`.

### Irregular meter

Some musical meters consist of a pattern of irregular beats. For
example, the meter `M7/8` is often realized as two "short" beats (two
eigth-notes each) and one "long" beat (three eigth-notes), forming a 2 +
2 + 3 pattern. If we want to count each eighth-note, we can simply
specify `unit = '8'` and get the `M7/8` beats counted as c(`1`, `3`,
`5`). However, if we want to count each short *or* long beat as a single
unit, we must specify the desired pattern as a `list` of beat durations:
for example, `unit = list(c("4", "4", "4."))`. Let's see what these two
cases look like, applied to two `M7/8` measures of straight
eighth-notes:

    rhythm <- rep('8', 14)

    timecount(rhythm, unit = '8'),

    # output is: 1  2  3  4  5  6  7  8  9 10 11 12 13 14

    timecount(rhythm, unit = list(c('4', '4', '4.')))

    # output is: 1 1 2 2 3 3 3 4 4 5 5 6 6 6

To accommodate changing meters, the `unit` argument can still accept
`list` of such patterns, so long as the list is the same length as
`dur`.

## Pickups

Another option is to pass the `pickup` argument a logical vector of the
same length as the input `dur`. Within each piece/group, any block of
`TRUE` values at the *beginning* of the `pickup` vector indicate a
pickup. The *first* index where the `pickup` logical is `FALSE` is used
as the location of beat `1`: all the earlier (`pickup == TRUE`) points
will be negative counts, counting backwards from the start. In
`humdrumR`, and datapoints before the first barline record (`=`) are
labeled `Bar == 0` in the `Bar`
[field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
Thus, a common use for the `pickup` argument is
`within(humData, timecount(Token, pickup = Bar < 1)`, which makes the
downbeat of the first complete bar `1` the stating point—any notes in
pickup bars are give negative counts.

**Note that there is never a 'beat zero'.** Beats before the starting
point progress directly from `-1` to `1` (the start). As a result, doing
arithmetic or other math with beat "counts" can be problematic when
using a `pickup` argument. It may be better to use `round(timeline())`
in cases where you want to do much math with counts.

## See also

`timecount()` and `subpos()` are closely related to the
[`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)
function. The
[`metcount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/metlev.md)
function applies `timecount()` within a metric framework.

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

show(within(humData, timecount(Token, unit = TimeSignature, pickup = Bar < 1)))
#> ######################## vvv chor001.krn vvv #########################
#>      1:  !!!COM: Bach, Johann Sebastian
#>      2:  !!!CDT: 1685/02/21/-1750/07/28/
#>      3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>      4:  !!!OTL@EN:      From the Depths of My Heart
#>      5:  !!!SCT: BWV 269
#>      6:  !!!PC#: 1
#>      7:  !!!AGN: chorale
#>      8:           **kern         **kern         **kern         **kern
#>      9:           *ICvox         *ICvox         *ICvox         *ICvox
#>     10:           *Ibass        *Itenor         *Ialto        *Isoprn
#>     11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
#>     12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>     13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>     14:              *>A            *>A            *>A            *>A
#>     15:          *clefF4       *clefGv2        *clefG2        *clefG2
#>     16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
#>     17:              *G:            *G:            *G:            *G:
#>     18:            *M3/4          *M3/4          *M3/4          *M3/4
#>     19:           *MM100         *MM100         *MM100         *MM100
#>     20:               -1             -1             -1             -1
#>     21:               =1             =1             =1             =1
#>     22:                1              1              1              1
#>     23:                1              1              1              .
#>     24:                .              1              .              .
#>     25:                1              1              1              1
#>     26:               =2             =2             =2             =2
#>     27:                2              2              2              2
#>     28:                2              2              .              .
#>     29:                .              .              .              2
#>     30:                2              2              2              2
#>     31:               =3             =3             =3             =3
#>     32:                3              3              3              3
#>     33:                .              3              3              .
#>     34:                3              3              3              .
#>     35:                3              .              3              3
#>     36:                3              3              3              3
#>     37:               =4             =4             =4             =4
#>     38:                4              4              4              4
#>     39:                4              4              4              4
#>     40:               =5             =5             =5             =5
#>     41:                5              5              5              5
#>     42:                5              5              5              .
#>     43:                5              5              5              5
#>     44:               =6             =6             =6             =6
#>     45:                6              6              6              6
#>     46:                6              6              .              6
#>     47:                6              6              6              .
#>     48:                .              6              .              .
#>     49:               =7             =7             =7             =7
#>     50:                7              7              7              7
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:                4              4              4              4
#>     55:                .              .              4              .
#>     56:               =5             =5             =5             =5
#>     57:                5              5              5              5
#>     58:                .              .              5              .
#>     59:                5              5              5              5
#>     60:                5              .              .              .
#>     61:                5              5              5              5
#>     62:                5              5              5              5
#>     63:               =6             =6             =6             =6
#>     64:                6              6              6              6
#>     65:                6              .              .              6
#>     66:                6              6              6              6
#>     67:                6              6              6              6
#>     68:                6              6              6              6
#>     69:               =7             =7             =7             =7
#>     70:                7              7              7              7
#>     71:                .              .              7              .
#>     72:                7              7              7              7
#>     73:                7              7              7              7
#>     74:                7              .              .              .
#>     75:                7              7              7              7
#>     76:                .              .              7              7
#>     77:               =8             =8             =8             =8
#>     78:                8              8              8              8
#>     79:                8              8              8              8
#>     80:                8              8              8              8
#>     81:                8              8              8              8
#>     82:               =9             =9             =9             =9
#>     83:                9              9              9              9
#>     84:                9              9              9              9
#>     85:                9              .              .              .
#>     86:                9              9              9              9
#>     87:                9              9              9              9
#>     88:                .              9              .              .
#>     89:              =10            =10            =10            =10
#>     90:               10             10             10             10
#>     91:               10              .             10             10
#>     92:               10             10             10             10
#>     93:               ==             ==             ==             ==
#>     94:               *-             *-             *-             *-
#>     95:  !!!hum2abc: -Q ''
#>     96:  !!!title: @{PC#}. @{OTL@@DE}
#>     97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Jo***
#>     98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: B***
#>     99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint:***
#>    100:  !!!YOR4: Chorales (New York: Associated Music Publi***
#>    101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, p***
#>    102:  !!!EED:  Craig Stuart Sapp
#>    103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ #########################
#>               (***five global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>           Token                     :: character
#>          *humdrumR:::timecount(...) :: integer
#> 

show(within(humData, timecount(Token, unit = tactus(TimeSignature))))
#> ######################## vvv chor001.krn vvv #########################
#>      1:  !!!COM: Bach, Johann Sebastian
#>      2:  !!!CDT: 1685/02/21/-1750/07/28/
#>      3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>      4:  !!!OTL@EN:      From the Depths of My Heart
#>      5:  !!!SCT: BWV 269
#>      6:  !!!PC#: 1
#>      7:  !!!AGN: chorale
#>      8:           **kern         **kern         **kern         **kern
#>      9:           *ICvox         *ICvox         *ICvox         *ICvox
#>     10:           *Ibass        *Itenor         *Ialto        *Isoprn
#>     11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
#>     12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>     13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>     14:              *>A            *>A            *>A            *>A
#>     15:          *clefF4       *clefGv2        *clefG2        *clefG2
#>     16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
#>     17:              *G:            *G:            *G:            *G:
#>     18:            *M3/4          *M3/4          *M3/4          *M3/4
#>     19:           *MM100         *MM100         *MM100         *MM100
#>     20:               -1             -1             -1             -1
#>     21:               =1             =1             =1             =1
#>     22:                1              1              1              1
#>     23:                2              2              2              .
#>     24:                .              2              .              .
#>     25:                3              3              3              3
#>     26:               =2             =2             =2             =2
#>     27:                4              4              4              4
#>     28:                5              5              .              .
#>     29:                .              .              .              5
#>     30:                6              6              6              6
#>     31:               =3             =3             =3             =3
#>     32:                7              7              7              7
#>     33:                .              7              7              .
#>     34:                8              8              8              .
#>     35:                8              .              8              8
#>     36:                9              9              9              9
#>     37:               =4             =4             =4             =4
#>     38:               10             10             10             10
#>     39:               12             12             12             12
#>     40:               =5             =5             =5             =5
#>     41:               13             13             13             13
#>     42:               14             14             14              .
#>     43:               15             15             15             15
#>     44:               =6             =6             =6             =6
#>     45:               16             16             16             16
#>     46:               17             17              .             17
#>     47:               18             18             18              .
#>     48:                .             18              .              .
#>     49:               =7             =7             =7             =7
#>     50:               19             19             19             19
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:               16             16             16             16
#>     55:                .              .             16              .
#>     56:               =5             =5             =5             =5
#>     57:               17             17             17             17
#>     58:                .              .             17              .
#>     59:               18             18             18             18
#>     60:               18              .              .              .
#>     61:               19             19             19             19
#>     62:               20             20             20             20
#>     63:               =6             =6             =6             =6
#>     64:               21             21             21             21
#>     65:               21              .              .             21
#>     66:               22             22             22             22
#>     67:               23             23             23             23
#>     68:               24             24             24             24
#>     69:               =7             =7             =7             =7
#>     70:               25             25             25             25
#>     71:                .              .             25              .
#>     72:               26             26             26             26
#>     73:               27             27             27             27
#>     74:               27              .              .              .
#>     75:               28             28             28             28
#>     76:                .              .             28             28
#>     77:               =8             =8             =8             =8
#>     78:               29             29             29             29
#>     79:               30             30             30             30
#>     80:               31             31             31             31
#>     81:               32             32             32             32
#>     82:               =9             =9             =9             =9
#>     83:               33             33             33             33
#>     84:               34             34             34             34
#>     85:               34              .              .              .
#>     86:               35             35             35             35
#>     87:               36             36             36             36
#>     88:                .             36              .              .
#>     89:              =10            =10            =10            =10
#>     90:               37             37             37             37
#>     91:               38              .             38             38
#>     92:               39             39             39             39
#>     93:               ==             ==             ==             ==
#>     94:               *-             *-             *-             *-
#>     95:  !!!hum2abc: -Q ''
#>     96:  !!!title: @{PC#}. @{OTL@@DE}
#>     97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Jo***
#>     98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: B***
#>     99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint:***
#>    100:  !!!YOR4: Chorales (New York: Associated Music Publi***
#>    101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, p***
#>    102:  !!!EED:  Craig Stuart Sapp
#>    103:  !!!EEV:  2009/05/22
#> ######################## ^^^ chor004.krn ^^^ #########################
#>               (***five global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>           Token                                                     :: character
#>          *humdrumR:::timecount(Token, unit = tactus(TimeSignature)) :: integer
#> 
 
  
```
