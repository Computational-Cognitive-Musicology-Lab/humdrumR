# Divide humdrumR data into groups

The `group_by()` method for [humdrumR
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
is used to define [grouping
factors](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md)
in your data fields. Note that groups created by grouping factors 1) are
not necessarily contiguous and 2) always exhaustively partition the
data. The
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
function can be used, as an alternative, to generate groups ("windows")
which are always contiguous and/or exclude some data.

The `ungroup()` function removes grouping from a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

Once groups are created, the `groups()` function can be used to tabulate
the number of tokens in each group, and find their indices in the
[humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

## Usage

``` r
# S3 method for class 'humdrumR'
group_by(.data, ..., .add = TRUE)

# S3 method for class 'humdrumR'
ungroup(x, ...)

groups(humdrumR, dataTypes = "D")
```

## Arguments

- .data, x, humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Any number of expressions to evaluate.***

  These expressions can reference
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the data by name, as well as variables outside the data.

  If the expressions are named, the names are used to name the new
  fields.

- .add:

  ***Should groups be added to existing groups?***

  Defaults to `TRUE`.

  Must be a single `logical` value: an on/off switch.

- dataTypes:

  ***Which types of humdrum records to include.***

  Defaults to `"D"`.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.)

## Details

The `group_by()` method for [humdrumR
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
takes any number of expressions as `...` arguments. These expressions
may simply be `character` strings or symbols indicating existing
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
in the data— For example, `group_by(Piece, Spine)`. However, the
[expressions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
can also be arbitrary "expression arguments" which are passed to
[within()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
to generate new fields for grouping. For example, you could group spines
into even and odd groups with `group_by(Spine %% 2)`.

The `group_by()` function returns a new [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
with grouping fields activated. The grouping fields, and the number of
groups, are shown when the humdrumR data is printed. The `groups()` can
be used to gather more information about groups: `group()` returns a
`data.table` with one row representing each group, the value of each
grouping field indicated, and with one or more columns indicating the
number of tokens of each type in the group (the desired types are
indicated by the `dataTypes` argument).

By default, each call to `group_by.humdrumR()` *adds* groups to any
groups already existing in the data. If `.add = FALSE`, any preexisting
groups are removed before creating new groups. Groups can be explicitly
removed using `ungroup()`.

When `.add = TRUE`, each call to `group_by()` computes new fields
*using* the preexisting groups, just like any normal call to
[within()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).
This means that you can, in some cases, create different groupings
depending on the order you create groups. For example, imagine we want
to divide each piece in our data into two groups: all pitches higher
than average in one group and all pitches lower than average in the
other. Consider a `humData` corpus with a numeric `Semits` field, and we
run these two different calls:

    humData |>
       group_by(Piece) |>
       group_by(Semits > mean(Semits))

    humData |>
       group_by(Semits > mean(Semits)) |>
       group_by(Piece)

In the first call, we first group by `Piece`, then divide each piece by
the *piece's* average. In the second example, we divide the corpus into
two halves based on the *overall* (cross-piece) average, *then* we
divide it into pieces.

## See also

Other Contextual grouping functions.:
[`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)

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

humData |> 
   group_by(Piece, Spine) |>
   groups()
#>     Piece Spine     D
#>     <int> <int> <int>
#>  1:     1     1    63
#>  2:     1     2    59
#>  3:     1     3    61
#>  4:     1     4    46
#>  5:     2     1    61
#>  6:     2     2    62
#>  7:     2     3    55
#>  8:     2     4    53
#>  9:     3     1    52
#> 10:     3     2    50
#> 11:     3     3    47
#> 12:     3     4    47
#> 13:     4     1    49
#> 14:     4     2    45
#> 15:     4     3    49
#> 16:     4     4    43

humData |> 
   group_by(Piece, Spine %% 2) |>
   groups()
#>    Spine%%2 Piece     D
#>       <num> <int> <int>
#> 1:        1     1   124
#> 2:        0     1   105
#> 3:        1     2   116
#> 4:        0     2   115
#> 5:        1     3    99
#> 6:        0     3    97
#> 7:        1     4    98
#> 8:        0     4    88
   
humData |> 
   group_by(Piece, Bar) |>
   mutate(NotesPerBar = length(Token)) |>
   ungroup()
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
#>     20:                4              4              4              4
#>     21:               =1             =1             =1             =1
#>     22:               12             12             12             12
#>     23:               12             12             12              .
#>     24:                .             12              .              .
#>     25:               12             12             12             12
#>     26:               =2             =2             =2             =2
#>     27:               11             11             11             11
#>     28:               11             11              .              .
#>     29:                .              .              .             11
#>     30:               11             11             11             11
#>     31:               =3             =3             =3             =3
#>     32:               16             16             16             16
#>     33:                .             16             16              .
#>     34:               16             16             16              .
#>     35:               16              .             16             16
#>     36:               16             16             16             16
#>     37:               =4             =4             =4             =4
#>     38:                8              8              8              8
#>     39:                8              8              8              8
#>     40:               =5             =5             =5             =5
#> 41-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-63::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     64:               18             18             18             18
#>     65:               18              .              .             18
#>     66:               18             18             18             18
#>     67:               18             18             18             18
#>     68:               18             18             18             18
#>     69:               =7             =7             =7             =7
#>     70:               20             20             20             20
#>     71:                .              .             20              .
#>     72:               20             20             20             20
#>     73:               20             20             20             20
#>     74:               20              .              .              .
#>     75:               20             20             20             20
#>     76:                .              .             20             20
#>     77:               =8             =8             =8             =8
#>     78:               16             16             16             16
#>     79:               16             16             16             16
#>     80:               16             16             16             16
#>     81:               16             16             16             16
#>     82:               =9             =9             =9             =9
#>     83:               18             18             18             18
#>     84:               18             18             18             18
#>     85:               18              .              .              .
#>     86:               18             18             18             18
#>     87:               18             18             18             18
#>     88:                .             18              .              .
#>     89:              =10            =10            =10            =10
#>     90:               11             11             11             11
#>     91:               11              .             11             11
#>     92:               11             11             11             11
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
#>          *NotesPerBar :: integer
#>           Token       :: character
#> 
   
humData |> 
   semits() |>
   group_by(Piece, Spine) |>
   with(mean(Semits))
#> Piece1,Spine1 Piece1,Spine2 Piece1,Spine3 Piece1,Spine4 Piece2,Spine1 
#>   -12.4126984     0.2881356     5.5737705    10.4347826    -8.1639344 
#> Piece2,Spine2 Piece2,Spine3 Piece2,Spine4 Piece3,Spine1 Piece3,Spine2 
#>     0.7903226     6.3818182    10.7358491    -7.7115385    -0.1200000 
#> Piece3,Spine3 Piece3,Spine4 Piece4,Spine1 Piece4,Spine2 Piece4,Spine3 
#>     5.6170213    10.8297872    -8.3265306     0.3111111     5.3877551 
#> Piece4,Spine4 
#>    10.2093023 
```
