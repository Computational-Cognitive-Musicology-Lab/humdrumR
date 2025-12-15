# Filter humdrum data

HumdrumR defines [subset()](https://rdrr.io/r/base/subset.html) (base R)
and [filter()](https://dplyr.tidyverse.org/reference/filter.html)
(tidyverse) methods for [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)—these
two `.humdrumR` methods are synonymous, working exactly the same. They
are used to "filter" the contents of the underlying [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
R's standard indexing operators (`[]` and `[[]]`) can also be used to
filter data— you can read about these indexing options
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/indexHumdrum.md)—however,
the `subset()`/`filter()` can accomplish much more sophisticated
filtering commands than the indexing methods.

Filtering with `subset()`/`filter()` is (by default) not destructive,
allowing you to recover the filtered data using `removeSubset()` or
`unfilter()` (which are also synonyms).

## Usage

``` r
# S3 method for class 'humdrumR'
subset(x, ..., dataTypes = "D", .by = NULL, removeEmptyPieces = TRUE)

# S3 method for class 'humdrumR'
filter(.data, ..., dataTypes = "D", .by = NULL, removeEmptyPieces = TRUE)

removeEmptyFiles(x)

removeEmptyPieces(x)

removeEmptySpines(x)

removeEmptyPaths(x)

removeEmptyRecords(x)

removeEmptyStops(x)

removeSubset(humdrumR, fields = dataFields(humdrumR), complement = NULL)

unfilter(humdrumR, fields = dataFields(humdrumR), complement = NULL)

complement(humdrumR, fields = dataFields(humdrumR))
```

## Arguments

- x, .data, humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Arbitrary expressions passed to
  [with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).***

  The "within" expression(s) must evaluate to either scalar or
  full-length `logical` values.

- dataTypes:

  ***Which types of humdrum records to include.***

  Defaults to `"D"`.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.)

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

- removeEmptyPieces:

  ***Should empty pieces be removed?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- fields:

  **Which fields to unfilter or complement?**

  Defaults to all data fields in the `humdrumR` data.

  Must be `character` strings, partially matching data
  [field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the input data.

- complement:

  **Which field to use as the subset complement to restore?**

  By default `NULL`, which means each data field's original complement
  is used.

  Must be a single `character` string, partially matching a
  [field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the input data.

## Details

`subset()` and `filter()` are passed one or more expressions which are
using the fields of the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
using a call to
[within](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md).
This evaluation can thus include all of
[`within.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)'s
functionality (and arguments) including group-apply. The only
requirement is that the expressions/functions fed to
`subset()`/`filter()` *must* be return a logical (`TRUE`/`FALSE`) vector
(`NA` values are treated as `FALSE`). The returned vector must either be
scalar (length `1`), or be the same length as the input data (the number
of rows in the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)).
If the logical result is scalar, it will be recycled to match the input
length: this is useful in combination with
[`group_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md);
for example, you can split the data into groups, then return a single
`TRUE` or `FALSE` for each group, causing the whole group to be filtered
or not.

Note that `subset()`/`filter()` are incompatible with [contextual
windows](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md);
if your data has contextual windows defined, they will be removed (with
a warning message) before filtering.

## Nullifying data

When using `subset()`/`filter()`, humdrumR doesn't actually delete the
data you filter out. Instead, what these functions do is set all
filtered data fields to `NA` (null) values, and changing their data type
to `"d"`. This ensures that the humdrum-syntax of the data is not broken
by filtering! Thus, when you print a filtered [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you'll see all the filtered data points turned to null data (`.`).
Since, most `humdrumR` functions ignore null data (`d`) by default, the
data is effectively filtered out for most practical purposes. However,
if you need to use those null (`'d'`) data points (like, with
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md)),
they can be accessed by setting `dataTypes = 'Dd'` in many functions.
See the
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md)
documentation for examples.

## Truly removing data

In many cases, filtering out large parts of your data leaves a bunch of
empty null data points (`"."`) in your printout...which maybe be
difficult to read. If you *want* to **actually** remove these filtered
data points, you can call `removeEmptyFiles()`, `removeEmptyPieces()`,
`removeEmptySpines()`, `removeEmptyPaths()`, `removeEmptyRecords()`, or
`removeEmptyStops()`. These functions will safely remove null data
without breaking the humdrum syntax; They do this by going through each
piece/spine/path/record and checking if *all* the data in that region is
null; if, and only if, *all* the data is null, that portion of data will
be removed.

By default, `subset.humdrumR()` automatically calls
`removeEmptyPieces()` before returning. However, you can stop this by
specifying `removeEmptyPieces = FALSE`.

## Renumbering

If filtered pieces, files, or spines are removed from a corpus (using
`removeEmptyPieces()` or `removeEmptySpines()`) the `File`, `Piece`,
`Record` and/or `Spine` fields are renumbered to represented the
remaining regions, starting from `1`. For example, if you have a corpus
of 10 pieces and remove the first piece (`Piece == 1`), the remaining
pieces are renumbered from `2:10` to `1:9`. Spine/record renumbering
works the same, except it is done independently *within* each piece.

## Complements (unfiltering)

When `subset()` is applied, `humdrumR` stores the complement of the
subset of each data field is retained (unless an explicit
`removeEmpty...()` function is called). The `removeSubset()` or
`unfilter()` functions can be used to restore the original data, by
combining the subset with the complement. The `fields` argument can be
used to control which data fields are unfiltered—by default, all data
fields are unfiltered.

Normally, each data field is restored with its own complement data.
However, the `complement` argument can be used to specify a field to use
as the complement. This allows you to, for instance, combine different
parts of separate fields into a single view. See the last example in the
"Examples" section for a demonstration of this argument.

The `complement()` function will directly swap the data-field subsets
with their complements.

## See also

The [indexing
operators](https://humdrumR.ccml.gtcmt.gatech.edu/reference/indexHumdrum.md)
`[]` and `[[]]` can be used as shortcuts for common `subset` calls.

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

# remove spine 1 (non destructive)
humData |> subset(Spine > 1)
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
#>     20:                .             4B             4d             4g
#>     21:               =1             =1             =1             =1
#>     22:                .             4B             4d             2g
#>     23:                .            8cL             4e              .
#>     24:                .            8BJ              .              .
#>     25:                .             4A             4d            4dd
#>     26:               =2             =2             =2             =2
#>     27:                .             4G             2d            4.b
#>     28:                .            4F#              .              .
#>     29:                .              .              .             8a
#>     30:                .             4G             4B             4g
#>     31:               =3             =3             =3             =3
#>     32:                .            8cL            8eL            4.g
#>     33:                .            8BJ             8d              .
#>     34:                .             4c             8e              .
#>     35:                .              .           8f#J             8a
#>     36:                .             4d             4g             4b
#>     37:               =4             =4             =4             =4
#>     38:                .            2d;           2f#;            2a;
#>     39:                .             4d             4g             4b
#>     40:               =5             =5             =5             =5
#>     41:                .             4A             4d            2dd
#>     42:                .             4B             4e              .
#>     43:                .             4c            4f#            4cc
#>     44:               =6             =6             =6             =6
#>     45:                .             4d             2g             4b
#>     46:                .             4e              .             2a
#>     47:                .            8dL            4f#              .
#>     48:                .            8cJ              .              .
#>     49:               =7             =7             =7             =7
#>     50:                .            2B;            2d;            2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:                .            4G#            8eL             4b
#>     55:                .              .           8f#J              .
#>     56:               =5             =5             =5             =5
#>     57:                .            4c#           8g#L            4ee
#>     58:                .              .           8a#J              .
#>     59:                .            4F#             4b           4dd#
#>     60:                .              .              .              .
#>     61:                .            4f#            4a#           4cc#
#>     62:                .            4f#             4b           4dd#
#>     63:               =6             =6             =6             =6
#>     64:                .             4B             4b           8eeL
#>     65:                .              .              .          8dd#J
#>     66:                .            4A#            4f#           4cc#
#>     67:                .           4d#;           4f#;            4b;
#>     68:                .             4e            4g#             4b
#>     69:               =7             =7             =7             =7
#>     70:                .             4B            8eL            4ee
#>     71:                .              .           8f#J              .
#>     72:                .             4e            4g#             4b
#>     73:                .             4e             4a           4cc#
#>     74:                .              .              .              .
#>     75:                .            4c#            8eL           8g#L
#>     76:                .              .           8f#J            8aJ
#>     77:               =8             =8             =8             =8
#>     78:                .             4d            4g#             4b
#>     79:                .            4c#            4f#             4a
#>     80:                .           4c#;           4e#;           4g#;
#>     81:                .            [4B             4e            4g#
#>     82:               =9             =9             =9             =9
#>     83:                .            4B]            4d#            4f#
#>     84:                .             4e            4c#             4a
#>     85:                .              .              .              .
#>     86:                .             4e             4B            4g#
#>     87:                .           8d#L             4B            4f#
#>     88:                .            8BJ              .              .
#>     89:              =10            =10            =10            =10
#>     90:                .            2F#            4c#            4c#
#>     91:                .              .             4B            4d#
#>     92:                .           4G#;            4B;            4e;
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
#>          *Token :: character
#> 

# remove spine 1 (destructive)
humData |> subset(Spine > 1) |> removeEmptySpines()
#> ######################## vvv chor001.krn vvv #########################
#>      1:  !!!COM: Bach, Johann Sebastian
#>      2:  !!!CDT: 1685/02/21/-1750/07/28/
#>      3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>      4:  !!!OTL@EN:      From the Depths of My Heart
#>      5:  !!!SCT: BWV 269
#>      6:  !!!PC#: 1
#>      7:  !!!AGN: chorale
#>      8:                **kern              **kern              **kern
#>      9:                *ICvox              *ICvox              *ICvox
#>     10:               *Itenor              *Ialto             *Isoprn
#>     11:              *I"Tenor             *I"Alto          *I"Soprano
#>     12:             *>[A,A,B]           *>[A,A,B]           *>[A,A,B]
#>     13:          *>norep[A,B]        *>norep[A,B]        *>norep[A,B]
#>     14:                   *>A                 *>A                 *>A
#>     15:              *clefGv2             *clefG2             *clefG2
#>     16:                *k[f#]              *k[f#]              *k[f#]
#>     17:                   *G:                 *G:                 *G:
#>     18:                 *M3/4               *M3/4               *M3/4
#>     19:                *MM100              *MM100              *MM100
#>     20:                    4B                  4d                  4g
#>     21:                    =1                  =1                  =1
#>     22:                    4B                  4d                  2g
#>     23:                   8cL                  4e                   .
#>     24:                   8BJ                   .                   .
#>     25:                    4A                  4d                 4dd
#>     26:                    =2                  =2                  =2
#>     27:                    4G                  2d                 4.b
#>     28:                   4F#                   .                   .
#>     29:                     .                   .                  8a
#>     30:                    4G                  4B                  4g
#>     31:                    =3                  =3                  =3
#>     32:                   8cL                 8eL                 4.g
#>     33:                   8BJ                  8d                   .
#>     34:                    4c                  8e                   .
#>     35:                     .                8f#J                  8a
#>     36:                    4d                  4g                  4b
#>     37:                    =4                  =4                  =4
#>     38:                   2d;                2f#;                 2a;
#>     39:                    4d                  4g                  4b
#>     40:                    =5                  =5                  =5
#>     41:                    4A                  4d                 2dd
#>     42:                    4B                  4e                   .
#>     43:                    4c                 4f#                 4cc
#>     44:                    =6                  =6                  =6
#>     45:                    4d                  2g                  4b
#>     46:                    4e                   .                  2a
#>     47:                   8dL                 4f#                   .
#>     48:                   8cJ                   .                   .
#>     49:                    =7                  =7                  =7
#>     50:                   2B;                 2d;                 2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:                   4G#                 8eL                  4b
#>     55:                     .                8f#J                   .
#>     56:                    =5                  =5                  =5
#>     57:                   4c#                8g#L                 4ee
#>     58:                     .                8a#J                   .
#>     59:                   4F#                  4b                4dd#
#>     60:                     .                   .                   .
#>     61:                   4f#                 4a#                4cc#
#>     62:                   4f#                  4b                4dd#
#>     63:                    =6                  =6                  =6
#>     64:                    4B                  4b                8eeL
#>     65:                     .                   .               8dd#J
#>     66:                   4A#                 4f#                4cc#
#>     67:                  4d#;                4f#;                 4b;
#>     68:                    4e                 4g#                  4b
#>     69:                    =7                  =7                  =7
#>     70:                    4B                 8eL                 4ee
#>     71:                     .                8f#J                   .
#>     72:                    4e                 4g#                  4b
#>     73:                    4e                  4a                4cc#
#>     74:                     .                   .                   .
#>     75:                   4c#                 8eL                8g#L
#>     76:                     .                8f#J                 8aJ
#>     77:                    =8                  =8                  =8
#>     78:                    4d                 4g#                  4b
#>     79:                   4c#                 4f#                  4a
#>     80:                  4c#;                4e#;                4g#;
#>     81:                   [4B                  4e                 4g#
#>     82:                    =9                  =9                  =9
#>     83:                   4B]                 4d#                 4f#
#>     84:                    4e                 4c#                  4a
#>     85:                     .                   .                   .
#>     86:                    4e                  4B                 4g#
#>     87:                  8d#L                  4B                 4f#
#>     88:                   8BJ                   .                   .
#>     89:                   =10                 =10                 =10
#>     90:                   2F#                 4c#                 4c#
#>     91:                     .                  4B                 4d#
#>     92:                  4G#;                 4B;                 4e;
#>     93:                    ==                  ==                  ==
#>     94:                    *-                  *-                  *-
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
#>          *Token :: character
#> 

# remove odd numbered bars

humData |> group_by(Bar) |> subset(Bar[1] %% 2 == 1)
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
#>     20:                .              .              .              .
#>     21:               =1             =1             =1             =1
#>     22:               4G             4B             4d             2g
#>     23:               4E            8cL             4e              .
#>     24:                .            8BJ              .              .
#>     25:              4F#             4A             4d            4dd
#>     26:               =2             =2             =2             =2
#>     27:                .              .              .              .
#>     28:                .              .              .              .
#>     29:                .              .              .              .
#>     30:                .              .              .              .
#>     31:               =3             =3             =3             =3
#>     32:               4C            8cL            8eL            4.g
#>     33:                .            8BJ             8d              .
#>     34:             8BBL             4c             8e              .
#>     35:             8AAJ              .           8f#J             8a
#>     36:              4GG             4d             4g             4b
#>     37:               =4             =4             =4             =4
#>     38:                .              .              .              .
#>     39:                .              .              .              .
#>     40:               =5             =5             =5             =5
#>     41:             4FF#             4A             4d            2dd
#>     42:              4GG             4B             4e              .
#>     43:              4AA             4c            4f#            4cc
#>     44:               =6             =6             =6             =6
#>     45:                .              .              .              .
#>     46:                .              .              .              .
#>     47:                .              .              .              .
#>     48:                .              .              .              .
#>     49:               =7             =7             =7             =7
#>     50:             2GG;            2B;            2d;            2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:               4E            4G#            8eL             4b
#>     55:                .              .           8f#J              .
#>     56:               =5             =5             =5             =5
#>     57:                .              .              .              .
#>     58:                .              .              .              .
#>     59:                .              .              .              .
#>     60:                .              .              .              .
#>     61:                .              .              .              .
#>     62:                .              .              .              .
#>     63:               =6             =6             =6             =6
#>     64:             8G#L             4B             4b           8eeL
#>     65:              8EJ              .              .          8dd#J
#>     66:              4F#            4A#            4f#           4cc#
#>     67:             4BB;           4d#;           4f#;            4b;
#>     68:               4E             4e            4g#             4b
#>     69:               =7             =7             =7             =7
#>     70:                .              .              .              .
#>     71:                .              .              .              .
#>     72:                .              .              .              .
#>     73:                .              .              .              .
#>     74:                .              .              .              .
#>     75:                .              .              .              .
#>     76:                .              .              .              .
#>     77:               =8             =8             =8             =8
#>     78:              4E#             4d            4g#             4b
#>     79:              4F#            4c#            4f#             4a
#>     80:             4C#;           4c#;           4e#;           4g#;
#>     81:               4E            [4B             4e            4g#
#>     82:               =9             =9             =9             =9
#>     83:                .              .              .              .
#>     84:                .              .              .              .
#>     85:                .              .              .              .
#>     86:                .              .              .              .
#>     87:                .              .              .              .
#>     88:                .              .              .              .
#>     89:              =10            =10            =10            =10
#>     90:             4AA#            2F#            4c#            4c#
#>     91:              4BB              .             4B            4d#
#>     92:             4EE;           4G#;            4B;            4e;
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
#>          *Token :: character
#> 
#>    Grouping fields: (eleven groups)
#>           Bar   :: integer
#> 

# unfiltering and complement

humData |> filter(Spine %in% 1:2) |> complement()
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
#>     20:                .              .             4d             4g
#>     21:               =1             =1             =1             =1
#>     22:                .              .             4d             2g
#>     23:                .              .             4e              .
#>     24:                .              .              .              .
#>     25:                .              .             4d            4dd
#>     26:               =2             =2             =2             =2
#>     27:                .              .             2d            4.b
#>     28:                .              .              .              .
#>     29:                .              .              .             8a
#>     30:                .              .             4B             4g
#>     31:               =3             =3             =3             =3
#>     32:                .              .            8eL            4.g
#>     33:                .              .             8d              .
#>     34:                .              .             8e              .
#>     35:                .              .           8f#J             8a
#>     36:                .              .             4g             4b
#>     37:               =4             =4             =4             =4
#>     38:                .              .           2f#;            2a;
#>     39:                .              .             4g             4b
#>     40:               =5             =5             =5             =5
#>     41:                .              .             4d            2dd
#>     42:                .              .             4e              .
#>     43:                .              .            4f#            4cc
#>     44:               =6             =6             =6             =6
#>     45:                .              .             2g             4b
#>     46:                .              .              .             2a
#>     47:                .              .            4f#              .
#>     48:                .              .              .              .
#>     49:               =7             =7             =7             =7
#>     50:                .              .            2d;            2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:                .              .            8eL             4b
#>     55:                .              .           8f#J              .
#>     56:               =5             =5             =5             =5
#>     57:                .              .           8g#L            4ee
#>     58:                .              .           8a#J              .
#>     59:                .              .             4b           4dd#
#>     60:                .              .              .              .
#>     61:                .              .            4a#           4cc#
#>     62:                .              .             4b           4dd#
#>     63:               =6             =6             =6             =6
#>     64:                .              .             4b           8eeL
#>     65:                .              .              .          8dd#J
#>     66:                .              .            4f#           4cc#
#>     67:                .              .           4f#;            4b;
#>     68:                .              .            4g#             4b
#>     69:               =7             =7             =7             =7
#>     70:                .              .            8eL            4ee
#>     71:                .              .           8f#J              .
#>     72:                .              .            4g#             4b
#>     73:                .              .             4a           4cc#
#>     74:                .              .              .              .
#>     75:                .              .            8eL           8g#L
#>     76:                .              .           8f#J            8aJ
#>     77:               =8             =8             =8             =8
#>     78:                .              .            4g#             4b
#>     79:                .              .            4f#             4a
#>     80:                .              .           4e#;           4g#;
#>     81:                .              .             4e            4g#
#>     82:               =9             =9             =9             =9
#>     83:                .              .            4d#            4f#
#>     84:                .              .            4c#             4a
#>     85:                .              .              .              .
#>     86:                .              .             4B            4g#
#>     87:                .              .             4B            4f#
#>     88:                .              .              .              .
#>     89:              =10            =10            =10            =10
#>     90:                .              .            4c#            4c#
#>     91:                .              .             4B            4d#
#>     92:                .              .            4B;            4e;
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
#>          *Token :: character
#> 

humData |> filter(Spine %in% 1:2) |> unfilter()
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
#>     20:              4GG             4B             4d             4g
#>     21:               =1             =1             =1             =1
#>     22:               4G             4B             4d             2g
#>     23:               4E            8cL             4e              .
#>     24:                .            8BJ              .              .
#>     25:              4F#             4A             4d            4dd
#>     26:               =2             =2             =2             =2
#>     27:               4G             4G             2d            4.b
#>     28:               4D            4F#              .              .
#>     29:                .              .              .             8a
#>     30:               4E             4G             4B             4g
#>     31:               =3             =3             =3             =3
#>     32:               4C            8cL            8eL            4.g
#>     33:                .            8BJ             8d              .
#>     34:             8BBL             4c             8e              .
#>     35:             8AAJ              .           8f#J             8a
#>     36:              4GG             4d             4g             4b
#>     37:               =4             =4             =4             =4
#>     38:              2D;            2d;           2f#;            2a;
#>     39:              4GG             4d             4g             4b
#>     40:               =5             =5             =5             =5
#>     41:             4FF#             4A             4d            2dd
#>     42:              4GG             4B             4e              .
#>     43:              4AA             4c            4f#            4cc
#>     44:               =6             =6             =6             =6
#>     45:              4BB             4d             2g             4b
#>     46:               4C             4e              .             2a
#>     47:               4D            8dL            4f#              .
#>     48:                .            8cJ              .              .
#>     49:               =7             =7             =7             =7
#>     50:             2GG;            2B;            2d;            2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:               4E            4G#            8eL             4b
#>     55:                .              .           8f#J              .
#>     56:               =5             =5             =5             =5
#>     57:              4C#            4c#           8g#L            4ee
#>     58:                .              .           8a#J              .
#>     59:             8D#L            4F#             4b           4dd#
#>     60:              8EJ              .              .              .
#>     61:              4F#            4f#            4a#           4cc#
#>     62:               4B            4f#             4b           4dd#
#>     63:               =6             =6             =6             =6
#>     64:             8G#L             4B             4b           8eeL
#>     65:              8EJ              .              .          8dd#J
#>     66:              4F#            4A#            4f#           4cc#
#>     67:             4BB;           4d#;           4f#;            4b;
#>     68:               4E             4e            4g#             4b
#>     69:               =7             =7             =7             =7
#>     70:              4G#             4B            8eL            4ee
#>     71:                .              .           8f#J              .
#>     72:               4E             4e            4g#             4b
#>     73:            8AnXL             4e             4a           4cc#
#>     74:              8BJ              .              .              .
#>     75:              4c#            4c#            8eL           8g#L
#>     76:                .              .           8f#J            8aJ
#>     77:               =8             =8             =8             =8
#>     78:              4E#             4d            4g#             4b
#>     79:              4F#            4c#            4f#             4a
#>     80:             4C#;           4c#;           4e#;           4g#;
#>     81:               4E            [4B             4e            4g#
#>     82:               =9             =9             =9             =9
#>     83:              4BB            4B]            4d#            4f#
#>     84:             8C#L             4e            4c#             4a
#>     85:             8D#J              .              .              .
#>     86:               4E             4e             4B            4g#
#>     87:              4BB           8d#L             4B            4f#
#>     88:                .            8BJ              .              .
#>     89:              =10            =10            =10            =10
#>     90:             4AA#            2F#            4c#            4c#
#>     91:              4BB              .             4B            4d#
#>     92:             4EE;           4G#;            4B;            4e;
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
#>          *Token :: character
#> 

humData |> filter(Spine %in% 1:2) |> solfa() |> unfilter(complement = 'Token')
#> ######################## vvv chor001.krn vvv #########################
#>      1:  !!!COM: Bach, Johann Sebastian
#>      2:  !!!CDT: 1685/02/21/-1750/07/28/
#>      3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>      4:  !!!OTL@EN:      From the Depths of My Heart
#>      5:  !!!SCT: BWV 269
#>      6:  !!!PC#: 1
#>      7:  !!!AGN: chorale
#>      8:          **solfa        **solfa        **solfa        **solfa
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
#>     20:             vvdo            vmi             4d             4g
#>     21:               =1             =1             =1             =1
#>     22:              vdo            vmi             4d             2g
#>     23:              vla             fa             4e              .
#>     24:                .            vmi              .              .
#>     25:              vti            vre             4d            4dd
#>     26:               =2             =2             =2             =2
#>     27:              vdo            vdo             2d            4.b
#>     28:              vso            vti              .              .
#>     29:                .              .              .             8a
#>     30:              vla            vdo             4B             4g
#>     31:               =3             =3             =3             =3
#>     32:              vfa             fa            8eL            4.g
#>     33:                .            vmi             8d              .
#>     34:             vvmi             fa             8e              .
#>     35:             vvre              .           8f#J             8a
#>     36:             vvdo             so             4g             4b
#>     37:               =4             =4             =4             =4
#>     38:              vso             so           2f#;            2a;
#>     39:             vvdo             so             4g             4b
#>     40:               =5             =5             =5             =5
#>     41:             vvti            vre             4d            2dd
#>     42:             vvdo            vmi             4e              .
#>     43:             vvre             fa            4f#            4cc
#>     44:               =6             =6             =6             =6
#>     45:             vvmi             so             2g             4b
#>     46:              vfa             la              .             2a
#>     47:              vso             so            4f#              .
#>     48:                .             fa              .              .
#>     49:               =7             =7             =7             =7
#>     50:             vvdo            vmi            2d;            2g;
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>     54:              vdo            vmi            8eL             4b
#>     55:                .              .           8f#J              .
#>     56:               =5             =5             =5             =5
#>     57:              vla             la           8g#L            4ee
#>     58:                .              .           8a#J              .
#>     59:              vti            vre             4b           4dd#
#>     60:              vdo              .              .              .
#>     61:              vre             re            4a#           4cc#
#>     62:              vso             re             4b           4dd#
#>     63:               =6             =6             =6             =6
#>     64:              vmi            vso             4b           8eeL
#>     65:              vdo              .              .          8dd#J
#>     66:              vre            vfi            4f#           4cc#
#>     67:             vvso             ti           4f#;            4b;
#>     68:              vdo             do            4g#             4b
#>     69:               =7             =7             =7             =7
#>     70:              vmi            vso            8eL            4ee
#>     71:                .              .           8f#J              .
#>     72:              vdo             do            4g#             4b
#>     73:              vfa             do             4a           4cc#
#>     74:              vso              .              .              .
#>     75:               la             la            8eL           8g#L
#>     76:                .              .           8f#J            8aJ
#>     77:               =8             =8             =8             =8
#>     78:              vdi             te            4g#             4b
#>     79:              vre             la            4f#             4a
#>     80:              vla             la           4e#;           4g#;
#>     81:              vdo            vso             4e            4g#
#>     82:               =9             =9             =9             =9
#>     83:             vvso            vso            4d#            4f#
#>     84:              vla             do            4c#             4a
#>     85:              vti              .              .              .
#>     86:              vdo             do             4B            4g#
#>     87:             vvso             ti             4B            4f#
#>     88:                .            vso              .              .
#>     89:              =10            =10            =10            =10
#>     90:             vvfi            vre            4c#            4c#
#>     91:             vvso              .             4B            4d#
#>     92:             vvdo            vmi            4B;            4e;
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
#>          *Solfa :: character (**solfa tokens)
#>           Token :: character
#> 
```
