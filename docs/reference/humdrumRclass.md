# `humdrumR` class

This `S4` class is the basic unit of the
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
package. Each `humdrumR object` represents data
[read](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
from one or more humdrum files. In the documentation, we refer to these
objects interchangeably as "`humdrumR` corpora", "`humdrumR` objects,"
or `humdrumR` data(sets). In coding examples we name them "`humData`."
Test is an object/variable is a `humdrumR` dataset using
`is.humdrumR()`.

## Usage

``` r
is.humdrumR(x)

# S3 method for class 'humdrumR'
print(
  humdrumR,
  view = humdrumRoption("view"),
  dataTypes = humdrumRoption("dataTypes"),
  firstAndLast = TRUE,
  screenWidth = options("width")$width - 10L,
  null = humdrumRoption("nullPrint"),
  syntaxHighlight = humdrumRoption("syntaxHighlight"),
  maxRecordsPerFile = if (length(humdrumR) == 1L) 800L else
    humdrumRoption("maxRecordsPerFile"),
  maxTokenLength = humdrumRoption("maxTokenLength"),
  censorEmptyRecords = humdrumRoption("censorEmptyRecords"),
  ...
)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a humdrumR data object.

- view:

  ***How should humdrumR data be printed?***

  There are three options: `"humdrum"`, `"score"`, and `"table"`
  (aliases `"data.frame"` and `"tibble"`). These options are [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md).

  Use
  [`select()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  to determine which fields to show.

- dataTypes:

  ***Which types of humdrum record(s) to view.***

  Defaults to `"GLIMDd"` for
  [`as.lines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  and
  [`as.matrix()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md);
  `"Dd"` for
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html);
  `"LIMDd"` for
  [`as.matrices()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  and
  [`as.data.frames()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md).

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation for explanation.)

- syntaxHighlight:

  ***Should syntax highlighting (coloring) be used in printout?***

  Defaults to `TRUE`.

  Must be a singleton logical value; an on/off switch.

- maxRecordsPerFile:

  ***How many records should be shown in each file, when more than one
  file is present?***

  Defaults to `40`.

  Can be any positive whole number.

- maxTokenLength:

  ***Length at which longer tokens are censored with ...***

  Defaults to `16`.

  Can be any positive whole number.

- censorEmptyRecords:

  ***Should consecutive records be "censored" (compressed) in
  printout?***

  Defaults to `30`.

  Can be any positive whole number, up to `Inf`. If `Inf`, no censoring
  will occur.

## Details

The most important part of a `humdrumR` object is the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md) it
holds within it; In essence, a `humdrumR` object is simply a wrapper
around its humdrum table, which helps users to to visualize,
[filter](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
[summarize](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
and
[manipulate](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
the table in a variety of ways.

Basic information about the size and shape of `humdrumR` objects can be
obtained with calls to [nrecord, npiece, length, ncol,
etc.](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md). More
detailed summary information can be obtained with the humdrumR [corpus
summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).
`humdrumR` data can also be coerced to more basic R data types using
[as.matrix, as.data.frame,
etc.](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md).
A number of helpful functions are also defined to "reshape" or
reorganize the data (e.g.,
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md),
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md),
[`collapseHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)).

The most powerful features of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
are the tools it gives you to...

- Print a readable view of the data in shorthand/curtailed humdrum
  syntax.

- Filter `humdrumR` data, using
  [`subset.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  and the standard `R` [indexing
  operators](https://rdrr.io/r/base/Extract.html): `[]` and `[[]]`.

- Apply arbitrary commands to
  [humtable](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  fields using the
  [with(in)Humdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  routines, and related tidyverse commands (`mutate()`, `summarize()`,
  etc.).

## Slots

- `Humtable`:

  A [humdrum
  tables](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)—i.e,
  a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with particular fields.

- `Files`:

  A list of two elements. The first, "`Search`", contains a single
  character representing the `pattern` used in the call to
  [`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  which created this humdrumR object. The second, "`Names`," is a vector
  of strings representing all the files which matched the `pattern` and
  were read into the `humdrumR` object, with
  [`names()`](https://rdrr.io/r/base/names.html) corresponding to their
  "subcorpora" labels (`Label`).

- `Fields`:

  A
  [data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  indicating the existing fields in the `humdrumR` object's [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
  The fields are divided into five categories: "Data", "Structure",
  "Interpretation", "Formal", and "Reference."

- `LoadTime`:

  A [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) value,
  indicating the time at which
  [`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  was called to create this `humdrumR` object.

- `Context`:

  A
  [data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with two columns: `Open` and `Close`. Each row represents a contextual
  window to apply to the data.

## Viewing your Data

If you type the name of an object on the R command line, R will "print"
the object in the console. This can be also be done explicitly using the
`humdrumR` method of the [`print()`](https://rdrr.io/r/base/print.html)
function. The humdrumR print method contains a number of arguments,
which can be manipulated directly in calls to
[`print()`](https://rdrr.io/r/base/print.html). However, the `humdrumR`
print argument draws its defaults from global `humdrumR` options which
can be also controlled with the
[`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
function. Generaly, changing print options with
[`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
is the best option, as once you change them, all automatic printing will
follow your new settings—this means you can avoid explicitly calling
[`print()`](https://rdrr.io/r/base/print.html).

When printing, only the [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
in the data are shown.

### View types

There are three options for how to view `humdrumR` data, which can be
toggled between using the `view` argument to
[`print()`](https://rdrr.io/r/base/print.html) or
[`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md).
Since `view` is the first argument to the
[`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
function, you can switch between views by simply calling
`humdrumR('humdrum')` or `humdrumR('score')` or `humdrumR('table')`. The
options are:

- `"humdrum"` (the default): prints a humdrum-syntax score
  representation, with record numbers enumerated at the left side.

  When the `Token` field is selected, and you haven't applied any
  [filters](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
  this view will show your original data as it was in the files you
  [read](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).

  If more than one field is selected, they are pasted together in the
  printed output.

- `"table"`: prints a view of the underlying [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

  In addition to the [selected
  fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md),
  the `Piece`, `Spine`, and `Record` fields will always print in the
  output table, as well as `Path` and `Stop` if any paths/stops are
  present.

- `"score"`: will (attempt to) show a rendered score of (the first
  piece) in your data.

  This view is only likely to work correctly if you are using Rstudio
  and connected to the internet. Score rendering is accomplished using
  Craig Sapp's [Humdrum Notation Plugin](https://plugin.humdrum.org/).

For `table` and `humdrum` views, if there are more than one pieces in
the object, the beginning of the first piece is printed, followed by the
end of the last piece; How *much* of the first/last piece are printed is
controlled by the `maxRecordsPerFile` print argument. Both of these
views also highlight the output with different colors representing
different types of data tokens: this can be disabled using
`syntaxHighlight = FALSE`. For `score` view, only the first piece is
shown.

## See also

The actual data is stored in the internal [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
You can set printing options globally using the
[`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
function.

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

humData 
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

humData |> print(view = 'table')
#>   Piece          Spine          Record               Token
#> #################### vvv chor001.krn vvv #####################
#>       1           <NA>               1!!!COM: Bach, J...nn
#>       1           <NA>               2!!!CDT: 1685/02.../-
#>       1           <NA>               3!!!OTL@@DE: Aus...in
#>       1           <NA>               4!!!OTL@EN:     ...om
#>       1           <NA>               5     !!!SCT: BWV 269
#>       1           <NA>               6           !!!PC#: 1
#>       1           <NA>               7     !!!AGN: chorale
#>       1           <NA>             125   !!!hum2abc: -Q ''
#>       1           <NA>             126!!!title: @{PC#...@{
#>       1           <NA>             127!!!YOR1: 371 vi...ti
#>       1           <NA>             128!!!YOR2: 4th ed...y 
#>       1           <NA>             129!!!YOR3: c.1875...17
#>       1           <NA>             130!!!YOR4: Choral...(N
#>       1           <NA>             131!!!SMS: B&H, 4t...d,
#>       1           <NA>             132!!!EED:  Craig ...ar
#>       1           <NA>             133 !!!EEV:  2009/05/22
#>       1              1               8              **kern
#>       1              1               9              *ICvox
#>       1              1              10              *Ibass
#>       1              1              11             *I"Bass
#>       1              1              12           *>[A,A,B]
#>       1              1              13        *>norep[A,B]
#>       1              1              14                 *>A
#>       1              1              15             *clefF4
#>       1              1              16              *k[f#]
#>       1              1              17                 *G:
#>       1              1              18               *M3/4
#>       1              1              19              *MM100
#>       1              1              20                 4GG
#>       1              1              21                  =1
#>       1              1              22                  4G
#>       1              1              23                  4E
#>       1              1              24                   .
#>       1              1              25                 4F#
#>       1              1              26                  =2
#>       1              1              27                  4G
#>       1              1              28                  4D
#>       1              1              29                   .
#>       1              1              30                  4E
#>       1              1              31                  =3
#>       1              1              32                  4C
#>       1              1              33                   .
#>       1              1              34                8BBL
#>       1              1              35                8AAJ
#>       1              1              36                 4GG
#>       1              1              37                  =4
#>       1              1              38                 2D;
#>       1              1              39                 4GG
#>       1              1              40                  =5
#>       1              1              41                4FF#
#> #################### ^^^ chor001.krn ^^^ #####################
#> 
#>      (two more pieces...)
#> 
#> #################### vvv chor004.krn vvv #####################
#>       4              4              45                  =4
#>       4              4              46                  4b
#>       4              4              47                   .
#>       4              4              48                   .
#>       4              4              49                4cc#
#>       4              4              50                   .
#>       4              4              51                 4b;
#>       4              4              52                =:|!
#>       4              4              53                 *>B
#>       4              4              54                  4b
#>       4              4              55                   .
#>       4              4              56                  =5
#>       4              4              57                 4ee
#>       4              4              58                   .
#>       4              4              59                4dd#
#>       4              4              60                   .
#>       4              4              61                4cc#
#>       4              4              62                4dd#
#>       4              4              63                  =6
#>       4              4              64                8eeL
#>       4              4              65               8dd#J
#>       4              4              66                4cc#
#>       4              4              67                 4b;
#>       4              4              68                  4b
#>       4              4              69                  =7
#>       4              4              70                 4ee
#>       4              4              71                   .
#>       4              4              72                  4b
#>       4              4              73                4cc#
#>       4              4              74                   .
#>       4              4              75                8g#L
#>       4              4              76                 8aJ
#>       4              4              77                  =8
#>       4              4              78                  4b
#>       4              4              79                  4a
#>       4              4              80                4g#;
#>       4              4              81                 4g#
#>       4              4              82                  =9
#>       4              4              83                 4f#
#>       4              4              84                  4a
#>       4              4              85                   .
#>       4              4              86                 4g#
#>       4              4              87                 4f#
#>       4              4              88                   .
#>       4              4              89                 =10
#>       4              4              90                 4c#
#>       4              4              91                 4d#
#>       4              4              92                 4e;
#>       4              4              93                  ==
#>       4              4              94                  *-
#> #################### ^^^ chor004.krn ^^^ #####################
#>   Piece          Spine          Record               Token
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 
```
