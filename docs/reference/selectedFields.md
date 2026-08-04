# The "selected" fields of a [humdrumR object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)

Every `humdrumR` object will have, at any given time, one or more of its
fields "selected." The selected fields are the fields that are shown
when a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
prints on the console. (At the bottom of the printout, the selected
fields are also marked by a `*`.) The selected fields can also be
queried directly using the `selectedFields()` function, or by inspecting
the output of
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
The selected fields also play other important roles in `humdrumR` (see
details).

## Usage

``` r
selectedFields(humdrumR)

# S3 method for class 'humdrumR'
select(.data, ..., fieldTypes = "any")
```

## Arguments

- humdrumR, .data:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Which fields to output.***

  If no arguments are provided, the `Token` field is selected.

  These arguments can be any combination of `character` strings,
  numbers, or symbols used to match fields in the `humdrumR` input using
  [tidyverse](https://dplyr.tidyverse.org/reference/select.html)
  semantics.

  Unlike in tidyverse `select()`, field names can be [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md).
  You can also include `character` strings [partially
  matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`
  or `"Grouping"`, which will select all fields of those types (see
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  for further explanation).

- fieldTypes:

  ***Which field types are available for numeric selecting?***

  Defaults to `"any"`, so all fields are counted for numeric selection.

  Must be a `character` vector. Legal options are `"Data"`,
  `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`,
  `"Grouping"`, and `"any"`, corresponding to the `Type` column in the
  output of
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
  Types can be [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)—for
  example, `"S"` for `"Structure"`.

## Details

The "selected" fields play an important role in
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md).
In addition to controlling what
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
you "see" in the console printout, the select fields are the fields that
many
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
functions will automatically apply themselves to. For example, if you
call
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`timecount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md),
or [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
on a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
these functions will be applied the selected field(s). (However, most
such functions are only applied to the *first* selected field, if there
is more than one; see their own manuals for details.) The first selected
field is also passed as the hidden `.` variable in calls to
[with()/within()/,
mutate()/summarize()/reframe()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)—so
if you don't remember what fields are selected you can just put a `.`!

The selected fields also have a role in identifying "null" data.
Whenever new fields are selected, their data tokens are checked for `NA`
values or null tokens (`"."`). Wherever *all* the selected fields are
null, the `Type` field is updated to `"d"`; wherever *any* field is
**not** null, the `Type` field is updated to `"D"`. Many functions
ignore `d` (null data) tokens by default, so selecting fields can be a
way to control which data you want to analyze and which you don't.

### Selecting fields

Fields can be selected using the tidyverse `select()` function, and can
use any of `select()`'s [special select
features](https://dplyr.tidyverse.org/reference/select.html). If you
call `select()` with no argument, the original `Token` field is selected
by default.

If you use `select()` with a numeric selection, like `select(1:3)`,
fields are numbered in the (row) order shown in the call to
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
Fields are always sorted first by `Type` (`Data` first), then by name.
If you provide a `fieldTypes` argument, the numeric selection is reduced
to only those fields you choose, matching with the row-numbers you'd see
if you call [fields(humData, fieldTypes =
...)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md). So,
for example, `select(humData, 1:3, fieldTypes = 'Structure')` will
select the first three structural fields. You can also simply provide
the keywords `"Data"`, `"Structure"`, `"Interpretation"`, `"Reference"`,
or `"Formal"` to select *all* fields of each [field
type](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

Note that when you call `select()` on [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
the selected field(s) change **in place**, meaning that the selection
changes *even if you don't (re)assign the output*!

## See also

Use
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
to see what fields are available, and how they are ordered. To actually
*extract* fields, see
[`pull()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md).

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpGCgWwF/temp_libpath2869ab26c00287/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

# see what is selected
selectedFields(humData)
#> [1] "Token"

# change selection
humData |> select(Spine, Record, Token) |> selectedFields()
#> [1] "Spine"  "Record" "Token" 

humData |> select(fieldTypes = 'Structure') |> selectedFields()
#> [1] "Token"

humData |> select(4)
#> ######################## vvv chor001.krn vvv #########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:                0              0              0              0
#>             9:                0              0              0              0
#>            10:                0              0              0              0
#>            11:                0              0              0              0
#>            12:                0              0              0              0
#>            13:                0              0              0              0
#>            14:                0              0              0              0
#>            15:                0              0              0              0
#>            16:                0              0              0              0
#>            17:                0              0              0              0
#>            18:                0              0              0              0
#>            19:                0              0              0              0
#>            20:                0              0              0              0
#>            21:                0              0              0              0
#>            22:                0              0              0              0
#>            23:                0              0              0              0
#>            24:                0              0              0              0
#>            25:                0              0              0              0
#>            26:                0              0              0              0
#>            27:                0              0              0              0
#>            28:                0              0              0              0
#>            29:                0              0              0              0
#>            30:                0              0              0              0
#>            31:                0              0              0              0
#>            32:                0              0              0              0
#>            33:                0              0              0              0
#>            34:                0              0              0              0
#>            35:                0              0              0              0
#>            36:                0              0              0              0
#>            37:                0              0              0              0
#>            38:                0              0              0              0
#>            39:                0              0              0              0
#>            40:                0              0              0              0
#>            41:                0              0              0              0
#>            42:                0              0              0              0
#>            43:                0              0              0              0
#>            44:                0              0              0              0
#>            45:                0              0              0              0
#>            46:                0              0              0              0
#>            47:                0              0              0              0
#>            48:                0              0              0              0
#>            49:                0              0              0              0
#>            50:                0              0              0              0
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>      (two more pieces...)
#> 
#> ######################## vvv chor004.krn vvv #########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:                0              0              0              0
#>            55:                0              0              0              0
#>            56:                0              0              0              0
#>            57:                0              0              0              0
#>            58:                0              0              0              0
#>            59:                0              0              0              0
#>            60:                0              0              0              0
#>            61:                0              0              0              0
#>            62:                0              0              0              0
#>            63:                0              0              0              0
#>            64:                0              0              0              0
#>            65:                0              0              0              0
#>            66:                0              0              0              0
#>            67:                0              0              0              0
#>            68:                0              0              0              0
#>            69:                0              0              0              0
#>            70:                0              0              0              0
#>            71:                0              0              0              0
#>            72:                0              0              0              0
#>            73:                0              0              0              0
#>            74:                0              0              0              0
#>            75:                0              0              0              0
#>            76:                0              0              0              0
#>            77:                0              0              0              0
#>            78:                0              0              0              0
#>            79:                0              0              0              0
#>            80:                0              0              0              0
#>            81:                0              0              0              0
#>            82:                0              0              0              0
#>            83:                0              0              0              0
#>            84:                0              0              0              0
#>            85:                0              0              0              0
#>            86:                0              0              0              0
#>            87:                0              0              0              0
#>            88:                0              0              0              0
#>            89:                0              0              0              0
#>            90:                0              0              0              0
#>            91:                0              0              0              0
#>            92:                0              0              0              0
#>            93:                1              1              1              1
#>            94:                1              1              1              1
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
#>           Token     :: character
#> 
#>    Formal fields: 
#>          *DoubleBar :: integer
#> 
humData |> select(1:3, fieldTypes = 'Structure')
#> ################### vvv chor001.krn vvv ####################
#>             1:      1chor001.krn                                    ***
#>             2:      1chor001.krn                                    ***
#>             3:      1chor001.krn                                    ***
#>             4:      1chor001.krn                                    ***
#>             5:      1chor001.krn                                    ***
#>             6:      1chor001.krn                                    ***
#>             7:      1chor001.krn                                    ***
#>             8:      1chor001.krn    1chor001.krn    1chor001.krn    ***
#>             9:            *ICvox          *ICvox          *ICvox    ***
#>            10:            *Ibass         *Itenor          *Ialto    ***
#>            11:           *I"Bass        *I"Tenor         *I"Alto    ***
#>            12:         *>[A,A,B]       *>[A,A,B]       *>[A,A,B]    ***
#>            13:      *>norep[A,B]    *>norep[A,B]    *>norep[A,B]    ***
#>            14:               *>A             *>A             *>A    ***
#>            15:           *clefF4        *clefGv2         *clefG2    ***
#>            16:            *k[f#]          *k[f#]          *k[f#]    ***
#>            17:               *G:             *G:             *G:    ***
#>            18:             *M3/4           *M3/4           *M3/4    ***
#>            19:            *MM100          *MM100          *MM100    ***
#>            20:     11chor001.krn   11chor001.krn   11chor001.krn    ***
#>            21:                =1              =1              =1    ***
#>            22:     21chor001.krn   21chor001.krn   21chor001.krn    ***
#>            23:     31chor001.krn   31chor001.krn   31chor001.krn    ***
#>            24:     41chor001.krn   41chor001.krn   41chor001.krn    ***
#>            25:     51chor001.krn   51chor001.krn   51chor001.krn    ***
#>            26:                =2              =2              =2    ***
#>            27:     61chor001.krn   61chor001.krn   61chor001.krn    ***
#>            28:     71chor001.krn   71chor001.krn   71chor001.krn    ***
#>            29:     81chor001.krn   81chor001.krn   81chor001.krn    ***
#>            30:     91chor001.krn   91chor001.krn   91chor001.krn    ***
#>            31:                =3              =3              =3    ***
#>            32:    101chor001.krn  101chor001.krn  101chor001.krn    ***
#>            33:    111chor001.krn  111chor001.krn  111chor001.krn    ***
#>            34:    121chor001.krn  121chor001.krn  121chor001.krn    ***
#>            35:    131chor001.krn  131chor001.krn  131chor001.krn    ***
#>            36:    141chor001.krn  141chor001.krn  141chor001.krn    ***
#>            37:                =4              =4              =4    ***
#>            38:    151chor001.krn  151chor001.krn  151chor001.krn    ***
#>            39:    161chor001.krn  161chor001.krn  161chor001.krn    ***
#>            40:                =5              =5              =5    ***
#>            41:    171chor001.krn  171chor001.krn  171chor001.krn    ***
#>            42:    181chor001.krn  181chor001.krn  181chor001.krn    ***
#>            43:    191chor001.krn  191chor001.krn  191chor001.krn    ***
#>            44:                =6              =6              =6    ***
#>            45:    201chor001.krn  201chor001.krn  201chor001.krn    ***
#>            46:    211chor001.krn  211chor001.krn  211chor001.krn    ***
#>            47:    221chor001.krn  221chor001.krn  221chor001.krn    ***
#>            48:    231chor001.krn  231chor001.krn  231chor001.krn    ***
#>            49:                =7              =7              =7    ***
#>            50:    241chor001.krn  241chor001.krn  241chor001.krn    ***
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ################### ^^^ chor001.krn ^^^ ####################
#> 
#>      (two more pieces...)
#> 
#> ################### vvv chor004.krn vvv ####################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:    292chor004.krn  292chor004.krn  292chor004.krn    ***
#>            55:    302chor004.krn  302chor004.krn  302chor004.krn    ***
#>            56:                =5              =5              =5    ***
#>            57:    312chor004.krn  312chor004.krn  312chor004.krn    ***
#>            58:    322chor004.krn  322chor004.krn  322chor004.krn    ***
#>            59:    332chor004.krn  332chor004.krn  332chor004.krn    ***
#>            60:    342chor004.krn  342chor004.krn  342chor004.krn    ***
#>            61:    352chor004.krn  352chor004.krn  352chor004.krn    ***
#>            62:    362chor004.krn  362chor004.krn  362chor004.krn    ***
#>            63:                =6              =6              =6    ***
#>            64:    372chor004.krn  372chor004.krn  372chor004.krn    ***
#>            65:    382chor004.krn  382chor004.krn  382chor004.krn    ***
#>            66:    392chor004.krn  392chor004.krn  392chor004.krn    ***
#>            67:    402chor004.krn  402chor004.krn  402chor004.krn    ***
#>            68:    412chor004.krn  412chor004.krn  412chor004.krn    ***
#>            69:                =7              =7              =7    ***
#>            70:    422chor004.krn  422chor004.krn  422chor004.krn    ***
#>            71:    432chor004.krn  432chor004.krn  432chor004.krn    ***
#>            72:    442chor004.krn  442chor004.krn  442chor004.krn    ***
#>            73:    452chor004.krn  452chor004.krn  452chor004.krn    ***
#>            74:    462chor004.krn  462chor004.krn  462chor004.krn    ***
#>            75:    472chor004.krn  472chor004.krn  472chor004.krn    ***
#>            76:    482chor004.krn  482chor004.krn  482chor004.krn    ***
#>            77:                =8              =8              =8    ***
#>            78:    492chor004.krn  492chor004.krn  492chor004.krn    ***
#>            79:    502chor004.krn  502chor004.krn  502chor004.krn    ***
#>            80:    512chor004.krn  512chor004.krn  512chor004.krn    ***
#>            81:    522chor004.krn  522chor004.krn  522chor004.krn    ***
#>            82:                =9              =9              =9    ***
#>            83:    532chor004.krn  532chor004.krn  532chor004.krn    ***
#>            84:    542chor004.krn  542chor004.krn  542chor004.krn    ***
#>            85:    552chor004.krn  552chor004.krn  552chor004.krn    ***
#>            86:    562chor004.krn  562chor004.krn  562chor004.krn    ***
#>            87:    572chor004.krn  572chor004.krn  572chor004.krn    ***
#>            88:    582chor004.krn  582chor004.krn  582chor004.krn    ***
#>            89:               =10             =10             =10    ***
#>            90:    592chor004.krn  592chor004.krn  592chor004.krn    ***
#>            91:    602chor004.krn  602chor004.krn  602chor004.krn    ***
#>            92:    612chor004.krn  612chor004.krn  612chor004.krn    ***
#>            93:                ==              ==              ==    ***
#>            94:                *-              *-              *-    ***
#>            95:      2chor004.krn                                    ***
#>            96:      2chor004.krn                                    ***
#>            97:      2chor004.krn                                    ***
#>            98:      2chor004.krn                                    ***
#>            99:      2chor004.krn                                    ***
#>           100:      2chor004.krn                                    ***
#>           101:      2chor004.krn                                    ***
#>           102:      2chor004.krn                                    ***
#>           103:      2chor004.krn                                    ***
#> ################### ^^^ chor004.krn ^^^ ####################
#>       (***one spine/path not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>           Token      :: character
#> 
#>    Structure fields: 
#>          *DataRecord :: integer
#>          *File       :: integer
#>          *Filename   :: character
#> 

# effect of selection

humData |> select(Token) |> count()
#> humdrumR count distribution 
#>  Token   n
#> 16BBJJ   1
#>  16C#L   1
#> 16c#LL   1
#>  16ccL   1
#> 16d#JJ   1
#>  16dJJ   2
#> 16ddJJ   1
#>   16eL   2
#>  2.AA;   1
#>   2.B;   1
#>  2.BB;   1
#>  2.GG;   1
#>   2.a;   1
#>    2.b   1
#>   2.b;   1
#>  2.c#;   1
#>    2.d   1
#>   2.d;   2
#>   2.e;   1
#>  2.f#;   1
#>   2.g;   1
#>    2B;   1
#>    2C;   1
#>    2D;   3
#>     2E   1
#>    2F#   1
#>   2GG;   1
#>     2a   5
#>    2a;   3
#>     2b   2
#>    2c#   1
#>    2c;   1
#>     2d   4
#>    2d;   4
#>    2dd   4
#>     2e   2
#>    2e;   1
#>   2f#;   3
#>     2g   5
#>    2g;   2
#>    4.B   3
#>   4.BB   2
#>    4.a   1
#>    4.b   1
#>   4.dd   1
#>    4.e   1
#>   4.ee   1
#>    4.g   3
#>     4A  15
#>    4A#   1
#>    4AA   3
#>   4AA#   1
#>   4AA;   3
#>     4B  29
#>    4B-   1
#>    4B;   3
#>    4BB  14
#>   4BB;   3
#>    4B]   1
#>     4C   5
#>    4C#   8
#>   4C#;   1
#>     4D   9
#>    4D#   4
#>     4E  20
#>    4E#   1
#>    4E;   5
#>   4EE;   2
#>    4E]   1
#>     4F   2
#>    4F#  15
#>   4F#;   2
#>   4F#X   1
#>   4FF#   2
#>     4G   5
#>    4G#   8
#>   4G#;   3
#>  4G#X;   1
#>    4GG  11
#>    4G]   1
#>     4a  32
#>    4a#   4
#>    4a;   3
#>     4b  39
#>    4b;   7
#>     4c  10
#>    4c#  13
#>   4c#;   2
#>    4c;   2
#>    4cc  13
#>   4cc#  13
#>     4d  22
#>    4d#   6
#>   4d#;   3
#>    4dd  10
#>   4dd#   2
#>     4e  43
#>   4e#;   1
#>    4e;  10
#>    4e]   1
#>    4ee   7
#>     4f   4
#>    4f#  22
#>   4f#;   1
#>     4g  15
#>    4g#  19
#>   4g#;   4
#>     8A   3
#>    8A#   2
#>    8AA   2
#>   8AAJ   3
#>   8AAL   2
#>    8AJ   6
#>    8AL   6
#>   8AL]   1
#>  8AnXL   1
#>    8BB   2
#>   8BBJ   1
#>   8BBL   1
#>    8BJ   7
#>    8BL   7
#>     8C   2
#>    8C#   1
#>   8C#J   1
#>   8C#L   3
#>    8CL   3
#>     8D   1
#>   8D#J   1
#>   8D#L   2
#>    8DJ  10
#>    8DL   2
#>    8EJ   6
#>    8EL   7
#>   8EL]   1
#>    8F#   1
#>   8F#J   4
#>   8F#L   4
#>    8FL   1
#>    8G#   2
#>   8G#J   6
#>   8G#L   3
#>   8GGJ   1
#>   8GGL   1
#>    8GJ   2
#>    8GL   3
#>     8a   4
#>   8a#J   1
#>    8aJ   5
#>    8aL   5
#>     8b   1
#>    8bJ   5
#>    8bL   3
#>   8c#J   1
#>   8c#L   5
#>    8cJ   6
#>    8cL   4
#>    8cc   1
#>  8cc#J   1
#>  8cc#L   2
#>   8ccL   1
#>     8d   4
#>   8d#J   4
#>   8d#L   1
#>    8dJ   7
#>    8dL   7
#>    8dd   1
#>  8dd#J   1
#>   8ddJ   1
#>   8ddL   1
#>   8dnJ   1
#>     8e   3
#>    8eJ   5
#>    8eL  17
#>   8eeL   2
#>    8f#   2
#>   8f#J  16
#>   8f#L   9
#>  8f#L]   1
#>  8f#XL   1
#>     8g   1
#>    8g#   1
#>   8g#J   5
#>   8g#L   5
#>  8g#XJ   1
#>    8gJ   4
#>    8gL   2
#>   8gL]   2
#>    [2e   1
#>    [4A   1
#>    [4B   1
#>    [4E   2
#>    [4G   1
#>   [4f#   1
#>    [4g   2
#>  Token   n
#> humdrumR count distribution 
humData |> select(Spine) |> count()
#> humdrumR count distribution 
#> Spine    n
#>     1  284
#>     2  284
#>     3  284
#>     4  284
#> Spine    n
#> humdrumR count distribution 
```
