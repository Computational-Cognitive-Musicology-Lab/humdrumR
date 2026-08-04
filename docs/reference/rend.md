# Separate data fields into new spines.

Rend, as in "to rend apart," splits data in separate fields into
separate spines or paths. Under the hood, `rend()` essentially runs a
specialized call to make the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
"longer"/"taller," similar to R functions like
[`data.table::melt()`](https://rdrr.io/pkg/data.table/man/melt.data.table.html)
(`reshape2`),
[`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html)
(`tidyr`), or
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
(`tidyr`). In fact, a humdrumR method for
[pivot_longer()](https://tidyr.tidyverse.org/reference/tidyr-package.html)
is defined, which is equivalent to `rend()`. The `rend()` function is
essentially the inverse of
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md).

## Usage

``` r
rend(humdrumR, ..., fieldName = NULL, removeRended = TRUE, rendEmpty = FALSE)

# S3 method for class 'humdrumR'
pivot_longer(data, cols, ...)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Which fields to rend?***

  These arguments can be any combination of `character` strings,
  numbers, or symbols used to match fields in the `humdrumR` input using
  [tidyverse](https://dplyr.tidyverse.org/reference/select.html)
  semantics. See the
  [select()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  docs for details.

- fieldName:

  ***A name for the newly rended field.***

  Defaults to pasting the names of selected fields (`...`) together,
  separated by `.`.

  Must be either `NULL`, or a single non-empty `character` string.

- removeRended:

  ***Should rended fields be removed from the output?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- rendEmpty:

  ***Empty spines be rended?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

## Details

The `rend()` function takes any number of `...` arguments to
[select](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
fields in the `humdrumR` data. The identified fields are then split into
new spines. If no fields are provided, the data's [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
are rended. New spines are generated from existing spines; if we start
with spines 1, 2, 3, and rend *two* fields...

- the original spine 1 will be rended into new spines 1 and 2;

- the original spine 2 will be rended into new spines 3 and 4;

- the original spine 3 will be rended into new spines 5 and 6.

However, by default, spines are only rended if they contain non-null
data points in the target fields. If, for example, the original spine 2
had no non-null data in one of the rended fields, it would not be rended
into two spines. However, if `rendEmpty` is set to `TRUE`, *all* spines
will be rended even if empty (all null data).

Note that, since different fields may be different data types, `rend()`
will generally coerce the result to `character`.

### Fields

When you rend fields, a new field is generated. The name of the new
field is specified by `newField`—by default, `newField` is `NULL` and
the names of the rended fields are simply pasted together. If
`removeRended = TRUE` (the default), the original fields are removed
from the data. However, certain fields, like `Token` and any [structural
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
cannot be removed from the data. Therefore, if you rend these fields,
they will not be deleted, even if `removeRended = TRUE`.

If you only provide one field name to rend, `Token` is assumed as the
first field automatically. Thus, `rend(humData, 'Solfa')` is equivalent
to `rend(humData, 'Token', 'Solfa')`.

## See also

The complement/opposite of `rend()` is
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md).

Other Humdrum table reshaping functions:
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md),
[`collapseHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md),
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)

Other Humdrum table pivoting functions:
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

humData |> 
   mutate(Recip = recip(Token), 
          Solfa = solfa(Token, simple = TRUE)) -> humData

humData |> rend(c('Recip', 'Solfa'))
#> ####################### vvv chor001.krn vvv ########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:         **recip       **recip       **recip       **recip    ***
#>             9:          *ICvox        *ICvox        *ICvox        *ICvox    ***
#>            10:          *Ibass        *Ibass       *Itenor       *Itenor    ***
#>            11:         *I"Bass       *I"Bass      *I"Tenor      *I"Tenor    ***
#>            12:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]    ***
#>            13:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]    ***
#>            14:             *>A           *>A           *>A           *>A    ***
#>            15:         *clefF4       *clefF4      *clefGv2      *clefGv2    ***
#>            16:          *k[f#]        *k[f#]        *k[f#]        *k[f#]    ***
#>            17:             *G:           *G:           *G:           *G:    ***
#>            18:           *M3/4         *M3/4         *M3/4         *M3/4    ***
#>            19:          *MM100        *MM100        *MM100        *MM100    ***
#>            20:               4            do             4            mi    ***
#>            21:              =1            =1            =1            =1    ***
#>            22:               4            do             4            mi    ***
#>            23:               4            la             8            fa    ***
#>            24:               .             .             8            mi    ***
#>            25:               4            ti             4            re    ***
#>            26:              =2            =2            =2            =2    ***
#>            27:               4            do             4            do    ***
#>            28:               4            so             4            ti    ***
#>            29:               .             .             .             .    ***
#>            30:               4            la             4            do    ***
#>            31:              =3            =3            =3            =3    ***
#>            32:               4            fa             8            fa    ***
#>            33:               .             .             8            mi    ***
#>            34:               8            mi             4            fa    ***
#>            35:               8            re             .             .    ***
#>            36:               4            do             4            so    ***
#>            37:              =4            =4            =4            =4    ***
#>            38:               2            so             2            so    ***
#>            39:               4            do             4            so    ***
#>            40:              =5            =5            =5            =5    ***
#>            41:               4            ti             4            re    ***
#>            42:               4            do             4            mi    ***
#>            43:               4            re             4            fa    ***
#>            44:              =6            =6            =6            =6    ***
#>            45:               4            mi             4            so    ***
#>            46:               4            fa             4            la    ***
#>            47:               4            so             8            so    ***
#>            48:               .             .             8            fa    ***
#>            49:              =7            =7            =7            =7    ***
#>            50:               2            do             2            mi    ***
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ####################### ^^^ chor001.krn ^^^ ########################
#> 
#>      (two more pieces...)
#> 
#> ####################### vvv chor004.krn vvv ########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:               4            do             4            mi    ***
#>            55:               .             .             .             .    ***
#>            56:              =5            =5            =5            =5    ***
#>            57:               4            la             4            la    ***
#>            58:               .             .             .             .    ***
#>            59:               8            ti             4            re    ***
#>            60:               8            do             .             .    ***
#>            61:               4            re             4            re    ***
#>            62:               4            so             4            re    ***
#>            63:              =6            =6            =6            =6    ***
#>            64:               8            mi             4            so    ***
#>            65:               8            do             .             .    ***
#>            66:               4            re             4            fi    ***
#>            67:               4            so             4            ti    ***
#>            68:               4            do             4            do    ***
#>            69:              =7            =7            =7            =7    ***
#>            70:               4            mi             4            so    ***
#>            71:               .             .             .             .    ***
#>            72:               4            do             4            do    ***
#>            73:               8            fa             4            do    ***
#>            74:               8            so             .             .    ***
#>            75:               4            la             4            la    ***
#>            76:               .             .             .             .    ***
#>            77:              =8            =8            =8            =8    ***
#>            78:               4            di             4            te    ***
#>            79:               4            re             4            la    ***
#>            80:               4            la             4            la    ***
#>            81:               4            do             4            so    ***
#>            82:              =9            =9            =9            =9    ***
#>            83:               4            so             4            so    ***
#>            84:               8            la             4            do    ***
#>            85:               8            ti             .             .    ***
#>            86:               4            do             4            do    ***
#>            87:               4            so             8            ti    ***
#>            88:               .             .             8            so    ***
#>            89:             =10           =10           =10           =10    ***
#>            90:               4            fi             2            re    ***
#>            91:               4            so             .             .    ***
#>            92:               4            do             4            mi    ***
#>            93:              ==            ==            ==            ==    ***
#>            94:              *-            *-            *-            *-    ***
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Seba***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf ***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bac***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, In***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ####################### ^^^ chor004.krn ^^^ ########################
#>            (***four spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Recip.Solfa :: character (**recip tokens)
#>           Token       :: character
#> 

humData |> select(c('Recip', 'Solfa')) |> rend()
#> ####################### vvv chor001.krn vvv ########################
#>             1:  !!!COM: Bach, Johann Sebastian
#>             2:  !!!CDT: 1685/02/21/-1750/07/28/
#>             3:  !!!OTL@@DE: Aus meines Herzens Grunde
#>             4:  !!!OTL@EN:      From the Depths of My Heart
#>             5:  !!!SCT: BWV 269
#>             6:  !!!PC#: 1
#>             7:  !!!AGN: chorale
#>             8:         **recip       **recip       **recip       **recip    ***
#>             9:          *ICvox        *ICvox        *ICvox        *ICvox    ***
#>            10:          *Ibass        *Ibass       *Itenor       *Itenor    ***
#>            11:         *I"Bass       *I"Bass      *I"Tenor      *I"Tenor    ***
#>            12:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]    ***
#>            13:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]    ***
#>            14:             *>A           *>A           *>A           *>A    ***
#>            15:         *clefF4       *clefF4      *clefGv2      *clefGv2    ***
#>            16:          *k[f#]        *k[f#]        *k[f#]        *k[f#]    ***
#>            17:             *G:           *G:           *G:           *G:    ***
#>            18:           *M3/4         *M3/4         *M3/4         *M3/4    ***
#>            19:          *MM100        *MM100        *MM100        *MM100    ***
#>            20:               4            do             4            mi    ***
#>            21:              =1            =1            =1            =1    ***
#>            22:               4            do             4            mi    ***
#>            23:               4            la             8            fa    ***
#>            24:               .             .             8            mi    ***
#>            25:               4            ti             4            re    ***
#>            26:              =2            =2            =2            =2    ***
#>            27:               4            do             4            do    ***
#>            28:               4            so             4            ti    ***
#>            29:               .             .             .             .    ***
#>            30:               4            la             4            do    ***
#>            31:              =3            =3            =3            =3    ***
#>            32:               4            fa             8            fa    ***
#>            33:               .             .             8            mi    ***
#>            34:               8            mi             4            fa    ***
#>            35:               8            re             .             .    ***
#>            36:               4            do             4            so    ***
#>            37:              =4            =4            =4            =4    ***
#>            38:               2            so             2            so    ***
#>            39:               4            do             4            so    ***
#>            40:              =5            =5            =5            =5    ***
#>            41:               4            ti             4            re    ***
#>            42:               4            do             4            mi    ***
#>            43:               4            re             4            fa    ***
#>            44:              =6            =6            =6            =6    ***
#>            45:               4            mi             4            so    ***
#>            46:               4            fa             4            la    ***
#>            47:               4            so             8            so    ***
#>            48:               .             .             8            fa    ***
#>            49:              =7            =7            =7            =7    ***
#>            50:               2            do             2            mi    ***
#> 51-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ####################### ^^^ chor001.krn ^^^ ########################
#> 
#>      (two more pieces...)
#> 
#> ####################### vvv chor004.krn vvv ########################
#>   1-53::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>            54:               4            do             4            mi    ***
#>            55:               .             .             .             .    ***
#>            56:              =5            =5            =5            =5    ***
#>            57:               4            la             4            la    ***
#>            58:               .             .             .             .    ***
#>            59:               8            ti             4            re    ***
#>            60:               8            do             .             .    ***
#>            61:               4            re             4            re    ***
#>            62:               4            so             4            re    ***
#>            63:              =6            =6            =6            =6    ***
#>            64:               8            mi             4            so    ***
#>            65:               8            do             .             .    ***
#>            66:               4            re             4            fi    ***
#>            67:               4            so             4            ti    ***
#>            68:               4            do             4            do    ***
#>            69:              =7            =7            =7            =7    ***
#>            70:               4            mi             4            so    ***
#>            71:               .             .             .             .    ***
#>            72:               4            do             4            do    ***
#>            73:               8            fa             4            do    ***
#>            74:               8            so             .             .    ***
#>            75:               4            la             4            la    ***
#>            76:               .             .             .             .    ***
#>            77:              =8            =8            =8            =8    ***
#>            78:               4            di             4            te    ***
#>            79:               4            re             4            la    ***
#>            80:               4            la             4            la    ***
#>            81:               4            do             4            so    ***
#>            82:              =9            =9            =9            =9    ***
#>            83:               4            so             4            so    ***
#>            84:               8            la             4            do    ***
#>            85:               8            ti             .             .    ***
#>            86:               4            do             4            do    ***
#>            87:               4            so             8            ti    ***
#>            88:               .             .             8            so    ***
#>            89:             =10           =10           =10           =10    ***
#>            90:               4            fi             2            re    ***
#>            91:               4            so             .             .    ***
#>            92:               4            do             4            mi    ***
#>            93:              ==            ==            ==            ==    ***
#>            94:              *-            *-            *-            *-    ***
#>            95:  !!!hum2abc: -Q ''
#>            96:  !!!title: @{PC#}. @{OTL@@DE}
#>            97:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Seba***
#>            98:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf ***
#>            99:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bac***
#>           100:  !!!YOR4: Chorales (New York: Associated Music Publishers, In***
#>           101:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>           102:  !!!EED:  Craig Stuart Sapp
#>           103:  !!!EEV:  2009/05/22
#> ####################### ^^^ chor004.krn ^^^ ########################
#>            (***four spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Recip.Solfa :: character (**recip tokens)
#>           Token       :: character
#> 
```
