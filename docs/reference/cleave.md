# Align data from separate spines into new fields.

Cleave, as in "to cleave together," moves data from separate spines (or
paths) into new fields in the *same* spine(s). Under the hood,
`cleave()` essentially runs a specialized call to make the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
"wider," similar to R functions like
[cast](https://rdrr.io/pkg/reshape2/man/cast.html),
[spread](https://tidyr.tidyverse.org/reference/spread.html), or
[pivot_wider](https://tidyr.tidyverse.org/reference/pivot_wider.html).
In fact, a humdrumR method for
[pivot_wider](https://tidyr.tidyverse.org/reference/pivot_wider.html) is
defined, which is equivalent to `cleave()`. The `cleave()` function is
essentially the inverse of
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md).

## Usage

``` r
cleave(humdrumR, ..., field = selectedFields(humdrumR)[1], newFields = NULL)

# S3 method for class 'humdrumR'
pivot_wider(data, names_from = "Spine", values_from = selectedFields(data)[1])

cleaveSpines(humdrumR, field = selectedFields(humdrumR)[1])

cleavePaths(humdrumR, field = selectedFields(humdrumR)[1])

cleaveStops(humdrumR, field = selectedFields(humdrumR)[1])
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***What to cleave?***

  Must be natural numbers, `character` strings representing exclusive
  interpretations, or lists of either.

- field:

  ***Which field cleave data from.***

  Defaults to first [selected
  field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).

  Must be a `character` string
  [partially](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  matching the name of a data field in the `humdrumR` input. For
  example, `"Tok"` would match the `Token` field.

- newFields:

  ***Names to use for new fields created by the cleave.***

  By default generates names by structure/number (like `Spine2`) or
  exclusive interpretation (like `Silbe`).

  Must be non-empty `character` string.

## Details

Many humdrum datasets encode data across multiple spines, spine-paths,
or stops. By default, `humdrumR` parses each separate spine, spine-path,
and stop as their own individual data points, taking up one row in the
[humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md). If
we want to treat data in multiple spines/paths/stops as different
aspects of the same data it is easiest to reshape the data so that the
information is in different humdrumR
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
rather than separate spines/paths/stops. In the humdrum syntax view, the
spines (or path/stops) are moved "on top" of each other, cleaving them
together.

The convenient `cleaveSpines()`, `cleaveStops()`, and `cleavePaths()`
functions automatically cleave *all* stops/paths in a dataset onto the
first spine/stop/path, creating new fields named, e.g., `Path1`,
`Path2`, etc.

### Syntax

The `cleave()` function takes any number of `...` arguments specifying
*groups* of spines/paths/stops to cleave together.

- `cleave(humData, Spine = 1:2)` will cleave the first and second spine.

- `cleave(humData, Spine = 1:4)` will cleave the first four spines.

- `cleave(humData, Spine = 1:2, Spine = 3:4)` will cleave spine 1 with
  spine 2, and separately, spine 3 with spine 4.

The default is to cleave spines, so you can actually omit the `Spine = `
part: e.g., `cleave(humData, 1:2)` is the same as
`cleave(humData, Spine = 1:2)`. If you want to cleave spine paths,
you'll need to explicitly call something like
`cleave(humData, Path = 0:1)`. The *first* element in each group is used
as the original location which all other spines/paths/stops are cleaved
into. The ordering of the remaining elements is irrelevant.

#### Piece-specific cleaving

By default, the same cleaving will be applied in all pieces (if the
pieces *have* the target spines/paths/stops). However, you can use an
alternate argument structure to apply differerent cleaves to different
pieces. To do so, provide groups to cleave arguments in a list, with
each element in the list representing cleave group in one piece. If the
listed groups are unnamed, the groups are mapped to pieces by index. For
example, the call `cleave(humData, list(1:2, 2:3, 3:4, NULL, 1:3))` will
result in the following cleaves:

- In piece 1, cleave spines 1 and 2.

- In piece 2, cleave spines 2 and 3.

- In piece 3, cleave spines 3 and 4.

- In piece 4, no cleave (no changes).

- In piece 5, cleave the first three spines.

- In any remaining pieces (6 or greater), no cleaves.

Alternatively, you can name the list elements with integers
corresponding to pieces. For example,
`cleave(humData, Path = c("1" = 0:1, "5" = 0:2))` will cleave paths 0
and 1 in piece 1, and paths `0:2` in piece 5.

#### Exclusive interpretations

When cleaving spines, you can specify spines using character strings
representing exclusive interpretations. So you can call
`cleave(humData, c('kern', 'silbe'))`, which will cause `**kern` and
`**silbe` spines in each piece to be cleaved. Note that any exclusive
interpretations which aren't "mentioned" in the call remain in their
original field.

The behavior of exclusive cleaving depends on the relative number of
each target exclusive interpretation in each piece. If there are equal
numbers of each interpretation, the spines are grouped in parallel. For
example, if a piece has the spines `**kern **silbe **kern **silbe`, the
command `cleave(humData, c('kern', 'silbe'))` will see that there are
two `**kern`/`**silbe` pairs, and cleave them just like
`cleave(humData, 1:2, 3:4)`.

If there are different numbers of spines matching each exclusive
interpretation, the cleave behavior depends on which field is the first
field in your input argument—we'll call that the "target" exclusive.
Consider a file with spines `**kern **kern **harm`. If we specify a
cleave call `cleave(humData, c('kern', 'harm'))` that means we want
`**kern` to be the "target" exclusive. Since there are fewer `**harm`
spines than `**kern` spines, the data in the `**harm` spine will be
*duplicated*, can cleaved to **both** `**kern` spines in parallel. If we
instead call `cleave(humData, c('harm', 'kern'))`, making `**harm` the
"target", the two `**kern` spines will both be "piled" atop the single
`**harm` spine, making two new `**kern` fields.

## Fields

Cleaving can (for now) only be applied to one field in our data, which
defaults to the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md);
You can change the target field with the `field` argument.

Cleaving will always introduce new fields into your data. The first
spine/path/stop in each cleave group is left in it's original (target)
field. Other spine/path/stops will be put into new fields. So if you
call `humData |> select(Token) |> cleave(humData, 1:2)`, spine 1 will
remain in the `Token` field and spine 2 data will be put in a new field.
By default, the new field(s) will be automatically named by appending
the type of cleave (spine vs path vs stop) to the number. In the
`cleave(humData, 1:2)` case, the new field will be called `Spine2`. You
can control the name with the `newFields` argument, which must be a
`character` vector. You can provide as many new field names as there
will be new fields. If you provide too few field names, your name(s)
will have numbers appended as necceasary to cover all the new fields; If
you provide too many field names, the extra names will simply be
ignored.

When cleaving by exclusive interpretation `newFields` can be used in the
exact same way. However, by default (if `newFields` is `NULL`),
`cleave()` will name fields by their exclusive interpretation. Note that
the original target field (specified by the `field`) argument will not
have it's name changed. So for example,
`humData |> select(Token) |> cleave(humData, c('kern', 'silbe'))` will
result in the spines `Token` and `Silbe`. No `Kern` field is created,
because the `Kern` data is left in the `Token` field.

## See also

The complement/opposite of `cleave()` is
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md).
The [collapse family of
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
serves a somewhat similar function to `cleave()`.

Other Humdrum table reshaping functions:
[`collapseHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md),
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md),
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)

Other Humdrum table pivoting functions:
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/MozartVariations/.*.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/Rtmpjdf00d/temp_libpath10ef5c6bd813c9/humdrumR/HumdrumData/MozartVariations/.*.krn' matches 8 text files in 1 directory.
#> Eight files read from disk.
#> Validating eight files...
#> all valid.
#> Parsing eight files...
#> Assembling corpus...
#> Done!


humData |> cleave(3:4)
#> #################### vvv M354_00_01a_a.krn vvv ####################
#>           1:  !!!COM: Mozart
#>           2:  !!!OTL: Zwolf Variationen in Es, uber die Romanze 'Je suis Lindor' ***
#>           3:  !!!Variation: Theme a
#>           4:       **function     **harm        **kern**kern         
#>           5:                *          *      *clefF4*clefG2         
#>           6:                *          *          *k[b-e-a-]         
#>           7:            *M2/4      *M2/4               *M2/4         
#>           8:             *E-:       *E-:                *E-:         
#>           9:               =1         =1                  =1         
#>          10:               2T         2I4EE- 4GG 4BB- 4...B-         
#>          11:                .          .           4r8e-L 8g         
#>          12:                .          .             8e-J 8g         
#>          13:               =2         =2                  =2         
#>          14:               2T         2I          8r4.e- 4.g         
#>          15:                .          .                 8E-         
#>          16:                .          .               8EE-L         
#>          17:                .          .         8GGJ8B- 8e-         
#>          18:               =3         =3                  =3         
#>          19:              4.D        4.V         4.BB-8dL 8f         
#>          20:                .          .              8dJ 8f         
#>          21:                .          .             (8dL 8f         
#>          22:               8D         8I         8E-8e-J) 8g         
#>          23:               =4         =4                  =4         
#>          24:                *          *                   *         
#>          25:               4D        4V7             8DL[8fL      8b-
#>          26:                .          .           8BB-J8fJ]      8a-
#>          27:               4T         4I             8E-L8e-       8g
#>          28:                *          *                   *        .
#>          29:                .          .             8EE-J8r         
#>          30:               *-         *-                  *-         
#> #################### ^^^ M354_00_01a_a.krn ^^^ ####################
#> 
#>      (six more pieces...)
#> 
#> #################### vvv M354_01_04d_a.krn vvv ####################
#> 1-14:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>          15:                .          .             (16ccLL        .
#>          16:                .          .                16b-        .
#>          17:                .          .               8f16a        .
#>          18:                .          .             16b-JJ)        .
#>          19:              =41        =41                 =41      =41
#>          20:               2D       2V7d          8r(16ee-LL      2A-
#>          21:                .          .                16dd        .
#>          22:                .          .         4B- 4f16cc#        .
#>          23:                .          .             16ddJJ)        .
#>          24:                .          .             (16ccLL        .
#>          25:                .          .                16b-        .
#>          26:                .          .           8B- 8f16a        .
#>          27:                .          .             16b-JJ)        .
#>          28:                *          *                  *v       *v
#>          29:              =42        =42                 =42         
#>          30:                *          *             *clefF4         
#>          31:               4T        4Ib          8r(16ee-LL         
#>          32:                .          .                16ff         
#>          33:                .          .         8G 8e-16gg)         
#>          34:                .          .              16b'JJ         
#>          35:               4P        4IV           8r(16ccLL         
#>          36:                .          .                16dd         
#>          37:                .          .       8A- 8e-16ee-)         
#>          38:                .          .             16a-'JJ         
#>          39:              =43        =43                 =43         
#>          40:                *          *                  *^         
#>          41:               4D        4Cc            8r(16gLL     2BB-
#>          42:                .          .                16a-        .
#>          43:                .          .            (8G16b-)        .
#>          44:                .          .              16e'JJ        .
#>          45:               4D        4V7          8A-L(16fLL        .
#>          46:                .          .                 16g        .
#>          47:                .          .           8FJ)16a-)        .
#>          48:                .          .              16d'JJ        .
#>          49:                *          *                  *v       *v
#>          50:              =44        =44                 =44         
#>          51:               2T         2I             8E-L4e-         
#>          52:                .          .               8BB-J         
#>          53:                .          .              4EE-4r         
#>          54:               *-         *-                  *-         
#> #################### ^^^ M354_01_04d_a.krn ^^^ ####################
#>              (***one global comment truncated due to screen size***)
#> 
#>  humdrumR corpus of eight pieces.
#> 
#>    Data fields: 
#>          *Spine4 :: character
#>          *Token  :: character
#> 
humData |> cleave(Path = 0:1, newFields = 'Ossia')
#> ##################### vvv M354_00_01a_a.krn vvv ######################
#>           1:  !!!COM: Mozart
#>           2:  !!!OTL: Zwolf Variationen in Es, uber die Romanze 'Je suis Lindor' ***
#>           3:  !!!Variation: Theme a
#>           4:      **function    **harm              **kern            **kern
#>           5:               *         *             *clefF4           *clefG2
#>           6:               *         *          *k[b-e-a-]        *k[b-e-a-]
#>           7:           *M2/4     *M2/4               *M2/4             *M2/4
#>           8:            *E-:      *E-:                *E-:              *E-:
#>           9:              =1        =1                  =1                =1
#>          10:              2T        2I   4EE- 4GG 4BB- 4E-    4B- 4e- 4g 4b-
#>          11:               .         .                  4r           8e-L 8g
#>          12:               .         .                   .           8e-J 8g
#>          13:              =2        =2                  =2                =2
#>          14:              2T        2I                  8r          4.e- 4.g
#>          15:               .         .                 8E-                 .
#>          16:               .         .               8EE-L                 .
#>          17:               .         .                8GGJ           8B- 8e-
#>          18:              =3        =3                  =3                =3
#>          19:             4.D       4.V               4.BB-            8dL 8f
#>          20:               .         .                   .            8dJ 8f
#>          21:               .         .                   .           (8dL 8f
#>          22:              8D        8I                 8E-          8e-J) 8g
#>          23:              =4        =4                  =4                =4
#>          24:               *         *                   *                *^
#>          25:              4D       4V7                 8DL           [8fL8b-
#>          26:               .         .               8BB-J           8fJ]8a-
#>          27:              4T        4I                8E-L             8e-8g
#>          28:               *         *                   *                *v
#>          29:               .         .               8EE-J                8r
#>          30:              *-        *-                  *-                *-
#> ##################### ^^^ M354_00_01a_a.krn ^^^ ######################
#> 
#>      (six more pieces...)
#> 
#> ##################### vvv M354_01_04d_a.krn vvv ######################
#> 1-14::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>          15:               .         .                   .           (16ccLL
#>          16:               .         .                   .              16b-
#>          17:               .         .                  8f               16a
#>          18:               .         .                   .           16b-JJ)
#>          19:             =41       =41                 =41               =41
#>          20:              2D      2V7d               8r2A-          (16ee-LL
#>          21:               .         .                   .              16dd
#>          22:               .         .              4B- 4f             16cc#
#>          23:               .         .                   .           16ddJJ)
#>          24:               .         .                   .           (16ccLL
#>          25:               .         .                   .              16b-
#>          26:               .         .              8B- 8f               16a
#>          27:               .         .                   .           16b-JJ)
#>          28:               *         *                  *v                 *
#>          29:             =42       =42                 =42               =42
#>          30:               *         *             *clefF4                 *
#>          31:              4T       4Ib                  8r          (16ee-LL
#>          32:               .         .                   .              16ff
#>          33:               .         .              8G 8e-             16gg)
#>          34:               .         .                   .            16b'JJ
#>          35:              4P       4IV                  8r           (16ccLL
#>          36:               .         .                   .              16dd
#>          37:               .         .             8A- 8e-            16ee-)
#>          38:               .         .                   .           16a-'JJ
#>          39:             =43       =43                 =43               =43
#>          40:               *         *                  *^                 *
#>          41:              4D       4Cc              8r2BB-            (16gLL
#>          42:               .         .                   .              16a-
#>          43:               .         .                 (8G             16b-)
#>          44:               .         .                   .            16e'JJ
#>          45:              4D       4V7                8A-L            (16fLL
#>          46:               .         .                   .               16g
#>          47:               .         .                8FJ)             16a-)
#>          48:               .         .                   .            16d'JJ
#>          49:               *         *                  *v                 *
#>          50:             =44       =44                 =44               =44
#>          51:              2T        2I                8E-L               4e-
#>          52:               .         .               8BB-J                 .
#>          53:               .         .                4EE-                4r
#>          54:              *-        *-                  *-                *-
#> ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
#>                 (***one global comment truncated due to screen size***)
#> 
#>  humdrumR corpus of eight pieces.
#> 
#>    Data fields: 
#>          *Ossia :: character
#>          *Token :: character
#> 
humData |> cleavePaths()
#> ##################### vvv M354_00_01a_a.krn vvv ######################
#>           1:  !!!COM: Mozart
#>           2:  !!!OTL: Zwolf Variationen in Es, uber die Romanze 'Je suis Lindor' ***
#>           3:  !!!Variation: Theme a
#>           4:      **function    **harm              **kern            **kern
#>           5:               *         *             *clefF4           *clefG2
#>           6:               *         *          *k[b-e-a-]        *k[b-e-a-]
#>           7:           *M2/4     *M2/4               *M2/4             *M2/4
#>           8:            *E-:      *E-:                *E-:              *E-:
#>           9:              =1        =1                  =1                =1
#>          10:              2T        2I   4EE- 4GG 4BB- 4E-    4B- 4e- 4g 4b-
#>          11:               .         .                  4r           8e-L 8g
#>          12:               .         .                   .           8e-J 8g
#>          13:              =2        =2                  =2                =2
#>          14:              2T        2I                  8r          4.e- 4.g
#>          15:               .         .                 8E-                 .
#>          16:               .         .               8EE-L                 .
#>          17:               .         .                8GGJ           8B- 8e-
#>          18:              =3        =3                  =3                =3
#>          19:             4.D       4.V               4.BB-            8dL 8f
#>          20:               .         .                   .            8dJ 8f
#>          21:               .         .                   .           (8dL 8f
#>          22:              8D        8I                 8E-          8e-J) 8g
#>          23:              =4        =4                  =4                =4
#>          24:               *         *                   *                *^
#>          25:              4D       4V7                 8DL           [8fL8b-
#>          26:               .         .               8BB-J           8fJ]8a-
#>          27:              4T        4I                8E-L             8e-8g
#>          28:               *         *                   *                *v
#>          29:               .         .               8EE-J                8r
#>          30:              *-        *-                  *-                *-
#> ##################### ^^^ M354_00_01a_a.krn ^^^ ######################
#> 
#>      (six more pieces...)
#> 
#> ##################### vvv M354_01_04d_a.krn vvv ######################
#> 1-14::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>          15:               .         .                   .           (16ccLL
#>          16:               .         .                   .              16b-
#>          17:               .         .                  8f               16a
#>          18:               .         .                   .           16b-JJ)
#>          19:             =41       =41                 =41               =41
#>          20:              2D      2V7d               8r2A-          (16ee-LL
#>          21:               .         .                   .              16dd
#>          22:               .         .              4B- 4f             16cc#
#>          23:               .         .                   .           16ddJJ)
#>          24:               .         .                   .           (16ccLL
#>          25:               .         .                   .              16b-
#>          26:               .         .              8B- 8f               16a
#>          27:               .         .                   .           16b-JJ)
#>          28:               *         *                  *v                 *
#>          29:             =42       =42                 =42               =42
#>          30:               *         *             *clefF4                 *
#>          31:              4T       4Ib                  8r          (16ee-LL
#>          32:               .         .                   .              16ff
#>          33:               .         .              8G 8e-             16gg)
#>          34:               .         .                   .            16b'JJ
#>          35:              4P       4IV                  8r           (16ccLL
#>          36:               .         .                   .              16dd
#>          37:               .         .             8A- 8e-            16ee-)
#>          38:               .         .                   .           16a-'JJ
#>          39:             =43       =43                 =43               =43
#>          40:               *         *                  *^                 *
#>          41:              4D       4Cc              8r2BB-            (16gLL
#>          42:               .         .                   .              16a-
#>          43:               .         .                 (8G             16b-)
#>          44:               .         .                   .            16e'JJ
#>          45:              4D       4V7                8A-L            (16fLL
#>          46:               .         .                   .               16g
#>          47:               .         .                8FJ)             16a-)
#>          48:               .         .                   .            16d'JJ
#>          49:               *         *                  *v                 *
#>          50:             =44       =44                 =44               =44
#>          51:              2T        2I                8E-L               4e-
#>          52:               .         .               8BB-J                 .
#>          53:               .         .                4EE-                4r
#>          54:              *-        *-                  *-                *-
#> ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
#>                 (***one global comment truncated due to screen size***)
#> 
#>  humdrumR corpus of eight pieces.
#> 
#>    Data fields: 
#>          *Path1 :: character
#>          *Token :: character
#> 

humData |> cleave(c('function', 'harm'), newFields = 'Harmony')
#> ##################### vvv M354_00_01a_a.krn vvv ######################
#>           1:  !!!COM: Mozart
#>           2:  !!!OTL: Zwolf Variationen in Es, uber die Romanze 'Je suis Lind***
#>           3:  !!!Variation: Theme a
#>           4:    **function**harm             **kern                **kern    ***
#>           5:                   *            *clefF4               *clefG2    ***
#>           6:                   *         *k[b-e-a-]            *k[b-e-a-]    ***
#>           7:               *M2/4              *M2/4                 *M2/4    ***
#>           8:                *E-:               *E-:                  *E-:    ***
#>           9:                  =1                 =1                    =1    ***
#>          10:                2T2I  4EE- 4GG 4BB- 4E-        4B- 4e- 4g 4b-    ***
#>          11:                   .                 4r               8e-L 8g    ***
#>          12:                   .                  .               8e-J 8g    ***
#>          13:                  =2                 =2                    =2    ***
#>          14:                2T2I                 8r              4.e- 4.g    ***
#>          15:                   .                8E-                     .    ***
#>          16:                   .              8EE-L                     .    ***
#>          17:                   .               8GGJ               8B- 8e-    ***
#>          18:                  =3                 =3                    =3    ***
#>          19:              4.D4.V              4.BB-                8dL 8f    ***
#>          20:                   .                  .                8dJ 8f    ***
#>          21:                   .                  .               (8dL 8f    ***
#>          22:                8D8I                8E-              8e-J) 8g    ***
#>          23:                  =4                 =4                    =4    ***
#>          24:                   *                  *                    *^    ***
#>          25:               4D4V7                8DL                  [8fL    ***
#>          26:                   .              8BB-J                  8fJ]    ***
#>          27:                4T4I               8E-L                   8e-    ***
#>          28:                   *                  *                    *v    ***
#>          29:                   .              8EE-J                    8r    ***
#>          30:                  *-                 *-                    *-    ***
#> ##################### ^^^ M354_00_01a_a.krn ^^^ ######################
#> 
#>      (six more pieces...)
#> 
#> ##################### vvv M354_01_04d_a.krn vvv ######################
#> 1-14::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>          15:                   .                  .     .         (16ccLL    ***
#>          16:                   .                  .     .            16b-    ***
#>          17:                   .                 8f     .             16a    ***
#>          18:                   .                  .     .         16b-JJ)    ***
#>          19:                 =41                =41   =41             =41    ***
#>          20:              2D2V7d                 8r   2A-        (16ee-LL    ***
#>          21:                   .                  .     .            16dd    ***
#>          22:                   .             4B- 4f     .           16cc#    ***
#>          23:                   .                  .     .         16ddJJ)    ***
#>          24:                   .                  .     .         (16ccLL    ***
#>          25:                   .                  .     .            16b-    ***
#>          26:                   .             8B- 8f     .             16a    ***
#>          27:                   .                  .     .         16b-JJ)    ***
#>          28:                   *                 *v    *v               *    ***
#>          29:                 =42                =42                   =42    ***
#>          30:                   *            *clefF4                     *    ***
#>          31:               4T4Ib                 8r              (16ee-LL    ***
#>          32:                   .                  .                  16ff    ***
#>          33:                   .             8G 8e-                 16gg)    ***
#>          34:                   .                  .                16b'JJ    ***
#>          35:               4P4IV                 8r               (16ccLL    ***
#>          36:                   .                  .                  16dd    ***
#>          37:                   .            8A- 8e-                16ee-)    ***
#>          38:                   .                  .               16a-'JJ    ***
#>          39:                 =43                =43                   =43    ***
#>          40:                   *                 *^                     *    ***
#>          41:               4D4Cc                 8r  2BB-          (16gLL    ***
#>          42:                   .                  .     .            16a-    ***
#>          43:                   .                (8G     .           16b-)    ***
#>          44:                   .                  .     .          16e'JJ    ***
#>          45:               4D4V7               8A-L     .          (16fLL    ***
#>          46:                   .                  .     .             16g    ***
#>          47:                   .               8FJ)     .           16a-)    ***
#>          48:                   .                  .     .          16d'JJ    ***
#>          49:                   *                 *v    *v               *    ***
#>          50:                 =44                =44                   =44    ***
#>          51:                2T2I               8E-L                   4e-    ***
#>          52:                   .              8BB-J                     .    ***
#>          53:                   .               4EE-                    4r    ***
#>          54:                  *-                 *-                    *-    ***
#> ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
#>                 (***one spine/path not displayed due to screen size***)
#> 
#>  humdrumR corpus of eight pieces.
#> 
#>    Data fields: 
#>          *Harmony :: character
#>          *Token   :: character
#> 

humData |> cleave(c('kern', 'function', 'harm'))
#> #################### vvv M354_00_01a_a.krn vvv ####################
#>           1:  !!!COM: Mozart
#>           2:  !!!OTL: Zwolf Variationen in Es, uber die Romanze 'Je suis Lindor' ***
#>           3:  !!!Variation: Theme a
#>           4:  **kern**functio...ha        **kern**functio...ha       
#>           5:               *clefF4                     *clefG2       
#>           6:            *k[b-e-a-]                  *k[b-e-a-]       
#>           7:                 *M2/4                       *M2/4       
#>           8:                  *E-:                        *E-:       
#>           9:                    =1                          =1       
#>          10:  4EE- 4GG 4BB- 4...T2          4B- 4e- 4g 4b-2T2I       
#>          11:                    4r                     8e-L 8g       
#>          12:                     .                     8e-J 8g       
#>          13:                    =2                          =2       
#>          14:                8r2T2I                4.e- 4.g2T2I       
#>          15:                   8E-                           .       
#>          16:                 8EE-L                           .       
#>          17:                  8GGJ                     8B- 8e-       
#>          18:                    =3                          =3       
#>          19:           4.BB-4.D4.V                8dL 8f4.D4.V       
#>          20:                     .                      8dJ 8f       
#>          21:                     .                     (8dL 8f       
#>          22:               8E-8D8I                8e-J) 8g8D8I       
#>          23:                    =4                          =4       
#>          24:                     *                          *^       
#>          25:              8DL4D4V7                   [8fL4D4V7    8b-
#>          26:                 8BB-J                        8fJ]    8a-
#>          27:              8E-L4T4I                     8e-4T4I     8g
#>          28:                     *                          *v     *v
#>          29:                 8EE-J                          8r       
#>          30:                    *-                          *-       
#> #################### ^^^ M354_00_01a_a.krn ^^^ ####################
#> 
#>      (six more pieces...)
#> 
#> #################### vvv M354_01_04d_a.krn vvv ####################
#> 1-14:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>          15:                     .       .             (16ccLL       
#>          16:                     .       .                16b-       
#>          17:                    8f       .                 16a       
#>          18:                     .       .             16b-JJ)       
#>          19:                   =41     =41                 =41       
#>          20:              8r2D2V7d     2A-      (16ee-LL2D2V7d       
#>          21:                     .       .                16dd       
#>          22:                4B- 4f       .               16cc#       
#>          23:                     .       .             16ddJJ)       
#>          24:                     .       .             (16ccLL       
#>          25:                     .       .                16b-       
#>          26:                8B- 8f       .                 16a       
#>          27:                     .       .             16b-JJ)       
#>          28:                    *v      *v                   *       
#>          29:                   =42                         =42       
#>          30:               *clefF4                           *       
#>          31:               8r4T4Ib               (16ee-LL4T4Ib       
#>          32:                     .                        16ff       
#>          33:                8G 8e-                       16gg)       
#>          34:                     .                      16b'JJ       
#>          35:               8r4P4IV                (16ccLL4P4IV       
#>          36:                     .                        16dd       
#>          37:               8A- 8e-                      16ee-)       
#>          38:                     .                     16a-'JJ       
#>          39:                   =43                         =43       
#>          40:                    *^                           *       
#>          41:               8r4D4Cc    2BB-         (16gLL4D4Cc       
#>          42:                     .       .                16a-       
#>          43:                   (8G       .               16b-)       
#>          44:                     .       .              16e'JJ       
#>          45:             8A-L4D4V7       .         (16fLL4D4V7       
#>          46:                     .       .                 16g       
#>          47:                  8FJ)       .               16a-)       
#>          48:                     .       .              16d'JJ       
#>          49:                    *v      *v                   *       
#>          50:                   =44                         =44       
#>          51:              8E-L2T2I                     4e-2T2I       
#>          52:                 8BB-J                           .       
#>          53:                  4EE-                          4r       
#>          54:                    *-                          *-       
#> #################### ^^^ M354_01_04d_a.krn ^^^ ####################
#>              (***one global comment truncated due to screen size***)
#> 
#>  humdrumR corpus of eight pieces.
#> 
#>    Data fields: 
#>          *Function :: character
#>          *Harm     :: character
#>          *Token    :: character
#> 
```
