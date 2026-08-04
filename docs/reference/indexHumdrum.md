# Indexing humdrumR objects

R's built-in indexing operators, `[]` (single brakcets) and `[[]]`
(double brackets) can be used to filter [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
by removing specific pieces, spines, or records from the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
Unlike the more flexible/powerful
[subset()/filter()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
methods, the indexing operators are generally destructive (by default),
meaning filtered data can no longer be accessed after indexing.

## Usage

``` r
humData[]

humData[x:y]

humData["regex"]

humData[[x:y]]

humData[[, x:y]]

humData[["regex"]]

humData[[, "regex"]]

humData[[x:y, l:m]]

humData[[, , regex]]
```

## Arguments

- x:

  ***HumdrumR data to index.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- i:

  ***Index for vectors or matrix/data.frame rows.***

  A numeric vector or a `character` string treated as a regular
  expression.

- drop:

  ***Should empty records/spines/pieces be removed?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- j:

  ***Index for matrix/data.frame columns.***

  A numeric vector or a `character` string treated as a regular
  expression.

## Details

In R, the fundamental [indexing
operators](https://rdrr.io/r/base/Extract.html), `[]` and `[[]]`, are
used to select subsets of data. For many data types (for instance, base
R [lists](https://rdrr.io/r/base/list.html)) the **`[`single
brackets`]`** are used for "shallower" extraction while the **`[[`double
brackets`]]`** are used for "deeper" extraction. By rough analogy with
this "shallow vs deep" dichotomy, [HumdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
indexing brackets are used in two ways:

- **`[`single brackets`]`** are used to select *pieces* in your data.

- **`[[`double brackets`]]`** are used to select records or spines
  *within the pieces* in your data.

(Accidentally writing `[]` when you need `[[]]` is a very common error,
so watch out!)

Whether, indexing by piece or within, `humdrumR` objects can use two
types of indexing arguments: `numeric` (ordinal integers) or `character`
string (interpreted as regular expressions).

### Numeric indexing:

Indexing `humdrumR` corpora with **`[`single brackets`]`** will accept
one numeric argument—only whole numbers are accepted. This argument will
be used to pick pieces within the `humdrumR` object ordinally. Thus,
`humData[1:10]` will select the first ten pieces in the data while
`humData[42]` will select only the 42nd piece.

Indexing `humdrumR` objects with **`[[`double brackets`]]`** will accept
one or two numeric arguments, `i` and `j`, either of which can be used
in isolation or in combination. (If `j` is used in isolation, it must be
named or placed after a comma, as in `humData[[ , j ]]`.)

- `i` is used to index records (i.e., based on the humtable `Record`
  field). Thus, `humData[[1:20]]` indexes the first twenty records *from
  each piece* in the corpus, and `humData[[42]]` extracts the 42nd
  record *from each piece*.

  To avoid breaking the humdrum syntax, exclusive interpretations and
  spine-path interpretations are not removed.

- `j` is used to index spines (i.e., based on the `Spine` field). Thus,
  `humData[[ , 3:4]]` returns the third and fourth spines *from each*
  piece in the corpus.

Pieces/spines/records are renumbered after indexing (see the
**Renumbering** section of the [subset()/filter()
docs](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
for explantion). As a result, `humdrumR` indexing is entirely
**ordinal**. For example,

    humsubset <- humData[11:20]
    humsubset[2]

will return the 12th piece from the original `humData` object. This is
because the first call to `[]` returns the 11th through 20th pieces,
which are renumbered `1:10` and the second index call returns the *new*
2nd index, which was the 12th originally. Similarly,

    humsubset2 <- humData[[ , 2:4]]
    humsubset2[[ , 2]]

will return the third spine from the original data.

#### Negative numbers

As in normal `R` indexing, negative numbers can be used, causing
corresponding elements to be *removed* instead of retained. Thus,
`humData[-3:-5]` will remove the third, fourth, and fifth pieces from
the data while `humData[[ , -3:-5]]` will remove the third, fourth, and
fifth spines from each piece. Positive and negative indices cannot be
mixed in a single argument.

#### Out of bounds indices

In all cases, indices outside of bounds (or of value `0`) are ignored.
E.g., if you have a corpus of twenty pieces and you call `corpus[21]`,
there is no 21st piece, so `21` is "out of bounds". If all your input
indices are `0` and error will result. If *all* your input indices are
out of bounds then an empty `humdrumR` object is returned. For instance,
`humData[[401:500, ]]` will return an empty `humdrumR` object if there
are no pieces with more than 400 data records.

### Character indexing:

If you index a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
with `character` strings, these strings are treated as [regular
expressions](https://en.wikipedia.org/wiki/Regular_expression)
(regexes), which are matched against non-null data tokens (`"D"`) in the
object's first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).
A match to **any** of the regular expressions considered a match.

Indexing with `[`single brackets`]` accepts one vector of `character`
regular expressions. Any piece that contains even a single match will be
retained. If no matches occur in any pieces, an empty `humdrumR` object
is returned.

Indexing `humdrumR` objects with `[[`double brackets`]]` accepts one or
two vectors of `character` strings, `i` and `j`, either of which can be
used in isolation or in combination. (If `j` is used in isolation, it
must be placed after a comma, as in `humData[[ , j]]`.) Any data record
which contains at least one match to the `i` regex(es) will be retained.
Similarly, any spine which contains at least one match to the `j`
regex(es) is retained. If `i` and `j` are used together, matching spines
(`j`) are indexed first, so that tokens matching the regular
expression(s) in `i` must be found in the matching spines.

### Exclusive indexing:

Spines can also be indexed ordinally by exclusive interpretation. To do
this, provide a double-bracket index with a *named* numeric (whole
number) argument, with name(s) corresponding to exclusive
interpretations in the data. For example, if you want to index the 3rd
`**kern` spine in each piece, use `humData[[kern = 3]]`. Note that
*other* exclusive interpretations in each piece are unaffected—in this
example, only the kern spines (if there are any) are indexed!

## See also

For more powerful/flexible indexing options, use
[subset()/filter()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md).

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/RollingStoneCorpus/*.hum")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/RollingStoneCorpus/*.hum' matches 13 text files in 1 directory.
#> Thirteen files read from disk.
#> Validating thirteen files...
#> all valid.
#> Parsing thirteen files...
#> Assembling corpus...
#> Done!

humData[1:2]
#> #################### vvv ACDC_BackInBlack.hum vvv ####################
#>              1:  !!!Rolling Stone List Rank: 187
#>              2:  !!!OTL: Back in Black
#>              3:  !!!COC: AC/DC
#>              4:  !!!RRD: 1980/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:     **harm  **harte   **harm  **harte    **kern  **silbe    ***
#>              7:    !T.d.C.  !T.d.C.    !D.T.    !D.T.   !T.d.C.        !    ***
#>              8:          !        !        !        !    !OCT=5        !    ***
#>              9:         =1       =1       =1       =1        =1       =1    ***
#>             10:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             11:      *M4/4    *M4/4    *M4/4    *M4/4     *M4/4        *    ***
#>             12:        *E:      *E:      *E:      *E:    *e:dor        *    ***
#>             13:          *        *        *        *  *k[f#c#]        *    ***
#>             14:          r        .        r        .         .        .    ***
#>             15:         =2       =2       =2       =2        =2       =2    ***
#>             16:          .        .        .        .         .        .    ***
#>             17:         =3       =3       =3       =3        =3       =3    ***
#>             18:    *>Intro  *>Intro  *>Intro  *>Intro   *>Intro  *>Intro    ***
#>             19:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             20:          I    E:maj        I    E:maj         .        .    ***
#>             21:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             22:         =4       =4       =4       =4        =4       =4    ***
#>             23:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             24:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             25:         =5       =5       =5       =5        =5       =5    ***
#>             26:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             27:          I    E:maj        I    E:maj         .        .    ***
#>             28:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             29:         =6       =6       =6       =6        =6       =6    ***
#>             30:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             31:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             32:         =7       =7       =7       =7        =7       =7    ***
#>             33:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             34:          I    E:maj        I    E:maj         .        .    ***
#>             35:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             36:         =8       =8       =8       =8        =8       =8    ***
#>             37:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             38:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             39:         =9       =9       =9       =9        =9       =9    ***
#>             40:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             41:          I    E:maj        I    E:maj         .        .    ***
#>             42:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             43:        =10      =10      =10      =10       =10      =10    ***
#>             44:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             45:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             46:        =11      =11      =11      =11       =11      =11    ***
#>             47:    *>Verse  *>Verse  *>Verse  *>Verse   *>Verse  *>Verse    ***
#>             48:      *tb16    *tb16    *tb16    *tb16     *tb16        *    ***
#>             49:          I    E:maj        I    E:maj        ee     back    ***
#>             50:          .        .        .        .        ee       in    ***
#> 51-1073:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> #################### ^^^ ACDC_BackInBlack.hum ^^^ ####################
#> ################ vvv AlGreen_LetsStayTogether.hum vvv ################
#>  1-1156:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>           1157:          .        .        .        .         c        _    ***
#>           1158:          .        .        .        .         d        _    ***
#>           1159:          .        .        .        .         .        .    ***
#>           1160:          .        .        .        .         .        .    ***
#>           1161:          .        .        .        .         c        _    ***
#>           1162:          .        .        .        .         d        _    ***
#>           1163:          .        .        .        .         .        .    ***
#>           1164:          .        .        .        .         .        .    ***
#>           1165:          .        .        .        .         .        .    ***
#>           1166:          .        .        .        .         .        .    ***
#>           1167:          .        .        .        .         .        .    ***
#>           1168:          .        .        .        .         .        .    ***
#>           1169:          .        .        .        .         .        .    ***
#>           1170:          .        .        .        .         .        .    ***
#>           1171:          .        .        .        .         .        .    ***
#>           1172:          .        .        .        .         .        .    ***
#>           1173:          .        .        .        .         .        .    ***
#>           1174:          .        .        .        .         d    times    ***
#>           1175:          .        .        .        .         .        .    ***
#>           1176:          .        .        .        .         .        .    ***
#>           1177:          .        .        .        .         .        .    ***
#>           1178:          .        .        .        .         c      are    ***
#>           1179:          .        .        .        .         .        .    ***
#>           1180:          .        .        .        .         .        .    ***
#>           1181:          .        .        .        .         .        .    ***
#>           1182:        =75      =75      =75      =75       =75      =75    ***
#>           1183:       *tb8     *tb8     *tb8     *tb8      *tb8        *    ***
#>           1184:        IV7  B-:maj7       IV   B-:maj         f     good    ***
#>           1185:          .        .        .        .         .        .    ***
#>           1186:          .        .        .        .         d       or    ***
#>           1187:          .        .        .        .         f      bad    ***
#>           1188:       iii7   A:min7      iii    A:min         .        .    ***
#>           1189:          .        .        .        .         c        _    ***
#>           1190:          .        .        .        .         d        _    ***
#>           1191:          .        .        .        .         f     hap-    ***
#>           1192:        =76      =76      =76      =76       =76      =76    ***
#>           1193:         ii    G:min       ii    G:min         .        .    ***
#>           1194:          .        .        .        .         c      -py    ***
#>           1195:          .        .        .        .         d       or    ***
#>           1196:          .        .        .        .         f      sad    ***
#>           1197:          V    C:maj        V    C:maj         .        .    ***
#>           1198:          .        .        .        .         f     come    ***
#>           1199:          .        .        .        .         d       on    ***
#>           1200:          .        .        .        .         .        .    ***
#>           1201:         *-       *-       *-       *-        *-       *-    ***
#>           1202:  !!!ONB: Translated from original encodings in the Rolling S***
#>           1203:  !!!ONB: Original transcribers noted in comments in each spi***
#>           1204:  !!!YOE: David Temperley, Trevor de Clercq
#>           1205:  !!!EED: Nathaniel Condit-Schultz
#>           1206:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ################ ^^^ AlGreen_LetsStayTogether.hum ^^^ ################
#>               (***two spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of two pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 
humData[-1]
#> ################ vvv AlGreen_LetsStayTogether.hum vvv ################
#>              1:  !!!Rolling Stone List Rank: 60
#>              2:  !!!OTL: Let's Stay Together
#>              3:  !!!COC: Al Green
#>              4:  !!!RRD: 1971/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:     **harm  **harte   **harm  **harte   **kern  **silbe    ***
#>              7:    !T.d.C.  !T.d.C.    !D.T.    !D.T.  !T.d.C.        !    ***
#>              8:          !        !        !        !   !OCT=3        !    ***
#>              9:         =1       =1       =1       =1       =1       =1    ***
#>             10:    *>Intro  *>Intro  *>Intro  *>Intro  *>Intro  *>Intro    ***
#>             11:       *tb2     *tb2     *tb2     *tb2     *tb2        *    ***
#>             12:      *M4/4    *M4/4    *M4/4    *M4/4    *M4/4        *    ***
#>             13:        *F:      *F:      *F:      *F:      *F:        *    ***
#>             14:          *        *        *        *   *k[b-]        *    ***
#>             15:        ii7   G:min7       ii    G:min        .        .    ***
#>             16:       iii7   A:min7      iii    A:min        .        .    ***
#>             17:         =2       =2       =2       =2       =2       =2    ***
#>             18:        ii7   G:min7       ii    G:min        .        .    ***
#>             19:       iii7   A:min7      iii    A:min        .        .    ***
#>             20:         =3       =3       =3       =3       =3       =3    ***
#>             21:        ii7   G:min7       ii    G:min        .        .    ***
#>             22:       iii7   A:min7      iii    A:min        .        .    ***
#>             23:         =4       =4       =4       =4       =4       =4    ***
#>             24:        ii7   G:min7       ii    G:min        .        .    ***
#>             25:         V7      C:7        V    C:maj        .        .    ***
#>             26:         =5       =5       =5       =5       =5       =5    ***
#>             27:    *>Verse  *>Verse  *>Verse  *>Verse  *>Verse  *>Verse    ***
#>             28:      *tb32    *tb32    *tb32    *tb32    *tb32        *    ***
#>             29:          I    F:maj        I    F:maj        A        I    ***
#>             30:          .        .        .        .        .        .    ***
#>             31:          .        .        .        .        .        .    ***
#>             32:          .        .        .        .        G        _    ***
#>             33:          .        .        .        .        A        _    ***
#>             34:          .        .        .        .        .        .    ***
#>             35:          .        .        .        .        .        .    ***
#>             36:          .        .        .        .        G        _    ***
#>             37:          .        .        .        .        A        _    ***
#>             38:          .        .        .        .        .        .    ***
#>             39:          .        .        .        .        .        .    ***
#>             40:          .        .        .        .        .        .    ***
#>             41:          .        .        .        .        .        .    ***
#>             42:          .        .        .        .        .        .    ***
#>             43:          .        .        .        .        .        .    ***
#>             44:          .        .        .        .        .        .    ***
#>             45:          .        .        .        .        .        .    ***
#>             46:          .        .        .        .        .        .    ***
#>             47:          .        .        .        .        .        .    ***
#>             48:          .        .        .        .        .        .    ***
#>             49:          .        .        .        .        .        .    ***
#>             50:          .        .        .        .        .        .    ***
#> 51-1206:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ################ ^^^ AlGreen_LetsStayTogether.hum ^^^ ################
#> 
#>      (ten more pieces...)
#> 
#> ################### vvv TheBeatles_HeyJude.hum vvv ###################
#>  1-1571:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>           1572:          .        .        .        .        .        .    ***
#>           1573:          .        .        .        .        .        .    ***
#>           1574:          .        .        .        .        A       na    ***
#>           1575:          .        .        .        .        .        .    ***
#>           1576:          .        .        .        .        c       na    ***
#>           1577:          .        .        .        .        .        .    ***
#>           1578:       =130     =130     =130     =130     =130     =130    ***
#>           1579:      *tb16    *tb16    *tb16    *tb16    *tb16        *    ***
#>           1580:       -VII   E-:maj     -VII   E-:maj        g       na    ***
#>           1581:          .        .        .        .        f       na    ***
#>           1582:          .        .        .        .        g       na    ***
#>           1583:          .        .        .        .        .        .    ***
#>           1584:          .        .        .        .        f       na    ***
#>           1585:          .        .        .        .        .        .    ***
#>           1586:          .        .        .        .        .        .    ***
#>           1587:          .        .        .        .        .        .    ***
#>           1588:          .        .        .        .        .        .    ***
#>           1589:          .        .        .        .        .        .    ***
#>           1590:          .        .        .        .        .        .    ***
#>           1591:          .        .        .        .        .        .    ***
#>           1592:          .        .        .        .        .        .    ***
#>           1593:          .        .        .        .        .        .    ***
#>           1594:          .        .        .        .        .        .    ***
#>           1595:          .        .        .        .        .        .    ***
#>           1596:       =131     =131     =131     =131     =131     =131    ***
#>           1597:         IV   B-:maj       IV   B-:maj        g       na    ***
#>           1598:          .        .        .        .        f       na    ***
#>           1599:          .        .        .        .        g       na    ***
#>           1600:          .        .        .        .        .        .    ***
#>           1601:          .        .        .        .        f       na    ***
#>           1602:          .        .        .        .        .        .    ***
#>           1603:          .        .        .        .        .        .    ***
#>           1604:          .        .        .        .        .        .    ***
#>           1605:          .        .        .        .        .        .    ***
#>           1606:          .        .        .        .        .        .    ***
#>           1607:          .        .        .        .        .        .    ***
#>           1608:          .        .        .        .        .        .    ***
#>           1609:          .        .        .        .       e-      hey    ***
#>           1610:          .        .        .        .        d        _    ***
#>           1611:          .        .        .        .        .        .    ***
#>           1612:          .        .        .        .        c     jude    ***
#>           1613:       =132     =132     =132     =132     =132     =132    ***
#>           1614:       *tb1     *tb1     *tb1     *tb1     *tb1        *    ***
#>           1615:          I    F:maj        I    F:maj        .        .    ***
#>           1616:         *-       *-       *-       *-       *-       *-    ***
#>           1617:  !!!ONB: Translated from original encodings in the Rolling ***
#>           1618:  !!!ONB: Original transcribers noted in comments in each sp***
#>           1619:  !!!YOE: David Temperley, Trevor de Clercq
#>           1620:  !!!EED: Nathaniel Condit-Schultz
#>           1621:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
#>               (***two spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of twelve pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 

humData[[ , 3:4]]
#> ########### vvv ACDC_BackInBlack.hum vvv ###########
#>              1:  !!!Rolling Stone List Rank: 187
#>              2:  !!!OTL: Back in Black
#>              3:  !!!COC: AC/DC
#>              4:  !!!RRD: 1980/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:                **harm             **harte
#>              7:                 !D.T.               !D.T.
#>              8:                     !                   !
#>              9:                    =1                  =1
#>             10:                  *tb1                *tb1
#>             11:                 *M4/4               *M4/4
#>             12:                   *E:                 *E:
#>             13:                     *                   *
#>             14:                     r                   .
#>             15:                    =2                  =2
#>             16:                     .                   .
#>             17:                    =3                  =3
#>             18:               *>Intro             *>Intro
#>             19:                  *tb2                *tb2
#>             20:                     I               E:maj
#>             21:                  -VII               D:maj
#>             22:                    =4                  =4
#>             23:                  *tb1                *tb1
#>             24:                   IVb             A:maj/3
#>             25:                    =5                  =5
#>             26:                  *tb2                *tb2
#>             27:                     I               E:maj
#>             28:                  -VII               D:maj
#>             29:                    =6                  =6
#>             30:                  *tb1                *tb1
#>             31:                   IVb             A:maj/3
#>             32:                    =7                  =7
#>             33:                  *tb2                *tb2
#>             34:                     I               E:maj
#>             35:                  -VII               D:maj
#>             36:                    =8                  =8
#>             37:                  *tb1                *tb1
#>             38:                   IVb             A:maj/3
#>             39:                    =9                  =9
#>             40:                  *tb2                *tb2
#>             41:                     I               E:maj
#>             42:                  -VII               D:maj
#>             43:                   =10                 =10
#>             44:                  *tb1                *tb1
#>             45:                   IVb             A:maj/3
#>             46:                   =11                 =11
#>             47:               *>Verse             *>Verse
#>             48:                 *tb16               *tb16
#>             49:                     I               E:maj
#>             50:                     .                   .
#> 51-1073:::::::::::::::::::::::::::::::::::::::::::::
#> ########### ^^^ ACDC_BackInBlack.hum ^^^ ###########
#> 
#>      (eleven more pieces...)
#> 
#> ########## vvv TheBeatles_HeyJude.hum vvv ##########
#>  1-1571:::::::::::::::::::::::::::::::::::::::::::::
#>           1572:                     .                   .
#>           1573:                     .                   .
#>           1574:                     .                   .
#>           1575:                     .                   .
#>           1576:                     .                   .
#>           1577:                     .                   .
#>           1578:                  =130                =130
#>           1579:                 *tb16               *tb16
#>           1580:                  -VII              E-:maj
#>           1581:                     .                   .
#>           1582:                     .                   .
#>           1583:                     .                   .
#>           1584:                     .                   .
#>           1585:                     .                   .
#>           1586:                     .                   .
#>           1587:                     .                   .
#>           1588:                     .                   .
#>           1589:                     .                   .
#>           1590:                     .                   .
#>           1591:                     .                   .
#>           1592:                     .                   .
#>           1593:                     .                   .
#>           1594:                     .                   .
#>           1595:                     .                   .
#>           1596:                  =131                =131
#>           1597:                    IV              B-:maj
#>           1598:                     .                   .
#>           1599:                     .                   .
#>           1600:                     .                   .
#>           1601:                     .                   .
#>           1602:                     .                   .
#>           1603:                     .                   .
#>           1604:                     .                   .
#>           1605:                     .                   .
#>           1606:                     .                   .
#>           1607:                     .                   .
#>           1608:                     .                   .
#>           1609:                     .                   .
#>           1610:                     .                   .
#>           1611:                     .                   .
#>           1612:                     .                   .
#>           1613:                  =132                =132
#>           1614:                  *tb1                *tb1
#>           1615:                     I               F:maj
#>           1616:                    *-                  *-
#>           1617:  !!!ONB: Translated from original encodings in the Rolling Stone C***
#>           1618:  !!!ONB: Original transcribers noted in comments in each spine: !D***
#>           1619:  !!!YOE: David Temperley, Trevor de Clercq
#>           1620:  !!!EED: Nathaniel Condit-Schultz
#>           1621:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ########## ^^^ TheBeatles_HeyJude.hum ^^^ ##########
#>                                               (***two
#>      global comments truncated due to screen size***)
#> 
#>  humdrumR corpus of thirteen pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 
humData[[1:40 , ]]
#> #################### vvv ACDC_BackInBlack.hum vvv ####################
#>               1:  !!!Rolling Stone List Rank: 187
#>               2:  !!!OTL: Back in Black
#>               3:  !!!COC: AC/DC
#>               4:  !!!RRD: 1980/
#>               5:  !!!In original RS 5x20 subset: True
#>               6:      **harm   **harte    **harm   **harte    **kern   **silbe    ***
#>               7:     !T.d.C.   !T.d.C.     !D.T.     !D.T.   !T.d.C.         !    ***
#>               8:           !         !         !         !    !OCT=5         !    ***
#>               9:          =1        =1        =1        =1        =1        =1    ***
#>              10:        *tb1      *tb1      *tb1      *tb1      *tb1         *    ***
#>              11:       *M4/4     *M4/4     *M4/4     *M4/4     *M4/4         *    ***
#>              12:         *E:       *E:       *E:       *E:    *e:dor         *    ***
#>              13:           *         *         *         *  *k[f#c#]         *    ***
#>              14:           r         .         r         .         .         .    ***
#>              15:          =2        =2        =2        =2        =2        =2    ***
#>              16:           .         .         .         .         .         .    ***
#>              17:          =3        =3        =3        =3        =3        =3    ***
#>              18:     *>Intro   *>Intro   *>Intro   *>Intro   *>Intro   *>Intro    ***
#>              19:        *tb2      *tb2      *tb2      *tb2      *tb2         *    ***
#>              20:           I     E:maj         I     E:maj         .         .    ***
#>              21:        -VII     D:maj      -VII     D:maj         .         .    ***
#>              22:          =4        =4        =4        =4        =4        =4    ***
#>              23:        *tb1      *tb1      *tb1      *tb1      *tb1         *    ***
#>              24:         IVb   A:maj/3       IVb   A:maj/3         .         .    ***
#>              25:          =5        =5        =5        =5        =5        =5    ***
#>              26:        *tb2      *tb2      *tb2      *tb2      *tb2         *    ***
#>              27:           I     E:maj         I     E:maj         .         .    ***
#>              28:        -VII     D:maj      -VII     D:maj         .         .    ***
#>              29:          =6        =6        =6        =6        =6        =6    ***
#>              30:        *tb1      *tb1      *tb1      *tb1      *tb1         *    ***
#>              31:         IVb   A:maj/3       IVb   A:maj/3         .         .    ***
#>              32:          =7        =7        =7        =7        =7        =7    ***
#>              33:        *tb2      *tb2      *tb2      *tb2      *tb2         *    ***
#>              34:           I     E:maj         I     E:maj         .         .    ***
#>              35:        -VII     D:maj      -VII     D:maj         .         .    ***
#>              36:          =8        =8        =8        =8        =8        =8    ***
#>              37:        *tb1      *tb1      *tb1      *tb1      *tb1         *    ***
#>              38:         IVb   A:maj/3       IVb   A:maj/3         .         .    ***
#>              39:          =9        =9        =9        =9        =9        =9    ***
#>              40:        *tb2      *tb2      *tb2      *tb2      *tb2         *    ***
#>              44:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>              47:     *>Verse   *>Verse   *>Verse   *>Verse   *>Verse   *>Verse    ***
#>              48:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>             151:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>             161:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>             179:    *>Chorus  *>Chorus  *>Chorus  *>Chorus  *>Chorus  *>Chorus    ***
#>             316:     *>Verse   *>Verse   *>Verse   *>Verse   *>Verse   *>Verse    ***
#>             453:    *>Chorus  *>Chorus  *>Chorus  *>Chorus  *>Chorus  *>Chorus    ***
#>             590:      *>Solo    *>Solo    *>Solo    *>Solo    *>Solo    *>Solo    ***
#>             591:        *tb2      *tb2      *tb2      *tb2      *tb2              ***
#> 601-1068::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> #################### ^^^ ACDC_BackInBlack.hum ^^^ ####################
#> 
#>      (eleven more pieces...)
#> 
#> ################### vvv TheBeatles_HeyJude.hum vvv ###################
#>    1-811::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>             846:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>             849:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>             859:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>             894:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>             897:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>             907:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>             942:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>             945:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>             955:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>             990:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>             993:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1003:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1038:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1041:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1051:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1086:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1089:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1099:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1134:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1137:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1147:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1182:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1185:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1195:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1230:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1233:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1243:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1278:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1281:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1291:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1326:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1329:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1339:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1374:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1377:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1387:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1422:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1425:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1435:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1470:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1473:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1483:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1518:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1521:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1531:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1566:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1569:        *tb8      *tb8      *tb8      *tb8      *tb8              ***
#>            1579:       *tb16     *tb16     *tb16     *tb16     *tb16              ***
#>            1614:        *tb1      *tb1      *tb1      *tb1      *tb1              ***
#>            1616:          *-        *-        *-        *-        *-        *-    ***
#> ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
#>               (***two spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of thirteen pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 

# find all pieces which use a flat 3
humData['b3']
#> ################ vvv AlGreen_LetsStayTogether.hum vvv ################
#>              1:  !!!Rolling Stone List Rank: 60
#>              2:  !!!OTL: Let's Stay Together
#>              3:  !!!COC: Al Green
#>              4:  !!!RRD: 1971/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:     **harm  **harte   **harm  **harte   **kern  **silbe    ***
#>              7:    !T.d.C.  !T.d.C.    !D.T.    !D.T.  !T.d.C.        !    ***
#>              8:          !        !        !        !   !OCT=3        !    ***
#>              9:         =1       =1       =1       =1       =1       =1    ***
#>             10:    *>Intro  *>Intro  *>Intro  *>Intro  *>Intro  *>Intro    ***
#>             11:       *tb2     *tb2     *tb2     *tb2     *tb2        *    ***
#>             12:      *M4/4    *M4/4    *M4/4    *M4/4    *M4/4        *    ***
#>             13:        *F:      *F:      *F:      *F:      *F:        *    ***
#>             14:          *        *        *        *   *k[b-]        *    ***
#>             15:        ii7   G:min7       ii    G:min        .        .    ***
#>             16:       iii7   A:min7      iii    A:min        .        .    ***
#>             17:         =2       =2       =2       =2       =2       =2    ***
#>             18:        ii7   G:min7       ii    G:min        .        .    ***
#>             19:       iii7   A:min7      iii    A:min        .        .    ***
#>             20:         =3       =3       =3       =3       =3       =3    ***
#>             21:        ii7   G:min7       ii    G:min        .        .    ***
#>             22:       iii7   A:min7      iii    A:min        .        .    ***
#>             23:         =4       =4       =4       =4       =4       =4    ***
#>             24:        ii7   G:min7       ii    G:min        .        .    ***
#>             25:         V7      C:7        V    C:maj        .        .    ***
#>             26:         =5       =5       =5       =5       =5       =5    ***
#>             27:    *>Verse  *>Verse  *>Verse  *>Verse  *>Verse  *>Verse    ***
#>             28:      *tb32    *tb32    *tb32    *tb32    *tb32        *    ***
#>             29:          I    F:maj        I    F:maj        A        I    ***
#>             30:          .        .        .        .        .        .    ***
#>             31:          .        .        .        .        .        .    ***
#>             32:          .        .        .        .        G        _    ***
#>             33:          .        .        .        .        A        _    ***
#>             34:          .        .        .        .        .        .    ***
#>             35:          .        .        .        .        .        .    ***
#>             36:          .        .        .        .        G        _    ***
#>             37:          .        .        .        .        A        _    ***
#>             38:          .        .        .        .        .        .    ***
#>             39:          .        .        .        .        .        .    ***
#>             40:          .        .        .        .        .        .    ***
#>             41:          .        .        .        .        .        .    ***
#>             42:          .        .        .        .        .        .    ***
#>             43:          .        .        .        .        .        .    ***
#>             44:          .        .        .        .        .        .    ***
#>             45:          .        .        .        .        .        .    ***
#>             46:          .        .        .        .        .        .    ***
#>             47:          .        .        .        .        .        .    ***
#>             48:          .        .        .        .        .        .    ***
#>             49:          .        .        .        .        .        .    ***
#>             50:          .        .        .        .        .        .    ***
#> 51-1206:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ################ ^^^ AlGreen_LetsStayTogether.hum ^^^ ################
#> 
#>      (two more pieces...)
#> 
#> ################### vvv TheBeatles_HeyJude.hum vvv ###################
#>  1-1571:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>           1572:          .        .        .        .        .        .    ***
#>           1573:          .        .        .        .        .        .    ***
#>           1574:          .        .        .        .        A       na    ***
#>           1575:          .        .        .        .        .        .    ***
#>           1576:          .        .        .        .        c       na    ***
#>           1577:          .        .        .        .        .        .    ***
#>           1578:       =130     =130     =130     =130     =130     =130    ***
#>           1579:      *tb16    *tb16    *tb16    *tb16    *tb16        *    ***
#>           1580:       -VII   E-:maj     -VII   E-:maj        g       na    ***
#>           1581:          .        .        .        .        f       na    ***
#>           1582:          .        .        .        .        g       na    ***
#>           1583:          .        .        .        .        .        .    ***
#>           1584:          .        .        .        .        f       na    ***
#>           1585:          .        .        .        .        .        .    ***
#>           1586:          .        .        .        .        .        .    ***
#>           1587:          .        .        .        .        .        .    ***
#>           1588:          .        .        .        .        .        .    ***
#>           1589:          .        .        .        .        .        .    ***
#>           1590:          .        .        .        .        .        .    ***
#>           1591:          .        .        .        .        .        .    ***
#>           1592:          .        .        .        .        .        .    ***
#>           1593:          .        .        .        .        .        .    ***
#>           1594:          .        .        .        .        .        .    ***
#>           1595:          .        .        .        .        .        .    ***
#>           1596:       =131     =131     =131     =131     =131     =131    ***
#>           1597:         IV   B-:maj       IV   B-:maj        g       na    ***
#>           1598:          .        .        .        .        f       na    ***
#>           1599:          .        .        .        .        g       na    ***
#>           1600:          .        .        .        .        .        .    ***
#>           1601:          .        .        .        .        f       na    ***
#>           1602:          .        .        .        .        .        .    ***
#>           1603:          .        .        .        .        .        .    ***
#>           1604:          .        .        .        .        .        .    ***
#>           1605:          .        .        .        .        .        .    ***
#>           1606:          .        .        .        .        .        .    ***
#>           1607:          .        .        .        .        .        .    ***
#>           1608:          .        .        .        .        .        .    ***
#>           1609:          .        .        .        .       e-      hey    ***
#>           1610:          .        .        .        .        d        _    ***
#>           1611:          .        .        .        .        .        .    ***
#>           1612:          .        .        .        .        c     jude    ***
#>           1613:       =132     =132     =132     =132     =132     =132    ***
#>           1614:       *tb1     *tb1     *tb1     *tb1     *tb1        *    ***
#>           1615:          I    F:maj        I    F:maj        .        .    ***
#>           1616:         *-       *-       *-       *-       *-       *-    ***
#>           1617:  !!!ONB: Translated from original encodings in the Rolling ***
#>           1618:  !!!ONB: Original transcribers noted in comments in each sp***
#>           1619:  !!!YOE: David Temperley, Trevor de Clercq
#>           1620:  !!!EED: Nathaniel Condit-Schultz
#>           1621:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
#>               (***two spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 

# find all records that use a flat 3
humData[['b3', ]]
#> ################ vvv AlGreen_LetsStayTogether.hum vvv ################
#>              1:  !!!Rolling Stone List Rank: 60
#>              2:  !!!OTL: Let's Stay Together
#>              3:  !!!COC: Al Green
#>              4:  !!!RRD: 1971/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:       **harm    **harte     **harm    **harte     **kern    ***
#>              7:      !T.d.C.    !T.d.C.      !D.T.      !D.T.    !T.d.C.    ***
#>              8:            !          !          !          !     !OCT=3    ***
#>              9:           =1         =1         =1         =1         =1    ***
#>             10:      *>Intro    *>Intro    *>Intro    *>Intro    *>Intro    ***
#>             11:         *tb2       *tb2       *tb2       *tb2       *tb2    ***
#>             12:        *M4/4      *M4/4      *M4/4      *M4/4      *M4/4    ***
#>             13:          *F:        *F:        *F:        *F:        *F:    ***
#>             14:            *          *          *          *     *k[b-]    ***
#>             15:            .          .          .          .          .    ***
#>             16:            .          .          .          .          .    ***
#>             17:           =2         =2         =2         =2         =2    ***
#>             18:            .          .          .          .          .    ***
#>             19:            .          .          .          .          .    ***
#>             20:           =3         =3         =3         =3         =3    ***
#>             21:            .          .          .          .          .    ***
#>             22:            .          .          .          .          .    ***
#>             23:           =4         =4         =4         =4         =4    ***
#>             24:            .          .          .          .          .    ***
#>             25:            .          .          .          .          .    ***
#>             26:           =5         =5         =5         =5         =5    ***
#>             27:      *>Verse    *>Verse    *>Verse    *>Verse    *>Verse    ***
#>             28:        *tb32      *tb32      *tb32      *tb32      *tb32    ***
#>             29:            .          .          .          .          .    ***
#>             30:            .          .          .          .          .    ***
#>             31:            .          .          .          .          .    ***
#>             32:            .          .          .          .          .    ***
#>             33:            .          .          .          .          .    ***
#>             34:            .          .          .          .          .    ***
#>             35:            .          .          .          .          .    ***
#>             36:            .          .          .          .          .    ***
#>             37:            .          .          .          .          .    ***
#>             38:            .          .          .          .          .    ***
#>             39:            .          .          .          .          .    ***
#>             40:            .          .          .          .          .    ***
#>             41:            .          .          .          .          .    ***
#>             42:            .          .          .          .          .    ***
#>             43:            .          .          .          .          .    ***
#>             44:            .          .          .          .          .    ***
#>             45:            .          .          .          .          .    ***
#>             46:            .          .          .          .          .    ***
#>             47:            .          .          .          .          .    ***
#>             48:            .          .          .          .          .    ***
#>             49:            .          .          .          .          .    ***
#>             50:            .          .          .          .          .    ***
#> 51-1206:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> ################ ^^^ AlGreen_LetsStayTogether.hum ^^^ ################
#> 
#>      (two more pieces...)
#> 
#> ################### vvv TheBeatles_HeyJude.hum vvv ###################
#>  1-1472:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>           1473:         *tb8       *tb8       *tb8       *tb8       *tb8    ***
#>           1474:            .          .          .          .          .    ***
#>           1475:            .          .          .          .          .    ***
#>           1476:            .          .          .          .          .    ***
#>           1477:            .          .          .          .          .    ***
#>           1478:            .          .          .          .          .    ***
#>           1479:            .          .          .          .          .    ***
#>           1480:            .          .          .          .          .    ***
#>           1481:            .          .          .          .          .    ***
#>           1482:         =122       =122       =122       =122       =122    ***
#>           1483:        *tb16      *tb16      *tb16      *tb16      *tb16    ***
#>      1484-1517:    ==123-124  ==123-124  ==123-124  ==123-124  ==123-124    ***
#>           1518:         *tb1       *tb1       *tb1       *tb1       *tb1    ***
#>           1519:            .          .          .          .          .    ***
#>           1520:         =125       =125       =125       =125       =125    ***
#>           1521:         *tb8       *tb8       *tb8       *tb8       *tb8    ***
#>           1522:            .          .          .          .          .    ***
#>           1523:            .          .          .          .          .    ***
#>           1524:            .          .          .          .          .    ***
#>           1525:            .          .          .          .          .    ***
#>           1526:            .          .          .          .          .    ***
#>           1527:            .          .          .          .          .    ***
#>           1528:            .          .          .          .          .    ***
#>           1529:            .          .          .          .          .    ***
#>           1530:         =126       =126       =126       =126       =126    ***
#>           1531:        *tb16      *tb16      *tb16      *tb16      *tb16    ***
#>      1532-1565:    ==127-128  ==127-128  ==127-128  ==127-128  ==127-128    ***
#>           1566:         *tb1       *tb1       *tb1       *tb1       *tb1    ***
#>           1567:            .          .          .          .          .    ***
#>           1568:         =129       =129       =129       =129       =129    ***
#>           1569:         *tb8       *tb8       *tb8       *tb8       *tb8    ***
#>           1570:            .          .          .          .          .    ***
#>           1571:            .          .          .          .          .    ***
#>           1572:            .          .          .          .          .    ***
#>           1573:            .          .          .          .          .    ***
#>           1574:            .          .          .          .          .    ***
#>           1575:            .          .          .          .          .    ***
#>           1576:            .          .          .          .          .    ***
#>           1577:            .          .          .          .          .    ***
#>           1578:         =130       =130       =130       =130       =130    ***
#>           1579:        *tb16      *tb16      *tb16      *tb16      *tb16    ***
#>      1580-1613:    ==131-132  ==131-132  ==131-132  ==131-132  ==131-132    ***
#>           1614:         *tb1       *tb1       *tb1       *tb1       *tb1    ***
#>           1615:            .          .          .          .          .    ***
#>           1616:           *-         *-         *-         *-         *-    ***
#>           1617:  !!!ONB: Translated from original encodings in the Rolling S***
#>           1618:  !!!ONB: Original transcribers noted in comments in each spi***
#>           1619:  !!!YOE: David Temperley, Trevor de Clercq
#>           1620:  !!!EED: Nathaniel Condit-Schultz
#>           1621:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
#>             (***three spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of four pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 

# Exclusive interpretation indexing
humData[[deg = 1]]
#> #################### vvv ACDC_BackInBlack.hum vvv ####################
#>              1:  !!!Rolling Stone List Rank: 187
#>              2:  !!!OTL: Back in Black
#>              3:  !!!COC: AC/DC
#>              4:  !!!RRD: 1980/
#>              5:  !!!In original RS 5x20 subset: True
#>              6:     **harm  **harte   **harm  **harte    **kern  **silbe    ***
#>              7:    !T.d.C.  !T.d.C.    !D.T.    !D.T.   !T.d.C.        !    ***
#>              8:          !        !        !        !    !OCT=5        !    ***
#>              9:         =1       =1       =1       =1        =1       =1    ***
#>             10:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             11:      *M4/4    *M4/4    *M4/4    *M4/4     *M4/4        *    ***
#>             12:        *E:      *E:      *E:      *E:    *e:dor        *    ***
#>             13:          *        *        *        *  *k[f#c#]        *    ***
#>             14:          r        .        r        .         .        .    ***
#>             15:         =2       =2       =2       =2        =2       =2    ***
#>             16:          .        .        .        .         .        .    ***
#>             17:         =3       =3       =3       =3        =3       =3    ***
#>             18:    *>Intro  *>Intro  *>Intro  *>Intro   *>Intro  *>Intro    ***
#>             19:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             20:          I    E:maj        I    E:maj         .        .    ***
#>             21:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             22:         =4       =4       =4       =4        =4       =4    ***
#>             23:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             24:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             25:         =5       =5       =5       =5        =5       =5    ***
#>             26:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             27:          I    E:maj        I    E:maj         .        .    ***
#>             28:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             29:         =6       =6       =6       =6        =6       =6    ***
#>             30:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             31:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             32:         =7       =7       =7       =7        =7       =7    ***
#>             33:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             34:          I    E:maj        I    E:maj         .        .    ***
#>             35:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             36:         =8       =8       =8       =8        =8       =8    ***
#>             37:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             38:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             39:         =9       =9       =9       =9        =9       =9    ***
#>             40:       *tb2     *tb2     *tb2     *tb2      *tb2        *    ***
#>             41:          I    E:maj        I    E:maj         .        .    ***
#>             42:       -VII    D:maj     -VII    D:maj         .        .    ***
#>             43:        =10      =10      =10      =10       =10      =10    ***
#>             44:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>             45:        IVb  A:maj/3      IVb  A:maj/3         .        .    ***
#>             46:        =11      =11      =11      =11       =11      =11    ***
#>             47:    *>Verse  *>Verse  *>Verse  *>Verse   *>Verse  *>Verse    ***
#>             48:      *tb16    *tb16    *tb16    *tb16     *tb16        *    ***
#>             49:          I    E:maj        I    E:maj        ee     back    ***
#>             50:          .        .        .        .        ee       in    ***
#> 51-1073:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#> #################### ^^^ ACDC_BackInBlack.hum ^^^ ####################
#> 
#>      (eleven more pieces...)
#> 
#> ################### vvv TheBeatles_HeyJude.hum vvv ###################
#>  1-1571:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#>           1572:          .        .        .        .         .        .    ***
#>           1573:          .        .        .        .         .        .    ***
#>           1574:          .        .        .        .         A       na    ***
#>           1575:          .        .        .        .         .        .    ***
#>           1576:          .        .        .        .         c       na    ***
#>           1577:          .        .        .        .         .        .    ***
#>           1578:       =130     =130     =130     =130      =130     =130    ***
#>           1579:      *tb16    *tb16    *tb16    *tb16     *tb16        *    ***
#>           1580:       -VII   E-:maj     -VII   E-:maj         g       na    ***
#>           1581:          .        .        .        .         f       na    ***
#>           1582:          .        .        .        .         g       na    ***
#>           1583:          .        .        .        .         .        .    ***
#>           1584:          .        .        .        .         f       na    ***
#>           1585:          .        .        .        .         .        .    ***
#>           1586:          .        .        .        .         .        .    ***
#>           1587:          .        .        .        .         .        .    ***
#>           1588:          .        .        .        .         .        .    ***
#>           1589:          .        .        .        .         .        .    ***
#>           1590:          .        .        .        .         .        .    ***
#>           1591:          .        .        .        .         .        .    ***
#>           1592:          .        .        .        .         .        .    ***
#>           1593:          .        .        .        .         .        .    ***
#>           1594:          .        .        .        .         .        .    ***
#>           1595:          .        .        .        .         .        .    ***
#>           1596:       =131     =131     =131     =131      =131     =131    ***
#>           1597:         IV   B-:maj       IV   B-:maj         g       na    ***
#>           1598:          .        .        .        .         f       na    ***
#>           1599:          .        .        .        .         g       na    ***
#>           1600:          .        .        .        .         .        .    ***
#>           1601:          .        .        .        .         f       na    ***
#>           1602:          .        .        .        .         .        .    ***
#>           1603:          .        .        .        .         .        .    ***
#>           1604:          .        .        .        .         .        .    ***
#>           1605:          .        .        .        .         .        .    ***
#>           1606:          .        .        .        .         .        .    ***
#>           1607:          .        .        .        .         .        .    ***
#>           1608:          .        .        .        .         .        .    ***
#>           1609:          .        .        .        .        e-      hey    ***
#>           1610:          .        .        .        .         d        _    ***
#>           1611:          .        .        .        .         .        .    ***
#>           1612:          .        .        .        .         c     jude    ***
#>           1613:       =132     =132     =132     =132      =132     =132    ***
#>           1614:       *tb1     *tb1     *tb1     *tb1      *tb1        *    ***
#>           1615:          I    F:maj        I    F:maj         .        .    ***
#>           1616:         *-       *-       *-       *-        *-       *-    ***
#>           1617:  !!!ONB: Translated from original encodings in the Rolling S***
#>           1618:  !!!ONB: Original transcribers noted in comments in each spi***
#>           1619:  !!!YOE: David Temperley, Trevor de Clercq
#>           1620:  !!!EED: Nathaniel Condit-Schultz
#>           1621:  !!!ENC: Nathaniel Condit-Schultz, automated
#> ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
#>               (***two spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of thirteen pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 

# pipe indexing
humData |> _[1:3] |> _[[20:30]]
#> ############### vvv ACDC_BackInBlack.hum vvv ################
#>      6:            **harm         **harte          **harm    ***
#>     10:              *tb1            *tb1            *tb1    ***
#>     18:           *>Intro         *>Intro         *>Intro    ***
#>     19:              *tb2            *tb2            *tb2    ***
#>     20:                 I           E:maj               I    ***
#>     21:              -VII           D:maj            -VII    ***
#>     22:                =4              =4              =4    ***
#>     23:              *tb1            *tb1            *tb1    ***
#>     24:               IVb         A:maj/3             IVb    ***
#>     25:                =5              =5              =5    ***
#>     26:              *tb2            *tb2            *tb2    ***
#>     27:                 I           E:maj               I    ***
#>     28:              -VII           D:maj            -VII    ***
#>     29:                =6              =6              =6    ***
#>     30:              *tb1            *tb1            *tb1    ***
#>     33:              *tb2            *tb2            *tb2    ***
#>     37:              *tb1            *tb1            *tb1    ***
#>     40:              *tb2            *tb2            *tb2    ***
#>     44:              *tb1            *tb1            *tb1    ***
#>     47:           *>Verse         *>Verse         *>Verse    ***
#>     48:             *tb16           *tb16           *tb16    ***
#>    151:              *tb8            *tb8            *tb8    ***
#>    161:             *tb16           *tb16           *tb16    ***
#>    179:          *>Chorus        *>Chorus        *>Chorus    ***
#>    316:           *>Verse         *>Verse         *>Verse    ***
#>    453:          *>Chorus        *>Chorus        *>Chorus    ***
#>    590:            *>Solo          *>Solo          *>Solo    ***
#>    591:              *tb2            *tb2            *tb2    ***
#>    601:              *tb8            *tb8            *tb8    ***
#>    611:              *tb2            *tb2            *tb2    ***
#>    621:              *tb8            *tb8            *tb8    ***
#>    631:              *tb2            *tb2            *tb2    ***
#>    641:              *tb8            *tb8            *tb8    ***
#>    651:              *tb2            *tb2            *tb2    ***
#>    661:             *tb16           *tb16           *tb16    ***
#>    687:          *>Chorus        *>Chorus        *>Chorus    ***
#>    824:    *>Instrumental  *>Instrumental  *>Instrumental    ***
#>    825:              *tb1            *tb1            *tb1    ***
#>    840:             *tb16           *tb16           *tb16    ***
#>    859:          *>Chorus        *>Chorus        *>Chorus    ***
#>    996:              *tb1            *tb1            *tb1    ***
#>   1001:         *>Fadeout       *>Fadeout       *>Fadeout    ***
#>   1002:              *tb2            *tb2            *tb2    ***
#>   1012:              *tb8            *tb8            *tb8    ***
#>   1022:              *tb2            *tb2            *tb2    ***
#>   1032:              *tb8            *tb8            *tb8    ***
#>   1042:              *tb2            *tb2            *tb2    ***
#>   1052:              *tb8            *tb8            *tb8    ***
#>   1062:              *tb2            *tb2            *tb2    ***
#>   1068:                *-              *-              *-    ***
#> ############### ^^^ ACDC_BackInBlack.hum ^^^ ################
#> 
#>      (one other piece...)
#> 
#> ########## vvv CarlPerkins_BlueSuedeShoes.hum vvv ###########
#> 
#>      6:            **harm         **harte          **harm    ***
#>     10:              *tb8            *tb8            *tb8    ***
#>     20:                 .               .               .    ***
#>     21:                 .               .               .    ***
#>     22:                 .               .               .    ***
#>     23:                 .               .               .    ***
#>     24:                 .               .               .    ***
#>     25:                 .               .               .    ***
#>     26:                =2              =2              =2    ***
#>     27:           *>Intro         *>Intro         *>Intro    ***
#>     28:                 I           A:maj               I    ***
#>     29:                 .               .               .    ***
#>     30:                 .               .               .    ***
#>     41:             *tb2.           *tb2.           *tb2.    ***
#>     45:              *tb8            *tb8            *tb8    ***
#>     60:             *tb2.           *tb2.           *tb2.    ***
#>     64:              *tb8            *tb8            *tb8    ***
#>     92:              *tb4            *tb4            *tb4    ***
#>    100:              *tb8            *tb8            *tb8    ***
#>    114:             *tb1.           *tb1.           *tb1.    ***
#>    117:              *tb8            *tb8            *tb8    ***
#>    157:             *tb1.           *tb1.           *tb1.    ***
#>    160:              *tb8            *tb8            *tb8    ***
#>    174:            *>Link          *>Link          *>Link    ***
#>    227:               *>A             *>A             *>A    ***
#>    280:              *tb4            *tb4            *tb4    ***
#>    288:              *tb8            *tb8            *tb8    ***
#>    302:             *tb1.           *tb1.           *tb1.    ***
#>    305:              *tb8            *tb8            *tb8    ***
#>    345:             *tb1.           *tb1.           *tb1.    ***
#>    372:              *tb8            *tb8            *tb8    ***
#>    386:            *>Link          *>Link          *>Link    ***
#>    439:               *>A             *>A             *>A    ***
#>    492:              *tb4            *tb4            *tb4    ***
#>    500:              *tb8            *tb8            *tb8    ***
#>    514:             *tb1.           *tb1.           *tb1.    ***
#>    517:              *tb8            *tb8            *tb8    ***
#>    557:             *tb1.           *tb1.           *tb1.    ***
#>    584:              *tb8            *tb8            *tb8    ***
#>    676:             *tb1.           *tb1.           *tb1.    ***
#>    679:              *tb8            *tb8            *tb8    ***
#>    719:             *tb1.           *tb1.           *tb1.    ***
#>    722:              *tb8            *tb8            *tb8    ***
#>    866:             *tb1.           *tb1.           *tb1.    ***
#>    870:                *-              *-              *-    ***
#> ########## ^^^ CarlPerkins_BlueSuedeShoes.hum ^^^ ###########
#>     (***five spines/paths not displayed due to screen size***)
#> 
#>  humdrumR corpus of three pieces.
#> 
#>    Data fields: 
#>          *Token :: character
#> 
```
