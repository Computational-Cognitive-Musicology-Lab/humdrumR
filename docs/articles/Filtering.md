# Filtering humdrum data

Welcome to “Filtering humdrum data”! This article explains how
humdrum$_{\mathbb{R}}$ can be used find subsets of humdrum data. Most
humdrum datasets contain a wealth of information: rhythms, pitches,
lyrics, etc. However, the particular analysis you want to conduct today
might not need all that information. The first step after [loading
humdrum
data](https://humdrumR.ccml.gtcmt.gatech.edu/articles/ReadWrite.md "Reading and writing humdrum data")
is usually to “filter out” the information you don’t need. (Of course,
you are not erasing it from the original files, which are safe on your
computer!) This is often just one part of larger task of [preparing your
data](https://humdrumR.ccml.gtcmt.gatech.edu/articles/Reshaping.md "Shaping humdrum data").
You don’t necessarily *need* to do this, but usually it makes things
simpler when you simply get anything you don’t need out of the way.

In this article, we’ll use our pre-packaged Bach chorale data to
illustrate how filtering works, so let’s load it up:

``` r
chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
```

## Indexing

The simplest, common, way to filter your data is through “indexing,” a
common means in programming for selecting subsets of data. In R, we use
the square brackets, `[]`, to index various data objects, including
basic atomic vectors, data.tables, and lists. For example, using
brackets I can extract subsets of a vector:

``` r
myname <- c('N', 'A', 'T', ' ', 'C', 'O', 'N', 'D', 'I', 'T')

myname[1:3]
>    [1] "N" "A" "T"

myname[5:10]
>    [1] "C" "O" "N" "D" "I" "T"

myname[c(1, 5)]
>    [1] "N" "C"
```

Watch out! R actually has **two** different ways of indexing: you can
use a single pair of matches brackets (`[ ]`) or a double pair
(`[[ ]]`). If you want a primer on how these two types of indexing are
used in normal R objects, check out the [R
primer](https://humdrumR.ccml.gtcmt.gatech.edu/articles/RPrimer.md "Basics of R")—you
really don’t *need* to do this before getting into
humdrum$_{\mathbb{R}}$, but you might want to eventually.

We can use the single-bracket `[]` or double-bracket `[[]]` commands to
index our humdrum$_{\mathbb{R}}$ data objects.

- The single-brackets are used to index whole *pieces* in your data.
- The double-brackets are used to index *within* pieces in your data.

Either type of bracket can accept either `numeric` or `character`
vectors to index by.

------------------------------------------------------------------------

To do indexing in a pipe, you can (since R 4.3.0) use the `_`
placeholder. For example, the commands above are equivalent to:

``` r
myname |> _[1:3]
>    [1] "N" "A" "T"

myname |> _[5:10]
>    [1] "C" "O" "N" "D" "I" "T"

myname |> _[c(1,5)]
>    [1] "N" "C"
```

### Single-bracket indexing

The single-brackets are used to index whole *pieces* in your data. So if
you have a dataset of 19 files, but you only want to look at some of
those files, you’d use the single-brackets `[]`.

#### Numeric indices (for single-brackets)

If we give a `numeric` value `i` to a single-bracket index command, that
number will select the `i`th file in the dataset. For example, if we
want to look only at the fifth chorale, we can call:

``` r

chorales[5]
```

```
>    ######################## vvv chor005.krn vvv #########################
>        1:  !!!COM: Bach, Johann Sebastian
>        2:  !!!CDT: 1685/02/21/-1750/07/28/
>        3:  !!!OTL@@DE: An Wasserfl&uuml;ssen Babylon
>        4:  !!!SCT: BWV 267
>        5:  !!!PC#: 5
>        6:  !!!AGN: chorale
>        7:           **kern         **kern         **kern         **kern
>        8:           *ICvox         *ICvox         *ICvox         *ICvox
>        9:           *Ibass        *Itenor         *Ialto        *Isoprn
>       10:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>       11:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>       12:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>       13:              *>A            *>A            *>A            *>A
>       14:          *clefF4       *clefGv2        *clefG2        *clefG2
>       15:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>       16:              *G:            *G:            *G:            *G:
>       17:            *M4/4          *M4/4          *M4/4          *M4/4
>       18:          *met(c)        *met(c)        *met(c)        *met(c)
>       19:           *MM100         *MM100         *MM100         *MM100
>       20:               4G             4B             4g            4dd
>       21:               =1             =1             =1             =1
>       22:               4C            8cL             4g            4ee
>       23:                .            8BJ              .              .
>       24:               4D             4A            4f#           8ddL
>       25:                .              .              .           8ccJ
>       26:               4E             4e             4g            8bL
>       27:                .              .              .           8ccJ
>       28:             8F#L            8AL            4f#            4dd
>       29:              8GJ            8BJ              .              .
>       30:               =2             =2             =2             =2
>       31:               4A            8cL            8eL           8ccL
>       32:                .            8eJ            8gJ            8bJ
>       33:               4D             4d            8gL            4cc
>       34:                .              .           8f#J              .
>       35:              4G;            4d;            4g;            4b;
>       36:              4F#             4d             4a             4a
>       37:               =3             =3             =3             =3
>       38:              8GL             4d             4g             4b
>       39:             8F#J              .              .              .
>       40:               4E            8GL             4g            4cc
>       41:                .            8AJ              .              .
>       42:              4BB            8BL             4g            4dd
>       43:                .            8GJ              .              .
>       44:               4C             4e             4g           8ccL
>       45:                .              .              .            8bJ
>       46:               =4             =4             =4             =4
>       47:               2D            8AL             4g            8aL
>       48:                .            8BJ              .            8gJ
>       49:                .             4c            4f#             4a
>       50:             4GG;            4B;            4d;            4g;
>       51:             =:|!           =:|!           =:|!           =:|!
>       52:              *>B            *>B            *>B            *>B
>       53:               4G             4B             4d             4g
>       54:               =5             =5             =5             =5
>       55:              4F#             4A            8dL             4a
>       56:                .              .            8eJ              .
>       57:              8EL             4d            4f#             4b
>       58:              8DJ              .              .              .
>       59:               4E             4G             4e           8ccL
>       60:                .              .              .            8bJ
>       61:              4F#             4A            8eL             4a
>       62:                .              .            8dJ              .
>       63:               =6             =6             =6             =6
>       64:               4G            8AL             4d             4b
>       65:                .            8GJ              .              .
>       66:               4D            8GL             4c             4a
>       67:                .           8F#J              .              .
>       68:             4GG;            4G;            4B;            4g;
>       69:               4C             4G             4e            8gL
>       70:                .              .              .            8aJ
>       71:               =7             =7             =7             =7
>       72:              8GL             4G             4d             4b
>       73:              8FJ              .              .              .
>       74:              8EL             4G             4e            4cc
>       75:              8DJ              .              .              .
>       76:              8CL             4G             4f            4dd
>       77:             8BBJ              .              .              .
>       78:              8CL             4G            8eL            4ee
>       79:              8DJ              .           8f#J              .
>       80:               =8             =8             =8             =8
>       81:               4E            8GL            8gL             4b
>       82:                .           16AL           8f#J              .
>       83:                .          16BJJ              .              .
>       84:              4AA             4A            8gL           4cc#
>       85:                .              .            8aJ              .
>       86:              4D;            4A;           4f#;           4dd;
>       87:              8GL             4G             4d            8bL
>       88:              8AJ              .              .           8ccJ
>       89:               =9             =9             =9             =9
>       90:               4B            8GL             4g            4dd
>       91:                .            8dJ              .              .
>       92:               4c            8cL             4g            4ee
>       93:                .            8BJ              .              .
>       94:              4F#             4A             4a           8ddL
>       95:                .              .              .           8ccJ
>       96:              4G#             4e             4b             4b
>       97:              =10            =10            =10            =10
>       98:               4A             2e            8bL            2cc
>       99:                .              .           8g#J              .
>      100:              4AA              .             4a              .
>      101:              4E;            4e;           4g#;            4b;
>      102:             4GG#            8eL             4b            4ee
>      103:                .            8dJ              .              .
>      104:              =11            =11            =11            =11
>      105:             8AAL            8cL             4a            4ee
>      106:             8BBJ            8BJ              .              .
>      107:              8CL            [4A            8eL            4ee
>      108:             8AAJ              .            8cJ              .
>      109:               4F           8AL]            8dL             4a
>      110:                .           8G#J            8eJ              .
>      111:              8EL            8AL             4f            4dd
>      112:              8DJ            8BJ              .              .
>      113:              =12            =12            =12            =12
>      114:               4E            8G#             4e            4cc
>      115:                .             4A              .              .
>      116:              4EE              .             4d             4b
>      117:                .            8G#              .              .
>      118:             4AA;            4A;            4c;            4a;
>      119:              4AA            8EL             4a            4cc
>      120:                .          8F#XJ              .              .
>      121:              =13            =13            =13            =13
>      122:             8BBL           4GnX            8dL             4b
>      123:             8C#J              .            8eJ              .
>      124:               2D            8dL             4f             4a
>      125:                .            8cJ              .              .
>      126:                .            4B-            8eL             4g
>      127:                .              .            8dJ              .
>      128:              4C#             4A            [4e            8aL
>      129:                .              .              .            8gJ
>      130:              =14            =14            =14            =14
>      131:               4D             4A           8eL]             4f
>      132:                .              .            8dJ              .
>      133:              4AA             4G            8dL             4e
>      134:                .              .           8c#J              .
>      135:              4D;          4F#X;            4d;            4d;
>      136:              4BB            4F#            8dL             4d
>      137:                .              .            8cJ              .
>      138:              =15            =15            =15            =15
>      139:               4E             4G            8BL             4g
>      140:                .              .            8AJ              .
>      141:               4D             4d            8GL             4a
>      142:                .              .           8F#J              .
>      143:              8GL             4d             4G            4.b
>      144:             8F#J              .              .              .
>      145:               4E             4e             4g              .
>      146:                .              .              .            8cc
>      147:              =16            =16            =16            =16
>      148:             8BBL             2d            4f#            4dd
>      149:             8AAJ              .              .              .
>      150:              4GG              .             4g           8ccL
>      151:                .              .              .            8bJ
>      152:              8CL            8eL             4g             4a
>      153:             8BBJ            8dJ              .              .
>      154:              4AA             4c            [4g            8bL
>      155:                .              .              .           8ccJ
>      156:              =17            =17            =17            =17
>      157:              8DL            8dL            4g]             2a
>      158:              8CJ            8eJ              .              .
>      159:               4D            8dL            4f#              .
>      160:                .            8cJ              .              .
>      161:             4GG;            4B;            4d;            4g;
>      162:               ==             ==             ==             ==
>      163:               *-             *-             *-             *-
>      164:  !!!hum2abc: -Q ''
>      165:  !!!title: @{PC#}. @{OTL@@DE}
>      166:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>      167:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>      168:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>      169:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>      170:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      171:  !!!EED:  Craig Stuart Sapp
>      172:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor005.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       Data fields: 
>               *Token :: character
```

We can also give the command a vector of `i` numbers:

``` r

chorales[c(1,3,5)]
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               23:               4E            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4G             4G             2d            4.b
>               28:               4D            4F#              .              .
>               29:                .              .              .             8a
>               30:               4E             4G             4B             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (one other piece...)
>    
>    ######################## vvv chor005.krn vvv #########################
>     1-142::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>              143:              8GL             4d             4G            4.b
>              144:             8F#J              .              .              .
>              145:               4E             4e             4g              .
>              146:                .              .              .            8cc
>              147:              =16            =16            =16            =16
>              148:             8BBL             2d            4f#            4dd
>              149:             8AAJ              .              .              .
>              150:              4GG              .             4g           8ccL
>              151:                .              .              .            8bJ
>              152:              8CL            8eL             4g             4a
>              153:             8BBJ            8dJ              .              .
>              154:              4AA             4c            [4g            8bL
>              155:                .              .              .           8ccJ
>              156:              =17            =17            =17            =17
>              157:              8DL            8dL            4g]             2a
>              158:              8CJ            8eJ              .              .
>              159:               4D            8dL            4f#              .
>              160:                .            8cJ              .              .
>              161:             4GG;            4B;            4d;            4g;
>              162:               ==             ==             ==             ==
>              163:               *-             *-             *-             *-
>              164:  !!!hum2abc: -Q ''
>              165:  !!!title: @{PC#}. @{OTL@@DE}
>              166:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>              167:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>              168:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>              169:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>              170:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>              171:  !!!EED:  Craig Stuart Sapp
>              172:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor005.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of three pieces.
>    
>       Data fields: 
>               *Token :: character
```

You might want to use the R sequence command, `:`, to select a range of
numbers:

``` r

chorales[6:10]
```

```
>    ######################## vvv chor006.krn vvv #########################
>               1:  !!!COM: Bach, Johann Sebastian
>               2:  !!!CDT: 1685/02/21/-1750/07/28/
>               3:  !!!OTL@@DE: Christus, der ist mein Leben
>               4:  !!!SCT: BWV 281
>               5:  !!!PC#: 6
>               6:  !!!AGN: chorale
>               7:          **kern         **kern        **kern           **kern
>               8:          *ICvox         *ICvox        *ICvox           *ICvox
>               9:          *Ibass        *Itenor        *Ialto          *Isoprn
>              10:         *I"Bass       *I"Tenor       *I"Alto       *I"Soprano
>              11:         *clefF4       *clefGv2       *clefG2          *clefG2
>              12:          *k[b-]         *k[b-]        *k[b-]           *k[b-]
>              13:             *F:            *F:           *F:              *F:
>              14:           *M4/4          *M4/4         *M4/4            *M4/4
>              15:         *met(c)        *met(c)       *met(c)          *met(c)
>              16:          *MM100         *MM100        *MM100           *MM100
>              17:             4FF             4A            4c               4f
>              18:              =1             =1            =1               =1
>              19:              4F             4c            4f               4a
>              20:              4E             4c            4g               4g
>              21:             4E-             4c            4f               4a
>              22:              4D             4d            4f              4b-
>              23:              =2             =2            =2               =2
>              24:             8CL             4G           8eL              2cc
>              25:              8D              .            8f                .
>              26:              8E             4c            8g                .
>              27:             8CJ              .           8eJ                .
>              28:             4F;            4c;           4f;              4a;
>              29:            4BB-            4B-            4f              4dd
>              30:              =3             =3            =3               =3
>    31-77:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor006.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>     1-70:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>              71:              4D            8F#            4d               4b
>              72:               .             4G             .                .
>              73:              4D              .            4c               4a
>              74:               .            8F#             .                .
>              75:            2GG;            2G;           2B;              2g;
>              76:             =11            =11           =11              =11
>              77:              2C             2G            2e               2g
>              78:             4AA             4A            4e              4cc
>              79:              4E            4G#           8eL               4b
>              80:               .              .           8dJ                .
>              81:             =12            =12           =12              =12
>              82:              4F             4A            4c               4a
>              83:              4C             4G            4c               4e
>              84:            4BB-             4G           [2d               4g
>              85:             4AA             4A             .               4f
>              86:             =13            =13           =13              =13
>              87:            4GG#             4B           4d]              1e;
>              88:             4AA             4A            4c                .
>              89:            2EE;          2G#X;           2B;                .
>              90:              ==             ==            ==               ==
>              91:              *-             *-            *-               *-
>              92:  !!!hum2abc: -Q ''
>              93:  !!!title: @{PC#}. @{OTL@@DE}
>              94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>              95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>              96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>              97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>              98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>              99:  !!!EED:  Craig Stuart Sapp
>             100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

#### Character indices (for single-brackets)

If you supply a humdrum$_{\mathbb{R}}$ object single-bracket index a
`character` string, humdrum$_{\mathbb{R}}$ will treat that string as a
regular expression and return all the files that contain a match to that
expression in *any* data token—even if there is only one.

For example, we might be only interested in files that use flat notes.
Since `**kern` represents flats with “`-`”, we can simply write:

``` r

chorales['-']
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:           **kern         **kern         **kern         **kern
>               10:           *ICvox         *ICvox         *ICvox         *ICvox
>               11:           *Ibass        *Itenor         *Ialto        *Isoprn
>               12:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               13:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               14:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               15:              *>A            *>A            *>A            *>A
>               16:          *clefF4       *clefGv2        *clefG2        *clefG2
>               17:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               18:              *a:            *a:            *a:            *a:
>               19:          !dorian        !dorian        !dorian        !dorian
>               20:            *M4/4          *M4/4          *M4/4          *M4/4
>               21:          *met(c)        *met(c)        *met(c)        *met(c)
>               22:           *MM100         *MM100         *MM100         *MM100
>               23:               4E             4e            4g#             4b
>               24:               =1             =1             =1             =1
>               25:               4A             4e             4a            4cc
>               26:               4B             4d            4g#             4b
>               27:               4c             4e             4a             4a
>               28:              8BL            8dL           8g#L            4ee
>               29:              8AJ            8cJ            8aJ              .
>               30:               =2             =2             =2             =2
>    31-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4D              .             4c             4a
>               74:                .            8F#              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:               2C             2G             2e             2g
>               78:              4AA             4A             4e            4cc
>               79:               4E            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               83:               4C             4G             4c             4e
>               84:             4BB-             4G            [2d             4g
>               85:              4AA             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:             4GG#             4B            4d]            1e;
>               88:              4AA             4A             4c              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

Look at that, five of our ten chorales contain *at least one* flat. The
other chorales have zero flats. Notice that we still get the *whole*
files returned to us!

### Double-bracket indexing

The double-brackets are used to index *within* pieces in your data.
Specifically, if you only want to work with certain spines or records
within your pieces. The double-brackets apply the same filters
separately to each file in the dataset.

In double-bracket indexing, you can provide two separate arguments,
either individually or together:

- `i` is the first argument which will index records *within* each file.
- `j` is the second argument, which will index spines *within* each
  file.

If you want to use the `j` (spine) argument by itself, you should put a
comma before it, to indicate that you are skipping over `i`. It is also
good practice to by clear and put a comma after `i`, but this is not
actually needed. Basically, commands should look like this:

- `chorales[[i, ]]` (records)
- `chorales[[ , j]]` (spines)
- `chorales[[i , j]]` (records *and* spines)

#### Numeric indices (for double-brackets)

`Numeric` values `i` *or* `j` can be given to double-bracket
humdrum$_{\mathbb{R}}$ index commands. For `i`, the number is simply
matched to record numbers in each file. For example, you could extract
the first fifty records from each file as so:

``` r

chorales[[1:50, ]]
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               23:               4E            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4G             4G             2d            4.b
>               28:               4D            4F#              .              .
>               29:                .              .              .             8a
>               30:               4E             4G             4B             4g
>    31-124::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-21::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               22:               4C             4A             4e             4e
>               23:              4BB             4d            4g#             4b
>               24:               =2             =2             =2             =2
>               25:              4AA             4e             4a            4cc
>               26:              4BB             4d          8gnXL            8bL
>               27:                .              .           8f#J            8aJ
>               28:              8CL            8eL             4e             4g
>               29:             8BBJ            8dJ              .              .
>               30:              4AA             4c            4f#             4a
>               31:               =3             =3             =3             =3
>               32:              2E;            2B;           2g#;            2b;
>               33:               2E             2e            2g#             2b
>               34:               =4             =4             =4             =4
>               35:               4A             4e             4a            4cc
>               36:              8GL             4f             4b            4dd
>               37:              8FJ              .              .              .
>               38:               4E             4g            4cc           8ccL
>               39:                .              .              .            8bJ
>               40:               4F             4c             4f             4a
>               41:               =5             =5             =5             =5
>               42:               4C            8cL             4e             4g
>               43:                .            8BJ              .              .
>               44:               4D             4A            8dL             4f
>               45:                .              .            8cJ              .
>               46:              2E;           2G#;            2B;            2e;
>               47:            =6:|!          =6:|!          =6:|!          =6:|!
>               48:              *>B            *>B            *>B            *>B
>               49:               2c             2A             2e             2a
>               50:               4B             4B             4d           4gnX
>               91:               *-             *-             *-             *-
>    ######################## ^^^ chor010.krn ^^^ #########################
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

For `j`, the number is matched to the spines in each file (from left to
right). So if you wanted the second spine in each file, you’d write:

``` r
chorales[[ , 2]]
```

```
>    #### vvv chor001.krn vvv ####
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern
>                9:                *ICvox
>               10:               *Itenor
>               11:              *I"Tenor
>               12:             *>[A,A,B]
>               13:          *>norep[A,B]
>               14:                   *>A
>               15:              *clefGv2
>               16:                *k[f#]
>               17:                   *G:
>               18:                 *M3/4
>               19:                *MM100
>               20:                    4B
>               21:                    =1
>               22:                    4B
>               23:                   8cL
>               24:                   8BJ
>               25:                    4A
>               26:                    =2
>               27:                    4G
>               28:                   4F#
>               29:                     .
>               30:                    4G
>    31-133:::::::::::::::::::::::
>    #### ^^^ chor001.krn ^^^ ####
>    
>           (eight more pieces...)
>    
>    #### vvv chor010.krn vvv ####
>      1-70:::::::::::::::::::::::
>               71:                   8F#
>               72:                    4G
>               73:                     .
>               74:                   8F#
>               75:                   2G;
>               76:                   =11
>               77:                    2G
>               78:                    4A
>               79:                   4G#
>               80:                     .
>               81:                   =12
>               82:                    4A
>               83:                    4G
>               84:                    4G
>               85:                    4A
>               86:                   =13
>               87:                    4B
>               88:                    4A
>               89:                 2G#X;
>               90:                    ==
>               91:                    *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    #### ^^^ chor010.krn ^^^ ####
>    (***four global comments truncated
>            due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

If you give indices that are larger than the number of records or spines
in a file, that file will be dropped. For example, if we call

``` r

chorales[[150:200, ]]
```

```
>    ######################## vvv chor005.krn vvv #########################
>        7:           **kern         **kern         **kern         **kern
>       11:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>       12:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>       13:              *>A            *>A            *>A            *>A
>       52:              *>B            *>B            *>B            *>B
>      150:              4GG              .             4g           8ccL
>      151:                .              .              .            8bJ
>      152:              8CL            8eL             4g             4a
>      153:             8BBJ            8dJ              .              .
>      154:              4AA             4c            [4g            8bL
>      155:                .              .              .           8ccJ
>      156:              =17            =17            =17            =17
>      157:              8DL            8dL            4g]             2a
>      158:              8CJ            8eJ              .              .
>      159:               4D            8dL            4f#              .
>      160:                .            8cJ              .              .
>      161:             4GG;            4B;            4d;            4g;
>      162:               ==             ==             ==             ==
>      163:               *-             *-             *-             *-
>      164:  !!!hum2abc: -Q ''
>      165:  !!!title: @{PC#}. @{OTL@@DE}
>      166:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>      167:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>      168:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>      169:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>      170:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      171:  !!!EED:  Craig Stuart Sapp
>      172:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor005.krn ^^^ #########################
>    
>           (one other piece...)
>    
>    ######################## vvv chor008.krn vvv #########################
>    
>        7:           **kern         **kern         **kern         **kern
>      150:               8F             4f           8a-L            4cc
>      151:              8GJ              .           8b-J              .
>      152:             8A-L             4c            4cc            4ff
>      153:               8G              .              .              .
>      154:              8A-             4F           8b-L            4ff
>      155:             8B-J              .           8a-J              .
>      156:              =20            =20            =20            =20
>      157:               4c             4c             4g            4ee
>      158:               4C             4c             4g            4ee
>      159:               4F             4c             4a            4ff
>      160:              4F;            4c;            4a;           4ff;
>      161:               ==             ==             ==             ==
>      162:               *-             *-             *-             *-
>      163:  !!!hum2abc: -Q ''
>      164:  !!!title: @{PC#}. @{OTL@@DE}
>      165:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>      166:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>      167:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>      168:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>      169:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      170:  !!!EED:  Craig Stuart Sapp
>      171:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor008.krn ^^^ #########################
>                 (***eight global comments truncated due to screen size***)
>    
>       humdrumR corpus of three pieces.
>    
>       Data fields: 
>               *Token :: character
```

We only get three files back, because the other seven files don’t have
any records above 150! (Notice that humdrum$_{\mathbb{R}}$ won’t remove
the Exclusive interpretation or spine spine closing (`*-`) records…since
that would break the humdrum syntax. However, if you only query for the
exclusive interpretation or reference record rows, the output will be
empty. For example, `chorales[[1, ]]` returns empty humdrumR object
because the first row in all chorales files are reference record, not
kern data.)

#### Character indices (for double-brackets)

`Character` string values `i` *or* `j` can also be given to
double-bracket humdrum$_{\mathbb{R}}$ index commands.
Humdrum$_{\mathbb{R}}$ will treat string as a regular expression and
return all records (`i`) or spines (`j`) that any match to that
expression in a data token—even if there is only one. For example, let’s
(again) say we are interesting in studying flats. Since `**kern`
represents flats as “`-`”, we can find records that contain a flat by
calling:

``` r

chorales[['-', ]]
```

```
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:          **kern        **kern        **kern        **kern
>               10:          *ICvox        *ICvox        *ICvox        *ICvox
>               11:          *Ibass       *Itenor        *Ialto       *Isoprn
>               12:         *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
>               13:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]
>               14:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]
>               15:             *>A           *>A           *>A           *>A
>               16:         *clefF4      *clefGv2       *clefG2       *clefG2
>               17:          *k[f#]        *k[f#]        *k[f#]        *k[f#]
>               18:             *a:           *a:           *a:           *a:
>               19:         !dorian       !dorian       !dorian       !dorian
>               20:           *M4/4         *M4/4         *M4/4         *M4/4
>               21:         *met(c)       *met(c)       *met(c)       *met(c)
>               22:          *MM100        *MM100        *MM100        *MM100
>            23-53:        =====1-4      =====1-4      =====1-4      =====1-4
>               54:             *>B           *>B           *>B           *>B
>            55-91:        =====5-9      =====5-9      =====5-9      =====5-9
>               92:             8DL           4B-            4g           4ee
>               93:               .             .             .             .
>               94:             =10           =10           =10           =10
>               95:               .             .             .             .
>               96:               .             .             .             .
>    97-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-36::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               37:               .             .             .             .
>               38:               .             .             .             .
>               39:               .             .             .             .
>               40:               .             .             .             .
>               41:              =5            =5            =5            =5
>               42:               .             .             .             .
>               43:               .             .             .             .
>               44:               .             .             .             .
>               45:               .             .             .             .
>               46:               .             .             .             .
>               47:           =6:|!         =6:|!         =6:|!         =6:|!
>               48:             *>B           *>B           *>B           *>B
>            49-83:      ======7-12    ======7-12    ======7-12    ======7-12
>               84:            4BB-            4G           [2d            4g
>               85:               .             .             .             .
>               86:             =13           =13           =13           =13
>               87:               .             .             .             .
>               88:               .             .             .             .
>               89:               .             .             .             .
>               90:              ==            ==            ==            ==
>               91:              *-            *-            *-            *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastia***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und ***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 3***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., ***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***five global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

As with our single-bracket search for flats (previous section) we only
get five files back, because only five of the chorales contain *any*
flats at all. However, we now see that all records that don’t contain a
flat are completely stripped away, leaving a (handful) of records with
at least one flat.

If we do the same thing with `j` (spines)

``` r
chorales[[ , '-']]
```

```
>    #### vvv chor003.krn vvv ####
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:                **kern
>               10:                *ICvox
>               11:               *Itenor
>               12:              *I"Tenor
>               13:             *>[A,A,B]
>               14:          *>norep[A,B]
>               15:                   *>A
>               16:              *clefGv2
>               17:                *k[f#]
>               18:                   *a:
>               19:               !dorian
>               20:                 *M4/4
>               21:               *met(c)
>               22:                *MM100
>               23:                    4e
>               24:                    =1
>               25:                    4e
>               26:                    4d
>               27:                    4e
>               28:                   8dL
>               29:                   8cJ
>               30:                    =2
>    31-110:::::::::::::::::::::::
>    #### ^^^ chor003.krn ^^^ ####
>    
>           (three more pieces...)
>    
>    #### vvv chor010.krn vvv ####
>      1-70:::::::::::::::::::::::
>               71:                    4D
>               72:                     .
>               73:                    4D
>               74:                     .
>               75:                  2GG;
>               76:                   =11
>               77:                    2C
>               78:                   4AA
>               79:                    4E
>               80:                     .
>               81:                   =12
>               82:                    4F
>               83:                    4C
>               84:                  4BB-
>               85:                   4AA
>               86:                   =13
>               87:                  4GG#
>               88:                   4AA
>               89:                  2EE;
>               90:                    ==
>               91:                    *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    #### ^^^ chor010.krn ^^^ ####
>    (***four global comments truncated
>            due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

It looks like we get only one spine in each of our five files. Those
single spines are simply which ever spine in *each* file contained *any*
flat. But a couple things to notice! In the first file, what is now
spine `1` was the old spine `2`, which we can tell because that
instrument interpretation is `*Itenor`. However, if you look at the last
of the five files

``` r

chorales[[ , '-']][5]
```

```
>    #### vvv chor010.krn vvv ####
>       1:  !!!COM: Bach, Johann Sebastian
>       2:  !!!CDT: 1685/02/21/-1750/07/28/
>       3:  !!!OTL@@DE: Aus tiefer Not schrei ich zu dir
>       4:  !!!SCT: BWV 38/6
>       5:  !!!PC#: 10
>       6:  !!!AGN: chorale
>       7:                **kern
>       8:                *ICvox
>       9:                *Ibass
>      10:               *I"Bass
>      11:             *>[A,A,B]
>      12:          *>norep[A,B]
>      13:                   *>A
>      14:               *clefF4
>      15:                  *k[]
>      16:                   *a:
>      17:                 *M4/4
>      18:               *met(c)
>      19:                *MM100
>      20:                   =1-
>      21:                    2D
>      22:                    4C
>      23:                   4BB
>      24:                    =2
>      25:                   4AA
>      26:                   4BB
>      27:                     .
>      28:                   8CL
>      29:                  8BBJ
>      30:                   4AA
>      31:                    =3
>      32:                   2E;
>      33:                    2E
>      34:                    =4
>      35:                    4A
>      36:                   8GL
>      37:                   8FJ
>      38:                    4E
>      39:                     .
>      40:                    4F
>      41:                    =5
>      42:                    4C
>      43:                     .
>      44:                    4D
>      45:                     .
>      46:                   2E;
>      47:                 =6:|!
>      48:                   *>B
>      49:                    2c
>      50:                    4B
>      51:                    4A
>      52:                     .
>      53:                    =7
>      54:                    4G
>      55:                 8FnXL
>      56:                   8EJ
>      57:                    4D
>      58:                     .
>      59:                    4E
>      60:                    =8
>      61:                  2AA;
>      62:                    2A
>      63:                    =9
>      64:                    4E
>      65:                   8DL
>      66:                   8CJ
>      67:                   4BB
>      68:                     .
>      69:                    4C
>      70:                   =10
>      71:                    4D
>      72:                     .
>      73:                    4D
>      74:                     .
>      75:                  2GG;
>      76:                   =11
>      77:                    2C
>      78:                   4AA
>      79:                    4E
>      80:                     .
>      81:                   =12
>      82:                    4F
>      83:                    4C
>      84:                  4BB-
>      85:                   4AA
>      86:                   =13
>      87:                  4GG#
>      88:                   4AA
>      89:                  2EE;
>      90:                    ==
>      91:                    *-
>      92:  !!!hum2abc: -Q ''
>      93:  !!!title: @{PC#}. @{OTL@@DE}
>      94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>      95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>      96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>      97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>      98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      99:  !!!EED:  Craig Stuart Sapp
>     100:  !!!EEV:  2009/05/22
>    #### ^^^ chor010.krn ^^^ ####
>    (***four global comments truncated
>            due to screen size***)
>    
>       Data fields: 
>               *Token :: character
```

we see `*Ibass`—so the new spine `1` was actually the original spine `1`
in that file. And wait, there’s more! If we dig in and look at the other
files returned to us (which aren’t shown by default),

``` r
chorales[[ , '-']][3]
```

```
>    ######################## vvv chor006.krn vvv #########################
>       1:  !!!COM: Bach, Johann Sebastian
>       2:  !!!CDT: 1685/02/21/-1750/07/28/
>       3:  !!!OTL@@DE: Christus, der ist mein Leben
>       4:  !!!SCT: BWV 281
>       5:  !!!PC#: 6
>       6:  !!!AGN: chorale
>       7:          **kern         **kern        **kern           **kern
>       8:          *ICvox         *ICvox        *ICvox           *ICvox
>       9:          *Ibass        *Itenor        *Ialto          *Isoprn
>      10:         *I"Bass       *I"Tenor       *I"Alto       *I"Soprano
>      11:         *clefF4       *clefGv2       *clefG2          *clefG2
>      12:          *k[b-]         *k[b-]        *k[b-]           *k[b-]
>      13:             *F:            *F:           *F:              *F:
>      14:           *M4/4          *M4/4         *M4/4            *M4/4
>      15:         *met(c)        *met(c)       *met(c)          *met(c)
>      16:          *MM100         *MM100        *MM100           *MM100
>      17:             4FF             4A            4c               4f
>      18:              =1             =1            =1               =1
>      19:              4F             4c            4f               4a
>      20:              4E             4c            4g               4g
>      21:             4E-             4c            4f               4a
>      22:              4D             4d            4f              4b-
>      23:              =2             =2            =2               =2
>      24:             8CL             4G           8eL              2cc
>      25:              8D              .            8f                .
>      26:              8E             4c            8g                .
>      27:             8CJ              .           8eJ                .
>      28:             4F;            4c;           4f;              4a;
>      29:            4BB-            4B-            4f              4dd
>      30:              =3             =3            =3               =3
>      31:             4AA             4c           4.f              4cc
>      32:             4GG            8dL             .              4b-
>      33:               .           8B-J            8e                .
>      34:            8FFL             4c           4.f               4a
>      35:            8AAJ              .             .                .
>      36:              4C             4c             .               4g
>      37:               .              .            8e                .
>      38:              =4             =4            =4               =4
>      39:            2FF;            2c;           2f;              2a;
>      40:              4r            4ry           4ry               4r
>      41:              4A             4f           [4a              4cc
>      42:              =5             =5            =5               =5
>      43:             4BB             4f          8aL]              4dd
>      44:               .              .          [8gJ                .
>      45:              4C            [4e          8gL]              4ee
>      46:               .              .        16ccLL                .
>      47:               .              .       16b-XJJ                .
>      48:             8DL           8eL]            4a              4ff
>      49:             8FJ            8dJ             .                .
>      50:             8AL            [4c            4a              4ee
>      51:             8GJ              .             .                .
>      52:              =6             =6            =6               =6
>      53:             8FL            4c]            4a              2dd
>      54:             8DJ              .             .                .
>      55:              4G             4B            4g                .
>      56:             4C;            4e;           4g;             4cc;
>      57:             4FF             4c           [4f               4a
>      58:              =7             =7            =7               =7
>      59:             4GG           4B-X          8fL]              4b-
>      60:               .              .           8eJ                .
>      61:             4AA             4c            4f               4a
>      62:            4BB-             4d            4f               4g
>      63:              4C           8.cL            4e               4g
>      64:               .         16B-Jk             .                .
>      65:              =8             =8            =8               =8
>      66:           2.FF;           2.A;          2.c;             2.f;
>      67:              ==             ==            ==               ==
>      68:              *-             *-            *-               *-
>      69:  !!!hum2abc: -Q ''
>      70:  !!!title: @{PC#}. @{OTL@@DE}
>      71:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>      72:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>      73:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>      74:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>      75:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      76:  !!!EED:  Craig Stuart Sapp
>      77:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor006.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       Data fields: 
>               *Token :: character
```

we see that what is now the third file actually had flats in all four
spines, and so all four spines are still there!

------------------------------------------------------------------------

In some cases you may find the renumbering of spines on a *per file*
basis confusing, and you want to keep track of the original spine
numbers. Fortunately, for you there is an option that might helpful,
which you will learn more about below: try specifying `drop = FALSE`.
(The `drop` argument is less intuitive, but is the standard R argument
for this sort of thing.)

``` r
chorales[[ , '-', drop = FALSE]]
```

```
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:           **kern         **kern         **kern         **kern
>               10:           *ICvox         *ICvox         *ICvox         *ICvox
>               11:           *Ibass        *Itenor         *Ialto        *Isoprn
>               12:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               13:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               14:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               15:              *>A            *>A            *>A            *>A
>               16:          *clefF4       *clefGv2        *clefG2        *clefG2
>               17:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               18:              *a:            *a:            *a:            *a:
>               19:          !dorian        !dorian        !dorian        !dorian
>               20:            *M4/4          *M4/4          *M4/4          *M4/4
>               21:          *met(c)        *met(c)        *met(c)        *met(c)
>               22:           *MM100         *MM100         *MM100         *MM100
>               23:                .             4e              .              .
>               24:               =1             =1             =1             =1
>               25:                .             4e              .              .
>               26:                .             4d              .              .
>               27:                .             4e              .              .
>               28:                .            8dL              .              .
>               29:                .            8cJ              .              .
>               30:               =2             =2             =2             =2
>    31-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D              .              .              .
>               72:                .              .              .              .
>               73:               4D              .              .              .
>               74:                .              .              .              .
>               75:             2GG;              .              .              .
>               76:              =11            =11            =11            =11
>               77:               2C              .              .              .
>               78:              4AA              .              .              .
>               79:               4E              .              .              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:               4F              .              .              .
>               83:               4C              .              .              .
>               84:             4BB-              .              .              .
>               85:              4AA              .              .              .
>               86:              =13            =13            =13            =13
>               87:             4GG#              .              .              .
>               88:              4AA              .              .              .
>               89:             2EE;              .              .              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

### Negative numeric indices

A nifty feature of R is that if you supply negative numbers to an
indexer, R will *remove* those numbers. This works in
humdrum$_{\mathbb{R}}$ too, so if you want all the files *except* the
first file, you could write:

``` r
chorales[-1]
```

```
>    ######################## vvv chor002.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ich dank dir, lieber Herre
>                4:  !!!SCT: BWV 347
>                5:  !!!PC#: 2
>                6:  !!!AGN: chorale
>                7:           **kern         **kern         **kern         **kern
>                8:           *ICvox         *ICvox         *ICvox         *ICvox
>                9:           *Ibass        *Itenor         *Ialto        *Isoprn
>               10:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               11:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               12:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               13:              *>A            *>A            *>A            *>A
>               14:          *clefF4       *clefGv2        *clefG2        *clefG2
>               15:       *k[f#c#g#]     *k[f#c#g#]     *k[f#c#g#]     *k[f#c#g#]
>               16:              *A:            *A:            *A:            *A:
>               17:            *M4/4          *M4/4          *M4/4          *M4/4
>               18:          *met(c)        *met(c)        *met(c)        *met(c)
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              8AL            4c#             4e             4a
>               21:             8G#J              .              .              .
>               22:               =1             =1             =1             =1
>               23:              4F#            4c#            4f#             4a
>               24:              4C#           8c#L             4e             4a
>               25:                .            8BJ              .              .
>               26:               4D            8AL            4f#             4a
>               27:                .           8G#J              .              .
>               28:              4D#            4F#            4f#             4b
>               29:               =2             =2             =2             =2
>               30:               4E            4.B             4e             4g
>    31-124::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor002.krn ^^^ #########################
>    
>           (seven more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4D              .             4c             4a
>               74:                .            8F#              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:               2C             2G             2e             2g
>               78:              4AA             4A             4e            4cc
>               79:               4E            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               83:               4C             4G             4c             4e
>               84:             4BB-             4G            [2d             4g
>               85:              4AA             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:             4GG#             4B            4d]            1e;
>               88:              4AA             4A             4c              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of nine pieces.
>    
>       Data fields: 
>               *Token :: character
```

Or if you want all the spines *except* the fourth spine, write

``` r

chorales[[ , -4]]
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern              **kern              **kern
>                9:                *ICvox              *ICvox              *ICvox
>               10:                *Ibass             *Itenor              *Ialto
>               11:               *I"Bass            *I"Tenor             *I"Alto
>               12:             *>[A,A,B]           *>[A,A,B]           *>[A,A,B]
>               13:          *>norep[A,B]        *>norep[A,B]        *>norep[A,B]
>               14:                   *>A                 *>A                 *>A
>               15:               *clefF4            *clefGv2             *clefG2
>               16:                *k[f#]              *k[f#]              *k[f#]
>               17:                   *G:                 *G:                 *G:
>               18:                 *M3/4               *M3/4               *M3/4
>               19:                *MM100              *MM100              *MM100
>               20:                   4GG                  4B                  4d
>               21:                    =1                  =1                  =1
>               22:                    4G                  4B                  4d
>               23:                    4E                 8cL                  4e
>               24:                     .                 8BJ                   .
>               25:                   4F#                  4A                  4d
>               26:                    =2                  =2                  =2
>               27:                    4G                  4G                  2d
>               28:                    4D                 4F#                   .
>               29:                     .                   .                   .
>               30:                    4E                  4G                  4B
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:                    4D                 8F#                  4d
>               72:                     .                  4G                   .
>               73:                    4D                   .                  4c
>               74:                     .                 8F#                   .
>               75:                  2GG;                 2G;                 2B;
>               76:                   =11                 =11                 =11
>               77:                    2C                  2G                  2e
>               78:                   4AA                  4A                  4e
>               79:                    4E                 4G#                 8eL
>               80:                     .                   .                 8dJ
>               81:                   =12                 =12                 =12
>               82:                    4F                  4A                  4c
>               83:                    4C                  4G                  4c
>               84:                  4BB-                  4G                 [2d
>               85:                   4AA                  4A                   .
>               86:                   =13                 =13                 =13
>               87:                  4GG#                  4B                 4d]
>               88:                   4AA                  4A                  4c
>               89:                  2EE;               2G#X;                 2B;
>               90:                    ==                  ==                  ==
>               91:                    *-                  *-                  *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

Or if you want to remove the first 20 records from each file:

``` r

chorales[[-1:-20, ]]
```

```
>    ######################## vvv chor001.krn vvv #########################
>                8:           **kern         **kern         **kern         **kern
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               23:               4E            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4G             4G             2d            4.b
>               28:               4D            4F#              .              .
>               29:                .              .              .             8a
>               30:               4E             4G             4B             4g
>               31:               =3             =3             =3             =3
>               32:               4C            8cL            8eL            4.g
>               33:                .            8BJ             8d              .
>               34:             8BBL             4c             8e              .
>               35:             8AAJ              .           8f#J             8a
>               36:              4GG             4d             4g             4b
>               37:               =4             =4             =4             =4
>               38:              2D;            2d;           2f#;            2a;
>               39:              4GG             4d             4g             4b
>               40:               =5             =5             =5             =5
>               41:             4FF#             4A             4d            2dd
>               42:              4GG             4B             4e              .
>               43:              4AA             4c            4f#            4cc
>               44:               =6             =6             =6             =6
>               45:              4BB             4d             2g             4b
>               46:               4C             4e              .             2a
>    47-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      7-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4D              .             4c             4a
>               74:                .            8F#              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:               2C             2G             2e             2g
>               78:              4AA             4A             4e            4cc
>               79:               4E            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               83:               4C             4G             4c             4e
>               84:             4BB-             4G            [2d             4g
>               85:              4AA             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:             4GG#             4B            4d]            1e;
>               88:              4AA             4A             4c              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

(Again, humdrum$_{\mathbb{R}}$ won’t remove the Exclusive interpretation
or spine spine closing (`*-`) records…since that would break the humdrum
syntax.)

## General Filtering

The indexing commands (previous sections) only get you so far. If you
want to be more precise about filtering, use the tidy-verse
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
method for humdrum$_{\mathbb{R}}$ data.
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
works exactly like
[mutate.humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/articles/DataFields.html#tidyverse-style "HumdrumR data fields article").
However, the expression(s) you give must evaluate to a logical vector
(`TRUE` or `FALSE`), or you will get an error otherwise.
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
will take your logical result and filter out data which matches `FALSE`.

We can reproduce the functionality of the `[]` and `[[]]` indexing
operators (previous section) using
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md):

``` r
chorales |>
  filter(Spine == 1)
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG              .              .              .
>               21:               =1             =1             =1             =1
>               22:               4G              .              .              .
>               23:               4E              .              .              .
>               24:                .              .              .              .
>               25:              4F#              .              .              .
>               26:               =2             =2             =2             =2
>               27:               4G              .              .              .
>               28:               4D              .              .              .
>               29:                .              .              .              .
>               30:               4E              .              .              .
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D              .              .              .
>               72:                .              .              .              .
>               73:               4D              .              .              .
>               74:                .              .              .              .
>               75:             2GG;              .              .              .
>               76:              =11            =11            =11            =11
>               77:               2C              .              .              .
>               78:              4AA              .              .              .
>               79:               4E              .              .              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:               4F              .              .              .
>               83:               4C              .              .              .
>               84:             4BB-              .              .              .
>               85:              4AA              .              .              .
>               86:              =13            =13            =13            =13
>               87:             4GG#              .              .              .
>               88:              4AA              .              .              .
>               89:             2EE;              .              .              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

But, now we can filter based on *any* arbitrary criteria we want. For
example, we could extract tokens from odd numbered spines in odd
numbered records *and* even numbered spines in even numbered records.
We’ll use Rs modulo command `%%` to separate even and odd numbers
(`odd %% 2 == 1`, `even %% 2 == 0`).

``` r

chorales |>
  filter((Record %% 2 == 0) == (Spine %% 2 == 0))
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:                .             4B              .             4g
>               21:               =1             =1             =1             =1
>               22:                .             4B              .             2g
>               23:               4E              .             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#              .             4d              .
>               26:               =2             =2             =2             =2
>               27:               4G              .             2d              .
>               28:                .            4F#              .              .
>               29:                .              .              .              .
>               30:                .             4G              .             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D              .             4d              .
>               72:                .             4G              .              .
>               73:               4D              .             4c              .
>               74:                .            8F#              .              .
>               75:             2GG;              .            2B;              .
>               76:              =11            =11            =11            =11
>               77:               2C              .             2e              .
>               78:                .             4A              .            4cc
>               79:               4E              .            8eL              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:                .             4A              .             4a
>               83:               4C              .             4c              .
>               84:                .             4G              .             4g
>               85:              4AA              .              .              .
>               86:              =13            =13            =13            =13
>               87:             4GG#              .            4d]              .
>               88:                .             4A              .              .
>               89:             2EE;              .            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

Would we ever want to do that? Probably not. However, returning to our
flats, study, lets grab *all* the flat notes:

``` r

chorales |>
  filter(Token %~% '-')
```

```
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:          **kern        **kern        **kern        **kern
>               10:          *ICvox        *ICvox        *ICvox        *ICvox
>               11:          *Ibass       *Itenor        *Ialto       *Isoprn
>               12:         *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
>               13:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]
>               14:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]
>               15:             *>A           *>A           *>A           *>A
>               16:         *clefF4      *clefGv2       *clefG2       *clefG2
>               17:          *k[f#]        *k[f#]        *k[f#]        *k[f#]
>               18:             *a:           *a:           *a:           *a:
>               19:         !dorian       !dorian       !dorian       !dorian
>               20:           *M4/4         *M4/4         *M4/4         *M4/4
>               21:         *met(c)       *met(c)       *met(c)       *met(c)
>               22:          *MM100        *MM100        *MM100        *MM100
>            23-53:        =====1-4      =====1-4      =====1-4      =====1-4
>               54:             *>B           *>B           *>B           *>B
>            55-91:        =====5-9      =====5-9      =====5-9      =====5-9
>               92:               .           4B-             .             .
>               93:               .             .             .             .
>               94:             =10           =10           =10           =10
>               95:               .             .             .             .
>               96:               .             .             .             .
>    97-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-36::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               37:               .             .             .             .
>               38:               .             .             .             .
>               39:               .             .             .             .
>               40:               .             .             .             .
>               41:              =5            =5            =5            =5
>               42:               .             .             .             .
>               43:               .             .             .             .
>               44:               .             .             .             .
>               45:               .             .             .             .
>               46:               .             .             .             .
>               47:           =6:|!         =6:|!         =6:|!         =6:|!
>               48:             *>B           *>B           *>B           *>B
>            49-83:      ======7-12    ======7-12    ======7-12    ======7-12
>               84:            4BB-             .             .             .
>               85:               .             .             .             .
>               86:             =13           =13           =13           =13
>               87:               .             .             .             .
>               88:               .             .             .             .
>               89:               .             .             .             .
>               90:              ==            ==            ==            ==
>               91:              *-            *-            *-            *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastia***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und ***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 3***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., ***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***five global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
```

Woh, we’re only seeing a couple of B flats in these files! But from
before, we know that there are a lot more flats in the fourth file
(after losing the files with no flats):

``` r

chorales |>
  filter(Token %~% '-') |>
  _[4]
```

```
>    ######################## vvv chor008.krn vvv #########################
>        1:  !!!COM: Bach, Johann Sebastian
>        2:  !!!CDT: 1685/02/21/-1750/07/28/
>        3:  !!!OTL@@DE: Freuet euch, ihr Christen alle
>        4:  !!!SCT: BWV 40/8
>        5:  !!!PC#: 8
>        6:  !!!AGN: chorale
>        7:           **kern         **kern         **kern         **kern
>        8:           *ICvox         *ICvox         *ICvox         *ICvox
>        9:           *Ibass        *Itenor         *Ialto        *Isoprn
>       10:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>       11:          *clefF4       *clefGv2        *clefG2        *clefG2
>       12:       *k[b-e-a-]     *k[b-e-a-]     *k[b-e-a-]     *k[b-e-a-]
>       13:           *f:dor         *f:dor         *f:dor         *f:dor
>       14:            *M4/4          *M4/4          *M4/4          *M4/4
>       15:          *met(c)        *met(c)        *met(c)        *met(c)
>       16:           *MM100         *MM100         *MM100         *MM100
>       17:              =1-            =1-            =1-            =1-
>       18:                .            4A-              .              .
>       19:              4E-              .              .              .
>       20:              4D-            4B-              .            4b-
>       21:                .              .              .              .
>       22:                .              .              .            4a-
>       23:               =2             =2             =2             =2
>       24:             4BB-            4d-              .              .
>       25:                .              .              .              .
>       26:                .              .              .              .
>       27:                .           8B-J              .              .
>       28:                .            4A-              .              .
>       29:                .           4A-;              .              .
>       30:               =3             =3             =3             =3
>       31:                .              .              .           4.a-
>       32:             8E-L              .              .              .
>       33:             8D-J            8B-              .            8b-
>       34:                .            4e-            4a-              .
>       35:            8BB-J              .              .              .
>       36:             4AA-            4e-            4a-              .
>       37:               =4             =4             =4             =4
>       38:              4D-              .           8a-L           4.b-
>       39:                .              .              .              .
>       40:              4E-           8e-L              .              .
>       41:                .          8d-XJ              .            8a-
>       42:            2AA-;              .           2e-;           2a-;
>       43:               =5             =5             =5             =5
>       44:              4D-            4d-              .            4a-
>       45:                .            4d-              .            4b-
>       46:            8BB-J              .              .              .
>       47:                .              .              .            4b-
>       48:              4D-              .              .            4a-
>       49:                .           8A-J              .              .
>       50:               =6             =6             =6             =6
>       51:             4BB-            4d-              .              .
>       52:                .              .              .              .
>       53:                .           8B-J              .              .
>       54:                .           2A-;              .              .
>       55:               =7             =7             =7             =7
>       56:                .              .              .              .
>       57:                .           8B-J              .              .
>       58:                .              .              .              .
>       59:                .              .              .              .
>       60:            8BB-L            4B-              .           4dd-
>       61:                .              .              .              .
>       62:              8D-            4B-              .           4dd-
>       63:            8BB-J              .              .              .
>       64:               =8             =8             =8             =8
>       65:             8E-L            4B-              .            4b-
>       66:                .              .          8a-XJ              .
>       67:                .            4e-           8b-L            4b-
>       68:             8E-J              .              .              .
>       69:              4A-            4e-            4a-              .
>       70:             4A-;           4e-;           4a-;              .
>       71:               =9             =9             =9             =9
>       72:             8D-L            4d-              .            4a-
>       73:              8E-              .              .              .
>       74:                .            4d-              .            4a-
>       75:             8D-J              .              .              .
>       76:            8BB-L              .            4b-           4dd-
>       77:                .              .              .              .
>       78:              8D-              .            4b-           4dd-
>       79:            8BB-J              .              .              .
>       80:              =10            =10            =10            =10
>       81:             8E-L            4g-            4b-              .
>       82:                .              .              .              .
>       83:                .              .              .              .
>       84:                .           8e-J              .              .
>       85:            2BB-;           2d-;              .           2b-;
>       86:              =11            =11            =11            =11
>       87:              4B-              .              .              .
>       88:              4A-              .              .           4a-X
>       89:                .            4B-              .            4b-
>       90:                .              .              .              .
>       91:                .              .              .            4a-
>       92:              =12            =12            =12            =12
>       93:             4BB-            4d-              .              .
>       94:                .              .              .              .
>       95:                .           8B-J              .              .
>       96:                .           2A-;              .              .
>       97:              =13            =13            =13            =13
>       98:                .              .            4a-              .
>       99:                .              .              .              .
>      100:             8AA-              .            4a-              .
>      101:            8BB-J              .              .              .
>      102:                .              .              .          4ee-X
>      103:                .              .              .              .
>      104:              8E-              .              .           4ee-
>      105:                .              .              .              .
>      106:              =14            =14            =14            =14
>      107:                .              .              .              .
>      108:                .              .           8a-J              .
>      109:                .              .              .              .
>      110:                .              .              .              .
>      111:                .              .              .              .
>      112:                .              .              .              .
>      113:              =15            =15            =15            =15
>      114:                .              .              .              .
>      115:              8E-              .              .              .
>      116:              8D-              .              .              .
>      117:                .              .              .              .
>      118:             4BB-            4B-              .           4dd-
>      119:                .              .              .              .
>      120:                .            4E-            4a-              .
>      121:             8D-J              .              .              .
>      122:              =16            =16            =16            =16
>      123:              4E-           4.e-           8a-L           4.b-
>      124:                .              .              .              .
>      125:              4E-              .              .              .
>      126:                .            8d-              .            8a-
>      127:             4AA-              .            4e-            4a-
>      128:            4AA-;              .           4e-;           4a-;
>      129:              =17            =17            =17            =17
>      130:             8D-L            4d-              .            4a-
>      131:              8E-              .              .              .
>      132:              8D-            4d-              .            4a-
>      133:                .              .              .              .
>      134:            8BB-L              .            4b-           4dd-
>      135:                .              .              .              .
>      136:              8D-              .            4b-           4dd-
>      137:            8BB-J              .              .              .
>      138:              =18            =18            =18            =18
>      139:             8E-L            4e-              .            4b-
>      140:                .              .              .              .
>      141:              8E-            4e-              .            4b-
>      142:             8D-J              .              .              .
>      143:                .              .              .           4ee-
>      144:              8D-              .              .              .
>      145:              8E-              .              .           4ee-
>      146:                .              .              .              .
>      147:              =19            =19            =19            =19
>      148:                .              .           8a-L              .
>      149:                .              .              .              .
>      150:                .              .           8a-L              .
>      151:                .              .           8b-J              .
>      152:             8A-L              .              .              .
>      153:                .              .              .              .
>      154:              8A-              .           8b-L              .
>      155:             8B-J              .           8a-J              .
>      156:              =20            =20            =20            =20
>      157:                .              .              .              .
>      158:                .              .              .              .
>      159:                .              .              .              .
>      160:                .              .              .              .
>      161:               ==             ==             ==             ==
>      162:               *-             *-             *-             *-
>      163:  !!!hum2abc: -Q ''
>      164:  !!!title: @{PC#}. @{OTL@@DE}
>      165:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>      166:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>      167:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>      168:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>      169:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      170:  !!!EED:  Craig Stuart Sapp
>      171:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor008.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       Data fields: 
>               *Token :: character
```

### Subseting by Group

Like `mutate()`,
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
will evaluate its logical filtering expression within groups created by
[`group_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md).
This can be a useful way to get some context for our searches. For
example, let’s say we want to find flats again, but we want to see the
whole *bar* of music that contains a flat. We can do this by grouping by
the `Bar` field (and the `Piece` field, of course). We’ll want to say
“within each bar, if there are *any* flats, return `TRUE` for the
*whole* bar, else return `FALSE` for the *whole* bar.” By default,
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
will
[recycle](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md)
scalar results, so if you return a single `TRUE`/`FALSE`, it will be
recycled to fill the whole group.

``` r
chorales |>
  group_by(Piece, Bar) |>
  filter(any(Token %~% '-'))
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:          **kern        **kern        **kern        **kern
>               10:          *ICvox        *ICvox        *ICvox        *ICvox
>               11:          *Ibass       *Itenor        *Ialto       *Isoprn
>               12:         *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
>               13:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]
>               14:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]
>               15:             *>A           *>A           *>A           *>A
>               16:         *clefF4      *clefGv2       *clefG2       *clefG2
>               17:          *k[f#]        *k[f#]        *k[f#]        *k[f#]
>               18:             *a:           *a:           *a:           *a:
>               19:         !dorian       !dorian       !dorian       !dorian
>               20:           *M4/4         *M4/4         *M4/4         *M4/4
>               21:         *met(c)       *met(c)       *met(c)       *met(c)
>               22:          *MM100        *MM100        *MM100        *MM100
>            23-53:        =====1-4      =====1-4      =====1-4      =====1-4
>               54:             *>B           *>B           *>B           *>B
>            55-86:        =====5-9      =====5-9      =====5-9      =====5-9
>               87:             4G#            4B            4e           4ee
>               88:             8AL            4A            4e           4cc
>               89:             8GJ             .             .             .
>               90:             8FL            4A            4f           4dd
>               91:             8EJ             .             .             .
>    92-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-38::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               39:               .             .             .             .
>               40:               .             .             .             .
>               41:              =5            =5            =5            =5
>               42:               .             .             .             .
>               43:               .             .             .             .
>               44:               .             .             .             .
>               45:               .             .             .             .
>               46:               .             .             .             .
>               47:           =6:|!         =6:|!         =6:|!         =6:|!
>               48:             *>B           *>B           *>B           *>B
>            49-81:      ======7-12    ======7-12    ======7-12    ======7-12
>               82:              4F            4A            4c            4a
>               83:              4C            4G            4c            4e
>               84:            4BB-            4G           [2d            4g
>               85:             4AA            4A             .            4f
>               86:             =13           =13           =13           =13
>               87:               .             .             .             .
>               88:               .             .             .             .
>               89:               .             .             .             .
>               90:              ==            ==            ==            ==
>               91:              *-            *-            *-            *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastia***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und ***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 3***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., ***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***five global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token :: character
>    
>       Grouping fields: (two groups)
>                Bar   :: integer
>                Piece :: integer
```

Not enough context for you? Maybe we should group the bars into even-odd
pairs:

``` r
chorales |>
  group_by(Piece, floor(Bar / 2)) |>
  filter(any(Token %~% '-'))
>    ######################## vvv chor003.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Ach Gott, vom Himmel sieh darein
>                4:  !!!OTL@EN: Oh God, look down from Heaven
>                5:  !!!OPR: Cantata BWV 153: Schau, lieber Gott, wie meine Feind
>                6:  !!!SCT: BWV 153/1
>                7:  !!!PC#: 3
>                8:  !!!AGN: chorale
>                9:          **kern        **kern        **kern        **kern
>               10:          *ICvox        *ICvox        *ICvox        *ICvox
>               11:          *Ibass       *Itenor        *Ialto       *Isoprn
>               12:         *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
>               13:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]
>               14:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]
>               15:             *>A           *>A           *>A           *>A
>               16:         *clefF4      *clefGv2       *clefG2       *clefG2
>               17:          *k[f#]        *k[f#]        *k[f#]        *k[f#]
>               18:             *a:           *a:           *a:           *a:
>               19:         !dorian       !dorian       !dorian       !dorian
>               20:           *M4/4         *M4/4         *M4/4         *M4/4
>               21:         *met(c)       *met(c)       *met(c)       *met(c)
>               22:          *MM100        *MM100        *MM100        *MM100
>            23-53:        =====1-4      =====1-4      =====1-4      =====1-4
>               54:             *>B           *>B           *>B           *>B
>            55-86:        =====5-9      =====5-9      =====5-9      =====5-9
>               87:             4G#            4B            4e           4ee
>               88:             8AL            4A            4e           4cc
>               89:             8GJ             .             .             .
>               90:             8FL            4A            4f           4dd
>               91:             8EJ             .             .             .
>    92-110::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor003.krn ^^^ #########################
>    
>           (three more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-38::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               39:               .             .             .             .
>               40:               .             .             .             .
>               41:              =5            =5            =5            =5
>               42:               .             .             .             .
>               43:               .             .             .             .
>               44:               .             .             .             .
>               45:               .             .             .             .
>               46:               .             .             .             .
>               47:           =6:|!         =6:|!         =6:|!         =6:|!
>               48:             *>B           *>B           *>B           *>B
>            49-81:      ======7-12    ======7-12    ======7-12    ======7-12
>               82:              4F            4A            4c            4a
>               83:              4C            4G            4c            4e
>               84:            4BB-            4G           [2d            4g
>               85:             4AA            4A             .            4f
>               86:             =13           =13           =13           =13
>               87:            4GG#            4B           4d]           1e;
>               88:             4AA            4A            4c             .
>               89:            2EE;         2G#X;           2B;             .
>               90:              ==            ==            ==            ==
>               91:              *-            *-            *-            *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastia***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und ***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 3***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., ***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***five global comments truncated due to screen size***)
>    
>       humdrumR corpus of five pieces.
>    
>       Data fields: 
>               *Token        :: character
>    
>       Grouping fields: (two groups)
>                floor(Bar/2) :: numeric
>                Piece        :: integer
```

### Removing vs Filtering

You probably noticed that, unlike the indexing commands `[]` and `[[]]`,
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
doesn’t seem to actually *remove* data that you filter out. So when we
say

``` r
chorales |>
   filter(Spine == 1)
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG              .              .              .
>               21:               =1             =1             =1             =1
>               22:               4G              .              .              .
>               23:               4E              .              .              .
>               24:                .              .              .              .
>               25:              4F#              .              .              .
>               26:               =2             =2             =2             =2
>               27:               4G              .              .              .
>               28:               4D              .              .              .
>               29:                .              .              .              .
>               30:               4E              .              .              .
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D              .              .              .
>               72:                .              .              .              .
>               73:               4D              .              .              .
>               74:                .              .              .              .
>               75:             2GG;              .              .              .
>               76:              =11            =11            =11            =11
>               77:               2C              .              .              .
>               78:              4AA              .              .              .
>               79:               4E              .              .              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:               4F              .              .              .
>               83:               4C              .              .              .
>               84:             4BB-              .              .              .
>               85:              4AA              .              .              .
>               86:              =13            =13            =13            =13
>               87:             4GG#              .              .              .
>               88:              4AA              .              .              .
>               89:             2EE;              .              .              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

we still have four spines, but spines 2–4 are just emptied. This is
correct. What
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
actually does is turn any filtered data points into null (`d`) data
points. Humdrum$_{\mathbb{R}}$ then ignores that data automatically. Why
do we do this? There are several reasons:

1.  Sometimes, seeing the full structure remain in place is
    cleaner/easier to interpret than actually removing them. Basically,
    all those empty spines remind you what you filtered, so it is easier
    to see if the filter is doing what you *think* it is doing.
    - A clear example of this is when we did `[[ , j]]` indexing for
      flats (above): when we simply removed the spines with no flats it
      was hard to tell which spine was which in the result.
2.  Many filters would break the humdrum syntax if the data was simply
    removed. If you removed all the tokens that don’t contain flats, the
    result would be humdrum data with holes in it.
3.  Its possible to undo your filters, using the
    [`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
    command (details
    [below](https://humdrumR.ccml.gtcmt.gatech.edu/articles/Filtering.html#complements)).

Once you’ve done some filtering with
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
if you *want* to get rid of empty parts of the data, you can do so using
the commands
[`removeEmptyFiles()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
[`removeEmptySpines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
[`removeEmptyRecords()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),[`removeEmptyPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
or
[`removeEmptyStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md).
By using this commands, we make sure that 1) you explicitly want to
remove them and 2) the humdrum syntax is not broken, because only whole
records/spines/paths/files are removed.

So for our spines example:

``` r

chorales |>
  filter(Spine == 1) |>
  removeEmptySpines()
```

```
>    #### vvv chor001.krn vvv ####
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern
>                9:                *ICvox
>               10:                *Ibass
>               11:               *I"Bass
>               12:             *>[A,A,B]
>               13:          *>norep[A,B]
>               14:                   *>A
>               15:               *clefF4
>               16:                *k[f#]
>               17:                   *G:
>               18:                 *M3/4
>               19:                *MM100
>               20:                   4GG
>               21:                    =1
>               22:                    4G
>               23:                    4E
>               24:                     .
>               25:                   4F#
>               26:                    =2
>               27:                    4G
>               28:                    4D
>               29:                     .
>               30:                    4E
>    31-133:::::::::::::::::::::::
>    #### ^^^ chor001.krn ^^^ ####
>    
>           (eight more pieces...)
>    
>    #### vvv chor010.krn vvv ####
>      1-70:::::::::::::::::::::::
>               71:                    4D
>               72:                     .
>               73:                    4D
>               74:                     .
>               75:                  2GG;
>               76:                   =11
>               77:                    2C
>               78:                   4AA
>               79:                    4E
>               80:                     .
>               81:                   =12
>               82:                    4F
>               83:                    4C
>               84:                  4BB-
>               85:                   4AA
>               86:                   =13
>               87:                  4GG#
>               88:                   4AA
>               89:                  2EE;
>               90:                    ==
>               91:                    *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    #### ^^^ chor010.krn ^^^ ####
>    (***four global comments truncated
>            due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

or for records:

``` r

chorales |>
  filter(Record %% 2 == 0) |>
  removeEmptyRecords()
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               24:                .            8BJ              .              .
>               26:               =2             =2             =2             =2
>               28:               4D            4F#              .              .
>               30:               4E             4G             4B             4g
>               31:               =3             =3             =3             =3
>               32:               4C            8cL            8eL            4.g
>               34:             8BBL             4c             8e              .
>               36:              4GG             4d             4g             4b
>    37-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-54::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               56:              8EJ            8eJ              .              .
>               58:                .              .            8aJ           8ccJ
>               60:               =8             =8             =8             =8
>               62:               2A             2e             2a            2cc
>               63:               =9             =9             =9             =9
>               64:               4E             4e             4g             4b
>               66:              8CJ              .              .              .
>               68:                .              .            8fJ              .
>               70:              =10            =10            =10            =10
>               72:                .             4G              .              .
>               74:                .            8F#              .              .
>               76:              =11            =11            =11            =11
>               78:              4AA             4A             4e            4cc
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               84:             4BB-             4G            [2d             4g
>               86:              =13            =13            =13            =13
>               88:              4AA             4A             4c              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

## Complements

When humdrum$_{\mathbb{R}}$ filters, it does not completely
discard/erase the data. The output of
[`filter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
is a subset of the original data: the *complement* of that subset is
retained (but hidden). This allows us to restore the filtered data,
using
[`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md):

``` r
chorales |>
  filter(Spine == 1) |>
  unfilter()
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               23:               4E            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4G             4G             2d            4.b
>               28:               4D            4F#              .              .
>               29:                .              .              .             8a
>               30:               4E             4G             4B             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4D              .             4c             4a
>               74:                .            8F#              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:               2C             2G             2e             2g
>               78:              4AA             4A             4e            4cc
>               79:               4E            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               83:               4C             4G             4c             4e
>               84:             4BB-             4G            [2d             4g
>               85:              4AA             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:             4GG#             4B            4d]            1e;
>               88:              4AA             4A             4c              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

We can also expliticely switch to the complement using the
[`complement()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
command:

``` r
chorales |>
  filter(Spine == 1) |>
  complement()
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:                .             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:                .             4B             4d             2g
>               23:                .            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:                .             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:                .             4G             2d            4.b
>               28:                .            4F#              .              .
>               29:                .              .              .             8a
>               30:                .             4G             4B             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:                .            8F#             4d             4b
>               72:                .             4G              .              .
>               73:                .              .             4c             4a
>               74:                .            8F#              .              .
>               75:                .            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:                .             2G             2e             2g
>               78:                .             4A             4e            4cc
>               79:                .            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:                .             4A             4c             4a
>               83:                .             4G             4c             4e
>               84:                .             4G            [2d             4g
>               85:                .             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:                .             4B            4d]            1e;
>               88:                .             4A             4c              .
>               89:                .          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *Token :: character
```

Why would we want to unfilter or complement our data? These functions
actually allow a number of useful functionality that is not immediately
obvious.

##### Example 1

Let’s say we want to find and look at the notes leading up to the
*third* phrase ending in each chorale? We’d first need to count the
phrase endings (`;`) in each chorale. We could filter our data, and
simply [`seq_along()`](https://rdrr.io/r/base/seq.html) (enumerate) the
fermatas in each bar:

``` r
chorales |>
  filter(Token %~% ';') |>
  group_by(Piece, Spine) |>
  mutate(FermataN = seq_along(Token)) -> chorales

chorales[1]
```

```
>    ######################## vvv chor001.krn vvv #########################
>        1:  !!!COM: Bach, Johann Sebastian
>        2:  !!!CDT: 1685/02/21/-1750/07/28/
>        3:  !!!OTL@@DE: Aus meines Herzens Grunde
>        4:  !!!OTL@EN:      From the Depths of My Heart
>        5:  !!!SCT: BWV 269
>        6:  !!!PC#: 1
>        7:  !!!AGN: chorale
>        8:           **kern         **kern         **kern         **kern
>        9:           *ICvox         *ICvox         *ICvox         *ICvox
>       10:           *Ibass        *Itenor         *Ialto        *Isoprn
>       11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>       12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>       13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>       14:              *>A            *>A            *>A            *>A
>       15:          *clefF4       *clefGv2        *clefG2        *clefG2
>       16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>       17:              *G:            *G:            *G:            *G:
>       18:            *M3/4          *M3/4          *M3/4          *M3/4
>       19:           *MM100         *MM100         *MM100         *MM100
>       20:                .              .              .              .
>       21:               =1             =1             =1             =1
>       22:                .              .              .              .
>       23:                .              .              .              .
>       24:                .              .              .              .
>       25:                .              .              .              .
>       26:               =2             =2             =2             =2
>       27:                .              .              .              .
>       28:                .              .              .              .
>       29:                .              .              .              .
>       30:                .              .              .              .
>       31:               =3             =3             =3             =3
>       32:                .              .              .              .
>       33:                .              .              .              .
>       34:                .              .              .              .
>       35:                .              .              .              .
>       36:                .              .              .              .
>       37:               =4             =4             =4             =4
>       38:                1              1              1              1
>       39:                .              .              .              .
>       40:               =5             =5             =5             =5
>       41:                .              .              .              .
>       42:                .              .              .              .
>       43:                .              .              .              .
>       44:               =6             =6             =6             =6
>       45:                .              .              .              .
>       46:                .              .              .              .
>       47:                .              .              .              .
>       48:                .              .              .              .
>       49:               =7             =7             =7             =7
>       50:                2              2              2              2
>       51:             =:|!           =:|!           =:|!           =:|!
>       52:              *>B            *>B            *>B            *>B
>       53:                .              .              .              .
>       54:               =8             =8             =8             =8
>       55:                .              .              .              .
>       56:                .              .              .              .
>       57:                .              .              .              .
>       58:                .              .              .              .
>       59:                .              .              .              .
>       60:                .              .              .              .
>       61:               =9             =9             =9             =9
>       62:                .              .              .              .
>       63:                .              .              .              .
>       64:                .              .              .              .
>       65:                .              .              .              .
>       66:                .              .              .              .
>       67:              =10            =10            =10            =10
>       68:                3              3              3              3
>       69:                .              .              .              .
>       70:              =11            =11            =11            =11
>       71:                .              .              .              .
>       72:                .              .              .              .
>       73:                .              .              .              .
>       74:                .              .              .              .
>       75:              =12            =12            =12            =12
>       76:                .              .              .              .
>       77:                .              .              .              .
>       78:                .              .              .              .
>       79:                .              .              .              .
>       80:                .              .              .              .
>       81:              =13            =13            =13            =13
>       82:                .              .              .              .
>       83:                .              .              .              .
>       84:                .              .              .              .
>       85:                .              .              .              .
>       86:              =14            =14            =14            =14
>       87:                4              4              4              4
>       88:                .              .              .              .
>       89:              =15            =15            =15            =15
>       90:                .              .              .              .
>       91:                .              .              .              .
>       92:                .              .              .              .
>       93:                .              .              .              .
>       94:                .              .              .              .
>       95:              =16            =16            =16            =16
>       96:                .              .              .              .
>       97:                .              .              .              .
>       98:                .              .              .              .
>       99:                .              .              .              .
>      100:              =17            =17            =17            =17
>      101:                .              .              .              .
>      102:                .              .              .              .
>      103:                .              .              .              .
>      104:                .              .              .              .
>      105:                .              .              .              .
>      106:                .              .              .              .
>      107:              =18            =18            =18            =18
>      108:                5              5              5              5
>      109:                .              .              .              .
>      110:              =19            =19            =19            =19
>      111:                .              .              .              .
>      112:                .              .              .              .
>      113:                .              .              .              .
>      114:                .              .              .              .
>      115:              =20            =20            =20            =20
>      116:                .              .              .              .
>      117:                .              .              .              .
>      118:                .              .              .              .
>      119:                .              .              .              .
>      120:                .              .              .              .
>      121:              =21            =21            =21            =21
>      122:                6              6              6              6
>      123:               ==             ==             ==             ==
>      124:               *-             *-             *-             *-
>      125:  !!!hum2abc: -Q ''
>      126:  !!!title: @{PC#}. @{OTL@@DE}
>      127:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>      128:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>      129:  !!!YOR3: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>      130:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>      131:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>      132:  !!!EED:  Craig Stuart Sapp
>      133:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor001.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       Data fields: 
>               *FermataN :: integer
>                Token    :: character
>    
>       Grouping fields: (four groups)
>                Piece    :: integer
>                Spine    :: integer
```

Cool! We now have a `FermataN` field, showing us the number of each
fermata. But wait, using filter *removed* most of our tokens, so we can
only see the notes during the fermatas, not leading up to them.

``` r
chorales |>
  select(Token)
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:                .              .              .              .
>               21:               =1             =1             =1             =1
>               22:                .              .              .              .
>               23:                .              .              .              .
>               24:                .              .              .              .
>               25:                .              .              .              .
>               26:               =2             =2             =2             =2
>               27:                .              .              .              .
>               28:                .              .              .              .
>               29:                .              .              .              .
>               30:                .              .              .              .
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:                .              .              .              .
>               72:                .              .              .              .
>               73:                .              .              .              .
>               74:                .              .              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:                .              .              .              .
>               78:                .              .              .              .
>               79:                .              .              .              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:                .              .              .              .
>               83:                .              .              .              .
>               84:                .              .              .              .
>               85:                .              .              .              .
>               86:              =13            =13            =13            =13
>               87:                .              .              .            1e;
>               88:                .              .              .              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                FermataN :: integer
>               *Token    :: character
>    
>       Grouping fields: (eight groups)
>                Piece    :: integer
>                Spine    :: integer
```

Luckily, we can
[`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md),
then filter again by bar:

``` r
chorales |> 
  unfilter() |>
  ungroup() |>
  group_by(File, Bar) 
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:              4GG             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4G             4B             4d             2g
>               23:               4E            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:              4F#             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4G             4G             2d            4.b
>               28:               4D            4F#              .              .
>               29:                .              .              .             8a
>               30:               4E             4G             4B             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4D            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4D              .             4c             4a
>               74:                .            8F#              .              .
>               75:             2GG;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:               2C             2G             2e             2g
>               78:              4AA             4A             4e            4cc
>               79:               4E            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:               4F             4A             4c             4a
>               83:               4C             4G             4c             4e
>               84:             4BB-             4G            [2d             4g
>               85:              4AA             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:             4GG#             4B            4d]            1e;
>               88:              4AA             4A             4c              .
>               89:             2EE;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                FermataN :: integer
>               *Token    :: character
>    
>       Grouping fields: (thirty-six groups)
>                Bar      :: integer
>                File     :: integer
```

##### Example 2

What if you wanted to transpose the bass voice an octave higher, but
keep the other voices the same? Well, we could try:

``` r
chorales |>
  filter(Spine == 1) |>
  mutate(Transposed = transpose(Token, by = 'P8')) |> 
  unfilter()
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:               4C              .              .              .
>               21:               =1             =1             =1             =1
>               22:               4c              .              .              .
>               23:               4A              .              .              .
>               24:                .              .              .              .
>               25:               4B              .              .              .
>               26:               =2             =2             =2             =2
>               27:               4c              .              .              .
>               28:               4G              .              .              .
>               29:                .              .              .              .
>               30:               4A              .              .              .
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4F              .              .              .
>               72:                .              .              .              .
>               73:               4F              .              .              .
>               74:                .              .              .              .
>               75:            2BB-;              .              .              .
>               76:              =11            =11            =11            =11
>               77:              2E-              .              .              .
>               78:               4C              .              .              .
>               79:               4G              .              .              .
>               80:                .              .              .              .
>               81:              =12            =12            =12            =12
>               82:              4A-              .              .              .
>               83:              4E-              .              .              .
>               84:              4D-              .              .              .
>               85:               4C              .              .              .
>               86:              =13            =13            =13            =13
>               87:              4BB              .              .              .
>               88:               4C              .              .              .
>               89:             2GG;              .              .              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                FermataN   :: integer
>                Token      :: character
>               *Transposed :: character
>    
>       Grouping fields: (eight groups)
>                Piece      :: integer
>                Spine      :: integer
```

We successfully unfiltered…but the `Token` field was unfiltered, not our
new `Transposed` field. This is because the `Tranposed` field has no
complement data: it didn’t exist until after the filter was applied.
Luckily, we can tell
[`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
to use a specific complement, using the `complement` argument:

``` r
chorales |>
  filter(Spine == 1) |>
  mutate(Transposed = transpose(Token, by = 'P8')) |> 
  unfilter(complement = 'Token')
```

```
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:           **kern         **kern         **kern         **kern
>                9:           *ICvox         *ICvox         *ICvox         *ICvox
>               10:           *Ibass        *Itenor         *Ialto        *Isoprn
>               11:          *I"Bass       *I"Tenor        *I"Alto     *I"Soprano
>               12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
>               13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
>               14:              *>A            *>A            *>A            *>A
>               15:          *clefF4       *clefGv2        *clefG2        *clefG2
>               16:           *k[f#]         *k[f#]         *k[f#]         *k[f#]
>               17:              *G:            *G:            *G:            *G:
>               18:            *M3/4          *M3/4          *M3/4          *M3/4
>               19:           *MM100         *MM100         *MM100         *MM100
>               20:               4C             4B             4d             4g
>               21:               =1             =1             =1             =1
>               22:               4c             4B             4d             2g
>               23:               4A            8cL             4e              .
>               24:                .            8BJ              .              .
>               25:               4B             4A             4d            4dd
>               26:               =2             =2             =2             =2
>               27:               4c             4G             2d            4.b
>               28:               4G            4F#              .              .
>               29:                .              .              .             8a
>               30:               4A             4G             4B             4g
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:               4F            8F#             4d             4b
>               72:                .             4G              .              .
>               73:               4F              .             4c             4a
>               74:                .            8F#              .              .
>               75:            2BB-;            2G;            2B;            2g;
>               76:              =11            =11            =11            =11
>               77:              2E-             2G             2e             2g
>               78:               4C             4A             4e            4cc
>               79:               4G            4G#            8eL             4b
>               80:                .              .            8dJ              .
>               81:              =12            =12            =12            =12
>               82:              4A-             4A             4c             4a
>               83:              4E-             4G             4c             4e
>               84:              4D-             4G            [2d             4g
>               85:               4C             4A              .             4f
>               86:              =13            =13            =13            =13
>               87:              4BB             4B            4d]            1e;
>               88:               4C             4A             4c              .
>               89:             2GG;          2G#X;            2B;              .
>               90:               ==             ==             ==             ==
>               91:               *-             *-             *-             *-
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian B***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&a***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.1***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                  (***four global comments truncated due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                FermataN   :: integer
>                Token      :: character
>               *Transposed :: character
>    
>       Grouping fields: (eight groups)
>                Piece      :: integer
>                Spine      :: integer
```

Thus,
[`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
can be used to combine the content of different fields into one field!
