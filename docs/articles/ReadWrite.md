# Reading and writing humdrum data

Welcome to “Reading and writing data with humdrumR”! This article will
give you a detailed overview of humdrum$_{\mathbb{R}}$’s data reading
and writing functionality.

This article, like all of our articles, closely parallels information in
humdrum$_{\mathbb{R}}$’s detailed code documentation, which can be found
in the “[Reference \> Reading and
Writing](https://humdrumr.ccml.gtcmt.gatech.edu/reference/index.html#reading-and-writing "HumdrumR function reference, Reading and Writing")”
section of the
humdrum$_{\mathbb{R}}$[homepage](https://humdrumR.ccml.gtcmt.gatech.edu).
You can also find this information within R, once humdrum$_{\mathbb{R}}$
is loaded, using
[`?readHumdrum`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
or
[`?writeHumdrum`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md).

## Reading humdrum data

The
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
function is the gateway to humdrum$_{\mathbb{R}}$: it is the function we
use to read humdrum data, encoded in
[humdrum-syntax](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md "The humdrum syntax, article")
text files, into R.

------------------------------------------------------------------------

In the following examples, we will make use the small number of raw
humdrum data files which are included with humdrum$_{\mathbb{R}}$. To
access these files, we need to navigate to the directory where your
computer installed humdrum$_{\mathbb{R}}$—fortunately,
humdrum$_{\mathbb{R}}$ records where this directory is on your computer
in a variable called `humdrumRroot`. Once we load the
humdrum$_{\mathbb{R}}$ library, we just need to set our R “working
directory” to that location using
[`setwd()`](https://rdrr.io/r/base/getwd.html).

``` r
library(humdrumR)

setwd(humdrumRroot)
```

The humdrum data is stored in a subdirectory called “`HumdrumData`”—we
can look at the contents of this directory using
`dir(recursive = TRUE)`:

``` r
dir('HumdrumData', recursive = TRUE)
>     [1] "BachChorales/chor001.krn"                            
>     [2] "BachChorales/chor002.krn"                            
>     [3] "BachChorales/chor003.krn"                            
>     [4] "BachChorales/chor004.krn"                            
>     [5] "BachChorales/chor005.krn"                            
>     [6] "BachChorales/chor006.krn"                            
>     [7] "BachChorales/chor007.krn"                            
>     [8] "BachChorales/chor008.krn"                            
>     [9] "BachChorales/chor009.krn"                            
>    [10] "BachChorales/chor010.krn"                            
>    [11] "BeethovenVariations/B075_00_01_a.krn"                
>    [12] "BeethovenVariations/B075_00_02_a.krn"                
>    [13] "BeethovenVariations/B075_00_03_a.krn"                
>    [14] "BeethovenVariations/B075_00_04_a.krn"                
>    [15] "BeethovenVariations/B075_00_05_a.krn"                
>    [16] "BeethovenVariations/B075_00_06_a.krn"                
>    [17] "BeethovenVariations/B075_01_01_a.krn"                
>    [18] "BeethovenVariations/B075_01_02_a.krn"                
>    [19] "BeethovenVariations/B075_01_03_a.krn"                
>    [20] "BeethovenVariations/B075_01_04_a.krn"                
>    [21] "BeethovenVariations/B075_01_05_a.krn"                
>    [22] "BeethovenVariations/B075_01_06_a.krn"                
>    [23] "InvalidFile.krn"                                     
>    [24] "MozartVariations/M354_00_01a_a.krn"                  
>    [25] "MozartVariations/M354_00_02b_a.krn"                  
>    [26] "MozartVariations/M354_00_03c_a.krn"                  
>    [27] "MozartVariations/M354_00_04d_a.krn"                  
>    [28] "MozartVariations/M354_01_01a_a.krn"                  
>    [29] "MozartVariations/M354_01_02b_a.krn"                  
>    [30] "MozartVariations/M354_01_03c_a.krn"                  
>    [31] "MozartVariations/M354_01_04d_a.krn"                  
>    [32] "RapFlow/2pac_IGetAround.rap"                         
>    [33] "RapFlow/BeastieBoys_BrassMonkey.rap"                 
>    [34] "RapFlow/Coolio_GangstasParadise.rap"                 
>    [35] "RapFlow/Ludacris_MoneyMaker.rap"                     
>    [36] "RapFlow/NotoriousBIG_Hypnotize.rap"                  
>    [37] "RapFlow/RobBase_ItTakesTwo.rap"                      
>    [38] "RapFlow/WillSmith_Summertime.rap"                    
>    [39] "RollingStoneCorpus/ACDC_BackInBlack.hum"             
>    [40] "RollingStoneCorpus/AlGreen_LetsStayTogether.hum"     
>    [41] "RollingStoneCorpus/CarlPerkins_BlueSuedeShoes.hum"   
>    [42] "RollingStoneCorpus/DerekAndTheDominos_Layla.hum"     
>    [43] "RollingStoneCorpus/EltonJohn_YourSong.hum"           
>    [44] "RollingStoneCorpus/JanisJoplin_MeAndBobbyMcGee.hum"  
>    [45] "RollingStoneCorpus/JohnnyCash_IWalkTheLine.hum"      
>    [46] "RollingStoneCorpus/LedZeppelin_Kashmir.hum"          
>    [47] "RollingStoneCorpus/Nirvana_AllApologies.hum"         
>    [48] "RollingStoneCorpus/Steppenwolf_BornToBeWild.hum"     
>    [49] "RollingStoneCorpus/StevieWonder_LivingForTheCity.hum"
>    [50] "RollingStoneCorpus/TheBeachBoys_GodOnlyKnows.hum"    
>    [51] "RollingStoneCorpus/TheBeatles_HeyJude.hum"
```

As we can see, there are six directories containing a total of fifty-one
files.

### Targeting files

To use
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
we call the function with one (or more) character-string arguments. (In
the function documentation
([`?readHumdrum`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)),
you’ll see these arguments called `...`—in R, `...` means *any* number
of arguments.) These arguments are interpreted as [regular
expressions](https://en.wikipedia.org/wiki/Regular_expression), and
matched against directories and files on your system. We call these
“**REpath-patterns**,” short for Regular Expression directory-path
patterns. For example,

``` r

readHumdrum('HumdrumData/BachChorales/chor.*.krn')
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

uses the REpath-pattern `"HumdrumData/BachChorales/chor.*.krn"` to match
all files in the ‘HumdrumData/BachChorales’ directory whose name matches
the regular expression `chor.*.krn`. As you can see, the REpath-pattern
includes directory paths (relative or absolute)—using your operating
system’s appropriate delimiter ("\\ for windows, "/" for everybody
else)—*and* a pattern for matching file paths. *Each* directory is also
treated as a regular expression, possibly matching multiple directories.
Let’s break this down: humdrum$_{\mathbb{R}}$ splits your REpath-pattern
input string into three parts: `"HumdrumData"`, `"BachChorales"`, *and*
`"chor.*.krn"`, and treats *each* of the three as regular expressions.
The first two expressions are used to match directories, while the last
expression is used to match files. So, while
`readHumdrum('HumdrumData/BeethovenVariations/.*.krn')` matches all the
files with the “.krn” extension in the “BeethovenVariations” directories
and `readHumdrum('HumdrumData/MozartVariations/.*.krn')` matches all the
files in the “MozartVariations” directories, the command
`readHumdrum('HumdrumData/.*Variations/.*.krn')` will match all kern
files in both directories!

``` r
readHumdrum('HumdrumData/.*Variations/.*.krn')
```

```
>    ###################### vvv B075_00_01_a.krn vvv ######################
>               1:  !!!COM: Beethoven
>               2:  !!!OTL: 7 Variations on a Quartet by Winter
>               3:  !!!Variation: Theme a
>               4:      **function    **harm       **kern                 **kern
>               5:               *         *      *clefG2                *clefG2
>               6:               *         *       *k[b-]                 *k[b-]
>               7:           *M2/4     *M2/4        *M2/4                  *M2/4
>               8:             *F:       *F:          *F:                    *F:
>               9:              4T        4I           4r               4aa 4ccc
>              10:              =1        =1           =1                     =1
>              11:              2T        2I           2f             8ff'L 8aa'
>              12:               .         .            .             8cc'J 8ff'
>              13:               .         .            .             8ff'L 8aa'
>              14:               .         .            .            8aa'J 8ccc'
>              15:              =2        =2           =2                     =2
>              16:              4T       4V7    (4c 4e 4g              (4gg 4bb-
>              17:              4T        4I          4f)               4ff) 4aa
>              18:              =3        =3           =3                     =3
>              19:              2T        2V           4c             8ee'L 8gg'
>              20:               .         .            .             8dd'J 8ff'
>              21:               .         .           4c             8cc'L 8ee'
>              22:               .         .            .             8dd'J 8ff'
>              23:              =4        =4           =4                     =4
>              24:              4T        4V           8c                8ee 8gg
>              25:               .         .           8r                     8r
>              26:              4T        4I           4r               4aa 4ccc
>              27:              =5        =5           =5                     =5
>              28:              2T        2I           2f             8ff'L 8aa'
>              29:               .         .            .             8cc'J 8ff'
>              30:               .         .            .             8ff'L 8aa'
>    31-45:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ###################### ^^^ B075_00_01_a.krn ^^^ ######################
>    
>           (eighteen more pieces...)
>    
>    ##################### vvv M354_01_04d_a.krn vvv ######################
>     1-24:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>              25:               .         .            .       .           16b-
>              26:               .         .       8B- 8f       .            16a
>              27:               .         .            .       .        16b-JJ)
>              28:               *         *           *v      *v              *
>              29:             =42       =42          =42                    =42
>              30:               *         *      *clefF4                      *
>              31:              4T       4Ib           8r               (16ee-LL
>              32:               .         .            .                   16ff
>              33:               .         .       8G 8e-                  16gg)
>              34:               .         .            .                 16b'JJ
>              35:              4P       4IV           8r                (16ccLL
>              36:               .         .            .                   16dd
>              37:               .         .      8A- 8e-                 16ee-)
>              38:               .         .            .                16a-'JJ
>              39:             =43       =43          =43                    =43
>              40:               *         *           *^                      *
>              41:              4D       4Cc           8r    2BB-         (16gLL
>              42:               .         .            .       .           16a-
>              43:               .         .          (8G       .          16b-)
>              44:               .         .            .       .         16e'JJ
>              45:              4D       4V7         8A-L       .         (16fLL
>              46:               .         .            .       .            16g
>              47:               .         .         8FJ)       .          16a-)
>              48:               .         .            .       .         16d'JJ
>              49:               *         *           *v      *v              *
>              50:             =44       =44          =44                    =44
>              51:              2T        2I         8E-L                    4e-
>              52:               .         .        8BB-J                      .
>              53:               .         .         4EE-                     4r
>              54:              *-        *-           *-                     *-
>    ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
>    
>       humdrumR corpus of twenty pieces.
>    
>       Data fields: 
>               *Token :: character
```

> Be careful: since each directory/file name is always treated like a
> regular expression, we can sometimes specify something that is more
> general than we intend. For instance, the command
> `readHumdrum('MyFolder/.*')` will match files in a folder called
> “MyFolder,” but would also match folders names “MyFolder_Also”, or
> “ThisIsMyFolder.” If you want to be sure to match exactly one and only
> one directory/file, use “^” and “\$” regular-expression markers to
> explicitly mark the beginning and end of your pattern: the command
> `readHumdrum('^MyFolder$/.*')` will only read files from one directory
> “MyFolder.”

To read *all* of our test files we can enter:

``` r

readHumdrum('HumdrumData/.*/.*')
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
>                8:          **kern        **kern        **kern        **kern    ***
>                9:          *ICvox        *ICvox        *ICvox        *ICvox    ***
>               10:          *Ibass       *Itenor        *Ialto       *Isoprn    ***
>               11:         *I"Bass      *I"Tenor       *I"Alto    *I"Soprano    ***
>               12:       *>[A,A,B]     *>[A,A,B]     *>[A,A,B]     *>[A,A,B]    ***
>               13:    *>norep[A,B]  *>norep[A,B]  *>norep[A,B]  *>norep[A,B]    ***
>               14:             *>A           *>A           *>A           *>A    ***
>               15:         *clefF4      *clefGv2       *clefG2       *clefG2    ***
>               16:          *k[f#]        *k[f#]        *k[f#]        *k[f#]    ***
>               17:             *G:           *G:           *G:           *G:    ***
>               18:           *M3/4         *M3/4         *M3/4         *M3/4    ***
>               19:          *MM100        *MM100        *MM100        *MM100    ***
>               20:             4GG            4B            4d            4g    ***
>               21:              =1            =1            =1            =1    ***
>               22:              4G            4B            4d            2g    ***
>               23:              4E           8cL            4e             .    ***
>               24:               .           8BJ             .             .    ***
>               25:             4F#            4A            4d           4dd    ***
>               26:              =2            =2            =2            =2    ***
>               27:              4G            4G            2d           4.b    ***
>               28:              4D           4F#             .             .    ***
>               29:               .             .             .            8a    ***
>               30:              4E            4G            4B            4g    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (forty-eight more pieces...)
>    
>    ################### vvv TheBeatles_HeyJude.hum vvv ###################
>    1-1591::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>             1592:               .             .             .             .    ***
>             1593:               .             .             .             .    ***
>             1594:               .             .             .             .    ***
>             1595:               .             .             .             .    ***
>             1596:            =131          =131          =131          =131    ***
>             1597:              IV        B-:maj            IV        B-:maj    ***
>             1598:               .             .             .             .    ***
>             1599:               .             .             .             .    ***
>             1600:               .             .             .             .    ***
>             1601:               .             .             .             .    ***
>             1602:               .             .             .             .    ***
>             1603:               .             .             .             .    ***
>             1604:               .             .             .             .    ***
>             1605:               .             .             .             .    ***
>             1606:               .             .             .             .    ***
>             1607:               .             .             .             .    ***
>             1608:               .             .             .             .    ***
>             1609:               .             .             .             .    ***
>             1610:               .             .             .             .    ***
>             1611:               .             .             .             .    ***
>             1612:               .             .             .             .    ***
>             1613:            =132          =132          =132          =132    ***
>             1614:            *tb1          *tb1          *tb1          *tb1    ***
>             1615:               I         F:maj             I         F:maj    ***
>             1616:              *-            *-            *-            *-    ***
>             1617:  !!!ONB: Translated from original encodings in the Rolling St***
>             1618:  !!!ONB: Original transcribers noted in comments in each spin***
>             1619:  !!!YOE: David Temperley, Trevor de Clercq
>             1620:  !!!EED: Nathaniel Condit-Schultz
>             1621:  !!!ENC: Nathaniel Condit-Schultz, automated
>    ################### ^^^ TheBeatles_HeyJude.hum ^^^ ###################
>                 (***four spines/paths not displayed due to screen size***)
>    
>       humdrumR corpus of fifty pieces.
>    
>       Data fields: 
>               *Token :: character
```

> Note: Since humdrum$_{\mathbb{R}}$ can read files from multiple
> directories at once, it is possible that two or more files with the
> same file name, but in different directories are matched. If this
> happens, humdrum$_{\mathbb{R}}$ will read both files, but identifies
> each file with enough of its directory path to make sure you can
> distinguish them.

------------------------------------------------------------------------

#### Multi-Argument Approach

Earlier we mentioned that we can have one *or more* of these RE-path
patterns. Why more than one? Well, we can divided our RE-path patterns
across multiple arguments (from left to right): Instead of writing
`readHumdrum("HumdrumData/BachChorales/chor.*.krn")`, we can write
`readHumdrum("HumdrumData", "BachChorales", "chor.*.krn")`. Thus these
two approaches

``` r
readHumdrum("HumdrumData/BachChorales/chor.*.krn")

readHumdrum("HumdrumData", "BachChorales", "chor.*.krn")
```

are identical. What’s the advantage of this approach? We’ll see in the
next section.

#### Multiple Patterns

Sometimes, expressing all the files you want in a single regular
expression is not possible. Luckily,
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
can accept as many separate patterns you want, just group them into a
vector. Instead of writing
`readHumdrum('HumdrumData/.*Variations/.*.krn')`, you could be explicit
and write

``` r
readHumdrum(c('HumdrumData/BeethovenVariations/.*.krn', 'HumdrumData/MozartVariations/.*.krn'))
```

```
>    ###################### vvv B075_00_01_a.krn vvv ######################
>               1:  !!!COM: Beethoven
>               2:  !!!OTL: 7 Variations on a Quartet by Winter
>               3:  !!!Variation: Theme a
>               4:      **function    **harm       **kern                 **kern
>               5:               *         *      *clefG2                *clefG2
>               6:               *         *       *k[b-]                 *k[b-]
>               7:           *M2/4     *M2/4        *M2/4                  *M2/4
>               8:             *F:       *F:          *F:                    *F:
>               9:              4T        4I           4r               4aa 4ccc
>              10:              =1        =1           =1                     =1
>              11:              2T        2I           2f             8ff'L 8aa'
>              12:               .         .            .             8cc'J 8ff'
>              13:               .         .            .             8ff'L 8aa'
>              14:               .         .            .            8aa'J 8ccc'
>              15:              =2        =2           =2                     =2
>              16:              4T       4V7    (4c 4e 4g              (4gg 4bb-
>              17:              4T        4I          4f)               4ff) 4aa
>              18:              =3        =3           =3                     =3
>              19:              2T        2V           4c             8ee'L 8gg'
>              20:               .         .            .             8dd'J 8ff'
>              21:               .         .           4c             8cc'L 8ee'
>              22:               .         .            .             8dd'J 8ff'
>              23:              =4        =4           =4                     =4
>              24:              4T        4V           8c                8ee 8gg
>              25:               .         .           8r                     8r
>              26:              4T        4I           4r               4aa 4ccc
>              27:              =5        =5           =5                     =5
>              28:              2T        2I           2f             8ff'L 8aa'
>              29:               .         .            .             8cc'J 8ff'
>              30:               .         .            .             8ff'L 8aa'
>    31-45:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ###################### ^^^ B075_00_01_a.krn ^^^ ######################
>    
>           (eighteen more pieces...)
>    
>    ##################### vvv M354_01_04d_a.krn vvv ######################
>     1-24:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>              25:               .         .            .       .           16b-
>              26:               .         .       8B- 8f       .            16a
>              27:               .         .            .       .        16b-JJ)
>              28:               *         *           *v      *v              *
>              29:             =42       =42          =42                    =42
>              30:               *         *      *clefF4                      *
>              31:              4T       4Ib           8r               (16ee-LL
>              32:               .         .            .                   16ff
>              33:               .         .       8G 8e-                  16gg)
>              34:               .         .            .                 16b'JJ
>              35:              4P       4IV           8r                (16ccLL
>              36:               .         .            .                   16dd
>              37:               .         .      8A- 8e-                 16ee-)
>              38:               .         .            .                16a-'JJ
>              39:             =43       =43          =43                    =43
>              40:               *         *           *^                      *
>              41:              4D       4Cc           8r    2BB-         (16gLL
>              42:               .         .            .       .           16a-
>              43:               .         .          (8G       .          16b-)
>              44:               .         .            .       .         16e'JJ
>              45:              4D       4V7         8A-L       .         (16fLL
>              46:               .         .            .       .            16g
>              47:               .         .         8FJ)       .          16a-)
>              48:               .         .            .       .         16d'JJ
>              49:               *         *           *v      *v              *
>              50:             =44       =44          =44                    =44
>              51:              2T        2I         8E-L                    4e-
>              52:               .         .        8BB-J                      .
>              53:               .         .         4EE-                     4r
>              54:              *-        *-           *-                     *-
>    ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
>    
>       humdrumR corpus of twenty pieces (two subcorpora: _1, _2).
>    
>       Data fields: 
>               *Token :: character
```

I’ve used the `c` command to create a vector with two REpath-patterns.
However, that was a lot of (bug-prone) typing…what if we combine these
multiple patterns with the Multi-Argument Approach from above?:

``` r
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn')
```

```
>    ###################### vvv B075_00_01_a.krn vvv ######################
>               1:  !!!COM: Beethoven
>               2:  !!!OTL: 7 Variations on a Quartet by Winter
>               3:  !!!Variation: Theme a
>               4:      **function    **harm       **kern                 **kern
>               5:               *         *      *clefG2                *clefG2
>               6:               *         *       *k[b-]                 *k[b-]
>               7:           *M2/4     *M2/4        *M2/4                  *M2/4
>               8:             *F:       *F:          *F:                    *F:
>               9:              4T        4I           4r               4aa 4ccc
>              10:              =1        =1           =1                     =1
>              11:              2T        2I           2f             8ff'L 8aa'
>              12:               .         .            .             8cc'J 8ff'
>              13:               .         .            .             8ff'L 8aa'
>              14:               .         .            .            8aa'J 8ccc'
>              15:              =2        =2           =2                     =2
>              16:              4T       4V7    (4c 4e 4g              (4gg 4bb-
>              17:              4T        4I          4f)               4ff) 4aa
>              18:              =3        =3           =3                     =3
>              19:              2T        2V           4c             8ee'L 8gg'
>              20:               .         .            .             8dd'J 8ff'
>              21:               .         .           4c             8cc'L 8ee'
>              22:               .         .            .             8dd'J 8ff'
>              23:              =4        =4           =4                     =4
>              24:              4T        4V           8c                8ee 8gg
>              25:               .         .           8r                     8r
>              26:              4T        4I           4r               4aa 4ccc
>              27:              =5        =5           =5                     =5
>              28:              2T        2I           2f             8ff'L 8aa'
>              29:               .         .            .             8cc'J 8ff'
>              30:               .         .            .             8ff'L 8aa'
>    31-45:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ###################### ^^^ B075_00_01_a.krn ^^^ ######################
>    
>           (eighteen more pieces...)
>    
>    ##################### vvv M354_01_04d_a.krn vvv ######################
>     1-24:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>              25:               .         .            .       .           16b-
>              26:               .         .       8B- 8f       .            16a
>              27:               .         .            .       .        16b-JJ)
>              28:               *         *           *v      *v              *
>              29:             =42       =42          =42                    =42
>              30:               *         *      *clefF4                      *
>              31:              4T       4Ib           8r               (16ee-LL
>              32:               .         .            .                   16ff
>              33:               .         .       8G 8e-                  16gg)
>              34:               .         .            .                 16b'JJ
>              35:              4P       4IV           8r                (16ccLL
>              36:               .         .            .                   16dd
>              37:               .         .      8A- 8e-                 16ee-)
>              38:               .         .            .                16a-'JJ
>              39:             =43       =43          =43                    =43
>              40:               *         *           *^                      *
>              41:              4D       4Cc           8r    2BB-         (16gLL
>              42:               .         .            .       .           16a-
>              43:               .         .          (8G       .          16b-)
>              44:               .         .            .       .         16e'JJ
>              45:              4D       4V7         8A-L       .         (16fLL
>              46:               .         .            .       .            16g
>              47:               .         .         8FJ)       .          16a-)
>              48:               .         .            .       .         16d'JJ
>              49:               *         *           *v      *v              *
>              50:             =44       =44          =44                    =44
>              51:              2T        2I         8E-L                    4e-
>              52:               .         .        8BB-J                      .
>              53:               .         .         4EE-                     4r
>              54:              *-        *-           *-                     *-
>    ##################### ^^^ M354_01_04d_a.krn ^^^ ######################
>    
>       humdrumR corpus of twenty pieces (two subcorpora: _1, _2).
>    
>       Data fields: 
>               *Token :: character
```

We get the same result! How did this work exactly? We’ve fed three
arguments to
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).
The first and last arguments (`"HumdrumData"` and `".*.krn"`) are length
one. However, the middle argument (`"...Variations"`) is of length two.
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
concatenates the three arguments together, making *two* separate
REpath-patterns:

``` r
"HumdrumData/BeethovenVariations/.*.krn"
```

**AND**

``` r
"HumdrumData/MozartVariations/.*.krn"
```

> Note: It is possible to write multiple patterns which match some (or
> all) of the same files. If the argument `multipleInstances = FALSE`,
> each unique file will only be read once (into the first matching
> pattern). If `multipleInstances = TRUE`, the same file(s) can be read
> more than once.

##### Pattern Names

Whenever we specify more than one REpath-pattern, humdrum$_{\mathbb{R}}$
gives them names which we can access in the `Label` field of the
resulting humdrum$_{\mathbb{R}}$ data object. You can choose these
labels by giving names to the patterns in your multi-pattern vectors:
Thus, we could write

``` r
readHumdrum('HumdrumData',
            c(Rap  = 'Rap/.*',
              Rock = 'RollingStone/.*')) -> rapAndRock
```

and these two separate patterns will be matched and read *and* the names
`"Rap"` and `"Rock"` will be associated with them in the resulting
`Label` field. This feature is very useful if you are working with
multiple heterogeneous datasets and you want to be able to apply some
analyses/parsing to only one of the data subsets.

#### Contains

Sometimes you want to only read humdrum files which contain particular
data. For instance, you might want to read only the files in a minor
key. One option is to read all the files in the corpus then filter out
the files you don’t want (see the [humdrumR filtering
vignette](https://humdrumR.ccml.gtcmt.gatech.edu/articles/Filtering.md)
to learn how). However, in some cases, it can save a lot of time and
effort to filter the files you want before parsing them. The `contains`
argument of
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
can be used in just this way! The contains argument must be a character
vector—each character string is treated as a regular expression, and
only files which contain matches to all of these regular expressions
will be read. Thus, we could only read pieces with minor keys by
writing:

``` r
readHumdrum('HumdrumData', 'BachChorales', 'chor.*.krn',
            contains = '\\*[a-g][-b#]*:') 
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
>           (one other piece...)
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
>       humdrumR corpus of three pieces.
>    
>       Data fields: 
>               *Token :: character
```

(This regular expression matches standard humdrum tandem interpretations
for minor keys.)

### Finding Files

[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
interprets REpath-patterns and finds files using the helper function
[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).
If you are not sure where your humdrum files are, or what pattern you
want to use to find them, you might start be using
[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).
[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
takes the same input as
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
but doesn’t fully parse the input into humdrum$_{\mathbb{R}}$. Instead,
it returns a `data.table` containing matching filenames (if any) and raw
file content.

If you use the `verbose = TRUE` option for either
[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
or
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
a complete list of all the matches files will be printed. This is useful
to check if you are reading the files you *intend* to read.

#### Validation

[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
and by extension
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
are smart functions that will ignore non-humdrum files matched by your
pattern. Non-text files (like `.pdf` or `.jpg`) will be ignored. Whats
more, text files which fail to conform to the humdrum syntax will not be
read either. You will see a message that checks which files are “valid
humdrum,” and tell you how many (if any) text files are not read.

If you are trying to read a file which you *think* should be valid
humdrum but
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
won’t read it, use
[`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md)
to see a detailed report of the problem. Humdrum$_{\mathbb{R}}$ won’t
read files with *any* humdrum-syntax violations, even relatively minor
ones. Once you see the problems that
[`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md)
is finding in your data, you’ll need to fix them yourself before you can
use humdrum$_{\mathbb{R}}$ on the data. Check out the
[validateHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/articles/reference/validateHumdrum.md)
documentation to learn more about humdrum$_{\mathbb{R}}$’s data
validation tools, and how they can be used to identify errors in your
humdrum data.

> Note that
> [`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md)
> does not automatically check if the humdrum *data* is encoded
> correctly or makes sense, just that it is structurally sound, in
> accordance with the humdrum syntax.

### Reading files

So, what actually happens when we run
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)?
By now, you’ve seen that
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
prints out a few messages describing its process.

1.  First, the files which match your search patterns (if any) are
    identified, and their text is read into R.
2.  Next, these files are checked to see if they are valid humdrum
    files.
3.  The files are parsed—i.e., translated into humdrum$_{\mathbb{R}}$’s
    [data.table
    backend](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumTable.md).
    This step takes the longest by far, as it includes parsing all the
    humdrum files’ data tokens, interpretations, reference records, and
    (worst of all) spine paths. Indeed, reading humdrum data with spine
    paths will take significantly longer than reading pathless data. To
    save yourself some time, you can potentially use the `tandems` and
    `reference` arguments (see “Parsing Metadata” below).

### Parsing Metadata

By default, humdrum$_{\mathbb{R}}$ parses all metadata in the humdrum
files it reads. This includes the true global metadata for files,
encoded in humdrum *reference records*, as well as the more local
*tandem interpretations*.

#### Reference Records

By default, humdrum$_{\mathbb{R}}$ parses all reference records in the
files it reads, creating a unique field for each reference key. (If
there are more than one instance of any given key, they are combined in
a string separated by semicolons.) If there are a lot of unique
reference keys in a dataset, they can end up taking a lot of memory in a
humdrum table. In these cases, you might find it useful to limit
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
to only parsing those reference records which you are actually going to
use. This can be accomplished quite easily with the `reference`
argument.

The `reference` argument defaults to `"all"`, which means all reference
records are parsed. However, reference can also be a character vector of
reference codes.

``` r
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn') |> fields('Reference')
>            Name     Class      Type Selected GroupedBy Complement
>          <char>    <char>    <char>    <int>    <lgcl>     <lgcl>
>    1:       COM character Reference        0     FALSE      FALSE
>    2:       OTL character Reference        0     FALSE      FALSE
>    3: Variation character Reference        0     FALSE      FALSE
```

Thus,

``` r
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn',
            reference = 'COM') |> fields('Reference')
>         Name     Class      Type Selected GroupedBy Complement
>       <char>    <char>    <char>    <int>    <lgcl>     <lgcl>
>    1:    COM character Reference        0     FALSE      FALSE
```

will only parse the `COM` reference record in each piece. We can use
this to rename our reference fields if we like—in this case,

``` r
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn',
            reference = c(Composer = 'COM')) |> fields('Reference')
>           Name     Class      Type Selected GroupedBy Complement
>         <char>    <char>    <char>    <int>    <lgcl>     <lgcl>
>    1: Composer character Reference        0     FALSE      FALSE
```

the `COM` reference records will be parsed but saved into a field called
`Composer` instead of `COM`.

If you don’t want to parse *any* reference records, specify
`reference = NULL`.

#### Tandem Interpretations

Tandem interpretations are used in humdrum data to represent “local,”
real-time metainformation associated with specific spines—review our
[humdrum
syntax](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md)
vignette for more details.

Humdrum$_{\mathbb{R}}$ can read any arbitrary tandem interpretations in
humdrum data. However, if non-standard interpretations are used,
humdrum$_{\mathbb{R}}$ has no way of knowing how to parse them.

By default, humdrum$_{\mathbb{R}}$ always reads tandem interpretations
into a field called `Tandem`. This field tabulates all tandem
interpretations that have so far appeared in a spine in order,
concatenating them into a single comma-delimited, from most recent to
most distant. For instance, the `Tandem` field for the spine

    **kern
    *C:
    *M4/4
    *MM100
    c e
    c g
    *D:
    d f#
    *-

is parsed as

    ""
    C:
    M4/4,C:
    MM100,M4/4,C:
    MM100,M4/4,C:
    MM100,M4/4,C:
    D:,MM100,M4/4,C:
    D:,MM100,M4/4,C:
    D:,MM100,M4/4,C:

So, the interpretations pile up into a longer and longer string as a
piece progresses. Notice that the `"D:"` and `"C:"` interpretations get
piled up separately, just like any other interpretation, even though
they are of the same tandem class, and the `"D:"` should supersede the
`"C:"`. Why do this? Well, *in general* there is no way to know if an
arbitrary tandem interpretations are of the same class. If you create
some humdrum data with new interpretations in it, humdrum$_{\mathbb{R}}$
doesn’t know how to parse them, so will just pile them up like this in
the `Tandem` field.

Fortunately, humdrum$_{\mathbb{R}}$*does* know how to parse many
standard tandem interpretations. If you run the command

``` r
knownInterpretations[Type == 'Tandem'] 
>                         Name Exclusive
>                       <char>    <char>
>     1:                   Key          
>     2:          KeySignature          
>     3:                  Clef          
>     4:         TimeSignature          
>     5:           Mensuration          
>     6:                   BPM          
>     7:              Timebase          
>     8:            Instrument          
>     9:       InstrumentClass          
>    10: TransposingInstrument          
>                                                         RE Pretty   Type
>                                                     <char> <char> <char>
>     1: [A-Ga-g][-#b]*:(mix|dor|phr|lyd|loc)?([#bn]?[1-7])*     _: Tandem
>     2:                              k\\[([a-g][#-]* *)*\\]   k[_] Tandem
>     3:                                   clef[A-G]v*[1-5]*  clef_ Tandem
>     4:                     M[1-9][0-9]*/((16)|(32)|[1248])   M_/_ Tandem
>     5:                                      met\\([^)]*\\) met(_) Tandem
>     6:                                            MM[0-9]+    MM_ Tandem
>     7:                     tb[1-9][0-9]*%?[1-9]?[0-9]*[.]*    tb_ Tandem
>     8:                                         I[^C,][^,]*     I_ Tandem
>     9:                                             IC[^,]+    IC_ Tandem
>    10:                                            ITr[^,]*   ITr_ Tandem
```

we see that there are currently ten classes of tandem interpretations
that `humdrumR` recognizes, each associated with a regular expression.
Thus, if we really apply
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
to this file

    **kern
    *C:
    *M4/4
    *MM100
    c e
    c g
    *D:
    d f#
    *-

it will recognize that the tokens `"C:"` and `"D:"` both match the known
regular expression for the `Key` tandem interpretation, and parse them
into a field called `Key`, which would look like:

    NA
    C:
    C:
    C:
    C:
    C:
    D:
    D:
    D:

Likewise, we’d get both `TimeSignature` and `BPM` fields.

##### Custom Tandems

[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
has a `tandems` argument which allows us take control of the tandem
interpretation parsing process. `tandems` is a character vector,
defaulting to `known`, which parses all known interpretations into their
own fields (following the `knownInterpretations` table). We can use
`tandems` two ways. 1.) We can remove `"known"` from the vector and
specify the `Name` of specific tandem interpretations we do want to
parse. For instance, if we write `tandem = "Clef"`, the `Clef` pattern
from the `knownInterpretations` table will be parsed, (but not any of
the others). 2.) If a character string in `tandems` is not `"known"` or
an exact match for a `Name` in `knownInterpretations`, it is instead
treated as a regular expression to match a new tandem interpretation
class. For instance, `tandems = "[Aa]pple|[Bb]anana"` would match
“\*Apple”, “\*apple”, “\*Banana”, or “\*banana,” parsing these four
patterns into their own field (called `[Aa]pple|[Bb]anana]`). We could
give this field a name by writing
`tandems = c(Fruit = "[Aa]pple|[Bb]anana")`. (In fact, we can rename
known interpretations as well, for instance writing
`tandems = c(Meter = "TimeSignature"`.)

If `tandems = NULL`, no tandem interpretations are parsed at all—this
can be used to shorten parsing time.

## Writing humdrum data

The complement of
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
is, of course,
[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md),
which takes a humdrum$_{\mathbb{R}}$ data object and writes it to new
data files. The content of the files which exactly match what is shown
to you in the “humdrum” view, when you print humdrum$_{\mathbb{R}}$, so
whatever fields are
[selected](https://humdrumR.ccml.gtcmt.gatech.edu/reference/DataFields.html#selecting-fields%20Getting%20started%20with%20humdrumR%20article;%20selecting%20fields%20section%22)
are written to files. The only question, then, is what files it writes
to.

[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
takes the original file names (and directories) of the data you read as
the starting point for determining output file names. The default, is
simply to take the original file name and adds the prefix “`humdrumR_`”
at the beginning of each file name.
[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
has a bunch of arguments (`prefix`, `affix`, `renamer`, and `extension`)
which can be used to modify the output file names—see
[writeHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md "writeHumdrum documentation")
for details.

The `directory` argument can, of course, be used to change the output
directory files are written to.

------------------------------------------------------------------------

Let’s say we’d like to write our `chorales` data, but with scale degree
data only. We’ll name these files by adding the affix “\_modified,” and
setting the extension to be “.deg” instead of “.kern.”

``` r
readHumdrum("HumdrumData", "BachChorales", "chor.*.krn") |>
  mutate(Deg = deg(Token, simple = TRUE)) |>
  writeHumdrum(prefix = '', affix = '_modified', extension = 'deg')
>    Writing humdrum data...
>    Determining validity of new filenames...
>    Preparing text...Writing 10 files...done!
```

The results are files (in the original directory) names
`chor001_modified.deg`, `chor002_modified.deg`, etc.

### Overwriting

Generally, overwriting your original humdrum data is probably a very bad
idea (make sure to back up your data!), which is why
[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
generates totally new files by default. To avoid accidental overwrites,
[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
will never overwrite files as long as the `overwrite` argument is
`FALSE`—which is the default. Even if you specify `overwrite = TRUE`,
[`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
will ask you to confirm before proceeding.

### !!!EMD

`!!!EMD:` is a humdrum reference code, meaning “*Document modification
description*.” This code is used to keep track of modifications to
humdrum datasets. Humdrum$_{\mathbb{R}}$ will, by default, insert an
`!!!EMD` record into any files it writes, saying: “Edited using
humdrumR, version 0.7.1.1 on (*current data/time*).”
