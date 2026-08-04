# Getting to know your humdrum data

Welcome to “Getting to know your humdrum data”! This article explains
how you can find essential metainformation about your data with
humdrum$_{\mathbb{R}}$: how much data is there, and how is it
structured? If you don’t understand your data, you won’t be able to do
intelligent analyses of it.

This article, like all of our articles, closely parallels information in
humdrum$_{\mathbb{R}}$’s detailed code documentation, which can be found
in the
“[Reference](https://humdrumr.ccml.gtcmt.gatech.edu/reference/index.html#summarizing "HumdrumR function reference, Summarizing")”
section of the
humdrum$_{\mathbb{R}}$[homepage](https://humdrumR.ccml.gtcmt.gatech.edu).
You can also find this information within R, once humdrum$_{\mathbb{R}}$
is loaded, using
[`?summary.humdrumR`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).

## Know your data

Before *any* data analysis, you should **know your data**. What’s in the
data? How much data is there? How is it formatted and encoded? Are there
errors or ambiguities? How was it sampled? If you can’t answer these
questions, you can’t make intelligent or useful scholarly inferences
using the data.

After you’ve done your background research and read up on the details of
the data you are working with, hopefully answering the questions posed
above, there is one more step to do before you really start analysis:
inspect (some of) the data. The fact that the [humdrum
syntax](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md "The humdrum syntax article")
is human readable is one of the great strengths of the humdrum
ecosystem! Open some of the data files up in a text-editor, or perhaps
drop them into the [Verovio Humdrum
Viewer](https://verovio.humdrum.org/). Of course, you can only look at
so much data “by eye”—still, its good practice to inspect as much of the
data as you can, selecting files at random and skimming through them to
see if they look the way you *think* they should look.

### Data summaries

Once your data is
[read](https://humdrumR.ccml.gtcmt.gatech.edu/articles/ReadWrite.md "Reading and writing humdrum data, article"),
the next step is to use humdrum$_{\mathbb{R}}$ to get high-level
summaries of the content of *all* the files in your data.

Humdrum$_{\mathbb{R}}$ defines a number of tools to quickly summarize
the structure and content of a humdrum data set. One of the most basic
functions in R is
[`summary()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md);
Calling
[`summary()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md)
on a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
will print a concise version of the output of humdrum$_{\mathbb{R}}$’s
[five summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
which are described in detail below. Let’s load our built-in
Bach-chorale dataset, which we’ll use throughout this article, and call
[`summary()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md):

``` r
setwd(humdrumRroot)
readHumdrum('HumdrumData/BachChorales/chor0.*') -> chorales

summary(chorales)
>           Summary of humdrumR corpus "chorales":
>    
>    ###### Census of GLIMDdSE records:
>                                   Records   Tokens (unique)   Characters    ***
>                                     2,610    9,486    (520)       34,573    ***
>    
>    
>                               (***one column not displayed due to screensize***)
>    
>    ###### Reference records :
>                               AGN  CDT  COM  EED  EEV  EMD  OPR  OTL@@DE    ***
>                         Any:   20   20   20   20   20   10    2       20    ***
>                         Sum:   20   20   20   20   20   10    2       20    ***
>                      Unique:    1    1    1    1    1    1    1       10    ***
>    
>    
>                               (seven columns not displayed due to screensize***)
>    
>    ###### Spine structure:
>                                     20 files with 4 spines
>    
>    
>    ###### Interpretation content:
>                                 {X}  **deg  **kern    BPM   Clef    ***
>                          Hits:          10      10     20     20    ***
>    
>                      (***six columns not displayed due to screensize***)
>        Tallies:
>                  {A} =       **deg, **deg, **deg, **deg:  10
>                  {B} =   **kern, **kern, **kern, **kern:  10
```

A lot of information, huh? There rest of this article will walk through
this output and [the specific functions that generate
it](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md "humSummary documentation page").

## Summarizing Structure

The most basic information you’ll want about a humdrum dataset is how
“big” it is—how much data is there? Printing a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
on the command line will always tell you how many files there are in
your data: you can also call
[`length()`](https://rdrr.io/r/base/length.html) to get this number. The
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
function, however, gives us much more detail about the size of the data,
telling us how many records, tokens, and characters there are:

``` r
chorales |> census()
>    
>    ###### Census of GLIMDdSE records in humdrumR corpus "chorales" (twenty pieces):
>    ###### Grouped by twenty Pieces:
>                                   Records   Tokens (unique)   Characters    ***
>    chor001_modified.deg  1 [ 1]       134      485     (71)         1566    ***
>             chor001.krn  2 [ 2]       133      484    (148)         1910    ***
>    chor002_modified.deg  3 [ 3]       125      452     (65)         1478    ***
>             chor002.krn  4 [ 4]       124      451    (148)         1897    ***
>    chor003_modified.deg  5 [ 5]       111      387     (68)         1606    ***
>             chor003.krn  6 [ 6]       110      386    (132)         1867    ***
>    chor004_modified.deg  7 [ 7]       104      368     (62)         1384    ***
>             chor004.krn  8 [ 8]       103      367    (129)         1711    ***
>    chor005_modified.deg  9 [ 9]       173      644     (70)         1696    ***
>             chor005.krn 10 [10]       172      643    (159)         2233    ***
>    chor006_modified.deg 11 [11]        78      264     (56)         1134    ***
>             chor006.krn 12 [12]        77      263    (125)         1313    ***
>    chor007_modified.deg 13 [13]       180      672     (89)         1883    ***
>             chor007.krn 14 [14]       179      671    (168)         2415    ***
>    chor008_modified.deg 15 [15]       172      640     (69)         1743    ***
>             chor008.krn 16 [16]       171      639    (165)         2298    ***
>    chor009_modified.deg 17 [17]       132      480     (65)         1498    ***
>             chor009.krn 18 [18]       131      479    (167)         1923    ***
>    chor010_modified.deg 19 [19]       101      356     (64)         1419    ***
>             chor010.krn 20 [20]       100      355    (133)         1599    ***
>                                   Records   Tokens (unique)   Characters    ***
>    ###### Totals:
>                                     2,610    9,486    (520)       34,573    ***
>    
>    ###### Census of GLIMDdSE records in humdrumR corpus "chorales" (twenty pieces):
>    
>                               (***one column not displayed due to screensize***)
```

The corpus contains, in total, 9,486 tokens in 2,610 records. The
`(unique)` column tells us how many unique `Tokens` there are per file
(and overall, at the bottom). The `(per token)` column indicates the
average number of characters in each file, and overall.

> Notice that
> [`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
> defaults to counting all records/tokens. If you want to count only
> data tokens, specify `census(chorales, dataTypes = 'D')`.

### Spines and Interpretations

To work with humdrum data, you really *need* to know how many spines
(and [spine
paths](https://humdrumR.ccml.gtcmt.gatech.edu/articles/ComplexSyntax.md "Complex syntax article"))
are present in the data, and what interpretations are present. The
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
and
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
functions give us just this information!

``` r
spines(chorales)
>    
>    ###### Spine structure in humdrumR corpus "chorales" (twenty piecess):
>                                 Spines  + Paths  In  *^  *v
>    ########################################################
>      chor001_modified.deg [ 1]       4        0            
>               chor001.krn [ 2]       4        0            
>      chor002_modified.deg [ 3]       4        0            
>               chor002.krn [ 4]       4        0            
>      chor003_modified.deg [ 5]       4        0            
>               chor003.krn [ 6]       4        0            
>      chor004_modified.deg [ 7]       4        0            
>               chor004.krn [ 8]       4        0            
>      chor005_modified.deg [ 9]       4        0            
>               chor005.krn [10]       4        0            
>      chor006_modified.deg [11]       4        0            
>               chor006.krn [12]       4        0            
>      chor007_modified.deg [13]       4        0            
>               chor007.krn [14]       4        0            
>      chor008_modified.deg [15]       4        0            
>               chor008.krn [16]       4        0            
>      chor009_modified.deg [17]       4        0            
>               chor009.krn [18]       4        0            
>      chor010_modified.deg [19]       4        0            
>               chor010.krn [20]       4        0            
>    ########################################################
>                                 Spines  + Paths  In  *^  *v
>    
>                               Tallies:
>                                     20 files with 4 spines
>    
>    ###### Spine structure in humdrumR corpus "chorales" (twenty piecess):

interpretations(chorales)
>    
>    ###### Interpretation content in humdrumR corpus "chorales" (twenty pieces):
>                                 {X}  **deg  **kern    BPM   Clef    ***
>                                            (Total.Unique.Spines)
>      chor001_modified.deg [ 1]  {A}      4       0  4.1.4  4.3.4    ***
>               chor001.krn [ 2]  {B}      0       4  4.1.4  4.3.4    ***
>      chor002_modified.deg [ 3]  {A}      4       0  4.1.4  4.3.4    ***
>               chor002.krn [ 4]  {B}      0       4  4.1.4  4.3.4    ***
>      chor003_modified.deg [ 5]  {A}      4       0  4.1.4  4.3.4    ***
>               chor003.krn [ 6]  {B}      0       4  4.1.4  4.3.4    ***
>      chor004_modified.deg [ 7]  {A}      4       0  4.1.4  4.3.4    ***
>               chor004.krn [ 8]  {B}      0       4  4.1.4  4.3.4    ***
>      chor005_modified.deg [ 9]  {A}      4       0  4.1.4  4.3.4    ***
>               chor005.krn [10]  {B}      0       4  4.1.4  4.3.4    ***
>      chor006_modified.deg [11]  {A}      4       0  4.1.4  4.3.4    ***
>               chor006.krn [12]  {B}      0       4  4.1.4  4.3.4    ***
>      chor007_modified.deg [13]  {A}      4       0  4.1.4  4.3.4    ***
>               chor007.krn [14]  {B}      0       4  4.1.4  4.3.4    ***
>      chor008_modified.deg [15]  {A}      4       0  4.1.4  4.3.4    ***
>               chor008.krn [16]  {B}      0       4  4.1.4  4.3.4    ***
>      chor009_modified.deg [17]  {A}      4       0  4.1.4  4.3.4    ***
>               chor009.krn [18]  {B}      0       4  4.1.4  4.3.4    ***
>      chor010_modified.deg [19]  {A}      4       0  4.1.4  4.3.4    ***
>               chor010.krn [20]  {B}      0       4  4.1.4  4.3.4    ***
>                                            (Total.Unique.Spines)
>    ###### Totals:
>                                 {X}  **deg  **kern    BPM   Clef    ***
>                          Hits:          10      10     20     20    ***
>    
>                      (***six columns not displayed due to screensize***)
>        Tallies:
>                  {A} =       **deg, **deg, **deg, **deg:  10
>                  {B} =   **kern, **kern, **kern, **kern:  10
>    
>    ###### Interpretation content in humdrumR corpus "chorales" (twenty pieces):
```

For this toy dataset of 10 chorales, the output of
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
is pretty boring: all the chorales have four spines, with no spine
paths. The
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
output is also boring, as we see that all 10 files have four `**kern`
exclusive interpretations; However,
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
also tells about the tandem interpretations it recognizes—in this case,
tempo, key, instrument, and time signature information.

------------------------------------------------------------------------

The `chorales` dataset is structurally homogeneous, which is generally a
*good* thing—it’s much easier to analyze this sort of data! However,
some humdrum datasets are more heterogeneous, which is where
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
and
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
come more in handy. Let’s switch over to another one of our pre-packaged
corpora, the Beethoven/Mozart variations (see the
[read/write](https://humdrumR.ccml.gtcmt.gatech.edu/articles/ReadWrite.md "Reading and writing humdrum data article")):

``` r
readHumdrum(humdrumRroot, 'HumdrumData/.*Variations/.*.krn') -> variations

spines(variations)
>    
>    ###### Spine structure in humdrumR corpus "variations" (twenty piecess):
>                              Spines  + Paths  In  *^  *v
>    #####################################################
>       B075_00_01_a.krn [ 1]       4        0            
>       B075_00_02_a.krn [ 2]       4        0            
>       B075_00_03_a.krn [ 3]       4        2   2   2   2
>       B075_00_04_a.krn [ 4]       4        1   1   1   1
>       B075_00_05_a.krn [ 5]       4        0            
>       B075_00_06_a.krn [ 6]       4        0            
>       B075_01_01_a.krn [ 7]       4        0            
>       B075_01_02_a.krn [ 8]       4        0            
>       B075_01_03_a.krn [ 9]       4        1   1   2   2
>       B075_01_04_a.krn [10]       4        0            
>       B075_01_05_a.krn [11]       4        0            
>       B075_01_06_a.krn [12]       4        0            
>      M354_00_01a_a.krn [13]       4        1   1   1   1
>      M354_00_02b_a.krn [14]       4        1   1   1   1
>      M354_00_03c_a.krn [15]       4        2   2   3   3
>      M354_00_04d_a.krn [16]       4        1   1   1   1
>      M354_01_01a_a.krn [17]       4        1   1   1   1
>      M354_01_02b_a.krn [18]       4        1   1   1   1
>      M354_01_03c_a.krn [19]       4        1   1   1   1
>      M354_01_04d_a.krn [20]       4        1   1   2   2
>    #####################################################
>                              Spines  + Paths  In  *^  *v
>    
>                            Tallies:
>                                  20 files with 4 spines (9 with 0 paths, 9 with 1 path, and 2 with 2 paths)
>    
>    ###### Spine structure in humdrumR corpus "variations" (twenty piecess):
```

Now we see something more interesting. Again, all the files have four
spines, but eleven of the files include spine paths (`"9 with 1 path"`
and `"2 with 2 paths"`).

Let’s check out the output of
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md):

``` r
interpretations(variations)
>    
>    ###### Interpretation content in humdrumR corpus "variations" (twenty pieces):
>                              {X}  **function  **harm  **kern   Clef    ***
>                                               (Total.Unique.Spines)
>       B075_00_01_a.krn [ 1]  {A}           1       1       2  3.2.3    ***
>       B075_00_02_a.krn [ 2]  {A}           1       1       2  2.2.2    ***
>       B075_00_03_a.krn [ 3]  {A}           1       1       2  2.2.2    ***
>       B075_00_04_a.krn [ 4]  {A}           1       1       2  2.2.2    ***
>       B075_00_05_a.krn [ 5]  {A}           1       1       2  2.2.2    ***
>       B075_00_06_a.krn [ 6]  {A}           1       1       2  2.2.2    ***
>       B075_01_01_a.krn [ 7]  {A}           1       1       2  3.2.3    ***
>       B075_01_02_a.krn [ 8]  {A}           1       1       2  2.2.2    ***
>       B075_01_03_a.krn [ 9]  {A}           1       1       2  2.2.2    ***
>       B075_01_04_a.krn [10]  {A}           1       1       2  2.2.2    ***
>       B075_01_05_a.krn [11]  {A}           1       1       2  2.1.2    ***
>       B075_01_06_a.krn [12]  {A}           1       1       2  4.2.4    ***
>      M354_00_01a_a.krn [13]  {A}           1       1       2  2.2.2    ***
>      M354_00_02b_a.krn [14]  {A}           1       1       2  2.2.2    ***
>      M354_00_03c_a.krn [15]  {A}           1       1       2  2.2.2    ***
>      M354_00_04d_a.krn [16]  {A}           1       1       2  2.2.2    ***
>      M354_01_01a_a.krn [17]  {A}           1       1       2  2.2.2    ***
>      M354_01_02b_a.krn [18]  {A}           1       1       2  2.2.2    ***
>      M354_01_03c_a.krn [19]  {A}           1       1       2  5.2.5    ***
>      M354_01_04d_a.krn [20]  {A}           1       1       2  3.2.3    ***
>                                               (Total.Unique.Spines)
>    ###### Totals:
>                              {X}  **function  **harm  **kern   Clef    ***
>                       Hits:               20      20      20     20    ***
>    
>                       (***three columns not displayed due to screensize***)
>        Tallies:
>                  {A} =   **function, **harm, **kern, **kern:  20
>    
>    ###### Interpretation content in humdrumR corpus "variations" (twenty pieces):
```

Ah, this time we see that each file has a `**function` and a `**harm`
spine, as well as two `**kern` spines. In fact, the “Tallies” at the
bottom tells us that all 20 files have the same exclusive
interpretations (in the same order), which humdrum$_{\mathbb{R}}$ labels
`{A}`: `**function, **harm, **kern, **kern`.

## Summarizing Metadata

Another question to ask about a dataset is what kind of meta data is
encoded in the data’s [reference
records](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md "The humdrum syntax").
The function
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)
answers this question for us:

``` r
reference(chorales)
>    
>    ###### Reference records in humdrumR corpus "chorales" (twenty pieces):
>    ###### By piece:
>                               AGN  CDT  COM  EED  EEV  EMD  OPR  OTL@@DE    ***
>    chor001_modified.deg [ 1]    1    1    1    1    1    1    0        1    ***
>             chor001.krn [ 2]    1    1    1    1    1    0    0        1    ***
>    chor002_modified.deg [ 3]    1    1    1    1    1    1    0        1    ***
>             chor002.krn [ 4]    1    1    1    1    1    0    0        1    ***
>    chor003_modified.deg [ 5]    1    1    1    1    1    1    1        1    ***
>             chor003.krn [ 6]    1    1    1    1    1    0    1        1    ***
>    chor004_modified.deg [ 7]    1    1    1    1    1    1    0        1    ***
>             chor004.krn [ 8]    1    1    1    1    1    0    0        1    ***
>    chor005_modified.deg [ 9]    1    1    1    1    1    1    0        1    ***
>             chor005.krn [10]    1    1    1    1    1    0    0        1    ***
>    chor006_modified.deg [11]    1    1    1    1    1    1    0        1    ***
>             chor006.krn [12]    1    1    1    1    1    0    0        1    ***
>    chor007_modified.deg [13]    1    1    1    1    1    1    0        1    ***
>             chor007.krn [14]    1    1    1    1    1    0    0        1    ***
>    chor008_modified.deg [15]    1    1    1    1    1    1    0        1    ***
>             chor008.krn [16]    1    1    1    1    1    0    0        1    ***
>    chor009_modified.deg [17]    1    1    1    1    1    1    0        1    ***
>             chor009.krn [18]    1    1    1    1    1    0    0        1    ***
>    chor010_modified.deg [19]    1    1    1    1    1    1    0        1    ***
>             chor010.krn [20]    1    1    1    1    1    0    0        1    ***
>                               AGN  CDT  COM  EED  EEV  EMD  OPR  OTL@@DE    ***
>    
>    ###### Totals:
>                         Any:   20   20   20   20   20   10    2       20    ***
>                         Sum:   20   20   20   20   20   10    2       20    ***
>                      Unique:    1    1    1    1    1    1    1       10    ***
>    
>    ###### Reference records in humdrumR corpus "chorales" (twenty pieces):
>    
>                               (seven columns not displayed due to screensize***)
```

We see that all ten chorale files have, for example `COM` and `CDT`
reference records, but only two have the `OTL@@EN` record. Not sure what
those codes mean? You can also call
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)
on a `character`-string for a reference code:

``` r
reference('COM')
>    
>    (Authorship Information)
>    
>       !!!COM  =  Composer's name
>    
>       Examples:
>               x!!COM: Chopin, Fryderyk; Chopin, Frederick
>               x!!COM1: Composer, A. 
>               x!!COM2: Composer, B.

reference('CDT')
>    
>    (Authorship Information)
>    
>       !!!CDT  =  Composer's dates
```

To see the actual reference records themselves, you can index the result
of the call to
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)
by column or row. For example, to see all the `ODT@@DE` records:

``` r
reference(chorales)[ , 'OTL@@DE']
>    
>    ###### Reference records in humdrumR corpus "chorales[, j]" (twenty pieces):
>    ###### By piece:
>                              OTL@@DE
>    chor001_modified.deg [ 1]      Aus meines Herzens Grunde
>            chor001.krn [ 2]   Aus meines Herzens Grunde
>    chor002_modified.deg [ 3]      Ich dank dir, lieber Herre
>            chor002.krn [ 4]   Ich dank dir, lieber Herre
>    chor003_modified.deg [ 5]      Ach Gott, vom Himmel sieh darein
>            chor003.krn [ 6]   Ach Gott, vom Himmel sieh darein
>    chor004_modified.deg [ 7]      Es ist das Heil uns kommen her
>            chor004.krn [ 8]   Es ist das Heil uns kommen her
>    chor005_modified.deg [ 9]      An Wasserfl&uuml;ssen Babylon
>            chor005.krn [10]   An Wasserfl&uuml;ssen Babylon
>    chor006_modified.deg [11]      Christus, der ist mein Leben
>            chor006.krn [12]   Christus, der ist mein Leben
>    chor007_modified.deg [13]      Nun lob, mein Seel, den Herren
>            chor007.krn [14]   Nun lob, mein Seel, den Herren
>    chor008_modified.deg [15]      Freuet euch, ihr Christen alle
>            chor008.krn [16]   Freuet euch, ihr Christen alle
>    chor009_modified.deg [17]      Ermuntre dich, mein schwacher Geist
>            chor009.krn [18]   Ermuntre dich, mein schwacher Geist
>    chor010_modified.deg [19]      Aus tiefer Not schrei ich zu dir
>            chor010.krn [20]   Aus tiefer Not schrei ich zu dir
>                              OTL@@DE
>    
>    ###### Totals:
>                        Any:  20
>                        Sum:  20
>                     Unique:  10
>    
>    ###### Reference records in humdrumR corpus "chorales[, j]" (twenty pieces):
```

Or to see all the reference records for the third file:

``` r
reference(chorales)[3, ]
>    
>    ###### Reference records in humdrumR corpus "chorales[i]" (one piece):
>    chor002_modified.deg [3]
>            AGN:   chorale
>            CDT:   1685/02/21/-1750/07/28/
>            COM:   Bach, Johann Sebastian
>            EED: Craig Stuart Sapp
>            EEV: 2009/05/22
>            EMD: Edited in humdrumR 0.7.1.1 on 2026-08-03
>        OTL@@DE:   Ich dank dir, lieber Herre
>            PC#:   2
>            SCT:   BWV 347
>            SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>            YOR: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
>        hum2abc: -Q ''
>          title: @{PC#}. @{OTL@@DE}
```

## Summarizing Data

The next thing to do, when getting started with a humdrum$_{\mathbb{R}}$
data analysis, is to get a sense of the data *content* itself. What
tokens does our data actually contain? R’s
[`unique()`](https://rdrr.io/r/base/unique.html),
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
and [`sort()`](https://rdrr.io/r/base/sort.html) functions are perfect
for this. We’ll need to use the some techniques from the [Data
Fields](https://humdrumR.ccml.gtcmt.gatech.edu/articles/DataFields.html#working-with-fields "Data fields article")
article, so review that if you don’t understand the following!

Let’s get the unique values, sorted:

``` r

chorales |>
  with(unique(Token)) |>
  sort()
```

```
>      [1] "[2d"     "[2e"     "[4a"     "[4A"     "[4B"     "[4c"     "[4d"    
>      [8] "[4e"     "[4E"     "[4f"     "[4f#"    "[4g"     "[4G"     "[8cJ"   
>     [15] "[8CJ"    "[8gJ"    "1"       "1+"      "16AL"    "16B-Jk"  "16b-XJJ"
>     [22] "16BBJJ"  "16BJJ"   "16C#L"   "16c#LL"  "16ccL"   "16ccLL"  "16d#JJ" 
>     [29] "16ddJJ"  "16dJJ"   "16EJJ"   "16eL"    "16F#L"   "1e;"     "2"      
>     [36] "2-"      "2.a;"    "2.A;"    "2.AA;"   "2.b"     "2.b;"    "2.B;"   
>     [43] "2.BB;"   "2.c;"    "2.C#"    "2.c#;"   "2.d"     "2.d;"    "2.e"    
>     [50] "2.e;"    "2.ee"    "2.f;"    "2.f#;"   "2.FF;"   "2.g;"    "2.GG;"  
>     [57] "2+"      "2a"      "2A"      "2a-;"    "2A-;"    "2a;"     "2A;"    
>     [64] "2AA-;"   "2AA;"    "2b"      "2B"      "2b-;"    "2b;"     "2B;"    
>     [71] "2BB"     "2BB-;"   "2BB;"    "2c"      "2C"      "2c;"     "2C;"    
>     [78] "2c#"     "2c#;"    "2cc"     "2cc#"    "2cc#;"   "2d"      "2D"     
>     [85] "2d-;"    "2d;"     "2D;"     "2d#"     "2D#"     "2d#;"    "2dd"    
>     [92] "2DnX"    "2e"      "2E"      "2e-;"    "2e;"     "2E;"     "2E#"    
>     [99] "2EE;"    "2f;"     "2f#"     "2F#"     "2f#;"    "2F#;"    "2FF;"   
>    [106] "2FF#;"   "2g"      "2G"      "2g;"     "2G;"     "2g#"     "2G#"    
>    [113] "2g#;"    "2G#;"    "2G#X;"   "2GG;"    "3"       "3-"      "4"      
>    [120] "4.a"     "4.a-"    "4.b"     "4.B"     "4.b-"    "4.BB"    "4.c"    
>    [127] "4.cc#"   "4.d"     "4.dd"    "4.e"     "4.e-"    "4.ee"    "4.f"    
>    [134] "4.f#"    "4.g"     "4+"      "4a"      "4A"      "4a-"     "4A-"    
>    [141] "4a-;"    "4A-;"    "4a-X"    "4a;"     "4A;"     "4a#"     "4A#"    
>    [148] "4AA"     "4AA-"    "4AA-;"   "4AA;"    "4AA#"    "4anX"    "4b"     
>    [155] "4B"      "4b-"     "4B-"     "4B-X"    "4b;"     "4B;"     "4B]"    
>    [162] "4BB"     "4BB-"    "4BB;"    "4c"      "4C"      "4c;"     "4C;"    
>    [169] "4c]"     "4c#"     "4C#"     "4c#;"    "4C#;"    "4cc"     "4cc;"   
>    [176] "4cc#"    "4ccnX"   "4CnX"    "4d"      "4D"      "4d-"     "4D-"    
>    [183] "4d;"     "4D;"     "4d]"     "4d#"     "4D#"     "4d#;"    "4dd"    
>    [190] "4DD"     "4dd-"    "4dd;"    "4dd#"    "4dnX"    "4DnX"    "4e"     
>    [197] "4E"      "4e-"     "4E-"     "4e-;"    "4e;"     "4E;"     "4e]"    
>    [204] "4E]"     "4e#"     "4E#"     "4e#;"    "4E#X"    "4ee"     "4EE"    
>    [211] "4ee-"    "4ee-X"   "4ee;"    "4EE;"    "4enX"    "4EnX"    "4f"     
>    [218] "4F"      "4f;"     "4F;"     "4f#"     "4F#"     "4f#;"    "4F#;"   
>    [225] "4F#X"    "4F#X;"   "4ff"     "4FF"     "4ff;"    "4FF;"    "4ff#"   
>    [232] "4FF#"    "4g"      "4G"      "4g-"     "4g;"     "4G;"     "4g]"    
>    [239] "4G]"     "4g#"     "4G#"     "4g#;"    "4G#;"    "4G#X"    "4G#X;"  
>    [246] "4gg"     "4GG"     "4GG;"    "4GG#"    "4gnX"    "4GnX"    "4r"     
>    [253] "4ry"     "5"       "5+"      "6"       "6-"      "7"       "7-"     
>    [260] "8.cL"    "8a"      "8A"      "8a-"     "8A-"     "8a-J"    "8A-J"   
>    [267] "8a-L"    "8A-L"    "8a-XJ"   "8A#"     "8a#J"    "8A#J"    "8AA"    
>    [274] "8AA-"    "8AAJ"    "8AAL"    "8aJ"     "8AJ"     "8aL"     "8AL"    
>    [281] "8aL]"    "8AL]"    "8AnXL"   "8b"      "8b-"     "8B-"     "8b-J"   
>    [288] "8B-J"    "8b-L"    "8BB"     "8BB-J"   "8BB-L"   "8BBJ"    "8BBL"   
>    [295] "8bJ"     "8BJ"     "8bL"     "8BL"     "8c"      "8C"      "8C#"    
>    [302] "8c#J"    "8C#J"    "8c#L"    "8C#L"    "8c#XJ"   "8cc"     "8cc#J"  
>    [309] "8cc#L"   "8ccJ"    "8ccL"    "8cJ"     "8CJ"     "8cL"     "8CL"    
>    [316] "8cL]"    "8CL]"    "8cnXJ"   "8d"      "8D"      "8d-"     "8D-"    
>    [323] "8D-J"    "8D-L"    "8d-XJ"   "8d#J"    "8D#J"    "8d#L"    "8D#L"   
>    [330] "8dd"     "8dd#J"   "8ddJ"    "8ddL"    "8dJ"     "8DJ"     "8dL"    
>    [337] "8DL"     "8dL]"    "8dnJ"    "8e"      "8E"      "8E-"     "8e-J"   
>    [344] "8E-J"    "8e-L"    "8E-L"    "8EEJ"    "8eeL"    "8EEL"    "8eJ"    
>    [351] "8EJ"     "8eL"     "8EL"     "8eL]"    "8EL]"    "8f"      "8F"     
>    [358] "8f#"     "8F#"     "8f#J"    "8F#J"    "8f#L"    "8F#L"    "8f#L]"  
>    [365] "8F#XJ"   "8f#XL"   "8FF#J"   "8FFL"    "8fJ"     "8FJ"     "8fL"    
>    [372] "8FL"     "8fL]"    "8FnXL"   "8g"      "8G"      "8g#"     "8G#"    
>    [379] "8g#J"    "8G#J"    "8g#L"    "8G#L"    "8g#XJ"   "8GG"     "8GGJ"   
>    [386] "8GGL"    "8gJ"     "8GJ"     "8gL"     "8GL"     "8gL]"    "8GL]"   
>    [393] "8gnXL"   "8GnXL"
```

Unlike [`unique()`](https://rdrr.io/r/base/unique.html),
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
will count each unique value, and we can then sort to see the most
common tokens:

``` r

chorales |>
  with(count(Token)) |>
  sort()
```

```
>    humdrumR count distribution 
>    Rank    Token    n
>    1           5  447
>    2           1  446
>    3           2  330
>    4           3  249
>    5           4  249
>    6           6  217
>    7           7  188
>    8          4e  103
>    9          3-   97
>    10         4a   90
>    11         7-   88
>    12         4b   81
>    13         4g   66
>    14         4A   58
>    15         4B   56
>    16         4d   53
>    17         6-   51
>    18         4f   50
>    19         4c   49
>    20        4cc   48
>    21        4f#   47
>    22         4E   43
>    23         4+   36
>    24        8eL   35
>    25        4g#   34
>    26         4G   33
>    27         4D   30
>    28        4F#   30
>    29        4dd   29
>    30       4cc#   28
>    31        4c#   27
>    32       8f#J   26
>    33        4BB   25
>    34        8dL   24
>    35         4C   23
>    36        4G#   21
>    37        4AA   20
>    38        4ee   20
>    39        8eJ   20
>    40        4C#   19
>    41        8dJ   19
>    42        8BL   18
>    43         1+   17
>    44         2a   17
>    45        8AJ   17
>    46        8GL   17
>    47        4b-   16
>    48        4a-   15
>    49        4GG   15
>    50        8aJ   15
>    51        8BJ   15
>    52        8cJ   15
>    53        8DJ   15
>    54        8EL   15
>    55        4e;   14
>    56        8bL   14
>    57        8cL   14
>    58        8CL   14
>    59       8F#J   14
>    60        8gJ   14
>    61        8gL   14
>    62         2e   13
>    63         4F   12
>    64        8AL   12
>    65       8f#L   12
>    66        8bJ   11
>    67         2A   10
>    68         2b   10
>    69        2f#   10
>    70        4B-   10
>    71         5+   10
>    72        8aL   10
>    73        2a;    9
>    74        2e;    9
>    75        4b;    9
>    76        4B;    9
>    77        4d-    9
>    78        4d#    9
>    79        2B;    8
>    80        2dd    8
>    81        4a;    8
>    82        4c;    8
>    83        4d;    8
>    84        4D#    8
>    85        4g;    8
>    86        8CJ    8
>    87        8GJ    8
>    88         2g    7
>    89       4BB-    7
>    90       4dd-    7
>    91        4e-    7
>    92        4E;    7
>    93       8AAJ    7
>    94       8ccJ    7
>    95       8ccL    7
>    96        8EJ    7
>    97       8F#L    7
>    98       8g#J    7
>    99       8G#J    7
>    100       2c;    6
>    101        2d    6
>    102       2d;    6
>    103       4E-    6
>    104       4ff    6
>    105      8B-J    6
>    106     8BB-J    6
>    107      8BBJ    6
>    108       8D-    6
>    109       8DL    6
>    110        8e    6
>    111       8E-    6
>    112       8FL    6
>    113       2c#    5
>    114        2D    5
>    115       2D;    5
>    116       2E;    5
>    117      2f#;    5
>    118       4.g    5
>    119      4AA;    5
>    120      4g#;    5
>    121      4GG;    5
>    122        8a    5
>    123        8A    5
>    124        8C    5
>    125      8c#L    5
>    126        8d    5
>    127      8d#J    5
>    128       8FJ    5
>    129       8fL    5
>    130      8g#L    5
>    131       [4g    4
>    132      2AA;    4
>    133        2E    4
>    134       2f;    4
>    135       2F#    4
>    136       2g#    4
>    137      2G#;    4
>    138       4A-    4
>    139       4a#    4
>    140       4D-    4
>    141       4D;    4
>    142      4f#;    4
>    143        4r    4
>    144       4ry    4
>    145      8a-L    4
>    146      8AAL    4
>    147      8BBL    4
>    148      8C#L    4
>    149      8D-J    4
>    150      8ddL    4
>    151      8E-L    4
>    152        8F    4
>    153       8fJ    4
>    154        8G    4
>    155       8g#    4
>    156       8G#    4
>    157        2-    3
>    158       2b;    3
>    159      2c#;    3
>    160       2cc    3
>    161      2cc#    3
>    162      2FF;    3
>    163       2g;    3
>    164       4.B    3
>    165       4.f    3
>    166       4A;    3
>    167      4BB;    3
>    168      4cc;    3
>    169      4d#;    3
>    170      4dd#    3
>    171       4e#    3
>    172      4ee-    3
>    173       4FF    3
>    174       4G;    3
>    175      4G#;    3
>    176       8AA    3
>    177       8BB    3
>    178     8BB-L    3
>    179      8c#J    3
>    180        8D    3
>    181        8f    3
>    182       8F#    3
>    183        8g    3
>    184      8G#L    3
>    185      8gL]    3
>    186       [4A    2
>    187       [4e    2
>    188       [4E    2
>    189       [4G    2
>    190     16dJJ    2
>    191      16eL    2
>    192      2.d;    2
>    193      2A-;    2
>    194       2A;    2
>    195        2B    2
>    196       2BB    2
>    197      2GG;    2
>    198       4.a    2
>    199       4.b    2
>    200      4.b-    2
>    201      4.BB    2
>    202      4a-;    2
>    203      4A-;    2
>    204       4A#    2
>    205      4AA-    2
>    206       4C;    2
>    207      4c#;    2
>    208      4dd;    2
>    209      4dnX    2
>    210      4e-;    2
>    211       4E#    2
>    212       4EE    2
>    213      4EE;    2
>    214       4f;    2
>    215       4F;    2
>    216      4F#;    2
>    217      4FF#    2
>    218       4g]    2
>    219      4GG#    2
>    220       8a-    2
>    221      8a-J    2
>    222       8A#    2
>    223      8AL]    2
>    224        8b    2
>    225      8b-L    2
>    226       8C#    2
>    227      8C#J    2
>    228       8cc    2
>    229     8cc#J    2
>    230     8cc#L    2
>    231      8D-L    2
>    232      8D#L    2
>    233      8ddJ    2
>    234        8E    2
>    235      8eeL    2
>    236      8eL]    2
>    237       8f#    2
>    238     8FF#J    2
>    239      8FFL    2
>    240       8GG    2
>    241      8GGJ    2
>    242      8GGL    2
>    243     8gnXL    2
>    244       [2d    1
>    245       [2e    1
>    246       [4a    1
>    247       [4B    1
>    248       [4c    1
>    249       [4d    1
>    250       [4f    1
>    251      [4f#    1
>    252      [8cJ    1
>    253      [8CJ    1
>    254      [8gJ    1
>    255      16AL    1
>    256    16B-Jk    1
>    257   16b-XJJ    1
>    258    16BBJJ    1
>    259     16BJJ    1
>    260     16C#L    1
>    261    16c#LL    1
>    262     16ccL    1
>    263    16ccLL    1
>    264    16d#JJ    1
>    265    16ddJJ    1
>    266     16EJJ    1
>    267     16F#L    1
>    268       1e;    1
>    269      2.a;    1
>    270      2.A;    1
>    271     2.AA;    1
>    272       2.b    1
>    273      2.b;    1
>    274      2.B;    1
>    275     2.BB;    1
>    276      2.c;    1
>    277      2.C#    1
>    278     2.c#;    1
>    279       2.d    1
>    280       2.e    1
>    281      2.e;    1
>    282      2.ee    1
>    283      2.f;    1
>    284     2.f#;    1
>    285     2.FF;    1
>    286      2.g;    1
>    287     2.GG;    1
>    288        2+    1
>    289      2a-;    1
>    290     2AA-;    1
>    291      2b-;    1
>    292     2BB-;    1
>    293      2BB;    1
>    294        2c    1
>    295        2C    1
>    296       2C;    1
>    297     2cc#;    1
>    298      2d-;    1
>    299       2d#    1
>    300       2D#    1
>    301      2d#;    1
>    302      2DnX    1
>    303      2e-;    1
>    304       2E#    1
>    305      2EE;    1
>    306      2F#;    1
>    307     2FF#;    1
>    308        2G    1
>    309       2G;    1
>    310       2G#    1
>    311      2g#;    1
>    312     2G#X;    1
>    313      4.a-    1
>    314       4.c    1
>    315     4.cc#    1
>    316       4.d    1
>    317      4.dd    1
>    318       4.e    1
>    319      4.e-    1
>    320      4.ee    1
>    321      4.f#    1
>    322      4a-X    1
>    323     4AA-;    1
>    324      4AA#    1
>    325      4anX    1
>    326      4B-X    1
>    327       4B]    1
>    328       4c]    1
>    329      4C#;    1
>    330     4ccnX    1
>    331      4CnX    1
>    332       4d]    1
>    333       4DD    1
>    334      4DnX    1
>    335       4e]    1
>    336       4E]    1
>    337      4e#;    1
>    338      4E#X    1
>    339     4ee-X    1
>    340      4ee;    1
>    341      4enX    1
>    342      4EnX    1
>    343      4F#X    1
>    344     4F#X;    1
>    345      4ff;    1
>    346      4FF;    1
>    347      4ff#    1
>    348       4g-    1
>    349       4G]    1
>    350      4G#X    1
>    351     4G#X;    1
>    352       4gg    1
>    353      4gnX    1
>    354      4GnX    1
>    355      8.cL    1
>    356       8A-    1
>    357      8A-J    1
>    358      8A-L    1
>    359     8a-XJ    1
>    360      8a#J    1
>    361      8A#J    1
>    362      8AA-    1
>    363      8aL]    1
>    364     8AnXL    1
>    365       8b-    1
>    366       8B-    1
>    367      8b-J    1
>    368        8c    1
>    369     8c#XJ    1
>    370      8cL]    1
>    371      8CL]    1
>    372     8cnXJ    1
>    373       8d-    1
>    374     8d-XJ    1
>    375      8D#J    1
>    376      8d#L    1
>    377       8dd    1
>    378     8dd#J    1
>    379      8dL]    1
>    380      8dnJ    1
>    381      8e-J    1
>    382      8E-J    1
>    383      8e-L    1
>    384      8EEJ    1
>    385      8EEL    1
>    386      8EL]    1
>    387     8f#L]    1
>    388     8F#XJ    1
>    389     8f#XL    1
>    390      8fL]    1
>    391     8FnXL    1
>    392     8g#XJ    1
>    393      8GL]    1
>    394     8GnXL    1
>    Rank    Token    n
>    humdrumR count distribution
```

Now we get a sense of the content of our dataset—in this case, there are
a lot of different (unique) tokens!

### Digging into Details

At this point we’re starting to get a better picture of the content of
our dataset. But don’t get too hasty—it’s a good idea to dig in a little
more before we get confident we really *know* our data.

Our call to
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
told us to expect `**kern` data, representing musical “notes” (pitch and
rhythm). So you probably *expected* to see things like `4.` (dotted
quarter note) and `f#` (F sharp above middle-C). But what is the `X` in
`4dnX`? Or all the `J`s and `L`s and `;`s? We can look these up in the
[`**kern` definition](https://www.humdrum.org/rep/kern/index.html), but
the point is, we probably didn’t know they were there until we took a
look! You might *think* you know what’s in your data…and get
unpleasantly surprised. This is especially true with less mature (newer)
datasets, which *WILL DEFINITELY CONTAIN ERRORS*.

------------------------------------------------------------------------

We see a lot of `;` tokens in our output. If you [look these
up](https://www.humdrum.org/rep/kern/index.html), you’ll learn that they
are “pause signs”, used to represent
[fermatas](https://en.wikipedia.org/wiki/Fermata). But how many tokens
have these fermatas?

Let’s use the [%~%
operator](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md),
which allows us to search for matches to a (regular expression) pattern
in a vector. In this case, we want to search for `";"` in `Token`. `%~%`
returns a logical value (`TRUE` or `FALSE`), which we can
[`sum()`](https://rdrr.io/r/base/sum.html) to get a count of all the
`TRUE`s:

``` r
chorales |>
  with(Token %~% ';') |>
  sum()
>    [1] 256
```

So there are 256 `;` tokens in the data. If we use
[`within()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
(or `mutate()`) instead of `with` (and get rid of the
[`sum()`](https://rdrr.io/r/base/sum.html)), we can see where these
fermatas appear:

``` r
chorales |>
  within(Token %~% ';')
>    #################### vvv chor001_modified.deg vvv ####################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:            **deg          **deg          **deg          **deg
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
>               20:            FALSE          FALSE          FALSE          FALSE
>               21:               =1             =1             =1             =1
>               22:            FALSE          FALSE          FALSE          FALSE
>               23:            FALSE          FALSE          FALSE              .
>               24:                .          FALSE              .              .
>               25:            FALSE          FALSE          FALSE          FALSE
>               26:               =2             =2             =2             =2
>               27:            FALSE          FALSE          FALSE          FALSE
>               28:            FALSE          FALSE              .              .
>               29:                .              .              .          FALSE
>               30:            FALSE          FALSE          FALSE          FALSE
>    31-134::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    #################### ^^^ chor001_modified.deg ^^^ ####################
>    
>           (eighteen more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:            FALSE          FALSE          FALSE          FALSE
>               72:                .          FALSE              .              .
>               73:            FALSE              .          FALSE          FALSE
>               74:                .          FALSE              .              .
>               75:             TRUE           TRUE           TRUE           TRUE
>               76:              =11            =11            =11            =11
>               77:            FALSE          FALSE          FALSE          FALSE
>               78:            FALSE          FALSE          FALSE          FALSE
>               79:            FALSE          FALSE          FALSE          FALSE
>               80:                .              .          FALSE              .
>               81:              =12            =12            =12            =12
>               82:            FALSE          FALSE          FALSE          FALSE
>               83:            FALSE          FALSE          FALSE          FALSE
>               84:            FALSE          FALSE          FALSE          FALSE
>               85:            FALSE          FALSE              .          FALSE
>               86:              =13            =13            =13            =13
>               87:            FALSE          FALSE          FALSE           TRUE
>               88:            FALSE          FALSE          FALSE              .
>               89:             TRUE           TRUE           TRUE              .
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
>       humdrumR corpus of twenty pieces.
>    
>       Data fields: 
>                Token         :: character
>               *Token %~% ";" :: logical
```

Ah, I see that the fermatas all tend to happen at the same time across
the four spines. Good to know!
