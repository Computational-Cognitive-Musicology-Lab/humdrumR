# Shaping humdrum data

Welcome to “Shaping humdrum data”! This article explains various steps
you can, and often need, to take to prepare humdrum datasets for
analysis, using humdrum$_{\mathbb{R}}$.

One of the great strengths of the [humdrum
syntax](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md "Intro to the humdrum syntax")
is its flexibility—there are lots of ways you can structure your data to
conveniently *represent* musical information. However, when it comes to
analyze data, we typically want to “reshape” our data into a particular
format that is ideal for *analysis*. The key idea is this: given our
data *and* our particular research question, we must determine/decide
what constitutes a *single data observation*. We want each data
observation, or data point, to correspond to one row in our humdrum
table. Depending on how your humdrum data files are organized, this
might not be the case when you first load your data.

In order to shape our data, there are three steps we might need to do to
our data:

1.  Removing information we don’t want.
2.  Splitting apart information that is bundled together.
3.  Pasting together or aligning information that is currently
    separated.

Consider the following small humdrum file, which is bundled with
humdrum$_{\mathbb{R}}$.

``` r
example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:        **kern      **silbe       **kern      **silbe      **harm
>       2:        *ICvox            *       *ICvox            *           *
>       3:        *Ialto            *      *Isoprn            *           *
>       4:         *M4/4            *        *M4/4            *           *
>       5:           *C:            *          *C:            *         *C:
>       6:           4.c         This          4.e         This           I
>       7:            8d           is           8f           is           .
>       8:            4e           an           4g           an           .
>       9:            4f          ex-           4a          ex-          IV
>      10:             =            =            =            =           =
>      11:            2g         -am-          4dd         -am-         I64
>      12:             .            .          4cc            _           .
>      13:            2g         -ple           2b         -ple           V
>      14:            *-           *-           *-           *-          *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Token :: character
```

This file contains (at least) **seven** different pieces of information!
There are two voices, alto and soprano; Each voice has its rhythm *and*
pitch information encoded in its `**kern` spine, plus lyrical
information in the next `**silbe` spine. In addition, the `**harm` spine
indicates the harmony accompanying *both* the vocal parts. Now,
depending on what sorts of analyses we want to perform, we need to
decide what combinations of these seven information streams consitute a
“data observation.”

## Filtering Data

The first step is often to simply remove data we don’t need. In *this*
article, we’ll show you the most common, basic, ways you might filter
your data. For more details about other humdrum$_{\mathbb{R}}$ filtering
functionality, check out the [data
filtering](https://humdrumR.ccml.gtcmt.gatech.edu/articles/Filtering.md "Filtering humdrum data article")
article.

### Indexing

For example, *if* we are studying tonality, we might simply want to
ignore the lyric data. The easiest way to do this is to index out spines
we don’t want, either using numeric indices

``` r
example[[ , c(1,3,5)]]
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:                **kern              **kern              **harm
>       2:                *ICvox              *ICvox                   *
>       3:                *Ialto             *Isoprn                   *
>       4:                 *M4/4               *M4/4                   *
>       5:                   *C:                 *C:                 *C:
>       6:                   4.c                 4.e                   I
>       7:                    8d                  8f                   .
>       8:                    4e                  4g                   .
>       9:                    4f                  4a                  IV
>      10:                     =                   =                   =
>      11:                    2g                 4dd                 I64
>      12:                     .                 4cc                   .
>      13:                    2g                  2b                   V
>      14:                    *-                  *-                  *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Token :: character
```

or (probably better) by exclusive interpretation:

``` r
example[[ , c('**kern', '**harm')]]
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:                **kern              **kern              **harm
>       2:                *ICvox              *ICvox                   *
>       3:                *Ialto             *Isoprn                   *
>       4:                 *M4/4               *M4/4                   *
>       5:                   *C:                 *C:                 *C:
>       6:                   4.c                 4.e                   I
>       7:                    8d                  8f                   .
>       8:                    4e                  4g                   .
>       9:                    4f                  4a                  IV
>      10:                     =                   =                   =
>      11:                    2g                 4dd                 I64
>      12:                     .                 4cc                   .
>      13:                    2g                  2b                   V
>      14:                    *-                  *-                  *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Token :: character
```

### Parsing Token

In this file, the `**kern` spines in our example file include rhythmic
data (`**recip`) *and* pitch data. *If* we are “just” studying tonality,
we could extract the pitch information from the `Token` field, and save
it to a new field. For example, we can use the
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
function to extract the pitch information from `Token`, and put it in a
new field, which we’ll call `Pitch`.

``` r
example |>
  mutate(Pitch = kern(Token)) -> example

example
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:        **kern      **kern      **kern      **kern      **kern
>       2:             .           .           .           .           .
>       3:             .           .           .           .           .
>       4:             .           .           .           .           .
>       5:           *C:           .         *C:           .         *C:
>       6:             c           .           e           .           .
>       7:             d           .           f           .           .
>       8:             e           .           g           .           .
>       9:             f           .           a           .           .
>      10:             =           =           =           =           =
>      11:             g           .          dd           .           .
>      12:             .           .          cc           .           .
>      13:             g           .           b           .           .
>      14:            *-          *-          *-          *-          *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Pitch :: character (**kern tokens)
>                Token :: character
```

Having now isolated the pitch information, we can analyze it. (Of
course, the rhythm information is still present, in the original `Token`
field.)

### Other filtering

Of course, there are many other filtering options you might want to do,
depending on your research goals. Perhaps you only want to study
pieces/passages in a flat keys, or only works from a particular time
period, etc. A common example would be limiting our rhythmic analyses to
music in 4/4 time, and perhaps ignoring pickup notes. The
`TimeSignature` field indicates the time signature, and the `Bar` field
can be used as an indicator of pickups at the beginning of pieces, as
they are marked `Bar == 0`. So for example (using the Bach chorale
data):

``` r
chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')

chorales |>
  filter(Bar > 0 & TimeSignature == 'M4/4') -> chorales

chorales
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
>               20:                .              .              .              .
>               21:                .              .              .              .
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
>           (six more pieces...)
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
>       humdrumR corpus of eight pieces.
>    
>       Data fields: 
>               *Token :: character
```

## Splitting/Separating Data

Humdrum data often packs multiple pieces of information into compact,
concise, readable tokens. The classic example, of course, is `**kern`
itself which often includes rhythm, pitch, phrasing, beaming, and pitch
ornamentation information! These tokens are great for reading/writing,
but not for analyzing, so we typically want to separate the information
we do want.

#### Isolating Pitch and Rhythm

As we’ve seen, the `**kern` spines in our example file include rhythmic
data (`**recip`) *and* pitch data. In some cases, we might want to
access *both* pieces of information, but separately. We can separate
them by applying different functions to the `Token` field, and saving
the output to new fields. For example,

``` r
example |>
  mutate(Rhythm = recip(Token),
         Pitch = kern(Token)) -> example
example
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:    **recip**kern  **recip**kern  **recip**kern  **recip**kern    ***
>       2:                .              .              .              .    ***
>       3:                .              .              .              .    ***
>       4:            *M4/4              .          *M4/4              .    ***
>       5:              *C:              .            *C:              .    ***
>       6:              4.c              .            4.e              .    ***
>       7:               8d              .             8f              .    ***
>       8:               4e              .             4g              .    ***
>       9:               4f              .             4a              .    ***
>      10:                =              =              =              =    ***
>      11:               2g              .            4dd              .    ***
>      12:                .              .            4cc              .    ***
>      13:               2g              .             2b              .    ***
>      14:               *-             *-             *-             *-    ***
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>                    (***one spine/path not displayed due to screen size***)
>    
>       Data fields: 
>               *Pitch  :: character (**kern tokens)
>               *Rhythm :: character (**recip tokens)
>                Token  :: character
```

Hey, the printout kind of looks the same…but if you look at the bottom
you’ll see that there are now separte `Pitch` and `Rhythm` fields. Since
both fields are automatically selected by `mutate()`, we are seeing them
both. But we could, for example, select one or the other field,

``` r
example |> select(Pitch)
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:        **kern      **kern      **kern      **kern      **kern
>       2:             .           .           .           .           .
>       3:             .           .           .           .           .
>       4:             .           .           .           .           .
>       5:           *C:           .         *C:           .         *C:
>       6:             c           .           e           .           .
>       7:             d           .           f           .           .
>       8:             e           .           g           .           .
>       9:             f           .           a           .           .
>      10:             =           =           =           =           =
>      11:             g           .          dd           .           .
>      12:             .           .          cc           .           .
>      13:             g           .           b           .           .
>      14:            *-          *-          *-          *-          *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Pitch  :: character (**kern tokens)
>                Rhythm :: character (**recip tokens)
>                Token  :: character

example |> select(Rhythm)
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:       **recip     **recip     **recip     **recip     **recip
>       2:             .           .           .           .           .
>       3:             .           .           .           .           .
>       4:         *M4/4           .       *M4/4           .           .
>       5:             .           .           .           .           .
>       6:            4.           .          4.           .           .
>       7:             8           .           8           .           .
>       8:             4           .           4           .           .
>       9:             4           .           4           .           .
>      10:             =           =           =           =           =
>      11:             2           .           4           .          64
>      12:             .           .           4           .           .
>      13:             2           .           2           .           .
>      14:            *-          *-          *-          *-          *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>                Pitch  :: character (**kern tokens)
>               *Rhythm :: character (**recip tokens)
>                Token  :: character
```

or run commands using them. For example, we could tabulate pitches
wherever the rhythmic duration is a quarter note:

``` r
example |>
  filter(Rhythm == '4') |>
  count(Pitch)
>    humdrumR count distribution 
>    Pitch  n
>        C  .
>       C#  .
>        D  .
>       E-  .
>        E  .
>        F  .
>       F#  .
>        G  .
>       A-  .
>        A  .
>       B-  .
>        B  .
>        c  .
>       c#  .
>        d  .
>       e-  .
>        e  1
>        f  1
>       f#  .
>        g  1
>       a-  .
>        a  1
>       b-  .
>        b  .
>       cc  1
>      cc#  .
>       dd  1
>      ee-  .
>       ee  .
>       ff  .
>      ff#  .
>       gg  .
>      aa-  .
>       aa  .
>      bb-  .
>       bb  .
>    Pitch  n
>    humdrumR count distribution
```

This is only possible because we separated the information which was
originally “pasted” together in the `**kern` tokens.

### Rend

What if we actually want to move the rhythm and pitch information to
separate spines—i.e., into their rows in the humdrum table? Use
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md):

``` r
example |>
  rend(Rhythm, Pitch)
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:    **recip  **recip  **recip  **recip  **recip  **recip  **recip
>       2:     *ICvox   *ICvox        *   *ICvox   *ICvox        *        *
>       3:     *Ialto   *Ialto        *  *Isoprn  *Isoprn        *        *
>       4:      *M4/4    *M4/4        *    *M4/4    *M4/4        *        *
>       5:        *C:      *C:        *      *C:      *C:        *      *C:
>       6:         4.        c        .       4.        e        .        .
>       7:          8        d        .        8        f        .        .
>       8:          4        e        .        4        g        .        .
>       9:          4        f        .        4        a        .        .
>      10:          =        =        =        =        =        =        =
>      11:          2        g        .        4       dd        .       64
>      12:          .        .        .        4       cc        .        .
>      13:          2        g        .        2        b        .        .
>      14:         *-       *-       *-       *-       *-       *-       *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Rhythm.Pitch :: character (**recip tokens)
>                Token        :: character
```

## Cleave (Pasting/Aligning)

The next step might be to align/combine information that is currently
separated. In many humdrum$_{\mathbb{R}}$ datasets, we have multiple
pieces of information spread across multiple spines, or in some cases,
across spine paths or stops. If, given our research question, we need to
think of multiple pieces of information as describing a *single data
point*, we’ll need to reshape the data. For example, in our `example`
file the `**silbe` (lyric) spines associate each syllable with exactly
one note in the adjacent `**kern` spines. Currently, by default, each
`**kern` token *and* each `**silbe` token are in their own, separate row
of the humdrum table (“row” in terms of `view = "table"`, rather than
spines in `view = "humdrum"`). We want them in the *same* row.  
In the humdrum syntax view, this means moving the `**silbe` data into
the same location as the `**kern` tokens.

### Cleaving Spines

To take separate spines—like `**kern` and `**silbe`—and paste them
together, we use
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md);
as in “to cleave together.” Simply run cleave, and tell it which spines
to cleave together. In our `example` file, the 1st and 3rd spines are
`**kern` while the 2nd and 4th spines are `**silbe`, so we can tell
`cleave` to cleave together `1:2` and `3:4`:

In our example, we want to align the notes in the `**kern` spines with
the syllables in the `**silbe` spine. We can do this directly using
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md):
use the `fold` argument to indicate which spine to fold, and the `onto`
argument to indicate which spine to move it onto.

``` r
example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')

example |>
  cleave(1:2, 3:4)
>    ################## vvv Reshaping_example.hum vvv ##################
>       1:         **kern**silbe       **kern**silbe          **harm
>       2:                *ICvox              *ICvox               *
>       3:                *Ialto             *Isoprn               *
>       4:                 *M4/4               *M4/4               *
>       5:                   *C:                 *C:             *C:
>       6:               4.cThis             4.eThis               I
>       7:                  8dis                8fis               .
>       8:                  4ean                4gan               .
>       9:                 4fex-               4aex-              IV
>      10:                     =                   =               =
>      11:                2g-am-             4dd-am-             I64
>      12:                     .                4cc_               .
>      13:                2g-ple              2b-ple               V
>      14:                    *-                  *-              *-
>    ################## ^^^ Reshaping_example.hum ^^^ ##################
>    
>       Data fields: 
>               *Spine2|4 :: character
>               *Token    :: character
```

It worked! The 1st and 3rd spines have dissappeared, with their content
now in the 1st and 3rd spines. But notice that the cleave put some data
into a new field, which it has called `Spine2|4`. We can improve this
name using the `newFields` argument:

``` r
example |>
  cleave(1:2, 3:4, newFields = 'Lyrics') -> example

example
>    ################## vvv Reshaping_example.hum vvv ##################
>       1:         **kern**silbe       **kern**silbe          **harm
>       2:                *ICvox              *ICvox               *
>       3:                *Ialto             *Isoprn               *
>       4:                 *M4/4               *M4/4               *
>       5:                   *C:                 *C:             *C:
>       6:               4.cThis             4.eThis               I
>       7:                  8dis                8fis               .
>       8:                  4ean                4gan               .
>       9:                 4fex-               4aex-              IV
>      10:                     =                   =               =
>      11:                2g-am-             4dd-am-             I64
>      12:                     .                4cc_               .
>      13:                2g-ple              2b-ple               V
>      14:                    *-                  *-              *-
>    ################## ^^^ Reshaping_example.hum ^^^ ##################
>    
>       Data fields: 
>               *Lyrics :: character
>               *Token  :: character
```

But, wait, where did the `**kern` and `**sible` data go exactly? Well,
since the `**kern` spines were the first spines we indicated to
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md),
that data stays in the original `Token` field. The other spines—which in
this case are the `**sible` data—are put into the new field(s). See:

``` r
example |> select(Token)
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:                **kern              **kern              **harm
>       2:                *ICvox              *ICvox                   *
>       3:                *Ialto             *Isoprn                   *
>       4:                 *M4/4               *M4/4                   *
>       5:                   *C:                 *C:                 *C:
>       6:                   4.c                 4.e                   I
>       7:                    8d                  8f                   .
>       8:                    4e                  4g                   .
>       9:                    4f                  4a                  IV
>      10:                     =                   =                   =
>      11:                    2g                 4dd                 I64
>      12:                     .                 4cc                   .
>      13:                    2g                  2b                   V
>      14:                    *-                  *-                  *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>                Lyrics :: character
>               *Token  :: character

example |> select(Lyrics)
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:               **silbe             **silbe              **harm
>       2:                *ICvox              *ICvox                   *
>       3:                *Ialto             *Isoprn                   *
>       4:                 *M4/4               *M4/4                   *
>       5:                   *C:                 *C:                 *C:
>       6:                  This                This                   .
>       7:                    is                  is                   .
>       8:                    an                  an                   .
>       9:                   ex-                 ex-                   .
>      10:                     =                   =                   =
>      11:                  -am-                -am-                   .
>      12:                     .                   _                   .
>      13:                  -ple                -ple                   .
>      14:                    *-                  *-                  *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Lyrics :: character
>                Token  :: character
```

Notice that the fifth spine, which wasn’t part of the cleave, is also
left untouched in the `Token` field.

------------------------------------------------------------------------

In some datasets, there might be different numbers of `**kern`/`**silbe`
spines in different files within the dataset. In these cases, it will be
much smarter to pass
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
the names of the exclusive interpretations we want to cleave. We should
get the same result we got manually before:

``` r
example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')

example |>
  cleave(c('kern', 'silbe'))
>    ################## vvv Reshaping_example.hum vvv ##################
>       1:         **kern**silbe       **kern**silbe          **harm
>       2:                *ICvox              *ICvox               *
>       3:                *Ialto             *Isoprn               *
>       4:                 *M4/4               *M4/4               *
>       5:                   *C:                 *C:             *C:
>       6:               4.cThis             4.eThis               I
>       7:                  8dis                8fis               .
>       8:                  4ean                4gan               .
>       9:                 4fex-               4aex-              IV
>      10:                     =                   =               =
>      11:                2g-am-             4dd-am-             I64
>      12:                     .                4cc_               .
>      13:                2g-ple              2b-ple               V
>      14:                    *-                  *-              *-
>    ################## ^^^ Reshaping_example.hum ^^^ ##################
>    
>       Data fields: 
>               *Silbe :: character
>               *Token :: character
```

Since we are cleaving by the name of exclusive interpretation here,
humdrum$_{\mathbb{R}}$ names the new field with the capitalized
exclusive interpretation name by default.

We may customize the new field name as before:

``` r
example |>
  cleave(c('kern', 'silbe'), newFields = 'Lyrics')
>    ################## vvv Reshaping_example.hum vvv ##################
>       1:         **kern**silbe       **kern**silbe          **harm
>       2:                *ICvox              *ICvox               *
>       3:                *Ialto             *Isoprn               *
>       4:                 *M4/4               *M4/4               *
>       5:                   *C:                 *C:             *C:
>       6:               4.cThis             4.eThis               I
>       7:                  8dis                8fis               .
>       8:                  4ean                4gan               .
>       9:                 4fex-               4aex-              IV
>      10:                     =                   =               =
>      11:                2g-am-             4dd-am-             I64
>      12:                     .                4cc_               .
>      13:                2g-ple              2b-ple               V
>      14:                    *-                  *-              *-
>    ################## ^^^ Reshaping_example.hum ^^^ ##################
>    
>       Data fields: 
>               *Lyrics :: character
>               *Token  :: character
```

------------------------------------------------------------------------

What if we want to study the note combinations that are formed between
the two `**kern` spines? As they are, this would be hard because the
tokens from each spine are all in their own rows (in the table view).
However, we could
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
together the two kern spines! (We’ll also isolate *just* the `**kern`
spines for this.)

``` r
example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')

example |>
  _[[ , '**kern']] |>
  kern(simple = TRUE) |>
  cleave(c(1, 2)) |> 
  count()
>    humdrumR count distribution 
>    Kern  Spine2                  
>               a  b  c  d  e  f  g
>      NA       .  .  1  .  .  .  .
>       c       .  .  .  .  1  .  .
>      c#       .  .  .  .  .  .  .
>       d       .  .  .  .  .  1  .
>      e-       .  .  .  .  .  .  .
>       e       .  .  .  .  .  .  1
>       f       1  .  .  .  .  .  .
>      f#       .  .  .  .  .  .  .
>       g       .  1  .  1  .  .  .
>      a-       .  .  .  .  .  .  .
>       a       .  .  .  .  .  .  .
>      b-       .  .  .  .  .  .  .
>       b       .  .  .  .  .  .  .
>               a  b  c  d  e  f  g
>    Kern  Spine2                  
>    humdrumR count distribution
```

#### Multi-cleaving

What about that `**harm` spine in our data. Unlike the `**silbe` spines,
the `**harm` spine is not “paired up” with the `**kern` spines. Rather,
the `**harm` actually describes what is happening in the entire *record*
of data. The chords indicated in this spine are associated with the
pitches in both, or either, of the `**kern` spines. Luckily,
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
will handle this:

``` r
example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')

example |>
  cleave(c('kern', 'harm'))
>    ################### vvv Reshaping_example.hum vvv ####################
>       1:        **kern**harm      **silbe      **kern**harm      **silbe
>       2:              *ICvox            *            *ICvox            *
>       3:              *Ialto            *           *Isoprn            *
>       4:               *M4/4            *             *M4/4            *
>       5:                 *C:            *               *C:            *
>       6:                4.cI         This              4.eI         This
>       7:                  8d           is                8f           is
>       8:                  4e           an                4g           an
>       9:                4fIV          ex-              4aIV          ex-
>      10:                   =            =                 =            =
>      11:               2gI64         -am-            4ddI64         -am-
>      12:                   .            .               4cc            _
>      13:                 2gV         -ple               2bV         -ple
>      14:                  *-           *-                *-           *-
>    ################### ^^^ Reshaping_example.hum ^^^ ####################
>    
>       Data fields: 
>               *Harm  :: character
>               *Token :: character
```

Data from the `**harm` spine is copied *twice* into a new field “on top
of” *both* `**kern` spines!

### Cleaving Stops and Paths

Though spines are the most common structure in humdrum data that you
might need to “cleave” together, we can also cleave other structures. Of
course, it depends on what questions you are trying to ask of your data!

#### Multi-Stops

Consider this example, with multi-stop chords in a `**kern` spine:

``` r
example_stops <- readHumdrum(humdrumRroot, 'examples/Reshaping_example2_stops.hum')

example_stops
>    ###### vvv Reshaping_example2_stops.hum vvv ######
>       1:                **kern              **harm
>       2:                 *M4/4                   *
>       3:                   *C:                 *C:
>       4:                    4c                   I
>       5:                    4B                  Vb
>       6:               4B- d g                  vb
>       7:                4A d f                 IVb
>       8:                     =                   =
>       9:                2G c e                 I64
>      10:                2G B d                   V
>      11:                    *-                  *-
>    ###### ^^^ Reshaping_example2_stops.hum ^^^ ######
>    
>       Data fields: 
>               *Token :: character
```

By default, humdrum$_{\mathbb{R}}$ treats each token (note) in *each*
stop as a separate data observation, with its own row in the humdrum
table view. If we are studying harmony, we might want to align those
stops “on top” of each other, in different fields. But, we can cleave
the stops together, putting them each into their own field!

``` r
example_stops |>
  cleave(Stop = 1:3)
>    ###### vvv Reshaping_example2_stops.hum vvv ######
>       1:                **kern              **harm
>       2:                 *M4/4                   *
>       3:                   *C:                 *C:
>       4:                    4c                   I
>       5:                    4B                  Vb
>       6:                 4B-dg                  vb
>       7:                  4Adf                 IVb
>       8:                     =                   =
>       9:                  2Gce                 I64
>      10:                  2GBd                   V
>      11:                    *-                  *-
>    ###### ^^^ Reshaping_example2_stops.hum ^^^ ######
>    
>       Data fields: 
>               *Stop2 :: character
>               *Stop3 :: character
>               *Token :: character
```

The first stop (`Stop == 1`) is left in the `Token` field, but we get
new `Stop2` and `Stop3` fields holding the other stops!

#### Spine Paths

When working with spine paths, it is often less obvious how we should
interpret different paths in terms of data observations, but if you want
to cleave them, you can. Don’t forget that `Path` is numbered starting
from `0`.

``` r
example_paths <- readHumdrum(humdrumRroot, 'examples/Reshaping_example3_paths.hum')

example_paths |>
  cleave(Path = 0:1)
>    ###### vvv Reshaping_example3_paths.hum vvv ######
>       1:                **kern              **harm
>       2:                 *M4/4                   *
>       3:                   *C:                 *C:
>       4:                    4c                   I
>       5:                    4d                   .
>       6:                    4e                   .
>       7:                    *^                   *
>       8:                  4d4f                  ii
>       9:                     =                   =
>      10:                  2c2g                   I
>      11:                  2B2g                  Vb
>      12:                    *v                   *
>      13:                     =                   =
>      14:                    *-                  *-
>    ###### ^^^ Reshaping_example3_paths.hum ^^^ ######
>    
>       Data fields: 
>               *Path1 :: character
>               *Token :: character
```
