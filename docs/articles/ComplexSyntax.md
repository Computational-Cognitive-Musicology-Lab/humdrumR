# Complex humdrum syntax

Welcome to “Complex humdrum syntax”! This article explains how
humdrum$_{\mathbb{R}}$ handles spine paths and multi-stop data tokens.

## Complex humdrum syntax

The [humdrum
syntax](https://humdrumR.ccml.gtcmt.gatech.edu/articles/HumdrumSyntax.md "The Humdrum Syntax")
includes a few complex structures: *spine paths* and *multi-stops*
(a.k.a., sub-tokens). Humdrum$_{\mathbb{R}}$ incorporates these
complexities into its data model, no problem, but they do make things
more complicated, and may require some thought depending on the analyses
you are trying to do. Understanding the how paths/stops are used is not
at all necessary if the data you are interested in doesn’t include spine
paths or multi-stops! You can always skip this article and come back to
it at a later time.

The way humdrum$_{\mathbb{R}}$ incorporates paths/stops is really just
an extension of our basic data model, as described in our [Data
Fields](https://humdrumR.ccml.gtcmt.gatech.edu/articles/DataFields.md "HumdrumR Data Fields article")
article. You should definitely read and understand that article before
reading this one. *Each and every* token, including tokens in various
spine paths and various multi-stops, is recorded as a separate *row* in
the humdrum table.

## Spine paths

Humdrum$_{\mathbb{R}}$ treats spine paths as “sub-spines” of the main
spine which they split from, and keeps track of each path (if any) in
the `Path` field. The starting path (leftmost) is numbered path `0`—in
datasets with no spine paths, the `Path` field will be all zeros. Other
paths are numbered with higher integers.

Let’s look at a simple example (found in the `humdrumRroot` example
files):

``` r
paths1 <- readHumdrum(humdrumRroot, "examples/Paths.krn")

paths1 |> print(view = "humdrum")
>    ######################### vvv Paths.krn vvv #########################
>       1:  !!!OTL: "Path" example
>       2:                **kern              **kern                  
>       3:                 *M4/4               *M4/4                  
>       4:                   *C:                 *C:                  
>       5:                    =-                  =-                  
>       6:                    4C             4cc 4ee                  
>       7:                    4D              4b 4dd                  
>       8:                    4E              2a 2cc                  
>       9:                    4F                   .                  
>      10:                     =                   =                  
>      11:                     *                  *^                  
>      12:                    1G                  8g               4cc
>      13:                     .                  8a                 .
>      14:                     .                  8g                4a
>      15:                     .                 8f#                 .
>      16:                     .                  4g                2b
>      17:                     .                  4f                 .
>      18:                     *                  *v                *v
>      19:                     =                   =                  
>      20:                    1C           1e 1g 1cc                  
>      21:                    ==                  ==                  
>      22:                    *-                  *-                  
>    ######################### ^^^ Paths.krn ^^^ #########################
>    
>       Data fields: 
>               *Token :: character

paths1 |> print(view = "table")
>      Piece    Spine    Path    Record    Stop               Token
>    ######################## vvv Paths.krn vvv #########################
>          1     <NA>    <NA>         1    <NA>!!!OTL: "Path" ...mp
>          1        1       0         2       1              **kern
>          1        1       0         3       1               *M4/4
>          1        1       0         4       1                 *C:
>          1        1       0         5       1                  =-
>          1        1       0         6       1                  4C
>          1        1       0         7       1                  4D
>          1        1       0         8       1                  4E
>          1        1       0         9       1                  4F
>          1        1       0        10       1                   =
>          1        1       0        11       1                   *
>          1        1       0        12       1                  1G
>          1        1       0        13       1                   .
>          1        1       0        14       1                   .
>          1        1       0        15       1                   .
>          1        1       0        16       1                   .
>          1        1       0        17       1                   .
>          1        1       0        18       1                   *
>          1        1       0        19       1                   =
>          1        1       0        20       1                  1C
>          1        1       0        21       1                  ==
>          1        1       0        22       1                  *-
>          1        2       0         2       1              **kern
>          1        2       0         3       1               *M4/4
>          1        2       0         4       1                 *C:
>          1        2       0         5       1                  =-
>          1        2       0         6       1                 4cc
>          1        2       0         6       2                 4ee
>          1        2       0         7       1                  4b
>          1        2       0         7       2                 4dd
>          1        2       0         8       1                  2a
>          1        2       0         8       2                 2cc
>          1        2       0         9       1                   .
>          1        2       0        10       1                   =
>          1        2       0        11       1                  *^
>          1        2       0        12       1                  8g
>          1        2       0        13       1                  8a
>          1        2       0        14       1                  8g
>          1        2       0        15       1                 8f#
>          1        2       0        16       1                  4g
>          1        2       0        17       1                  4f
>          1        2       0        18       1                  *v
>          1        2       0        19       1                   =
>          1        2       0        20       1                  1e
>          1        2       0        20       2                  1g
>          1        2       0        20       3                 1cc
>          1        2       0        21       1                  ==
>          1        2       0        22       1                  *-
>          1        2       1        12       1                 4cc
>          1        2       1        13       1                   .
>          1        2       1        14       1                  4a
>          1        2       1        15       1                   .
>          1        2       1        16       1                  2b
>          1        2       1        17       1                   .
>          1        2       1        18       1                  *v
>    ######################## ^^^ Paths.krn ^^^ #########################
>      Piece    Spine    Path    Record    Stop               Token
>    
>       Data fields: 
>               *Token :: character
```

Here is a more complex example:

``` r
paths2 <- readHumdrum('examples/Paths2.krn')

paths2 |> print(view = "humdrum")
>    ######################### vvv Paths2.krn vvv #########################
>       1:  !!!OTL: complex "Path" example
>       2:              **path                                          
>       3:                   a                                          
>       4:                  *^                                          
>       5:                   b                           c              
>       6:                   d                           e              
>       7:                  *^                           *              
>       8:                   f             g             h              
>       9:                   i             j             k              
>      10:                  *v            *v             *              
>      11:                   l                           m              
>      12:                   n                           o              
>      13:                   *                          *^              
>      14:                   p                           q             r
>      15:                   s                           t             u
>      16:                   *                          *v            *v
>      17:                   v                           w              
>      18:                   x                           y              
>      19:                  *v                          *v              
>      20:                   z                                          
>      21:                  *-                                          
>    ######################### ^^^ Paths2.krn ^^^ #########################
>    
>       Data fields: 
>               *Token :: character

paths2 |> print(view = "table")
>      Piece       Spine       Path       Record               Token
>    ######################## vvv Paths2.krn vvv ########################
>          1        <NA>       <NA>            1!!!OTL: complex...at
>          1           1          0            2              **path
>          1           1          0            3                   a
>          1           1          0            4                  *^
>          1           1          0            5                   b
>          1           1          0            6                   d
>          1           1          0            7                  *^
>          1           1          0            8                   f
>          1           1          0            9                   i
>          1           1          0           10                  *v
>          1           1          0           11                   l
>          1           1          0           12                   n
>          1           1          0           13                   *
>          1           1          0           14                   p
>          1           1          0           15                   s
>          1           1          0           16                   *
>          1           1          0           17                   v
>          1           1          0           18                   x
>          1           1          0           19                  *v
>          1           1          0           20                   z
>          1           1          0           21                  *-
>          1           1          1            8                   g
>          1           1          1            9                   j
>          1           1          1           10                  *v
>          1           1          2            5                   c
>          1           1          2            6                   e
>          1           1          2            7                   *
>          1           1          2            8                   h
>          1           1          2            9                   k
>          1           1          2           10                   *
>          1           1          2           11                   m
>          1           1          2           12                   o
>          1           1          2           13                  *^
>          1           1          2           14                   q
>          1           1          2           15                   t
>          1           1          2           16                  *v
>          1           1          2           17                   w
>          1           1          2           18                   y
>          1           1          2           19                  *v
>          1           1          3           14                   r
>          1           1          3           15                   u
>          1           1          3           16                  *v
>    ######################## ^^^ Paths2.krn ^^^ ########################
>      Piece       Spine       Path       Record               Token
>    
>       Data fields: 
>               *Token :: character
```

Notice that humdrum$_{\mathbb{R}}$ prints paths in a way that is more
readable than reading humdrum syntax directly: paths are “shifted” over
into columns that align. This is an option to the function
[`as.matrix.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md).

### Working with Paths

How you work with spine paths depends, as always, on the nature of the
data. If you are simply doing global counting of notes, for example,
tokens in paths might be treated no differently than any other data. In
some data sets, information in spine paths might not be relevant to your
analyses—for example, if the paths are used to store
[ossia](https://en.wikipedia.org/wiki/Ossia). In other
analyses—especially, if you are analyzing data in a linear/melodic way—,
incorporating/handling spine paths may be very difficult, with no
obviously correct way to do it. In fact, many of
humdrum$_{\mathbb{R}}$’s standard “lagged” functions—like
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`mint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md), and
[`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)—can
give weird results in the presence of spine paths. Often, the simplest
solution is to simply ignore/remove spine paths from your data. Since
the `Path` field is numbered starting from `0`, this can be achieved
easily by filtering out anywhere where `Path > 0`.

``` r
paths1 |> 
  filter(Path == 0) |>
  removeEmptyPaths()
>    ############### vvv Paths.krn vvv ################
>       1:  !!!OTL: "Path" example
>       2:                **kern              **kern
>       3:                 *M4/4               *M4/4
>       4:                   *C:                 *C:
>       5:                    =-                  =-
>       6:                    4C             4cc 4ee
>       7:                    4D              4b 4dd
>       8:                    4E              2a 2cc
>       9:                    4F                   .
>      10:                     =                   =
>      11:                     *                  *^
>      12:                    1G                  8g
>      13:                     .                  8a
>      14:                     .                  8g
>      15:                     .                 8f#
>      16:                     .                  4g
>      17:                     .                  4f
>      18:                     *                  *v
>      19:                     =                   =
>      20:                    1C           1e 1g 1cc
>      21:                    ==                  ==
>      22:                    *-                  *-
>    ############### ^^^ Paths.krn ^^^ ################
>    
>       Data fields: 
>               *Token :: character
```

#### Expanding Paths

One way of doing linear/melodic analyses in the presence of spine paths
is to “expand” the paths into full spines. The
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)
function will “expand” paths by copying the parts of spines that are
shared by multiple paths into separate paths (or spines). Observe:

``` r
paths2 |> expandPaths(asSpines = TRUE)
>    ######################### vvv Paths2.krn vvv #########################
>       1:  !!!OTL: complex "Path" example
>       2:           **path         **path         **path         **path
>       3:                a              a              a              a
>       4:               *^             *^             *^             *^
>       5:                b              b              c              c
>       6:                d              d              e              e
>       7:               *^             *^              *              *
>       8:                f              g              h              h
>       9:                i              j              k              k
>      10:               *v             *v              *              *
>      11:                l              l              m              m
>      12:                n              n              o              o
>      13:                *              *             *^             *^
>      14:                p              p              q              r
>      15:                s              s              t              u
>      16:                *              *             *v             *v
>      17:                v              v              w              w
>      18:                x              x              y              y
>      19:               *v             *v             *v             *v
>      20:                z              z              z              z
>      21:               *-             *-             *-             *-
>    ######################### ^^^ Paths2.krn ^^^ #########################
>    
>       Data fields: 
>               *Token :: character
```

Humdrum$_{\mathbb{R}}$’s [tidyverse
methods](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
like `mutate()` and
[`within()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
also have an `expandPaths` argument. If `expandPaths = TRUE`, these
functions will expand spine paths, apply their expression, then
“unexpand” the paths back to normal. Look at the differnce between these
two calls:

``` r
paths2 |>
  group_by(Spine, Path) |>
  mutate(Enumerate = seq_along(Token))
>    ######################### vvv Paths2.krn vvv #########################
>       1:  !!!OTL: complex "Path" example
>       2:              **path                                          
>       3:                   1                                          
>       4:                  *^                                          
>       5:                   2                           1              
>       6:                   3                           2              
>       7:                  *^                           *              
>       8:                   4             1             3              
>       9:                   5             2             4              
>      10:                  *v            *v             *              
>      11:                   6                           5              
>      12:                   7                           6              
>      13:                   *                          *^              
>      14:                   8                           7             1
>      15:                   9                           8             2
>      16:                   *                          *v            *v
>      17:                  10                           9              
>      18:                  11                          10              
>      19:                  *v                          *v              
>      20:                  12                                          
>      21:                  *-                                          
>    ######################### ^^^ Paths2.krn ^^^ #########################
>    
>       Data fields: 
>               *Enumerate :: integer
>                Token     :: character
>    
>       Grouping fields: (four groups)
>                Path      :: integer
>                Spine     :: integer


paths2 |>
  group_by(Spine, Path) |>
  mutate(Enumerate = seq_along(Token), expandPaths = TRUE)
>    ######################### vvv Paths2.krn vvv #########################
>       2:              **path                                          
>       3:                   1                                          
>       4:                  *^                                          
>       5:                   2                           2              
>       6:                   3                           3              
>       7:                  *^                           *              
>       8:                   4             4             4              
>       9:                   5             5             5              
>      10:                  *v            *v             *              
>      11:                   6                           6              
>      12:                   7                           7              
>      13:                   *                          *^              
>      14:                   8                           8             8
>      15:                   9                           9             9
>      16:                   *                          *v            *v
>      17:                  10                          10              
>      18:                  11                          11              
>      19:                  *v                          *v              
>      20:                  12                                          
>      21:                  *-                                          
>    ######################### ^^^ Paths2.krn ^^^ #########################
>    
>       Data fields: 
>               *Enumerate :: integer
>                Token     :: character
>    
>       Grouping fields: (four groups)
>                Path      :: integer
>                Spine     :: integer
```

Notice how, when we *don’t* use `expandPaths`, each path is counted
entirely separately from the rest.

## Stops

In humdrum syntax, multiple tokens can be placed “in the same place”
(i.e., same record, same spine) by simply separating them with spaces.
(This is most commonly used to represent chords in `**kern` data.) In
humdrum$_{\mathbb{R}}$, we call these “Stops”—as always, **every**
humdrum token, including stops, get their own row in a
humdrum$_{\mathbb{R}}$[humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
Thus, we need the `Stop` field to tell us which stop a token came from!
In much data, all/most tokens are simply `Stop == 1` (the first
position), but if there are more than one tokens in the same
record/spine, they will be numbered ascending from one.

Let’s look at an example to make sense of this! Let’s start by looking
at our humdrum-data view.

``` r
stops <- readHumdrum(humdrumRroot, 'examples/Stops.krn')

stops |> print(view = 'humdrum')
>    ############### vvv Stops.krn vvv ################
>       1:  !!!OTL: "Stop" example
>       2:                **kern              **kern
>       3:                 *M4/4               *M4/4
>       4:                   *C:                 *C:
>       5:                    =-                  =-
>       6:                    2G            4g cc ee
>       7:                     .             4f b dd
>       8:                   2C;             2e; cc;
>       9:                     =                   =
>      10:                    *-                  *-
>    ############### ^^^ Stops.krn ^^^ ################
>    
>       Data fields: 
>               *Token :: character

stops |> print(view = 'data.frame')
>      Piece       Spine       Record       Stop               Token
>    ######################## vvv Stops.krn vvv #########################
>          1        <NA>            1       <NA>!!!OTL: "Stop" ...mp
>          1           1            2          1              **kern
>          1           1            3          1               *M4/4
>          1           1            4          1                 *C:
>          1           1            5          1                  =-
>          1           1            6          1                  2G
>          1           1            7          1                   .
>          1           1            8          1                 2C;
>          1           1            9          1                   =
>          1           1           10          1                  *-
>          1           2            2          1              **kern
>          1           2            3          1               *M4/4
>          1           2            4          1                 *C:
>          1           2            5          1                  =-
>          1           2            6          1                  4g
>          1           2            6          2                  cc
>          1           2            6          3                  ee
>          1           2            7          1                  4f
>          1           2            7          2                   b
>          1           2            7          3                  dd
>          1           2            8          1                 2e;
>          1           2            8          2                 cc;
>          1           2            9          1                   =
>          1           2           10          1                  *-
>    ######################## ^^^ Stops.krn ^^^ #########################
>      Piece       Spine       Record       Stop               Token
>    
>       Data fields: 
>               *Token :: character
```

Here we have a file with chords in the second spine: individual note
tokens separated by spaces. Now, we can switch back to table view: You
can see that each note of the chords gets its own row, numbered `1`,
`2`, and `3` in the `Stop` field!

### Working with Multi-Stops

Working with multi-stops is often even more challenging than working
with spine paths. How you work with stops depends, as always, on the
nature of the data. Again, If you are simply doing global counting of
notes, for example, tokens in stops might be treated no differently than
any other data. In other analyses—especially, if you are analyzing data
in a linear/melodic way—, incorporating/handling multiple stops may be
very difficult, with no obviously correct way to do it. In fact, many of
humdrum$_{\mathbb{R}}$’s standard “lagged” functions—like
[`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md),
[`mint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md), and
[`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)—can
give weird results in the presence of stops. Often, the simplest
solution is to simply ignore/remove stops from your data. Since the
`Stop` field is numbered starting from `1`, this can be achieved easily
by filtering out anywhere where `Stop > 1`.

``` r
stops |> 
  filter(Stop == 1) |>
  removeEmptyStops()
>    ############### vvv Stops.krn vvv ################
>       1:  !!!OTL: "Stop" example
>       2:                **kern              **kern
>       3:                 *M4/4               *M4/4
>       4:                   *C:                 *C:
>       5:                    =-                  =-
>       6:                    2G                  4g
>       7:                     .                  4f
>       8:                   2C;                 2e;
>       9:                     =                   =
>      10:                    *-                  *-
>    ############### ^^^ Stops.krn ^^^ ################
>    
>       Data fields: 
>               *Token :: character
```
