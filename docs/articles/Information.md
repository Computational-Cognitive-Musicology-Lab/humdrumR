# Counts, Probability, and Information

Welcome to “Counts, Probability, and Information”! This article explains
some of the features R and humdrum$_{\mathbb{R}}$ have for extracting
count distributions, probability distributions, and information theory
metrics from data.

## Counting things

Nearly all of our articles, from the very
[start](https://humdrumR.ccml.gtcmt.gatech.edu/articles/GettingStarted.html#counting-things "Getting started with humdrumR > Counting things"),
have made use of the
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function. This function can be used to count the number of unique values
in a singe vector, or the unique *combinations of values* across
multiple of vectors.

Let’s take a moment to explain all of what
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
can do, starting from the basics. The
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function accepts one or more atomic vectors, all of which must be the
same length. It counts all the unique combination of values across the
vectors, and builds a table. This can be illustrated with some simple
examples:

``` r
v1 <- c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c', 'a')
v2 <- c('A', 'A', 'A', 'B', 'B', 'C', 'C', 'C', 'C', 'C')


count(v1)
>    humdrumR count distribution 
>    v1  n
>     a  4
>     b  3
>     c  3
>    v1  n
>    humdrumR count distribution
count(v2)
>    humdrumR count distribution 
>    v2  n
>     A  3
>     B  2
>     C  5
>    v2  n
>    humdrumR count distribution
count(v1, v2)
>    humdrumR count distribution 
>    v1  v2      
>         A  B  C
>     a   1  1  2
>     b   1  1  1
>     c   1  .  2
>         A  B  C
>    v1  v2      
>    humdrumR count distribution
```

Each input vector is considered one data “dimension.” You can name your
dimensions by naming the input arguments:

``` r
count(lowercase = v1, uppercase = v2)
>    humdrumR count distribution 
>    lowercase  uppercase      
>                       A  B  C
>            a          1  1  2
>            b          1  1  1
>            c          1  .  2
>                       A  B  C
>    lowercase  uppercase      
>    humdrumR count distribution
```

If there are `NA` values in your data, they will be counted unless you
specify `na.rm = TRUE`.

``` r
v1[3] <- NA

count(v1, v2)
>    humdrumR count distribution 
>    v1  v2      
>         A  B  C
>    NA   1  .  .
>     a   1  1  2
>     b   1  1  1
>     c   .  .  2
>         A  B  C
>    v1  v2      
>    humdrumR count distribution

count(v1, v2, na.rm = TRUE)
>    humdrumR count distribution 
>    v1  v2      
>         A  B  C
>     a   1  1  2
>     b   1  1  1
>     c   .  .  2
>         A  B  C
>    v1  v2      
>    humdrumR count distribution
```

There is also an option to sort the table. You can use `-1` for
increasing sort.

``` r
count(v1, v2, sort = TRUE)
>    humdrumR count distribution 
>    Rank  v1  v2  n
>    1      a   C  2
>    2      c   C  2
>    3      a   A  1
>    4      b   A  1
>    5     NA   A  1
>    6      a   B  1
>    7      b   B  1
>    8      b   C  1
>    9      c   A  .
>    10     c   B  .
>    11    NA   B  .
>    12    NA   C  .
>    Rank  v1  v2  n
>    humdrumR count distribution
count(v1, v2, sort = -1)
>    humdrumR count distribution 
>    Rank  v1  v2  n
>    12     c   A  .
>    11     c   B  .
>    10    NA   B  .
>    9     NA   C  .
>    8      a   A  1
>    7      b   A  1
>    6     NA   A  1
>    5      a   B  1
>    4      b   B  1
>    3      b   C  1
>    2      a   C  2
>    1      c   C  2
>    Rank  v1  v2  n
>    humdrumR count distribution
```

> In base R, the
> [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
> function serves basically the same role of the
> [`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
> function. You can use
> [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
> in many areas where you can use
> [`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)—you’ll
> see that their output is essentially equivalent, but in a different
> structure (a `matrix` rather than a `data.frame`). However,
> [`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
> is fully integrated into the `dplyr` universe and workflow, so we
> recommend using it.

##### Counting Real Numbers

If you have a distribution of continuous numbers, you probably don’t
want to count every unique value (because there will be one of each).
Thus,
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
will (attempt) to treat numeric values in a smart way by binning them
into ranges using the base-R
[`hist()`](https://rdrr.io/r/graphics/hist.html) function’s binning
algorithm. So for example:

``` r
numbers <- rnorm(1000, 2, 2)

numbers |> head(30)
>     [1] -0.8000870  2.5106341 -2.8745272  1.9888574  3.2431054  4.2968232
>     [7] -1.6436353  1.5053494  1.5116008  1.4345891  0.8926012  3.2579641
>    [13]  6.1300498 -1.2619788  3.0248539 -1.7260230  0.9559750  1.8947962
>    [19]  3.0859927  0.1718503  2.9363088  2.7259025 -0.6090871  3.4755526
>    [25]  5.7770099  1.8051098  0.1283053  1.9680994  0.3464221 -1.0247993

count(numbers)
>    humdrumR count distribution 
>    numbers    n
>    [-4,-3]    4
>    (-3,-2]   18
>    (-2,-1]   45
>     (-1,0]   86
>      (0,1]  162
>      (1,2]  180
>      (2,3]  188
>      (3,4]  151
>      (4,5]  102
>      (5,6]   39
>      (6,7]   19
>      (7,8]    6
>    numbers    n
>    humdrumR count distribution
```

The binning process can be controlled using the `binArgs` argument,
which must be a list of arguments passed to \[hist()\]. A particularly
useful one is `breaks`, which controls the number of, or location of,
bins.

``` r
count(numbers, binArgs = list(breaks = 12))
>    humdrumR count distribution 
>    numbers    n
>    [-4,-3]    4
>    (-3,-2]   18
>    (-2,-1]   45
>     (-1,0]   86
>      (0,1]  162
>      (1,2]  180
>      (2,3]  188
>      (3,4]  151
>      (4,5]  102
>      (5,6]   39
>      (6,7]   19
>      (7,8]    6
>    numbers    n
>    humdrumR count distribution
count(numbers, binArgs = list(breaks = -8:12))
>    humdrumR count distribution 
>    numbers    n
>    [-8,-7]    .
>    (-7,-6]    .
>    (-6,-5]    .
>    (-5,-4]    .
>    (-4,-3]    4
>    (-3,-2]   18
>    (-2,-1]   45
>     (-1,0]   86
>      (0,1]  162
>      (1,2]  180
>      (2,3]  188
>      (3,4]  151
>      (4,5]  102
>      (5,6]   39
>      (6,7]   19
>      (7,8]    6
>      (8,9]    .
>     (9,10]    .
>    (10,11]    .
>    (11,12]    .
>    numbers    n
>    humdrumR count distribution
```

------------------------------------------------------------------------

If you *don’t* want to bin numbers, just apply
[`as.character()`](https://rdrr.io/r/base/character.html) (perhaps after
rounding the numbers).

``` r
numbers |> round(1) |> as.character() |> count()
>    humdrumR count distribution 
>    as.character(round(numbers, 1))   n
>                               -0.1  11
>                               -0.2   9
>                               -0.3  15
>                               -0.4   7
>                               -0.5   5
>                               -0.6   7
>                               -0.7   7
>                               -0.8  11
>                               -0.9   5
>                                 -1  12
>                               -1.1   6
>                               -1.2   2
>                               -1.3   5
>                               -1.4   6
>                               -1.5   6
>                               -1.6   4
>                               -1.7   1
>                               -1.8   7
>                                 -2   4
>                               -2.1   3
>                               -2.2   3
>                               -2.3   1
>                               -2.4   2
>                               -2.5   1
>                               -2.6   1
>                               -2.7   1
>                               -2.8   1
>                               -2.9   3
>                                 -3   1
>                               -3.2   1
>                               -3.3   1
>                               -3.6   1
>                               -3.9   1
>                                  0   7
>                                0.1  20
>                                0.2  14
>                                0.3  19
>                                0.4  16
>                                0.5  13
>                                0.6  14
>                                0.7  18
>                                0.8  15
>                                0.9  17
>                                  1  20
>                                1.1  14
>                                1.2  17
>                                1.3  18
>                                1.4  17
>                                1.5  17
>                                1.6  18
>                                1.7  22
>                                1.8  24
>                                1.9  17
>                                  2  19
>                                2.1  24
>                                2.2  19
>                                2.3  15
>                                2.4  14
>                                2.5  25
>                                2.6  12
>                                2.7  21
>                                2.8  20
>                                2.9  20
>                                  3  16
>                                3.1  26
>                                3.2  17
>                                3.3  15
>                                3.4  16
>                                3.5  13
>                                3.6  11
>                                3.7  10
>                                3.8  15
>                                3.9  13
>                                  4  12
>                                4.1  14
>                                4.2   9
>                                4.3  15
>                                4.4  12
>                                4.5   7
>                                4.6   7
>                                4.7   9
>                                4.8  12
>                                4.9   8
>                                  5   6
>                                5.1   3
>                                5.2   3
>                                5.3   3
>                                5.4   3
>                                5.5   5
>                                5.6   5
>                                5.7   2
>                                5.8   7
>                                5.9   2
>                                  6   2
>                                6.1   5
>                                6.2   3
>                                6.3   5
>                                6.4   2
>                                6.5   1
>                                6.6   1
>                                6.8   1
>                                  7   1
>                                7.1   1
>                                7.3   2
>                                7.4   2
>                                7.5   1
>    as.character(round(numbers, 1))   n
>    humdrumR count distribution
```

#### Counting real data

Enough with the artificial examples; let’s have a go at using
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
on some musical data. This should feel familiar, given we’ve done
similar stuff before in other articles:

``` r
bach <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')

bach |> mutate(Pitch = kern(Token, simple = TRUE), Rhythm = recip(Token)) -> bach

bach |> count(Pitch, na.rm = TRUE)
>    humdrumR count distribution 
>    Pitch    n
>        c  232
>       c#  115
>       d-   35
>        d  253
>       d#   37
>       e-   34
>        e  334
>       e#    8
>        f  121
>       f#  182
>       g-    1
>        g  237
>       g#  109
>       a-   45
>        a  318
>       a#   11
>       b-   60
>        b  297
>    Pitch    n
>    humdrumR count distribution

bach |> count(Rhythm)
>    humdrumR count distribution, ~rounded 
>    Rhythm       n
>        16      17
>         8     706
>        8.       1
>         4  ~1.44k
>        4.      28
>         2     222
>        2.      21
>         1       1
>    Rhythm       n
>    humdrumR count distribution, ~rounded

bach |> count(Pitch, Rhythm, sort = TRUE)
>    humdrumR count distribution 
>    Rank  Pitch  Rhythm    n
>    1         e       4  200
>    2         a       4  188
>    3         b       4  185
>    4         g       4  142
>    5         c       4  137
>    6         d       4  132
>    7         e       8   92
>    8        f#       4   89
>    9         a       8   82
>    10        d       8   81
>    11        c       8   78
>    12        f       4   78
>    13       c#       4   77
>    14        g       8   74
>    15        b       8   73
>    16       f#       8   69
>    17       g#       4   67
>    18        a       2   42
>    19       b-       4   34
>    20        e       2   33
>    21        d       2   32
>    22        f       8   31
>    23       g#       8   31
>    24       a-       4   27
>    25        b       2   26
>    26       d#       4   23
>    27       c#       8   21
>    28       f#       2   21
>    29       b-       8   20
>    30       d-       4   20
>    31       e-       4   19
>    32       d-       8   14
>    33        g       2   14
>    34       e-       8   13
>    35       a-       8   13
>    36        c       2   12
>    37       c#       2   12
>    38       g#       2   11
>    39       d#       8   10
>    40       e#       4    7
>    41       a#       4    7
>    42        b      4.    7
>    43        f       2    7
>    44        g      4.    5
>    45       a#       8    4
>    46       NA       4    4
>    47       a-       2    4
>    48        b      2.    4
>    49        d      16    3
>    50        e      16    3
>    51        f      4.    3
>    52       d#       2    3
>    53        d      2.    3
>    54        e      2.    3
>    55        a      2.    3
>    56        c      16    2
>    57       c#      16    2
>    58       b-      16    2
>    59        b      16    2
>    60        d      4.    2
>    61        e      4.    2
>    62        a      4.    2
>    63       b-      4.    2
>    64       b-       2    2
>    65       c#      2.    2
>    66        f      2.    2
>    67        g      2.    2
>    68       d#      16    1
>    69       f#      16    1
>    70        a      16    1
>    71        c      8.    1
>    72       g-       4    1
>    73        c      4.    1
>    74       c#      4.    1
>    75       e-      4.    1
>    76       f#      4.    1
>    77       a-      4.    1
>    78       d-       2    1
>    79       e-       2    1
>    80       e#       2    1
>    81        c      2.    1
>    82       f#      2.    1
>    83        e       1    1
>    84       d-      16    .
>    85       e-      16    .
>    86       e#      16    .
>    87        f      16    .
>    88       g-      16    .
>    89        g      16    .
>    90       g#      16    .
>    91       a-      16    .
>    92       a#      16    .
>    93       NA      16    .
>    94       e#       8    .
>    95       g-       8    .
>    96       NA       8    .
>    97       c#      8.    .
>    98       d-      8.    .
>    99        d      8.    .
>    100      d#      8.    .
>    101      e-      8.    .
>    102       e      8.    .
>    103      e#      8.    .
>    104       f      8.    .
>    105      f#      8.    .
>    106      g-      8.    .
>    107       g      8.    .
>    108      g#      8.    .
>    109      a-      8.    .
>    110       a      8.    .
>    111      a#      8.    .
>    112      b-      8.    .
>    113       b      8.    .
>    114      NA      8.    .
>    115      d-      4.    .
>    116      d#      4.    .
>    117      e#      4.    .
>    118      g-      4.    .
>    119      g#      4.    .
>    120      a#      4.    .
>    121      NA      4.    .
>    122      g-       2    .
>    123      a#       2    .
>    124      NA       2    .
>    125      d-      2.    .
>    126      d#      2.    .
>    127      e-      2.    .
>    128      e#      2.    .
>    129      g-      2.    .
>    130      g#      2.    .
>    131      a-      2.    .
>    132      a#      2.    .
>    133      b-      2.    .
>    134      NA      2.    .
>    135       c       1    .
>    136      c#       1    .
>    137      d-       1    .
>    138       d       1    .
>    139      d#       1    .
>    140      e-       1    .
>    141      e#       1    .
>    142       f       1    .
>    143      f#       1    .
>    144      g-       1    .
>    145       g       1    .
>    146      g#       1    .
>    147      a-       1    .
>    148       a       1    .
>    149      a#       1    .
>    150      b-       1    .
>    151       b       1    .
>    152      NA       1    .
>    Rank  Pitch  Rhythm    n
>    humdrumR count distribution
```

Woh, there are a lot of `.` values in that last table!—that dot is a
shorthand for “zero.” The reason there are so many zeros is because
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
by default, recognizes every possible combination of values that could
occur, even if they never occur. To get rid of these unused levels, we
can set `.drop = TRUE`.

``` r
bach |> count(Pitch, Rhythm, sort = TRUE, .drop = TRUE)
>    humdrumR count distribution 
>    Rank  Pitch  Rhythm    n
>    1         e       4  200
>    2         a       4  188
>    3         b       4  185
>    4         g       4  142
>    5         c       4  137
>    6         d       4  132
>    7         e       8   92
>    8        f#       4   89
>    9         a       8   82
>    10        d       8   81
>    11        c       8   78
>    12        f       4   78
>    13       c#       4   77
>    14        g       8   74
>    15        b       8   73
>    16       f#       8   69
>    17       g#       4   67
>    18        a       2   42
>    19       b-       4   34
>    20        e       2   33
>    21        d       2   32
>    22        f       8   31
>    23       g#       8   31
>    24       a-       4   27
>    25        b       2   26
>    26       d#       4   23
>    27       c#       8   21
>    28       f#       2   21
>    29       b-       8   20
>    30       d-       4   20
>    31       e-       4   19
>    32       d-       8   14
>    33        g       2   14
>    34       e-       8   13
>    35       a-       8   13
>    36        c       2   12
>    37       c#       2   12
>    38       g#       2   11
>    39       d#       8   10
>    40       e#       4    7
>    41       a#       4    7
>    42        b      4.    7
>    43        f       2    7
>    44        g      4.    5
>    45       a#       8    4
>    46       NA       4    4
>    47       a-       2    4
>    48        b      2.    4
>    49        d      16    3
>    50        e      16    3
>    51        f      4.    3
>    52       d#       2    3
>    53        d      2.    3
>    54        e      2.    3
>    55        a      2.    3
>    56        c      16    2
>    57       c#      16    2
>    58       b-      16    2
>    59        b      16    2
>    60        d      4.    2
>    61        e      4.    2
>    62        a      4.    2
>    63       b-      4.    2
>    64       b-       2    2
>    65       c#      2.    2
>    66        f      2.    2
>    67        g      2.    2
>    68       d#      16    1
>    69       f#      16    1
>    70        a      16    1
>    71        c      8.    1
>    72       g-       4    1
>    73        c      4.    1
>    74       c#      4.    1
>    75       e-      4.    1
>    76       f#      4.    1
>    77       a-      4.    1
>    78       d-       2    1
>    79       e-       2    1
>    80       e#       2    1
>    81        c      2.    1
>    82       f#      2.    1
>    83        e       1    1
>    Rank  Pitch  Rhythm    n
>    humdrumR count distribution
```

What if don’t care about combinations that only occurred once either?
Luckily, the output of
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
is a data.frame, and thus we can filter it. The count value in our table
is stored in the field called `n`, which we can reference:

``` r
bach |> 
  count(Pitch, Rhythm, sort = TRUE) |>
  filter(n > 20)
>    humdrumR count distribution 
>    Pitch  Rhythm          
>                8    4    2
>        c      78  137    .
>       c#      21   77    .
>        d      81  132   32
>       d#       .   23    .
>        e      92  200   33
>        f      31   78    .
>       f#      69   89   21
>        g      74  142    .
>       g#      31   67    .
>       a-       .   27    .
>        a      82  188   42
>       b-       .   34    .
>        b      73  185   26
>                8    4    2
>    Pitch  Rhythm          
>    humdrumR count distribution
```

Or maybe we’d like to see only the counts for quarter and eighth notes:

``` r
bach |> 
  count(Pitch, Rhythm, sort = TRUE) |>
  filter(Rhythm %in% c('4', '8'))
>    humdrumR count distribution 
>    Pitch  Rhythm     
>                8    4
>       NA       .    4
>        c      78  137
>       c#      21   77
>       d-      14   20
>        d      81  132
>       d#      10   23
>       e-      13   19
>        e      92  200
>       e#       .    7
>        f      31   78
>       f#      69   89
>       g-       .    1
>        g      74  142
>       g#      31   67
>       a-      13   27
>        a      82  188
>       a#       4    7
>       b-      20   34
>        b      73  185
>                8    4
>    Pitch  Rhythm     
>    humdrumR count distribution
```

### Probabilities

The
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function is a partner to the
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function. It computes the *proportion* of input data which equals each
unique value—which we can interpret as *probabilities*. Note that
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
has all the same arguments as
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
(`sort`, `.drop`, etc.).

``` r
bach |>  
  pdist(Pitch)
>    humdrumR probability distribution P(Pitch), ~rounded 
>    Pitch        p
>        c  ~.09540
>       c#  ~.04730
>       d-  ~.01440
>        d  ~.10400
>       d#  ~.01520
>       e-  ~.01400
>        e  ~.13700
>       e#  ~.00329
>        f  ~.04970
>       f#  ~.07480
>       g-   ~.411m
>        g  ~.09740
>       g#  ~.04480
>       a-  ~.01850
>        a  ~.13100
>       a#  ~.00452
>       b-  ~.02470
>        b  ~.12200
>       NA  ~.00164
>    Pitch        p
>    humdrumR probability distribution P(Pitch), ~rounded

bach |>  
  pdist(Rhythm)
>    humdrumR probability distribution P(Rhythm), ~rounded 
>    Rhythm        p
>        16  ~.00699
>         8  ~.29000
>        8.   ~.411m
>         4  ~.59100
>        4.  ~.01150
>         2  ~.09120
>        2.  ~.00863
>         1   ~.411m
>    Rhythm        p
>    humdrumR probability distribution P(Rhythm), ~rounded


bach |>  
  pdist(Pitch, Rhythm)
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch  Rhythm        p
>        c      16   ~.822m
>       c#      16   ~.822m
>       d-      16        .
>        d      16  ~.00123
>       d#      16   ~.411m
>       e-      16        .
>        e      16  ~.00123
>       e#      16        .
>        f      16        .
>       f#      16   ~.411m
>       g-      16        .
>        g      16        .
>       g#      16        .
>       a-      16        .
>        a      16   ~.411m
>       a#      16        .
>       b-      16   ~.822m
>        b      16   ~.822m
>       NA      16        .
>        c       8  ~.03210
>       c#       8  ~.00863
>       d-       8  ~.00575
>        d       8  ~.03330
>       d#       8  ~.00411
>       e-       8  ~.00534
>        e       8  ~.03780
>       e#       8        .
>        f       8  ~.01270
>       f#       8  ~.02840
>       g-       8        .
>        g       8  ~.03040
>       g#       8  ~.01270
>       a-       8  ~.00534
>        a       8  ~.03370
>       a#       8  ~.00164
>       b-       8  ~.00822
>        b       8  ~.03000
>       NA       8        .
>        c      8.   ~.411m
>       c#      8.        .
>       d-      8.        .
>        d      8.        .
>       d#      8.        .
>       e-      8.        .
>        e      8.        .
>       e#      8.        .
>        f      8.        .
>       f#      8.        .
>       g-      8.        .
>        g      8.        .
>       g#      8.        .
>       a-      8.        .
>        a      8.        .
>       a#      8.        .
>       b-      8.        .
>        b      8.        .
>       NA      8.        .
>        c       4  ~.05630
>       c#       4  ~.03160
>       d-       4  ~.00822
>        d       4  ~.05430
>       d#       4  ~.00945
>       e-       4  ~.00781
>        e       4  ~.08220
>       e#       4  ~.00288
>        f       4  ~.03210
>       f#       4  ~.03660
>       g-       4   ~.411m
>        g       4  ~.05840
>       g#       4  ~.02750
>       a-       4  ~.01110
>        a       4  ~.07730
>       a#       4  ~.00288
>       b-       4  ~.01400
>        b       4  ~.07600
>       NA       4  ~.00164
>        c      4.   ~.411m
>       c#      4.   ~.411m
>       d-      4.        .
>        d      4.   ~.822m
>       d#      4.        .
>       e-      4.   ~.411m
>        e      4.   ~.822m
>       e#      4.        .
>        f      4.  ~.00123
>       f#      4.   ~.411m
>       g-      4.        .
>        g      4.  ~.00206
>       g#      4.        .
>       a-      4.   ~.411m
>        a      4.   ~.822m
>       a#      4.        .
>       b-      4.   ~.822m
>        b      4.  ~.00288
>       NA      4.        .
>        c       2  ~.00493
>       c#       2  ~.00493
>       d-       2   ~.411m
>        d       2  ~.01320
>       d#       2  ~.00123
>       e-       2   ~.411m
>        e       2  ~.01360
>       e#       2   ~.411m
>        f       2  ~.00288
>       f#       2  ~.00863
>       g-       2        .
>        g       2  ~.00575
>       g#       2  ~.00452
>       a-       2  ~.00164
>        a       2  ~.01730
>       a#       2        .
>       b-       2   ~.822m
>        b       2  ~.01070
>       NA       2        .
>        c      2.   ~.411m
>       c#      2.   ~.822m
>       d-      2.        .
>        d      2.  ~.00123
>       d#      2.        .
>       e-      2.        .
>        e      2.  ~.00123
>       e#      2.        .
>        f      2.   ~.822m
>       f#      2.   ~.411m
>       g-      2.        .
>        g      2.   ~.822m
>       g#      2.        .
>       a-      2.        .
>        a      2.  ~.00123
>       a#      2.        .
>       b-      2.        .
>        b      2.  ~.00164
>       NA      2.        .
>        c       1        .
>       c#       1        .
>       d-       1        .
>        d       1        .
>       d#       1        .
>       e-       1        .
>        e       1   ~.411m
>       e#       1        .
>        f       1        .
>       f#       1        .
>       g-       1        .
>        g       1        .
>       g#       1        .
>       a-       1        .
>        a       1        .
>       a#       1        .
>       b-       1        .
>        b       1        .
>       NA       1        .
>    Pitch  Rhythm        p
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded
```

Cool! But what are the `~` and `m` about? The tilde (`~`) means that the
probability printed in the table is being rounded—because who wants to
see their screen filled with numbers like `0.0012449583492384`? By
rounding off probabilities when showing you the
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
table, humdrum$_{\mathbb{R}}$ keeps it (relatively) readable. The little
`m` serves a similar purpose: the “m” is short for “milli,” which means
the probability is actually one thousand times smaller than printed.
Thus, the probability `0.000012` will print as `0.012m`—again, to keep
things (relatively) readable. If you get really small values, you might
see a $\mu$ printed, which is short for “micro” (one millionth).

If we really want a readable table, we might just want to filter out the
really small numbers, which we can do just like we did with a
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
table, except we’ll refer to `p` (probability) instead of `n` (count).

``` r
bach |>  
  pdist(Pitch, Rhythm) |>
  filter(p > .001)
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch   Rhythm                                             
>                16        8        4       4.        2       2.
>       NA   .        .       ~.00167   .        .        .     
>        c   .       ~.03260  ~.05730   .       ~.00501   .     
>       c#   .       ~.00878  ~.03220   .       ~.00501   .     
>       d-   .       ~.00585  ~.00836   .        .        .     
>        d  ~.00125  ~.03380  ~.05520   .       ~.01340  ~.00125
>       d#   .       ~.00418  ~.00961   .       ~.00125   .     
>       e-   .       ~.00543  ~.00794   .        .        .     
>        e  ~.00125  ~.03840  ~.08360   .       ~.01380  ~.00125
>       e#   .        .       ~.00293   .        .        .     
>        f   .       ~.01300  ~.03260  ~.00125  ~.00293   .     
>       f#   .       ~.02880  ~.03720   .       ~.00878   .     
>        g   .       ~.03090  ~.05930  ~.00209  ~.00585   .     
>       g#   .       ~.01300  ~.02800   .       ~.00460   .     
>       a-   .       ~.00543  ~.01130   .       ~.00167   .     
>        a   .       ~.03430  ~.07860   .       ~.01760  ~.00125
>       a#   .       ~.00167  ~.00293   .        .        .     
>       b-   .       ~.00836  ~.01420   .        .        .     
>        b   .       ~.03050  ~.07730  ~.00293  ~.01090  ~.00167
>                16        8        4       4.        2       2.
>    Pitch   Rhythm                                             
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded
```

#### Joint and conditional probabilities

When we apply
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
to two or more variables, it by default computes the *joint
probability*: the probability of each combination of values. These
probabilities (the entire multidimensional table) will sum to 1. It is
often useful to instead compute *conditional probabilities*: the
probabilities of observing some variables *given* other variables. We
can do this with the `condition` argument. Simply indicate which
dimension(s) to condition on, either by index or by name:

``` r
pdist(v1, v2, condition = 2)
>    humdrumR probability distribution P(v1|v2), ~rounded 
>    v1     v2              
>            A      B      C
>    NA  ~.333      .      .
>     a  ~.333   .500   .400
>     b  ~.333   .500   .200
>     c      .      .   .400
>            A      B      C
>    v1     v2              
>    humdrumR probability distribution P(v1|v2), ~rounded

pdist(lower = v1, upper = v2, condition = 'lower')
>    humdrumR probability distribution P(upper|lower), ~rounded 
>    lower   upper               
>                A      B       C
>       NA   1.000      .       .
>        a    .250   .250    .500
>        b   ~.333  ~.333   ~.333
>        c       .      .   1.000
>                A      B       C
>    lower   upper               
>    humdrumR probability distribution P(upper|lower), ~rounded

bach |>
  pdist(Pitch, Rhythm, condition = 'Rhythm')
>    humdrumR probability distribution P(Pitch|Rhythm), ~rounded 
>    Pitch  Rhythm         p
>        c      16   ~.11800
>       c#      16   ~.11800
>       d-      16         .
>        d      16   ~.17600
>       d#      16   ~.05880
>       e-      16         .
>        e      16   ~.17600
>       e#      16         .
>        f      16         .
>       f#      16   ~.05880
>       g-      16         .
>        g      16         .
>       g#      16         .
>       a-      16         .
>        a      16   ~.05880
>       a#      16         .
>       b-      16   ~.11800
>        b      16   ~.11800
>       NA      16         .
>        c       8   ~.11000
>       c#       8   ~.02970
>       d-       8   ~.01980
>        d       8   ~.11500
>       d#       8   ~.01420
>       e-       8   ~.01840
>        e       8   ~.13000
>       e#       8         .
>        f       8   ~.04390
>       f#       8   ~.09770
>       g-       8         .
>        g       8   ~.10500
>       g#       8   ~.04390
>       a-       8   ~.01840
>        a       8   ~.11600
>       a#       8   ~.00567
>       b-       8   ~.02830
>        b       8   ~.10300
>       NA       8         .
>        c      8.   1.00000
>       c#      8.         .
>       d-      8.         .
>        d      8.         .
>       d#      8.         .
>       e-      8.         .
>        e      8.         .
>       e#      8.         .
>        f      8.         .
>       f#      8.         .
>       g-      8.         .
>        g      8.         .
>       g#      8.         .
>       a-      8.         .
>        a      8.         .
>       a#      8.         .
>       b-      8.         .
>        b      8.         .
>       NA      8.         .
>        c       4   ~.09530
>       c#       4   ~.05360
>       d-       4   ~.01390
>        d       4   ~.09190
>       d#       4   ~.01600
>       e-       4   ~.01320
>        e       4   ~.13900
>       e#       4   ~.00487
>        f       4   ~.05430
>       f#       4   ~.06190
>       g-       4    ~.696m
>        g       4   ~.09880
>       g#       4   ~.04660
>       a-       4   ~.01880
>        a       4   ~.13100
>       a#       4   ~.00487
>       b-       4   ~.02370
>        b       4   ~.12900
>       NA       4   ~.00278
>        c      4.   ~.03570
>       c#      4.   ~.03570
>       d-      4.         .
>        d      4.   ~.07140
>       d#      4.         .
>       e-      4.   ~.03570
>        e      4.   ~.07140
>       e#      4.         .
>        f      4.   ~.10700
>       f#      4.   ~.03570
>       g-      4.         .
>        g      4.   ~.17900
>       g#      4.         .
>       a-      4.   ~.03570
>        a      4.   ~.07140
>       a#      4.         .
>       b-      4.   ~.07140
>        b      4.    .25000
>       NA      4.         .
>        c       2   ~.05410
>       c#       2   ~.05410
>       d-       2   ~.00450
>        d       2   ~.14400
>       d#       2   ~.01350
>       e-       2   ~.00450
>        e       2   ~.14900
>       e#       2   ~.00450
>        f       2   ~.03150
>       f#       2   ~.09460
>       g-       2         .
>        g       2   ~.06310
>       g#       2   ~.04950
>       a-       2   ~.01800
>        a       2   ~.18900
>       a#       2         .
>       b-       2   ~.00901
>        b       2   ~.11700
>       NA       2         .
>        c      2.   ~.04760
>       c#      2.   ~.09520
>       d-      2.         .
>        d      2.   ~.14300
>       d#      2.         .
>       e-      2.         .
>        e      2.   ~.14300
>       e#      2.         .
>        f      2.   ~.09520
>       f#      2.   ~.04760
>       g-      2.         .
>        g      2.   ~.09520
>       g#      2.         .
>       a-      2.         .
>        a      2.   ~.14300
>       a#      2.         .
>       b-      2.         .
>        b      2.   ~.19000
>       NA      2.         .
>        c       1         .
>       c#       1         .
>       d-       1         .
>        d       1         .
>       d#       1         .
>       e-       1         .
>        e       1   1.00000
>       e#       1         .
>        f       1         .
>       f#       1         .
>       g-       1         .
>        g       1         .
>       g#       1         .
>       a-       1         .
>        a       1         .
>       a#       1         .
>       b-       1         .
>        b       1         .
>       NA       1         .
>    Pitch  Rhythm         p
>    humdrumR probability distribution P(Pitch|Rhythm), ~rounded
```

So here we are seeing the probability of each pitch value *given* each
possible rhythmic value. Each *column* in this table will sum to 1.

### Marginal distributions

When you have a multi-dimensional count/proportion table, you might want
to “collapse” some dimensions, to get what’s called the marginal
distribution. You can do this with a
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)/[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
table simply, by indexing by dimension (either by name or by index):

``` r
bach |>
  count(Pitch, Rhythm) -> twodim

bach |> 
  pdist(Pitch, Rhythm, Spine) -> threedim


twodim[ , 'Rhythm']
>    humdrumR count distribution, ~rounded 
>    Rhythm       n
>        16      17
>         8     706
>        8.       1
>         4  ~1.44k
>        4.      28
>         2     222
>        2.      21
>         1       1
>    Rhythm       n
>    humdrumR count distribution, ~rounded

threedim[ , 'Rhythm']
>    humdrumR probability distribution P(Rhythm), ~rounded 
>    Rhythm        p
>        16  ~.00699
>         8  ~.29000
>        8.   ~.411m
>         4  ~.59100
>        4.  ~.01150
>         2  ~.09120
>        2.  ~.00863
>         1   ~.411m
>    Rhythm        p
>    humdrumR probability distribution P(Rhythm), ~rounded
threedim[ , 2:3]
>    humdrumR probability distribution P(Rhythm,Spine), ~rounded 
>    Rhythm    Spine                           
>                  1        2        3        4
>        16   ~.822m  ~.00370  ~.00164   ~.822m
>         8  ~.09740  ~.08010  ~.07930  ~.03330
>        8.        .   ~.411m        .        .
>         4  ~.14800  ~.14400  ~.14300  ~.15500
>        4.   ~.822m  ~.00288  ~.00288  ~.00493
>         2  ~.01850  ~.02220  ~.02420  ~.02630
>        2.  ~.00206  ~.00247  ~.00164  ~.00247
>         1        .        .        .   ~.411m
>                  1        2        3        4
>    Rhythm    Spine                           
>    humdrumR probability distribution P(Rhythm,Spine), ~rounded
```

This is nice because you don’t have to separately compute tables for
different combinations of your variables: you can just compute one table
with all the variables, then index that table to get different marginal
tables that might interest you.

### Combining tables

Sometimes, we make multiple tables of different data samples:

``` r

chor1 <- bach |> filter(File == 1) |> count(Pitch)
chor2 <- bach |> filter(File == 2) |> count(Pitch)

chor1
>    humdrumR count distribution 
>    Pitch   n
>        c  27
>       c#   .
>        d  48
>       e-   .
>        e  21
>        f   1
>       f#  22
>        g  50
>       a-   .
>        a  24
>       b-   .
>        b  36
>    Pitch   n
>    humdrumR count distribution
chor2
>    humdrumR count distribution 
>    Pitch   n
>        c   .
>       c#  33
>        d  24
>       d#   4
>       e-   .
>        e  41
>        f   .
>       f#  27
>        g   4
>       g#  23
>       a-   .
>        a  35
>       a#   3
>       b-   .
>        b  37
>    Pitch   n
>    humdrumR count distribution
```

Notice that our `chor1` and `chor2` tables have slightly different
categories—for example, the second chorale includes D#s, G#s and A#s
which never occur in the first chorale. What if we want to compare, or
combine these two tables? Luckily, humdrum$_{\mathbb{R}}$ makes this
easy:

``` r
chor1 + chor2
>    humdrumR count distribution 
>    Pitch   n
>        c  27
>       c#  33
>        d  72
>       e-   .
>       d#   4
>        e  62
>        f   1
>       f#  49
>        g  54
>       a-   .
>        a  59
>       g#  23
>       b-   .
>        b  73
>       a#   3
>    Pitch   n
>    humdrumR count distribution

cbind(chor1, chor2)
>         
>    Pitch [,1] [,2]
>       c    27    0
>       c#    0   33
>       d    48   24
>       e-    0    0
>       d#    0    4
>       e    21   41
>       f     1    0
>       f#   22   27
>       g    50    4
>       a-    0    0
>       a    24   35
>       g#    0   23
>       b-    0    0
>       b    36   37
>       a#    0    3
```

They correctly combined (by level). Note that this won’t work unless the
tables have the same dimension names—it worked here because both tables
have one dimension called `Kern`. Also, notice that the result of using
`cbind` is now a “plain” R data.frame—this is because it is not a single
distribution of data anymore, but rather, two distributions pasted
together.

#### Estimating joint probabilities

We’ve [seen](#marginal-distributions) that we can take a two (or more)
dimensional table and “collapse” one dimension to get a 1-dimensional
marginal distribution. What if want to reverse this? Combine two
one-dimensional distributions to get a two dimensional one? Wait! There
is *no a general way to correctly to do this*—just because we know the
marginal distributions of two variables doesn’t mean we know what their
joint distribution would be. However, if we **assume** that the two
distributions are *independent* of each other, we can compute what their
joint distribution *would* be—it ends up being the product of the
marginal distributions. We can think of this as a simple, “naive”
estimate of the joint probability.

To illustrate this, let’s compute Rhythm and Pitch for our Bach
chorales, then combine them. To compute the independent joint
distribution of two variables, we’ll use R’s outer-product operator,
`%o%`.

``` r
bach |> pdist(Pitch) -> pitch
bach |> pdist(Rhythm) -> rhythm

pitch %o% rhythm
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch  Rhythm         p
>        c      16  ~.66600m
>       c#      16  ~.33000m
>       d-      16  ~.10100m
>        d      16  ~.72700m
>       d#      16  ~.10600m
>       e-      16  ~.09760m
>        e      16  ~.95900m
>       e#      16  ~.02300m
>        f      16  ~.34700m
>       f#      16  ~.52300m
>       g-      16  ~.00287m
>        g      16  ~.68100m
>       g#      16  ~.31300m
>       a-      16  ~.12900m
>        a      16  ~.91300m
>       a#      16  ~.03160m
>       b-      16  ~.17200m
>        b      16  ~.85300m
>       NA      16  ~.01150m
>        c       8   ~.02770
>       c#       8   ~.01370
>       d-       8   ~.00417
>        d       8   ~.03020
>       d#       8   ~.00441
>       e-       8   ~.00406
>        e       8   ~.03980
>       e#       8  ~.95400m
>        f       8   ~.01440
>       f#       8   ~.02170
>       g-       8  ~.11900m
>        g       8   ~.02830
>       g#       8   ~.01300
>       a-       8   ~.00537
>        a       8   ~.03790
>       a#       8   ~.00131
>       b-       8   ~.00716
>        b       8   ~.03540
>       NA       8  ~.47700m
>        c      8.  ~.03920m
>       c#      8.  ~.01940m
>       d-      8.  ~.00591m
>        d      8.  ~.04270m
>       d#      8.  ~.00625m
>       e-      8.  ~.00574m
>        e      8.  ~.05640m
>       e#      8.  ~.00135m
>        f      8.  ~.02040m
>       f#      8.  ~.03070m
>       g-      8.    ~.169𝜇
>        g      8.  ~.04000m
>       g#      8.  ~.01840m
>       a-      8.  ~.00760m
>        a      8.  ~.05370m
>       a#      8.  ~.00186m
>       b-      8.  ~.01010m
>        b      8.  ~.05020m
>       NA      8.    ~.676𝜇
>        c       4   ~.05630
>       c#       4   ~.02790
>       d-       4   ~.00850
>        d       4   ~.06140
>       d#       4   ~.00898
>       e-       4   ~.00825
>        e       4   ~.08110
>       e#       4   ~.00194
>        f       4   ~.02940
>       f#       4   ~.04420
>       g-       4  ~.24300m
>        g       4   ~.05750
>       g#       4   ~.02650
>       a-       4   ~.01090
>        a       4   ~.07720
>       a#       4   ~.00267
>       b-       4   ~.01460
>        b       4   ~.07210
>       NA       4  ~.97100m
>        c      4.   ~.00110
>       c#      4.  ~.54400m
>       d-      4.  ~.16600m
>        d      4.   ~.00120
>       d#      4.  ~.17500m
>       e-      4.  ~.16100m
>        e      4.   ~.00158
>       e#      4.  ~.03780m
>        f      4.  ~.57200m
>       f#      4.  ~.86100m
>       g-      4.  ~.00473m
>        g      4.   ~.00112
>       g#      4.  ~.51600m
>       a-      4.  ~.21300m
>        a      4.   ~.00150
>       a#      4.  ~.05200m
>       b-      4.  ~.28400m
>        b      4.   ~.00140
>       NA      4.  ~.01890m
>        c       2   ~.00870
>       c#       2   ~.00431
>       d-       2   ~.00131
>        d       2   ~.00949
>       d#       2   ~.00139
>       e-       2   ~.00128
>        e       2   ~.01250
>       e#       2  ~.30000m
>        f       2   ~.00454
>       f#       2   ~.00683
>       g-       2  ~.03750m
>        g       2   ~.00889
>       g#       2   ~.00409
>       a-       2   ~.00169
>        a       2   ~.01190
>       a#       2  ~.41300m
>       b-       2   ~.00225
>        b       2   ~.01110
>       NA       2  ~.15000m
>        c      2.  ~.82300m
>       c#      2.  ~.40800m
>       d-      2.  ~.12400m
>        d      2.  ~.89800m
>       d#      2.  ~.13100m
>       e-      2.  ~.12100m
>        e      2.   ~.00118
>       e#      2.  ~.02840m
>        f      2.  ~.42900m
>       f#      2.  ~.64600m
>       g-      2.  ~.00355m
>        g      2.  ~.84100m
>       g#      2.  ~.38700m
>       a-      2.  ~.16000m
>        a      2.   ~.00113
>       a#      2.  ~.03900m
>       b-      2.  ~.21300m
>        b      2.   ~.00105
>       NA      2.  ~.01420m
>        c       1  ~.03920m
>       c#       1  ~.01940m
>       d-       1  ~.00591m
>        d       1  ~.04270m
>       d#       1  ~.00625m
>       e-       1  ~.00574m
>        e       1  ~.05640m
>       e#       1  ~.00135m
>        f       1  ~.02040m
>       f#       1  ~.03070m
>       g-       1    ~.169𝜇
>        g       1  ~.04000m
>       g#       1  ~.01840m
>       a-       1  ~.00760m
>        a       1  ~.05370m
>       a#       1  ~.00186m
>       b-       1  ~.01010m
>        b       1  ~.05020m
>       NA       1    ~.676𝜇
>    Pitch  Rhythm         p
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded
```

So here we see what the joint probability of Pitch and Rhythm would be
*if they were independent*. We can compare this to the *actual*
distribution of pitch and rhythm in the data:

``` r
bach |>
  pdist(Pitch, Rhythm)
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch  Rhythm        p
>        c      16   ~.822m
>       c#      16   ~.822m
>       d-      16        .
>        d      16  ~.00123
>       d#      16   ~.411m
>       e-      16        .
>        e      16  ~.00123
>       e#      16        .
>        f      16        .
>       f#      16   ~.411m
>       g-      16        .
>        g      16        .
>       g#      16        .
>       a-      16        .
>        a      16   ~.411m
>       a#      16        .
>       b-      16   ~.822m
>        b      16   ~.822m
>       NA      16        .
>        c       8  ~.03210
>       c#       8  ~.00863
>       d-       8  ~.00575
>        d       8  ~.03330
>       d#       8  ~.00411
>       e-       8  ~.00534
>        e       8  ~.03780
>       e#       8        .
>        f       8  ~.01270
>       f#       8  ~.02840
>       g-       8        .
>        g       8  ~.03040
>       g#       8  ~.01270
>       a-       8  ~.00534
>        a       8  ~.03370
>       a#       8  ~.00164
>       b-       8  ~.00822
>        b       8  ~.03000
>       NA       8        .
>        c      8.   ~.411m
>       c#      8.        .
>       d-      8.        .
>        d      8.        .
>       d#      8.        .
>       e-      8.        .
>        e      8.        .
>       e#      8.        .
>        f      8.        .
>       f#      8.        .
>       g-      8.        .
>        g      8.        .
>       g#      8.        .
>       a-      8.        .
>        a      8.        .
>       a#      8.        .
>       b-      8.        .
>        b      8.        .
>       NA      8.        .
>        c       4  ~.05630
>       c#       4  ~.03160
>       d-       4  ~.00822
>        d       4  ~.05430
>       d#       4  ~.00945
>       e-       4  ~.00781
>        e       4  ~.08220
>       e#       4  ~.00288
>        f       4  ~.03210
>       f#       4  ~.03660
>       g-       4   ~.411m
>        g       4  ~.05840
>       g#       4  ~.02750
>       a-       4  ~.01110
>        a       4  ~.07730
>       a#       4  ~.00288
>       b-       4  ~.01400
>        b       4  ~.07600
>       NA       4  ~.00164
>        c      4.   ~.411m
>       c#      4.   ~.411m
>       d-      4.        .
>        d      4.   ~.822m
>       d#      4.        .
>       e-      4.   ~.411m
>        e      4.   ~.822m
>       e#      4.        .
>        f      4.  ~.00123
>       f#      4.   ~.411m
>       g-      4.        .
>        g      4.  ~.00206
>       g#      4.        .
>       a-      4.   ~.411m
>        a      4.   ~.822m
>       a#      4.        .
>       b-      4.   ~.822m
>        b      4.  ~.00288
>       NA      4.        .
>        c       2  ~.00493
>       c#       2  ~.00493
>       d-       2   ~.411m
>        d       2  ~.01320
>       d#       2  ~.00123
>       e-       2   ~.411m
>        e       2  ~.01360
>       e#       2   ~.411m
>        f       2  ~.00288
>       f#       2  ~.00863
>       g-       2        .
>        g       2  ~.00575
>       g#       2  ~.00452
>       a-       2  ~.00164
>        a       2  ~.01730
>       a#       2        .
>       b-       2   ~.822m
>        b       2  ~.01070
>       NA       2        .
>        c      2.   ~.411m
>       c#      2.   ~.822m
>       d-      2.        .
>        d      2.  ~.00123
>       d#      2.        .
>       e-      2.        .
>        e      2.  ~.00123
>       e#      2.        .
>        f      2.   ~.822m
>       f#      2.   ~.411m
>       g-      2.        .
>        g      2.   ~.822m
>       g#      2.        .
>       a-      2.        .
>        a      2.  ~.00123
>       a#      2.        .
>       b-      2.        .
>        b      2.  ~.00164
>       NA      2.        .
>        c       1        .
>       c#       1        .
>       d-       1        .
>        d       1        .
>       d#       1        .
>       e-       1        .
>        e       1   ~.411m
>       e#       1        .
>        f       1        .
>       f#       1        .
>       g-       1        .
>        g       1        .
>       g#       1        .
>       a-       1        .
>        a       1        .
>       a#       1        .
>       b-       1        .
>        b       1        .
>       NA       1        .
>    Pitch  Rhythm        p
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded
```

These tables are, obviously, not exactly the same, because pitch
information and rhythm information in the Chorales are *not* totally
independent of each other. However, if you look closely, they are
actually not *that* different. Comparing the “naive” (independent) joint
distribution of variables with their *actual* joint distribution can be
quite useful, as we’ll see more below.

``` r
pitch %o% rhythm |>
  filter(p > .001)
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch   Rhythm                                    
>                 8        4       4.        2       2.
>        c  ~.02820  ~.05750  ~.00112  ~.00888   .     
>       c#  ~.01400  ~.02850   .       ~.00440   .     
>       d-  ~.00426  ~.00867   .       ~.00134   .     
>        d  ~.03080  ~.06270  ~.00122  ~.00969   .     
>       d#  ~.00450  ~.00917   .       ~.00142   .     
>       e-  ~.00414  ~.00843   .       ~.00130   .     
>        e  ~.04070  ~.08280  ~.00161  ~.01280  ~.00121
>       e#   .       ~.00198   .        .        .     
>        f  ~.01470  ~.03000   .       ~.00463   .     
>       f#  ~.02220  ~.04510   .       ~.00697   .     
>        g  ~.02890  ~.05870  ~.00114  ~.00907   .     
>       g#  ~.01330  ~.02700   .       ~.00417   .     
>       a-  ~.00548  ~.01120   .       ~.00172   .     
>        a  ~.03870  ~.07880  ~.00154  ~.01220  ~.00115
>       a#  ~.00134  ~.00273   .        .        .     
>       b-  ~.00730  ~.01490   .       ~.00230   .     
>        b  ~.03620  ~.07360  ~.00143  ~.01140  ~.00108
>                 8        4       4.        2       2.
>    Pitch   Rhythm                                    
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded

bach |>
  pdist(Pitch, Rhythm) |>
  filter(p > .001)
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded 
>    Pitch   Rhythm                                             
>                16        8        4       4.        2       2.
>       NA   .        .       ~.00167   .        .        .     
>        c   .       ~.03260  ~.05730   .       ~.00501   .     
>       c#   .       ~.00878  ~.03220   .       ~.00501   .     
>       d-   .       ~.00585  ~.00836   .        .        .     
>        d  ~.00125  ~.03380  ~.05520   .       ~.01340  ~.00125
>       d#   .       ~.00418  ~.00961   .       ~.00125   .     
>       e-   .       ~.00543  ~.00794   .        .        .     
>        e  ~.00125  ~.03840  ~.08360   .       ~.01380  ~.00125
>       e#   .        .       ~.00293   .        .        .     
>        f   .       ~.01300  ~.03260  ~.00125  ~.00293   .     
>       f#   .       ~.02880  ~.03720   .       ~.00878   .     
>        g   .       ~.03090  ~.05930  ~.00209  ~.00585   .     
>       g#   .       ~.01300  ~.02800   .       ~.00460   .     
>       a-   .       ~.00543  ~.01130   .       ~.00167   .     
>        a   .       ~.03430  ~.07860   .       ~.01760  ~.00125
>       a#   .       ~.00167  ~.00293   .        .        .     
>       b-   .       ~.00836  ~.01420   .        .        .     
>        b   .       ~.03050  ~.07730  ~.00293  ~.01090  ~.00167
>                16        8        4       4.        2       2.
>    Pitch   Rhythm                                             
>    humdrumR probability distribution P(Pitch,Rhythm), ~rounded
```

## Modeling probability and likelihood

Statisticians refer to the sorts of distributions created by
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
or and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
as *empirical distributions*—distributions of a sample of data. We will
often regard a empirical distribution as an estimate, or “model,” of
some real-world probability distribution. For example, the Bach chorales
are often considered an idealized model of “common practice tonality” in
general. If we observe, for example, that $\approx 1.04$ % of notes in
our Bach chorale data are E flats, we might estimate that about 1.04% of
notes in “common practice music” more generally are E flats. Or to put
it in a probabilistic way, if we randomly select a note from a
common-practice piece, the *estimated* probability of that note being an
E flat is about 1.04%—this estimate is based on our current model, which
is based on our empirical analysis of the first 10 chorales.

#### Likelihood

Sometimes, we’d like to ask what probability a model assigns to each
data point in sample. This kind of “retrospective probability”
assignment is called a *likelihood*. In humdrum$_{\mathbb{R}}$, we can
assign likelihoods quite easily using the
[`like()`](https://rdrr.io/pkg/data.table/man/like.html) function.
Consider this simple vector again:

``` r

v1 <- c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c', 'a')

p1 <- pdist(v1)
```

If we imagine that `v1` is created by randomly drawing from an
underlying probability distribution, then `p1` is an estimate of that
probability distribution—`p1` is our “model” of `v1`. We can then
compute the estimated likelihood of observing each data point, given
that model:

``` r
like(v1)
>     [1] 0.4 0.3 0.3 0.4 0.3 0.3 0.4 0.3 0.3 0.4
```

What’s happening here? Notice that
[`like()`](https://rdrr.io/pkg/data.table/man/like.html) assigns each
data point a likelihood by looking that value up in the `p1` table. For
example, `b` makes up `.3` of `v1`, so every time there is `b` in the
input, [`like()`](https://rdrr.io/pkg/data.table/man/like.html) assigns
`0.3`. If we do this with our Bach data, we’d see that every E flat that
appears gets that 1.04% (`0.0104`) value that we just talked about:

``` r
bach |> like(Pitch)
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern              **kern              **kern    ***
>                9:                *ICvox              *ICvox              *ICvox    ***
>               10:                *Ibass             *Itenor              *Ialto    ***
>               11:               *I"Bass            *I"Tenor             *I"Alto    ***
>               12:             *>[A,A,B]           *>[A,A,B]           *>[A,A,B]    ***
>               13:          *>norep[A,B]        *>norep[A,B]        *>norep[A,B]    ***
>               14:                   *>A                 *>A                 *>A    ***
>               15:               *clefF4            *clefGv2             *clefG2    ***
>               16:                *k[f#]              *k[f#]              *k[f#]    ***
>               17:                   *G:                 *G:                 *G:    ***
>               18:                 *M3/4               *M3/4               *M3/4    ***
>               19:                *MM100              *MM100              *MM100    ***
>               20:    0.0974106041923551   0.122071516646116   0.103986847513358    ***
>               21:                    =1                  =1                  =1    ***
>               22:    0.0974106041923551   0.122071516646116   0.103986847513358    ***
>               23:     0.137279079325935  0.0953555281545417   0.137279079325935    ***
>               24:                     .   0.122071516646116                   .    ***
>               25:    0.0748047677764077   0.130702836004932   0.103986847513358    ***
>               26:                    =2                  =2                  =2    ***
>               27:    0.0974106041923551  0.0974106041923551   0.103986847513358    ***
>               28:     0.103986847513358  0.0748047677764077                   .    ***
>               29:                     .                   .                   .    ***
>               30:     0.137279079325935  0.0974106041923551   0.122071516646116    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:     0.103986847513358  0.0748047677764077   0.103986847513358    ***
>               72:                     .  0.0974106041923551                   .    ***
>               73:     0.103986847513358                   .  0.0953555281545417    ***
>               74:                     .  0.0748047677764077                   .    ***
>               75:    0.0974106041923551  0.0974106041923551   0.122071516646116    ***
>               76:                   =11                 =11                 =11    ***
>               77:    0.0953555281545417  0.0974106041923551   0.137279079325935    ***
>               78:     0.130702836004932   0.130702836004932   0.137279079325935    ***
>               79:     0.137279079325935  0.0448006576243321   0.137279079325935    ***
>               80:                     .                   .   0.103986847513358    ***
>               81:                   =12                 =12                 =12    ***
>               82:    0.0497328401150843   0.130702836004932  0.0953555281545417    ***
>               83:    0.0953555281545417  0.0974106041923551  0.0953555281545417    ***
>               84:    0.0246609124537608  0.0974106041923551   0.103986847513358    ***
>               85:     0.130702836004932   0.130702836004932                   .    ***
>               86:                   =13                 =13                 =13    ***
>               87:    0.0448006576243321   0.122071516646116   0.103986847513358    ***
>               88:     0.130702836004932   0.130702836004932  0.0953555281545417    ***
>               89:     0.137279079325935  0.0448006576243321   0.122071516646116    ***
>               90:                    ==                  ==                  ==    ***
>               91:                    *-                  *-                  *-    ***
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
>                    (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *P(Pitch) :: numeric
>                Pitch    :: character (**kern tokens)
>                Rhythm   :: character (**recip tokens)
>                Token    :: character
```

We can extend this to two (or more) dimensions, where each likelihood is
estimated from the empirical joint probability of the input variables:

``` r
bach |> like(Pitch, Rhythm)
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                 **kern               **kern              **kern    ***
>                9:                 *ICvox               *ICvox              *ICvox    ***
>               10:                 *Ibass              *Itenor              *Ialto    ***
>               11:                *I"Bass             *I"Tenor             *I"Alto    ***
>               12:              *>[A,A,B]            *>[A,A,B]           *>[A,A,B]    ***
>               13:           *>norep[A,B]         *>norep[A,B]        *>norep[A,B]    ***
>               14:                    *>A                  *>A                 *>A    ***
>               15:                *clefF4             *clefGv2             *clefG2    ***
>               16:                 *k[f#]               *k[f#]              *k[f#]    ***
>               17:                    *G:                  *G:                 *G:    ***
>               18:                  *M3/4                *M3/4               *M3/4    ***
>               19:                 *MM100               *MM100              *MM100    ***
>               20:     0.0583641594739005   0.0760378133990958  0.0542540073982737    ***
>               21:                     =1                   =1                  =1    ***
>               22:     0.0583641594739005   0.0760378133990958  0.0542540073982737    ***
>               23:      0.082203041512536    0.032059186189889   0.082203041512536    ***
>               24:                      .   0.0300041101520756                   .    ***
>               25:     0.0365803534730785   0.0772708590217838  0.0542540073982737    ***
>               26:                     =2                   =2                  =2    ***
>               27:     0.0583641594739005   0.0583641594739005  0.0131524866420058    ***
>               28:     0.0542540073982737   0.0365803534730785                   .    ***
>               29:                      .                    .                   .    ***
>               30:      0.082203041512536   0.0583641594739005  0.0760378133990958    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:     0.0542540073982737   0.0283600493218249  0.0542540073982737    ***
>               72:                      .   0.0583641594739005                   .    ***
>               73:     0.0542540073982737                    .  0.0563090834360871    ***
>               74:                      .   0.0283600493218249                   .    ***
>               75:    0.00575421290587752  0.00575421290587752  0.0106863953966297    ***
>               76:                    =11                  =11                 =11    ***
>               77:    0.00493218249075216  0.00575421290587752  0.0135635018495684    ***
>               78:     0.0772708590217838   0.0772708590217838   0.082203041512536    ***
>               79:      0.082203041512536   0.0275380189066995  0.0378133990957665    ***
>               80:                      .                    .  0.0332922318125771    ***
>               81:                    =12                  =12                 =12    ***
>               82:      0.032059186189889   0.0772708590217838  0.0563090834360871    ***
>               83:     0.0563090834360871   0.0583641594739005  0.0563090834360871    ***
>               84:     0.0139745170571311   0.0583641594739005  0.0131524866420058    ***
>               85:     0.0772708590217838   0.0772708590217838                   .    ***
>               86:                    =13                  =13                 =13    ***
>               87:     0.0275380189066995   0.0760378133990958  0.0542540073982737    ***
>               88:     0.0772708590217838   0.0772708590217838  0.0563090834360871    ***
>               89:     0.0135635018495684  0.00452116728318948  0.0106863953966297    ***
>               90:                     ==                   ==                  ==    ***
>               91:                     *-                   *-                  *-    ***
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian ***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc., c.***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ######################## ^^^ chor010.krn ^^^ #########################
>                    (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *P(Pitch,Rhythm) :: numeric
>                Pitch           :: character (**kern tokens)
>                Rhythm          :: character (**recip tokens)
>                Token           :: character
```

Here’s a thought: what if we know the rhythm value in advance, and we
want to estimate the likelihood of each pitch given the rhythm? This is
the “conditional likelihood” and we can compute it using the same
`condition` argument we used with
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md):

``` r
bach |> like(Pitch, Rhythm, condition = 'Rhythm')
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern              **kern              **kern    ***
>                9:                *ICvox              *ICvox              *ICvox    ***
>               10:                *Ibass             *Itenor              *Ialto    ***
>               11:               *I"Bass            *I"Tenor             *I"Alto    ***
>               12:             *>[A,A,B]           *>[A,A,B]           *>[A,A,B]    ***
>               13:          *>norep[A,B]        *>norep[A,B]        *>norep[A,B]    ***
>               14:                   *>A                 *>A                 *>A    ***
>               15:               *clefF4            *clefGv2             *clefG2    ***
>               16:                *k[f#]              *k[f#]              *k[f#]    ***
>               17:                   *G:                 *G:                 *G:    ***
>               18:                 *M3/4               *M3/4               *M3/4    ***
>               19:                *MM100              *MM100              *MM100    ***
>               20:    0.0988169798190675   0.128740431454419  0.0918580375782881    ***
>               21:                    =1                  =1                  =1    ***
>               22:    0.0988169798190675   0.128740431454419  0.0918580375782881    ***
>               23:     0.139178844815588   0.110481586402266   0.139178844815588    ***
>               24:                     .   0.103399433427762                   .    ***
>               25:    0.0619345859429367   0.130828114126653  0.0918580375782881    ***
>               26:                    =2                  =2                  =2    ***
>               27:    0.0988169798190675  0.0988169798190675   0.144144144144144    ***
>               28:    0.0918580375782881  0.0619345859429367                   .    ***
>               29:                     .                   .                   .    ***
>               30:     0.139178844815588  0.0988169798190675   0.128740431454419    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:    0.0918580375782881  0.0977337110481587  0.0918580375782881    ***
>               72:                     .  0.0988169798190675                   .    ***
>               73:    0.0918580375782881                   .  0.0953375086986778    ***
>               74:                     .  0.0977337110481587                   .    ***
>               75:    0.0630630630630631  0.0630630630630631   0.117117117117117    ***
>               76:                   =11                 =11                 =11    ***
>               77:    0.0540540540540541  0.0630630630630631   0.148648648648649    ***
>               78:     0.130828114126653   0.130828114126653   0.139178844815588    ***
>               79:     0.139178844815588   0.046624913013222   0.130311614730878    ***
>               80:                     .                   .   0.114730878186969    ***
>               81:                   =12                 =12                 =12    ***
>               82:    0.0542797494780793   0.130828114126653  0.0953375086986778    ***
>               83:    0.0953375086986778  0.0988169798190675  0.0953375086986778    ***
>               84:      0.02366040361865  0.0988169798190675   0.144144144144144    ***
>               85:     0.130828114126653   0.130828114126653                   .    ***
>               86:                   =13                 =13                 =13    ***
>               87:     0.046624913013222   0.128740431454419  0.0918580375782881    ***
>               88:     0.130828114126653   0.130828114126653  0.0953375086986778    ***
>               89:     0.148648648648649  0.0495495495495496   0.117117117117117    ***
>               90:                    ==                  ==                  ==    ***
>               91:                    *-                  *-                  *-    ***
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
>                    (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *P(Pitch|Rhythm) :: numeric
>                Pitch           :: character (**kern tokens)
>                Rhythm          :: character (**recip tokens)
>                Token           :: character
```

#### Using a different model

Remember, there is no “true” or correct probability model for a given
data. We should never say the “probability of E flat **is**
1.04%”—rather, 1.04% is our “estimated probability of E flat.” It is
very common, as a basic analysis step, to compute the empirical
distribution of a data sample then immediately use that distribution as
a probability model of that data…essentially, using the data as a model
of itself. This is what
[`like()`](https://rdrr.io/pkg/data.table/man/like.html) does by
default. However, we might sometimes want to take a different tack,
assigning likelihoods to our data based on a different model. For
example, what if we observed a different sample of lowercase letters?:

``` r
v3 <- c('a', 'a','b','b','b','b','b','c','c')
```

There’s a much bigger proportion of bs here then we saw in `v1`. If we,
for whatever reason, think the `v1` model we had is a better model of
the “true” probability distribution we are interested in, we could
assign likelihoods to `v3` but using the `v1` probability model. To do
this, use the `model` argument. Look what the difference is:

``` r
like(lower = v3)
>    [1] 0.2222222 0.2222222 0.5555556 0.5555556 0.5555556 0.5555556 0.5555556
>    [8] 0.2222222 0.2222222

like(lower = v3, model = pdist(lower = v1))
>    [1] 0.4 0.4 0.3 0.3 0.3 0.3 0.3 0.3 0.3
```

We do this sort of thing with real data all the time. For example, we
might want to assign probabilities to the notes in the first Bach
chorale, based on the empirical probabilities observed in all the
*other* chorales. (This is similar to
“[leave-one-out](https://en.wikipedia.org/wiki/Cross-validation_(statistics)#Leave-one-out_cross-validation)”
cross validation.)

``` r

bach |>
   filter(File != 1) |>
   pdist(Pitch) -> allExcept1

bach |> like(Pitch, model = allExcept1)
>    ######################## vvv chor001.krn vvv #########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:                **kern              **kern              **kern    ***
>                9:                *ICvox              *ICvox              *ICvox    ***
>               10:                *Ibass             *Itenor              *Ialto    ***
>               11:               *I"Bass            *I"Tenor             *I"Alto    ***
>               12:             *>[A,A,B]           *>[A,A,B]           *>[A,A,B]    ***
>               13:          *>norep[A,B]        *>norep[A,B]        *>norep[A,B]    ***
>               14:                   *>A                 *>A                 *>A    ***
>               15:               *clefF4            *clefGv2             *clefG2    ***
>               16:                *k[f#]              *k[f#]              *k[f#]    ***
>               17:                   *G:                 *G:                 *G:    ***
>               18:                 *M3/4               *M3/4               *M3/4    ***
>               19:                *MM100              *MM100              *MM100    ***
>               20:    0.0848457350272232   0.118421052631579  0.0930127041742287    ***
>               21:                    =1                  =1                  =1    ***
>               22:    0.0848457350272232   0.118421052631579  0.0930127041742287    ***
>               23:     0.142014519056261  0.0930127041742287   0.142014519056261    ***
>               24:                     .   0.118421052631579                   .    ***
>               25:    0.0725952813067151   0.133393829401089  0.0930127041742287    ***
>               26:                    =2                  =2                  =2    ***
>               27:    0.0848457350272232  0.0848457350272232  0.0930127041742287    ***
>               28:    0.0930127041742287  0.0725952813067151                   .    ***
>               29:                     .                   .                   .    ***
>               30:     0.142014519056261  0.0848457350272232   0.118421052631579    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ######################## ^^^ chor001.krn ^^^ #########################
>    
>           (eight more pieces...)
>    
>    ######################## vvv chor010.krn vvv #########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:    0.0930127041742287  0.0725952813067151  0.0930127041742287    ***
>               72:                     .  0.0848457350272232                   .    ***
>               73:    0.0930127041742287                   .  0.0930127041742287    ***
>               74:                     .  0.0725952813067151                   .    ***
>               75:    0.0848457350272232  0.0848457350272232   0.118421052631579    ***
>               76:                   =11                 =11                 =11    ***
>               77:    0.0930127041742287  0.0848457350272232   0.142014519056261    ***
>               78:     0.133393829401089   0.133393829401089   0.142014519056261    ***
>               79:     0.142014519056261  0.0494555353901996   0.142014519056261    ***
>               80:                     .                   .  0.0930127041742287    ***
>               81:                   =12                 =12                 =12    ***
>               82:    0.0544464609800363   0.133393829401089  0.0930127041742287    ***
>               83:    0.0930127041742287  0.0848457350272232  0.0930127041742287    ***
>               84:    0.0272232304900181  0.0848457350272232  0.0930127041742287    ***
>               85:     0.133393829401089   0.133393829401089                   .    ***
>               86:                   =13                 =13                 =13    ***
>               87:    0.0494555353901996   0.118421052631579  0.0930127041742287    ***
>               88:     0.133393829401089   0.133393829401089  0.0930127041742287    ***
>               89:     0.142014519056261  0.0494555353901996   0.118421052631579    ***
>               90:                    ==                  ==                  ==    ***
>               91:                    *-                  *-                  *-    ***
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
>                    (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *P(Pitch) :: numeric
>                Pitch    :: character (**kern tokens)
>                Rhythm   :: character (**recip tokens)
>                Token    :: character
```

## Information Theory

Working with actually likelihoods, as we do above, is rarely a good idea
because, in practice, the likelihoods can get so small that we run into
problems representing them in our computers. Thus, we usually compute
the log of the likelihood, like this:

``` r
bach |>
  like(Pitch, model = allExcept1) |>
  within(LogLike = log(., base = 2))
>    ####################### vvv chor001.krn vvv ########################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:               **kern             **kern             **kern    ***
>                9:               *ICvox             *ICvox             *ICvox    ***
>               10:               *Ibass            *Itenor             *Ialto    ***
>               11:              *I"Bass           *I"Tenor            *I"Alto    ***
>               12:            *>[A,A,B]          *>[A,A,B]          *>[A,A,B]    ***
>               13:         *>norep[A,B]       *>norep[A,B]       *>norep[A,B]    ***
>               14:                  *>A                *>A                *>A    ***
>               15:              *clefF4           *clefGv2            *clefG2    ***
>               16:               *k[f#]             *k[f#]             *k[f#]    ***
>               17:                  *G:                *G:                *G:    ***
>               18:                *M3/4              *M3/4              *M3/4    ***
>               19:               *MM100             *MM100             *MM100    ***
>               20:    -3.55901404868352  -3.07800251200127  -3.42642840906571    ***
>               21:                   =1                 =1                 =1    ***
>               22:    -3.55901404868352  -3.07800251200127  -3.42642840906571    ***
>               23:    -2.81588966163854  -3.42642840906571  -2.81588966163854    ***
>               24:                    .  -3.07800251200127                  .    ***
>               25:     -3.7839804136838  -2.90623616373479  -3.42642840906571    ***
>               26:                   =2                 =2                 =2    ***
>               27:    -3.55901404868352  -3.55901404868352  -3.42642840906571    ***
>               28:    -3.42642840906571   -3.7839804136838                  .    ***
>               29:                    .                  .                  .    ***
>               30:    -2.81588966163854  -3.55901404868352  -3.07800251200127    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ####################### ^^^ chor001.krn ^^^ ########################
>    
>           (eight more pieces...)
>    
>    ####################### vvv chor010.krn vvv ########################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:    -3.42642840906571   -3.7839804136838  -3.42642840906571    ***
>               72:                    .  -3.55901404868352                  .    ***
>               73:    -3.42642840906571                  .  -3.42642840906571    ***
>               74:                    .   -3.7839804136838                  .    ***
>               75:    -3.55901404868352  -3.55901404868352  -3.07800251200127    ***
>               76:                  =11                =11                =11    ***
>               77:    -3.42642840906571  -3.55901404868352  -2.81588966163854    ***
>               78:    -2.90623616373479  -2.90623616373479  -2.81588966163854    ***
>               79:    -2.81588966163854  -4.33772418379423  -2.81588966163854    ***
>               80:                    .                  .  -3.42642840906571    ***
>               81:                  =12                =12                =12    ***
>               82:    -4.19901791296264  -2.90623616373479  -3.42642840906571    ***
>               83:    -3.42642840906571  -3.55901404868352  -3.42642840906571    ***
>               84:    -5.19901791296264  -3.55901404868352  -3.42642840906571    ***
>               85:    -2.90623616373479  -2.90623616373479                  .    ***
>               86:                  =13                =13                =13    ***
>               87:    -4.33772418379423  -3.07800251200127  -3.42642840906571    ***
>               88:    -2.90623616373479  -2.90623616373479  -3.42642840906571    ***
>               89:    -2.81588966163854  -4.33772418379423  -3.07800251200127    ***
>               90:                   ==                 ==                 ==    ***
>               91:                   *-                 *-                 *-    ***
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Sebas***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf u***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, Inc***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ####################### ^^^ chor010.krn ^^^ ########################
>                  (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>               *LogLike  :: numeric
>                P(Pitch) :: numeric
>                Pitch    :: character (**kern tokens)
>                Rhythm   :: character (**recip tokens)
>                Token    :: character
```

That’s the “log likelihood.”

> By default, humdrum$_{\mathbb{R}}$ uses base-2 logarithms in all our
> information-theory-related functions. This means that the values we
> get are “bits”—each bit is equivalent to a 50/50 chance, or one fair
> coin flip. You can use other bases by setting the `base` argument in
> our information-theory functions.

Log likelihoods are always negative numbers, with larger negative
numbers indicating lower probability. By convention, we like to invert
that relationship by multiplying the log likelihood by `-1`. If we do
this, we get whats called the **information content** of each data
point. The smaller the information content the *more likely*—and thus
more predictable and less informative—a data point is. The higher the
information content, the *less likely*—and thus more surprising and more
informative—a data point is. This way of interpreting inverted log
probabilities is the core of **information theory**. We can estimate the
information content directly using the
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
function, which is used just like the
[`like()`](https://rdrr.io/pkg/data.table/man/like.html) function:

``` r
info(v1)
>     [1] 1.321928 1.736966 1.736966 1.321928 1.736966 1.736966 1.321928 1.736966
>     [9] 1.736966 1.321928

bach |>
  info(Pitch, model = allExcept1)
>    ###################### vvv chor001.krn vvv ######################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:              **kern            **kern            **kern    ***
>                9:              *ICvox            *ICvox            *ICvox    ***
>               10:              *Ibass           *Itenor            *Ialto    ***
>               11:             *I"Bass          *I"Tenor           *I"Alto    ***
>               12:           *>[A,A,B]         *>[A,A,B]         *>[A,A,B]    ***
>               13:        *>norep[A,B]      *>norep[A,B]      *>norep[A,B]    ***
>               14:                 *>A               *>A               *>A    ***
>               15:             *clefF4          *clefGv2           *clefG2    ***
>               16:              *k[f#]            *k[f#]            *k[f#]    ***
>               17:                 *G:               *G:               *G:    ***
>               18:               *M3/4             *M3/4             *M3/4    ***
>               19:              *MM100            *MM100            *MM100    ***
>               20:    3.55901404868352  3.07800251200127  3.42642840906571    ***
>               21:                  =1                =1                =1    ***
>               22:    3.55901404868352  3.07800251200127  3.42642840906571    ***
>               23:    2.81588966163854  3.42642840906571  2.81588966163854    ***
>               24:                   .  3.07800251200127                 .    ***
>               25:     3.7839804136838  2.90623616373479  3.42642840906571    ***
>               26:                  =2                =2                =2    ***
>               27:    3.55901404868352  3.55901404868352  3.42642840906571    ***
>               28:    3.42642840906571   3.7839804136838                 .    ***
>               29:                   .                 .                 .    ***
>               30:    2.81588966163854  3.55901404868352  3.07800251200127    ***
>    31-133:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ###################### ^^^ chor001.krn ^^^ ######################
>    
>           (eight more pieces...)
>    
>    ###################### vvv chor010.krn vvv ######################
>      1-70:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:    3.42642840906571   3.7839804136838  3.42642840906571    ***
>               72:                   .  3.55901404868352                 .    ***
>               73:    3.42642840906571                 .  3.42642840906571    ***
>               74:                   .   3.7839804136838                 .    ***
>               75:    3.55901404868352  3.55901404868352  3.07800251200127    ***
>               76:                 =11               =11               =11    ***
>               77:    3.42642840906571  3.55901404868352  2.81588966163854    ***
>               78:    2.90623616373479  2.90623616373479  2.81588966163854    ***
>               79:    2.81588966163854  4.33772418379423  2.81588966163854    ***
>               80:                   .                 .  3.42642840906571    ***
>               81:                 =12               =12               =12    ***
>               82:    4.19901791296264  2.90623616373479  3.42642840906571    ***
>               83:    3.42642840906571  3.55901404868352  3.42642840906571    ***
>               84:    5.19901791296264  3.55901404868352  3.42642840906571    ***
>               85:    2.90623616373479  2.90623616373479                 .    ***
>               86:                 =13               =13               =13    ***
>               87:    4.33772418379423  3.07800251200127  3.42642840906571    ***
>               88:    2.90623616373479  2.90623616373479  3.42642840906571    ***
>               89:    2.81588966163854  4.33772418379423  3.07800251200127    ***
>               90:                  ==                ==                ==    ***
>               91:                  *-                *-                *-    ***
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Se***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkop***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. B***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, ***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.***
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ###################### ^^^ chor010.krn ^^^ ######################
>               (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                Pitch    :: character (**kern tokens)
>                Rhythm   :: character (**recip tokens)
>                Token    :: character
>               *h(Pitch) :: numeric
```

Notice that is exactly like the log likelihood, just negated (`-3.42`
becomes `3.42`). So the fancy sounding term “information content” just
means the negated log of the likelihood assigned to a data point by a
probability model.

### Entropy

Information content and (log)likelihood associate values with each and
every data point. If we have a random variable (or sample from one), we
can also ask how much information each data point contains *on average*.
This is called the (Shannon) **entropy**. Entropy is simply the average
information content—which is really a property of the probability
distribution. To compute entropy, we can pass a probability distribution
(created with
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md))
to the
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
function. (You can also pass variables straight to
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
and it will run
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
automatically.)

``` r
v1 |> pdist() |> entropy()
>       H(v1) 
>    1.570951


bach |> entropy(Pitch)
>    H(Pitch) 
>    3.627846
```

We should remind ourselves that what we are computing here is the
*empirical entropy*—the entropy of the probability distribution we are
estimating from our data. The *true* entropy of pitch in common practice
music is certainly not this exact number, but we might argue/hope that
the empirical entropy we observe in these ten Bach chorales is a
reasonable estimate of the true entropy of common practice music. Also,
we are only considering the entropy/likelihood of pitches, taken as a
set of independent observations—this is a very simplistic perspective on
pitch in music, so we aren’t modeling the “empirical entropy of the Bach
chorales,” but rather that “empirical entropy of the marginal
distribution of pitch in the Bach chorales.”

#### Multi-dimensional entropy

The idea of entropy can be extended to multiple dimensions—its really no
different than what we’ve been doing throughout this article. We can
compute the empirical *joint* entropy of two or more variables like
this:

``` r
bach |>
  entropy(Pitch, Rhythm)
>    H(Pitch,Rhythm) 
>           5.067266

bach |>
  entropy(Pitch, Rhythm, Spine)
>    H(Pitch,Rhythm,Spine) 
>                 6.866279
```

Alternatively, we can compute the *conditional entropy*—which is simply
the average information content of the *conditional probabilities*. As
before, we just need to provide that `conditional` argument:

``` r
bach |>
  entropy(Pitch, Rhythm, condition = 'Rhythm')
>    H(Pitch|Rhythm) 
>           3.592855
```

##### Mutual Information

Notice that the conditional entropy of pitch *given rhythm* (3.59), is
slightly lower than the marginal entropy of pitch (3.63). This shows us
that *knowing* the rhythm reduces our uncertainty with regard to pitch
(slightly)—or we can say that the rhythm information provides us a
little bit of information about the pitch. Any “shared” information like
this, between two or more variables, is called their *mutual
information*. We can compute the mutual information directly using the
\[mutual()\] function:

``` r
bach |>
  mutual(Pitch, Rhythm)
>    I(Pitch;Rhythm) 
>         0.03499102
```

Remember [above](#estimating-joint-probabilities), where we saw that we
could compare the “naive” joint probability of two (or more) variables
with the actual, observed, joint distribution? As it happens, the mutual
information quantifies exactly how different the naive and actual joint
distributions are. If they are very different, that would mean there is
a lot of shared (mutual) information. If not (like here), the mutual
information is low.

The small mutual information between pitch and rhythm reflects that,
well, chorale music doesn’t have a lot rhythmic or pitch variety, so
there isn’t much information to share. However, if we dig around a
little bit, we should be able to find some other variables in our data
that will have more interesting relationships. For example, let’s
compare the distribution of the complex pitches (*including* octave
information) with the vocal part (soprano, alto, tenor, bass), which is
captured in the `Instrument` field. We’ll start by looking at the
marginal (independent) entropy of each variable:

``` r
bach |> select(Token) |> mutate(ComplexPitch = kern(Token)) -> bach

bach |> entropy(Instrument)
>    H(Instrument) 
>         1.996911
bach |> entropy(ComplexPitch)
>    H(ComplexPitch) 
>           5.017594
```

What we see here makes sense: the entropy of the voice is almost exactly
two bits, because there are four categories that are just about equally
common. The entropy of the complex pitch is quite a bit higher (5.02)
than the entropy of the simple pitch was (3.62). In information theory
terms, complex pitch contains more information than simple pitch (that
makes sense right?). But what if we know the vocal part we are looking
at? How does this affect our uncertainty/knowledge about the complex
pitch?

``` r
bach |>
  entropy(ComplexPitch, Instrument, condition = 'Instrument')
>    H(ComplexPitch|Instrument) 
>                      3.773724
```

Woh, it knocks it down to about `3.77` bits, almost as low as the
entropy of the simple pitch information! Basically, for chorale music,
the distribution of pitches *within* each voice is pretty much the same
as the distribution of simple pitches in general. This makes sense
because each voice in a chorale will very rarely cover more than one
octave anyway, right? The fact that the conditional entropy of complex
pitch is a little bit higher than the entropy of simple pitch probably
reflects that the voices do occasionally use more than one octave
(especially the bass), so knowing the octave information does convey a
little information beyond just knowing the voice.

Based on what we see here, we should know that the mutual information
between complex pitch and vocal part must be fairly large:

``` r
bach |> mutual(ComplexPitch, Instrument)
>    I(ComplexPitch;Instrument) 
>                       1.24387
```

The mutual information in our data is about 1.24 bits. We can interpret
this as saying that knowing which voice is singing provides us with
`1.24` bits of information about the complex pitch (and vice versa).

##### Pointwise mutual information

We aren’t quite done with mutual information yet. We’ve [already
learned](#entropy) that the entropy of a distribution is the average of
the information content of all the observations. Similarly, we can view
mutual information as an average. But an average of what? Basically, if
we look at each data point (which may includes multiple variables) we
can compare the estimated information content based on the actual
(observed) joint probability with the “naive” joint probability which we
would observe if the variables were independent. When they are
different, it means that a data combination is more or less probable
than we’d expect *if the variables were independent*. The average of the
log of the ratio between the two values is the mutual information; The
individual log ratios (one for each data point) are then the *pointwise
mutual information*. Luckily for us, humdrum$_{\mathbb{R}}$ includes the
[`pmutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
function, which we can use just like
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
or [`like()`](https://rdrr.io/pkg/data.table/man/like.html):

``` r
bach |>
  filter(File == 1) |>
  pmutual(Pitch, Rhythm)
>    ######################## vvv chor001.krn vvv #########################
>        1:  !!!COM: Bach, Johann Sebastian
>        2:  !!!CDT: 1685/02/21/-1750/07/28/
>        3:  !!!OTL@@DE: Aus meines Herzens Grunde
>        4:  !!!OTL@EN:      From the Depths of My Heart
>        5:  !!!SCT: BWV 269
>        6:  !!!PC#: 1
>        7:  !!!AGN: chorale
>        8:                 **kern              **kern               **kern    ***
>        9:                 *ICvox              *ICvox               *ICvox    ***
>       10:                 *Ibass             *Itenor               *Ialto    ***
>       11:                *I"Bass            *I"Tenor              *I"Alto    ***
>       12:              *>[A,A,B]           *>[A,A,B]            *>[A,A,B]    ***
>       13:           *>norep[A,B]        *>norep[A,B]         *>norep[A,B]    ***
>       14:                    *>A                 *>A                  *>A    ***
>       15:                *clefF4            *clefGv2              *clefG2    ***
>       16:                 *k[f#]              *k[f#]               *k[f#]    ***
>       17:                    *G:                 *G:                  *G:    ***
>       18:                  *M3/4               *M3/4                *M3/4    ***
>       19:                 *MM100              *MM100               *MM100    ***
>       20:      0.146437997841273    0.22181980968341   -0.129087352175718    ***
>       21:                     =1                  =1                   =1    ***
>       22:      0.146437997841273    0.22181980968341   -0.129087352175718    ***
>       23:      0.124958270430821   0.598889458763233    0.124958270430821    ***
>       24:                      .  -0.553113634681816                    .    ***
>       25:      -0.20519033126151   -0.65264930823273   -0.129087352175718    ***
>       26:                     =2                  =2                   =2    ***
>       27:      0.146437997841273   0.146437997841273    0.875729664122058    ***
>       28:     -0.129087352175718   -0.20519033126151                    .    ***
>       29:                      .                   .                    .    ***
>       30:      0.124958270430821   0.146437997841273     0.22181980968341    ***
>       31:                     =3                  =3                   =3    ***
>       32:     0.0843162859334758   0.598889458763233    0.446886365318184    ***
>       33:                      .  -0.553113634681816   -0.745758712624212    ***
>       34:     -0.553113634681816  0.0843162859334758    0.446886365318184    ***
>       35:      0.616811366760496                   .    0.742342248844355    ***
>       36:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>       37:                     =4                  =4                   =4    ***
>       38:      0.875729664122058   0.875729664122058   -0.320667548681445    ***
>       39:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>       40:                     =5                  =5                   =5    ***
>       41:      -0.20519033126151   -0.65264930823273   -0.129087352175718    ***
>       42:      0.146437997841273    0.22181980968341    0.124958270430821    ***
>       43:      -0.65264930823273  0.0843162859334758    -0.20519033126151    ***
>       44:                     =6                  =6                   =6    ***
>       45:       0.22181980968341  -0.129087352175718  -0.0900546205400286    ***
>       46:     0.0843162859334758   0.124958270430821                    .    ***
>       47:     -0.129087352175718  -0.745758712624212    -0.20519033126151    ***
>       48:                      .   0.598889458763233                    .    ***
>       49:                     =7                  =7                   =7    ***
>       50:    -0.0900546205400286   -1.03116093148646    0.875729664122058    ***
>       51:                   =:|!                =:|!                 =:|!    ***
>       52:                    *>B                 *>B                  *>B    ***
>       53:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>       54:                     =8                  =8                   =8    ***
>       55:      0.146437997841273  -0.129087352175718   -0.612007323735384    ***
>       56:                      .                   .    0.742342248844355    ***
>       57:      -0.65264930823273  0.0843162859334758    0.446886365318184    ***
>       58:                      .                   .    0.742342248844355    ***
>       59:       0.22181980968341  -0.553113634681816    0.146437997841273    ***
>       60:                      .   0.616811366760496                    .    ***
>       61:                     =9                  =9                   =9    ***
>       62:       1.25424128737579  -0.553113634681816   -0.612007323735384    ***
>       63:                      .   0.598889458763233    0.616811366760496    ***
>       64:                      .  -0.129087352175718   -0.612007323735384    ***
>       65:      0.616811366760496                   .    0.742342248844355    ***
>       66:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>       67:                    =10                 =10                  =10    ***
>       68:      0.875729664122058   0.875729664122058   -0.320667548681445    ***
>       69:      0.124958270430821    0.22181980968341    0.124958270430821    ***
>       70:                    =11                 =11                  =11    ***
>       71:      0.124958270430821   0.146437997841273    0.124958270430821    ***
>       72:     -0.129087352175718    0.22181980968341    0.742342248844355    ***
>       73:                      .                   .   -0.612007323735384    ***
>       74:     0.0843162859334758   0.124958270430821    -0.65264930823273    ***
>       75:                    =12                 =12                  =12    ***
>       76:       1.25424128737579   0.875729664122058    -0.65264930823273    ***
>       77:                      .                   .    0.780310099043376    ***
>       78:      0.598889458763233                   .                    .    ***
>       79:     -0.129087352175718  -0.129087352175718                    .    ***
>       80:                      .                   .    0.742342248844355    ***
>       81:                    =13                 =13                  =13    ***
>       82:     -0.612007323735384   0.669278786654632  -0.0900546205400286    ***
>       83:      0.616811366760496                   .                    .    ***
>       84:       0.22181980968341                   .                    .    ***
>       85:      0.146437997841273                   .    0.932313192488426    ***
>       86:                    =14                 =14                  =14    ***
>       87:      -1.20108593292877   -1.20108593292877   -0.838515853544064    ***
>       88:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>       89:                    =15                 =15                  =15    ***
>       90:      -0.20519033126151  -0.745758712624212    0.254241287375788    ***
>       91:                      .   0.598889458763233                    .    ***
>       92:      0.146437997841273    0.22181980968341                    .    ***
>       93:                      .                   .   -0.612007323735384    ***
>       94:      -0.65264930823273  0.0843162859334758    -0.20519033126151    ***
>       95:                    =16                 =16                  =16    ***
>       96:       0.22181980968341   0.875729664122058  -0.0900546205400286    ***
>       97:      0.146437997841273                   .                    .    ***
>       98:     -0.129087352175718  -0.745758712624212    -0.20519033126151    ***
>       99:                      .   0.598889458763233                    .    ***
>      100:                    =17                 =17                  =17    ***
>      101:      0.446886365318184    0.22181980968341    0.742342248844355    ***
>      102:     -0.745758712624212                   .    0.446886365318184    ***
>      103:      0.598889458763233  0.0843162859334758    0.446886365318184    ***
>      104:     -0.553113634681816                   .    0.742342248844355    ***
>      105:      0.616811366760496  -0.129087352175718    0.146437997841273    ***
>      106:     -0.612007323735384                   .                    .    ***
>      107:                    =18                 =18                  =18    ***
>      108:      0.875729664122058   0.875729664122058   -0.320667548681445    ***
>      109:      0.146437997841273  -0.129087352175718    0.146437997841273    ***
>      110:                    =19                 =19                  =19    ***
>      111:      0.146437997841273   0.875729664122058    0.553801569234696    ***
>      112:      -0.20519033126151                   .                    .    ***
>      113:      0.124958270430821   0.124958270430821   -0.612007323735384    ***
>      114:                      .                   .    0.742342248844355    ***
>      115:                    =20                 =20                  =20    ***
>      116:      0.446886365318184  -0.838515853544064  -0.0900546205400286    ***
>      117:     -0.745758712624212                   .                    .    ***
>      118:     0.0843162859334758                   .                    .    ***
>      119:     -0.129087352175718  -0.745758712624212    -0.20519033126151    ***
>      120:                      .   0.598889458763233                    .    ***
>      121:                    =21                 =21                  =21    ***
>      122:      0.610385097601063    1.08431628593348    0.669278786654632    ***
>      123:                     ==                  ==                   ==    ***
>      124:                     *-                  *-                   *-    ***
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
>                    (***one spine/path not displayed due to screen size***)
>    
>       Data fields: 
>                ComplexPitch    :: character (**kern tokens)
>                Pitch           :: character (**kern tokens)
>                Rhythm          :: character (**recip tokens)
>                Token           :: character
>               *i(Pitch;Rhythm) :: numeric
```

What do these numbers mean? If a pointwise mutual information value is
positive, it means this data point is *more* likely to occur than we’d
expect if the variables (rhythm and pitch) were independent. If is
negative, this indicates that it is *less* likely to occur. (Since these
numbers are bits, a `-1` would indicate that that data point is half as
likely to occur.)

#### Using a different model (cross entropy)

[Above](#using-a-different-model), we saw that we could assign
likelihoods to one set of data based on a model extracted from different
data. For example, assigning likelihoods to pitches in one chorale based
on the distribution of pitches in the other nine chorales. We can do the
same thing when computing entropy. The result is called the *cross
entropy*, and it is an excellent measure of how well a model “fits” some
data. We can compute the cross entropy using the
[`xentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
function, which requires a `model` argument.

``` r
bach |>
  with(entropy(Pitch))
>    [1] 3.616216


bach |>
  with(xentropy(Pitch, model = allExcept1))
>    [1] 3.623015
```

Notice that the cross entropy is *always* going to be higher than the
“self entropy” of a dataset—because that “self entropy” is estimated
directly on itself, it “fits” the data distribution as well as it
possibly can. However, if the fit is good, the cross entropy will be
close to the “self entropy” of the data.

Here the cross entropy is very close to the self entropy—this suggests
that the distribution of pitches in the first Chorale is very similar to
the distribution in the other nine. To better understand this concept,
lets explore some examples where the cross entropy will fit better or
worse. Take a moment to check what keys our nine chorales are in:

``` r
bach |> group_by(File) |> with(unique(Key))
>      File1   File2   File3   File4   File5   File6   File7   File8   File9  File10 
>       "G:"    "A:"    "a:"    "E:"    "G:"    "F:"    "A:" "f:dor"    "G:"    "a:"
```

Lets try computing the (cross) entropy of Chorale 1 using either (1)
Chorale 5, which is also in G major or (2) Chorale 4, which is in E
major, as a model.

``` r
bach |> filter(File == 5) |> pdist(Pitch) -> GmajorP
bach |> filter(File == 4) |> pdist(Pitch) -> EmajorP

bach |> filter(File == 1) |> entropy(Pitch)
>    H(Pitch) 
>    2.751188
bach |> filter(File == 1) |> with(xentropy(Pitch, model = GmajorP))
>    [1] 2.859749
bach |> filter(File == 1) |> with(xentropy(Pitch, model = EmajorP))
>    [1] 3.652187
```

If we compute the (cross) entropy of pitches in the first G major
chorale using the distribution of pitches in the other G major Chorale
(#5), the fit is really good—only slightly higher than the first
Chorale’s “self” entropy. On the other hand, if we use the E major
Chorale as the model, the fit is quite a bit worse (3.65). How would we
interpret that? Well, if our goal is to generalize about how frequently
different pitches occur in common practice music which is composed *in G
major*, but we use E major Chorales to come up with those probabilities,
we’ll end up underestimating a lot. For example, our `EmajorP` model
says that the note D natural should only occur about 2% of the time, and
that the note G natural should occur…never. That is obviously a bad
model of *G major* music! This is why the cross entropy for predicting
the first (G major) chorale using the `EmajorP` data is high—it’s a bad
fit.

Obviously, `EmajorP` would be a better model of common practice music
that is composed in E major. But…that doesn’t mean we believe that
`EmajorP` is a great model of the distribution of pitches in common
practice E major, or that `GmajorP` is a great model of music composed
in G major—they are each only based on one short piece by Bach! If we
want better estimates of the *true* distribution of pitches in common
practice music (in whatever key), it would be better to base them on
larger and more representative samples of music. We could, for a
starting point, look at the distribution of pitches in *all* Bach’s G
major or E major chorales—this would give us much better estimates than
models based on *one* piece. Still, whether analyzing Bach’s chorales
really tells us useful information about all “common practice” music is
another question. Based on the observations of many expert music
theorists, it seems that Bach’s music is a *pretty good* approximation
of general common practice, but that doesn’t mean it is a perfect model.
So whenever we calculate statistics (entropy, likelihoods, etc.) from
data, we should always remember that we are not computing “the” entropy
or “the” information content. We are (at best) *estimating* the real
values that interest us based on the sample of data that we happen to
have.

##### Log likelihood

The cross entropy metric is one of the main “loss functions” used in
machine learning. Cross entropy is also closely related to the *log
likelihood* statistic, which is often used in statistical tests. The
log-likelihood statistic is the *sum* of all the estimated
pointwise-log-likelihood values (the negative of the sum of the
information content), rather than the average. This means that the
log-likelihood statistic is the same as the cross entropy multiplied by
`N` (the number of data points).

##### kld

The difference between the cross entropy and the “self” entropy is
called the [Kullback-Leibler
Divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence),
more commonly *kld*. If the kld is small, it means the model matches the
data pretty closely. You can get the kld directly using the \[kld()\]
function:

``` r
bach |> filter(File == 1) |> kld(Pitch, model = GmajorP)
>    Dkl(p, q) 
>    0.1085608
bach |> filter(File == 1) |> kld(Pitch, model = EmajorP)
>    Dkl(p, q) 
>    0.9009984
```

This is the same information we saw before, really, but perhaps easier
to interpret. Close to zero is good fit; Larger numbers are worse fit.

### Pointwise Entropy

Many researchers reading this article so far might be a little confused
regarding the definition of “conditional entropy” we’ve used so far.
This is because there is another very common way that “conditional
entropy” has been used extensively in music research which we haven’t
discussed so far. This approach, because it has become so common, is
often called “conditional entropy”—or just “entropy”—in papers, even
though this is a slight abuse of the terminology.

Properly, as described in the previous sections, the conditional entropy
of a set of variables is a *single* value—the *average* information
content of those variables *given* some other conditioning variables. In
contrast, we can look at each data point, and ask what the entropy
associated with conditioning variables at that point is. To clarify this
distinction, let’s back up. Consider this small set of data:

``` r
chords <- c('Cmaj', 'Cmaj', 'Cmaj', 'Gmaj', 'Cmaj', 'Cmaj', 'Cmaj','Gmaj')
notes  <- c('c',    'c',    'c',    'd',    'c',   'c',     'c', 'b')

data.frame(Harmony = chords, Melody = notes)
>      Harmony Melody
>    1    Cmaj      c
>    2    Cmaj      c
>    3    Cmaj      c
>    4    Gmaj      d
>    5    Cmaj      c
>    6    Cmaj      c
>    7    Cmaj      c
>    8    Gmaj      b
```

Lets look at the entropy of the melody, conditioned on the harmony. We
can start by looking at the conditional probability distribution (though
we can probably do this in our head here!):

``` r
pdist(Harmony = chords, Melody = notes, condition = 'Harmony')
>    humdrumR probability distribution P(Melody|Harmony) 
>    Harmony  Melody           
>                  b     c    d
>       Cmaj       .   1.0    .
>       Gmaj      .5     .   .5
>                  b     c    d
>    Harmony  Melody           
>    humdrumR probability distribution P(Melody|Harmony)
```

So, when the chord is `Cmaj`, there is 100% chance that the melody is
`c`; when the chord is `Gmaj`, the melody splits 50/50 between `d` and
`b`. Those are the likelihood values, and we can assign them to each
data point using
[`like()`](https://rdrr.io/pkg/data.table/man/like.html)—and let’s do
the information content too, because it’s just the negative log of the
likelihood anyway:

``` r
likelihood  <- like(Harmony = chords, Melody = notes, condition = 'Harmony')
information <- info(Harmony = chords, Melody = notes, condition = 'Harmony')

data.frame(Harmony = chords, Melody = notes, Likelihood = likelihood, Information = information)
>      Harmony Melody Likelihood Information
>    1    Cmaj      c        1.0           0
>    2    Cmaj      c        1.0           0
>    3    Cmaj      c        1.0           0
>    4    Gmaj      d        0.5           1
>    5    Cmaj      c        1.0           0
>    6    Cmaj      c        1.0           0
>    7    Cmaj      c        1.0           0
>    8    Gmaj      b        0.5           1
```

When the likelihoods are converted to information content they end up
being `0` bits ($p = 1$) or `1` bit ($p = 0.5$). So, what’s the
conditional entropy? It’s the average of those eight point-wise
`Information` values: 0.25.

``` r
entropy(Harmony = chords, Melody = notes, condition = 'Harmony')
>    H(Melody|Harmony) 
>                 0.25
```

Notice that the conditional entropy is one quarter of a bit, not half a
bit. This is because the `Cmaj` condition occurs more often than the
`Gmaj` condition; i.e., there are six `Cmaj` data points (`0` bits) and
only two `Gmaj` data points (`1` bit), which averages out to 0.25. This
is the proper interpretation of “conditional entropy,” as we already
learned about.

But what if we simply want to know what the average information content
is for *each* condition—in this case, each chord—? Conditional entropy
doesn’t tell me that, because it averages across all the chords.
Luckily, we have the
[`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
function:

``` r
entropy_by(Harmony = chords, Melody = notes, condition = 'Harmony')
>    humdrumR entropy distribution H(Melody|Harmony) 
>    Harmony  H(Melody)
>       Cmaj          0
>       Gmaj          1
>    Harmony  H(Melody)
>    humdrumR entropy distribution H(Melody|Harmony)
```

Ah, now we get the entropy of each condition! Basically,
[`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
is short for “entropy by condition.” Now, next we simply want to assign
the values from this table “pointwise” to the original data points,
which we can do with the
[`pentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
function:

``` r
pH <- pentropy(Harmony = chords, Melody = notes, condition = 'Harmony')

data.frame(Harmony = chords, Melody = notes, PEntropy = pH)
>      Harmony Melody PEntropy
>    1    Cmaj      c        0
>    2    Cmaj      c        0
>    3    Cmaj      c        0
>    4    Gmaj      d        1
>    5    Cmaj      c        0
>    6    Cmaj      c        0
>    7    Cmaj      c        0
>    8    Gmaj      b        1
```

This gives us what we wanted: the “current” conditional entropy at each
data point.

#### Real data

Let’s try out the
[`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
and
[`pentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
functions with some real data. Starting with entropy by condition:

``` r
bach |> entropy_by(Pitch, Rhythm, condition = "Rhythm")
>    humdrumR entropy distribution H(Pitch|Rhythm) 
>    Rhythm  H(Pitch)
>        16      3.06
>         8      3.61
>        8.      0.00
>         4      3.63
>        4.      3.24
>         2      3.38
>        2.      3.05
>         1      0.00
>    Rhythm  H(Pitch)
>    humdrumR entropy distribution H(Pitch|Rhythm)
```

Generally, the empirical entropy of pitch doesn’t seem to vary much
depending on the rhythmic duration, which I guess isn’t that surprising
for chorale data. But what about those zeroes? Aren’t they something.
Those zeroes mean that dotted-eighth-notes and whole notes are always
the same pitch, so there is zero entropy—but this is just because they
only happen once each in our data!

``` r
bach |> count(Rhythm)
>    humdrumR count distribution, ~rounded 
>    Rhythm       n
>        16      17
>         8     706
>        8.       1
>         4  ~1.43k
>        4.      28
>         2     222
>        2.      21
>         1       1
>    Rhythm       n
>    humdrumR count distribution, ~rounded
```

This is another good illustration that the *empirical* entropy in a
dataset is not necessarily a good estimate of the true entropy in music.
Let’s look at something that is (possibly) more interesting:

``` r
bach |> entropy_by(Pitch, Instrument, condition = "Instrument")
>    humdrumR entropy distribution H(Pitch|Instrument) 
>    Instrument  H(Pitch)
>        I"Alto      3.41
>        I"Bass      3.67
>     I"Soprano      3.40
>       I"Tenor      3.53
>    Instrument  H(Pitch)
>    humdrumR entropy distribution H(Pitch|Instrument)
```

There’s not *much* variation between voices either, but the variation
here at least makes sense: for example, the bass voice has the most
entropy, perhaps because it leaps more. The soprano has the least
entropy (though basically tied with the alto), perhaps reflecting that
the soprano lines are drawn from fixed prexisting chant melodies. Of
course, this is a very minimal definition of pitch, so we should take
this all with a grain of salt.

------------------------------------------------------------------------

Let’s check that the
[`pentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
function works as we hope:

``` r
bach |> pentropy(Pitch, Instrument, condition = 'Instrument')
>    ##################### vvv chor001.krn vvv ######################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:              **kern            **kern           **kern    ***
>                9:              *ICvox            *ICvox           *ICvox    ***
>               10:              *Ibass           *Itenor           *Ialto    ***
>               11:             *I"Bass          *I"Tenor          *I"Alto    ***
>               12:           *>[A,A,B]         *>[A,A,B]        *>[A,A,B]    ***
>               13:        *>norep[A,B]      *>norep[A,B]     *>norep[A,B]    ***
>               14:                 *>A               *>A              *>A    ***
>               15:             *clefF4          *clefGv2          *clefG2    ***
>               16:              *k[f#]            *k[f#]           *k[f#]    ***
>               17:                 *G:               *G:              *G:    ***
>               18:               *M3/4             *M3/4            *M3/4    ***
>               19:              *MM100            *MM100           *MM100    ***
>               20:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               21:                  =1                =1               =1    ***
>               22:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               23:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               24:                   .  3.53274838036562                .    ***
>               25:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               26:                  =2                =2               =2    ***
>               27:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               28:    3.66716836587307  3.53274838036562                .    ***
>               29:                   .                 .                .    ***
>               30:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>    31-133::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ##################### ^^^ chor001.krn ^^^ ######################
>    
>           (eight more pieces...)
>    
>    ##################### vvv chor010.krn vvv ######################
>      1-70::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               72:                   .  3.53274838036562                .    ***
>               73:    3.66716836587307                 .  3.4124404841983    ***
>               74:                   .  3.53274838036562                .    ***
>               75:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               76:                 =11               =11              =11    ***
>               77:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               78:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               79:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               80:                   .                 .  3.4124404841983    ***
>               81:                 =12               =12              =12    ***
>               82:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               83:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               84:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               85:    3.66716836587307  3.53274838036562                .    ***
>               86:                 =13               =13              =13    ***
>               87:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               88:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               89:    3.66716836587307  3.53274838036562  3.4124404841983    ***
>               90:                  ==                ==               ==    ***
>               91:                  *-                *-               *-    ***
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann S***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitko***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. ***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers,***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V***
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ##################### ^^^ chor010.krn ^^^ ######################
>              (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                ComplexPitch        :: character (**kern tokens)
>               *H(Pitch|Instrument) :: numeric
>                Pitch               :: character (**kern tokens)
>                Rhythm              :: character (**recip tokens)
>                Token               :: character
```

#### Conditioning on context

Lots of research in music cognition has pointed the importance of
predicting the next musical event (note or chord, usually) based on the
previous event(s). In humdrum$_{\mathbb{R}}$, we can take advantage of
our
[lag](https://humdrumR.ccml.gtcmt.gatech.edu/articles/Context.html#n-grams%20Contextualizing%20humdrum%20data%20with%20N-grams)
features to compute conditional entropy based on past context. So, for
example, to compute the conditional entropy of each simple pitch given
the previous pitch, we could do this:

``` r
# plain pitch entropy
bach |> entropy(Pitch) 
>    H(Pitch) 
>    3.616216

# use lag = 1 to get the previous data point
bach |> entropy(Pitch, PrevPitch = Pitch[lag = 1], condition = 'PrevPitch')
>    H(Pitch|PrevPitch) 
>              2.705926
```

So overall, knowing the previous pitch reduces entropy. Of course, we
now know how to see what the entropy is for each condition:

``` r
bach |> entropy_by(Pitch, PrevPitch = Pitch[lag = 1], condition = 'PrevPitch')
>    humdrumR entropy distribution H(Pitch|PrevPitch) 
>    PrevPitch  H(Pitch)
>            c     2.784
>           c#     2.400
>           d-     2.213
>            d     2.719
>           d#     1.227
>           e-     2.493
>            e     3.033
>           e#     1.299
>            f     2.697
>           f#     2.622
>           g-     0.000
>            g     2.885
>           g#     2.130
>           a-     2.488
>            a     2.909
>           a#     0.866
>           b-     2.378
>            b     2.723
>           NA     3.146
>    PrevPitch  H(Pitch)
>    humdrumR entropy distribution H(Pitch|PrevPitch)
```

We get one weird zero again (because g-flat only happens once in these
ten chorales) but everything else makes sense: for example, sharp notes
like g# and d# are relatively low entropy because they probably tend to
act as leading tones. This would probably be more informative if we also
conditioned on key:

``` r
bach |> entropy_by(Pitch, PrevPitch = Pitch[lag = 1], Key, condition = c('PrevPitch', 'Key'))
>    humdrumR entropy distribution H(Pitch|PrevPitch,Key) 
>    PrevPitch    Key                                   
>                  a:     A:     E:     F:  f:dor     G:
>           NA  1.906  1.500  1.500  1.500  1.500  1.500
>            c  1.938  0.000  0.000  2.387  2.458  1.896
>           c#  0.000  2.184  2.547  0.000  0.000  1.157
>           d-  0.000  0.000  0.000  0.000  2.213  0.000
>            d  2.031  2.053  0.811  2.040  1.585  2.448
>           d#  0.722  1.281  1.299  0.000  0.000  0.000
>           e-  0.000  0.000  0.000  0.000  2.432  0.000
>            e  2.778  2.773  2.616  1.988  1.252  2.600
>           e#  0.000  0.918  1.000  0.000  0.000  0.000
>            f  1.502  0.000  0.000  2.243  2.525  0.000
>           f#  1.566  2.412  2.435  0.000  0.000  1.779
>           g-  0.000  0.000  0.000  0.000  0.000  0.000
>            g  2.420  1.208  0.000  2.182  2.557  2.509
>           g#  0.988  2.080  2.366  0.000  0.000  0.866
>           a-  0.000  0.000  0.000  0.000  2.488  0.000
>            a  2.632  2.620  1.784  2.286  1.500  2.523
>           a#  0.000  0.000  1.252  0.000  0.000  0.000
>           b-  0.000  0.000  0.000  1.000  2.257  0.000
>            b  1.880  2.304  2.717  1.000  0.000  2.381
>                  a:     A:     E:     F:  f:dor     G:
>    PrevPitch    Key                                   
>    humdrumR entropy distribution H(Pitch|PrevPitch,Key)
```

Nice! Notice how the note f-natural is relatively low entropy in the
keys of G major and A minor, but high entropy in the keys of F major and
F dorian—which is exactly what we’d expect.

Finally, we can apply these same models point-wise, so we can track how
these conditional/contextual entropies change through each piece: This
is exactly the way “entropy” is often used in music cognition research.

``` r
bach |> pentropy(Pitch, PrevPitch = Pitch[lag = 1], Key, condition = c('PrevPitch', 'Key'))
>    ####################### vvv chor001.krn vvv #######################
>                1:  !!!COM: Bach, Johann Sebastian
>                2:  !!!CDT: 1685/02/21/-1750/07/28/
>                3:  !!!OTL@@DE: Aus meines Herzens Grunde
>                4:  !!!OTL@EN:      From the Depths of My Heart
>                5:  !!!SCT: BWV 269
>                6:  !!!PC#: 1
>                7:  !!!AGN: chorale
>                8:               **kern             **kern            **kern    ***
>                9:               *ICvox             *ICvox            *ICvox    ***
>               10:               *Ibass            *Itenor            *Ialto    ***
>               11:              *I"Bass           *I"Tenor           *I"Alto    ***
>               12:            *>[A,A,B]          *>[A,A,B]         *>[A,A,B]    ***
>               13:         *>norep[A,B]       *>norep[A,B]      *>norep[A,B]    ***
>               14:                  *>A                *>A               *>A    ***
>               15:              *clefF4           *clefGv2           *clefG2    ***
>               16:               *k[f#]             *k[f#]            *k[f#]    ***
>               17:                  *G:                *G:               *G:    ***
>               18:                *M3/4              *M3/4             *M3/4    ***
>               19:               *MM100             *MM100            *MM100    ***
>               20:                    .                  .                 .    ***
>               21:                   =1                 =1                =1    ***
>               22:     2.50889428153327   2.38100054468805  2.44801860756626    ***
>               23:     2.50889428153327   2.38100054468805  2.44801860756626    ***
>               24:                    .   1.89609502987381                 .    ***
>               25:     2.60048027044198   2.38100054468805  2.60048027044198    ***
>               26:                   =2                 =2                =2    ***
>               27:     1.77887432766071   2.52308214817404  2.44801860756626    ***
>               28:     2.50889428153327   2.50889428153327                 .    ***
>               29:                    .                  .                 .    ***
>               30:     2.44801860756626   1.77887432766071  2.44801860756626    ***
>    31-133:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>    ####################### ^^^ chor001.krn ^^^ #######################
>    
>           (eight more pieces...)
>    
>    ####################### vvv chor010.krn vvv #######################
>      1-70:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
>               71:      1.9383112445326    1.9383112445326  2.77761691119536    ***
>               72:                    .    1.5655962303576                 .    ***
>               73:     2.03059983227804                  .  2.03059983227804    ***
>               74:                    .   2.41987070272086                 .    ***
>               75:     2.03059983227804    1.5655962303576   1.9383112445326    ***
>               76:                  =11                =11               =11    ***
>               77:     2.41987070272086   2.41987070272086  1.87966439800145    ***
>               78:      1.9383112445326   2.41987070272086  2.77761691119536    ***
>               79:     2.63163176581085   2.63163176581085  2.77761691119536    ***
>               80:                    .                  .  2.77761691119536    ***
>               81:                  =12                =12               =12    ***
>               82:     2.77761691119536  0.988180837004676  2.03059983227804    ***
>               83:     1.50161447181018   2.63163176581085   1.9383112445326    ***
>               84:      1.9383112445326   2.41987070272086   1.9383112445326    ***
>               85:                    0   2.41987070272086                 .    ***
>               86:                  =13                =13               =13    ***
>               87:     2.63163176581085   2.63163176581085  2.03059983227804    ***
>               88:    0.988180837004676   1.87966439800145  2.03059983227804    ***
>               89:     2.63163176581085   2.63163176581085   1.9383112445326    ***
>               90:                   ==                 ==                ==    ***
>               91:                   *-                 *-                *-    ***
>               92:  !!!hum2abc: -Q ''
>               93:  !!!title: @{PC#}. @{OTL@@DE}
>               94:  !!!YOR1: 371 vierstimmige Choralges&auml;nge von Johann Seba***
>               95:  !!!YOR2: 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf ***
>               96:  !!!YOR2: c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bac***
>               97:  !!!YOR4: Chorales (New York: Associated Music Publishers, In***
>               98:  !!!SMS: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
>               99:  !!!EED:  Craig Stuart Sapp
>              100:  !!!EEV:  2009/05/22
>    ####################### ^^^ chor010.krn ^^^ #######################
>                 (***one spine/path not displayed due to screen size***)
>    
>       humdrumR corpus of ten pieces.
>    
>       Data fields: 
>                ComplexPitch           :: character (**kern tokens)
>               *H(Pitch|PrevPitch,Key) :: numeric
>                Pitch                  :: character (**kern tokens)
>                Rhythm                 :: character (**recip tokens)
>                Token                  :: character
```

This can be extended to higher-order N-grams without any difficulty.
