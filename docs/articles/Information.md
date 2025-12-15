# Counts, Probability, and Information

Welcome to “Counts, Probability, and Information”! This article explains
some of the features R and humdrum$`_{\mathbb{R}}`$ have for extracting
count distributions, probability distributions, and information theory
metrics from data.

## Counting things

Nearly all of our articles, from the very
[start](https://humdrumR.ccml.gtcmt.gatech.edu/articles/GettingStarted.html#counting-things "Getting started with humdrumR > Counting things"),
have made use of the
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function. This is a humdrum$`_{\mathbb{R}}`$ workhorse function, which
is used to count the number of unique values in a vector, or unique
combinations of values across combinations of vectors.

Let’s take a moment to explain all of what
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
can do, starting from the basics. The
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
function accepts one or more atomic vectors, all of which must be the
same length. It counts all the unique combination of values across the
vectors. This can be illustrates with some simple examples:

``` r
v1 <- c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c')
v2 <- c('A', 'A', 'B', 'B', 'B', 'C', 'C', 'C', 'C')


count(v1)
>    humdrumR count distribution 
>    v1  n
>     a  3
>     b  3
>     c  3
>    v1  n
>    humdrumR count distribution
count(v2)
>    humdrumR count distribution 
>    v2  n
>     A  2
>     B  3
>     C  4
>    v2  n
>    humdrumR count distribution
count(v1, v2)
>    humdrumR count distribution 
>    v1  v2      
>         A  B  C
>     a   1  1  1
>     b   1  1  1
>     c   .  1  2
>         A  B  C
>    v1  v2      
>    humdrumR count distribution
```

#### Counting Numbers

If you have a distribution of continuous numbers, you don’t want to
count every unique value (because there will be one of each). So in
humdrum$`_{\mathbb{R}}`$,
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
will (attempt) to treat numeric values in a smart way, by binning them
into ranges using the base R
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

Deatils of this process can be controlled using the `binArgs` argument,
which must be a list of arguments. In particular, you pass arguments to
the [`hist()`](https://rdrr.io/r/graphics/hist.html) alrgorithm, such as
`breaks` (to control the number of bins).

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
