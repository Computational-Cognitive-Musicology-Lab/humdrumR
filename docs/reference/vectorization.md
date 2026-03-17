# What is "vectorization"?

What is "vectorization"?

## Vectorization explained

Many R operations/functions are "vectorized," meaning that they take in
vectors and output vectors that are the same length. This means that we,
as programmers, don't need to worry about each element of the vector; We
can treat a vector like a single object, and R will oblige us. For
example, we can do math like:

    2^(0:10) - 1
    #> [1]    0    1    3    7   15   31   63  127  255  511 1023

    (1:10) - (10:1)
    #> [1] -9 -7 -5 -3 -1  1  3  5  7  9

    sqrt(c(5, 10, 16))
    #> [1] 2.236068 3.162278 4.000000

Or work with strings like:

    paste(1:26, letters, sep = ': ')
    #> [1] "1: a"  "2: b"  "3: c"  "4: d"  "5: e"  "6: f"  "7: g"  "8: h"  "9: i"  "10: j" "11: k" "12: l"
    #> [13] "13: m" "14: n" "15: o" "16: p" "17: q" "18: r" "19: s" "20: t" "21: u" "22: v" "23: w" "24: x"
    #> [25] "25: y" "26: z"

    paste('Chord', 1:10)
    #> [1] "Chord 1"  "Chord 2"  "Chord 3"  "Chord 4"  "Chord 5"  "Chord 6"  "Chord 7"  "Chord 8"
    #> [9] "Chord 9"  "Chord 10"

    # Regular expressions:
    grepl('[aeiou]', letters)
    #>  [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    #>  [17] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

Or get logical values:

    2^(0:100) > 50
    #>  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [17]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [33]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [49]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [65]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [81]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    #>  [97]  TRUE  TRUE  TRUE  TRUE  TRUE

    1:10 %% 2 == 0
    #> [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE

    1:20 %in% 2^(0:4)
    #> [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    #> [17] FALSE FALSE FALSE FALSE

Of course, other R functions take in vectors and return totally new
vectors (or just scalars). Examples:

    length(seq(50, 90, by = .2))
    #> [1] 201

    length(letters) # letters is a built-in vector which is always there!
    #> [1] 26

    sum(c(1, 5, 9))
    #> [1] 15

    mean(c(1, 5, 9))
    #> [1] 5

    max(c(1, 5, 9))
    #> [1] 9

    range(c(1, 100, 2, -4))
    #> [1] -4 100

    which(c(TRUE, FALSE, TRUE, TRUE))
    #> [1] 1 3 4

Vectorization works very well when you are working with vectors that are
either 1) all the same length or 2) length 1 (scalar). If vectors are
different lengths, the shorter one will be "recycled" (repeated) to
match the longer one.

    c(0, 5) * 1:10
    #> [1]  0 10  0 20  0 30  0 40  0 50

## See also

Other R lessons.:
[`evaluatingExpressions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md),
[`groupingFactors`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md),
[`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md),
[`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md)
