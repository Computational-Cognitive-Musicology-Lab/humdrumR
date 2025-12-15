# What is "vectorization"?

What is "vectorization"?

## Vectorization explained

Many R operations/functions are "vectorized," meaning that they take in
vectors and output vectors that are the same length. This means that we,
as programmers, don't need to worry about each element of the vector; We
can treat a vector like a single object, and R will oblige us. For
example, we can do math like:

2^(0:10) - 1

(1:10) - (10:1)

sqrt(c(5, 10, 16))

Or work with strings like:

paste(1:26, letters, sep = ': ')

paste('Chord', 1:10)Regular expressions:

grepl('aeiou', letters)

Or get logical values:

2^(0:100) \> 50

1:10 %% 2 == 0

1:20 %in% 2^(0:4)

Of course, other R functions take in vectors and return totally new
vectors (or just scalars). Examples:

length(seq(50, 90, by = .2))

length(letters) \# letters is a built-in vector which is always there!

sum(c(1, 5, 9)) mean(c(1, 5, 9)) max(c(1, 5, 9))

range(c(1, 100, 2, -4)) which(c(TRUE, FALSE, TRUE, TRUE))

Vectorization works very well when you are working with vectors that are
either 1) all the same length or 2) length 1 (scalar). If vectors are
different lengths, the shorter one will be "recycled" (repeated) to
match the longer one.

c(0, 5) \* 1:10

## See also

Other R lessons.:
[`evaluatingExpressions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md),
[`groupingFactors`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md),
[`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md),
[`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md)
