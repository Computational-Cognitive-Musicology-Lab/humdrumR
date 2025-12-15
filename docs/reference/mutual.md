# Calculate mutual information between variables

The [mutual
information](https://en.wikipedia.org/wiki/Mutual_information) is a
measure of how statistically dependent two variables are: in information
theory terms, how much information about one variable is learned from
observing the other variable(s). The overall mutual information can be
calculated using `mutual()` (analogous to
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)),
while the point-wise mutual information can be calculated using
`pmutual()` (analogous to
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)).

## Usage

``` r
mutual(..., base = 2)

# Default S3 method
mutual(..., base = 2)

# S3 method for class 'probability'
mutual(x, base = 2)

# Default S3 method
mutual(..., base = 2)

pmutual(
  ...,
  model,
  base = 2,
  condition = NULL,
  na.rm = FALSE,
  .drop = FALSE,
  binArgs = list()
)
```

## Details

Mutual information is a property of probability distributions over two
or more variables. HumdrumR's
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
methods can be used to calculate empirical distributions over atomic
data, and we can then calculate their mutual information.

The `mutual()` and `pmutual()` functions are called just like
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
and
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md).
`mutual()` can be provided a probability
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
(made with
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)),
or can be directly provided two or more atomic vectors, which are simply
passed to
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md);
in other words, `mutual(x, y) == mutual(pdist(x, y))`. `pmutual()`, like
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
can only be passed raw atomic vectors, like `pmutual(x, y)`. Note that,
unlike the entropy functions, the mutual information functions will
throw an error if you only provide them a single variable.

## Further explanation

If two (or more) variables are statistically independent, their joint
entropy will be the sum of their independent entropies.

\$\$ H(X, Y) = H(X) + H(Y) \$\$

However, *if they are not independent*, their joint entropy will be less
than the summed independent entropies. The mutual information is the
difference between the summed independent entropies and their actual
observed joint entropy.

\$\$ I(X,Y) = (H(X) + H(Y)) - H(X,Y) \$\$

For the point-wise mutual information, we get a single value for each
data observation. The value represents the difference between the
observed joint likelihood of each observation and the value we'd expect
if the variable were independent. For example, consider the binary
variables "person likes heavy metal" (\\P(metal)\\) and "person plays
electric guitar" (\\P(guitar)\\). Imagine that \\P(metal) = 0.05\\ and
\\P(guitar) = 0.1\\. If these two variables are independent, we'd expect
that the joint probability of liking heavy metal *and* playing guitar
would be \\\bar{P(metal, guitar)} = .05 \* 0.1 = 0.005\\ (one out of 200
people). However, on measuring some data, we might find that actually
one in fifty people like metal *and* play guitar (\\P(metal, guitar) =
0.02\\). This means that the combination of liking metal and playing
guitar is \\\frac{0.02}{0.005} = 4\\ times more likely than we'd expect
if they were independent. This would translate to a point-wise mutual
information of (using default base-2 "bits") \\\\log_2(4) = +2\\. The
overall mutual information is the average over all the point-wise values
(including other combinations, like heavy metal fans who don't play
guitar).

## See also

The HumdrumR [information
theory](https://humdrumR.ccml.gtcmt.gatech.edu/reference/information.md)
overview.

Other Information theory functions:
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
[`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)

## Examples

``` r
guitar <- c(T, T, T, T, T, T, T, T, F, F, F, F, F, F, F, F)
metal <- c(T, T, T, T,T,T,F,F,T,T,F,F,F,F,F,F)

mutual(pdist(guitar, metal))
#> I(guitar;metal) 
#>       0.1887219 
mutual(guitar, metal)
#> I(guitar;metal) 
#>       0.1887219 

pmutual(guitar, metal)
#> Error: Failed to parse glue component
#> Caused by error in `parse()`:
#> ! <text>:1:11: unexpected symbol
#> 1:    NULL   base
#>               ^
```
