# Calculate Entropy or Information Content of variables

Information content and entropy are fundamental concepts in [information
theory](https://en.wikipedia.org/wiki/Information_theory), quantifying
the amount of information in samples from a random variable; they are
often characterized as measures of how "expected" (low information) or
"surprising" (high information) data is. Both concepts are closely
related the probability density/mass of events: improbable events have
higher information content. The probability of *each* (point-wise)
observation maps to the [information
content](https://en.wikipedia.org/wiki/Information_content); The average
information content of a variable is the
[entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)).
Information content/entropy can be calculated for discrete probabilities
or continuous probabilities, and humdrumR defines methods for
calculating both.

## Usage

``` r
entropy(..., model, base = 2)

H(..., model, base = 2)

# S3 method for class 'probability'
entropy(pdist, model, condition = NULL, base = 2)

# S3 method for class 'numeric'
entropy(x, model, base = 2, na.rm = TRUE, ...)

# S3 method for class 'density'
entropy(x, model, base = 2, na.rm = TRUE)

# Default S3 method
entropy(..., model, base = 2)

xentropy(..., model, base = 2)

kld(..., model, base = 2)

# S3 method for class 'probability'
kld(pdist, model, condition = NULL, base = 2)

# Default S3 method
kld(..., model, base = 2)

info(..., model, base = 2)
```

## Arguments

- ...:

  ***Distribution (or atomic vectors) to compute entropy/information
  of.***

  Must either be a distribution object (created by
  [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md),
  [`density()`](https://rdrr.io/r/stats/density.html),
  [`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
  or
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)),
  or one or more atomic vectors of equal length.

  If atomic vectors are provided, and `model` is missing, the atomic
  vectors are passed to
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
  in order to calculate the `model`.

- model:

  ***The expected probability model.***

  Must either be omitted (not allowed in calls to `xentropy()`) or must
  be a probability distribution created by
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md).

  In calls to `entropy()` or `info()`, if `model` is missing, `...`
  arguments are used to generate the `model`.

- base:

  ***The logarithmic base.***

  Defaults to `2`, so information is measured in units "bits."

  Must be a single, non-zero positive number.

  Use `base = exp(1)` for natural-log "nats," or `base = 10` for
  Hartley/"dits".

- condition:

  ***Compute conditional entropy/information, conditioned on this
  variable.***

  Defaults to `NULL` (no condition), so the joint entropy is calculated.

  Must be a non-empty `character` string, which matches the name of one
  or of the named variables in the distribution, or a positive whole
  number which indexes the variables.

  This argument is simply passed to
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md).

## Details

To calculate information content or entropy, we must assume (or more
often, estimate) a probability distribution. HumdrumR's
[`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
and
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
methods can be used calculate empirical distributions from atomic data.
For numeric data we can also use R's standard
[`stats::density()`](https://rdrr.io/r/stats/density.html) function to
estimate the continuous probability density.

The `entropy()` function takes an object representing a probability
distribution—ideally a humdrumR
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
object, base-R
[table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md),
or a [`density()`](https://rdrr.io/r/stats/density.html) object (for
continuous variables)—and returns the entropy, defaulting to base-2
entropy ("bits"). However, if you are lazy, you can pass `entropy()`
atomic data vectors directly and it will automatically pass them to the
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
or [`stats::density()`](https://rdrr.io/r/stats/density.html) functions
for you; for example, if you want to calculate the joint entropy of
variables `x` and `y` (which must be the same length), you can either
call `entropy(pdist(x, y))` or just `entropy(x, y)`. Other arguments can
be provided to
[`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
as well; notably, if you want to calculate the *conditional* entropy,
you can, for example, say `entropy(x, y, condition = 'y')`.

Using the `info()` function is similar to calling `entropy()` directly
on data vectors: anywhere where you can call `entropy(x, y)`, you can
call `info(x, y)` instead. The difference is that `info()` will return a
vector of numbers representing the information content of each input
observation. By definition, the entropy of the data distribution is the
average of all these point-wise information values: thus,
`mean(info(x, y)) == entropy(x, y)`.

## Cross entropy

In many cases, we simply use entropy/information content to describe a
set of data. In this case, the data we observe and the probability model
(distribution) are the same—i.e., the probability model is the
("empirical") distribution of the data itself. However, we can also use
a *different model*—in this case, a different probability
distribution—to describe data. This is called the [cross
entropy](https://en.wikipedia.org/wiki/Cross-entropy), and can be
interpreted as a measure of how well the model fits the data. The cross
entropy is lowest when the model exactly matches the data, when it will
be the same as the normal ("self") entropy. If the model doesn't exactly
match the data, the cross entropy will be higher then the self entropy.
If the data matches the model well, the cross entropy will be a little
bit higher than the self entropy; if the data matches the model poorly,
the cross entropy can be much higher. The difference between the cross
entropy and the self entropy is always positive (or zero), and is called
the [Kullback-Leibler
Divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence)
(KLD).

To calculate cross entropy, use the `xentropy()` command. (The
Kullback-Leibler Divergence can be calculated in the same way using the
`kld()` function.) The `xentropy()` command works just like the entropy
command, except you need to provide it a `model` argument, which must be
*another* probability distribution. Note that the data and the model
have to have the **exact** same variable names or `humdrumR` will throw
an error! Name your arguments to avoid this (this is illustrated in the
example below, where we name everything `X`).

To illustrate the propertise of cross entropy, lets create three sets of
data, two of which are similar, and one which is very different:

    set.seed(1)
    dorian <- c('A', 'B', 'C', 'D', 'E', 'F#', 'G')
    N <- 1000

    sample1 <- sample(dorian, N, replace = TRUE, prob = 7:1)
    sample2 <- sample(dorian, N, replace = TRUE, prob = 7:1)
    sample3 <- sample(dorian, N, replace = TRUE, prob = 1:7)


    ## first the self entropy
    entropy(X = sample1) # 2.607
    entropy(X = sample2) # 2.597
    entropy(X = sample3) # 2.592

    ## now the cross entropy

    xentropy(X = sample1, model = pdist(X = sample2)) # 2.619
    xentropy(X = sample2, model = pdist(X = sample2)) # 2.597 (same as self entropy above)
    xentropy(X = sample3, model = pdist(X = sample2)) # 3.538

`sample1` and `sample2` have very similar distributions, so when we use
`sample2` as a model for `sample1`, the cross entropy is only slightly
higher than the self entropy of `sample1`. However, when we use
`sample2` as the model for `sample3` (which is distributed very
differently) the entropy is quite a lot higher than the entropy of
sample 3.

The `info()` command can also be passed a `model` argument. As always,
`mean(info(x, model)) == xentropy(x, model)`. There is no standard name
for this "cross information content." However, cross
entropy/information-content are closely related to the more general
concept of *likelihood* (see the next section).

## Likelihood

The output of `info()` is identical to the log of the modeled
[likelihood](https://en.wikipedia.org/wiki/Likelihood_function) of each
data point, which can be computed using the
[`data.table::like()`](https://rdrr.io/pkg/data.table/man/like.html)
function. Literally, `info(x, base) == log(like(x), base)`. The
[`data.table::like()`](https://rdrr.io/pkg/data.table/man/like.html)
function works just like `info()`, computing pointwise probabilities for
each data point based on the probability distribution in `model`.
However, we can use it to, for example, calculate the total *log
likelihood* of data using `sum(log(like(...)))`. This value divided by N
is the cross entropy (make sure to use the right log base!):
`-sum(log(like(...), base = 2)) == xentropy(...)`.

## See also

The HumdrumR [information
theory](https://humdrumR.ccml.gtcmt.gatech.edu/reference/information.md)
overview.

Other Information theory functions:
[`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md),
[`mutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
