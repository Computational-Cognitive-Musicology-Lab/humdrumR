# Calculate point-wise or contextual entropy

These functions model [conditional
entropy](https://en.wikipedia.org/wiki/Conditional_entropy) as a dynamic
process. The `entropy_by()` function returns the entropy of outcome
variables, grouped by conditioning variables, with a separate (isolated)
entropy values for each condition. The `pentropy()` function computes
and returns these same values in a vectorized form, with the entropy of
each condition returned at each point in the input vectors. This
"pointwise-entropy" is often used as a model of dynamic "uncertainty" in
music.

## Usage

``` r
entropy_by(..., condition, independent = TRUE, base = 2)

# S3 method for class 'probability'
entropy_by(pdist, condition, independent = TRUE, base = 2)

# Default S3 method
entropy_by(..., condition, independent = TRUE, base = 2)

pentropy(
  ...,
  model,
  base = 2,
  condition = NULL,
  na.rm = FALSE,
  .drop = FALSE,
  binArgs = list()
)
```

## Arguments

- ...:

  ***Distribution (or atomic vectors) to compute entropy of.***

  Must either be a distribution object (created by
  [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md),
  [`density()`](https://rdrr.io/r/stats/density.html),
  [`count()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md),
  or
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)),
  or one or more atomic vectors of equal length.

- condition:

  ***Compute conditional entropy, conditioned on this variable.***

  Must be a non-empty `character` string, which matches the name of one
  or of the named variables in the distribution, or a positive whole
  number which indexes the variables.

- base:

  ***The logarithmic base.***

  Defaults to `2`, so information is measured in units "bits."

  Must be a single, non-zero positive number.

  Use `base = exp(1)` for natural-log "nats," or `base = 10` for
  Hartley/"dits".

## Details

The formal information theoretic notion of[conditional
entropy](https://en.wikipedia.org/wiki/Conditional_entropy) is a
description of how much entropy is observed in explained variables when
conditioned on other variables, but *averaged over all possible
conditions and outcomes* in the proportions they appear in the
distribution. This "proper" conditional entropy is always a single
value, and can be computed using humdrumR's [entropy(..., condition =
'var')](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
command.

Consider the following sequence: A, B, A, B, A, B, A, B, C, A, B. If we
compute the conditional probability of each letter, conditioned on the
previous letter, we get the conditional entropy

    seq <- c('A', 'B', 'A', 'B', 'A', 'B', 'A', 'B', 'C', 'A', 'B', 'C', 'B')

    entropy(From = lag(seq,-1), To = seq, condition = 'From', na.rm = TRUE)

The result is `0.5954373` bits of entropy, which is pretty low. However,
if we analyze the conditions in the this sequence closely we can observe
a few things:

- When the previous value is A, the next value is *always* B—so the
  entropy is `0`.

- When the previous value is B, the next value is A three times, and C
  once—so the entropy of *that* distribution would be
  `entropy(c('A', 'A','A', 'C')) = .8112781`.

- Finally, when the previous value is C, the next values is always A—so
  again, the entropy is `0`.

However, these three outcomes don't occur equally often. If we walked
through this sequence, we'd see five As (`0` each), four Bs (`.81`
each), and one C (`0` zero). If we average six `0` and four `.8112781`
we get...`0.3245112`. That's the conditional entropy which we computed
above; The *average* conditional entropy over the sequence. In music
research practice, sometimes we don't just want to know the overall
conditional entropy. Rather, we want to keep track of the dynamic
changes in the conditional entropy, as we did above. This is the purpose
of the `entropy_by()` and `pentropy()` functions.

The `entropy_by()` and `pentropy()` functions can be use just like the
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
and
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
functions respectively, except 1) there must be at least two dimensions
and 2) a `condition` *must* be supplied, as a `character` string
matching dimension names or whole number indices of dimensions. Unlike
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
`entropy_by()` will return a vector of entropy values, corresponding to
each combination of levels in the `condition` arguments. Unlike
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
the output of `pentropy()` represents the entropy at each index,
conditioned on all the `condition` variables, not the information
content of each observed data point.

## See also

The HumdrumR [information
theory](https://humdrumR.ccml.gtcmt.gatech.edu/reference/information.md)
overview.

Other Information theory functions:
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md),
[`mutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
