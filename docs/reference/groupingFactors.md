# What are "grouping factors"?

What are "grouping factors"?

## Grouping and "split-apply-combine" explained

The concept of "grouping factors" is widely used in R, allowing us to
quickly *split* datasets ([vector](https://rdrr.io/r/base/vector.html)s
or [data.frame](https://rdrr.io/r/base/data.frame.html)s) into
subgroups, work with the subgroups independent (*apply* functions to
them), and then re*combine* them as needed. Various R functions specify
"grouping factors" in a confusing variety of subtly different ways,
usually as function arguments named things like `INDEX`, `INDICES`, `f`,
`by`, or `groupby`. In `humdrumR`, we adopt the tidyverse
[dplyr::dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
approach, using the
[`group_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
function (and/or the `.by` argument).

Any [atomic vector](https://rdrr.io/r/base/vector.html) with at least
two unique values, or "levels", can be used as a grouping
factor—generally, grouping vectors are coerced into
[factor](https://rdrr.io/r/base/factor.html)s. Each unique level in a
grouping vector/factor represents a single group. Any vector, or
[data.frame](https://rdrr.io/r/base/data.frame.html) that is *the same
length/height* as the grouping factor can then be broken into these
groups, taking all the indices where the grouping factor equals each
group in turn. Since we generally try to work with data.frames, which by
definition contain a bunch of vectors that are the same length, we can
use any vector/column in a data.frame to group any of the other vectors,
or the rows of the whole data.frame.

Most functions allow you to specifiy multiple grouping factors/vectors
(so long as they are all the same length). The groups are then defined
by every *unique combination* of elements in the vectors. So, for
example, if we use the vectors `c('A', 'A', 'A', 'B', 'B', 'B')` and
`c(1, 1, 2, 2, 3, 3)` as grouping factors, we'll get four groups with
levels `1A`, `2A`, `2B`, and `3B`.

Note that groups created by grouping factors are *not* neccessarily
contiguous. If we use a vector like `c(1, 1, 2, 2, 1, 1)` as grouping
factor, we get two groups: `1` and `2`. The `1` group would include the
1st, 2nd, 5th, and 6th indices, even though they are separated in the
grouping factor. If you *want* contiguous groups you must make them. The
`humdrumR` function
[`segments()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/segments.md)
can be used to generate strictly contiguous grouping factors. For
example, `segments(c(1, 1, 2, 2, 1, 1))` will return
`c(1, 1, 2, 2, 3, 3)`.

## See also

Other R lessons.:
[`evaluatingExpressions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md),
[`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md),
[`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md),
[`vectorization`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md)
