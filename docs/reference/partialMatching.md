# What is "partial matching"?

What is "partial matching"?

## Partial matching explained

`R` has a very useful functionality called "*partial matching*," where
we can match a **incomplete** character string or variable name with a
list of options. This is achieved using the base-`R` function
[`pmatch()`](https://rdrr.io/r/base/pmatch.html), but many `R` functions
make use of it, as do *many* `humdrumR` functions.

For example, let's say we have a `data.frame` (let's call it `df`) with
three columns: `"Number"`, `"Letter"`, and `"Date"`:

    df <- data.frame(Number = 1:2, Letter = c('A', 'B'), Date = c("January", "February"))

If I want to access the `Number` column, most programming languages
would require I write at very least `df$Number`. However, `R` will give
me the correct field even if I write `df$Numb`, `df$Num`, or even
`df$N`. This is partial matching! The matching happens left-to-right, so
as long as I get the beginning of variable right, it will work.

Of course, partial matching only works up to the point that the string
matches unambiguously. For example, if added a `Dare` column to `df`,
then `df$D` or `df$Da` would return `NULL` because they are ambiguous.
You'd need to write at least `Dar` or `Dat` to get the `Dare` and `Date`
columns respectively.

## See also

Other R lessons.:
[`evaluatingExpressions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md),
[`groupingFactors`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md),
[`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md),
[`vectorization`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md)
