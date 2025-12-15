# "Evaluating" "Expressions" in "Environments"?

"Evaluating" "Expressions" in "Environments"?

## Expressions

The term "expression" is just a fancy way of describing any bit of
(valid) code than can be parsed and evaluated (executed) by R. For
example, the following bits of code are all valid R "expressions":

- `2 + 2`

- `sqrt(2)`

- `x <- (1:10)^2`

- `log(x, base = 10) |> mean(na.rm = TRUE)`

- `sum((x - mean(x))^2)`

- {
        x <- 1:10
        b <- mean(x)
        z <- x * z
      }

Expressions are frequently built of other expressions: so `2 + 2` is an
expression, and `sqrt(2 + 2)` is an expression. The
[`{ }`](https://rdrr.io/r/base/Paren.html) operators are used to group
any valid expressions into one bigger expression. You can also use `;`
to write two expressions on the same line, like `x <- 2; log(x^2)`

## Evaluation

An expression like `sum((x - mean(x))^2)` is just a sequence of
characters until we do something with it. We call this "*evaluating*"
the expression. This exactly what the R "interpreter" does when you
"run" some R code.

In R, an evaluated expression always "returns" a "*result*"—a value,
like a number, `character` string, or some other data. Some expressions
might "return" `NULL` as their result, but it's still a result! In a
multi-line expression, like

    {sqrt(2); 2 + 2}

or

    {
      x <- 2
      x^2
    }

the result of the overall expression is simply the result of the last
expression—so the last two examples both return the result `4`.

## Environment

To evaluate an expression, R must look up any variable names in the
expression in the current "*environment*"; For example, the expression
`sum((x - mean(x))^2)` includes the variables `sum`, `mean`, and `x`.
The variables `sum` and `mean` are
[base](https://rdrr.io/r/base/base-package.html) R functions, so R will
find them no problem (unless you
[remove](https://rdrr.io/r/base/rm.html) them). However, `x` is not
generally going to be "defined" unless *you've* defined it. If you try
to evaluate `sum((x - mean(x))^2)`, you'll get an error if `x` is not
defined.

There are many different
"[environments](https://rdrr.io/r/base/environment.html)" in R, where R
will search for variables: When you run R, you can save variables in the
*global environment*; R-packages have their own environments; *every*
time you call a function, the function has its *own* environment inside
it—this is why a function can "see" it's own arguments, but variable you
save inside a function isn't "visible" outside the function.

One of the greatest features of R is that we can often tell R to
evaluate an expression using a specific
[data.frame](https://rdrr.io/r/base/data.frame.html) as the environment,
so we can use the column names of our data as variables. The humdrumR
[with(in)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
(and their tidyverse equivalents) use this functionality a lot!

### Incomplete expressions

One of the most annoying things that can happen in R is if you try
running something and it kind just hangs, getting stuck with nothing
happening no matter how many times you press enter. This is usually
because you have (accidentally) provided R an *incomplete* expression.
For example, `2 + ` is an incomplete expression—that `+` needs a number
after it! Failing to have properly paired parentheses will often result
in incomplete expressions: For example, `mean(sqrt(log(x))` is an
incomplete expression!

## See also

Other R lessons.:
[`groupingFactors`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md),
[`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md),
[`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md),
[`vectorization`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md)
