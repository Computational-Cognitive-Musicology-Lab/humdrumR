# An R primer for humdrumR users

Humdrum$_{\mathbb{R}}$ is a package for the R programming language. You
don’t need to be an R master to use humdrum$_{\mathbb{R}}$, but there
are some basic concepts from R that you will need to learn, and
ultimately, if you want to get really advanced, you’ll need to develop
some R skills. This document is a basic primer for R, which will teach
you the basics you need to know in order to make the most out of
humdrum$_{\mathbb{R}}$.

## Basic Commands

R code is made up of “expressions” like `2 + 2`, `sqrt(2)`, or
`(x - mean(x))^2`. As you can see, you can create very intuitive
*arithmetic* expressions, like `5 / 2` or `3 * 3`. However, the most
common elements of R expressions are “calls” to *functions*. A
“function” in R is a pre-built bit of code that does something. Most
functions take one or more input *arguments*, and “return” some kind of
output. For example, the function
[`sqrt()`](https://rdrr.io/r/base/MathFun.html) takes a number as an
input argument, and “returns” the square root of that number.

``` r
sqrt(2)
>    [1] 1.414214

sqrt(9)
>    [1] 3
```

To “call” a function, we write the function’s name, followed by
parentheses (`()`). Any input arguments to the function must go inside
the parentheses, separated by commas if there are more than one. Here
are some examples of common functions being “called” with zero or more
input arguments:

``` r
abs(-3)
>    [1] 3

mean(1:5)
>    [1] 3

max(1, 5) # two arguments
>    [1] 5

c(1, 2, 3) # three arguments
>    [1] 1 2 3

Sys.time() # no arguments!
>    [1] "2026-07-30 21:39:28 EDT"
```

Different functions have different arguments they recognize, with
specific names. For example, the function
[`log()`](https://rdrr.io/r/base/Log.html) takes two arguments, called
`x` and `base`. Other functions can take any number of arguments, with
any name. You can learn about a function, including the arguments it
accepts, by typing `?functionName` at the command line; for example,
[`?sqrt`](https://rdrr.io/r/base/MathFun.html) or
[`?mean`](https://rdrr.io/r/base/mean.html). If you see an argument
called `...`, that tells you that the function can take any number of
arguments.

We can explicitly “name” the function arguments we want by putting
`argname = argument` into our calls: For example, you could say
`log(10, base = 2)`. Named arguments are very useful when we are
creating data, like vectors and `data.frame`s (see below).

#### Pipes

Complex expressions might involve a large number of function calls,
which can get tiresome to read (or write). For example, something like

    log(round(sqrt(mean(x^2)), base = 2)

calls *four* functions! An expression like that is a bit tricky to read,
and it can be really easy to make a mistake where you put the wrong
number of parentheses. As an alternative, R gives us the option of
calling functions in a “pipe.” The way this works is we use the “pipe”
command `|>`, which takes an input on the left and “pipes” it into a
function call on the right. For example, we can rewrite the previous
command as:

    x^2 |> mean() |> sqrt() |> round() |> log(base = 2)

Much better! To make things even cleaner, R will understand if you
spread your expressions across multiple lines, by putting a new line
after each `|>`, or function argument:

    x^2 |>
      mean() |>
      sqrt() |>
      round() |>
      log(base = 2)
      
    max(sqrt(2),
        log(2),
        exp(2),
        pi / 2)

### Variables

When coding in R, you’ll often want to “save” data or other objects so
you can reuse them. We do this by “assigning” something (often the
result of a function) to a “*variable*”. This is done using the
assignment operators, either `<-` or `->`. A variable name can be any
combination of upper and lowercase letters. Let’s calculate the
square-root of two and save it to a variable:

``` r
tworoot <- sqrt(2)
```

We can then reuse that value as many times as we want:

``` r
tworoot^2
>    [1] 2

tworoot * 2
>    [1] 2.828427

c(tworoot, tworoot)
>    [1] 1.414214 1.414214
```

You can also assign from left to right, using `->`. This is useful in
combination with pipes:

``` r

tworoot |> exp() |> round() -> newvalue
```

------------------------------------------------------------------------

Note your variable names can also include `_`, `.`, or numeric digits,
as long as they aren’t at the beginning of the name. For example, `X1`
or `my_name` are valid names—but not `2X`.

## Basic Data Structures

In R, there two fundamental data structures that are used all the time:

- “atomic” **vectors**
- data.frames

### Vectors

In R, the basic units—the *atoms*, if you will—of information are called
“atomic” vectors. There are three basic atomic data types:

- **Numbers**—`numeric` values.
  - Examples: `3`, `4.2`, `-13`, `254.30`
- **Strings of characters**: `character` values.
  - Examples: `"note"`, `"a"`, `"do, a dear, a female dear"`
- **Logical Trues and Falses**: `logical` values.
  - Examples: `TRUE`, `FALSE`

You might be wondering, why are we calling these basic atoms
“*vectors*”? Well, in R, the basic atomic data types are always
considered a *collection* of ordered values. These ordered collections
are called vectors. In the simple examples above, each vector only had a
single value, so it just looks like one value—single values like this
are often called “scalars”. However, R doesn’t *really* distinguish
between scalars (single values) and vectors (multiple values)—everything
is always a vector. (Still, we sometimes refer to length-1 vectors as
scalars.)

To make a vector from scratch in R, use
[`c()`](https://rdrr.io/r/base/c.html), as so:

``` r

c(1, 2, 3)
>    [1] 1 2 3

c("Bach", "Mozart", "Beethoven", "Brahms")
>    [1] "Bach"      "Mozart"    "Beethoven" "Brahms"

c(TRUE, FALSE)
>    [1]  TRUE FALSE

c(32.3)
>    [1] 32.3

32.3
>    [1] 32.3
```

In this example, we’ve created five vectors.

- A `numeric` vector of length 3.
- A `character` vector of length four (composers).
- A `logical` vector of length 2.
- Two `numeric` vectors of length 1.
  - That’s right, `c(32.3)` and `32.3` are the same thing—a vector of
    length 1.

------------------------------------------------------------------------

Notice that vectors can’t mix-and-match different data types; which
makes sense because a vector *is* a single type of thing. But this means
that commands like `c(3, "a")` will actually create a `character`
vector, where the `3` is forced to be a character (`"3"`).

#### Vectorization

Having everything be a vector all the time is very useful, because it
allows us to think of and use collections of data as single thing. If I
give you, say, ten thousand numbers, you don’t have to worry about
manipulating ten thousand things: rather, you just work with *one*
thing: a vector, which happens to be of length 10,000. In R, we call
this **vectorization**—generally, in R and in humdrum$_{\mathbb{R}}$ we
will constantly be taking advantage of vectorization to make our lives
super easy!

------------------------------------------------------------------------

For an example of vectorization, watch this:

``` r

c(1, 1, 2, 3, 5, 8, 13, 21) * 2
>    [1]  2  2  4  6 10 16 26 42
```

We created two `numeric` vectors:

1.  The first eight numbers of the Fibonacci sequence
2.  the single number `2`

and multiplied them together! Notice that the entire Fibonacci vector is
multiplied by two! We don’t have to worry about multiplying each number
of the vector, it’s done for us.

------------------------------------------------------------------------

There are two ideal circumstances for working with vectors.

1.  They are the same length.
2.  One vector is length 1, and the other isn’t.

In the first case, we work with multiple vectors that are all the same
length, each value in each vector is “lined” up with values in the other
vector. If we, for example, add two such vectors together, each “lined
up” pair of numbers is added:

``` r
c(1, 2, 3) + c(5, 4, 3)
>    [1] 6 6 6


paste(c('a', 'b', 'c'), c(1, 2, 3))
>    [1] "a 1" "b 2" "c 3"
```

In the second case, one of the vectors is length-1 (a “scalar”). In this
case, the scalar value is paired with each value in the longer vector
(as in the Fibonacci example above).

``` r
c(1, 2, 3) + 5
>    [1] 6 7 8


paste(c('a', 'b', 'c'), 1)
>    [1] "a 1" "b 1" "c 1"
```

------------------------------------------------------------------------

What happens if we have vectors that are longer than one, but are not
the same length? Well, R will generally attempt to “recycle” the shorter
vector—which means repeat it— as necessary to match the length of the
longer vector. If the shorter vector evenly divides the longer vector,
you generally won’t have a problem:

``` r

c(1, 2, 3, 4) * c(2, 3)
>    [1]  2  6  6 12
```

If the division is not perfect, R will still “recycle” the shorter
vector, but you’ll get a warning:

``` r
c(1, 2, 3, 4) * c(2, 3, 4)
>    Warning in c(1, 2, 3, 4) * c(2, 3, 4): longer object length is not a multiple
>    of shorter object length
>    [1]  2  6 12  8
```

You see the warning message R have us?

> “longer object length is not a multiple of shorter object length”

That’s R telling us that we’ve got an obvious mismatch in the lengths of
our vectors.

------------------------------------------------------------------------

Generally, it is best to work with vectors that are all the same length
and/or scalar values (length-1 vectors), so you can avoid worrying about
how exactly R is “recycling” values. This brings us too…

#### Factors

Factors are a useful modification of `character` vectors, which keep
track of all the possible values (“levels”) you expect in your data,
*even when some of those levels are missing from the vector*. This is
mainly useful when we are counting data with
[`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md).

For example, let’s consider the built-in R object called `letters`:

``` r
letters
>     [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
>    [20] "t" "u" "v" "w" "x" "y" "z"
```

What happens if we call
[`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
on `letters`?:

``` r
table(letters)
>    letters
>    a b c d e f g h i j k l m n o p q r s t u v w x y z 
>    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

Every letter appears once in the table, duh! What if we randomly sample
a handful letters and table the result?

``` r
sample(letters, 15, replace = TRUE) |> table()
>    
>    d e f i l m o w x 
>    1 3 2 1 1 2 3 1 1
```

Notice that not all the letters from table appear in the output. E.g.,
if a letter never appears in the sample, it doesn’t get counted.

Let’s try something new: before sampling, I will call the command
[`factor()`](https://bit64.r-lib.org/reference/factor.html) on
`letters`:

``` r
factor(letters) |> sample(15, replace = TRUE) |> table()
>    
>    a b c d e f g h i j k l m n o p q r s t u v w x y z 
>    0 3 1 1 1 1 0 0 0 0 2 1 0 0 0 1 0 0 0 0 1 1 1 0 1 0
```

Ah! Now our table includes all possible letters, even though many of
them appear `0` times.

------------------------------------------------------------------------

So how does this work? Well the
[`factor()`](https://bit64.r-lib.org/reference/factor.html) function
looks at a `character` vector and outputs a new “factor” vector. The
factor vector acts just like a `character` vector, except it remembers
all the unique values, or “levels”, in the vector:

``` r
factor(letters)
>     [1] a b c d e f g h i j k l m n o p q r s t u v w x y z
>    Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
```

Even if we remove some values from the factor vector, the vector will
“remember” these levels. The factor will also remember the *order* of
the levels, so you can make tables ordered the way you want them.

You can access, or set, the levels of a factor using these using the
[`levels()`](https://rdrr.io/r/base/levels.html) function, or with the
`levels` argument to the
[`factor()`](https://bit64.r-lib.org/reference/factor.html) function
itself. Maybe we want to tabulate the letters, but put the vowels first:

``` r
factor(letters, levels = c('a', 'e', 'i', 'o', 'u',
                           "b", "c", "d", "f", "g", "h", "j", "k", 
                           "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")) |>
  sample(15) |>
  table()
>    
>    a e i o u b c d f g h j k l m n p q r s t v w x y z 
>    1 0 1 0 1 1 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1 0 0 0
```

Note that if a `character` string contains values that you don’t include
in your `levels`, the value will show up `NA` in the resulting factor,
and you may see warnigns like “`invalid factor level, NA generated`.”

### Data frames

Data frames are the heart and soul of R. A `data.frame` is simply a
collection of vectors that are *all the same length*—ideal for
vectorized operations! The vectors in a `data.frame` are arranged as
columns in a two dimension table. Let’s make a data frame, by feeding
some vectors to the \[data.frame()\] function:

``` r
X <- c("C", "D", "E", "F", "G", "A", "B", "C")
Y <- c(0, 2, 4, 5, 7, 9, 11, 12)
Z <- c("P1", "M2", "M3", "P4", "P5", "M6", "M7", "P8")

df <- data.frame(X, Y, Z)

df
>      X  Y  Z
>    1 C  0 P1
>    2 D  2 M2
>    3 E  4 M3
>    4 F  5 P4
>    5 G  7 P5
>    6 A  9 M6
>    7 B 11 M7
>    8 C 12 P8
```

Notice that each of your columns/vectors can be a different type, with
no problem. Also notice, that each column has a name; we can inspect
these names using the
[`colnames()`](https://rdrr.io/r/base/colnames.html) function.

``` r
colnames(df)
>    [1] "X" "Y" "Z"
```

Or change them:

``` r
colnames(df) <- c('Letters', "Semitones", "Intervals")
df
>      Letters Semitones Intervals
>    1       C         0        P1
>    2       D         2        M2
>    3       E         4        M3
>    4       F         5        P4
>    5       G         7        P5
>    6       A         9        M6
>    7       B        11        M7
>    8       C        12        P8
```

Finally, it’s also possible to assign the column name we want when
creating the data frame:

``` r
data.frame(Letters = X, Semitones = Y, Intervals = Z)
>      Letters Semitones Intervals
>    1       C         0        P1
>    2       D         2        M2
>    3       E         4        M3
>    4       F         5        P4
>    5       G         7        P5
>    6       A         9        M6
>    7       B        11        M7
>    8       C        12        P8
```

------------------------------------------------------------------------

Remember, the vectors in a `data.frame` must all be the same length. If
you tried to make a `data.frame` with a vectors that don’t match in
length, you’ll get an error
“`arguments imply differing number of rows`.” The one exception is that
you can call `data.frame` with some *scalar* single values, which will
be automatically recycled to match the length of the other vectors.

#### With and Within You

We often want to access the columns/vectors held in a data frame. We can
do this several ways. One approach is with the `$` operator, combined
with the name of the column we want. For example, we can get the
`Letters` column from the data frame we made above using `df$Letters`.

Often, we’ll want to write code that uses a bunch of different columns
from the same data.frame—in fact, this is the *main* thing we do most of
the time in R! To avoid writing `df$` over and over again, we can use
the
[`with()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
function.
[`with()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
allows us to drop “inside” our `data.frame`, where our R commands can
“see” the columns variables:

``` r
with(df,
     paste(Intervals, Semitones, sep = ' = '))
>    [1] "P1 = 0"  "M2 = 2"  "M3 = 4"  "P4 = 5"  "P5 = 7"  "M6 = 9"  "M7 = 11"
>    [8] "P8 = 12"
```

### Missing Data

Sometimes we’ll encounter data points which are irrelevant, meaningless,
or “not applicable.” In other cases, there may be relevant data that is
“missing.” R provides two distinct ways to represent missing/irrelevant
data: `NULL` and `NA`.

`NULL` is a special R object/variable, which is used represent something
that is totally missing or empty. `NULL` has no length
(`length(NULL) == 0`) and no value. It cannot be indexed. Many functions
will give an error if passed a `NULL`.

`NA` is quite different than `NULL`. Any atomic vector can have `NA`
value at any (or all) indices—in fact, you can have vectors or `NA`
values. The `NA` values are still “values” in a vector, but they are
used to indicate when there are values that are missing or problematic.
Passing a vector with `NA` values to most functions does not lead to an
error, though you’ll often get a warning message instead. For example,
consider what happens if we apply the command
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to the following
strings:

``` r
numbers <- as.numeric(c("1", "2", "apple", "4.2"))
>    Warning: NAs introduced by coercion
numbers
>    [1] 1.0 2.0  NA 4.2
```

Four of the strings in this vector are converted to numbers without a
problem, but the string `"apple"` makes no sense as a number. So what
does R do? It converts the three strings to numbers, just like
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) is supposed to,
but the `"apple"` string appears as `NA` in the outut. We also get
warning message: `NAs introduced by coercion`. You might see that
warning sometimes, so now you know what it means!

What would happen if we tried applying a different function onto our
vector with an `NA`?

``` r
sqrt(numbers)
>    [1] 1.000000 1.414214       NA 2.049390
```

The [`sqrt()`](https://rdrr.io/r/base/MathFun.html) function has no
problem taking the square-roots of the three numbers, and it simply
“propogates” the `NA` value in its input through to its output. The
“propogation” of missing values is a very useful feature in R: it makes
sure that we *keep track* of what data is missing, while keeping our
vectors all their original lengths.

## Common Functions

- [`getwd()`](https://rdrr.io/r/base/getwd.html) — Get R’s current
  working directory.
- [`setwd()`](https://rdrr.io/r/base/getwd.html) — Set R’s working
  directory.
- [`summary()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md)
  — Summarize the contents of an R object.

### Vector functions

- [`sort()`](https://rdrr.io/r/base/sort.html) — Put values of a vector
  into ascending order.
  - set `decreasing = TRUE` for decreasing order.
- [`rev()`](https://rdrr.io/r/base/rev.html) — Reverse the order of a
  vector.
- [`rep()`](https://rdrr.io/r/base/rep.html) — Repeat a vector.
- [`unique()`](https://rdrr.io/r/base/unique.html) — Returns only the
  unique values of a vector.
- `x %in% y` — Which elements of the vector `x` appear in the vector
  `y`?
- [`length()`](https://rdrr.io/r/base/length.html) — How long is the
  vector (or [`list()`](https://rdrr.io/r/base/list.html))?
- [`head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html) — Return the first or
  last $N$ elements of a vector.
  - Provide the `n` argument a natural number to control $N$.

#### Sequences and Indices

- `x:y` — Create a sequence of integers from `x` to `y`.
  - For example, `1:10` makes a vector of integers from one to ten.
- [`seq()`](https://rdrr.io/r/base/seq.html) — Create arbitrary
  sequences of numbers.
- [`which()`](https://rdrr.io/r/base/which.html) — Which indices in a
  logical vector are true?
  - For example, `which(c(TRUE, FALSE, TRUE))` returns `c(1,3)`.

### String functions

- [`paste()`](https://rdrr.io/r/base/paste.html) — Paste together
  multiple `character` strings.
- [`nchar()`](https://rdrr.io/r/base/nchar.html) — Counts how many
  characters there are in each string of a character vector.

### Math

#### Arithmetic

- `x + y` — Addition; $x + y$.
- `x - y` — Subtraction; $x - y$.
- `-x` — Negation; $-x$.
- `x * y` — Multiplication; $xy$
- `x^y` — Exponentiation; $x^{y}$.
  - Use parentheses for things like `x^(1/3)`; $x^{\frac{1}{3}}$.
- `x / y` — Real division; $\frac{x}{y}$.
- `x %/% y` — [Euclidean
  division](https://en.wikipedia.org/wiki/Euclidean_division);
  $\lfloor\frac{x}{y}\rfloor$.
  - E.g., whole-number division with remainder.
- `x %% y` — `x` modulo `y`; $x{\mspace{8mu}\operatorname{mod}\ y}$.
  - E.g., remainder after whole-number division.
- `diff(x)` — This function calculates the differences between
  consecutive values in a numeric vector.
  - `diff(c(5, 3))` is the same as `3 - 5`.

#### Other Math functions

- `sqrt(x)` — Square-root of numbers; $\sqrt{x}$.
- `abs(x)` — Absolute value of numbers; $|x|$
- `round(x)` — Round number to nearest integer; $\lfloor x\rceil$
- `log(x)` — Log of number (natural log by default); $\log(x)$
- `sign(x)` — Sign (1, -1, or 0) of x; $\text{sgn}\ x$

#### Distribution and Tendency Functions

- `sum(x)` — The sum of a numeric vector.
- `max(x)` — The maximum value in a numeric vector.
- `min(x)` — The minimum value in a numeric vector.
- `range(x)` — The minimum *and* maximum values of a numeric vector.
  - To get the size of the range, use `diff(range(x))`.
- `mean(x)` — The arithmetic mean of numeric vector.
- `median(x)` — The median of numeric vector.
- `quantile(x)` — Other distribution
  [quantiles](https://en.wikipedia.org/wiki/Quantile) of numeric vector.

### Randomization functions

- [`sample()`](https://rdrr.io/r/base/sample.html) — Takes a random
  sample from a vector. Can also be used to randomize the order of a
  vector.

### Analysis Functions

- [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  — Tabulate all unique values in vector, or cross-tabulate across
  multiple vectors.
  - When using humdrum$_{\mathbb{R}}$, you should use the similar
    \[count()\] instead!

## Useful tricks

- Would you like to know how many elements in a vector match a logical
  criteria? Take the sum of the logic:
  - `sum((1:100) > 55)`
  - `sum(letters %in% c('a', 'e', 'i', 'o', 'u'))`
- Would you like to know what *proportion* of values match your logical
  criteria? Take the *mean* of the logic:
  - `mean((1:100) > 55)`
  - `mean(letters %in% c('a', 'e', 'i', 'o', 'u'))`

## Making your own functions

To make your function in R, you use the `function` keyword, like so:

    function(argument1, argument2, etc.) {
      Expressions to evaluate here, involving the arguments

    }

For example, let’s make a function that subtracts the mean from a vector
of numbers. We’ll have one argument, which we’ll call `numbers`.

``` r

myfunc <- function(numbers) {
  mean <- mean(numbers)
  numbers - mean
}
```

We’ve created our function, and assigned it the name `myfunc`, just like
any other assignment. Let’s try it out:

``` r
myfunc(1:9)
>    [1] -4 -3 -2 -1  0  1  2  3  4
```

Notice that the *last* expression in your function definition is the
value that gets “returned” by the function.

------------------------------------------------------------------------

If you are feeling lazy, you can also define a function using a few less
keystrokes using the command `\()` instead of `function()`. For example,

``` r
myfunc <- \(x) x + 1
myfunc(1:9)
>    [1]  2  3  4  5  6  7  8  9 10
```

## Common Errors and Warnings

TBA
