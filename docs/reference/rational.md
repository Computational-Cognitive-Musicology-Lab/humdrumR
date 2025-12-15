# Rational numbers

R has no built in rational number representation; `humdrumR` defines
one.

## Usage

``` r
rational(numerator, denominator = as.integer64(1L))

e1 %R% e2

numerator(x)

denominator(x)

# S4 method for class 'rational'
numerator(x)

# S4 method for class 'rational'
denominator(x)

is.rational(x)

# S4 method for class 'rational'
is.numeric(x)

# S4 method for class 'rational'
rank(x, na.last = TRUE, ties.method = "average")

# S4 method for class 'rational,rational'
Compare(e1, e2)

# S4 method for class 'rational,ANY'
Compare(e1, e2)

# S4 method for class 'ANY,rational'
Compare(e1, e2)

# S4 method for class 'rational'
Summary(x)

# S4 method for class 'rational'
prod(x, ..., na.rm = FALSE)

# S4 method for class 'rational'
abs(x)

# S4 method for class 'rational'
sign(x)

# S4 method for class 'rational'
max(x, ..., na.rm = FALSE)

# S4 method for class 'rational'
min(x, ..., na.rm = FALSE)

# S4 method for class 'rational'
mean(x)

# S4 method for class 'rational'
round(x)

# S4 method for class 'rational'
floor(x)

# S4 method for class 'rational'
ceiling(x)

# S4 method for class 'rational'
trunc(x)

# S4 method for class 'rational'
expand(x)

# S4 method for class 'rational'
sum(x, ..., na.rm = FALSE)

# S4 method for class 'rational'
cumsum(x)

as.rational(x, ...)

# S4 method for class 'rational'
as.rational(x, ...)

# S4 method for class 'matrix'
as.rational(x)

# S4 method for class 'integer'
as.rational(x)

# S4 method for class 'numeric'
as.rational(x)

# S4 method for class 'logical'
as.rational(x)

# S4 method for class 'character'
as.rational(x, sep = "/|%")

# S4 method for class 'fraction'
as.rational(x, sep = "/|%")

fraction(numerator, denominator, sep = "/")

as.fraction(x, sep = "/")

# S3 method for class 'fraction'
as.double(x)

# S3 method for class 'fraction'
as.integer(x)
```

## Details

Using rational numbers, we can represent numbers like 1/3 without any
numeric inaccuracies. In other words, \\1/3 \* 3 = 3\\, never
\\.999999999\\. On the other hand, if our rational numbers start to have
numerators or demoninators that are too large, we can run into integer
overflow problems. Since the rational numbers we'll be using in the
context of music analysis are relatively simple, we can safely use such
numbers without any numeric inaccuracy.

`fraction` is a class (and associated constructor) which represents
rational numbers as `character` strings. Unlike `rational`, the
`fraction` class is not numeric and thus cannot do arithmetic. However,
`fraction` can be converted to/from `rational`.

## See also

[`as.real()`](https://rdrr.io/r/base/base-defunct.html)
[`as.numeric()`](https://rdrr.io/r/base/numeric.html)
