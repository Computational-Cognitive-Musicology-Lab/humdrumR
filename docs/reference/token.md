# Humdrum tokens

`token` is an `S4` class which acts as a simple "wrapper" around
`atomic` data, allowing `humdrumR` to give that data special treatment.
They are basically `atomic` vectors with a known exclusive
interpretation. You should be able to treat them exactly like their
"normal" class of `atomic` vector—e.g., `character`, or `numeric`.

## Usage

``` r
token(x, Exclusive = NULL, ...)

# S4 method for class 'token'
c(x, ...)

# S3 method for class 'token'
rep(x, ...)

# S3 method for class 'token'
unique(x, ...)

# S4 method for class 'token,ANY,ANY'
x[i, j, ..., drop = FALSE]

# S4 method for class 'token'
show(object)

# S3 method for class 'token'
format(x, ...)

is.token(x)

order.token(
  x,
  ...,
  na.last = TRUE,
  decreasing = FALSE,
  method = c("auto", "shell", "radix")
)

# S4 method for class 'token,token'
Arith(e1, e2)

# S4 method for class 'token'
diff(x, ...)

# S4 method for class 'token,character'
Arith(e1, e2)

# S4 method for class 'character,token'
Arith(e1, e2)

# S4 method for class 'token'
Summary(x, ..., na.rm = FALSE)
```
