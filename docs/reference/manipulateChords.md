# Manipulate chord data

Manipulate chord data

## Usage

``` r
reduceHarmony(
  x,
  max.extension = 5L,
  unSus = TRUE,
  unAlter = FALSE,
  fill.extensions = FALSE,
  ...
)

# S3 method for class 'tertianSet'
reduceHarmony(
  x,
  max.extension = 5L,
  unSus = TRUE,
  unAlter = FALSE,
  fill.extensions = FALSE,
  Key = NULL
)

# S3 method for class 'character'
reduceHarmony(
  x,
  max.extension = 5L,
  unSus = TRUE,
  unAlter = FALSE,
  fill.extensions = FALSE,
  Key = NULL,
  ...
)

rootPosition(x, ...)

# S3 method for class 'tertianSet'
rootPosition(x)

# S3 method for class 'character'
rootPosition(x, ...)
```
