# Extract properties of chords

Extract properties of chords

## Usage

``` r
root(x, deparser = kern, ..., parseArgs = list())

bass(x, deparser = kern, ..., parseArgs = list())

inversion(x, inversion.labels = NULL, parseArgs = list())

# Default S3 method
is.major(x, ...)

# Default S3 method
is.minor(x, ...)

hasExtension(x, extension = c(7L, 9L, 11L, 13L), ...)

# S3 method for class 'tertianSet'
hasExtension(x, extension = c(7L, 9L, 11L, 13L))

# S3 method for class 'character'
hasExtension(x, extension = c(7L, 9L, 11L, 13L), ...)

isAltered(x, extension = c(7L, 9L, 11L, 13L))

# S3 method for class 'tertianSet'
isAltered(x, extension = c(7L, 9L, 11L, 13L))

# S3 method for class 'character'
isAltered(x, extension = c(7L, 9L, 11L, 13L), ...)

hasThird(x)

hasFifth(x)

hasSeventh(x)
```
