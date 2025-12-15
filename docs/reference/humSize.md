# [humdrumR data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md) size and shape

These functions can be used to quickly get basic information about the
size and "shape" of a [humdrumR corpus
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
For more details, use the
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
or
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
functions instead.

[HumdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
objects can be divided into "subcorpora." `anySubcorpora` and
`namesSubcorpora` functions tell us if there are any subcorpora and, if
so, what they are called.

## Usage

``` r
nrecord(humdrumR, dataTypes = "GLIMDd")

# S4 method for class 'humdrumR'
nrow(x)

ntoken(humdrumR, dataTypes = "GLIMDd")

npieces(humdrumR)

nfiles(humdrumR)

# S4 method for class 'humdrumR'
length(x)

# S4 method for class 'humdrumR'
ncol(x)

# S4 method for class 'humdrumR'
dim(x)

is.empty(humdrumR)

anyMultiPieceFiles(humdrumR)

anyPaths(humdrumR)

anyStops(humdrumR)

anySubcorpora(humdrumR)

namesSubcorpora(humdrumR)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- dataTypes:

  ***Which types of humdrum record(s) to include in the census.***

  Defaults to `"GLIMDd"`.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.)

## Details

The following functions are defined.

- `nfile` : The number of input files in the corpus.

  - [length](https://rdrr.io/r/base/length.html)`(humdrumR)` is a
    synonym.

- `npiece`: The number of pieces in the corpus. (There may be multiple
  pieces per file.)

- `nrecord`: The number of records in the corpus.

  - [nrow](https://rdrr.io/r/base/nrow.html)`(humdrumR)` is a synonym.

- `ntoken`: The number of tokens in the corpus.

- `ncol(humdrumR)`: Returns the maximum number of "columns" need to
  represent the data in a 2d matrix. Matches the default output from
  [as.matrix(humdrumR)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md).

- `dim(humdrumR)`: the same as `c(nrow(humdrumR), ncol(humdrumR))`.

## Is/Any

A few additional functions return quick `TRUE`/`FALSE` answers regarding
a [humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md):

- `is.empty`: Returns `TRUE` is a corpus contains no *non-null* data
  tokens (`D` tokens).

- `anyPaths`: Returns `TRUE` if there are any spine paths (`Path > 0`)
  in any pieces in the corpus.

- `anyStops`: Returns `TRUE` if there are any multi-stops (`Stop > 1`)
  in any pieces in the corpus.

- `anySubcorpora`: Returns `TRUE` if the corpus was
  [read](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  with different regex patterns matching "subcorpora" labels.

  - `namesSubcorpora` returns the names of the subcorpora labels
    (`Label` field).

- `anyMultiPieceFiles`: Returns `TRUE` if any files contain more than
  one piece (`Piece != File`).
