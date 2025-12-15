# humdrumR coercion

Many users may wish to work with humdrum data, without having to rely on
`humdrumR`'s
[with(in).humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
functionality. Rather, you'd like to just get "normal" `R` objects out
of your humdrum data. `humdrumR` defines a number of functions/methods
for "coercing" [humdrum
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
into basic `R` data types.

## Usage

``` r
# S4 method for class 'humdrumR'
as.vector(x, mode = "any")

as.lines(
  humdrumR,
  dataTypes = "GLIMDd",
  padPaths = "dont",
  padder = "",
  sep = "\t"
)

# S3 method for class 'humdrumR'
as.matrix(x, dataTypes = "GLIMDd", padPaths = "corpus", padder = NA)

# S4 method for class 'humdrumR'
as.data.frame(x, dataTypes = "Dd", padPaths = "corpus", padder = NA)

# S4 method for class 'humdrumR'
as.data.frame(x, dataTypes = "Dd", padPaths = "corpus", padder = NA)

as.matrices(humdrumR, dataTypes = "LIMDd", padPaths = "piece", padder = NA)

as.data.frames(humdrumR, dataTypes = "LIMDd", padPaths = "piece", padder = NA)
```

## Arguments

- mode:

  ***The desired output class.***

  Defaults to `"any"`.

  Must be a single `character` string naming an [atomic vector
  type](https://rdrr.io/r/base/vector.html) to coerce the output to
  (i.e., `logical` or `numeric`).

  If set to `"any"`, the output type is simply whatever the type of the
  [selected
  field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  is.

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- dataTypes:

  ***Which types of humdrum record(s) to include.***

  Defaults to `"GLIMDd"` for `as.lines()` and `as.matrix()`; `"Dd"` for
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html);
  `"LIMDd"` for `as.matrices()` and `as.data.frames()`.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation for explanation.)

- padPaths:

  ***Determines how spine-paths are aligned in the output.***

  Defaults to `"dont"` for `as.lines()`; `"corpus"` for `as.matrix()`
  and [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html);
  `"piece"` for `as.matrices()` and `as.data.frames()`

  Must be a single `character` string, `"corpus"`, `"piece"`, or
  `"dont"`.

  See the details for an explanation.

- padder:

  ***Used to fill in differences in the number of columns between files
  and/or spine paths.***

  Defaults to `NA`.

  Must be a single `atomic` value.

- sep:

  ***Separator to place between columns in collapsed lines.***

  Defaults to `"\t"` (tab).

  Must be a single `character` string.

## Details

Generally, coercion works by evaluating a humdrumR object's [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
and forcing the result to be an atomic vector. When multiple fields are
selected, they are pasted together, separated by `", "`. If a field is
not atomic (like a `list`, or `lm` object), a concise representation of
the list or object class is printed. The as.vector(humdrumR) has the
additional option of coercing the resulting vector to a particular type
using the `mode` argument.

The [as.matrix(humdrumR)](https://rdrr.io/r/base/matrix.html) method
take things a step further by putting the evaluated fields into a
two-dimensional matrix, with rows representing records and columns
indicating spine paths (see Padding section below).
[as.data.frame(humdrumR)](https://rdrr.io/r/base/as.data.frame.html)
first calls `as.matrix` then converts the matrix to a `data.frame`. Note
that `as.matrix(humdrumR)` places the *entire* corpus object into one
matrix, even if there are multiple pieces. In contrast, the plural
`as.matrices` and `as.data.frames` call their respective singular
versions separately on each individual file in a [humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
and return them all in a list. The [row
names](https://rdrr.io/r/base/colnames.html) of the
`matrix`/`data.frame`(s) consist of two integer values, separated by a
`.`, representing: `Piece.Record`.

The `as.lines` function converts a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
into a `character` vector of text lines, with columns separated by the
`sep` argument (defaults to `"\t"`), just as you'd see in a
humdrum-syntax file. Each line is a single row from a
`as.matrix.humdrumR`, with padded values at the right side removed. The
matrix's `Piece.Record` [row
names](https://rdrr.io/r/base/colnames.html) are preserved as the lines'
[names](https://rdrr.io/r/base/names.html).

Note that multiple-stop token (where `Stop > 1L`) cannot by incorporated
into the two dimensional `matrix`/`data.frame`. Thus,
`as.matrix(humdrumR)` calls [collapseStops(collapseAtomic = TRUE, sep =
"
")](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
on the [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
before creating a matrix.

## Padding

Different pieces in a single [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
often differ in the number of spines and/or spine paths they contain. To
squish them into a two dimensional object (`matrix` or `data.frame`)
they must necessarily be padded to the same number of columns. (Global
comments—which actually have `NA` spines—are also padded, placing the
record in column 1.) The `pad` argument is a single atomic value which
is used to pad the matrix.

Another consideration is the behavior of spine paths. In the humdrum
syntax, a spine path in a leftward spine "bumps" data in higher spines
into new columns, as in this example:

    **kern  **kern
    A       E
    *^      *
    A       C       E
    G       B       D
    *v      *v      *
    A       C
    *-      *-

At the beginning and end of the file, the second column holds data for
the second spine. However, in the middle of the file, the second column
holds data from the second spine path of the first spine. To make the
spine structure clearer, `as.matrix(humdrumR)` has the option to pad
spine paths. For example, using `"_"` as our `pad` argument:

    **kern   _        **kern
    A        _        E
    *^       _        *
    A        C        E
    G        B        D
    *v       *v       *
    A        _        C
    *-       _        *-

This aspect of the matrix padding behavior can be controlled with the
`padPaths` argument, with three possible values/behaviors:

- `"corpus"`: Paths are padded such that spine-paths across all pieces
  in the corpus all align in the same columns. If even one file has a
  spine path, all the other files are padded so their spines stay
  aligned. This is the default behavior for `as.matrix(humdrumR)`.

- `"piece"`: Paths are padded, but only *within* each piece. The
  spines/paths between different pieces may not align.

- `"dont"`: Paths are not padded at all.
