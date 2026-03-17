# humdrumR

`humdrumR` is a toolkit for the analysis of data encoded in the [humdrum
syntax](http://www.humdrum.org/guide/ch05/). The humdrum syntax is an
incredibly flexible, and powerful, scheme for encoding musical data.
Tens of thousands of musical scores (and other musical data) have been
encoded in the humdrum syntax, many available online through
repositories such as [KernScores](http://kern.ccarh.org/). The
`humdrumR` package is intended as a modernized replacement for the
original [humdrum toolkit](http://www.humdrum.org/), leveraging the
power of `R` to give us unprecedented power to manipulate and analyze
humdrum data using concise, expressive syntax.

`humdrumRroot` is the path to where the `humdrumR` package is install on
your machine. When you installed `humdrumR` a few basic humdrum files
were stored here as well, in subdirectories `examples` and
`HumdrumData`.

The `humdrumR()` *function* is used to set global package options within
an R session, mostly regarding the viewing of [humdrumR
datasets](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Usage

``` r
humdrumRroot

humdrumRdata

humdrumR(
  view,
  dataTypes,
  maxRecordsPerFile,
  maxTokenLength,
  nullPrint,
  syntaxHighlight,
  censorEmptyRecords
)
```

## Format

An object of class `character` of length 1.

An object of class `list` of length 5.

## Arguments

- view:

  ***How should humdrumR data be printed?***

  There are three options: `"humdrum"`, `"score"`, and `"table"`
  (aliases `"data.frame"` and `"tibble"`). These options are [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md).

  Use
  [`select()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  to determine which fields to show.

- dataTypes:

  ***Which types of humdrum record(s) to view.***

  Defaults to `"GLIMDd"` for
  [`as.lines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  and
  [`as.matrix()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md);
  `"Dd"` for
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html);
  `"LIMDd"` for
  [`as.matrices()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  and
  [`as.data.frames()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md).

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation for explanation.)

- maxRecordsPerFile:

  ***How many records should be shown in each file, when more than one
  file is present?***

  Defaults to `40`.

  Can be any positive whole number.

- maxTokenLength:

  ***Length at which longer tokens are censored with ...***

  Defaults to `16`.

  Can be any positive whole number.

- nullPrint:

  ***How should null data points print?***

  Default is `"NA2dot"`.

  Must be a single character string, [partially
  matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  `"NA2dot"`, `"dot2NA"`, `'charNA2dot"`, or `"asis"`. `"NA2dot"` means
  all `NA` values are converted to `"."`; `"dot2NA` means all `"."` are
  converted to `NA`; `charNA2dot` means `NA` values in `character`
  vectors are converted to `NA`, but not in other atomic types; `"asis"`
  means either `NA` or `"."` values may print, depending on what is in
  the field.

- syntaxHighlight:

  ***Should syntax highlighting (coloring) be used in printout?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value; an on/off switch.

- censorEmptyRecords:

  ***Should consecutive records be "censored" (compressed) in
  printout?***

  Defaults to `30`.

  Can be any positive whole number, up to `Inf`. If `Inf`, no censoring
  will occur.

## Package design

The package `humdrumR` has seven main components:

- To represent humdrum data in R, we have the
  [humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
  [S4 class](http://adv-r.had.co.nz/S4.md), and it's core component the
  [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).

- To create `humdrumR` data, a sophisticated humdrum data parser:
  [readHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).
  `humdrumR` data can also be written back to humdrum-syntax text files
  using
  [writeHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md).

- To filter `humdrumR` data, we have the
  [subset()/filter()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  functions, as well as methhods for `R`'s standard [indexing
  operators](https://rdrr.io/r/base/Extract.html) (`[]` and `[[]]`).

- To manipulate and modify `humdrumR` data, we have the [with and
  within](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  methods for `humdrumR` objects, and tidyverse aliases
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  and
  [`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html).

- To facilitate the development of functions to work with humdrum
  tokens—which are simple character strings packed with information—, a
  useful API we call our [regular-expression dispatch
  system](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md).

- Several
  [modules](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumPitch.md)
  for representing and manipulating musical pitch information, including
  our core
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
  class to represent tonal pitch.

- A module for representing and manipulating musical rhythm information,
  with a core
  [rhythmInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
  class to represent rhythms.

## Package options

The `humdrumR()` function sets general options for the package, mostly
related to how [humdrumR data
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
are viewed. Each argument to the function manipulates a print/package
option: for any argument that is not used, the option remains in its
current setting (i.e., unchanged). These package options are all
enumerated and explained in the **Arguments** section above.

## Examples

``` r
# change default view to table
humdrumR("table")

humdrumR(view = 'humdrum', maxRecordsPerFile = 50)

# see the humdrumR package directory contents
dir(humdrumRroot) 
#>  [1] "CITATION"    "DESCRIPTION" "HumdrumData" "INDEX"       "LICENSE"    
#>  [6] "Meta"        "NAMESPACE"   "NEWS.md"     "R"           "data"       
#> [11] "examples"    "extdata"     "help"        "html"       
```
