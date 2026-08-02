# Tabulate records and tokens in a humdrumR corpus

`census` tabulates the raw "size" of a [humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
including the total number of records and tokens. `census` is one of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)'s
basic [corpus summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).

## Usage

``` r
census(
  humdrumR,
  dataTypes = "GLIMDd",
  by = Piece,
  removeEmpty = FALSE,
  drop = FALSE
)

census(humdata)[i]

# S3 method for class 'humCensus'
print(censusTable, showEach = TRUE, screenWidth = options("width")$width - 10L)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- dataTypes:

  ***Which types of humdrum records to include in the census.***

  Defaults to `"GLIMDd"`.

  Must be `character`. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'`
  or any combination of these (e.g., `"LIM"`). (see the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.).

- by:

  ***An arbitrary expression which indicates how to group the data.***

  Defaults to `Piece` (a humdrumR [data
  field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)).

- removeEmpty:

  ***Whether to include zero tokens.***

  Defaults to `FALSE`

  Must be a singleton `logical` value: an on/off switch.

  If set `TRUE`, any groups that have zero tokens are not included in
  the `humCensus` table.

- drop:

  ***Whether to return normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) or a
  `humCensus` table.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `drop = TRUE`, a normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) is
  returned instead of a `humCensus` table.

- i:

  ***Index for rows.***

  If `numeric`, selects rows by index. If `character`, the string is
  matched as a regular expression against the "by-group" names.

## Details

`census` returns a special `data.frame` called a `humCensus` table. A
`humCensus` table has five columns of information:

1.  Records

    - The total number of records.

2.  Tokens

    - The total number of tokens.

3.  (unique)

    - The number of **unique** tokens

4.  Characters

    - The total number of characters. (This includes humdrum control
      characters like `*` and `!!`.)

5.  (per token)

    - This is simply `Characters / Tokens`, indicating the mean length
      of each token.

By default, `census` tabulates data within pieces in the corpus, with
each piece tabulated in a row of the `humCensus` table. Rows are labeled
with each file name. When a `humCensus` object is printed, the totals
across all pieces are printed as well—(unique) and (per token) values
are calculated across all pieces as well, not summed. The `by` argument
can be used to tabulate data across other divisions in the data (see
next section).

## Tabulate "by" other groups

The `by` argument to `census` indicates groupings in the data to
tabulate within, grouping across pieces in the corpus by default. `by`
can be an arbitrary expression which is evaluated inside the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md),
like the `groupby` argument to a
[with/within](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
call. The by expression must be the full length of the humdrum table.

## See also

Other corpus summary functions:
[`humSummary`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md),
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md),
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)

## Examples

``` r
chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/*.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpL03I08/temp_libpath1dfb3eb89ac20/humdrumR/HumdrumData/BachChorales/*.krn' matches 10 text files in 1 directory.
#> Ten files read from disk.
#> Validating ten files...
#> all valid.
#> Parsing ten files...
#> Assembling corpus...
#> Done!
census(chorales)
#> 
#> ###### Census of GLIMDdSE records in humdrumR corpus "chorales" (ten pieces):
#> ###### Grouped by ten Pieces:
#>                       Records   Tokens (unique)   Characters    ***
#> chor001.krn  1 [ 1]       133      484    (148)         1910    ***
#> chor002.krn  2 [ 2]       124      451    (148)         1897    ***
#> chor003.krn  3 [ 3]       110      386    (132)         1867    ***
#> chor004.krn  4 [ 4]       103      367    (129)         1711    ***
#> chor005.krn  5 [ 5]       172      643    (159)         2233    ***
#> chor006.krn  6 [ 6]        77      263    (125)         1313    ***
#> chor007.krn  7 [ 7]       179      671    (168)         2415    ***
#> chor008.krn  8 [ 8]       171      639    (165)         2298    ***
#> chor009.krn  9 [ 9]       131      479    (167)         1923    ***
#> chor010.krn 10 [10]       100      355    (133)         1599    ***
#> ###### Totals:
#>                         1,300    4,738    (503)       19,166    ***
#> 
#> 
#>                   (***one column not displayed due to screensize***)
```
