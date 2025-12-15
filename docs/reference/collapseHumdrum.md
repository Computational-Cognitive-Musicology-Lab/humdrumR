# "Collapse" humdrumR data into a field

`collapseHumdrum` allows you to collapse a data field across across
groups within the data indicated by the `by` argument. Data is
"collapsed" either by [pasting](https://rdrr.io/r/base/paste.html) the
data into a string, or by putting them into
[list](https://rdrr.io/r/base/list.html). `collapseStops`,
`collapsePaths`, and `collapseRecords` are built-in calls to
`collapseHumtab`, with some additional optimizations.

## Usage

``` r
collapseHumdrum(
  humdrumR,
  by,
  fields = selectedFields(humdrumR),
  dataTypes = "GLIMDd",
  collapseAtomic = TRUE,
  sep = " "
)

collapseStops(
  humdrumR,
  fields = selectedFields(humdrumR),
  collapseAtomic = TRUE,
  sep = " "
)

collapsePaths(
  humdrumR,
  fields = selectedFields(humdrumR),
  collapseAtomic = TRUE,
  sep = " "
)

collapseRecords(
  humdrumR,
  fields = selectedFields(humdrumR),
  collapseAtomic = TRUE,
  sep = " "
)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- by:

  ***Fields to collapse data within.***

  Must be `character`.

  Must be `character` strings
  [partially](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  matching the name(s) of a
  [field(s)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the `humdrumR` input.

  Data in the `fields` will be collapsed within these fields.

- fields:

  ***The target field(s) in the `humdrumR` data to collapse.***

  Defaults to `selectedFields(humdrumR)`.

  Must be `character` strings
  [partially](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  matching the name(s) of a data
  [field(s)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  in the `humdrumR` input.

- dataTypes:

  ***Which types of humdrum record(s) to collapse.***

  Defaults to `"GLIMDd"`.

  Must be `character`. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'`
  or any combination of these (e.g., `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation **Fields** section for explanation.)

- collapseAtomic:

  ***Whether to collapse the data into `character` strings.***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, data is collapsed into a single `character` string. If
  `FALSE`, data is conctanated in a `list`.

- sep:

  ***A separator for collapsed strings.***

  Defaults to `" "` (space).

  Must be a single `character` string.

  Only has an effect if `collapseAtomic == TRUE`.

## See also

The humdrum [folding
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
serve a similar function, "folding" data into *new* fields, rather than
collapsing it within a field.

Other Humdrum table reshaping functions:
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md),
[`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md),
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)
