# Export [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md) plots to files.

Export
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
plots to files.

## Usage

``` r
drawToFile(
  plot,
  filename = "humdrumR_draw.png",
  overwrite = FALSE,
  width = NULL,
  height = NULL,
  ...
)
```

## Arguments

- plot:

  ***A plot object (created by
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)).***

- filename:

  ***What filename to draw to?***

  Must be a single `character` string.

  The `filename` string must end with a file extension: either `.bmp`,
  `.jpg` (or `.jpeg`), `.pdf`, `.png`, `.svg`, or `.tiff`. This
  determines what format is used.

- overwrite:

  ***Whether to overwrite files without asking for permission.***

  Defaults to `FALSE`.

  If `TRUE`, `drawToFile()` will overwrite files without asking for
  permission. Otherwise, it will ask for keyboard confirmation before
  overwriting.

- width, height:

  ***Width and height of plot in output file.***

  Must be single `character` string or `numeric` values, or may be
  `NULL`

  If a `character` string, the string must begin with a number followed
  immediately by a valid unit abbreviation—either `mm` (millimeters),
  `cm` (centimeters), `in` (inches) or `px` (pixels). If `numeric`, the
  unit is taken to be inches.

  If `NULL`, sizes are selected based on plot itself.

## See also

Used with the
[`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
function.
