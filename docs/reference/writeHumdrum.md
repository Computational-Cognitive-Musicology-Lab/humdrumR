# Write [humdrumR data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md) to humdrum files

`writeHumdrum` writes `humdrumR` data into humdrum-syntax text files.
The current [selected
field(s)](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
are evaluated to generate the humdrum output data. The written output
should match the printout if printing the data in the `R` terminal.

## Usage

``` r
writeHumdrum(
  humdrumR,
  prefix = "humdrumR_",
  renamer = force,
  affix = "",
  extension = NULL,
  separatePieces = FALSE,
  directory = NULL,
  overwrite = FALSE,
  verbose = FALSE,
  EMD = paste0("Edited in humdrumR ", packageVersion("humdrumR"), " on ", Sys.Date())
)
```

## Arguments

- humdrumR:

  ***HumdrumR data to write.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- prefix:

  ***A prefix to add to output filenames.***

  Defaults to `"humdrumR_"`.

  Must be a single `character` string.

- renamer:

  ***A `function` to modify output filenames.***

  Defaults to `force` (keep the original filenames).

  Must be a `function` which accepts a `character` vector and returns a
  new `charater` vector of the same length.

- affix:

  ***An affix to add to output filenames.***

  Defaults to `""`.

  Must be a single `character` string.

  Affix is appended at end of filename, but before the extension.

- extension:

  ***What extension to use for new files.***

  Defaults to `NULL`, which means the file extension of the original
  files is used.

  Must be `NULL`, or a single, non-empty `character` string.

- directory:

  ***A directory to write the files into.***

  Defaults to `NULL`.

  Must be a single `character` string.

  If `NULL`, files are written to the same directory (or directories)
  they were [read
  from](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).

- overwrite:

  ***Whether to overwrite existing files.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `FALSE`, `writeHumdrum` will refuse to overwrite any files. If
  `TRUE`, `writeHumdrum` will overwrite files, but only after an
  additional prompt from the user.

- verbose:

  ***Whether to show file names while writing.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, each *new* output file name is printed on the console as
  the writing happens.

- EMD:

  ***A string to write to a new `!!!EMD:` record in each file.***

  Defaults to "Edited using humdrumR, version X.X.X.XXX, on current
  data/time."

  Must be a single `character` string.

  If `NULL`, not appended.

## File names

The main option to control with `writeHumdrum` is what files to write
to. `writeHumdrum` uses the original names of the files, as read by
[readHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
as the basis for *new* file names. By default, `writeHumdrum` will
refuse to overwrite the original files—overwriting will only be allowed
if you specify `overwrite == TRUE` *and* respond with `"y"` to a prompt.

`writeHumdrum` generates *new* file names by modifying the original read
file names. The `renamer` argument must be a function which takes the
original names as an input `character` vector (excluding the directory
path and the file extension) and returns a new `character` vector of the
same length (the default is `R`'s identity function `force`). After
running `renamer`, the `character`-string `affix` and `prefix` arguments
are appended/prepended to the renamed names. (The `affix` is affixed
*before* the extension.) Finally, the `extension` argument can be used
to specify a different file extension.

If any files in your data set contain multiple pieces, you can either
write these pieces into the same files (`seperateFiles = FALSE`), as
they were originally, or write each piece into its own new file
(`separateFiles = TRUE`); if writing pieces into separate files, each
piece's file name is appended with `_pieceN`, where `N` is the number of
the piece within the file.

The `directory` argument indicates a file path where to write the files.
If the `directory` doesn't exist, it is created. If `directory` is
`NULL`, files are written to their original input directory (or
directories).

The `EMD` argument specifies a character string to put into a new
`!!!EMD` reference record at the end of each file. `EMD` (*Document
modification description*) records keep track of modifications to
humdrum data. The default behavior is to print a string indicating the
`humdrumR` version number and date. If `EMD` is set to `NULL`, it is not
appended to the files.
