# Validate humdrum files

This function checks files on your local machine for violations of the
[humdrum syntax](http://www.humdrum.org/guide/ch05/). Detailed error
reports can be generated, pointing to specific problematic records in
files.

## Usage

``` r
validateHumdrum(..., errorReport.path = NULL)
```

## Arguments

- ...:

  ***Arguments passed to
  [`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).***

  Used to identify files on the local machine to test for humdrum
  validity. This is mainly used to pass [regex file-path search
  patterns](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
  but may also be used to pass the `recursive` and/or `contains`
  arguments to
  [`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).

- errorReport.path:

  ***A directory path where to write error report files.***

  Defaults to `NULL`.

  Must be a single `character` string.

  If `NULL` (the default), no error report files are written.

## Value

`validateHumdrum()` returns an "error frame" data.table object,
[invisibly](https://rdrr.io/r/base/invisible.html) (to "see" the output,
you must save it to a variable, *then* look at it). The error frame is a
`data.table` with three columns:

- `Filepath`: The file name.

- `Record`: Which record contains the error.

- `Message`: A description of the error.

Valid files will have no rows in the error frame.

## Details

Only violations of the general [humdrum
syntax](http://www.humdrum.org/guide/ch05/) are identified. For example,
missing exclusive interpretations, `*-` spine enders, or null data
tokens. The `validateHumdrum` function *does not* check for ill-formed
data content—for example, a `**kern` spine containing the token
`"Lsharp"` will not be rejected. Note that `validateHumdrum` is quite
picky about details! "Hanging white space," even in global records, will
be marked as invalid files!

`validateHumdrum` is called in the same manner as
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
by providing one or more regex search patterns to match files on your
machine. (The `...`, `recursive`, and `contains` arguments are all
simply passed to
[`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).)
When called, `validateHumdrum` prints basic messages informing you about
the result of the file matching and validity testing.

## Error reports

If desired, the contents of the `validateHumdrum` "error frame" can be
written into text files. This allows us to print the errors tagged right
alongside the original raw data. To write an error report, set the
`errorReport.path` argument to a non-`NULL` string, pointing to a
directory path on your machine. If the directory doesn't exist, `R` will
(attempt to) create it.

In the `errorReport.path` directory, the complete error report(s) for
all the files (same as in the returned "fileFrame", see above) are
written into a single file named `'humdrumR_syntaxErrorReport_DATE.txt'`
(with the `date` coming from
[Sys.Date](https://rdrr.io/r/base/Sys.time.html)`). In addition, a sub directory called `AnnotatedFiles`is created. In this directory, copies of all the files which contain errors are written, with`\_errorAnnotations\`
appended to their names. In each file, individual errors are directly
indicated in the record where they occur. The output looks like this:

                                                 | **whatever
                                                 | =
                                                 | The original records from the input file
                                                 | appear on the right side.
                                                 | =
    Error message for record six printed here.   | Exactly as they did
                                                 | in the input file.
                                                 | *-

## Examples

``` r
validateHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B.*.krn")
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/BeethovenVariations/B.*.krn' matches 12 text files in 1 directory.
#> Twelve files read from disk.
#> Validating twelve files...
#> all valid.

errorFrame <- validateHumdrum(humdrumRroot, "HumdrumData/InvalidFile.krn")
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> eight errors in one files...
#> zero valid files.
errorFrame
#>                                                                                                                              Filepath
#>                                                                                                                                <char>
#> 1: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 2: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 3: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 4: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 5: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 6: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 7: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#> 8: /private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/InvalidFile.krn
#>    Record                                                 Error
#>     <int>                                                <char>
#> 1:      4 ** and *- records in file are missing or don't add up
#> 2:     46        mixes data, comment, and data tokens (. . ! 1)
#> 3:     52               mixes barline and data tokens (= . = =)
#> 4:     10                                      drops one column
#> 5:     11                                       adds one column
#> 6:     81                                       adds one column
#> 7:     86                                      drops one column
#> 8:     89                                    drops four columns
```
