# Summarize humdrumR corpora

Summarizes the content of a [humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
by calling five different corpus summary functions and printing their
results.

## Usage

``` r
# S3 method for class 'humdrumR'
summary(object)
```

## Details

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a number of separate functions for summarizing different
aspects of [humdrumR data
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md):

- [`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)

  - Tabulates the raw size of the humdrumR corpus.

- [`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)

  - Tabulates reference records (metadata) for each piece.

- [`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)

  - Tabulates the number of spines and spine paths in pieces in the
    corpus.

- [`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)

  - Tabulates the types of exclusive and tandem interpretations in the
    corpus.

- `sections()`

  - Tabulates any formal data (`*>`) in the corpus, including barlines.

Each function takes a
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object and returns a data.table. The `summary` method for [humdrumR
objects](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
simply calls all of the above functions and prints a condensed version
of each.

## See also

Other corpus summary functions:
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md),
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
summary(chorales)
#>      Summary of humdrumR corpus "chorales":
#> Error in eval(rlang::expr((!!f)(!!quoted))): object 'chorales' not found
```
