# Summarize spines in humdrum dataset.

`spines` tabulates the spines and spine paths within the files in a
[humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
`spines` is one of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)'s
basic [corpus summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).

## Usage

``` r
spines(humdrumR, drop = FALSE)

# S3 method for class 'humSpines'
spineTable[i, drop = FALSE]

# S3 method for class 'humSpines'
print(spineTable, showEach = TRUE)
```

## Arguments

- humdrumR:

  ***HumdrumR data to summarize.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- drop:

  ***Whether to return normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) or a
  `humSpines` table.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `drop = TRUE`, a normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) is
  returned instead of a `humSpines` table.

- i:

  ***Index for rows.***

  If `numeric`, selects rows by index. If `character`, the string is
  matched as a regular expression against filenames in the corpus.

## Details

`spines` returns a special `data.frame` called a `humSpines` table. A
`humSpines` table has five columns of information about each piece:

1.  Spines

    - The number of spines.

2.  Paths

    - The total number of spine paths.

3.  In

    - The number of spines which contain any spine paths.

4.  \*^

    - The total number of spine splits (`"*^"`).

5.  \*v

    - The total number of spine joins (`"*v"`).

When `humSpine` table prints on the command line, "tallies" of the
unique combinations of spines and paths in the files are also printed.

## See also

Other corpus summary functions:
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md),
[`humSummary`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md),
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)

## Examples

``` r
chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/*.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpGCgWwF/temp_libpath2869ab26c00287/humdrumR/HumdrumData/BachChorales/*.krn' matches 10 text files in 1 directory.
#> Ten files read from disk.
#> Validating ten files...
#> all valid.
#> Parsing ten files...
#> Assembling corpus...
#> Done!
spines(chorales)
#> 
#> ###### Spine structure in humdrumR corpus "chorales" (ten piecess):
#>                     Spines  + Paths  In  *^  *v
#> ###############################################
#>   chor001.krn [ 1]       4        0            
#>   chor002.krn [ 2]       4        0            
#>   chor003.krn [ 3]       4        0            
#>   chor004.krn [ 4]       4        0            
#>   chor005.krn [ 5]       4        0            
#>   chor006.krn [ 6]       4        0            
#>   chor007.krn [ 7]       4        0            
#>   chor008.krn [ 8]       4        0            
#>   chor009.krn [ 9]       4        0            
#>   chor010.krn [10]       4        0            
#> ###############################################
#>                     Spines  + Paths  In  *^  *v
#> 
#>                   Tallies:
#>                         10 files with 4 spines
#> 
```
