# Summarize humdrum corpus interpretations.

`interpretations` is used to summarize the interpretations in the pieces
of a humdrumR corpus, including *exclusive* (`**`) and *tandem* (`*`)
interpretations. `interpretations` is one of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)'s
basic [corpus summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).

## Usage

``` r
interpretations(humdrumR)

# S3 method for class 'humInterpretations'
interps[i]

# S3 method for class 'humInterpretations'
print(interps, showEach = TRUE, screenWidth = options("width")$width - 10L)
```

## Arguments

- humdrumR:

  ***HumdrumR data to summarize.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- i:

  ***Index for rows.***

  If `numeric`, selects rows by index. If `character`, the string is
  matched as a regular expression against the filenames in the corpus.

## Details

`interpretations` returns a special `data.frame` called a
`humInterpretations` table. Each row in the table represents a single
piece in the corpus. The first column (`{X}`) is a variable indicating a
unique "exclusive pattern" associated with each piece—the exclusive
patterns are tallied at the bottom of the printout. The remaining
columns indicate how many of each interpretation (indicated by column
name) appear in each piece.

For tandem interpretations, counts are returned in the format
`Total.Unique.Spines`:

- `Total`: The total instances of this interpretation, across all
  spines.

- `Unique`: The number of unique versions of this interpretation.

- `Spines`: The number of spines that this interpretation appears in.

For example, consider the following piece:

    **kern   **kern   **silbe
       *C:      *C:         *
         c        e        La
         d        f        la
         e        g        la
       *e:      *e:         *
        f#       d#        la
         g        e         _
         a        b         _
       *G:      *G:         *
        f#        a       doo
         g        b       wop
        *-       *-        *-

In this piece, there is several tandem key interpretations, which
`humdrumR` will call `Key`. The tabulation by `interpretations` will
return a `Key` column with the value `6.3.2` for this piece:

- `6` because there are six key interpretations in total.

- `3` because there are three unique keys: `*C:`, `*e:` and `*G:`.

- `2` because the key interpretations only occur in two spines.

## See also

Other corpus summary functions:
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md),
[`humSummary`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
[`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md),
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)

## Examples

``` r
chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/*.krn")
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/BachChorales/*.krn' matches 10 text files in 1 directory.
#> Ten files read from disk.
#> Validating ten files...
#> all valid.
#> Parsing ten files...
#> Assembling corpus...
#> Done!
interpretations(chorales)
#> 
#> ###### Interpretation content in humdrumR corpus "chorales" (ten pieces):
#>                     {X}  **kern    BPM   Clef  Instrument    ***
#>                                     (Total.Unique.Spines)
#>   chor001.krn [ 1]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor002.krn [ 2]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor003.krn [ 3]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor004.krn [ 4]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor005.krn [ 5]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor006.krn [ 6]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor007.krn [ 7]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor008.krn [ 8]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor009.krn [ 9]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>   chor010.krn [10]  {A}       4  4.1.4  4.3.4       8.8.8    ***
#>                                     (Total.Unique.Spines)
#> ###### Totals:
#>                     {X}  **kern    BPM   Clef  Instrument    ***
#>              Hits:           10     10     10          10    ***
#> 
#>              (***five columns not displayed due to screensize***)
#>     Tallies:
#>               {A} =   **kern, **kern, **kern, **kern:  10
#> 
```
