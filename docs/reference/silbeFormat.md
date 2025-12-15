# silbeFormat

Check that the formatting of the lyrics is correct, with -'s in the
right places (i.e., to denote the start or end of a syllable)

## Usage

``` r
silbeFormat(cVector)
```

## Arguments

- cVector:

  ***The data to be checked for improper formatting.***

  Must be `data.frame`.

  For now, please read in your spine as a dataframe with 1 column.

## Value

"Formatted properly." if the lyrics are formatted properly, else print
error message with corrections.

## Note

This function might detect multiple inconsistencies/errors in a given
value at a particular index, which could help the user determine the
exact issue(s) with their transcription.
