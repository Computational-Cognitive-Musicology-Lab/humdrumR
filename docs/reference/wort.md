# Paste syllables together into words

Most humdrum datasets which include lyrics, include them in a `**silbe`
spine, representing each syllable from the lyrics on one so they line up
with notes in the music. Syllables from multi-syllabic words are
connected with `-` markers at the end of the first syllable, beginning
of the last syllable and both beginning and end of medial syllables. The
`wort()` command translates this syllable representation to words,
simply collapsing them together. The resulting word is aligned with the
first syllable of each word in `**silbe`.

If `wort()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first
[selectedFields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# S3 method for class 'character'
wort(
  x,
  ...,
  sep = "-",
  keep.sep = TRUE,
  number.syllables = FALSE,
  groupby = list(),
  Exclusive = NULL,
  multiDispatch = FALSE
)

humData |> select(Token) |> wort() 
humData |> wort(Token)

wort(
  x,
  ...,
  sep = "-",
  keep.sep = TRUE,
  number.syllables = FALSE,
  groupby = list(),
  Exclusive = NULL,
  multiDispatch = FALSE
)
```

## Arguments

- x:

  ***A vector of exclusive intepretations to control dispatch.***

  Defaults to `NULL`.

  Must be `NULL`, or a `character` vector which is either length `1` or
  `length(x)`.

- sep:

  **What separator is in input and/or output.**

  Defaults to `"-"`.

  Must be a single, non-empty `character` string.

- keep.sep:

  **Should syllable separators be kept in output?**

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- number.syllables:

  ***Should output show words with numbered syllables?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- groupby:

  **Optional vectors to group words within.**

  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list`; every element of the list must be length
  `length(x)`.

## Details

If a non-null `Exclusive` argument is provided, `wort()` will only apply
where `Exclusive == "silbe"`. When used in a
[`withinHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
call, `wort()` will by automally passed the `Exclusive` field from the
humdrum data, as well as `groupby = list(Piece, Spine, Path)`, so that
words are not collapsed across pieces/spines/paths. The output of
`wort()` is always the same length as the input. Any collapsed syllables
are replaced by the `**silbe` melisma marker, `"_"`. If
`number.syllables = TRUE`, the whole word is repeated for each syllable,
but with each numbered in square brackets: e.g.,
`c("yesterday[1], "yesterday[2]", "yesterday[3]", "yesterday[4]")`. This
format is seen a lot in computational linguistics.

By default, the syllable separators are retained in the collapsed
output: this makes it possible to recreate the syllables if necessary.
Any mid-word melismas (indicated by `"_"`) are kept collapsed as well,
for the same reason. However, if `keep.sep = FALSE`, seperators (and
mid-word melismas) are removed, making the function non invertible (you
can't easily get back to the syllables).

## Examples

``` r
wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'))
#>  [1] "now"          "let"          "me"           "wel-come"     "_"           
#>  [6] "ev-ery-bo-dy" "_"            "_"            "_"            "to"          
#> [11] "the"          "wild"         "wild"         "west"        

wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'), 
     keep.sep = FALSE)
#>  [1] "now"       "let"       "me"        "welcome"   "_"         "everybody"
#>  [7] "_"         "_"         "_"         "to"        "the"       "wild"     
#> [13] "wild"      "west"     

wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'),
     keep.sep = FALSE, number.syllables = TRUE)
#>  [1] "now"          "let"          "me"           "welcome[1]"   "welcome[2]"  
#>  [6] "everybody[1]" "everybody[2]" "everybody[3]" "everybody[4]" "to"          
#> [11] "the"          "wild"         "wild"         "west"        
```
