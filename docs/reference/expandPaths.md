# Expand paths into new spines

This function takes a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
and "expands" the content of any spine paths by filling them in with the
content of their parent path(s).

## Usage

``` r
expandPaths(x, asSpines)
```

## Arguments

- asSpines:

  ***Should paths expanded into new spines?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the expanded paths are copied into their own new spines
  (shifting higher spines over as needed).

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Details

For example, imagine that in humdrum representation of a eight-measure
piano score, the annotator included an
[ossia](https://en.wikipedia.org/wiki/Ossia) passage in the seventh
measure. If we want to simply ignore the ossia passage, we can just
specify a
[`subset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
where `Path == 0`. If we want to study *only* the ossia passage, we can
grab a
[`subset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
where `Path == 1`. However, what if we want to study the ossia as it
would be performed, with the ossia measure swapped in for measure 7, but
still using measures 1-6 and 8 from the main path? `expandPaths()` will
help us do just this: `expandPaths()` will copy the contents of measure
1-6 and 8 into the second path and, if `asSpines = TRUE`, then copy the
path into it's own new spine. We can then treat that new "full"
path/spine just like any other path/spine.

## See also

Other Humdrum table reshaping functions:
[`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md),
[`collapseHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md),
[`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)
