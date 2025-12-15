# Making Regular Expressions

`humdrumR` includes some helpful functions for creating new regular
expressions which work with the
[stringr::stringr](https://stringr.tidyverse.org/reference/stringr-package.html)
package.

## Usage

``` r
captureRE(strs, n = "")

captureUniq(strs, zero = TRUE)

orRE(...)
```

## Details

`captureRE` will take a character vector and collapse it to a "capture
group." The `n` argument can be used to append a number tag, for
instance `'*'` (zero or more) to the group. I.e.,
`captureRE(c("a", "b", "c"), '*')` will output `"[abc]*"`.

`captureUniq` will make a similar capture group to `captureRE`, but with
an expression that makes sure that only 1 or more *of the same
character* repeats. For instance, `captureUniq(c('a', 'b','c'))` will
return `"([abc])\\1*"`—this expression will match `"aaa"` or `"bb"` but
not `"aabb"`.
