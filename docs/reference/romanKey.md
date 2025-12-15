# Roman numeral key areas

Roman numeral key areas

## Usage

``` r
romanKey(x, ..., Key = NULL, parseArgs = list())
```

## Arguments

- x:

  ***Input data, interpreted as diatonic keys.***

  Must be an `atomic` vector.

- Key:

  ***The key used by the parser, deparser, and transposer.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

- parseArgs:

  ***An optional list of arguments passed to the [key
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [key
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md).

## Examples

``` r
romanKey(c('C:', 'd:dor', 'G:', 'g:'))
#> [1] "I"     "iidor" "V"     "v"    
```
