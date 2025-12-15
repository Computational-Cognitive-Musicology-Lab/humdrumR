# Humdrum key signature

Humdrum key signature

## Usage

``` r
signature(x, ..., Key = NULL, parseArgs = list())
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
signature(c('I', 'ii', 'ii:dor', 'v', '-vi', 'V/V', 'ii/V'))
#> [1] "k[]"               "k[b-]"             "k[]"              
#> [4] "k[b-e-]"           "k[b-e-a-d-g-c-f-]" "k[f#]"            
#> [7] "k[b-]"            

signature(c('C:', 'c:', 'A-:', 'a-:', 'E:'))
#> [1] "k[]"               "k[b-e-a-]"         "k[b-e-a-d-]"      
#> [4] "k[b-e-a-d-g-c-f-]" "k[f#c#g#d#]"      
```
