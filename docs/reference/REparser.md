# Parse String Using Regular expressions

Takes an input string and parses it into a sequence of regular
expressions.

## Usage

``` r
REparser(
  res,
  parse.strict = TRUE,
  parse.exhaust = TRUE,
  parse.lead = FALSE,
  parse.rest = FALSE,
  toEnv = FALSE
)

REparse(
  str,
  res,
  parse.strict = TRUE,
  parse.exhaust = TRUE,
  parse.lead = FALSE,
  parse.rest = FALSE,
  reverse = FALSE,
  sep = NULL,
  toEnv = FALSE
)
```

## Details

If `exhaustive` is TRUE, the string must be exhaustively broken up by
the matching regular expressions. Superfluous (non-match) characters at
the begginning, end, or in bettween matches, will result in all `NA`
being returned.

——————————————-\> NEEDS DOCUMENTATION \<——————————————-
