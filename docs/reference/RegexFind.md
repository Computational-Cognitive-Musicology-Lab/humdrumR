# Match strings against regular expression

The functions give you a concise way to search for regular expressions
in `character` vectors. They are "infix" functions, meaning you write
the function between its two arguments: `myvector %~% regex`.

## Usage

``` r
x %~l% regex

x %~i% regex

x %~n% regex

x %~m% regex

x %~% regex

... %!~% NA

... %!~l% NA

x %!~i% pattern
```

## Arguments

- x:

  ***A `character` vector to search in.***

  Must be `character`.

- regex:

  ***One or more regular expressions.***

  Must be `character`.

  If more than one regex is supplied, matches to *any* of the regexes
  are returned. (See "Multiple regexes" section.)

## Details

Each version of the function returns a different type of information
about regex matches (if any) in the input vector:

- `%~l%`: returns `logical` (`TRUE`/`FALSE`) indicating where in `x`
  there are matches.

- `%~i%`: returns `integer` indicating the indices of matches in `x`.

- `%~n%`: returns `integer` indicating the number (count) of matches in
  each string.

- `%~m%`: returns `character` string of the matched string itself.
  Returns `NA` where there is no match.

The basic function (`%~%`) is the same as `%~l%`. There is also a
negative versions of the `l` and `i` functions: giving all strings that
*don't* match the given regular expression. These are `%!~%`, `%!~l%`,
and `%!~i%`.

These functions are simply syntactic sugar for existing `R` regular
expression matching functions:

- `%~l%`: [`base::grepl()`](https://rdrr.io/r/base/grep.html)

- `%~i%`: [`base::grep()`](https://rdrr.io/r/base/grep.html)

- `%~n%`:
  [`stringi::stri_count_regex()`](https://rdrr.io/pkg/stringi/man/stri_count.html)

- `%~m%`:
  [`stringi::stri_extract_first_regex()`](https://rdrr.io/pkg/stringi/man/stri_extract.html)

## Multiple regexes

If more than one regex is supplied, `%~l%` and `%~i%` return the indices
where *any* of the regexes match. In the case of `%~n%`, each matching
regex is counted separately, and they are all summed. In the case of
`%~m%`, all matches (if any) are pasted together, including multiple
matches of the same string.
