# Expand numbers outwards from zero

Expand is a complement to the base `R` rounding functions, particularly
`trunc`.

## Usage

``` r
expand(x)
```

## Details

Each of the four base `R` functions—`round`, `ceiling`, `floor`, and
`trunc`—follow a different logic in how they round real numbers to
integers:

- `round`: round to *nearest* integer in either direction.

- `floor`: round downward *towards negative infinity*.

  - Negative numbers are rounded to "more negative" numbers.

- `ceiling`: round upward *towards infinity*.

  - Negative numbers are rounded to "less negative" numbers.

- `trunc`: round "inward" *towards* zero.

  - Negative numbers are rounded *up* to "less negative" numbers, but
    positive numbers are still rounded downwards to "less positive"
    numbers.

Just as `ceiling` compliments `floor`, the `humdrumR` function `expand`
acts as a compliment to `trunc`: `expand` rounds "outward" *away from
zero*. Negative numbers are rounded to "more negative" numbers and
positive numbers are rounded to "more positive" numbers.

A table explains better than words:

|                                    |                   |
|------------------------------------|-------------------|
| Call                               | Returns           |
| ` round(c(2.9, 3.1, -2.9, -3.1))`  | `c(3, 3, -3, -3)` |
| ` floor(c(2.9, 3.1, -2.9, -3.1))`  | `c(2, 3, -3, -4)` |
| `ceiling(c(2.9, 3.1, -2.9, -3.1))` | `c(3, 4, -2, -3)` |
| ` trunc(c(2.9, 3.1, -2.9, -3.1))`  | `c(2, 3, -2, -3)` |
| ` expand(c(2.9, 3.1, -2.9, -3.1))` | `c(3, 4, -3, -4)` |
