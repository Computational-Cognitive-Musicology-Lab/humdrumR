# "Unfold" data into multiple stops

If some record/spine/path locations have different numbers of stops in
different fields, this function spreads the data from the smaller fields
into multiple stops.

## Usage

``` r
unfoldStops(humdrumR, fromFields = fields(humdrumR, "D")$Name)
```

## See also

The opposite (kinda) of `foldStops()`
