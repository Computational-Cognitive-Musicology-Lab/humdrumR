# Extract field(s) from [humdrumR data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)

Individual fields from the [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md) can
be extracted using `pull()`. Multiple fields can be extracted using
`pull_data.frame()`, `pull_data.table`, or `pull_tibble()` —the
resulting data.frames are a column-subset of the humdrum table. You can
also use the `$` operator to extract a single field, just like `pull()`.

## Usage

``` r
pull_data.table(humdrumR, ..., dataTypes = "D", null = "charNA2dot")

pull_data.frame(humdrumR, ..., dataTypes = "D", null = "charNA2dot")

pull_tibble(humdrumR, ..., dataTypes = "D", null = "charNA2dot")

# S3 method for class 'humdrumR'
pull(.data, var, dataTypes = "D", null = "asis")

# S4 method for class 'humdrumR'
x$name
```

## Arguments

- humdrumR, .data, x:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- ...:

  ***Which fields to output.***

  If no arguments are provided, the object's [selected
  fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  are pulled.

  These arguments can be any combination of `character` strings,
  numbers, or symbols used to match fields in the `humdrumR` input using
  [tidyverse](https://dplyr.tidyverse.org/reference/select.html)
  semantics.

  Unlike in tidyverse
  [`select()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md),
  field names can be [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md).
  You can also include `character` strings [partially
  matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`
  or `"Grouping"`, which will select all fields of those types (see
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  for further explanation).

- dataTypes:

  ***Which types of humdrum record(s) to include.***

  Only non-null data tokens (`"D"`) are returned by default.

  Must be a single `character` string. Legal values are
  `'G', 'L', 'I', 'M', 'D', 'd'` or any combination of these (e.g.,
  `"LIM"`). (See the [humdrum
  table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  documentation for explanation.)

- null:

  ***How should null data points be output?***

  Default is `"charNA2dot"`.

  Must be a single character string, [partially
  matching](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  `"NA2dot"`, `"dot2NA"`, `'charNA2dot"`, or `"asis"`.

- var:

  ***Which field to output.***

  Defaults to `selectedFields(humdrumR)[1]`.

  Must be either a single `character` string or `symbol` which
  [partially
  matches](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  a field name, or a single whole-number, which selects the field by the
  row index of the
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  output. If a negative number is provided, nth-to-last index is
  used—for example, `-1` would grab the last field.

## Details

The functions `pull()`, `pull.data.xxx()`, `pull.tibble()`, and `$` are
the "escape hatch" to pull your data out of the [humdrumR data
world](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
into "normal" R. Use the `pull()` function or the `$` to access the
actual vector content of a single field. The other functions *always*
return a `data.frame`/`data.table`/`tibble`, even if it has only one
column.

Choose which field(s) to return using the `...`, `var`, or `name`
arguments. The `var` and `...` options use tidyverse style select
semantics (see
[select()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)).
If no fields are indicated, the data's [selected
fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
are pulled; in the case of `pull()` and `$`, only the *first* selected
field is pulled.

The `dataTypes` argument controls which *types* of data are pulled—by
default, only non-null data (`Type == "D"`) is pulled. The `$` operator
can only grab non-null data.

The `null` argument controls how null data is returned, with four
options:

- `"NA2dot"` means all `NA` values are converted to `"."`; note that
  this will cause all output to be coerced to `character`.

- `"dot2NA"` means all `"."` are converted to `NA`.

- `"charNA2dot"` means `NA` values in `character` vectors are converted
  to `"."`, but not in other atomic types.

- `"asis"` means either `NA` or `"."` values may print, depending on
  what is in the field.

Note that `pull_tibble()` won't work if you don't independently load the
`tibble` (or `tidyverse`) package— i.e., call
[`library(tibble)`](https://tibble.tidyverse.org/).

## See also

To know what fields are available to pull, use
[`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
To know what fields are selected—the default fields to pull—use
[`selectedFields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpL03I08/temp_libpath1dfb3eb89ac20/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

humData |> pull(Token)
#>   [1] "4GG"    "4G"     "4E"     "4F#"    "4G"     "4D"     "4E"     "4C"    
#>   [9] "8BBL"   "8AAJ"   "4GG"    "2D;"    "4GG"    "4FF#"   "4GG"    "4AA"   
#>  [17] "4BB"    "4C"     "4D"     "2GG;"   "4GG"    "4GG"    "4AA"    "4BB"   
#>  [25] "4.BB"   "8AA"    "4GG"    "2D;"    "[4E"    "4E]"    "4D"     "4C"    
#>  [33] "4.BB"   "8C"     "4D"     "8GGL"   "8AAJ"   "4BB"    "4GG"    "2C;"   
#>  [41] "4GG"    "4FF#"   "4GG"    "4AA"    "4BB"    "4GG"    "4D"     "8EL"   
#>  [49] "8D"     "8C"     "8BB"    "8AA"    "8GGJ"   "2D;"    "[4G"    "4G]"   
#>  [57] "4F#"    "[4E"    "8EL]"   "8DJ"    "4C"     "4D"     "2.GG;"  "4B"    
#>  [65] "4B"     "8cL"    "8BJ"    "4A"     "4G"     "4F#"    "4G"     "8cL"   
#>  [73] "8BJ"    "4c"     "4d"     "2d;"    "4d"     "4A"     "4B"     "4c"    
#>  [81] "4d"     "4e"     "8dL"    "8cJ"    "2B;"    "4d"     "4d"     "4c"    
#>  [89] "8BL"    "8AJ"    "8BL"    "8cJ"    "4d"     "4d"     "2d;"    "4B"    
#>  [97] "4G"     "4B"     "4e"     "2d"     "4d"     "2.d"    "2c;"    "4d"    
#> [105] "8dL"    "8cJ"    "4B"     "4c"     "2d"     "8dL"    "8cJ"    "4B"    
#> [113] "4c"     "4d"     "2d;"    "4d"     "2d"     "4e"     "2e"     "8dL"   
#> [121] "8cJ"    "2.B;"   "4d"     "4d"     "4e"     "4d"     "2d"     "4B"    
#> [129] "8eL"    "8d"     "8e"     "8f#J"   "4g"     "2f#;"   "4g"     "4d"    
#> [137] "4e"     "4f#"    "2g"     "4f#"    "2d;"    "[4g"    "8gL]"   "8f#J"  
#> [145] "8eL"    "8f#J"   "[4g"    "8gL]"   "8aJ"    "8gL"    "8f#J"   "4g"    
#> [153] "2f#;"   "4e"     "4e"     "8f#L"   "8gJ"    "4a"     "4a"     "4.g"   
#> [161] "8f#"    "2g"     "4f"     "2e;"    "4g"     "4.a"    "8g"     "4f#"   
#> [169] "2g"     "[4f#"   "8f#L]"  "8eJ"    "8eL"    "8f#J"   "4g"     "2f#;"  
#> [177] "4g"     "2a"     "8gL"    "8f#J"   "2g"     "4f#"    "2.d;"   "4g"    
#> [185] "2g"     "4dd"    "4.b"    "8a"     "4g"     "4.g"    "8a"     "4b"    
#> [193] "2a;"    "4b"     "2dd"    "4cc"    "4b"     "2a"     "2g;"    "4b"    
#> [201] "4b"     "4cc"    "4dd"    "4.dd"   "8cc"    "4b"     "2a;"    "4g"    
#> [209] "2b"     "4cc"    "2dd"    "4cc"    "2.b"    "2g;"    "4b"     "2dd"   
#> [217] "4cc"    "2b"     "4a"     "4.g"    "8a"     "4b"     "2a;"    "4b"    
#> [225] "2dd"    "4cc"    "4b"     "2a"     "2.g;"   "8AL"    "8G#J"   "4F#"   
#> [233] "4C#"    "4D"     "4D#"    "4E"     "4BB"    "4EE;"   "4E"     "4A"    
#> [241] "4B"     "4c#"    "8BL"    "8AJ"    "4B"     "4BB"    "4E;"    "4C#"   
#> [249] "8F#L"   "8G#J"   "4A"     "4E"     "8C#L"   "8AAJ"   "4D"     "8C#L"  
#> [257] "8DJ"    "4E;"    "4C#"    "4BB"    "4C#"    "8DL"    "8GJ"    "4F#"   
#> [265] "2.BB;"  "4C#"    "4F#"    "8EL"    "8DJ"    "4C#"    "4BB"    "8AAL"  
#> [273] "8BB"    "8C#"    "8DJ"    "4E;"    "4BB"    "8F#L"   "8G#J"   "4A"    
#> [281] "8G#L"   "8EJ"    "[4A"    "8AL]"   "8G#J"   "4F#"    "8EL"    "8DJ"   
#> [289] "4E"     "2.AA;"  "4c#"    "4c#"    "8c#L"   "8BJ"    "8AL"    "8G#J"  
#> [297] "4F#"    "4.B"    "8A"     "4G#X;"  "4e"     "4e"     "4d#"    "2c#"   
#> [305] "4.B"    "8A"     "4G#;"   "4G#"    "8AL"    "8BJ"    "8c#L"   "8dJ"   
#> [313] "4e"     "4e"     "4d"     "4e"     "4e;"    "4e"     "8f#"    "4B"    
#> [321] "8A#"    "4B"     "4c#"    "2.d;"   "4G#"    "4A"     "8G#L"   "8F#J"  
#> [329] "8EL"    "8eJ"    "4d"     "8c#L"   "8d"     "8e"     "8f#J"   "4g#;"  
#> [337] "4f#"    "8f#L"   "8eJ"    "8dL"    "8c#J"   "4B"     "8c#L"   "8dJ"   
#> [345] "4.e"    "8d"     "8c#L"   "8f#J"   "8BL"    "16eL"   "16dJJ"  "2.c#;" 
#> [353] "4e"     "4f#"    "4e"     "4f#"    "4f#"    "4e"     "4d#"    "4B;"   
#> [361] "4g#"    "4a"     "8g#L"   "8f#J"   "[2e"    "4e]"    "4d#"    "4B;"   
#> [369] "8cc#L"  "8bJ"    "4a"     "4a"     "4g#"    "8aL"    "8gJ"    "8f#L"  
#> [377] "8g#XJ"  "4a"     "4g#;"   "4a#"    "8bL"    "8aJ"    "4g"     "8f#L"  
#> [385] "8eJ"    "4f#"    "2.f#;"  "8eL"    "8dJ"    "4c#"    "4d"     "4e"    
#> [393] "8f#L"   "8g#J"   "2a"     "4e;"    "4b"     "8aL"    "8g#J"   "4f#"   
#> [401] "4e"     "8f#L"   "8g#J"   "2a"     "4a"     "4g#"    "2.e;"   "4a"    
#> [409] "4a"     "4a"     "4a"     "4b"     "4g"     "4f#"    "4e;"    "4b"    
#> [417] "4cc#"   "4b"     "4a"     "8g#L"   "8f#J"   "4g#"    "4f#"    "4e;"   
#> [425] "4ee"    "4dd"    "4cc#"   "4b"     "4a"     "8aL"    "8bJ"    "4cc#"  
#> [433] "4b;"    "4cc#"   "4dd"    "4cc#"   "4b"     "4a#"    "2.b;"   "4e"    
#> [441] "4a"     "4b"     "4cc#"   "4dd"    "4ee"    "8ddL"   "8cc#J"  "4b;"   
#> [449] "4dd"    "4cc#"   "4b"     "4.ee"   "8dd"    "8cc#L"  "8b"     "8a"    
#> [457] "8bJ"    "4cc#"   "4b"     "2.a;"   "4E"     "4A"     "4B"     "4c"    
#> [465] "8BL"    "8AJ"    "4G#"    "4A"     "4E;"    "4BB"    "8CL"    "8DJ"   
#> [473] "4E"     "4F"     "8EL"    "8DJ"    "2E"     "4AA;"   "4F#"    "8GL"   
#> [481] "8F#J"   "4E"     "8BL"    "8AJ"    "8GL"    "8F#J"   "8EL"    "8DJ"   
#> [489] "4C"     "4BB;"   "4E"     "4F"     "8CL"    "8DJ"    "4E"     "8AAL"  
#> [497] "8BBJ"   "8CL"    "8DJ"    "4E"     "4AA;"   "4A"     "4G#"    "8AL"   
#> [505] "8GJ"    "8FL"    "8EJ"    "8DL"    "8C#J"   "4D"     "4D#"    "4E;"   
#> [513] "4e"     "4e"     "4d"     "4e"     "8dL"    "8cJ"    "4B"     "8cL"   
#> [521] "8dJ"    "4e;"    "4f"     "4e"     "8eL"    "8dJ"    "4c"     "4d"    
#> [529] "8G#"    "4A"     "8G#"    "4c;"    "4A"     "8GL"    "8AJ"    "4B"    
#> [537] "4B"     "8BL"    "8AJ"    "4B"     "4c"     "4F#;"   "8eL"    "8dnJ"  
#> [545] "8cL"    "8dJ"    "4e"     "4e"     "8eL"    "8dJ"    "4c"     "4B"    
#> [553] "4c;"    "4c"     "4B"     "4A"     "4A"     "4B-"    "8AL"    "8EJ"   
#> [561] "4F#X"   "4G#;"   "4g#"    "4a"     "4g#"    "4a"     "8g#L"   "8aJ"   
#> [569] "4b"     "8eL"    "8f#J"   "4g#;"   "4g#"    "4a"     "4g#"    "8aL"   
#> [577] "8gJ"    "4f"     "2e"     "4e;"    "4d"     "8dL"    "8d#J"   "4e"    
#> [585] "4d#"    "8eL"    "8d#J"   "8eL"    "8gJ"    "8f#L"   "8eJ"    "4d#;"  
#> [593] "4B"     "4A"     "4a"     "4g#"    "4a"     "4e"     "4e"     "4e;"   
#> [601] "4e"     "4e"     "4e"     "4f"     "4g"     "8f#XL"  "8g#J"   "4a"    
#> [609] "4e;"    "4b"     "4cc"    "4b"     "4a"     "4ee"    "8eeL"   "8ddJ"  
#> [617] "4cc"    "4b;"    "4dd"    "4cc"    "4b"     "4a"     "8bL"    "16ccL" 
#> [625] "16ddJJ" "4cc"    "4b"     "4a;"    "4a"     "8bL"    "8aJ"    "4g"    
#> [633] "4f#"    "8eL"    "8f#J"   "4g"     "4a"     "4b;"    "4g"     "8aL"   
#> [641] "8bJ"    "4cc"    "4b"     "8ccL"   "8bJ"    "4a"     "4g#"    "4a;"   
#> [649] "4a"     "4ee"    "4cc"    "4dd"    "4ee"    "4dd"    "4cc"    "4b;"   
#> [657] "4E"     "4D#"    "4BB"    "4E"     "8F#L"   "8G#J"   "4A"     "4E"    
#> [665] "4AA;"   "4D#"    "8EL"    "8F#J"   "4G#"    "4C#"    "8F#L"   "8EJ"   
#> [673] "8D#L"   "16C#L"  "16BBJJ" "4F#"    "4BB;"   "4E"     "4C#"    "8D#L"  
#> [681] "8EJ"    "4F#"    "4B"     "8G#L"   "8EJ"    "4F#"    "4BB;"   "4E"    
#> [689] "4G#"    "4E"     "8AnXL"  "8BJ"    "4c#"    "4E#"    "4F#"    "4C#;"  
#> [697] "4E"     "4BB"    "8C#L"   "8D#J"   "4E"     "4BB"    "4AA#"   "4BB"   
#> [705] "4EE;"   "4e"     "4f#"    "8eL"    "8d#J"   "4e"     "8AL"    "8BJ"   
#> [713] "16c#LL" "16dJJ"  "4e"     "8d"     "4c#;"   "4B"     "4.B"    "8A"    
#> [721] "4G#"    "4F#"    "8F#"    "4B"     "8A#"    "4F#;"   "4G#"    "4c#"   
#> [729] "4F#"    "4f#"    "4f#"    "4B"     "4A#"    "4d#;"   "4e"     "4B"    
#> [737] "4e"     "4e"     "4c#"    "4d"     "4c#"    "4c#;"   "[4B"    "4B]"   
#> [745] "4e"     "4e"     "8d#L"   "8BJ"    "2F#"    "4G#;"   "4g#"    "8f#L"  
#> [753] "8g#J"   "4a"     "4g#"    "4f#"    "8e"     "4a"     "8g#"    "4e;"   
#> [761] "4f#"    "4e"     "8eL"    "8d#J"   "4e"     "4c#"    "8f#L"   "16eL"  
#> [769] "16d#JJ" "4e"     "4d#;"   "8eL"    "8f#J"   "8g#L"   "8a#J"   "4b"    
#> [777] "4a#"    "4b"     "4b"     "4f#"    "4f#;"   "4g#"    "8eL"    "8f#J"  
#> [785] "4g#"    "4a"     "8eL"    "8f#J"   "4g#"    "4f#"    "4e#;"   "4e"    
#> [793] "4d#"    "4c#"    "4B"     "4B"     "4c#"    "4B"     "4B;"    "4b"    
#> [801] "4b"     "4b"     "4b"     "4dd"    "4cc#"   "4b"     "4a;"    "4b"    
#> [809] "4g#"    "8eL"    "8f#J"   "4g#"    "4a#"    "4b"     "4cc#"   "4b;"   
#> [817] "4b"     "4ee"    "4dd#"   "4cc#"   "4dd#"   "8eeL"   "8dd#J"  "4cc#"  
#> [825] "4b;"    "4b"     "4ee"    "4b"     "4cc#"   "8g#L"   "8aJ"    "4b"    
#> [833] "4a"     "4g#;"   "4g#"    "4f#"    "4a"     "4g#"    "4f#"    "4c#"   
#> [841] "4d#"    "4e;"   
humData$Token
#>   [1] "4GG"    "4G"     "4E"     "4F#"    "4G"     "4D"     "4E"     "4C"    
#>   [9] "8BBL"   "8AAJ"   "4GG"    "2D;"    "4GG"    "4FF#"   "4GG"    "4AA"   
#>  [17] "4BB"    "4C"     "4D"     "2GG;"   "4GG"    "4GG"    "4AA"    "4BB"   
#>  [25] "4.BB"   "8AA"    "4GG"    "2D;"    "[4E"    "4E]"    "4D"     "4C"    
#>  [33] "4.BB"   "8C"     "4D"     "8GGL"   "8AAJ"   "4BB"    "4GG"    "2C;"   
#>  [41] "4GG"    "4FF#"   "4GG"    "4AA"    "4BB"    "4GG"    "4D"     "8EL"   
#>  [49] "8D"     "8C"     "8BB"    "8AA"    "8GGJ"   "2D;"    "[4G"    "4G]"   
#>  [57] "4F#"    "[4E"    "8EL]"   "8DJ"    "4C"     "4D"     "2.GG;"  "4B"    
#>  [65] "4B"     "8cL"    "8BJ"    "4A"     "4G"     "4F#"    "4G"     "8cL"   
#>  [73] "8BJ"    "4c"     "4d"     "2d;"    "4d"     "4A"     "4B"     "4c"    
#>  [81] "4d"     "4e"     "8dL"    "8cJ"    "2B;"    "4d"     "4d"     "4c"    
#>  [89] "8BL"    "8AJ"    "8BL"    "8cJ"    "4d"     "4d"     "2d;"    "4B"    
#>  [97] "4G"     "4B"     "4e"     "2d"     "4d"     "2.d"    "2c;"    "4d"    
#> [105] "8dL"    "8cJ"    "4B"     "4c"     "2d"     "8dL"    "8cJ"    "4B"    
#> [113] "4c"     "4d"     "2d;"    "4d"     "2d"     "4e"     "2e"     "8dL"   
#> [121] "8cJ"    "2.B;"   "4d"     "4d"     "4e"     "4d"     "2d"     "4B"    
#> [129] "8eL"    "8d"     "8e"     "8f#J"   "4g"     "2f#;"   "4g"     "4d"    
#> [137] "4e"     "4f#"    "2g"     "4f#"    "2d;"    "[4g"    "8gL]"   "8f#J"  
#> [145] "8eL"    "8f#J"   "[4g"    "8gL]"   "8aJ"    "8gL"    "8f#J"   "4g"    
#> [153] "2f#;"   "4e"     "4e"     "8f#L"   "8gJ"    "4a"     "4a"     "4.g"   
#> [161] "8f#"    "2g"     "4f"     "2e;"    "4g"     "4.a"    "8g"     "4f#"   
#> [169] "2g"     "[4f#"   "8f#L]"  "8eJ"    "8eL"    "8f#J"   "4g"     "2f#;"  
#> [177] "4g"     "2a"     "8gL"    "8f#J"   "2g"     "4f#"    "2.d;"   "4g"    
#> [185] "2g"     "4dd"    "4.b"    "8a"     "4g"     "4.g"    "8a"     "4b"    
#> [193] "2a;"    "4b"     "2dd"    "4cc"    "4b"     "2a"     "2g;"    "4b"    
#> [201] "4b"     "4cc"    "4dd"    "4.dd"   "8cc"    "4b"     "2a;"    "4g"    
#> [209] "2b"     "4cc"    "2dd"    "4cc"    "2.b"    "2g;"    "4b"     "2dd"   
#> [217] "4cc"    "2b"     "4a"     "4.g"    "8a"     "4b"     "2a;"    "4b"    
#> [225] "2dd"    "4cc"    "4b"     "2a"     "2.g;"   "8AL"    "8G#J"   "4F#"   
#> [233] "4C#"    "4D"     "4D#"    "4E"     "4BB"    "4EE;"   "4E"     "4A"    
#> [241] "4B"     "4c#"    "8BL"    "8AJ"    "4B"     "4BB"    "4E;"    "4C#"   
#> [249] "8F#L"   "8G#J"   "4A"     "4E"     "8C#L"   "8AAJ"   "4D"     "8C#L"  
#> [257] "8DJ"    "4E;"    "4C#"    "4BB"    "4C#"    "8DL"    "8GJ"    "4F#"   
#> [265] "2.BB;"  "4C#"    "4F#"    "8EL"    "8DJ"    "4C#"    "4BB"    "8AAL"  
#> [273] "8BB"    "8C#"    "8DJ"    "4E;"    "4BB"    "8F#L"   "8G#J"   "4A"    
#> [281] "8G#L"   "8EJ"    "[4A"    "8AL]"   "8G#J"   "4F#"    "8EL"    "8DJ"   
#> [289] "4E"     "2.AA;"  "4c#"    "4c#"    "8c#L"   "8BJ"    "8AL"    "8G#J"  
#> [297] "4F#"    "4.B"    "8A"     "4G#X;"  "4e"     "4e"     "4d#"    "2c#"   
#> [305] "4.B"    "8A"     "4G#;"   "4G#"    "8AL"    "8BJ"    "8c#L"   "8dJ"   
#> [313] "4e"     "4e"     "4d"     "4e"     "4e;"    "4e"     "8f#"    "4B"    
#> [321] "8A#"    "4B"     "4c#"    "2.d;"   "4G#"    "4A"     "8G#L"   "8F#J"  
#> [329] "8EL"    "8eJ"    "4d"     "8c#L"   "8d"     "8e"     "8f#J"   "4g#;"  
#> [337] "4f#"    "8f#L"   "8eJ"    "8dL"    "8c#J"   "4B"     "8c#L"   "8dJ"   
#> [345] "4.e"    "8d"     "8c#L"   "8f#J"   "8BL"    "16eL"   "16dJJ"  "2.c#;" 
#> [353] "4e"     "4f#"    "4e"     "4f#"    "4f#"    "4e"     "4d#"    "4B;"   
#> [361] "4g#"    "4a"     "8g#L"   "8f#J"   "[2e"    "4e]"    "4d#"    "4B;"   
#> [369] "8cc#L"  "8bJ"    "4a"     "4a"     "4g#"    "8aL"    "8gJ"    "8f#L"  
#> [377] "8g#XJ"  "4a"     "4g#;"   "4a#"    "8bL"    "8aJ"    "4g"     "8f#L"  
#> [385] "8eJ"    "4f#"    "2.f#;"  "8eL"    "8dJ"    "4c#"    "4d"     "4e"    
#> [393] "8f#L"   "8g#J"   "2a"     "4e;"    "4b"     "8aL"    "8g#J"   "4f#"   
#> [401] "4e"     "8f#L"   "8g#J"   "2a"     "4a"     "4g#"    "2.e;"   "4a"    
#> [409] "4a"     "4a"     "4a"     "4b"     "4g"     "4f#"    "4e;"    "4b"    
#> [417] "4cc#"   "4b"     "4a"     "8g#L"   "8f#J"   "4g#"    "4f#"    "4e;"   
#> [425] "4ee"    "4dd"    "4cc#"   "4b"     "4a"     "8aL"    "8bJ"    "4cc#"  
#> [433] "4b;"    "4cc#"   "4dd"    "4cc#"   "4b"     "4a#"    "2.b;"   "4e"    
#> [441] "4a"     "4b"     "4cc#"   "4dd"    "4ee"    "8ddL"   "8cc#J"  "4b;"   
#> [449] "4dd"    "4cc#"   "4b"     "4.ee"   "8dd"    "8cc#L"  "8b"     "8a"    
#> [457] "8bJ"    "4cc#"   "4b"     "2.a;"   "4E"     "4A"     "4B"     "4c"    
#> [465] "8BL"    "8AJ"    "4G#"    "4A"     "4E;"    "4BB"    "8CL"    "8DJ"   
#> [473] "4E"     "4F"     "8EL"    "8DJ"    "2E"     "4AA;"   "4F#"    "8GL"   
#> [481] "8F#J"   "4E"     "8BL"    "8AJ"    "8GL"    "8F#J"   "8EL"    "8DJ"   
#> [489] "4C"     "4BB;"   "4E"     "4F"     "8CL"    "8DJ"    "4E"     "8AAL"  
#> [497] "8BBJ"   "8CL"    "8DJ"    "4E"     "4AA;"   "4A"     "4G#"    "8AL"   
#> [505] "8GJ"    "8FL"    "8EJ"    "8DL"    "8C#J"   "4D"     "4D#"    "4E;"   
#> [513] "4e"     "4e"     "4d"     "4e"     "8dL"    "8cJ"    "4B"     "8cL"   
#> [521] "8dJ"    "4e;"    "4f"     "4e"     "8eL"    "8dJ"    "4c"     "4d"    
#> [529] "8G#"    "4A"     "8G#"    "4c;"    "4A"     "8GL"    "8AJ"    "4B"    
#> [537] "4B"     "8BL"    "8AJ"    "4B"     "4c"     "4F#;"   "8eL"    "8dnJ"  
#> [545] "8cL"    "8dJ"    "4e"     "4e"     "8eL"    "8dJ"    "4c"     "4B"    
#> [553] "4c;"    "4c"     "4B"     "4A"     "4A"     "4B-"    "8AL"    "8EJ"   
#> [561] "4F#X"   "4G#;"   "4g#"    "4a"     "4g#"    "4a"     "8g#L"   "8aJ"   
#> [569] "4b"     "8eL"    "8f#J"   "4g#;"   "4g#"    "4a"     "4g#"    "8aL"   
#> [577] "8gJ"    "4f"     "2e"     "4e;"    "4d"     "8dL"    "8d#J"   "4e"    
#> [585] "4d#"    "8eL"    "8d#J"   "8eL"    "8gJ"    "8f#L"   "8eJ"    "4d#;"  
#> [593] "4B"     "4A"     "4a"     "4g#"    "4a"     "4e"     "4e"     "4e;"   
#> [601] "4e"     "4e"     "4e"     "4f"     "4g"     "8f#XL"  "8g#J"   "4a"    
#> [609] "4e;"    "4b"     "4cc"    "4b"     "4a"     "4ee"    "8eeL"   "8ddJ"  
#> [617] "4cc"    "4b;"    "4dd"    "4cc"    "4b"     "4a"     "8bL"    "16ccL" 
#> [625] "16ddJJ" "4cc"    "4b"     "4a;"    "4a"     "8bL"    "8aJ"    "4g"    
#> [633] "4f#"    "8eL"    "8f#J"   "4g"     "4a"     "4b;"    "4g"     "8aL"   
#> [641] "8bJ"    "4cc"    "4b"     "8ccL"   "8bJ"    "4a"     "4g#"    "4a;"   
#> [649] "4a"     "4ee"    "4cc"    "4dd"    "4ee"    "4dd"    "4cc"    "4b;"   
#> [657] "4E"     "4D#"    "4BB"    "4E"     "8F#L"   "8G#J"   "4A"     "4E"    
#> [665] "4AA;"   "4D#"    "8EL"    "8F#J"   "4G#"    "4C#"    "8F#L"   "8EJ"   
#> [673] "8D#L"   "16C#L"  "16BBJJ" "4F#"    "4BB;"   "4E"     "4C#"    "8D#L"  
#> [681] "8EJ"    "4F#"    "4B"     "8G#L"   "8EJ"    "4F#"    "4BB;"   "4E"    
#> [689] "4G#"    "4E"     "8AnXL"  "8BJ"    "4c#"    "4E#"    "4F#"    "4C#;"  
#> [697] "4E"     "4BB"    "8C#L"   "8D#J"   "4E"     "4BB"    "4AA#"   "4BB"   
#> [705] "4EE;"   "4e"     "4f#"    "8eL"    "8d#J"   "4e"     "8AL"    "8BJ"   
#> [713] "16c#LL" "16dJJ"  "4e"     "8d"     "4c#;"   "4B"     "4.B"    "8A"    
#> [721] "4G#"    "4F#"    "8F#"    "4B"     "8A#"    "4F#;"   "4G#"    "4c#"   
#> [729] "4F#"    "4f#"    "4f#"    "4B"     "4A#"    "4d#;"   "4e"     "4B"    
#> [737] "4e"     "4e"     "4c#"    "4d"     "4c#"    "4c#;"   "[4B"    "4B]"   
#> [745] "4e"     "4e"     "8d#L"   "8BJ"    "2F#"    "4G#;"   "4g#"    "8f#L"  
#> [753] "8g#J"   "4a"     "4g#"    "4f#"    "8e"     "4a"     "8g#"    "4e;"   
#> [761] "4f#"    "4e"     "8eL"    "8d#J"   "4e"     "4c#"    "8f#L"   "16eL"  
#> [769] "16d#JJ" "4e"     "4d#;"   "8eL"    "8f#J"   "8g#L"   "8a#J"   "4b"    
#> [777] "4a#"    "4b"     "4b"     "4f#"    "4f#;"   "4g#"    "8eL"    "8f#J"  
#> [785] "4g#"    "4a"     "8eL"    "8f#J"   "4g#"    "4f#"    "4e#;"   "4e"    
#> [793] "4d#"    "4c#"    "4B"     "4B"     "4c#"    "4B"     "4B;"    "4b"    
#> [801] "4b"     "4b"     "4b"     "4dd"    "4cc#"   "4b"     "4a;"    "4b"    
#> [809] "4g#"    "8eL"    "8f#J"   "4g#"    "4a#"    "4b"     "4cc#"   "4b;"   
#> [817] "4b"     "4ee"    "4dd#"   "4cc#"   "4dd#"   "8eeL"   "8dd#J"  "4cc#"  
#> [825] "4b;"    "4b"     "4ee"    "4b"     "4cc#"   "8g#L"   "8aJ"    "4b"    
#> [833] "4a"     "4g#;"   "4g#"    "4f#"    "4a"     "4g#"    "4f#"    "4c#"   
#> [841] "4d#"    "4e;"   

humData |> pull_data.table(Token, Spine)
#>       Token Spine
#>      <char> <int>
#>   1:    4GG     1
#>   2:     4G     1
#>   3:     4E     1
#>   4:    4F#     1
#>   5:     4G     1
#>  ---             
#> 838:    4g#     4
#> 839:    4f#     4
#> 840:    4c#     4
#> 841:    4d#     4
#> 842:    4e;     4
humData |> pull_tibble(everything())
#> # A tibble: 842 × 42
#>    Token   Bar BarLabel DoubleBar Formal BPM   Clef   Exclusive Instrument
#>    <chr> <int> <chr>        <int> <chr>  <chr> <chr>  <chr>     <chr>     
#>  1 4GG       0 .                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  2 4G        1 1                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  3 4E        1 1                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  4 4F#       1 1                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  5 4G        2 2                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  6 4D        2 2                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  7 4E        2 2                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  8 4C        3 3                0 A      MM100 clefF4 kern      "I\"Bass" 
#>  9 8BBL      3 3                0 A      MM100 clefF4 kern      "I\"Bass" 
#> 10 8AAJ      3 3                0 A      MM100 clefF4 kern      "I\"Bass" 
#> # ℹ 832 more rows
#> # ℹ 33 more variables: InstrumentClass <chr>, Key <chr>, KeySignature <chr>,
#> #   Mensuration <chr>, Tandem <chr>, TimeSignature <chr>, AGN <chr>, CDT <chr>,
#> #   COM <chr>, EED <chr>, EEV <chr>, OPR <chr>, `OTL@@DE` <chr>,
#> #   `OTL@EN` <chr>, `PC#` <chr>, SCT <chr>, SMS <chr>, YOR <chr>,
#> #   hum2abc <chr>, title <chr>, DataRecord <int>, File <int>, Filename <chr>,
#> #   Filepath <chr>, Global <lgl>, Label <chr>, ParentPath <int>, Path <int>, …
```
