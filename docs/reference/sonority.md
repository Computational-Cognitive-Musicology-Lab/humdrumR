# Interpret tertian sonorities from set(s) of notes.

The `sonority()` function accepts vectors of notes, usually grouped into
multiple chords by a `groupby` argument, and interprets those notes as a
tertian sonority. Chords are output using the representation indicated
by the `deparser` argument. By default,
[with/within.humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
will automatically pass `sonority` the `groupby` argument
`groupby = list(Piece, Record)`, so chords are estimated for each record
in the dataset.

## Usage

``` r
sonority(
  x,
  deparser = chord,
  Key = NULL,
  inversions = TRUE,
  incomplete = TRUE,
  enharmonic = FALSE,
  inPlace = length(groupby) > 0,
  fill = TRUE,
  groupby = list(),
  ...
)
```

## Arguments

- x:

  ***Input data, interpreted as pitches.***

  This vector is interpreted as pitch information using
  [`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- deparser:

  ***What output representation do you want?***

  Defaults to
  [`chord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md).

  Must be a [chord
  function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md),
  like `roman()`,
  [`harm()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harm.md)
  or
  [`chord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md).

- Key:

  ***The input key used by the deparser.***

  Defaults to `NULL`, indicating c major. However,
  [with/within.humdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  will automatically pass a `Key` field in the data to `sonority`, if
  there is one.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

  Some chord parsers don't use `Key`, so it is irrelevant, you *will*
  want to use a `Key` for roman numerals.

- inversions:

  ***Should we interpret note sets as inversions?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- incomplete:

  ***Should we return incomplete chords?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

- enharmonic:

  ***Should pitches be interpreted enharmonically?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- inPlace:

  ***Should the output always match the input?***

  Defaults to `FALSE` is there is no `groupby` list; but `TRUE` if there
  is.

  Must be a singleton `logical` value: an on/off switch.

- fill:

  ***Should the output duplicate each chord for every note in the
  input?***

  Defaults to `TRUE`.

  Must be a singleton `logical` value: an on/off switch.

  This argument only has an effect if `inPlace = TRUE`.

## Details

If `inPlace = TRUE`, `sonority()` returns vectorized output, with the
output matching the length of the input vector. When `inPlace = TRUE`,
by default, `fill = TRUE`, and each output chord is repeated to align
with the notes of the chord. If `fill = FALSE`, each chord is returned
only once, but padded with null tokens to match length of the input.
Finally, if `inPlace = FALSE` only one chord is returned for each group
in `groupby`.

If `inversions = TRUE`, the notes are interpreted in the chordal
inversion that is most compact (triad like) on the circle of thirds. If
`inversions = FALSE`, the lowest note is always interpreted as the root.

If `incomplete = TRUE`, incomplete chords are returns as they are, so
you might see things like "C7no5" (seventh chord with no fifth). If
`incomplete = FALSE`, `sonority()` will (attempt) to fill in missing but
"implied" triad notes, note like missing 5ths.

By default, `sonority()` will interpret the spelling of notes strictly,
so a "mispelled" triad, like *B, E♭, F♯* will be interpreted as
something weird—in this case an augmented *Eb* chord with no third and a
sharp 9! Note that in the case of [cross
relations](https://en.wikipedia.org/wiki/False_relation)—for example,
*B♭* **and** *B♮* in the same chord—`sonority()` will simply ignore the
later species that appears. However, if `enharmonic = TRUE`,
`sonority()` will reinterpret input notes by collapsing them into a
single diatonic set on the circle-of-fifths. This means that the set *B,
Eb, F♯* will be interpreted as *B, D♯, F♯* and the set *B♭, D, F, B♮*
will be interpreted as *B♭, D, F, C♭*.

## Examples

``` r
sonority(c('C', 'e', 'g', 'b-'))
#> [1] "C7"
sonority(c('G', 'BB', 'd', 'f', 'a'))
#> [1] "G9/B"

sonority(c('C', 'b-', 'd', 'f'))
#> [1] "Bbadd2/C"
sonority(c('C', 'b-', 'd', 'f'), inversions = FALSE)
#> [1] "C17sus2sus4"

chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
#> Finding and reading files...
#>  REpath-pattern '/private/var/folders/z2/2l9p7g8n0jjb9kjwgl6q8slw0000gn/T/RtmpgxrtCu/temp_libpath14125129ec8b8/humdrumR/HumdrumData/BachChorales/chor001.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
chorale <- within(chorales, dataTypes = 'Dd', ditto(Token) -> Token_dittoed) 
#> Error: object 'chorales' not found
chorale[[20:30,]]
#> ######################## vvv chor001.krn vvv #########################
#>     8:           **kern         **kern         **kern         **kern
#>    12:        *>[A,A,B]      *>[A,A,B]      *>[A,A,B]      *>[A,A,B]
#>    13:     *>norep[A,B]   *>norep[A,B]   *>norep[A,B]   *>norep[A,B]
#>    14:              *>A            *>A            *>A            *>A
#>    20:              4GG             4B             4d             4g
#>    21:               =1             =1             =1             =1
#>    22:               4G             4B             4d             2g
#>    23:               4E            8cL             4e              .
#>    24:                .            8BJ              .              .
#>    25:              4F#             4A             4d            4dd
#>    26:               =2             =2             =2             =2
#>    27:               4G             4G             2d            4.b
#>    28:               4D            4F#              .              .
#>    29:                .              .              .             8a
#>    30:               4E             4G             4B             4g
#>    52:              *>B            *>B            *>B            *>B
#>   124:               *-             *-             *-             *-
#> ######################## ^^^ chor001.krn ^^^ #########################
#> 
#>    Data fields: 
#>          *Token :: character
#> 
within(chorale[[20:30,]], sonority(Token_dittoed))
#> Error: object 'Token_dittoed' not found
within(chorale[[20:30,]], sonority(Token_dittoed, deparser = harm))
#> Error: object 'Token_dittoed' not found
```
