# German-style pitch notation.

Based on the common German system of notating pitches, as encoded in the
humdrum [`**Tonh`](https://www.humdrum.org/rep/Tonh/index.html)
interpretation.

If `tonh()` is applied to a [humdrumR data
class](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
you may use the data's
[fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
as arguments. If no field names are specified, the first [selected
field](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
is used as `x`.

## Usage

``` r
# Default S3 method
tonh(
  x,
  ...,
  generic = FALSE,
  simple = FALSE,
  octave.relative = FALSE,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  gamutArgs = list(),
  inPlace = FALSE
)

humData |> select(Token) |> tonh() 
humData |> tonh(simple = TRUE)
humData |> tonh(Token, Key = Key)

tonh(
  x,
  ...,
  generic = FALSE,
  simple = FALSE,
  octave.relative = FALSE,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  gamutArgs = list(),
  inPlace = FALSE
)
```

## Arguments

- x:

  ***Input data to parse as pitch information.***

  The `x` argument can be any
  ([atomic](https://rdrr.io/r/base/vector.html)) vector, or a
  [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  or `NULL`.

- ...:

  ***Arguments passed to the [pitch
  deparser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).***

  There are also two hidden (advanced) arguments you can specify:
  `memoize` and `deparse` (see the details below).

- generic:

  ***Should "specific" pitch information (accidentals and qualites) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- simple:

  ***Should "compound" pitch information (octave/contour) be
  discarded?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

- Key:

  ***The input `Key` used by the parser, deparser, and transposer.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

- transposeArgs:

  ***An optional list of arguments passed to a special
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  call.***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to
  [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).

- parseArgs:

  ***An optional list of arguments passed to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).***

  Defaults to an empty [`list()`](https://rdrr.io/r/base/list.html).

  Must be a `list` of named arguments to the [pitch
  parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- inPlace:

  ***Should non-pitch information be retained in the output string.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  This argument only has an effect if the input (the `x` argument) is
  `character` strings, *and* there is extra, non-pitch information in
  the input strings "besides" the pitch information. If so, and
  `inPlace = TRUE`, the output will be placed into an output string
  beside the original non-pitch information. If `inPlace = FALSE`, only
  the pitch output information will be returned (details below).

- S:

  ***Should the special shorthand for Eb and Ab be used?.***

  Defaults to `TRUE`.

  If `S = TRUE`, E-flat (`Ees`) will be output as `"S"` and A-flat
  (`Aes`) will be output `"As"`.

## Basic pitch arguments

Each pitch function has a few standard arguments which control details
of the output. The most important are the `generic` and `simple`
arguments, which allow you to control what type of pitch information is
returned.

### Generic vs Specific

If `generic = TRUE`, [specific pitch
information](https://en.wikipedia.org/wiki/Generic_and_specific_intervals)
(accidentals or qualities) is omitted from the output. As an alternative
way of controlling the same functionality, you can use the `specific`
argument, where `specific == !generic`.

In the case of atonal functions, the "generic" version of that pitch is
output: for example, `semits('c#', generic = TRUE)` will return `0`,
because the "generic" version of *C#* is *C*, which corresponds to `0`.
However, note that the generic version of a pitch follows the key, so
`semits('c#', generic = TRUE, Key = 'A:')` will return `1`!

### Simple vs Compound

If `simple = TRUE`, [compound pitch
information](https://en.wikipedia.org/wiki/Interval_(music)#Simple_and_compound)
(octave and contour) is omitted from the output. As an alternative way
of controlling the same functionality, you can use the `compound`
argument ,where `compound == !simple`.

There is actually more than one way you might want to divide compound
intervals up into simple and octave parts. For example, you might like
to call an output `-M2` (descending major 2nd) *OR* `+m7` (ascending
minor 7th in the octave below). This functionality can be controlled
with the `octave.round` argument: see the [pitch deparsing
documentation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).

### Key

The `Key` argument must be a
[diatonicSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html),
or something that can be parsed into one. The `Key` argument is passed
to the
[parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md),
deparser, *and* transpose—*unless* an alternate `Key` is passed to
`transposeArgs` or `parseArgs`. Various deparsing options use the `Key`
argument; for example, use of `implicitSpecies` (see advanced parsing
section) is dependent on the `Key`.

If you use any [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
within a special call to
[withinHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
`humdrumR` will automatically pass the `Key` field from the humdrum data
to the function—this means that, in most cases, you don't need to
explicitly do anything with the `Key` argument! (If you want this *not*
to happen, you need to explicitly specify your own `Key` argument, or
`Key = NULL`.)

### Parse arguments

The `parseArgs` argument must be a
[`list()`](https://rdrr.io/r/base/list.html) of (named) arguments which
are passed to the input
[parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
For example, if our input representation uses `"X"` to represent double
sharps, we could specify
`kern('CX5', parseArgs = list(doublesharp = 'X'))` and get the correct
result (`"cc##"`). As a convenient shorthand, or "syntactic sugar," you
can specify `parseArgs` in an alternate way: Simply input
`parse(args...)` as unnamed argument to any pitch function. For example,
we can get the exact same result as before by typing
`kern('CX5', parse(doublesharp = 'X'))`.

### Transpose arguments

The `transposeArgs` argument must be a
[`list()`](https://rdrr.io/r/base/list.html) of (named) arguments which
are passed to an internal call to
[`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md),
allowing us to easily transpose pitch information. For example, we could
type `kern(c('C', 'D', 'E'), transposeArgs = list(by = 'M9'))` can get
the output `c('d', 'e', 'f#')`. The possible transpose args are:

- `by`
  ([tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
  `length == 1 | length == (x)`)

- `from`
  ([diatonicSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html),
  `length == 1 | length == (x)`)

- `to`
  ([diatonicSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html),
  `length == 1 | length == (x)`)

- `real` (`logical`, `length == 1`) Should transposition be real or
  tonal?

- `relative` (`logical`, `length == 1`) Should key-wise transposition be
  based on relative or parallel keys?

As a convenient shorthand, or "syntactic sugar," you can specify
`transposeArgs` in an alternate way: Simply input `transpose(args...)`
as unnamed argument to any pitch function. For example, we can get the
exact same result as before by typing
`kern(c('C', 'D', 'E'), transpose(by = 'M9'))`.

#### Transposing by interval

As when calling
[`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
directly, the `by` argument can be anything coercable to a
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
and the output will be transposed by that amount. If `real = FALSE`,
tonal transposition (within the `Key`) will be performed. For more
details on transposition behavior, check out the
[`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
docs.

#### Transposing by key

Another way of transposing is by specifying an input ("from") key and an
output ("to") key. By default, the `Key` argument is passed to
`transpose` as both `from` and `to`, so nothing actually happens. Thus,
if you specify either a `from` key or `to` key, transposition will
happen to/from that key to `Key`. Of course, if you specify `from` *and*
`to` the transposition will happen between the keys you specify.

If you use any [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
within a special call to
[withinHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
`humdrumR` will automatically pass the `Key` field from the humdrum data
to the function. If you specify a `to` key, the `Key` field will be
passed as the transpose `from` key, and as a result, all the pitches in
the input will be transposed from whatever keys they are in to your
target (`to`) key!

The `real` and `relative` arguments give you special control of how
key-wise transposition works, so check out the
[`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
docs for more details!

### In-place parsing

In humdrum data, character strings are often encoded with multiple
pieces of musical information right besides each other: for example,
`**kern` data might include tokens like `"4.ee-[`. The `humdrumR` parser
(`tonalInterval`) will automatically "pull out" pitch information from
within strings, if it can find any using the appropriate known regular
expressions. For example, `pitch('4.ee-[')` returns Eb5. However, all
the pitch functions (like
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)
and
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md))
have an option to keep the "extra" information and return the result "in
place"—i.e., embedded right where it was found in the input string. This
is controlled with the `inPlace` argument, which is `FALSE` by default.
So, `pitch('4.ee-[', inPlace = TRUE)` will return 4.Eb5\[—keeping the
`"4."` and the `"["`. (This obviously only works if the input is a
string, not a numeric!) Note that `inPlace = TRUE` will force functions
like `semits`, which normally return `numeric` values, to return
`character` strings *if* their input is a character string.

## Pitch-Gamut Levels

The
[`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
will automatically generate factor levels for pitch data using the
[`gamut()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/gamut.md)
function. This makes sure tabulated data is sorted in a logical order,
and includes missing pitches. The `simple`/`complex` and
`generic`/`specific` arguments are automatically passed to
[`gamut()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/gamut.md);
additional arguments can be passed to gamut using
`gamutArgs = list(...)`, or with the syntactic sugar `gamut(...)`. (Read
the
[`gamut()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/gamut.md)
docs for an explanation of gamut generation.) This feature is used to
control table layout of pitch data, as well as to assure consistent
tables when grouping data.

When `inPlace = TRUE` no special tabulation will occur.

## See also

To better understand how this function works, read about the [family of
pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
or how pitches are
[parsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
and
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).

Other absolute pitch functions:
[`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md),
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
[`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md),
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md),
[`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md)

Other pitch functions:
[`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md),
[`bhatk()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bhatk.md),
[`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md),
[`freq()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/freq.md),
[`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md),
[`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md),
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
[`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md),
[`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md),
[`pc()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pc.md),
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md),
[`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md),
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md),
[`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md),
[`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md)

## Examples

``` r
exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
results <- tonh(exampleToken)
results
#> **tonh (character)
#> [1] G2   G3   E3   Fis3 G3   D3   E3  

exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/BeethovenVariations/B075_00_05_a.krn' matches 1 text files in 1 directory.
#> One file read from disk.
#> Validating one file...
#> all valid.
#> Parsing one file...
#> Assembling corpus...
#> Done!
results <- with(exampleHumdrum[[,3:4]], tonh(Token))
results
#> **tonh (character)
#>  [1] .    F3   C4   E3   C4   F3   C4   Bes2 Bes3 Bes2 Bes3 C3   A3   C3   G3  
#> [16] F3   .    A4   C5   F4   A4   C4   F4   F4   A4   A4   C5   A4   C5   G4  
#> [31] Bes4 F4   A4   F4   D5   F4   D5   F4   C5   Bes3 E4   A3   F4   .   
```
