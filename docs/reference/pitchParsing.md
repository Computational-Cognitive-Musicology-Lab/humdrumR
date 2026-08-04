# Parsing pitch information

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a easy-to-use but powerful system for *parsing* pitch
information: various basic pitch representations (including `numeric`
and `character`-string representations) can be "parsed"—read and
interpreted by `humdrumR`. For the most part, parsing automatically
happens "behind the scenes" whenever you use any humdrumR [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
like
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
or
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md).

## Usage

``` r
tonalInterval(...)

# S3 method for class 'tonalInterval'
tonalInterval(x, ...)

# S3 method for class 'logical'
tonalInterval(x, ...)

# S3 method for class '`NULL`'
tonalInterval(x, ...)

# S3 method for class 'numeric'
tonalInterval(x, ..., Exclusive = NULL, multiDispatch = FALSE)

# S3 method for class 'character'
tonalInterval(x, ..., Exclusive = NULL, multiDispatch = FALSE)

# S3 method for class 'factor'
tonalInterval(x, Exclusive = NULL, ...)

# S3 method for class 'token'
tonalInterval(x, Exclusive = NULL, ...)
```

## Arguments

- Exclusive:

  ***An exclusive interpretation to guide parsing of the input.***

  Must be `NULL`, or a `character` vector which is either length `1` or
  `length(x)`.

- str:

  ***The input vector.***

  Must be either `character` or `numeric`.

- Key:

  ***The onic key used to interpret the pitch information.***

  Defaults to `NULL`.

  Must be a `diatonicSet` or something coercable to `diatonicSet`; must
  be either length `1` or `length(x)`

  For example, use of `implicitSpecies` (see advanced parsing section)
  is dependent on the `Key`. The output `tonalInterval` is output
  *within the key*: thus, `tonalInterval('C#', Key = "A:")` returns the
  tint representing a **Major 3rd**, because *C#* is the major third of
  A major.

## Value

`tonalInterval()` returns a
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
object of the same length and dimensions as `x`. `NULL` inputs (`x`
argument) return a `NULL` output. `NA` values in the input `x` are
propagated to the output.

## Details

The underlying parser used by all `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
can be called explicitly using the function `tonalInterval()`. The
`tonalInterval` parser will attempt to parse any input information into
a
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
object—a back-end pitch representation that you probably don't need to
care about! When you use one of the main [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
like
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md) or
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
the input is parsed into a
[tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
object, then immeely
[deparsed](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)
to the representation you asked for (e.g., `**kern` or `**semits`).
Thus, the underlying pipeline for `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
looks something like:

- **Input** representation (e.g., `**pitch` or `**semits`) `|>`

  - *Parsing* (done by `tonalInterval()`) `|>`

    - **Intermee**
      ([tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md))
      representation `|>`

  - *Deparsing* `|>`

- **Output** representation (e.g. `**kern` or `**solfa`)

*This* documentation talks about the parsing step. For an overview of
the "deparsing" process, look
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md).
To learn about the "deparsing" of specific representations, [start
here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
or go straight to the docs for specific functions— for example, call
[`?kern`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md) to
learn about
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).

## Dispatch

The pitch parser (`tonalInterval()`) is a generic function, meaning it
accepts a variety of inputs and automatically "dispatches" the
appropriate method for parsing the input. R's standard `S3` system is
used to dispatch for either `numeric` or `character`-string input:
Generally, `numeric` (or `integer`) inputs are interpreted as various
*atonal* pitch representations while `character` strings are interpreted
as various *tonal* pitch representations. Given either a `character`
string or a number, `humdrumR` then uses either regular-expression
matching or humdrum exclusive interpretation matching to dispatch
specific parsing methods.

## Tonal Parsing (`character`-string inputs)

Since humdrum data is inherently string-based, the most powerful part of
the `humdrumR` pitch-parser is its system for parsing pitch (mostly
tonal) information from character strings. (This includes character
tokens with pitch information embedded alongside other information;
Details below.) The pitch parser (`tonalInterval()`) uses a combination
of regular-expressions and exclusive interpretations to decide how to
parse an input string. There are twelve regular-expression patterns for
pitch that `tonalInterval()` knows how to parse automatically:

|                                                                                 |                           |                  |
|---------------------------------------------------------------------------------|---------------------------|------------------|
| Representation                                                                  | Exclusive                 | Example          |
| [Kern](https://www.humdrum.org/rep/kern/index.html)                             | \*\*kern                  | `ee-`            |
| [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch)              | \*\*pitch                 | `Eb5`            |
| [Helmholtz pitch](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                   | none                      | `eb'`            |
| [Lilypond pitch](https://lilypond.org/doc/v2.22/Documentation/notation/pitches) | none                      | `ees'`           |
| [German pitch](https://www.humdrum.org/rep/Tonh/index.html) notation            | \*\*Tonh                  | `S5`             |
| [Interval](https://en.wikipedia.org/wiki/Interval_(music))                      | \*\*hint/\*\*mint/\*\*int | `+m3`            |
| [Scale degree](https://en.wikipedia.org/wiki/Degree_(music))                    | \*\*deg or \*\*degree     | `^^3-` or `3-/5` |
| [Pitch Class](https://en.wikipedia.org/wiki/Pitch_class#Integer_notation)       | \*\*pc                    | `3`              |
| Relative-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)               | \*\*solfa                 | `^me`            |
| Fixed-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                  | \*\*solfg                 | `mi~b5`          |
| [Swara](https://en.wikipedia.org/wiki/Svara)                                    | \*\*bhatk                 | `g`              |

### Exclusive Dispatch

If you call `tonalInterval()` (or *any* [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md))
on a `character`-string vector, with a non-`NULL` `Exclusive` argument,
that `Exclusive` argument will be used to choose the input
interpretation you want, based on the "Exclusive" column in the table
above. For example, `kern(x, Exclusive = 'solfa')` will force the parser
to interpret `x` as `**solfa` data. Similarly,
`solfa(x, Exclusive = 'kern')` will force the parser to interpret `x` as
`**kern` data. If you use any [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
within a special call to
[withinHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
`humdrumR` will automatically pass the `Exclusive` field from the
humdrum data to the function—this means, that in most cases, you don't
need to explicitly do anything with the `Exclusive` argument! (If you
want this *not* to happen, you need to explicitly specify your own
`Exclusive` argument, or `Exclusive = NULL`.)

### Regex Dispatch

If you call `tonalInterval()` (or *any* [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md))
on a `character`-string vector, but the `Exclusive` argument is missing
or `NULL`, `humdrumR` will instead use regular-expression patterns to
select a known interpretation. For example, `pitch('so')` will
automatically recognize that `'so'` is solfege, and will interpret the
data accordingly (the output should be G4). If there are more than one
matches, `humdrumR` will use the longest match, and if they tie, pick
based on the order in the table above (topmost first).

If there is no match, `tonalInterval` (and all other [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md))
return `NA` values. Remember, if `Exclusive` is specified, it overrides
the regex-based dispatch, which means that
`pitch('so', Exclusive = 'kern')` will return `NA`, because `'so'` can't
be interpreted as a `**kern` value.

#### "In place" parsing

In lots of humdrum data, character strings are encoded with multiple
pieces of musical information right besides each other: for example,
`**kern` data might include tokens like `"4.ee-[`. The `humdrumR` pitch
parser (`tonalInterval()`) will automatically "pull out" pitch
information from within strings, if it can find any, using the
appropriate known regular expressions. Various [pitch parsing
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
have an option to keep the original "extra" data, using their `inPlace`
argument.

### Advanced Tonal Parsing Options

The eleven tonal representations listed above are parsed through a
common interface. By using "advanced" parsing arguments, you can tweak
how this parsing is done, so as to accommodate even more input
representations! This means we are controlling the behavior of
`tonalInterval()`, in the second step of our pipeline:

- **Input** representation `|>`

  - *Parsing* (done by `tonalInterval(PARSE ARGS GO IN HERE!)`) `|>`

    - **Intermee**
      ([tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md))
      representation `|>`

  - *Deparsing* `|>`

- **Output** representation

Note that these arguments are similar or identical to parallel
"advanced" deparsing arguments used by various [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md).
The following "advanced" parsing arguments are available (read all the
details about them further down):

- **Steps**

  - `step.labels`

  - `step.signed`

- **Species** (accidentals or qualities)

  - `qualities`

  - `specifier.maximum`

  - *Accidentals*

    - `natural`, `flat`, `sharp`, `doubleflat`, `doublesharp`

  - *Qualities*

    - `perfect`, `major`, `minor`, `augment`, `diminish`

  - *Implicit vs Explicit Species*

    - `implicitSpecies`

    - `absoluteSpecies`

    - `memory`, `memoryWindows`

- **Octave**

  - `octave.integer`

  - `up`, `down`, `same`

  - `octave.offset`

  - `octave.round`

  - `octave.relative`, `octave.absolute`

- **String parsing**

  - `parts`

  - `sep`

These "advanced" arguments can be used directly in *any* [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
or in a call to `tonalInterval` itself. To use them with `tonalInterval`
just specify them directly as arguments: for example,
`tonalInterval(x, qualities = TRUE)`. To use them with other [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
you can either...

- Put them in the `parseArgs` argument:

  - `kern(x, parseArgs = list(qualities = TRUE))`

- Or use the "syntactic sugar" short-hand form:

  - `kern(x, parse(qualities = TRUE))`

Each of the known Exclusive/Regex-dispatch combo (see the table above)
is associated with default parsing arguments. For example, if you set
`Exclusive = 'kern'` or just use data that *look* like `**kern`, the
`flat` argument is set to `"-"`, However, if you had, for example, input
data that looked like `**kern` **except** it used a different flat
symbol, like `"_"`, you could modify the parser:
`kern("EE_", parse(flat = "_"))` This overrides the default value for
`**kern`—notice, that it *also* updates the `**kern` regular expression
accordingly, so it works exactly the same as the standard
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
function.

### Steps

Any representation of "tonal" pitch information will include a
representation of *onic steps*. You can control how the parser reads
onic steps from a pitch representation using the `step.labels` argument.
The `step.labels` argument must be an atomic vector of unique values,
with a length which is a positive multiple of seven. Examples of
`step.labels` arguments that are currently used by preset `humdrumR`
pitch parsers include:

- `parse(step.labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))` —
  (`**Tonh`)

- `parse(step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't'))` —
  (`**solfa`)

- `parse(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'))` —
  (roman numerals)

If `step.labels` is `NULL`, steps are assumed to be represented by
integers, including negative integers representing downward steps.

There is also a `step.signed` (`logical`, `length == 1`) argument: if
`step.signed = TRUE`, lowercase versions of `step.labels` are
interpreted as negative (downward) steps and uppercase versions of
`step.labels` are interpreted as positive (upwards) steps. This option
is used, for example, by the default
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md) and
[`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md)
parsers.

### Species

In tonal pitch representations, "*specific*" versions of tonal
pitches—the tonal "species"—are indicated by "specifiers": either
*accidentals* or *qualities*. The `qualities` (`logical`, `length == 1`)
argument indicates whether accidentals are used (`qualities = FALSE`) or
qualities (`qualities = TRUE`). Some specifiers can be repeated any
number of times, like "triple sharps" or "doubly augmented"; The
`specifier.maximum` (`integer`, `length == 1`) argument sets a maximum
limit on the number of specifiers to read. For example, you could force
all triple sharps (`"###"`) or double sharps (`"##"`) to parse as just
`"#"`, by specifying `specifier.maximum = 1L`.

#### Accidentals

If `qualities = FALSE` the parser will look for accidentals in the
input, recognizing three types: naturals, flats, and sharps. The
`natural`, `flat`, and/or `sharp` (`character`, `length == 1`) arguments
can be used to indicate how accidentals are represented in the input.
For example, if the input strings look like `c("Eflat", "C")`, you could
set the argument `flat = "flat"`.

Examples of accidental argument combinations that are currently used by
preset `humdrumR` pitch parsers include:

- `parse(flat = "b", sharp = "#")` -\> `**pitch`

- `parse(flat = "-", sharp = "#")` -\> `**kern`

- `parse(flat = "-", sharp = "+")` -\> `**degree`

The `doubleflat`, and `doublesharp` (`character`, `length == 1`)
arguments are `NULL` by default, but can be set if a special symbol is
used to represent two sharps or flats. For example, you might have an
input which represents double sharps as `"x"`. You could call
`kern("Fx", parse(doublesharp = "x"))` and the output will be `"F##"`.

#### Qualities

If `qualities = TRUE` the parser will look for qualities in the input,
recognizing five types: perfect, minor, major, augmented, and
diminished. The `perfect`, `major`, `minor`, `diminish`, and/or
`augment` (`character`, `length == 1`) arguments can be used to indicate
how qualities are represented in the input. (Note: we are talking about
interval/degree qualities here, not chord qualities!) For example, if
the input strings look like `c("maj3", "p4")`, you could set the
arguments `major = "maj"` and `perfect = "p"`. Examples of quality
argument combinations that are currently used by `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
include:

- `parse(major = "M", minor = "m", perfect = "P", diminish = "d", augment = "A")`

- `parse(diminish = "o", augment = "+")`

#### Implicit vs Explicit Species

In some musical data, specifiers (e.g., accidentals or qualities) are
not explicitly indicated; instead, you must infer the species of each
pitch from the context—like the key signature!.

##### From the Key

The most important argument here is `implicitSpecies` (`logical`,
`length == 1`): if `implicitSpecies = TRUE`, the species of input
without an explicit species indicated is interpreted using the `Key`.
For example,

- `kern('C', Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as
  `"C#"`

  - C is sharp in A major.

- `kern('C', Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as
  `"C"`

  - C is natural in A minor.

- `kern('C', Key = 'a-:', parse(implicitSpecies = TRUE))` is parsed as
  `"C-"`

  - C is flat in A-flat minor.

By default, if you input *already has* specifiers, they are interpreted
absolutely—overriding the "implicit" `Key`—, even if
`implicitSpecies = TRUE`. Thus, if we are in A major:

- `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as
  `"C#"`.

  - The `"#"` is unnecessary.

- `kern("Cn", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as
  `"C"`.

  - The `"n"` overrides the `Key`.

- `kern("C#", Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as
  `"C#"`.

  - The `"#"` overrides the `Key`.

However! You can also change this behavior by setting the
`absoluteSpecies` (`logical`, `length == 1`) argument to `FALSE`. If you
do so, the specifiers in the input are interpreted "on top of" the key
accidental:

- `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE, absoluteSpecies = FALSE))`
  is parsed as `"C##"`.

  - The `"#"` from the input is added to the `"#"` from the `Key`,
    resulting in double sharp!

This is an unusual behavior, for absolute pitch representations like
`**kern`. However, for use with scale or chord degrees,
`absoluteSpecies = FALSE` might be appropriate. For example, if we are
reading a [figured bass](https://en.wikipedia.org/wiki/Figured_bass) in
the key of E minor, a `"b7"` figure above an E in the bass should be
interpreted as a *double flat* (diminished) 7th (Db over E)! If this is
how your data is encoded, use `absoluteSpecies = FALSE`.

##### Memory

In some musical data, it is assumed that a accidental on a note "stays
in effect" on that scale step until the next bar, or until a different
accidental replaces it. Fortunately, the `humdrumR` parser
(`tonalInterval()`) also knows how to parse data encoded with "memory"
this way. If `memory = TRUE`, the accidental (or quality) of each input
note is "remembered" from previous appearances of that scale step. For
example,

- `kern(c("D#", "E", "D", "E", "Dn", "C", "D"), parse(memory = TRUE))`

  - is parsed as `c("D#", "E", "D#", "E", "D", "C", "D")`

If we want the "memory" to only last when specific time windows (like
bars), we can also specify a `memoryWindows` argument. `memoryWindows`
must be an atomic vector which is the same length as the input (`x`
argument). Each unique value within the `memoryWindows` vector is
treated as a "window" within which `memory` operates. The most common
use case would be to pass the `Bar` field from a `humdrumR` dataset to
`memoryWindows`!

The `memory` and `memoryWindows` argument work whatever values of
`implicitSpecies` or `absoluteSpecies` are specified! Though all the
examples here use accidentals, these arguments all have the same effect
if parsing qualities (`qualities = TRUE`).

### Octave

The final piece of information encoded in most (but not) all pitch
representations is an indication of the "compound pitch"— incorporating
octave information. In `humdrumR` octaves are *always* defined in terms
of scale steps: so two notes with the same scale degree/letter name will
always be the same octave. This mainly comes up with regards to Cb and
B#: Cb4 is a semitone below ; B#3 is enharmonically the same as
middle-**C**.

#### Integer Octaves

The simplest way octave information can be encoded is as an integer
value, as in [Scientific
Pitch](https://en.wikipedia.org/wiki/Scientific_pitch). If you need to
parse integer-encoded octaves, set the `octave.integer` (`logical`,
`length == 1`) argument to `TRUE`. By default, `humdrumR` considers the
"central" octave (`octave == 0`) to be the octave of , or equivalently,
a unison. However, if a different octave is used as the central octave,
you can specify the `octave.offset` (`integer`, `length == 1`) argument.

To illustrate, the default [Scientific
Pitch](https://en.wikipedia.org/wiki/Scientific_pitch) parser used the
arguments:

- `kern('C5', parse(octave.integer = TRUE, octave.offset = 4)`

  - Returns `"cc"` (the octave above middle C).

#### Non-integer Octave Markers

If `octave.integer = FALSE`, the `humdrumR` parser instead looks for
three possible symbols to indicate octave information. These symbols are
controlled using the `up`, `down`, and `same` (`character`,
`length == 1`) arguments. A `same` symbol, or no symbol, is interpreted
as the "central" octave; repeating strings of the `up` or `down` symbols
indicate increasing positive (`up`) or negative (`down`) octaves. For
example, in `lilypond` notation, `,` represents lower octaves, and `'`
(single apostrophe) represents upper octaves. So the default
[`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md)
parser uses these arguments:

- `pitch(c("c", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",", octave.offset = 1))`

  - Returns `c("C2", "C3", "C4")`.

(Note that lilypond makes the octave *below* the central octave, using
`octave.offset = 1`.)

#### Octave "Rounding"

In some situations, pitch data might interpret the "boundaries" between
octaves a little differently. In most absolute pitch representations
(e.g.,
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)),
the "boundary" between one octave and the next is between B (degree 7)
and C (degree 1). However, if for example, we are working with data
representing intervals, we might think of an "octave" as spanning the
range `-P4` (`G`) to `+P4` (`f`). In this case, the "octave boundary" is
*centered* around the unison (or ), rather than starting *at*
middle-**C**/unison. If our data was represented this way, we could use
the `octave.round` argument; `octave.round` must be a rounding
*function*, either round, floor, ceiling, trunc, or
[expand](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expand.md).
These functions indicate how we interpret simple pitches "rounding" to
the nearest C/unison. The default behavior for most pitch
representations is `octave.round = floor`: each scale step is rounded
downwards to the nearest C. So B is associated with the C 7 steps below
it. If, on the other hand, `octave.round = round`, then scale-steps are
"rounded" to the closest C, so B and A are associated with the closer C
*above* them. Indeed, `octave.round = round` gets us the `-P4` \<-\>
`+P4` behavior we mentioned earlier!

When working parsing
[intervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md),
the `octave.round` option allows you to control how the "simple part"
(less than an octave) of a compound interval is represented. For
example, we might think of a ascending major 12th as being an ascending
octave *plus* a ascending perfect 5th: \*\* +P8 + P5\*\*. **Or** we
could encode that same interval as *two* ascending octaves *minus* a
perfect fourth: **+ P15 - P4**. The following table illustrates how
different `octave.round` arguments "partition" compound intervals into
simple parts and octaves:

|       |           |           |           |           |           |
|-------|-----------|-----------|-----------|-----------|-----------|
|       | round     | floor     | ceiling   | trunc     | expand    |
| -P12: | -P15 + P4 | -P15 + P4 | -P8 - P5  | -P8 - P5  | -P15 + P4 |
| +P12: | +P15 - P4 | +P8 + P5  | +P15 - P4 | +P8 + P5  | +P15 - P4 |
| +P8:  | +P8 + P1  | +P8 + P1  | +P8 + P1  | +P8 + P1  | +P8 + P1  |
| -P5:  | -P8 + P4  | -P8 + P4  | +P1 - P5  | +P1 - P5  | -P8 + P4  |
| +P19: | +P22 - P4 | +P15 + P5 | +P22 - P4 | +P15 + P5 | +P22 - P4 |
| +P15: | +P15 + P1 | +P15 + P1 | +P15 + P1 | +P15 + P1 | +P15 + P1 |
| +P4:  | +P1 + P4  | +P1 + P4  | +P8 - P5  | +P1 + P4  | +P8 - P5  |
| +P26: | +P29 - P4 | +P22 + P5 | +P29 - P4 | +P22 + P5 | +P29 - P4 |
| +P22: | +P22 + P1 | +P22 + P1 | +P22 + P1 | +P22 + P1 | +P22 + P1 |
| +P11: | +P8 + P4  | +P8 + P4  | +P15 - P5 | +P8 + P4  | +P15 - P5 |
| +P33: | +P36 - P4 | +P29 + P5 | +P36 - P4 | +P29 + P5 | +P36 - P4 |
| +P29: | +P29 + P1 | +P29 + P1 | +P29 + P1 | +P29 + P1 | +P29 + P1 |
| +P18: | +P15 + P4 | +P15 + P4 | +P22 - P5 | +P15 + P4 | +P22 - P5 |
| +P40: | +P43 - P4 | +P36 + P5 | +P43 - P4 | +P36 + P5 | +P43 - P4 |

Notice that, if `octave.floor` is being used, all simple intervals are
represented as ascending.

When parsing ["absolute"
pitch](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)
representations, the `octave.round` option allows you to control which
octave notes are associated with. The following table illustrates:

|         |       |       |         |       |        |
|---------|-------|-------|---------|-------|--------|
|         | round | floor | ceiling | trunc | expand |
| FF:     | F2    | F2    | F3      | F3    | F2     |
| gg:     | G6    | G5    | G6      | G5    | G6     |
| cc:     | C5    | C5    | C5      | C5    | C5     |
| F:      | F3    | F3    | F4      | F4    | F3     |
| ggg:    | G7    | G6    | G7      | G6    | G7     |
| ccc:    | C6    | C6    | C6      | C6    | C6     |
| f:      | F4    | F4    | F5      | F4    | F5     |
| gggg:   | G8    | G7    | G8      | G7    | G8     |
| cccc:   | C7    | C7    | C7      | C7    | C7     |
| ff:     | F5    | F5    | F6      | F5    | F6     |
| ggggg:  | G9    | G8    | G9      | G8    | G9     |
| ccccc:  | C8    | C8    | C8      | C8    | C8     |
| fff:    | F6    | F6    | F7      | F6    | F7     |
| gggggg: | G10   | G9    | G10     | G9    | G10    |

#### Absolute or Relative (contour) Octave

In some notation encoding schemes, the "octave" of each note is
interpreted *relative* the previous note, rather than any absolute
reference. The most prominent system is Lilypond's [relative octave
entry](https://lilypond.org/doc/v2.22/Documentation/notation/writing-pitches#relative-octave-entry)
style. This style is often used in combination with scale degree
representations—as in the
[RS200](http://rockcorpus.midside.com/melodic_transcriptions.md) corpus.
For example, a data set might say `Do Re Mi vSo La Ti Do`, with the
`"v"` indicating a jump down to `So`. To activate relative-octave
parsing, set `octave.relative = TRUE`—alternatively, you can use
`octave.absolute = FALSE`, which is equivalent.

In a relative-octave data, we assume that octave indications indicate a
shift relative to the previous note. This would usually be used in
combination with octave markers like `"^"` (up) or `"v"` (down).
Different combinations of `octave.round` allow us to parse different
behaviors:

- If `octave.round = round`, a `same` marker (or no marker) indicates
  that the note is the pitch *closest* to the previous pitch. Octave
  markers indicate alterations to this assumption. As always, this is
  based on scale steps, not semitones! Any fourth is "closer" than any
  fifth, regardless of their quality: So *C F#* is ascending and *C Gb*
  is descending! A ascending diminished 5th would be written
  `C ^Gb`—with `up = ^`.

- If `octave.round = floor`, a `same` marker (or no marker) indicates
  that the note is in the octave above the previous pitch. Octave
  markers indicate alterations to this assumption. With this setting,
  going from *C* down to *B* always requires a `down` mark.

### String Parsing

In addition to the three types of *musical* parsing considerations
reviewed above (steps, species, and octaves), there are also some
general string-parsing issues that we can consider/control.

#### Parts and Order

So far (above) we've discussed various ways that tonal pitch information
(step, species, and octave) can be encoded, and how the `humdrumR`
parser can be modified to handle different options. However, there are
two general parsing issues/options to consider: what information is
encoded, and in *what order*? The `parts` argument can be specified to
indicate this. The `parts` argument must be a `character` vector of
length 1–3. The characters in the vector must [partial
match](https://rdrr.io/r/base/pmatch.html) either `"step"`, `"species"`,
or `"octave"`. The presence of any of these strings in the `parts`
vector indicate that the corresponding information should be parsed. The
*order* of the strings indicates what order the pieces of pitch
information are encoded in input strings.

To illustrate, imagine that we had input data which was identical to a
standard interval representation—e.g., `M2` and `P5`—except the quality
appears *after* the step—e.g., `2M` and `5P`. We could call
`interval(c("2M", "5P"), parse(parts = c("step", "species")))` and sure
enough we'd get the correct parse!

One final string-parsing argument is `sep`, which indicates if there is
a character string separating the pitch information components. The most
common case would be a comma or space. For example, we could use a parse
command like this: `kern("E flat 5", parse(flat = "flat", sep = " "))`.

## Atonal Parsing (`numeric` inputs)

The `humdrumR` pitch parser (`tonalInterval()`) will interpret numeric
inputs as atonal pitch information. By default, numbers are interpreted
as semitones. However, parses for
[`midi()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
[`cents()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
and
[frequencies](https://humdrumR.ccml.gtcmt.gatech.edu/reference/freq.md)
are also defined. Dispatch to these different parsers is controlled by
the `Exclusive` argument.

|                |                        |                |
|----------------|------------------------|----------------|
| Representation | Exclusive              | Example        |
| Semitones      | \*\*semits (or `NULL`) | `3` -\> `e-`   |
| MIDI           | \*\*midi               | `63` -\> `e-`  |
| Cents          | \*\*cents              | `300` -\> `e-` |
| Frequency (Hz) |                        |                |

### Enharmonic Interpretation

When converting from an atonal representation to a tonal one, we must
decide how to interpret the tonality of the input—specifically, which
[enharmonic spelling](https://en.wikipedia.org/wiki/Enharmonic) of notes
to use. The `humdrumR` numeric parser interprets atonal pitches in a
"enharmonic window" of 12 steps on the line-of-fifths. The position of
this window is set with the `enharmonic.center` (`integer`,
`length == 1`) argument. By default, `enharmonic.center = 0`, which
creates a window from a `-5` (*b2*) to `+6`) (*\#4*). If you prefer
*\#1* instead of *b2*, set `enharmonic.center = 1`. For all flats, set
`enharmonic.center = -1`. For all sharps, set `enharmonic.center = 4`.

|                     |     |     |     |     |     |     |     |     |     |     |     |     |
|---------------------|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
| `enharmonic.center` | 0   | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  |
| `-2`                | 1   | b2  | 2   | b3  | 3   | 4   | b5  | 5   | b6  | 6   | b7  | b1  |
| `-1`                | 1   | b2  | 2   | b3  | 3   | 4   | b5  | 5   | b6  | 6   | b7  | 7   |
| `0`                 | 1   | b2  | 2   | b3  | 3   | 4   | \#4 | 5   | b6  | 6   | b7  | 7   |
| `1`                 | 1   | \#1 | 2   | b3  | 3   | 4   | \#4 | 5   | b6  | 6   | b7  | 7   |
| `2`                 | 1   | \#1 | 2   | b3  | 3   | 4   | \#4 | 5   | \#5 | 6   | b7  | 7   |
| `3`                 | 1   | \#1 | 2   | \#2 | 3   | 4   | \#4 | 5   | \#5 | 6   | b7  | 7   |
| `4`                 | 1   | \#1 | 2   | \#2 | 3   | 4   | \#4 | 5   | \#5 | 6   | \#6 | 7   |

The `enharmonic.center` argument will work the same when translating to
any pitch representation, like
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md).
However, we present the table above in terms of scale degrees because
the *atonal -\> enharmonic* calculation is centered on a key. So, if
`Key` argument is specified, the "enharmonic window" is centered around
that key. So if you are translating to `kern` and the `Key = F#:`, the
output will range from `Gn` to `B#`. If you don't want this, set
`Key = NULL`.

#### Melodic Interpretation of Chromatic Notes

It is very common for chromatic notes in melodic passages to be labeled
based on their melodic contour: i.e., ascending chromatic notes labeled
sharp and descending chromatic notes labeled flat. This behavior can be
engaged by setting the `accidental.melodic` (`logical`, `length == 1`)
argument. When `accidental.melodic = TRUE`, the input is first centered
in the enharmonic window (above), but then any places where a chromatic
alteration proceeds upwards to a non-chromatic note will be altered (if
necessary) to a sharp, and vice verse for a descending notes and flats.
For example, while `kern(0:2)` returns `c("c", "d-", "d")`,
`kern(0:2, parse(accidental.melodic = TRUE))` returns
`c("c", "c#", "d")`.

## See also

All `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
make use of the parsing functionality.

## Examples

``` r
tonalInterval('II#', step.labels =c('I', 'II', 'III','IV','V','VI','VII'))
#> tonalInterval
#> [1] -d7

kern('E x 5', parse(doublesharp = 'x', sep = ' '))
#> **kern (character)
#> [1] ee##
```
