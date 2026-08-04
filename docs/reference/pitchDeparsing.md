# Generating ("deparsing") pitch representations

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a easy-to-use system for generating a variety of tonal (or
atonal) pitch representations, which can be flexibly modified by users.
"Under the hood" `humdrumR` represents all tonal pitch information using
the [same underlying
representation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md),
which is typically extracted from input data using the [pitch
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
This representation can then be "deparsed" into a variety of predefined
output formats (like `**kern`), or into new formats that you create!

## Details

Deparsing is the second step in the [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
processing pipeline:

- **Input** representation `|>`

  - *Parsing* `|>`

    - **Intermee**
      ([tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md))
      representation `|>`

    - **Transformation** `|>`

  - *Deparsing* (DEPARSING ARGS GO HERE) `|>`

- **Output** representation

Various pitch representations like `**kern`, `**solfa`, and `**semits`
can be generated using predefined [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
like
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)
[`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md),
and
[`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md)
respectively. All of these functions use a common deparsing framework,
and are specified using different combinations of arguments to the
deparser. By modifying these *"deparsing" arguments*, you can exercise
fine control over how you want pitch information to be represented in
your output. *This* documentation talks about this deparsing step. For
an overview of the parsing process, look
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

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
with the `octave.round` argument: see the pitch deparsing documentation.

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

## Deparsing arguments

The following "advanced" deparsing arguments are available (read all the
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

    - `explicitNaturals`

    - `cautionary`

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

Note that the deparsing arguments are similar (sometimes identical) to
parallel [parsing
arguments](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).
These "advanced" arguments can be used directly in *any* [pitch
function](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md):
for example, `kern(x, qualities = TRUE)`.

Each of the `humdrumR` pitch functions is associated with default
deparsing arguments. For example, if you use
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
`flat` is set (by default) to `"-"`. However, if you wanted to print
`**kern`-like pitch data, **except** with a different flat symbol, like
`"_"`, you could modify the deparser: `kern('Eb5', flat = "_")`. This
overrides the default value for `**kern`, so the output would be `"ee_"`
instead of `"ee-"`.

### Steps

All representations of "tonal" pitch information include a
representation of *onic steps*. You can control how the deparser writes
onic steps using the `step.labels` argument. The `step.labels` argument
must be an atomic vector of unique values, with a length which is a
positive multiple of seven. Examples of `step.labels` arguments that are
currently used by `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
include:

- `step.labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G')`

- `step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')`

- `step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't')`

If `step.labels` is `NULL`, steps are assumed printed as integers,
including negative integers representing downward steps.

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
limit on the number of specifiers to write. For example, you could force
all triple sharps (`"###"`) or double sharps (`"##"`) to deparse as just
`"#"`, by specifying `specifier.maximum = 1L`.

#### Accidentals

If `qualities = FALSE` the deparser will print accidentals of three
types: naturals, flats, and sharps. The `natural`, `flat`, and/or
`sharp` (`character`, `length == 1`) arguments can be used to indicate
how accidentals are printed in the output. For example, if set the
`kern('Eb5', flat = 'flat')` you get the output `"eeflat"`.

Examples of accidental argument combinations that are currently used by
`humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
include:

- `(flat = "b", sharp = "#")` -\>
  [`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)

- `(flat = "-", sharp = "#")` -\>
  [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md)

- `(flat = "es", sharp = "is")` -\>
  [`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md)

- `(flat = "-", sharp = "+")` -\>
  [`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md)

The `doubleflat`, and `doublesharp` (`character`, `length == 1`)
arguments are `NULL` by default, but can be set if a special symbol is
wanted to represent two sharps or flats. For example, you could modify
[`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)
to use a special double sharp symbol: `pitch("f##", doublesharp = "x")`
and the output will be `"Fx4"`.

The printing of naturals is controlled by the `natural` argument.
However, by default, the `humdrumR` deparsers don't both printing
naturals. You can force *all* naturals to print my setting the
`explicitNaturals` (`logical`, `length == 1`) argument to `TRUE`. The
exact behavior of `explicitNaturals` depends on the `implicitSpecies`,
`absoluteSpecies`, and `Key` argument (details below).

#### Qualities

If `qualities = TRUE` the deparser will print qualities, of five types:
perfect, minor, major, augmented, and diminished. The `perfect`,
`major`, `minor`, `diminish`, and/or `augment` (`character`,
`length == 1`) arguments can be used to indicate how qualities are
printed in the output. (Note: we are talking about interval/degree
qualities here, not chord qualities!) For example, you can write
`interval(c("g-", "f#"), augment = 'aug', diminish = 'dim')` and the
output `c("+dim5", "+aug4")`. Examples of quality argument combinations
that are currently used by `humdrumR` [pitch
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

In some musical data, it is assume that a accidental on a note "stays in
effect" on that scale step until the next bar, or until a different
accidental replaces it. Fortunately, the `humdrumR` parser
([`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md))
also knows how to parse data encoded with "memory" this way. If
`memory = TRUE`, the accidental (or quality) of each input note is
"remembered" from previous appearances of that scale step. For example,

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
example, in `lilypond` notation, `,` (comma) represents lower octaves,
and `'` (single apostrophe) represents upper octaves. So the default
[`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md)
parser uses these arguments:

- `pitch(c("c", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",", octave.offset = 1))`

  - Returns `c("C2", "C3", "C4")`.

(Note that lilypond makes the octave *below* the central octave, using
`octave.offset = 1`.)

#### Octave "Rounding"

In some situations, pitch data might interpret the "groupby" between
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

When parsing
[intervals](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md),
the `octave.round` option allows you to control how the "simple part"
(less than an octave) of a compound interval is represented. For
example, we might think of an ascending major 12th as being an ascending
octave *plus* an ascending perfect 5th: \*\* +P8 + P5\*\*. **Or** we
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
interpreted *relative* to the previous note, rather than any absolute
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
  is descending! An ascending diminished 5th would be written
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
length 1–3. The characters must [partial
match](https://rdrr.io/r/base/pmatch.html) either `"step"`, `"species"`,
or `"octave"`. The presence of any of these strings in the `parts`
vector indicate that that information should be parsed. The *order* of
the strings indicates in what order the pieces of pitch information are
encoded in input strings.

To illustrate, imagine that we had input data which was identical to a
standard interval representation—e.g., `M2` and `P5`—except the quality
appears *after* the step—e.g., `2M` and `5P`. We could call
`interval(c("2M", "5P"), parse(parts = c("step", "species")))` and sure
enough we'd get the correct parse!

One final string-parsing argument is `sep`, which indicates if there is
a character string separating the pitch information components: The most
common case would be a comma or space. For example, we could use a parse
command like this: `kern("E flat 5", parse(flat = "flat", sep = " "))`.

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

All `humdrumR` [pitch
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
make use of the deparsing functionality.
