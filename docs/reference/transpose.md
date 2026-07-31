# Transpose pitches and keys

This function
[transposes](https://en.wikipedia.org/wiki/Transposition_(music))
pitches or keys by various intervals or to target keys. Inside the box,
inputs and transpositions take place as `tonalInterval`s or
`diatonicSet`s, but any numeric or character string representation of
pitches can be transposed as well. This function is incorporated
directly into [pitch
functions](https://rdrr.io/pkg/humdrumR/man/diatonicSetS4.html), and
thence, all [pitch
translation](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
functions, so you probably won't call it directly very often.

## Usage

``` r
transpose(x, by, Key, to, real, relative, ...)
```

## Arguments

- x:

  \*\*\*The input pitch(es) to transpose. \*\*\*

  Can be a `tonalInterval` or something [intepretable as a pitch
  information](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md).

- by:

  ***Transpose by this interval.***

  Can be a `tonalInterval` or something intepretable as a
  `tonalInterval`.

- Key:

  ***Transpose *from* this key (to the `to` key).***

  Can be a `diatonicSet` or something intepretable as a `diatonicSet`.

  For tonal and/or to transpositions, this is the "from" key. If this
  value is `NULL`, it defaults to C major.

- to:

  ***Transpose *to* this key (from the `Key` key).***

  Can be a `diatonicSet` or something intepretable as a `diatonicSet`.

- real:

  ***Should transposition be real (or tonal)?***

  Defaults to `TRUE`.

  Must be a singleon `logical` value: an on/off switch.

  If `real == FALSE`, transposition is tonal.

- relative:

  ***Should transposition between keys be relative (or parallel)?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  Only relavent if using transposing between keys (`Key` and `to`) with
  different modes. If `relative == FALSE`, transposition is parallel.

## Details

There are two distinct types of transposition (real and tonal). There
are also two different approaches to *specifying* transpositions: "to"
and "by". "To" transpositions can also be either *parallel* or
*relative*.

## Types of Transposition

There are two different types of transposition: **real** transposition
and **tonal** transposition. In *real* transposition, all inputs are
transposed by the same *specific* interval. For example, the pitches
`{C D E F G}` could be transposed up a major second to `{D E F# G A}`.
In *tonal* transposition, inputs are transposed by *generic* intervals,
within a key. For example, the sequence `{C D E F G}`, in the key of C
major, could be translated up a generic second to `{D E F G A}`.

To choose between real and tonal transposition, use the `real` argument:
`real = TRUE` for real transposition, `real = FALSE` for tonal
transposition.

### Alterations

Tonal transposition is complicated by the presence of any alterations in
the input pitches. For instance, if we are given the pitches
`{C F# G D# E}` in the key of C major, how should they by tonally
transposed up a second, within C major? There is not one obvious,
correct answer that can be easily identified. The algorithm implemented
by `humdrumR` is as follows:

1.  Alterations/accidentals in the input are identified. (In this case,
    F# and D#).

2.  The generic pitches are transposed within the key, resulting in
    `{D G A E F}`.

3.  Alterations in the input are added to the output *unless* the
    resulting pitches are interpreted as a comma by a call to
    `tintPartition`, with a given enharmonic wrap value (the default is
    `12`). In this example, adding the first accidental results in
    `{G#}` which is not a comma. However, the second accidental results
    in `{E#}` which *is* a comma away from the natural `{F}`. Thus, this
    accidental is not added to the output, resulting in `{E}`, not
    `{E#}`. The resulting output is `{D G# A E F}`.

The size of `enharmonicWrap` effectively determines how extreme
accidentals are allowed. The default value, `12`, assures that no output
notes are enharmonically equivalent to notes in the key. To further
illustrate, here is the sequence `{C F# G D# E B- A A- G C# D B D- C}`
transposed tonally within C major by all seven possible generic
intervals, with `enharmonicWrap = 12`:

|          |                                 |
|----------|---------------------------------|
| Interval | Output                          |
| Unison   | C F# G D E B- A A- G C D B D- C |
| 2nd      | D G A E F c B B- A D E c E- D   |
| 3rd      | E A B F# G d- c c B E F d F E   |
| 4th      | F B c G A e- d d- c F# G e G F  |
| 5th      | G c d A B f e e- d G A f A- G   |
| 6th      | A d e B c g f f e A B g B- A    |
| 7th      | B e f c d a- g g f B c a c B    |

## Specifying Transpositions

There are two approaches to specifying transpositions, the `by` and `to`
arguments. The `by` argument must be an interval, and the input is
translated by that interval. If the `by` interval is specific but
`real = FALSE`, the input is treated as a generic interval, and
tranposition takes place within the key indicated by the `Key` argument.

The `to` argument translates an input *to* a desired key. For example,
if the input is in the key of E major but we want it transposed to G
major, we could say `to = '*G:'`. If `real = TRUE`, input is simply
translated to the root of the `to` key, with all the exact same
intervals. If `real = FALSE`, the input is translated to the root of the
new key, with its intervals changed to match the new key as well. In
either case, the result depends on what the input's key is, which is
indicated by the
[standard](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
`Key` argument. The `Key` arguments is like the "from" key. If
`Key = NULL`, the input key is interpreted as C major.

Consider the input notes `{D B C A# B, D C# D E D}` in the key of the G
major. If we specify `to = e:, real = TRUE`, the output will be
`{B G# A F## G#, B A# B C# B}`. (Notice that even though the `to` key is
minor, the output is still clearly in E major). If we specify
`to = e:, real = FALSE`, the output will instead be
`{B G A F# G, B A# B C B}`.

Building off the previous example, consider how the input *key* matters
as well. If we use the same input notes (`{D B C A# B D C# D E D}`) but
the input `Key` is C major, then: If we specify `to = e:, real = TRUE`,
the output will be `{F# D# E C## D# F# E# F# G# F#}`. If we specify
`to = e:, real = FALSE`, the output will instead be
`{F# D E C# D F# E F# G F#}`.

If *both* `by` and `to` are specified, the `to` transposition is applied
first, followed by the `by` transposition. If `real = FALSE`, the `by`
transposition happens within the `to` key, not the `Key` key.

### Relative vs Parallel

When transposing to, we have difFerent approaches to determine the
relationship between the "from" key (`Key` argument) and the "to" key
(`to` argument). If we think of "parallel" relationships between keys,
we match the roots of the keys regardless of modes. For instance, C
major and C minor are parallel keys. If we instead think of "relative"
relationships between keys, we match the modes of the keys, not the
roots. For instance, C major and A minor are relative keys. This is
similar to the distinction between "la-based minor" solfege (relative)
vs "fixed-do" solfege (parallel).

When transposing using a `to` argument, if `relative = FALSE`, the input
key (`Key` argument) is transposed to match the *root* of the `to`
argument. For example, if the input key is G minor and the `to` key is C
major, the output is transposed to C minor, the parallel minor of the
`to` key. However, if `relative = TRUE` the input key is transposed to
match the mode of the `to` key: A G minor input with a C major `to`
would be translated to A minor, the relative minor of the `to` key. If
the `Key` (from key) and `to` (to key) arguments have the same mode, the
parallel and relative transpositions are the same.

## Special Operators +-

As a note, real transposition `by` and interval can be achieved more
concisely using the `+` and `-` operators, as long as at least one side
of the operators is an actual `tonalInterval` object. `humdrumR`
preassigns all common tonalIntervals to objects in your global
environment. Thus, you can type commands like `"c#" + M2` to get `d#`,
or `c("C4", "E4", "C5") - m6` to get `"E3" "G#3" "E4"`.

## See also

Other tonal transformations:
[`invert()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/invert.md)
