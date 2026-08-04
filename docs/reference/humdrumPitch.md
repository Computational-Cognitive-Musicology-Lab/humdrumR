# humdrumR and pitch

[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
includes a number of intertwined data structures, and associated
functions, for representing and manipulating musical pitch information.

## Tonality

There are four data types extensively used in `humdrumR` to
encode/process [tonal](https://en.wikipedia.org/wiki/Tonality) musical
information:

- [integers](https://rdrr.io/r/base/integer.html) — used to encode
  "[line-of-fifths](https://en.wikipedia.org/wiki/Circle_of_fifths)"
  tonal information

- [tonalInterval](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
  — embeds line-of-fifth tonal integers alongside
  [octave](https://en.wikipedia.org/wiki/Octave) and [cent](NA)
  information to encode most tonal pitch representations (solfege,
  intervals, letternames, etc.)

- [diatonicSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.html)
  — combines line-of-fifth tonal integer representations to represent
  diatonic tonality, including alterations of basic diatonic scale(s).

- [tertianSet](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
  — an extension of `diatonicSet` used to encode
  [tertian](https://en.wikipedia.org/wiki/Tertian) diatonic harmonies.

Users will rarely need to engage with these data types. Rather, users
will work with humdrum data where pitch information is encoded in
strings, and wish to manipulate and analyze such data. The most widely
used `humdrumR` tools are your [pitch conversion/manipulation
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md),
including
[`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md),
and functions like
[`invert()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/invert.md)
and
[`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md).
These functions make use of sophisticated, and flexible pitch
[parsing](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
and
[deparsing](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)
functions, which are the bridge between the "core" pitch representations
listed above and real-world humdrum data.

## Atonality

**THIS SECTION IS INCOMPLETE**

In addition, there are xxx data types used to encode non-tonal (or
[atonal](https://en.wikipedia.org/wiki/Atonality)) pitch information.

- [integers](https://rdrr.io/r/base/integer.html) — used to encode
  [semitones](https://en.wikipedia.org/wiki/Semitone) (as well as
  [MIDI](https://en.wikipedia.org/wiki/MIDI) numbers).

- — sets?

- — 12-tone rows?
