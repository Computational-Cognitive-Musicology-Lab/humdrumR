# Package index

## Reading and Summarizing Data

These tools allow you to import, validate, and abstractly summarize the
content of humdrum data.

### Reading and Writing

- [`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md)
  : Validate humdrum files

- [`knownInterpretations`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  [`findHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  [`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
  : Find and read humdrum files into R

- [`writeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
  :

  Write [humdrumR
  data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
  to humdrum files

### Summarizing

- [`summary(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md)
  : Summarize humdrumR corpora
- [`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
  [`` `[` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
  [`print(`*`<humCensus>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md)
  : Tabulate records and tokens in a humdrumR corpus
- [`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
  [`` `[`( ``*`<humInterpretations>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
  [`print(`*`<humInterpretations>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md)
  : Summarize humdrum corpus interpretations.
- [`reference()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)
  [`` `[`( ``*`<humReference>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/reference.md)
  : Summarize reference records in a humdrumR corpus
- [`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
  [`` `[`( ``*`<humSpines>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
  [`print(`*`<humSpines>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)
  : Summarize spines in humdrum dataset.

## Working with Data

These pages describe tools used to manipulate the musical data ensconced
within humdrumR data structures.

- [`token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`c(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`rep(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`unique(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`` `[`( ``*`<token>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`show(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`format(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`is.token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`order.token()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`Arith(`*`<token>`*`,`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`diff(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`Arith(`*`<token>`*`,`*`<character>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`Arith(`*`<character>`*`,`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  [`Summary(`*`<token>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/token.md)
  : Humdrum tokens

### Manipulating Humdrum Data

- [`with(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  [`within(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  [`mutate(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  [`summarise(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  [`reframe(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  [`ggplot(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
  :

  Working *with* humdrum data fields

- [`subset(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`filter(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptyFiles()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptyPieces()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptySpines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptyPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptyRecords()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeEmptyStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`removeSubset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`unfilter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  [`complement()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/subset.humdrumR.md)
  : Filter humdrum data

- [`` `[` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/indexHumdrum.md)
  [`` `[[` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/indexHumdrum.md)
  : Indexing humdrumR objects

- [`group_by(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
  [`ungroup(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
  [`groups()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md)
  : Divide humdrumR data into groups

- [`context()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
  [`uncontext()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
  [`windows()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/context.md)
  : Group vectors into contextual windows

## Musical Tools

These pages describe the classes and functions that directly represent
or engage musical/music-theoretic concepts.

### Pitch overviews

These pages overview general concepts in humdrumR pitch:

- [`humdrumPitch`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumPitch.md)
  : humdrumR and pitch
- [`pitchFunctions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchFunctions.md)
  : Translate between pitch representations.
- [`tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchParsing.md)
  : Parsing pitch information
- [`pitchDeparsing`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitchDeparsing.md)
  : Generating ("deparsing") pitch representations
- [`chordFunctions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordFunctions.md)
  : Parsing and deparsing chord information
- [`tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordParsing.md)
  : Parsing chord information
- [`chordDeparsing`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chordDeparsing.md)
  : Generating ("deparsing") chord representations
- [`keyFunctions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyFunctions.md)
  : Parsing and deparsing key information
- [`diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyParsing.md)
  : Parsing key information
- [`keyDeparsing`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/keyDeparsing.md)
  : Generating ("deparsing") key representations
- [`tint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
  [`is.tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
  [`order.tonalInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonalIntervalS4.md)
  : Representation of tonal pitch information
- [`tset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  [`is.tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  [`tertian(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  : Tertian set
- [`diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`dset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`is.diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`order.diatonicSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`` `==`( ``*`<diatonicSet>`*`,`*`<diatonicSet>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`Compare(`*`<diatonicSet>`*`,`*`<diatonicSet>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  [`` `==`( ``*`<tertianSet>`*`,`*`<tertianSet>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/diatonicSetS4.md)
  : Tonal (diatonic) sets

### Pitch functions

These functions translate between pitch representations:

- [`pitch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pitch.md)
  : Scientific pitch representation

- [`degree()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md)
  [`deg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/degree.md)
  :

  Tonal [scale degree](https://en.wikipedia.org/wiki/Degree_(music))
  representation (absolute)

- [`interval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interval.md)
  : Tonal (pitch) interval representation

- [`kern()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/kern.md) :
  Kern pitch representation

- [`solfa()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfa.md)
  :

  Relative-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)
  representation

- [`solfg()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/solfg.md)
  :

  Fixed-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)
  representation

- [`lilypond()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lilypond.md)
  : Lilypond pitch representation

- [`helmholtz()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/helmholtz.md)
  : Helmholtz pitch representation

- [`tonh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tonh.md) :
  German-style pitch notation.

- [`octave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/octave.md)
  : Extract octave.

- [`step()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/step.md) :
  Extract scale step.

- [`accidental()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/accidental.md)
  : Extract accidental from pitch.

- [`quality()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/quality.md)
  : Extract quality from pitch

- [`bhatk()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bhatk.md)
  : Swara representation

- [`semits()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)
  [`midi()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)
  [`cents()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/semits.md)
  : Atonal pitch representations

- [`pc()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pc.md) :
  Representation of atonal pitch classes

- [`freq()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/freq.md) :
  Translate pitches to frequency (Hz)

- [`LO5th()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/LO5th.md)
  : Line of Fifths

- [`transpose()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/transpose.md)
  : Transpose pitches and keys

- [`invert()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/invert.md)
  : Invert or transpose pitches.

- [`int()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md)
  [`mint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md)
  [`hint()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/int.md) :
  Calculate intervals between pitches

- [`is.simple()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.simple.md)
  [`is.generic()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.simple.md)
  : Test the properties of tonal information

- [`gamut()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/gamut.md)
  : Make a pitch gamut

### Key and chord functions

These functions translate between diatonic and tertian representations:

- [`romanNumerals`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/romanNumerals.md)
  : Roman Numeral
- [`key()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/key.md) :
  Humdrum key interpretation
- [`signature()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/signature.md)
  : Humdrum key signature
- [`romanKey()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/romanKey.md)
  : Roman numeral key areas
- [`harm(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harm.md)
  [`roman()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/harm.md)
  : Roman numeral representations of harmony
- [`chord(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md)
  [`harte(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/chord.md)
  : "Pop/Jazz" chord symbols
- [`tset()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  [`is.tertianSet()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  [`tertian(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tertianSetS4.md)
  : Tertian set
- [`figuredBass(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/figuredBass.md)
  : Figured bass representation of harmony
- [`is.major()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.major.md)
  [`is.minor()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/is.major.md)
  : Test the major/minor modality of a set
- [`sonority()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sonority.md)
  : Interpret tertian sonorities from set(s) of notes.
- [`root()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`bass()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`inversion()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`is.major(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`is.minor(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`hasExtension()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`isAltered()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`hasThird()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`hasFifth()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  [`hasSeventh()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/analyzeChords.md)
  : Extract properties of chords
- [`reduceHarmony()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/manipulateChords.md)
  [`rootPosition()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/manipulateChords.md)
  : Manipulate chord data

### Rhythm overviews

These pages overview general concepts in humdrumR pitch:

- [`rhythmFunctions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmFunctions.md)
  : Translate between rhythm representations.
- [`rhythmInterval()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmParsing.md)
  : Parsing rhythm information
- [`rhythmDeparsing`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rhythmDeparsing.md)
  : Generating ("deparsing") rhythm representations
- [`humMeter`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humMeter.md)
  : Tools for analyzing rhythm and meter.

### Duration and Time functions

These functions translate between rhythm representations:

- [`bpm2sec()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bpm2sec.md)
  [`sec2bpm()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bpm2sec.md)
  [`bpm2ms()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bpm2sec.md)
  [`ms2bpm()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/bpm2sec.md)
  : Translate between durations and tempos
- [`recip()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recip.md)
  : Reciprocal representation of duration
- [`duration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
  [`quarters()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duration.md)
  : Numeric (double) representation of durations
- [`grid()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/grid.md)
  [`togrid()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/grid.md)
  [`fromgrid()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/grid.md)
  : Drum-machine grid representation of rhythmic durations.
- [`notehead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/notehead.md)
  : Note value representation of duration
- [`seconds()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)
  [`ms()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md)
  [`dur()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/time.md) :
  Clock-time representations of duration
- [`ioi()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ioi.md)
  [`sumTies()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ioi.md)
  : Sum "connected" durations
- [`timeline()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)
  [`timestamp()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timeline.md)
  : Rhythmic timeline of a piece
- [`timebase()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timebase.md)
  : Represent time on a regular grid
- [`recordDuration()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recordDuration.md)
  : Calculate duration of each record in a corpus

### Beats and Meter

Rhythm and meter analysis functions.

- [`meter()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/meter.md)
  : Musical Meter in humdrumR
- [`duple()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/duple.md)
  : Generate duple meters
- [`tactus()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tactus.md)
  [`measure()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tactus.md)
  : Extract levels from meters
- [`nbeats()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/nbeats.md)
  : Counting beats
- [`tatum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tatum.md)
  : Find common denominator of beats
- [`timecount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
  [`subpos()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
  [`onbeat()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/timecount.md)
  : Count beats or measures
- [`metlev()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/metlev.md)
  [`metcount()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/metlev.md)
  [`metsubpos()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/metlev.md)
  : Count or measure metric position
- [`syncopation()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/syncopation.md)
  : Identify syncopated rhythms

### Lyrics

Lyrics and text functions.

- [`wort()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/wort.md) :
  Paste syllables together into words

## Data Structures

These pages describe the data structures of humdrumR, and how to query
or manipulate them.

- [`is.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
  [`print(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
  :

  `humdrumR` class

- [`tandem()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tandem.md)
  : Get tandem interpretation information from humdrum data

- [`pull_data.table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
  [`pull_data.frame()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
  [`pull_tibble()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
  [`pull(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
  [`` `$`( ``*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
  :

  Extract field(s) from [humdrumR
  data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)

- [`selectedFields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  [`select(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md)
  :

  The "selected" fields of a [humdrumR
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)

- [`as.vector(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  [`as.lines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  [`as.matrix(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  [`as.data.frame(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  [`as.matrices()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  [`as.data.frames()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
  : humdrumR coercion

- [`mergeHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humMerge.md)
  : Merge two (or more) humdrumR datasets

- [`collapseHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
  [`collapseStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
  [`collapsePaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
  [`collapseRecords()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
  : "Collapse" humdrumR data into a field

- [`expandPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expandPaths.md)
  : Expand paths into new spines

- [`cleave()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
  [`pivot_wider(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
  [`cleaveSpines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
  [`cleavePaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
  [`cleaveStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
  : Align data from separate spines into new fields.

- [`rend()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)
  [`pivot_longer(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rend.md)
  : Separate data fields into new spines.

- [`combineFields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/combineFields.md)
  : Combine one or more fields into a new field

- [`cleaveGraceNotes()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleaveGraceNotes.md)
  : "Fold" grace notes into neighbors

- [`unfoldStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/unfoldStops.md)
  : "Unfold" data into multiple stops

- [`nrecord()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`nrow(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`ntoken()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`npieces()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`nfiles()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`length(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`ncol(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`dim(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`is.empty()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`anyMultiPieceFiles()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`anyPaths()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`anyStops()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`anySubcorpora()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  [`namesSubcorpora()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSize.md)
  :

  [humdrumR
  data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
  size and shape

- [`getHumtab()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  [`fields()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  [`names(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
  : Humdrum tables (and their "fields")

- [`is.struct()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/struct.md)
  : struct

## Other Functions

Other useful tools.

### Plotting tools

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md) :
  Visualize data

- [`drawMore()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMore.md)
  :

  Add content to
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
  plots.

- [`drawBeside()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMultiple.md)
  [`drawBelow()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMultiple.md)
  [`drawNothing()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawMultiple.md)
  : Draw multiple plots next to each other.

- [`drawToFile()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/drawToFile.md)
  :

  Export
  [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw.md)
  plots to files.

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_area.md)
  : Draw area plot

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_barplot.md)
  : Draw bar plot

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_density.md)
  : Draw density plot (histogram or contour)

- [`draw_heat()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_heat.md)
  : Draw a "heatmap" of 2d data

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_Qplot.md)
  : Draw quantile plot

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_scatter.md)
  : Draw a scatter plot

- [`draw()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/draw_violins.md)
  : Draw "violin" plot

### Probability/Information theory

- [`varnames()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`` `varnames<-`() ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`print(`*`<distribution>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`sort(`*`<distribution>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`pdist(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`pdist(`*`<data.frame>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`pdist(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  [`table()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
  : Distributions
- [`information`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/information.md)
  : Information theory
- [`count(`*`<default>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
  [`count(`*`<humdrumR>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
  [`pdist()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/count.md)
  : Tabulate and/or cross-tabulate data
- [`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
  [`H()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
  [`xentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
  [`kld()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
  [`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
  : Calculate Entropy or Information Content of variables
- [`entropy_by()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
  [`pentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
  : Calculate point-wise or contextual entropy
- [`mutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
  [`pmutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
  : Calculate mutual information between variables

### Numeric values

- [`rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`` `%R%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`numerator()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`denominator()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`is.rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`is.numeric(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`rank(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`Compare(`*`<rational>`*`,`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`Compare(`*`<rational>`*`,`*`<ANY>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`Compare(`*`<ANY>`*`,`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`Summary(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`prod(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`abs(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`sign(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`max(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`min(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`mean(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`round(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`floor(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`ceiling(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`trunc(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`expand(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`sum(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`cumsum(`*`<rational>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`as.rational()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`fraction()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`as.fraction()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`as.double(`*`<fraction>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  [`as.integer(`*`<fraction>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/rational.md)
  : Rational numbers
- [`expand()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/expand.md)
  : Expand numbers outwards from zero

### Manipulating Vectors

- [`delta()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/delta.md)
  : Lagged differences
- [`ditto()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/ditto.md)
  : Propagate data points to "fill" null data.
- [`lag()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md)
  [`lead()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/lag.md) :
  Shift data within a vector/matrix/data.frame
- [`segments()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/segments.md)
  [`changes()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/segments.md)
  : Identify contiguous segments of data in a vector
- [`sigma()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/sigma.md)
  : Cumulative sum of numeric vector
- [`hop()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/hop.md) :
  Generate regular sequence "along" input
- [`enum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/enum.md) :
  Enumerate vector

### Regular Expressions

[Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression)
are essential tools for working with humdrum data. These functions
provide functionality for working with regular expressions.

- [`` `%~l%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%~i%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%~n%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%~m%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%~%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%!~%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%!~l%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  [`` `%!~i%` ``](https://humdrumR.ccml.gtcmt.gatech.edu/reference/RegexFind.md)
  : Match strings against regular expression
- [`humdrumDispatch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md)
  [`exclusiveDispatch()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md)
  [`makeDispatchDF()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md)
  [`makeHumdrumDispatcher()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md)
  [`print(`*`<humdrumDispatch>`*`)`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumDispatch.md)
  : Regular expression method dispatch and function application
- [`captureRE()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/regexConstruction.md)
  [`captureUniq()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/regexConstruction.md)
  [`orRE()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/regexConstruction.md)
  : Making Regular Expressions
- [`REparser()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/REparser.md)
  [`REparse()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/REparser.md)
  : Parse String Using Regular expressions

### Miscallaneus

- [`humdrumRroot`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
  [`humdrumRdata`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
  [`humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
  : humdrumR
- [`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  : What is "partial matching"?

## Some R basics lessons

Concise descriptions of some important concepts in R coding.

- [`evaluatingExpressions`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/evaluatingExpressions.md)
  : "Evaluating" "Expressions" in "Environments"?
- [`groupingFactors`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md)
  : What are "grouping factors"?
- [`partialMatching`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  : What is "partial matching"?
- [`recycling`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md)
  [`padding`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/recycling.md)
  : What are "recycling" or "padding"?
- [`vectorization`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/vectorization.md)
  : What is "vectorization"?
