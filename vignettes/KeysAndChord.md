---
title: Diatonic and Tertian Sets in humdrumR
author: "Nathaniel Condit-Schultz"
date:   "2021-03-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Tonality and Meter in humdrumR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Diatonic Sets

As mentioned in the Pitch and Tonality vignette, a normative diatonic key consists of a set of seven consetutive pitch chroma on the Line of Fifths.

A diatonic set can be orderd either by line-of-fifths position: 

**LoF**  -1  0  1  2  3  4  5
-------- --- -- -- -- -- -- --
Note     F   C  G  D  A  E  B

or in "scale-order," which corresponds to steps of $+2$ (or $-5$) modulo 7.

**LoF**   0  2  4  -1  1  3  5
--------- -- -- -- --- -- -- --
**Note**  C  D  E  F   G  A  B
**Step**  1  2  3  4   5  6  7

# Tertian Sets


The set of seven notes in a diatonic key can be reimagined as a *chord*---a set of notes played at the same time.
Specifically, a full seven-note diatonic chord is refered to as a 13th chord.
However, most chords used in tonal music are subsets of the full diatonic set, in particular three-note *triads*.

When viewing a diatonic set as a chord, we traditionally order the set as a sequence of ascending thirds, corresponding to intervals of $+4$ on the line-of-fifths, modulo 7.
These *tertian* steps are usually not wrapped to the octave, resulting in steps 9, 11, and 13, instead of 2, 4, and 6.

**LoF**    0  4  1  5  2  -1  3
---------- -- -- -- -- -- --- ---
**Note**   C  E  G  B  D  F   A
**Step**   1  3  5  7  9  11  13


There are $2^7=$ 128 possible subsets that can be formed from the full diatonic set.
Of these, the seven possiblities that are built from consecutive tertian steps are theoritically priveledged : i.e., $\{\{1\}$, \{1,3\}, \{1,3,5\}, \{1,3,5,7\}, \{1,3,5,7,9\}, \{1,3,5,7,9,11\}, \{1,3,5,7,9,11,13\}\}$.

A few other possible sets are fairly commonplace in Western theory as well: $\{1,5, 11\}$ ("sus4"), $\{1,3,5,9\}$ ("add9"), $\{1,3,5,13\}$ ("add6"), etc.

