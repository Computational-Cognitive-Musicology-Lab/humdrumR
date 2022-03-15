#' humdrumR
#'
#' `humdrumR` is a toolkit for the analysis of data encoded in the [humdrum syntax](http://www.humdrum.org/guide/ch05/).
#' The humdrum syntax is an incredibly flexible, and powerful, scheme for encoding musical data.
#' Tens of thousands of musical scores (and other musical data) have been encoded in the humdrum syntax, many available online through repositories such as 
#' [KernScores](http://kern.ccarh.org/).
#' 
#' `humdrumR` is intended as a modernized replacement for the original [humdrum toolkit](http://www.humdrum.org/), levaraging
#' the power of R to give us enprecedented power to manipulate and analyze humdrum data using concise, expressive syntax.
#'
#' @section Package design:
#' 
#' The package `humdrumR` has XXX main components:
#' 
#' + To represent humdrum data in R, we have the [humdrumR][humdrumR::humdrumRclass] [S4 class](http://adv-r.had.co.nz/S4.html), and it's core component 
#'   the [humdrum table][humdrumR::humTable]. 
#' + To create `humdrumR` data, a sophisticated humdrum data parser: [readHumdrum].
#'   `humdrumR` data can also be written back to humdrum-syntax text files using [writeHumdrum].
#' + To filter and "index" `humdrumR` data, we have the [filterHumdrum] function, which can also be called in a variety of 
#'   short hands using R's standard [indexing operators][base::Extract]: `[]` and `[[]]`.
#' + To manipulate and modify `humdrumR` data, we have the [with(in)Humdrum][humdrumR::with-in-Humdrum] and [humApply] functions.
#' + A set of ["pipe" operators][humdrumR::humPipe] (`%hum>%`, `%hum<%`, etc.), so that `humdrumR` data can be manipulated and filtered in concise, 
#'   [bash-style pipes](https://en.wikipedia.org/wiki/Vertical_bar#Pipe).
#' + To facilate the development of functions to work with humdrum tokens---which are simple character strings packed with information---, 
#'   a useful API we call our [regular-expression dispatch system][humdrumR::humdrumDispatch].
#' + Several [modules][humdrumPitch] for representing and manipulating musical pitch information, 
#'   including our core [tonalInterval] class to represent tonal pitch.
#' + A [module][humdrumR::humRhythm] for representing and manipulating musical rhythm information, 
#'   with a core [rhythmInterval] class to represent rhythms.
#'
#' @keywords internal
#' @docType package
#' @name humdrumR
#' @importFrom MASS fractions
#' @importFrom combinat permn
#' @importFrom utils combn
#' @importFrom glue glue glue_collapse
#' @importFrom abind abind
#' @importFrom stringr str_count str_detect str_dup str_extract str_match str_pad str_replace str_split str_sub
#' @importFrom stringi stri_enc_detect2 stri_read_raw stri_trans_totitle
#' @importFrom rlang %|% %||%
#' @importFrom data.table data.table rbindlist setorderv setcolorder copy as.data.table is.data.table 
NULL


#' ----------------------------------------------->      NEEDS DOCUMENTATION (A SECTION IS INCOMPLETE)       <------------------------------------------------------
#' `humdrumR`'s root directory on your machine.
#'
#' `humdrumRroot` is the path to where the `humdrumR` package is install on your machine.
#' A few simple humdrum files are stored here.
#' @export
humdrumRroot <- system.file(package = 'humdrumR')


setOldClass('quosure')

####### Pitch  ----


#' humdrumR and pitch
#' 
#' [humdrumR][humdrumR::humdrumR] includes a number of intertwined data structures, and associated functions, for representing and manipulating musical pitch information.
#' 
#' @section Tonality:
#' 
#' There are four data types extensively used in `humdrumR` to encode/process [tonal](https://en.wikipedia.org/wiki/Tonality) musical information:
#' 
#' + [integers][base::integer] --- used to encode "[line-of-fifths]" tonal information
#' + [tonalInterval] --- embeds line-of-fifth tonal integers alongside [octave](https://en.wikipedia.org/wiki/Octave) and [cent]("https://en.wikipedia.org/wiki/Cent_(music)") information to encode most tonal pitch representations (solfege, intervals, letternames, etc.)
#' + [diatonicSet] --- combines line-of-fifth tonal integer representations to represent diatonic tonality, including alterations of basic diatonic scale(s).
#' + [tertianSet] --- an extension of `diatonicSet` used to encode  [tertian](https://en.wikipedia.org/wiki/Tertian) diatonic harmonies.
#' 
#' 
#' For a detailed explanation of the theory and specifics of `humdrumR`'s treatment of tonality, see the *Tonality in humdrumR* vignette.
#' 
#' @section Atonality:
#' 
#' **THIS SECTION IS INCOMPLETE**
#' 
#' In addition, there are xxx data types used to encode non-tonal (or [atonal](https://en.wikipedia.org/wiki/Atonality)) pitch information.
#' 
#' + [integers][base::integer] --- used to encode [semitones](https://en.wikipedia.org/wiki/Semitone) (as well as [MIDI](https://en.wikipedia.org/wiki/MIDI) numbers).
#' + [xxx][xxx] --- sets?
#' + [xxx][xxx] --- 12-tone rows?
#' 
#' @name humdrumPitch
NULL
