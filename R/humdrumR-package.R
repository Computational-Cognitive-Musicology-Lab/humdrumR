#' humdrumR
#'
#' `humdrumR` is a toolkit for the analysis of data encoded in the [humdrum syntax](http://www.humdrum.org/guide/ch05/).
#' The humdrum syntax is an incredibly flexible, and powerful, scheme for encoding musical data.
#' Tens of thousands of musical scores (and other musical data) have been encoded in the humdrum syntax, many available online through repositories such as 
#' [KernScores](http://kern.ccarh.org/).
#' 
#' `humdrumR` is intended as a modernized replacement for the original [humdrum toolkit](http://www.humdrum.org/), leveraging
#' the power of `R` to give us unprecedented power to manipulate and analyze humdrum data using concise, expressive syntax.
#'
#' @section Package design:
#' 
#' The package `humdrumR` has seven main components:
#' 
#' + To represent humdrum data in R, we have the [humdrumR][humdrumR::humdrumRclass] [S4 class](http://adv-r.had.co.nz/S4.html), and it's core component 
#'   the [humdrum table][humdrumR::humTable]. 
#' + To create `humdrumR` data, a sophisticated humdrum data parser: [readHumdrum].
#'   `humdrumR` data can also be written back to humdrum-syntax text files using [writeHumdrum].
#' + To filter `humdrumR` data, we have the [subset.humdrumR()] function, which can also be called 
#'   using `R`'s standard [indexing operators][base::Extract]: `[]` and `[[]]`.
#' + To manipulate and modify `humdrumR` data, we have the [with and within][withinHumdrum] methods for `humdrumR` objects.
#' + To facilitate the development of functions to work with humdrum tokens---which are simple character strings packed with information---, 
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
#' @importFrom data.table data.table rbindlist setorder setorderv setcolorder copy as.data.table is.data.table frank
#' @importFrom lubridate period is.period
NULL


#' `humdrumR`'s root directory on your machine.
#'
#' `humdrumRroot` is the path to where the `humdrumR` package is install on your machine.
#' When you installed `humdrumR` a few basic humdrum files were stored here as well, in subdirectories `examples` and `HumdrumData`.
#'
#' @export
humdrumRroot <- system.file(package = 'humdrumR')

exclusiveFunctions <- c('mint', 'hint', 'int')
keyedFunctions <- c('mint', 'hint', 'int')
melodicBounds <- c('mint', 'delta', 'sigma', 'lag', 'ditto')
harmonicBounds <- c('hint')

setOldClass('quosure')

####### Pitch  ----


#' humdrumR and pitch
#' 
#' [humdrumR][humdrumR::humdrumR] includes a number of intertwined data structures, and associated functions, 
#' for representing and manipulating musical pitch information.
#' 
#' @section Tonality:
#' 
#' There are four data types extensively used in `humdrumR` to encode/process [tonal](https://en.wikipedia.org/wiki/Tonality) musical information:
#' 
#' + [integers][base::integer] --- used to encode "[line-of-fifths](https://en.wikipedia.org/wiki/Circle_of_fifths)" tonal information
#' + [tonalInterval][tonalIntervalS4] --- embeds line-of-fifth tonal integers alongside [octave](https://en.wikipedia.org/wiki/Octave)
#'    and [cent]("https://en.wikipedia.org/wiki/Cent_(music)") information to encode most tonal pitch representations (solfege, intervals, letternames, etc.)
#' + [diatonicSet] --- combines line-of-fifth tonal integer representations to represent diatonic tonality, including alterations of basic diatonic scale(s).
#' + [tertianSet] --- an extension of `diatonicSet` used to encode  [tertian](https://en.wikipedia.org/wiki/Tertian) diatonic harmonies.
#' 
#' Users will rarely need to engage with these data types.
#' Rather, users will work with humdrum data where pitch information is encoded in strings, and wish to manipulate and analyze such data.
#' The most widely used `humdrumR` tools are your [pitch conversion/manipulation functions][pitchFunctions], including [kern()],
#' and functions like [invert()] and [transpose()].
#' These functions make use of sophisticated, and flexible pitch [parsing][pitchParsing] and [deparsing][pitchDeparsing] functions, 
#' which are the bridge between the "core" pitch representations listed above and real-world humdrum data.
#' 
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

# Lessons ----


#' Partial matching
#' 
#' `R` has a very useful functionality called "*partial matching*," where we can
#' match a **incomplete** character string or variable name with a list of options.
#' This is achieved using the base-`R` function [pmatch()], but many `R` functions make use of it,
#' as do *many* `humdrumR` functions.
#' 
#' For example, let's say we have a `data.frame` (let's call it `df`) with three columns: `"Number"`, `"Letter"`, and `"Date"`:
#' 
#' ```{r}
#' df <- data.frame(Number = 1:2, Letter = c('A', 'B'), Date = c("January", "February"))
#' ````
#' 
#' If I want to access the `Number` column, most programming languages would require I write at very least `df$Number`.
#' However, `R` will give me the correct field even if I write `df$Numb`, `df$Num`, or even `df$N`.
#' This is partial matching!
#' The matching happens left-to-right, so as long as I get the beginning of variable right, it will work.
#' 
#' Of course, partial matching only works up to the point that the string matches unambiguously.
#' For example, if added a `Dare` column to `df`, then `df$D` or `df$Da` would return `NULL` because they are ambiguous.
#' You'd need to write at least `Dar` or `Dat` to get the `Dare` and `Date` columns respectively.
#' 
#' 
#' @name partialMatching
NULL
