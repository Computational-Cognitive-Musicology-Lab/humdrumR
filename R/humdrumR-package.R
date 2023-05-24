# Package documenation ----

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
#' @importFrom bit64 as.integer64 is.integer64 as.integer64 as.integer64.integer
#' @importFrom data.table data.table rbindlist setorder setorderv setcolorder copy as.data.table is.data.table frank CJ
#' @importFrom numbers primeFactors
#' @importFrom dplyr summarise select filter mutate pull reframe group_by ungroup summarize
#' @importFrom ggplot2 ggplot update_geom_defaults scale_color_gradientn scale_type
#' @importFrom scales ContinuousRange
NULL


#' `humdrumR`'s root directory on your machine.
#'
#' `humdrumRroot` is the path to where the `humdrumR` package is install on your machine.
#' When you installed `humdrumR` a few basic humdrum files were stored here as well, in subdirectories `examples` and `HumdrumData`.
#'
#' @export
humdrumRroot <- system.file(package = 'humdrumR')

withinFields <- list(Exclusive = c('mint', 'hint', 'int'),
                     Key = c('mint', 'hint', 'int', 'sonority'),
                     BPM = c('timestamp'),
                     Tandem = c('tandem'))

# Package global data ----


### groupby ----

byTable  <- rbind(data.table(Argument = 'groupby',  Type = 'melodic',  
                             Function = c('mint', 'delta', 'sigma', 'lag', 'ditto', 'ioi', 'untie', 'hop'), 
                             Expression = list(quote(list(Piece = Piece, Spine = Spine, Path = Path)))),
                  data.table(Argument = 'groupby', Type = 'harmonic',
                             Function = c('hint', 'sonority'),
                             Expression = list(quote(list(Piece = Piece, Record = Record)))),
                  data.table(Argument = 'groupby', Type = 'structure',       
                             Function = c('timeline', 'timestamp', 'count', 'onbeat', 'subpos', 'metlev', 'metcount'),
                             Expression = list(quote(list(Piece = Piece, Spine = Spine, Path = Path, ParentPath = ParentPath, Record = Record, Stop = Stop)))),
                  data.table(Argument = 'orderby', Type = 'harmonic',
                             Function = 'hint',
                             Expression = list(quote(list(Piece = Piece, Record = Record, Spine = Spine, Path = Path, Stop = Stop)))),
                  data.table(Argument = 'meter', Type = 'meter',
                             Function = c('metlev', 'metcount'),
                             Expression = list(quote(TimeSignature)))
                  )

setOldClass('quosure')
setOldClass('quosures')


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




# Options -----

humdrumR_defaults <- list(
  view = 'score',
  maxRecordsPerFile = 40L,
  maxTokenLength = 16L,
  nullPrint = 'NA2dot',
  syntaxHighlight = TRUE,
  censorEmptyRecords = 30L
  
)

humdrumRoptions <- function() options('humdrumR_options')[[1]]
humdrumRoption <- function(name) {
  opts <- humdrumRoptions()
  opts[[pmatch(name[1], names(opts))]]
}

#' Set options for humdrumR!
#' 
#' @export
humdrumR <- function(view, maxRecordsPerFile, maxTokenLength, nullPrint, syntaxHighlight, censorEmptyRecords) {
  curOptions <- oldOptions <- humdrumRoptions()
  

  if (!missing(view)) {
    view <- checks(view, xplegal(c('score', 'table', 'tibble', 'humdrum', 'data.frame')))
    curOptions$view <- match.arg(view, c('score', 'table', 'tibble', 'humdrum', 'data.frame'))
  }
  if (!missing(maxRecordsPerFile)) {
    checks(maxRecordsPerFile, xwholenum & xpositive)
    curOptions$maxRecordsPerFile <- maxRecordsPerFile
  }
  if (!missing(maxTokenLength)) {
    checks(maxTokenLength, xwholenum & xpositive)
    curOptions$maxTokenLength <- maxTokenLength
  }
  if (!missing(nullPrint)) {
    checks(nullPrint, xcharacter & xlen1 & xvalues('NA2dot', 'charNA2dot', 'asis', 'dot2NA'))
    curOptions$nullPrint <- nullPrint
  }
  if (!missing(syntaxHighlight)) {
    checks(syntaxHighlight, xTF)
    curOptions$syntaxHighlight <- syntaxHighlight
  }
  if (!missing(censorEmptyRecords)) {
    checks(censorEmptyRecords, xTF | (xwholenum & xpositive))
    
    if (is.logical(censorEmptyRecords)) censorEmptyRecords <- if (censorEmptyRecords) 30L else Inf
    curOptions$censorEmptyRecords <- censorEmptyRecords
  }
  
  options(humdrumR_options = curOptions)
  
  invisible(oldOptions)
}



# Default settings ----

oldoptions <- options()
# options(conflicts.policy = 'depends.ok')


.onLoad <- function(libname, pkgname) {
  oldoptions <<- options()
  options(#prompt = 'humdrumℝ> ', continue = 'humdrumℝ... ', 
          scipen = 4, digits = 7, humdrumR_options = humdrumR_defaults)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to humdrumℝ!\n', '\tYou are using humdrumℝ version ', 
      as.character(packageVersion('humdrumR')))
}

.onUnload <- function(libpath) {
  cat('Thanks for using humdrumℝ!\n')
  do.call('options', oldoptions)
}

#' @export
humdrumR_version <- as.character(packageVersion('humdrumR'))

## Bootswatch flatly in plots ----

flatly <- c('#18BC9C', '#F39C12', '#3498DB', '#E74C3C', '#2C3E50')
flatly_continuous <- function(n, k = 2, alpha = 1) {
  structure(setalpha(colorRampPalette(flatly[k + 0:1])(n), alpha = alpha),
            name = 'flatly_continuous', class = 'palette')
}
flatly_discrete <- function(n, alpha = 1) {
  structure(setalpha(colorRampPalette(flatly)(n), alpha = alpha), name = 'flatly_discrete', class = 'palette')
}
# setHook('plot.new', function() {
#   par(family = 'Lato', col.main = flatly[5], col.axis = flatly[5], col.sub = flatly[5],
#       col.lab = flatly[5])
#   })




