###################################################################### ###
# Basic time and tempo stuff #############################################
###################################################################### ###


#' Translate between durations and tempos
#' 
#' Functions for translating between durations (in seconds) and *tempos*---expressed as BPM (beats-per-minute).
#' The "beats" in beats-per-minute are specified using the `unit` argument; 
#' `unit` defaults to `.25` (a quarter-note), as is conventional.
#' 
#' The pairs of functions involving `ms` (milliseconds) and `sec` (seconds), are identical except
#' for the change of scale between seconds and milliseconds.
#' 
#' @param BPM ***The tempo.***
#' 
#' Defaults to `60`.
#' 
#' Must be a number or a `character` string in the format `"MM120"` (for 120 bpm). 
#' 
#' By default, [with(in).humdrumR][withinHumdrum] passes the `BPM` [field][fields()], if present.
#' 
#' @param unit ***The "Beat" in BPM.***
#' 
#' Defaults to a quarter-note.
#'
#' Must be a value which can be interpreted as [rhythmic duration][rhythmParsing].
#'  
#' @family time functions
#' @export
bpm2sec <- function(BPM, unit = .25) {
  if (is.character(BPM)) BPM <- as.numeric(gsub('^MM', '', BPM))
  
  checks(BPM, xpositive) #"The BPM argument must be positive...you can't have a negative tempo!")
  checks(unit, (xnumber | xcharacter) & xlen1)
  
  dur <- duration(unit)
  tempo  <- as.numeric(gsub('\\*?MM', '', BPM))
  
  
  if (any(is.na(dur))) .stop("The unit argument {quotemark(unit[is.na(dur)])} cannot be interpreted as a duration.")
  if (any(is.na(tempo))) .stop("The BPM argument {quotemark(BPM[is.na(tempo)])} cannot be interpreted as a tempo.")
  
  240 * dur / tempo
  
}

#' @rdname bpm2sec
#' @export
sec2bpm <- function(sec, unit  =.25 ) {
  checks(sec, xpositive)
  checks(unit, (xnumber | xcharacter) & xlen1)
  
  dur <- duration(unit)
  if (is.na(dur)) .stop("The unit argument {quotemark(unit)} cannot be interpreted as a duration.")
  
  
  
  240 * unit / sec
  
}

#' @rdname bpm2sec
#' @export
bpm2ms <- function(BPM, unit = .25) bpm2sec(BPM, unit) * 1000

#' @rdname bpm2sec
#' @export
ms2bpm <- function(ms, unit = .25) sec2bpm(ms / 1000, unit)


sec2dur <- function(x, 
                    minutes = FALSE,
                    hours = FALSE,
                    days = FALSE,
                    months = FALSE,
                    years = FALSE,
                    milliseconds = TRUE,
                    trim = TRUE,
                    sep.date = '/', sep.time = ':', sep.decimal = '.') {
  
  secsPerUnit <- rev(cumprod(c(1, 60, 60, 24, 365)))
  
  counts <- outer(x, secsPerUnit, `%/%`)
  
  modulo <- c(365,  24, 60, 60)
  counts[ , -1] <- sweep(counts[ , -1, drop = FALSE], 2, modulo, '%%')
  
  
  units <- c(years | months, days, hours, minutes)
  if (any(!units)) {
    for (j in which(!units)) {
      counts[ , j + 1] <- counts[ , j + 1] + counts[ , j] * modulo[j]
    }
    counts <- sweep(counts, 2, c(units, TRUE), '*')
  }
  
  ##
  counts <- cbind(counts[ , 1, drop = FALSE], 0, counts[ , -1, drop = FALSE])
  if (months) {
    for (j in 3:6) {
      mods <- c(30, 30 * 24, 30 * 24 * 60, 30 * 24 * 3600)
      if (units[j - 1]) {
        counts[ , 2] <- counts[ , 2] + counts[ , j] %/% mods[j - 2]
        counts[ , j] <- counts[ , j] %% mods[j - 2]
        break
      }
    }
  }
  
  ## output format
  counts[] <- format(counts, scientific = FALSE, trim = TRUE)
  counts[counts == '0' & col(counts) < 6L] <- ''
  
  output <- paste0(applyrows(counts[ , 1:3, drop = FALSE], paste, collapse = sep.date),
                   sep.date,
                   applyrows(counts[ , 4:6, drop = FALSE], paste, collapse = sep.time))
  
  if (trim) {
    output <- stringr::str_replace(output, paste0('^', sep.date, '+', sep.time, '+'), sep.time)
    output <- stringr::str_replace(output, paste0('^', sep.date, '{3}'), '')
    output <- stringr::str_replace(output, paste0(sep.date, '+', sep.time, '+', '$'), sep.date)
  }
  
  if (milliseconds) {
    
    milli <-  round((x %% 1) * 1000)
    if (trim) milli[milli == 0] <- ''
    output <- paste0(output, ifelse(milli == '', '', sep.decimal), milli)
  } 
  
  output
}


###################################################################### ###
# Deparsing Rhythm Representations (rint2x) ##############################
###################################################################### ###



## Deparsing (rhythmInterval) documentation ----

#' Generating ("deparsing") rhythm representations
#' 
#' [humdrumR] includes a easy-to-use system for 
#' generating a variety of rhythm (time duration) representations,
#' which can be flexibly modified by users.
#' "Under the hood" `humdrumR` represents all rhythmic duration information as [rational numbers][rational],
#' which is typically extracted from input data using the [rhythm parser][rhythmParsing].
#' This [rational] representation can then be "deparsed" into a variety of predefined output formats (like `**recip`), 
#' or into new formats that you create!
#' 
#' Deparsing is the second step in the [rhythm function][rhythmFunctions] processing pipeline:
#' 
#' + **Input** representation `|>` 
#'   + *Parsing* `|>`
#'     + **Intermediate** ([rational]) representation `|>`
#'   + *Deparsing* (DEPARSING ARGS GO HERE) `|>`
#' +  **Output** representation 
#' 
#' Various rhythm representations like `**recip`, `**dur`, and `**duration` can be generated
#'  using predefined [rhythm functions][rhythmFunctions] like [recip()]
#' [dur()], and [duration()] respectively.
#' All of these functions use a common deparsing framework.
#' *This* documentation talks about this deparsing step.
#' For an overview of the parsing process, look [here][rhythmParsing].
#' 
#' @section Basic rhythm arguments:
#' 
#' Different rhythms share a few standard arguments which control details of the output.
#' The most important is the `scale` argument.
#' 
#' ## Scalar unit 
#' 
#' The `scale` argument is a `numeric` or [rational] value which indicates the reference unit used 
#' for duration values: what is "1" duration?
#' By default, the unit is a "whole note" or duration.
#' By changing the unit, you can rescale your output.
#' For example, a recip value represents a fraction of the unit: e.g., `"2"` equals 1/2 of the unit.
#' If we call `recip('2', scale = 1/16)` this is telling us to get half of a sixteenth: which in this case would be `'32'`.
#' 
#' 
#' 
#' 
#' ## In-place parsing
#' 
#' In humdrum data, character strings are often encoded with multiple pieces of musical information right besides each other:
#' for example, `**recip` data might include tokens like `"4.ee-[`.
#' The `humdrumR` parser (`rhythmInterval`) will automatically "pull out" rhythm information from within strings, if it can find any 
#' using the appropriate known regular expressions.
#' For example, `duration('4.ee-[')` returns `r duration('4.ee-[')`.
#' However, all the pitch functions (like [recip()] and [dur()]) have an option to keep the "extra" information
#' and return the result "in place"---i.e., embedded right where it was found in the input string.
#' This is controlled with the `inPlace` argument, which is `FALSE` by default.
#' So, `duration('4.ee-[', inPlace = TRUE)` will return `r duration('4.ee-[', inPlace = TRUE)`---keeping the `"ee-["`.
#' Note that `inPlace = TRUE` will force functions like `duration`, which normally return `numeric` values, to return `character` strings
#' *if* their input is a `character` string. 
#' 
#' 
#' @seealso All `humdrumR` [rhythm functions][rhythmFunctions] make use of the 
#' deparsing functionality.
#' @name rhythmDeparsing
NULL

## Rhythm deparsers ####

### Symbolic ####

rint2dur <- function(x, sep.time = ':', 
                     sep.date = '/', sep.decimal = '.',
                     BPM = 60,
                     ...) {
  
  secs <- rint2seconds(x, BPM = BPM)
  
  sec2dur(secs, 
          sep.time = sep.time, sep.date = sep.date, sep.decimal = sep.decimal,
          ...)

}

rint2recip <- function(x, sep = '%', ...) {
          #modify this to print 0 and 00
  
          num <- as.numeric(x@Numerator)
          den <- as.numeric(x@Denominator)
          
          
          # Get the sign
          SIGN <- c('-', '', '')[2 + sign(num)]
          num <- abs(num)
          den[num == 0L] <- 1
          
          while (any(doubles <- den <= 1L & den >= .25 & num > 2L & num < 16L & num %% 2 == 0 & num %% 5 != 0)) {
              # if we want dotted "0" "00" or "000" values, we need to do some transformations
              .ifelse(doubles, den / 2, den) -> den
              .ifelse(doubles, num / 2, num) -> num
          }
                 
          #### any dots?
          # dots only apply to powers of 2, up to 00, 0, 1, 2, 4, 8, etc.
          dots <- log(num + 1L, base = 2L) 
          dots <- dots - 1L
          dots[!(dots %in% 1:100  & (log(den, 2) %% 1) == 0) | (num / den) > 16] <- 0L
          # dots[dots < 0] <- 0L 
          # change numerator to reflect dots 
          # (if there are does, the numerator is always one)
          num[dots != 0L] <- 1L
          
          # change denominator to reflect dots
          den[dots != 0L] <- den[dots != 0L] / (2 ^ dots[dots != 0L])
          
          # add in weird "0" (duration) and "00" (longa) and "000" (maxima)
          den[(num / den) %in% c(2, 4, 8)] <- strrep('0', log(num / den, 2)[(num / den) %in% c(2, 4, 8)])
          num[den %in% c('0', '00', '000')] <- '1'
          
          output <- .ifelse(num == 1L, den, .paste(den, sep, num)) 

          .paste(SIGN, output, strrep('.', dots))

}


rint2grid <- function(x, tick = '16', sep = '', 
                      on = 'X', off = 'O', offbeat = TRUE) {
  
  
  x <- x / rhythmInterval(tick)
  
  ontick <- x == round(x)
  
  num <- numerator(round(x))
  
  
  grid <- paste0(ifelse(ontick, on, off), strrep(off, pmax(as.integer64(0), num - 1L)))
  grid[num == 0] <- NA_character_
  
  if (!offbeat) grid[!ontick] <- NA_character_
  
  grid
  
}

###As unicode duration string




rint2notehead <- function(x, ...) {
  
    recip <- rint2recip(x)
    
    recip[recip == '000.'] <- '1%12'
    recip[recip == '000']  <- '1%8'
    recip[recip == '00.']  <- '1%6'
    recip[recip == '00']  <- '1%4'
    recip[recip == '0.']  <- '1%3'
    
    
    # base notation
    REparse(recip,
            res = list(denominator = "[1-9][0-9]*|0{1,2}", 
                       numerator = "(%[0-9]+)?", 
                       dots = '[.]*$'),
            toEnv = TRUE)
    
    
    symbols <- setNames(notehead.unicode$Unicode, notehead.unicode$Recip)
    base <- symbols[denominator]
    
    ##
    
    unknown <- is.na(base) & !is.na(recip)
    primes <- c(3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 
                53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 
                109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 
                173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 
                233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
                293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 
                367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 
                433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 
                499, 503, 509, 521, 523, 541)
    i <- 1L
    divides <- character(length(recip))
    while(any(unknown)) {
        den <- as.integer(denominator)
        
        fitbase <- den > 0 & den %% primes[i] == 0 
        newbase <- den %/% primes[i]
        newbase <- .ifelse(log(newbase, 2) %% 1L == 0,
                          as.character(newbase),
                          '1')
        
        base[fitbase] <- symbols[newbase[fitbase]]
        divides[fitbase] <- paste0(" \U2215", ifelse(newbase[fitbase] == '1', den[fitbase], primes[i]))
        
        unknown <- is.na(base) & !is.na(recip)
        i <- i + 1L
    }
    
    # add multiples
    multiples <- .ifelse(numerator == "", 
                        "",
                        paste0(stringr::str_sub(numerator, start = 2)))
   
    
    # add dots
    dots <- stringr::str_replace_all(dots,
                                     '\\.', 
                                     '\U1D16D\U2009')
    
    result <- paste0(multiples,  base,  divides,  ' ', dots)
    result[recip == '1%0'] <- '.'
    result
}


### Numeric ####


rint2duration <- function(x, ...) {
  as.double(x)
} 


rint2quarters <- function(x, ...) {
  as.double(x) * 4
}

rint2seconds <- function(x, BPM = 60, ...) {
  rint2duration(x) * bpm2sec(BPM) * 4
}


rint2ms <- function(x, BPM = 60, ...) {
  rint2duration(x) * bpm2ms(BPM) * 4
}






###################################################################### ### 
# Parsing Rhythm Representations (x2rint) ################################
###################################################################### ### 

## Parsing (rhythmInterval) documentation ----

#' Parsing rhythm information
#' 
#' [humdrumR] includes a easy-to-use but powerful system for *parsing* rhythm (time duration) information:
#' various basic rhythm representations (including `numeric` and `character`-string 
#' representations) can be "parsed"---read
#' and interpreted by `humdrumR`.
#' For the most part, parsing automatically happens "behind the scenes" whenever you use any humdrumR [rhythm function][rhythmFunctions], 
#' like [recip()], [dur()], or [duration()].
#' 
#' @details 
#' 
#' The underlying parser used by all `humdrumR` [rhythm functions][rhythmFunctions] can be called explicitly using the function `rhythmInterval()`.
#' The `rhythmInterval` parser will attempt to parse any input information into a [ratioanl number][rationa] object.
#' When you use one of the main [rhythm functions][rhythmFunctions], like [recip()] or [dur()], 
#' the input is parsed into a [rational] object, then immediately [deparsed][rhythmDeparsing]
#' to the representation you asked for (e.g., `**recip` or `**dur`).
#' Thus, the underlying pipeline for `humdrumR` [rhythm functions][rhythmFunctions] looks something like:
#' 
#' + **Input** representation (e.g., `**recip` or `**dur`) `|>` 
#'   + *Parsing* (done by `rhythmInterval()`) `|>`
#'     + **Intermediate** ([rational]) representation `|>`
#'   + *Deparsing* `|>`
#' +  **Output** representation (e.g. `**recip` or `**duration`)
#' 
#' *This* documentation talks about the parsing step.
#' For an overview of the "deparsing" process, look [here][rhythmDeparsing].
#' To learn about the "deparsing" of specific representations, [start here][rhythmFunctions] or go straight to the docs for specific functions---
#' for example, call `?recip` to learn about [recip()].
#' 
#' 
#' @section Dispatch:
#' 
#' The rhythm parser (`rhythmInterval()`) is a generic function, meaning it accepts a variety of inputs 
#' and automatically "dispatches" the appropriate method for the input.
#' R's standard `S3` system is used to dispatch for either `numeric` or `character`-string input:
#' Though most rhythmic representations are essentially numbers, several standard 
#' representations included a mix of numeric and non-numeric symbols.
#' Given either a `character` string or a number, `humdrumR` then uses either regular-expression matching or humdrum
#' exclusive interpretation matching to dispatch specific parsing methods.
#' 
#' @section Symbolic Parsing:
#' 
#' Since humdrum data is inherently string-based, all our input data ultimately starts as `character` strings.
#' (This includes character tokens with rhythm information embedded alongside other information; Details below.)
#' The rhythm parser (`rhythmInterval()`) uses a combination of regular-expressions and exclusive interpretations to decide how to 
#' parse an input string.
#' There are three regular-expression patterns for rhythm that `rhythmInterval()` knows how to parse automatically:
#' 
#' | Representation                                                                     | Exclusive                 | Example          |
#' | ---------------------------------------------------------------------------------- | ------------------------: | ---------------: |
#' | [Recip](https://www.humdrum.org/rep/recip/index.html)                              | **recip                   | `4.`             |
#' | [Note values](https://en.wikipedia.org/wiki/Note_value)                            | **notevalue               | `𝅘𝅥 𝅭`          |
#' | [Time durations](https://www.humdrum.org/rep/dur/index.html)                       | **dur                     | `/1.5`           |
#' 
#' ## Exclusive Dispatch
#' 
#' If you call `rhythmInterval()` (or *any* [rhythm function][rhythmFunctions]) on a `character`-string vector, with a non-`NULL` `Exclusive` argument,
#' that `Exclusive` argument will be used to choose the input interpretation you want, based on the "Exclusive" column in the 
#' table above.
#' For example, `seconds(x, Exclusive = 'recip')` will force the parser to interpret `x` as `**recip` data.
#' Similarly, `recip(x, Exclusive = 'dur')` will force the parser to interpret `x` as `**dur` data.
#' If you use any [rhythm function][rhythmFunctions] within a special call to [withinHumdrum],
#' `humdrumR` will automatically pass the `Exclusive` field from the humdrum data to the function---this means, that in most cases, 
#' you don't need to explicitly do anything with the `Exclusive` argument!
#' (If you want this *not* to happen, you need to explicitly specify your own `Exclusive` argument, or `Exclusive = NULL`.)
#' 
#' ## Regex Dispatch
#' 
#' If you call `rhythmInterval()` (or *any* [rhythm function][rhythmFunctions]) on a `character`-string vector, but the `Exclusive` argument is missing
#' or `NULL`, `humdrumR` will instead use regular-expression patterns to select a known interpretation.
#' For example, `seconds('4.')` will automatically recognize that `'4.'` is a `**recip` token, and will interpret the 
#' data accordingly (the output should be `r seconds('4.')`).
#' If there are more than one matches, `humdrumR` will use the longest match, and if they tie, 
#' pick based on the order in the table above (topmost first).
#' 
#' 
#' If there is no match, `rhythmInterval()` (and all other [rhythm function][rhythmFunctions]) return `NA` values.
#' Remember, if `Exclusive` is specified, it overrides the regex-based dispatch, which means that `pitch('4.', Exclusive = 'notevalue')` will 
#' return `NA`, because
#' `'4.'` can't be interpreted as a `**notevalue`.
#' 
#' ### "In place" parsing
#' 
#' In lots of humdrum data, character strings are encoded with multiple pieces of musical information right besides each other:
#' for example, `**kern` data might include tokens like `"4.ee-[`.
#' The `humdrumR` rhythm parser (`rhythmInterval()`) will automatically "pull out" rhythm information from within strings, if it can find any, 
#' using the appropriate known regular expressions.
#' Various [rhythm parsing functions][rhythmFunctions] have an option to keep the original "extra" data, using their `inPlace` argument.
#' 
#' @seealso All `humdrumR` [rhythm functions][rhythmFunctions] make use of the
#'  parsing functionality.
#' @name rhythmParsing
NULL


## Rhythm parsers ####

### Symbolic ####



dur2rint <- function(x, 
                     sep.time = ':', 
                     sep.date = '/', sep.decimal = '\\.',
                     BPM = 60, ...) {
  REparse(x, 
          makeRE.dur(collapse = FALSE, sep.time = ':', sep.date = '/', sep.decimal = '\\.'), 
          toEnv = TRUE)
  
  datetimes <- strsplit(datetime, split = '')
  
  datetimes <- sapply(datetimes, 
                \(dt) {
               
                  
                  time <- dt == sep.time
                  date <- dt == sep.date
                  
                  
                  
                  groups <- if (any(time | date)) cumsum(time | date) else rep(6, length(dt))
                  
                  if (any(time)) {
                    groups <- groups + (6L - (sum(time | date)))
                  }
                  if (any(date)) {
                    groups <- groups + 1 - (3 - sum(time)) * any(time)
                  }
                  
                  as.integer(tapply(dt[!(time | date)],
                                    factor(groups[!(time | date)], 1:6), 
                                    paste, collapse = ''))
                })
  
  rownames(datetimes) <- c('Years', 'Months', 'Days', 'Hours', 'Minutes', 'Seconds')
  datetimes[is.na(datetimes)] <- 0
  
  secsPerUnit <- rev(cumprod(c(1, 60, 60, 24, 30, 365/30)))
  secs <- colSums(sweep(datetimes, 1, secsPerUnit, '*'))
  
  # decimal
  decimal[decimal == ''] <- '0'
  secs <- secs + as.numeric(decimal)
  
  seconds2rint(secs, BPM = BPM, ...)
}

recip2rint <- function(x, grace = FALSE, sep = '%') {
  REparse(x, makeRE.recip(collapse = FALSE), toEnv = TRUE) # makes recip, graceMark1, and graceMark2
  
  
  # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
  recip <- gsub('^000', paste0('1', sep, '8'), recip)
  recip <- gsub('^00',  paste0('1', sep, '4'), recip)
  recip <- gsub('^0',   paste0('1', sep, '2'), recip)
  
  ndots <- stringr::str_count(recip, '\\.')
  recip <- gsub('\\.+', '', recip)
  recip[recip == '1%0'] <- '0'
  
  rational <- as.rational(recip, sep = sep)
  rational[recip != '0'] <- reciprocal(rational[recip != '0'])
  
  
  dots <- 2L ^ ndots
  dotscale <- rational((2L * dots) - 1L, dots)

  rint <- rational * dotscale
  if (is.na(grace) || !grace) {
    graceNotes <- grepl('^[Qq]', graceMark1) | grepl('[Qq]$', graceMark2)
    rint[graceNotes] <- if (is.na(grace)) rational(NA) else rational(0L)
  }
  
  
  rint
  
}

timesignature2rint <- function(x, sep = '/') {
  x <- stringr::str_remove(x, '^\\*?M?')
  
  x <- strsplit(x, split = sep)
  numerator <- sapply(x, '[', 1)
  numerator <- sapply(strsplit(numerator, split = '\\+'), \(n) sum(as.integer(n)))
  
  
  denominator <- as.integer(sapply(x, '[', 2))
  
  rational(numerator, denominator)
}


grid2rint <- function(x, tick = '16', sep = '', on = 'X', off = 'O') {
  
  x <- strsplit(x, split = sep)
  
  lengths(x) * rhythmInterval(tick)
  
}

notehead2rint <- function(x, sep =" \U2215") {
  
  
  
  REparse(x, 
          makeRE.notehead(sep = sep, collapse = FALSE),
          toEnv = TRUE)
  
  # 
  symbols <- setNames(notehead.unicode$Recip, notehead.unicode$Unicode)
  notehead <- symbols[value]
  dots  <- stringr::str_replace_all(dots, '\U1D16D\U2009', '.')
  
  multiplies <- ifelse(multiplies == '', '', paste0('%', multiplies))
  
  recip <- paste0(notehead, multiplies, dots)
  
  rint <- recip2rint(recip)
  
  #
  divides <- as.numeric(gsub('\U2215', '', divides))
  rint <- rint / (divides %|% 1)
  
  rint
}

### Numbers ####

duration2rint <- function(x, ...) as.rational(x) # as.rational(ifelse(x >= 0, x, NA)) 


seconds2rint <- function(x, BPM = 60, ...) {
  duration2rint(x / bpm2sec(BPM))
}

ms2rint <- function(x, BPM = 60, ...) {
  duration2rint(x / bpm2ms(BPM))
}

## Rhythm Parsing Dispatch ######################################


### Parse 2rint generic and methods ####


#' @name rhythmParsing
#' @export 
rhythmInterval <- function(x, ...) UseMethod('rhythmInterval')

#' @rdname rhythmParsing
#' @export 
rhythmInterval.default <- function(x, ...) as.rational(x, ...)


#' @rdname rhythmParsing
#' @export
rhythmInterval.integer <- function(x, ...) as.rational(x)

#' @rdname rhythmParsing
#' @export
rhythmInterval.NULL <- function(x, ...) NULL


#### Numbers ####

#' @rdname rhythmParsing
#' @export
rhythmInterval.numeric <- makeHumdrumDispatcher(list('duration' ,         NA,  duration2rint),
                                                list('seconds'   ,        NA,  seconds2rint),
                                                list('ms'   ,             NA,  ms2rint),
                                                # list('semiqua vers',        NA, semiquavers2rint),
                                                funcName = 'rhythmInterval.numeric',
                                                outputClass = 'rational')

#### Characters ####

#' @rdname rhythmParsing
#' @export
rhythmInterval.character <- makeHumdrumDispatcher(list(c('recip', 'kern', 'harm'), makeRE.recip,         recip2rint),
                                                  list('grid',                     makeRE.grid,          grid2rint),
                                                  list('dur',                      makeRE.dur,           dur2rint),
                                                  list('any',                      makeRE.timeSignature, timesignature2rint),
                                                  list('duration',                 makeRE.double,        duration2rint),
                                                  list('notehead',                makeRE.notehead(),   notehead2rint),
                                                  funcName = 'rhythmInterval.character',
                                                  outputClass = 'rational')

#' @rdname rhythmParsing
#' @export
rhythmInterval.factor <- function(x, Exclusive = NULL, ...) {
  levels <- levels(x)
  
  rints <- rhythmInterval.character(levels, Exclusive = Exclusive, ...)
  
  c(rational(NA), rints)[ifelse(is.na(x), 1L, 1L + as.integer(x))]
}

#' @rdname rhythmParsing
#' @export
rhythmInterval.token <- function(x, Exclusive = NULL, ...) {
  rhythmInterval(x@.Data, Exclusive = Exclusive %||% getExclusive(x), ...)
}

#### setAs rhythmInterval ####

# See "setAs rational"

###################################################################### ###
# Making duration factor levels ##########################################
###################################################################### ###

makeRamut <- function(reference, deparseArgs = list(), deparser) {
  deparseArgs <- local({
    deparseFormals <- formals(deparser)
    # deparseFormals[intersect(names(deparseArgs), names(deparseFormals))] <- deparseArgs[intersect(names(deparseArgs), names(deparseFormals))]
    deparseFormals[names(deparseArgs)] <- deparseArgs
    deparseFormals$x <- deparseFormals$... <- NULL
    deparseFormals
    # lapply(deparseFormals, eval, envir = rlang::new_environment(deparseFormals, environment(deparser)))
  })
  
  reference <- do.call('rhythmInterval', c(list(reference), deparseArgs))
  reference <- reference[!is.na(reference)]
  
  gamut <- unique(reference)
  if (length(gamut) > 1L) {
    num1 <- numerator(gamut) == 1L
    simple <- gamut[num1]
    simple <- c(simple, rational(rep(1L, sum(!num1)), denominator(gamut[!num1])))
    gamut <- sort(unique(c(reference, 
                           harmonicInterpolate(min(simple), max(simple), includeEdges = TRUE, bigFirst = TRUE),
                           harmonicInterpolate(min(simple), max(simple), includeEdges = TRUE, bigFirst = FALSE))))
  }
  
  if (!is.null(deparser)) do.call(deparser, c(list(gamut), deparseArgs)) else gamut
}



set.ramut <- function(token) {
  deparseArgs <- token@Attributes$deparseArgs
  
  levels <- do.call(makeRamut, list(reference = token@.Data, 
                                    deparseArgs = deparseArgs, 
                                    deparser = token@Attributes$deparser))
  
  factor(token@.Data, levels = levels)
}


###################################################################### ### 
# Translating Rhythm Representations (x2y) ###############################
###################################################################### ### 

## Rhythm function documentation ####

rhythmFunctions <- list(Metric  = list(Symbolic = c('recip' = 'reciprocal note values', 'notehead' = 'traditional note-value symbols'),
                                       Numeric = c('duration' = 'Whole notes', 'quarters' = 'quarter notes/crotchets')),
                        Ametric = list(Symbolic = c('dur' = 'durations of time'),
                                       Numeric = c('seconds', 'ms' = 'milliseconds'))
                        )

#' Translate between rhythm representations.
#' 
#' These functions are used to extract and translate between different representations
#' of rhythmic (time duration) information.
#' 
#' @details 
#' 
#' The full list of rhythm functions is:
#' 
#' ```{r echo = FALSE, results = 'asis'}
#' 
#' rfs <- rapply(rhythmFunctions, 
#'                 \(func) paste0('    + [', 
#'                                 ifelse(.names(func) == '', func, paste0(.names(func))), 
#'                                 '()]', ifelse(.names(func) == '', '', paste0(' (', func, ')'))), how = 'list')
#' 
#' rfs <- lapply(rfs, \(top) Map(\(name, rf) paste(c(paste0('  + *', name, ' rhythm representations*'), rf), collapse = '\n'), names(top), top))
#' 
#' rfs <- Map(\(name, l) paste(c(paste0('+ **', name, ' rhythm representations**'), unlist(l)), collapse ='\n'), names(rfs), rfs)
#' cat(unlist(rfs), sep = '\n')
#' 
#' 
#' ```
#' 
#' These rhythm functions all work in similar ways, with similar arguments and functionality.
#' Each function takes an input rhythm (time duration) representation (which can be anything) and outputs
#' *its* own rhythm representation. 
#' For example, [recip()] takes any input representation and outputs `**recip` 
#' ([reciprocal durations](https://www.humdrum.org/rep/recip/index.html)) data.
#' Underneath the hood, the full processing of each function looks like this:
#' 
#' + **Input** representation (e.g., `**recip` or `**dur`) `|>` 
#'   + *Parsing* (done by [rhythmInterval()]) `|>`
#'     + **Intermediate** ([rational]) representation `|>`
#'   + *Deparsing* `|>`
#' +  **Output** representation (e.g. `**recip` or `**duration`) 
#' 
#' 
#' To read the details of the parsing step, read [this][rhythmParsing].
#' To read the details of the "deparsing" step, read [this][rhythmDeparsing].
#' To read more details about each specific function, click on the links in the list above, 
#' or type `?func` in the R command line: for example, `?notehead`.
#' 
#' ## Grace notes
#' 
#' `**recip` and `**kern` data sometime include tokens indicating [grace notes](https://en.wikipedia.org/wiki/Grace_note)---a special
#' category of duration, usually used to indicate "freely" a-metric notes in an otherwise metric context.
#' In humdrum data, grace notes are marked with `"q"` or `"Q"`; `q` should be reserved
#' for tokens with no (other) duration information, while `Q` should be marked along with
#' duration information: for example, `aa-q` or `16aa-Q`.
#' In practice, this distinction is not always made, and is rarely important.
#'
#' By default, the `**recip` parser treats input marked as grace notes as having a duration of zero.
#' However, if you pass a `grace` argument to the [rhythm parser][rhythmParsing], you can control this behavior.
#' If `parse(grace = TRUE)`, grace-note durations (like the `16` in `"16aa-Q"`) are parsed like any other duration.
#' If `grace = NA`, grace-notes return as `NA`.
#' If `grace = FALSE`, the duration returns as zero (the default behavior).
#' 
#' 
#' @param x ***An input  vector.***
#' 
#' The `x` argument can be any ([atomic][base::vector]) vector, or a [rational (rhythmInterval)][rational], or `NULL`.
#'
#' Must be [parsable as rhythm infromation][rhythmParsing].
#' 
#' @param ... ***Arguments passed to the [rhythm parser][rhythmParsing].***
#' 
#' There are also two hidden (advanced) arguments you can specify: `memoize` and `deparse` (see the details below).
#'
#' @param scale ***A `numeric` or [rational] value which is used as the output unit of measurement.***
#'  
#' Defaults to `rational(1, 1)`.
#'  
#' Must be `numeric` or [rational].
#'   
#' @param Exclusive,parseArgs ***An vector of exclusive interpretations and/or an optional list of arguments passed to the [rhythm parser][rhythmParsing].***
#' 
#' Default to `NULL` and an empty `list()` respectively.
#' 
#' `Exclusive` must be `NULL`, or a `character` vector of either length 1 or length(x);
#' `parseArgs` must  be a `list` of named arguments to the [rhythm parser][rhythmParsing].
#' 
#' @param inPlace ***Should non-rhythm information be retained in the output string?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton  `logical` value: an on/off switch.
#'  
# @param grace ***How are grace notes treated?***
#' 
#' Defaults to `FALSE`.
#'
#' A singleton `logical` value, or `NA`. 
#'
#' See "Grace notes" section below.
#' 
#'     
#' 
#' @name rhythmFunctions
#' @seealso To better understand how these functions work, 
#' read about how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
NULL

## Rhythm transform maker ####

rhythmArgCheck <- function(args, callname) {
  argnames <- .names(args)
  
  if ('scale' %in% argnames){
    args$scale <- rhythmInterval(args$scale[1])
    if (is.null(args$scale) || is.na(args$scale)) .stop("In your call to {callname}, your 'scale' argument cannot be parsed by rhythmInterval().")
  }
  if ('unit' %in% argnames){
    args$unit <- rhythmInterval(args$unit[1])
    if (is.null(args$unit) || is.na(args$unit)) .stop("In your call to {callname}, your 'unit' argument cannot be parsed by rhythmInterval().")
  }
  
  
  if ('grace' %in% argnames) {
    checks(args$grace, argname = 'grace', xlen1 & xlogical)
  }
  for (arg in intersect(c('sep', 'on', 'off'), argnames)) {
    checks(args[[arg]], argname = arg, xcharacter & xlen1)
  }
  
  args
}

makeRhythmTransformer <- function(deparser, callname, outputClass = 'character', 
                                  tandem = c('TimeSignature', 'Mensuration', 'BPM'),
                                  extraArgs = list()) {
  # this function will create various rhythm transform functions
  
  autoArgTable <<- rbind(autoArgTable,
                         data.table(Argument = 'Exclusive', Type = 'Exclusive', Function = callname, Expression = list(quote(Exclusive))))
  
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- c(alist(x = , 
                  ... = ), # don't move this! Needs to come before other arguments, otherwise unnamed parse() argument won't work!
            extraArgs,
            alist(parseArgs = list(), 
                  scale = 1, unit = 1,
                  inPlace = FALSE))
  
  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr( {
    
    checks(x, xatomic | xclass('rational'))
    
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    c('args...', 'parseArgs') %<-% specialArgs(rlang::enquos(...), 
                                               parse = parseArgs)
    
    formalArgs <- list(!!!fargcall)
    namedArgs <- formalArgs[.names(formalArgs) %in% .names(as.list(match.call())[-1])]
    namedArgs$scale <- namedArgs$scale %||% formalArgs$scale
    # There are three kinds of arguments: 
    # ... arguments (now in args...), 
    # FORMAL arguments, if specified (now in namedArgs)
    # parseArgs
    # Exclusive
    parseArgs$Exclusive <- parseArgs$Exclusive %||% args...$Exclusive
    
    parseArgs   <- rhythmArgCheck(parseArgs, !!callname)
    deparseArgs <- rhythmArgCheck(c(args..., namedArgs), !!callname)
    
    # memoize % deparse
    memoize <- args...$memoize %||% TRUE
    deparse <- args...$deparse %||% TRUE
    
    ############# #
    ### Parse
    ############# #
    
    unit <- (parseArgs$unit %||% 1L) / (deparseArgs$unit %||% 1L)
    scale <- deparseArgs$scale * unit
    parseArgs$unit <- deparseArgs$unit <- deparseArgs$scale <- NULL
    
    parsedRint <- do(rhythmInterval, 
                     c(list(x), parseArgs), 
                     memoize = memoize, 
                     outputClass = 'rational')
    
    ## scaling
    if (scale != 1L) parsedRint <- parsedRint * scale
    
    deparseArgs <- c(list(parsedRint), deparseArgs)
    output <- if (deparse && is.rational(parsedRint))  do(!!deparser, 
                                                          deparseArgs, 
                                                          memoize = memoize, 
                                                          outputClass = !!outputClass) else parsedRint
    if (deparse && !is.null(output)) {
      dispatch <- attr(parsedRint, 'dispatch')
      
      output <- if (inPlace) {
        rePlace(output, dispatch)
      } else {
        token(output, Exclusive = callname, 
              deparseArgs = deparseArgs[!names(deparseArgs) %in% c('x', 'Exclusive')][-1], 
              factorizer = set.ramut,
              tandem = tandem,
              parser = rhythmInterval,
              deparser = !!deparser)

      }
      
    }
    
    output
    
  })) %class% 'rhythmFunction'
}


### Rhythm functions ####

#' Reciprocal representation of duration
#' 
#' The standard approach to representing conventional note values in humdrum 
#' is the "reciprocal" [**recip](https://www.humdrum.org/rep/recip/index.html).
#' Representation.
#' The `**recip` rhythmic values are often used as a part of `**kern` representation,
#' which also includes [pitch][kern()] information and notation details.
#' 
#' @details 
#' 
#' `**recip` values are literally the reciprocal of a duration value.
#' Since most note values in conventional music notation are simple fractions
#' the reciprocal approach is highly concise and very similar to conventional western notation and terminology.
#' A "quarter note" is represented as the reciprocal of 1/4: simply `"4"`.
#' Full reciprocal fractions can be specified: `"2%3"` to indicate 3/2.
#' The `%` separator can be changed using the `sep` argument.
#' 
#' 
#' As in conventional [note values][notehead()], "dots" can be added after a value to increase the duration by 
#' the ratio of `(2 - (2^{-n}))`, where `n` is the number of dots.
#' (One dot is 3/2; two dots is 7/4; etc.).
#' 
#' @examples
#' 
#' tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
#' 
#' recip(tokens)
#' recip(tokens, parse(grace = NA))
#' recip(tokens, scale = 2)
#' 
#' recip(c(.25, .25, .25, .25, .5, 1))
#' recip(c(.25, .25, .25, .25, .5, 1), parse(unit = '4'))
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' within(B075, subset = Spine > 2,  recip(Token))
#' 
#' with(B075, subset = Spine > 2, recip(Token)) |> table()
#' }
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#' @param sep ***The separator printed between numerator and denominator.***
#'
#' A single `character` string.
#' 
#' Must be a single `character` string.
#' 
#' @inheritParams rhythmFunctions
#' 
#' @name recip
#' @export 
recip.default <- makeRhythmTransformer(rint2recip, 'recip', extraArgs = alist(sep = '%'))
#' Apply to humdrumR data
#' 
#' If `recip()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' ## humdrumR S3 method:
#' humData |> select(Token) |> recip() 
#' humData |> recip(simple = TRUE)
#' humData |> recip(Token, Key = Key)
#' 
#' @rdname recip
#' @export
recip.humdrumR <- humdrumRmethod(recip.default)
#' @rdname recip
#' @export
recip <- humdrumRgeneric(recip.default)

#' Numeric (double) representation of durations
#' 
#' Output is `numeric` (real number).
#' 
#' @examples
#' 
#' tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
#' 
#' duration(tokens)
#' quarters(tokens, parse(grace = NA))
#' quarters(tokens, inPlace = TRUE)
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' 
#' with(B075, subset = Spine > 2, duration(Token)) 
#' with(B075, subset = Spine > 2, quarters(Token)) 
#' }
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' 
#' @family {rhythm functions}
#' @inheritParams rhythmFunctions
#' @name duration
#' @export 
duration.default <- makeRhythmTransformer(rint2duration, 'duration', 'numeric')
#' Apply to humdrumR data
#' 
#' If `duration()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> duration() 
#' humData |> duration(Token)
#' humData |> duration(simple = TRUE)
#' 
#' @rdname duration
#' @export
duration.humdrumR <- humdrumRmethod(duration.default)
#' @rdname duration
#' @export
duration <- humdrumRgeneric(duration.default)

#' @rdname duration
#' @export 
quarters.default <- makeRhythmTransformer(rint2quarters, 'quarters', 'numeric')
#' Apply to humdrumR data
#' 
#' If `quarters()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> quarters() 
#' humData |> quarters(Token)
#' humData |> quarters(simple = TRUE)
#' 
#' @rdname duration
#' @export
quarters.humdrumR <- humdrumRmethod(quarters.default)
#' @rdname duration
#' @export
quarters <- humdrumRgeneric(quarters.default)

#' Note value representation of duration
#' 
#' This function outputs duration information in as traditional [note value](https://en.wikipedia.org/wiki/Note_value).
#' symbols, as in Western notation.
#'
#' @details
#' 
#' Note-value symbols are simply encoded in `character` vectors, since the
#' [unicode character table](https://unicode-table.com/en/blocks/musical-symbols/) includes these musical symbols.
#' Of course, this depends on your system having a unicode font installed and working:
#' the symbols might not show up properly on your machine!
#' In fact, the symbols always print a bit strangely (out of alignment) and can be hard to manipulate
#' like "normal" `character` strings.
#' 
#' The note-value symbols are most useful for making the labels of plots.
#' For example, if you tabulate note values and use [barplot()], you get nice bar labels:
#' 
#' ```
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/Chorales/.*krn')
#' with(chorales, barplot(table(notehead(Token)), cex.names = 2))
#' 
#' ```
#' 
#' @examples 
#' 
#' tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
#' notehead(tokens)
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#'
#' with(B075[[,3:4]], notehead(Token)) |> table()
#' }
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#' @inheritParams rhythmFunctions
#' @name notehead
#' @export 
notehead.default <- makeRhythmTransformer(rint2notehead, 'notehead') 
#' Apply to humdrumR data
#' 
#' If `notehead()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> notehead() 
#' humData |> notehead(Token)
#' humData |> notehead(simple = TRUE)
#' 
#' @rdname notehead
#' @export
notehead.humdrumR <- humdrumRmethod(notehead.default)
#' @rdname notehead
#' @export
notehead <- humdrumRgeneric(notehead.default)

#' Clock-time representations of duration
#'
#' These functions convert duration values to clock-time representations.
#' `seconds()` and `ms()` output `numeric` values.
#' `dur()` outputs a `character` string encoding the humdrum [**dur](https://www.humdrum.org/rep/dur/index.html)
#' representation of time.
#'
#' @details 
#' 
#' These functions require a `BPM` (beats-per-minute) argument to be specified.
#' By default, the value is 60 bpm.
#' 
#' @section dur:
#' 
#' The [**dur](https://www.humdrum.org/rep/dur/index.html) output can be modified to include different clock-time units:
#' The `minutes`, `hours`, `days`, `months`, and `years` arguments are all true/false `logical` arguments, indicating whether
#' to use that unit in the output (all default to `FALSE`).
#' For example, if `minutes = FALSE`, an input of 90 seconds will return `":90"` (90 seconds!), but if
#' `minutes = TRUE`, the output will be `:1:30` (one minute and thirty seconds).
#' 
#' 
#' 
#' @param minutes (`logical`, T/F) Should the `dur` output include minutes?
#' @param hours (`logical`, T/F) Should the `dur` output include hours?
#' @param days (`logical`, T/F) Should the `dur` output include days?
#' @param months (`logical`, T/F) Should the `dur` output include months?
#' @param years (`logical`, T/F) Should the `dur` output include years?
#' 
#' @examples
#' 
#' tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
#' 
#' seconds(tokens)
#' seconds(tokens, BPM = '90')
#' ms(tokens, BPM = '90')
#' dur(tokens)
#' 
#' wholenotes <- c(10, 30, 90, 310, 5000)
#' dur(wholenotes)
#' dur(wholenotes, minutes = TRUE)
#' dur(wholenotes, minutes = TRUE, hours = TRUE)
#' 
#' quarternotes <- c(10, 30, 90, 310, 5000)
#' dur(quarternotes, minutes = TRUE, BPM = '120', parse(unit = '4'))
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' 
#' with(B075[[,3:4]], seconds(Token))
#' }
#' 
#' 
#' @family time functions
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @inheritParams rhythmFunctions
#' @name time
#' @export 
seconds.default <- makeRhythmTransformer(rint2seconds, 'seconds', 'numeric', extraArgs = alist(BPM = '*M60'))
#' Apply to humdrumR data
#' 
#' If `seconds()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> seconds() 
#' humData |> seconds(Token)
#' 
#' @rdname time
#' @export
seconds.humdrumR <- humdrumRmethod(seconds.default)
#' @rdname time
#' @export
seconds <- humdrumRgeneric(seconds.default)

#' @rdname time
#' @export 
ms.default <- makeRhythmTransformer(rint2ms, 'ms', 'numeric', extraArgs = alist(BPM = '*M60'))
#' Apply to humdrumR data
#' 
#' If `ms()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> ms() 
#' humData |> ms(Token)
#' 
#' @rdname time
#' @export
ms.humdrumR <- humdrumRmethod(ms.default)
#' @rdname time
#' @export
ms <- humdrumRgeneric(ms.default)

#' @rdname time
#' @export 
dur.default <- makeRhythmTransformer(rint2dur, 'dur', extraArgs = alist(BPM = '*M60', 
                                                                        minutes = FALSE,
                                                                        hours = FALSE,
                                                                        days = FALSE,
                                                                        months = FALSE,
                                                                        years = FALSE))
#' Apply to humdrumR data
#' 
#' If `dur()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> dur() 
#' humData |> dur(Token)
#' 
#' @rdname time
#' @export
dur.humdrumR <- humdrumRmethod(dur.default)
#' @rdname time
#' @export
dur <- humdrumRgeneric(dur.default)

###################################################################### ###
# Manipulating rhythm intervals ##########################################
###################################################################### ###



## IOI ----

#' Sum "connected" durations
#' 
#' These functions are used to sum (melodically) adjacent rhythmic duration values which are not associated with new onsets/attacks.
#' `ioi()` adds the duration of [rests](https://en.wikipedia.org/wiki/Rest_(music)) to the previous
#' non-rest (onset) duration, to create [interonset intervals](https://en.wikipedia.org/wiki/Time_point#Interonset_interval) (IOIs).
#' `sumTies()` sums [tied](https://en.wikipedia.org/wiki/Tie_(music)) durations.
#' 
#' @details 
#' 
#' 
#' Both functions return "collapsed" durations are as null data tokens. 
#' For example, `sumTies(c('[4a', '4a]', '2g'))` returns `c('2a', '.', '2g')`, with the second (tied) duration null (`"."`).
#' 
#' For interonset intervals, the last duration in a string of durations is undefined---there is a final onset, but no *next* onset, so there
#' can't really be a "interonset" interval.
#' Thus, by default, `ioi()` will return `NA` at the location of the final duration.
#' However, if the `finalOnset` argument is set to `TRUE`, the function will act like there is one additional onset *after* the end of the sequence:
#' the last "IOI" is calculated between the last onset and this fictional "final onset."
#' For example, if we run `ioi(c('4.a','8r', '4.a','8r','2a', '2r'))` the result is `c("2a", ".", "2a", ".", NA, ".")`,
#' with the last onset (`2a`) returning `NA`.
#' However, if we run `ioi(c('4.a','8r', '4.a','8r','2a', '2r'), finalOnset = TRUE)` the result is `c("2a", ".", "2a", ".", "1a", ".")`---the
#' last onset's whole duration through the end is returned!
#' 
#' Non-onsets (rests) that occur *before* the first onset are returned as null.
#' 
#' @param x ***Input rhythm information.***
#' 
#' The `x` argument can be any ([atomic][base::vector]) vector, or `NULL`. 
#' 
#' Must be [parsable as rhythm information][rhythmParsing].
#' 
#' @param onsets ***A `logical` vector denotes the onsets.***
#' 
#' Defaults to a `logical` vector with `TRUE` wherever there are rests, indicated by the presence of an `"r"` character, in the input `x`.
#' 
#' Must be `logical`; must be length `length(x)`.
#' 
#' All durations in `x` where `onsets == FALSE` are added to the previous value where `onsets == TRUE`.
#' 
#' @param finalOnset ***Whether to count the last onset.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the last IOI is computed between the last onset and the end of the input vector.
#' Otherwise, this last IOI is undefined (`NA`).
#' 
#' @param parseArgs ***An optional list of arguments passed to the [rhythm parser][rhythmParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [rhythm parser][rhythmParsing].
#' 
#' @param groupby ***A `list` of vectors to group `x`.***
#' 
#' Defaults to `list()`.
#' 
#' Must be a `list`; every element of the list must be length `length(x)`.
#' 
#' @param inPlace ***Should non-rhythm information be retained in the output string?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton  `logical` value: an on/off switch.
#'        
#' 
#' @examples 
#' tokens_withrests <- c('2c', '4d', '4e', '4r', '8f','8d#', '4r', '4e')
#' 
#' ioi(tokens_withrests)
#' ioi(tokens_withrests, finalOnset = TRUE)
#'
#' tokens_withties <- c('2c', '4d', '[4e', '4e]', '8f','[8d#', '4d#]', '4e')
#' 
#' sumTies(tokens_withties) 
#' 
#' @name ioi
#' @export 
ioi.default <- function(x, onsets = !grepl('r', x) & !is.na(x) & x != '.', ..., 
                        finalOnset = FALSE,
                        groupby = list(), parseArgs = list(), Exclusive = NULL,
                        inPlace = TRUE) {
  
  checks(onsets, xlogical & xmatch(x))
  checks(finalOnset, xTF)
  checks(inPlace, xTF)
  if (!is.list(groupby)) groupby <- list(groupby)
  
  na <- is.na(x) | x == '.'
  bounds <- (if (length(groupby)) do.call('changes', groupby) else seq_along(x) == 1)[!na]
  
  rint <- do.call('rhythmInterval', c(list(x[!na], Exclusive = Exclusive[!na]), parseArgs))
  dispatch <- attr(rint, 'dispatch')
  
  onsets <- onsets[!na]
  if (any(!onsets)) {
    # duration <- rint2duration(rint)
    
    newdur <- diff(c(cumsum(c(rational(0), head(rint, -1)))[which(onsets | bounds)],
                     sum(rint)))
    newdur <- newdur[which(onsets | bounds) %in% which(onsets)]
    
    rint[!onsets] <- rational(NA)
    rint[onsets] <- newdur
    # rint <- duration2rint(duration)
  }
  
  output <- reParse(rint, dispatch, reParsers = c('recip', 'duration', 'notehead'), ...)
  
  if (inPlace) {
    output <- rePlace(as.character(output), dispatch) 
    output[!onsets] <- '.'
  } else {
    
    output[!onsets] <- NA 
  }
  
  if (!finalOnset) {
    if (length(groupby)) {
      maxes <- unlist(tapply(seq_along(x)[onsets], lapply(groupby, '[', onsets), max))
      output[maxes] <- NA
    } else {
      output[max(which(onsets))] <- NA
    }
  }

  humdrumRattr(output) <- list(dispatch = NULL)
  
  x[!na] <- output
  x
  
}
#' Apply to humdrumR data
#' 
#' If `ioi()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> ioi() 
#' humData |> ioi(Token)
#' 
#' @rdname ioi
#' @export
ioi.humdrumR <- humdrumRmethod(ioi.default)
#' @rdname ioi
#' @export
ioi <- humdrumRgeneric(ioi.default)

#' @param open ***How are the beginnings of ties indicated in `x`?***
#' 
#' Defaults to `[`.
#' 
#' Must be a single, non-empty `character` string, interpreted as a regular expression, or a `logical` of the same length as `x`.
#'
#' @param close ***How are the ends of ties indicated in `x`?***
#' 
#' Defaults to `]`.
#' 
#' Must be a single, non-empty `character` string, interpreted as a regular expression, or a `logical` of the same length as `x`.
#' 
#' @rdname ioi
#' @export 
sumTies.default <- function(x, open = '[', close = ']', ..., 
                          groupby = list(), 
                          inPlace = TRUE) {
  checks(inPlace, xTF)
  checks(open, (xcharnotempty & xlen1) | (xlogical & xmatch(x)))
  checks(close, (xcharnotempty & xlen1) | (xlogical & xmatch(x)))
  if (!is.list(groupby)) groupby <- list(groupby)
  
  na <- is.na(x) | x == '.'
  
  rint <- rhythmInterval(x[!na], ...)
  dispatch <- attr(rint, 'dispatch')
  
  openl <- grepl('[', x, fixed = TRUE)
  closel <- grepl(']', x, fixed = TRUE)
  
  opencloses <- checkOpenClosePairs(openl, closel, groupby, 'sumTies', 
                                    if (is.character(open) && is.character(close)) c(open, close))
  
  openl <- openl[!na]
  closel <- closel[!na]
  
  cumrint <- head(cumsum(c(rational(0), rint)), -1L)
  
  newrint <- rational(rep(NA, sum(!na)))
  newrint[openl] <- (cumrint[closel] - cumrint[openl]) + rint[closel]
  newrint[opencloses$outside[!na]] <- rint[opencloses$outside[!na]]
  rint <- newrint

  output <- reParse(rint, dispatch, reParsers = c('recip', 'duration', 'notehead'), ...)
  
  if (inPlace) {
    output <- rePlace(as.character(output), dispatch)
    if (is.character(open)) output <- stringr::str_remove(output,
                                                          if (open %in% c('[', ']', '(', ')')) paste0('\\', open) else open)
    output[is.na(output)] <- '.'
  } 
  humdrumRattr(output) <- list(dispatch = NULL)

  x[!na] <- output
  
  x
}
#' Apply to humdrumR data
#' 
#' If `sumTies()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> sumTies() 
#' humData |> sumTies(Token)
#' 
#' @rdname ioi
#' @export
sumTies.humdrumR <- humdrumRmethod(sumTies.default)
#' @rdname ioi
#' @export
sumTies <- humdrumRgeneric(sumTies.default)


# minutes <- function(seconds, format = TRUE) {
#   
#   sign <- ifelse(seconds >= 0, '', '-')
#   seconds <- abs(seconds)
#   
#   minutes <- seconds %/% 60
#   
#   seconds <- round(seconds %% 60, 3)
#   paste0(sign, minutes, ':', ifelse(seconds >= 10, '', '0'), format(seconds, nsmall = 3L, trim = TRUE))
# }

## Timelines ----

# Calculate overall duration of a group
# 
# `localDuration()` calculates the "overall" duration within groups in an input vector.
# What the hell does that mean?
# Usually, it is used to find the duration of each *record* in a humdrum file.
# 
# @details 
# 
# The way rhythm and time are typically encoded in humdrum format, the "overall" duration of
# a record is determined by the shortest duration in the record, if there are any.
# So, if we have a file like this:
# 
# ```
# **kern  **kern     **silbe
#     4c      8g        Hum-
#      .      8f           _
#      !       !    !melisma  
#     8b      8f       -drum
#     8c      8e           _
#     2d      4a         da-
#      .       .           .
#      .      4g         -ta
#     G;      g;         ooh
#     *-      *-          *-     
# ```
#
# The "local" duration of each record would be (in `**recip`):
# 
# ```
# **kern  **kern     **silbe   -> 1%0
#     4c      8g        Hum-   -> 8
#      .      8f           _   -> 8
#      !       !    !melisma   -> 1%0
#     8b      8f       -drum   -> 8
#     8c      8e           _   -> 8
#     2d      4a         da-   -> 4
#      .       .           .   -> 1%0
#      .      4g         -ta   -> 4
#     G;      g;         ooh   -> 1%0
#     *-      *-          *-   -> 1%0
# ```
#
# Note that some records are length zero (`1%0`), because they are missing any duration information.
# (In this example we are showing durations of `1%0` for comment, interpretation, and null data records. In most cases, we'd 
# be doing `within(humData, dataTypes ='D')`, which is the default behavior, so these records wouldn't be counted at all.)
# 
# `localDuration()` begins with a call to [duration()] on the input argument `x`---the `parseArgs()` argument can be used to pass arguments to the [parser][rhythmParsing] (the `Exclusive` argument is passed as well).
# `localDuration()` then groups the durations based on unique combinations of values in the `groupby` argument, which must be a list of
# vectors that are the same length as `x`.
# By default, the minimum duration within each group is returned, recycled as necassary to match the input length.
# The `choose` argument can be set to another function, if desired.
# For example, you could use `localDuration(x, choose = max)` to find the *maximum* duration in each group.
# If the `groupby` argument is empty (the default) the durations are returned unchanged, except that `NA` durations are set to `0`.
# Luckily, if `localDuration()` is used inside a [with(in).humdrumR][withinHumdrum] expression, the `groupby = list(Piece, Record)` is *automatically*
# passed (this can be overridden by explicitely setting the argument).
# This means that `with(humData, localDuration(Token))` will automatically calculate the minimum duration of each record.
# 
# Note that, `localDuration()` follows the default behavior of [duration()] by treating grace-notes as duration `0`.
# If you want to use the duration(s) of grace notes, specify `grace = TRUE`.
#
# The output representation can be controlled using the `deparser` argument, defaulting to [duration()].
# For example, `deparser = recip` will return the output in `**recip` format.
# `...` arguments are passed to the deparser.
# 
# @examples 
# 
# humData <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/.*krn")
# 
# within(humData, localDuration(Token))
#
# @param x An input vector which is parsed for duration information using the [rhythm parser][rhythmParsing].
# @param choose A function which takes a vector of `numeric` and returns a single `numeric` value. Defaults to `min`; `max`, `median`, or `mode` might be reasonable alternatives.
# @param deparser A [rhythm function][rhythmFunction] to generate the output representation.
# @param parseArgs A `list` of arguments to pass to the [rhythm parser][rhythmInterval()].
# @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x`.
localDuration <- function(x, choose = min, deparser = duration, ..., Exclusive = NULL, parseArgs = list(), groupby = list()) {
  
  checks(choose, xclass('function'))
  checks(deparser, xnull || xclass('rhythmFunction'))
  
  durations <- do.call('duration', c(list(x, Exclusive = Exclusive), parseArgs))
  
  durations[is.na(durations) | durations == 0L] <- NA
  
  if (length(groupby)) {
    groupby <- checkWindows(durations, groupby)
    groups <- do.call('paste', groupby)
    picks <- tapply(durations[!is.na(durations)], groups[!is.na(durations)], choose) 
    durations <- picks[match(groups, names(picks))] %<-dim% NULL
  } 
  durations[is.na(durations)] <- 0
  
  deparser(durations, ...)
}

#' Rhythmic timeline of a piece
#' 
#' These functions calculate the ammount of time (either in beats, or seconds)
#' that have unfolded since the beginning of a piece, giving a sense of the timeline in which events unfold.
#' In `music21` this information is described as "offsets"---however,
#' we prefer to reserve the words "onset" and "offset" to refer
#' to the beginning (attack) and end (release) of rhythmic events.
#'
#' @details 
#'
#' Music unfolds over time, and humdrum data typically represents this 
#' by placing simultaneous events in the same record, with successive events
#' in ever higher records---progressing "top down" through the file.
#' In some humdrum data, only this (implicit) ordering of data over time is present.
#' The `Record` and `DataRecord` [fields][fields()] capture this ordering in all data parsed by `humdrumR`.
#' However, many (probably most) humdrum data files contain at least some information about the relative 
#' duration of events, representing more detailed information about timing and rhythm.
#' 
#' `timeline()` parses and input vector `x` as [durations][duration()],
#' computes the [cumulative sum][sigma()] of the durations, with the `start` argument appended to the beginning.
#' The result is a `numeric` vector representing the total duration since the beginning of the vector (plus the value of `start`, which defaults to zero).
#' The cumulative durations of `timeline()` represent musical duration units, where `1` equals a whole note.
#' `timestamp()` converts these durations to seconds, either using the `BPM` argument/field to determine the tempo or using the
#' default tempo of 60 beats per minute.
#' If `minutes == TRUE`, the output is formatted into `"minute:seconds.milliseconds"` character strings.
#'
#' If a `groupby` argument is provided, [localDuration()] is used to compute the minimum durations in each group before 
#' computing the cumulative sum only with unique values from each `Record` in the `groupby`.
#' By default, [with(in).humdrumR][withinHumdrum] will automatically pass `groupby = list(Piece = Piece, Record = Record)`
#' into calls to `timeline()` or `timestamp()`.
#' Thus, a call like `within(humData, timeline(Token))` will compute the correct timeline position for *all*
#' tokens across all spines/paths/stops---all values in the same record will be the same.
#' 
#' 
#' Note that, `timeline()` and `timestamp()` follow the default behavior of [duration()] by treating grace-notes as duration `0`.
#' This means that their position on the timeline is simply inherited from the previous event on the timeline, as if they occur
#' at the same time.
#' If you want to use the specified duration(s) of grace notes, specify `grace = TRUE`.
#' By default, any *other* tokens without (parsable) rhythm information are returned a `NA`.
#' However, if `threadNA = FALSE`, rhythm-less tokens will be treated as if they have a duration of `0` as well, and thus
#' have a (shared) position on the timeline.
#' 
#' @section Pickups:
#' 
#' Another option is to pass the `pickup` argument a logical vector of the same length as the input `x`.
#' Within each piece/group, any block of `TRUE` values at the *beginning* of the `pickup` vector 
#' indicate a pickup.
#' The *first* index where the `pickup` logical is `FALSE` is used as the starting point of the timeline/timecount;
#' All the earlier (`pickup == TRUE`) points will be negative numbers, measured backwards from the start index.
#' In `humdrumR`, and datapoints before the first barline record (`=`) are labeled `Bar == 0` in the `Bar` [field][fields()].
#' Thus, a common use for the `pickup` argument is `within(humData, timeline(Token, pickup = Bar < 1)`, which makes the downbeat of
#' the first complete bar `1` the starting point of the timeline---any notes in pickup bars are negative on the timeline.
#' 
#' 
#' @param x ***Input rhythm information.***
#'
#' Must be `atomic`, or `NULL`.
#'
#' Is [parsed as duration information][rhythmParsing].
#' 
#' @param start ***Where does timeline begin?***
#'
#' Defaults to `0`.
#' 
#' Must be a single number.
#' 
#' @param pickup ***Where is there a pickup (anacrusis)?***
#' 
#' Defaults to `NULL`
#' 
#' Must be `logical` of same `length(x)`, Or `NULL`.
#' See "Pickups" section below.
#' 
#' @param minutes ***Should minutes be counted in output?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, output seconds are converted to a character string encoding minutes, seconds, and milliseconds in the format `MM.SS.ms`. 
#' 
#' @param BPM ***The tempo.***
#' 
#' Defaults to `60`.
#' 
#' Must be a single number or a `character` string in the format `"MM120"` (for 120 bpm). 
#' 
#' By default, [with(in).humdrumR][withinHumdrum] passes the `BPM` [field][fields()], if present.
#' 
#' @param groupby ***A `list` of vectors to group `x`.***
#' 
#' Defaults to `list()`.
#' 
#' Must be a `list`; every element of the list must be length `length(x)`.
#' 
#' To function as a by-record timeline, the `groupby` list music include a *named* `Piece` and `Record` fields.
#' Luckily, these are automatically passed by [with(in).humdrumR][withinHumdrum], so you won't need to worry about it!
#'
#' @param threadNA ***Should rhythm-less tokens return `NA`?***
#'
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' @param parseArgs ***An optional list of arguments passed to the [rhythm parser][rhythmParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [rhythm parser][rhythmParsing].
#' 
#' @examples 
#' tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')
#' 
#' timeline(tokens)
#' timestamp(tokens, BPM = '90')
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_0._a.krn")
#' within(B075, timeline(Token))
#' }
#' 
#' @seealso {The [timecount()] and [metcount()] functions provide "higher level" musical interpretations of timeline information.}   
#' @family rhythm analysis tools
#' @name timeline
#' @export 
timeline.default <- function(x, start = 0, pickup = NULL, ..., 
                             Exclusive = NULL, threadNA = TRUE, parseArgs = list(), groupby = list()) {
  
  rints <- do('rhythmInterval', c(list(x, Exclusive = Exclusive), parseArgs))
  
  timerints <- pathSigma(rints, groupby = groupby, start = start, pickup = pickup, threadNA = threadNA, callname = 'timeline')
  
  rint2duration(timerints, ...)
  
}
#' Apply to humdrumR data
#' 
#' If `timeline()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> timeline() 
#' humData |> timeline(Token)
#' 
#' @rdname timeline
#' @export
timeline.humdrumR <- humdrumRmethod(timeline.default)
#' @rdname timeline
#' @export
timeline <- humdrumRgeneric(timeline.default)


#' @rdname timeline
#' @export 
timestamp.default <- function(x, BPM = 60, start = 0, pickup = NULL, minutes = TRUE, ..., 
                              Exclusive = NULL, threadNA = TRUE, parseArgs = list(), groupby = list()) {
  
  rints <- do('rhythmInterval', c(list(x, Exclusive = Exclusive), parseArgs))
  seconds <- rint2seconds(rints, BPM = BPM)
  rints <- as.rational(seconds)
  timerints <- pathSigma(rints, groupby = groupby, start = start, pickup = pickup, threadNA = threadNA, callname = 'timestamp')
  
  rint2dur(timerints, BPM = 240, minutes = minutes, ...) # BPM has already been incorporated, 240 is value we need now.
  
  
}
#' Apply to humdrumR data
#' 
#' If `timestamp()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> timestamp() 
#' humData |> timestamp(Token)
#' 
#' @rdname timeline
#' @export
timestamp.humdrumR <- humdrumRmethod(timestamp.default)
#' @rdname timeline
#' @export
timestamp <- humdrumRgeneric(timestamp.default)

pathSigma <- function(rints, groupby, start, pickup, threadNA = TRUE, callname) {
  # this does most of work for timestamp and timeline

  start <- rhythmInterval(start)
  
  na <- is.na(rints)
  rints[na] <- rational(0L)
  
  fractions <- match_fraction(numerator(c(start, rints)), denominator(c(start, rints)))
  
  start <- fractions$Numerator[1]
  
  .SD <- structureTab(Numerator = fractions$Numerator[-1L], groupby = groupby)
  
  .SD[Stop == 1L, Time := sigma.default(c(as.integer64(0L), head(Numerator, -1L))), by = list(Piece, Spine, Path)]
  
  .SD[ , Time := Time + start]
  
  # make empty events fill from PREVIOUS event
  .SD$Time[.SD$Numerator == 0L] <- ditto(.SD$Time, null = .SD$Numerator == 0L)[.SD$Numerator == 0L]
  
  .SD[ , Time := ditto.default(Time, null = Stop > 1L, groupby = list(Piece, Spine, Path))]
  
  
  if (!is.null(pickup)) {
    .SD$Pickup <- pickup
    .SD[ , Time := {
      if (all(!Pickup, na.rm = TRUE)) Time else Time - Time[which(!Pickup)[1]]
      }, by = list(Piece, Spine, Path)]
  }
  
  
  # .SD$Time
  if (threadNA) .SD$Time[na] <- NA_integer64_
  
  rational(.SD$Time, fractions$Denominator)
}


## recordDuration

#' Calculate duration of each record in a corpus
#' 
#' @param humdrumR ***HumdrumR data.***
#'
#' @export
recordDuration <- function(humdrumR) {
  checks(humdrumR, xhumdrumR)
  
  
  humdrumR <- .recordDuration(humdrumR)
  
  humdrumR@Humtable[ , ..Timeline.. := NULL]
  humdrumR <- updateFields(humdrumR)
  
  humdrumR 
}

.recordDuration <- function(humdrumR) {
  
  selectedFields <- selectedFields(humdrumR)
  humtab <- getHumtab(humdrumR, 'LIMDd')
  
  humdrumR <- within(humdrumR, ..Timeline.. <- timeline(.))
  humdrumR <- selectFields(humdrumR, selectedFields)
  
  humdrumR <- within(humdrumR, dataTypes = c('Dd'),
                     fill = ..Timeline.. <- max(c(-1, ..Timeline..), na.rm = TRUE), by = list(File, Record))
  
  within(humdrumR, RecordDuration <- {
    
    tl <- sort(unique(..Timeline..[..Timeline.. != -1]))
    durs <- diff(sort(unique(..Timeline..[..Timeline.. != -1])))
    durs <- durs[match(..Timeline.., tl)]
    durs[is.na(durs)] <- 0
    durs
    
  }, by = File, dataTypes = 'LIMDd')
  
}

## Timebase

#' Represent time on a regular grid
#' 
#' The `timebase()` function takes a [humdrumR][humdrumRclass] dataset
#' and converts rhythmic information in the data into a step-sequencer like
#' representation, with each humdrum data record representing one step.
#' The duration of each step is the the "timebase", which can be controlled with the `tb` argument.
#' The `timebase()` function is currently in a beta-draft, so may not work well.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' This data must have at least one spine with rhythmic ([duration][rhythmFunctions]) encoded.
#' 
#' @param timebase ***The duration of each step in the output sequence.***
#' 
#' Defaults to a sixteenth-note.
#' 
#' Must be a single atomic value, which can be [parsed as a duration][rhythmFunctions].
#' 
#' @export
timebase <- function(humdrumR, tb = '16') {
  checks(humdrumR, xhumdrumR)
  message('This is an early draft of timebase()...you might encounter errors.')
  
  selectedFields <- selectedFields(humdruamR)
  
  tb <- if (is.null(tb)) with(humdrumR, tatum(., deparser = duration)) else duration(tb)
  
  humdrumR <- .recordDuration(humdrumR)
  
  humtab <- getHumtab(humdrumR, 'LIMDd')
  
  humtab[ , RecordDuration := as.integer(floor(RecordDuration / tb))]
  # remove records that dont line up with tb
  humtab <- humtab[is.na(..Timeline..) | (is.whole(..Timeline.. / tb) & ..Timeline.. > -1L)]
  
  humtab[ , Nrep := ifelse(RecordDuration == 0 & Type != 'd', 1, RecordDuration)]
  # humtab <- humtab[Nrep > 0]
  # humtab$.tatum.[humtab.]
  
  humtab <- humtab[rep(seq_len(nrow(humtab)), humtab$Nrep)]
  
  humtab[ , Duplicated := duplicated(Record), by = list(File, Spine)]
  
  tb <- paste0('*tb', recip(tb))
  humtab <- rbind(humtab[!Type %in% c('D', 'd')],
                         humtab[Type %in% c('D', 'd'), {
                           firstrow <- .SD[1]
                           firstrow$Token <- tb
                           firstrow$Type <- 'I'
                           rbind(firstrow, .SD)
                           
                         }, by = list(File, Spine)])
  humtab <- orderHumtab(humtab)
  humtab[ , Record := seq_along(Token), by = list(File, Spine)]
  
  for (field in fields(humdrumR, 'D')$Name) humtab[[field]][humtab$Duplicated] <- NA
  humtab$Type[humtab$Duplicated] <- 'd'
  
  putHumtab(humdrumR) <- humtab
  humdrumR@Humtable[ , c('..Timeline..', 'RecordDuration') := NULL]
  humdrumR <- updateFields(humdrumR)
  
  selectFields(humdrumR, selectedFields)
 
}

## Find lag ----

findLag2 <- function(x, lag = 1, minlag = 0, maxlag = Inf, prefer = 'closest', range = 5, allow.duplicates = FALSE) {
  candidates <- sapply(1:range, \(l) delta(x, lag = l))
  
  if (all(candidates < minlag, na.rm = TRUE) && range < length(x) / 2) return(Recall(x, minlag = minlag, maxlag = maxlag, prefer = prefer, range = range * 2L))
  
  hits <- candidates >= minlag & candidates <= maxlag
  
  candidates[!hits] <- NA
  candidates <- candidates - lag
 
  prefer <- pmatches(prefer, c('closest', 'short', 'long'), callname = 'findLag')
  
  na <- is.na(candidates)
  candidates[na] <- max(candidates, na.rm = TRUE) + 1L
  c.candi <- c(t(candidates))
  row <- c(t(row(candidates))) - 1L
  order <- if (prefer == 'closest') {
    order(row, abs(c.candi)) - (row  * ncol(candidates))
  } else {
    sign <- sign(c.candi)
    if (prefer == 'short') sign <- -sign
    order(c(row(candidates)), sign, abs(c.candi)) - ((c(col(candidates)) - 1L)  * ncol(candidates))
  }
  
  order <- matrix(order, ncol = ncol(candidates), byrow = TRUE)
  order[na] <- NA
  imat <- row(candidates) - col(candidates)
  
  
  
  i <- imat[cbind(1:nrow(order), order[ , 1])] 
  j <- 2
  
  while (!allow.duplicates && any(duplicated(i))) {
    dup <- duplicated(i)
    if (j > ncol(order)) {
     i[dup] <- NA
     break
    }
    newi <- imat[cbind(1:nrow(order), order[ , j])]
    update <- dup & !newi %in% i & newi > shift(.cummax(i), 1L)
    
    i[update] <- newi[update]
    j <- j + 1
  }
  
  i
   
}

findLag <- function(x, lag = 1, minlag = 0, maxlag = Inf, prefer = 'closest', range = 5) {
  candidates <- sapply(1:range, \(l) delta(x, lag = l))
  
  if (all(candidates < minlag, na.rm = TRUE) && range < length(x) / 2) return(Recall(x, minlag = minlag, maxlag = maxlag, prefer = prefer, range = range * 2L))
  
  hits <- candidates > minlag & candidates < maxlag

  candidates[!hits] <- NA
  candidates <- candidates - lag
  
  prefer <- pmatches(prefer, c('closest', 'short', 'long'))
  
  offseti <- if (prefer == 'closest') {
    applyrows(abs(candidates), which.min)
  } else {
     ncandidates <- -candidates
     
     candidates[candidates < 0] <- NA
     ncandidates[ncandidates <= 0] <- NA
     
     long  <- applyrows(candidates,  which.min)
     short <- applyrows(ncandidates,  which.min)
    
  }
  
  candidates[candidates < 0] <- NA
  offset <- apply(candidates, 1, which.min)

  offset[lengths(offset) == 0] <- list(NA)
  lagi <- seq_along(x) - unlist(offset)
  
  cbind(x, x[lagi], x - x[lagi])
  
  
}


rhythmAlign <- function(x, y) {
  tick <- gcd(min(x), min(y))
  xi <- as.integer(x / tick)
  yi <- as.integer(y / tick)
  
  xi <- SOI(xi)$On
  yi <- SOI(yi)$On
  
  alli <- union(xi, yi)
  
  ox <- vectorNA(length(alli), class(x))
  oy <- vectorNA(length(alli), class(y))
  ox[match(xi, alli)] <- x
  oy[match(yi, alli)] <- y
  
  remove <- is.na(ox) & is.na(oy)
  
  
  .data.frame(ox[!remove], oy[!remove])
  
  
  
}


## Grids ----

#' Drum-machine grid representation of rhythmic durations.
#' 
#' These functions read and write a sequencer-like representation of rhythm.
#' Rhythms are represented as either strings or vectors of "on"/"off" values,
#' which indicate where rhythmic onsets occur in a regular time grid.
#' For example, `"X00X00X0"` or `c(1, 0, 0, 1, 0, 0, 1, 0)`.
#'
#' @details 
#'
#' The `grid()` function, is a fully vectorized [rhythm function][rhythmFunctions], which translates
#' *individual* durations to a grid-representation strings.
#' For example, on a 16th-note grid, a dotted eighth-note would be represented `"XOO"`.
#' The `fromgrid()` and `togrid()` functions create/read fuller 
#' grid representations, representing whole rhythms at once: in their case,
#' the length of input and output will not be the same.
#' 
#' @param tick ***What is the unit of the grid?***
#' 
#' Defaults to sixteenth-note (`fromgrid()`) or the [tatum()] of the `x` argument.
#' 
#' Must be parsed as a rhythm by [rhythmInterval()].
#' 
#' @param on,off ***What represents onsets (attacks) and rests in the grid?***
#' 
#' Default to `"X"` and `"O"` respectively.
#' 
#' Must be singleton atomic values.
#'
#' @param collapse ***Should the output be collapsed to a single string per measure?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be singleton `logical` value: an on/off switch.
#' 
#' @param sep ***Separator between ticks in collapsed output.***
#'
#' Defaults to an empty string (no separator).
#'  
#' Must be a singleton `character` string.
#' 
#' @param deparser ***What output representation should be returned?***
#' 
#' Defaults to [recip()].
#' 
#' Must be a function which accepts [rational()] numbers.
#' 
#' @param ... ***Argments passed to the [deparser][rhythmDeparsing].***
#' 
#' 
#' @examples 
#' 
#' rhythm <- c('8.', '8.', '8', '8.', '8', '16', '8')
#' 
#' grid(rhythm)
#' 
#' togrid(rhythm, on = '1', off = '0')
#' 
#' togrid(rhythm, collapse = FALSE)
#' 
#' fromgrid('XOXOXOOXXOXOXXOO', tick = '8')
#' 
#' 
#' @seealso To better understand how `grid()` works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#'
#' @inheritParams rhythmFunctions 
#' @name grid
#' @export 
grid.default <- makeRhythmTransformer(rint2grid, 'grid', 'character')
#' Apply to humdrumR data
#' 
#' If `grid()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> grid() 
#' humData |> grid(Token)
#' 
#' @rdname grid
#' @export
grid.humdrumR <- humdrumRmethod(grid.default)
#' @rdname grid
#' @export
grid <- humdrumRgeneric(grid.default)

### To grid ----

#' @rdname grid
#' @export
togrid <- function(x, tick = tatum(x), measure = '1', on = 'X', off = 'O', collapse = TRUE, sep = '') {
  tick <- rhythmInterval(tick)
  measure <- rhythmInterval(measure)
  
  checks(collapse, xTF)
  checks(sep, xcharacter & xlen1)
  checks(on, xcharacter & xlen1)
  checks(off, xcharacter & xlen1)
  
  n <- as.integer(measure %/% tick)
  
  grids <- grid(x, tick = tick, on = on, off = off)
  
  if (collapse) {
    lens <- nchar(grids)
    unname(c(tapply(grids, head(cumsum(c(0, lens)), -1L) %/% n, paste, collapse = sep)))
  } else {
    ticks <- unlist(strsplit(grids, split = ''))
    
    nrow <- ceiling(length(ticks) / n)
    
    matrix(c(ticks, rep(NA, (nrow * n) - length(ticks))),
           nrow = nrow, ncol = n, byrow = TRUE)
  }
  
  
}



### From grid ----
#' @rdname grid
#' @export
fromgrid <- function(x, tick, meter) UseMethod('fromgrid')

#' @rdname grid
#' @export
fromgrid.matrix <- function(x, tick = '16') {
  fromgrid(c(t(x)))
}

#' @rdname grid
#' @export
fromgrid.character <- function(x, tick = '16', on = 'X', off = 'O', sep = '', deparser = recip, ...) {
  checks(sep, xcharacter & xlen1)
  checks(on, xcharacter & xlen1)
  checks(off, xcharacter & xlen1)
  checks(deparser, xinherits('function'))
  
 fromgrid.logical(unlist(strsplit(x, split = sep)) == on, tick = tick, deparser = deparser, ...) 
}

#' @rdname grid
#' @export
fromgrid.logical <- function(x, tick = '16', deparser = rint2recip, ...) {
  
  tick <- rhythmInterval(tick)
  
  rint <- diff(c(which(x), length(x) + 1L)) * tick
  
  if (!is.null(deparser)) deparser(rint, ...) else rint
}

#' @rdname grid
#' @export
fromgrid.numeric <- function(x, tick = '16', deparser = recip, ...) {
  fromgrid.logical(x > 0, tick = tick, deparser = deparser, ...) 
}
  
#' @rdname grid
#' @export
fromgrid.integer <- function(x, tick = '16', deparser = recip, ...) {
  fromgrid.logical(x > 0, tick = tick, deparser = deparser, ...) 
}




###################################################################### ###
# Rhythmic visualizations rhythm intervals ###############################
###################################################################### ###


londonPlot <- function(pos, beat = 1) {
  pos <- pos / beat
  
  radian <- -(pos * 2 * pi) + pi / 2
  
  counts <- table(radian)
  vals <- as.numeric(names(counts))
  
  x <- setNames(cos(vals), names(counts))
  y <- setNames(sin(vals), names(counts))
  
  plot.new()
  plot.window(xlim = c(-1L, 1L), ylim = c(-1L, 1L))
  
  lines(cos(seq(0, 2*pi, length.out = 1e3)), 
        sin(seq(0, 2*pi, length.out = 1e3)),
        lty = 'dashed', col = 'grey50')
  
  # polygon(x = x, y = y, )
  size <- counts ^ 2
  size <- ((size / max(size)) + 1) * 2
  points(x, y, type = 'p', pch = 16, cex = size)
  
  # transitions
  trans <- table(radian, lag(radian, -1))#^2
  trans[row(trans) == col(trans)] <- 0
  trans <- c(trans)
  empty <- trans == 0
  trans <- ((trans / max(trans))) * 3 + 1
  
  arrows(x0 = rep(x, length(x))[!empty], 
         x1 = rep(x, each = length(x))[!empty], 
         y0 = rep(y, length(y))[!empty], 
         y1 = rep(y, each = length(y))[!empty],
         lwd = trans[!empty],
         code = 3, angle = 20, length = .2)
  # i <- tapply(seq_along(x), INDEX = paste(round(x, 2), round(y, 2)), paste, collapse = ';')
  # xy <- do.call('rbind', lapply(strsplit(names(i), split = ' '), as.numeric))
  # text(xy[ , 1]*1.1, xy[ , 2]*1.1, i, xpd = T)
  
}



