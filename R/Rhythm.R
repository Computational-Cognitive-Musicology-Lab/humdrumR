###################################################################### ###
# Basic time and tempo stuff #############################################
###################################################################### ###


#' Basic time transformations
#' 
#' 
#' @name time
#' @export
bpm2sec <- function(BPM, unit = .25) {
  BPM <- as.numeric(gsub('\\*?MM', '', BPM))
  240 * unit / BPM
}

#' @rdname time
#' @export
sec2bpm <- function(sec, unit  =.25 ) 240 * unit / sec

#' @rdname time
#' @export
bpm2ms <- function(BPM, unit = .25) bpm2sec(BPM, unit) * 1000

#' @rdname time
#' @export
ms2bpm <- function(ms, unit = .25) sec2bpm(ms / 1000, unit)






###################################################################### ###
# Deparsing Rhythm Representations (rint2x) ##############################
###################################################################### ###



## Deparsing (rhythmInterval) documentation ----

#' Generating ("deparsing") rhythm representations
#' 
#' @seealso All `humdrumR` [rhythm functions][rhythmFunctions] make use of the 
#' deparsing functionality.
#' @name rhythmDeparsing
NULL

## Rhythm deparsers ####

### Symbolic ####


rint2recip <- function(x, sep = '%', unit = rational(1L)) {
          #modify this to print 0 and 00
          
          x <- x / rhythmInterval(unit)
  
          num <- x@Numerator
          den <- x@Denominator
          
          
          # Get the sign
          SIGN <- c('-', '', '')[2 + sign(num)]
          num <- abs(num)
          den[num == 0L] <- 1L
          
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
          
          # add in weird "0" (semibreve) and "00" (longa) and "000" (maxima)
          den[(num / den) %in% c(2, 4, 8)] <- strrep('0', log(num / den, 2)[(num / den) %in% c(2, 4, 8)])
          num[den %in% c('0', '00', '000')] <- '1'
          
          output <- .ifelse(num == 1L, den, .paste(den, sep, num)) 

          .paste(SIGN, output, strrep('.', dots))

}

###As unicode duration string




rint2noteValue <- function(x, unit = rational(1L)) {
    x <- x / rhythmInterval(unit)
  
    recip <- recip(x)
    
    recip[recip == '000.'] <- '1%12'
    recip[recip == '000']  <- '1%8'
    recip[recip == '00.']  <- '1%6'
    recip[recip == '00']  <- '1%4'
    recip[recip == '0.']  <- '1%3'
    
    
    # base notation
    REparse(recip,
            res = list(denominator = "[1-9][0-9]*|0{1,2}", 
                       numerator = "(%[1-9][0-9]*)?", 
                       dots = '[.]*$'),
            toEnv = TRUE)
    
    
    symbols <- setNames(noteValue.unicode$Unicode, noteValue.unicode$Recip)
    
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
    
    paste0(multiples,  base,  divides,  ' ', dots)
}


### Numeric ####


rint2semibreves <- function(x, unit = rational(1L)) {
  as.double(x / rhythmInterval(unit))
} 

rint2breves       <- partialApply(rint2semibreves, unit = rational(2L, 1L))
rint2crotchets    <- partialApply(rint2semibreves, unit = rational(1L, 4L))
rint2quavers      <- partialApply(rint2semibreves, unit = rational(1L, 8L))
rint2semiquavers  <- partialApply(rint2semibreves, unit = rational(1L, 16L))

rint2seconds <- function(x, BPM = 60, unit = 1) {
  rint2semibreves(x) * bpm2sec(BPM, unit = unit)
}


rint2ms <- function(x, BPM = 60, unit = 1) {
  rint2semibreves(x) * bpm2ms(BPM, unit = unit)
}






###################################################################### ### 
# Parsing Rhythm Representations (x2rint) ################################
###################################################################### ### 

## Parsing (rhythmInterval) documentation ----

#' Parsing rhythm information
#' 
#' 
#' @seealso All `humdrumR` [rhythm functions][rhythmFunctions] make use of the
#'  parsing functionality.
#' @name rhythmParsing
NULL


## Rhythm parsers ####

### Symbolic ####

recip2rint <- function(x, graceDurations = FALSE, unit = rational(1L)) {
  
  REparse(x, makeRE.recip(collapse = FALSE), toEnv = TRUE) # makes grace and recip
  
  # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
  recip <- gsub('^000', '1%8', recip)
  recip <- gsub('^00',  '1%4', recip)
  recip <- gsub('^0',   '1%2', recip)
  
  ndots <- stringr::str_count(recip, '\\.')
  recip <- gsub('\\.+', '', recip)
  
  rational <- as.rational(recip, sep = '%')
  rational <- reciprocal(rational)
  
  
  dots <- 2L ^ ndots
  dotscale <- rational((2L * dots) - 1L, dots)

  rint <- rational * dotscale
  
  rint <- rint * rhythmInterval(unit)
  if (!graceDurations) rint[grace != ''] <- rint[grace != ''] * 0
  
  
  rint
  
}

timesignature2rint <- function(x, sep = '/', unit = rational(1L)) {
  x <- stringr::str_remove(x, '^\\*?M?')
  as.rational(x * rhythmInterval(unit), sep = '/')
}


noteValue2rint <- function(x, sep =" \U2215", unit = rational(1L)) {
  
  
  
  REparse(x, 
          makeRE.noteValue(sep = sep, collapse = FALSE),
          toEnv = TRUE)
  
  # 
  symbols <- setNames(noteValue.unicode$Recip, noteValue.unicode$Unicode)
  noteValue <- symbols[value]
  dots  <- stringr::str_replace_all(dots, '\U1D16D\U2009', '.')
  
  multiplies <- ifelse(multiplies == '', '', paste0('%', multiplies))
  
  recip <- paste0(noteValue, multiplies, dots)
  
  rint <- recip2rint(recip)
  
  #
  divides <- as.numeric(gsub('\U2215', '', divides))
  rint <- rint / (divides %|% 1)
  
  rint * rhythmInterval(unit)
}

### Numbers ####

semibreves2rint <- function(x, unit = rational(1L), ...) {
  as.rational(x) * rhythmInterval(unit)
}

breves2rint        <- function(x) semibreves2rint(x, unit = rational(2L, 1L))
crotchets2rint     <- function(x) semibreves2rint(x, unit = rational(1L, 4L))
quavers2rint       <- function(x) semibreves2rint(x, unit = rational(1L, 8L))
semiquavers2rint   <- function(x) semibreves2rint(x, unit = rational(1L, 16L))

seconds2rint <- function(x, BPM = 60, unit = 1, ...) {
  semibreves2rint(x / bpm2sec(BPM, unit = unit))
}

ms2rint <- function(x, BPM = 60, unit = 1, ...) {
  semibreves2rint(x / bpm2ms(BPM, unit = unit))
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
rhythmInterval.numeric <- makeHumdrumDispatcher(list('semibreves' ,        NA, semibreves2rint),
                                                list('seconds'   ,        NA, seconds2rint),
                                                list('crotchets'  ,        NA, crotchets2rint),
                                                list('quavers'    ,        NA, quavers2rint),
                                                list('semiquavers',        NA, semiquavers2rint),
                                                funcName = 'rhythmInterval.numeric',
                                                outputClass = 'rational')

#### Characters ####

#' @rdname rhythmParsing
#' @export
rhythmInterval.character <- makeHumdrumDispatcher(list(c('recip', 'kern', 'harm'), makeRE.recip,  recip2rint),
                                                  list('any',                      makeRE.timeSignature, timesignature2rint),
                                                  list('semibreves',                 makeRE.double, semibreves2rint),
                                                  list('any',                      makeRE.noteValue(), noteValue2rint),
                                                  funcName = 'rhythmInterval.character',
                                                  outputClass = 'rational')

#### setAs rhythmInterval ####

# See "setAs rational"

###################################################################### ### 
# Translating Rhythm Representations (x2y) ###############################
###################################################################### ### 

## Rhythm function documentation ####

#' Manipulate pitch data
#'  
#' @name rhythmFunctions
#' @seealso To better understand how these functions work, 
#' read about how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
NULL

## Rhythm transform maker ####

rhythmArgCheck <- function(args, callname) {
  args
}

makeRhythmTransformer <- function(deparser, callname, outputClass = 'character') {
  # this function will create various rhythm transform functions
  
  withinFields$Exclusive  <<- c(withinFields$Exclusive, callname)
  
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- alist(x = , 
                ... = , # don't move this! Needs to come before other arguments, otherwise unnamed parse() argument won't work!
                parseArgs = list(), timeArgs = list(),
                graceDurations = FALSE,
                inPlace = FALSE)
  
  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr( {
    
    checkVector(x, structs = 'rational', argname = 'x', 
                callname = !!callname, matrix = TRUE)
    
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    c('args...', 'parseArgs', 'timeArgs') %<-% specialArgs(rlang::enquos(...), 
                                                           parse = parseArgs, 
                                                           time = timeArgs)

    
    formalArgs <- list(!!!fargcall)
    namedArgs <- formalArgs[.names(formalArgs) %in% .names(as.list(match.call())[-1])]
    # There are four kinds of arguments: 
    # ... arguments (now in args...), 
    # FORMAL arguments, if specified (now in namedArgs)
    # parseArgs
    # timeArgs
    
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
    
    parsedRint <- do(rhythmInterval, 
                     c(list(x), parseArgs), 
                     memoize = memoize, 
                     outputClass = 'rational')
    
    
    deparseArgs <- c(list(parsedRint), deparseArgs)
    output <- if (deparse && is.rational(parsedRint))  do(!!deparser, 
                                                          deparseArgs, 
                                                          memoize = memoize, 
                                                          outputClass = !!outputClass) else parsedRint
    if (deparse && !is.null(output)) {
      dispatch <- attr(parsedRint, 'dispatch')
      if (inPlace) output <- rePlace(output, dispatch)
      
      if (!is.null(parseArgs$Exclusive)) humdrumRattr(output) <- list(Exclusive = makeExcluder(dispatch$Exclusives, !!callname))
    }
    
    output
    
  })) %class% 'rhythmFunction'
}


### Rhythm functions ####

#' Reciprocal representation of duration
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#' @inheritParams rhythmFunctions
#' @export 
recip <- makeRhythmTransformer(rint2recip, 'recip')

#' Numeric (double) representation of durations
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#' @inheritParams rhythmFunctions
#' @export 
semibreves <- makeRhythmTransformer(rint2semibreves, 'semibreves', 'numeric')

#' @rdname semibreves
#' @export 
breves <- makeRhythmTransformer(rint2breves, 'breves', 'numeric')

#' @rdname semibreves
#' @export 
crotchets <- makeRhythmTransformer(rint2crotchets, 'crotchets', 'numeric')

#' @rdname semibreves
#' @export 
quavers <- makeRhythmTransformer(rint2quavers, 'quavers', 'numeric')


#' @rdname semibreves
#' @export 
semiquavers <- makeRhythmTransformer(rint2semiquavers, 'semiquavers', 'numeric')


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
#' with(chorales, barplot(table(noteValue(Token)), cex.names = 2))
#' 
#' ```
#' 
#' 
#' @seealso To better understand how this function works, 
#' read about the [family of rhythm functions][rhythmFunctions], 
#' or how rhythms are [parsed][rhythmParsing] and [deparsed][rhythmDeparsing].
#' @family {rhythm functions}
#' @inheritParams rhythmFunctions
#' @export 
noteValue <- makeRhythmTransformer(rint2noteValue, 'noteValue')



#' @rdname time
#' @export
seconds <- makeRhythmTransformer(rint2seconds, 'seconds')

#' @rdname time
#' @export
ms <- makeRhythmTransformer(rint2ms, 'ms')


###################################################################### ###
# Manipulating rhythm intervals ##########################################
###################################################################### ###



## IOI ----

#' Sum "connected" durations
#' 
#' These functions are used to sum (melodically) adjacent rhythmic duration values which are not associated with new onsets/attacks.
#' `ioi()` adds the duration of [rests](https://en.wikipedia.org/wiki/Rest_(music)) to the previous
#' non-rest (onset) duration, to create [interonset intervals](https://en.wikipedia.org/wiki/Time_point#Interonset_interval) (IOIs).
#' `untie()` sums [tied](https://en.wikipedia.org/wiki/Tie_(music)) durations.
#' 
#' @details 
#' 
#' 
#' Both functions return "collapsed" durations are as null data tokens. 
#' For example, `untie(c('[4a', '4a]', '2g'))` returns `c('2a', '.', '2g')`, with the second (tied) duration null (`"."`).
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
#' @param x An input vector which is parsed for duration information using the 
#' [rhythm parser][rhythmParsing]. `max`, `median`, or `mode` might be reasonable alternatives.
#' @param onsets A `logical` vector of the same length as `x`. All durations in `x` where `onsets == FALSE`
#' are added to the previous value where `onsets == TRUE`.
#' @param finalOnset (`logical`, `length == 1`) If `TRUE`, the last IOI is computed between the last onset and the end of the input vector.
#' Otherwise, this last IOI is undefined (`NA`).
#' @param parseArgs A `list` of arguments to pass to the [rhythm parser][rhythmInterval()].
#' @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x`.
#' @param inPlace (`logical`, `length == 1`) This argument only has an effect if the input (the `x` argument) is `character` strings,
#'        *and* there is extra, non-pitch information in the input strings "besides" the duration information.
#'        If so, and `inPlace = TRUE`, the output will be placed into an output string beside the original non-duration information.
#'        If `inPlace = FALSE`, only the rhythm output information will be returned.
#'        
#' @export
ioi <- function(x, onsets = !grepl('r', x) & !is.na(x) & x != '.', ..., 
                finalOnset = FALSE,
                groupby = list(), parseArgs = list(), Exclusive = NULL,
                inPlace = TRUE) {
  
  checkLogical(onsets, 'onsets', 'ioi')
  if (length(x) != length(onsets)) .stop("In a call to ioi(), the 'onsets' and 'x' arguments must be the same length.")
  checkTF(finalOnset, 'finalOnset', 'ioi')
  checkTF(inPlace, 'inPlace', 'ioi')
  
  rint <- do.call('rhythmInterval', c(list(x, Exclusive = Exclusive), parseArgs))
  
  if (any(!onsets)) {
    windows <- windows(x, onsets, ~c(Next(open) - 1L, length(x)), groupby = groupby)
    rint <- windowsSum(rint, windows)
  }
  
  dispatch <- attr(rint, 'dispatch')
  output <- reParse(rint, dispatch, reParsers = c('recip', 'semibreves'))
  
  if (is.character(output)){
    if (inPlace) output <- rePlace(output, dispatch) else humdrumRattr(output) <- NULL
    output[!onsets] <- '.'
  } else {
    output[!onsets] <- as(NA, class(output))
  }
  
  if (!finalOnset) {
    if (length(groupby)) {
      output[tapply(seq_along(onsets)[onsets], lapply(groupby, '[', i = onsets), max)] <- as(NA, class(output))
      
    } else {
      output[max(which(onsets), na.rm = TRUE)] <- as(NA, class(output))
      
    }
  }
  
  
  output
  
  
}

#' @param open A `character` string (regular expression) to identify the beginning of ties.
#'   (May also be formula: see [humWindows].)
#' @param close A `character` string (regular expression) to identify the end of ties.
#'   (May also be formula: see [humWindows].)
#' @rdname ioi
#' @export
untie <- function(x, open = '[', close = ']', ..., 
                  groupby = list(), 
                  inPlace = TRUE) {
  checkTF(inPlace, 'inPlace', 'untie')
  
  rint <- rhythmInterval(x, ...)
  
  
  windows <- windows(x, open, close, groupby = groupby)
  
  rint <- windowsSum(rint, windows)
  
  
  dispatch <- attr(rint, 'dispatch')
  output <- reParse(rint, dispatch, reParsers = c('recip', 'semibreves'))
  
  null <- unlist(Map(":", windows$Open + 1L, windows$Close))
  if (is.character(output)){
    if (inPlace) output <- rePlace(output, dispatch) else humdrumRattr(output) <- NULL
    output[null] <- '.'
    if (is.character(open)) output <- stringr::str_remove(output, 
                                                          if (open %in% c('[', ']', '(', ')')) paste0('\\', open) else open)
  } else {
    output[null] <- as(NA, class(output))
  }
  
  
  output
  
}




minutes <- function(seconds, format = TRUE) {
  
  sign <- ifelse(seconds >= 0, '', '-')
  seconds <- abs(seconds)
  
  minutes <- seconds %/% 60
  
  seconds <- round(seconds %% 60, 3)
  paste0(sign, minutes, ':', ifelse(seconds >= 10, '', '0'), format(seconds, nsmall = 3L, trim = TRUE))
}

### timelines ----

#' Calculate overall duration of a group
#' 
#' `localDuration()` calculates the "overall" duration within groups in an input vector.
#' What the hell does that mean?
#' Usually, it is used to find the duration of each *record* in a humdrum file.
#' 
#' @details 
#' 
#' The way rhythm and time are typically encoded in humdrum format, the "overall" duration of
#' a record is determined by the shortest duration in the record, if there are any.
#' So, if we have a file like this:
#' 
#' ```
#' **kern  **kern     **silbe
#'     4c      8g        Hum-
#'      .      8f           _
#'      !       !    !melisma  
#'     8b      8f       -drum
#'     8c      8e           _
#'     2d      4a         da-
#'      .       .           .
#'      .      4g         -ta
#'     G;      g;         ooh
#'     *-      *-          *-     
#' ```
#'
#' The "local" duration of each record would be (in `**recip`):
#' 
#' ```
#' **kern  **kern     **silbe   -> 1%0
#'     4c      8g        Hum-   -> 8
#'      .      8f           _   -> 8
#'      !       !    !melisma   -> 1%0
#'     8b      8f       -drum   -> 8
#'     8c      8e           _   -> 8
#'     2d      4a         da-   -> 4
#'      .       .           .   -> 1%0
#'      .      4g         -ta   -> 4
#'     G;      g;         ooh   -> 1%0
#'     *-      *-          *-   -> 1%0
#' ```
#'
#' Note that some records are length zero (`1%0`), because they are missing any duration information.
#' (In this example we are showing durations of `1%0` for comment, interpretation, and null data records. In most cases, we'd 
#' be doing `within(humData, dataTypes ='D')`, which is the default behavior, so these records wouldn't be counted at all.)
#' 
#' `localDuration()` begins with a call to [semibreves()] on the input argument `x`---the `parseArgs()` argument can be used to pass arguments to the [parser][rhythmParsing] (the `Exclusive` argument is passed as well).
#' `localDuration()` then groups the durations based on unique combinations of values in the `groupby` argument, which must be a list of
#' vectors that are the same length as `x`.
#' By default, the minimum duration within each group is returned, recycled as necassary to match the input length.
#' The `choose` argument can be set to another function, if desired.
#' For example, you could use `localDuration(x, choose = max)` to find the *maximum* duration in each group.
#' If the `groupby` argument is empty (the default) the durations are returned unchanged, except that `NA` durations are set to `0`.
#' Luckily, if `localDuration()` is used inside a [with(in).humdrumR][withinHumdrum] expression, the `groupby = list(File, Record)` is *automatically*
#' passed (this can be overridden by explicitely setting the argument).
#' This means that `with(humData, localDuration(Token))` will automatically calculate the minimum duration of each record.
#' 
#' Note that, `localDuration()` follows the default behavior of [semibreves()] by treating grace-notes as duration `0`.
#' If you want to use the duration(s) of grace notes, specify `graceDurations = TRUE`.
#'
#' The output representation can be controlled using the `deparser` argument, defaulting to [semibreves()].
#' For example, `deparser = recip` will return the output in `**recip` format.
#' `...` arguments are passed to the deparser.
#' 
#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/.*krn")
#' 
#' within(humData, localDuration(Token))
#'
#' @param x An input vector which is parsed for duration information using the [rhythm parser][rhythmParsing].
#' @param choose A function which takes a vector of `numeric` and returns a single `numeric` value. Defaults to `min`; `max`, `median`, or `mode` might be reasonable alternatives.
#' @param deparser A [rhythm function][rhythmFunction] to generate the output representation.
#' @param parseArgs A `list` of arguments to pass to the [rhythm parser][rhythmInterval()].
#' @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x`.
#'
#' @family rhythm analysis tools
#' @export 
localDuration <- function(x,  choose = min, deparser = semibreves, ..., Exclusive = NULL, parseArgs = list(), groupby = list()) {
  
  checkFunction(choose, 'choose', 'int')
  if (!is.null(deparser)) checkArg(deparser, 'deparser', callname = 'localDuration', classes = c('rhythmFunction'))
  
  durations <- do.call('semibreves', c(list(x, Exclusive = Exclusive), parseArgs))
  
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
#' In `music21` this inforfmation is described as "offsets"---however,
#' we prefer to reserve the words "onset" and "offset" to refer
#' to the beginning (attack) and end (release) of rhythmic events.
#'
#' @details 
#'
#' Music unfolds over time, and humdrum data typically represents this 
#' by placing simultanteous events in the same record, with successive events
#' in ever higher records---progressing "top down" through the file.
#' In some humdrum data, only this (implicit) ordering of data over time is present.
#' The `Record` and `NData` [fields][fields()] capture this ordering in all data parsed by `humdrumR`.
#' However, many (probably most) humdrum data files contain at least some information about the relative 
#' duration of events, representing more detailed information about timing and rhythm.
#' 
#' `timeline()` parses and input vector `x` as [durations][semibreves()],
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
#' Note that, `timeline()` and `timestamp()` follow the default behavior of [semibreves()] by treating grace-notes as duration `0`.
#' If you want to use the duration(s) of grace notes, specify `graceDurations = TRUE`.
#' 
#' @section Logical start:
#' 
#' Another option is to pass the `start` argument a logical vector of the same length as the input `x`.
#' Within each piece, the the *first* timepoint where the `start` logical is `TRUE` is used as the zero:
#' all early points will be negative numbers.
#' In `humdrumR`, and datapoints before the first barline record (`=`) are labeled `Bar == 0` in the `Bar` [field][fields()].
#' Thus, a common use for a `logical` `start` argument is `within(humData, timeline(Token, start = Bar == 1)`, which makes the downbeat of
#' the first complete bar `0`---any notes in a pickup bar are give negative numbers on the timeline.

#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/.*krn")
#' 
#' within(humData, timeline(Token))
#' 
#' within(humData, timestamp(Token, minutes = TRUE))
#' 
#' 
#' @param x An input vector which is parsed for duration information using the [rhythm parser][rhythmParsing].
#' @param start A `numeric` value from which the timeline begins, or a `logical` vector of same length as `x`.
#' @param minutes (`logical`, `length == 1`) If `TRUE`, output seconds are converted to a character string
#' encoding minutes, seconds, and milliseconds in the format `MM.SS.ms`. 
#' @param `BPM` A numeric values or `character` string in the format `"MM120"` (for 120 bpm). By default,
#' [with(in).humdrumR][withinHumdrum] passed the `BPM` [field][fields()], if present.
#' @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x` into.
#'   To function as a by-record timeline, the `groupby` list music include a *named* `Piece` and `Record` fields.
#'   Luckily, these are automatically passed by [with(in).humdrumR][withinHumdrum], so you won't need to worry about it!
#' @param parseArgs A `list` of arguments to pass to the [rhythm parser][rhythmInterval()].
#'   
#' @family rhythm analysis tools
#' @export
timeline <- function(x, start = 0, ..., Exclusive = NULL, parseArgs = list(), groupby = list()) {
  durations <- localDuration(x, groupby = groupby, parseArgs = parseArgs, Exclusive = Exclusive)

  
  
  groupby$Piece <- groupby$Piece %||% rep(1, length(durations))
  groupby$Record <- groupby$Record %||% seq_along(durations)
  
  
  dt <- groupby <- as.data.table(checkWindows(durations, groupby))
  dt$Duration <- durations
  
  
  if (is.logical(start)) {
    if (length(start) != length(x)) .stop("In a call to timeline, a logical 'start' argument must be the same length as the x argument.")
    dt$Start <- start
    start <- 0L
  } else {
    checkArg(start, 'start', 'timeline', classes = c('logical', 'numeric'), max.length = 1L, min.length = 1L)
  }
  
  dtuniq <- dt[!duplicated(groupby)]
  setorder(dtuniq, Piece, Record)
  dtuniq[ , Duration := c(start, head(Duration, -1L)), by = Piece]
  dtuniq[ , Time := sigma.default(Duration, groupby = list(Piece))]
  if (!is.null(dtuniq$Start)) dtuniq[ , Time := Time - Time[which(Start)[1]], by = Piece]
  
  dtuniq[dt, on = c('Piece', 'Record')]$Time
  
  
}

#' @rdname timeline
#' @export
timestamp <- function(x, BPM = 'MM60', start = 0, minutes = FALSE, ..., Exclusive = NULL, parseArgs = list(), groupby = list()) {
  durations <- semibreves(x, Exclusive = Exclusive, ...)
  seconds <- seconds(durations, as.integer(gsub('\\*?MM', '', BPM)))
  
  seconds <- timeline(seconds, Exclusive = Exclusive, groupby = groupby, ...)
  

  
  if (minutes) minutes(seconds) else seconds
  
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
