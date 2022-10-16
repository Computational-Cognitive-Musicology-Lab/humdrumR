###################################################################### ###
# Deparsing Rhythm Representations (rint2x) ##############################
###################################################################### ###


## Rhythm deparsers ####

### Symbolic ####


rint2recip <- function(rint) {
          #modify this to print 0 and 00
          num <- rint@Numerator
          den <- rint@Denominator
          
          
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
          
          # add in weird "0" (breve) and "00" (longa) and "000" (maxima)
          den[(num / den) %in% c(2, 4, 8)] <- strrep('0', log(num / den, 2)[(num / den) %in% c(2, 4, 8)])
          num[den %in% c('0', '00', '000')] <- '1'
          
          output <- .ifelse(num == 1L, den, .paste(den, '%', num)) 

          .paste(SIGN, output, strrep('.', dots))

}

###As unicode duration string

notevalue.unicode <- data.frame(stringsAsFactors = FALSE,
                                Unicode = c('\U1D15C', '\U1D15D', '\U1D15E', '\U1D15F', 
                                            '\U1D160', '\U1D161', '\U1D162', '\U1D163', '\U1D164'),
                                Recip = c('0', '1', '2', '4', '8', '16', '32', '64', '128'))


rint2notevalue <- function(rint) {
    recip <- recip(rint)
    
    # base notation
    parsed<- REparse(recip,
                     res = list(denominator = "^[0-9]+", 
                                numerator = "(%[1-9][0-9]*)?", 
                                dots = '[.]*$'))
    
    
    symbols <- setNames(notevalue.unicode$Unicode, notevalue.unicode$Recip)
    
    base <- symbols[parsed[ , 'denominator']]
    
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
        den <- as.integer(parsed[unknown, 'denominator'])
        
        fitbase <- den %% primes[i] == 0 
        newbase <- den %/% primes[i]
        newbase <- .ifelse(log(newbase, 2) %% 1L == 0,
                          as.character(newbase),
                          '1')
        
        base[which(unknown)[fitbase]] <- symbols[newbase[fitbase]]
        divides[which(unknown)[fitbase]] <- paste0("\U2215", primes[i])
        
        unknown <- is.na(base) & !is.na(recip)
        i <- i + 1L
    }
    
    # add multiples
    multiples <- .ifelse(parsed[ , 'numerator'] == "", 
                        "",
                        paste0("\U2217", 
                               stringr::str_sub(parsed[, 'numerator'], start = 2)))
    notes <- paste0(base, divides, multiples)
    
    # add dots
    dots <- gsub('.', '\U1D16D', parsed[ , 'dots'])
    
    paste0(notes, dots)
    
}


### Numeric ####


rint2double <- function(x) as.double(x)









###################################################################### ### 
# Parsing Rhythm Representations (x2rint) ################################
###################################################################### ### 

## Rhythm parsers ####

### Symbolic ####

recip2rint <- function(str, graceDurations = FALSE) {
  
  REparse(str, makeRE.recip(collapse = FALSE), toEnv = TRUE) # makes grace and recip
  
  # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
  recip <- .ifelse(grepl('^0\\.|^0$', recip),     gsub('^0',   '1%2', recip), recip)
  recip <- .ifelse(grepl('^00\\.|^00$', recip),   gsub('^00',  '1%4', recip), recip)
  recip <- .ifelse(grepl('^000\\.|^000$', recip), gsub('^000', '1%8', recip), recip)
  
  ndots <- stringr::str_count(recip, '\\.')
  recip <- gsub('\\.+', '', recip)
  
  rational <- as.rational(recip, sep = '%')
  rational <- reciprocal(rational)
  
  
  dots <- 2L ^ ndots
  dotscale <- rational((2L * dots) - 1L, dots)

  rint <- rational * dotscale
  
  if (!graceDurations) rint[grace != ''] <- rint[grace != ''] * 0
  
  rint
  
}

timesignature2rint <- function(str, sep = '/') {
  str <- stringr::str_remove(str, '^\\*?M?')
  as.rational(str)
}


notevalue2rint <- function(notevalues) {
  
  
  
  parsed <- REparse(notevalues,
                    list(notevalue = paste0('^[', 
                                            paste(notevalue.unicode$Unicode, collapse = ''), 
                                            ']'),
                         divide = "(\U2215[1-9][0-9]*)?",
                         multiples = "(\U2217[1-9][0-9]*)?",
                         dots = '\U1D16D*'))
  
  
  # 
  symbols <- setNames(notevalue.unicode$Recip, notevalue.unicode$Unicode)
  notevalue <- symbols[parsed[ , 'notevalue']]
  dots  <- gsub('\U1D16D', '.', parsed[ , 'dots'])
  multiples <- gsub('\U2217', '%', parsed[ , 'multiples'])
  
  recip <- paste0(notevalue, multiples, dots)
  
  rint <- read.recip2rhythmInterval(recip)
  
  #
  divides <- as.numeric(gsub('\U2215', '', parsed[ , 'divide']))
  rint / (divides %|% 1)
}

### Numbers ####

duration2rint <- function(x, ...) as.rational(x)

## Rhythm Parsing Dispatch ######################################


### Parse 2rint generic and methods ####


##### As x #####


####. generics ####

#' parse rhythmic information
#' 
#' @name rhythmInterval
#' @export 
rhythmInterval <- function(x, ...) UseMethod('rhythmInterval')

#' @rdname rhythmInterval
#' @export 
rhythmInterval.default <- function(x, ...) as.rational(x, ...)

#' @rdname rhythmInterval
#' @export
rhythmInterval.numeric <- function(x, ...) as.rational(x)

#' @rdname rhythmInterval
#' @export
rhythmInterval.integer <- function(x, ...) as.rational(x)

#' @rdname rhythmInterval
#' @export
rhythmInterval.NULL <- function(x, ...) NULL



#### Characters ####

#' @rdname rhythmInterval
#' @export
rhythmInterval.character <- makeHumdrumDispatcher(list(c('recip', 'kern', 'harm'), makeRE.recip,  recip2rint),
                                                  list('any',                      makeRE.timeSignature, timesignature2rint),
                                                  list('duration',                 makeRE.double, duration2rint),
                                                  funcName = 'rhythmInterval.character',
                                                  outputClass = 'rational')



###################################################################### ### 
# Translating Rhythm Representations (x2y) ###############################
###################################################################### ### 

## Rhythm transformer documentation ####

#' Manipulate pitch data
#'  
#' @name rhythmFunctions
#' @seealso rhythmInterval
NULL

## Rhythm transform maker ####



makeRhythmTransformer <- function(deparser, callname, outputClass = 'character') {
  # this function will create various pitch transform functions
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- alist(x = , ... = , Exclusive = NULL,
                parseArgs = list(), timeArgs = list(),
                graceDurations = FALSE,
                inPlace = FALSE,  memoize = TRUE, deparse = TRUE)
  
  rlang::new_function(args, rlang::expr( {
    
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    c('args', 'parseArgs', 'timeArgs') %<-% specialArgs(rlang::enquos(...), parse = parseArgs, time = timeArgs)

    args$Exclusive <- parseArgs$Exclusive <- parseArgs$Exclusive %||% Exclusive
    deparseArgs <- args
    # parseArgs   <- rhythmArgCheck(parseArgs, !!callname)
    # deparseArgs <- rhythmArgCheck(args,      !!callname)
    
  
    
    # Parse
    parsedRint <- do(rhythmInterval, c(list(x), parseArgs), memoize = memoize, outputClass = 'rhythmInterval')
    
    # if (length(transposeArgs) > 0L && is.tonalInterval(parsedRint)) {
    #   parsedRint <- do(transpose.tonalInterval, c(list(parsedRint), transposeArgs))
    # }
    
    deparseArgs <- c(list(parsedRint), deparseArgs)
    output <- if (deparse && is.rational(parsedRint))  do(!!deparser, deparseArgs, memoize = memoize, 
                                                          outputClass = !!outputClass) else parsedRint
    if (deparse && inPlace) output <- rePlace(output, attr(parsedRint, 'dispatch'))
    
    output
    
  })) %class% 'rhythmFunction'
}


### Rhythm Transformers ####

#' @rdname rhythmFunctions
#' @export 
recip <- makeRhythmTransformer(rint2recip, 'recip')
#' @rdname rhythmFunctions
#' @export 
duration <- makeRhythmTransformer(rint2double, 'duration')









###################################################################### ###
# Manipulating rhythm intervals ##########################################
###################################################################### ###

## Time ####


#' Basic time transformations
#' 
#' @name time
#' @export
bpm2ms <- function(bpm) 60000/bpm

#' @rdname time
#' @export
ms2bpm <- function(ms) 60000/ms

#' @rdname time
#' @export
seconds <- function(x, bpm) {
  x * bpm2ms(bpm) / 250
}


minutes <- function(seconds, format = TRUE) {
  
  sign <- ifelse(seconds >= 0, '', '-')
  seconds <- abs(seconds)
  
  minutes <- seconds %/% 60
  
  seconds <- round(seconds %% 60, 3)
  paste0(sign, minutes, ':', ifelse(seconds >= 10, '', '0'), format(seconds, nsmall = 3L, trim = TRUE))
}

## timelines ----

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
#' The "local" duration of each record would be (in `**duration`):
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
#' `localDuration()` begins with a call to [duration()] on the input argument `x`---the `parseArgs()` argument can be used to pass arguments to the [parser][rhythmParsing] (the `Exclusive` argument is passed as well).
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
#' Note that, `localDuration()` follows the default behavior of [duration()] by treating grace-notes as duration `0`.
#' If you want to use the duration(s) of grace notes, specify `graceDurations = TRUE`.
#'
#' The output representation can be controlled using the `deparser` argument, defaulting to [duration()].
#' For example, `deparser = recip` will return the output in `**recip` format.
#' `...` arguments are passed to the deparser.
#' 
#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/.*krn")
#' 
#' within(humData, localDuration(Token))
#'
#' @param x An input vector, which is parsed for duration information using the [rhythm parser][rhythmParsing].
#' @param choose A function, which takes a vector of `numeric` and returns a single `numeric` value. Defaults to `min`; `max`, `median`, or `mode` might be reasonable alternatives.
#' @param deparser A [rhythm function][rhythmFunction] to generate the output representation.
#' @param parseArgs A `list` of arguments to pass to the [rhythm parser][rhythmInterval()].
#' @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x`
#'   into.
#'
#' @family rhythm analysis tools
#' @export 
localDuration <- function(x,  choose = min, deparser = duration, ..., Exclusive = NULL, parseArgs = list(), groupby = list()) {
  
  checkFunction(choose, 'choose', 'int')
  if (!is.null(deparser)) checkArg(deparser, 'deparser', callname = 'localDuration', classes = c('rhythmFunction'))
  
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
#' @param x An input vector, which is parsed for duration information using the [rhythm parser][rhythmParsing].
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
  durations <- duration(x, Exclusive = Exclusive, ...)
  seconds <- seconds(durations, as.integer(gsub('\\*?MM', '', BPM)))
  
  seconds <- timeline(seconds, Exclusive = Exclusive, groupby = groupby, ...)
  

  
  if (minutes) minutes(seconds) else seconds
  
}

## IOI ----

ioi <- function(x, onsets = !grepl('r', x), ..., 
                groupby = list(),
                inPlace = TRUE) {
  
  rint <- rhythmInterval(x, ...)
  
  windows <- windows(x, onsets, groupby = groupby)
  
  # durations <- duration(rint)
  
  
  for (l in 1:min(max(unique(windows$Length)), 10L)) { # 10 is arbitrary...could be optimized for number of instances?
    windows[Length >= l & Length <= 10L, 
            {
              curClose <- Open + l
              rint[Open] <<- rint[Open] + rint[curClose]
              rint[curClose] <<- rational(NA)
              
            }]
    windows <- windows[Length != l]
  }
  
  if (nrow(windows)) rint <- windowApply(rint, sum, windows = windows, passOutside = TRUE)
  
  dispatch <- attr(rint, 'dispatch')
  output <- reParse(rint, dispatch, reParsers = c('recip', 'duration'))
  
  if (inPlace && is.character(output)) output <- rePlace(output, dispatch)
  
  output[is.na(output) & !is.na(rint)] <- '.'
  output
  
  
}



untie <- function(x, open = '\\[', close = '\\]', ..., 
                  deparser = partialApply(reParse, reParsers =  c('recip', 'duration')), 
                  inPlace = TRUE) {
  
  
  openl <- grepl(open, x)
  closel <- grepl(close, x)
  
  rint <- rhythmInterval(x, ...)
  
  ties <- Map(`:`, which(openl), which(closel))
  
  rint[openl] <- do.call('c', lapply(ties, \(i) sum(rint[i])))
  
  rint[unlist(lapply(ties, '[', i = -1L))] <- rational(NA)
  
  output <- deparser(rint)
  
  if (is.character(output)) {
    if (inPlace) output <- rePlace(output) else humdrumRattr(output) <- NULL
    stringr::str_remove_all(output, paste0(open, '|', close))
  } else {
    
    output
  }
  
 
  
}




findTatum <- function(dur) {
  rational <- lapply(dur, as.rational)
  do.call('gcd', as.list(unique(rational)))
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