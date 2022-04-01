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

recip2rint <- function(str) {
  
  
  # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
  str <- .ifelse(grepl('^0\\.|^0$', str),     gsub('^0',   '1%2', str), str)
  str <- .ifelse(grepl('^00\\.|^00$', str),   gsub('^00',  '1%4', str), str)
  str <- .ifelse(grepl('^000\\.|^000$', str), gsub('^000', '1%8', str), str)
  
  ndots <- stringr::str_count(str, '\\.')
  str <- gsub('\\.+', '', str)
  
  rational <- as.rational(str, sep = '%')
  rational <- reciprocal(rational)
  
  
  dots <- 2L ^ ndots
  dotscale <- rational((2L * dots) - 1L, dots)

  rational * dotscale
  
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


#' @rdname rhythmInterval
#' @export 
rhythmInterval <- function(x, ...) UseMethod('rhythmInterval')

#' @rdname rhythmInterval
#' @export 
rhythmInterval.default <- function(x, ...) as.rational(x, ...)

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
                inPlace = FALSE,  memoize = TRUE, deparse = TRUE)
  
  rlang::new_function(args, rlang::expr( {
    
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    args <- lapply(rlang::enexprs(...),
                   \(argExpr) {
                     if (is.call(argExpr) && as.character(argExpr[[1]]) %in% c('parse', 'time')) {
                       type <- as.character(argExpr[[1]])
                       argExpr[[1]] <- quote(list)
                       assign(paste0(type, 'Args'), eval(argExpr), envir = parent.frame(2))
                       NULL
                     } else {
                       rlang::eval_tidy(argExpr)
                     }
                     
                   })
    
    args <- args[!sapply(args, is.null)]
    args$Exclusive <- parseArgs$Exclusive <- parseArgs$Exclusive %||% Exclusive
    deparseArgs <- args
    # parseArgs   <- rhythmArgCheck(parseArgs, !!callname)
    # deparseArgs <- rhythmArgCheck(args,      !!callname)
    
  
    
    # Parse
    parsedRint <- do(rhythmInterval, c(list(x), parseArgs), memoize = memoize)
    
    # if (length(transposeArgs) > 0L && is.tonalInterval(parsedRint)) {
    #   parsedRint <- do(transpose.tonalInterval, c(list(parsedRint), transposeArgs))
    # }
    
    deparseArgs <- c(list(parsedRint), deparseArgs)
    output <- if (deparse && is.rational(parsedRint))  do(!!deparser, deparseArgs, memoize = memoize) else parsedRint
    if (deparse && inPlace) output <- rePlace(output, attr(parsedRint, 'dispatch'))
    
    output
    
  }))
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


#' Time transformations
#' 
#' @name time
#' @export
bpm2ms <- function(bpm) 60000/bpm

#' @name time
#' @export
ms2bpm <- function(ms) 60000/ms



### Offset ####


#' Calculate rhythmic "offset"
#' 
#' Borrowing the term from `music21`, rhythmic "offset"
#' refers to a duration of time since a starting point (usually, the beginning
#' of a piece).
#' `rhythmOffset` takes a vector of numbers representing durations
#' (maybe `[rhythmInterval][rhythmInterval]s`, maybe other
#' numeric values) and cummulatively sums them from a starting value.
#' The output is a vector of durations of the same type as the input
#' where each output value corresponds to the duration of time elapsed
#' at that point.
#' 
#' @param durations A vector of numeric values representing durations.
#' @param start A duration value (coerced to same class as `durations`), from which the
#' offset begins. 
#' @param groups A vector of equal length as `durations` representing a grouping factor,
#' usable by `[base][tapply]`. If `!is.null(groups)`, offsets are calculated
#' for duration values within each group. The `start` argument is recycle to match
#' the length of the number of groups, so a different start value can be applied to each group.
#' If `is.null(groups)`, offsets are calculated for the whole `durations` vector, from the 
#' first `start` value.
#' 
#' @family rhythm analysis tools
#' @export
rhythmOffset <- function(durations, start = rational(0), tatum = rational(1), phase = rational(0)) {
  durations <- rhythmInterval(durations)
  
  durations <- durations / tatum
  
  head(sigma(c(start, durations)), -1L) + phase
}

### Augmentation and Dimminution  ####


# augment <- function(x, scalar = 2, ...) UseMethod('augment')
# 
# 
# #' @name RhythmScaling
# #' @export
# augment.rhythmInterval <- function(rint, scalar) rint * scalar
# 
# #' @name RhythmScaling
# #' @export
# augment.character <- regexDispatch('Recip'   = recip.rhythmInterval %.% augment.rhythmInterval %.% read.recip2rhythmInterval, 
#                                    '[0-9]+[%/][0-9]+' = as.ratio.rhythmInterval %.% augment.rhythmInterval %.% read.fraction2rhythmInterval, 
#                                    'Decimal' = as.double.rhythmInterval %.% augment.rhythmInterval %.% read.numeric2rhythmInterval)
# 
# 
# #' @name RhythmScaling
# #' @export
# diminish <- function(x, scalar = 2, ...) UseMethod('diminish')
# 
# 
# #' @name RhythmScaling
# #' @export
# diminish.rhythmInterval <- function(rint, scalar) rint / scalar
# 
# #' @name RhythmScaling
# #' @export
# diminish.character <- regexDispatch('Recip'   = recip.rhythmInterval %.% diminish.rhythmInterval %.% read.recip2rhythmInterval, 
#                                     '[0-9]+[%/][0-9]+' = as.ratio.rhythmInterval %.% diminish.rhythmInterval %.% read.fraction2rhythmInterval, 
#                                     'Decimal' = as.double.rhythmInterval %.% diminish.rhythmInterval %.% read.numeric2rhythmInterval)





## Meter ####

#' Tools for analyzing rhythm and meter.
#' 
#' [humdrumR] includes a number of useful
#' functions for working with rhythms and meter.
#'
#' + [rhythmDecompose()] decomposes a series of rhythms in terms of desired pulses.
#' + [rhythmOffset()] Calculates the cummulative offset of durations from a starting point.
#' 
#' 
#' @name humMeter
NULL

#' Decompose durations in terms of other durations
#' 
#' 
#' @family rhythm analysis tools
#' @export
rhythmDecompose <- function(rhythmInterval, into = rint(c(1, 2, 4, 8, 16, 32))) {
          # into <- sort(into, decreasing = TRUE)
          
          lapply(as.list(rhythmInterval), 
                 \(rs) {
                           divs <- rs %/% into
                           parts <- into * divs
                           
                           for (i in 2:length(parts)) {
                                     parts[i] <- parts[i] - sum(parts[1:(i - 1)])       
                           }
                           parts
                 }) -> decompositions
          
          lapply(1:length(into),
                 \(j) {
                           do.call('c', lapply(decompositions, '[', j))
                 }) -> decompositions
          
          
          decompositions <- do.call('struct2data.frame', decompositions)
          colnames(decompositions) <- as.character(into)
          rownames(decompositions) <- make.unique(as.character(rhythmInterval))
          decompositions
}

#' Calculate metric positions from duration data.
#' 
#' 
#' @family rhythm analysis tools
#' @export
metricPosition <- function(rints, bars = NULL, 
                           beats = rint(c(2, 4, 8, 16, 32))) {
  
  offset <- rhythmOffset(rints, bars = bars, as = rhythmInterval)
  
  output <- rhythmDecompose(offset, into = beats)
  # durnames <- as.character(rints)
  
  output <- output[1:(nrow(output) - 1), ] 
  # rownames(output) <- make.unique(durnames)
  
  for (j in 1:ncol(output)) {
   output[ , j] <- output[ , j] / beats[j]         
  }
  output

}


# M4/4
# 0/1  1/8 1/4 3/8 1/2 5/8 3/4 7/8

# M3/4

# 0/2.  1/8 1/4 3/8 2/4 5/8

# M6/8

# 0/2.  1/8 2/8 1/4. 4/8 5/8

# M9/8

# 0/%9 1/8 2/8 1/4. 4/8 5/8 6/8 2/4. 7/8 8/8

# M5/8

# 0/8&5 1/8 1/4 3/8 4/8
# 0/8%5 1/8 2/8 1/4. 4/8

beats <- function(dur, beat = rational(1, 4), phase = rational(0), ...) {
  
  beatsize <- sum(beat)
  
  offset <- rhythmOffset(dur, phase = phase, ...)
  
  beatcount <- offset %/% beatsize
  beatoffset <- offset %% beatsize
  
  if (length(beat) > 1L) {
    beatoff <- rhythmOffset(beat)
    subbeat <- Reduce('+', lapply(as.list(beatoff), `<=`, e2 = beatoffset)) 
    
    beatcount <- beatcount * length(beat)
    beatcount <- beatcount + subbeat - 1
    beatoffset <- beatoffset - beatoff[subbeat]
    
  }
  struct2data.frame(beatcount, beatoffset)
  
}

beats2 <- function(moff,  beats = rational(1,4), subdiv = rational(1,8), measure = rational(1)) {
  
  span <- sum(beats)
  beats <- rep(beats, measure %/% span)
  if (sum(beats) != measure) .stop('In call to beats, the beats argument cannot evenly divide the measure span.')
  
  durs <- unique(c(delta(beats), subdiv))
  durs <- durs[durs != rational(0)]
  if (Reduce('gcd', durs) != subdiv) .stop('{subbeat} is not a valid subdivider of the beats pattern.')
  
  beat <- findInterval(moff, cumsum(beats), rightmost.closed = TRUE)
  
  subbeat <- floor(sigma(c(0, head(beats, -1))) / subdiv)
  subbeat <- round(moff / subdiv) - subbeat[beat + 1]
  data.table(Moffset = moff,
             Beat = beat,
             Subbeat = subbeat,
             Remainder = moff %% subdiv)
}


measure <- function(dur, tatum = rational(1L), start = rational(0L), phase = rational(0L)) {
  # if correct meter is known (and aligned with dur)
  if (length(tatum) > 1L && length(tatum) != length(dur)) tatum <- rep(tatum, length.out = length(dur))
  
  dur <- dur / tatum
  # 
  offset <- sigma(c(start, dur)) + phase
  
  mcount <- offset %/% rational(1L)
  moffset <- offset %% rational(1L)
  
  
  struct2data.frame(N = head(mcount, -1), Offset = head(moffset, -1) * tatum)
  
  # if (length(tatum) > 1L && length(unique(tatum)) > 1L) {
    # Reduce(\(x, y) {y + max(x)}, tapply(dur, tatum, \(d) sigma(c(rational(0L), d)) %/% tatum), accumulate = TRUE)
  # } else {
    # sigma(c(rational(0L), dur)) %/% tatum
  # }
  
}


meterAnalyze <- function(dur, meter = c(.25, .25, .25, .25), subdiv = 1/16) {
 measure <- sum(meter)
 
 moff <- measureOffset(dur, measure)
 
 beats <- beats(moff, meter, subdiv = subdiv, measure = measure)
 
 beats$Dur <- dur
 beats$Moff <- moff
 beats
  
}
# test <- data.table(Dur = c('4.','8','4','4','4','4','2','4.','8','4','4.','8','4','4','8','8','2'),
                   # Meter = c(1, 1, 1, 1, 1, 1, .75, .75, .75, .75, .75, .75, .75, 1,1,1,1))
