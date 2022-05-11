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


#' Onset/Offset interval since beginning.
#' 
#' Refers to a duration of rhythmic time elapsed since a starting point (usually, the beginning
#' of a piece).
#' In `music21` these are described as "offsets"---however,
#' we prefer to reserve the words "onset" and "offset" to refer
#' to the beginning (attacK) and end (release) of rhythmic events.

#' `elapsed` takes a vector of numbers representing durations
#' (numeric values) and cummulatively sums them from a starting value.
#' Unlike [sigma()], `elapsed` returns both the timestamp of the onset of 
#' each rhythmic duration *and* the offset.
#' `elapsed` interprets the first duration as starting at zero---or a different
#' value specified by the `start` argument.
#' 
#' @return A S3 object of class `"rhythmOffset"`, which
#' is essentially a data.frame with two columns---`Onset` and `Offset`---
#' of numeric values of the same class as the input `durations` argument.
#' 
#' 
#' @param durations A vector of numeric values representing durations.
#' @param start A duration value (coerced to same class as `durations`), from which the
#' offset begins. 
#' 
#' @family rhythm analysis tools
#' @export
OIs <- function(durations, start = 0L) {
  start <- as(start, class(durations))
  
  offset <- sigma(c(start, durations))
  struct2data.frame(Onset = head(offset, -1), Offset = tail(offset, -1)) %class% "rhythmOffset"
}                           

rhythmAlign <- function(x, y) {
  tick <- gcd(min(x), min(y))
  xi <- as.integer(x / tick)
  yi <- as.integer(y / tick)
  
  xi <- elapsed(xi)$On
  yi <- elapsed(yi)$On
  
  alli <- union(xi, yi)
  
  ox <- vectorNA(length(alli), class(x))
  oy <- vectorNA(length(alli), class(y))
  ox[match(xi, alli)] <- x
  oy[match(yi, alli)] <- y
  
  remove <- is.na(ox) & is.na(oy)
  
  
  struct2data.frame(ox[!remove], oy[!remove])
  
  
  
}


durations <- function(ois) {
  ois$Offset - ois$Onset
}



IOIs <- function(ois) {
  c(diff(ois$Onset), as(NA, class(ois$Onset)))
}


tatum <- function(dur) {
  gcd(unique(dur))
}
