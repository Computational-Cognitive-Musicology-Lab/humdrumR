#################################### ###
# rhythmInterval S4 class ##############
#################################### ###

## Definition, validity, initialization ####


#' Representation of rhythmic information
#' 
#' This *S4* class is the core rhythm representation in the [humdrumR] package.
#' The object is used to represent rhythmic durations 
#' and metric positions.
#' Each duration is represented in 
#' [whole note](https://en.wikipedia.org/wiki/Whole_note) units.
#' Numerically, these whole note units are represented as a ratio
#' between integers, held in the slots `@Numerator` and `@Denominator`.
#' This allows use to represent any rational number with no loss of precision
#' due to rounding errors and weak decimal expansions (like `0.333333`).
#' Rhythm intervals are similar to standard musical 
#' termoniology (i.e, "three eighth-notes" is the ratio (3/8).
#' 
#' @section Vectorization:
#'
#' `rhythmInterval` inherits from the virtual class 
#' [struct].
#' This means you can apply normal vectorized commands to `rhythmInterval`s, 
#' and even put them in [base::data.frame()].
#' 
#' @section Arithmetic:
#'
#' `rhythmInterval` objects have arithmetic operations defined.
#' Addition and subtraction are straightword and intuitive (i.e., (1/8) + (3/8) = (1/2)).
#' 
#' Multiplication and division are slightly more complicated: 
#' [scalar multiplication](https://en.wikipedia.org/wiki/Scalar_multiplication)
#' is defined *for rational numbers*: (3/8) * 2 = (3/4) 
#' (the result is always a new `rhythmInterval`).
#' However, note that a `rhythmInterval` cannot be multiplied by another
#' `rhythmInterval`---afterall, what would a quarter-note times a quarter-note be? 
#' A `rhythmInterval` can be divided by another `rhythmInterval` to produce
#' a real number: (1/2) / (1/4) = 2.
#' Like other rational values in `R` we can also do either 
#' ``true'' (rational) division (using the [/][base::Arithmetic] operator)
#' *or* [Euclidean](https://en.wikipedia.org/wiki/Euclidean_division)
#' division (using the [%%][base::Arithmetic] operator).
#' Rational division (`/`) of a `rhythmInterval` by another `rhythmInterval`
#' results in a rational number. For instance, $(1/2) / (1/4) = 2$.
#' Rational division of a `rhythmInterval` by a rational number results in a
#' new `rhythmInterval`: $(1/2) / 2 = (1/4)$.
#' Eucliean (a.k.a., integer) division can only be applied between `rhythmInterval`s
#' resulting in an integer quotient---the remainder, which is a `rhythmInterval`,
#' can be calculated with the [%%][base::Arithmetic] operator.
#' The remainder (a.k.a., *modulo*) operator (`%%`) is especially
#' useful, for instance in calculating metric positions.
#' 
#' @section Relational Operators:
#'
#' `rhythmInterval`s can be compared using the standard
#' [relational operators][base::Comparison]---`==`, `!=`, `>`, `>=`, etc.
#' 
#' @slot Numerator Integers 
#' @slot Octave Integers
#' 
#' @family {core rhythm representation}
#' @name rhythmInterval
#' @export
setClass('rhythmInterval', 
         contains = 'struct', 
         slots = c(Numerator = 'integer', Denominator = 'integer')) -> rhythmInterval

setValidity('rhythmInterval', 
            function(object) {
                all(object@Denominator[!is.na(object@Denominator)] != 0L)
            }
)

setMethod('initialize', 'rhythmInterval',
          function(.Object, Denominator = 4L, Numerator = 1L) {
            .Object <- callNextMethod()
              
            # negative numbers should live in the numeratora
            Numerator[Denominator < 0L] <- -Numerator[Denominator < 0L]
            Denominator <- abs(Denominator)
            
            fraction <- reduce_fraction(.Object@Numerator, .Object@Denominator)
            fraction <- do.call('match_size', fraction) 
            fraction <- lapply(fraction, as.integer)
            max_length <- max(lengths(fraction))
            fraction <- lapply(fraction, rep, length.out = max_length)
            
            .Object@Numerator <- fraction$Numerator
            .Object@Denominator <- fraction$Denominator
            .Object
            
          })

## Constructors ####


#' The basic constructor for `[humdrumR:rhythmInterval][rhythmIntervals]`.
#' @rdname rhythmInterval
#' @export
rint <- function(denominator, numerator = 1L) {
    # reduced <- reduce_fraction(numerator, denominator)
    
    if (any(denominator == 0, na.rm = TRUE)) stop(call. = FALSE, "Can't have rhythmInterval with denominator of 0.")
    new('rhythmInterval', 
        Denominator = as.integer(denominator), 
        Numerator = as.integer(numerator))
}

## Accessors ####


## Formatting methods ####

#' @rdname rhythmInterval
#' @export
setMethod('as.character', c(x = 'rhythmInterval'), function(x) recip(x))

#' @rdname rhythmInterval
#' @export
as.double.rhythmInterval <-  function(x) as.decimal(x)

## Logic methods ####

### is.methods ####

#' @rdname rhythmInterval
#' @export
is.rhythmInterval <- function(x) inherits(x, 'rhythmInterval')


#' @rdname rhythmInterval
#' @export
setMethod('is.numeric', signature = c('rhythmInterval'),
          function(x) { TRUE })

## Order/relations methods ####


#' @rdname rhythmInterval
#' @export
order.rhythmInterval <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
                    order(as.double(x), 
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method
                    )
          }

#' @rdname rhythmInterval
#' @export
setMethod('Compare', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
              checkSame(e1, e2, 'Compare')
              callGeneric(as.double(e1), as.double(e2))
          })

#' @rdname rhythmInterval
#' @export
setMethod('Summary', signature = c('rhythmInterval'),
          function(x) {
              read.numeric2rhythmInterval(callGeneric(as.double(x)))
          })

## Arithmetic methods ####

### Addition ####

#' @export
setMethod('+', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
                    
            d1 <- e1@Denominator
            d2 <- e2@Denominator
            
            d3 <- d1 * d2
            n1 <- e1@Numerator * (d3 / d1)
            n2 <- e2@Numerator * (d3 / d2)
            
            rint(d3, n1 + n2)
          })



#' @export
setMethod('Math', signature = c(x = 'rhythmInterval'),
          function(x) {
                    read.numeric2rhythmInterval(callGeneric(as.double(x)))
          })


### Subtraction ####

#' @export
setMethod('-', signature = c(e1 = 'rhythmInterval', e2 = 'missing'),
          function(e1) {
              e1@Numerator <- e1@Numerator * -1L
              e1
          })




#' @export
setMethod('diff', signature = c('rhythmInterval'),
          function(x, ..., na.rm = TRUE) {
            x <- do.call('c', list(x, ...))
            rhythmInterval(diff(as.double(x), na.rm = na.rm))
          })

### Multiplication ####


#' @export
setMethod('*', signature = c(e1 = 'rhythmInterval', e2 = 'integer'),
          function(e1, e2) {
             # multiplying by integers is simple!
            rint(.ifelse(e2 == 0, 1L, e1@Denominator), 
                 e1@Numerator * e2)
          })


#' @export
setMethod('*', signature = c(e1 = 'rhythmInterval', e2 = 'numeric'),
          function(e1, e2) {
              # multiplying by float can be hard
              # use MASS::fractions to do hard work
              
              frac <- numeric2fraction(e2) 
              
              e1 <- rint(.ifelse(e2 == 0, 1L, e1@Denominator * frac$Denominator),
                         e1@Numerator   * frac$Numerator)
              e1
          })

#' @export
setMethod('*', signature = c(e1 = 'numeric', e2 = 'rhythmInterval'),
          function(e1, e2) {
              e2 * e1
          })

### Division/modulo  ####

#' @export
setMethod('/', signature = c(e1 = 'rhythmInterval', e2 = 'integer'),
          function(e1, e2) {
              if (any(e2 == 0L)) stop(call. = FALSE, "You can't divide a rhythmInterval by zero.")
              
              rint(e1@Denominator * e2, e1@Numerator)

          })

#' @export
setMethod('/', signature = c(e1 = 'rhythmInterval', e2 = 'numeric'),
          function(e1, e2) {
            if (any(e2 == 0)) stop(call. = FALSE, "You can't divide a rhythmInterval by zero.")
              
              frac <- numeric2fraction(e2) 
              
              rint(e1@Denominator * frac$Numerator,
                   e1@Numerator * frac$Denominator)
          })

#' @export
setMethod('/', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            (e1@Numerator * e2@Denominator) / (e2@Numerator * e1@Denominator)
          })


#' @export
setMethod('%/%', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            (e1@Numerator * e2@Denominator) %/% (e2@Numerator * e1@Denominator)
          })
          
#' @export
setMethod('%%', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
                    if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
                    n <- e1@Numerator * e2@Denominator
                    d <- e2@Numerator * e1@Denominator
                    
                    n <- n %% d
                    
                    rint(d * e2@Denominator,
                         n * e2@Numerator)
          })



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



rint2rational <- function(rint) .paste(rint@Numerator, '/', rint@Denominator)

rint2decimal <- function(rint) as.double(rint@Numerator / rint@Denominator)

rint2integer <- function(rint) {
  dub <- rint2decimal
  
  as.integer(dub * (1 / min(dub)))
}






###################################################################### ### 
# Parsing Rhythm Representations (x2rint) ################################
###################################################################### ### 

## Rhythm parsers ####

### Symbolic ####

recip2rint <- function(str) {
  
  uniqstr <- unique(str)
  
  # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
  uniqstr <- .ifelse(grepl('^0\\.|^0$', uniqstr), gsub('^0', '1%2', uniqstr), uniqstr)
  uniqstr <- .ifelse(grepl('^00\\.|^00$', uniqstr), gsub('^00', '1%4', uniqstr), uniqstr)
  uniqstr <- .ifelse(grepl('^000\\.|^000$', uniqstr), gsub('^000', '1%8', uniqstr), uniqstr)
  
  ndots <- stringr::str_count(uniqstr, '\\.')
  rhythmInterval <- gsub('\\.+', '', uniqstr)
  
  rhythmInterval[grepl('%', rhythmInterval)] <- unlist(lapply(rhythmInterval[grepl('%', uniqstr)], \(f) eval(parse(text = gsub('%', '/', f)))))
  
  rhythmInterval <- 1 / as.numeric(rhythmInterval)
  rhythmInterval <- rhythmInterval * (2 - (0.5 ^ (ndots)))
  
  rhythmInterval <- rhythmInterval(rhythmInterval)
  
  rhythmInterval[match(str, unique(str))]
  
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


### Numeric ####

numeric2rint <- function(n) {
  if (!is.numeric(n)) n <- as.numeric(n)
  
  frac <- as.rational(n)
  
  frac$Denominator[frac$Numerator == frac$Denominator] <- 1L
  
  rint(frac$Denominator, frac$Numerator)
}


rational2rint <- function(str, split = '/') {
  split <- strsplit(str, split = split)
  
  ns <- lapply(split, \(n) {
    if (all(is.na(n))) return(rint(NA))
    
    if (tail(n, 1) == '') n[length(n)] <- 1L
    if (n[1] == '') n[1] <- '1'
    if (any(n == '')) n <- n[n != '']
    
    if (length(n) == 1) n <- c(n, '1') 
    
    n <- as.numeric(n)
    
    ## if more than two numbers in any token, 
    ## evaluate all but the rightmost as division
    if (length(n) >  2L) n <- c(Reduce(`/`, head(n, -1)), tail(n, 1))
    
    rint(n[2], n[1])
  })
  
  do.call('c', ns)
  
}

## Rhythm Parsing Dispatch ######################################


### Parse 2rint generic and methods ####


##### As x #####


####. generics ####


#' @rdname rhythmInterval
#' @export 
rhythmInterval <- function(x, ...) UseMethod('rhythmInterval')

#' @rdname tonalInterval
#' @export
rhythmInterval.rhythmInterval <- function(x, ...) x

#' @rdname rhythmInterval
#' @export 
rhythmInterval.logical <- function(x, ...) vectorNA(length(x), 'rhythmInterval')

#' @rdname rhythmInterval
#' @export
rhythmInterval.NULL <- function(x, ...) NULL


#### Numbers ####


#' @rdname rhythmInterval
#' @export
rhythmInterval.numeric <- numeric2rint
#' @rdname rhythmInterval
#' @export
rhythmInterval.rational <- \(x) .stop("There is no rational -> rhythmInterval method")
#' @rdname rhythmInterval
#' @export
rhythmInterval.fraction <- \(x) .stop("There is no fraction -> rhythmInterval method")
#' @rdname rhythmInterval
#' @export
rhythmInterval.integer  <- \(x) .stop("There is no integer -> rhythmInterval method")


#### Characters ####

#' @rdname rhythmInterval
rhythmInterval.character <- makeHumdrumDispatcher(list(c('recip', 'kern', 'harm'), makeRE.recip,  recip2rint),
                                                  list('duration',                 makeRE.decimal, numeric2rint),
                                                  funcName = 'rhythmInterval.character',
                                                  outputClass = 'rhythmInterval')

#### setAs tonal interval ####

setAs('integer', 'rhythmInterval', function(from) numeric2rint(from))
setAs('numeric', 'rhythmInterval', function(from) numeric2rint(from))
setAs('character', 'rhythmInterval', function(from) char2rint(from))
setAs('matrix', 'rhythmInterval', function(from) rhythmInterval(c(from)) %dim% from)

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
    output <- if (deparse && is.rhythmInterval(parsedRint))  do(!!deparser, deparseArgs, memoize = memoize) else parsedRint
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
duration <- makeRhythmTransformer(rint2decimal, 'duration')









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
rhythmOffset <- function(durations, start = 0, bars = NULL, tatum = 1, as = as.decimal) {
  durations <- as.decimal(durations)
  start <- as.decimal(start)
  tatum <- as.decimal(tatum)
  
  durations <- durations / tatum
  
  
  off <- function(d, s) cumsum(c(s, d))[seq_len(length(d))]
  
  offsets <- if (is.null(bars)) {
    off(durations, start)
    
  }   else {
    if (length(bars) != length(durations)) {
      stop(call. = FALSE,
           "In call to rhythmOffset, length of durations argument and length of groups argument are different.")
    }
    
    dur.groups <- split(as.numeric(durations), as.numeric(bars))
    durs <- unlist(Map(off, dur.groups, as.numeric(start)))
    as(durs, class(durations))
    
  }
  
  if (identical(as, as.decimal)) offsets else as(offsets)
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
#                                    'Decimal' = as.decimal.rhythmInterval %.% augment.rhythmInterval %.% read.numeric2rhythmInterval)
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
#                                     'Decimal' = as.decimal.rhythmInterval %.% diminish.rhythmInterval %.% read.numeric2rhythmInterval)





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
          into <- sort(into, decreasing = TRUE)
          
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
          
          
          decompositions <- do.call('data.frame', decompositions)
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





