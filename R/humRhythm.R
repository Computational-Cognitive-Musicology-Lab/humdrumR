#######################################-
##################rhythmInterval S4 class ####
#######################################-

#' Representation of rhythmic information
#' 
#' This \emph{S4} class is the core rhythm representation in the 
#' \code{\link[humdrumR:humdrumR]{humdrumR package}}.
#' The object is used to represent rhythmic durations 
#' and metric positions.
#' Each duration is represented in 
#' \href{https://en.wikipedia.org/wiki/Whole_note}{whole note} units.
#' Numerically, these whole note units are represented as a ratio
#' between integers, held in the slots \code{@Numerator} and \code{@Denominator}.
#' This allows use to represent any rational number with no loss of precision
#' due to rounding errors and weak decimal expansions (like \code{0.333333}).
#' Rhythm intervals are similar to standard musical 
#' termoniology (i.e, ``three eighth-notes'' is the ratio \eqn{\frac{3}{8}}).
#' 
#' @section Vectorization:
#' \code{rhythmInterval} inherits from the virtual class 
#' \code{\linkS4class{humdrumVector}}.
#' This means you can apply normal vectorized commands to \code{rhythmInterval}s, 
#' and even put them in \code{\link[base:data.frame]{data.frames}}.
#' 
#' @section Arithmetic:
#' \code{rhythmInterval} objects have arithmetic operations defined.
#' Addition and subtraction are straightword and intuitive (i.e., \eqn{\frac{1/8} + \frac{3/8} = \frac{1/2}}).
#' 
#' Multiplication and division are slightly more complicated: 
#' \href{https://en.wikipedia.org/wiki/Scalar_multiplication}{scalar multiplication}
#' is defined \emph{for rational numbers}: \eqn{\frac{3}{8} * 2 = \frac{3}{4}} 
#' (the result is always a new \code{rhythmInterval}).
#' However, note that a \code{rhythmInterval} cannot be multiplied by another
#' \code{rhythmInterval}---afterall, what would a quarter-note times a quarter-note be? 
#' A \code{rhythmInterval} can be divided by another \code{rhythmInterval} to produce
#' a real number: \eqn{\frac{1}{2} / \frac{1}{4} = 2 }.
#' Like other rational values in \code{R} we can also do either 
#' ``true'' (rational) division (using the \code{\link[base:Arithmetic]{/}} operator)
#' \emph{or} \href{https://en.wikipedia.org/wiki/Euclidean_division}{Euclidean} 
#' division (using the \code{\link[base:Arithmetic]{\%/\%}} operator).
#' Rational division (\code{/}) of a \code{rhythmInterval} by another \code{rhythmInterval}
#' results in a rational number. For instance, \eqn{\frac{1}{2} / \frac{1}{4} = 2}.
#' Rational division of a \code{rhythmInterval} by a rational number results in a
#' new \code{rhythmInterval}: \eqn{\frac{1}{2} / 2 = \frac{1}{4}}.
#' Eucliean (a.k.a., integer) division can only be applied between \code{rhythmInterval}s
#' resulting in an integer quotient---the remainder, which is a \code{rhythmInterval},
#' can be calculated with the \code{\link[base:Arithmetic]{\%\%}} operator.
#' The remainder (a.k.a., \emph{modulo}) operator (\code{\%\%}) is especially
#' useful, for instance in calculating metric positions.
#' 
#' @section Relational Operators:
#' \code{rhythmInterval}s can be compared using the standard
#' \code{\link[base:Comparison]{relational operators}}---\code{==},
#' \code{!=}, \code{>}, \code{>=}, etc.
#' 
#' @slot Numerator Integers 
#' @slot Octave Integers
#' 
#' @export
setClass('rhythmInterval', 
         contains = 'humdrumVector', 
         slots = c(Numerator = 'integer', Denominator = 'integer')) -> rhythmInterval

setValidity('rhythmInterval', 
            function(object) {
                all(getDenominator(object) != 0L)
            }
)

setMethod('initialize', 'rhythmInterval',
          function(.Object, Denominator = 4L, Numerator = 1L) {
            .Object <- callNextMethod()
              
            fraction <- reduce_fraction(getNumerator(.Object), getDenominator(.Object))
            fraction <- do.call('match_size', fraction) 
            fraction <- lapply(fraction, as.integer)
            max_length <- max(lengths(fraction))
            fraction <- lapply(fraction, rep, length.out = max_length)
            
            .Object@Numerator <- fraction$Numerator
            .Object@Denominator <- fraction$Denominator
            .Object
            
          })

reduce_fraction <- function(n ,d) {
  # Used by rhythmInterval initialize method
  gcds <- gcd(n, d)
  
  list(Numerator = as.integer(n / gcds), Denominator = as.integer(d / gcds))
}

gcd <- function(x, y) {
  # Used by reduce_fraction
  r <- x %% y
  ifelse(r, Recall(y, r), y)
}

######rhythmInterval constructors and accessors ####


#' The basic constructor for \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}}.
#' @name rhythmInterval
#' @export
rint <- function(denominator, numerator = 1L) {
    if (any(denominator == 0, na.rm = TRUE)) stop(call. = FALSE, "Can't have rhythmInterval with denominator of 0.")
    new('rhythmInterval', Denominator = as.integer(denominator), Numerator = as.integer(numerator))
}

# rint_empty <- rint(1, 0)

#' @name rhythmInterval
#' @export
getNumerator   <- function(rint) rint@Numerator
#' @name rhythmInterval
#' @export
getDenominator <- function(rint) rint@Denominator



######rhythmInterval vector (and other core) methods ####



####Indexing ####

#' @name rhythmInterval
#' @export
is.rhythmInterval <- function(x) inherits(x, 'rhythmInterval')


#' @name humdrumVector
#' @export
setMethod('is.numeric', signature = c('rhythmInterval'),
          function(x) { TRUE })



######rhythmInterval order/relations methods ####

#' @name rhythmInterval
#' @export
order.rhythmInterval <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
                    order(as.double(x), 
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method
                    )
          }

#' @name rhythmInterval
#' @export
setMethod('Compare', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
              checkSame(e1, e2, 'Compare')
              callGeneric(as.double(e1), as.double(e2))
          })

#' @name rhythmInterval
#' @export
setMethod('Summary', signature = c('rhythmInterval'),
          function(x) {
              read.numeric2rhythmInterval(callGeneric(as.double(x)))
          })


######rhythminterval formatting methods ####

#' @name rhythmInterval
#' @export
setMethod('as.character', c(x = 'rhythmInterval'), function(x) as.recip(x))

#' @name rhythmInterval
#' @export
as.double.rhythmInterval <-  function(x) as.decimal(x)



######rhythmInterval arithmetic methods ####


####Addition

#' @export
setMethod('+', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
                    
            d1 <- getDenominator(e1)
            d2 <- getDenominator(e2)
            
            d3 <- d1 * d2
            n1 <- getNumerator(e1) * (d3 / d1)
            n2 <- getNumerator(e2) * (d3 / d2)
            
            rint(d3, n1 + n2)
          })



#' @export
setMethod('Math', signature = c(x = 'rhythmInterval'),
          function(x) {
                    read.numeric2rhythmInterval(callGeneric(as.double(x)))
                    
          })

# 

####Subtraction

#' @export
setMethod('-', signature = c(e1 = 'rhythmInterval', e2 = 'missing'),
          function(e1) {
              e1@Numerator <- getNumerator(e1) * -1L
              e1
          })




#' @export
setMethod('diff', signature = c('rhythmInterval'),
          function(x, ..., na.rm = TRUE) {
            x <- do.call('c', list(x, ...))
            as.rhythmInterval(diff(as.double(x), na.rm = na.rm))
          })

####Multiplication

#' @export
setMethod('*', signature = c(e1 = 'rhythmInterval', e2 = 'numeric'),
          function(e1, e2) {
            IfElse(abs(e2) < 1, 
                   rint(e1@Denominator     ,  e1@Numerator * e2),
                   rint(e1@Denominator * e2,  e1@Numerator     )
                   )
            
          })

#' @export
setMethod('*', signature = c(e1 = 'numeric', e2 = 'rhythmInterval'),
          function(e1, e2) {
                    e2 * e1
          })


####Division and modulo

#' @export
setMethod('/', signature = c(e1 = 'rhythmInterval', e2 = 'numeric'),
          function(e1, e2) {
            
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            IfElse(abs(e2) < 1, 
                   rint(e1@Denominator  * e2,  e1@Numerator),
                   as.rhythmInterval((e1@Numerator  * 1 / e2) / e1@Denominator)
                   )
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



############################################-
####### Writing rhythm representations ----
############################################-
#' Writing \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}} to various representations
#' 
#' These functions all translate \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}} to 
#' various representations. 
#' 
#' @name rhythmInterval-write
NULL


#### As recip

#' @name rhythmInterval
#' @export
as.recip <- function(...) UseMethod('as.recip')

#' @name rhythmInterval-write
#' @export
as.recip.rhythmInterval <- function(rint) {
          #modify this to print 0 and 00
          num <- getNumerator(rint)
          
          den <- getDenominator(rint)
          
          SIGN <- c('-', '', '')[2 + sign(num)]
          num <- abs(num)
          dots <- log(num + 1L, base = 2L) 
          dots <- dots - 1L
          dots[!dots %in% 1:100  & (log(den, 2) %% 1) == 0 & den != 1] <- 0L
          
          
          den.needdot <- dots != 0L & den != 1L & log(den, 2) %% 1 == 0
          den[num == 0L] <- NA_integer_
          num[den.needdot | num == 0L] <- 1L
          
          den[which(den.needdot)] <- den[which(den.needdot)] / (2 ^ dots[which(den.needdot)])
          
          den[which(den.needdot)] <- .paste(den[which(den.needdot)], 
                                            sapply(dots[which(den.needdot)], strrep, x = '.'))
          
          den[is.na(den)] <- NA_character_
          den <- .paste(SIGN, den)
          output <- IfElse(num == 1L, den, .paste(den, '%', num)) 
          
          if (any(output %in% c('1%2', '1%3', '1%4', '1%6'))) {
                output[output == '1%2'] <- '0'    
                output[output == '1%3'] <- '0.'    
                output[output == '1%4'] <- '00'    
                output[output == '1%6'] <- '00.'    
          }
          output
}


#### As fraction

#' @name rhythmInterval
#' @export
as.fraction <- function(...) UseMethod('as.fraction')

#' @name rhythmInterval-write
#' @export
as.fraction.numeric <- function(n) {
          fractions <- IfElse(is.na(n), NA_character_, attr(MASS::fractions(n), 'fracs'))
          
          gsub('/', '%', fractions)
}

#' @name rhythmInterval-write
#' @export
as.fraction.rhythmInterval <- function(rint) {
          .paste(getNumerator(rint), '%', getDenominator(rint))
}

#### As decimal
#' @name rhythmInterval
#' @export
as.decimal <- function(...) UseMethod('as.decimal')

#' @name rhythmInterval-write
#' @export
as.decimal.rhythmInterval <- function(rint) {
          as.double(getNumerator(rint) / getDenominator(rint))
}


#####################################-
#### Reading rhythm representations ----
#######################################-
#' Reading \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}} from various representations
#' 
#' These functions all translate other pitch representations
#' into \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}}.
#' 
#' These functions all assume that thheir string input is a well-formed
#' example of the target pitch representation, with no extra strings.
#' (The \code{\link[humdrumR:regexDispatch]{regex dispatch}} functions can be 
#' used to clean/filter inputs into these functions.
#' 
#' @name rhythmInterval-read
NULL


#### From recip

#' @name rhythmInterval-read
#' @export
read.recip2rhythmInterval <- function(str) {
          
          uniqstr <- unique(str)
          
          # Get rid of 0 and 00 ---shorthand for double and quadruple whole notes
          uniqstr <- IfElse(grepl('^0\\.|^0$', uniqstr), gsub('^0', '1%2', uniqstr), uniqstr)
          uniqstr <- IfElse(grepl('^00\\.|^00$', uniqstr), gsub('^00', '1%4', uniqstr), uniqstr)
          
          ndots <- stringr::str_count(uniqstr, '\\.')
          rhythmInterval <- gsub('\\.+', '', uniqstr)
          
          rhythmInterval[grepl('%', rhythmInterval)] <- unlist(lapply(rhythmInterval[grepl('%', uniqstr)], function(f) eval(parse(text = gsub('%', '/', f)))))
          
          rhythmInterval <- 1 / as.numeric(rhythmInterval)
          rhythmInterval <- rhythmInterval * (2 - (0.5 ^ (ndots)))
          
          rhythmInterval <- as.rhythmInterval(rhythmInterval)
          
          rhythmInterval[match(str, unique(str))]
          
}

#### From numeric

#' @name rhythmInterval-read
#' @export
read.numeric2rhythmInterval <- function(n) {
          if (!is.numeric(n)) n <- as.numeric(n)
          
          frac <- attr(MASS::fractions(n, cycles = 8), 'frac')
          
          num <- as.integer(stringi::stri_extract_first_words(frac))
          den <- as.integer(stringi::stri_extract_last_words(frac))
          # num <- as.integer(sapply(frac, '[',  i = 1))
          # den <- as.integer(sapply(frac, tail, n = 1))
          den[num == den] <- 1L
          
          rint(den, num)
}

#### From ratio (string, separated by % or /)

#' @name rhythmInterval-read
#' @export
read.fraction2rhythmInterval <- function(str) {
          split <- strsplit(str, split = '%|/')
          
          ns <- lapply(split, function(n) {
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

#### From anything!


#' @name rhythmInterval
#' @export 
setAs('character', 'rhythmInterval', function(from) as.rhythmInterval.character(from))
#' @name rhythmInterval
#' @export 
setAs('numeric', 'rhythmInterval', function(from) as.rhythmInterval.numeric(from))

#' @name rhythmInterval
#' @export 
as.rhythmInterval <- function(...) UseMethod('as.rhythmInterval')

#' @name rhythmInterval-read
#' @export 
as.rhythmInterval.numeric <- read.numeric2rhythmInterval

#' @name rhythmInterval-read
#' @export 
as.rhythmInterval.character <- regexDispatch('Recip'   = read.recip2rhythmInterval,
                                             '[0-9]+[%/][0-9]+' = read.fraction2rhythmInterval,
                                             'Decimal' = read.numeric2rhythmInterval)


#############################################################################-
#### Translating rhythm representations ----
####################################################################-

#' Rhythm translations
#' 
#' These functions translate various rhythm representations
#' between each other. Using the \code{humdrumR} \code{\link[humdrumR:regexDispatch]{regular-expression dispatch system}}
#' they will even (automatically) read parts of a string which represent a rhythm,
#' and translate only that part (leaving the rest of the string unchanged).
#' They all have an option \code{inPlace} which can be set to \code{FALSE}
#' if you want them to discard non-pitch parts of the string(s).
#' 
#' Under the hood, these functions use the \code{\link{humdrumR}} 
#' \code{\link[humdrumR:rhythmInterval]{rhythmInterval}} \code{S4} class as the 
#' fundamental, \emph{lingua franca} representation of rhythm.
#' 
#' @name humRhythm
NULL


#' @name humRhythm
#' @export 
as.fraction.character <- as.fraction.rhythmInterval %.% as.rhythmInterval

#' @name humRhythm
#' @export
as.recip.character <- as.recip.rhythmInterval %.% as.rhythmInterval

#' @name humRhythm
#' @export
as.decimal.character <- as.decimal.rhythmInterval %.% as.rhythmInterval

#################################################-
######Special rhythm functions ####----
##################################################-

#' Tools for analyzing rhythm and meter.
#' 
#' \code{\link[humdrumR:humdrumR]{humdrumR}} includes a number of useful
#' functions for working with rhythms and meter.
#' \describe{
#' \item{\code{\link{rhythmDecompose}}}{Decomposes a series of rhythms in terms of desired pulses.}
#' \item{\code{\link{rhythmOffset}}}{Calculates the cummulative offset of durations from a starting point.}
#' }
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
                 function(rs) {
                           divs <- rs %/% into
                           parts <- into * divs
                           
                           for (i in 2:length(parts)) {
                                     parts[i] <- parts[i] - sum(parts[1:(i - 1)])       
                           }
                           parts
                 }) -> decompositions
          
          lapply(1:length(into),
                 function(j) {
                           do.call('c', lapply(decompositions, '[', j))
                 }) -> decompositions
          
          
          decompositions <- do.call('data.frame', decompositions)
          colnames(decompositions) <- as.character(into)
          rownames(decompositions) <- make.unique(as.character(rhythmInterval))
          decompositions
}

#' Calculate metric positions from duration data.
#' 
#' @family rhythm analysis tools
#' @export
metricPosition <- function(rints, measurelength = rint(1), 
                           beats = rint(c(4, 8, 16, 32))) {
  cumrints <- cumsum(c(rint(1, 0), rints)) %% measurelength
  
  output <- decompose(cumrints, beats)
  durnames <- as.character(rints)
  
  output <- output[1:(nrow(output) - 1), ] 
  rownames(output) <- make.unique(durnames)
  
  for (j in 1:ncol(output)) {
   output[ , j] <- output[ , j] / beats[j]         
  }
  output

}


#' Calculate rhythmic "offset"
#' 
#' Borrowing the term from \code{music21}, rhythmic "offset"
#' refers to a duration of time since a starting point (usually, the beginning
#' of a piece).
#' \code{rhythmOffset} takes a vector of numbers representing durations
#' (maybe \code{\linkS4class{rhythmInterval}s}, maybe other
#' numeric values) and cummulatively sums them from a starting value.
#' The output is a vector of durations of the same type as the input
#' where each output value corresponds to the duration of time elapsed
#' at that point.
#' 
#' @param durations A vector of numeric values representing durations.
#' @param start A duration value (coerced to same class as \code{durations}), from which the
#' offset begins. 
#' @param groups A vector of equal length as \code{durations} representing a grouping factor,
#' usable by \code{\link[base]{tapply}}. If \code{!is.null(groups)}, offsets are calculated
#' for duration values within each group. The \code{start} argument is recycle to match
#' the length of the number of groups, so a different start value can be applied to each group.
#' If \code{is.null(groups)}, offsets are calculated for the whole \code{durations} vector, from the 
#' first \code{start} value.
#' 
#' @family rhythm analysis tools
#' @export
rhythmOffset <- function(durations, start = 0, groups = NULL) {
          start <- as(start, class(durations))
          
          off <- function(d, s) cumsum(c(s, d))[seq_len(length(d))]
                 
          if (is.null(groups)) {
             off(durations, start)
                    
          }   else {
                    if (length(groups) != length(durations)) {
                              stop(call. = FALSE,
                                   "In call to rhythmOffset, length of durations argument and length of groups argument are different.")
                    }
                    
                    dur.groups <- split(as.numeric(durations), as.numeric(groups))
                    durs <- unlist(Map(off, dur.groups, as.numeric(start)))
                    as(durs, class(durations))
                    
          }

          
}

#### Augmentation and dimminution 

#' Scale rhythmic duration
#' @name RhythmScaling
#' @family rhythm analysis tools
#' @export
augment <- function(x, scalar = 2, ...) UseMethod('augment')


#' @name RhythmScaling
#' @export
augment.rhythmInterval <- function(rint, scalar) rint * scalar

#' @name RhythmScaling
#' @export
augment.character <- regexDispatch('Recip'   = as.recip.rhythmInterval %.% augment.rhythmInterval %.% read.recip2rhythmInterval, 
                                   '[0-9]+[%/][0-9]+' = as.fraction.rhythmInterval %.% augment.rhythmInterval %.% read.fraction2rhythmInterval, 
                                   'Decimal' = as.decimal.rhythmInterval %.% augment.rhythmInterval %.% read.numeric2rhythmInterval)


#' @name RhythmScaling
#' @export
diminish <- function(x, scalar = 2, ...) UseMethod('diminish')


#' @name RhythmScaling
#' @export
diminish.rhythmInterval <- function(rint, scalar) rint / scalar

#' @name RhythmScaling
#' @export
diminish.character <- regexDispatch('Recip'   = as.recip.rhythmInterval %.% diminish.rhythmInterval %.% read.recip2rhythmInterval, 
                                   '[0-9]+[%/][0-9]+' = as.fraction.rhythmInterval %.% diminish.rhythmInterval %.% read.fraction2rhythmInterval, 
                                   'Decimal' = as.decimal.rhythmInterval %.% diminish.rhythmInterval %.% read.numeric2rhythmInterval)
