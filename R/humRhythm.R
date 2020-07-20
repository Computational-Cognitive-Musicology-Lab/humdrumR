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
#' \code{\linkS4class{struct}}.
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
         contains = 'struct', 
         slots = c(Numerator = 'integer', Denominator = 'integer')) -> rhythmInterval

setValidity('rhythmInterval', 
            function(object) {
                all(object@Denominator != 0L)
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

######rhythmInterval constructor ####


#' The basic constructor for \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}}.
#' @name rhythmInterval
#' @export
rint <- function(denominator, numerator = 1L) {
    # reduced <- reduce_fraction(numerator, denominator)
    
    if (any(denominator == 0, na.rm = TRUE)) stop(call. = FALSE, "Can't have rhythmInterval with denominator of 0.")
    new('rhythmInterval', 
        Denominator = as.integer(denominator), 
        Numerator = as.integer(numerator))
}



######rhythmInterval vector (and other core) methods ####



####Indexing ####

#' @name rhythmInterval
#' @export
is.rhythmInterval <- function(x) inherits(x, 'rhythmInterval')


#' @name rhythmInterval
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

# 

####Subtraction

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
            as.rhythmInterval(diff(as.double(x), na.rm = na.rm))
          })

####Multiplication

#' @export
setMethod('*', signature = c(e1 = 'rhythmInterval', e2 = 'integer'),
          function(e1, e2) {
             # multiplying by integers is simple!
            rint(IfElse(e2 == 0, 1L, e1@Denominator), 
                 e1@Numerator * e2)
          })


#' @export
setMethod('*', signature = c(e1 = 'rhythmInterval', e2 = 'numeric'),
          function(e1, e2) {
              # multiplying by float can be hard
              # use MASS::fractions to do hard work
              
              frac <- numeric2fraction(e2) 
              
              e1 <- rint(IfElse(e2 == 0, 1L, e1@Denominator * frac$Denominator),
                         e1@Numerator   * frac$Numerator)
              e1
          })

#' @export
setMethod('*', signature = c(e1 = 'numeric', e2 = 'rhythmInterval'),
          function(e1, e2) {
              e2 * e1
          })


####Division and modulo
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
          num <- rint@Numerator
          den <- rint@Denominator
          
          
          # Get the sign
          SIGN <- c('-', '', '')[2 + sign(num)]
          num <- abs(num)
          den[num == 0L] <- 1L
          
          while (any(doubles <- den <= 1L & den >= .25 & num > 2L & num < 16L & num %% 2 == 0 & num %% 5 != 0)) {
              # if we want dotted "0" "00" or "000" values, we need to do some transformations
              IfElse(doubles, den / 2, den) -> den
              IfElse(doubles, num / 2, num) -> num
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
          
          output <- IfElse(num == 1L, den, .paste(den, '%', num)) 

          .paste(SIGN, output, strrep('.', dots))

}


#### As fraction




#' @name rhythmInterval-write
#' @export
as.ratio.rhythmInterval <- function(rint) {
          .paste(rint@Numerator, '/', rint@Denominator)
}

#### As decimal

#' @name rhythmInterval-write
#' @export
as.decimal.rhythmInterval <- function(rint) {
          as.double(rint@Numerator / rint@Denominator)
}

#' @name rhythmInterval-write
#' @export
#' 
as.decimal.integer <- function(x) as.numeric(x)
#' @name rhythmInterval-write
#' @export
as.decimal.numeric <- function(x) x


#### As unicode duration string

notevalue.unicode <- data.frame(stringsAsFactors = FALSE,
                                Unicode = c('\U1D15C', '\U1D15D', '\U1D15E', '\U1D15F', 
                                            '\U1D160', '\U1D161', '\U1D162', '\U1D163', '\U1D164'),
                                Recip = c('0', '1', '2', '4', '8', '16', '32', '64', '128'))

#' @name rhythmInterval
#' @export
as.notevalue <- function(...) UseMethod('as.notevalue')


#' @name rhythmInterval-write
#' @export
as.notevalue.rhythmInterval <- function(rint) {
    recip <- as.recip(rint)
    
    # base notation
    parser <- REparser(denominator = "^[0-9]+", 
                       numerator = "(%[1-9][0-9]*)?", 
                       dots = '[.]*$')
    parsed <- parser(recip)
    
    
    
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
        newbase <- IfElse(log(newbase, 2) %% 1L == 0,
                          as.character(newbase),
                          '1')
        
        base[which(unknown)[fitbase]] <- symbols[newbase[fitbase]]
        divides[which(unknown)[fitbase]] <- paste0("\U2215", primes[i])
        
        unknown <- is.na(base) & !is.na(recip)
        i <- i + 1L
    }
    
    # add multiples
    multiples <- IfElse(parsed[ , 'numerator'] == "", 
                        "",
                        paste0("\U2217", 
                               stringr::str_sub(parsed[, 'numerator'], start = 2)))
    notes <- paste0(base, divides, multiples)
    
    # add dots
    dots <- gsub('.', '\U1D16D', parsed[ , 'dots'])
    
    paste0(notes, dots)
    
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
          uniqstr <- IfElse(grepl('^000\\.|^000$', uniqstr), gsub('^000', '1%8', uniqstr), uniqstr)
          
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
          
          frac <- numeric2fraction(n)
          
          frac$Denominator[frac$Numerator == frac$Denominator] <- 1L
          
          rint(frac$Denominator, frac$Numerator)
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



#' @name rhythmInterval-read
#' @export
#### From unicode note values.
read.notevalue2rhythmInterval <- function(notevalues) {
    
    
    
    parser <- REparser(notevalue = paste0('^[', 
                                          paste(notevalue.unicode$Unicode, collapse = ''), 
                                          ']'),
                       divide = "(\U2215[1-9][0-9]*)?",
                       multiples = "(\U2217[1-9][0-9]*)?",
                       dots = '\U1D16D*')
    
    parsed <- parser(notevalues)
    
    # 
    symbols <- setNames(notevalue.unicode$Recip, notevalue.unicode$Unicode)
    notevalue <- symbols[parsed[ , 'notevalue']]
    dots  <- gsub('\U1D16D', '.', parsed[ , 'dots'])
    multiples <- gsub('\U2217', '%', parsed[ , 'multiples'])
    
    recip <- paste0(notevalue, multiples, dots)
    
    rint <- read.recip2rhythmInterval(recip)
    
    #
    divides <- as.numeric(gsub('\U2215', '', parsed[ , 'divide']))
    rint / IfElse(is.na(divides), 1, divides)
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
as.rhythmInterval.character <- regexDispatch('NoteValue' = read.notevalue2rhythmInterval,
                                             'Recip' = read.recip2rhythmInterval,
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
metricPosition <- function(rints, bars = NULL, 
                           beats = rint(c(2, 4, 8, 16, 32))) {
  
  offset <- rhythmOffset(rints, bars = bars, as = as.rhythmInterval)
  
  output <- rhythmDecompose(offset, into = beats)
  # durnames <- as.character(rints)
  
  output <- output[1:(nrow(output) - 1), ] 
  # rownames(output) <- make.unique(durnames)
  
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
                                   '[0-9]+[%/][0-9]+' = as.ratio.rhythmInterval %.% augment.rhythmInterval %.% read.fraction2rhythmInterval, 
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
                                   '[0-9]+[%/][0-9]+' = as.ratio.rhythmInterval %.% diminish.rhythmInterval %.% read.fraction2rhythmInterval, 
                                   'Decimal' = as.decimal.rhythmInterval %.% diminish.rhythmInterval %.% read.numeric2rhythmInterval)


#### Miscalaneous (however that is supposed to be spelled)

#' Time transformations
#' 
#' @name time
#' @export
bpm2ms <- function(bpm) 60000/bpm

#' @name time
#' @export
ms2bpm <- function(ms) 60000/ms



