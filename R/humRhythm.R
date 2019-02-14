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
#' The \code{rhythmInterval} class has all the \code{\link[rhythmInterval-asvector]{basic vector methods}} 
#' defined for it to function like an \code{R} atomic vector (\code{\link[base]{c}}, \code{\link[base]{[}}, 
#' \code{\link[base]{length}}, etc.).
#' (Each slot can actually hold a vector of integers representing
#' Numerators/Denominators). This means you can apply normal vectorized commands to \code{rhythmInterval}s, and even put
#' them in \code{\link[base:data.frame]{data.frames}}.
#' However, \code{R} is limited in this regard---users can't define
#' \code{S4} classes that really act like \code{R} atomics---, so you may 
#' run in to problems if you take this too far.
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
setClass('rhythmInterval', slots = c(Numerator = 'integer', Denominator = 'integer'))  

setValidity('rhythmInterval', 
            function(object) {
              n <- object@Numerator
              d <- object@Denominator
              
              length(n) == length(d) && all(d != 0) && 
		is.integer(d) && is.integer(n)
            }
)

setMethod('initialize', 'rhythmInterval',
          function(.Object, Denominator = 4L, Numerator = 1L) {
            fraction <- reduce_fraction(Numerator, Denominator)
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
rint <- function(denominator, numerator = 1L)  new('rhythmInterval', Denominator = denominator, Numerator = numerator)

#' @name rhythmInterval
#' @export
getNumerator   <- function(rint) rint@Numerator
#' @name rhythmInterval
#' @export
getDenominator <- function(rint) rint@Denominator



######rhythmInterval vector (and other core) methods ####


#' Methods required to make rhythmIntervals act like vectors
#' 
#' These methods allow us to treat 
#' \code{\link[humdrumR:rhythmInterval]{rhythmIntervals}}
#' a lot like base \code{R} 
#' \code{\link[base:vector]{atomic vectors}}.
#' @name rhythmInterval-asvector
NULL 

####Indexing ####

#' @name rhythmInterval-asvector
#' @export
setMethod('[', signature = c(x = 'rhythmInterval'),
          function(x, i) {
            if (missing(i)) return(x)
	    rint(getDenominator(x)[i], getNumerator(x)[i])
          })

#' @name rhythmInterval-asvector
#' @export
setMethod('[<-', signature = c(x = 'rhythmInterval', i = 'ANY', j = 'missing', value = 'rhythmInterval'),
          function(x, i, value) {
            if (missing(i)) return(x)
            
            x@Numerator[i] <- getNumerator(value)
            x@Denominator[i] <- getDenominator(value)
            
            x
          })


####Shape ####

#' @name rhythmInterval-asvector
#' @export
setMethod('c', signature = c('rhythmInterval'),
          function(x, ...) {
            durs <- list(x, ...)
            
            ns <- unlist(sapply(durs, getNumerator))
            ds <- unlist(sapply(durs, getDenominator))
            
            rint(ds, ns)
            })

#' @name rhythmInterval-asvector
#' @export
setMethod('length', signature = c(x = 'rhythmInterval'),
          function(x) {
            length(getNumerator(x))
          })

#' @name rhythmInterval-asvector
#' @export
setMethod('dim', signature = c(x = 'rhythmInterval'),
          function(x) {
            # length(x)
            NULL
          })

####Is/As ####

#' @name rhythmInterval
#' @export
is.rhythmInterval <- function(x) inherits(x, 'rhythmInterval')


#' @name rhythmInterval-asvector
#' @export
as.data.frame.rhythmInterval <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names)) row.names <- 1:length(x)
  
  value <- list(x)
  attr(value, 'row.names') <- row.names
  attr(value, 'names') <- if (is.null(names(x))) 'rhythmInterval' else names(x)
  class(value) <- c('data.frame')
  value
}

#' @name rhythmInterval-asvector
#' @export
as.data.table.rhythmInterval <- function(x, row.names = NULL, optional = FALSE, ...) {
          if (is.null(row.names)) row.names <- 1:length(x)
          
          value <- list(x)
          attr(value, 'row.names') <- row.names
          attr(value, 'names') <- if (is.null(names(x))) 'rhythmInterval' else names(x)
          class(value) <- c('data.table')
          value
}

#' @name rhythmInterval-asvector
#' @export
setMethod('as.vector', signature = c('rhythmInterval'),
          function(x) { x })

#' @name rhythmInterval-asvector
#' @export
setMethod('as.list', signature = c('rhythmInterval'),
          function(x, ...) {
            x <- list(x, ...)
            x <- do.call('c', x)
            
            lapply(seq_along(x), function(i) x[i])
          })

#' @name rhythmInterval-asvector
#' @export
setMethod('is.vector', signature = c('rhythmInterval'),
          function(x) { TRUE })

#' @name rhythmInterval-asvector
#' @export
setMethod('is.numeric', signature = c('rhythmInterval'),
          function(x) { TRUE })

#' @name rhythmInterval-asvector
#' @export
is.atomic.rhythmInterval <- function(x) TRUE

#' @name rhythmInterval-asvector
#' @export
rep.rhythmInterval <- function(x, ...) {
           rint(rep(getDenominator(x), ...),
		rep(getNumerator(x), ...))
}

######rhythmInterval order/relations methods ####

#' @name rhythmInterval-asvector
#' @export
order.rhythmInterval <- function(x, na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
                    order(as.double(x), 
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method
                    )
          }

#' @name rhythmInterval-asvector
#' @export
sort.rhythmInterval <- function(x, decreasing = FALSE) {
                    x[order(x, decreasing = decreasing)]
          }


#' @name rhythmInterval
#' @export
setMethod('==', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            getNumerator(e1) == getNumerator(e2) & 
		getDenominator(e1) == getDenominator(e2)
          })

#' @name rhythmInterval
#' @export
setMethod('!=', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            getNumerator(e1) != getNumerator(e2) |
		getDenominator(e1) != getDenominator(e2)
          })


#' @name rhythmInterval
#' @export
setMethod('>', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
                    as.double(e1) > as.double(e2)
          })

#' @name rhythmInterval
#' @export
setMethod('>=', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
                    as.double(e1) >= as.double(e2)
          })

#' @name rhythmInterval
#' @export
setMethod('<', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
                    as.double(e1) < as.double(e2)
          })

#' @name rhythmInterval
#' @export
setMethod('<=', signature = c('rhythmInterval', 'rhythmInterval'),
          function(e1, e2) {
                    as.double(e1) <= as.double(e2)
          })


######rhythminterval formatting methods ####

#' @name rhythmInterval-asvector
#' @export
setMethod('show', signature = c(object = 'rhythmInterval'), 
          function(object) {
            output <- as.recip(object)
            cat(output)
            invisible(output)
          }
)

#' @name rhythmInterval-asvector
#' @export
format.rhythmInterval <- function(x, ...) {
          as.recip(x)
  # n <- x@Numerator
  # d <- x@Denominator
  # dots <- log(n + 1L, base = 2L) 
  # dots <- dots - 1L
  # dots[!dots %in% 1:100  & (log(d, 2) %% 1) == 0 & d != 1] <- 0L
  # 
  # targets <- dots != 0L & (log(d, 2) %% 1) == 0 & d!= 1
  # n[targets] <- 1L
  # 
  # 
  # d[targets] <- d[targets] / (2 ^ dots[targets])
  # d[targets] <- paste0(d[targets], sapply(dots[targets], function(x) paste(rep('.', x), collapse ='')))
  # 
  # output <- ifelse(n == 1L, d, paste0(d, '%', n))
  # output
}

#' @name rhythmInterval
#' @export
setMethod('as.character', c(x = 'rhythmInterval'),
          function(x) as.recip(x))

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
setMethod('+', signature = c(e1 = 'rhythmInterval', e2 = 'ANY'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e2 <- as.rhythmInterval(e2)
		  e1 + e2
          })

#' @export
setMethod('+', signature = c(e1 = 'ANY', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e1 <- as.rhythmInterval(e1)
		  e1 + e2
          })


#' @export
setMethod('sum', signature = c(x = 'rhythmInterval'),
          function(x, ..., na.rm = TRUE) {
            x <- do.call('c', list(x, ...))
            as.rhythmInterval(sum(as.double(x), na.rm = na.rm))
          })

#' @export
setMethod('cumsum', signature = c(x = 'rhythmInterval'),
          function(x ) {
            as.rhythmInterval(cumsum(as.double(x)))
          })


####Subtraction

#' @export
setMethod('-', signature = c(e1 = 'rhythmInterval', e2 = 'missing'),
          function(e1, e2) {
                    e1 * -1
          })


#' @export
setMethod('-', signature = c(e1 = 'rhythmInterval', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
            d1 <- e1@Denominator
            d2 <- e2@Denominator
            
            d3 <- d1 * d2
            n1 <- e1@Numerator * (d3 / d1)
            n2 <- e2@Numerator * (d3 / d2)
            
            rint(d3, n1 - n2)
            
          })

#' @export
setMethod('-', signature = c(e1 = 'rhythmInterval', e2 = 'ANY'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e2 <- as.rhythmInterval(e2)
		  e1 - e2
          })

#' @export
setMethod('-', signature = c(e1 = 'ANY', e2 = 'rhythmInterval'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e1 <- as.rhythmInterval(e1)
		  e1 - e2
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
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            IfElse(abs(e2) < 1, 
                   rint(e1@Denominator,  e1@Numerator * e2),
                   as.rhythmInterval(e1@Numerator / (e1@Denominator * 1 / e2))
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
          
          den[den.needdot] <- den[den.needdot] / (2 ^ dots[den.needdot])
          
          den[den.needdot] <- paste0(den[den.needdot], 
                                 sapply(dots[den.needdot], 
                                        function(rint) paste(rep('.', rint), collapse = '')))
          
          den[is.na(den)] <- "empty rhythm"
          den <- paste0(SIGN, den)
          output <- IfElse(num == 1L, den, paste0(den, '%', num)) 
          
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
          fractions <- attr(MASS::fractions(n), 'fracs')
          
          gsub('/', '%', fractions)
}

#' @name rhythmInterval-write
#' @export
as.fraction.rhythmInterval <- function(rint) {
          paste0(getNumerator(rint), '%', getDenominator(rint))
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
          
          frac <- strsplit(frac, split = '/')
          
          num <- as.integer(sapply(frac, head, n = 1))
          den <- as.integer(sapply(frac, tail, n = 1))
          den[lengths(frac) == 1] <- 1L
          
          rint(den, num)
}

#### From ratio (string, separated by % or /)

#' @name rhythmInterval-read
#' @export
read.fraction2rhythmInterval <- function(str) {
          split <- strsplit(str, split = '%|/')
          
          ns <- lapply(split, function(n) {
                    if (tail(n, 1) == '') n[length(n)] <- 1L
                    if (n[1] == '') n[1] <- '1'
                    if (any(n == '')) n <- n[n != '']
                    
                    if (length(n) == 1) n <- c(n, '1') 
                    
                    n <- as.numeric(n)
                    
                    ## if more than two numbers in any token, 
                    ## evaluate all but the rightmost as division
                    if (length(n) >  2L) n <- c(Reduce(`/`, head(n, -1)), tail(n, 1))
                    
                    n
          })
          
          do.call('c', lapply(ns, function(n) rint(n[2], n[1])))
          
}

#### From anything!


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

#################################################-
######Special rhythm functions ####----
##################################################-

#' @export
decompose <- function(rhythmInterval, into = rint(c(1, 2, 4, 8, 16, 32))) {
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

#' @export
metricposition <- function(rints, measurelength = rint(1), 
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

#### Augmentation and dimminution

#' Scale rhythmic duration
#' @name RhythmScaling
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