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
rint <- function(denominator, numerator = 1L) {
	new('rhythmInterval', Denominator = denominator, Numerator = numerator)
}

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
            # c(length(x), 1)
                    NULL
          })

####Is/As ####

#' @name rhythmInterval-asvector
#' @export
as.data.frame.rhythmInterval <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names)) row.names <- 1:length(x)
  
  value <- list(x)
  attr(value, 'row.names') <- row.names
  attr(value, 'names') <- 'rhythmInterval'
  class(value) <- c('data.frame')
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
setMethod('order', signature = c('rhythmInterval'),
          function(..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
                    x <- do.call('c', list(...))
                    order(as.double(x), 
 			  na.last = na.last,
                          decreasing = decreasing,
                          method = method
                          )
          })

#' @name rhythmInterval-asvector
#' @export
setMethod('sort', signature = c(x = 'rhythmInterval'),
          function(x, decreasing = FALSE) {
                    x[order(x, decreasing = decreasing)]
          })


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
            output <- as.character(object)
            cat(output)
            invisible(output)
          }
)

#' @name rhythmInterval-asvector
#' @export
format.rhythmInterval <- function(x, ...) {
  n <- x@Numerator
  d <- x@Denominator
  dots <- log(n + 1L, base = 2L) 
  dots <- dots - 1L
  dots[!dots %in% 1:100  & (log(d, 2) %% 1) == 0 & d != 1] <- 0L
  
  targets <- dots != 0L & (log(d, 2) %% 1) == 0 & d!= 1
  n[targets] <- 1L
  
  
  d[targets] <- d[targets] / (2 ^ dots[targets])
  d[targets] <- paste0(d[targets], sapply(dots[targets], function(x) paste(rep('.', x), collapse ='')))
  
  output <- ifelse(n == 1L, d, paste0(d, '%', n))
  output
}

#' @name rhythmInterval
#' @export
setMethod('as.character', c(x = 'rhythmInterval'),
          function(x) {
                    n <- getNumerator(x)
                    d <- getDenominator(x)
                    
                    SIGN <- c('-', '', '')[2 + sign(n)]
                    n <- abs(n)
                    dots <- log(n + 1L, base = 2L) 
                    dots <- dots - 1L
                    dots[!dots %in% 1:100  & (log(d, 2) %% 1) == 0 & d != 1] <- 0L
                    
                    targets <- dots != 0L & (log(d, 2) %% 1) == 0 & d!= 1
                    d[n == 0L] <- 0L
                    n[targets | n == 0L] <- 1L
                    
                    d[targets] <- d[targets] / (2 ^ dots[targets])
                    d[targets] <- paste0(d[targets], sapply(dots[targets], function(x) paste(rep('.', x), collapse ='')))
                    d <- paste0(SIGN, d)
                    
                    ifelse(n == 1L, d, paste0(d, '%', n))  
          })

#' @name rhythmInterval
#' @export
as.double.rhythmInterval <-  function(x) as.double(getNumerator(x) / getDenominator(x))



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
            
            rhythmInterval(d3, n1 + n2)
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
            
            rhythmInterval(d3, n1 - n2)
            
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
                   rhythmInterval(e1@Denominator,  e1@Numerator * e2),
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
                   rhythmInterval(e1@Denominator  * e2,  e1@Numerator),
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
                    
                    rhythmInterval(d * e2@Denominator,
                          n * e2@Numerator)
          })



######Special methods ####


#' @export
decompose <- function(rhythmInterval, into = rhythmInterval(c(1, 2, 4, 8, 16, 32))) {
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
          rownames(decompositions) <- as.character(rhythmInterval)
          decompositions
}

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

#####################################-
#### Reading pitch representations ----
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

#' @name rhythmInterval-read
#' @export
setGeneric('as.rhythmInterval', function(x) standardGeneric('as.rhythmInterval'))
#' @export
setMethod('as.rhythmInterval', signature = c(x = 'numeric'),
          function(x) { 
            frac <- attr(MASS::fractions(x, cycles = 8), 'frac')
            
            frac <- strsplit(frac, split = '/')
            
            n <- as.integer(sapply(frac, head, n = 1))
            d <- as.integer(sapply(frac, tail, n = 1))
            d[lengths(frac) == 1] <- 1L
            
            rhythmInterval(d, n)
          })

#' @export
setMethod('as.rhythmInterval', signature = c(x = 'character'),
          function(x) {
            reciptoken <- stringr::str_match(x, '(([0-9][1-9]*%)?[1-9][0-9]*\\.*)')[ , 1]
            
            ndots <- stringr::str_count(reciptoken, '\\.')
            rhythmInterval <- gsub('\\.+', '', reciptoken)
            
            rhythmInterval[grepl('%', rhythmInterval)] <- unlist(lapply(rhythmInterval[grepl('%', recip)], function(f) eval(parse(text = gsub('%', '/', f)))))
            
            rhythmInterval <- 1 / as.numeric(rhythmInterval)
            rhythmInterval <- rhythmInterval * (2 - (0.5 ^ (ndots)))
            
            as.rhythmInterval(rhythmInterval)
            
          })



