#################################### ###
# rational S4 class ####################
#################################### ###

## Definition, validity, initialization ####

#' Rational numbers
#' 
#' R has no built in rational number representation; `humdrumR` defines one.
#' 
#' 
#' 
#' @seealso [as.decimal()] [as.numeric()] 
#' @family {humdrumR numeric functions}
#' @name rational
setClass('rational', 
         contains = 'struct', 
         slots = c(Numerator = 'integer', Denominator = 'integer')) 

setValidity('rational', 
            function(object) {
                all(object@Denominator[!is.na(object@Denominator)] != 0L)
            }
)


setMethod('initialize', 'rational',
          function(.Object, Numerator = 1L, Denominator = 4L) {
              .Object <- callNextMethod()
              # negative numbers should live in the numerator
              Numerator[Denominator < 0L] <- -Numerator[Denominator < 0L]
              Denominator <- abs(Denominator)
              
              fraction <- reduce_fraction(.Object@Numerator, .Object@Denominator)
              fraction <- do.call('match_size', fraction) 
              fraction <- lapply(fraction, as.integer)
              
              .Object@Numerator <- fraction$Numerator
              .Object@Denominator <- fraction$Denominator
              .Object
              
          })


## Constructors ####

#' @rdname rational
#' @export
rational <- function(numerator, denominator = 1L) {
    if (any(denominator == 0L, na.rm = TRUE)) stop(call. = FALSE, "Can't have rational number with denominator of 0.")
    
    new('rational',  Denominator = as.integer(denominator),  Numerator = as.integer(numerator))
}

#' @rdname rational
#' @export
`%r%` <- function(e1, e2) rational(e1, e2)

## Accessors ####

#' @rdname rational
#' @export
setGeneric('numerator', \(x) standardGeneric('numerator'))
#' @rdname rational
#' @export
setGeneric('denominator', \(x) standardGeneric('denominator'))
#' @rdname rational
#' @export
setMethod('numerator', 'rational', \(x) x@Numerator)
#' @rdname rational
#' @export
setMethod('denominator', 'rational', \(x) x@Denominator)


setMethod('numerator', 'numeric', \(x) numerator(as.rational(x)))
setMethod('denominator', 'numeric', \(x) denominator(as.rational(x)))


## Formatting methods ####


#' @rdname rational
#' @export
setMethod('as.character', c(x = 'rational'), \(x, sep = '/') paste0(numerator(x), sep, denominator(x)))



## Logic methods ####

### is.methods ####


#' @rdname rational
#' @export
is.rational <- function(x) inherits(x, 'rational')


#' @rdname rational
#' @export
setMethod('is.numeric', signature = c('rational'), \(x) TRUE)


## Order/relations methods ####

#' @rdname rational
#' @export
order.rational <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                                 method = c("auto", "shell", "radix")) {
    order(as.double(x), 
          na.last = na.last,
          decreasing = decreasing,
          method = method
    )
}

#' @rdname rational
#' @export
setMethod('Compare', signature = c('rational', 'rational'),
          function(e1, e2) {
              checkSame(e1, e2, 'Compare')
              callGeneric(as.double(e1), as.double(e2))
          })

#' @rdname rational
#' @export
setMethod('Summary', signature = c('rational'),
          function(x) {
              as.rational(callGeneric(as.double(x)))
          })


#' @rdname rational
#' @export
setMethod('sum', 'rational', \(x, ...) {
    x <- do.call('c', list(x, ...))
    
    nums <- x@Numerator
    dens <- x@Denominator
    
    
    nums <- tapply(nums, dens, sum)
    dens <- tapply(dens, dens, unique)
    den <- do.call('lcm', as.list(dens))
    if (den > 1e6) {
        as.rational(sum(as.double(x)))
    } else {
        rational(sum(nums * (den / dens)), den)
    }
    
})

## Arithmetic methods ####

setGeneric('reciprocal', \(x) standardGeneric('reciprocal'))

setMethod('reciprocal', 'rational', \(x) rational(x@Denominator, x@Numerator))

### Addition ####

#' @export
setMethod('+', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
              
              d1 <- e1@Denominator
              d2 <- e2@Denominator
              
              d3 <- d1 * d2
              n1 <- e1@Numerator * (d3 / d1)
              n2 <- e2@Numerator * (d3 / d2)
              
              rational(n1 + n2, d3)
          })




### Subtraction ####

#' @export
setMethod('-', signature = c(e1 = 'rational', e2 = 'missing'),
          function(e1) {
              e1@Numerator <- e1@Numerator * -1L
              e1
          })

#' @export
setMethod('-', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              e1 + -e2
          })


#' @export
setMethod('diff', signature = c('rational'),
          function(x, ..., na.rm = TRUE) {
              x <- do.call('c', list(x, ...))
              tail(x, -1) - head(x, - 1)
          })

### Multiplication ####


#' @export
setMethod('*', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              rational(e1@Numerator * e2@Numerator, e1@Denominator * e2@Denominator)
          })

#' @export
setMethod('*', signature = c(e1 = 'rational', e2 = 'numeric'),
          function(e1, e2) {
               e1 * as.rational(e2)
          })


#' @export
setMethod('*', signature = c(e1 = 'numeric', e2 = 'rational'),
          function(e1, e2) {
              as.rational(e1) * e2
          })

### Division/modulo  ####


#' @export
setMethod('/', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              e1 * reciprocal(e2)
          })

#' @export
setMethod('/', signature = c(e1 = 'rational', e2 = 'numeric'),
          function(e1, e2) {
              e1 / as.rational(e2)
          })


#' @export
setMethod('/', signature = c(e1 = 'numeric', e2 = 'rational'),
          function(e1, e2) {
              as.rational(e1) / e2
          })



#' @export
setMethod('%/%', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              e3 <- e1 / e2
              
              e3@Numerator %/% e3@Denominator
          })

#' @export
setMethod('%%', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              e1 - (e2 * (e1 %/% e2))
          })




###################################################################### ###
# Deparsing Rational Representations (rational2x) ########################
###################################################################### ###

#' @rdname rational
#' @export
setMethod('as.double', 'rational', \(x) x@Numerator / x@Denominator)

#' @rdname rational
#' @export
setMethod('as.integer', 'rational', \(x) as.integer(as.double(x)))

###################################################################### ### 
# Parsing Rational Representations (x2rational) ##########################
###################################################################### ### 

#' @rdname rational
#' @export
setGeneric('as.rational', \(x) standardGeneric('as.rational'))

#' @rdname rational
#' @export
setMethod('as.rational', 'integer', \(x) rational(x, 1L))

#' @rdname rational
#' @export
setMethod('as.rational', 'numeric', 
          \(x) {
              frac <- attr(MASS::fractions(x, cycles = 8), 'fracs')
              frac <- stringi::stri_split_fixed(frac, '/', simplify = TRUE)
              
              
              if (ncol(frac) == 1L) frac <- cbind(frac, '1')
              
              numerator <- as.numeric(frac[ , 1])
              denominator <- as.numeric(frac[ , 2])
              
              frac <- reduce_fraction(numerator, denominator)
              numerator <- frac$Numerator
              denominator <- frac$Denominator
              
              denominator[is.na(denominator)] <- 1L
              
              rational(numerator, denominator)
              
          })

#' @rdname rational
#' @export
setMethod('as.rational', 'logical', 
          \(x) {
              as.rational(as.integer(x))
          })


