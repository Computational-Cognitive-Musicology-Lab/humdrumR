#################################### ###
# rational S4 class ####################
#################################### ###


## rational documentation ----



#' Rational numbers
#' 
#' R has no built in rational number representation; `humdrumR` defines one.
#'
#' Using rational numbers, we can represent numbers like 1/3 without any numeric inaccuracies.
#' In other words, \eqn{1/3 * 3 = 3}, never \eqn{.999999999}.
#' On the other hand, if our rational numbers start to have numerators or demoninators that are too large, we can run into 
#' integer overflow problems.
#' Since the rational numbers we'll be using in the context of music analysis are relatively simple,
#' we can safely use such numbers without any numeric inaccuracy.
#' 
#' `fraction` is a class (and associated constructor) which represents rational numbers as `character` strings.
#' Unlike `rational`, the `fraction` class is not numeric and thus cannot do arithmetic.
#' However, `fraction` can be converted to/from `rational`.
#' 
#' 
#' @seealso [as.real()] [as.numeric()] 
#' @family {humdrumR numeric functions}
#' @name rational
NULL

## Definition, validity, initialization ####


setClass('rational', 
         contains = 'struct', 
         slots = c(Numerator = 'integer64', Denominator = 'integer64')) 

setValidity('rational', 
            function(object) {
                na <- is.na(object@Denominator)
                all(na) || all(object@Denominator[!na] != as.integer64(0L))
            }
)


setMethod('initialize', 'rational',
          function(.Object, Numerator = as.integer64(1L), Denominator = as.integer64(4L)) {
              .Object <- callNextMethod()
              # negative numbers should live in the numerator
              na <- is.na(Numerator) | is.na(Denominator)
              
              Numerator[!na & Denominator < 0L] <- -Numerator[!na & Denominator < 0L]
              Denominator <- abs(Denominator)
              
              fraction <- reduce_fraction(Numerator, Denominator)
              # fraction <- do.call('match_size', fraction) 
              # fraction <- lapply(fraction, as.integer64)
              
              .Object@Numerator <- fraction$Numerator
              .Object@Denominator <- fraction$Denominator
              .Object
              
          })


## Constructors ####

#' @rdname rational
#' @export
rational <- function(numerator, denominator = as.integer64(1L)) {
    if (any(denominator == 0L, na.rm = TRUE)) stop(call. = FALSE, "Can't have rational number with denominator of 0.")
    match_size(numerator = numerator, denominator = denominator, toEnv = TRUE)
    new('rational',  Denominator = as.integer64(denominator),  Numerator = as.integer64(numerator))
}

#' @rdname rational
#' @export
`%R%` <- function(e1, e2) rational(e1, e2)

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
setMethod('order', 'rational', 
          function(x, ..., na.last = TRUE, decreasing = FALSE,
                                 method = c("auto", "shell", "radix")) {
    order(as.double(x), 
          na.last = na.last,
          decreasing = decreasing,
          method = method
    )
})

#' @rdname rational
#' @export
setMethod('Compare', signature = c('rational', 'rational'),
          function(e1, e2) {
              checkSame(e1, e2, 'Compare')
              callGeneric(as.double(e1), as.double(e2))
              # 
              # d1 <- denominator(e1)
              # d2 <- denominator(e2)
              # 
              # d3 <- lcm(d1, d2)
              # 
              # x1 <- numerator(e1) * (d3 / d1)
              # x2 <- numerator(e2) * (d3 / d2)
              # 
              # callGeneric(x1, x2)
              
          })

#' @rdname rational
#' @export
setMethod('Compare', signature = c('rational', 'ANY'),
          function(e1, e2) {
              callGeneric(as.double(e1), as.double(e2))
          })

#' @rdname rational
#' @export
setMethod('Compare', signature = c('ANY', 'rational'),
          function(e1, e2) {
              callGeneric(as.double(e1), as.double(e2))
          })

## Arithmetic methods ####


setGeneric('reciprocal', \(x) standardGeneric('reciprocal'))

setMethod('reciprocal', 'rational', \(x) {
    newden <- vectorNA(length(x), 'integer64')
    valid <- !(x@Numerator == as.integer64(0) | is.na(x@Numerator))
    newden[valid] <- x@Numerator[valid]
    
    rational(x@Denominator, newden)
})
setMethod('reciprocal', 'numeric', \(x) 1/x)

### Math


#' @rdname rational
#' @export
setMethod('Summary', signature = c('rational'),
          function(x) {
              as.rational(callGeneric(as.double(x)))
          })





#' @rdname rational
#' @export
setMethod('prod', 'rational', \(x, ...) {
    rational(prod(x@Numerator), prod(x@Denominator))
    
})

#' @rdname rational
#' @export
setMethod('abs', 'rational', \(x) {
    x@Numerator <- abs(x@Numerator)
    x
})

#' @rdname rational
#' @export
setMethod('sign', 'rational', \(x) {
    as.integer(sign(x@Numerator))
})

#' @rdname rational
#' @export
setMethod('max', 'rational', \(x, ...) {
    x <- c(x, ...)
    x[which.max(as.double(x))]
})

#' @rdname rational
#' @export
setMethod('min', 'rational', \(x, ...) {
    x <- c(x, ...)
    x[which.min(as.double(x))]
})

#' @rdname rational
#' @export
setMethod('mean', 'rational', \(x) {
    sum(x) / rational(length(x))
})

#### Rounding ----

#' @rdname rational
#' @export
setMethod('round', 'rational', 
          \(x) {
              rational(round(x@Numerator / x@Denominator), 1L)
          })

#' @rdname rational
#' @export
setMethod('floor', 'rational', 
          \(x) {
              rational(floor(x@Numerator / x@Denominator), 1L)
})

#' @rdname rational
#' @export
setMethod('ceiling', 'rational', 
          \(x) {
              rational(ceiling(x@Numerator / x@Denominator), 1L)
          })

#' @rdname rational
#' @export
setMethod('trunc', 'rational', 
          \(x) {
              rational(trunc(x@Numerator / x@Denominator), 1L)
          })

#' @rdname rational
#' @export
setMethod('expand', 'rational', 
          \(x) {
              rational(expand(x@Numerator / x@Denominator), 1L)
          })



### Addition ####

#' @export
setMethod('+', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              # if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
              
              d1 <- e1@Denominator
              d2 <- e2@Denominator
              
              d3 <- d1 * d2
              n1 <- e1@Numerator * (d3 / d1)
              n2 <- e2@Numerator * (d3 / d2)
              
              rational(n1 + n2, d3)
          })



#' @rdname rational
#' @export
setMethod('sum', 'rational', \(x, ..., na.rm = FALSE) {
    x <- do.call('c', list(x, ...))
    
    if (any(is.na(x)) && !na.rm) return(rational(NA_integer64_))
    
    x <- x[!is.na(x)]
    
    nums <- x@Numerator
    dens <- x@Denominator
    
    den <- do.call('lcm', as.list.numeric_version(unique(dens)))
    if (is.na(den) || den > as.integer64(1e9)) {
        as.rational(sum(as.double(x)))
    } else {
        rational(sum(nums * (den %/% dens)), den)
    }
    
})

#' @rdname rational
#' @export
setMethod('cumsum', 'rational', \(x) {
    nums <- x@Numerator
    dens <- x@Denominator
    
    den <- unique(dens)
    den <- do.call('lcm', as.list(den))
    if (den > 1e10) {
        as.rational(cumsum(as.double(x)))
    } else {
        rational(cumsum(nums * (den %/% dens)), den)
    }
    
})



cumsumg <- function(x, g) {
    nums <- numerator(x)
    dens <- denominator(x)
    
    den <- do.call('lcm', as.list(sort(unique(dens), decreasing = TRUE)))
    
    if (den > 1e6) {
       as.rational(tapply_inplace(as.double(x), g, cumsum))
        
    } else {
       nums <- nums * (den / dens)
       num <- tapply_inplace(nums, g, cumsum)
       rational(num, den)
    }
    
    
}

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
              # if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              (e1@Numerator * e2@Denominator) %/% (e1@Denominator * e2@Numerator)
              
          })

#' @export
setMethod('%%', signature = c(e1 = 'rational', e2 = 'rational'),
          function(e1, e2) {
              e1 - (e2 * (e1 %/% e2))
          })





###################################################################### ###
# Deparsing Rational Representations (rational2x) ########################
###################################################################### ###



setMethod('as.double', 'rational', \(x) (x@Numerator / x@Denominator) %<-matchdim% x)

setMethod('as.integer', 'rational', \(x) as.integer(as.double(x)) %<-matchdim% x)

setMethod('as.character', 'rational', \(x, sep = '/') paste0(x@Numerator, sep, x@Denominator) %<-matchdim% x)

setMethod('as.logical', 'rational', \(x) (x != rational(0L)) %<-matchdim% x)




###################################################################### ### 
# Parsing Rational Representations (x2rational) ##########################
###################################################################### ### 

#' @rdname rational
#' @export
setGeneric('as.rational', \(x, ...) standardGeneric('as.rational'))

#' @rdname rational
#' @export
setMethod('as.rational', 'rational', force)



#' @rdname rational
#' @export
setMethod('as.rational', 'matrix', \(x) rational(dropdim(x), 1L) %<-matchdim% x)

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
              frac[frac[ , 2] == '', 2] <- '1'
              
              numerator <- as.numeric(frac[ , 1])
              denominator <- as.numeric(frac[ , 2])
              
              denominator[is.na(denominator)] <- 1L
              
              rational(numerator, denominator)
              
          })

#' @rdname rational
#' @export
setMethod('as.rational', 'logical', 
          \(x) {
              as.rational(as.integer64(x))
          })

#' @rdname rational
#' @export
setMethod('as.rational', 'character', 
          \(x, sep = '/|%') {
              x <- strsplit(x, split = sep)
              
              x <- suppressWarnings(lapply(x, as.numeric))
              na <- sapply(x, \(x) any(is.na(x)))
              if (any(na)) warning(call. = FALSE, 'When converting character strings to rational numbers, NAs introduced because ' ,
                                   num2word(sum(na)), plural(sum(na), 
                                                             " strings couldn't be interpreted as numbers.", 
                                                             " string couldn't be interpreted as a number."))
              
              matx <- do.call('cbind', lapply(1:max(2L, max(lengths(x))), \(i) sapply(x, '[', i = i)))
              matx[is.na(matx)] <- 1
              matx[na, ] <- NA
              
              dfx <- as.data.frame(matx)
              
              # whole values interpreted directly as rational
              whole <- sapply(x, \(x) all(is.whole(x))) 
              wholerat <- Reduce('rational', dfx[whole & !na, ])
              
              # nonwhole values are just computed as real and converted to rational
              realrat <- do.call('c', (lapply(Reduce('/', dfx[!whole & !na, ]), as.rational)))
              output <- vectorNA(length(x), 'rational')
              if (length(wholerat) > 0L) output[ whole & !na] <- wholerat
              if (length(realrat) > 0L) output[!whole & !na] <- realrat
              
              output
              
          })

#' @rdname rational
#' @export
setMethod('as.rational', 'fraction',
          \(x, sep = '/|%') {
              as.rational(unclass(x), sep = sep)
          })
          


#### setAs rational  ####

setAs('integer', 'rational', function(from) as.rational(from))
setAs('numeric', 'rational', function(from) as.rational(from))
setAs('logical', 'rational', function(from) as.rational(from))
setAs('character', 'rational', function(from) as.rational(from))

#################################### ###
# Other numeric S3 classes #############
#################################### ###

## Real ----

# this is just an extension of numeric to understand my fraction and rational representations

# #' Real numbers
# #' 
# #' These functions create real numbers that are identical to base R
# #' `numeric` (real) numbers.
# #' However, these numbers are understood by the `humdrumR` [rational numbers][rational()].
# #' 
# #' @family {humdrumR numeric functions}
# #' @seealso [rational()]
# #' @export
# real <- function(x) (as.numeric(x) %class% 'real') %<-matchdim% x
# 
# #' @rdname real
# #' @export
# as.real <- function(x, ...) UseMethod('as.real')
# #' @export
# as.real.character <- function(x) {
#     x[grepl('[^0-9.%/\\(\\)-]', x)] <- NA
#     as.real.fraction(x)
# }
# #' @export
# as.real.numeric <- real
# #' @export
# as.real.rational <- function(x) real(as.double(x))
# #' @export
# as.real.fraction <- function(x) {
#     exprs <- parse(text = stringi::stri_replace_all_fixed(x, '%', '/'))
#     real(sapply(exprs, eval) %<-matchdim% x)
# }




## Fractions ----

# rational numbers as character string

#' @rdname rational
#' @export
fraction <- function(numerator, denominator, sep = '/') {
    .paste(numerator, denominator, sep = sep) %class% 'fraction'
}

#' @rdname rational
#' @export
as.fraction <- function(x, sep = '/') {
    if (!is.rational(x)) x <- as.rational(unclass(x), sep = sep)
    as.character(x, sep = sep) %class% 'fraction'
}

#' @rdname rational
#' @export
as.double.fraction <- function(x) as.double(as.rational(x))
#' @rdname rational
#' @export
as.integer.fraction <- function(x) as.integer(as.rational(x))


#' @export
print.fraction <- function(x) print(unclass(x))
