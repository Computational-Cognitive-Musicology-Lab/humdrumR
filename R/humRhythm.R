#' @export
setClass('recip', slots = c(Numerator = 'integer', Denominator = 'integer')) -> recip
#' @export
setValidity('recip', 
            function(object) {
              n <- object@Numerator
              d <- object@Denominator
              
              length(n) == length(d) && all(d != 0)
            }
)

#' @export recip
NULL



#' @export
setMethod('initialize', 'recip',
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

#' @export
setMethod('show', signature = c(object = 'recip'), 
          function(object) {
            n <- object@Numerator
            d <- object@Denominator
            
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
            
            output <- ifelse(n == 1L, d, paste0(d, '%', n))
            
            
            cat(output)
            invisible(output)
          }
)



#' @export
gcd <- function(x, y) {
  r <- x %% y
  ifelse(r, Recall(y, r), y)
  
}


#' @export
reduce_fraction <- function(n ,d) {
  gcds <- gcd(n, d)
  
  list(Numerator = as.integer(n / gcds), Denominator = as.integer(d / gcds))
  
}

###
#' @export
setMethod('as.numeric', signature = c(x = 'recip'),
          function(x) { as.double(x@Numerator / x@Denominator) })

#' @export
setMethod('[', signature = c(x = 'recip'),
          function(x, i) {
            if (missing(i)) return(x)
            
            x@Numerator <- x@Numerator[i]
            x@Denominator <- x@Denominator[i]
            
            x
          })

#' @export
setMethod('[[', signature = c(x = 'recip'),
          function(x, i) {
            if (missing(i)) return(x)
            
            x@Numerator <- x@Numerator[i]
            x@Denominator <- x@Denominator[i]
            
            as.numeric(x)
          })

#' @export
setMethod('[<-', signature = c(x = 'recip', i = 'ANY', j = 'missing', value = 'recip'),
          function(x, i, value) {
            if (missing(i) || missing(value)) return(x)
            stopifnot(length(i) == length(value))
            
            x@Numerator[i] <- value@Numerator
            x@Denominator[i] <- value@Denominator
            
            x
          })


setMethod('dim', signature = c(x = 'recip'),
          function(x) {
            c(length(x), 1)
          })

is.atomic.recip <- function(x) TRUE

#' @export
setMethod('c', signature = c('recip'),
          function(x, ...) {
            durs <- list(x, ...)
            
            ns <- unlist(sapply(durs, slot, 'Numerator'))
            ds <- unlist(sapply(durs, slot, 'Denominator'))
            
            recip(ds, ns)
            })

#' @export
setMethod('rep', signature = c(x = 'recip'),
         function(x, ...) {
           x@Numerator <- rep(x@Numerator, ...)
           x@Denominator <- rep(x@Denominator, ...)
           x
           
         })

#' @export
setMethod('length', signature = c(x = 'recip'),
          function(x) {
            length(x@Numerator)
          })

#' @export 
setMethod('is.vector', signature = c(x = 'recip'), function(x) TRUE)

#' @export
setMethod('as.list', signature = c('recip'),
          function(x, ...) {
            x <- list(x, ...)
            x <- do.call('c', x)
            
            lapply(seq_along(x), function(i) x[i])
          })


### arithmatic
# sums and differences

#' @export
setMethod('+', signature = c(e1 = 'recip', e2 = 'recip'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
            d1 <- e1@Denominator
            d2 <- e2@Denominator
            
            d3 <- d1 * d2
            n1 <- e1@Numerator * (d3 / d1)
            n2 <- e2@Numerator * (d3 / d2)
            
            recip(d3, n1 + n2)
          })

#' @export
setMethod('+', signature = c(e1 = 'recip', e2 = 'ANY'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e2 <- as.recip(e2)
		  e1 + e2
          })

#' @export
setMethod('+', signature = c(e1 = 'ANY', e2 = 'recip'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e1 <- as.recip(e1)
		  e1 + e2
          })


#' @export
setMethod('sum', signature = c('recip'),
          function(x, ..., na.rm = TRUE) {
            x <- list(x, ...)
            x <- do.call('c', x)
            x <- as.numeric(x)
            as.recip(sum(x))
          })

#' @export
setMethod('cumsum', signature = c(x = 'recip'),
          function(x ) {
            x <- as.numeric(x)
            as.recip(cumsum(x))
          })




#' @export
setMethod('-', signature = c(e1 = 'recip', e2 = 'recip'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
            d1 <- e1@Denominator
            d2 <- e2@Denominator
            
            d3 <- d1 * d2
            n1 <- e1@Numerator * (d3 / d1)
            n2 <- e2@Numerator * (d3 / d2)
            
            recip(d3, n1 - n2)
            
          })

#' @export
setMethod('-', signature = c(e1 = 'recip', e2 = 'ANY'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e2 <- as.recip(e2)
		  e1 - e2
          })

#' @export
setMethod('-', signature = c(e1 = 'ANY', e2 = 'recip'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1, e2, toEnv = TRUE)
            
		  e1 <- as.recip(e1)
		  e1 - e2
          })

#' @export
setMethod('diff', signature = c('recip'),
          function(x, ..., na.rm = TRUE) {
            x <- list(x, ...)
            x <- do.call('c', x)
            x <- as.numeric(x)
            as.recip(diff(x))
          })




# products and divisos
#

#' @export
setMethod('*', signature = c(e1 = 'recip', e2 = 'numeric'),
          function(e1, e2) {
            stopifnot(all(e2 >= 0))
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            IfElse(e2 < 1, 
                   as.recip(e1@Numerator / (e1@Denominator * 1 / e2)), 
                   recip(e1@Denominator, 
                         e1@Numerator * e2))
            
          })

#' @export
setMethod('*', signature = c(e1 = 'numeric', e2 = 'recip'),
          function(e1, e2) {
            stopifnot(all(e1 >= 0))
            
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            IfElse(e1 < 1, 
                   as.recip(e1@Numerator / (e1@Denominator * 1 / e1)), 
                    recip(e2@Denominator, 
                          e2@Numerator * e1))
            
          })

#' @export
setMethod('/', signature = c(e1 = 'recip', e2 = 'numeric'),
          function(e1, e2) {
            stopifnot(all(e2 > 0))
            
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            IfElse(e2 < 1, 
                   as.recip((e1@Numerator  * 1 / e2) / e1@Denominator), 
                    recip(e1@Denominator  * e2, 
                          e1@Numerator))
          })

#' @export
setMethod('/', signature = c(e1 = 'recip', e2 = 'recip'),
          function(e1, e2) {
            if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            (e1@Numerator * e2@Denominator) / (e2@Numerator * e1@Denominator)
          })

#' @export
setGeneric('as.recip', function(x) standardGeneric('as.recip'))
#' @export
setMethod('as.recip', signature = c(x = 'numeric'),
          function(x) { 
            frac <- attr(MASS::fractions(x, cycles = 8), 'frac')
            
            frac <- strsplit(frac, split = '/')
            
            n <- as.integer(sapply(frac, head, n = 1))
            d <- as.integer(sapply(frac, tail, n = 1))
            d[lengths(frac) == 1] <- 1L
            
            recip(d, n)
          })

#' @export
setMethod('as.recip', signature = c(x = 'character'),
          function(x) {
            reciptoken <- stringr::str_match(x, '(([0-9][1-9]*%)?[1-9][0-9]*\\.*)')[ , 1]
            
            ndots <- stringr::str_count(reciptoken, '\\.')
            recip <- gsub('\\.+', '', reciptoken)
            
            recip[grepl('%', recip)] <- unlist(lapply(recip[grepl('%', recip)], function(f) eval(parse(text = gsub('%', '/', f)))))
            
            recip <- 1 / as.numeric(recip)
            recip <- recip * (2 - (0.5 ^ (ndots)))
            
            as.recip(recip)
            
          })


#' @export
as.data.frame.recip <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (is.null(row.names)) row.names <- 1:length(x)
  
  value <- list(x)
  attr(value, 'row.names') <- row.names
  attr(value, 'names') <- 'recip'
  class(value) <- c('data.frame')
  value
}

#' @export
format.recip <- function(x, ...) {
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
