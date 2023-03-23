
## Token S4 class ----

setClassUnion('maybecharacter', c('character', 'NULL'))

setClass('token', contains = 'vector', c(Exclusive = 'maybecharacter')) -> token



#' Humdrum tokens
#' @export
# token <- function(x, Exclusive = NULL, ...) {
#   
#   attr(x, 'Exclusive') <- Exclusive
#   humdrumRattr(x) <- list(...)
#   
#   class(x) <- c('token', class(x))
#   x
#   
#   
# }


setMethod('[', 'token',
          function(x, i, ...) {
            x@.Data <- x@.Data[i, ...]
            x
          })

#' @rdname token
#' @export
# `[.token` <- function(x, ...) {
#   
#   callNextMethod(x)
#   # humdrumRattr(result) <- humdrumRattr(x)
#   # class(result) <- class(x)
#   result
#   
# }



#' @rdname token
#' @export
setMethod('show', 'token',
         function(object ) {
           exclusive <- object@Exclusive
           if (!is.null(exclusive)) {
             cat('**')
             cat(exclusive, sep = '**')
             cat('\n')
             attr(object, 'Exclusive') <- NULL
           }
           x <- object@.Data
           if (is.factor(object)) object <- as.character(object)
           # attributes(object) <- list()
           print(x, quote = FALSE, na.print = '.')
         })

#' @rdname token
#' @export
format.token <- function(x, ...) {
  x[is.na(x)] <- '.'
  x
}

#' @rdname token
#' @export
# c.token <- function(...) {
#   args <- list(...)
#   
#   exclusives <- unique(unlist(lapply(args, attr, which = 'Exclusive')))
#   
#   humattr <- humdrumRattr(args[[1]])
#   
#   args <- lapply(args, \(x) `class<-`(x, class(x)[-1]))
#   result <- do.call('c', args)
#   
#   humdrumRattr(result) <- humattr
#   class(result) <- c('token', class(result))
#   result
#   
# }

setMethod('c', c('token'),
          function(x, ...) {
            args <- list(x, ...)
            exclusives <- unique(unlist(lapply(args, \(arg) if (inherits(arg, 'token')) arg@Exclusive else NULL)))
             
            # humattr <- humdrumRattr(args[[1]])
            output <- args[[1]]
            output@.Data <- c(output@.Data, unlist(lapply(args, 
                                                          \(arg) {
                                                            if (inherits(arg, 'token')) return(arg@.Data)
                                                            if (is.vector(arg)) return(arg)
                                                          })))
             
            output@Exclusive <- exclusives
            # args <- lapply(args, \(x) `class<-`(x, class(x)[-1]))
            # result <- do.call('c', args)
             
            # humdrumRattr(result) <- humattr
            # class(result) <- c('token', class(result))
            output
            
          })



## Math ----

setMethod('Arith', c('token', 'token'),
          function(e1, e2) {
           pitch <- unlist(pitchFunctions)
           
           if (!(e1@Exclusive %in% pitch & e2@Exclusive %in% pitch)) .stop("Can't add these tokens together.")
           
           e1 <- tonalInterval(e1@.Data)
           e2 <- tonalInterval(e2@.Data)
           
           e3 <- callGeneric(e1, e2)
           token(tint2kern(e3), Exclusive = 'kern')
             
          })


setMethod('Summary', c('token'),
          function(x) {
            pitch <- unlist(pitchFunctions)
            
            if (!x@Exclusive %in% pitch) .stop("Can't interpret this token.")
            
            x <- tonalInterval(x@.Data)
            
            output <- callGeneric(x)
            token(tint2kern(output), Exclusive = 'kern')
            
          })