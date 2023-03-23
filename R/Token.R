
## Token S4 class ----

setClassUnion('maybecharacter', c('character', 'NULL'))

setClass('token', contains = 'vector', c(Exclusive = 'maybecharacter', Attributes = 'list'))

#' Humdrum tokens
#' @export
token <- function(x, Exclusive = NULL, ...) {
  new('token', x, Exclusive = Exclusive, Attributes = list(...))
}


#' @rdname token
#' @export
setMethod('[', 'token',
          function(x, i, ...) {
            x@.Data <- x@.Data[i, ...]
            x
          })




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
setMethod('c', c('token'),
          function(x, ...) {
            args <- list(x, ...)
            exclusives <- unique(unlist(lapply(args, \(arg) if (inherits(arg, 'token')) arg@Exclusive else NULL)))
             
            attributes <- unlist(lapply(args, \(arg) if (inherits(arg, 'token')) arg@Attributes else NULL), recursive = FALSE)
            attributes <- attributes[!duplicated(.names(attributes))]

            values <-  unlist(lapply(args, 
                                     \(arg) {
                                       if (inherits(arg, 'token')) return(arg@.Data)
                                       if (is.vector(arg)) return(arg)
                                     }))
            
            new('token', values, Exclusive = exclusives, Attributes = attributes)
         
            
          })



## Math ----

#' @rdname token
#' @export
setMethod('Arith', c('token', 'token'),
          function(e1, e2) {
           pitch <- unlist(pitchFunctions)
           
           if (!(e1@Exclusive %in% pitch & e2@Exclusive %in% pitch)) .stop("Can't add these tokens together.")
           
           e1 <- tonalInterval(e1@.Data)
           e2 <- tonalInterval(e2@.Data)
           
           e3 <- callGeneric(e1, e2)
           token(tint2kern(e3), Exclusive = 'kern')
             
          })

#' @rdname token
#' @export
setMethod('Summary', c('token'),
          function(x) {
            pitch <- unlist(pitchFunctions)
            
            if (!x@Exclusive %in% pitch) .stop("Can't interpret this token.")
            
            x <- tonalInterval(x@.Data)
            
            output <- callGeneric(x)
            token(tint2kern(output), Exclusive = 'kern')
            
          })