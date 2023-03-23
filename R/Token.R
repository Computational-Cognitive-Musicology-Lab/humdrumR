################################## ###
# token S4 class #####################
################################## ###

## Definition, validity, initialization ####

setClassUnion('maybecharacter', c('character', 'NULL'))


setClass('token', contains = 'vector', c(Exclusive = 'maybecharacter', Attributes = 'list'))

## Constructors ####

#' Humdrum tokens
#' 
#' `token` is an `S4` class which acts as a simple
#' "wrapper" around `atomic` data, allowing `humdrumR` to give
#' that data special treatment.
#' They are basically `atomic` vectors with a known
#' exclusive interpretation.
#' 
#' 
#' @export
token <- function(x, Exclusive = NULL, ...) {
  new('token', x, Exclusive = Exclusive, Attributes = list(...))
}



## Accessors ####

getExclusive <- function(x) if (inherits(x, 'token')) x@Exclusive


#' @rdname token
#' @export
setMethod('c', c('token'),
          function(x, ...) {
            args <- list(x, ...)
            exclusives <- unique(unlist(lapply(args, getExclusive)))
            
            attributes <- unlist(lapply(args, \(arg) if (inherits(arg, 'token')) arg@Attributes else NULL), recursive = FALSE)
            attributes <- attributes[!duplicated(.names(attributes))]
            
            values <-  unlist(lapply(args, 
                                     \(arg) {
                                       if (inherits(arg, 'token')) return(arg@.Data)
                                       if (is.vector(arg)) return(arg)
                                     }))
            
            new('token', values, Exclusive = exclusives, Attributes = attributes)
            
            
          })

#' @rdname token
#' @export
rep.token <- function(x, ...) {
  x@.Data <- rep(x@.Data, ...)
  x
}



## Constructors ####

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


## Order/relations methods ####




## Math ----

#' @rdname token
#' @export
setMethod('Arith', c('token', 'token'),
          function(e1, e2) {
           exclusives <- humdrumR_exclusives[Exclusive %in% c(e1@Exclusive, e2@Exclusive)]
           if (nrow(exclusives) == 0L) .stop("humdrumR can't do arithmetic with this data, because it doesn't know how to parse it.")
           
           parser <- unique(exclusives$Parser)
           
           
           if (length(parser) > 1L) .stop("You can't do arithmetic with these two different types of humdrum tokens.",
                                          "Your first argument is {exclusives[1]$Type} data, while",
                                          "the second is {exclusives[2]$Type} data.")
           
           parser <- match.fun(parser)
           
           e1 <- parser(e1@.Data)
           e2 <- parser(e2@.Data)
           
           e3 <- callGeneric(e1, e2)
           token(tint2kern(e3), Exclusive = 'kern')
             
          })

#' @rdname token
#' @export
setMethod('Summary', c('token'),
          function(x) {
            exclusives <- humdrumR_exclusives[Exclusive == x@Exclusive]
            
            if (nrow(exclusives) == 0L) .stop("humdrumR can't do max/min/range with this data, because it doesn't know how to parse it.")
            
            parser <- match.fun(unique(exclusives$Parser))
            
            x <- parser(x@.Data)
            output <- callGeneric(x)
            token(tint2kern(output), Exclusive = 'kern')
            
          })