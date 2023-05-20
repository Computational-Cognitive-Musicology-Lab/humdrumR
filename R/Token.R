################################## ###
# token S4 class #####################
################################## ###

## Definition, validity, initialization ####

setClassUnion('maybecharacter', c('character', 'NULL'))
setClassUnion('matrixorvector', c('vector', 'matrix'))

setClass('token', contains = 'matrixorvector', c(Exclusive = 'maybecharacter', Attributes = 'list'))

## Constructors ####

#' Humdrum tokens
#' 
#' `token` is an `S4` class which acts as a simple
#' "wrapper" around `atomic` data, allowing `humdrumR` to give
#' that data special treatment.
#' They are basically `atomic` vectors with a known
#' exclusive interpretation.
#' You should be able to treat them exactly like their "normal" class
#' of `atomic` vector---e.g., `character`, or `numeric`.
#' 
#' @name token
#' @export
NULL


## Constructors ####

#' @rdname token
#' @export
token <- function(x, Exclusive = NULL, ...) {
  if (is.null(x)) x <- character(0)
  
  new('token', x, Exclusive = Exclusive, Attributes = list(...))
}


## Accessors ####

getExclusive <- function(x) if (inherits(x, 'token')) x@Exclusive

untoken <- function(x) x@.Data

## Vectorization ####

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

#' @export
rbind.token <- function(...) {
  xs <- list(...)
  
  xs <- lapply(xs, \(x) if (hasdim(x)) t(x) else x) # keep vectors as vectors
  
  t(do.call('cbind.token', xs))
}

#' @export
cbind.token <-  function(...) {
  
  args <- list(...)
  args <- Filter(Negate(is.null), args)
  
  values <-  do.call('cbind', lapply(args, 
                                     \(arg) {
                                       if (inherits(arg, 'token')) return(arg@.Data)
                                       if (is.vector(arg)) return(arg)
                                     }))
  
  exclusives <- unique(unlist(lapply(args, getExclusive)))
  attributes <- unlist(lapply(args, \(arg) if (inherits(arg, 'token')) arg@Attributes else NULL), recursive = FALSE)
  attributes <- attributes[!duplicated(.names(attributes))]
  
  
  new('token', values, Exclusive = exclusives, Attributes = attributes)
  
}

#' @rdname token
#' @export
rep.token <- function(x, ...) {
  x@.Data <- rep(x@.Data, ...)
  x
}

#' @rdname token
#' @export
unique.token <- function(x, ...) {
  x@.Data <- unique(x@.Data, ...)
  x
}

### Indexing ----

#' @rdname token
#' @export
setMethod('[', 'token',
          function(x, i, j, ..., drop = FALSE) {
            
            x@.Data <- if (hasdim(x)) {
              .x <- x@.Data[i, j, ..., drop = drop]
              if (!hasdim(.x)) dim(x) <- NULL
              .x
            } else {
              x@.Data[i]
            }
            
            x
          })





#' @rdname token
#' @export
setMethod('show', 'token',
         function(object ) {
           exclusive <- object@Exclusive
           if (!is.null(exclusive)) {
             cat('**')
             cat(paste(exclusive, collapse = '**'), 
                 ' (', 
                 if (is.array(object@.Data)) paste0(class(object@.Data[1,1])[1], ' array') else class(object@.Data)[1],
                 ')', 
                 sep = '')
             
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

factorize <- function(token) {
  factorizer <- token@Attributes$factorizer
  if (is.null(factorizer)) return(factor(token@.Data))
  
  factorizer(token)
  
}


#' @export
setMethod('as.factor', 'token', function(x) factorize(x))


# #' @export
#tapply <- rlang::new_function(formals(base::tapply), body = body(base::tapply))


## Logic methods ####

### is.methods ####

#' @rdname token
#' @export
is.token <- function(x) inherits(x, 'token')



## Order/relations methods ####


#' @export order.token
#' @rdname token
#' @exportMethod > >= < <= Summary abs sign
order.token <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                                method = c("auto", "shell", "radix")) {
  parsedx <- unparse(x)
  
  if (is.null(parsedx)) {
    return(order(x@.Data))
  } else {
    order(parsedx)
  }
}


#' @export
setMethod('sort', signature = c(x = 'token'),
          function(x, decreasing = FALSE) {
            x[order.token(x, decreasing = decreasing)]
          })

## Math ----

#' @rdname token
#' @export
setMethod('Arith', c('token', 'token'),
          function(e1, e2) {
            if (is.numeric(e1) && is.numeric(e2)) {
              e1@.Data <-  callGeneric(e1@.Data, e2@.Data)
              return(e1)
            }
            
           parsed1 <- unparse(e1)
           parsed2 <- unparse(e2)
           # exclusives <- humdrumR_exclusives[Exclusive %in% c(e1@Exclusive, e2@Exclusive)]
           if (class(parsed1) != class(parsed2)) .stop("humdrumR can't do arithmetic with this data, because it doesn't know how combine them.")
           
           e3 <- callGeneric(parsed1, parsed2)
           
           reparse(e3, e1)
             
          })

#' @rdname token
#' @export
setMethod('Arith', c('token', 'character'),
          function(e1, e2) {
            parser <- e1@Attributes$parser
            reparse(callGeneric(unparse(e1), parser(e2)), e1)
            
          })

#' @rdname token
#' @export
setMethod('Arith', c('character', 'token'),
          function(e1, e2) {
            parser <- e2@Attributes$parser
            reparse(callGeneric(parser(e1), unparse(e2)), e2)
            
          })

#' @rdname token
#' @export
setMethod('Summary', c('token'),
          function(x, ..., na.rm = FALSE) {
            if (is.numeric(x)) {
              dim(x) <- NULL
              x@.Data <- callGeneric(x@.Data, ..., na.rm = na.rm)
              return(x)
            }
            
            parsedx <- unparse(x)
            
            if (is.null(parsedx)) .stop("humdrumR can't do max/min/range with this data, because it doesn't know how to parse it.")
            
            xsummary <- callGeneric(parsedx, ..., na.rm = na.rm)
            dispatch <- attr(parsedx, 'dispatch')
            
            reparse(xsummary, x)
            # reParse(xsummary, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree'))
            # token(tint2kern(output), Exclusive = 'kern')
            
          })

################################ ###
# parsing and unparsing tokens ####
################################ ###


unparse <- function(token) {
  if (!inherits(token, 'token')) return(NULL)
  parser <- token@Attributes$parser
  
  if (is.null(parser)) return(NULL)
  
  do.call(parser, c(list(token@.Data), token@Attributes$deparseArgs))
}

reparse <- function(x, token) {
  if (!is.token(token)) return(NULL)
  deparser <- token@Attributes$deparser
  
  if (is.null(deparser)) return(NULL)
  
  token@.Data <- do.call(deparser, c(list(c(x)), token@Attributes$deparseArgs)) %<-matchdim% x
  token
  
}