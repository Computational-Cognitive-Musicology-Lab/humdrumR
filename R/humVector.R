
#' Pseudo-vector classes for humdrum data.
#' 
#' R's "vectorization" is a key strength, so being able to define
#' S4 classes that act in a vectorized manner is very useful.
#' Unfortunetaly, defining this classes is a bit tedious.
#' \code{humdrumVector} is a \emph{virtual} S4 class which takes care of some of 
#' this tediousness for developers. The \code{humdrumVector}
#' defines all the necessarry methods to treat an object as a vector---simply
#' make your new class inherit \code{humdrumVector} and it is all taken care of!
#' (To do this, specifify \code{contains = 'humdrumVector'} in your call to \code{setClass}.)
#' 
#' Be warned, \code{R} is limited in this regard---users can't \emph{really} define
#' \code{S4} classes that really act fully like \code{R} atomics---, so you may 
#' run in to problems if you take this too far. 
#' For instance, though \code{humdrumVector} classes work (ok) in \code{\link[base]{data.frame}}s
#' \code{data.table}s and \code{tibbles} might give you problems.
#' 
#' @section Requirements:
#' 
#' To work, \code{humdrumVector} makes a few assumptions about your class.
#' Your class must one or more slots which themselves contain vectors or matrices, with 
#' all vector lengths and matrix number-of-rows the same.
#' \code{humdrumVector}'s indexing method will cause all of these vectors to be indexed as one.
#' When you define a new subclass of \code{humdrumVector}, it will inherit a 
#' \code{validObject} method which assures that all elements are the same length/number-of-rows.
#' Thus, if you are writing your own \code{validObject} method (using \code{setValidity})
#' you just have to worry specifically about the validity of the information in your slots,
#' not that the slots are all the same length.
#' 
#' 
#' @section Initialize:
#' An initialize method which automatically makes all slots the same length is predefined
#' for \code{humdrumVectors}. If you want to make a more specialized \code{initialize} method,
#' you can still take advantage of the inherited method by using \code{UseNextMethod} at the 
#' beginning of your function.
#' 
#' 
#' @section Predefined methods:
#' 
#' You must \code{order} and any arithmetic/comparison methods for your class
#' yourself. However,
#' \itemize{
#'  \item{If you define \code{>} and \code{>=}, \code{<} and \code{<=} will be automatically defined.}
#'  \item{If you define \code{order}, \code{sort} will be automatically defined.}
#'  \item{If you define \code{as.character} for your class, \code{show} and
#'   \code{format} methods are defined #' automatically.}
#'  \item{If you define \code{+} methods for your class (adding two of class together), inefficient but
#'   functional implementations of \code{sum} and \code{cumsum} are defined.}
#'  \item{If you \emph{also} define a prefix \code{-} method (with 
#'  \code{signature = c(e1 = 'myclass', e2 = 'missing')}),  \code{-} methods between 
#'  two of your classes are defined, as well as a inefficient default implementation 
#'  of \code{diff}.}
#' }
#' 
#' @name humdrumVector
#' @export
setClass('humdrumVector')

setValidity('humdrumVector',
            function(object) {
                slots <- getSlots(object)
                
                
                dims <- sapply(slots,
                               function(s) {
                                   dims <- dim(s)
                                    if (is.null(dims)) c(length(s), 0) else dims
                                      
                                })
                
                nrow(unique(dims)) == 1L
                
            })


setMethod('initialize', 
          'humdrumVector',
          function(.Object, ...) {
              args <- list(...)
              args <- do.call('match_size', c(args, list(margin = 1:2)))
              
              setSlots(.Object) <- args
              .Object              
          } )

getSlots <- function(x) {
    slotnames <- slotNames(x)
    slots <- lapply(slotnames, slot, object = x)
    names(slots) <- slotnames
    slots
}

`setSlots<-` <- function(x, value) {
    slotnames <- slotNames(x)
    for (s in slotnames) {
        slot(x, s) <- value[[s]]
    }
    
    x
}
#' @name humdrumVector
#' @export
setMethod('names', c(x = 'humdrumVector'),
          function(x) {
              NULL
          })



checkSame <- function(x, y, call) {
    if (class(x) != class(y)) stop(call. = FALSE,  
                                   glue::glue("Can't apply {call} between two humdrumVectors of different types."))
    x
}

#' @name humdrumVector
#' @export
setMethod('[', c(x = 'humdrumVector'),
          function(x, i) {
            slots <- getSlots(x)
            slots <- lapply(slots, 
                            function(slot) {
                              if (is.null(dim(slot))) slot[i] else slot[i, , drop = FALSE]
                            })
            setSlots(x) <- slots
            x
          })







#' @name humdrumVector
#' @export
setMethod('[<-', c(x = 'humdrumVector', i = 'ANY', j = 'missing', value = 'humdrumVector'),
          function(x, i, value) {
              checkSame(x, value, '[<-')
              
              slotsx <- getSlots(x)
              slotsv <- getSlots(value)
              
              slots <- Map(function(slotx, slotv) {
                                  if (is.null(dim(slotx))) slotx[i] <- slotv else slotx[i,] <- slotv
                                  slotx
                             },
                           slotsx, slotsv)
              setSlots(x) <- slots
              x
          })

#' @name humdrumVector
#' @export
setMethod('c', 'humdrumVector',
          function(x, ...) {
              xs <- list(x, ...)
              xs <- lapply(xs, function(x) if (!is.humdrumVector(x) && all(is.na(x))) tint(x) else x)
              
              Reduce(function(x, y) checkSame(x, y, 'c'), xs)
              
              xslots <- lapply(xs, getSlots)
              xslots <- Reduce(function(a, b) Map(function(c, d) if (is.null(dim(c))) c(c, d) else rbind(c, d), a, b) , xslots)
              
              
              
              setSlots(x) <- xslots
              x
          })


#' @name humdrumVector
#' @export
setMethod('rep', 'humdrumVector',
          function(x, ...) {
              slots <- getSlots(x)
              isvec <- sapply(slots, is.vector)
              
              slots[isvec] <- lapply(slots[isvec], rep, ...)
              
              setSlots(x) <- slots
              x
              
          })

#' @name humdrumVector
#' @export
setMethod('length', signature = c('humdrumVector'),
          function(x) {
              slots <- getSlots(x)
              subvects <- sapply(slots, is.vector)
              if (any(subvects)) {
                  length(slots[subvects][[1]])
              } else {
                  nrow(slots[!subvects][[1]])
              }
          })

#' @name humdrumVector
#' @export
setMethod('dim', signature = 'humdrumVector', function(x) NULL)

#' @name humdrumVector
#' @export
setMethod('is.na', signature = 'humdrumVector',
          function(x) {
              slots <- getSlots(x)
              
              subvects <- do.call('cbind', lapply(slots, is.na))
              apply(subvects, 1, any)
          })


#' @name humdrumVector
#' @export
is.humdrumVector <- function(x) inherits(x, 'humdrumVector')

#' @name humdrumVector
#' @export
setMethod('is.vector', signature = 'humdrumVector', function(x) TRUE)


#' @name humdrumVector
#' @export
setMethod('as.vector', signature = 'humdrumVector', force)

#' @name humdrumVector
#' @export
setMethod('as.list', signature = c('humdrumVector'),
          function(x, ...) {
              
              x <- list(x, ...)
              x <- do.call('c', x)
              
              lapply(seq_along(x), function(i) x[i])
          })

#' @name humdrumVector
#' @export
as.data.frame.humdrumVector <- function(x, row.names = NULL, optional = FALSE, ...) {
    if (is.null(row.names)) row.names <- 1:length(x)
    
    value <- list(x)
    attr(value, 'row.names') <- row.names
    attr(value, 'names') <- class(x)[1]
    class(value) <- c('data.frame')
    value
}

#######-
#' @name humdrumVector
#' @export
setMethod('as.character', 'humdrumVector',
          function(x) {
              slots <- getSlots(x)
              slots <- Filter(is.vector, slots)
              do.call('.paste', c(slots, sep = ' '))
          })
#' @name humdrumVector
#' @export
setMethod('show', signature = c(object = 'humdrumVector'), 
          function(object) { 
              if (length(object) == 0L) {
                  cat(paste0(class(object), '(0)'))
              } else {
                  strs <- format(as.character(object), justify = 'left')
                  
                  cat(strs)
              }
            invisible(object)
            }  )


#' @name humdrumVector
#' @export
rep.humdrumVector <- function(x, ...) {
              slots <- getSlots(x)
              slots <- lapply(slots, 
                              function(x) {
                                  if (is.null(dim(x))) rep(x, ...) else apply(x, 2, rep, ...)
                              })
              setSlots(x) <- slots
              x
}

order <- function(x, ..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) UseMethod('order')

order.default <- function(x, ..., na.last = TRUE, 
                          decreasing = FALSE, method = c("auto", "shell", "radix")) {
    do.call(base::order, list(x, ..., na.last = na.last, decreasing = decreasing, method = method))
}



#' @name humdrumVector
#' @export
setMethod('sort', signature = c(x = 'humdrumVector'),
          function(x, decreasing = FALSE) {
              x[order(x, decreasing = decreasing)]
          })


#' @name humdrumVector
#' @export
setMethod('==', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              checkSame(e1, e2, '==')
              
              slots1 <- getSlots(e1)
              slots2 <- getSlots(e2)
              comparisonmatrix <- do.call('rbind', Map(`==`, slots1, slots2))
              
              apply(comparisonmatrix, 2, all)
          })

#' @export
setMethod('!=', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              checkSame(e1, e2, '!=')
              
              !(e1 == e2)
          })

#' @name humdrumVector
#' @export
setMethod('<', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              !(e1 >= e2 )
          })


#' @name humdrumVector
#' @export
setMethod('<=', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              !(e1 > e2)
          })


#' @name humdrumVector
#' @export
format.humdrumVector <- function(x, ...) { as.character(x)}
###

#' @name humdrumVector
#' @export
setMethod('sum', signature = c('humdrumVector'),
          function(x) {
              Reduce(`+`, as.list(x))
          })

#' @name humdrumVector
#' @export
setMethod('cumsum', signature = c('humdrumVector'),
          function(x) {
              do.call('c', (Reduce(`+`, as.list(x), accumulate = TRUE)))
          })

#' @name humdrumVector
#' @export
setMethod('-', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              e1 + (-e2)
          })

#' @name humdrumVector
#' @export
setMethod('diff', signature = c('humdrumVector'),
          function(x, lag = 1L, differences = 1L) {
              out <- tail(x, -lag) - head(x, -lag)
              
              if (differences > 1L) {
                  Recall(out, lag, differences - 1L)
              } else {
                  out
              }
          })


#' @name humdrumVector
#' @export
setMethod('+', signature = c('humdrumVector', 'ANY'),
          function(e1, e2) {
              if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              e1 + as(e2, class(e1))
          })

#' @name humdrumVector
#' @export
setMethod('+', signature = c('ANY', 'humdrumVector'),
          function(e1, e2) {
              e1 + as(e2, class(e1))
          })

#' @name humdrumVector
#' @export
setMethod('-', signature = c('humdrumVector', 'ANY'),
          function(e1, e2) {
              e1 - as(e2, class(e1))
          })

#' @name humdrumVector
#' @export
setMethod('-', signature = c('ANY', 'humdrumVector'),
          function(e1, e2) {
              e1 - as(e2, class(e1))
          })



#' @name humdrumVector
#' @export
setMethod('*', signature = c('humdrumVector', 'numeric'),
          function(e1, e2) {
              setSlots(e1) <- lapply(getSlots(e1), 
                                     function(x) {
                                       x * as(e2, class(x))  
                                         
                                     })
              e1 
          })

#' @name humdrumVector
#' @export
setMethod('*', signature = c('numeric', 'humdrumVector'),
          function(e1, e2) {
              e2 * e1
          })



##### "restoring" ----

`%restore%` <- function(e1, e2) {
    attr(e1, 'restorer') <- e2
    e1
}

`restorer<-` <- function(x, value) {
    attr(x, 'restorer') <- value
    x
}

restore <- function(vector, restorer = NULL) {
    if (is.null(restorer))  restorer <- attr(vector, 'restorer')
    
    if (is.null(restorer)) return(vector)
    
    restoreFunc <- match.fun(restorer)
    
    restoreFunc(vector)
}

restorer <- function(x) attr(x, 'restorer')