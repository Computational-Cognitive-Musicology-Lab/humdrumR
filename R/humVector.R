
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
#' Your class must one or more slots which are themselves matrices, with 
#' all dims the same.
#' \code{humdrumVector}'s indexing method will cause all of these vectors to be indexed as one.
#' When you define a new subclass of \code{humdrumVector}, it will inherit a 
#' \code{validObject} method which assures that all elements are the same dimension.
#' Thus, if you are writing your own \code{validObject} method (using \code{setValidity})
#' you just have to worry specifically about the validity of the information in your slots,
#' not that the slots are all the same length.
#' 
#' 
#' @section Initialize:
#' An initialize method which automatically makes all slots the same length is predefined
#' for \code{humdrumVectors}. If you want to make a more specialized \code{initialize} method,
#' you can still take advantage of the inherited method by using \code{callNextMethod} at the 
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
#'   \code{format} methods are defined automatically.}
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
                
                dims <- sapply(slots, dim)
                nrow(unique(dims)) == 1L
                
            })


setMethod('initialize', 
          'humdrumVector',
          function(.Object, ...) {
              args <- list(...)
              args <- lapply(args, function(x) if (is.null(dim(x))) cbind(x) else x)
              args <- do.call('match_size', c(args, list(margin = 1:2), list(recycle = c(TRUE, FALSE))))
              
              na <- Reduce(`|`, lapply(args, is.na))
              allna <- apply(na, 2, all)
              
              if (any(na)) args <- lapply(args, function(x) {x[na] <- NA ; x[, !allna, drop = FALSE]})
              
              
              
              args[-1] <- lapply(args[-1], unname)
              
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
              rownames(x)
          })

#' @name humdrumVector
#' @export
setMethod('rownames', c(x = 'humdrumVector'),
          function(x) {
              rownames(getSlots(x)[[1]])
          })

#' @name humdrumVector
#' @export
setMethod('colnames', c(x = 'humdrumVector'),
          function(x) {
              colnames(getSlots(x)[[1]])
          })
#' @name humdrumVector
#' @export
setMethod('dimnames', c(x = 'humdrumVector'),
          function(x) {
              list(rownames(x), colnames(x))
          })

#' @name humdrumVector
#' @export
setMethod('names<-', c(x = 'humdrumVector'),
          function(x, value) {
              rownames(x) <- value
              x
          })
#' @name humdrumVector
#' @export
setMethod('colnames<-', c(x = 'humdrumVector'),
          function(x, value) {
              slots <- getSlots(x)
              colnames(slots[[1]]) <- value
              setSlots(x) <- slots
              x
          })
#' @name humdrumVector
#' @export
setMethod('rownames<-', c(x = 'humdrumVector'),
          function(x, value) {
              slots <- getSlots(x)
              rownames(slots[[1]]) <- value
              setSlots(x) <- slots
              x
          })

#' @name humdrumVector
#' @export
setMethod('dimnames<-', c(x = 'humdrumVector'),
          function(x, value) {
              slots <- getSlots(x)
              dimnames(slots[[1]]) <- value
              setSlots(x) <- slots
              x
          })





checkSame <- function(x, y, call) {
    if (all(class(x) != class(y))) {
        stop(call. = FALSE, 
             glue::glue("Can't apply {call} to a humdrumR vector with something else that is not the same class. "),
             glue::glue("In this case, you are trying to combine a value of class '{class(x)[1]}' with another value of class '{class(y)[1]}'."))
    }
    x
}

setMethod('[', c(x = 'humdrumVector', j = 'missing'),
          function(x, i) {
            slots <- getSlots(x)
            slots <- lapply(slots, 
                            function(slot) {
                              slot[i, , drop = FALSE]
                            })
            setSlots(x) <- slots
            x
          })

setMethod('[', c(x = 'humdrumVector', i = 'missing'),
          function(x, j) {
              slots <- getSlots(x)
              slots <- lapply(slots, 
                              function(slot) {
                                  slot[ , j, drop = FALSE]
                              })
              setSlots(x) <- slots
              x
          })

setMethod('[', c(x = 'humdrumVector'),
          function(x, i, j) {
              slots <- getSlots(x)
              slots <- lapply(slots, 
                              function(slot) {
                                  slot[i, j, drop = FALSE]
                              })
              setSlots(x) <- slots
              x
          })



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

setMethod('[<-', c(x = 'humdrumVector', i = 'missing', j = 'ANY', value = 'humdrumVector'),
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


####

#' @name humdrumVector
#' @export
setMethod('c', 'humdrumVector',
          function(x, ...) {
              xs <- list(x, ...)
              xs <- lapply(xs, function(x) if (!is.humdrumVector(x)) as(x, class(xs[[1]])) else x)
              
              Reduce(function(x, y) checkSame(x, y, 'c'), xs)
              
              xslots <- lapply(xs, getSlots)
              ncols <- sapply(xslots, function(x) ncol(x[[1]]))
              
              if (length(unique(ncols)) != 1L) {
                  ncol <- max(ncols)
                  slots[ncols < ncol] <- lapply(slots[ncols < ncol], 
                                                function(slot) do.call('cbind', c(list(slot), rep(NA, ncol - ncol(slot)))))
              }
              xslots <- do.call('Map', c(rbind, xslots))
              
              setSlots(x) <- xslots
              x
          })

rbind.humdrumVector <- function(...) do.call('c', list(...))
cbind.humdrumVector <-  function(...) {
    xs <- list(...)
    xs <- lapply(xs, function(x) if (!is.humdrumVector(x)) as(x, class(xs[[1]])) else x)
    
    Reduce(function(x, y) checkSame(x, y, 'cbind'), xs)
    
    xslots <- lapply(xs, getSlots)
    nrows <- sapply(xslots, function(x) nrow(x[[1]]))
    
    xslots <- lapply(xslots, function(slots) lapply(slots, function(slot) slot[1:min(nrows), , drop = FALSE]))
    
    xslots <- do.call('Map', c(cbind, xslots))
    
    if (max(nrows) > min(nrows)) {
        pad <- matrix(NA, max(nrows) - min(nrows), ncol(xslots[[1]]))
        xslots <- lapply(xslots, function(slot) rbind(slot, pad) )
    }
    
    x <- xs[[1]]
    setSlots(x) <- xslots
    x
}
#' @name humdrumVector
#' @export dim ncol nrow length
NULL

#' @name humdrumVector
#' @export length dim ncol nrow
setMethod('dim', signature = 'humdrumVector', function(x) dim(getSlots(x)[[1]]))
setMethod('ncol', signature = 'humdrumVector', function(x) dim(x)[2])
setMethod('nrow', signature = 'humdrumVector', function(x) dim(x)[1])
setMethod('length', signature = c('humdrumVector'), function(x) length(getSlots(x)[[1]]))

#' @name humdrumVector
#' @export
setMethod('is.na', signature = 'humdrumVector',
          function(x) {
              is.na(getSlots(x)[1][ , 1])
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
as.matrix.humdrumVector <- function(x, ...) {
    
    slots <- getSlots(x)
    mat <- slots[[1]]
    
    mat[] <- Reduce(function(x, y) .paste(x, y, sep = ',', na.rm = TRUE), slots)
    mat[is.na(slots[[1]])] <- NA_character_
    
    rownames(mat) <- rownames(x)
    colnames(mat) <- colnames(x)
    mat
}


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
              x <- as.matrix.humdrumVector(x)
              x[] <- as.character(as.matrix)
              x
          })
#' @name humdrumVector
#' @export
setMethod('show', signature = c(object = 'humdrumVector'), 
          function(object) { 
              if (nrow(object) == 0L) {
                  cat(paste0(class(object), '(NULL)'))
              } else {
                  mat <- as.matrix(object)
                  if (ncol(mat) == 1L) mat <- setNames(c(mat), rownames(mat))
                  print(mat, quote = FALSE)
              }
            invisible(object)
            }  )


#' @name humdrumVector
#' @export
setMethod('rep', c(x = 'humdrumVector'),
          function(x, ...) {
              slots <- getSlots(x)
              i <- 1:nrow(x)
              x[rep(i, ...), ]
})

order <- function(x, ..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) UseMethod('order')

order.default <- function(x, ..., na.last = TRUE, 
                          decreasing = FALSE, method = c("auto", "shell", "radix")) {
    do.call(base::order, list(x, ..., na.last = na.last, decreasing = decreasing, method = method))
}



#' @name humdrumVector
#' @export
setMethod('sort', signature = c(x = 'humdrumVector'),
          function(x, decreasing = FALSE) {
              x[order(x, decreasing = decreasing), ]
          })


#' @name humdrumVector
#' @export
setMethod('==', signature = c('humdrumVector', 'humdrumVector'),
          function(e1, e2) {
              checkSame(e1, e2, '==')
              
              slots1 <- getSlots(e1)
              slots2 <- getSlots(e2)
              Reduce(`&`, Map(`==`, slots1, slots2))
              
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
#' @exportMethod + - sum cumsum diff

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
              if (length(e1) != length(e2)) match_size(e1 = e1, e2 = e2, toEnv = TRUE, margin = 1:2, recycle = c(TRUE, FALSE))
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




setClass('test', contains = 'humdrumVector', slots= c(x='matrix', y= 'matrix'))

new('test', x= 1:5, y= 1:10) -> a
