
#' Pseudo-vector classes for humdrum data.
#' 
#' R's "vectorization" is a key strength, so being able to define
#' S4 classes that act in a vectorized manner is very useful.
#' Unfortunetaly, defining such classes is a bit tedious.
#' \code{humVector} is a \emph{virtual} S4 class which takes care of most of
#' this tediousness for developers. The \code{humVector}
#' defines all the necessarry methods to treat an object as a vector---simply
#' make your new class inherit \code{humVector} and it is all taken care of!
#' (To do this, specifify \code{contains = 'humVector'} in your call to \code{setClass}.)
#' 
#' Be warned, \code{R} is limited in this regard---users can't \emph{really} define
#' \code{S4} classes that \emph{really} act fully like \code{R} atomics---, so you may 
#' run in to problems if you take this too far. 
#' For instance, though \code{humVector} classes work (ok) in \code{\link[base]{data.frame}}s
#' \code{data.table}s and \code{tibbles} might give you problems.
#' 
#' \code{humVector} subclasses behave very similarly to normal R vectors.
#' However, they do differ in a few respects, mostly in ways that
#' avoid some of the quirky behaviors with R vectors:
#' 
#' Firstly, \code{humVectors} always have dimensions---when we treat
#' them as one-dimensional vectors they are "really" under the hood
#' "column-vectors." Thus, the sometimes irritating distinction
#' between matrices and vectors in R is avoided.
#' Every \code{humVector} is a matrix, which can have one
#' column, or multiple columns.
#' 
#' \code{humVectors} are indexed just like humdrum vectors/matrices.
#' If it is a 1-column humVector, index just like it's a vector.
#' If there are more than 1-columns, you can index them like matrices.
#' One exception is that \code{humVectors} always give an error if you
#' try an index that is larger than the vector...instead of padding with \code{NA}s,
#' as base R does.
#' 
#' \code{humVectors} also have a useful \code{cartesian} indexing argument.
#' If \code{cartesian = TRUE} and both \code{i} and \code{j} indices are included,
#' \code{i} and \code{j} are treated like cartesian coordinates.
#' (This behavior can be achieved with base R matrices by inputing a 
#' matrix with two columns.)
#' 
#' 
#' 
#' @section Requirements:
#' 
#' To work, \code{humVector} makes a few assumptions about your class.
#' Your class must have one or more slots which are vectors, all of which are the same length.
#' \code{humVector}'s indexing method will cause all of these vectors to be indexed as one.
#' When you define a new subclass of \code{humVector}, it will inherit a 
#' \code{validObject} method which assures that all elements are the same dimension.
#' Thus, if you are writing your own \code{validObject} method (using \code{setValidity})
#' you just have to worry specifically about the validity of the information in your slots,
#' not that the slots are all the same length.
#' 
#' 
#' @section Initialize:
#' 
#' An initialize method which automatically makes all slots the same length is predefined
#' for \code{humVectors}. If you want to make a more specialized \code{initialize} method,
#' you can still take advantage of the inherited method by using \code{callNextMethod} at the 
#' beginning of your function.
#' 
#' 
#' @section Predefined methods:
#' 
#' You must specify \code{order} and any arithmetic/comparison methods for your class
#' yourself. However,
#' \itemize{
#'  \item{If you define \code{>} and \code{>=}, \code{<} and \code{<=} will be automatically defined.}
#'  \item{If you define \code{order}, \code{sort} will be automatically defined.}
#'  \item{If you define \code{as.character} for your class, \code{show} and
#'   \code{format} methods are defined automatically.}
#' }
#' 
#' Default arithmetic methods for addition, multiplication, negation (\code{-x}) are defined.
#' They assume that adding your class to another is simply the same as adding each numeric slot in parallel.
#' If this is not the case, you'll need to create your own, more specific, method!
#' 
#' @name humVector
#' @export
NULL

setClassUnion('dimnames', c('character', 'integer', 'NULL'))

setClass('humVector', contains = 'VIRTUAL', slots = c(ncol = 'integer', nrow = 'integer', rownames = 'dimnames', colnames = 'dimnames'))

setValidity('humVector', 
            function(object) {
                slots <- getSlots(object)
                errors <- c(
                    if (!all(sapply(slots, is.vector)) || !all(sapply(slots, is.atomic))) 'humVector slots must all be atomic vectors.',
                    if (!allsame(lengths(slots))) "humVector slots must all be the same length.",
                    if (length(object@ncol) != 1L || object@ncol < 0L) "The @ncol slot of a humVector must be a single non-negative integer.",
                    if (length(object@nrow) != 1L || object@ncol < 0L) "The @nrow slot of a humVector must be a single non-negative integer.",
                    if (!is.null(object@colnames) && length(object@colnames) != object@ncol) "The colnames slot of a humVector must be NULL or must be @ncol in length.",
                    if (!is.null(object@rownames) && length(object@rownames) != object@nrow) "The rownames slot of a humVector must be NULL or must be @nrow in length.",
                    if (length(slots[[1]]) != object@ncol * object@nrow) 'The length of the vectors inside the humVector must be @ncol * @nrow.'
                )
                
                if (length(errors) > 0L) errors else TRUE
                
            })


setMethod('initialize', 
          'humVector',
          function(.Object, ..., ncol = 1L, nrow = NULL, colnames = NULL, rownames = NULL) {
              slots <- list(...)
              slots <- lapply(slots, unname)
              slots <- do.call('match_size', slots)
              
              .Object@nrow <- as.integer(length(slots[[1]]) / ncol)
              
              .Object@ncol <- ncol
              .Object@colnames <- colnames
              .Object@rownames <- rownames
              
              if (length(slots) == 0L) return(.Object)
              
              ## NA in any slot is NA in all slots
              na <- Reduce(`|`, lapply(slots, is.na))
              slots <- lapply(slots, function(slot) `[<-`(slot, na, NA))
              
              setSlots(.Object) <- slots
              validObject(.Object)
              .Object
          } )

getSlots <- function(x, classes = c('numeric', 'integer', 'logical', 'character')) {
    slotinfo <- methods::getSlots(class(x))
    slotinfo <- slotinfo[!names(slotinfo) %in% c('ncol', 'nrow', 'colnames', 'rownames')]
    slotinfo <- slotinfo[slotinfo %in% classes]

    slots <- lapply(names(slotinfo), slot, object = x)
    names(slots) <- names(slotinfo)
    slots
}

`setSlots<-` <- function(x, value) {
    slotnames <- slotNames(x)
    
    slotnames <- slotnames[!slotnames %in% c('ncol', 'nrow', 'colnames', 'rownames')]
    for (s in slotnames) {
        slot(x, s) <- value[[s]]
    }
    
    x
}

columns <- function(humvec) {
    rep(1:ncol(humvec), each = length(humvec))
}



########## shape ----

#' @name humVector
#' @export dim ncol nrow length
NULL

#' @name humVector
#' @export length dim ncol nrow
setMethod('ncol', signature = 'humVector', function(x) x@ncol)
setMethod('nrow', signature = 'humVector', function(x) x@nrow)
setMethod('length', signature = 'humVector', function(x) nrow(x) * (x@ncol > 0L))
setMethod('dim', signature = 'humVector', function(x) c(nrow = x@nrow, ncol = x@ncol))

`ncol<-` <- function(x, value) {
    if (ncol(x) == value) {
        return(x) 
    } 
    dimnames(x) <- NULL
    
    if (is.null(value) || length(value) != 1 || value < 1) stop(call. = FALSE,
                                                                "The @ncol slot of a humVector must be a single non-negative integer.")
    if (((nrow(x) * ncol(x)) %% value) != 0) stop(call. = FALSE, "The @ncol slot of a humVector must be an even divisor of the total amount of data in the humVector.")
    x@colnames <- NULL
    x@rownames <- NULL
    x@ncol <- as.integer(value)
    
    x@nrow <- as.integer(length(getSlots(x)[[1]]) / value)
    
    x
    
}

### tools 


`setdim<-` <- function(x, value) {
    dim(x) <- dim(value)
    colnames(x) <- colnames(value)
    rownames(x) <- rownames(value)
    x
}

`copydim<-` <- function(x, value) {
    if (is.null(dim(x)) && is.atomic(x)) {
        matrix(x, ncol = ncol(value), nrow = nrow(value), dimnames = dimnames(value))
    } else {
        
        ncol(x) <- ncol(value)
        dimnames(x) <- dimnames(value)
        x
        
    }
}

`%<-dim%` <- function(x, value) {
    setdim(x) <- value
   x
    
}

`%@%` <- function(x, slot) {
    slot <- rlang::expr_text(rlang::enexpr(slot))
    slotnames <- slotNames(x)
    slot <- slotnames[pmatch(slot, slotnames, duplicates.ok = TRUE)]
    slot(x, slot) %<-dim% x
    
}


arecycledim <- function(..., funccall) {
    # accepts two named args
    args <- list(...)[1:2]
    d1 <- dim(args[[1]])
    d2 <- dim(args[[2]])
    
    if (all(d1 == d2) || all(d1 == c(1L, 1L)) || all(d2 == c(1L, 1L))) return(NULL)
    
    drat <- d1 / d2
    if (drat[1] == 1 && (d1[2] == 1 || d2[2] == 1)) {
        args <- setNames(match_size(args[[1]], args[[2]], margin = 2), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    if (drat[2] == 1&& (d1[1] == 1 || d2[1] == 1)) {
        args <- setNames(match_size(args[[1]], args[[2]], margin = 1), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    
    stop(call. = FALSE, glue::glue("In call to {funccall}, the two humVectors are nonconformable.\n",
                                   ,"To confirm, at least one of their dimensions needs to be the same, while the other is either (also) the same, or 1."))
    
}



#' @name humVector
#' @export names colnames rownames
setMethod('names',    c(x = 'humVector'), function(x) x@rownames)
setMethod('rownames', c(x = 'humVector'), function(x) x@rownames)
setMethod('colnames', c(x = 'humVector'), function(x) x@colnames)
setMethod('dimnames', c(x = 'humVector'), function(x) list(rownames = x@rownames, colnames = x@colnames))

#' @exportMethod names<- colnames<- rownames<-
setMethod('names<-', c(x = 'humVector'),
          function(x, value) {
              rownames(x) <- value
              x
          })
setMethod('colnames<-', c(x = 'humVector'),
          function(x, value) {
              if (!is.null(x@rownames) && !is.null(value) && length(value) != ncol(x)) stop(call. = FALSE, "Colnames assigned to humVector must be the same length as the number of cols.")
              x@colnames <- value
              x
          })
setMethod('rownames<-', c(x = 'humVector'),
          function(x, value) {
              if (!is.null(value) && length(value) != length(x)) stop(call. = FALSE, "Rownames assigned to humVector must be the same length as the number of rows.")
              x@rownames <- value
              x
          })
setMethod('dimnames<-', c(x = 'humVector'),
          function(x, value) {
              if (is.null(value) || length(value) == 1L) {
                  x@rownames <- NULL
                  x@colnames <- NULL
                  return(x)
              }
              
              if (!is.list(value) || length(value) != 2L) stop(call. = FALSE, 'When assigning dimnames to a humVector using dimnames(x) <- value, value must be a list of length two.')
              rownames(x) <- value[[1]]
              colnames(x) <- value[[2]]
              x
          })




##############

checkSame <- function(x, y, call) {
    # this function tests if two arguments are the same class
    # it is necessary for any generic method for two humVectors, where the two humVectors might be some specific subclass.
    if (all(class(x) != class(y))) {
        stop(call. = FALSE, 
             glue::glue("Can't apply {call} to a humVector with something else that is not the same class. "),
             glue::glue("In this case, you are trying to combine a value of class '{class(x)[1]}' with another value of class '{class(y)[1]}'."))
    }
    x
}

##### [i, j] ----

humvectorI <- function(i, x) {
    c(outer(i, (seq_len(ncol(x)) - 1) * length(x), '+'))
}
humvectorJ <- function(j, x) {
    columns <- columns(x)
    unlist(locate(j, columns), use.names = FALSE)
}

emptyslots <- function(x) {
    setSlots(x) <- lapply(getSlots(x), function(slot) vector(class(slot), 0L))
    x
}

### [i, ] ----

setMethod('[', c(x = 'humVector', i = 'numeric', j = 'missing'),
          function(x, i) {
            i <- i[i != 0]
            if (all(i < 0)) i <- 1L:length(x)[i] # flip negative indices (only if ALL negative)
            if (length(i) == 0L) {
                x <- emptyslots(x)
                rownames(x) <- NULL
                x@nrow <- 0L
                return(x)
            }
            
            if (ncol(x) == 0L) {
                x@row <- length(i)
                rownames(x) <- rownames(x)[j]
                return(x)
            }
            
            # 
            if (any(i < 0))  stop(call. = FALSE, "Can't mix negative and positive numbers in index.")
            #i[i < 0] <- NA 
            if (any(i > length(x))) stop(call. = FALSE, "Index is greater than the length of the humVector.\nNormal R vectors don't throw an error for this, but we do.")
            # i[i > length(x)] <- NA 
            rownames <- rownames(x)[i]
            nrow <- length(i)
            # 
            if (any(i != 0, na.rm = TRUE)) i <- humvectorI(i, x)
            # 
            setSlots(x) <- lapply(getSlots(x), '[', i = i)
            x@nrow <- nrow
            rownames(x) <- rownames
            
            x
          })
setMethod('[', c(x = 'humVector', i = 'character', j = 'missing'),
          function(x, i) {
              
              if (is.null(rownames(x))) stop(call. = FALSE, "You can't row-index a humVector (i.e. humVector[i, ])", 
                                             " with a character string if the humVector has no rownames (i.e., humVector@rownames = NULL).")
              i <- locate(i, rownames(x))
              nomatch <- lengths(i) == 0L
              if (any(nomatch))  stop(call. = FALSE, "In your attempt to row-index a humVector (i.e., humVector[i, ]) ",
                                      glue::glue_collapse(paste0("'", names(i)[nomatch], "'"), 
                                                          sep = ', ', last = ', and '),
                                      plural(sum(nomatch), ' are not rownames', ' is not a rowname'),
                                      ' in the humVector.')
              
              i <- unlist(i)
              x[i, ]
          })
setMethod('[', c(x = 'humVector', i = 'logical', j = 'missing'),
          function(x, i ) {
              if (length(i) != nrow(x)) stop(call. = FALSE,
                                             "Can't index[i , ] a humVector with a logical vector that is a different length than humVector@nrow.")
            x[which(i), ]
          })
setMethod('[', c(x = 'humVector', i = 'matrix', j = 'missing'),
          function(x, i ) {
              matclass <- class(i[1, 1])
              if (matclass %in% c('character', 'numeric')) {
                  if (ncol(i) == 1L || nrow(i) == 1L) return(x[c(i), ])
                  
                  stop(call. = FALSE,
                       "Can't index a humVector with a numeric or character matrix unless that matrix is a 1-column or 1-row matrix.")
              }
              if (matclass != 'logical') stop(call. = FALSE, glue::glue("Can't index a humVector with a {matclass} matrix."))
              
              
              if (ncol(i) == 1L && length(i) == nrow(x)) return(x[which(i), ])
              
              if (!identical(dim(i), dim(x))) stop(call. = FALSE, "Can't index humVector[i , ] with a logical matrix, unless either has the exact same",
                                                   "dimensions as the humVector, or has the same number of rows but only one column.")
              
              ij <- which(i, arr.ind = TRUE)
              
              x[ij[ , 'row'], ij [, 'col'], cartesian = TRUE]
              
          })


### [ , j] ----

setMethod('[', c(x = 'humVector', i = 'missing', j = 'numeric'),
          function(x, j) {
              j <- j[j != 0]
              if (all(j < 0L)) j <- seq_len(x@ncol)[j] #negative indices
              
              if (length(j) == 0L) {
                  x <- emptyslots(x)
                  colnames(x) <- NULL
                  x@ncol <- 0L
                  return(x)
              }
              
              if (nrow(x) == 0L) {
                  x@ncol <- length(j)
                  colnames(x) <- colnames(x)[j]
                  return(x)
              }
              
              if (any(j < 0))  stop(call. = FALSE, "Can't mix negative and positive numbers in index.")
              if (any(j > ncol(x))) stop(call. = FALSE, "Index[, j] is greater than the ncol of the humVector.")
              
              colnames <- colnames(x)[j]
              ncol <- length(j)
              # 
              j <- humvectorJ(j, x)
              setSlots(x) <- lapply(getSlots(x), '[', i = j)
              # 
              x@colnames <- colnames
              x@ncol <- ncol
              x
          })



setMethod('[', c(x = 'humVector', i = 'missing', j = 'character'),
          function(x, j) {
              if (is.null(colnames(x))) stop(call. = FALSE, "You can't column-index a humVector (i.e. humVector[ , j])", 
                                             " with a character string if the humVector has no colnames (i.e., humVector@colnames = NULL).")
              
              j <- match(j, colnames(x))
              nomatch <- is.na(j)
              if (any(nomatch))  stop(call. = FALSE, "In your attempt to column-index a humVector (i.e., humVector[ , j]) ",
                                      glue::glue_collapse(paste0("'", names(j)[nomatch], "'"), 
                                                          sep = ', ', last = ', and '),
                                      plural(sum(nomatch), ' are not colnames', ' is not a colname'),
                                      ' in the humVector.')
              
              j <- unlist(j)
              x[ , j]
              
          })


setMethod('[', c(x = 'humVector', i = 'missing', j = 'logical'),
          function(x, j ) {
              if (length(j) != ncol(x)) stop(call. = FALSE,
                                             "Can't index[  , j] a humVector with a logical vector that is a different length than humVector@ncol.")
              x[ , which(j)]
          })

### [i, j]


setMethod('[', c(x = 'humVector'),
          function(x, i, j, cartesian = FALSE) {
              i <- i[i != 0]
              j <- j[j != 0]
              
              if (cartesian) {
                  if (!is.numeric(i) || !is.numeric(j)) stop(call. = FALSE, "Can't do cartesian indexing from humVector if i and j aren't both numeric.")
                  # XXX MAKE CHARACTER POSSIBLE
                  
                  match_size(i = i, j = j, toEnv = TRUE)
                  
                  if (any(i < 0 | j < 0)) stop(call. = FALSE, "Can't do cartesian indexing with negative indices.")
                  
                  i.internal <- humvectorI(i, x)
                  i.internal <- split(i.internal, rep(1:length(i), length.out = length(i.internal)))
                  i.internal <- unlist(Map('[', i.internal, j))
                  
                  
                  x@ncol <- 1L
                  x@nrow <- length(i)
                  setSlots(x) <- lapply(getSlots(x), '[', i.internal)
                  x@colnames <- paste(colnames(x)[j], collapse = '.')
                  x@rownames <- rownames(x)[i]
                  
                  x
                             
                  
              } else {
                 x <- x[i,  ]
                 x <- x[ , j]
              }
              
              x
          })

#### [i, j] <- value ----

setMethod('[<-', c(x = 'humVector', i = 'ANY', j = 'missing', value = 'humVector'),
          function(x, i, value) {
              checkSame(x, value, '[i , ]<-')
              
              xindexed <- x[i, ] # this will return appropriate error if indices are invalid
              
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # recycle columns
              if (length(value) %in% c(1, length(i)) && ncol(value) == 1L && ncol(x) > 1L) {
                  value <- Repeat(value, length.out = ncol(x), margin = 2)
              }
              
              # sizes still don't match
              if (ncol(value) != ncol(x)) stop(call. = FALSE,
                                               glue::glue("Can't assign ([i]<-) a humVector with {ncol(value)} columns into rows of a humVector with {ncol(x)} columns."))
              
              # character indices
              if (is.character(i) && !is.null(rownames(x))) i <- locate(i, rownames(x))
              
              # negative indices
              if (all(i < 0)) i <- (1L:nrow(x))[i]
              if (any(i < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in humVector assignment index.")
              
              #
              if (length(value) == 1L && length(i) != 1L) value <- rep(value, length.out = length(i))
              if (length(value) != length(i)) stop(call. = FALSE, 
                                                   glue::glue("Can't assign ([i]<-) a humVector with {length(value)} rows into {length(i)} rows of another humVector.\n",
                                                              "To conform, the value being assigned must have the same number of rows, or have only one row, in which case that one row is recycled."))
              
              
              ## do it
              i <- humvectorI(i, x)
              
              slotsx <- getSlots(x)
              slotsv <- getSlots(value)
              slots <- Map(function(slotx, slotv) {
                                  slotx[i] <- slotv
                                  slotx
                             },
                           slotsx, slotsv)
              setSlots(x) <- slots
              x
          })

setMethod('[<-', c(x = 'humVector', i = 'missing', j = 'ANY', value = 'humVector'),
          function(x, j, value) {
              checkSame(x, value, '[ , j]<-')
              
              xindexed <- x[, j] # this will return appropriate error if indices are invalid
              
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # recycle rows
              if (ncol(value) %in% c(1, length(j)) && nrow(value) == 1L && nrow(x) > 1L) {
                  value <- Repeat(value, length.out = nrow(x), margin = 1)
              }
              
              # sizes still don't match
              if (nrow(value) != nrow(x)) stop(call. = FALSE,
                                               glue::glue("Can't assign ([ , j]<-) a humVector with {nrow(value)} rows into columns of a humVector with {nrow(x)} rows."))
              
              # character indices
              if (is.character(j) && !is.null(colnames(x))) j <- locate(j, colnames(x))
              
              
              ## negative indices
              if (all(j < 0)) j <- (1L:col(x))[j]
              if (any(j < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in humVector assignment index.")
              
              
              # do it 
              
              j <- humvectorJ(j, x)
              slotsx <- getSlots(x)
              slotsv <- getSlots(value)
              slots <- Map(function(slotx, slotv) {
                  slotx[j] <- slotv
                  slotx
              },
              slotsx, slotsv)
              setSlots(x) <- slots
              x
          })

setMethod('[<-', c(x = 'humVector', i = 'ANY', j = 'ANY', value = 'humVector'),
          function(x, i, j, value, cartesian = FALSE) {
              checkSame(x, value, '[ i, j]<-')
              
              if (cartesian) return(cartesianAssign(x, i, j, value))
              
              xindexed <- x[i, j] # this will return appropriate error if indices are invalid
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # character indices
              if (is.character(i)) i <- locate(i, colnames(x))
              if (is.character(j)) j <- locate(j, colnames(x))
              
              ## negative indices
              if (all(j < 0)) j <- (1L:col(x))[j]
              if (any(j < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in humVector assignment index.")
              
              # sizes still don't match
              
              if (!all(dim(xindexed) == dim(value))) stop(call. = FALSE,
                                               glue::glue("Can't assign ([ i, j, cartesian = FALSE]<-) a humVector with dimensions ({nrow(value)}, {ncol(value)}) into", 
                                                          " a subset of a humVector with dimensions ({length(i)}, {length(j)})."))
              
              # do it 
              i.internal <- humvectorI(i, x)
              j.internal <- humvectorJ(j, x)
              ij.internal <- intersect(i.internal, j.internal)
              
              slotsx <- getSlots(x)
              slotsv <- getSlots(value)
              slots <- Map(function(slotx, slotv) {
                  slotx[ij.internal] <- slotv
                  slotx
              }, slotsx, slotsv)
              
              setSlots(x) <- slots
              x
          })

cartesianAssign <- function(x, i, j, value) {
    xindexed <- x[i, j, cartesian = TRUE] # this will return appropriate error if indices are invalid
    
    # if value is right dimensions (exactly) when transposed
    if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
    
    # character indices XXX
    
    # sizes still don't match
    if (!all(dim(xindexed) == dim(value))) stop(call. = FALSE,
                                                glue::glue("Can't assign ([ i, j, cartesian = TRUE]<-) {length(value)} values into", 
                                                           " {length(i)} cartesian coordinates."))
    
    # do it 
    i.internal <- humvectorI(i, x)
    i.internal <- split(i.internal, rep(1:length(i), length.out = length(i.internal)))
    i.internal <- unlist(Map('[', i.internal, j))
    
    slotsx <- getSlots(x)
    slotsv <- getSlots(value)
    slots <- Map(function(slotx, slotv) {
        slotx[i.internal] <- slotv
        slotx
    }, slotsx, slotsv)
    
    setSlots(x) <- slots
    x
    
}

####

setMethod('rep', c(x = 'humVector'),
          function(x, ...) {
              slots <- getSlots(x)
              columns <- columns(x)
              slots <- lapply(slots,
                     function(slot) {
                         unlist(tapply(slot, columns, rep, ..., simplify = FALSE), use.names = FALSE)
                     })
              setSlots(x) <- slots
              x@nrow <- as.integer(length(slots[[1]]) / ncol(x))
              if (!is.null(rownames(x))) rownames(x) <- rep(rownames(x), ...)
              x
          })

setMethod('rev', c(x = 'humVector'),
          function(x) {
              x[length(x):1]
          })

#' @name humVector
#' @export
setMethod('c', 'humVector',
          function(x, ...) {
              xs <- list(x, ...)
              do.call('rbind', xs)
          })

rbind.humVector <- function(...) {
    xs <- list(...)
    xs <- lapply(xs, function(x) if (!is.humVector(x)) as(x, class(xs[[1]])) else x) # force to class of first thing
    Reduce(function(x, y) checkSame(x, y, 'rbind'), xs)
    
    ncols <- sapply(xs, ncol)
    if (length(unique(ncols)) != 1L) stop(call. = FALSE, "Can't concatinate humVectors with different numbers of columns.")
    
    x <- xs[[1]]
    xslots <- lapply(xs, getSlots) 
    
    xslots <- lapply(xslots,
                     function(slots) {
                         lapply(slots, `setdim<-`, value = x)
                     })
    slots <- Reduce(function(cur, rest) Map(rbind, cur, rest), xslots)
    slots <- lapply(slots, c)
    
    setSlots(x) <- slots
    
    ##
    rownames <- lapply(xs, rownames)
    rownames <- if (any(sapply(rownames, is.null))) NULL else unlist(rownames)
    rownames(x) <- rownames
    x@nrow <- sum(sapply(xs, nrow))
    
    x

    
}
cbind.humVector <-  function(...) {
    xs <- list(...)
    xs <- lapply(xs, function(x) if (!is.humVector(x)) as(x, class(xs[[1]])) else x) # force to class of first thing
    Reduce(function(x, y) checkSame(x, y, 'c'), xs)
    
    nrows <- sapply(xs, nrow)
    if (length(unique(nrows)) != 1L) stop(call. = FALSE, "Can't cbind to humVector's with different numbers of rows.")
    
    x <- xs[[1]]
    xslots <- lapply(xs, getSlots)
    
    setSlots(x) <- Reduce(function(cur, rest) Map(c, cur, rest), xslots)
    ##
    colnames <- lapply(xs, colnames)
    colnames <- if (any(sapply(colnames, is.null))) NULL else unlist(colnames)
    x@colnames <- colnames
    x@ncol <- sum(sapply(xs, ncol))
     
    x
}

setMethod('t', signature = 'humVector',
          function(x) {
              ncol <- ncol(x)
              nrow <- nrow(x)
              colnames <- colnames(x)
              rownames <- rownames(x)
              setSlots(x) <- lapply(getSlots(x),
                     function(slot) {
                         setdim(slot) <- x
                         c(t(slot))
                     })
              
              x@colnames <- rownames
              x@rownames <- colnames
              x@nrow <- ncol
              x@ncol <- nrow
              
              x
          })

setMethod('diag', signature = 'humVector',
          function(x) {
              if (dim(x)[1] != dim(x)[2]) stop(call. = FALSE, "Can't get diagonal of a non-square humVector. (I.e., the number of rows and columns must be equal.)")
              
              x[1:nrow(x), 1:ncol(x), cartesian = TRUE]
          })

##### is/as xx -----



#' @name humVector
#' @export
setMethod('is.na', signature = 'humVector',
          function(x) {
              mat <- is.na(getSlots(x)[[1]])
              setdim(mat) <- x
              mat
          })


#' @name humVector
#' @export
is.humVector <- function(x) inherits(x, 'humVector')

#' @name humVector
#' @export
setMethod('is.vector', signature = 'humVector', function(x) TRUE)


#' @name humVector
#' @export
setMethod('as.vector', signature = 'humVector', force)

#' @name humVector
#' @export
setMethod('as.list', signature = c('humVector'),
          function(x, ...) {
              
              x <- list(x, ...)
              x <- do.call('c', x)
              
              lapply(seq_along(x), function(i) x[i])
          })

#' @name humVector
#' @export
as.matrix.humVector <- function(x, ..., collapse = function(x, y) .paste(x, y, sep = ',', na.rm = TRUE)) {
    slots <- getSlots(x)
    mat <- Reduce(collapse, slots)
    
    mat[is.na(slots[[1]])] <- NA
    
    dim(mat) <- c(length(x), ncol(x))
    colnames(mat) <- x@colnames
    rownames(mat) <- x@rownames
    
    mat
}


#' @name humVector
#' @export
as.data.frame.humVector <- function(x, row.names = NULL, optional = FALSE, ...) {
    if (is.null(row.names)) row.names <- 1:length(x)
    
    value <- list(x)
    attr(value, 'row.names') <- row.names
    attr(value, 'names') <- class(x)[1]
    class(value) <- c('data.frame')
    value
}

#######-
#' @name humVector
#' @export
setMethod('as.character', 'humVector',
          function(x) {
              x <- as.matrix.humVector(x)
              x[] <- as.character(x)
              x
          })
#' @name humVector
#' @export
setMethod('show', signature = c(object = 'humVector'), 
          function(object) { 
              if (length(object) == 0L) {
                  cat(paste0(class(object), '[', nrow(object), ' , ', ncol(object), ']'))
              } else {
                  mat <- as.matrix(object)
                  if (ncol(mat) == 1L) {
                      mat <- c(mat)
                      names(mat) <- names(object)
                      if (!is.null(colnames(object))) cat('    ', colnames(object), '\n')
                  }
                  print(mat, quote = FALSE)
              }
            invisible(object)
            }  )


########## order, equality ----

order <- function(x, ..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) UseMethod('order')

order.default <- function(x, ..., na.last = TRUE, 
                          decreasing = FALSE, method = c("auto", "shell", "radix")) {
    do.call(base::order, list(x, ..., na.last = na.last, decreasing = decreasing, method = method))
}



#' @name humVector
#' @export
setMethod('sort', signature = c(x = 'humVector'),
          function(x, decreasing = FALSE) {
              x[order(x, decreasing = decreasing), ]
          })

##### comparisons ----


#' @name humVector
#' @export
setMethod('==', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              checkSame(e1, e2, '==')
              
              match_size(e1 = e1, e2 = e2, margin = 1:2, toEnv = TRUE)
              
              slots1 <- getSlots(e1)
              slots2 <- getSlots(e2)
              mat <- Reduce(`&`, Map(`==`, slots1, slots2))
              setdim(mat) <- e1
              mat
              
          })

#' @export
setMethod('!=', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              checkSame(e1, e2, '!=')
              
              !(e1 == e2)
          })

#' @name humVector
#' @export
setMethod('<', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              !(e1 >= e2 )
          })


#' @name humVector
#' @export
setMethod('<=', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              !(e1 > e2)
          })


#' @name humVector
#' @export
format.humVector <- function(x, ...) { as.character(x)}

#### arithmatic ----



setMethod('sum', signature = c('humVector'),
          function(x, ..., na.rm = FALSE) {
              x <- c(list(x), ...)
              
              x <- lapply(x, function(humv) {
                  setSlots(humv) <- lapply(getSlots(humv, c('numeric', 'integer', 'logical')), sum, na.rm = na.rm)
                  rownames(humv) <- NULL
                  colnames(humv) <- "sum"
                  humv
              })
              
              x <- if (length(x) > 1) {
                  Reduce(`+`, x)
              } else {
                  x[[1]]
                  
              }
              x@nrow <- x@ncol <- 1L
              x
          })

#' @name humVector
#' @export
setMethod('colSums', signature = c('humVector'),
          function(x, na.rm = FALSE) {
              
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        setdim(slot) <- x
                                        as.integer(unname(c(colSums(slot, na.rm = na.rm))))
                                    })
              rownames(x) <- NULL
              x@nrow <- 1L
                                    
              x
          })

#' @name humVector
#' @export
setMethod('rowSums', signature = c('humVector'),
          function(x, na.rm = FALSE) {
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        setdim(slot) <- x
                                        as.integer(unname(c(rowSums(slot, na.rm = na.rm))))
                                    })
              colnames(x) <- NULL
              x@ncol <- 1L
              x
          })

#' @name humVector
#' @export
setMethod('cumsum', signature = c('humVector'),
          function(x) {
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        setdim(slot) <- x
                                        c(apply(slot, 2, cumsum))
                                    })
              
              x
          })


#' @name humVector
#' @export
setMethod('-', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              e1 + (-e2)
          })

#' @name humVector
#' @export
setMethod('diff', signature = c('humVector'),
          function(x, lag = 1L) {
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        setdim(slot) <- x
                                        c(diff(slot, lag = lag))
                                    })
              rownames(x) <- rownames(x)[-1]
              x@nrow <- x@nrow - 1L
              x
          })


#' @name humVector
#' @export
setMethod('+', signature = c('humVector', 'humVector'),
          function(e1, e2) {
              checkSame(e1, e2, '+')
              recycledim(e1 = e1, e2 = e2, funccall = '+')
              
              slots <- Map(`+`, getSlots(e1, c('numeric', 'integer', 'logical')), getSlots(e2, c('numeric', 'integer', 'logical')))
              setSlots(e1) <- slots
              e1
          })

#' @name humVector
#' @export
setMethod('+', signature = c('humVector', 'ANY'),
          function(e1, e2) {
              e1 + as(e2, class(e1))
          })

#' @name humVector
#' @export
setMethod('+', signature = c('ANY', 'humVector'),
          function(e1, e2) {
              as(e1, class(e2)) + e2
          })

#' @name humVector
#' @export
setMethod('-', signature = c( 'humVector', 'missing'),
          function(e1) {
              
              setSlots(e1) <- lapply(getSlots(e1), function(slot) -slot )
              e1
          })


#' @name humVector
#' @export
setMethod('-', signature = c( 'humVector', 'humVector'),
          function(e1, e2) {
              e1 + -e2
              
          })

#' @name humVector
#' @export
setMethod('-', signature = c('humVector', 'ANY'),
          function(e1, e2) {
              e1 - as(e2, class(e1))
          })

#' @name humVector
#' @export
setMethod('-', signature = c('ANY', 'humVector'),
          function(e1, e2) {
             as(e1, class(e2)) - e2
          })



#' @name humVector
#' @export
setMethod('*', signature = c('humVector', 'numeric'),
          function(e1, e2) {
              setSlots(e1) <- lapply(getSlots(e1, c('numeric', 'integer')), 
                                     function(x) {
                                       as(x * e2, class(x))
                                         
                                     })
              e1 
          })

#' @name humVector
#' @export
setMethod('*', signature = c('numeric', 'humVector'),
          function(e1, e2) {
              e2 * e1
          })






