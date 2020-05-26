
#' Pseudo-vector classes for humdrum data.
#' 
#' HumdrumR defines a number of S4 classes which are,
#' underneath the surface, [https://en.wikipedia.org/wiki/Composite_data_type](composite data types),
#' made up of collections of base::R atomic vectors, stuck together.
#' (Things like this are called structs, or tuples, or records in other languages.)
#' The "vectorization" or R's atomic types is R's key strength so we
#' want, as much as possible for our composite types to act just R atomics.
#' 
#' `struct` is a \emph{virtual} S4 class for just such composite atomic vectors. 
#' The `struct` defines all the necessarry methods to treat a collection of
#' atomic vectors as a single vector/matrix-like object---simply
#' make your new class inherit `struct` and it is all taken care of!
#' (To do this, specifify `contains = "struct"` in your call to [methods::setClass()].)
#' 
#' Be warned, `R` is limited in this regard---users can't *really* define
#' S4 classes that act fully like R atomics---, so you may 
#' run in to problems if you take this too far. 
#' For instance, though `struct` classes work (ok) in [base::data.frame]s
#' [data.table::data.table]s and [tibble::tibble]s might give you problems.
#' 
#' @section Behavior:
#' 
#' `struct` subclasses behave very similarly to normal R vectors.
#' However, they do differ in a few respects, mostly in ways that
#' avoid some of the quirky behaviors with R vectors:
#' In general, the distinction between dimensionless vectors
#' and dimensioned vectors is slightly weakened with `structs`
#' compared to normal R atomic vectors.
#' In general, dimensionless `struct`s are treated more implicitely
#' like column-matrices.
#' Notably, if the struct has rows, `length(struct) == nrow(struct)`.
#' Most importantly, dimensioned `struct`s won't drop their dimensions
#' under various common operations (`c`, `[]`, etc.), the way
#' base-R matrices do.
#' Thie biggest difference is that `c` doesn't always cause `struct`s to lose their dimensions.
#' Rather, if the first argument to `c` has dimensions, the dimensions are kept and the `struct`s
#' are `rbind`ed (assuming the number of columns are conformable).
#' 
#' Other differences:
#' * `struct`s can only have no dimensions (`dim(struct) == NULL`) or two dimentions. Higher dimensional arrays are not possible (yet).}
#' * `rowSums` and `colSums` will coerce a dimensionless struct to a column matrix.
#' * `struct`s always throw an error if you try to index them with a index value
#'    that is greater than the length/nrow of the `struct`. This is different than atomic vectors,
#'    which will pad the vector up to the length of the index you give---a sometimes useful but quirky behavior.
#' * `struct`s with two dimensions have a `cartesian` indexing argument.
#'    If `cartesian = TRUE`, the `i` and `j` arguments are treated as cartesian coordinates.
#'    (This behavior can be achieved with base R matrices (or `struct`s) by inputing a matrix with two columns.)
#' 
#' 
#' 
#' 
#' 
#' @section Requirements:
#' 
#' To work, `struct` makes a few assumptions about your class.
#' Your class must have one or more slots which are vectors, all of which are the same length.
#' `struct`'s indexing method will cause all of these vectors to be indexed as one.
#' When you define a new subclass of `struct`, it will inherit a 
#' `validObject` method which assures that all elements are the same dimension.
#' Thus, if you are writing your own [methods::validObject()] method (using [methods::setValidity()])
#' you just have to worry specifically about the validity of the information in your slots,
#' not that the slots are all the same length.
#' 
#' 
#' @section Initialize:
#' 
#' An initialize method which automatically makes all slots the same length is predefined
#' for `structs`. If you want to make a more specialized [methods::initialize()] method,
#' you can still take advantage of the inherited method by using [methods::callNextMethod()] at the 
#' beginning of your function.
#' 
#' 
#' @section Predefined methods:
#' 
#' You must specify [base::order] and any arithmetic/comparison methods for your class
#' yourself. However,
#' \itemize{
#'  \item{If you define `>` and `>=`, `<` and `<=` will be automatically defined.}
#'  \item{If you define `order`, `sort` will be automatically defined.}
#'  \item{If you define `as.character` for your class, `show` and
#'   `format` methods are defined automatically.}
#' }
#' 
#' Default arithmetic methods for addition, (scalar) multiplication, and negation (`-x`) are defined.
#' They assume that adding your class to another is simply the same as adding each numeric slot in parallel.
#' If this is not the case, you'll need to create your own, more specific, method!
#' 
#' @name struct
NULL

setClassUnion('dimnames', c('character', 'integer', 'NULL'))
setClassUnion('maybeinteger', c('NULL', 'integer'))

setClass('struct', contains = 'VIRTUAL', slots = c(dim = 'maybeinteger', rownames = 'dimnames', colnames = 'dimnames')) 

setValidity('struct', 
            function(object) {
                class <- class(object)
                #
                slots <- getSlots(object)
                slotlen <- length(slots[[1]])
                
                dim <- object@dim
                rownames <- object@rownames
                colnames <- object@colnames
                errors <- c(
                    if (!{
                            all(lengths(slots) == slotlen)
                                    }) glue::glue("Vectors in {class} data slots must all be the same length."),
                    if (!{
                            all(sapply(slots, is.vector)) && 
                            all(sapply(slots, is.atomic))
                                    }) glue::glue('{class} data slots must all be atomic vectors.'),
                    
                    ######## if dim is NULL
                    if (is.null(dim)) {
                        c(
                            if (!{
                                    is.null(colnames)
                                            }) glue::glue("{class} can't have @colnames if @dim is NULL."),
                            if (!{
                                    is.null(rownames) ||
                                    length(rownames) == slotlen
                                            }) glue::glue("If {class}@dim is NULL, it is a dimensionless vector, and the (row)names must ", 
                                                          "be the same length as the vectors in the data slots.")
                        )
                    ####### if dim is not NULL
                    }  else { 
                        c(
                            if (!{
                                    length(dim) == 2L &&
                                    all(dim >= 0)
                                            }) glue::glue("The {class}@dim must either be NULL, or a two-length integer vector with both integers >= 0."),
                            if (!{
                                    slotlen == prod(dim)
                                            }) glue::glue("If {class}@dim is not NULL, the product of the two dimensions (i.e., nrow * ncol) must match the total length",
                                               " of the atomic-vector data slots."),
                            if (!{
                                    is.null(colnames) ||
                                    length(colnames) == dim[2]
                                            }) glue::glue("{class}@colnames must be either NULL or the same length as ncol({class})."),
                            if (!{
                                    is.null(rownames) ||
                                    length(rownames) == (slotlen / dim[2]) 
                                   }) glue::glue("{class}@rownames must be either NULL or the same length as the data slot vectors divided by ncol({class}).")
                            
                        )
                    })
                            
                if (length(errors) == 0L) TRUE else errors 
                
            })


setMethod('initialize', 
          'struct',
          function(.Object, ..., dim = NULL, colnames = NULL, rownames = NULL) {
              slots <- list(...)
              slots <- lapply(slots, unname)
              if (length(slots) == 0L) return(.Object)
              slots <- do.call('match_size', slots)
              
              .Object@dim <- dim
              .Object@colnames <- colnames
              .Object@rownames <- rownames
              
              
              ## NA in any slot is NA in all slots
              na <- Reduce(`|`, lapply(slots, is.na))
              slots <- lapply(slots, function(slot) `[<-`(slot, na, NA))
              
              setSlots(.Object) <- slots
              validObject(.Object)
              .Object
          } )

getSlots <- function(x, classes = c('numeric', 'integer', 'logical', 'character')) {
    slotinfo <- methods::getSlots(class(x))
    slotinfo <- slotinfo[!names(slotinfo) %in% c('dim', 'colnames', 'rownames')]
    slotinfo <- slotinfo[slotinfo %in% classes]

    slots <- lapply(names(slotinfo), slot, object = x)
    names(slots) <- names(slotinfo)
    slots
}

`setSlots<-` <- function(x, value) {
    slotnames <- slotNames(x)
    
    slotnames <- slotnames[!slotnames %in% c('dim', 'colnames', 'rownames')]
    for (s in slotnames) {
        slot(x, s) <- value[[s]]
    }
    
    x
}

columns <- function(humvec) {
    ncol <- if (hasdim(humvec)) ncol(humvec) else 1L
    rep(1:ncol, each = length(humvec))
}


########## shape ----

#' @name struct
#' @export dim ncol nrow length
NULL

#' @name struct
#' @export length dim ncol nrow
setMethod('nrow', signature = 'struct', function(x) x@dim[1])
setMethod('ncol', signature = 'struct', function(x) x@dim[2])
setMethod('length', signature = 'struct', function(x) if (is.null(x@dim)) length(getSlots(x)[[1]]) else x@dim[1])
setMethod('dim', signature = 'struct', function(x) x@dim )

setMethod('dim<-', 'struct',
          function(x, value) {
              value %!<-% as.integer(value)
              if (is.null(value) || any(dim(x) != value)) {
                  x@colnames <- NULL
                  x@rownames <- NULL
              }
              x@dim <- value
              
              validObject(x)
              x
              
          })

### tools 




recycledim <- function(..., funccall) {
    # SHOULD DOCUMENT THIS BETTER XXX
    # It's a more limited alternative to match_size
    # accepts two named args
    args <- list(...)[1:2]
    
    havedim <- sapply(args, hasdim)
    # no dim
    if (all(!havedim)) {
        args <- setNames(match_size(args[[1]], args[[2]]), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    
    # one without dim (make it into column vector)
    args[!havedim] <- lapply(args[!havedim], cbind)
    
    # Both have dim
    
    d1 <- dim(args[[1]])
    d2 <- dim(args[[2]])
    
    if (all(d1 == d2)) {
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    
    if (all(d1 == 1L) || all(d2 == 1L)) {
        args <- setNames(match_size(args[[1]], args[[2]], margin = 1:2), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }   
    
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
    
    .stop("In call to {funccall}, the two structs are nonconformable.\n",
          "To confirm, eitehr one object must be of dim = c(1,1), or 
          at least one of their dimensions needs to be the same, while the other is either (also) the same, or 1.")
    
}



#' @name struct
#' @export names colnames rownames
setMethod('names',    c(x = 'struct'), function(x) x@rownames)
setMethod('rownames', c(x = 'struct'), function(x) x@rownames)
setMethod('colnames', c(x = 'struct'), function(x) x@colnames)
setMethod('dimnames', c(x = 'struct'), function(x) {
    dn <- list(x@rownames, x@colnames)
    if (sum(lengths(dn)) == 0L) NULL else dn
})

#' @exportMethod names<- colnames<- rownames<-

setMethod('colnames<-', c(x = 'struct'),
          function(x, value) {
              if (is.null(x@dim)) .stop("Can't add colnames to a {class(x)} with no dimensions.")
              if (!is.null(value) && length(value) != ncol(x)) .stop("Colnames assigned to {class(x)} must be of length ncol({class(x)}).",
                                                                     " In this case, you are tring to assign {length(value)} ", 
                                                                     plural(length(value), 'colnames', 'colname'),
                                                                     " into a {class(x)} with {ncol(x)} ",
                                                                     plural(ncol(x), "columns.", "column."))
              x@colnames <- value
              x
          })
setMethod('rownames<-', c(x = 'struct'),
          function(x, value) {
              if (!is.null(value) && length(value) != length(x)) .stop(ifelse = is.null(x@dim),
                                                                       "Rownames assigned to {class(x)} must be ", 
                                                                       "<the same length as the {class(x)}|be of length nrow({class(x)})>",
                                                                       " In this case, you are tring to assign {length(value)} ",  plural(length(value), 'rownames', 'rowname'), 
                                                                       " into a {class(x)} ", 
                                                                       "<of length ({length(x)})|with {nrow(x)}> ", plural(nrow(x), "rows.", "row."))
              x@rownames <- value
              x
          })
setMethod('names<-', c(x = 'struct'),
          function(x, value) {
              x@rownames <- value
              x
          })
setMethod('dimnames<-', c(x = 'struct'),
          function(x, value) {
              if (is.null(value) || length(value) == 1L) {
                  x@rownames <- NULL
                  x@colnames <- NULL
                  return(x)
              }
              
              if (!is.list(value) || length(value) != 2L) .stop('When assigning dimnames to a {class(x)} using dimnames({class(x)}) <- value, value must be a list of length two.')
              rownames(x) <- value[[1]]
              colnames(x) <- value[[2]]
              x
          })


##### indexing ----


checkSame <- function(x, y, call) {
    # this function tests if two arguments are the same class
    # it is necessary for any generic method for two structs, where the two structs might be some specific subclass.
    if (all(class(x) != class(y))) {
        .stop("Can't apply {call} to a struct with something else that is not the same class.\n",
              "In this case, you are trying to combine a value of class '{class(x)[1]}' with another value of class '{class(y)[1]}'.")
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

setMethod('[', c(x = 'struct', i = 'numeric', j = 'missing'),
          function(x, i) {
            i <- i[i != 0] # zeros are ignored
            
            ### First, special cases where i is empty
            if (length(i) == 0L) {
                x <- emptyslots(x)
                
                x@rownames %!<-% vector(class(x@rownames), 0)
                x@dim[1]   %!<-% 0L
                
                return(x)
            }
            
            if (all(i < 0)) i <- (1L:length(x))[i] # flip negative indices (only if ALL negative)
            
            # Check for problems with i 
            if (any(i < 0))  .stop("When indexing a {class(x)}, you can't mix negative and positive numbers in the index.")
            if (any(i > length(x))) .stop(ifelse = !hasdim(x),
                                          "The i-index is greater than the <length|nrow> of the {class(x)} object you are trying to index.",
                                          "<Normal R vectors don't throw an error for this, but we do.|>")
            
            ### translate by-row i to actual i of internal vectors
            i.internal <- if (hasdim(x) && ncol(x) > 1) humvectorI(i, x) else i
            
            # modify dimension info
            x@rownames %!<-% rownames(x)[i]
            x@dim[1]   %!<-% length(i)
            
            if (!is.null(ncol(x)) && ncol(x) == 0L) return(x) # in this case, the slots are already empty so no further changes are needed.
            
            # do it! 
            setSlots(x) <- lapply(getSlots(x), '[', i = i.internal)
            
            x
          })

setMethod('[', c(x = 'struct', i = 'character', j = 'missing'),
          function(x, i) {
              if (is.null(rownames(x))) {
                       .stop(ifelse = !hasdim(x),
                             "You can't <|row->index a {class(x)} (i.e. {class(x)}[i<|, >])", 
                                             "with a character string if the {class(x)} has no <|row>names (i.e., <|row>names({class(x)}) = NULL).")
              }
              if (is.numeric(rownames(x))) rownames(x) <- as.character(rownames(x))
              
              i <- locate(i, rownames(x))
              nomatch <- lengths(i) == 0L
              if (any(nomatch))  .stop(ifelse = !hasdim(x),
                                       "In your attempt to <|row->index a {class(x)} (i.e., {class(x)}[i<|, >])",
                                       " using character indices, ", 
                                       glue::glue_collapse(paste0("'", names(i)[nomatch], "'"), 
                                                           sep = ', ', last = ', and '),
                                       plural(sum(nomatch), ' are not rownames', ' is not a rowname'),
                                       ' in the {class(x)} object.')
              
              i <- unlist(i)
              x[i, ]
          })
setMethod('[', c(x = 'struct', i = 'logical', j = 'missing'),
          function(x, i) {
              if (length(i) != length(x)) .stop(ifelse = !hasdim(x),
                                              "Can't index[i<|, >] a {class(x)} with a logical vector of a length that does not match <length|nrow>({class(x)}).")
              x[which(i), ]
          })

setMethod('[', c(x = 'struct', i = 'matrix', j = 'missing'),
          function(x, i ) {
              if (!hasdim(x)) .stop("You can't index a dimensionless {class(x)} object with a matrix.")
              matclass <- class(i[1, 1])
              
              if (matclass %in% c('character', 'numeric', 'integer')) {
                  if (ncol(i) == 1L) return(x[c(i), ])
                  if (nrow(i) == 1L) return(x[ , c(i)])
                  if (nrow(i) == nrow(x) && ncol(i) == 2)  return(x[i[ , 1], i[ , 2], cartesian = TRUE])
                  
                  .stop("To index a {class(x)} a numeric or character matrix, that matrix must either:\n",
                        "\t1) Be a 1-column (index rows) or 1-row (index columns) matrix---in which case, it is treated like a vector.\n",
                        "\\2) Be a 2-column matrix, which will be treated as cartesian coordinates.\n",
                        "\t\t(If you use a character matrix, the {class(x)} object must have appropriate row/colnames defined!)")
              }
              if (matclass != 'logical') stop(call. = FALSE, glue::glue("Can't index a struct with a {matclass} matrix."))
              
              
              if (ncol(i) == 1L && length(i) == nrow(x)) return(x[which(i), ])
              if (nrow(i) == 1L && length(i) == ncol(x)) return(x[, which(i)])
              
              
              if (!identical(dim(i), dim(x))) stop(call. = FALSE, "Can't index struct[i , ] with a logical matrix, unless it either has the exact same",
                                                   " dimensions as the struct, or matches one dimension while the other dimension == 1, ",
                                                   "i.e., if the indexing matrix is a single column with the same number of rows as the indexed {class(x)} object OR the indexing matrix ",
                                                   "has is a single row with the same number of columns as the indexed {class(x)} object.")
              
              ij <- which(i, arr.ind = TRUE)
              
              x[ij[ , 'row'], ij [, 'col'], cartesian = TRUE]
              
          })



### [ , j] ----

setMethod('[', c(x = 'struct', i = 'missing', j = 'numeric'),
          function(x, j) {
              if (!hasdim(x)) .stop("You can't take a j (column-wise) index of a {class(x)} object with no dimensions!")
              
              j <- j[j != 0] # zeros are ignored
              
              ### First, special cases where j is empty
              if (length(j) == 0L) {
                  x <- emptyslots(x)
                  
                  x@colnames %!<-% vector(class(x@colnames), 0)
                  x@dim[2]     <- 0L # we have already seen that x@dim is not NULL so we don't need %!<-%
                  
                  return(x)
              }
              
              if (all(j < 0)) j <- (1L:ncol(x))[j] # flip negative indices (only if ALL negative)
              
              # check for problems with j
              if (any(j < 0))  .stop("When indexing a {class(x)}, you can't mix negative and positive numbers in the index.")
              if (any(j > ncol(x))) .stop("Index[, j] is greater than the ncol of this {class(x)} object.")
              
              ### translate by-col j to actual j of internal vectors
              j.internal <- humvectorJ(j, x) 
              
              # modify dimension info
              x@colnames %!<-% colnames(x)[j]
              x@dim[2]     <-  length(j)
              
              if (nrow(x) == 0L) return(x) # in this case, the slots are already empty so no further changes are needed.
              
              # do it! 
              setSlots(x) <- lapply(getSlots(x), '[', i = j.internal)
              
              x
          })



setMethod('[', c(x = 'struct', i = 'missing', j = 'character'),
          function(x, j) {
              if (is.null(colnames(x))) .stop("You can't column-index a {class(x)} (i.e. {class(x)}[ , j])", 
                                              " with a character string if the {class(x)} has no colnames (i.e., colnames({class(x)}) = NULL).")
              if (is.numeric(colnames(x))) colnames(x) <- as.character(colnames(x))
              
              j <- locate(j, colnames(x))
              nomatch <- lengths(j) == 0L
              if (any(nomatch))  .stop("In your attempt to column-index a {class(x)} (i.e., {class(x)}[ , j])",
                                       " using character indices, ", 
                                       glue::glue_collapse(paste0("'", names(j)[nomatch], "'"), 
                                                           sep = ', ', last = ', and '),
                                       plural(sum(nomatch), ' are not colnames', ' is not a colname'),
                                       ' in the {class(x)} object.')
              
              j <- unlist(j)
              x[ , j]
          })


setMethod('[', c(x = 'struct', i = 'missing', j = 'logical'),
          function(x, j ) {
              if (length(j) != ncol(x)) .stop("Can't index a {class(x)} (i.e., {class(x)}[ , j]) with a logical vector that is a different length than ncol({class(x)}).")
              x[ , which(j)]
          })

setMethod('[', c(x = 'struct', i = 'missing', j = 'matrix'),
          function(x, j ) {
              .stop("You can't index a {class(x)} with a matrix in the j indexing argument.")
          })

### [i, j]


setMethod('[', c(x = 'struct'),
          function(x, i, j, cartesian = FALSE) {
              if (is.numeric(i)) i <- i[i != 0] # zeros are ignored
              if (is.numeric(j)) j <- j[j != 0]
              
              if (cartesian) {
                  if (!is.numeric(i) || !is.numeric(j)) .stop( "Can't do cartesian-index a {class(x)} if i and j aren't BOTH numeric.")
                  # XXX MAKE CHARACTER POSSIBLE?
                  
                  match_size(i = i, j = j, toEnv = TRUE)
                  
                  if (any(i < 0 | j < 0)) stop(call. = FALSE, "Can't do cartesian indexing with negative indices.")
                  
                  i.internal <- humvectorI(i, x)
                  i.internal <- split(i.internal, rep(1:length(i), length.out = length(i.internal)))
                  i.internal <- unlist(Map('[', i.internal, j))
                  
                  
                  x@dim[2] <- 1L
                  x@dim[1] <- length(i)
                  setSlots(x) <- lapply(getSlots(x), '[', i.internal)
                  x@rownames %!<-% rownames(x)[i]
                  x@colnames   <- NULL
                  
                  x
                             
                  
              } else {
                 x <- x[i,  ]
                 x <- x[ , j]
              }
              
              x
          })

#### [i, j] <- value ----

setMethod('[<-', c(x = 'struct', i = 'ANY', j = 'missing', value = 'struct'),
          function(x, i, value) {
              checkSame(x, value, '[i , ]<-')
              
              if (length(value) == 0 || any(dim(value) == 0L)) return(x)
              
              # if either are vectors, make them into column vectors
              dimx <- dim(x)
              if (!hasdim(x)) x@dim <- c(length(x), 1L)
              if (!hasdim(value)) value@dim <- c(length(value), 1L)
              
              
              # this will return appropriate error if indices are invalid
              xindexed <- x[i, ] 
              
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # recycle columns
              if (length(value) %in% c(1, length(i)) && ncol(value) == 1L && ncol(x) > 1L) {
                  value <- Repeat(value, length.out = ncol(x), margin = 2)
              }
              
              # sizes still don't match
              if (ncol(value) != ncol(xindexed)) .stop("Can't assign ([i]<-) a {class(x)} object with {ncol(value)} columns into rows of a {class(x)} object with {ncol(x)} columns.")
              
              # character or logical indices
              if (is.character(i) && !is.null(rownames(x))) i <- locate(i, rownames(x))
              if (is.logical(i)) {
                  i.internal <- which(i)
              } else {
                  # negative indices
                  if (all(i < 0)) i <- (1L:nrow(x))[i]
                  if (any(i < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in struct assignment index.")
                  
                  #
                  if (length(value) == 1L && length(i) != 1L) value <- rep(value, length.out = length(i))
                  if (length(value) != length(i)) stop(call. = FALSE, 
                                                       glue::glue("Can't row-assign ([i]<-) a {class(value)} with {length(value)} rows into {length(i)} rows of another {class(x)}.\n",
                                                                  "To conform, the value being assigned must have the same number of rows, or have only one row, in which case that one row is recycled."))
                  
                  
                  i.internal <- humvectorI(i, x)
              }
              
              ## do it
             
              
              slotsx <- getSlots(x)
              slotsv <- getSlots(value)
              slots <- Map(function(slotx, slotv) {
                                  slotx[i.internal] <- slotv
                                  slotx
                             },
                           slotsx, slotsv)
              setSlots(x) <- slots
              
              x@dim <- dimx # this removes dimensions if were none to begin with
              x
          })

setMethod('[<-', c(x = 'struct', i = 'matrix', j = 'missing', value = 'struct'),
          function(x, i, value) {
              checkSame(x, value, '[i , ]<-')
              
              if (is.numeric(i) && ncol(i) == 2) {
                  i[] <- as.integer(i)
              } else {
                  if (is.logical(i) && all(dim(i) == dim(x))) {
                      i <- which(i, arr.ind = TRUE)
                  } else {
                      
                      .stop("Can't assign to a {class(x)} object with a matrix in the i-index argument, unless that i matrix is a",
                            " numeric matrix with two rows or a logical matrix with the same dimensions as the {class(x)}.")
                  }
              }
              

              
              return(cartesianAssign(x, i[,1], i[,2], value))
          })

setMethod('[<-', c(x = 'struct', i = 'missing', j = 'ANY', value = 'struct'),
          function(x, j, value) {
              checkSame(x, value, '[ , j]<-')
              
              if (!hasdim(x)) .stop("You can't do a j (column-wise) assignment to {class(x)} object with no dimensions!")
              
              # if value is a vector, make it into a column vector
              if (!hasdim(value)) value@dim <- c(length(value), 1L)
              
              # this will return appropriate error if indices are invalid
              xindexed <- x[, j] 
              
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # recycle rows
              if (ncol(value) %in% c(1, length(j)) && nrow(value) == 1L && nrow(x) > 1L) {
                  value <- Repeat(value, length.out = nrow(x), margin = 1)
              }
              
              # sizes still don't match
              if (nrow(value) != nrow(x)) stop(call. = FALSE,
                                               glue::glue("Can't assign ([ , j]<-) a struct with {nrow(value)} rows into columns of a struct with {nrow(x)} rows."))
              
              # character indices
              if (is.character(j) && !is.null(colnames(x))) j <- locate(j, colnames(x))
              if (is.logical(j)) j <- which(j)
              
              ## negative indices
              if (all(j < 0)) j <- (1L:col(x))[j]
              if (any(j < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in struct assignment index.")
              
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




setMethod('[<-', c(x = 'struct', i = 'ANY', j = 'ANY', value = 'struct'),
          function(x, i, j, value, cartesian = FALSE) {
              checkSame(x, value, '[ i, j]<-')
              
              if (!hasdim(x)) .stop("You can't do a j (column-wise) assignment to {class(x)} object with no dimensions!")
              
              # if value is a vector, make it into a column vector
              if (!hasdim(value)) value@dim <- c(length(value), 1L)
              
              if (cartesian) return(cartesianAssign(x, i, j, value))
              
              # this will return appropriate error if indices are invalid
              xindexed <- x[i, j]
              
              # if value is right dimensions (exactly) when transposed:
              if (!all(dim(xindexed) == dim(value)) && all(dim(xindexed) == rev(dim(value)))) value <- t(value)
              
              # character indices
              if (is.character(i)) i <- locate(i, rownames(x))
              if (is.character(j)) j <- locate(j, colnames(x))
              
              # logical indices
              if (is.logical(i)) i <- which(i)
              if (is.logical(j)) j <- which(j)
              
              ## negative indices
              if (all(j < 0)) j <- (1L:col(x))[j]
              if (any(j < 0)) stop(call. = FALSE, "Can't mix negative and positive numbers in struct assignment index.")
              
              # sizes still don't match
              
              if (!all(dim(xindexed) == dim(value))) stop(call. = FALSE,
                                               glue::glue("Can't assign ([ i, j, cartesian = FALSE]<-) a {class(value)} object with dimensions ({nrow(value)}, {ncol(value)}) into", 
                                                          " a subset of a {class(x)} object with indices of dim ({length(i)}, {length(j)})."))
              
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

setMethod('[<-', c(x = 'struct', i = 'matrix'),
          function(x, i, value) {
            .stop("Assignment to a matrix index is not (yet) defined for {class(x)} objects.")  
              
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

setMethod('rep', c(x = 'struct'),
          function(x, ...) {
              slots <- getSlots(x)
              
              slots <- if (!hasdim(x)) {
                  lapply(slots, rep, ...)
              } else {
                  columns <- columns(x)
                  lapply(slots,
                         function(slot) {
                             unlist(tapply(slot, columns, rep, ..., simplify = FALSE), use.names = FALSE)
                         })
              }
              
              setSlots(x) <- slots
              x@dim[1]   %!<-% as.integer(length(slots[[1]]) / (ncol(x) %maybe% 1))
              x@rownames %!<-% rep(rownames(x), ...)
              
              x
          })

setMethod('rev', c(x = 'struct'),
          function(x) {
              x[length(x):1]
          })

#' @name struct
#' @export
setMethod('c', 'struct',
          function(x, ..., rbind = FALSE) {
              xs <- list(x, ...)
              
              classes <- unique(sapply(xs, class))
              if (length(classes) > 1L) .stop(ifelse = length(classes) > 1L,
                                              "You can't concatinate a {class(x)} object with objects of class<|es> ",
                                              glue::glue_collapse(classes[-1], sep = ', ', last = ', or '), '.', sep = '')
              
              #
              nulldim <- !hasdim(xs[[1]])
              
              # make vectors into column vectors
              xs <- lapply(xs, 
                           function(x) {
                               if (!hasdim(x))  x@dim <- c(length(x), 1L)
                               x
                               })
              dims <- t(sapply(xs, dim))
              # row vectors can be transposed to fit in
              if (!rbind) {
                  rowvectors <- dims[ , 1] == 1L
                  
                  if(any(!rowvectors) && all(dims[!rowvectors, 2] == 1L)) {
                      xs[rowvectors] <- lapply(xs[rowvectors], t)
                      dims[rowvectors, ] <- apply(dims[rowvectors, , drop = FALSE], 1, rev)
                      
                  }
              }
              if (length(unique(dims[ , 2])) != 1L) .stop( "Can't concatinate {class(x)} objects with different numbers of columns.")
              
              # do it
              x <- xs[[1]]
              
              xslots <- lapply(xs, getSlots) 
              xslots <- Map(function(slots, curx) {
                                    lapply(slots, function(slot) {slot %dim% curx})
                               },
                               xslots, xs)
              slots <- Reduce(function(cur, rest) Map(rbind, cur, rest), xslots)
              slots <- lapply(slots, c)
              setSlots(x) <- slots
              
              # rownames
              rownames <- lapply(xs, rownames)
              x@rownames <-  if (all(!sapply(rownames, is.null))) do.call('c', rownames) else NULL 
              
              if (nulldim) {
                  x@dim <- NULL
                  x@colnames <- NULL
                  
              } else {
                  #dim
                  x@dim[1] <- sum(lengths(xs))
                  x@dim[2] <- dims[1, 2]
                  
                  # colnames
                  colnames <- lapply(xs, colnames)
                  colnames <- Filter(Negate(is.null), colnames)
                  x@colnames <- if (length(colnames) == 1L || (length(colnames) > 0L && Reduce(identical, colnames))) colnames[[1]] else NULL
              }
              x
              
          })
#' @export
rbind.struct <- function(...) {
    xs <- list(...)

    xs <- lapply(xs, function(x) if (!hasdim(x)) t(x) else x)
    
    x <- do.call('c', c(unname(xs), list(rbind = TRUE)))
    
    if (!hasdim(x)) x@dim <- c(length(x), 1L)
    
    x
}

#' @export
cbind.struct <-  function(...) {
    xs <- list(...)
    xs <- lapply(xs, t)
    
    #
    x <- do.call('rbind', xs)
    x <- t(x)
    
    # new colnames
    existing <- sapply(xs, rownames) 
    new <- .names(xs)
    new <- Map(function(old, new, len) {if (new == '') old else rep(new, len) }, existing, new, lapply(xs, nrow))
    if (any(!sapply(new, is.null))) new[sapply(new, is.null)] <- list("")
    colnames(x) <- unlist(new)
    x
    
}

#' @export
setMethod('t', signature = 'struct',
          function(x) {
              if (!hasdim(x)) x@dim <- c(length(x), 1L)
              
              setSlots(x) <- lapply(getSlots(x),
                     function(slot) {
                         slot <- slot %dim% x
                         c(t(slot))
                     })
              
              x@dim <- rev(x@dim)
              
              rownames <- rownames(x)
              x@rownames <- colnames(x)
              x@colnames <- rownames
              
              x
          })

#' @export
setMethod('diag', signature = 'struct',
          function(x) {
              if (!hasdim(x)) .stop("Can't get the diagonal of a {class(x)} with no dimensions!")
              if (dim(x)[1] != dim(x)[2]) .stop("Can't get diagonal of a non-square {class(x)}.", 
                                               "(I.e., the number of rows and columns must be equal.)")
              
              x[1:nrow(x), 1:ncol(x), cartesian = TRUE]
          })

##### is/as xx -----



#' @name struct
#' @export
setMethod('is.na', signature = 'struct',
          function(x) {
              na <- is.na(getSlots(x)[[1]])
              na %dim% x
          })


#' @name struct
#' @export
is.struct <- function(x) inherits(x, 'struct')

#' @name struct
#' @export
setMethod('is.vector', signature = 'struct', function(x) TRUE)


#' @name struct
#' @export
setMethod('as.vector', signature = 'struct', force)

#' @name struct
#' @export
setMethod('as.list', signature = c('struct'),
          function(x, ...) {
              
              x <- list(x, ...)
              x <- do.call('c', x)
              
              lapply(seq_along(x), function(i) x[i])
          })

#' @export
setGeneric('as.atomic', function(x, ...) standardGeneric('as.atomic'))
setMethod('as.atomic', 'struct',
          function(x,  collapse = function(x, y) .paste(x, y, sep = ',', na.rm = TRUE)) {
              slots <- getSlots(x)
              atom <- Reduce(collapse, slots)
              
              atom[is.na(slots[[1]])] <- NA
              dim(atom) <- dim(x)
              colnames(atom) <- x@colnames
              if (hasdim(atom)) rownames(atom) <- x@rownames else names(atom) <- x@rownames
              atom
          })


#' @name struct
#' @export
as.matrix.struct <- function(x, ..., collapse = function(x, y) .paste(x, y, sep = ',', na.rm = TRUE)) {

    mat <- as.atomic(x, collapse = collapse)
    
    if (!hasdim(mat)) dim(mat) <- c(length(x), 1L)
    colnames(mat) <- x@colnames

    mat
}


#' @name struct
#' @export
as.data.frame.struct <- function(x, optional = FALSE, ...) {
    row.names <- x@rownames %maybe% 1:length(x)
    
    value <- list(x)
    attr(value, 'row.names') <- row.names
    attr(value, 'names') <- class(x)[1]
    class(value) <- c('data.frame')
    value
}


#' @name struct
#' @export
format.struct <- function(x, ...) { as.character(x)}


#' @name struct
#' @export
setMethod('as.character', 'struct',
          function(x) {
              x <- as.atomic(x)
              x[] <- as.character(x)
              x
          })
#' @name struct
#' @export
setMethod('show', signature = c(object = 'struct'), 
          function(object) { 
              
              cat(class(object), 
                  if (!hasdim(object)) paste0('[', nrow(object), ' , ', ncol(object), ']'),
                  '\n', sep = '')
              if (length(object) > 0L) {
                  print(as.character(object), quote = FALSE)
              }
            invisible(object)
            }  )

########## order, equality ----

order <- function(x, ..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) UseMethod('order')

order.default <- function(x, ..., na.last = TRUE, 
                          decreasing = FALSE, method = c("auto", "shell", "radix")) {
    do.call(base::order, list(x, ..., na.last = na.last, decreasing = decreasing, method = method))
}



#' @name struct
#' @export
setMethod('sort', signature = c(x = 'struct'),
          function(x, decreasing = FALSE) {
              x[order(x, decreasing = decreasing), ]
          })

##### comparisons ----


#' @name struct
#' @export
setMethod('==', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '==')
              
              match_size(e1 = e1, e2 = e2, margin = 1:2, toEnv = TRUE)
              
              slots1 <- getSlots(e1)
              slots2 <- getSlots(e2)
              mat <- Reduce(`&`, Map(`==`, slots1, slots2))
              mat %dim% e1
              
          })

#' @export
setMethod('!=', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '!=')
              
              !(e1 == e2)
          })

#' @name struct
#' @export
setMethod('<', signature = c('struct', 'struct'),
          function(e1, e2) {
              !(e1 >= e2 )
          })


#' @name struct
#' @export
setMethod('<=', signature = c('struct', 'struct'),
          function(e1, e2) {
              !(e1 > e2)
          })



#### arithmatic ----



setMethod('sum', signature = c('struct'),
          function(x, ..., na.rm = FALSE) {
              x <- c(list(x), ...)
              
              x <- lapply(x, function(humv) {
                  setSlots(humv) <- lapply(getSlots(humv, c('numeric', 'integer', 'logical')), sum, na.rm = na.rm)
                  humv@dim <- humv@colnames <- humv@rownames <- NULL
                  humv
              })
              
              x <- if (length(x) > 1) {
                  Reduce(`+`, x)
              } else {
                  x[[1]]
              }
              x@rownames <- 'sum'
              x
          })

#' @name struct
#' @export
setMethod('colSums', signature = c('struct'),
          function(x, na.rm = FALSE, drop = FALSE) {
              if (!hasdim(x)) x <- cbind(x)
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        as.integer(unname(c(colSums(slot %dim% x, na.rm = na.rm))))
                                    })
              rownames(x) <- NULL
              x@dim <- c(1L, ncol(x))
                                    
              x
          })

#' @name struct
#' @export
setMethod('rowSums', signature = c('struct'),
          function(x, na.rm = FALSE) {
              if (!hasdim(x)) x <- rbind(x)
              
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        as.integer(unname(c(rowSums(slot %dim% x, na.rm = na.rm))))
                                    })
              colnames(x) <- NULL
              x@dim <- c(nrow(x), 1L)
              x
          })

#' @name struct
#' @export
setMethod('cumsum', signature = c('struct'),
          function(x) {
              
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        if (hasdim(x)) {
                                            c(apply(slot %dim% x, 2, cumsum))
                                        } else {
                                            cumsum(slot)
                                        }

                                    })
              
              x
          })



#' @name struct
#' @export
setMethod('diff', signature = c('struct'),
          function(x, lag = 1L) {
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    function(slot) {
                                        c(diff(slot %dim% x, lag = lag))
                                    })
              x@dim[1] <- x@dim[1] - 1L
              x@rownames <- x@rownames[-1]
              x
          })


#' @name struct
#' @export
setMethod('+', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '+')
              recycledim(e1 = e1, e2 = e2, funccall = '+')
              
              slots <- Map(`+`, getSlots(e1, c('numeric', 'integer', 'logical')), getSlots(e2, c('numeric', 'integer', 'logical')))
              setSlots(e1) <- slots
              e1
          })

#' @name struct
#' @export
setMethod('+', signature = c('struct', 'ANY'),
          function(e1, e2) {
              e1 + as(e2, class(e1))
          })

#' @name struct
#' @export
setMethod('+', signature = c('ANY', 'struct'),
          function(e1, e2) {
              as(e1, class(e2)) + e2
          })
#' @name struct
#' @export
setMethod('-', signature = c('struct', 'struct'),
          function(e1, e2) {
              e1 + (-e2)
          })

#' @name struct
#' @export
setMethod('-', signature = c( 'struct', 'missing'),
          function(e1) {
              
              setSlots(e1) <- lapply(getSlots(e1), function(slot) -slot )
              e1
          })


#' @name struct
#' @export
setMethod('-', signature = c( 'struct', 'struct'),
          function(e1, e2) {
              e1 + -e2
              
          })

#' @name struct
#' @export
setMethod('-', signature = c('struct', 'ANY'),
          function(e1, e2) {
              e1 - as(e2, class(e1))
          })

#' @name struct
#' @export
setMethod('-', signature = c('ANY', 'struct'),
          function(e1, e2) {
             as(e1, class(e2)) - e2
          })



#' @name struct
#' @export
setMethod('*', signature = c('struct', 'numeric'),
          function(e1, e2) {
              setSlots(e1) <- lapply(getSlots(e1, c('numeric', 'integer')), 
                                     function(x) {
                                       as(x * e2, class(x))
                                         
                                     })
              e1 
          })

#' @name struct
#' @export
setMethod('*', signature = c('numeric', 'struct'),
          function(e1, e2) {
              e2 * e1
          })






