
#' struct
#' 
#' Virtual class to help create atomic-vector-like composite data objects.
#' 
#' `humdrumR` defines a number of [S4 classes](http://adv-r.had.co.nz/S4.html) which are, underneath the surface, [composite data types](https://en.wikipedia.org/wiki/Composite_data_type)
#' made up of collections of [base-R atomic vectors][base::vector], stuck together.
#' The "vectorized" nature of R's atomic types is one of R's key strengths, so in `humdrumR` we try to A) mostly use the standard atomic types B) 
#' make all the new types we *do* define act as much like atomic vectors as possible.
#' `struct` is a *virtual* S4 class which serves this purpose: creating composite atomic vectors which act (mostly) like base-R atomic vectors.
#' 
#' As a "virtual class" `struct`s themselves don't really exist as independent objects, but the `struct` class defines (abstractly) all the necessarry methods to treat a collection of
#' atomic vectors as a single vector/matrix-like object---simply make your new subclass [inherit](https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming)) `struct` 
#' and it is all taken care of. (To do this, specify `contains = "struct"` in your call to [setClass][methods::setClass()].)
#' 
#' Important `humdrumR` classes which inherit from `struct` include:
#' + [tonal intervals][tonalIntervalS4]
#' + [diatonicSet]
#' + [tertianSet]
#' + [rational()]
#' 
#' Be warned, R's S4 object-system is limited in this regard: you can't really define S4 classes that act *fully* like R atomics, as
#' many of their features are hard-coded into R itself and can't be replicated.
#' The most important limitation of `struct` that you may encounter is that, though `struct` classes work (ok) in [data.frames][base::data.frame],
#' [data.tables][data.table::data.table] and [tibbles][tibble::tibble] will either not work or give strange behaviors if you put `struct`s into them.
#' 
#' @section Behavior:
#' 
#' `struct` subclasses (i.e., classes which inherit from `struct`) behave very similarly to normal [R atomic vectors/matrices][base::vector].
#' However, they do differ in a few respects, mostly in ways that are intended to avoid some of the quirky behaviors of R matrices:
#' In general, the distinction between dimensionless vectors and dimensioned vectors ([matrices][base::matrix()]) is slightly weaker in `structs` than with normal R atomic vectors/matrices.
#' Most importantly, dimensioned `struct`s won't drop their dimensions under various common operations ([c][base::c()], `[]`, etc.), the way base-R matrices do.
#' In general, it is easier to interact with a multi-column (matrix-)`struct` in the same way as a dimensionless (vector-)`struct`.
#' For example, if the struct has dimensions then `length(struct) == nrow(struct)`, instead of `length(matrix) == nrow(matrix) * ncol(matrix)`---i.e., the "height"
#'  of the `struct` (the number of rows) is its length.
#' Another big difference is in the behaviors of [c][base::c()]: `c` doesn't always cause `struct`s to lose their dimensions and `c` can be used to concatinated multi-column `struct`s,
#' or even mixes of dimensionless and dimensioned `struct`s:
#' If any `struct` arguments to `c` have dimensions, the `struct`s are concatinated via a call to [rbind][base::rbind()], with any dimensionless vectors coerced to 
#' 1-column matrices.
#' Of course, the (resulting) number of columns must all be the same or an error will occur!
#' 
#' Other differences:
#' * `struct`s can have either no dimensions (`dim(struct) == NULL`) or two dimensions. Higher dimensional `struct`s are not supported (yet).
#' * `rowSums` and `colSums` will coerce a dimensionless struct to a column matrix.
#' * `struct`s always throw an error if you try to index them with a index value that is greater than the length/nrow of the `struct`. 
#'    This is different than atomic vectors, which will pad the vector up to the length of the index you give---a sometimes useful but quirky behavior.
#' * `struct`s with two dimensions have a `cartesian` indexing argument.
#'    If `cartesian = TRUE`, the `i` and `j` arguments are treated as cartesian coordinates.
#'    (This behavior can be achieved with base R matrices (or `struct`s) by inputing a matrix with two columns.)
#' 
#' 
#' @section Requirements:
#' 
#' To work, `struct` makes a few assumptions about your class.
#' Your class must have one or more slots which are vectors, all of which are the same length.
#' `struct`'s indexing method will cause all of these vectors to be indexed as one.
#' When you define a new subclass of `struct`, it will inherit a 
#' [validObject][methods::validObject()] method which assures that all elements are the same dimension.
#' Thus, if you are writing your own `validObject` method (using [setValidity][methods::setValidity()])
#' you just have to worry specifically about the validity of the information in your slots,
#' not that the slots are all the same length.
#' 
#' 
#' @section Initialize:
#' 
#' An initialize method which automatically makes all slots the same length is predefined
#' for `structs`. If you want to make a more specialized [initialize][methods::initialize()] method,
#' you can still take advantage of the inherited method by using [callNextMethod][methods::callNextMethod()] at the 
#' beginning of your method.
#' 
#' 
#' @section Predefined methods:
#' 
#' The main purpose of the `struct` virtual class is that it defines many of the basic methods you need to manipulate subclass objects.
#' Most importantly, [indexing][base::Extract] methods are fully defined (that mimic base-R atomic vector/matrix indexing), as well as 
#' basic "structural" methods like [(col/row)names][base::colnames()], [dim][base::dim()], [length][base::length()], [ncol, nrow][base::ncol()], etc.
#' In addition:
#' 
# #' + If you specify a [order][base::order()] (must be an S3 method!) for you subclass, [sort][base::sort()] will automatically be defined. 
#' + If you define [> and >=][base::Comparison], `<` and `<=` will be automatically defined.
#' + If you define [as.character][base::as.character()] for your subclass, [show][methods::show()] and [format][base::format()] methods are automatically defined.
#' 
#' What's more, default arithmetic methods for addition, subtraction, (scalar-integer) multiplication, and negation (`-x`) are defined.
#' The default addition behavior is that each numeric ([base::integer] or [base::numeric]) slot from your subclasses will be added together.
#' Thus, `struct1 + struct2` will extract each numeric/integer slot from each `struct`, add them together and create a new `struct` from the result.
#' `-struct` will negate all numeric fields, and subtraction is simply defined as adding the negation.
#' Since *scalar* multiplication is defined, two `struct`s cannot be multiplied, but a struct can be multiplied by an integer (all numeric fields are multiplied by the integer(s)).
#' If these definitions don't work for your subclass, you'll need to create your own, more specific, method!
#' 
#' @slot dim Either `NULL` or a non-negative [integer-vector][base::integer] of `length == 2L`, representing the number of rows and columns respectively. Dimensions *can* be zero.
#' @slot rownames Either `NULL` or a [integer][base::integer]/[character][base::character]-vector which is the same length as either
#' A) if `dim == NULL`, the length of the `struct` B) if `dim != NULL`, the number of rows in the `struct`.
#' @slot colnames Either `NULL` (it *must* be `NULL` if `dim == NULL`) or a [integer][base::integer]/[character][base::character]-vector of length equal to the number of columns in the `struct`. 
#' 
#' @name struct
#' @seealso Examples of `struct` subclasses: [tonalInterval] [rhythmInterval] [diatonicSet] [tertianSet]
#' @examples 
#' setClass('mynewsubclass', contains = 'struct', slots = c(X= 'numeric', Y = 'numeric'))
#' 
#' test <- new('mynewsubclass', X = 1:10, Y = 10:1)
#' 
#' # all of these should work:
#' test[1:5]
#' rev(test)  == test
#' cbind(test, test)
#' c(test, test)
#' test * 3
#' test - test
#' 
#' 
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
                            all(sapply(slots, \(slot) is.vector(slot) | is.integer64(slot))) #&&  all(sapply(slots, is.atomic))
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
                            
                if (length(errors))  errors else TRUE
                
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
              
              slots <- lapply(slots, \(slot) {slot[na] <- NA; slot})
              
              setSlots(.Object) <- slots
              validObject(.Object)
              .Object
          } )

getSlots <- function(x, classes = c('numeric', 'integer', 'integer64', 'logical', 'character', 'list')) {
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
    rep(1:ncol, each = nrow(humvec))
}

vectorNA <- function(n, mode = 'character') {
  rep(as(NA_integer_, Class = mode), n)
}



########## shape ----


setMethod('nrow', signature = 'struct', function(x) x@dim[1])
setMethod('ncol', signature = 'struct', function(x) x@dim[2])
setMethod('length', signature = 'struct', function(x) length(getSlots(x)[[1]]))
setMethod('dim', signature = 'struct', function(x) x@dim )
setMethod('lengths', signature = 'struct', 
          function(x) {
             if (hasdim(x)) array(1, dim(x)) else rep(1L, length(x))
          })

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
        args <- setNames(match_size(args[[1]], args[[2]]), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }   
    
    drat <- d1 / d2
    if (drat[1] == 1 && (d1[2] == 1 || d2[2] == 1)) {
        args <- setNames(match_size(args[[1]], args[[2]]), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    if (drat[2] == 1&& (d1[1] == 1 || d2[1] == 1)) {
        args <- setNames(match_size(args[[1]], args[[2]]), names(args))
        list2env(args, envir = parent.frame(1))
        return(NULL)
    }
    
    .stop("In call to {funccall}, the two structs are nonconformable.\n",
          "To confirm, eitehr one object must be of dim = c(1,1), or 
          at least one of their dimensions needs to be the same, while the other is either (also) the same, or 1.")
    
}



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
              if (!is.null(value) && length(value) != nrow(x)) .stop(ifelse = is.null(x@dim),
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
    c(outer(i, (seq_len(ncol(x)) - 1) * nrow(x), '+'))
}
humvectorJ <- function(j, x) {
    columns <- columns(x)
    unlist(locate(j, columns), use.names = FALSE)
}

emptyslots <- function(x) {
    setSlots(x) <- lapply(getSlots(x), \(slot) vectorNA(0L, class(slot)))
    x
}

### [i, ] ----

setMethod('[', c(x = 'struct', i = 'numeric', j = 'missing'),
          function(x, i, drop = FALSE) {
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
            
            if (drop) dropdim(x) else x
          })

setMethod('[', c(x = 'struct', i = 'character', j = 'missing'),
          function(x, i, drop = FALSE) {
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
              x[i, drop = drop]
          })
setMethod('[', c(x = 'struct', i = 'logical', j = 'missing'),
          function(x, i, drop = FALSE) {
              if (length(i) != if(hasdim(x)) nrow(x) else length(x)) .stop(ifelse = !hasdim(x),
                                              "Can't index[i<|, >] a {class(x)} with a logical vector of a length that does not match <length|nrow>({class(x)}).")
              
            x[which(i), drop = FALSE]
          })

setMethod('[', c(x = 'struct', i = 'matrix', j = 'missing'),
          function(x, i) {
              if (!hasdim(x)) .stop("You can't index a dimensionless {class(x)} object with a matrix.")
              matclass <- class(i[1, 1])
              
              if (matclass %in% c('character', 'numeric', 'integer')) {
                  if (ncol(i) == 1L) return(x[c(i), ])
                  if (nrow(i) == 1L) return(x[ , c(i)])
                  if (ncol(i) == 2)  return(x[i[ , 1], i[ , 2], cartesian = TRUE])
                  
                  .stop("To index a {class(x)} a numeric or character matrix, that matrix must either:\n",
                        "\t1) Be a 1-column (index rows) or 1-row (index columns) matrix---in which case, it is treated like a vector.\n",
                        "\\2) Be a 2-column matrix, which will be treated as cartesian coordinates.\n",
                        "\t\t(If you use a character matrix, the {class(x)} object must have appropriate row/colnames defined!)")
              }
              if (matclass != 'logical') stop(call. = FALSE, glue::glue("Can't index a struct with a {matclass} matrix."))
              
              if (ncol(i) == 1L && length(i) == nrow(x)) return(x[which(i), ])
              # if (nrow(i) == 1L && length(i) == ncol(x)) return(x[, which(i)])
              
              
              if (!identical(dim(i), dim(x))) stop(call. = FALSE, "Can't index struct[i , ] with a logical matrix, unless it either has the exact same",
                                                   " dimensions as the struct, or matches one dimension while the other dimension == 1, ",
                                                   "i.e., if the indexing matrix is a single column with the same number of rows as the indexed {class(x)} object OR the indexing matrix ",
                                                   "has is a single row with the same number of columns as the indexed {class(x)} object.")
              
              ij <- which(i, arr.ind = TRUE)
              
              x[ij[ , 'row'], ij [, 'col'], cartesian = TRUE]
              
          })



### [ , j] ----

setMethod('[', c(x = 'struct', i = 'missing', j = 'numeric'),
          function(x, j, drop = FALSE) {
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
              
              if (drop) dropdim(x) else x
          })



setMethod('[', c(x = 'struct', i = 'missing', j = 'character'),
          function(x, j, drop = FALSE) {
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
              x[ , j, drop = drop]
          
          })


setMethod('[', c(x = 'struct', i = 'missing', j = 'logical'),
          function(x, j, drop = FALSE) {
              if (length(j) != ncol(x)) .stop("Can't index a {class(x)} (i.e., {class(x)}[ , j]) with a logical vector that is a different length than ncol({class(x)}).")
              x[ , which(j), drop = drop]
          })

setMethod('[', c(x = 'struct', i = 'missing', j = 'matrix'),
          function(x, j ) {
              .stop("You can't index a {class(x)} with a matrix in the j indexing argument.")
          })

### [i, j]


setMethod('[', c(x = 'struct'),
          function(x, i, j, cartesian = FALSE, drop = FALSE) {
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
             
                  
              } else {
                 x <- x[i,  ]
                 x <- x[ , j, drop = drop]
              }
              
              x
          })

#### [i, j] <- value ----

setMethod('[<-', c(x = 'struct', i = 'ANY', j = 'missing', value = 'struct'),
          function(x, i, value) {
              checkSame(x, value, '[i , ]<-')
              if (length(value) == 0 || any(dim(value) == 0L) || (is.logical(i) && !any(i))) return(x)
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
                  value <- .rep(value, length.out = ncol(x), margin = 2)
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
              slots <- Map(\(slotx, slotv) {
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
                  value <- .rep(value, length.out = nrow(x), margin = 1)
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
              slots <- Map(\(slotx, slotv) {
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
              slots <- Map(\(slotx, slotv) {
                  slotx[ij.internal] <- slotv
                  slotx
              }, slotsx, slotsv)
              
              setSlots(x) <- slots
              x
          })

setMethod('[<-', c(x = 'struct', i = 'matrix'),
          function(x, i, value) {
              
            xdim <- dim(x)
            idim <- dim(i)
            
            if (!identical(xdim, idim)) .stop("Can't assign to a strcut using a matrix index which doesn't have the same dimensions as the struct.") 
             
            xflat <- x %<-matchdim% NULL
            iflat <- i %<-matchdim% NULL
            xflat[iflat] <- value
            
            xflat %<-matchdim% x
            
              
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
    slots <- Map(\(slotx, slotv) {
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
                         \(slot) {
                             unlist(tapply(slot, columns, rep, ..., simplify = FALSE), use.names = FALSE)
                         })
              }
              
              setSlots(x) <- slots
              x@dim[1]   %!<-% as.integer(length(slots[[1]]) / (ncol(x) %||% 1))
              x@rownames %!<-% rep(rownames(x), ...)
              
              x
          })


setMethod('rep_len', c(x = 'struct'),
          function(x, length.out) {
            slots <- getSlots(x)
            
            slots <- if (!hasdim(x)) {
              lapply(slots, rep_len, length.out = length.out)
            } else {
              columns <- columns(x)
              lapply(slots,
                     \(slot) {
                       unlist(tapply(slot, columns, rep_len, length.out = length.out, simplify = FALSE), use.names = FALSE)
                     })
            }
            
            setSlots(x) <- slots
            x@dim[1]   %!<-% as.integer(length(slots[[1]]) / (ncol(x) %||% 1))
            x@rownames %!<-% rep_len(rownames(x), length.out = length.out)
            
            x
          })

setMethod('rev', c(x = 'struct'),
          function(x) {
              x[length(x):1]
          })


#' @export
setMethod('c', 'struct',
          function(x, ...) {
              xs <- list(x, ...)
              xs <- lapply(xs, dropdim)
              
              classes <- lapply(xs, class)
              if (length(unique(classes)) > 1L) {
                outclass <- classes[[1]]
                sameclass <- sapply(classes, \(class) class == outclass)
                
                xs[!sameclass] <- lapply(xs[!sameclass],
                                         function(y) {
                                           y <- try(as(y, outclass))
                                           
                                           if (class(y) == 'try-error') {
                                             .stop(ifelse = length(class(y)) > 1L,
                                                   "You can't concatinate a {class(x)} struct object with objects of class<|es>",
                                                   harvard(class(y), 'or', quote = FALSE), '.')
                                           }
                                           
                                           y
                                           
                                         })
                       
                
                
              }
              
           
              # do it
              
              xslots <- lapply(xs, getSlots) 
              slots <- Reduce(\(cur, rest) Map(c, cur, rest), xslots)
              setSlots(x) <- slots
              
              dropdim(x)
              
          })

#' @export
unique.struct <- function(x) {
  x[!duplicated(x)]
}

#' @export
rbind.struct <- function(...) {
    xs <- list(...)
    
    xs <- lapply(xs, \(x) if (hasdim(x)) t(x) else x) # keep vectors as vectors
    
    t(do.call('cbind.struct', xs))
}

#' @export
cbind.struct <-  function(...) {
    xs <- list(...)
    xs <- Filter(Negate(is.null), xs)
    
    ldims <- ldims(xs)
    
    nrow <- unique(sapply(xs, \(x) if (hasdim(x)) nrow(x) else length(x)))
    if (length(nrow) > 1L) .stop("Can't cbind matrices/vectors of mismatching dimensions.")
    
    xs[!sapply(xs, hasdim)] <-lapply(xs[!sapply(xs, hasdim)], \(x) {x %<-dim% c(nrow, 1L)})
    
    x <- do.call('c', xs)
    
    dim(x) <- c(nrow, sum(sapply(xs, ncol)))
    
    # dimnames
    colnames <- c(sapply(xs, \(x) colnames(x) %||% rep("", ncol(x))))
    colnames(x) <- if (!all(colnames == "")) colnames
      
    rownames(x) <- do.call('rbind', lapply(xs, rownames))[1, ]
    
    x
}


#' @export
setMethod('t', signature = 'struct',
          function(x) {
              if (!hasdim(x)) x@dim <- c(length(x), 1L)
              
              setSlots(x) <- lapply(getSlots(x),
                     function(slot) {
                         slot <- slot %<-matchdim% x
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


#' @export
setMethod('is.na', signature = 'struct',
          function(x) {
              na <- is.na(getSlots(x)[[1]])
              na %<-matchdim% x
          })


#' @name struct
#' @export
is.struct <- function(x) inherits(x, 'struct')

#' @export
setMethod('is.vector', signature = 'struct', function(x) TRUE)


#' @export
setMethod('as.vector', signature = 'struct', force)

#' @export
setMethod('as.list', signature = c('struct'),
          function(x, ...) {
              
              x <- list(x, ...)
              x <- do.call('c', x)
              
              lapply(seq_along(x), \(i) x[i])
          })

#' @export
setGeneric('as.atomic', function(x, ...) standardGeneric('as.atomic'))
setMethod('as.atomic', 'struct',
          function(x,  collapse = \(x, y) .paste(x, y, sep = ',')) {
              slots <- getSlots(x)
              atom <- Reduce(collapse, slots)
              
              atom[is.na(slots[[1]])] <- NA
              dim(atom) <- dim(x)
              colnames(atom) <- x@colnames
              if (hasdim(atom)) rownames(atom) <- x@rownames else names(atom) <- x@rownames
              atom
          })

setMethod('matrix', 'struct',
          function(data, nrow, ncol, byrow = FALSE) {
            data %<-dim% c(nrow, ncol)
          })


#' @export
as.matrix.struct <- function(x, ..., collapse = \(x, y) .paste(x, y, sep = ',', na.rm = TRUE)) {

    mat <- as.atomic(x, collapse = collapse)
    
    if (!hasdim(mat)) dim(mat) <- c(length(x), 1L)
    colnames(mat) <- x@colnames

    mat
}

#' @export
setAs('struct', 'data.frame', function(from) as.data.frame(from))


#' @export
setMethod('as.data.frame', 'struct',
          function(x, optional = FALSE, ...) {
              row.names <- x@rownames %||% 1:length(x)
              value <- list(x)
              attr(value, 'row.names') <- row.names
              if (!optional) attr(value, 'names') <- class(x)[1]
              class(value) <- c('data.frame')
              value
          })

.unlist <- function(x, recursive = TRUE, use.names = TRUE) {
  if (!is.list(x)) return(x)
  if (is.struct(x[[1]])) do.call('c', unname(x)) else unlist(x, recursive, use.names)
  
}
.data.frame <- function(...) {
    structs <- list(...)
    
    names <- names(structs)
    if (is.null(names)) names <- character(length(structs))
    names <- ifelse(names == "", sapply(structs, class), names)
    
    df <- do.call('cbind', lapply(structs, as.data.frame))
    colnames(df) <- names
    
    df
    
}

list2dt <- function(l) {
    # This takes a list and makes it into a data.table, spreading struct slots onto separate columns if necessarry.
    
    l <- l[!sapply(l, is.list)]
    if (length(l) == 0) return(data.table())
    
    structs <- sapply(l, is.struct)
    
    l[structs] <- lapply(l[structs],
                            \(struct) as.data.table(getSlots(struct)))
    
    as.data.table(l)
}


#' @export
format.struct <- function(x, ...) { as.character(x)}


#' @export
setMethod('as.character', 'struct',
          function(x) {
              x <- as.atomic(x)
              x[] <- as.character(x)
              x
          })
#' @export
setMethod('show', signature = c(object = 'struct'), 
          function(object) { 
            cat(class(object), 
                if (hasdim(object)) paste0('[', nrow(object), ' , ', ncol(object), ']'),
                '\n', sep = '')
            
            if (length(object) > 0L) {
              toprint <- c(object)
              toprint <-  ifelse(is.na(toprint), 'NA', as.character(toprint))
              dim(toprint) <- dim(object)
            
              print(toprint, quote = FALSE)
              
            }
            
            invisible(object)
              
            
          }  )

########## order, equality ----


#' @export
setMethod('sort', signature = c(x = 'struct'),
          function(x, decreasing = FALSE) {
              x[order(x, decreasing = decreasing)]
          })

##### comparisons ----


#' @export
setMethod('==', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '==')
            
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              slots1 <- getSlots(e1)
              slots2 <- getSlots(e2)
              
              mat <- Reduce(`&`, Map(`==`, slots1, slots2))
              mat %<-matchdim% e1
              
          })

#' @export
setMethod('!=', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '!=')
              
              !(e1 == e2)
          })

#' @export
setMethod('<', signature = c('struct', 'struct'),
          function(e1, e2) {
              !(e1 >= e2 )
          })


#' @export
setMethod('<=', signature = c('struct', 'struct'),
          function(e1, e2) {
              !(e1 > e2)
          })

#' @export
setMethod('Compare', signature = c('struct', 'matrix'),
          function(e1, e2) {
            v1 <- rep(e1, length(e2)) 
            v2 <- e2 %<-matchdim% NULL
            
            (v1 == v2) %<-matchdim% e2
            
          })

#' @export
setMethod('Compare', signature = c('matrix', 'struct'),
          function(e1, e2) {
            v2 <- rep(e2, length(e1)) 
            v1 <- e1 %<-matchdim% NULL
            
            (v1 == v2) %<-matchdim% e1
            
          })


#' @export
duplicated.struct <- function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA) {

    duplicated(as.data.frame(lapply(getSlots(x), I)), incomparables = incomparables, fromLast, nmax)
}

#### arithmatic ----



setMethod('sum', signature = c('struct'),
          function(x, ..., na.rm = FALSE) {
              x <- c(list(x), ...)
              
              x <- lapply(x, \(humv) {
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

#' @export
setMethod('colSums', signature = c('struct'),
          function(x, na.rm = FALSE, drop = FALSE) {
              if (!hasdim(x)) x <- cbind(x)
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    \(slot) {
                                        as.integer(unname(c(colSums(slot %<-matchdim% x, na.rm = na.rm))))
                                    })
              rownames(x) <- NULL
              x@dim <- c(1L, ncol(x))
                                    
              x
          })

#' @export
setMethod('rowSums', signature = c('struct'),
          function(x, na.rm = FALSE) {
              if (!hasdim(x)) x <- rbind(x)
              
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    \(slot) {
                                        as.integer(unname(c(rowSums(slot %<-matchdim% x, na.rm = na.rm))))
                                    })
              colnames(x) <- NULL
              x@dim <- c(nrow(x), 1L)
              x
          })

#' @export
setMethod('cumsum', signature = c('struct'),
          function(x) {
              
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    \(slot) {
                                        if (hasdim(x)) {
                                            c(apply(slot %<-matchdim% x, 2, cumsum))
                                        } else {
                                            cumsum(slot)
                                        }

                                    })
              
              x
          })



#' @export
setMethod('diff', signature = c('struct'),
          function(x, lag = 1L) {
              setSlots(x) <- lapply(getSlots(x, c('numeric', 'integer', 'logical')),
                                    \(slot) {
                                        c(diff(slot %<-matchdim% x, lag = lag))
                                    })
              x@dim[1] <- x@dim[1] - 1L
              x@rownames <- x@rownames[-1]
              x
          })


setMethod('+', signature = c('struct', 'struct'),
          function(e1, e2) {
              checkSame(e1, e2, '+')
              # recycledim(e1 = e1, e2 = e2, funccall = '+')
              match_size(e1 = e1, e2 = e2)
              
              slots <- Map(`+`, getSlots(e1, c('numeric', 'integer', 'logical')), getSlots(e2, c('numeric', 'integer', 'logical')))
              setSlots(e1) <- slots
              e1
          })

setMethod('+', signature = c('struct', 'ANY'),
          function(e1, e2) {
              e1 + as(e2, class(e1))
          })

setMethod('+', signature = c('ANY', 'struct'),
          function(e1, e2) {
              as(e1, class(e2)) + e2
          })

setMethod('-', signature = c('struct', 'struct'),
          function(e1, e2) {
              e1 + (-e2)
          })

setMethod('-', signature = c( 'struct', 'missing'),
          function(e1, e2) {
              
              setSlots(e1) <- lapply(getSlots(e1), \(slot) -slot )
              e1
          })


setMethod('-', signature = c( 'struct', 'struct'),
          function(e1, e2) {
              e1 + -e2
              
          })

setMethod('-', signature = c('struct', 'ANY'),
          function(e1, e2) {
              e1 - as(e2, class(e1))
          })

setMethod('-', signature = c('ANY', 'struct'),
          function(e1, e2) {
             as(e1, class(e2)) - e2
          })



setMethod('*', signature = c('struct', 'numeric'),
          function(e1, e2) {
              setSlots(e1) <- lapply(getSlots(e1, c('numeric', 'integer')), 
                                     \(x) {
                                       as(x * e2, class(x))
                                         
                                     })
              e1 
          })

setMethod('*', signature = c('numeric', 'struct'),
          function(e1, e2) {
              e2 * e1
          })






