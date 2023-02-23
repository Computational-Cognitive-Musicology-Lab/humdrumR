# Null and NA values ----


doubleswitch <- function(pred1, pred2, ...) {
    if (length(pred1) > 1 || length(pred2) > 1) .stop('doubleswitch predicates must be of length 1.')
    exprs <- rlang::exprs(...)
    
    if (any(.names(exprs) == '')) .stop('doubleswitch requires all named arguments.')
    
    argnames <- .names(exprs)
    
    
    order <- c('neither', 'either', 'both', 'xor', 'notxor', 'first', 'second', 'notfirst', 'notsecond')
    preds <- c(!pred1 && !pred2,
               pred1 || pred2,
               pred1 && pred2,
               xor(pred1,pred2),
               !xor(pred1, pred2),
               pred1,
               pred2,
               !pred1,
               !pred2)
    
    hits <- order %in% argnames
    hit <- which(preds[hits])[1]
    
    eval(exprs[[hit]], envir = parent.frame())
    
    
}





truthy <- function(x) !is.null(x) && length(x) > 0L && ((!is.logical(x) && !(length(x) == 1 && x[1] == 0)) || (length(x) == 1L & x[1]))
true <- function(x) is.logical(x) && x[1]
false <- function(x) is.null(x) || is.logical(x) && !x[1]
# these two functions allow us to test if a variable is 
# a logical TRUE or FALSE, but not give an error if
# the variable is NOT logical.
# (If the variable is NOT logical) it always returns FALSE






###

`%class%` <- function(object, newclass){
  if (!newclass %in% class(object)) class(object) <- append(newclass, class(object))
  object
}

popclass <- function(object) `class<-`(object, class(object)[-1])

fargs <- function(func) formals(args(func))

`%!<-%` <- function(e1, e2) {
    # this assigns e2 to e1, UNLESS e1 is NULL
    x <- rlang::enexpr(e1)
    
    if (is.null(e1)) return(invisible(NULL))
    
    value <- rlang::enexpr(e2)
    
    eval(rlang::expr(!!x <- !!value), envir = parent.frame())
    
}


`%<-%` <- function(names, values) {
    names <- as.character(rlang::enexpr(names))[-1]
    if (length(names) > 0L && length(names) != length(values)) stop(call. = FALSE,
                                              "Left side of multiassign (%<-%) operator must be the same length as the right side.")
    
    if (length(names) > 0L) names(values) <- names
    
    if (is.null(names(values)) || all(names(values) == "")) stop(call. = FALSE,
                                                                 "In use of multiassign operator (%<-%), no names have been provided.")
    list2env(as.list(values), envir = parent.frame())
    return(invisible(values))
}
`%->%` <- function(values, names) {
    names <- as.character(rlang::enexpr(names))[-1]
    if (length(names) > 0L && length(names) != length(values)) stop(call. = FALSE,
                                                                    "Left side of multiassign (%<-%) operator must be the same length as the right side.")
    
    if (length(names) > 0L) names(values) <- names
    
    if (is.null(names(values)) || all(names(values) == "")) stop(call. = FALSE,
                                                                 "In use of multiassign operator (%<-%), no names have been provided.")
    list2env(as.list(values), envir = parent.frame())
    return(invisible(values))
}



# Names ----

.names <- function(x) { #:: a -> character
    # gets names with no possibility of NULL
    # is there are no names, returns vector of empty strings
    nam <- names(x)
    
    if(is.null(nam)) nam <- character(length(x))
    
    nam
}

allnamed <- function(x) { !is.null(names(x)) && !any(names(x) == '')}

# Arrays/Vectors ----

valind <- function(vec) {
  
  values <- unique(vec)
  
  group <- if (!is.atomic(values)) {
    as.character(vec)
  } else {
    vec
  }
  list(indices = match(group, unique(group)), values = values)
  
}

inverse.valind <- function(valind) valind$values[valind$i]
  
.apply <- function(x, margin = 1, f, ...){
    result <- apply(x, margin, f, ..., simplify = FALSE)
    result[lengths(result) == 0L] <- list(NA)
    
    result <- do.call(if (margin == 1) 'rbind' else 'cbind', result)
    
    if (all(dim(result) == dim(x))) {
      dimnames(result) <- dimnames(x)
    } else {
      if (margin == 1)  rownames(result) <- rownames(x) else colnames(result) <- colnames(x)
      
    }
    result
}
applyrows <- function(x, f, ...) .apply(x, 1, f, ...)
applycols <- function(x, f, ...) .apply(x, 2, f, ...)


#' Shift data within a vector/matrix/data.frame
#' 
#' The `lag` and `lead` functions take input vectors, matrices, or data.frames and 
#' shifts their data
#' by `n` indices. 
#' They are similar to the [data.table::shift()] function, but with a few additional options.
#' 
#' @details 
#' 
#' A lagged vector has the same values as the original vector, except offset by `n` indices.
#' `lag` moves each value to a high index (if `n > 0`); `lead` does the opposite,
#' moving each value to a lower index (if `n > 0`).
#' `n` can be positive or negative---negative lags are equivalent to leads, and vice versa.
#' Values near the end/beginning are either "wrapped" to the opposite end of the
#' vector, or replaced/padded with the value of the `fill` argument.
#'
#' The vector `r letters[1:7]` can be lagged by `n==1` is `r lag(letters[1:7])`.
#' If we set `wrap == TRUE`, the `"g"` moved to the beginning of the output: 
#' is `r lag(letters[1:7], wrap = TRUE)`.
#' 
#' 
#' @param x The input argument. Should be a vector (including lists), `matrix`, or `data.frame`.
#' @param n The amount to lag/lead the data.  If `n == 0`, `x` is returned unchanged.
#' @param fill If `wrap = FALSE` parts of the output are padded with the `fill` argument. 
#' Defaults to `NA`.
#' @param wrap If `wrap = TRUE`, data from the end (head or tail) is copied to the
#'  other end of the output, "wrapping" the data
#' within the data structure.
#' @param groupby A vector or list of vectors, all of the same length as `x`. Each segment of `x` delineated
#' by the `groupby` vector(s) is treated separately.
#' @param margin Arrays and data.frames can be lagged lead in multiple dimensions 
#' using the `margin` argument: `margin == 1` shifts across rows while `margin == 2`
#' shifts across columns.
#' 
#' @family {Lagged vector functions}
#' @inheritSection sigma Grouping
#' @seealso [data.table::shift()]
#' @export
lag <- function(x, n = 1, fill, wrap, groupby, ...) UseMethod('lag')

#' @rdname lag
#' @export
lead <- function(x, n = 1, ...) lag(x, -n, ...)


#' @export
lag.data.frame <- function(x, n = 1, margin = 1, fill = NA, wrap = FALSE, groupby = list(), orderby = list()) {
         if (length(n) < length(margin)) n <- rep(n, length(margin))
        
         if (1L %in% margin) {
             x[] <- lapply(x, lag, n = n[margin == 1L], fill = fill, wrap = wrap, groupby = groupby)
             rown <- lag(rownames(x), n[margin == 1L], wrap = wrap, pad = '_', groupby = groupby)
             rown[rown == '_'] <- make.unique(rown[rown == '_'])
             rownames(x) <- rown
         } 
         if (2L %in% margin) {
             cols <- lag(colnames(x), n[margin == 2L], fill = 'XXX', wrap = wrap)
             x[] <-if (wrap) x[cols] else c(list(XXX = rep(NA, nrow(x))),x)[cols]
         }
    
         x
}
#' @export
lag.default <- function(x, n = 1, fill = NA, wrap = FALSE, groupby = list(), orderby = list()) {
          checks(n, xwholenum & xlen1)
          checks(fill, xatomic & xlen1)
          checks(wrap, xTF)
  
          if (length(x) == 0L || n == 0) return(x)
          
          groupby <- checkWindows(x, groupby)
          orderby <- checkWindows(x, orderby)
          
          groupby <- reorder(groupby, orderby = orderby, toEnv = FALSE)
          reorder(list(x = x), orderby = orderby)
          
          if (wrap && n >= length(x))  n <- sign(n) * (abs(n) %% size) #if rotation is greater than size, or negative, modulo
          
          output <- data.table::shift(x, n, type = 'lag', fill = fill)
            
            
          if (wrap) {
            if (n > 0) {
                output[1:n] <- tail(x, n)
            } else{
                output[(length(output) - n):length(output)] <- head(x, abs(n))
            }
          }
            
          groupby <- checkWindows(x, groupby)
          
          if (length(groupby)) {
              
              groupby <- Reduce('|', lapply(groupby, \(w) w != lag(w, n = n, fill = NA, wrap = FALSE)))
              output[groupby] <- fill
          }

          reorder(output)
}

#' @export
lag.matrix <- function(x, n = 1, margin = 1, fill = NA, wrap = FALSE, groupby = list()) {
    if (length(n) > 1L && length(n) != length(margin)) .stop('rotation and margin args must be the same length.')
    
    
          if ( length(margin) > 1L) {
                    rest.mar <- margin[-1]
                    margin   <- margin[1]

                    rest.rot <- if (length(n) > 1L) n[-1] else n

                    on.exit(return(Recall(output, n = rest.rot, margin = rest.mar, wrap = wrap, fill = fill, groupby = groupby())))
          }
         if (is.na(dim(x)[margin])) .stop("This matrix can not be rotated in dimension", margin, "because it doesn't have that dimension!" )
         if (dim(x)[margin] == 0L) return(x)
        
          n <- n[1]

          size <- dim(x)[margin]
          n <- sign(n) * (abs(n) %% size) #if rotation is greater than size, or negative, modulo
          if (n == 0L) {
              return(if (wrap) x else matrix(vectorNA(length(x), class(x[1])), ncol = ncol(x), nrow = nrow(x)))
          } 

          ind <- seq_len(size) - n

          if (wrap) ind <- ((ind - 1L) %% size) + 1L else ind[ind > size | ind < 1] <- NA

          calls <- alist(x, i = , j = )
          calls[[margin + 1]] <- ind

          output <- do.call('[', calls)

          if (!is.na(fill)) {
                    calls[[margin + 1]] <- which(is.na(ind))
                    calls$value <- fill
                    calls[[1]] <- output
                    output <- do.call('[<-', calls)

          }

          output
}

reorder <- function(xs, orderby = list(), toEnv = TRUE) {
  
  xs <- checkWindows(xs[[1]], xs)
  if (length(xs) == 0L & !toEnv) return(xs)
  orderby <- checkWindows(xs[[1]], orderby)
  
  if (length(orderby)) {
    ord <- do.call('order', orderby)
    
    xs <- lapply(xs, '[', i = ord)
    reorder <- \(X) X[match(seq_along(X), ord)]
  } else {
    reorder <- force
  } 
  
  if (toEnv) {
    xs$reorder <- reorder
    list2env(xs[.names(xs) != ''], envir = parent.frame()) 
  }
    

  xs
} 
  
   

## Matrices ----



most <- function(mat, whatmost = 'right', which = FALSE) {
  # returns the column which is the rightmost, leftmost, topmost, or bottommost, TRUE in each row/col
  
  kind <- pmatch(whatmost, c('right', 'left', 'bottom', 'top'))
  if (!hasdim(mat)) {
    ind <- switch(kind,
           max(which(mat)),
           which(mat)[1],
           .stop("A dimensionless vector has no 'topmost'"),
           .stop("A dimensionless vector as no 'bottomost'"))  
    
    if (which) ind else seq_along(mat) == ind
    
  } else {
    if (kind > 2) mat <- t(mat)
    
    rows <- rowSums(mat) > 0
    ind <- ifelse(rows,
                  max.col(mat, ties.method = c('last', 'first', 'last', 'first')[kind]),
                  0)
    
    ind <- cbind(seq_along(rows), ind)
    
    if (kind > 2) ind <- ind[ , 2:1]
    
    colnames(ind) <- c('row', 'col')
    
    if (which) return(ind)
    
    output <- matrix(FALSE, ncol = ncol(mat), nrow = nrow(mat))
    output[ind] <- TRUE
    
    output
    
  }
  
}

rightmost <- function(mat, which = FALSE) most(mat, 'right', which = which)
leftmost  <- function(mat, which = FALSE) most(mat, 'left', which = which)
topmost <- function(mat, which = FALSE) most(mat, 'top', which = which)
bottommost <- function(mat, which = FALSE) most(mat, 'bottom', which = which)

## Vectors ----





### Other ----



allsame <- function(x) length(unique(x)) == 1L

hasdim <- function(x) !is.null(dim(x))

list.flatten <- function(list) {
  output <- list()
  
  rapply(list, \(x) output <<- c(output, list(x)))
  output
}

empty <- function(object, len = length(object), dimen = dim(object), value = NA) {
    if (is.atomic(object)) {
        return(if (is.null(dimen)) rep(as(value, class(object)), len) else array(as(value, class(c(object))), dim = dimen))
    }
    
    if (inherits(object, 'struct')) {
        struct <- new(class(object))
        slots <- getSlots(struct)
        if (!is.null(dimen)) len <- prod(dimen)
        setSlots(struct) <- lapply(slots, \(slot) rep(as(value, class(slot)), len))
        
        struct %<-matchdim% object
        
    }
    
    
} 

catlists <- function(lists) {
    # this is just like do.call('c', lists) except it never returns NULL
    # and always returns a list.
    # if the lists are all empty, it returns an empty list
    if (any(lengths(lists) > 0)) browser()
    out <- do.call('c', lists)
    if(is.null(out)) out <- list() 
    if (!is.list(out)) out <- list(out)
    out
}

# indices

matches <- function(x, table, ..., multi = FALSE) {
  # x and table are both lists/data.frames
  
  x <- do.call('paste', c(x, list(sep = ' ')))
  table <- do.call('paste', c(table, list(sep = ' ')))
  
  if (multi) multimatch(x, table, ...) else match(x, table, ...)
  
}

multimatch <- function(x, table, ...) {
  ns <- tapply_inplace(table, table, seq_along)
  
  tables <- lapply(unique(ns), \(n) { 
    table[ns < n] <- NA
    table
    })
  do.call('cbind', lapply(tables, \(tab) match(x, tab, ...)))
}

`%ins%` <- function(x, table) !is.na(matches(x, table))

`%pin%` <- function(x, table) pmatch(x, table, nomatch = 0L, duplicates.ok = TRUE) > 0L

closest <- function(x, where, direction = 'either', diff_func = `-`) {
          direction <- pmatch(direction, c('either', 'below', 'above', 'lessthan', 'morethan'))
          
          
          sortedwhere <- sort(where)
          intervals <- findInterval(x, sortedwhere)
          hits <- ifelse(intervals == 0,
                         if (direction %in% c(2,4)) Inf else 1,
                         if (direction == 1) {
                                   intervals + mapply(FUN = \(a,b) which.min(c(a,b)) - 1,
                                                      abs(x - sortedwhere[intervals]),
                                                      abs(x - sortedwhere[intervals + 1]))
                         } else {
                                   if (direction %in% c(3, 5))  intervals + 1  else intervals
                         })
          sortedwhere[hits]
          
}




remove.duplicates <- function(listofvalues) {
    # takes a list of vectors of values and elements from later vectors which
    # appear in earlier vectors
    if (sum(lengths(listofvalues)) == 0L) return(listofvalues)
    
    groups <- factor(rep(seq_along(listofvalues), lengths(listofvalues)), 
                     levels = seq_along(listofvalues)) # must specificy levels again because there may be empty vectors

    values <- unlist(listofvalues, use.names = FALSE)

    dups <- duplicated(values)
    setNames(tapply(values[!dups], groups[!dups], c, simplify = FALSE), names(listofvalues))
    
}


.cbind <- function(...) {
    #cbind except skips NULL
    
    x <- list(...)
    x <- x[lengths(x) > 0]
    
    do.call('cbind', x)
    
}

tapply_inplace <- function(X, INDEX, FUN = NULL, ..., head = TRUE) {
    attr <- humdrumRattr(X)
    result <- tapply(X, INDEX, FUN, ..., simplify = FALSE) %<-dim% NULL
    
    headortail <- if (head) match.fun('head') else tail
    indices <- Map(\(x, n) headortail(x, n), tapply(seq_along(X), INDEX, force), lengths(result))
    
    result <- do.call('c', result)
    indices <- do.call('c', indices)
    
    result <- if (length(result) > 0L && length(result) == length(X)) {
      result[order(indices)]
    } else {
      output <- vectorNA(length(X), class(result))
      # output[is.na(output)] <- as(0, class(output))
      output[indices] <- result
      output
    }
    
    humdrumRattr(result) <- attr
    
    result
    
}

#' Identify contiguous segments of data in a vector
#' 
#' `segments` and `changes` are extremely useful functions for finding 
#' contiguous "segments" indicated in a vector.
#' 
#' @section Changes:
#' 
#' `changes` takes and input vector and finds all indices `i`
#' where the value of `x[i] != x[i-1]`---i.e., where the value at one index
#' has "changed" since the last index.
#' By default, `changes` returns a `logical` vector the same length as the input,
#' with `TRUE` only at indices where a change occured.
#' The `first` argument indicates whether the first index (`i == 1`)
#' is marked `TRUE`.
#' 
#' `changes` can accept more than one input vector.
#' If the `any` argument is set to `TRUE` (the default),
#' a change in *any* input is marked as a change (`TRUE`) in the output.
#' If `any == FALSE`, changes must happen in *all* vectors to be marked in the output.
#' 
#' Finally, the `reverse` argument reverses the behavior of `changes`,
#' checkig instead if `x[i] != x[i + 1]`.
#' 
#' ### Values
#' 
#' By default, the values of the input vector(s) where a change occurs
#' are placed in a matrix and put in the `values` attribute of the `logical` output.
#' However, if the `value` argument is set to `TRUE`, the values themselves are returned.
#' 
#' @section Segments:
#' 
#' The `segments` builds off of the `changes` function.
#' The segments function takes a `logical` input and *cummulatively* tallies each
#' `TRUE` value in the vector, from left to right (or right to left, if `reverse == TRUE`).
#' Thus, the input `c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)`
#' would return `c(1, 1, 2, 2, 2, 3, 4, 4)`.
#' This creates contiguous blocks of values which can be used for a `groupby` argument in a call
#' to [within.humdrumR()], or similar functions like [base::tapply()].
#' 
#' Any input vector(s) to `segments` which are not `logical`, are first fed to 
#' `changes` to create a `logical` input.
#' 
#' @examples 
#' 
#' segments(letters %~% '[aeiou]')
#' 
#' changes(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), 
#'         c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'         any = TRUE)
#' # result is T,F,F,T,T,F,T,F,T,T,F,F
#' 
#' @param ... A list of atomic vectors. If the vectors differ in length,
#' they are all recycled to match the length of the longest vector.
#' @param  reverse (`logical`, `length == 1`) If `TRUE` the function is excecuted 
#'   backwards through the input vector(s).
#' @param first (`logical`, `length == 1`) Is the first index (or last index if `reverse == TRUE`)
#'   marked as a "change."
#' @param value (`logical`, `length == 1`) If `TRUE`, the input values where changes occur
#' are returned in a matrix, with each row matching a change and each column containing the
#' value from the associated input vector.
#' @param any (`logical`, `length == 1`) If `TRUE`, a change in *any* input vector
#' is marked a change. If `FALSE`, changes must occur in *all* input vectors to be marked as a change.
#' @family {Window functions}
#' @export
segments <- function(..., first = TRUE, any = TRUE, reverse = FALSE) {
  checks(any, xTF)
  checks(reverse, xTF)
  
  xs <- list(...)
  if (length(xs) == 0L) return(NULL)
  if (max(lengths(xs)) == 0L) return(logical(0))
  xs <- do.call('match_size', xs)
  
  logical <- sapply(xs, is.logical)
  
  changes <- xs
  changes[!logical] <- lapply(changes[!logical], changes, reverse = reverse, first = first)
  
  change <- Reduce(if (any) `|` else `&`, changes)
  if (first) change[if (reverse) length(change) else 1L] <- TRUE
  
  values <- if (any(!logical)) do.call('cbind', lapply(xs[!logical], '[', i = change))
  
  
  if (reverse) change <- rev(change)
    
  segments <- cumsum(change)
  
  segments[segments == 0L] <- NA_integer_
    
  if (reverse) segments <- rev(segments) 
    
  attr(segments, 'values') <- values
    
  segments
    
}

#' @export
#' @rdname segments
changes <- function(..., first = TRUE, value = FALSE, any = TRUE, reverse = FALSE) {
  checks(first, xTF)
  checks(value, xTF)
  checks(any, xTF)
  checks(reverse, xTF)
  
  xs <- list(...)
  if (length(xs) == 0L) return(NULL)
  if (max(lengths(xs)) == 0L) return(logical(0))
  xs <- do.call('match_size', xs)
  
  changes <- lapply(xs, \(x) c(if (!reverse) first, 
                               head(x, -1L) != tail(x, -1L),
                               if (reverse) first))
  changes <- Reduce(if (any) '|' else '&', changes)
  
  
  values <- do.call('cbind', lapply(xs, '[', !is.na(changes) & changes))
  rownames(values) <- which(changes)
  
  if (value) {
    values
  } else {
    attr(changes, 'values') <- values
    changes
  }
}





#' Propagate data points to "fill" null data.
#' 
#' `ditto` is a function that allow you to "fill" null values in a vector
#' with non-null values from earlier/later in the same vector.
#' The default, "forward," behavior fills each null value with the previous (lower index) non-null value, if there are any.
#' The `reverse` argument can be used to cause "backward" filling, where the *next* (higher index) non-null value is used.
#' If the input begins (or ends if `reverse == TRUE`) with a null value, the `initial` argument is filled instead; defaults to `NA`.
#' 
#' Which values are considered "null" can be controlled using the `null` argument.
#' The `null` argument can either be a logical vector which is the same length as the input (`x`) argument, a numeric
#' vector of positive indices, or a function which, when applied to `x` returns an appropriate logical/numeric vector.
#' The values of `x` where `null == FALSE` are copied forward/backwards to replace any adjacent vales where `null == TRUE`.
#' By default, `null` is the function `\(x) is.na(x) | x == '.'`, which means that `NA` values and the string `"."` are 
#' "null", and are overwritten by adjacent values.
#' 
#' `ditto` methods are defined for data.frames and matrices.
#' The `data.frame` method simply applies `ditto` to each column of the `data.frame` separately.
#' For matrices, ditto can be applied across columns (`margin == 2`), rows (`margin == 1`), or other dimensions.
#' 
#' The `ditto` method for a [humdrumR object][humdrumRclass] simply applies `ditto` to the, by default,
#' the active field; thus `ditto(humData)` is equivalent to `within(humData, newField <- ditto(.), dataTypes = 'Dd')`.
#' The `field` argument can be used to indicated a different field to apply to. The result of the dittoing
#' is saved to a new field---the `newField` argument can be used to control what to name the new field.
#' 
#' @param x A vector.
#' @param null Either a logical vector where (`length(x) == length(null)`), a numeric
#' vector of positive indices, or a function which, when applied to `x` returns an appropriate logical/numeric vector.
#' @param initial A value (`length == 1`) of the same class as `x`, used to pad the beginning (or end, if `reverse == TRUE`) of the output,
#' if necessary.
#' @param reverse (`logical` & `length == 1`) If `reverse == TRUE`, the "non-null" values are coped to overwrite null values
#' *earlier* (lower indices) in the vector. 
#' @param margin a vector giving the subscripts which the function will be applied over. 
#'     E.g., for a matrix `1` indicates rows, `2` indicates columns.
#'     Where `x` has named dimnames, it can be a character vector selecting dimension names.
#' @param field Which field ([partially matched][partialMatching]) in the `humdrumR` dataset should be dittoed?
#' @param newField (`character` of `length == 1`) What to name the new (dittoed) field.
#' 
#' @inheritParams lag
#' @inheritSection sigma Grouping
#' @inheritSection sigma Order
#' @family {Lagged vector functions}
#' @export
ditto <- function(x, ...) UseMethod('ditto')

#' @rdname ditto
#' @export
ditto.default <- function(x, null = \(x) is.na(x) | x == '.', initial = NA, reverse = FALSE, 
                          groupby = list(), orderby = list()) {

    checks(x, xatomic)
    checks(null, xclass('function') | (xlogical & xmatch(x)))
    checks(initial, xatomic & xlen1)
    checks(reverse, xTF)
    groupby <- checkWindows(x, groupby)
  
    if (length(x) == 0L) return(x)
    
    hits <- !(if (is.function(null)) null(x) else null)
    
    groupby <- checkWindows(x, reorder(groupby, orderby = orderby, toEnv = FALSE))
    reorder(list(x = x, hits = hits), orderby = orderby)
    
    
    groupby <- if (length(groupby)) {
      do.call('changes', c(groupby, list(reverse = reverse)))
    } else {
      seq_along(x) == if (reverse) length(x) else 1L
    }
    
    
    x[groupby & !hits] <- initial
    
    seg <- segments(hits | groupby, reverse = reverse)
    vals <- x[hits | groupby]
    
    
    output <- setNames(rep(vals, rle(seg)$lengths), seg)
    
    reorder(output)
}

#' @rdname ditto
#' @export
ditto.data.frame <- function(x, ...) {
  x[] <- lapply(x, ditto, ...)
  x
}

#' @rdname ditto
#' @export
ditto.matrix <- function(x, margin = 2, ...) {
  checks(margin, xwholenum & xlen1 & xmin(1) & xmax(2))
  result <- apply(x, margin, ditto, ..., simplify = FALSE)
  
  do.call(if (margin == 1) 'rbind' else 'cbind', result)
  
}

#' @rdname ditto
#' @export
ditto.humdrumR <- function(x, field = getActiveFields(x)[1], ..., newField = paste0(field, '_ditto')) {
  checks(newField, xcharacter & xlen1)
  field <- rlang::sym(fieldMatch(x, field, 'ditto.humdrumR', 'field'))
  newField <- rlang::sym(newField)
  rlang::eval_tidy(rlang::expr({

      within(x, !!newField <- ditto(!!field, ...), dataTypes = 'Dd')

  }))
}



## Dimensions ----

ldim <- function(x) {
  # ldim is like dim, but it always counts length,
  # regardless of dimensions
  # So all vector/matrices always have a ldim
  
    ldim <- if (hasdim(x)) c(0L, dim(x)) else c(length(x), 0L, 0L)
    ldim[4] <- if (ldim[1] == 0L) prod(ldim[-1]) else ldim[1]
    names(ldim) <- c('length', 'nrow', 'ncol', 'size')
    as.data.frame(rbind(ldim))
}

ldims <- function(xs) do.call('rbind', lapply(xs, ldim))

.dim <- function(x) if (hasdim(x)) dim(x) else length(x)

size <- function(x) ldim(x)$size

height <- function(x) {
  if ((!is.factor(x) && is.object(x)) || !(is.vector(x) || is.atomic(x) || is.list(x) || is.factor(x))) return(1L)
  if (hasdim(x)) nrow(x) else length(x)
}

`%<-matchdim%` <- function(x, value) {
    # set the dimensions of x to equal the dimensions of value
    # only works if x is actually the right size!
    # if (inherits(x, 'partition')) {
    #     x[] <- lapply(x, `%<-matchdim%`, value =value)
    #     return(x)
    # }
    # 
    if (is.null(value)) {dim(x) <- NULL; return(x)}
    
    if (size(x) != size(value)) .stop("%<-matchdim% is trying to match the dimensions of two objects, but the target object is not the right size.")
    
	dim(x) <- dim(value)
	if (hasdim(x)) {
	    rownames(x) <- rownames(value) 
	    colnames(x) <- colnames(value)
	} else {
	    names(x) <- names(value)
	} 
	
	x
}

`%<-dim%` <- function(x, value) {
  dim(x) <- value
  x
}

dropdim <- function(x) {
    if (is.atomic(x)) {
        c(x) 
    } else {
        x %<-dim% NULL
        
    }
    
}

forcedim <- function(ref, ..., toEnv = FALSE, byrow = FALSE) {
    # the same as %<-matchdim%, except it forces all the ... to be the same dim as ref (recycling if necessary)
    refdim <- ldim(ref)
    
    targets <- list(...)
    targets <- if (hasdim(ref)) {
        lapply(targets, 
               \(x) {
                   xdim <- ldim(x)
                   if (hasdim(x)) {
                       if (xdim$nrow != refdim$nrow) x <- .rep(x, length.out = refdim$nrow, margin = 1L)
                       if (xdim$ncol != refdim$ncol) x <- .rep(x, length.out = refdim$ncol, margin = 2L)
                       x
                   } else {
                       matrix(rep(x, length.out = refdim$size), refdim$nrow, refdim$ncol, byrow = byrow)
                   }})
    } else {
        lapply(targets, 
               \(x) {
                   if (hasdim(x)) x <- dropdim(x)
                   rep(x, length.out = refdim$length)
                   })
        
    }

    if (toEnv) {
        list2env(targets[.names(targets != '')], envir = parent.frame(1))
        invisible(targets)
    } else {
        targets
    }
    
    
    
}






## My versions of some standard utitilies

match_size <- function(..., recycle = TRUE, toEnv = FALSE) {
  
          x <- list(...)
          x <- x[!sapply(x, is.null)]
          
          ldims <- ldims(x)
          target <- order(ldims[ , 'size'], ldims[ , 'nrow'], decreasing = TRUE)[1]
          targetdim <- .dim(x[[target]])
          
          x[-target] <- lapply(x[-target], if (hasdim(x[[target]])) cbind else c)
          
          
          recycleF <- if (recycle) match.fun('recycle') else match.fun('stretch')
          x[-target] <- lapply(x[-target],
                         \(y) {
                           recycleF(y, targetdim)
                     })
          
          
  
          if (toEnv) {
            list2env(x[.names(x) != ''], envir = parent.frame(1))
            invisible(x)
          } else {
            x
          }
          
}

recycle <- function(x, length.out = if (hasdim(x)) dim(x) else length(x)) {
  .fillout(x, length.out, recycle = TRUE)
}

stretch <- function(x, length.out = if (hasdim(x)) dim(x) else length(x)) {
  .fillout(x, length.out, recycle = FALSE)
}

.fillout <- function(x, length.out, recycle = TRUE) {
  if (length(length.out) <= 0) .stop(ifelse = recycle, "You can't <recycle|stretch> vector with a length argument of less than length 1.")
  if (!is.vector(x)) return(x)
  if (!hasdim(x)) {
    if (length(length.out) > 1) {
      x <- cbind(x) 
    } else {
      return (if (recycle) rep_len(x, length.out) else x[seq_len(length.out)])
    }
  } 
  dim <- dim(x)
  dim[seq_along(length.out) > length(dim)] <- 1
  dim(x) <- dim
  
  length.out[seq_along(dim) > length(length.out)] <- dim[seq_along(dim) > length(length.out)]
  length.out[is.na(length.out)] <- dim[is.na(length.out)]
  
  if (recycle) .recycle(x, length.out, dim) else .stretch(x, length.out, dim) 
}

.recycle <- function(x, length.out, dim) {  
  
  ind <- Map(\(d, l) rlang::expr(rep_len(seq_len(!!d), !!l)), dim, length.out )
  rlang::eval_tidy(rlang::expr(`[`(x, !!!ind, drop = FALSE)))

}


.stretch <- function(x, length.out, dim) {

  pad <- length.out - dim
  # if length.out is smaller in any dimension!
  ind <- Map(\(l, d) if (l < d) seq_len(l) else rlang::missing_arg(),
             length.out, dim)
  x <- rlang::eval_tidy(rlang::expr(`[`(x, !!!ind, drop = FALSE)))
  
  class <- class(c(x))
  Reduce(\(cur, pmar) {
    p <- pmar[1]
    margin <- pmar[2] 
    if (p <= 0) {
      cur
    } else {
      dim <- dim(cur)
      
      dim[margin] <- p
      pad <- array(as(NA, class), dim =dim)
      
      abind(cur, pad, along = margin)
      
    }
  }, Map(c, pad, seq_along(pad)), init = x)
  
}



.rep <- function(x, ..., margin = 1L) {
# Smart version of base::repeat which replicates things in any
# dimension
  if (is.null(dim(x))) {
     # out <- do.call('rep', list(x = x, ...)) 
     if (margin == 1L) do.call('rep', list(x = x, ...)) else x
  } else {
      
    dim <- dim(x)
    dim[margin] <- ifelse(margin %in% seq_along(dim), dim[margin], 1)
    dim(x) <- dim
    
    ind <- replicate(length(dim), rlang::missing_arg())
    ind[margin] <- lapply(dim[margin], \(d) rlang::expr(rep(seq_len(!!d), ...)))
    
    rlang::eval_tidy(rlang::expr(`[`(x, !!!ind)))
    

  }

}


# Lazy version of base::ifelse

.ifelse <- function(bool, texpr, fexpr) {
    # this is a truly lazy ifelse!
    # i.e., it only evaluates the part of the 
    # true/false condtions that need to be evaluated.
    # advantages are:
    # 1 speed enhancements (doesn't have to calculate two
    # entire things)
    # 2 it allows you to include exprs that will cause errors
    # or warning in some conditions.
    texpr <- rlang::enquo(texpr)
    fexpr <- rlang::enquo(fexpr)
    
    #
    bool[is.na(bool)] <- FALSE
    
    if (length(bool) == 0) return(c())
    
    if (any(!bool)) {
        fparsed <- captureValues(fexpr, parent.env(environment()), doatomic = FALSE)
        fvars <- do.call('forcedim', c(list(bool), fparsed$value))
        
        fexpr <- fparsed$expr
        f <- rlang::eval_tidy(fexpr, data = lapply(fvars, '[', i = !bool))
        f <- rep(f, length.out = sum(!bool))
        
        output <- empty(f, length(bool), dim(bool))
    }
    if (any(bool)) {
        tparsed <- captureValues(texpr, parent.env(environment()), doatomic = FALSE) 
        tvars <- do.call('forcedim', c(list(bool), tparsed$value))
        
        texpr <- tparsed$expr 
        t <- rlang::eval_tidy(texpr, data = lapply(tvars, '[', i =  bool))
        t <- rep(t, length.out = sum(bool))
        
        output <- empty(t, length(bool), dim(bool))
        output[bool] <- t
    }
    if (any(!bool))  output[!bool] <- f
    output %<-matchdim% bool
}





captureValues <- function(expr, env, doatomic = TRUE) {
    if (rlang::is_quosure(expr)) {
        env <- rlang::quo_get_env(expr)
        expr <- rlang::quo_squash(expr)
    }
    
    if (is.atomic(expr)) {
        if (doatomic) {
            name <- tempvar('atom', asSymbol = FALSE)
            return(list(value = setNames(list(rlang::eval_tidy(expr, env = env)), name),
                    expr = rlang::sym(name)))
        } else {
            return(list(value = NULL, expr = expr))
        }
    }
    if (!is.call(expr) ) {
        return(list(value = setNames(list(rlang::eval_tidy(expr, env = env)), rlang::expr_text(expr)),
                    expr = expr))
    }
    if (rlang::expr_text(expr[[1]]) %in% c(':', '`[`', '`[[`', '`@`', '`$`')) {
        name <- tempvar(':', asSymbol = FALSE)
        return(list(value = setNames(list(rlang::eval_tidy(expr, env = env)), name),
                    expr = rlang::sym(name)))
    }
    
    values <- list()
    for (i in 2:length(expr)) {
        recalled <- Recall(expr[[i]], env, doatomic = doatomic)
        expr[[i]] <- recalled$expr
        values <- c(values, recalled$value)
    }
    
    list(value = values, expr = expr)
}




# Math ----

setAs('integer', 'integer64', \(from) as.integer64.integer(from))
setAs('numeric', 'integer64', \(from) as.integer64.double(from))
setAs('logical', 'integer64', \(from) as.integer64.logical(from))
setAs('character', 'integer64', \(from) as.integer64.character(from))


entropy <- function(x, base) UseMethod('entropy')
entropy.table <- function(x, base = 2) {
  if (sum(x) != 1) x <- x / sum(x)
  
  -sum(x * log(x, base))
  
}
entropy.default <- function(x, base = 2) {
  entropy.table(table(x), base = base)
}

find2Dlayout <- function(n) {
  
  options <- c(1, 2, 4, 6, 8, 9, 12, 15, 16)
  
  if (n > max(options)) n <- max(options)
  div     <- c(1, 2, 2, 2, 2, 3, 3 , 3,  4)
  
  
  div <- div[which(options >= n)[1]]
  n <- options[which(options >= n)[1]]
  
  c(div, n / div)
  
}

pmaxmin <- function(x, min = -Inf, max = Inf) as(pmax(pmin(x, max), min), class(x))



reduce_fraction <- function(n, d) {
    # Used by rational initialize method
    sign <- sign(n)
    n <- abs(n)
    gcds <- do(gcd, list(n, d))
    num <- n %/% gcds
    den <- d %/% gcds
    list(Numerator = sign * num, Denominator = den)
}

match_fraction <- function(n, d) {
  # d <- d[!is.na(d)]
  # if (length(d) == 0L) return(list(Numerator = rep(as(NA, class(n))), length(d), Denominator = as(NA, class(n))))
  
  newdenominator <- do.call('lcm', as.list.numeric_version(sort(unique(d), decreasing = TRUE)))
  
  if (newdenominator > 1e9) {
    data.frame(Numerator = n / d, Denominator = rep(1L, length(n)))
    
  } else {
    newnumerators <- n * (newdenominator %/% d)
    list(Numerator = newnumerators, Denominator = newdenominator)
  }
  
  
}

gcd <- function(...) {
    x <- list(...)
    x <- x[lengths(x) > 0]
    if (length(x) == 1L) return(x[[1]])
    if (length(x) == 0L) return(numeric(0))
    
    x <- do.call('match_size', x)
    na <- Reduce('|', lapply(x, is.na))
    output <- vectorNA(length(x[[1]]), class(x[[1]]))
    output[!na] <- Reduce(.gcd, lapply(x, '[', !na))
    output
}

.gcd <- function(x, y) {
  if (is.na(x) || is.na(y)) return(as(NA, class(x)))
    r <- x %% y
    
    notyet <- r > 0
    if (any(notyet)) y[notyet] <- Recall(y[notyet], r[notyet])
    y
}



lcm <- function(...) {
    x <- list(...)
    x <- x[lengths(x) > 0]
    if (length(x) == 1L) return(x[[1]])
    if (length(x) == 0L) return(integer64(0))
    na <- Reduce('|', lapply(x, is.na))
    
    output <- vectorNA(length(x[[1]]), class(x[[1]]))
    output[!na] <- Reduce(.lcm, lapply(x, '[', !na))
    output
}

.lcm <- function(x, y) {
    gcd <- .gcd(x, y)
    # output <- abs(x * y) / .gcd(x, y)
    output <- as.integer(abs(x) %/% gcd) * abs(y)
    
    output
    # if (is.integer(x) & is.integer(y)) as.integer(output) else output
}

`%divides%` <- function(e1, e2) gcd(e1, e2) == e1

# modulo starting from 1
`%1%` <- function(e1, e2) ((e1 - 1L) %% e2) + 1L

#### calculus

# sigma (integrate) and delta (derive) should be perfect inverses, 
# so long as their skip arguments are the same

checkWindows <- function(x, windows) {
  if (!is.list(windows)) windows <- list(windows)
  if (length(windows)) windows[sapply(windows, \(w) !is.null(w) && length(w) == length(x))] else windows 
}


harmonicInterpolate <- function(x, y, includeEdges = FALSE, bigFirst = FALSE) {
  # finds integers between x and y, which can be found as 
  # cummulative products starting with x
  
  ratio <- as.integer(y %/% x)
  pFactors <- numbers::primeFactors(ratio)
  if (bigFirst) pFactors <- rev(pFactors)
  
  cumFactors <- cumprod(pFactors)
  cumFactors <- cumFactors[cumFactors < ratio]
  
  z <- x * cumFactors
  
  sort(if (includeEdges) c(x, z, y) else z)
  
}

#' Cumulative sum of numeric vector
#' 
#' Calculate sequential cummulative sum of values in numeric vectors.
#' 
#' 
#' `sigma` is very similar base-`R` [cumsum()].
#' However, `sigma` should be favored in [humdrumR] use because:
#' 
#' 1. It has a `groupby` argument, which is *automatically* used by `humdrumR` [with(in)][withinHumdrum]
#'    commands to constrain the differences within files/spines/paths of `humdrum` data.
#'    Using the `groupby` argument to a function (details below) is generally faster than using a `groupby` argument to [withinHumdrum()].
#' 2. They (can) automatically skip `NA` (or other) values.
#' 3. `sigma` also has a `init` argument which can be used to ensure full invertability with [delta()]. See the "Invertability"
#' section below.
#' 
#' If applied to a matrix, `sigma` is applied separately to each column, unless `margin` is set to `1` (rows)
#' or, if you have a higher-dimensional array, a higher value.
#'
#'
#' @section Invertability:
#' 
#' The `sigma` and `delta` functions are inverses of each other, meaning that with the right arguments set,
#' `sigma(delta(x)) == x` and `delta(sigma(x)) == x`.
#' In other words, the two functions "reverse" each other.
#' The key is that the `init` argument needs to be set to `0`, and all other 
#' arguments (`lag`, `skip`, `groupby`, etc.) need to match.
#' So *actually*,  `sigma(delta(x, init = 0, ...)) == x` and `delta(sigma(x), init = 0)) == x`.
#'
#' When we take the differences between values (`delta(x)`), the resulting differences can't tell us 
#' fully how to reconstruct the original unless we know where to "start" (a constant offset).
#' For example, 
#'
#' + `delta(c(5, 7, 5, 6)) == c(NA, 2, -2, 1)`
#'
#' We know our input goes up 2, back down 2, then up 1, but the starting value (the first `5`)
#' is lost.
#' If we call sigma on this, we'll get:
#' 
#' + `sigma(c(NA, 2, -2, 1)) == c(0, 2,0, 1)`
#' 
#' We get the right contour, but we're offset by that constant `5`.
#'  
#' If we call `delta(x, init = 0)` the necessary constant (the first value) is kept at the beginning of the vector
#' 
#' + `delta(c(5, 7, 5, 6), init = 0) == c(5, 2, -2, 1)`
#' 
#' so `sigma` gets what we want, full invertability:
#' 
#' + `sigma(delta(c(5, 7, 5, 6), init = 0)) == c(5, 7, 5, 6)`
#'  
#' Alternatively, we could specify the necessary constant as the `init` argument of `sigma`:
#' 
#' + `sigma(delta(c(5, 7, 5, 6)), init = 5) == c(5, 7, 5, 6)`
#' 
#' so the `init` arguments of the two functions are complementary.
#'
#' Currently, the `right` argument of `delta` has no complement in `sigma`, so invertability
#' only holds true if `right = FALSE` (the default).
#'
#' @section Greater lags:
#' 
#' The behavior of `sigma` when `abs(lag) > 1` is easiest to understand as the inverse of the 
#' behavior of [delta(abs(lag) > 1)][delta], which is more intuitive. (`sigma` is the inverse of [delta()], see the
#' *Invertability* section above).
#' 
#' Generally, if `abs(lag) > 1`, `x` is grouped by its indices modulo `lag`, and the cumulative sum is calculated separately
#' for each set of modulo indices.
#' For example, consider `lag == 2` for the following input:
#' 
#' | `x`     | index  | index modulo 2   |
#' |---------|--------|------------------|
#' | 1       | 1      | 1                |
#' | 3       | 2      | 0                |
#' | 2       | 3      | 1                |
#' | 2       | 4      | 0                |
#' | 5       | 2      | 1                |
#' 
#' The cumulative sum of the `1` and `0` modulo-index groups are:
#' 
#' + Index `1`: `cumsum(c(1,2,5)) == c(1, 3, 8)`.
#' + Index `0`: `cumsum(c(3,2)) == c(3, 5)`
#' 
#' Interleaved back into order, the result is `c(1,3,3,5,8)`.
#' This may not be very clear, but sure enough `delta(c(1, 3, 3, 5, 8), lag = 2, init = 0)` returns the original
#' `c(1,3,2,2,5)` vector!
#' Again, understanding [delta(..., lag = n)][delta()] is easier than `sigma(..., lag = n)` (see the *Invtertability* section
#' below.)
#' 
#' @section Negative lag:
#' 
#' If `lag` is negative, the output is the same as the equivalent positive lag, except
#' the sign is reversed (`output * -1`).
#' This behavior is easiest to understand as the inverse of the 
#' behavior of [delta(lag < 0)][delta], which is more intuitive. (`sigma` is the inverse of [delta()], see the
#' *Invertability* section above).

#'
#' 
#' @section Grouping:
#' 
#' In many cases we want to perform lagged calculations in a vector, but *not across certain boundaries*.
#' For example, if your vector includes data from multiple pieces, we wouldn't want to calculate melodic intervals
#' between pieces, only within pieces.
#' The `groupby` argument indicates one, or more, grouping vectors, which break the `x` (input) argument
#' into groups.
#' If more than `groupby` vectors are given, a change in *any* vector indicates a boundary.
#' 
#' Value pairs which cross between groups are treated as if they were at the beginning.
#' Basically, using the `groupby` argument to a function should be 
#' similar or identical to using `tapply(x, groupby, laggedFunction, ...)` or using a `groupby`
#' expession in a call to [with(in).humdrumR][withinHumdrum].
#' However, using a `groupby` argument directly is usually much faster, as they have been
#' specially optimized for this functions.
#' 
#' The most common use case in humdrum data, is looking at "melodies" within spines.
#' For this, we want `groupby = list(File, Spine, Path)`.
#' In fact, `humdrumR` [with(in)][withinHumdrum] calls will *automatically* feed these 
#' three fields as `groupby` arguments to certain functions: `r harvard(byTable[byTable$Type == 'melodic', ]$Function, 'or')`.
#' So any use of `delta` in a call to [with(in)][withinHumdrum], will automatically calculate the `delta`
#' in a "melodic" way, within each spine path of each file.
#' However, if you wanted, for instance, to calculate differences across spines (like harmonic intervals)
#' you could manually set `groupby = list(File, Record)`.
#' 
#' @section Order:
#' 
#' When performing lagged calculations, we typically assume that the order of the values in the input vector
#' (`x`) is the order we want to "lag" across.
#' E.g., the first element is "before" the second element, which is "before" the third element, etc.
#' [Humdrum tables][humTable] are always ordered `File > Piece > Spine > Path > Record > Stop`.
#' Thus, any lagged calculations across fields of the humtable will be, by default, "melodic":
#' the *next* element is the next element in the spine path.
#' For example, consider this data:
#' 
#' ```
#' **kern  **kern
#' a       d
#' b       e
#' c       f
#' *-      *-
#' ```
#' 
#' The default order of these tokens (in the `Token` field) would be `a b c d e f`.
#' If we wanted to instead lag across our tokens *harmonically* (across records) we'd need to specifiy a different order
#' For example, we could say `orderby = list(File, Record, Spine)`---the lagged function
#' would interpret the `Token` field above as `a d b e c f`.
#' 
#' For another example, note `Stop` comes last in the order.
#' Let's consider what happens then if here are stops in our data:
#' 
#' ````
#' **kern  **kern
#' a       d
#' b D     e g
#' c A     f a
#' *-      *-
#' ```
#' 
#' The default ordering here (`File > Spine > Record > Stop`) "sees" this in the order `a b D c A d e g f a`.
#' That may or may not be what you want!
#' If we wanted, we could reorder such that `Stop` takes precedence over `Record`: `orderby = list(File, Spine, Stop, Record)`.
#' The resulting order would be `a b c d e f D G g a`.
#' 
#'    
#' @param x (Any numeric vector.) `NULL` values are returned `NULL`.
#' @param lag (Non-zero integer.) Which lag to use. (See *Greater lags* section, below.) 
#' @param skip (`function`.) This must be a function which can be applied to `x` and returns a logical vector
#' of the same length. And `TRUE` values are skipped over in the calculations.
#' By default, the `skip` function is `is.na`, so `NA` values in the input (`x` argument) are skipped.
#' The skipped values are returned as is in the output vector.
#' @param init (Atomic value of same class as `x`, with `length(init) <= lag`.) `NA` values at the beginning
#' (or end of `right == TRUE`) are filled with these values *before* summing.
#' @param right (single `logical` value) Should the `init` padding be at the "right" (end of the vector)?
#' By default, `right == FALSE` so the `init` padding is at the beginning of the output.
#' @param groupby (vector of same length as `x`, or a list of such vectors) Differences are not calculated
#' across groups indicated by the `groupby` vector(s).
#' @param orderby (vector of same length as `x`, or a list of such vectors) Differences in `x` are calculated
#' based on the order of `orderby` vector(s), as determined by [base::order()].
#' 
#' 
#' @family {Lagged vector functions}
#' @seealso This function's inverse is [delta()]. 
#' @export
sigma <- function(x, lag, skip = is.na, init, groupby = list(), ...) UseMethod('sigma')
#' @rdname sigma
#' @export
sigma.default <- function(x, lag = 1, skip = is.na, init = 0, groupby = list(), orderby = list(), ...) {
  if (is.null(x)) return(NULL)
  
  checks(x, xnumber)
  checks(lag, xwholenum & xlen1 & xnotzero)
  checks(skip, xnull | xclass('function'))
  checks(init, xatomic & xminlength(1) & 
           argCheck(\(arg) length(arg) <= abs(lag), 
                    "must be as short or shorter than the absolute lag",  
                    \(arg) paste0(.mismatch(length)(arg), ' and lag == ', lag)))
  
  
  groupby <- checkWindows(x, groupby)
  orderby <- checkWindows(x, orderby)
  
  groupby <- reorder(groupby, orderby = orderby, toEnv = FALSE)
  reorder(list(x = x), orderby = orderby)
  
  if (length(groupby)) {
    segments <- segments(do.call('changes', c(groupby, list(...))))
    return(unname(tapply_inplace(x, segments, sigma.default, lag = lag, skip = skip, init = init)))
  } 
  
  if (lag < 0) {
    x <- -x
    lag <- -lag
  }
  
  init <- rep(init, length.out = lag)
  if (all(is.na(x[1:lag]))) x[1:lag] <- init
  
  skip <- if (is.null(skip)) logical(length(x)) else skip(x)
  
  result <- x
  x <- x[!skip]
  
  result[!skip] <- if (abs(lag) > 1L) {
    groups <- seq_along(x) %% lag
    groups[groups == 0] <- lag
    unname(tapply_inplace(x, groups, cumsum))
  } else {
    cumsum(x)
  }
  
  reorder(result)
}
#' @rdname sigma
#' @export
sigma.matrix <- function(x, margin = 2L, ...) {
  if (margin > length(dim(x))) .stop("You can't use a `margin` argument of higher dimension than your input.",
                                     "In this case, you're asking for margin == {margin}, but your input only has",
                                     "{num2word(length(dim(x)))} dimensions.")
  
  results <- apply(x, margin, sigma.default, ..., simplify = FALSE)
  switch(margin,
         do.call('rbind', results),
         do.call('cbind', results),
         results)
}



#' Lagged differences
#'
#' Calculate sequential differences of values in numeric vectors.
#' 
#' 
#' `delta` is very similar base-`R` [diff()].
#' However, `delta` should be favored in [humdrumR] use because:
#' 
#' 1. Its output is *always* the same length as its  input.
#'    This is achieved by padding the beginning or end of the output with1 `NA` values (or other options).
#' 2. It has a `groupby` argument, which is *automatically* used by `humdrumR` [with(in)][withinHumdrum]
#'    commands to constrain the differences within files/spines/paths of `humdrum` data.
#'    The `groupby` approach (details below) is generally faster than applying the commands within `groupby` groups.
#' 3. They (can) automatically skip `NA` (or other) values.
#' 
#' If applied to a matrix, `delta` is applied separately to each column, unless `margin` is set to `1` (rows)
#' or, if you have a higher-dimensional array, a higher value.
#' 
#' # Initial/padding values
#' 
#' Each lagged pair of numbers in the vector is summed/subtracted.
#' This leaves `abs(lag)` numbers at the end with nothing to pair with them.
#' For example, `lag == 1`, the indices which are getting subtracted look like this:
#' 
#' + \eqn{x_1 - x_?}
#' + \eqn{x_2 - x_1}
#' + \eqn{x_3 - x_2}
#' + \eqn{x_4 - x_3}
#' + \eqn{x_5 - x_4}
#' 
#' If `lag == 3`: 
#' 
#' + \eqn{x_1 - x_?}
#' + \eqn{x_2 - x_?}
#' + \eqn{x_3 - x_?}
#' + \eqn{x_4 - x_1}
#' + \eqn{x_5 - x_2}
#' 
#' The `init` argument (for "initial") is a value, or values, to pair with the first `lag` values.
#' By default, `init` is `NA`, and since `n + NA` or `n - NA` are themselves, `NA`, the output vector is
#' padded with `NA` values. For `lag == 3` again:
#' 
#' + \eqn{x_1 - NA}
#' + \eqn{x_2 - NA}
#' + \eqn{x_3 - NA}
#' + \eqn{x_4 - x_1}
#' + \eqn{x_5 - x_2}
#' 
#' However, if the `init` argument can between 1 and `abs(lag)` numeric values.
#' The result, for `lag==3` is:
#' 
#' + \eqn{x_1 - init_1}
#' + \eqn{x_2 - init_2}
#' + \eqn{x_3 - init_3}
#' + \eqn{x_4 - x_1}
#' + \eqn{x_5 - x_2} 
#' 
#' If `right == TRUE`, the `init` values are placed at the end, like:
#' 
#' + \eqn{x_4 - x_1}
#' + \eqn{x_5 - x_2} 
#' + \eqn{init[1] - x_3}
#' + \eqn{init[2] - x_4}
#' + \eqn{init[3] - x_5}
#' 
#' The `init` argument functions similarly to the `init` argument of [Reduce()].
#' 
#' # Negative lag
#' 
#' If `lag` is negative, the differences are simply reversed, resulting in the same numbers as the 
#' equivalent positive lag, but `* -1`.
#' 
#' + \eqn{x_1 - NA}
#' + \eqn{x_2 - x_1}
#' + \eqn{x_3 - x_2}
#' + \eqn{x_4 - x_3}
#' + \eqn{x_5 - x_5}
#' 
#' to
#' 
#' + \eqn{NA - x_1}
#' + \eqn{x_1 - x_2}
#' + \eqn{x_2 - x_3}
#' + \eqn{x_3 - x_4}
#' + \eqn{x_4 - x_5}
#' 
#' @param lag (Non-zero integer.) Which lag to use. Results will look like: `x[i] - x[i - lag]`.
#' 
#' @inheritParams sigma
#' @inheritSection sigma Grouping
#' @inheritSection sigma Order
#' @inheritSection sigma Invertability
#' 
#' @family {Lagged vector functions}
#' @seealso This function's inverse is [sigma()]. 
#' @export
delta <- function(x, lag, skip, init, right, ...) UseMethod('delta') 
#' @rdname delta
#' @export
delta.default <- function(x, lag = 1, skip = is.na, init = as(NA, class(x)), right = FALSE, 
                          groupby = list(), orderby = list(), ...) {
    if (is.null(x)) return(NULL)
    checks(x, xnumber | xclass('tonalInterval'))
    checks(lag, xwholenum & xlen1 & xnotzero)
    checks(skip, xnull | xclass('function'))
    checks(init, (xatomic | xclass('tonalInterval')) & xminlength(1) & 
             argCheck(\(arg) length(arg) <= abs(lag), 
                      "must be as short or shorter than the absolute lag",  
                      \(arg) paste0(.mismatch(length)(arg), ' and lag == ', lag)))
    checks(right, xTF)
    
    groupby <- reorder(groupby, orderby = orderby, toEnv = FALSE)
    reorder(list(x = x), orderby = orderby)
    
    init <- rep(init, length.out = abs(lag))
    if (lag < 0) {
      x <- rev(x)
      right <- !right
    }
    skip <- if (is.null(skip)) logical(length(x)) else skip(x)
    #
    if (right)  {
      skip_pad <- c(skip, logical(abs(lag)))
      x_pad    <- c(x,    init) 
    } else {
      skip_pad <- c(logical(abs(lag)), skip)
      x_pad    <- c(init,  x)
    }
      
    output <- x
    output[!skip] <-  diff(x_pad[!skip_pad], lag = abs(lag))
    
    if (lag < 0) output <- rev(output)
    
    groupby <- checkWindows(x, groupby)
    if (length(groupby)) {
      bounds <- which(do.call('changes', c(groupby, list(reverse = right, ...))))
      if (abs(lag) > 1L) {
        arith <- if (right) (\(l) bounds - l) else (\(l) bounds + l )
        bounds <- sort(Reduce(`union`, lapply((2:abs(lag)) - 1L, arith), init = bounds))
      }
      output[bounds] <- x[bounds] - rep(init, length.out = length(bounds))
    }
    
    reorder(output)
}
 
#' @rdname delta
#' @export
delta.matrix <- function(x, margin = 2L, ...) {
  if (margin > length(dim(x))) .stop("You can't use a `margin` argument of higher dimension than your input.",
                                     "In this case, you're asking for margin == {margin}, but your input only has",
                                     "{num2word(length(dim(x)))} dimensions.")
  results <- apply(x, margin, delta.default, ..., simplify = FALSE)
  switch(margin,
         do.call('rbind', results),
         do.call('cbind', results),
         results)
}

makeCumulative <- function(n, groupby = list()) {
  if (length(groupby) == 0L) return(n)
  
  diff <- delta.default(n, init = 0L)
  diff[diff <= 0 & do.call('changes', groupby)] <- 1
  # diff[which(!is.na(n))[1]] <- 1
  sigma.default(diff, init = NA)
}

.cummax <- function(x) {
  x[!is.na(x)] <- cummax(x[!is.na(x)])
  x
}

#' Expand numbers outwards from zero
#' 
#' Expand is a complement to the base `R` [rounding functions][base::round()], particularly `trunc`.
#' 
#' @details 
#' Each of the four base `R` functions---`round`, `ceiling`, `floor`, and `trunc`---follow
#' a different logic in how they round real numbers to ingegers:
#' 
#' + `round`: round to *nearest* integer in either direction.
#' + `floor`: round downward *towards negative infinity*.
#'   + Negative numbers are rounded to "more negative" numbers.
#' + `ceiling`: round upward *towards infinity*.
#'   + Negative numbers are rounded to "less negative" numbers.
#' + `trunc`: round "inward" *towards* zero.
#'   + Negative numbers are rounded *up* to "less negative" numbers, but positive
#'     numbers are still rounded downwards to "less positive" numbers.
#'     
#' Just as `ceiling` compliments `floor`, the `humdrumR` function `expand` acts 
#' as a compliment to `trunc`: `expand` rounds "outward" *away from zero*.
#' Negative numbers are rounded to "more negative" numbers and positive numbers
#' are rounded to "more positive" numbers.
#' 
#' A table explains better than words:
#' 
#' | Call                                   | Returns           |
#' |:---------------------------------------|:------------------|
#' | `  round(c(2.9, 3.1, -2.9, -3.1))`     | `c(3, 3, -3, -3)` |
#' | `  floor(c(2.9, 3.1, -2.9, -3.1))`     | `c(2, 3, -3, -4)` |
#' | `ceiling(c(2.9, 3.1, -2.9, -3.1))`     | `c(3, 4, -2, -3)` |
#' | `  trunc(c(2.9, 3.1, -2.9, -3.1))`     | `c(2, 3, -2, -3)` |
#' | ` expand(c(2.9, 3.1, -2.9, -3.1))`     | `c(3, 4, -3, -4)` |
#' 
#' 
#' @export
setGeneric('expand', def = \(x) {
  .ifelse(x >=0, ceiling(x), -ceiling(abs(x)))
})

         

locate <- function(x, table) {
    if (is.null(dim(table)) || length(dim(x)) == 1) {
        setNames(lapply(x, \(val) which(table == val)), x)
    } else {
        apply(x, 1, 
              \(val) {
                  which(Reduce('&', Map('==', table, val)))
                  
              })
    }
}


# bitwise tools

ints2bits <- function(n, nbits = 8) {
    mat <- t(sapply(n, \(x) as.integer(intToBits(x))))[ , 1:nbits, drop = FALSE]
    
    rownames(mat) <- n
    colnames(mat) <- 2 ^ (0:(nbits - 1))
    mat
}

bits2ints <- function(x) as.integer(rowSums(sweep(x, 2, 2L ^ (0L:(ncol(x) - 1L)), `*`)))


ints2nits <- function(n, it = 2, nits = 8) {
    if (hasdim(n)) {
        cols <- list()
        for (j in 1:ncol(n)) cols[[j]] <- Recall(n[ , j], it = it, nits = nits)
        return(do.call('abind', c(along = 3, cols)))
    }
    
    #
    
    cur <- n %% it
    out <- if (nits == 1) {
        cbind(cur)
    } else {
        
        out <- cbind(Recall(n %/% it, it , nits = nits - 1L), cur)
        colnames(out) <- (it^((nits - 1L) : 0L))
        out
        
    }
    
    rownames(out) <- n
    out
}




ints2baltern <- function(n, ntrits = 8L) {
    # integers to balanced ternary
    tern <- ints2nits(abs(n), it = 3L, ntrits)
    
    while(any(tern == 2L, na.rm = TRUE)) {
        twos <- which(tern == 2L, arr.ind = TRUE)
        
        if (any(twos[, 2] == 1L)) return(Recall(n, ntrits + 1))
        
        tern[twos] <- -1L
        
        twos[ , 2] <- twos[ , 2] - 1L
        tern[twos] <- tern[twos] + 1L
        
    }
    
    ## allow negative numbers
    # notzero <- which( tern != 0, arr.ind = TRUE)
    # firstnotzero <- cbind(which(n != 0), tapply(notzero[ , 'col'], notzero[ , 'row'], min))
    # tern[firstnotzero] <- tern[firstnotzero] * sign(n[n != 0])
    
    ## incorporate sign
    sweep(tern, c(1, if (length(dim(n))>1) 3), as.integer(sign(n)), '*')
    
}

baltern2int <- function(mat) {
    as.integer(rowSums(sweep(mat, 2, 3^((ncol(mat)-1):0), `*`)))
}

bitwRotateL <- function(a, n, nbits = 8L) {
    bitwOr(bitwShiftL(a, n), bitwShiftR(a, nbits - n)) %% (2^nbits)
    
}

bitwRotateR <- function(a, n, nbits = 8L) {
    bitwOr(bitwShiftL(a, nbits - n), bitwShiftR(a, n)) %% (2^nbits)
    
}


## bitwise tools, with decimal place...
# 
# num2bits <- function(n, nbits = 8) {
#     positive <- floor(n)
#     negative <- as.integer((n - positive) / 2^-31)
#     # mat <- t(as.integer(sapply(positive, intToBits)) - as.integer(sapply(negative, intToBits)))[, 1:nbits, drop = FALSE]
#     posmat <- t((sapply(positive, \(x) as.integer(intToBits(x)))))[ , 1:nbits, drop = FALSE]
#     negmat <- t((sapply(negative, \(x) as.integer(intToBits(x)))))[ , 32:(32-nbits + 1), drop = FALSE]
#     mat <- posmat - negmat
#     rownames(mat) <- n
#     colnames(mat) <- 2 ^ (0:(nbits - 1))
#     # colnames(mat) <- 2 ^ c((nbits+1):1, 0:(nbits-1 ))
#     mat
# }
# 
# bits2num <- function(x) {
#     twos <- sweep(x, 2, 2L ^ (0L:(ncol(x) - 1L)), `*`)
# 
#     twos[twos < 0] <- 1 / abs( twos[twos < 0])
# 
#     rowSums(twos)
# }
# 



# Metaprogramming ----


visible <- function(withV) {
  if (is.null(withV$value)) return(NULL)
  visible <- withV$visible
  result <- withV$value
  
  attr(result, 'visible') <- visible %||% TRUE
  
  result
}

withExpression <- function(expr, predicate, func, applyTo = c('call', 'atomic', 'symbol')) {
    exprA <- analyzeExpr(expr)
    output <- NULL
    if (exprA$Type %in% applyTo) {
        hit <- do...(predicate, exprA, envir = parent.frame())
        if (hit) {
            output <- func(exprA)
        } 
    } else {
        hit <- FALSE
    }
    
    if (exprA$Type == 'call' && !hit) {
        output <- list()
        for (i in seq_along(exprA$Args)) {
            output[[i]] <- Recall(exprA$Args[[i]], 
                                      func = func, 
                                      predicate = predicate, 
                                      applyTo = applyTo)
        }
        if (length(output) == 0L || all(lengths(output) == 0L)) output <- NULL
        
    }
    output
}


namesInExprs <- function(names, exprs) {
    unique(unlist(lapply(exprs, namesInExpr, names = names)))
}

namesInExpr <- function(names, expr, applyTo = 'symbol') {
    ## This function identifies which, if any,
    ## of a character vector ("names") are referenced as a name 
    ## (not including things called as functions) in an expression 
    ## (or rhs for formula).
    
    unlist(withExpression(expr, applyTo = applyTo,
              \(Head) Head %in% names,
              \(exprA) {
                  matches <- names[pmatch(exprA$Head, names)]
                  
                  matches
              }))
}

substituteName <- function(expr, subs) {
  if (length(subs) == 0) return(expr)
  
  if (is.call(expr) && length(expr) > 1L) {
   
            for (i in 2:length(expr)) {
                if (!is.null(expr[[i]])) expr[[i]] <- Recall(expr[[i]], subs)
            }
  } else { 
            if (deparse(expr) %in% names(subs)) expr <- subs[[which(deparse(expr) == names(subs))]]
   
  }
  expr
          
}




tempvar <- function(prefix = '', asSymbol = TRUE) {
    # this makes random symbols to use as variable names
    
    random <- paste(sample(c(letters, 0:9), 5, replace = TRUE), collapse = '')
    
    char <- paste0('._', prefix, '_', random)
    
    if (asSymbol) rlang::sym(char) else char
    
}


analyzeExpr <- function(expr, stripBrackets = FALSE) {
    exprA <- list()
    exprA$Form <- if (!rlang::is_formula(expr)) {
        'expression'
    } else {
        exprA$Environment <- environment(expr)
        exprA$Original <- expr
        if (rlang::is_quosure(expr)) {
            expr <- rlang::quo_get_expr(expr)
            'quosure'
        } else {
            exprA$LHS <- rlang::f_lhs(expr)
            expr <- rlang::f_rhs(expr)
            'formula'
        }
        
    }
    
    exprA$Type <- if (is.null(expr)) 'NULL' else if (is.atomic(expr)) 'atomic' else {if (is.call(expr)) 'call' else 'symbol'}
    exprA$Class <- if(exprA$Type == 'atomic') class(expr)
    exprA$Head <- switch(exprA$Type,
                         call = as.character(expr[[1]]),
                         atomic = 'c',
                         symbol = as.character(expr),
                         'NULL' = 'NULL')
    exprA$Args <- switch(exprA$Head,
                         'function' = {
                           exprA$Type <- 'lambda'
                           exprA$Pairlist <- expr[[2]]
                           list(expr[[3]])
                         },
                         atomic = as.list(expr),
                         call = as.list(expr[-1]),
                         list())
    
    exprA$Args <- if (exprA$Head[1] == 'function') {
      exprA$Type <- 'lambda'
      exprA$Pairlist <- expr[[2]]
      list(expr[[3]])
    } else {
      switch(exprA$Type,
             call = as.list(expr[-1]),
             atomic = as.list(expr),
             list())
    }

    
    

    if (stripBrackets && 
        exprA$Head %in% c("(", "{") && 
        length(exprA$Args) == 1L) {
      recurse <- switch(exprA$Form,
             quosure = rlang::new_quosure(exprA$Args[[1]], exprA$Environment),
             formula = rlang::new_formula(exprA$LHS, exprA$Args[[1]], exprA$Environment),
             exprA$Args[[1]])
      return(Recall(recurse, stripBrackets = TRUE))
    } 
    
    exprA
    
    
    
}

unanalyzeExpr <- function(exprA) {
  
    if (exprA$Type == 'atomic' && exprA$Head == 'c' && length(exprA$Args) == 1L) exprA$Type <- 'scalar'
    expr <- switch(exprA$Type,
                   scalar = exprA$Args[[1]],
                   atomic = ,
                   call =  do.call('call', c(exprA$Head, exprA$Args), quote = TRUE),
                   symbol = rlang::sym(exprA$Head),
                   lambda = call('function', exprA$Pairlist, exprA$Args[[1]]))
    
    if (missing(expr)) return(rlang::missing_arg())
    if (exprA$Form != 'expression') {
        expr <- if (exprA$Form == 'formula') {
            rlang::new_formula(exprA$LHS, expr, env = exprA$Environment)
        }   else {
            rlang::new_quosure(expr, env = exprA$Environment)
        }
    }
    expr
}



withinExpression <- function(expr, predicate = \(...) TRUE, func, applyTo = 'call', stopOnHit = TRUE) {
  if (is.null(expr)) return(expr)
  exprA <- analyzeExpr(expr)
  
  if (exprA$Type %in% applyTo) {
    hit <- do...(predicate, exprA, envir = parent.frame())
    if (hit) {
      exprA <- func(exprA)
    } 
  } else {
    hit <- FALSE
  }
  
  
  if (exprA$Type == 'call' && !(hit && stopOnHit)) {
    for (i in seq_along(exprA$Args)) {
      # print(exprA$Args[[i]])
      cur <- exprA$Args[[i]]
      if (!missing(cur) && !is.null(cur)) exprA$Args[[i]] <- Recall(cur, 
                                                                    func = func, 
                                                                    predicate = predicate, 
                                                                    stopOnHit = stopOnHit,
                                                                    applyTo = applyTo)
    }
    
  }
  
  unanalyzeExpr(exprA)
  
}

withExpression <- function(expr, predicate, func, applyTo = c('call', 'atomic'), stopOnHit = TRUE) {
  output <- list()
  if (is.null(expr)) return(output)
  
  exprA <- analyzeExpr(expr)
  
  
  if (exprA$Type %in% applyTo) {
    hit <- do...(predicate, exprA, envir = parent.frame())
    if (hit) {
      output <- func(exprA)
    } 
  } else {
    hit <- FALSE
  }
  
  if (exprA$Type == 'call' && !(hit && stopOnHit)) {
    outputRecurse <- list()
    for (i in seq_along(exprA$Args)) {
      outputRecurse[[i]] <- Recall(exprA$Args[[i]], 
                            func = func, 
                            predicate = predicate, 
                            applyTo = applyTo)
    }
    if (length(outputRecurse) == 0L || all(lengths(outputRecurse) == 0L)) outputRecurse <- NULL
    
    output <- c(output, outputRecurse)
  }
  output
}


is.givenCall <- function(expr, call) {
    if (rlang::is_quosure(expr)) expr <- rlang::quo_squash(expr)
    is.call(expr) && as.character(expr[[1]]) == call
    
    
}

wrapInCall <- function(call, x, ...) {
    isquo  <- rlang::is_quosure(x)
    isform <- rlang::is_formula(x)
    
    expr <- if (isform & !isquo) rlang::f_rhs(x) else x
    
    result <- (if (isquo) rlang::quo else rlang::expr)((!!rlang::sym(call))(((!!expr)), !!!list(...)))
    
    if (isform & !isquo) rlang::new_formula(rlang::f_lhs(x), result, rlang::f_env(x)) else result
    
    
}



.function <- function(args, body) {
    rlang::new_function(args, rlang::enexpr(body), parent.frame())
    
}


getStructure <- function(...) {
  getArgs(c('File', 'Spine', 'Patch'), ...)
}

getArgs <- function(args, ...) {
  # gets arguments from ... if they exist
  ldots <- list(...)
  
  ldots <- ldots[.names(ldots) != '']
  ldots[args[args %in% names(ldots)]]
}




callArgs <- function(call) {
    call <- if (is.call(call)) call[[1]] else call
    func <- rlang::eval_tidy(call)
    fargs <- fargs(func)
    fargs[-1]
    
}

append2expr <- function(expr, exprs) {
    l <- length(expr)
    expr <- as.list(expr)
    
    as.call(append(expr, exprs, 2))
}

## Splitting expressions 

ast <- function(expr) {
    if (!is.call(expr)) return(expr)
    
    if (rlang::is_formula(expr)) {
        env <- environment(expr)
        expr <-  if (rlang::is_quosure(expr)) {
            rlang::quo_get_expr(expr) 
        } else {
            rlang::f_rhs(expr) 
        }
        ast <- Recall(expr)
        attr(ast, 'environment') <- env
        return(ast)
    }
    
    call <- deparse(expr[[1]])
    
    ast <- list()
    
    for (i in 2:length(expr)) ast[[i - 1]] <- Recall(expr[[i]])
    names(ast)[1] <- call
    
    ast %class% 'ast'
    
    
    
    
}


print.ast <- function(x, depth = 0L) {
    pad <- strrep(' ', depth)
    if (!inherits(x, 'ast')) {
        local({
            if (!is.atomic(x)) x <- deparse(x)
            cat(pad, x, sep = '', '\n')
        })
        
       return(invisible(x))
    }
    
    
    env <- attr(x, 'environment')
    env <- if (!is.null(env)) paste0('  <environment: ', rlang::env_label(env), '>')
    cat(pad, names(x)[1], env, '\n', sep = '')
    
    for (i in 1:length(x)) print.ast(x[[i]], depth + 2L)
    invisible(ast)
}

collapseAST <- function(ast, calls = NULL) {
    if (!inherits(ast, 'ast')) return(ast)
    
    collapse <- is.null(calls) || !names(ast)[1] %in% calls
    
    if (length(ast)) {
        for(i in seq_along(ast)) {
            ast[[i]] <- Recall(ast[[i]], calls = if (collapse) NULL else calls)
        }
    }
    
    
    if (collapse) {
        do.call('call', c(names(ast)[1], unname(ast)), quote = TRUE)
      
    } else {
        ast
    }
    
}

splitExpression <- function(expr, on = '|') {
    dexpr <- deparse(expr)
    exprs <- strsplit(dexpr, split = on)[[1]]
    if (length(exprs) == 1L) return(list(expr))
    on <- unlist(stringr::str_extract_all(dexpr, on))
    exprs <- paste0('(', exprs, ')')
    newexpr <- paste(paste(exprs,  c(on, ''), sep = ' '), collapse = ' ')
               
    newexpr <- parse(text = newexpr)[[1]]
    
    
    ast <- ast(newexpr)
    ast <- collapseAST(ast, calls = on)
    
    exprs <- unlist(unclass(ast))
    exprs <- rapply(exprs, \(x) if (as.character(x[[1]]) == '(') x[[2]] else x)
    
    names(exprs) <- sapply(strsplit(names(exprs), split = '.', fixed = TRUE), \(n) tail(n[n != 'NA'], 1L))
    
    if (rlang::is_formula(expr)) {
        lhs <- rlang::f_lhs(expr)
        env <- rlang::f_env(expr)
        exprs <- lapply(exprs, \(ex) rlang::new_formula(lhs, ex, env))
        
    }
    exprs
}


specialArgs <- function(quos, ...) {
  argLists <- list(...)
  listNames <- names(argLists)
  
  if (length(quos) == 0L) return(c(list(args = list()), argLists))
  
  # divide arguments into those that match one (or more) of the "listNames'
  # and those that do not (the normal "args")
  
  normalArgs <- list()
  
  for (i in 1:length(quos)) {
    exprA <- analyzeExpr(quos[[i]])
    
    
    if (exprA$Type == 'call' && exprA$Head %in% listNames) {
      type <- exprA$Head
      exprA$Head <- 'list'
      
      argLists[[type]] <- c(rlang::eval_tidy(unanalyzeExpr(exprA)), argLists[[type]])
      
    } else {
      normalArgs <- c(normalArgs, 
                      setNames(list(rlang::eval_tidy(quos[[i]])), 
                               names(quos)[i]))
    }
    
  }

  argLists <- lapply(argLists, \(list) list[!duplicated(.names(list))])
  
  c(list(args = normalArgs), argLists)
  
}

# splitExpression <- function(expr, on = '|', keepenv = FALSE) {
#     # This function takes an expression and
#     # and breaks it into separate expressions based on
#     # top level calls to an infix function.
#     if (!is.call(expr)) return(expr)
#     
#     if (rlang::is_formula(expr)) {
#         return(
#             if (rlang::is_quosure(expr)) {
#                 exprs <- Recall(rlang::quo_get_expr(expr), on = on)
#                 if (keepenv) as_quosures(exprs,  rlang::quo_get_env(expr)) else exprs
#             } else {
#                 lhs <- rlang::f_lhs(expr)
#                 rhs <- rlang::f_rhs(expr) 
#                 env <- rlang::f_env(expr)
#                 exprs <- Recall(rhs, on = on)
#                 if (keepenv) lapply(exprs, \(ex) rlang::new_formula(lhs, ex, env = env)) else exprs
#                 
#             })
#     }
#     
#     call <- deparse(expr[[1]])
# 
#     if (!(call %in% c('|', '+', '&', '-', '^') | grepl('%', call))) return(expr)
#     
#     output <- c(Recall(expr[[2]], on = on), if (length(expr) > 2L) Recall(expr[[3]], on = on))
#     needname <- .names(output) == ''
#     names(output)[which(needname)[1]] <- call
#     if (sum(needname) > 1L) names(output)[which(needname)[-1]] <- 'X'
#     
#     
#     #
#     names <- .names(output)
#     bad <- which(!names %in% on & names != 'X')
#     remove <- c()
#     if (length(bad)) {
#         for (i in bad) {
#             collapse <- call(names[i], output[[i]], if (length(output) >= (i + 1)) output[[i + 1]])
#             output[[i]] <- collapse
#             remove <- c(remove, i + 1)
#         }
#     }
#     if (length(remove)) output[-remove] else output
#     
#     
#   
#     
# }






# Strings ----


matched <- function(x, table, nomatch = NA) {
  y <- table[pmatch(x, table)]
  ifelse(is.na(y), nomatch, y)
}

.paste <- function(..., sep = '', collapse = NULL, na.if = any, fill = NA_character_) {
# paste, but smart about NA values
    args <- list(...)
        # return(paste(args[[1]], collapse = collapse))
    # }
    args <- do.call('match_size', lapply(args, `c`))
    nas <- lapply(args, is.na)
    
    args <- Map(`[<-`, args, nas, value = "")
    nas <- apply(do.call('rbind', nas), 2, na.if)
    
    if (length(sep) > 1L && length(args) > 1L) {
      args[1:(length(args) - 1L)] <- Map(\(arg, s) paste0(arg, s), 
                                         args[1:(length(args) - 1L)], 
                                          rep(sep, length.out = length(args) - 1L))
      sep <- ''
    }
    
    
    ifelse(nas, fill, do.call('paste', c(args, list(sep = sep, collapse = collapse))))
}



.glue <- function(..., ifelse = TRUE, sep = ' ', trim = FALSE, envir = parent.frame()) {
  strs <- unlist(list(...))
  ifelses <- stringr::str_extract_all(strs, '<[^>]*\\|[^>]*>')
  ifelses[lengths(ifelses) > 0L] <- lapply(ifelses[lengths(ifelses) > 0L],
                                           \(pairs) {
                                             pairs <- stringr::str_sub(pairs, 2L, -2L) # rid of <>
                                             pairs <- strsplit(pairs, split = '\\|')
                                             
                                             pick <- sapply(pairs, '[', i = if (ifelse) 1 else 2)
                                             pick %|% ""
                                             
                                           })
  
  strs <- Map(function(s, r) {
    while (length(r) > 0) {
      s <- stringr::str_replace(s,  '<[^>]*\\|[^>]*>', r[1])
      r <- r[-1]
    }
    s
  }, 
  strs, ifelses)
  
  strs <- paste(unlist(strs), collapse = sep)
  glue::glue(strs, .envir = envir, .sep = sep, trim = trim)
}

harvard <- function(x, conjunction = '', quote = FALSE, quoteNA = FALSE) {
  x <- as.character(x)
  if (quote) x <- quotemark(x, quoteNA = quoteNA)
  x[is.na(x)] <- "NA"
  if (conjunction != '') conjunction <- paste0(if (length(x) > 2L) ',', ' ', conjunction, ' ')
  glue::glue_collapse(x, sep = ', ', last = conjunction)
}


plural <- function(n, then, els) .ifelse(n > 1 | n == 0, then, els)

quotemark <- function(x, quoteNA = FALSE) if(quoteNA) paste0("'", x, "'") else .paste("'", x, "'")

nthfix <- function(n) {
  affix <- rep('th', length(n))
  mod10 <- abs(n) %% 10
  mod100 <- abs(n) %% 100
  affix[mod10 == 1 & mod100 != 11] <- "st"
  affix[mod10 == 2 & mod100 != 12] <- "nd"
  affix[mod10 == 3 & mod100 != 13] <- 'rd'
  
  paste0(n, affix)
  
}


pmatches <- function(x, table, error = TRUE, callname = 'pmatches') {
  n <- pmatch(x, table, nomatch = 0, duplicates.ok = TRUE)
  
  
  x[n > 0] <- table[n[n > 0]]
  if (error && any(n == 0L)) {
    bad <- harvard(x[n == 0L], 'and', quote = TRUE)
    table <- harvard(table, 'or', quote = TRUE)
    .stop('In a call to {callname}, The <value|values> {bad} <does|do> not unambiguously match anything in the set {table}.',
          ifelse = sum(n == 0L) == 1)
  } else {
   x[n == 0] <- NA
  }
  x
}

pasteordered <- function(order, ..., sep = '', collapse = TRUE) {
    # pastes named elements of ... using order supplied in order
    strs <- list(...) # named vector of strings,
    strs <- strs[lengths(strs) > 0L]
    
    if (length(sep) > 1L) {
      sep <- sep[(order %in% names(strs))[-1]]
      if (length(sep) == 0L) sep <- ''
    }
    
    labels <- names(strs)
    ordered <- strs[pmatch( order, labels, nomatch = 0)]
    
    # do.call('.paste', c(ordered, list(sep = sep)))
    if (collapse) {
      do.call('.paste', c(ordered, sep = list(sep)))
    } else {
      as.data.frame(ordered)
    }
    
}



object2str <- function(object) {
  object <- object[[1]]
    class <- if (is.atomic(object) && !is.table(object)) 'list' else class(object)
    switch(class,
           table = {
             glue::glue("<table: k={length(object)}, n={num2str(sum(object))}>")
           },
           list = {
             if (length(object) < 5) {
               glue::glue('list({harvard(unlist(object), quote = is.character(unlist(object)))})')
             } else {
               glue::glue('list[{num2str(length(object))}]')
             }
             
           },
           paste0('<', class, '>')
            )

        
}

num2str <- function(n, pad = FALSE) format(n, digits = 3, trim = !pad, zero.print = T, big.mark = ',', justify = 'right')


num2print <- function(n, label = NULL, capitalize = FALSE) {
          n_str <- ifelse(n <= 100L, num2word(n, capitalize = capitalize), num2str(n))
          
          if (!is.null(label)) n_str <- paste0(n_str, ' ', label, ifelse(n > 1L, 's', ''))
          
          n_str
}


num2word <- function(num, capitalize = FALSE) {
  words = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
            'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen')
  tens = c('', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')

  

  out = num
  out[num < 101] = unlist(lapply(num[num < 101],
                                 \(n) {
                                   if(n == 100) return('one-hundred')
                                   if(n < 20) { words[n + 1]  } else {
                                    gsub('-zero$', '', paste0(tens[1 + floor(n / 10)], '-', words[n %% 10 + 1]))
                                   }
                                   }
                                 )
                          )
  if (capitalize) stringi::stri_trans_totitle(out) else out
}

padder <- function(strs, sizes = max(nchar(strs)) + 1) {
    if (is.matrix(strs) && length(sizes) == ncol(strs)) {
        # if matrix, do columnwise   
        strs <- t(strs)
        
        strs[] <- stringi::stri_pad_left(c(strs), sizes)
        
        t(strs)
    } else {
        stringi::stri_pad_left(strs, sizes)
    }
    
}



trimTokens <- function(tokmat, max.token.length) {
    # This function  trims strings that are too long, replacing the last
    # three characters before the cuttoff with "..."
    
    toklen  <- nchar(tokmat)
    
    toklen[is.na(toklen)] <- 0L
    
    toolong <- toklen > max.token.length
    tokmat[toolong] <- stringi::stri_sub(tokmat[toolong], from = 0L, max.token.length)
    tokmat[toolong] <- stringi::stri_replace_last_regex(tokmat[toolong], pattern = '...', replacement = '...') # these two ... are not the same! one is RE other is literal
    tokmat[is.na(tokmat)] <- ''
    
    tokmat
    
}


smartPadWrap <- function(str, width, side = 'left') {
    
    
    if (length(str) != 1L) .stop('In call to smartWrap, str argument must be a single character string.')
    
    if (nchar(str) <= width) return(stringr::str_pad(str, width = width, side = side))
    
    strs <- strsplit(str, split = '  *')[[1]]
    
    # while (any(nchar(strs) >= width)) { }
        
    ns <- nchar(strs)
    strs <- if (side == 'left') {
        rev(tapply(strs, rev(cumsum(rev(ns) + 1L) %/% width), paste, collapse = ' '))
    } else {
        tapply(strs, cumsum(ns + 1L) %/% width, paste, collapse = ' ')
    }
    
    
    strs <- stringr::str_pad(strs, width = width, side = side)
    
    paste(strs, collapse = '\n')

}

strPartition <- function(str, split = '/') {
    # split strs into columns
    # if (hasdim(str)) {
    #     output <- do.call('cbind', lapply(1:ncol(str), \(j) ofColumns(str[, j, drop = TRUE])))
    #     rownames(output) <- apply(str, 1, paste, collapse = '/')
    #     
    # } else {
    #     output <- stringi::stri_split_fixed(str, pattern = split, simplify = TRUE)
    #     rownames(output) <- str
    # }
    # 
    # output[output == ""] <- NA_character_
    # output
    
    mat <- stringi::stri_split_fixed(str, pattern = split, simplify = FALSE) %<-matchdim% str
    
    maxdepth <- max(lengths(mat))
    df <- lapply(1:maxdepth,
                               \(i) {
                                   sapply(mat, '[', i = i) %<-matchdim% str
                                   
                               }) 
    
    df <- as.data.frame(lapply(df, I))
    colnames(df) <- c('base', rep('of', ncol(df) - 1L))
    df
    
}



## Token S3 class ----



#' Humdrum tokens
#' @export
token <- function(x, Exclusive = NULL, ...) {
  
  attr(x, 'Exclusive') <- Exclusive
  humdrumRattr(x) <- list(...)
  
  class(x) <- c('token', class(x))
  x
  
  
}


#' @rdname token
#' @export
`[.token` <- function(x, ...) {
  
  result <- NextMethod('[')
  humdrumRattr(result) <- humdrumRattr(x)
  class(result) <- class(x)
  result
  
}

#' @rdname token
#' @export
print.token <- function(x) {
  exclusive <- attr(x, 'Exclusive')
  if (!is.null(exclusive)) {
    cat('**')
    cat(exclusive, sep = '**')
    cat('\n')
    attr(x, 'Exclusive') <- NULL
  }
  class(x) <- setdiff(class(x), 'token')
  if (is.factor(x)) x <- as.character(x)
  attributes(x) <- list()
  print(x, quote = FALSE, na.print = '.')
}

#' @rdname token
#' @export
format.token <- function(x, ...) {
  x[is.na(x)] <- '.'
  x
}

#' @rdname token
#' @export
c.token <- function(...) {
  args <- list(...)
  
  exclusives <- unique(unlist(lapply(args, attr, which = 'Exclusive')))
  
  humattr <- humdrumRattr(args[[1]])
  
  args <- lapply(args, \(x) `class<-`(x, class(x)[-1]))
  result <- do.call('c', args)
  
  humdrumRattr(result) <- humattr
  class(result) <- c('token', class(result))
  result
  
}

## table ----


#' @export
table <- function(..., 
                  exclude = if (useNA == 'no') c(NA, NaN),
                  useNA = 'ifany',
                  dnn = names(list(...)),
                  deparse.level = 1) {
  
  args <- list(...)
  
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) factorize(arg) else arg
                 })
  tab <- do.call(base::table,
                 c(args, list(exclude = exclude, useNA = useNA,
                              dnn = dnn, deparse.level = deparse.level)))
  
  dimnames(tab) <- lapply(dimnames(tab), \(dn) ifelse(is.na(dn), '.', dn))
  
  tab

}

factorize <- function(token) {
  factorizer <- attr(token, 'factorizer')
  class(token) <- setdiff(class(token), 'token')
  if (is.null(factorizer)) return(token)
  
  factorizer(token)
  
}


## Plotting defaults stuff ----

#' @export
plot <- function(x, y, ..., col = sample(flatly[1:5], 1), pch = 16, cex = .5,
                 log = "",
                 xaxis, yaxis) {
  
  if (is.logical(log)) log <- if (log[1]) 'y' else ''
  
  base::plot(x, y, ..., col = col, pch = pch, cex = cex, axes = FALSE)
  
  axis(1, pretty(x, n = 10L, min.n = 5L), las = 1, tick = FALSE)
  axis(2, pretty(y, n = 10L, min.n = 5L), las = 1, tick = FALSE)
}

#' @export
hist <- function(x, ..., col = flatly[1],
                 xaxis, yaxis, 
                 freq = TRUE, probability = !freq) {
  
  
  y <- graphics::hist(x, ..., 
                      col = col, border = flatly[5],
                      axes = FALSE, freq = freq, probability = probability)
  
  axis(1, pretty(x, n = 10L, min.n = 5L), las = 1, tick = FALSE)
  
  yrange <- pretty(if (freq) y$counts else y$density, n = 10L, min.n = 5L)
  axis(2, yrange, tick = FALSE, las = 1)
  invisible(y)
}

#' @export
barplot <- function(height,  ..., 
                    beside = TRUE, col = NULL,
                    ylim = NULL, log = '', border = NA, yaxis, 
                    freq = TRUE, probability = !freq) {
  if (!freq) {
    height <- height / sum(height)
    if (is.null(ylim)) ylim <- c(0, 1)
    yaxis <- pretty(ylim, n = 10L, min.n = 5L)
  }
  
  twoD <- hasdim(height) && length(dim(height)) > 1L
  
  if (is.null(col)) {
    col <- flatly[if (twoD) 1:nrow(height) else 1]
  }
  
  if (is.logical(log)) log <- if (log[1]) 'y' else ''
  
  plot <- graphics::barplot(height, ..., beside = beside, legend.text = twoD, 
                            col = col, ylim = ylim, border = border, axes = FALSE, log = log)
  
  
  if (missing(yaxis)) {
    yran <- range(setdiff(height, 0))
    if (log == 'y') yran <- log10(yran)
    yaxis <- axisTicks(yran, log = log == 'y', nint = 12)
    
  }
  axis(2, yaxis, las = 1, tick = FALSE)
 
  plot 
}

