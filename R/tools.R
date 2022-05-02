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



.glue <- function(..., ifelse = TRUE, sep = ' ', envir = parent.frame()) {
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
    glue::glue(strs, .envir = envir)
}

.stop <- function(..., ifelse = TRUE, sep = ' ') {
    stack <- rlang::call_stack()
    stack <- stack[!duplicated(stack)]
    calls <- rev(sapply(stack, \(x) gsub('[ \t]+', ' ', paste(deparse(x[[3]]), collapse = ' ')))[-1])
    calls <- paste0('\t', strrep(' ', 1:length(calls) * 2), calls)
    
    cat('HumdrumR error in call stack:\n')
    cat(calls, sep = '\n')
    
    message <- .glue(..., ifelse = ifelse, sep = sep, envir = parent.frame())
   
     stop(call. = FALSE, message)
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

applyrows <- function(x, f, ...){
    result <- apply(x, 1, f, ...)
    result <- if (is.null(dim(result))) cbind(result) else t(result)
    
    if (all(dim(result) == dim(x))) dimnames(result) <- dimnames(x)
    result
}
applycols <- function(x, f, ...){
    result <- apply(x, 2, f, ...)
    result <- if (is.null(dim(result))) rbind(result) else result
    
    
    if (all(dim(result) == dim(x))) dimnames(result) <- dimnames(x)
    result
}


#' Shift data within a vector/matrix/data.frame
#' 
#' The `lag` and `lead` functions take input vectors, matrices, or data.frames and shifts their data
#' by `n` indices. 
#' They are similiar to the [data.table::shift] function, but with a few additional options:
#' @param x The input argument. Should be vector (including lists), array, or data.frame
#' @param n The amount to lag/lead the data. 
#' @param fill If `wrap = FALSE` and/or `windows = NULL`, parts of the output are padded with the `fill` argument. Defaults to `NA`.
#' @param wrap If `wrap = TRUE`, data from the end (head or tail) is copied to the other end of the output, "wrapping" the data
#' within the data structure.
#' @param windows A vector or list of vectors, all of th same length as `x`. Lags crossing the boundaries indicated in `windows`
#' are filled.
#' @param margin Arrays and data.frames can be lagged lead in multiple dimensions using the `margin` argument.
#' 
#' @seealso [data.table::shift()]
#' @export
lag <- function(x, n = 1, fill = NA, wrap = FALSE, windows = NULL, ...) UseMethod('lag')

#' @rdname lag
#' @export
lead <- function(x, n, ...) lag(x, -n, ...)


#' @export
lag.data.frame <- function(x, n = 1, margin = 1, fill = NA, wrap = FALSE, windows = NULL) {
         if (length(n) < length(margin)) n <- rep(n, length(margin))
        
         if (1L %in% margin) {
             x[] <- lapply(x, rotate, n = n[margin == 1L], fill = fill, wrap = wrap, windows = windows)
             rown <- rotate(rownames(x), n[margin == 1L], wrap = wrap, pad = '_')
             rown[rown == '_'] <- make.unique(rown[rown == '_'])
             rownames(x) <- rown
         } 
         if (2L %in% margin) {
             cols <- rotate(colnames(x), n[margin == 2L], fill = 'XXX', wrap = wrap)
             x[] <-if (wrap) x[cols] else c(list(XXX = rep(NA, nrow(x))),x)[cols]
         }
    
         x
}
#' @export
lag.default <- function(x, n = 1, fill = NA, wrap = FALSE, windows = NULL) {
          if (length(n) > 1L) .stop('rotate cannot accept multiple rotation values for a vector argument.')
          if (length(x) == 0L || n == 0) return(x)

          if (wrap && n >= length(x))  n <- sign(n) * (abs(n) %% size) #if rotation is greater than size, or negative, modulo
          
          output <- data.table::shift(x, n, type = 'lag', fill = fill)
            
            
          if (wrap) {
            if (n > 0) {
                output[1:n] <- tail(x, n)
            } else{
                output[(length(output) - n):length(output)] <- head(x, abs(n))
            }
          }
            
          if (!is.null(windows)) {
              if (!is.list(windows)) windows <- list(windows)
              if (!all(lengths(windows) == length(x))) .stop('Windows must be same length as input vector.')
              
              boundaries <- Reduce('|', lapply(windows, \(w) w != lag(w, n = n, fill = NA, wrap = FALSE, windows = NULL)))
              output[boundaries] <- fill
          }

          output
}

#' @export
lag.matrix <- function(x, n = 1, margin = 1, fill = NA, wrap = FALSE, windows = NULL) {
    if (length(n) > 1L && length(n) != length(margin)) .stop('rotation and margin args must be the same length.')
    
    
          if ( length(margin) > 1L) {
                    rest.mar <- margin[-1]
                    margin   <- margin[1]

                    rest.rot <- if (length(n) > 1L) n[-1] else n

                    on.exit(return(Recall(output, n = rest.rot, margin = rest.mar, wrap = wrap, fill = fill, windows = windows)))
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

## Vectors ----



allsame <- function(x) length(unique(x)) == 1L

hasdim <- function(x) !is.null(dim(x))



empty <- function(object, len = length(object), dimen = dim(object), value = NA) {
    if (is.atomic(object)) {
        return(if (is.null(dimen)) rep(as(value, class(object)), len) else array(as(value, class(c(object))), dim = dimen))
    }
    
    if (inherits(object, 'struct')) {
        struct <- new(class(object))
        slots <- getSlots(struct)
        if (!is.null(dimen)) len <- prod(dimen)
        setSlots(struct) <- lapply(slots, \(slot) rep(as(value, class(slot)), len))
        
        struct %dim% object
        
    }
    
    
} 

padNA <- function(x, n, before = TRUE, margin = 1L) {
### pad vector with NA
    if (is.null(dim(x)) && margin == 1L) {
        padding <- vectorNA(n - length(x), class(x))
        func <- `c`
    }
    if (!is.null(dim(x)) && margin == 1L) {
        padding <- matrix(as(NA, class(x)), n - nrow(x), ncol(x))
        func <- `rbind`
    }
    if (margin == 2L) {
        padding <- matrix(as(NA, class(x)), nrow(x), n - ncol(x))
        func <- `cbind`
    }
    
    
   if (before) func(padding, x) else func(x, padding)
}

catlists <- function(lists) {
    # this is just like do.call('c', lists) except it never returns NULL
    # and always returns a list.
    # if the lists are all empty, it returns an empty list
    
    out <- do.call('c', lists)
    if(is.null(out)) out <- list() 
    if (!is.list(out)) out <- list(out)
    out
}

# indices

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

tapply_inplace <- function(X, INDEX, FUN = NULL, ...) {
    
    output <- do.call('c', tapply(X, INDEX, FUN, ...))
    indices <- tapply(seq_along(X), INDEX, force) |> unlist()
    
    output[order(indices)]
}

changes <- function(x, value = FALSE) {
    change <- c(TRUE, head(x, -1L) != tail(x, -1L))
    
    if (value) {
        output <- vectorNA(length(x), class(x))
        output[change] <- x[change]
        output
    } else {
        attr(change, 'values') <- x[change]
        change
    }
    
    
    
}


segments <- function(x, reverse = FALSE) {
    change <- if (!is.logical(x)) changes(x) else x
    values <- attr(change, 'values')
    
    if (reverse) change <- rev(change)
    
    seg <- cumsum(change)
    
    if (reverse) {
        seg <- rev(-seg) + max(seg) + 1
    }
    
    attr(seg, 'values') <- values
    
    seg
    
}






#' Propogate data points to "fill" null data.
#' 
#' `fillThru` is a function that allow you to "fill" null values in a vector
#' with non-null values from earlier/later in the same vector.
#' The default, "foward," behavior fills each null value with the previous (lower index) non-null value, if there are any.
#' The `reverse` argument can be used to cause "backeward" filling, where the *next* (higher index) non-null value is used.
#' 
#' Which values are considered "non-null" can be controlled using the `nonnull` argument.
#' The `nonnull` argument can either be a logical vector which is the same length as the input (`x`) argument, a numeric
#' vector of positive indices, or a function which, when applied to `x` returns an appropriate logical/numeric vector.
#' 
#' 
#' 
#' @export
#' @name fillThru
fillThru <- function(x, nonnull = \(x) !is.na(x) & x != '.', reverse = FALSE) {
    
    if (is.function(nonnull)) nonnull <- nonnull(x)
    
    seg <- segments(nonnull, reverse = reverse)
    
    vals <- x[nonnull]
    if (!head(nonnull, 1) && !reverse) vals <- c(x[1], vals)
    if (!tail(nonnull, 1) && reverse) vals <- c(vals, tail(x, 1))
    
    setNames(rep(vals, rle(seg)$lengths), seg)
}

#' @export
#' @name fillThru
fillForward <- function(...) fillThru(..., reverse = FALSE)
#' @export
#' @name fillThru
fillBackwards <- function(...) fillThru(..., reverse = TRUE)

## Dimensions ----

ldim <- function(x) {
    ldim <- if (hasdim(x)) c(0L, dim(x)) else c(length(x), 0L, 0L)
    ldim[4] <- if (ldim[1] == 0L) prod(ldim[-1]) else ldim[1]
    names(ldim) <- c('length', 'nrow', 'ncol', 'size')
    as.data.frame(rbind(ldim))
}

ldims <- function(xs) do.call('rbind', lapply(xs, ldim))

size <- function(x) ldim(x)$size

`%dim%` <- function(x, value) {
    # set the dimensions of x to equal the dimensions of value
    # only works if x is actually the right size!
    if (inherits(x, 'partition')) {
        x[] <- lapply(x, `%dim%`, value =value)
        return(x)
    }
    
    if (is.null(value)) {dim(x) <- NULL; return(x)}
    
    if (size(x) != size(value)) .stop("%dim% is trying to match the dimensions of two objects, but the target object is not the right size.")
    
	dim(x) <- dim(value)
	if (hasdim(x)) {
	    rownames(x) <- rownames(value) 
	    colnames(x) <- colnames(value)
	} else {
	    names(x) <- names(value)
	} 
	
	x
}

dropdim <- function(x) {
    if (is.atomic(x)) {
        c(x) 
    } else {
        dim(x) <- NULL
        dimnames(x) <- NULL
        x
        
    }
    
}

forcedim <- function(ref, ..., toEnv = FALSE, byrow = FALSE) {
    # the same as %dim%, except it forces all the ... to be the same dim as ref (recycling if necessary)
    refdim <- ldim(ref)
    
    targets <- list(...)
    targets <- if (hasdim(ref)) {
        lapply(targets, 
               \(x) {
                   xdim <- ldim(x)
                   if (hasdim(x)) {
                       if (xdim$nrow != refdim$nrow) x <- Repeat(x, length.out = refdim$nrow, margin = 1L)
                       if (xdim$ncol != refdim$ncol) x <- Repeat(x, length.out = refdim$ncol, margin = 2L)
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

match_size <- function(..., size.out = max, margin = 1, toEnv = FALSE, recycle = TRUE) {
          stuff   <- list(...)
          if (is.function(size.out) && (length(stuff) <= 1L || Reduce('identical', lapply(stuff, ldim)))) return(invisible(stuff))
          
          recycle <- rep(recycle, length.out = length(margin))
          notnull <- !sapply(stuff, is.null)
          
          if (is.function(size.out)) {
                    sizes <- lapply(stuff[notnull],
                                    \(thing) {
                                              dim <- dim(thing)
                                              if (is.null(dim)) {
                                                  if (length(margin) == 1L) length(thing) else c(length(thing), 1L)
                                              } else {
                                                  dim[margin]
                                              }
                                    })
                    
                    size.out <- apply(do.call('rbind', sizes), 2, size.out)
          }
          
          for (i in seq_along(margin)) {
              stuff[notnull] <- if (recycle[i]) {
                 lapply(stuff[notnull], Repeat, length.out = size.out[i], margin = margin[i])
              } else {
                 lapply(stuff[notnull], padNA, before = FALSE, n = size.out[i], margin = margin[i])
                  
              }
          }
          if (toEnv) list2env(stuff[names(stuff != '')], envir = parent.frame(1))
          
          if (toEnv) invisible(stuff) else stuff
          
}

match_size2 <- function(..., toEnv = FALSE, byrow = FALSE) {
    objects <- list(...)
    
    nodim <- !sapply(objects, hasdim)
    
    sizes <- vector('list', length(objects))
    sizes[nodim]  <- lapply(objects[nodim],  length)
    sizes[!nodim] <- lapply(objects[!nodim], dim)
    
    if (all(nodim)) {
        size <- max(unlist(sizes))
        objects <- lapply(objects, rep, length.out = size)
    } else {
        
        size <- apply(sizes, 1, max)
        browser()
        objects[nodim] <- lapply(objects[nodim], \(x) matrix(rep(x, length.out = prod(size)), nrow = size[1], ncol = size[2]))
        
        objects[!nodim] <- lapply(objects[!nodim], 
                                  \(x) {
                                      x <- Repeat(x, length.out = size[1], margin = 1)
                                      x <- Repeat(x, length.out = size[2], margin = 2)
                                      x
                                  })
        
    }
    objects
}


Repeat <- function(x, ..., margin = 1L) {
# Smart version of base::repeat which replicates things in any
# dimension
  if (is.null(dim(x))) {
     # out <- do.call('rep', list(x = x, ...)) 
     out <- if (margin == 1L) do.call('rep', list(x = x, ...)) else x
  } else {
      
    out <- if (margin == 1) {
        x[rep(seq_len(nrow(x)), ...), , drop = FALSE]
    } else {
        x[ , rep(seq_len(ncol(x)), ...), drop = FALSE]
    }

    if (is.data.frame(x)) out <- as.data.frame(out, stringsAsFactors = FALSE)

    if (!is.null(rownames(out)))  rownames(out) <- make.unique(rownames(out))
    if (!is.null(colnames(out)))  colnames(out) <- make.unique(colnames(out))
  }
  out
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
    output %dim% bool
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



.switch <- function(x, groups, ..., parallel = list(), defaultClass = 'character') {
    # this function plays a key role in Exclusive dispatch
    exprs <- rlang::enexprs(...)
    
    missing <- sapply(exprs, rlang::is_missing)
    
    names(exprs)[.names(exprs) == ""] <- 'rest'
    switch <- names(exprs)
    rest <- any(switch == 'rest')
    
    switchg <- segments(!missing)
    switchg <- tapply(switch, switchg, paste, collapse = ' | ')[as.character(switchg)]
    # used to group missing arguments in with the next non missing expression
    
    exprs <- exprs[!missing]
    exprs <- exprs[switch %in% c(groups, if (rest) 'rest')]
    
    
    if (length(exprs) == 0L) return(if (rest) x else vectorNA(length(x), defaultClass))
    
    groupvec <- c(if (rest) 'rest' else 'nomatch', switchg)[match(groups, switch, nomatch = 0) + 1] 
    
    # this maps unkown exclusives to "nomatch"
    grouped <- lapply(c(list(i = seq_along(x), 
                             group = groupvec,
                             x = x), 
                        parallel), split, f = groupvec )
    
    exprs <- setNames(exprs[names(grouped$group)], names(grouped$group)) 
    # makes sure there is a "nomatch" expr, and they are in right order
    frame <- parent.frame(1)
    results <- do.call('Map', 
                       c(\(expr, ...) {
                           exclgroup <- list(...)
                           
                           if (is.null(expr)) return(exclgroup$x)
                           
                           rlang::eval_tidy(expr, exclgroup, env = frame)
                       }, 
                       c(list(exprs), 
                         grouped)))
    results$nomatch <- vectorNA(length(results$nomatch), class(results[names(results) != 'nomatch'][[1]]))
    results <- unstick(do.call('c', unname(results )))
    
    i <- order(unlist(grouped$i))
    results <- results[i]
    
    
    results
}


# Math ----

pmaxmin <- function(x, min = -Inf, max = Inf) as(pmax(pmin(x, max), min), class(x))

is.whole <- function(x) x %% 1 == 0

reduce_fraction <- function(n, d) {
    # Used by rational initialize method
    gcds <- do(gcd, list(n, d))
    
    num <- as.integer(n / gcds)
    den <- as.integer(d / gcds)
    list(Numerator = num, Denominator = den)
}

gcd <- function(...) {
    x <- list(...)
    x <- x[lengths(x) > 0]
    if (length(x) == 1L) return(x[[1]])
    if (length(x) == 0L) return(numeric(0))
    
    na <- Reduce('|', lapply(x, is.na))
    output <- vectorNA(length(x[[1]]), class(x[[1]]))
    output[!na] <- Reduce(.gcd, lapply(x, '[', !na))
    output
}

.gcd <- function(x, y) {
    r <- x %% y
    
    notyet <- r > 0
    if (any(notyet)) y[notyet] <- Recall(y[notyet], r[notyet])
    y
}

lcm <- function(...) {
    x <- list(...)
    x <- x[lengths(x) > 0]
    if (length(x) == 1L) return(x[[1]])
    if (length(x) == 0L) return(numeric(0))
    
    na <- Reduce('|', lapply(x, is.na))
    
    output <- vectorNA(length(x[[1]]), class(x[[1]]))
    output[!na] <- Reduce(.lcm, lapply(x, '[', !na))
    output
}

.lcm <- function(x, y) {
    abs(x * y) / .gcd(x, y)
}

# modulo starting from 1
`%1%` <- function(e1, e2) ((e1 - 1L) %% e2) + 1L

#### calculus

# sigma (integrate) and delta (derive) should be perfect inverses, 
# so long as their skip arguments are the same, and the scalar
# argument is NULL

#' Interval "calculus"
#' @name intervalCalculus
#' @export 
integrate <- function(intervals, skip = list(is.na)) {
    intmat <-  if (hasdim(intervals)) intervals else cbind(intervals) 
    
    skip <- Reduce('any', lapply(skip,  \(f) f(intmat)))
    
    lapply(1:ncol(intmat),
           \(j) {
               intmat[!skip[ , j], j] <<- cumsum(intmat[!skip[ , j], j])
           }
    ) 
    intmat %dim% intervals

}

#' @name intervalCalculus
#' @export 
sigma <- integrate

#' @name intervalCalculus
#' @export 
derive <- function(intervals, skip = list(is.na)) {
    intmat <-  if (hasdim(intervals)) intervals else cbind(intervals) 
    
    skip <- Reduce('any', lapply(skip,  \(f) f(intmat)))
    
    lapply(1:ncol(intmat),
           \(j) {
               intmat[which(!skip[ , j])[-1], j] <<- diff(intmat[!skip[ , j], j])
           }
    ) 
    intmat %dim% intervals
    
}

#' @name intervalCalculus
#' @export 
delta <- derive

#' @name intervalCalculus
#' @export 
calculus <- function(x, n, skip = list(na)) {
    n <- as.integer(n[1])
    if (n == 0L) return(x)
    
    if (n > 0L) {
        Recall(derive(x, skip), n - 1L)
    } else {
        Recall(integrate(x, skip), n + 1L)
    }
}

#' @export
expand <- function(x) {
    .ifelse(x >=0, ceiling(x), -ceiling(abs(x)))
}

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


applyExpr <- function(expr, predicate, func, applyTo = c('call', 'atomic', 'symbol')) {
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
    
    unlist(applyExpr(expr, applyTo = applyTo,
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


analyzeExpr <- function(expr) {
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
    
    exprA$Type <- if (is.atomic(expr)) 'atomic' else {if (is.call(expr)) 'call' else 'symbol'}
    exprA$Class <- if(exprA$Type == 'atomic') class(expr)
    exprA$Head <- switch(exprA$Type,
                         call = as.character(expr[[1]]),
                         atomic = expr,
                         symbol = as.character(expr))
    exprA$Args <- if (exprA$Type == 'call') as.list(expr[-1]) else list()

    
    
    exprA
    
    
    
}

unanalyzeExpr <- function(exprA) {
    expr <- switch(exprA$Type,
                   call =  do.call('call', c(exprA$Head, exprA$Args), quote = TRUE),
                   atomic = exprA$Head,
                   symbol = rlang::sym(exprA$Head))
    
    if (exprA$Form != 'expression') {
        expr <- if (exprA$Form == 'formula') {
            rlang::new_formula(exprA$LHS, expr, env = exprA$Environment)
        }   else {
            rlang::new_quosure(expr, env = exprA$Environment)
        }
    }
    
    expr
}



modifyExpression <- function(expr, predicate = \(...) TRUE, func, applyTo = 'call', stopOnHit = TRUE) {
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
            if (!is.null(exprA$Args[[i]])) exprA$Args[[i]] <- Recall(exprA$Args[[i]], 
                                                                     func = func, 
                                                                     predicate = predicate, 
                                                                     stopOnHit = stopOnHit,
                                                                     applyTo = applyTo)
        }
       
    }
    
    unanalyzeExpr(exprA)
    
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






partialApply <- function(func, ...) {
    fargs <- fargs(func)
    newargs <- list(...)
    
    hits <- names(fargs)[names(fargs) %in% names(newargs)]
    fargs[hits] <- newargs[hits]
    
    formals(func) <- fargs
    func
    
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


## Building smart functions ----



overdot <- function(call) {
    # this function removes redundant arguments 
    # in ldots 
    
    call <- rlang::enexpr(call)
    
    dots <- eval(quote(list(...)), envir = parent.frame())
    
    if (length(dots) > 0) {
        call <- call[!sapply(call, \(x) deparse(x) == "...")]
        call <- call[!names(call) %in% names(dots)]
        
        call <- append2expr(call, dots)
    }    
    eval(call, envir = parent.frame())
    
    
}


# Checking arguments ----


checkArg <- function(arg,  argname, callname = NULL, 
                     atomic = FALSE,
                     valid, validoptions = NULL, min.length = 1L, max.length = 1L, warnSuperfluous = TRUE, classes = NULL) {
    # arg a argument to check
    # 
    if (length(sys.calls()) > 6L) return(arg) 
    
    argNames <- if (length(arg) > 1L) paste0('c(', glue::glue_collapse(quotemark(arg), sep = ', '), ')') else quotemark(arg)
    callname <- if (is.null(callname)) '' else glue::glue("In the call humdrumR::{callname}({argname} = {argNames}): ")
    
    if (atomic && !is.atomic(arg)) .stop(callname, "The {argname} argument must be an 'atomic' vector.")
    
    if (length(arg) <  min.length) .stop(callname, "length(args)} is too few '{argname}' arguments.")
    if (length(arg) >  max.length) .stop(callname, "length(args)} is too many '{argname}' arguments.")
    
    
    if (!is.null(classes) && !any(sapply(classes, inherits, x = arg))) {
        classNames <- glue::glue_collapse(classes, sep = ', ', last =  ', or ')
        .stop(callname, "The '{argname}' argument must inherit the class <{classNames}>, but you have provided a <{class(arg)}> argument.")
    }
    
    if (!missing(valid) && !is.null(valid)) {
        ill <- !valid(arg)
        
        if (any(ill)) {
            if (is.null(validoptions)) {
                .stop(callname, "{arg} is not a valid value for the {argname} argument.")
            } else {
                case <- glue::glue(plural(sum(ill), " are not valid {argname} values. ", "is not a valid {argname} value. "))
                illNames <- glue::glue_collapse(quotemark(arg[ill]), sep = ', ', last = if (sum(ill) > 2) ', and ' else ' and ')
                legalNames <-  paste0(glue::glue_collapse(quotemark(valid), sep = ', ', last = if (sum(ill) > 2) ', and ' else ' and '), '.')
                
                message <- list(callname, illNames, case, 'Valid options are ', legalNames)
                
                do.call(if (warnSuperfluous && any(!ill)) 'warning' else '.stop', message)
            }
            
        }
        
        arg[!ill]
    } else {
        arg
    }
    
    
    
}



checkNumeric <- function(x, argname, callname = NULL, minval = -Inf, maxval = Inf, ...) {
    checkArg(x, argname = argname, callname = callname,
             classes = c('numeric', 'integer'), 
             valid = \(arg) arg >= minval & arg <= maxval,
             ...)
}
checkInteger <- function(x, argname, callname = NULL, minval = -Inf, maxval = Inf, ...) {
    checkArg(x, argname = argname, callname = callname,
             classes = c('integer'), 
             valid = \(arg) arg >= minval & arg <= maxval & !is.double(x),
             ...)
}
checkCharacter <- function(x, argname, callname = NULL, allowEmpty = TRUE, ...) {
    checkArg(x, argname = argname, callname = callname, classes = c('character'),
             valid = if (!allowEmpty) \(arg) arg != "", ...)
}
checkLogical <- function(x, argname, callname = NULL, ...) {
    checkArg(x, argname = argname, callname = callname, classes = c('logical'), ...)
}

checkTF <- function(x, argname, callname) checkArg(x, valid = \(arg) !is.na(arg), 
                                                   validoptions = c(TRUE, FALSE), argname, callname, max.length = 1L, classes = 'logical')
checkTFs <- function(..., callname = NULL) {
    args <- list(...)
    mapply(checkTF, args, names(args), MoreArgs = list(callname = callname))
}

checkhumdrumR <- function(x, callname, argname = 'humdrumR') {
    if (!is.humdrumR((x))) .stop("In the call {callname}({argname} = _), the argument {argname} must be a humdrumR object.")      
}

checkTypes <- function(dataTypes, callname, argname = 'dataTypes') {
    dataTypes <- unique(unlist(strsplit(dataTypes, split = '')))
    checkArg(dataTypes,
             valid = \(arg) arg %in% c('G', 'L', 'I', 'M', 'D', 'd', 'P'),
              validoptions = c('G', 'L', 'I', 'M', 'D', 'd', 'P'),
              argname, callname, warnSuperfluous = TRUE, 
              min.length = 1L, max.length = 7L,
              classes = "character")
}

# Strings ----


matched <- function(x, table) table[pmatch(x, table)]

.paste <- function(..., sep = '', collapse = NULL, na.if = any, fill = NA_character_) {
# paste, but smart about NA values
    args <- list(...)
        # return(paste(args[[1]], collapse = collapse))
    # }
    
    args <- do.call('match_size', lapply(args, `c`))
    nas <- lapply(args, is.na)
    
    args <- Map(`[<-`, args, nas, value = "")
    nas <- apply(do.call('rbind', nas), 2, na.if)
    ifelse(nas, fill, do.call('paste', c(args, list(sep = sep, collapse = collapse))))
}


pasteordered <- function(order, ..., sep = '') {
    # pastes named elements of ... using order supplied in order
    strs <- list(...) # named vector of strings,
    strs <- strs[lengths(strs) > 0L]
    
    labels <- names(strs)
    ordered <- strs[pmatch( order, labels, nomatch = 0)]
    
    # do.call('.paste', c(ordered, list(sep = sep)))
    do.call('.paste', c(ordered, sep = sep))
    
}


plural <- function(n, then, els) .ifelse(n > 1 | n == 0, then, els)

quotemark <- function(x) if (is.character(x)) paste0('"', x, '"') else x

nthfix <- function(n) {
    affix <- rep('th', length(n))
    mod10 <- abs(n) %% 10
    mod100 <- abs(n) %% 100
    affix[mod10 == 1 & mod100 != 11] <- "st"
    affix[mod10 == 2 & mod100 != 12] <- "nd"
    affix[mod10 == 3 & mod100 != 13] <- 'rd'
    
    paste0(n, affix)
     
}


object2str <- function(object) {
    class <- class(object)[1]
    if (class == 'table') {
        n <- sum(object)
        n <- if (n > 1000)  {
            paste0('~', gsub('e\\+0?', 'e', formatC(n, format='e', digits = 0)))
        } else {
            paste0('=', n)
        }
        glue::glue("<table: k={length(object)}, n{n}>")
    } else {
        paste0('<', class, '>')
        
    }
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
    
    mat <- stringi::stri_split_fixed(str, pattern = split, simplify = FALSE) %dim% str
    
    maxdepth <- max(lengths(mat))
    df <- lapply(1:maxdepth,
                               \(i) {
                                   sapply(mat, '[', i = i) %dim% str
                                   
                               }) 
    
    df <- as.data.frame(lapply(df, I))
    colnames(df) <- c('base', rep('of', ncol(df) - 1L))
    df
    
}




