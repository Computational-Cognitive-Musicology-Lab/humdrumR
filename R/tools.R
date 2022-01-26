### Null and NA values ----

with... <- function(arg, expression, orelse = NULL) {
    expression <- rlang::enexpr(expression)
    
    if (eval(rlang::expr(hasArg(!!arg)), envir = parent.frame())) {
        expression <- substituteName(expression, setNames(list(rlang::expr(list(...)[[!!arg]])), arg))
        eval(expression, envir = parent.frame())
    } else {
        orelse
    }
    
}


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

`%maybe%` <- function(e1, e2) if (is.null(e1)) e2 else e1
`%fmap%` <- function(e1, e2) {
    if (is.null(e1)) return(NULL)
    
    e2 <- rlang::enexpr(e2)
    
    if (length(e2) == 1L) {
        e2 <- eval(e2, envir = parent.frame(1))
        if (is.function(e2)) e2(e1) else e2
        
    } else {
        e1 <- rlang::enexpr(e1)
        eval(substituteName(e2, list(. = e1)))
        
    }
    
}
`%==%` <- function(e1, e2) !is.null(e1) &&  e1 == e2 
`%iN%` <- function(e1, e2) !is.null(e1) && e1 %in% e2 

`%allin%` <- function(e1, e2) !is.null(e1) && all(e1 %in% e2)

`%if%` <- function(e1, e2) {
    expr <- rlang::enexpr(e2)
    
    if (!is.null(e1) && e1) eval(expr, parent.frame()) else NULL
}
truthy <- function(x) !is.null(x) && length(x) > 0L && ((!is.logical(x) && !(length(x) == 1 && x[1] == 0)) || (length(x) == 1L & x[1]))
true <- function(x) is.logical(x) && x[1]
false <- function(x) is.null(x) || is.logical(x) && !x[1]
# these two functions allow us to test if a variable is 
# a logical TRUE or FALSE, but not give an error if
# the variable is NOT logical.
# (If the variable is NOT logical) it always returns FALSE


`%!if%` <- function(e1, e2) {
    expr <- rlang::enexpr(e2)
    
    if (!is.null(e1) && !e1) eval(expr, parent.frame()) else NULL
}

`%if<-%` <- function(e1, e2) {
    var <- rlang::expr_text(rlang::enexpr(e1))
    
    if (!is.null(e2)) assign(var, e2, envir = parent.frame())
} 

`%!<-%` <- function(e1, e2) {
    # this assigns e2 to e1, UNLESS e1 is NULL
   x <- rlang::enexpr(e1)
   
    if (is.null(e1)) return(invisible(NULL))
    
   value <- rlang::enexpr(e2)
   
   eval(rlang::expr(!!x <- !!value), envir = parent.frame())
    
}


###

`%class%` <- function(object, newclass){
  if (!newclass %in% class(object)) class(object) <- append(newclass, class(object))
  object
}

popclass <- function(object) `class<-`(object, class(object)[-1])

fargs <- function(func)  args(func) %fmap% formals  



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


insert <- function(x, i, values) {
    if (is.logical(i)) i <- which(i)
    if (length(i) == 0L || length(values) == 0L) return(x)
    if (length(i) > 1L) .stop("In your call to humdrumR:::insert, you can't insert values into multiple indices.")
    if (i <= 0) .stop("In your call to humdrumR:::insert, you can't insert at a negative index.")
    x <- x[-i]
    
    append(x, values, i - 1)
    
    
}

.glue <- function(..., ifelse = TRUE, sep = ' ', envir = parent.frame()) {
    strs <- unlist(list(...))
    ifelses <- stringr::str_extract_all(strs, '<[^>]*\\|[^>]*>')
    ifelses <- lapply(ifelses,
                      function(pairs) {
                          pairs <- stringr::str_sub(pairs, 2L, -2L) # rid of <>
                          pairs <- strsplit(pairs, split = '\\|')
                          
                          pick <- sapply(pairs, '[', i = if (ifelse) 1 else 2)
                          ifelse(is.na(pick), '', pick)
                          
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
    message <- .glue(..., ifelse = ifelse, sep = sep, envir = parent.frame())
   
     stop(call. = FALSE, message)
}

### Names ----

.names <- function(x) { #:: a -> character
    # gets names with no possibility of NULL
    # is there are no names, returns vector of empty strings
    nam <- names(x)
    
    if(is.null(nam)) nam <- character(length(x))
    
    nam
}

allnamed <- function(x) { !is.null(names(x)) && !any(names(x) == '')}

### Arrays/Vectors

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


#' @export
rotate <- function(obj, rotation = 1, wrap = FALSE, pad = NA, ...) UseMethod('rotate')



#' @export
rotate.data.frame <- function(df, rotation = 1, margin = 1, wrap = FALSE, pad = NA) {
         df[] <- rotate.matrix(as.matrix(df), rotation = rotation, margin = margin, wrap = wrap, pad = pad)
         Map(rotation, margin,
             f = function(r, m) {
                              newnames <- rotate(dimnames(df)[[m]], r, wrap = wrap, pad = '')
                              dimnames(df)[[m]] <<- make.names(newnames, TRUE)
                       }
             )
         df
}
#' @export
rotate.default <- function(obj, rotation = 1, wrap = FALSE, pad = NA) {
          rotation <- rotation[1]

          size <- length(obj)
          rotation <- sign(rotation) * (abs(rotation) %% size) #if rotation is greater than size, or negative, modulo
          if (rotation == 0L) return(obj)

          ind <- seq_len(size) - rotation

          if (wrap) ind <- ((ind - 1L) %% size) + 1L else ind[ind > size | ind < 1] <- NA

          output <- obj[ind]

          if (!is.na(pad)) output[which(is.na(ind))] <- pad

          output
}

#' @export
rotate.matrix <- function(mat, rotation = 1, margin = 1, wrap = FALSE, pad = NA) {
          if ( margin %len>% 1L ) {
                    rest.mar <- margin[-1]
                    margin   <- margin[1]

                    rest.rot <- if (rotation %len>% 1L ) rotation[-1] else rotation

                    on.exit(return(Recall(output, rotation = rest.rot, margin = rest.mar, wrap = wrap))        )
          }
          rotation <- rotation[1]

          size <- dim(mat)[margin]
          rotation <- sign(rotation) * (abs(rotation) %% size) #if rotation is greater than size, or negative, modulo
          if (rotation == 0) return(mat)

          ind <- seq_len(size) - rotation

          if (wrap) ind <- ind %mod% size else ind[ind > size | ind < 1] <- NA

          calls <- alist(mat, i = , j = )
          calls[[margin + 1]] <- ind

          output <- do.call('[', calls)

          if (!is.na(pad)) {
                    calls[[margin + 1]] <- which(is.na(ind))
                    calls$value <- pad
                    calls[[1]] <- output
                    output <- do.call('[<-', calls)

          }

          output
}

### Vectors ----

changeifmap <- function(x, table) {
    .ifelse(x %in% names(table), table[match(x, names(table))], x)
    
}

allsame <- function(x) length(unique(x)) == 1L

hasdim <- function(x) !is.null(dim(x))

vectorNA <- function(n, mode = 'character') rep(as(NA_integer_, Class = mode), n)

empty <- function(object, len = length(object), dimen = dim(object), value = NA) {
    if (is.atomic(object)) {
        return(if (is.null(dimen)) rep(as(value, class(object)), len) else array(as(value, class(c(object))), dim = dimen))
    }
    
    if (inherits(object, 'struct')) {
        struct <- new(class(object))
        slots <- getSlots(struct)
        if (!is.null(dimen)) len <- prod(dimen)
        setSlots(struct) <- lapply(slots, function(slot) rep(as(value, class(slot)), len))
        
        struct %dim% object
        
    }
    
    
} 

padNA <- function(x, n, before = TRUE, margin = 1L) {
### pad vector with NA
    if (is.null(dim(x)) && margin == 1L) {
        padding <- vector(class(x), n - length(x))
        func <- `c`
    }
    if (margin == 1L) {
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
          intervals <- findInterval(x, sortedwhere, )
          hits <- ifelse(intervals == 0,
                         if (direction %in% c(2,4)) Inf else 1,
                         if (direction == 1) {
                                   intervals + mapply(FUN = function(a,b) which.min(c(a,b)) - 1,
                                                      abs(x - sortedwhere[intervals]),
                                                      abs(x - sortedwhere[intervals + 1]))
                         } else {
                                   if (direction %in% c(3, 5))  intervals + 1  else intervals
                         })
          sortedwhere[hits]
          
}


locate <- function(x, table) {
    if (is.null(dim(table)) || length(dim(x)) == 1) {
        setNames(lapply(x, function(val) which(table == val)), x)
    } else {
        apply(x, 1, 
              function(val) {
                  which(Reduce('&', Map('==', table, val)))
                  
                  })
    }
}

locate.uniq <- function(x) {
    locate(x, unique(x))
    

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
    
    output <- tapply(X, INDEX, FUN, ...) |> unlist()
    indices <- tapply(seq_along(X), INDEX, force) |> unlist()
    
    output[order(indices)]
}


segments <- function(x, reverse = FALSE) {
    if (!is.logical(x)) x <- c(TRUE, head(x, -1L) != tail(x, -1L))
    if (reverse) x <- rev(x)
    
    x <- cumsum(x)
    
    if (reverse) {
        x <- rev(-x) + max(x) + 1
    }
    
    x
    
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
fillThru <- function(x, nonnull = function(x) !is.na(x) & x != '.', reverse = FALSE) {
    
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

##### Dimensions ----
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
               function(x) {
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
               function(x) {
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





`%@%` <- function(x, slot) {
    slot <- rlang::expr_text(rlang::enexpr(slot))
    slotnames <- slotNames(x)
    slot <- slotnames[pmatch(slot, slotnames, duplicates.ok = TRUE)]
    slot(x, slot) %dim% x
    
}
## My versions of some standard utitilies

match_size <- function(..., size.out = max, margin = 1, toEnv = FALSE, recycle = TRUE) {
          stuff   <- list(...)
          if (is.function(size.out) && (length(stuff) <= 1L || Reduce('identical', lapply(stuff, ldim)))) return(invisible(stuff))
          
          recycle <- rep(recycle, length.out = length(margin))
          notnull <- !sapply(stuff, is.null)
          
          if (is.function(size.out)) {
                    sizes <- lapply(stuff[notnull],
                                    function(thing) {
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
        objects[nodim] <- lapply(objects[nodim], function(x) matrix(rep(x, length.out = prod(size)), nrow = size[1], ncol = size[2]))
        
        objects[!nodim] <- lapply(objects[!nodim], 
                                  function(x) {
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


ifif <- function(cond1, cond2, ...) {
    # cond1 <- is.null(cond1) || cond1
    # cond2 <- is.null(cond2) || cond2
    # if (!null) {cond1 <- !cond1; cond2 <- !cond2}
    # 
	vals <- rlang::enexprs(...) 

	switch <- sum(c(2,1) * c(cond1, cond2))
	# 0 = neither, 1 = cond2, 2 = cond1, 3 = both

	if (hasArg('both') & switch == 3L) return(eval(vals$both, envir = parent.frame()))
	if (hasArg('and')  & switch == 3L) return(eval(vals$and, envir = parent.frame()))
	if (hasArg('or')   & switch >  0L) return(eval(vals$or, envir = parent.frame()))
	if (hasArg('xor1') & switch == 2L) return(eval(vals$xor1, envir = parent.frame()))
	if (hasArg('xor2') & switch == 1L) return(eval(vals$xor2, envir = parent.frame()))

	if (hasArg('.else')) eval(vals$.else, envir = parent.frame()) else NULL
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

captureSymbols <- function(expr) {
    if (is.atomic(expr)) return(setNames(list(rlang::eval_tidy(expr)),
                                         tempvar('atom', asSymbol = FALSE)))
    if (!is.call(expr) ) {  return(setNames(list(rlang::eval_tidy(expr)), 
                                            rlang::expr_text(expr))) }
    if (as.character(expr[[1]]) == ":") return(setNames(list(rlang::eval_tidy(expr)),
                                                        tempvar(':', asSymbol = FALSE)))
    
    result <- list()
    for (i in 2:length(expr)) {
        result <- c(result, Recall(expr[[i]]))
    }
    
    result
}


.switch <- function(x, groups, ..., parallel = list()) {
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
    
    
    if (length(exprs) == 0L) return(if (rest) x else vectorNA(length(x), class(x)))
    
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
                       c(function(expr, ...) {
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

reduce_fraction <- function(n ,d) {
    # Used by rhythmInterval initialize method
    gcds <- gcd(n, d)
    
    list(Numerator = as.integer(n / gcds), Denominator = as.integer(d / gcds))
}

gcd <- function(x, y) {
    # Used by reduce_fraction
    r <- x %% y
    ifelse(r, Recall(y, r), y)
}

## new numeric representations ####

#### decimal ####
# this is just an extension of numeric to understand my fraction and rational representations

#' @export
as.decimal <- function(x, ...) UseMethod('as.decimal')
#' @export
as.decimal.character <- function(x) {
    x[grepl('[^0-9.%/\\(\\)-]', x)] <- NA
    as.decimal.fraction(x)
}
#' @export
as.decimal.numeric <- as.numeric
#' @export
as.decimal.rational <- function(x) (x$Numerator / x$Denominator) %dim% x[[1]]
#' @export
as.decimal.fraction <- function(x) {
    exprs <- parse(text = stringi::stri_replace_all_fixed(x, '%', '/'))
    sapply(exprs, eval) %dim% x
}

#### rational ####
# represent rational numbers as list of numerator and denominator

#' @export
as.rational <- function(x, ...) UseMethod('as.rational') 
#' @export
as.rational.character <- function(x) as.rational.numeric(as.decimal.character(x))
#' @export
as.rational.fraction <- function(x) as.rational.numeric(as.decimal.fraction(x))
#' @export
as.rational.rational <- force
#' @export
as.rational.numeric <- function(x) {
    frac <- attr(MASS::fractions(x, cycles = 15), 'fracs')
    frac <- stringi::stri_split_fixed(frac, '/', simplify = TRUE)
    if (ncol(frac) == 1L) frac <- cbind(frac, '1')
    
    num <- as.integer(frac[ , 1])
    den <- as.integer(frac[ , 2])
    
    den[is.na(den)] <- 1L
    
    list(Numerator = num %dim% x, Denominator = den %dim% x) %class% 'rational'
}

#### fraction ####
# rational numbers as character string

#' @export
as.fraction <- function(x, sep, ...) UseMethod('as.fraction')
#' @export
as.fraction.character <- function(x, sep = '/') as.fraction.rational(as.rational.character(x), sep = sep) %dim% x
#' @export
as.fraction.rational  <- function(x, sep = '/') .paste(x$Numerator, x$Denominator, sep = sep) 
#' @export
as.fraction.numeric   <- function(x, sep = '/') as.fraction.rational(as.rational.numeric(x), sep = sep) %dim% x
#' @export
as.fraction.fraction  <- function(x, sep = '/') as.fraction.rational(as.rational.fraction(x), sep = sep) %dim% x %class% 'fraction'
#' @export
print.rational <- function(x) print(as.fraction(x))

#### calculus

# sigma (integrate) and delta (derive) should be perfect inverses, 
# so long as their skip arguments are the same, and the scalar
# argument is NULL

#' Interval "calculus"
#' @name intervalCalculus
#' @export 
integrate <- function(intervals, skip = list(is.na)) {
    intmat <-  if (hasdim(intervals)) intervals else cbind(intervals) 
    
    skip <- Reduce('any', lapply(skip,  function(f) f(intmat)))
    
    lapply(1:ncol(intmat),
           function(j) {
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
    
    skip <- Reduce('any', lapply(skip,  function(f) f(intmat)))
    
    lapply(1:ncol(intmat),
           function(j) {
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


# bitwise tools

ints2bits <- function(n, nbits = 8) {
    mat <- t(sapply(n, function(x) as.integer(intToBits(x))))[ , 1:nbits, drop = FALSE]
    
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
#     posmat <- t((sapply(positive, function(x) as.integer(intToBits(x)))))[ , 1:nbits, drop = FALSE]
#     negmat <- t((sapply(negative, function(x) as.integer(intToBits(x)))))[ , 32:(32-nbits + 1), drop = FALSE]
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



### Metaprogramming ----

applyExpr <- function(ex, func, rebuild = TRUE, ignoreHead = TRUE) {
  # helper function
  accum <- c()
  
  if (length(ex) <= 1L) {
    return(func(ex))
  } else {
    for (i in (2 - !ignoreHead):length(ex)) {
      missing <- rlang::is_missing(ex[[i]]) || is.null(ex[[i]])
      
      if (!missing) {
        out <- Recall(ex[[i]], func, rebuild = rebuild) 
        
        if (rebuild) {
          ex[[i]] <- out 
        } else {
          accum <- c(accum, out) 
        }
      }
    }
  }
  
  if (rebuild) ex else accum
}

apply2ExprAsString <- function(func, ...) {
  function(expr) {
    str <- func(deparse(expr), ...)
    parse(text = str)[[1]]
    
  }
}


namesInExprs <- function(names, exprs) {
    unique(unlist(lapply(exprs, namesInExpr, names = names)))
}

namesInExpr <- function(names, expr) {
    ## This function identifies which, if any,
    ## of a character vector ("names") are referenced as a name 
    ## (not including things called as functions) in an expression 
    ## (or rhs for formula).
    if (rlang::is_formula(expr)) expr <- rlang::f_rhs(expr)
    
    applyExpr(expr, rebuild = FALSE,
              function(ex) {
                  exstr <- deparse(ex)
                  match <- names[pmatch(exstr, names)]
                  if (is.na(match)) NULL else match
              }) -> usedInExpr
    unique(unlist(usedInExpr))
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


recurseQuosure <- function(quo, predicate, do, stopOnHit = TRUE) {
    isquo <- rlang::is_quosure(quo)
    
    if (!isquo)  quo <- rlang::new_quosure(quo)
    
    if (!is.call(quo[[2]])) return(if (isquo) quo else quo[[2]])
    
    
    s <- (rlang::as_label(quo[[2]][[1]]) %in% c('{', '(')) + 1L
    
    if (s == 1L) {
        pred <- predicate(quo) 
        if (pred) quo <- do(quo)
    }
    
    if (s == 2L || !(stopOnHit && pred)) {
        for (i in s:length(quo[[2]])) {
            if (!is.null(quo[[2]][[i]]))  quo[[2]][[i]] <- Recall(quo[[2]][[i]], predicate, do, stopOnHit)
        }
    }

    
    if (isquo) quo else quo[[2]]
       
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

as.arglist <- function(names) {
    al <- alist(x = )[rep('x', length(names))]
    
    setNames(al, names)
}


.function <- function(args, body) {
    rlang::new_function(args, rlang::enexpr(body), parent.frame())
    
}

getArglist <- function(form) {
   form <- form[names(form) != '...']
   gets <- mget(names(form), envir = parent.frame(), inherits = FALSE, 
                ifnotfound = replicate(length(form), NULL, simplify = FALSE))
   gets[sapply(gets, rlang::is_missing)] <- NULL
   
   gets
}

`appendformals<-` <- function(x, values, after = length(x)) {
    # values must be alist
    cur <- formals(x)
    cur <- cur[!names(cur) %in% names(values) ]
    
    formals(x) <- c(head(cur, after), 
                    values, 
                    tail(cur, length(cur) - after))
    x
}

`partialApply<-` <- function(x, values) {
    cur <- formals(x)
    cur[.names(values)[.names(values) %in% .names(cur)]] <- values[.names(values) %in% .names(cur)]
    
    formals(x) <- c(cur, values[!.names(values) %in% names(cur)])
    x
    
}

append2expr <- function(expr, exprs) {
    l <- length(expr)
    expr <- as.list(expr)
    
    as.call(append(expr, exprs, 2))
}

### Building smart functions ----

setoptions... <- function(options, defaults) {
    setoptions(options) <- defaults
    options
}


`setoptions<-` <- function(x, values) {
    # used to set options
    # This function is extremely useful whenever you want to have a list/vector of named options
    # and let users change SOME of the options without having to reset all of them
    # The values are the default values you want.
    # x, is the values that users want to change (if any)
    # so 
    # # setoptions(c()) <- c(a=1, b = 2)
    # will keep the default values (a = 1, b = 2) but
    # # setoptions(c(a = 2)) <- c(a = 1, b = 2)
    # will overwrite the default (a = 1) with the user choice (a = 2)
    # if x is TRUE, return values
    # if x is FALSE, return FALSE
    
    if (is.logical(x)) if (x[1]) x <- NULL else return(FALSE) 
    if (is.null(x)) return(as.list(values))
    poss <- names(values)
    ind <- pmatch(names(x), poss)
    hits <- !is.na(ind)
    
    values[ind[hits]] <- x[hits]
    
    as.list(c(x[!hits], values))
}

logicalOption <- function(opt) {
    optname <- rlang::expr_text(rlang::enexpr(opt))
    if (is.logical(opt) && !opt[1]) return(FALSE)
    
    if (is.logical(opt)) opt <- list()
    
    assign(optname, opt, envir = parent.frame())
    return(TRUE)
    
}

nestoptions <- function(opts, ...) {
    # nests named values in a list within
    # a new vector of given name
    nests <- list(...)
    
    for (i in seq_along(nests)) {
        vals <- nests[[i]]
        opts[[names(nests)[i]]] <- do.call('c', opts[vals])
    }
    opts[!names(opts) %in% unlist(nests)]
    
}




`%dots%` <- function(call, predicate) {
    # this magic function takes a call with a ...
    # and applies that expression, except first filters out arguments from the ...
    # which don't match predicate function
    # allows us to send DIFFERENT ... to sub functions
    call <- rlang::enexpr(call)
    funccall <- deparse(call[[1]])
    
    call[[1]] <- quote(list)
    call <- call[!sapply(call, function(x) deparse(x) == "...")]
    args <- eval(call, parent.frame())
    
    #
    dots <- eval(quote(list(...)), envir = parent.frame())
    dots <- dots[predicate(dots)]
    
    do.call(funccall, c(args, dots))
    
}

overdot <- function(call) {
    # this function removes redundant arguments 
    # in ldots 
    
    call <- rlang::enexpr(call)
    
    dots <- eval(quote(list(...)), envir = parent.frame())
    
    if (length(dots) > 0) {
        call <- call[!sapply(call, function(x) deparse(x) == "...")]
        call <- call[!names(call) %in% names(dots)]
        
        call <- append2expr(call, dots)
    }    
    eval(call, envir = parent.frame())
    
    
}


### Checking arguments

checkArgs <- function(args, valid, argname, callname = NULL, min.length = 1L, max.length = 1L, warnSuperfluous = TRUE, classes = NULL) {
    if (length(sys.calls()) > 6L) return(args) 
    
    argNames <- paste0('c(', glue::glue_collapse(paste0("'", args, "'"), sep = ', '), ')')
    callname <- if (is.null(callname)) '' else glue::glue("In the call humdrumR::{callname}({argname} = {argNames}): ")
    
    if (length(args) <  min.length) stop(callname, glue::glue("{length(args)} is too few {argname} arguments."))
    if (length(args) >  max.length) stop(callname, glue::glue("{length(args)} is too many {argname} arguments."))
    
    
    if (!is.null(classes) && !any(sapply(classes, inherits, x = args))) {
        classNames <- glue::glue_collapse(classes, sep = ', ', ', or ')
        stop(callname, glue::glue("The {argname} argument must inherit {classNames}, but you have input a {class(args)}."))
    }
    
    
    
    ill <- !args %in% valid
    
    
    if (any(ill)) {
        case <- glue::glue(if (sum(ill) == 1) "is not a valid {argname} value. " else " are not valid {argname} values. ")
        illNames <- glue::glue_collapse(paste0("'", args[ill], "'"), sep = ', ', last = ', and ')
        legalNames <-  glue::glue_collapse(paste0("'", valid, "'"), sep = ', ', last = ', and ')
        
        
        message <- list(callname, illNames, case, 'Valid options are ', legalNames, '.', call. = FALSE)
        
        do.call(if (warnSuperfluous && any(!ill)) 'warning' else 'stop', message)
    }
    
    args[!ill]
}

checkhumdrumR <- function(x, callname, argname = 'humdrumR') {
    if (!is.humdrumR((x))) stop(call. = FALSE,
                                glue::glue("In the call {callname}({argname} = _), the argument {argname} must be a humdrumR object."))         
}

checkTypes <- function(dataTypes, callname, argname = 'dataTypes') {
    dataTypes <- unique(unlist(strsplit(dataTypes, split = '')))
    checkArgs(dataTypes,
              c('G', 'L', 'I', 'M', 'D', 'd', 'P'),
              argname, callname, warnSuperfluous = TRUE, 
              min.length = 1L, max.length = 7L,
              classes = "character")
}

### Strings ----


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

has.prefix <- function(prefix, x) {
    prefix <- strsplit(prefix, split = '\\|')
    prefix <- sapply(prefix, function(pre) paste(paste0('^', pre), collapse = '|'))
    
    if (missing(x))  function(x) grepl(prefix, x) else grepl(prefix, x)
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

affixer <- function(str, fix, prefix = TRUE, sep = "") .paste(if (prefix) fix, str, if (!prefix) fix, sep = sep)

plural <- function(n, then, els) .ifelse(n > 1, then, els)

nth <- function(n) {
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
                                 function(n) {
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

trimLongString <- function(strs, n = 20L) {
  strs[str_length(strs) > n] <- paste0(stri_trim_both(str_sub(strs[str_length(strs) > n], end = n)), '...')
  strs
}


strPartition <- function(str, split = '/') {
    # split strs into columns
    # if (hasdim(str)) {
    #     output <- do.call('cbind', lapply(1:ncol(str), function(j) ofColumns(str[, j, drop = TRUE])))
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
                               function(i) {
                                   sapply(mat, '[', i = i) %dim% str
                                   
                               }) 
    
    df <- as.data.frame(lapply(df, I))
    colnames(df) <- c('base', rep('of', ncol(df) - 1L))
    df
    
}




