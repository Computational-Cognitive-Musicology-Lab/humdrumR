locate <- function(x, values) {
    if (is.null(dim(x)) || length(dim(x)) == 1) {
        lapply(values, function(val) which(x == val))
    } else {
        apply(values, 1, 
              function(val) {
                  which(Reduce('&', Map('==', x, val)))
                  
                  })
    }
}

locate.uniq <- function(x) {
    locate(x, unique(x))
    
}

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

numeric2fraction <- function(n) {
    frac <- attr(MASS::fractions(n, cycles = 15), 'fracs')
    frac <- stringi::stri_split_fixed(frac, '/', simplify = TRUE)
    if (ncol(frac) == 1L) frac <- cbind(frac, '1')
    
    num <- as.integer(frac[ , 1])
    den <- as.integer(frac[ , 2])
    
    den[is.na(den)] <- 1L
    
    list(Denominator = den, Numerator = num)
}

# integrate and derive should be perfect inverses, 
# so long as their skip arguments are the same, and the scalar
# argument is NULL
integrate <- function(intervals, scalar = NULL, skip = list(na)) {
    
    skip <- applyrows(sapply(skip, function(f) f(intervals)), any)
     
    intervals[!skip] <- cumsum(intervals[!skip])
    if (is.null(scalar)) intervals else intervals + scalar

}

derive <- function(intervals, skip = list(na)) {
    
    skip <- applyrows(sapply(skip, function(f) f(intervals)), any)
    
    intervals[which(!skip)[-1]] <- diff(intervals[!skip])
    
    intervals
}


applyrows <- function(x, f, ...){
    result <- apply(x, 1, f, ...)
    result <- if (is.null(dim(result))) cbind(result) else t(result)
    
    if (all(dim(result) == dim(x))) dimnames(result) <- dimnames(x)
    result
}

tempvar <- function(prefix = '', asSymbol = TRUE) {
    # this makes random symbols to use as variable names
    
    random <- paste(sample(c(letters, 0:9), 5, replace = TRUE), collapse = '')
    
    char <- paste0('._', prefix, '_', random)
    
    if (asSymbol) rlang::sym(char) else char
    
}

applycols <- function(x, f, ...){
    result <- apply(x, 2, f, ...)
    result <- if (is.null(dim(result))) rbind(result) else result
    
    
    if (all(dim(result) == dim(x))) dimnames(result) <- dimnames(x)
    result
}

#' Shortcuts to making data.frames
#' 
#' These are just convenient (shorthands) for data.table and data.frame
#' @name data.frame.alias
#' @export
d.t <- data.table::data.table

#' @name data.frame.alias
#' @export
d.f <- function(...) data.frame(..., stringsAsFactors = FALSE, fix.empty.names = FALSE)

Repeat <- function(x, ..., margin = 1L) {
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

plural <- function(n, then, els) IfElse(n > 1, then, els)

nth <- function(n) {
    affix <- character(length(n))
    affix[n >= 4] <- "th"
    affix[n == 1] <- "st"
    affix[n == 2] <- "nd"
    affix[n == 3] <- 'rd'
    
    paste0(n, affix)
     
}

catlists <- function(lists) {
    # this is just like do.call('c', lists) except it never returns NULL
    
    out <- do.call('c', lists)
    if(is.null(out)) out <- list() 
    if (!is.list(out)) out <- list(out)
    out
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

testCall <- function(expr, call) {
    # takes an expression or 
    
    if (rlang::is_quosure(expr)) expr <- rlang::quo_get_expr(expr)
    
    if (!is.call(expr)) return(FALSE)
    
    expr[[1]] == rlang::sym(call)
    
}

recurseQuosure <- function(quo, predicate, do) {
    expr <- if (rlang::is_quosure(quo)) rlang::quo_get_expr(quo) else quo
    if (!is.call(expr)) return(quo)
    
    s <- if (expr[[1]] == rlang::sym('{')) 2L else 1L
    
    if (predicate(expr)) {
        expr <- do(expr)
    } else {
        for (i in s:length(expr)) {
            expr[[i]] <- Recall(expr[[i]], predicate, do)
        }
    }
    
    
    if (rlang::is_quosure(quo)) {
        quo <- rlang::quo_set_expr(quo, expr) 
        return(quo)
    } else {
        return(expr)
    }
       
    
}

wrapInCall <- function(form, call) {
    # This function takes a formula and wraps the rhs
    # with any given call.
    # i.e. form -> any(form)
    rhs <- rlang::f_rhs(form)
    rhs <- call(call, call('(', rhs))
    
    as.formula(rhs, lhs = rlang::f_lhs(form))
}

as.formula <- function(x, lhs = NULL) {
    if (rlang::is_quosure(x)) {
        class(x) <- 'formula'
        rlang::f_lhs(x) <- lhs
        x
    } else {
        rlang::new_formula(lhs = lhs, rhs = x,
                           env = parent.env(environment()))
    }
    
}

names_ <- function(x) {
    # gets names with no possibility of NULL
    nam <- names(x)
    
    if(is.null(nam)) nam <- character(length(x))
    
    nam
}

allnamed <- function(x) { !is.null(names(x)) && !any(names(x) == '')}

substituteName <- function(expr, subs) {
  if (length(subs) == 0) return(expr)
  
  if (is.call(expr) && length(expr) > 1L) {
            for (i in 2:length(expr)) expr[[i]] <- Recall(expr[[i]], subs)
  } else { 
            if (deparse(expr) %in% names(subs)) expr <- subs[[which(deparse(expr) == names(subs))]]
  }
  expr
          
}

remove.duplicates <- function(listofvalues) {
    # takes a list of vectors of values and removes elements from later vectors which
    # appear in earlier vectors
    if (sum(lengths(listofvalues)) == 0L) return(listofvalues)
    
    groups <- factor(rep(seq_along(listofvalues), lengths(listofvalues)), 
                     levels = seq_along(listofvalues)) # must specificy levels again because there may be empty vectors

    values <- unlist(listofvalues, use.names = FALSE)

    dups <- duplicated(values)
    setNames(tapply(values[!dups], groups[!dups], c, simplify = FALSE), names(listofvalues))
    
}


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
`xnames<-` <- function(x, values) {
          if (is.null(dim(x))) names(x) <- values else rownames(x) <- values
          
          x
}

match_size <- function(..., size.out = max, margin = 1, toEnv = FALSE, recycle = TRUE) {
          stuff   <- list(...)
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
              stuff[notnull] <- if (recycle) {
                 lapply(stuff[notnull], Repeat, length.out = size.out[i], margin = margin[i])
              } else {
                 lapply(stuff[notnull], pad, before = FALSE, n = size.out[i])
                  
              }
          }
          if (toEnv) list2env(stuff[names(stuff != '')], envir = parent.frame(1))
          
          if (toEnv) invisible(stuff) else stuff
          
}

pad <- function(x, n, before = TRUE) {
    lenx <- length(x)
    
    padding <- rep(as(NA, Class = class(x)), n - lenx)
    
    if (before) c(padding, x) else c(x, padding)
}

#' @export
setGeneric('compose', function(f1, f2, ...) standardGeneric('compose'))

#' @export
setMethod('compose', signature = c(f1 = 'function', f2 = 'function'),  
          function(f1, f2, ...) {
                    .args <- c(f1, f2, list(...))
                    .funcs <- Filter(is.function, .args)
                    .args  <- Filter(Negate(is.function), .args)
                    if (length(.funcs) <= 1L) stop("Can't compose one or zero functions.")
                    
                    .funcsArgs <- lapply(.funcs, function(f) formals(args(f))[-1])
                    newfunc <- function() {
                              for (i in 1:length(.funcs)) {
                                        currentFunc <- .funcs[[i]]
                                        currentArgs <- .funcsArgs[[i]]
                                        # ...
                                        elips <- names(currentArgs) == '...'
                                        not_elips <- names(currentArgs)[!elips]
                                        currentArgs <- setNames(lapply(not_elips, get, envir = environment()), not_elips)
                                        currentArgs <- c(list(x), currentArgs, if (any(elips)) list(...) else list())
                                        
                                        x <- do.call('currentFunc', currentArgs)
                              }
                              
                              x
                    }
                    allArgs <- c(.args, unlist(.funcsArgs, recursive = FALSE))
                    # names(allArgs)[names(allArgs) != '...'] <- gsub('^.*\\.', '', names(allArgs)[names(allArgs) != '...'])
                    allArgs <- allArgs[!duplicated(names(allArgs))]
                    formals(newfunc) <- c(alist(x = ), allArgs)
                    
                    newfunc
          }
)



#' @export
`%.%` <- function(e1, e2) { compose(e2, e1) }



allsame <- function(x) length(unique(x)) == 1L

`%class%` <- function(object, newclass){
  class(object) = append(newclass, class(object))
  object
}

is.whole <- function(x) x %% 1 == 0

# removes the first class from an object
popclass <- function(object) `class<-`(object, class(object)[-1])




fargs <- function(func) formals(args(func))


setClass('predicate.function', contains = 'function', 
         slots = c(string = 'character'))

setMethod('show', 'predicate.function',
          function(object) cat(c('f(x) =', object@string)) )

setMethod('&', c('predicate.function', 'predicate.function'),
          function(e1, e2) {
              f1 <- e1@.Data
              f2 <- e2@.Data
              func <- function(x) {
                  f1(x) & f2(x)
              }
              
              s1 <- e1@string
              s2 <- e2@string
              
              if (grepl('&|\\|', s1)) s1 <- paste0('(', s1, ')')
              if (grepl('&|\\|', s2)) s2 <- paste0('(', s2, ')')
              new('predicate.function', func,
                  string = paste(s1, s2, sep = ' & '))
          })
setMethod('|', c('predicate.function', 'predicate.function'),
          function(e1, e2) {
              f1 <- e1@.Data
              f2 <- e2@.Data
              func <- function(x) {
                  f1(x) | f2(x)
              }
              
              s1 <- e1@string
              s2 <- e2@string
              
              if (grepl('&|\\|', s1)) s1 <- paste0('(', s1, ')')
              if (grepl('&|\\|', s2)) s2 <- paste0('(', s2, ')')
              new('predicate.function', func,
                  string = paste(s1, s2, sep = ' | '))
          })
#' @export
EQ <- function(pat) {
  func <- function(x) {
    c('pat', 'x') %<-% match_size(pat,x)
    ifelse(is.na(pat), is.na(x), x == pat)
  }


  new('predicate.function', func, string = glue::glue('x == {deparse(pat)}'))
}

LEN <- function(p.f) {
    func <- function(x) {
        p.f(length(x))
    }
 
    new('predicate.function', func,
        string = gsub('x', 'length(x)', p.f@string))
       
}

#' @export
GT <- function(n) {
  func <- function(x) x > n

  new('predicate.function', func, string = glue::glue('x > {deparse(n)}'))
}

#' @export
RE <- function(pat) {
  func <- function(x)  if (!hasdim(x)) grepl(pat, x) else greplmat(pat, x)

  new('predicate.function', func, string = glue::glue('x ~ {deparse(pat)}'))
}

#' @export
LT <- function(n) {
  func <- function(x) x < n

  new('predicate.function', func, string = glue::glue('x < {deparse(n)}'))
}

na <- new('predicate.function', function(x) is.na(x), string = "NA")

#' @export
grepls <- function(patterns, string, combine = any) {
  if (length(patterns) == 1L) return(grepl(patterns, string))

  matches <- matrix(sapply(patterns, grepl, x = string), ncol = length(patterns))
  apply(matches, 1, combine)

}


#' Lazy version of base::ifelse

#' This function is exactly like \code{\link{base::ifelse}}, except it is lazy.
#' \code{\link{base::ifelse}} applies the x and y cases to the whole vector,
#' regardless of the condition. IfElse only computes the output y where actually 
#' asked to.
#' @export
IfElse <- function(true, yes, no) {
  if (length(true) == 0L) return(vector(class(yes), 0L))
  match_size(true = true, yes = yes, no = no, toEnv = TRUE)
  out <- no
  if (any(true & !is.na(true))) out[!is.na(true) & true ] <- yes[!is.na(true) & true]
  out
}

ifE <- function(bool, Texpr, Fexpr, varnames = c(), outmode = 'character') {
    Texpr <- enquo(Texpr)
    Fexpr <- enquo(Fexpr)
    
    
    vars <- mget(varnames, parent.env(environment()))
    true  <- eval_tidy(Texpr, lapply(vars, '[', bool))
    false <- eval_tidy(Fexpr, lapply(vars, '[', !bool))
    
    output <- vector(mode = outmode, length(bool))
    
    output[bool]  <- as(true, outmode)
    output[!bool] <- as(false, outmode)
    
    output
}



#' @export
init <- function(x, n = 1) {
  if (length(x) == 0L) return(vector(mode(x)))

  if (!hasdim(x)) x[1 : (length(x) - n)] else x[1 : (nrow(x) - n), , drop = FALSE]
}

hasdim <- function(x) !is.null(dim(x))



#' @export
num2str <- function(n, pad = FALSE) format(n, digits = 3, trim = !pad, zero.print = T, big.mark = ',', justify = 'right')


num2print <- function(n, label = NULL, capitalize = FALSE) {
          n_str <- ifelse(n <= 100L, num2word(n, capitalize = capitalize), num2str(n))
          
          if (!is.null(label)) n_str <- paste0(n_str, ' ', label, ifelse(n > 1L, 's', ''))
          
          n_str
}

if1 <- function(n, str1, strmore) ifelse(n == 1, str1, strmore)

#' @export
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

padder <- function(strs, sizes = max(nchar(strs)) + 1) {unlist(Map(stringi::stri_pad_left, strs, sizes))}

##Rotate

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
          rotation = sign(rotation) * (abs(rotation) %% size) #if rotation is greater than size, or negative, modulo
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

as.arglist <- function(argNames, argValues = NULL) {
 arglist <- alist(x = )[rep('x', length(argNames))] 
 
 arglist <- setNames(arglist, argNames)
 
 if (!is.null(argValues) && any(!is.na(argValues))) {
     arglist[!is.na(argValues)] <- argValues[!is.na(argValues)]
 }
 
 arglist
 
}

`%splat|%` <- function(obj, func) {
  if (is.atomic(obj)) obj <- as.list(obj)
  do.call(func, obj)
}

trimLongString <- function(strs, n = 20L) {
  strs[str_length(strs) > n] <- paste0(stri_trim_both(str_sub(strs[str_length(strs) > n], end = n)), '...')
  strs
}

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

grepl_list <- function(pattern, ls) sapply(ls, function(el) any(grepl(pattern, el, useBytes = TRUE), na.rm = TRUE)) 
    

switchpatch <- function(x,  ...) {
    funcs <- list(...)
    predicates <- sapply(names(funcs)[names(funcs) != 'otherwise'], match.fun)
    
    hits <- sapply(predicates, function(f) f(x))
    if (any(hits)) {
              funcs[[which(hits)[1]]](x) 
    } else {
     funcs[['otherwise']](x)         
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




`setoptions<-` <- function(x, values) {
    # used to set options
    if (is.null(x)) return(values)
    
    poss <- names(values)
    ind <- Filter(Negate(is.na), pmatch(names(x), poss))
    
    values[ind] <- x
    
    values
    
}

.paste <- function(..., sep = '', collapse = NULL, na.empty = FALSE) {
    args <- do.call('match_size', list(...))
    
    nas <- lapply(args, is.na)
    
    if (na.empty) {
        args <- Map(`[<-`, args, nas, value = "")
        do.call('paste', c(args, sep = sep, collapse = collapse))
    } else {
        nas <- apply(do.call('rbind', nas), 2, any)
        IfElse(nas, NA_character_, paste(..., sep = sep, collapse = collapse))
    }
    
}

vectorna <- function(n, mode = 'character') {
    rep(as(NA, Class = mode), n)
    
}




