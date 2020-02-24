####### Regex Parsing

#' Make a regular expression parser
#' 
#' 
#' @export
REparser <- function(...) {
    
    res <- list(...)
    res <- lapply(res, function(re) paste0('^', re))
    
    if (is.null(names(res)) | any(names(res) == "")) stop(call. = FALSE,
                                                          "In call to REparser, all arguments must be named.")
    function(str) {
        complete <- !logical(length(str))
        
        for (re in names(res)) {
            locs <- stringr::str_locate(str, res[[re]])
            
            hits <- !is.na(locs[ , 1])
            complete <- complete & hits
            res[[re]] <- stringr::str_sub(str, locs[ , 'start'], locs[ , 'end'])
            
            str[hits] <- stringr::str_sub(str[hits], start = locs[hits, 'end'] + 1)
        }
        
        output <- do.call('cbind', res)
        output[!complete, ] <- NA_character_
        output
    }
    
    
}



####### Regex dispatch ----

#' Regular expression method dispatch and function application
#' 
#' The \code{\link{humdrumR}} \strong{regular-expression method dispatch}
#' system is a simple system for making new functions which can by smartly
#' applied to complex character strings.
#' 
#' The function \code{do2RE} accepts and arbitrary function
#' and a \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expression} (regex)
#' and makes a new function that applies the original function only to
#' any part of a string which matches the regex.
#' 
#' The function \code{regexDispatch} accepts a list
#' of functions, each with a matching regular expression,
#' and creates a new function which applies whichever function
#' based on which regexs it finds in its input.
#' @name regexDispatch
NULL







#' @name regexDispatch
#' @export
regexDispatch <- function(...) {
          funcs <- Filter(is.function, list(...))
          if (length(funcs) == 0L) stop("Can't regexDispatch on zero functions.")
          
          regexes <- getRE(names(funcs))
          funcsArgs <- lapply(funcs, function(rf) formals(args(rf))[-1])
          
          genericFunc <- function() {
              if (!is.character(str)) stop(call. = FALSE,
                                           "The regex-dispatch function you've called requires a character argument.")
              
              dispatch <- regexFindMethod(str, regexes)  
              if (dispatch == 0L) return(if (inPlace) str else vectorna(length(str), 'character'))
              dispatchFunc <- funcs[[dispatch]]
              dispatchRE   <- regexes[[dispatch]]
              dispatchArgs <- funcsArgs[[dispatch]]
              # ... args
              elips <- names(dispatchArgs) == '...'
              not_elips <- names(dispatchArgs)[!elips]
              #
              dispatchArgs <- setNames(lapply(not_elips, get, envir = environment()), not_elips)
              dispatchArgs <- c(x = list(str), regex = dispatchRE, .func = dispatchFunc, inPlace = inPlace,
                                dispatchArgs, if (any(elips)) list(...) else list())
              do.call('.REapply', dispatchArgs)
          }
          
          # Assemble the new function's arguments
          genericArgs <- do.call('c', c(funcsArgs, use.names = FALSE))
          genericArgs <- genericArgs[!duplicated(names(genericArgs))]
          formals(genericFunc) <- c(alist(str = ), genericArgs, alist(inPlace = FALSE))
          
          genericFunc
}

# 
regexFindMethod <- function(str, regexes) {
    # this takes a str of character values and a string of REs
    # and returns a single interger value representing the 
    # index (of regexes) to dispatch.
    
    Nmatches <- sapply(regexes, function(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
    if (!any(Nmatches > 0L)) return(0L)
    
    #which function to dispatch
    Ncharmatches <- sapply(regexes[Nmatches > 0],
                           function(re) {
                               nchars <- nchar(stringi::stri_extract_first_regex(str, re))
                               nchars[is.na(nchars)] <- 0L
                               sum(nchars)
                           })
    which(Nmatches > 0L)[which.max(Ncharmatches)]
}


#' @name regexDispatch
#' @export
REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    if (!is.character(x)) stop(call. = FALSE,
                               "Sorry, REapply can only apply to an x argument that is a character vector.")
    .REapply(x, getRE(regex), .func, inPlace = inPlace, ...)
}

#' 
.REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    # accepts a regex (whereas REapply can take a unparsed regex name
    # like "Recip").
    matches <- stringi::stri_extract_first(str = x, regex = regex)
    result <- do.call(.func, c(list(matches), list(...)))
    
    if (inPlace) {
        restorer <- restorer(result)
        result <- as.character(result)         
        result <- stringi::stri_replace_first(str = x, regex = regex, 
                                              replacement = result) %restore% restorer
    }
    
    result

}


############### Composing predicate functions----

#' @name regexDispatch
#' @export
`%predate%` <- function(func, predicate) {
    predicateDispatch( rlang::expr_text(rlang::enexpr(func)), predicate)
}


#' @name regexDispatch
#' @export
`%pREdate%` <- function(func, regex) {
    args <- list(func)
    names(args) <- regex
    do.call('regexDispatch', args)
}



predicateDispatch <- function(fname, predicateFunc) {
    func <- match.fun(fname)
    #argnames
    argnames <- names(fargs(func))
    
    if (length(argnames) == 0L) stop(call. = FALSE, "predicateDispatch (%predicate%) can't add a predicate to a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "predicateDispatch (%predicate%) doesn't if the first argument of the method is ..." )
    argnames <- argnames[argnames != "..."]
    
    #
    fbody <- normalizeBody(fname)
    
    body <- rlang::quo({
        rebuild <- predicateParse(predicateFunc, argnames,
                                  !!!rlang::syms(argnames[argnames != '...']))
        result <- {!!fbody}
        rebuild(result)
    })
    body(func) <- rlang::quo_squash(body)
    environment(func) <- new.env(parent = environment(func))
    
    assign('predicateFunc', predicateFunc, envir = environment(func))
    assign('argnames', argnames, envir = environment(func))
    
    func
}

normalizeBody <- function(fname, func = NULL, removeElips = TRUE) {
    # this takes the name of a function (as a string)
    # and creates a usable function body
    # including jumping through hoops to for
    # primitives and generics
    
    if (is.null(func)) func <- match.fun(fname)
    fname <- rlang::sym(fname)
    ftext <- rlang::quo_text(func)
    
    
    # args
    argnames <- names(fargs(func))
    if (argnames[[1]] == '...' && removeElips) argnames[[1]] <- 'tmp'
    argnames <- rlang::syms(argnames)
    
    namedfuncexpr <- rlang::expr((!!fname)(!!!argnames))
    
    if (is.primitive(func) | 
        grepl('\\.Internal|\\.Primitive', ftext) |
        grepl('UseMethod|standardGeneric', ftext)) {
        namedfuncexpr
        
    } else {
        
        body <- rlang::fn_body(func)
        if (sum(stringi::stri_count_fixed(as.character(body), '\n')) > 1 ||
            !grepl('function\\(', as.character(body))) {
            namedfuncexpr
        } else {
            body
        }
    }
}




predicateParse <- function(predicate, argnames, ...) {
    args <- setNames(list(...), argnames)
    
    lengths <- lengths(args)
    targets <- args[lengths == lengths[1]]
    bool <- apply(sapply(targets, predicate), 1, any)
    list2env(lapply(targets, '[', i = !bool), 
             envir = parent.frame())
    
    output <- args[[1]]
    
    function(result) {
        if (length(result) != sum(!bool, na.rm = TRUE)) return(result)
        output[!bool] <- result
        output
    }
}


memoify <- function(fname) {
    func <- match.fun(fname)
    #argnames
    argnames <- names(fargs(func))
    
    if (length(argnames) == 0L) stop(call. = FALSE, "Can't memoify a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "Can't memoify a function if the first argument is ..." )
    argnames <- argnames[argnames != "..."]
    
    #
    fbody <- normalizeBody(fname)
    
    body <- rlang::quo({
        rebuild <- memoiseParse(argnames, !!!rlang::syms(argnames[argnames != '...']))
        result <- {!!fbody}
        rebuild(result)
    })
    body(func) <- rlang::quo_squash(body)
    environment(func) <- new.env(parent = environment(func))
    
    assign('argnames', argnames, envir = environment(func))
    
    func
    
}

memoiseParse <- function(argnames, ...) {
    args <- setNames(list(...), argnames)
    
    target <- args[[1]]
    
    bool <- duplicated(target)
    
    uniq <- target[!bool]
    
    assign(argnames[1], uniq, parent.frame())
    
    matrix <- vapply(uniq, function(x) x == target, FUN.VALUE = integer(length(target)))
    i <- rowSums(matrix * col(matrix))
    
    function(result) {
        if (length(result) != ncol(matrix)) return(result)

        result[i]
    }
    
    
}
###################  Regex tools ----


#' Match strings against regular expression
#' 
#' These infix functions are simply syntactic sugar for
#' existing \code{R} regular expression matching functions.
#' 
#' \describe{
#' \item{\%~l\%}{Matches \code{pattern} in \code{x} and returns \code{logical}. Shorthand for \code{\link[base]{grepl}}}
#' \item{\%~\%}{The "default"---same as \code{\%~l\%}}
#' \item{\%~i\%}{Matches \code{pattern} in \code{x} and returns \code{integer} indices. Shorthand for \code{\link[base]{grep}}}
#' \item{\%~n\%}{Matches \code{pattern} in \code{x} and returns \code{integer} counts (can be greater than one if more 
#' than one match occurs in the same token). Shorthand for \code{\link[stringi]{stri_count_regex}}}
#' }
#' @export
#' @name RegexFind
`%~l%` <- function(x, pattern) grepl(pattern, x)
#' @export
#' @name RegexFind
`%~i%` <- function(x, pattern) grep(pattern, x)
#' @export
#' @name RegexFind
`%~n%` <- function(x, pattern) stringi::stri_count_regex(x, pattern)
#' @name RegexFind
#' @export
`%~%` <- function(x, pattern) grepl(pattern, x)




