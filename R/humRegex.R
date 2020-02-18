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
do2RE <- function(.func, regex) {
          .funcargs <- formals(args(.func))
          
          if (length(.funcargs) == 0L) stop("Can't make a new function using do2RE if the original function takes no arguments.")
          
          newfunc <- function() {
                    matches <- stringi::stri_extract_first(str = .first.input, regex = regex)
                    
                    args <- lapply(names(.funcargs), get, envir = environment())
                    args[[1]] <- matches
                    result  <- do.call('.func', args)
                    
                    if (inPlace) {
                       result <- as.character(result)         
                       result <- stringi::stri_replace_first(str = .first.input, regex = regex, replacement = result)
                    }
                    result
                    
          }
          formals(newfunc) <- c(.funcargs, alist(inPlace = TRUE))
          .arg1 <- names(.funcargs)[1]
          body(newfunc) <- substituteName(body(newfunc), list(.first.input = as.symbol(.arg1)))
          newfunc
}

#' @name regexDispatch
#' @export
applyRE <- function(x, regex, .func, inPlace = TRUE, ...) {
          if (!is.character(x)) x <- as.character(x)
          regex <- getRE(regex)
          matches <- stringi::stri_extract_first(str = x, regex = regex)
          result <- do.call(.func, c(list(matches), list(...)))
          
          if (inPlace) {
                    result <- as.character(result)         
                    result <- stringi::stri_replace_first(str = x, regex = regex, replacement = result)
          }
          result
}

#' @name regexDispatch
#' @export
regexDispatch <- function(...) {
          funcs <- Filter(is.function, list(...))
          if (length(funcs) <= 1L) stop("Can't regexDispatch on one or zero functions.")
          
          regexes <- getRE(names(funcs))
          funcsArgs <- lapply(funcs, function(rf) formals(args(rf))[-1])
          genericFunc <- function() {
                    if (!is.character(str)) return(str)
                    Nmatches <- sapply(regexes, function(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
                    if (any(Nmatches > 0)) {
                              #which function to dispatch
                              Ncharmatches <- sapply(regexes[Nmatches > 0],
                                                     function(re) {
                                                               nchars <- nchar(stringi::stri_extract_first_regex(str, re))
                                                               nchars[is.na(nchars)] <- 0L
                                                               sum(nchars)
                                                     })
                              dispatch <- which(Nmatches > 0)[which.max(Ncharmatches)]
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
                              do.call('applyRE', dispatchArgs)
                    } else {
                              str
                    }
          }
          
          # Assembel the new function's arguments
          genericArgs <- do.call('c', c(funcsArgs, use.names = FALSE))
          genericArgs <- genericArgs[!duplicated(names(genericArgs))]
          formals(genericFunc) <- c(alist(str = ), genericArgs, alist(inPlace = FALSE))
          
          genericFunc
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




