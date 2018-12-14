####### Regex dispatch

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
          .funcargs <- fargs(.func)
          
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
regexDispatch <- function(..., doRE = TRUE) {
          .args <- list(...)
          .funcs <- Filter(is.function, .args)
          .args  <- Filter(Negate(is.function), .args)
          if (length(.funcs) <= 1L) stop("Can't regexDispatch on one or zero functions.")
          
          regexes <- names(.funcs)
          regexes <- getRE(regexes)
          if (doRE) reFuncs <- Map(do2RE, .funcs, regexes)
          reFuncsArgs <- lapply(reFuncs, function(rf) fargs(rf)[-1])
          
          genericFunc <- function() {
                    Nmatches <- sapply(regexes,  
                                       function(re) sum(stringi::stri_detect(str, regex = re)))
                    if (any(Nmatches > 0)) {
                              Ncharmatches <- sapply(regexes[Nmatches > 0],
                                                     function(re) sum(nchar(stringi::stri_extract_first(str, regex = re))))
                              dispatch <- which(Nmatches > 0)[which.max(Ncharmatches)]
                              dispatchFunc <- reFuncs[[dispatch]]
                              dispatchArgs <- reFuncsArgs[[dispatch]]
                              # ...
                              elips <- names(dispatchArgs) == '...'
                              not_elips <- names(dispatchArgs)[!elips]
                              dispatchArgs <- setNames(lapply(not_elips, get, envir = environment()), not_elips)
                              dispatchArgs <- c(list(str), dispatchArgs, if (any(elips)) list(...) else list())
                              
                              do.call('dispatchFunc', dispatchArgs)
                    } else {
                              str
                    }
          }
          
          allArgs <- c(.args, unlist(reFuncsArgs))
          names(allArgs) <- gsub('^.*\\.', '', names(allArgs))
          allArgs <- allArgs[!duplicated(names(allArgs))]
          
          formals(genericFunc) <- c(alist(str = ), allArgs)
          
          genericFunc
}

