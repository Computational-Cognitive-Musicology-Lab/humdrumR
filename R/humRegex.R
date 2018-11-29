



####### Regex dispatch

#' @export
.do2substr_inplace <- function(.func, regex) {
          
          function(str) {
                    matches <- stringi::stri_extract_first(str = str, regex = regex)
                    
                    modified <- as.character(.func(matches))
                    stringi::stri_replace_first(str, regex = regex,  
                                                replacement = modified)
          }
}

#' @export
.do2substr <- function(.func, regex) {
          
          function(...) {
                    strs <- list(...)
                    matches <- lapply(strs, stringi::stri_extract_first, regex = regex)
                    
                    do.call('.func', matches)
          }
          
}


#' @export
regexDispatch <- function(..., inplace = FALSE) {
          .funcs <- list(...)
          if (length(.funcs) == 0) return(id)
          
          regexes <- names(.funcs)
          reFuncs <- Map(if (inplace) .do2substr_inplace else .do2substr, 
                         .funcs, regexes)
          function(str) {
                    Nmatches <- sapply(regexes,  
                                       function(re) sum(stringi::stri_detect(str, regex = re)))
                    
                    if (any(Nmatches > 0)) {
                              Ncharmatches <- sapply(regexes[Nmatches > 0],
                                                     function(re) sum(nchar(stringi::stri_extract_first(str, regex = re))))
                              reFuncs[[which(Nmatches > 0)[which.max(Ncharmatches)]]](str)
                    } else {
                              str
                    }
          }
}

