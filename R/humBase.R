
 #' Humdrum Reference Codes (!!!).
 #' 
 #' This data frame contatains detailed information for the standard Humdrum
 #' Reference codes, as described in the Humdrum user guide.
 #' 
 #' @docType data
 #' @usage data(ReferenceCodes)
 #' @format A \code{data.frame}.
 #' @export
"ReferenceCodes"

###Ditto


# cppFunction('CharacterVector ditto(CharacterVector vec) {
# 
#             CharacterVector newvec = clone(vec);
# 
#             int n = vec.size();
# 
#             for(int i = 1; i < n; ++i) {
#             if (CharacterVector::is_na(vec[i]) || vec[i] == ".") {
#             newvec[i] = newvec[i - 1];
#             }
#             }
# 
#             return(newvec);
# 
#             }')
setClass('humdrumFunction', contains = 'function') -> humFunc
setMethod('^', 'humdrumFunction',
          function(e1, e2) e1(e2))

applyFunc <- function(func, x) func(x)

#' @export
classify <- function(..., other = 'Other') {
  #takes any number of predicate functions
  funcs <- list(...)
  
  funcs <- lapply(funcs, function(func) if (is.function(func)) func else EQ(func))
  
  classes <- names(funcs)
  if (is.null(classes) || any(classes == '')) stop('classify requires named predicate functions.', call. = FALSE)
  
  (function(x) {
    hits <- lapply(funcs, applyFunc, x = x)
    hits <- do.call('cbind', Map(function(x,y) c(NA, x)[y + 1], names(hits), hits))
    
    apply(hits, 1,  
          function(row) {
            if (all(is.na(row))) {
              other 
            } else {
              paste(row[!is.na(row)], collapse = ',')
            }})
    
  }) -> func
  humFunc(func)
}

#' @export
RErid <- function(...) {
  res <- unlist(list(...))
  
  humFunc(function(strs) {
    for (re in res) strs <- strs %str-% re
    strs[strs == ''] <- '.'
    strs
  }) 
}
#' @export
REkeep <- function(re) {
  humFunc(function(strs) {
    matches <- stringr::str_extract(strs, re)
    matches[is.na(matches)] <- ''
    matches
  } )
}

#' @export
humtable <- function(..., levels = NULL) {
  args <- list(...)
  
  if (!is.null(levels)) {
    if (!is.list(levels)) levels <- list(levels)
    if (length(levels) < length(args)) match_size(args = args, levels = levels, toEnv = TRUE)
    args <- Map(factor, args, levels)
  }
  
  do.call('table', c(args, useNA = 'ifany'))
}

#' @export
append2string <- function(app, sep = '') {
 newfunc <- function(str) paste(str, sep, app, collapse = '')
 
 if (is.function(func)) body(newfunc) <- call('{', quote(app <- app(str)), body(newfunc))
 
 humFunc(newFunc)
}

#' @export
prepend2string <- function(prep, sep = '') {
  newfunc <- function(str) paste(prep, sep, str, prep, collapse = '')
  
  if (is.function(func)) body(newfunc) <- call('{', quote(prep <- prep(str)), body(newfunc))
  
  humFunc(newFunc)
}

#' @export
subtract <- curriedfunction(alist(x = , from = ), {from - x})
#' @export
add      <- curriedfunction(alist(x = , y = ), {x + y})
#' @export
multiply <- curriedfunction(alist(x = , y = ), {x * y})
#' @export
divide   <- curriedfunction(alist(x = , by = ), {x / by})
#' @export
divideInt <- curriedfunction(alist(x = , by = ), {x %/% by})



### segmentation
# 
# ngram = function(vec, n = 2) {
# 
#   ngrams = Reduce(x = vec,
#                   f = function(x,y) {
#                     c(x, head(y, max(n) - 1))
#                   },
#                   accumulate = TRUE,
#                   right = TRUE
#   )
#   if(length(n) > 1) ngrams = lapply(ngrams, '[', n)
# 
#   ngrams
# 
# }

# .ngram = (function(n = 2) {
#   function(func, ...) {
#     function(vec) {
#       sapply(ngram(vec, n), func, ...)
#     }
#   }
# } )



###

#' @export
applyto = (function(pattern, skip) {
  #creates function which takes and input function and creates a new function which ignores tokens matching, or not matching, regexs
  #if the output of this new function is the same length as the nonskipped tokens, then the original vec is returned with
  #only the changes to the nonskipped tokens. Otherwise, just the output of the function is returned
  function(func, ...) {
    function(vec) {
      if(inherits(pattern, 'character')) { hits = grepl(pattern, vec)  }
      if(inherits(pattern, 'function')) { hits = pattern(vec)  }

      if(skip) hits = !hits
      raw = func(vec[hits], ...)
      out = raw

      out
    }
  }
})




.reverse = (function(func) {
#' @export
    function(vec) {
      rev(func(rev(vec)))
    }
}) 









#' @export
combinations = function(vec, n = 2, involving = NA) {
  if(any(!is.na(involving))) {
    if(is.character(involving) || inherits(involving, 'regex')) {
      hits = greps(involving, vec, value = TRUE)
    }
    if(is.numeric(involving) || is.logical(involving)) hits = vec[involving]
  } else {
    hits = vec
  }

  out = utils::combn(vec, n, simplify = FALSE)

  Filter( function(comb) {  any(comb %in% hits)  },  out)
}

#' @export
.combinations = (function(n = 2, involving = NA) {
  function(func, ...) {
    function(vec) { lapply(combinations(vec, n, involving), func, ...)
    }
  }
} )


#' @export
permutations = function(vec) {
  combinat::permn(vec)}


#' @export
subTokens = (function(token) { 
  strsplit_single(token, pattern = ' ')  }) # %% Vectorize : c(SIMPLIFY = FALSE)

#' @export
tokenCompare = function(func, token1, token2, ...) {
  output = outer(subTokens(token2), subTokens(token1), FUN = func, ...)
  rownames(output) = subTokens(token2)
  colnames(output) = subTokens(token1)
  output
}

#' @export
applyExpr <- function(ex, func, rebuild = TRUE, ignoreHead = TRUE) {
  # helper function
  accum <- c()
  
  if (len1(ex)) {
    return(func(ex))
  } else {
    for (i in (2 - !ignoreHead):length(ex)) {
      out <- Recall(ex[[i]], func, rebuild = rebuild) 
      
      if (rebuild) {
         ex[[i]] <- out 
         } else {
          accum <- c(accum, out) 
         }
    }
  }
  
  if (rebuild) ex else accum
}

#' @export
apply2ExprAsString <- function(func, ...) {
  function(expr) {
    str <- func(deparse(expr), ...)
    parse(text = str)[[1]]
    
  }
}



#####TOOLS FOR THE COMBINE APPLIER

combineTables = function(tableList) {
  #' @export
  stacked = unlist(cbind(tableList))

  tapply(stacked, names(stacked), sum)
}


#
