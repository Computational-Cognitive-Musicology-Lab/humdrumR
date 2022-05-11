
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



### segmentation

ngram = function(vec, n = 2) {
#' @export
#'

  ngrams = Reduce(x = vec,
                  f = function(x,y) {
                    c(x, head(y, max(n) - 1))
                  },
                  accumulate = TRUE,
                  right = TRUE
  )
  if(length(n) > 1) ngrams = lapply(ngrams, '[', n)

  ngrams

}

.ngram = (function(n = 2) {
#' @export
#'
  function(func, ...) {
    function(vec) {
      sapply(ngram(vec, n), func, ...)
    }
  }
} )


#' @export 
seq_pat = function(from, to, by) {
  if(length(by) == 1) return(seq(from, to, by))

  n = ceiling((to - (from - 1)) / sum(by))

  out = cumsum(c(from, rep(by, n)))

  out[out <= to]
}

#' @export 
windows_index_open = function(vec, pat, offset = 0, from = 1, to = 0){

  to[to < 1] = length(vec) + to[to < 1]

  if(is.numeric(pat)) { inds = seq_pat(from[1], to[1], pat) }

  if(is.character(pat) || any(grepl('regex', class(pat)))) {
    inds = greps(pat, vec[from:to]) + from - 1
  }

  inds = inds + offset
  inds[ inds >= 1 & inds <= length(vec)]
}

#' @export 
windows_index_close = function(vec, openings, pat = NULL, offset = 0, matchN = 1) {
  if(is.numeric(pat)) {
    inds = openings + pat
  }

  if(is.null(pat)) {
    inds = rotate(openings, -matchN)
  }

  if(length(pat) == 1 && pat %in% c('~', '==')) {
    unlist(lapply(seq_along(openings),
                  function(i) {
                    if(pat == '==') hits = which(vec == vec[openings[i]])
                    if(pat == '~' ) hits = grep(vec[openings[i]], vec)
                    if(matchN > 0) {
                       hits = hits[hits > openings[i]][matchN]
                       } else {
                         hits = hits[hits < openings[i]][length(hits) - abs(matchN) + 1]
                       }

            hits
            }
            )) -> inds

  } else {

  if(is.character(pat) || any(grepl('regex', class(pat)))) {
    hits = greps(pat, vec)
    if(matchN < 0) matchN = matchN + 1

    whichgreater = apply(outer(openings,hits,'>'), 1, sum)

    unlist(lapply(whichgreater,
                  function(w) {
                    whichhit = matchN + w
                    if(whichhit < 1) return(NA)
                    out = hits[matchN + w]
                    if(length(out) == 0) out = NA
                    out
                    }
                  )
           ) -> inds
    }
  }

  inds = inds + offset
  inds[ inds < 1 | inds > length(vec)] = NA
  inds


}


#' @export 
windows_index = function(vec, open, close,
                         open_offset = 0, close_offset = 0, matchN = 1,
                         bookends = FALSE) {

  openind = windows_index_open(vec, pat = open, offset = open_offset)

  endind = windows_index_close(vec, openind, close, offset = close_offset, matchN = matchN)



  Map(function(from, to){
      if(is.na(to)) {
        if(!bookends)  return(NA)
        to = ifelse(matchN < 0 || is.numeric(close) && close < 0, 1, length(vec))
      }
      sort(from:to)
    },
      openind, endind)


}


#' @export
windows = function(vec, open, close, open_offset = 0, 
                   close_offset = 0, matchN = 1, bookends = FALSE) {
  lapply(windows_index(vec, open, close, open_offset, close_offset, matchN, bookends),
         function(inds) {
           if(length(inds) == 1 && is.na(inds)) NA else vec[inds]
         }
  ) -> winds

  Filter((f(`[`, 1) | Negate(is.na)), winds)

}


index_segments_f = function(vec) { cumsum(c(TRUE,  head(vec, -1) != tail(vec, -1))) }

index_groups_f = function(vec) {split(seq_along(vec), f = vec)}



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

#' @export
.skip = (function(regexes = '^\\.$') {
  applyto(regexes, skip = TRUE) }) 

#' @export
.to   = (function(regexes = '') {
  applyto(regexes, skip = FALSE) }) 

. = (function() { 
#' @export
  .skip('^\\.$') }) 


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



#####TOOLS FOR THE COMBINE APPLIER

combineTables = function(tableList) {
  #' @export
  stacked = unlist(cbind(tableList))

  tapply(stacked, names(stacked), sum)
}


#
