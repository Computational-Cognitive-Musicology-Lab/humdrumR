### Null and NA values

`%just%` <- function(e1, e2) if (is.null(e1)) e2 else e1
`%maybe%` <- function(e1, e2) if(is.null(e1)) e1 else e2(e1)

###

`%class%` <- function(object, newclass){
  class(object) = append(newclass, class(object))
  object
}

popclass <- function(object) `class<-`(object, class(object)[-1])

fargs <- function(func) formals(args(func))

### Names ----

.names <- function(x) { #:: a -> character
    # gets names with no possibility of NULL
    # is there are no names, returns vector of empty strings
    nam <- names(x)
    
    if(is.null(nam)) nam <- character(length(x))
    
    nam
}

allnamed <- function(x) { !is.null(names(x)) && !any(names(x) == '')}

### Vectors ----


init <- function(x, n = 1) head(x, -n)

allsame <- function(x) length(unique(x)) == 1L

hasdim <- function(x) !is.null(dim(x))

vectorna <- function(n, mode = 'character') rep(as(NA, Class = mode), n)

padNA <- function(x, n, before = TRUE) {
### pad vector with NA
    lenx <- length(x)
    
    padding <- rep(as(NA, Class = class(x)), n - lenx)
    
    if (before) c(padding, x) else c(x, padding)
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

## My versions of some standard utitilies

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
