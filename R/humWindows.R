#' @export
applyNgram <- function(n = 2, vecs, f = c, by = NULL, pad = TRUE, 
                       padder = NA, splat = !is.null(by), ...) {
  # x is list of vectors of same length
  if (!is.null(by)) vecs <- lapply(vecs, split, f = by)
  
  if (n == 0) stop("You called applyNgram with n = 0, but you can't make an 0-gram!", call. = FALSE)
  if (!allsame(lengths(vecs))) stop("You have tried to applyNgram across multiple vectors of different lengths, but this is not supported.", .call = FALSE)
  
  n <- n - (sign(n))
  
  
  if (n == 0) { 
    output <- do.call('Map', c(f, vecs))
  } else {
    
    starts <- seq_along(vecs[[1]])
    if (pad) {
      vecs <- lapply(vecs, function(vec) c(rep(NA, abs(n)), vec, rep(NA, abs(n))))
      starts <- starts + abs(n)
    } else {
      starts <- starts[ (starts + n) <= length(vecs[[1]]) & starts + n >= 1]
    }
    
    inds   <- if (sign(n) == 1) Map(`:`, starts, starts + n) else Map(`:`, starts + n, starts)
    
    #
    
    ngs <- lapply(vecs, 
                  function(vec) {
                    lapply(inds, function(i) vec[i])
                  })
    .f <- if (splat) { 
      function(...) {do.call('f', unlist(list(...), use.names = FALSE, recursive = FALSE)) }
      } else {
        f
      }
    output <- do.call('Map', c(.f, ngs))
    
    if (pad && !is.na(padder)) output <- lapply(output,
                                                function(out) {
                                                  if (is.character(out)) gsub('NA', padder, out) else `[<-`(out, is.na(out), padder)
                                                })
                                                
  } #end of if(n == 0) else
  
  if (all(lengths(output) == 1)) output <- unlist(output)
  
  output
  
}




## open ~ c(1,2,3)
## close ~ -1 
##
## 
##

### 
### 
### 
### 
### 
getNames <- function(expr) {
 if (length(expr) == 1) {
        if (is.name(expr))  return(expr) else return(list())
 }
 names <- list()
 for (i in 2:length(expr)) {
          names <- c(names, Recall(expr[[i]]))
 }
 sapply(names, as.character)
         
}

makeHopper <- function(hops) {
 function(vec) {
           end <- length(vec)
           cumsum(c(1, rep(hops, end / sum(hops))))         
 }
}
makeMatcher <- function(re) {
 function(vec) {
        grep(re, vec)
 }
}
makeAdder <- function(sizes) {
          function(vec, inds) {
                 do.call('mapply', c(`+`, match_size(inds, sizes)))   
          }
          
}
makeNexter <- function(re) {
          matcher <- makeMatcher(re)
          function(vec, inds) {
                  hits <- matcher(vec)
                  findNextAfter(hits, inds)
          }
          
}






makeFormula <- function(form, type) {
 reference <- as.character(lazyeval::f_lhs(form)         )
 val      <- lazyeval::f_eval(form)
 
 if (type == reference) {
  if (is.character(val)) makeMatcher(val) else makeHopper(val)         
 } else {
  if (is.character(val)) makeNexter(val) else makeAdder(val)         
 }
 
 
}

windowing <- function(vec, open, close, start = 1, end = length(vec), edging = 'trim', min.length = 2) {
 openfunc <- makeFormula(open, 'open')         
 closefunc <- makeFormula(close, 'close')         
 
 if (length(formals(openfunc)) == 1) opens <- openfunc(vec)
 
 if (length(formals(closefunc)) == 1) closes <- closefunc(vec)
 
 if (length(formals(openfunc)) == 2) opens <- openfunc(vec, closes)
 
 if (length(formals(closefunc)) == 2) closes <- closefunc(vec, opens)
 
 paired <- findNextAfter(closes, opens)
 
 paired <- windowEdges(paired, start, end, type = edging)
 
 paired <- do.call('cbind', paired)
 
 paired[(apply(paired, 1, diff) + 1) >= min.length, ]
}

findNextAfter <- function(x, after) {
 x <- x[x > min(after)]
 
 intervals <- findInterval(after, sort(x)) + 1
 # hits <- sapply(seq_along(after), function(a) x[intervals == a][1])
 
 # list(Before = after, After = hits)
 list(Open = after, Close = x[intervals])
}


nestingWindows <- function(open, close) {
          function(vec, openlevels = 1, closelevels = openlevels) {
                    opens <- stringi::stri_count_regex(vec, open)
                    closes <- stringi::stri_count_regex(vec, close)
                    
                    openscum <- cumsum(opens)
                    closescum <- head(cumsum(c(0, closes)), -1)
                    
                    depth <- openscum - closescum
                    if (tail(depth, 1) != 0) stop(paste0("In your call to a function created by humdrumR::nestingWindows, 
                                                         the input vector does not have matching ", 
                                                         open, 
                                                         " and ", 
                                                         close, " tokens."))
                    
                    cdepth <- sapply(1:max(closes), function(m) (depth * (closes >= m)) - (closes > 0) * (m - 1))
                    odepth <- sapply(1:max(opens ), function(m) (depth * (opens >= m)) - (opens > 0) * (m - 1))
                    
                    lapply(1:max(depth),
                           function(d) {
                                     cbind(opens = apply(odepth == d, 1, any),
                                           closes = apply(cdepth == d, 1, any))
                                     
                           }) -> hits
                    
                    out <- do.call('abind', c(hits, along = 3))
                    
                    dimnames(out) <- list(Vector = vec,
                                          Bound = c('Open', 'Close'),
                                          Depth = 1:dim(out)[3])
                    
                    list(Opens = unlist(`dim<-`(apply(out[ , 1, openlevels], 2, which), NULL)),
                         Close = unlist(`dim<-`(apply(out[ , 2, closelevels], 2, which), NULL)))
                    
                    
          }
          
}

# makeWindowFunction <- function(oform = ~open + 1, cform = ~open + 1) {
#  ######################################
#  oexpr <- lazyeval::f_rhs(oform)
#  oexpr <-  windowExprRE(oexpr)
#  onames <- getNames(oexpr)
#  orecurs <- isRecursive(oexpr, 'open')
#  
#  ofunc <- function() {  }
#  body(ofunc) <- oexpr
#  formals(ofunc) <- as.arglist(onames)
#  
#  ##################################
#  
#  cexpr <- lazyeval::f_rhs(cform)
#  cexpr <-  windowExprRE(cexpr)
#  cnames <- getNames(cexpr)
#  crecurs <- isRecursive(cexpr, 'close')
#  
#  
#  cfunc <- function() {  }
#  body(cfunc) <- cexpr
#  formals(cfunc) <- as.arglist(cnames)
#  
#  
#  
#  ############################
#  function(vec, start = 1, end = length(vec)) {}
#  
#  list(ofunc, cfunc)
#  
#  
#  
#  
#  
# 
#  
# }
# 
# 
# isRecursive <- function(expr, target) {
#  any(sapply(getNames(expr), identical, y = target))         
# }
# 
# windowExprRE <- function(expr) {
#  if (length(expr) == 1) { 
#   val <- eval(expr)
#   
#   if (is.character(val)) expr <- call('grep', expr, quote(vec))
#   
#  } else {
#           for (i in 2:length(expr)) {
#            expr[[i]] <- Recall(expr[[i]])         
#            }
#  }
#           
#  expr          
#           
#           
# }
# 
# 
# getWindows <- function(vec, start = 1, end = length(vec), open, close, type = 'exclude') {
#   rawinds <- windowIndices(vec, open, close)
#   inds <- windowEdges(rawinds, start, end, type)
#   
#   inds <- do.call('Map', c(`:`, inds))
#   
#   lapply(inds, function(i) vec[i])
#   
# }


windowEdges <- function(inds, start, end, type = 'exclude') {
          if (type == 'exclude') {
                    ok <- !unlist(do.call('Map', c(`|`, lapply(inds, function(x) x < start | x > end | is.na(x)))))
          }
          if (type == 'trim') {
                    inds <- lapply(inds, 
                                  function(x) {
                                    x[x < start] <- start
                                    x[x > end] <- end
                                    x
                                  })
                    inds$Open[is.na(inds$Open)] <- start
                    inds$Close[is.na(inds$Close)] <- end
                    
                    ok <- !unlist(do.call('Map', c(`>=`, inds)))
          }
          return(lapply(inds, '[', ok))
          
          
}


