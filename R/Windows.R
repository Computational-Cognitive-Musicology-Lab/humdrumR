#' 
#' @export
applyNgram <- function(n = 2, vecs, f = c, by = NULL, pad = TRUE, 
                       fill = NA, splat = !is.null(by), ...) {
  # x is list of vectors  of same length
  if (!is.null(by)) vecs <- lapply(vecs, split, f = by)
  
  if (n == 0) stop("You called applyNgram with n = 0, but you can't make an 0-gram!", call. = FALSE)
  if (!allsame(lengths(vecs))) stop("You have tried to applyNgram across multiple vectors of different lengths, but this is not supported.", .call = FALSE)
  
  n <- n - (sign(n))
  
  
  if (n == 0) { 
    output <- do.call('Map', c(f, vecs))
  } else {
    
    starts <- seq_along(vecs[[1]])
    if (pad) {
      vecs <- lapply(vecs, \(vec) c(rep(NA, abs(n)), vec, rep(NA, abs(n))))
      starts <- starts + abs(n)
    } else {
      starts <- starts[ (starts + n) <= length(vecs[[1]]) & starts + n >= 1]
    }
    
    inds   <- if (sign(n) == 1) Map(`:`, starts, starts + n) else Map(`:`, starts + n, starts)
    
    #
    
    ngs <- lapply(vecs, 
                  function(vec) {
                    lapply(inds, \(i) vec[i])
                  })
    .f <- if (splat) { 
      function(...) {do.call('f', list(...)) }
      } else {
        f
      }
    output <- do.call('Map', c(.f, ngs))
    
    if (pad && !is.na(fill)) output <- lapply(output,
                                                function(out) {
                                                  if (is.character(out)) gsub('NA', fill, out) else `[<-`(out, is.na(out), fill)
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



###########################

matchClose <- function(x, table, n = 1L) {
          
          if (n <= 0) {
                    table <- table[table < max(x)]
          } else {
                    table <- table[table >= min(x)]
          }
          
          intervals <- findInterval(x, sort(table))
          
          if (n == 0) {
                    after <- intervals + 1L
                    before <- intervals 
                    after[after <= 0] <- length(table) + 1L
                    before[before <= 0] <- length(table) + 1L
                    
                    before <- table[before]
                    before[is.na(before)] <- Inf
                    after <- table[after]
                    after[is.na(after)] <- Inf
                    output <- ifelse(abs(x - before) >= abs(x - after), after, before)
          } else {
          
                    intervals <- intervals + n
                    intervals[intervals <= 0] <- length(table) + 1L
                    output <- table[intervals]
          
          }
          # hits <- sapply(seq_along(after), \(a) x[intervals == a][1])
          
          # list(Before = after, After = hits)
          list(Table = x, X = output)
}


findAfter <- function(vec, after, pattern, npattern = 1) {
 
    if (is.numeric(pattern)) {
      pmaxmin(after + pattern, 1, length(vec))
    } else {
     inds <- grep(pattern, vec)      
     matchClose(inds, after, npattern)$Close
    }
                   
}


trans <- function(expr) {
 if (!is.call(expr) || !deparse(expr[[1]]) %in% c('+', '-')) {
           return(\(x) x) 
 } else {
  val <- expr[[3]]
  if (is.call(val) && deparse(val[[1]]) == '*') {
   n <- val[[3]]
   val <- val[[2]]
  } else {
   n <- quote(1)         
  }
  
  from <- expr[[2]]
           
 } 
 
 .func <- function() {}
 body(.func) <- call('findAfter', quote(vec), from, val, n)
 
 formls <- alist(vec = , x = )
 names(formls)[2] <- deparse(from)
 
 formals(.func) <- formls
 
 .func
 
}



find.anchors <- function(vec, expr) {
  
  if (!is.call(expr)) return(list(Hits = NULL, Expr = expr))
          
  hits <- list()
  for (i in 2:length(expr)) {
            
                        if (is.character(expr[[i]])) {
                                  hit <- list(grep(expr[[i]], vec))
                                  names(hit) <- expr[[i]]
                                  hits <- c(hits, hit)
                                  
                                  expr[[i]] <- as.name(expr[[i]])
                                  
                        } else {
                                  recur <- Recall(vec, expr[[i]])        
                                  hits <- c(hits, recur$Hits)
                                  expr[[i]] <- recur$Expr
                        }
  }
                   
  list(Hits = hits, Expr = expr)
}

#' Applying functions across arbitrary windows.
#' @export
#' @name humWindows
windows <- function(df, form, with = list(), ..., 
                    start = 1L, end = nrow(df), bounds = 'exclude') {
          if (start < 1) start <- 1L
          if (end > nrow(df)) end <- nrow(df)
          
          
 with <- lapply(c(with, list(...)), rlang::as_quosure)
 with[] <- lapply(with, rlang::eval_tidy, data = df)
 with <- c(with, df)
 
 open  <- parseWindowExpression(rlang::f_lhs(form))
 close <- parseWindowExpression(rlang::f_rhs(form))
 
 ##
 for (obj in c('open', 'close')) {
           ind <- eval(get(obj)$Expr, envir = with)
           if (is.logical(ind))  {
                     ind <- if (is.null(dim(ind)))  {
                               which(ind)
                     } else {
                               sort(unique(which(ind, arr.ind = TRUE)[ , 'row']))
                     }
           }
           assign(paste0(obj, '.ind'), value = ind, envir = environment())
 }
 
 output <- matchClose(open.ind, close.ind, n = close$N)
 names(output) <- c("Open", "Close")
 
 windowEdges(output, start, end, bounds = bounds)
}

parseWindowExpression <- function(expr) {
 if(!is.call(expr)) return(list(Expr = expr, N = 1L))
          
 if (deparse(expr[[1]]) == '[') {
        n <- eval(expr[[3]])
        expr <- expr[[2]]
 } else {
           ns <- c()
           for (i in 2:length(expr)) {
            subexpr <-  Recall(expr[[i]])         
            expr[[i]] <- subexpr$Expr
            ns <- c(ns, subexpr$N)
           }
           n <- max(ns)
 }
 list(Expr = expr, N = n)
}

#' @export
#' @name humWindows
hop <- function(vec, pattern, start = 1L, end = length(vec)) {
          if (is.character(start)) start <- grep(start, vec)
          if (is.character(end))   end <- grep(end, vec)
          
          interval <- sum(pattern)
          
          if (interval == 0L) stop("In call to humdrumR::hop, pattern argument cannot sum to zero")
          
          fullpattern <- rep(pattern, ceiling(length(vec) / abs(interval)))
          ind <- if (interval > 0L) {
                    cumsum(c(start, fullpattern))
          } else {
                    cumsum(c(end, fullpattern))
          }
          
          ind
}



windowEdges <- function(inds, start, end, bounds = 'exclude') {
          if (pmatch(bounds, 'exclude', 0L)) {
                    ok <- !unlist(do.call('Map', c(`|`, lapply(inds, \(x) x < start | x > end | is.na(x)))))
          } else {
          # if (pmatch(bounds, 'trim', 0L)) {
                    inds <- lapply(inds, 
                                   \(x) {
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


#' @export
#' @name humWindows
nest <- function(vec, open, close, depth = 1) {
          opens <- stringi::stri_count_regex(vec, open)
          closes <- stringi::stri_count_regex(vec, close)
          
          if (!any(opens) && !any(closes)) return(list(Open = c(), Close = c()))
          
          openscum  <- cumsum(opens)
          closescum <- head(cumsum(c(0, closes)), -1)
          depths <- openscum - closescum
          if (tail(depths, 1) != 0L && 
              tail(closescum, 1) == 0L) stop(call. = FALSE,
                                             paste0("In your call to humdrumR::nest", 
                                                    "the input vector does not have matching ", 
                                                    open, 
                                                    " and ", 
                                                    close, " tokens."))
          
          cdepth <- sapply(1:max(closes), \(m) (depths * (closes >= m)) - (closes > 0) * (m - 1))
          odepth <- sapply(1:max(opens ), \(m) (depths * (opens >= m)) - (opens > 0) * (m - 1))
          
          lapply(1:max(depths),
                 \(d) {
                           cbind(opens = apply(odepth == d, 1, any),
                                 closes = apply(cdepth == d, 1, any))
                           
                 }) -> hits
          
          out <- do.call('abind', c(hits, along = 3))
          
          dimnames(out) <- list(Vector = vec,
                                Bound = c('Open', 'Close'),
                                Depth = 1:dim(out)[3])
          
          if (!any(depth <= length(dim(out)))) return(list(Open = c(), Close = c()))
          
          list(Open  = which(out[ , 1, depth, drop = FALSE], arr.ind = T)[ , 'Vector'],
               Close = which(out[ , 2, depth, drop = FALSE], arr.ind = T)[ , 'Vector'])
          
          
}



