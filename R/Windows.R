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
#' ---------------------------------------------->      NEEDS DOCUMENTATION          <----------------------------------------------------
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



windowEdges <- function(inds, start, end, bounds = 'exclude', nested = TRUE) {
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

#' Windowing data
#' 
#' @name humWindows
NULL

#' @export
#' @rdname humWindows
nested <- function(x, open = '(', close = ')', depth = 1L) {
  open <- gsub('\\(', '\\\\(', open)
  opens <- stringi::stri_count_regex(x, open)
  
  close <- gsub('\\)', '\\\\)', close)
  closes <- stringi::stri_count_regex(x, close)
  
  if (!any(opens) && !any(closes)) return(integer(length(x)))
  
  openscum  <- cumsum(opens)
  closescum <- cumsum(lag(closes, 1L, fill = 0L))
  depth_vec <- openscum - closescum
  if (tail(depth_vec, 1L) != 0L && 
      tail(closescum, 1L) == 0L) .stop("In your call to humdrumR::nest", 
                                       "the input vector does not have matching {open}", 
                                       " and {close} tokens.")
  
  indices <- rbindlist(c(list(data.table(Open = integer(0L), Close = integer(0L), Depth = integer(0L))),
                         lapply(setdiff(intersect(depth_vec, depth), 0L),
                                \(d) {
                                  
                                  dhit <- ifelse(pmax(opens - 1, 0) == d, depth_vec - pmax(opens - 1, 0), depth_vec) == d
                                  data.table(Open = which(as.logical(opens) & dhit),
                                             Close = which(as.logical(closes) & dhit),
                                             Depth = d)
                                })))  
  
  if (0L %in% depth && 0L %in% depth_vec) {
    zeroblocks <- segments(as.integer(depth_vec == 0L)) * (depth_vec == 0L)
    zeroblocks[zeroblocks == 0L] <- NA_integer_
    zeroblocks <- tapply(seq_along(x), zeroblocks, simplify = FALSE,
                         \(block) {
                           data.table(Open = min(block),
                                      Close = max(block),
                                      Depth = 0L)
                           })
    indices <- rbindlist(c(list(indices), zeroblocks))
  }
 
  setorder(indices, Open, Depth) 
  indices

  
}


# context ~ open('(') ~ close(next(open) - 1)
# context ~ open('(') ~ close(')')
# context ~ close(';') ~ open(next(close) - 5)
# context ~ close(';') ~ open(next(close) - 'IV|ii')
# context ~ open(hop = 1, start = 'a', end = ';') ~ close(open + 4)
# context ~ open(hop = 1, start = 'a', end = ';') ~ close(open + ';', open + 4)
# context ~ open('1') ~ close(';') ~ open(close + next('1'))

# context ~ '(' ~ Next(open) - 1
# context ~ '(' ~ ')'
# context ~ '(' ~ ')' ~ nested(depth = 1)
# context ~ Next(close) - 5       ~ ';'
# context ~ Next(close) - 'IV|ii' ~ ';'
# context ~ 'a' ~ hop(5) ~ open + 4 ~ ';'

# context ~ windower(Token, '(', Next(open) - 1L))
# context ~ windower(Token, '(', ')')
# context ~ windower(Token, '(', ')', nested = TRUE, depth = 1:2)
# context ~ windower(Token, close - 5, ';')
# context ~ windower(Token, before(close, 'IV|ii'), ';')

windower <- function(x, open, close = Next(open) - 1L, start =1, end = length(x), nest = FALSE, depth = NULL) {
  open <- rlang::enexpr(open)
  close <- rlang::enexpr(close)
  
  openexpr <- deparse(open)
  closeexpr <- deparse(close)
  
  open_depends  <- !is.null(namesInExpr('close', open))
  close_depends <- !is.null(namesInExpr('open', close))
  
  if (open_depends && close_depends) .stop("Contextual window anchors (open or close) can't refer to each other.")
  
  open  <- symbolApply(open,  \(atom) if (is.character(atom)) rlang::expr({grepi_multi(x, !!atom)}) else atom)
  open <- recurseExpr(open, \(expr) expr[[1]] == quote(hop), \(expr) rlang::expr(seq(!!start, !!end, by = !!expr[[2]])))
  close <- symbolApply(close, \(atom) if (is.character(atom)) rlang::expr({grepi_multi(x, !!atom)}) else atom)
  close <- recurseExpr(close, \(expr) expr[[1]] == quote(hop), \(expr) rlang::expr(seq(!!start, !!end, by = !!expr[[2]])))
  
  if (open_depends) {
    close <- eval(close)
    open <- eval(open)
    
  } else {
    open <- eval(open)
    close <- eval(close)
  }
  
  if (length(open) != length(close)) {
    if (open_depends || close_depends) {
      .stop("Sorry, in the windower function, if use open or close arguments that refer to each other,",
            "the resulting dependent must be the same length as the thing it depends on.",
            "In this case, {{ {openexpr} }} evaluates with length {length(open)}, while {{ {closeexpr} }}",
            "evaluates with length {length(close)}.",
            "It's best to stick with the basic usages of windower described in the documentation (?windower).",
            "More sophisticated things are possible, but you'll need to do a little more 'manual' wrangling.")
    } 
    close <- close[close > min(open)]
    # close <- close %after% open
    
  }

  
  output <- data.table(Open = open, Close = close)
  output <- depth(output, nest = nest, depth = depth)
  
  
  attr(output, 'vector') <- x
  output
  
}

print.windows <- function(x) {
  plot.new()
  vec <- attr(x, 'vector')
  
  plot.window(ylim = c(0, max(x$Depth)),xlim = c(0, length(vec)))
  
  text(1:length(vec), rep(0, length(vec)), vec)
  y <- x$Depth
  graphics::arrows(x$Open, x$Close, y0 = y, y1= y, angle = 90, code = 3)
  
 
}


depth <- function(ind, nest = FALSE, depth = NULL) {
  open <- ind$Open
  close <- ind$Close
  
  steps <- c(rep(1, length(open)), rep(-1, length(close)))[order(c(open, close))]
  contour <- cumsum(steps)
  
  ind$Depth <- contour[steps == 1L]
  depthclose <- contour[steps == -1L]
  
  if (nest) {
    matches <- which(outer(ind$Close, ind$Open, `>`) & outer(depthclose + 1L, ind$Depth, '=='), arr.ind = TRUE)
    newind <- matches[ !duplicated(matches[ , 'col']), 'row']
    
    ind$Close <- ind$Close[newind]
    depthclose <- depthclose[newind]
    
  }
  
  if (!is.null(depth)) {
    depth <- ifelse(depth < 0L, max(ind$Depth) + 1L + depth, depth)
    ind <- ind[Depth %in% depth]
  }
  
  ind 
}

grepi_multi <- function(x, pattern) {
  pattern <- stringr::str_replace_all(pattern, '\\(' , '\\\\(')
  pattern <- stringr::str_replace_all(pattern, '\\)' , '\\\\)')
  
  ns <- x %grepn% pattern
  rep(x %grepi% pattern, ns[ns > 0L])
}






first <- function(x) nth(x, 1L)
last  <- function(x) lth(x, 1L)
nth <- first <- function(x, n) {
  if (n > length(x)) return(vectorNA(1L, class(x)))
  x[n]
}
lth  <- function(x, n) {
  if (n > length(x)) return(vectorNA(1L, class(x))) 
  x[(length(x) + 1L) - n]
  }
  
Next <- function(x) lead(x, 1L)
Prev <- function(x) lag(x, 1L)


`%before%` <- function(hits, anchors) {
  i <- findInterval(anchors, hits, left.open = T)
  i[i == 0 | i > length(hits)] <- NA_integer_
  hits[i]
}
`%after%` <- function(hits, anchors) {
  -rev(sort(-hits) %before% sort(-anchors))
}
