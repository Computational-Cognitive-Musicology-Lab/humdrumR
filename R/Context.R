#################################-
# Finding context ############----
#################################-


## Parsing context expressions ----




parseContextExpression <- function(expr, other) {
  if (is.null(expr)) expr <- quote('.')
  expr <- substituteName(expr, list(N = quote(length(.)),
                                    'next' = rlang::expr(lead(!!other, 1L)),
                                    prev   = rlang::expr(lag(!!other, 1L)),
                                    last   = rlang::expr(lag(!!other, 1L))))
  
  regexes <- c()
  
  expr <- withinExpression(expr, applyTo = c('atomic'),
                           \(Type, Class) Type == 'atomic' && Class == 'character',
                           \(exprA) {
                             regexes <<- c(regexes, exprA$Args[[1]])
                             exprA$Args <- rlang::expr(grepi_multi(., !!exprA$Args[[1]]))
                             # exprA$Args <- rlang::expr(grepn(x, !!exprA$Args[[1]]))
                             exprA
                           } )
  
  exprA <- analyzeExpr(expr)
                        
  if (exprA$Head == ':') expr <- parseContextRange(exprA)
  if (exprA$Head == '|') {
    exprA$Head <- 'c'
    expr <- unanalyzeExpr(exprA)
  }
  
  attr(expr, 'regexes') <- regexes
  expr
}

parseContextRange <- function(exprA) {
  
  arg1 <- exprA$Args[[1]]
  
  arg1A <- analyzeExpr(arg1)
  if (arg1A$Type == 'call' && arg1A$Head == ':') {
    by <- arg1A$Args[[2]]
    from <- arg1A$Args[[1]]
  } else {
    from <- arg1A$Args[[1]]
    by <- 1
  }
  
  to <- exprA$Args[[2]]
  # to <- withinExpression(to, \())
  
  rlang::expr(seq((!!from)[1], to = (!!to)[1], by = !!by))
  
}

## Actual window finding ----

#' Create arbitrary "context" across vectors.
#' @export
context <- function(x, open, close, reference = x, ..., 
                    nested = FALSE, depth = NULL, groupby = NULL,
                    min_length = 1L, max_length = Inf,
                    collapse = TRUE, sep = ', ',
                    alignToOpen = TRUE, inPlace = FALSE) {
  
  open  <- rlang::enexpr(open)
  close <- rlang::enexpr(close)
  windowFrame <- findWindows(reference, open, close, ...,
                             nested = nested, depth = depth, groupby = groupby,
                             min_length = 1L, max_length = Inf)
  
  regexes <- attr(windowFrame, 'regexes')
  for (re in escapebraces(regexes)) x <- gsub(re, '', x)
 
  expr <- if (collapse) rlang::expr(paste(.x., collapse = !!sep)) else rlang::expr(.x.)
  
  .applyWindows(data.table(.x. = x), windowFrame, expr, activeField = '.x.',
                inPlace = inPlace, alignToOpen = alignToOpen)
  
  
  
  
}
  
  
findWindows <- function(x, open, close = quote(next - 1), ..., 
                        activeField = 'Token', 
                        nested = FALSE, depth = NULL, groupby = NULL,
                        min_length = 1L, max_length = Inf) {
  
  
  if (!is.data.frame(x)) x <- setNames(data.table::data.table(. = x), activeField %||% '.')
  
  open <- parseContextExpression(open, other = quote(close))
  close <- parseContextExpression(close, other = quote(open))
  regexes <- union(attr(open, 'regexes'), attr(close, 'regexes'))

  if (!is.null(activeField)) {
    open  <- substituteName(open, list('.' = rlang::sym(activeField)))
    close <- substituteName(close, list('.' = rlang::sym(activeField)))
  }
  
  # do the expressions reference each other?
  
  openDepends <- 'close' %in% namesInExpr('close', open) 
  closeDepends <- 'open' %in% namesInExpr('open', close)
  
  if (openDepends && closeDepends) .stop("In your call to context, your open and clsoe arguments are mutually referential in a circular manner.")
  

  open_indices <- close_indices <- NULL
  for (i in order(c(openDepends, closeDepends))) { # this is all just to make sure any the independent expressions are evaluated first
    assign(c('open_indices', 'close_indices')[i],
           rlang::eval_tidy(list(open, close)[[i]], 
                            data = c(x, list(open = open_indices, close = close_indices))))
  }
  
  # 
  windowFrame <- align(open_indices, close_indices,
                       nested = nested, 
                       min_length = min_length, max_length = max_length,
                        ...)
  windowFrame <- windowFrame[Open >= 1L & Open <= nrow(x) & Close >= 1L & Close <= nrow(x)]
  if (length(groupby)) removeCrossing(windowFrame, groupby)

  windowFrame <- depth(windowFrame, depth = depth, ...)

  windowFrame <- windowFrame[Reduce('&', lapply(windowFrame, Negate(is.na)))]
  
  attr(windowFrame, 'regexes') <- regexes
  
  windowFrame
  
}


print.windows <- function(x) {
  plot.new()
  vec <- attr(x, 'vector')
  
  plot.window(ylim = c(0, max(x$Depth)),xlim = c(0, length(vec)))
  
  text(1:length(vec), rep(0, length(vec)), vec)
  y <- x$Depth
  graphics::arrows(x$Open, x$Close, y0 = y, y1= y, angle = 90, code = 3)
  
 
}

### Window finding rules ----
# 
align <- function(open, close, nested = FALSE, min_length = 1L, max_length = Inf,...) {
  # output <- if (length(open) == length(close) && all(ordPredicate(open, close), na.rm = TRUE)) {
    # data.table(Open = open, Close = close)
  # } 
  
  open <- open[!is.na(open)]
  close <- close[!is.na(close)]
  dists <- outer(close, open, '-')
  
  
  steps <- c(rep(1, length(open)), rep(-1, length(close)))[order(c(open, close))]
  depth <- sigma(steps) # skips NA
    
  if (nested) {
    samelevel <- outer(depth[steps == -1L] + 1L, depth[steps == 1L], '==')
    dists[!samelevel] <- NA
  } 
  
  accept <- rightmost(!is.na(dists) & dists >= min_length & dists <= max_length)
  accept <- shunt(accept)
  
  accept <- which(accept, arr.ind = TRUE)
  
  windowFrame <- data.table(Open = open[accept[ , 'col']],
                            Close = close[accept[ , 'row']])
  
  windowFrame$Depth <-  depth[steps == -1L][accept[ , 'row']] + 1L
  windowFrame[ , Length := Close - Open]
  
  setorder(windowFrame, Open, Close)
  
  windowFrame
  
}

shunt <- function(accept) {
  # "shunt" looks for window Close positions that have aleady been matched
  # with a previous Open, and "shunts" them to the next Close.
  
  bad <- colSums(accept) > 1L
  
  while (any(bad)) {
    ind <- which(accept, arr.ind = TRUE)
    ind <- ind[ind[ , 'col'] %in% which(bad), ]
    
    accept[ind] <- FALSE
    
    ind[ , 'col'] <- ind[ , 'col'] - unlist(lapply(rle(ind[ , 'col'])$lengths, \(l) l - seq_len(l)))
    accept[ind] <- TRUE
    ind <- ind[ind[ , 'col'] >= 1L, ]
    
    bad <- colSums(accept) > 1L
  }
  
  accept
  
}
# 
# align <- function(indices, length, depth = NULL,
#                   overlap = 'none', 
#                   duplicateOpen = FALSE, duplicateClose = TRUE,
#                   min_length = 1, max_length = Inf, 
#                   groupby = list()) {
#   open <- indices$Open
#   close <- indices$Close
#   
#   
#   windowFrame <- as.data.table(expand.grid(iOpen = seq_along(open), iClose = seq_along(close)))
#   windowFrame[ , Open := open[iOpen]]
#   windowFrame[ , Close := close[iClose]]
#   setorder(windowFrame, iOpen, iClose)
#   
#   
#   windowFrame <- windowFrame[ , Length := Close - Open]
#   windowFrame <- windowFrame[Length <= max_length & Length >= min_length]
#   
#   if (length(groupby)) windowFrame <- removeCrossing(windowFrame, checkWindows(x, groupby))
#   
#   windowFrame[, XClose := seq_along(Open) - 1, by = factor(iOpen)]
#   windowFrame[, XOpen := rev(seq_along(Close) - 1L), by = factor(iClose)]
#   
#   windowFrame <- switch(overlap,
#                         nested = windowFrame[XOpen == XClose],
#                         upward = windowFrame[XOpen == 0L],
#                         downward = windowFrame[XClose == 0L],
#                         none = windowFrame[XClose == 0L & XOpen == 0L],
#                         windowFrame
#   )
#   if (overlap == 'shunted') {
#     # each open pairs with next UNUSED close, no sharing
#     windowFrame[ , X := seq_along(Open) - 1    , by = factor(iClose)]
#     windowFrame[ , Y := rev(seq_along(Open)) -1, by = factor(iOpen)]
#     windowFrame <- windowFrame[XClose == X | XOpen == Y]
#     windowFrame[, c('X', 'Y') := NULL]
#   }
#   
#   if (!duplicateOpen) windowFrame <- windowFrame[!duplicated(iOpen)]
#   if (!duplicateClose) windowFrame <- windowFrame[!duplicated(iClose)]
#   
#   windowFrame <- depth(windowFrame, depth = depth)
#   
#   windowFrame[, c('iClose', 'iOpen', 'XOpen', 'XClose') := NULL]
#   windowFrame
#   
#   
# }

### Sorting, filtering, or modifying windows ----

depth <- function(windowFrame, depth = NULL) {

  maxdepth <- max(windowFrame$Depth)
  windowFrame$RevDepth <- windowFrame$Depth - maxdepth - 1L
  
  if (!is.null(depth)) {
    windowFrame <- windowFrame[Depth %in% depth | RevDepth %in% depth]
  }
  
  windowFrame
}

removeCrossing <- function(windowFrame, groupby) {
  groupby <- do.call('paste', groupby)
  windowFrame[groupby[Open] == groupby[Close]]
}

##########################################-
# Applying functions in context #######----
##########################################-

# contextApply <- function(x, open, close, func = c, ..., groupby = list(), inPlace = FALSE, reference = x, vectorize = TRUE, openIndex = TRUE) {
#   
#   open  <- rlang::enexpr(open)
#   close <- rlang::enexpr(close)
#   
#   windowFrame <- findWindows(reference, open, close, ..., groupby = groupby)
#   func
#   .applyWindows(data.table(.x. = x), windowFrame, rlang::quo(func(.x.)), ..., activeToken = '.x.',
#                 inPlace = inPlace, openIndex = openIndex)
# }


.applyWindows <- function(dt, windowFrame, expr, activeField = 'Token', ..., 
                          inPlace = TRUE, alignToOpen = TRUE) {
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- dt[unlist(indices)]
  dt_extended[ , .windowN. := rep(seq_along(indices), lengths(indices))]
  
  results <- rlang::eval_tidy(rlang::quo({ dt_extended[ , list(list(!!expr)), by = .windowN.]}), env = parent.frame(1))$V1
  # should be a list of results, one result per window
  
  # if (!vectorize) {
  #   names(results_windowed) <- windowFrame[ , paste0(Open, ':', Close)]
  #   return(results_windowed)
  # }
  
  edges <- windowFrame[ , if (alignToOpen) Open else Close]
  edge <- if (alignToOpen) head else tail
  
  
  result_lengths <- lengths(results)
  newindices <- Map(indices, result_lengths, f = \(i, l) edge(i, n = l))
  
  
  output <- if (inPlace) dt[[activeField]] else rep(NA, nrow(dt))
  output[.unlist(newindices)] <- .unlist(results)
  
  if (inPlace) output[setdiff(unlist(indices), unlist(newindices))] <- NA
  
  output
  
} 

applyNgram <- function(n = 2, vecs, f = c, by = NULL, pad = TRUE, 
                       fill = NA, splat = !is.null(by), ...) {
  # vecs is list of vectors  of same length
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


# context tools ----



grepi_multi <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  ns <- x %~n% pattern %|% 0L
  rep(x %~i% pattern, ns[ns > 0L])
  
}

grepn <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  x %~n% pattern %|% 0L
  
}


windowsSum <- function(x, windowFrame, na.rm = FALSE, cuttoff = 10) {
  # takes elements of x and a window frame and quickly sums x within windows
  
  lengths <- table(windowFrame[Length > 1L, Length])
  
  vectorize <- lengths >= cuttoff & as.integer(names(lengths)) < 20L
  
  if (any(vectorize)) {
    maxsize <- max(as.integer(names(lengths)[vectorize]))
    
    na <- as(NA, class(x))
    for (l in 1:(maxsize - 1L)) {
      
      windowFrame[Length == l, 
              {
                curClose <- Open + l
                x[Open] <<- x[Open] + x[curClose]
                x[curClose] <<- na
                
              }]
      windowFrame <- windowFrame[Length != l]
    }
  }
  
  
  
  if (nrow(windowFrame)) x <- windowApply(x, sum, na.rm = na.rm, windows = windowFrame, passOutside = TRUE)
  
  x
  
  
}

