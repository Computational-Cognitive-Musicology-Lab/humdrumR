

#
#
# '(' ~ ')'
# 
# 1:N ~ +3
# -3 ~ 1:2:N
#
# last + 1 ~ ';'
# 'e' %after% last  ~ ';'
#
# 1:5:N ~ next

# Finding windows ----

## Parsing "anchor" ----

parseAnchors <- function(x, open, close, start = 1, end = length(x)) {
 
  
  open <- parseAnchor(open, x, 'open')
  close <- parseAnchor(close, x, 'close')
  start <- parseAnchor(start, x, 'start')
  end <- parseAnchor(end, x, 'end')
  
  anchors <- list(open, close, start, end)
  names(anchors) <-  names <- c('open', 'close', 'start', 'end')
  references <- lapply(anchors,
                       \(anchor) {
                         if (rlang::is_formula(anchor)) {
                           names %in% namesInExprs(names, anchor)
                         } else {
                           logical(length(names))
                         }
                       })
  references <- do.call('cbind', references)
  colnames(references) <- names
  
  # check that formulae can be evaluated in a safe order
  if (any(references)) {
    bad <- abs(eigen(diag(nrow(references)) - references)$values) < .00001 # is graph connected?!
    if (any(bad)) .stop("In your calls to context, your formulae arguments are mutually referential in a circular manner.")
    
    anchors <- local({
      forms <- list()
      while(length(forms) < length(anchors)) {
        safe <- colSums(references) == 0L
        forms <- c(forms, anchors[colnames(references)[safe]])
        references <- references[!safe, , drop = FALSE]
        references <- references[, !safe, drop = FALSE]
      }
      forms
    })
    
  }
  
   
  
  evaled <- local( {
    env <- new.env()
    for (arg in names(anchors)) {
      anchor <- anchors[[arg]]
      if (rlang::is_formula(anchor)) {
        assign(arg, eval_tidy(anchor, data = env), envir = env)
      } else {
        assign(arg, anchor, envir = env)
      }
      
    }
    as.list(env)
  })
  evaled
}

parseAnchor <- function(anchor, x, name)  UseMethod('parseAnchor')
  
parseAnchor.list <- function(anchor, x, name) {
  do.call('c', lapply(anchor, parseAnchor, x = x, name = name))
}
parseAnchor.logical <- function(anchor, x, name) {
  if(length(anchor) != length(x)) .stop("If the {name} argument in a call to context is logical", 
                                        "it must be the same length as the vector you are windowing across.")
  
  which(anchor)
}
parseAnchor.numeric <- function(anchor, x, name) {

    if (any(anchor != round(anchor))) .stop("In a call to context, a numeric {name} argument needs to be an integer value.")
    anchor <- as.integer(anchor)
    
    if (any(anchor < 0) && any(anchor > 0)) .stop("In a call to context, you can't mix negative and positive numbers in a {name} argument.")
    if (any(length(abs(anchor)) > length(x))) .stop("In a call to context, the {name} argument can't be numeric values greater than the length",
                                                    "of the vector you are windowing across.")
    
    if (all(anchor < 0)) anchor <- 1L + length(x) + anchor 
    anchor
}

parseAnchor.character <- function(anchor, x, name) {
  anchor <- grepi_multi(x, anchor)
  anchor
}
  
parseAnchor.formula <- function(anchor, x, name){ 
    env <- environment(anchor)
    env$x <- x
    env$name <- name
    anchors <- splitExpression(anchor, on = '\\|')
    if (length(anchors) > 1L) {
      
      anchors <- parseAnchor(unname(anchors), x, name)
      anchors <- lapply(anchors, rlang::f_rhs)
      # anchors <- lapply(anchors, \(expr) call('parseAnchor', expr, x = quote(x), name = quote(name)))
      anchors <- do.call('call', c('c', anchors, use.names = FALSE), quote = TRUE)
      return(rlang::new_quosure(anchors, env = env))
    }
    anchor <- rlang::f_rhs(anchor)
    anchor <- withinExpression(anchor, 
                               \(Head) Head == 'hop', 
                               \(exprA) {
                                 exprA$Args <- c(quote(x), exprA$Args)
                                 exprA
                                 })
    
    anchor <- withinExpression(anchor, applyTo = 'atomic',
                               \(Class) Class == 'character',
                               \(exprA) {
                                 exprA$Args <- list(quote(x), exprA$Head)
                                 exprA$Head <- 'grepi_multi'
                                 exprA$Type <- 'call'
                                 exprA
                               })
    rlang::new_quosure(anchor, env)
}



grepi_multi <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  ns <- x %~n% pattern %|% 0L
  rep(x %~i% pattern, ns[ns > 0L])
  
}

grepn <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  x %~n% pattern %|% 0L
  
}


## Parsing context expressions ----

parseFormula <- function(formula) {
  open <- parseContextExpression(rlang::f_lhs(formula))
  close <- parseContextExpression(rlang::f_rhs(formula))
  
  list(open = open$Expr, close = close$Expr, Regexes = union(open$Regexes, close$Regexes))
}


parseContextExpression <- function(expr) {
  if (is.null(expr)) expr <- quote('.')
  expr <- substituteName(expr, list(N = quote(length(x)),
                                    'next' = quote(lead(., 1L)),
                                    last = quote(lag(., 1L))))
  
  regexes <- c()
  
  expr <- withinExpression(expr, applyTo = c('atomic'),
                           \(Type, Class) Type == 'atomic' && Class == 'character',
                           \(exprA) {
                             regexes <<- c(regexes, exprA$Args[[1]])
                             exprA$Args <- rlang::expr(grepi_multi(x, !!exprA$Args[[1]]))
                             # exprA$Args <- rlang::expr(grepn(x, !!exprA$Args[[1]]))
                             exprA
                           } )
  
  exprA <- analyzeExpr(expr)
                        
  if (exprA$Head == ':') expr <- parseContextRange(exprA)
  if (exprA$Head == '|') {
    exprA$Head <- 'c'
    expr <- unanalyzeExpr(exprA)
  }
  list(Expr = expr, Regexes = regexes)
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
context <- function(x, formula, ..., 
                    nested = FALSE, depth = NULL, groupby = NULL,
                    min_length = 1L, max_length = Inf,
                    collapse = TRUE, sep = ', ',
                    openIndex = TRUE, passOutside = FALSE) {
  
  
  windowFrame <- findWindows(x, formula, ...,
                             nested = nested, depth = depth, groupby = groupby,
                             min_length = 1L, max_length = Inf)
  
  regexes <- attr(windowFrame, 'regexes')
  for (re in escapebraces(regexes)) x <- gsub(re, '', x)
 
  
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  x_windows <- lapply(indices, \(i) x[i])
  
  edges <- windowFrame[ , if (openIndex) Open else Close]
  if (collapse) {
    output <- rep('.', length(x))
    output[edges] <- sapply(x_windows, paste, collapse = sep)
  } else {
    output <- vector('list', length(x))
    output[edges] <- x_windows
    
  }
  
  if (passOutside) {
    outside <- setdiff(seq_along(x), unlist(indices))
    output[outside] <- x[outside]
  }
  output
  
  
  
  
}
  
  
findWindows <- function(x, formula, ..., 
                        nested = FALSE, depth = NULL, groupby = NULL,
                        min_length = 1L, max_length = Inf) {
  
  if (!rlang::is_formula(formula)) formula <- rlang::new_formula(quote('.'), 
                                                                 rhs = rlang::expr(!!formula))
  formulae <- parseFormula(formula)
  regexes <- formulae$Regexes
  formulae$Regexes <- NULL
  
  # do the formulae reference each other?
  independent <- lengths(lapply(formulae, namesInExpr, names = '.')) == 0
  
  if (!any(independent)) .stop("In your call to context, your formula argument is mutually referential in a circular manner.")
  
  indices <- list(Open = NULL, Close = NULL)
  for (i in order(!independent)) {
    indices[[i]] <- rlang::eval_tidy(formulae[[i]], data = list(x = x, . = indices[[setdiff(1:2, i)]]))
  }
  # definitions <- enquos(...)
  
  
  # 
  windowFrame <- align(indices$Open, indices$Close, 
                       nested = nested, 
                       min_length = min_length, max_length = max_length,
                        ...)
  windowFrame <- windowFrame[Open >= 1L & Open <= length(x) & Close >= 1L & Close <= length(x)]
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
  depth <- cumsum(steps)
    
  if (nested) {
    samelevel <- outer(depth[steps == -1L] + 1L, depth[steps == 1L], '==')
    dists[!samelevel] <- NA
  } 
  
  accept <- rightmost(!is.na(dists) & dists >= min_length & dists <= max_length)
  accept <- shunt(accept)
  
  windowFrame <- data.table(Open = open[row(accept)[accept]], Close = close)
  
  windowFrame$Depth <-  depth[steps == -1L] + 1L
  
  
  setorder(windowFrame, Open, Close)
  
  
  windowFrame
}

shunt <- function(accept) {
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

# nestedWindows <- function(windowFrame) {
#   nested <- windowFrame[ , outer(Open, Open, '>=')] & windowFrame[ , outer(Close, Close, '<=')]
#   nested[row(nested) == col(nested)] <- FALSE
#   nested
#   
# }


# cases: 
#   ( ( ) ( ( ) ( )
#   (---)
#   (---------)
#   (-------------)
#     (-)   (-) (-)
#     (-------)
#     (-----------)
#         (---)
#         (-------)
#           (-)
#           (-----)
#               (-)
# Each open is paired once with NEXT close
#   (---)
#     (-)   (-) (-)
#         (---)
# Each open is paired once with next UNUSED close
#   (---) 
#     (-------) 
#         (-------)
# Each open is paired with with NEXT close at same level
#     (-)   (-) (-)
#
#   ( ( ) ) ( )
# Each open is paired once with NEXT close
#   (---)
#     (---) (-)
# Each open is paired once with next UNUSED close
#   (---)
#     (---) (-)
# 
# align <- function(indices, length, min_length = 1, max_length = Inf, ...) {
#   open <- indices$Open
#   close <- indices$Close
# 
#   depth <- cumsum(open - lag(close, fill = 0))
# 
#   pairs <- as.data.table(expand.grid(Open = unique(which(open > 0)),
#                                      Close = unique(which(close > 0))))
#   pairs <- pairs[ , Length := Close - Open]
#   pairs <- pairs[Length <= max_length & Length >= min_length]
# 
#   setorder(pairs, Open, Close)
#   pairs[ , openDepth := depth[Open]]
#   pairs[ , closeDepth := depth[Close]]
#   pairs[,NOpen := open[Open]]
#   pairs[,NClose := close[Close]]
#   
#   pairs[,XClose := cumsum(NClose) - 1L, by = factor(Open)]
#   pairs[ , X := cumsum(NOpen) - 1L, by = factor(Close)]
#   pairs[, XOpen := rev(cumsum(rev(NOpen))) - 1L, by = factor(Close)]
#   browser()
#   pairs
# # 
# }
# 


# Tools ----


#' @export
#' @name context
hop <- function(along, pattern = 1, start = 1L, end = length(along)) {
  if (is.character(start)) start <- grep(start, along)
  if (is.character(end))   end <- grep(end, along)
  
  interval <- sum(pattern)
  
  if (interval == 0L) stop("In call to humdrumR::hop, pattern argument cannot sum to zero")
  
  fullpattern <- rep_len(pattern, ((end - start) / abs(interval)) * length(pattern))
  ind <- if (interval > 0L) {
    cumsum(c(start, fullpattern))
  } else {
    cumsum(c(end, fullpattern))
  }
  
  ind
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

# 
# 


# Applying functions across windows ----

#' @rdname context
#' @export
windowApply <- function(x, func = c, windows, ..., groupby = list(), passOutside = FALSE, reference = x, rebuild = TRUE, leftEdge = TRUE) {
  if (length(x) != length(reference)) .stop('In a call to windowApply, x and reference must be the same length!')
  
  if (missing(windows)) windows <- windows(reference, ...)

  
  indices <- Map(':', windows$Open, windows$Close)
  
  x_windowed <- lapply(indices, \(i) x[i])
  
  result_windowed <- lapply(x_windowed, func, ...)
  
  if (rebuild) {
    
    if (all(lengths(result_windowed) == 1L)) {
      result <- .unlist(result_windowed)
      x[if (leftEdge) windows$Open else windows$Close ] <- result
      if (passOutside) {
        x[setdiff(unlist(indices), c(windows$Open, which(is.na(x))))] <- as(NA, class(result))
        
      } else {
        notstarts <- seq_along(x)
        notstarts <- notstarts[!notstarts %in% unique(if (leftEdge) windows$Open else windows$Close)]
        x[notstarts] <- NA
      }
      
    } else {
      Map(\(i, v) x[i] <<- v[1:min(length(i), length(v))], indices, result_windowed)
    }
    
    
  
    x 
  } else {
    result_windowed
  }
}

#' @rdname context
#' @export
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


windowsSum <- function(x, windowFrame, na.rm = FALSE, cuttoff = 10) {
  
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

