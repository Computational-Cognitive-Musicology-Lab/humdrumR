



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
    if (any(bad)) .stop("In your calls to windows, your formulae arguments are mutually referential in a circular manner.")
    
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
  if(length(anchor) != length(x)) .stop("If the {name} argument in a call to windows is logical", 
                                        "it must be the same length as the vector you are windowing across.")
  
  which(anchor)
}
parseAnchor.numeric <- function(anchor, x, name) {

    if (any(anchor != round(anchor))) .stop("In a call to windows, a numeric {name} argument needs to be an integer value.")
    anchor <- as.integer(anchor)
    
    if (any(anchor < 0) && any(anchor > 0)) .stop("In a call to windows, you can't mix negative and positive numbers in a {name} argument.")
    if (any(length(abs(anchor)) > length(x))) .stop("In a call to windows, the {name} argument can't be numeric values greater than the length",
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
  pattern[pattern %in% c('(', ')', '[', ']')] <- paste0('\\', pattern[pattern %in% c('(', ')', '[', ']')])
  
  ns <- x %~n% pattern %|% 0L
  rep(x %~i% pattern, ns[ns > 0L])
  
}

## Actual window finding ----

#' Create arbitrary "windows" across vectors.
#' @export
#' @name humWindows
windows <- function(x, open, close = ~Next(open) - 1L, start = 1, end = length(x), 
                    nest = FALSE, depth = NULL, groupby = NULL,
                    min_length = 1L, max_length = Inf) {
  
  
  list2env(parseAnchors(x, open = open, close = close, start = start, end = end), envir = environment())
  
  
  windowFrame <- align(open, close)

  windowFrame <- windowFrame[Open <= end & Close <= end & Open >= start & Close >= start]

  windowFrame <- windowFrame[Reduce('&', lapply(windowFrame, Negate(is.na)))]
  windowFrame[ , Length := Close - Open]
  windowFrame <- windowFrame[Length >= min_length & Length <= max_length]
  
  windowFrame <- depth(windowFrame, nest = nest, depth = depth)
  
  if (length(groupby)) windowFrame <- removeCrossing(windowFrame, checkWindows(x, groupby))
  
  
  attr(windowFrame, 'vector') <- x
  # windowFrame[ , list(Open, Close)]
  windowFrame
  
}

#' @export
#' @rdname humWindows
nested <- function(x, open, close, depth = 1L) {
  windows(x, open, close, depth, nest = TRUE)
}

print.windows <- function(x) {
  plot.new()
  vec <- attr(x, 'vector')
  
  plot.window(ylim = c(0, max(x$Depth)),xlim = c(0, length(vec)))
  
  text(1:length(vec), rep(0, length(vec)), vec)
  y <- x$Depth
  graphics::arrows(x$Open, x$Close, y0 = y, y1= y, angle = 90, code = 3)
  
 
}

### Sorting, filtering, or modifying windows ----

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
  

  maxdepths <- tapply_inplace(contour, segments(contour == 0),\(x) rep(max(x), length(x)))[steps == 1]
  ind$RevDepth <- ind$Depth - maxdepths - 1L
  
  if (!is.null(depth)) {
    ind <- ind[Depth %in% depth | RevDepth %in% depth]
  }
  
  ind[ , OpenDepth := Depth - min(Depth), by = Open]
  ind[ , CloseDepth := Depth - min(Depth), by = Open]
  ind[ , OpenRevDepth := RevDepth - max(RevDepth), by = Open]
  ind[ , CloseRevDepth := RevDepth - max(RevDepth), by = Close]
  
  ind 
}

removeCrossing <- function(windowFrame, groupby) {
  groupby <- do.call('paste', groupby)
  windowFrame[groupby[Open] == groupby[Close]]
}

### Window finding rules ----



# Tools ----


#' @export
#' @name humWindows
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

align <- function(open, close, nearest = NULL, duplicateOpen = FALSE, duplicateClose = FALSE) {
  
  output <- if (length(open) == length(close) && is.null(nearest) && all(open <= close, na.rm = TRUE)) {
    data.table(Open = open, Close = close) 
  } else {
    if (is.null(nearest)) nearest <- 0L
    
    close <- close[!is.na(close)]
    open <- open[!is.na(open)]
    i <- findInterval(close, open)
    
    dts <- lapply(nearest,
                  \(o) {
                    i <- i - o
                    i[i <= 0L] <- NA_integer_
                    data.table(Open = open[i], Close = ifelse(is.na(i), NA_integer_, close))
                  })
    
    rbindlist(dts)
  }
  
  output <- output[!Reduce('|', lapply(output, is.na))]
  
  setorder(output, Open, Close)
  
  dups <- output[ , list(Open = !duplicateOpen & duplicated(Open),
                         Close = !duplicateClose & duplicated(Close))]
    
  output <- output[!Reduce('|', dups)]
  # 
  output
  
}



# Applying functions across windows ----

#' @rdname humWindows
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

#' @rdname humWindows
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

