



# Finding windows ----





#' Create arbitrary "windows" across vectors.
#' @export
#' @name humWindows
windows <- function(x, open, close = ~Next(open) - 1L, start = 1, end = length(x), nest = FALSE, depth = NULL, boundaries = NULL) {
  
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
      .stop("Sorry, in the windows function, if use open or close arguments that refer to each other,",
            "the resulting dependent must be the same length as the thing it depends on.",
            "In this case, {{ {openexpr} }} evaluates with length {length(open)}, while {{ {closeexpr} }}",
            "evaluates with length {length(close)}.",
            "It's best to stick with the basic usages of windows described in the documentation (?windows).",
            "More sophisticated things are possible, but you'll need to do a little more 'manual' wrangling.")
    } 
    close <- close[close >= min(open)]
    close <- close[1:length(open)]
    # close <- close %after% open
    
  }

  
  output <- data.table(Open = open, Close = close)
  output <- output[Reduce('&', lapply(output, Negate(is.na)))]
  output <- depth(output, nest = nest, depth = depth)
  
  
  if (!is.null(boundaries)) {
    if (any(lengths(boundaries) != length(x))) .stop("In a call to windows, all vectors in the list boundaries must be", 
                                                     "the same length as x.")
    output <- removeCrossing(output, boundaries)
  }
  
  attr(output, 'vector') <- x
  output
  
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
  
  maxdepths <- tapply_inplace(contour, segments(contour ==0),\(x) rep(max(x), length(x)))[steps == 1]
  ind$RevDepth <- ind$Depth - maxdepths - 1L
  
  if (!is.null(depth)) {
    ind <- ind[Depth %in% depth | RevDepth %in% depth]
  }
  
  ind 
}

removeCrossing <- function(x, boundaries) {
  
  boundaries <- sort(unlist(lapply(boundaries, \(b) which(b != lag(b)))))
  
  bad <- outer(boundaries, x$Open, '>') & outer(boundaries, x$Close, '<=')
  remove <- colSums(bad) > 0L
  
  x <- lapply(x, '[', !remove)
  
  x
  
}

### Window finding rules ----

grepi_multi <- function(x, pattern) {
  if (pattern %in% c('(', ')', '[', ']')) pattern <- paste0('\\', pattern)
  
  ns <- x %grepn% pattern
  rep(x %grepi% pattern, ns[ns > 0L])
}


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




# Applying functions across windows ----

#' @rdname humWindows
#' @export
windowApply <- function(x, func = c, windows, ..., reference = x, rebuild = TRUE, leftEdge = TRUE) {
  if (length(x) != length(reference)) .stop('In a call to windowApply, x and reference must be the same length!')
  
  if (missing(windows)) windows <- windows(reference, ...)
  
  indices <- Map(':', windows$Open, windows$Close)
  
  x_windowed <- lapply(indices, \(i) x[i])
  
  result_windowed <- lapply(x_windowed, func)
  
  if (rebuild) {
    
    if (all(lengths(result_windowed) == 1L)) {
      result <- unlist(result_windowed)
      x[if (leftEdge) windows$Open else windows$Close ] <- result
      notstarts <- seq_along(x)
      notstarts <- notstarts[!notstarts %in% unique(if (leftEdge) windows$Open else windows$Close)]
      x[notstarts] <- NA
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




