#################################-
# Finding context ############----
#################################-


## Parsing context expressions ----




parseContextExpression <- function(expr, other) {
  if (is.null(expr)) expr <- quote('.')
  
  expr <- withinExpression(expr, applyTo = 'call', stopOnHit = TRUE,
                         \(Head) Head == 'hop',
                         \(exprA) {
                           
                           if (!'along.with' %in% .names(exprA$Args)) {
                             exprA$Args$along.with <- quote(.)
                           }
                           exprA
                           
                         })
  
  expr <- substituteName(expr, list('next' = rlang::expr(lead(!!other, 1L)),
                                    prev   = rlang::expr(lag(!!other, 1L)),
                                    last   = rlang::expr(lag(!!other, 1L))))
  
  
  
  
  exprA <- analyzeExpr(expr)
  
  regexes <- c()
  if(exprA$Type == 'atomic' && exprA$Class == 'character') {
    regexes <- c(regexes, exprA$Args[[1]])
    expr <- rlang::expr(grepi_multi(., !!exprA$Args[[1]]))
  } else {
    if (exprA$Head == '|') {
      exprA$Head <- 'c'
      expr <- unanalyzeExpr(exprA)
    }
  }
  
  attr(expr, 'regexes') <- regexes
  expr
}


## Actual window finding ----

#' Create arbitrary "context" across vectors.
#' @export
context <- function(x, open, close, reference = x, ..., 
                    stripregex = TRUE,
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
  if (stripregex) for (re in escapebraces(regexes)) x <- gsub(re, '', x)
  
  expr <- if (collapse) rlang::expr(paste(.x., collapse = !!sep)) else rlang::expr(.x.)
  
  .applyWindows(data.table(.x. = x), windowFrame, expr, activeField = '.x.',
                inPlace = inPlace, alignToOpen = alignToOpen)
  
  
  
  
}




findWindows <- function(x, open, close = quote(next - 1), ..., 
                        activeField = 'Token', 
                        overlap = 'paired', rightward = TRUE, depth = NULL, groupby = NULL,
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
    val <- rlang::eval_tidy(list(open, close)[[i]], 
                            data = c(x, list(open = open_indices, close = close_indices)))
    if (is.logical(val)) val <- which(val)
    
    assign(c('open_indices', 'close_indices')[i], val)
  }
  
  
  # 
  windowFrame <- align(open_indices, close_indices,
                       overlap = overlap, rightward = rightward, depth = depth, groupby = groupby,
                       min_length = min_length, max_length = max_length)
                       
  windowFrame <- windowFrame[Open >= 1L & Open <= nrow(x) & Close >= 1L & Close <= nrow(x)]
  

  
  windowFrame <- windowFrame[Reduce('&', lapply(windowFrame, Negate(is.na)))]
  
  attr(windowFrame, 'regexes') <- regexes
  attr(windowFrame, 'vector') <- x
  windowFrame
  
}


print.windows <- function(x) {
  plot.new()
  vec <- attr(x, 'vector')[[1]]
  
  plot.window(ylim = c(0, max(x$Depth)),xlim = c(0, length(vec)))
  
  text(1:length(vec), rep(0, length(vec)), vec)
  y <- x$Depth
  y <- as.numeric(make.unique(as.character(y)))
  graphics::arrows(x$Open, x$Close, y0 = y, y1= y, angle = 90, code = 3)
  
  print(x)
}

### Window finding rules ----


# 
align <- function(open, close, groupby = list(),
                  overlap = 'paired', rightward = TRUE, depth = NULL, 
                  min_length = 2L, max_length = Inf) {
  # output <- if (length(open) == length(close) && all(ordPredicate(open, close), na.rm = TRUE)) {
  # data.table(Open = open, Close = close)
  # } 
  
  open <- open[!is.na(open)]
  close <- close[!is.na(close)] 
  
  if (!rightward) {
    openx <- open
    open <- sort(-close)
    close <- sort(-openx)
  }
  
  dists <- outer(close, open, '-') 
  
  if (length(groupby)) {
    groupby <- do.call('paste', groupby)
    dists[outer(groupby[close], groupby[open], '!=')] <- NA
  }
  
  accept <- dists >= min_length & dists <= max_length
  

  
  overlap <- pmatch(overlap, c('nested', 'paired', 'edge', 'none'))
  accept <- switch(overlap,
                   nested = {
                     steps <- c(rep(1, length(open)), rep(-1, length(close)))[order(c(open, close))]
                     stepdepth <- sigma(steps)
                     samelevel <- outer(stepdepth[steps == -1L] + 1L, stepdepth[steps == 1L], '==')
                     topmost(accept & samelevel)
                   },
                   paired =   shunt(topmost(accept)), 
                   edge = topmost(accept),
                   none = leftmost(topmost(accept))
                   )
 
  
  # Prepare windowFrame
  accept <- which(accept, arr.ind = TRUE)
  
  windowFrame <- data.table(Open = open[accept[ , 'col']],
                            Close = close[accept[ , 'row']])
  
  
  # update depth
  windowFrame <- depth(windowFrame, depth = depth)

  
  if (!rightward) windowFrame[ , c('Open', 'Close') := list(-Close, -Open)]
  
  windowFrame[ , Length := Close - Open + 1L]
  
  setorder(windowFrame, Open, Close)
  
  windowFrame
  
}


### Sorting, filtering, or modifying windows ----

shunt <- function(accept) {
  # "shunt" looks for window Close positions that have aleady been matched
  # with a previous Open, and "shunts" them to the next Close.
  
  bad <- rowSums(accept) > 1L
  
  while (any(bad)) {
    ind <- which(accept, arr.ind = TRUE)
    ind <- ind[order(ind[ , 'row']), , drop = FALSE]
    ind <- ind[ind[ , 'row'] %in% which(bad), ]
    
    accept[ind] <- FALSE
    
    move <- unlist(lapply(rle(ind[ , 'row'])$lengths,
                                           seq_len)) - 1L
    ind[ , 'row'] <- ind[ , 'row'] + move
    
    ind <- ind[ind[ , 'row'] >= 1L & ind[ , 'row'] <= nrow(accept), , drop = FALSE]
    accept[ind] <- TRUE
    
    bad <- rowSums(accept) > 1L
  }
  
  accept
  
}



depth <- function(windowFrame, depth = NULL, ...) {
  windowFrame[ , Depth := {
    steps <- c(rep(1, length(Open)), rep(-1, length(Close)))[order(c(Open, Close))]
    stepdepth <- sigma(steps)
    stepdepth[steps == 1L]
  }]
  
  maxdepth <- max(windowFrame$Depth)
  windowFrame$RevDepth <- windowFrame$Depth - maxdepth - 1L
  

  if (!is.null(depth))   windowFrame <- windowFrame[Depth %in% depth | RevDepth %in% depth]
  
  windowFrame
}
# 
# removeCrossing <- function(windowFrame, groupby) {
#   groupby <- do.call('paste', groupby)
#   windowFrame[groupby[Open] == groupby[Close]]
# }

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


windows2groups <- function(dt, windowFrame) {
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- dt[unlist(indices)]
  dt_extended[ , contextWindow := rep(seq_along(indices), lengths(indices))]
  
  dt_extended
}

.applyWindows <- function(dt, windowFrame, expr, activeField = 'Token', ..., 
                          inPlace = TRUE, alignToOpen = TRUE) {
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- windows2groups(dt, windowFrame)
  
  results <- rlang::eval_tidy(rlang::quo({ dt_extended[ , list(list(!!expr)), by = contextWindow]}))$V1
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


#' Generate regular sequence "along" input
#' 
#' `hop()` is similar to base R's [seq()], but with some additional features, including
#' special sugar when used with humdrumR's [context()] command.
#' `hop()` is used to create customizable sequences of indices for a vector;
#' for example, if you want to index every third value from a vector.
#' This is useful for, as when used with [context()], defining the start points of "rolling-window"
#' analyses along a vector; the "hop size" is the gap between the start of each window, 
#' defined by `hop()`'s `by` argument.
#' 
#' @details 
#' 
#' `hop()` has similar arguments to [base::seq()], but focused on the `along.with` argument,
#' a vector which you'd like to generate indices for.
#' If you simply call `hop(myvector)`, the output will be the same as `1:length(myvector)`.
#' The `by` argument can be used to specify a different "hop" pattern: `by = 2` will get you *every other*
#' index, `1`, `3`, `5`, `7`, etc.
#' Unlike [base::seq()], `hop()`'s `by` argument can be a vector of numbers, allowing you to specify a pattern of hops.
#' For example, `by = c(2, 3)` will first hop `2`, then hop `3`, then repeat---so the output would be `1`, `3`, `6`, `8`, `11`, `13`, etc.
#' 
#' The by pattern can be comprised of negative numbers, or a mix of negative and positive numbers.
#' If `by` mixes negative and positive numbers, the pattern can hop up and down, as it climbs.
#' For example, you could go up two, then down one, then repeat using `by = c(2,-1)`.
#' If the pattern is *overall* (sums) negative, the `from` argument must be greater than the `to` argument (see next section);
#' if the pattern sums to zero, an error occurs because the pattern would never end!
#' If a pattern changes directions, it is possible for pattern to hop outside the bounds of the vector;
#' if this happens, the outside indices return `NA`.
#' 
#'
#' @returns
#' 
#' By default, `hop()` returns an `integer` vector, appropriates indices for the `along.with` vector.
#' However, there are two other options:
#' 
#' + If `logical = TRUE`, the indices are returned as a logical vector, the same length as `along.with`,
#'   with `TRUE` values indicating indices. Note that any ordering in the output (due to a mix of positive and negative
#'   values in the `by` argument) is lost.
#' + If `value = TRUE`, the actual indixed elements of the `along.with` vector are returned:
#'   Thus, `hop(myvector, ..., value = TRUE)` is simply the same as `myvector[hop(myvector, ...)]`.
#'   If `value = TRUE`, the `logical` argument is ignored.
#' 
#' 
#' @section Starting and Ending:
#' 
#' By default, `hop()` builds indices from `1` to the end of the `along.with` vector.
#' The `from` and `to` arguments can be used to control this.
#' Either argument can simply be a natural number, indicating where to start and end the output sequences.
#' (If `to` is `NULL`, it is set to `length(along.with)`.)
#' An alternate approach is to provide either argument a single `character` string, which is treated as a regular 
#' expression and matched against `along.with`, or  a `logical` vector the same length as `along.with`.
#' The first match/`TRUE` is used for the `from` index and the last match/`TRUE` for the `to` index.
#' This means you can say things like `from = Record == 33` in a [within()][withinHumdrum] call.
#' 
#' If the `by` argument is *overall* (sums) positive, `from` must be less than `to`.
#' If the `by` argument is *overall* (sums) negative, `from` must be greater than `to`.
#' 
#' If the `by` pattern doesn't ever actually the actual `to` index---perhaps jumping over it---
#' the output stops when it would *pass* the `to`.
#' 
#' @section Grouping:
#' 
#' In many cases we want to how along vectors, but *not across certain boundaries*.
#' For example, if we want all even numbered indices, we can set `by = 2` and `from = 2`.
#' However, if your vector includes data from multiple pieces, and some of the pieces have an odd number of data points,
#' our "even" sequence would end up hitting odd numbers in some pieces.
#' To get around this, the `groupby` argument indicates one, or more, grouping vectors, which break the `x` (input) argument
#' into groups.
#' If more than `groupby` vectors are given, a change in *any* vector indicates a boundary.
#' Each grouped segement of `along.with` is treated just like a separate call to `hop()`;
#' for example, if `from = 2`, the hop sequence will start on the second index of *each* group.
#' However, the output indices still represent the original `along.with` indices.
#' 
#' Since `hop()` is usually used with [context()] to create rolling windows within musical parts,
#' we want typically want to apply `hop()` using using `groupby = list(File, Spine, Path)`.
#' In fact, `humdrumR` [with(in)][withinHumdrum] calls will *automatically* feed these 
#' three fields as `groupby` arguments to `hop()`.
#' So any use of `hop()` in a call to [with(in)][withinHumdrum], will automatically generate the hop sequence
#' in a "melodic" way, within each spine path of each file.
#'
#' @param along.with ***The vector you want indices to "hop" along.***
#' 
#' Must be a vector (either atomic, or a `list()`).
#' 
#' @param by ***The pattern of "hops" to to use.***
#' 
#' Defaults to `1`: returning all indices `from:to`.
#' 
#' Must be one or more whole numbers.
#' 
#' `sum(by)` must non-zero.
#'
#' @param from ***Where to start the sequence.***
#' 
#' Defaults to `1`: starting from the first index.
#'
#' Must be either a single natural number, a single `character` string, or 
#' a `logical` vector the same length as `along.with`.
#' 
#' A `character`-string input is treated as a regular expression,
#' which is matched against `along.with` using [grepl()] to generate a `logical` vector.
#' The index of the first `TRUE` is used.
#' 
#' @param to ***Where to end the sequence.***
#' 
#' Defaults to `NULL`.
#'
#' Must be either `NULL`, a single natural number, a single `character` string, or 
#' a `logical` vector the same length as `along.with`.
#' 
#' If `NULL`, `to` is set to the last index of `along.with` (or of each group in `groupby`).
#' A `character`-string input is treated as a regular expression,
#' which is matched against `along.with` using [grepl()] to generate a `logical` vector.
#' The index of the last `TRUE` is used.
#' 
#' @param value ***Should actual values from `along.with` be returned?***
#'
#' Defaults to `FALSE`.  
#'
#' Must be a singleton `logical` value; an on/off switch.
#' 
#' @param value ***Should indices be returned as logical `TRUE`s?***
#'
#' Defaults to `FALSE`.  
#'
#' Must be a singleton `logical` value; an on/off switch.
#'
#' @param groupby ***Optional vectors to group hop sequences within.***
#' 
#' Defaults to empty `list()`.
#'
#' Must be a [list()], which is either empty or contains vectors which are all the same length as `along.with`.
#' In calls to [with/within.humdrumR][withinHumdrum], `groupby` is passed `list(File, Spine, Path)` by default.
#'
#' @examples 
#' 
#' # use the built-in 'letters' vector
#' 
#' hop(letters)
#' 
#' hop(letters, by = 3)
#' 
#' hop(letters, by = 2, from = 4)
#' 
#' hop(letters, by = 2, from = 'e', to = 'x'))
#'
#' hop(letters, by = c(-1, 2), from = 'e', to = 'w', value = TRUE)
#' 
#' hop(letters, by = -1, from = 'z', to = 3)
#' 
#' @export
hop <- function(along.with, by = 1, from = 1L, to = NULL, value = FALSE, logical = FALSE, groupby = list()) {
  if (length(along.with) == 0L) return(integer())
  
  checks(along.with, xvector)
  checks(by, xwholenum & xminlength(1))
  checks(from, ((xwholenum | xcharacter) & xlen1) | (xlogical & xmatch(along.with)))
  checks(to, xnull | ((xwholenum | xcharacter) & xlen1) | (xlogical & xmatch(along.with)))
  checks(value, xTF)
  checks(logical, xTF)
  

  along <- if (length(groupby)) tapply(along.with, groupby, c, simplify = FALSE) else list(along.with)
  
  dt <- data.table(i = seq_along(along.with),
                   Group = if (length(groupby)) squashGroupby(groupby) else 1)
  dt[, igrouped := seq_along(i), by = Group]
  dt[ , Max := rep(max(i), length(i)), by = Group]
  
  # from
  starts <- if (is.numeric(from)) {
    dt[igrouped == from]
  } else {
    if (is.character(from)) from <- grepl(from, along.with)
    dt[from, .SD[1], by = Group]
  }
  
  # to
  if (is.null(to)) to <- max(dt$igrouped)
  ends <- if (is.numeric(to)) {
    dt[igrouped <= to, .SD[.N], by = Group]
  } else {
    if (is.character(to)) to <- grepl(to, along.with)
    dt[to, .SD[.N], by = Group]
  }
  ranges <- starts[ends, on = 'Group']
  ranges[ , Length := i.i - i]
  ranges[ , Length := Length + sign(Length)]
  
  
  ranges <- ranges[!is.na(i) & !is.na(i.i)]
  
  # actual sequences!
  interval <- sum(by)
  if (interval < 0) {
    if (any(ranges$Length > 0)) .stop("In your call to hop(), your 'by' pattern is negative overall, but your `from` argument is less than your `to` argument.",
                                      "There is no way to hop down from a lower number to a higher number.")
    ranges <- ranges[nrow(ranges):1]
  } else {
    if (any(ranges$Length < 0)) .stop("In your call to hop(), your 'by' pattern is positive overall, but your `from` argument is greater than your `to` argument.",
                                      "There is no way to hop up from higher number to a lower number.")
  }
  
  if (interval == 0L) .stop("In call to hop(), the by argument cannot sum to zero")
  
  fullpattern <- rep_len(by, (ceiling(max(abs(ranges$Length)) / abs(interval))) * length(by))
  fullpattern <- cumsum(c(0L, fullpattern))
  
  
  result <- ranges[ , {
    pat <- fullpattern + i
    endon <- pat == i.i
    if (any(endon)) {
      pat <- pat[1:which(endon)[1]]
    } else {
      past <- if (interval > 0L) pat > i.i else pat < i.i
      if (any(past)) pat <- pat[1:(which(past)[1] - 1L)]
    }
    pat[ pat <= (i - igrouped) | pat > Max] <- NA
    pat
  }, by = seq_len(nrow(ranges))]$V1
  
  if (value) {
    result <- along.with[result]
  } else { 
    if (logical) result <- seq_along(along.with) %in% result
    
  }
  
  result
  
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

