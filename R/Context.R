#################################-
# Finding context ############----
#################################-


## Parsing context expressions ----




parseContextExpression <- function(expr, other, groupby) {
  if (is.null(expr)) expr <- quote('.')
  
  expr <- withinExpression(expr, applyTo = 'call', stopOnHit = TRUE,
                         \(Head) Head == 'hop',
                         \(exprA) {
                           
                           exprA$Args$along.with <- exprA$Args$along.with %||% quote(.)
                           exprA$Args$groupby <- exprA$Args$groupby %||% groupby
                           exprA
                           
                         })
  
  expr <- substituteName(expr, list(nextopen = rlang::expr(lead(!!other, 1L)),
                                    end  = rlang::expr(length(.)),
                                    prevclose = rlang::expr(lag(!!other, 1L)),
                                    lastclose = rlang::expr(lag(!!other, 1L))))
  
  
  
  
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
#' 
#' The `context()` command can be used to group input data (vectors)
#' into arbitrary contextual windows.
#' Unlike the contextual-grouping you can achieve with `groupby` arguments
#' to various functions (or `by` expressions in [with(in).humdrumR)][withHumdrum]),
#' `context()` can produce windows that *overlap* or, the opposite case, that don't exhaustively
#' divide the data.
#' The `context()` should generally be used as a special argument to
#' [with(in).humdrumR)][withHumdrum], but it can also be called directly itself.
#'  
#' @details 
#' 
#' `context()` takes an input vector (`x`) and divides it into windows based on
#' the content of the `reference` vector, which must be the same length as `x`---
#' by default, `x` is reused as `reference`, so windows are based on the input `x` itself.
#' As a more complex option, `reference` can be a named `list()` or `data.frame` (`nrow == length(x)`)---
#' the (named) elements of `reference` are then visible to the `open` and `close` arguments (see below).
#'
#' The `collapse`, `inPlace`, and `complement` arguments how windows are output.
#' 
#' + The "complement" refers to elements of the input vector that don't fall inside
#'   any indicated windows: if `complement = FALSE` (the default), these "outside" values are
#'   dropped; if `complement = TRUE`, they are retained.
#' + If `inPlace = TRUE`, windows are output in a vector of the same length as the input,
#'   padded with `NA` as needed---otherwise (the default), only the windows are returned
#' + If `collapse = TRUE`, the windows are collapsed to strings (separed by `sep`), otherwise,
#'   a `list()` of windows is returned.
#'
#' 
#' @section Defining windows:
#' 
#' The system `context()` uses to define/identify windows in the data is quite sophisticated,
#' and can take some time to master!
#' The basic idea is that you must indicate where you want windows to start ("*open*") and 
#' where you want them to end ("*close*"):
#' you indicate this using the `open` and `close` arguments.
#' These arguments simple indicate indices in the input vector;
#' For example, if we want a window to open at the 4th and 11th indices, 
#' and close at the 15th and 24th index,
#' we can write (using the built-in `letters` vector for practice):
#' 
#' ```
#' context(letters, open = c(4, 11), close = c(15, 24))
#'
#' ```
#' 
#' This is quite trivial.
#' However, the `open` and `close` arguments can actually be arbitary expressions
#' which do a number of special tricks, including refering to each other.
#' For example, if either argument includes a call to [hop()],
#' `hop()` will automatically be applied along the input vector.
#' Consider this example:
#' 
#' ```
#' context(letters, open = hop(2), close = open + 3)
#' ```
#' 
#' In this example, the `hop()` command generates `open` indices for every odd
#' number from `1` to `25`.
#' The `close` argument then references these `open` indices, and adds `3` to each---
#' the result is the pairs like `1:4`, `2:5`, `3:6`, `4:7`, etc.
#' If we give `hop()` different arguments (like `by` or `from`), we can modify this process.
#' In fact, if we use the default `by` value for `hop()` (`1`), we can use this approach to
#' create standard N-grams.
#'
#'
#' The minimum and maximum length of windows can be controlled using the `min_length` and `max_length` arguments.
#' We can also indicate open/closes by providing `logical` vectors (the same length as `x`).
#'
#' As mentioned above, if `reference` is a named `list()` or `data.frame()`, `open` and 
#' `close` and refer to the elements of `reference`.
#' This is useful if you want to open/close windows in reference to multiple vectors:
#'
#' ```
#' context(letters, 
#'         reference = data.frame(Threes = rep(1:3, length.out = 26), 
#'                                Fours = rep(4:1, length.out = 26)),
#'         open = Threes == Fours, close = Fours == 1)
#' ```
#' 
#' ### Regular Expressions
#' 
#' If either `open` or `close` are provided a `character` string, this string is treated
#' as a regular expression and is matched against the `reference` vector.
#' For exaple, we could make windows in the alphabet starting or ending on each vowel:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = open + 4)
#' context(letters, open = close - 4, close = '[aeiou]', alignToOpen = FALSE)
#' 
#' ```
#' 
#' (Notice that we can use the `alignToOpen` argument to change how the output is aligned.)
#' 
#' If the `stripRegex = TRUE` (not the default), the matching `open` or `close` regular expressions are removed
#' from the output.
#' This can be useful if the character/tokens used to indicate windows are no longer needed
#' once windowing is done.
#' 
#' ### Special References
#' 
#' The `open` and `close` arguments have a few more special behaviors.
#' What if we'd like each of our windows to close right before the next window opens?
#' We can do this by making the `close` argument to refer to the *next* `open`, by
#' referring to `nextopen` object:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L)
#' ```
#' 
#' Conversely, `open` can refer to the `prevclose` close:
#' 
#' ```
#' context(letters, open = prevclose + 1, close = '[aeiou]', alignToOpen = FALSE)
#' ```
#' 
#' Notice that when we called `context(letters, open = '[aeiou]', close = nextopen - 1L)`,
#' the window opening on `"u"` is not returned.
#' This is because there is no "`nextopen`" open to close on.
#' We can instead provide an `context()` alternative, using `|` (or):
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L | 26)
#' 
#' ```
#' 
#' What if we don't know exactly how long our input vector is?
#' Refer to the `end` object:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L | end)
#' ```
#' 
#' @section Nested Windows:
#' 
#' A common use case for `context()` is analyzing phrases indicated in music.
#' In `**kern`, phrases are indicated with opening (`(`) and close (`)`) parentheses,
#' which we can capture with regular expressions for `open` and `close`.
#' Here is an example:
#' 
#' ```
#' nesting1 <- c('(a', 'b)', '(c', 'd', 'e)', '(d', 'e', 'f)', '(e', 'f', 'f#', 'g', 'g#', 'a)')
#' 
#' context(nesting1, open = '(', close = ')')
#' ```
#' 
#' Perfect.
#' However, what if there are nested phrasing indicators?
#' 
#' ```
#' nesting2 <- c('(a', 'b)', '(c', '(d', 'e)',  '(d', 'e)', 'f)', '(e', '(f', '(f#', 'g)', 'g#)', 'a)')
#' 
#' context(nesting2, open = '(', close = ')')
#' ```
#' 
#' That's not what we want!
#' By default, `context()` "pairs" each `open` with the next `close`, which often makes the most sense.
#' But in this case, we want different behavior.
#' We can get what we want by specifying `overlap = 'nested'`:
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested')
#' ```
#' 
#' Now context aligns each `open` with the corresponding `close` at the same *nesting level*.
#' What if we are only interested in the highest (or lowest) level of nesting?
#' Use the `depth` argument, which can be non-zero integers: the highest level is `1`,
#' with "deeper" levels incrementing up.
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 1)
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2)
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2:3)
#' ```
#' 
#' You can also use negative `depth` to specify from the deepest levels outward.
#' For example, in this case  `depth == -1` should get us that deepest level:
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = -1)
#' ```
#' 
#' I `depth` is `NULL` (the default), all depths are returned.
#' 
#' 
#' 
#' 
#' @section Controlling Overlap:
#'
#' There are some other options for controlling how windows can, or cannot, overlap.
#' Perhaps we'd like to look at every melodic phrase moving from so (dominant) to do (tonic).
#' 
#' ```
#' melody <- c('so', 'la', 'ti', 'do', 'so', 'fi', 'so', 'la', 'ti', 're', 'do', 'so', 'la', 're', 'do')
#' 
#' context(melody, open = 'so', close = 'do')
#' 
#' ```
#' 
#' This output is probably not what we want.
#' Again, `context()` (by default) pairs each opening with the next close *which hasn't already been paird*.
#' In this case, that means the third so is getting pairs with the third do, 
#' even though there is another do in between!
#' We might want to try either the `"edge"` or `"none"` options for the `overlap` argument:
#' 
#' ````
#' context(melody, open = 'so', close = 'do', overlap = 'edge')
#' context(melody, open = 'so', close = 'do', overlap = 'none')
#' ```
#' 
#' The `"edge"` option allows the closing edge of windows to share a `close`---in this case,
#' the second and third so (`open`) are paired with the same do.
#' On the other hand, with `overlap = "none"`, overlapping windows are simply not allowed, so the third `open` 
#' simply doesn't get paired with anything.
#' 
#' What if you would like to pair windows on their left (opening) edge?
#' If you specify `rightward = FALSE`, the overlap argument works backwards (right-to-left) through
#' the input vector, starting on each `close` and ending on each `open`.
#' By combining `righward = FALSE` with various `overlap` options, you can achieve a lot of windowing 
#' options you might need.
#' 
#' @section Repeated Indices:
#' 
#' Note that if `duplicates_indices = TRUE` (the default) the `open` and `close` arguments can 
#' incorporate repeated indices, including multiple matches to a regular expression in the same index.
#' This is useful with, for example, nested phrases:
#' 
#' ```
#' nesting3 <- c('(a', 'b)', '((c', 'd', 'e)',  '(d', 'e', 'f))', '(e', 'f', '((f#', 'g)', 'g#)', 'a)')
#' 
#' context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 1)
#' context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 2)
#' ````
#'
#' In some cases, you might want to turn `duplicate_indices = FALSE`. 
#' 
#' @param x ***Input data to group into windows.***
#' 
#' Must be an atomic vector.
#' 
#' @param open ***Where to "open" (start) windows.***
#' 
#' Can be natural numbers, `logical` vectors (of the same length as `x`),
#' a single `character` string (interpreted as a regular expression).
#' May also be an arbitrary expression which returns natural numbers;
#' the expression can refer to named elements of `reference`, to `end` (last index),
#' to `close`, or to `prevclose` (the previous close).
#' 
#' @param close ***Where to "close" (end) windows.***
#' 
#' Can be natural numbers, `logical` vectors (of the same length as `x`),
#' a single `character` string (interpreted as a regular expression).
#' May also be an arbitrary expression which returns natural numbers;
#' the expression can refer to named elements of `reference`, to `end` (previous index),
#' to `open`, or to `nextopen` (the next open).
#' 
#' @param reference ***Vector(s) to use to identify window open/closes.***
#' 
#' Defaults to `x`.
#' 
#' Must be either an atomic vector of the same length as `x`, or a `list()`/`data.frame`
#' of such vectors, all [named][names()].
#'  
#' @param overlap ***How are overlapping windows treated/created?***
#' 
#' Defaults to `'paired'`.
#' 
#' Must be a single `character`, [partially matching][partialMatching]
#' either `"paired"`, `"nested"`, `"edge"`, or `"none"`.
#' 
#' @param depth ***How "deep" can windows overlap?***
#'
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or a vector of non-zero whole numbers.
#' 
#' @param rightward ***Should window alignment/overlap be determined from left to right?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param duplicate_indices ***Can the same index open/close multiple windows?***
#'
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param min_length,max_length ***The minimum/maximum lengths of output windows.***
#' 
#' Default to two and infinity (no maximum) respectively.
#' 
#' Must be single, positive whole numbers.
#'
#' @param inPlace ***Should output be padded to same length as input?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param complement ***Should input "outside" any windows, be output?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param alignToOpen ***Should '`inPlace`' output be aligned to the open of each window?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param collapse ***Should output windows be collapsed to single `character` strings?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param sep ***Separator for collapsed output.***
#' 
#' Defaults to a comma (`","`).
#' 
#' Must be a single `character` string.
#' 
#' @param stripRegex ***Should regular expressions matched by the `open`/`close` arguments be removed from the output?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param groupby ***Optional vectors to group windows within.***
#' 
#' Defaults to empty `list()`.
#' 
#' Must be a [list()], which is either empty or contains vectors which are all the same length as `x`.
#' In calls to [with/within.humdrumR][withinHumdrum], `groupby` is passed `list(Piece, Spine, Path)` by default.
#' 
#' Windows cannot cross group boundaries.
#' 
#' @examples 
#' 
#' # use the built-in 'letters' vector
#'
#' context(letters, open = hop(4), close = open + 3)
#' 
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end)
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end, inPlace = TRUE)
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end, collapse = FALSE)
#' 
#' 
#' \dontrun{
#' # within.humdrumR
#' chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")
#' 
#' # 4-grams
#' within(chorales,
#'        paste(Token, collapse = '->'), 
#'        context(open = hop(), open + 3))
#'        
#' # phrases leading to fermatas
#' with(chorales, 
#'      paste(Token, collapse = ','), 
#'      context(open = 1 | prevclose + 1, close = ';', overlap = 'none'))
#' }
#' 
#' @export
context <- function(x, open, close, reference = x, 
                    overlap = 'paired', depth = NULL, rightward = TRUE, duplicate_indices = TRUE, 
                    min_length = 2L, max_length = Inf,
                    inPlace = FALSE, complement = FALSE, alignToOpen = TRUE, 
                    collapse = TRUE, sep = ',', stripRegex = FALSE,  
                    groupby = list()
                    ) {
  
  checks(x, xatomic)
  # checks(reference, xmatch(x) | xnrowmatch(x))
  checks(overlap, xplegal(c('paired', 'nested', 'edge', 'none')))
  checks(depth, xnull | (xwholenum & xnotzero))
  checks(rightward, xTF)
  checks(duplicate_indices, xTF)
  checks(min_length, xpnatural)
  checks(max_length, xpnatural)
  checks(inPlace, xTF)
  checks(complement, xTF)
  checks(alignToOpen, xTF)
  checks(collapse, xTF)
  checks(sep, xcharacter & xlen1)
  checks(stripRegex, xTF)
  
  open  <- rlang::enexpr(open)
  close <- rlang::enexpr(close)
  windowFrame <- findWindows(reference, open, close, 
                             duplicate_indices = duplicate_indices,
                             overlap = overlap, depth = depth, rightward = rightward,
                             min_length = min_length, max_length = max_length)
  
  regexes <- attr(windowFrame, 'regexes')
  if (stripRegex) for (re in escapebraces(regexes)) x <- gsub(re, '', x)
  
  expr <- if (collapse) rlang::expr(paste(.x., collapse = !!sep)) else rlang::expr(list(.x.))
  
  .applyWindows(data.table(.x. = x), windowFrame, expr, activeField = '.x.',
                inPlace = inPlace, complement = complement, alignToOpen = alignToOpen)
  
  
  
  
}




findWindows <- function(x, open, close = quote(nextopen - 1), ..., 
                        activeField = quote(Token),
                        overlap = 'paired', depth = NULL,  rightward = TRUE, duplicate_indices = TRUE,
                        groupby = list(),
                        min_length = 2L, max_length = Inf) {
  
  
  if (!is.data.frame(x)) x <- setNames(data.table::data.table(. = x), rlang::expr_text(activeField) %||% '.')
  
  open <- parseContextExpression(open, other = quote(close), groupby = rlang::enexpr(groupby))
  close <- parseContextExpression(close, other = quote(open), groupby = rlang::enexpr(groupby))
  regexes <- union(attr(open, 'regexes'), attr(close, 'regexes'))
  if (!is.null(activeField)) {
    open  <- substituteName(open, list('.' = activeField))
    close <- substituteName(close, list('.' = activeField))
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
  if (!duplicate_indices) {
    open_indices <- unique(open_indices)
    close_indices <- unique(close_indices)
  }


  # 
  windowFrame <- if (length(open_indices) && length(close_indices)) {
   align(open_indices, close_indices, fullLength = nrow(x),
                       overlap = overlap, rightward = rightward, depth = depth, groupby = groupby,
                       min_length = min_length, max_length = max_length)
  } else {
    data.table(Open = integer(0L), Close = integer(0L), Length = integer(0L), 
               Depth = integer(0L), RevDepth = integer(0L))
  }
  
  #                      
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
align <- function(open, close, fullLength, groupby = list(),
                  overlap = 'paired', rightward = TRUE, depth = NULL, 
                  min_length = 2L, max_length = Inf) {

  overlap <- pmatches(overlap, c('nested', 'paired', 'edge', 'none'))
  groupby <- squashGroupby(groupby)
  
  if (!rightward) {
    openx <- open
    open  <- (fullLength + 1L) - close
    close <- (fullLength + 1L) - openx
    groupby <- rev(groupby)
    
  }
  
  open <- sort(open)
  close <- sort(close)
  
  if (overlap == 'paired' && 
      (length(open) == length(close)) && 
      all((close - open + 1L) >= min_length & (close - open + 1L) <= max_length, na.rm = TRUE)) {
    
    windowFrame <- data.table(Open = open, Close = close) 
    windowFrame <- windowFrame[!is.na(Open) & !is.na(Close) & Open > 0L & Close > 0L]
    
    if (length(groupby)) {
      windowFrame <- windowFrame[groupby[Open] == groupby[Close]]
    }
    
  } else {
    open <- open[!is.na(open)]
    close <- close[!is.na(close)] 
    
    if (length(groupby)) {
      groupbyopen <- groupby[open]
      groupbyclose <- groupby[close]
    } else {
      groupbyopen <- integer(length(open))
      groupbyclose <- integer(length(close))
    }
    
    openframe  <- data.table(Open = open,   OpenInd = seq_along(open),   Group = groupbyopen)
    closeframe <- data.table(Close = close, CloseInd = seq_along(close), Group = groupbyclose)
    
    windowFrame <- openframe[closeframe, on = 'Group', allow.cartesian = TRUE]
    #
    windowFrame[ , Length := Close - Open + 1L]
    windowFrame <- windowFrame[Length >= min_length & Length <= max_length]
  
    
    if (overlap == 'nested') {
      
      windowFrame <- nest(windowFrame)
    } else {
      windowFrame[ , Hit := CloseInd == min(CloseInd), by = OpenInd]
      windowFrame <- windowFrame[Hit == TRUE]
      windowFrame <- switch(overlap,
                            paired = shunt(windowFrame, open, close),
                            edge = windowFrame,
                            none = { 
                              i <- 1
                              while(i <= nrow(windowFrame)) {
                                windowFrame <- windowFrame[Open > Close[i] | Close <= Close[i]]
                                i <- i + 1
                              }
                              windowFrame[ , Hit := OpenInd == min(OpenInd), by = CloseInd]
                              windowFrame[Hit == TRUE]
                              }
                            )
    }
    
    setorder(windowFrame,  OpenInd, CloseInd)
    windowFrame[ , c('OpenInd', 'CloseInd', 'Group', 'Hit') := NULL]
  }
  
  # update depth
  windowFrame <- depth(windowFrame, depth = depth)

  if (!rightward) windowFrame[ , c('Open', 'Close') := list((fullLength + 1L) - Close, 
                                                            (fullLength + 1L) - Open)]

  
  windowFrame
  
}


### Sorting, filtering, or modifying windows ----
nest <- function(windowFrame) {
  # setorder(windowFrame, Open, Close)
  open <- windowFrame[!duplicated(OpenInd), Open]
  close <- windowFrame[!duplicated(CloseInd), Close]
  
  steps <- rep(c(1L, -1L), c(length(open), length(close)))[order(c(open, close))]
  stepdepth <- sigma(steps)
  
  opendepth  <- stepdepth[steps == 1L][windowFrame$OpenInd]
  closedepth <- stepdepth[steps == -1L][windowFrame$CloseInd]
  

  windowFrame <- windowFrame[opendepth == (closedepth + 1L)]
  
  windowFrame[ , Hit := CloseInd == min(CloseInd), by = OpenInd]
  windowFrame[Hit == TRUE]
  
}
shunt <- function(windowFrame, open, close) {
  # "shunt" looks for window Close positions that have aleady been matched
  # with a previous Open, and "shunts" them to the next Close.

  closeinds <- unique(windowFrame$CloseInd)
  
  while(any(duplicated(windowFrame$CloseInd))) {
    windowFrame[ , CloseInd := CloseInd + (seq_along(Open) - 1), by = CloseInd]
    windowFrame <- windowFrame[CloseInd %in% closeinds]
  }
  windowFrame[ , Open := open[OpenInd]]
  windowFrame[ , Close := close[CloseInd]]
  
  windowFrame
  
}



depth <- function(windowFrame, depth = NULL, ...) {
  if (nrow(windowFrame) == 0L) {
    windowFrame[ , c('Depth', 'RevDepth') := list(integer(), integer())]
    return(windowFrame)
  }
  
  windowFrame[ , Depth := {
    steps <- rep(c(1L, -1L), c(length(Open), length(Close)))[order(c(Open, Close))]
    stepdepth <- sigma(steps)
    stepdepth[steps == 1L]
  }]
  
  windowFrame[ , RevDepth := Depth - max(Depth) - 1L]

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
  # expands overlapping windows into "groups" appropriate for use with data.table[ , , by = group]
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- dt[unlist(indices)]
  dt_extended[ , contextWindow := rep(seq_along(indices), lengths(indices))]
  
  dt_extended
}

.applyWindows <- function(dt, windowFrame, expr, activeField = 'Token', ..., 
                          inPlace = TRUE, complement = TRUE, alignToOpen = TRUE) {
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- windows2groups(dt, windowFrame)
  
  results <- rlang::eval_tidy(rlang::quo({ dt_extended[ , list(list(!!expr)), by = contextWindow]}))$V1
  # should be a list of results, one result per window
  
  
  edges <- windowFrame[ , if (alignToOpen) Open else Close]
  edge <- if (alignToOpen) head else tail
  
  
  result_lengths <- lengths(results)
  edgeindices <- Map(indices, result_lengths, f = \(i, l) edge(i, n = l))
  
  
  output <- if (complement) dt[[activeField]] else rep(NA, nrow(dt))
  output[.unlist(edgeindices)] <- .unlist(results, recursive = FALSE)
  
  
  if (inPlace) {
    output[setdiff(unlist(indices), unlist(edgeindices))] <- NA
    output
  } else {
    if (complement) {
      output[-setdiff(unlist(indices), unlist(edgeindices))]
    } else {
      output[unlist(edgeindices)]
      
    }
  }
  
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
  

  if (nrow(windowFrame)) x <- .applyWindows(data.table(.x. = x), activeField = '.x.', 
                                            windowFrame,
                                            expr = rlang::expr(sum(.x., na.rm = !!na.rm)),
                                            inPlace = TRUE, complement = TRUE)
  
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
#' we want typically want to apply `hop()` using `groupby = list(Piece, Spine, Path)`.
#' In fact, `humdrumR` [with(in)][withinHumdrum] calls will *automatically* feed these 
#' three fields as `groupby` arguments to `hop()`.
#' So any use of `hop()` in a call to [with(in)][withinHumdrum], will automatically generate the hop sequence
#' in a "melodic" way, within each spine path of each piece.
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
#' In calls to [with/within.humdrumR][withinHumdrum], `groupby` is passed `list(Piece, Spine, Path)` by default.
#'
#' @examples 
#' # use the built-in 'letters' vector
#' 
#' hop(letters)
#' 
#' hop(letters, by = 3)
#' 
#' hop(letters, by = 2, from = 4)
#' 
#' hop(letters, by = 2, from = 'e', to = 'x')
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

