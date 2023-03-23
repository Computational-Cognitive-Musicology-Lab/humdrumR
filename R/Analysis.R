# Probabilities ----

#' Tabulate and cross proportions
#' 
#' 
#' @export
ptable <- function(...) UseMethod('ptable')

#' @rdname ptable
#' @export
ptable.default <- function(..., margin = NULL) ptable.table(table(...), margin = margin)


#' @rdname ptable
#' @export
ptable.table <- function(tab, margin = NULL, na.rm = TRUE) {
  na <- is.na(tab)
  tab[na] <- 0
  ptab <- proportions(tab, margin = margin) 
  
  if (na.rm) ptab[na] <- NA
  
  attr(ptab, 'margin') <- margin
  
  dimnames <- names(dimnames(tab))
  names(dimnames(ptab)) <- ifelse(dimnames == '', 
                                  make.unique(rep(c('X', 'Y', 'Z', LETTERS[9:23], letters[c(24:26, 9:23)]), length.out = length(dimnames))), 
                                  dimnames)
  
  ptab %class% 'ptable'
}




#' @rdname ptable
#' @export
`[.ptable` <- function(ptab, i, j, ..., drop = FALSE) {
  
  new <- unclass(ptab)[i, j, ..., drop = drop]
  
  
  class(new) <- class(ptab)
  attr(new, 'margin') <- attr(new, 'margin')
  new
  
}

#' @rdname ptable
#' @export
print.ptable <- function(x, digits = 4) {
  x <- unclass(x)
  zeros <- x == 0
  x[] <- as.character(x)
  n <- nchar(x) - 2
  long <- n > digits & !is.na(x)
  x[long] <- paste0(substr(x[long], start = 1, stop = digits + 2), '*')
  
  # dimension names
  header <- pdist.name(x)
  attr(x, 'margin') <- NULL
  
  x[zeros] <- '.'
  x[] <- gsub('^0', '', x)
  print(x, quote = FALSE, na.print = '.NA')
  cat('\t\tP(', header, ')\n', sep = '')
}

pdist.name <- function(ptab) {
  dimnames <- names(dimnames(ptab))
  
  if (length(attr(ptab, 'margin'))) {
    independent <- dimnames[attr(ptab, 'margin')]
    dependent <- setdiff(dimnames, independent)
    
    paste0(paste(dependent, collapse = ','), 
           '|', 
           paste(independent, collapse = ','))
    
    
  } else {
    paste(dimnames, collapse = ',') 
  }
  
}


# Information theory ----



#' Calculate Entropy or Information Content of variables 
#'
#' Information content and entropy are fundamental concepts in [information theory](https://en.wikipedia.org/wiki/Information_theory),
#' which quantify the amount of information (or "surprise") in a random variable.
#' Both concepts are closely related the probability density/mass of events: improbable events have higher information content.
#' The probability of *each* observation maps to the [information content](https://en.wikipedia.org/wiki/Information_content);
#' The average information content of a variable is the [entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)).
#' Information content/entropy can be calculated for discrete probabilities or continuous probabilities,
#' and humdrumR defines methods for calculating both.
#' 
#' @details 
#' 
#' To calculate information content or entropy, we must assume (or estimate) a probability distribution.
#' HumdrumR uses R's standard [table()] and [density()] functions to estimate discrte and continuous probability
#' distributions respectively.
#' 
#' Entropy is the average information content of a variable.
#' The `entropy()` function can accept either a [table()] object (for discrete variables), 
#' or a [density()] object (for continuous variables).
#' If `entropy()` is passed an [atomic][base::vector()] vector,
#' the values of the vector are treated as observations or a random variable:
#' for `numeric` vectors, the [stats::density()] function is used to estimate the probability distribution
#' of the random (continuous) variable, then entropy is computed for the density.
#' For other atomic vectors, [table()] is called to tabulate the discrete probability mass for each
#' observed level, and entropy is then computed for the table.
#'
#' The `ic()` function only accepts atomic vectors as its main (`x`) argument, but must also
#' be provided a `distribution` argument.
#' By default, the `distribution` argument is estimated using [density()] (`numeric` input) or [table()] (other input).
#' 
#' 
#' @family {Information theory functions} 
#' @export
entropy <- function(..., base) UseMethod('entropy')
#' @rdname entropy
#' @export
entropy.default <- function(..., base = 2, margin = NULL) {
  entropy.table(table(...), base = base, margin = margin)
}

#' @rdname entropy
#' @export
entropy.table <- function(tab, base = 2, margin = NULL, ...) {
  info <- information(ptable(tab, margin = margin), base = base)
  frequency <- proportions(tab, margin = NULL) 
  
  name <- paste0('H(', pdist.name(info), ')')
  
  setNames(sum(info * frequency, na.rm = TRUE), name)
  
}
#' @rdname entropy
#' @export
entropy.density <- function(x, base = 2) {
  dx <- diff(x$x[1:2])
  -sum(log(x$y, base = base) * x$y * dx)
}
#' @rdname entropy
#' @export
entropy.numeric <- function(x, base = 2, ...) entropy.density(density(x, ...), base = base)

#' @rdname entropy
#' @export
ic <- function(..., distribution, base) UseMethod('ic')
#' @rdname entropy
#' @export
ic.numeric <-function(x, distribution = density(x), base = 2) {
  
  dx <- diff(distribution$x[1:2])
  pmass <- -(log(distribution$y, base = base))
  
  pmass[findInterval(x, distribution$x)]
  
}
#' @rdname entropy
#' @export
ic.default <- function(..., distribution = ptable(...), base = 2) {
  checks(distribution, xclass('table'))
  
  observations <- lapply(list(...), as.character)
  
  if (length(observations) != length(dim(distribution))) .stop("The number of observation vectors must match dimensions of the distribution.")
  
  distribution <- information(distribution, base = base)
  
  distribution[do.call('cbind', observations)]
  
}

information <- function(ps, base = 2) {
  lps <- -(log(ps, base = base))
  lps[ps == 0] <- NA
  lps
}


#' Calculate Mutual Information of variables
#' 
#' @family {Information theory functions} 
#' @export
mutualInfo <- function(x, ..., base = 2) UseMethod('mutualInfo') 

#' @rdname mutualInfo
#' @export
mutualInfo.table <- function(x, base = 2) {
  sum(.mutualinfo(x, base = base) * proportions(x), na.rm = TRUE)
}

.mutualinfo <- function(tab, base = 2){
  joint <- proportions(tab)
  
  marginals <- lapply(seq_along(dim(tab)), \(m) apply(joint, m ,sum))
  ind.joint <- Reduce('*', do.call(expand.grid, marginals))
  dim(ind.joint) <- dim(tab)
  dimnames(ind.joint) <- dimnames(tab)
  # ind.joint <- outer(rowSums(joint), colSums(joint), '*')
  
  joint[tab == 0] <- ind.joint[tab == 0] <- NA
  
  log(joint / ind.joint, base = base) 
}
   
#' @rdname mutualInfo
#' @export
mutualInfo.default <- function(..., base = 2) {
  mutualInfo.table(table(...), base = base)
}
  
#' @rdname mutualInfo
#' @export 
pmutualInfo <- function(..., base = base) {
  
  
  info <- .mutualinfo(table(...), base = base)
  
  observations <- lapply(list(...), as.character)
  
  info[do.call('cbind', observations)]
}


#' Calculate cross entropy between two distributions
#' 
#' TBA
#' @family {Information theory functions}
#' @export
crossentropy <- function(..., distribution, base) UseMethod('crossentropy')

#' @rdname crossentropy
#' @export
crossentropy.table <- function(tab, distribution, base = 2){
  if (!all(dim(tab) == dim(distribution))) .stop("The number of observation vectors must match dimensions of the distribution.")
  
  info <- information(proportions(tab), base = base)
  
  if (!is.table(distribution)) {
    distribution <- if (is.list(distribution)) do.call('table', distribution) else table(distribution)
  }
  frequency <- proportions(distribution, margin = NULL) 
  
  info <- info[tab > 0]
  frequency <- frequency[tab > 0]
  
  
  sum(info * frequency)
  
}
#' @rdname crossentropy
#' @export
crossentropy.default <- function(..., distribution, base = 2) {
  tab <- table(...)
 
  
  crossentropy.table(tab, distribution, base = base)
}

# table ----


#' @export
table <- function(..., 
                  exclude = if (useNA == 'no') c(NA, NaN),
                  useNA = 'ifany',
                  dnn = NULL,
                  deparse.level = 1) {
  
  # exprs <- rlang::enexprs(...)
  exprs <- as.list(substitute(list(...)))[-1L]
  
  args <- list(...)
  dimnames <- .names(args)
  if (is.null(dnn) && deparse.level > 0 && any(dimnames == '')) {
    
    symbols <- sapply(exprs, rlang::is_symbol)
    labels <- sapply(exprs, rlang::expr_name)
    
    deparse <- switch(deparse.level,
                      dimnames == '' & symbols,
                      dimnames == '')
    dimnames[deparse] <- labels[deparse]
  }
  
  
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) factorize(arg) else arg
                 })
  tab <- do.call(base::table,
                 c(args, list(exclude = exclude, useNA = useNA, deparse.level = 0)))
  
  dimnames(tab) <- lapply(dimnames(tab), \(dn) ifelse(is.na(dn), '.', dn))
  names(dimnames(tab)) <- dimnames
  
  tab

}

factorize <- function(token) {
  factorizer <- token@Attributes$factorizer
  if (is.null(factorizer)) return(factor(token@.Data))
  
  factorizer(token)
  
}


## Plotting defaults stuff ----

#' @export
plot <- function(x, y = NULL, ..., type = 'p', recycle = TRUE, add = FALSE, 
                 col = flatly[1], pch = 16, 
                 cex = seq(.7, .2, length.out = 8)[findInterval(ceiling(log10(length(x))), 1:8)],
                 log = "", 
                 xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
                 col.axis = par('col.axis')) {
  
  if (is.logical(log)) log <- if (log[1]) 'y' else ''
  
  xlabel <- if (!missing(x)) deparse1(substitute(x))
  ylabel <- if (!missing(y)) deparse1(substitute(y))
  
  xy <- xy.coords(x = x, y = y, xlab = xlabel, ylab = ylabel, log = log, recycle = recycle)
  
  xlim <- xlim %||% range(xy$x)
  ylim <- ylim %||% range(xy$y)
  if (!add) {
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, log = log, ...)
    
    title(...)
    axis(1, pretty(xlim, n = 10L, min.n = 5L), las = 1, tick = FALSE, xpd = TRUE)
    axis(2, pretty(ylim, n = 10L, min.n = 5L), las = 1, tick = FALSE)
    mtext(xlab %||% xy$xlab, 1, line = 2.5, col = col.axis)
    ylab <- ylab %||% xy$ylab 
    mtext(ylab, 2, line = 2.5, las = if (nchar(ylab) <= 3) 1 else 3, col = col.axis)
  }
  plot.xy(xy, type = type, ..., col = col, pch = pch, cex = cex)
    
    
  
}

#' @export
hist <- function(x, ..., col = flatly[1],
                 log = '', 
                 xlim = range(x, na.rm = TRUE), ylim = NULL,
                 freq = TRUE) {
  
  # if (log == 'x') x <- log(x)
  
  y <- graphics::hist(x, ..., xlim = xlim, ylim = ylim,
                      col = col, border = flatly[5],
                      axes = FALSE, freq = freq)
  
  axis(1, pretty(xlim, n = 10L, min.n = 5L), las = 1, tick = FALSE)
  
  yrange <- pretty(if (freq) y$counts else y$density, n = 10L, min.n = 5L)
  axis(2, yrange, tick = FALSE, las = 1)
  invisible(y)
}

#' @export
barplot <- function(height,  ..., 
                    beside = TRUE, col = NULL,
                    ylim = NULL, log = '', border = NA, yaxis, 
                    freq = TRUE, probability = !freq) {
  if (!freq) {
    height <- height / sum(height)
    if (is.null(ylim)) ylim <- c(0, 1)
    yaxis <- pretty(ylim, n = 10L, min.n = 5L)
  }
  
  twoD <- hasdim(height) && length(dim(height)) > 1L
  
  if (is.null(col)) {
    col <- flatly[if (twoD) 1:nrow(height) else 1]
  }
  
  if (is.logical(log)) log <- if (log[1]) 'y' else ''
  
  plot <- graphics::barplot(height, ..., beside = beside, legend.text = twoD, 
                            col = col, ylim = ylim, border = border, axes = FALSE, log = log)
  
  
  if (missing(yaxis)) {
    yran <- range(setdiff(height, 0))
    if (log == 'y') yran <- log10(yran)
    yaxis <- axisTicks(yran, log = log == 'y', nint = 12)
    
  }
  axis(2, yaxis, las = 1, tick = FALSE)
 
  invisible(plot)
}

