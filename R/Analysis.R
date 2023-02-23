
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
#' @export
entropy.table <- function(tab, base = 2, margin = NULL, ...) {
  info <- information(proportions(tab, margin = margin), base = base)
  frequency <- proportions(tab, margin = NULL) 
  
  info <- info[tab > 0]
  frequency <- frequency[tab > 0]
  
  
  sum(info * frequency)
  
}
#' @export
entropy.density <- function(x, base = 2) {
  dx <- diff(x$x[1:2])
  -sum(log(x$y, base = base) * x$y * dx)
}
#' @export
entropy.numeric <- function(x, base = 2, ...) entropy.density(density(x, ...), base = base)
#' @export
entropy.default <- function(..., base = 2, margin = NULL) {
  entropy.table(table(...), base = base, margin = margin)
}


#' @rdname entropy
#' @export
ic <- function(..., distribution, base) UseMethod('ic')
#' @export
ic.numeric <-function(x, distribution = density(x), base = 2) {
  
  dx <- diff(distribution$x[1:2])
  pmass <- -(log(distribution$y, base = base))
  
  pmass[findInterval(x, distribution$x)]
  
}
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


#' @export
crossentropy <- function(..., distribution, base) UseMethod('crossentropy')

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
#' @export
crossentropy.default <- function(..., distribution, base = 2) {
  tab <- table(...)
 
  
  crossentropy.table(tab, distribution, base = base)
}