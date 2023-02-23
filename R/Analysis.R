
#' Calculate Shannon entropy/information of data 
#'
#' Information content and entropy are fundamental concepts in [information theory](https://en.wikipedia.org/wiki/Information_theory),
#' which quantify the amount of information (or "surprise") in a random variable.
#' Both concepts are closely related the probability density/mass of events: improbable events have higher information content.
#' The probablity of *each* observation maps to the [information content](https://en.wikipedia.org/wiki/Information_content);
#' The average information content of a variable is the [entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)).
#' Infromation content/entropy can be calculated for discrete probabilities or continuous probabilities,
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
#' 
#' 
#' @export
entropy <- function(x, base, ...) UseMethod('entropy')
#' @export
entropy.table <- function(x, base = 2, margin = NULL, ...) {
  info <- information(proportions(x, margin = margin), base = base)
  frequency <- proportions(x, margin = NULL) 
  
  info <- info[x > 0]
  frequency <- frequency[x > 0]
  
  
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
ic <- function(x, distribution, base) UseMethod('ic')
#' @export
ic.numeric <-function(x, distribution = density(x), base = 2) {
  
  dx <- diff(distribution$x[1:2])
  pmass <- -(log(distribution$y, base = base))
  
  pmass[findInterval(x, distribution$x)]
  
}
#' @export
ic.default <- function(x, distribution = ptable(x), base = 2) {
  
  distribution <- information(distribution, base = base)
  
  distribution[as.character(x)]
  
}

information <- function(ps, base = 2) -(log(ps, base = base))

#' Tabulate and cross proportions
#' 
#' 
#' @export
ptable <- function(..., margin = NULL) proportions(table(...), margin = margin) 


#' Calculate mutual information of two or more variables
#' 
#' 
#' @export
mi <- function(x, ..., base = 2) UseMethod('mi') 

#' @export
mi.table <- function(x, base = 2) {
  
  joint <- proportions(x)
  
  marginals <- lapply(seq_along(dim(x)), \(m) apply(joint, m ,sum))
  ind.joint <- Reduce('*', do.call(expand.grid, marginals))
  dim(ind.joint) <- dim(x)
  # ind.joint <- outer(rowSums(joint), colSums(joint), '*')
  
  joint <- joint[x > 0]
  ind.joint <- ind.joint[x > 0]
  
  sum(log(joint / ind.joint, base = base) * joint)
  
}
   
#' @export
mi.default <- function(..., base = 2) {
  mi.table(table(...), base = base)
}
  
