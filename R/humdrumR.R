#' humdrumR: a toolkit for the analysis of humdrum data.
#'
#' humdrumR is a toolkit for the analysis of
#' musical peformance or analysis data encoded in the Humdrum syntax.
#' There is also a \code{\link{humdrumR}} function.
#'
#' The package includes three basic contributions.
#' The first is the humScore S3 class, which is basically a data.frame
#' with a few additional attributes and methods which
#' make them more suitable for representing humdrum data.
#' The second is the humdrumR S3 object, which is a closure of multiple functions
#' for manipulating sets of humScores.
#' The last is a set of functions for manipulating and analyzing
#' humScores, as well as special syntax for composing
#' new functions.
#'
"_PACKAGE"

#' @importFrom pander evals
#' @importFrom MASS fractions
#' @importFrom combinat permn
#' @importFrom utils combn
#' @importFrom glue glue
NULL


# options(humdrum = list(print = 'svg'))

