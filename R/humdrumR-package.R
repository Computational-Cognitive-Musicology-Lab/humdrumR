#' humdrumR
#'
#' A toolkit for the analysis of humdrum data.
#'
#' @section Package design:
#' The package \code{humdrumR} has xx main components:
#' \enumerate{
#' \item To represent humdrum data in R, we have the \code{\linkS4class{humdrumR}} \emph{S4} class, and it's core
#' component the \code{\link[humdrumR:humtable]{Humdrum Table}}. 
#' \item To create \code{\linkS4class{humdrumR}} data, a sophisticated
#' humdrum data parser: \code{\link{readHumdrum}}.
#' \item To manipulate and modify \code{\linkS4class{humdrumR}} data,
#' we have the \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} functions,
#' as well as the \code{\link[humdrumR:humPipe]{\%hum>\%}} operator which allows
#' us to use these commands in a pipe-line.
#' \item To facilate the development of functions to work with humdrum tokens---which are simple
#' character strings packed with information---, a useful API we call a 
#' \code{\link[humdrumR:regexDispatch]{regular-expression dispatch system}}.
#' \item A module for representing and manipulating musical pitch information, 
#' with a core \code{\linkS4class{tonalInterval}} \emph{S4} class to represent pitch.
#' \item A module for representing and manipulating musical rhythm information,
#' with a core \code{\linkS4class{recip}} \emph{S4} class to represent rhythm.
#' }
#'
#' @docType package
#' @name humdrumR
#' @importFrom pander evals
#' @importFrom MASS fractions
#' @importFrom combinat permn
#' @importFrom utils combn
#' @importFrom glue glue
NULL

# #' Parsed \code{\linkS4class{humdrumR}} data for 371 Bach chorales
# #' 
# #' This is humdrumR's parsing (via \code{\link{readHumdrum}}) of kern files
# #' representing J.S. Bach's 371 chorales.
# #' 
# #' @format A \code{\linkS4class{humdrumR}} object containing 371 files.
# #' 
# #' @source \url{http://kern.humdrum.org/search?s=t&keyword=chorale}
# "Bach371"