#' humdrumR
#'
#' \code{humdrumR} is a toolkit for the analysis of data encoded in the \href{http://www.humdrum.org/guide/ch05/}{humdrum syntax}.
#' The humdrum syntax is an incredibly flexible, and powerful, scheme for encoding musical data.
#' Tens of thousands of musical scores (and other musical data) have been encoded in humdrum, many available online, 
#' for instance at \href{http://kern.ccarh.org/}{KernScores}.
#' 
#' \code{humdrumR} is intended as a modernized replacement for the original \href{http://www.humdrum.org/}{humdrum toolkit}, levaraging
#' the power of R to give us enprecedented power to manipulate and analyze humdrum data using concise, expressive syntax.
#'
#' @section Package design:
#' The package \code{humdrumR} has xx main components:
#' \enumerate{
#' \item To represent humdrum data in R, we have the \code{\linkS4class{humdrumR}} \emph{S4} class, and it's core
#' component the \code{\link[humdrumR:humTable]{Humdrum Table}}. 
#' \item To create \code{\linkS4class{humdrumR}} data, a sophisticated
#' humdrum data parser: \code{\link{readHumdrum}}. \code{\linkS4class{humdrumR}} data
#' can also be written back to humdrum-syntax text files using \code{\link{writeHumdrum}}.
#' \item To filter and "index" \code{\linkS4class{humdrumR}} data, we have the 
#' \code{\link{filterHumdrum}} function, which can also be called in a variety of 
#' short hands using \code{R}'s standard \code{\link[base:Extract]{indexing operators}}: \code{[]} and \code{[[]]}.
#' \item To manipulate and modify \code{\linkS4class{humdrumR}} data,
#' we have the \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} and
#' \code{\link{humApply}} functions,
#' as well as the \code{\link[humdrumR:humPipe]{\%hum>\%}} operator which allows
#' us to use these commands in a pipe-line.
#' \item A set of "pipe" operators, so that \code{\linkS4class{humdrumR}} data can be 
#' manipulated and filtered in concise, bash-style pipe. 
#' \item To facilate the development of functions to work with humdrum tokens---which are simple
#' character strings packed with information---, a useful API we call a 
#' \code{\link[humdrumR:regexDispatch]{regular-expression dispatch system}}.
#' \item A \code{\link[humdrumR:humPitch]{module}} for representing and manipulating musical pitch information, 
#' with a core \code{\linkS4class{tonalInterval}} \emph{S4} class to represent pitch.
#' \item A \code{\link[humdrumR:humRhythm]{module}} for representing and manipulating musical rhythm information,
#' with a core \code{\linkS4class{rhythmInterval}} \emph{S4} class to represent rhythm.
#' }
#'
#' @docType package
#' @name humdrumR
#' @importFrom MASS fractions
#' @importFrom combinat permn
#' @importFrom utils combn
#' @importFrom glue glue
#' @importFrom abind abind
#' @importFrom stringr str_count str_detect str_dup str_extract str_match str_pad str_replace str_split str_sub
#' @importFrom data.table data.table rbindlist setorder setcolorder copy as.data.table is.data.table
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



setOldClass('quosure')
