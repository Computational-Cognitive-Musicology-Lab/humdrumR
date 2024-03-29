% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humdrumR-package.R
\name{partialMatching}
\alias{partialMatching}
\title{What is "partial matching"?}
\description{
What is "partial matching"?
}
\section{Partial matching explained}{


\code{R} has a very useful functionality called "\emph{partial matching}," where we can
match a \strong{incomplete} character string or variable name with a list of options.
This is achieved using the base-\code{R} function \code{\link[=pmatch]{pmatch()}}, but many \code{R} functions make use of it,
as do \emph{many} \code{humdrumR} functions.

For example, let's say we have a \code{data.frame} (let's call it \code{df}) with three columns: \code{"Number"}, \code{"Letter"}, and \code{"Date"}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{df <- data.frame(Number = 1:2, Letter = c('A', 'B'), Date = c("January", "February"))
}\if{html}{\out{</div>}}

If I want to access the \code{Number} column, most programming languages would require I write at very least \code{df$Number}.
However, \code{R} will give me the correct field even if I write \code{df$Numb}, \code{df$Num}, or even \code{df$N}.
This is partial matching!
The matching happens left-to-right, so as long as I get the beginning of variable right, it will work.

Of course, partial matching only works up to the point that the string matches unambiguously.
For example, if added a \code{Dare} column to \code{df}, then \code{df$D} or \code{df$Da} would return \code{NULL} because they are ambiguous.
You'd need to write at least \code{Dar} or \code{Dat} to get the \code{Dare} and \code{Date} columns respectively.
}

\seealso{
Other {R lessons.}: 
\code{\link{evaluatingExpressions}},
\code{\link{groupingFactors}},
\code{\link{recycling}},
\code{\link{vectorization}}
}
\concept{{R lessons.}}
