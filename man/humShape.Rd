% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humClass.R
\name{humShape}
\alias{humShape}
\alias{mergeHumdrum}
\alias{foldHumdrum}
\alias{foldStops}
\alias{foldPaths}
\alias{foldRecords}
\title{These functions are used to change the "shape"
of data stored in \code{\link[humdrumR:humTable]{humdrum tables}}
(held within \code{\linkS4class{humdrumR}} objects of course).}
\usage{
mergeHumdrum(...)

foldHumdrum(humdrumR, byfields, foldAtomic = TRUE, sep = " ", padPaths = FALSE)

foldStops(humdrumR, foldAtomic = TRUE, sep = " ")

foldPaths(humdrumR, foldAtomic = TRUE, sep = " ")

foldRecords(humdrumR, foldAtomic = TRUE, sep = " ", padPaths = FALSE)
}
\arguments{
\item{humdrumR}{A \code{\linkS4class{humdrumR}} data object.
(see the \code{\link[humdrumR:humTable]{humdrum table}} documentation \strong{Fields} section for explanation.).}

\item{foldAtomic}{\code{logical}. If \code{foldAtomic == TRUE}, each stop is collapsed to a single string
\code{foldAtomic == FALSE}, each stop is collapsed to a list of tokens.}

\item{sep}{\code{character}. If \code{foldAtomic == TRUE}, collapsed tokens are separated by this string.}

\item{pad}{\code{logical}. Should \code{\link[humdrumR:humColumns]{path/column padding tokens}} be included?}
}
\description{
The \code{foldXXX} family allows you collapse all
\code{\link[humdrumR:humTable]{user fields}}
across groups in another field.
}
