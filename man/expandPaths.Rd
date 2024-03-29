% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reshape.R
\name{expandPaths}
\alias{expandPaths}
\title{Expand paths into new spines}
\usage{
expandPaths(x, asSpines)
}
\arguments{
\item{asSpines}{\emph{\strong{Should paths expanded into new spines?}}

Defaults to \code{TRUE}.

Must be a singleton \code{logical} value: an on/off switch.

If \code{TRUE}, the expanded paths are copied into their
own new spines (shifting higher spines over as needed).}

\item{humdrumR}{\emph{\strong{HumdrumR data.}}

Must be a \link[=humdrumRclass]{humdrumR data object}.}
}
\description{
This function takes a \link[=humdrumRclass]{humdrumR object}
and "expands" the content of any spine paths by filling them in with
the content of their parent path(s).
}
\details{
For example, imagine that in humdrum representation of a eight-measure
piano score, the annotator included an \href{https://en.wikipedia.org/wiki/Ossia}{ossia}
passage in the seventh measure.
If we want to simply ignore the ossia passage, we can just specify a \code{\link[=subset]{subset()}} where \code{Path == 0}.
If we want to study \emph{only} the ossia passage, we can grab a \code{\link[=subset]{subset()}} where \code{Path == 1}.
However, what if we want to study the ossia as it would be performed, with the ossia measure
swapped in for measure 7, but still using measures 1-6 and 8 from the main path?
\code{expandPaths()} will help us do just this:
\code{expandPaths()} will copy the contents of measure 1-6 and 8 into the second path and,
if \code{asSpines = TRUE}, then copy the path into it's own new spine.
We can then treat that new "full" path/spine just like any other path/spine.
}
\seealso{
Other {Humdrum table reshaping functions}: 
\code{\link{cleave}()},
\code{\link{collapseHumdrum}()},
\code{\link{rend}()}
}
\concept{{Humdrum table reshaping functions}}
