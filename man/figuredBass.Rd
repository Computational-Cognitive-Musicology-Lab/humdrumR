% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Chords.R
\name{figuredBass}
\alias{figuredBass}
\title{Figured bass representation of harmony}
\usage{
figuredBass(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)
}
\arguments{
\item{x}{\emph{\strong{An \code{atomic} vector.}}

The \code{x} argument can be any (\link[base:vector]{atomic}) vectors}

\item{Key}{\emph{\strong{The diatonic key used by the parser, deparser, and transposer.}}

Defaults to \code{NULL}, which is interpreted as C major.

Must be a \code{diatonicSet} or something coercable to \code{diatonicSet}; must be either length \code{1} or \code{length(x)}.}

\item{transposeArgs}{\emph{\strong{An optional list of arguments passed to a \code{\link[=transpose]{transpose()}} call.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to \code{\link[=transpose]{transpose()}}.}

\item{parseArgs}{\emph{\strong{An optional list of arguments to the \link[=chordParsing]{chord parser}.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to the \link[=chordParsing]{chord parser}.}

\item{inPlace}{\emph{\strong{Should non-chord information be retained in the output string.}}

Defaults to \code{FALSE}.

Must be a singleton \code{logical} value: an on/off switch.}
}
\description{
This function outputs a \href{https://en.wikipedia.org/wiki/Figured_bass}{figured bass}
representation of a tertian harmony.
}
\examples{
romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')

figuredBass(romanNumerals)

tertian <- c('CM', 'CMm/3', 'FM', 'Fm', 'D-MM', 'GMmm')

figuredBass(tertian)

\dontrun{
B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
with(B075[[ , 2]], figuredBass(Token))
}

}
