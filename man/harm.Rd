% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Chords.R
\name{harm}
\alias{harm}
\alias{roman}
\title{Roman numeral representations of harmony}
\usage{
harm(
  x,
  ...,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  inPlace = FALSE
)

roman(
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
These functions output \href{https://en.wikipedia.org/wiki/Roman_numeral_analysis}{roman numeral}
representations of a tertian harmony.
The \verb{**harm} representation is the most widely used standard for roman numeral notation in humdrum data.
Unlike traditional roman numerals, \verb{**harm} does not indicate inversions with figuration, using lowercase letters
(\code{a}, \code{b}, \code{c}, etc.) instead.
The \code{roman} function however does output (relatively) traditional figures.

The output format of \code{roman()} is very similar to \verb{**harm}.
The main difference is that inversions are indicated using traditional figures
, like \code{653}, instead of \verb{**harm}'s simpler system (using letters).
So, for example, if we take the input \code{E7/B} in the key of A major, we'll get:
}
\details{
\itemize{
\item \code{harm('E7/B', Key = 'A:')} => \code{"V7c"}
\item \code{roman('E7/B', Key = 'A:')} => \code{"V643"}
}
}
\examples{
tertian <- c('AM', 'AMm/3', 'DM', 'Dm', 'B-MM', 'AM/5', 'EMmm')

harm(tertian, Key = 'A:')
roman(tertian, Key = 'A:')

\dontrun{
B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")

with(B075[[ , 2]], harm(Token))
with(B075[[ , 2]], roman(Token))
}

}
