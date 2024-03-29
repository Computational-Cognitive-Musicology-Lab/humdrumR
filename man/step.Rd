% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pitch.R
\name{step}
\alias{step}
\title{Extract scale step.}
\usage{
step(
  x,
  ...,
  generic = FALSE,
  simple = FALSE,
  octave.relative = FALSE,
  Key = NULL,
  transposeArgs = list(),
  parseArgs = list(),
  gamutArgs = list(),
  inPlace = FALSE
)
}
\arguments{
\item{x}{\emph{\strong{Input data to parse as pitch information.}}

The \code{x} argument can be any (\link[base:vector]{atomic}) vector, or a \link[=tonalIntervalS4]{tonalInterval}, or \code{NULL}.}

\item{...}{\emph{\strong{Arguments passed to the \link[=pitchDeparsing]{pitch deparser}.}}

There are also two hidden (advanced) arguments you can specify: \code{memoize} and \code{deparse} (see the details below).}

\item{generic}{\emph{\strong{Should "specific" pitch information (accidentals and qualites) be discarded?}}

Defaults to \code{FALSE}.

Must be a singleton \code{logical} value: an on/off switch.}

\item{simple}{\emph{\strong{Should "compound" pitch information (octave/contour) be discarded?}}

Defaults to \code{FALSE}.

Must be a singleton \code{logical} value: an on/off switch.}

\item{Key}{\emph{\strong{The input \code{Key} used by the parser, deparser, and transposer.}}

Defaults to \code{NULL}.

Must be a \code{diatonicSet} or something coercable to \code{diatonicSet}; must be either length \code{1} or \code{length(x)}}

\item{transposeArgs}{\emph{\strong{An optional list of arguments passed to a special \code{\link[=transpose]{transpose()}} call.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to \code{\link[=transpose]{transpose()}}.}

\item{parseArgs}{\emph{\strong{An optional list of arguments passed to the \link[=pitchParsing]{pitch parser}.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to the \link[=pitchParsing]{pitch parser}.}

\item{inPlace}{\emph{\strong{Should non-pitch information be retained in the output string.}}

Defaults to \code{FALSE}.

Must be a singleton  \code{logical} value: an on/off switch.

This argument only has an effect if the input (the \code{x} argument) is \code{character} strings,
\emph{and} there is extra, non-pitch information in the input strings "besides" the pitch information.
If so, and \code{inPlace = TRUE}, the output will be placed into an output string beside the original non-pitch information.
If \code{inPlace = FALSE}, only the pitch output information will be returned (details below).}
}
\description{
This is equivalent to using any \link[=pitchFunctions]{pitch function} with the
arguments \code{generic = TRUE}, \code{simple = TRUE}, and \code{step.labels = NULL}.
By default, \code{step()} will returns steps relative to the key---set \code{Key = NULL} if you don't want this.
}
\examples{
\dontrun{
chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')

within(chorales, step(Token))

within(chorales, step(Token, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B')))
}

}
\seealso{
Other {pitch functions}: 
\code{\link{accidental}()},
\code{\link{bhatk}()},
\code{\link{degree}()},
\code{\link{freq}()},
\code{\link{helmholtz}()},
\code{\link{interval}()},
\code{\link{kern}()},
\code{\link{lilypond}()},
\code{\link{octave}()},
\code{\link{pc}()},
\code{\link{pitch}()},
\code{\link{quality}()},
\code{\link{semits}()},
\code{\link{solfa}()},
\code{\link{solfg}()},
\code{\link{tonh}()}

Other {partial pitch functions}: 
\code{\link{accidental}()},
\code{\link{octave}()},
\code{\link{quality}()}
}
\concept{{partial pitch functions}}
\concept{{pitch functions}}
