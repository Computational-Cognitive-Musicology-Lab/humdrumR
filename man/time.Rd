% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rhythm.R
\name{time}
\alias{time}
\alias{seconds.default}
\alias{seconds.humdrumR}
\alias{seconds}
\alias{ms.default}
\alias{ms.humdrumR}
\alias{ms}
\alias{dur.default}
\alias{dur.humdrumR}
\alias{dur}
\title{Clock-time representations of duration}
\usage{
\method{seconds}{default}(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> seconds() 
humData |> seconds(Token)

seconds(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

\method{ms}{default}(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> ms() 
humData |> ms(Token)

ms(
  x,
  ...,
  BPM = "*M60",
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

\method{dur}{default}(
  x,
  ...,
  BPM = "*M60",
  minutes = FALSE,
  hours = FALSE,
  days = FALSE,
  months = FALSE,
  years = FALSE,
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)

humData |> select(Token) |> dur() 
humData |> dur(Token)

dur(
  x,
  ...,
  BPM = "*M60",
  minutes = FALSE,
  hours = FALSE,
  days = FALSE,
  months = FALSE,
  years = FALSE,
  parseArgs = list(),
  scale = 1,
  unit = 1,
  inPlace = FALSE
)
}
\arguments{
\item{x}{\emph{\strong{An input  vector.}}

The \code{x} argument can be any (\link[base:vector]{atomic}) vector, or a \link[=rational]{rational (rhythmInterval)}, or \code{NULL}.

Must be \link[=rhythmParsing]{parsable as rhythm infromation}.}

\item{...}{\emph{\strong{Arguments passed to the \link[=rhythmParsing]{rhythm parser}.}}

There are also two hidden (advanced) arguments you can specify: \code{memoize} and \code{deparse} (see the details below).}

\item{scale}{\emph{\strong{A \code{numeric} or \link{rational} value which is used as the output unit of measurement.}}

Defaults to \code{rational(1, 1)}.

Must be \code{numeric} or \link{rational}.}

\item{inPlace}{\emph{\strong{Should non-rhythm information be retained in the output string?}}

Defaults to \code{FALSE}.

Must be a singleton  \code{logical} value: an on/off switch.

Defaults to \code{FALSE}.

A singleton \code{logical} value, or \code{NA}.

See "Grace notes" section below.}

\item{minutes}{(\code{logical}, T/F) Should the \code{dur} output include minutes?}

\item{hours}{(\code{logical}, T/F) Should the \code{dur} output include hours?}

\item{days}{(\code{logical}, T/F) Should the \code{dur} output include days?}

\item{months}{(\code{logical}, T/F) Should the \code{dur} output include months?}

\item{years}{(\code{logical}, T/F) Should the \code{dur} output include years?}
}
\description{
These functions convert duration values to clock-time representations.
\code{seconds()} and \code{ms()} output \code{numeric} values.
\code{dur()} outputs a \code{character} string encoding the humdrum \href{https://www.humdrum.org/rep/dur/index.html}{**dur}
representation of time.

If \code{seconds()} is applied to a \link[=humdrumRclass]{humdrumR data class}
you may use the data's \link[=fields]{fields} as arguments.
If no field names are specified, the first \link{selectedField} is used as \code{x}.

If \code{ms()} is applied to a \link[=humdrumRclass]{humdrumR data class}
you may use the data's \link[=fields]{fields} as arguments.
If no field names are specified, the first \link{selectedField} is used as \code{x}.

If \code{dur()} is applied to a \link[=humdrumRclass]{humdrumR data class}
you may use the data's \link[=fields]{fields} as arguments.
If no field names are specified, the first \link{selectedField} is used as \code{x}.
}
\details{
These functions require a \code{BPM} (beats-per-minute) argument to be specified.
By default, the value is 60 bpm.
}
\section{dur}{


The \href{https://www.humdrum.org/rep/dur/index.html}{**dur} output can be modified to include different clock-time units:
The \code{minutes}, \code{hours}, \code{days}, \code{months}, and \code{years} arguments are all true/false \code{logical} arguments, indicating whether
to use that unit in the output (all default to \code{FALSE}).
For example, if \code{minutes = FALSE}, an input of 90 seconds will return \code{":90"} (90 seconds!), but if
\code{minutes = TRUE}, the output will be \verb{:1:30} (one minute and thirty seconds).
}

\examples{

tokens <- c('4.GG', '8G', '16E', '16F#', '16G', '16D', 'q8D#', '4E')

seconds(tokens)
seconds(tokens, BPM = '90')
ms(tokens, BPM = '90')
dur(tokens)

wholenotes <- c(10, 30, 90, 310, 5000)
dur(wholenotes)
dur(wholenotes, minutes = TRUE)
dur(wholenotes, minutes = TRUE, hours = TRUE)

quarternotes <- c(10, 30, 90, 310, 5000)
dur(quarternotes, minutes = TRUE, BPM = '120', parse(unit = '4'))

\dontrun{
B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")

with(B075[[,3:4]], seconds(Token))
}


}
\seealso{
To better understand how this function works,
read about the \link[=rhythmFunctions]{family of rhythm functions},
or how rhythms are \link[=rhythmParsing]{parsed} and \link[=rhythmDeparsing]{deparsed}.

Other time functions: 
\code{\link{bpm2sec}()}
}
\concept{time functions}
