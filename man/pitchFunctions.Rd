% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pitch.R
\name{pitchFunctions}
\alias{pitchFunctions}
\title{Translate between pitch representations.}
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

\item{parseArgs}{\emph{\strong{An optional list of arguments passed to the \link[=pitchParsing]{pitch parser}.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to the \link[=pitchParsing]{pitch parser}.}

\item{transposeArgs}{\emph{\strong{An optional list of arguments passed to a special \code{\link[=transpose]{transpose()}} call.}}

Defaults to an empty \code{list()}.

Must be a \code{list} of named arguments to \code{\link[=transpose]{transpose()}}.}

\item{inPlace}{\emph{\strong{Should non-pitch information be retained in the output string.}}

Defaults to \code{FALSE}.

Must be a singleton  \code{logical} value: an on/off switch.

This argument only has an effect if the input (the \code{x} argument) is \code{character} strings,
\emph{and} there is extra, non-pitch information in the input strings "besides" the pitch information.
If so, and \code{inPlace = TRUE}, the output will be placed into an output string beside the original non-pitch information.
If \code{inPlace = FALSE}, only the pitch output information will be returned (details below).}
}
\value{
\code{NULL} inputs (\code{x} argument) return a \code{NULL} output.
Otherwise, returns a vector/matrix of the same length/dimension as \code{x}.
\code{NA} values in the input \code{x} are propagated to the output.
}
\description{
These functions are used to extract and translate between different representations
of pitch information.
The functions can also do things like transposing and simplifying pitches.
}
\details{
The full list of pitch functions is:
\itemize{
\item \strong{Tonal pitch representations}
\itemize{
\item \emph{Absolute pitch representations}
\itemize{
\item \code{\link[=kern]{kern()}}
\item \code{\link[=pitch]{pitch()}}
\item \code{\link[=lilypond]{lilypond()}}
\item \code{\link[=helmholtz]{helmholtz()}}
\item \code{\link[=tonh]{tonh()}} (German-style notation)
}
\item \emph{Relative pitch representations}
\itemize{
\item \code{\link[=interval]{interval()}}
\item \code{\link[=solfa]{solfa()}} (relative-do solfege)
\item \code{\link[=solfg]{solfg()}} (French-style fixed-do solfege)
\item \code{\link[=degree]{degree()}} (absolute scale degrees)
\item \code{\link[=deg]{deg()}} (melodic scale degrees)
\item \code{\link[=bhatk]{bhatk()}} (hindustani swara)
}
\item \emph{Partial pitch representations}
\itemize{
\item \code{\link[=step]{step()}}
\item \code{\link[=accidental]{accidental()}}
\item \code{\link[=quality]{quality()}}
\item \code{\link[=octave]{octave()}}
}
}
\item \strong{Atonal pitch representations}
\itemize{
\item \emph{Musical pitch representations}
\itemize{
\item \code{\link[=semits]{semits()}}
\item \code{\link[=midi]{midi()}}
\item \code{\link[=cents]{cents()}}
\item \code{\link[=pc]{pc()}} (pitch classes)
}
\item \emph{Physical pitch representations}
\itemize{
\item \code{\link[=freq]{freq()}}
}
}
}

These pitch functions all work in similar ways, with similar arguments and functionality.
Each function takes an input pitch representation (which can be anything) and outputs
\emph{its} own pitch representation.
For example, \code{\link[=kern]{kern()}} takes any input representation and outputs \verb{**kern} (pitch) data.
Underneath the hood, the full processing of each function looks like this:
\itemize{
\item \strong{Input} representation (e.g., \verb{**pitch} or \verb{**semits}) \verb{|>}
\itemize{
\item \emph{Parsing} (done by \code{\link[=tonalInterval]{tonalInterval()}}) \verb{|>}
\itemize{
\item \strong{Intermediate} (\link[=tonalIntervalS4]{tonalInterval}) representation \verb{|>}
\item \strong{Transformation} (e.g., \code{\link[=transpose]{transpose()}}) \verb{|>}
}
\item \emph{Deparsing} \verb{|>}
}
\item \strong{Output} representation (e.g. \verb{**kern} or \verb{**solfa})
}

To read the details of the parsing step, read \link[=pitchParsing]{this}.
To read the details of the "deparsing" step, read \link[=pitchDeparsing]{this}.
To read more details about each specific function, click on the links in the list above,
or type \code{?func} in the R command line: for example, \code{?kern}.

The "partial" pitch functions \code{\link[=octave]{octave()}}, \code{\link[=step]{step()}}, \code{\link[=accidental]{accidental()}}, and \code{\link[=quality]{quality()}} are so-called
because they each only return one part/aspect of pitch information, and only that part.
For example, \code{accidental()} only returns he accidentals (if any) of pitches.
}
\seealso{
To better understand how these functions work, read about
how pitches are \link[=pitchParsing]{parsed} and \link[=pitchDeparsing]{deparsed}.
}
