% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pitch.R
\name{cents.humdrumR}
\alias{cents.humdrumR}
\alias{cents}
\title{Apply to humdrumR data}
\usage{
humData |> select(Token) |> cents() 
humData |> cents(simple = TRUE)
humData |> cents(Token, Key = Key)

cents(
  x,
  ...,
  generic = FALSE,
  simple = FALSE,
  octave.relative = FALSE,
  Key = NULL,
  tonalHarmonic = 2^(19/12),
  transposeArgs = list(),
  parseArgs = list(),
  gamutArgs = list(),
  inPlace = FALSE
)
}
\description{
If \code{cents()} is applied to a \link[=humdrumRclass]{humdrumR data class}
you may use the data's \link[=fields]{fields} as arguments.
If no field names are specified, the first \link{selectedField} is used as \code{x}.
}
