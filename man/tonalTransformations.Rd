% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humPitch.R
\name{tonalTransformations}
\alias{tonalTransformations}
\alias{tonalTransform}
\alias{invert}
\alias{transposeBy}
\alias{transposeTo}
\title{Tonal Transformations}
\usage{
tonalTransform(
  x,
  direction = TRUE,
  contour = FALSE,
  delta = FALSE,
  sigma = Exclusive \%allin\% c("mint"),
  generic = FALSE,
  simple = FALSE,
  simplifier = floor,
  enharmonic = FALSE,
  Key = NULL,
  Exclusive = NULL
)

invert(tint, around, Key, ...)

transposeBy(x, by, Key, ...)

transposeTo(x, Key, ...)
}
\description{
Various transformations of pitch information

This function transposes tonalIntervals by other tonal intervals.
By default, does real transposition.
However, if a \code{key} argument is specified, tonal transposition
takes place in that (major) key.
}
