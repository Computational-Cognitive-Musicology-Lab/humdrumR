% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Regex.R
\name{REparser}
\alias{REparser}
\alias{REparse}
\title{Parse String Using Regular expressions}
\usage{
REparser(
  res,
  parse.strict = TRUE,
  parse.exhaust = TRUE,
  parse.lead = FALSE,
  parse.rest = FALSE,
  toEnv = FALSE
)

REparse(
  str,
  res,
  parse.strict = TRUE,
  parse.exhaust = TRUE,
  parse.lead = FALSE,
  parse.rest = FALSE,
  reverse = FALSE,
  sep = NULL,
  toEnv = FALSE
)
}
\description{
Takes an input string and parses it into a sequence of regular expressions.
}
\details{
If \code{exhaustive} is TRUE, the string must be exhaustively broken up by the matching regular expressions.
Superfluous (non-match) characters at the begginning, end, or in bettween matches, will result in
all \code{NA} being returned.

------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
}
