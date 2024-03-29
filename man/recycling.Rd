% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humdrumR-package.R
\name{recycling}
\alias{recycling}
\alias{padding}
\title{What are "recycling" or "padding"?}
\description{
What are "recycling" or "padding"?
}
\section{Recycling and Padding results}{


Many R functions will "recycle" their results; other functions will "pad" their results.
What does this mean?
The two options refer to different strategies R code often provides to
maintain \link{vectorization}.
The key idea of vectorization is that we want the length/size of function inputs and outputs to be the same.
If I give a 100-long vector to a function as input, I want a 100-long vector as output.
\subsection{Result is too short (pad or recycle)}{

What happens if I have a that outputs a result that is \emph{shorter} than the input, but I \strong{need}
it to be the same length?
Well, one option is to "pad" the output with \code{NA} values.
For example, if I call \code{mean(1:9)}, my 9-long input results in a scalar (1-long) output (\code{5}).
To force the output to be 9-long (to match the input), I could pad it with \code{NA} like \code{c(5, NA, NA, NA, NA, NA, NA, NA, NA)}.

In many cases, padding a result like \code{mean(1:9)} is not very useful---the useful information (the mean, in this case)
is stuck at only one index!
Instead, it is more useful to "recycle" the result.
What we do is take the result and duplicate it ("recycle") over and over again until it matches the length of the
input.
So we'd recycle the result of \code{mean(1:9)} to be \code{c(5, 5, 5, 5, 5, 5, 5, 5, 5)}.
This is often (but not always) very useful!

The most common (and best) case for recycling is, like our example above, when the result we recycle is a "scalar" (single)
value.
That one value is simply copied \code{length(input)} times.
We can also recycle results that are non-scalar.
For example, imagine we have a function \code{summ()} which calculates the minimum, median, and maximum of a vector:
the call \code{summ(1:9)} would return \code{c(1, 5, 9)}.
This result could be recycled to get \code{c(1, 5, 9, 1, 5, 9, 1, 5, 9)}.
This sort of recycling is less likely to be useful, and can be confusing sometimes---for example, there is
probably no meaningful reason why the median should line up with the original input values \code{c(2, 5, 8)}.
However, R will (generally) happily do it for you!
It is good practice to \emph{only} rely on scalar recycling, and avoid non-scalar recycling unless you are really
sure it is what you want.

One final note: if the result you are recycling isn't a length which evenly divides the input, you will see an \emph{warning} message
saying \verb{longer object length is not a multiple of shorter object length}.
To give an example, image if we used the \code{range()} function, which returns the minimum and the maximum but not the median, on our input:
the result of \code{range(1:9)} is \code{c(1, 9)}, and this would recycle as \code{c(1, 9, 1, 9, 1, 9, 1, 9, 1)}.
The last repetition of the result is cut short, because two does not evenly divide nine.
Since non-scalar recycling of results is often not useful or meaningful in \emph{general}, R takes it
as a particularly bad sign that your result does not evenly divide your input, which is why you get a warning.
}

\subsection{Result is too long (index)}{

What happens if I have a function that outputs a result that is \emph{longer} than the input, but I \strong{need}
it to be the same length?
Well, the most obvious thing to do is cut off the excess---so something like \verb{head(output, n = length(input)}.
Of course, that may or may not make sense depending
on what the function is doing!
}
}

\seealso{
Other {R lessons.}: 
\code{\link{evaluatingExpressions}},
\code{\link{groupingFactors}},
\code{\link{partialMatching}},
\code{\link{vectorization}}
}
\concept{{R lessons.}}
