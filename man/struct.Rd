% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Struct.R
\name{struct}
\alias{struct}
\alias{is.struct}
\title{struct}
\usage{
is.struct(x)
}
\description{
Virtual class to help create atomic-vector-like composite data objects.
}
\details{
\code{humdrumR} defines a number of \href{http://adv-r.had.co.nz/S4.html}{S4 classes} which are, underneath the surface, \href{https://en.wikipedia.org/wiki/Composite_data_type}{composite data types}
made up of collections of \link[base:vector]{base-R atomic vectors}, stuck together.
The "vectorized" nature of R's atomic types is one of R's key strengths, so in \code{humdrumR} we try to A) mostly use the standard atomic types B)
make all the new types we \emph{do} define act as much like atomic vectors as possible.
\code{struct} is a \emph{virtual} S4 class which serves this purpose: creating composite atomic vectors which act (mostly) like base-R atomic vectors.

As a "virtual class" \code{struct}s themselves don't really exist as independent objects, but the \code{struct} class defines (abstractly) all the necessarry methods to treat a collection of
atomic vectors as a single vector/matrix-like object---simply make your new subclass \href{https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming)}{inherit} \code{struct}
and it is all taken care of. (To do this, specify \code{contains = "struct"} in your call to \link[methods:setClass]{setClass}.)

Important \code{humdrumR} classes which inherit from \code{struct} include:
\itemize{
\item \link[=tonalIntervalS4]{tonal intervals}
\item \link{diatonicSet}
\item \link{tertianSet}
\item \code{\link[=rational]{rational()}}
}

Be warned, R's S4 object-system is limited in this regard: you can't really define S4 classes that act \emph{fully} like R atomics, as
many of their features are hard-coded into R itself and can't be replicated.
The most important limitation of \code{struct} that you may encounter is that, though \code{struct} classes work (ok) in \link[base:data.frame]{data.frames},
\link[data.table:data.table]{data.tables} and \link[tibble:tibble]{tibbles} will either not work or give strange behaviors if you put \code{struct}s into them.
}
\section{Slots}{

\describe{
\item{\code{dim}}{Either \code{NULL} or a non-negative \link[base:integer]{integer-vector} of \code{length == 2L}, representing the number of rows and columns respectively. Dimensions \emph{can} be zero.}

\item{\code{rownames}}{Either \code{NULL} or a \link[base:integer]{integer}/\link[base:character]{character}-vector which is the same length as either
A) if \code{dim == NULL}, the length of the \code{struct} B) if \code{dim != NULL}, the number of rows in the \code{struct}.}

\item{\code{colnames}}{Either \code{NULL} (it \emph{must} be \code{NULL} if \code{dim == NULL}) or a \link[base:integer]{integer}/\link[base:character]{character}-vector of length equal to the number of columns in the \code{struct}.}
}}

\section{Behavior}{


\code{struct} subclasses (i.e., classes which inherit from \code{struct}) behave very similarly to normal \link[base:vector]{R atomic vectors/matrices}.
However, they do differ in a few respects, mostly in ways that are intended to avoid some of the quirky behaviors of R matrices:
In general, the distinction between dimensionless vectors and dimensioned vectors (\link[base:matrix]{matrices}) is slightly weaker in \code{structs} than with normal R atomic vectors/matrices.
Most importantly, dimensioned \code{struct}s won't drop their dimensions under various common operations (\link[base:c]{c}, \verb{[]}, etc.), the way base-R matrices do.
In general, it is easier to interact with a multi-column (matrix-)\code{struct} in the same way as a dimensionless (vector-)\code{struct}.
For example, if the struct has dimensions then \code{length(struct) == nrow(struct)}, instead of \code{length(matrix) == nrow(matrix) * ncol(matrix)}---i.e., the "height"
of the \code{struct} (the number of rows) is its length.
Another big difference is in the behaviors of \link[base:c]{c}: \code{c} doesn't always cause \code{struct}s to lose their dimensions and \code{c} can be used to concatinated multi-column \code{struct}s,
or even mixes of dimensionless and dimensioned \code{struct}s:
If any \code{struct} arguments to \code{c} have dimensions, the \code{struct}s are concatinated via a call to \link[base:cbind]{rbind}, with any dimensionless vectors coerced to
1-column matrices.
Of course, the (resulting) number of columns must all be the same or an error will occur!

Other differences:
\itemize{
\item \code{struct}s can have either no dimensions (\code{dim(struct) == NULL}) or two dimensions. Higher dimensional \code{struct}s are not supported (yet).
\item \code{rowSums} and \code{colSums} will coerce a dimensionless struct to a column matrix.
\item \code{struct}s always throw an error if you try to index them with a index value that is greater than the length/nrow of the \code{struct}.
This is different than atomic vectors, which will pad the vector up to the length of the index you give---a sometimes useful but quirky behavior.
\item \code{struct}s with two dimensions have a \code{cartesian} indexing argument.
If \code{cartesian = TRUE}, the \code{i} and \code{j} arguments are treated as cartesian coordinates.
(This behavior can be achieved with base R matrices (or \code{struct}s) by inputing a matrix with two columns.)
}
}

\section{Requirements}{


To work, \code{struct} makes a few assumptions about your class.
Your class must have one or more slots which are vectors, all of which are the same length.
\code{struct}'s indexing method will cause all of these vectors to be indexed as one.
When you define a new subclass of \code{struct}, it will inherit a
\link[methods:validObject]{validObject} method which assures that all elements are the same dimension.
Thus, if you are writing your own \code{validObject} method (using \link[methods:validObject]{setValidity})
you just have to worry specifically about the validity of the information in your slots,
not that the slots are all the same length.
}

\section{Initialize}{


An initialize method which automatically makes all slots the same length is predefined
for \code{structs}. If you want to make a more specialized \link[methods:new]{initialize} method,
you can still take advantage of the inherited method by using \link[methods:NextMethod]{callNextMethod} at the
beginning of your method.
}

\section{Predefined methods}{


The main purpose of the \code{struct} virtual class is that it defines many of the basic methods you need to manipulate subclass objects.
Most importantly, \link[base:Extract]{indexing} methods are fully defined (that mimic base-R atomic vector/matrix indexing), as well as
basic "structural" methods like \link[base:colnames]{(col/row)names}, \link[base:dim]{dim}, \link[base:length]{length}, \link[base:nrow]{ncol, nrow}, etc.
In addition:
\itemize{
\item If you define \link[base:Comparison]{> and >=}, \code{<} and \code{<=} will be automatically defined.
\item If you define \link[base:character]{as.character} for your subclass, \link[methods:show]{show} and \link[base:format]{format} methods are automatically defined.
}

What's more, default arithmetic methods for addition, subtraction, (scalar-integer) multiplication, and negation (\code{-x}) are defined.
The default addition behavior is that each numeric (\link[base:integer]{base::integer} or \link[base:numeric]{base::numeric}) slot from your subclasses will be added together.
Thus, \code{struct1 + struct2} will extract each numeric/integer slot from each \code{struct}, add them together and create a new \code{struct} from the result.
\code{-struct} will negate all numeric fields, and subtraction is simply defined as adding the negation.
Since \emph{scalar} multiplication is defined, two \code{struct}s cannot be multiplied, but a struct can be multiplied by an integer (all numeric fields are multiplied by the integer(s)).
If these definitions don't work for your subclass, you'll need to create your own, more specific, method!
}

\examples{
setClass('mynewsubclass', contains = 'struct', slots = c(X= 'numeric', Y = 'numeric'))

test <- new('mynewsubclass', X = 1:10, Y = 10:1)

# all of these should work:
test[1:5]
rev(test)  == test
cbind(test, test)
c(test, test)
test * 3
test - test


}
\seealso{
Examples of \code{struct} subclasses: \link{tonalInterval} \link{rhythmInterval} \link{diatonicSet} \link{tertianSet}
}
