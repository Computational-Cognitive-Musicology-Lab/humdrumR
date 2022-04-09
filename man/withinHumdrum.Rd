% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Apply.R
\name{withinHumdrum}
\alias{withinHumdrum}
\alias{withHumdrum}
\alias{inHumdrum}
\alias{humApply}
\title{with(in)Humdrum}
\usage{
withinHumdrum(humdrumR, ...)

withHumdrum(humdrumR, ..., drop = TRUE)

inHumdrum(humdrumR, ...)

humApply(humdrumR, FUN, ..., within = TRUE, doplot = FALSE)
}
\arguments{
\item{humdrumR}{A \link{humdrumRclass} data object.}

\item{...}{Any arguments which can be fed to
\verb{[humdrumR:with-in-Humdrum][with(in)Humdrum]} as formulae (except for
\code{do} expressions, which are replaced by the \code{FUN} argument!).
However, rather that writinging formula in the format \code{Keyword ~ Expression},
\code{humApply} arguments should be written as normal \code{R} arguments:
\code{Keyword = Expression}.
Unnamed arguments are ignored.
#' @param within A logical. If \code{TRUE} (the default),
\verb{[humdrumR:with-in-Humdrum][withinHumdrum]} is used to apply the
function---meaning that the output is reconstituted into a new field in the
\code{humdrumR} object. If \code{within == FALSE},
\verb{[humdrumR:with-in-Humdrum][withHumdrum]} is used instead,
which results in the function's output being returned inprocessed.}

\item{drop}{This argument is concetually similar to the \code{drop} argument in R matrices and data.frames.
If \code{drop = TRUE}, the output of \code{withHumdrum} is simplified as much as possible (trying to return
the "raw" vector, list, table, etc. within it). If \code{drop = FALSE}, the result is \emph{always}
a \code{data.table}. The default value (\code{drop = TRUE}) is usually what we want because it is more
intuitive, but in more complex code, it can be helpful to set \code{drop = FALSE} so that
the output is consistent.}

\item{FUN}{A function to apply to the \link[=Active]{humdrumR:humdrumR}\verb{field(s) in the}humdrumR` object.}

\item{doplot}{Boolean. If \code{TRUE} the \code{FUN} argument is treated
as a \code{doplot} expression by \verb{[humdrumR:with-in-Humdrum][with(in)Humdrum]},
so the result is ignored (for plotting or side-effects purposes).}
}
\value{
From \code{withinHumdrum} and \code{inHumdrum}, a new humdrumR data object.
From \code{withHumdrum}, whatever value is returned by expression.
}
\description{
Apply arbitrary expressions to fields within \verb{[S4class][humdrumRclass]} data.
}
\section{Overview}{

These functions are the primary means of working with
humdrumR data. (They are analogous to the base functions
\verb{[base][with]} and \verb{[base][within]}
as applied to \verb{[base:data.frame][data.frames]}.)
Specifically they allow you to evaluate arbitrary
expressions involving fields in a humdrumR data object.
They also includes a number of special evaluation options:
\itemize{
\item Evaluate an expression in only matching parts of the data.
\item Evaluate an expression separately in subsets of the data.
\item Evaluate an expression across windows in the data (e.g., ngrams, rolling windows).
\item Evaluate an expression which produces a plot, with particular plotting parameters set using \verb{[graphics][par]}.
}

The difference between \code{withHumdrum} and \code{withinHumdrum} is
analogous to the difference between \verb{[base][with]} and \verb{[base][within]}.
\code{withHumdrum} evaluates your expression(s) and then simply returns the result of
the evaluation. \code{withinHumdrum} evaluates your expression(s) and then
(attempts) to insert the results back into the humdrumR object, generating new
fields called \code{PipeX} (see details).

\code{inHumdrum} is simply a short hand for \code{withinHumdrum}.
}

\section{\code{Formulae}}{

Every formula in the \code{formulae} argument
is treated as a \code{Keyword ~ Expression(s)}
pairing. Multiple expressions can be input using multiple \code{~} operators:
\code{Keyword ~ Expression1 [~ Expression2 ~ ... ~ ExpressionN]}
(the leftmost expression is treated as the keyword.)
If there is no leftmost expression (i.e., \code{~ Expression}), the Keyword
defaults to "\code{do}." The keyword expression must be a single, simple name/symbol, following
standard R rules (i.e., "\code{.foobar}" is acceptable but "\code{3 + foobar}" is not).

Legal keywords, and their meanings are:
\enumerate{
\item \code{do} An expression to be evaluated within the \code{humdrumR} data object (see "Expression evaluation").
\item \code{doplot} An expression to be evaluated within the \code{humdrumR} data object while ignoring the result of the expression (see "Expression evaluation" and "Plotting".
\item \code{by} An expression used to break the data into groups, with the \code{do} expression(s) evaluated
separately in each group (see "Partitioning").
\item \code{where} An expression indicating a subset of the data in which to evaluate the \code{do} expression (see "Partitioning").
\item \code{ngrams} A positive number \emph{n}. The expression is evaluated across overlapping length-\emph{n} windows.
\item \code{recordtypes} A string or vector of characters drawn from \code{c("D", "d", "I", "L", "M","G")}. These characters
correspond to types of humdrum records: \strong{D}ata, null \strong{d}ata, \strong{I}nterpretations,
\strong{M}easures, \strong{L}ocal comments, and \strong{G}lobal comments respectively. The expression
is only evaluated on data drawn from the specified record types (defaults to \code{"D"}).
\item \code{pre} An expression to evaluate once before evaluating the do expression(s). Useful, for instance, for taking logs
or opening a graphing window. The \code{pre} expression is evaluated in the global environment.
\item \code{post} An expression evaluate once after evaluating the do expression(s). Always evaluated in the global environment.
}
}

\section{Expression evaluation}{


The right-hand side of any formula in the \code{formulae} argument with the keyword \code{do} or \code{doplot}
(or with no keyword specified) is evaluated within the \code{humdrumR} data object.
The expression can, thus, refer to any field in the humdrumR object (Record, Token, File, etc.).
You can also include a \code{.} anywhere in the expression, which will be
interpreted as the humdrumR object's current \verb{[dest=humdrumR][Active]}
expression.\preformatted{humdata <- readHumdrum('directorywithdata/*.krn') # read some data

withinHumdrum(humdata, ~getPitch(Token)) # The most basic pattern
withinHumdrum(humdata, ~getPitch(.)) # Same as previous (unless `Active` field has been changed))

withinHumdrum(humdata, ~solfa(getPitch(Token), key = Key)) 
# Assumes that the Key field was parsed during the call to `[readHumdrum][readHumdrum]`

withinHumdrum(humdata, ~getSemits(Token) - mean(getSemits(Token))) 

}

If multiple \code{do} expressions are provided, they are each evaluated one at a time,
with the result of each piped into the next. Other, non-\code{do}, formulae (like \verb{by~} or
\verb{ngrams~}) are reused for each expression evaluated.
}

\section{Partitioning}{


A \code{by} expression is used to break the data into subsets, with the \code{do} expression(s) evaluated
separately within each subset. This works the similarly to the \code{by} argument in
\verb{[data.table][data.table]}s, the \code{INDEX}
argument of \verb{[base][tapply]}, or the \code{INDICES} argument of \verb{[base][by]}.
Each \code{by} expression must evaluate, within the \code{humdrumR} data object, to a vector (or a list of vectors
of equal length) of categories to group the data by.
Most commonly, the \code{by} expression(s) are simply field(s) in the data:
for instance,\preformatted{withinHumdrum(humdata,
         do ~ table(Token),
         by ~ File)
}

will apply the function \verb{[base][table]} to the \code{Token} field
\emph{separately} for each file in the \code{humdrumR} data.
However, we can also use more complex expressions like\preformatted{withinHumdrum(humdata,
         do ~ table(Token), 
         by ~ Spine > 3 | Record \\\%\\\% 2 == 0)
}

which will evaluate the do expression in two groups, one where either the spine number is
three or less \emph{or} the record number is even, and another group where the opposite is true.

If the \code{by} expression evaluates to a list of grouping vectors,
the \code{do} expressions are evaulated across every combination of categories in all the vectors.
Thus,
withinHumdrum(humdata,
do ~ table(Token),
by ~ list(File, Spine))
will apply \code{table} to \code{Token} across each spine \emph{in} each file.
As some \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}, if the
\code{by} expression has more than two parts, all parts except
the (leftmost) keyword part are combined in a list (i.e., \code{by ~ File ~ Spine}
becomes \verb{by ~ list(File, Spine)\}}.
Thus the previous example can also be written:
withinHumdrum(humdata,
do ~ table(Token),
by ~ File ~ Spine)

A \code{where} expression is used to identify a subset of the data and evaluate
the \code{do} expression(s) \emph{only} in that subset.
\code{where} expressions must evaluated, within the \code{humdrumR} data object, to
a single logical vector. The \code{do} expression(s) are only evaluated where this logical
vector is \code{TRUE}.
Wherever the \code{where} expression evaluates to \code{FALSE}, the original \code{humdrumR} data is
kept unchanged.

If multiple partitioning formulae (i.e, \code{by} and \code{where}) expressions
are evaluated recursively, in order from left to right. Thus if you specify
withinHumdrum(humdata,
do ~ sd(semits),
by ~ File,
where ~ semits > mean(semits))
a the standard deviation of the \code{semits} field will be calculated only in each file,
but only where the \code{semits} field is greater than the mean \code{semits} value
\emph{within that file}. Contrast this with this call:
withinHumdrum(humdata,
do ~ sd(semits)
where ~ semits > mean(semits),
by ~ File)
wherein the standard deviation of \code{semits} is, again, calculated for each file,
but this time wherever the \code{semits} field is greater than the mean value \emph{across all the data}.
}

\section{Plotting}{

The \code{doplot} keyword behaves exactly like the \code{do} keyword, except that the result of the
evaluation is ignored. This is useful for plotting \emph{as well as} other side-effects (like writing to a file).
If \code{doplot} is used with \code{withHumdrum}, the function simply returns \code{NULL} (after executing the \code{doplot}
expression
If \code{doplot} is used with \code{withinHumdrum} (or \code{inHumdrum}), the function simply returns the unaltered
\code{humdrumR} argument.

\code{withinHumdrum} also allows you to specify plotting options in line, without having to make a separate call
to \verb{[graphics][par]}. Any \verb{[graphics][par]} argument can be specified as a \code{Keyword ~ Expression} pair
in the \code{formulae} argument. For instance, if you call a \code{doplot} expression with a \code{by} expression
that creates four groups, R will create four plots---but you will only see the last one! Normally, you would need to
call \code{par(mfcol = c(2,2))} \emph{before} calling your plotting function. However, with \code{withinHumdrum} you can
soecific \code{mfcol = c(2,2)} right in a \code{formulae} formula:
withinHumdrum(humdata,
doplot ~ fooplot(.),
by ~ list(Two, byTwo),
mfcol ~ c(2, 2))
The best part is \code{withinHumdrum} will reset \code{par} to it's previous state after \code{withinHumdrum} is done.
}

\section{Tandem interpretations}{


The function \verb{[readHumdrum][readHumdrum]} automatically parses
tandem interpretations (that it recognizes) into
their own fields in the resulting \verb{[humdrumR][humdrumRclass]} data.
For instance, data with a \code{'*clefF4'} will show
up as a \code{Clef} field. However, users might read humdrum data with their
own custom tandem interpretations that are not built into \code{humdrumR}.
\code{humdrumR} includes the function \verb{[getTandem][getTandem]} to help us
extract arbitrary tandem intrpretation data.
Luckily, \code{withinHumdrum} knows some
\href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
which makes it easy to do this anywhere in our expressions, simply by putting a
named object beginning with the symbol \code{*}. Of course, R doesn't normally
allow names to begin with symbols like \code{*}, but you can force it by
placing grave symbols around the name \verb{*name}. If you do this in a \code{withinHumdrum}
expression, \code{withinHumdrum} will treat this name as a
regular expression and substitute a call \code{getTandem(Tandem, 'regular expression')} in the expression.
This means you can could do something like
withinHumdrum(humdata,
do ~ myFunction(Token, \verb{*mytandempattern}))
and \code{myFunction} will be called with the first argument being the
\code{Token} field, and the second argument being tandem interpretations
which match \code{'mytandempattern'} (extracted from the \code{Tandem} field).
}

\section{Splatting}{


("Splatting" refers to feeding a function a list/vector of arguments.)
Sometimes we want to divide our data into pieces (a l\'a \code{partition} option), but
rather than applying the same expression to each piece, we want to feed
the separate pieces as separate arguments to the same function.
In \code{withinHumdrum} you can use some
\href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
to do just this, using the \code{@} symbol in the format \code{myFunction(TargetExpr@GroupingExpr)}.
If we make this call

withinHumdrum(humdata,
do ~ myFunction(Token@Spine))

and there are four spines
this is how \code{withinHumdrum} will intepret the expression:

withinHumdrum(humData,
do ~ myFunction(Token\link{Spine == 1}, # first argument when Spine == 1
Token\link{Spine == 2}, # second argument when Spine == 2
Token\link{Spine == 3}, # etc.
Token\link{Spine == 4}))
}

\section{Argument interpolation}{


Any named arguments to \code{withinHumdrum} are \verb{[humdrumR:interpolateArguments][interpolated]} into the
\code{do} expressions. This is useful if you've already created a list of formulas that you like, but would like
to make small changes to a function call within the \code{do} expressions, without starting from scratch.
Examples:\preformatted{mycommand <- c(do ~ mean(., na.rm = TRUE), by ~ Spine ~ File)
withinHumdrum(humdata,
              mycommand,
              na.rm = FALSE)
# mycommand is executed with na.rm changed to FALSE              
}
}

\section{Piping}{


For calls to \code{withinHumdrum}, the result of each \code{do} expression
is insterted back into the \verb{[humtable][humdrum table]}. The results
are put into new field(s) labeled Pipe1, PipeX, ..., PipeN. If the results
of the expression are shorter than the rows in the \link[=humdrum table]{humtable},
or an \code{object}, the humdrum table is shrunk to fit them.
}

\examples{
humdata <- readHumdrum('directorywithdata/*.krn')

withinHumdrum(humdata, ~nchar(.)) # counts characters in each data token.
withinHumdrum(humdata, ~table(.), by ~ Spine) # Tabulates data tokens in each Spine.

}