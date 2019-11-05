# This file defines the functions withHumdrum and withinHumdrum, which are used to apply
# expressions to fields in a humdrumR data object in a manner analogous to the base
# with and within functions (as applied to data.frames). There is also the function humApply
# which acts more like R lapply, but is really just using withinHumdrum.
#
# humApply, withHumdrum, withinHumdrum, and inHumdrum (an alias of withinHumdrum) 
# are each exported for users to use.
# getTandem is also exported.
# All other functions are just used by with/withinHumdrum.
#
# withinHumdrum has two major parts to it:
#   1: Applying the desired expression to the humdrumR object,
#   with various specical options specified by the desired arguments (e.g., partition, ngram).
#   2: Taking the results of the expression and (re)assembling/inserting
#   it into the humdrumR object. This second part, is primarily
#   accomplished by the function putHumtab. 
# withHumdrum skips the second step!

#################################################-
############################ withinHumdrum ----
##################################################-



#' with(in)Humdrum
#' 
#' Apply arbitrary expressions to fields within \code{\linkS4class{humdrumR}} data.
#' 
#' @section Overview:
#' These functions are the primary means of working with
#' humdrumR data. (They are analogous to the base functions
#' \code{\link[base]{with}} and \code{\link[base]{within}} 
          #' as applied to \code{\link[base:data.frame]{data.frames}}.)
#' Specifically they allow you to evaluate arbitrary
#' expressions involving fields in a humdrumR data object.
#' They also includes a number of special evaluation options:
#' \itemize{
#'   \item Evaluate an expression in only matching parts of the data.
#'   \item Evaluate an expression separately in subsets of the data.
#'   \item Evaluate an expression across windows in the data (e.g., ngrams, rolling windows).
#'   \item Evaluate an expression which produces a plot, with particular plotting parameters set using \code{\link[graphics]{par}}.
#' }
#' 
#' The difference between \code{withHumdrum} and \code{withinHumdrum} is
#' analogous to the difference between \code{\link[base]{with}} and \code{\link[base]{within}}.
#' \code{withHumdrum} evaluates your expression(s) and then simply returns the result of
#' the evaluation. \code{withinHumdrum} evaluates your expression(s) and then
#' (attempts) to insert the results back into the humdrumR object, generating new
#' fields called \code{PipeX} (see details).
#' 
#' \code{inHumdrum} is simply a short hand for \code{withinHumdrum}.
#' 
#' @section \code{Formulae}:
#' Every formula in the \code{formulae} argument 
#' is treated as a \code{Keyword ~ Expression(s)}
#' pairing. Multiple expressions can be input using multiple \code{~} operators:
#' \preformatted{Keyword ~ Expression1 [~ Expression2 ~ ... ~ ExpressionN]}
#' (the leftmost expression is treated as the keyword.)
#' If there is no leftmost expression (i.e., \code{~ Expression}), the Keyword
#' defaults to "\code{do}." The keyword expression must be a single, simple name/symbol, following 
#' standard R rules (i.e., "\code{.foobar}" is acceptable but "\code{3 + foobar}" is not).
#' 
#' Legal keywords, and their meanings are:
#' \describe{
#'   \item{do}{An expression to be evaluated within the \code{humdrumR} data object (see "Expression evaluation").}
#'   \item{doplot}{An expression to be evaluated within the \code{humdrumR} data object while ignoring the result of the expression (see "Expression evaluation" and "Plotting".}
#'   \item{by}{An expression used to break the data into groups, with the \code{do} expression(s) evaluated 
#'   separately in each group (see "Partitioning").}
#'   \item{where}{An expression indicating a subset of the data in which to evaluate the \code{do} expression (see "Partitioning").}
#'   \item{ngrams}{A positive number \emph{n}. The expression is evaluated across overlapping length-\emph{n} windows.}
#'   \item{recordtype}{A string or vector of characters drawn from \code{c("D", "d", "I", "L", "M","G")}. These characters
#'   correspond to types of humdrum records: \strong{D}ata, null \strong{d}ata, \strong{I}nterpretations, 
#'   \strong{M}easures, \strong{L}ocal comments, and \strong{G}lobal comments respectively. The expression
#'   is only evaluated on data drawn from the specified record types (defaults to \code{"D"}).}
#' }
#' 
#' @section Expression evaluation:
#' The right-hand side of any formula in the \code{formulae} argument with the keyword \code{do} or \code{doplot} 
#' (or with no keyword specified) is evaluated within the \code{humdrumR} data object.
#' The expression can, thus, refer to any field in the humdrumR object (Record, Token, File, etc.). 
#' You can also include a \code{.} anywhere in the expression, which will be 
#' interpreted as the humdrumR object's current \code{\link[dest=humdrumR]{Active}} 
#' expression.
#' \preformatted{
#' humdata <- readHumdrum('directorywithdata/*.krn') # read some data
#' 
#' withinHumdrum(humdata, ~getPitch(Token)) # The most basic pattern
#' withinHumdrum(humdata, ~getPitch(.)) # Same as previous (unless \code{Active} field has been changed))
#' 
#' withinHumdrum(humdata, ~solfa(getPitch(Token), key = Key)) 
#' # Assumes that the Key field was parsed during the call to \code{\link{readHumdrum}}
#' 
#' withinHumdrum(humdata, ~getSemits(Token) - mean(getSemits(Token))) 
#' }
#' 
#' If multiple \code{do} expressions are provided, they are each evaluated one at a time,
#' with the result of each piped into the next. Other, non-\code{do}, formulae (like \code{by~} or 
#' \code{ngrams~}) are reused for each expression evaluated.

#' 
#' @section Partitioning:
#' 
#' A \code{by} expression is used to break the data into subsets, with the \code{do} expression(s) evaluated 
#' separately within each subset. This works the similarly to the \code{by} argument in 
#' \code{\link[data.table]{data.table}}s, the \code{INDEX} 
#' argument of \code{\link[base]{tapply}}, or the \code{INDICES} argument of \code{\link[base]{by}}.
#' Each \code{by} expression must evaluate, within the \code{humdrumR} data object, to a vector (or a list of vectors 
#' of equal length) of categories to group the data by.
#' Most commonly, the \code{by} expression(s) are simply field(s) in the data: 
#' for instance, 
#' \preformatted{withinHumdrum(humdata,
#'                        do ~ table(Token),
#'                        by ~ File)} 
#' will apply the function \code{\link[base]{table}} to the \code{Token} field
#' \emph{separately} for each file in the \code{humdrumR} data. 
#' However, we can also use more complex expressions like
#' \preformatted{withinHumdrum(humdata,
#'                        do ~ table(Token), 
#'                        by ~ Spine > 3 | Record \%\% 2 == 0}
#' which will evaluate the do expression in two groups, one where either the spine number is 
#' three or less \emph{or} the record number is even, and another group where the opposite is true. 
#' 
#' If the \code{by} expression evaluates to a list of grouping vectors,
#' the \code{do} expressions are evaulated across every combination of categories in all the vectors.
#' Thus,
#' \preformatted{withinHumdrum(humdata, 
#'                        do ~ table(Token),
#'                        by ~ list(File, Spine))}
#' will apply \code{table} to \code{Token} across each spine \emph{in} each file.
#' As some \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}, if the 
#' \code{by} expression has more than two parts, all parts except 
#' the (leftmost) keyword part are combined in a list (i.e., \code{by ~ File ~ Spine} 
#' becomes \code{by ~ list(File, Spine)}).
#' Thus the previous example can also be written:
#' \preformatted{withinHumdrum(humdata, 
#'                        do ~ table(Token),
#'                        by ~ File ~ Spine)}
#'                      
#' A \code{where} expression is used to identify a subset of the data and evaluate
#' the \code{do} expression(s) \emph{only} in that subset. 
#' \code{where} expressions must evaluated, within the \code{humdrumR} data object, to
#' a single logical vector. The \code{do} expression(s) are only evaluated where this logical
#' vector is \code{TRUE}.
#' Wherever the \code{where} expression evaluates to \code{FALSE}, the original \code{humdrumR} data is 
#' kept unchanged.
#' 
#' If multiple partitioning formulae (i.e, \code{by} and \code{where}) expressions
#' are evaluated recursively, in order from left to right. Thus if you specify
#' \preformatted{withinHumdrum(humdata,
#'                        do ~ sd(semits),
#'                        by ~ File, 
#'                        where ~ semits > mean(semits))}
#' a the standard deviation of the \code{semits} field will be calculated only in each file,
#' but only where the \code{semits} field is greater than the mean \code{semits} value
#' \emph{within that file}. Contrast this with this call:
#' \preformatted{withinHumdrum(humdata,
#'                        do ~ sd(semits)
#'                        where ~ semits > mean(semits), 
#'                        by ~ File)} 
#' wherein the standard deviation of \code{semits} is, again, calculated for each file,
#' but this time wherever the \code{semits} field is greater than the mean value \emph{across all the data}.
#' 
#' @section Plotting:
#' The \code{doplot} keyword behaves exactly like the \code{do} keyword, except that the result of the
#' evaluation is ignored. This is useful for plotting \emph{as well as} other side-effects (like writing to a file).
#' If \code{doplot} is used with \code{withHumdrum}, the function simply returns \code{NULL} (after executing the \code{doplot}
#' expression
#' If \code{doplot} is used with \code{withinHumdrum} (or \code{inHumdrum}), the function simply returns the unaltered
#' \code{humdrumR} argument.
#' 
#' \code{withinHumdrum} also allows you to specify plotting options in line, without having to make a separate call
#' to \code{\link[graphics]{par}}. Any \code{\link[graphics]{par}} argument can be specified as a \code{Keyword ~ Expression} pair
#' in the \code{formulae} argument. For instance, if you call a \code{doplot} expression with a \code{by} expression
#' that creates four groups, R will create four plots---but you will only see the last one! Normally, you would need to
#' call \code{par(mfcol = c(2,2))} \emph{before} calling your plotting function. However, with \code{withinHumdrum} you can
#' soecific \code{mfcol = c(2,2)} right in a \code{formulae} formula:
#' \preformatted{
#'              withinHumdrum(humdata,
#'                       doplot ~ fooplot(.),
#'                       by ~ list(Two, byTwo),
#'                       mfcol ~ c(2, 2))}
#' The best part is \code{withinHumdrum} will reset \code{par} to it's previous state after \code{withinHumdrum} is done.
#' 
#' 
#' @section Tandem interpretations:
#' The function \code{\link{readHumdrum}} automatically parses
#' tandem interpretations (that it recognizes) into
#' their own fields in the resulting \code{\linkS4class{humdrumR}} data.
#' For instance, data with a \code{'*clefF4'} will show
#' up as a \code{Clef} field. However, users might read humdrum data with their
#' own custom tandem interpretations that are not built into \code{humdrumR}.
#' \code{humdrumR} includes the function \code{\link{getTandem}} to help us
#' extract arbitrary tandem intrpretation data.
#' Luckily, \code{withinHumdrum} knows some
#'  \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
#' which makes it easy to do this anywhere in our expressions, simply by putting a 
#' named object beginning with the symbol \code{*}. Of course, R doesn't normally 
#' allow names to begin with symbols like \code{*}, but you can force it by
#' placing grave symbols around the name \code{`*name`}. If you do this in a \code{withinHumdrum}
#' expression, \code{withinHumdrum} will treat this name as a 
#' regular expression and substitute a call \code{getTandem(Tandem, 'regular expression')} in the expression.
#' This means you can could do something like 
#' \preformatted{withinHumdrum(humdata, 
#'                        do ~ myFunction(Token, `*mytandempattern`))}
#' and \code{myFunction} will be called with the first argument being the 
#' \code{Token} field, and the second argument being tandem interpretations
#' which match \code{'mytandempattern'} (extracted from the \code{Tandem} field).
#' 
#' @section Splatting:
#' ("Splatting" refers to feeding a function a list/vector of arguments.)
#' Sometimes we want to divide our data into pieces (a l\'a \code{partition} option), but
#' rather than applying the same expression to each piece, we want to feed
#' the separate pieces as separate arguments to the same function.
#' In \code{withinHumdrum} you can use some 
#' \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
#' to do just this, using the \code{@} symbol in the format \code{myFunction(TargetExpr@GroupingExpr)}.
#' If we make this call
#' \preformatted{
#' withinHumdrum(humdata, 
#'          do ~ myFunction(Token@Spine))
#' }
#' and there are four spines
#' this is how \code{withinHumdrum} will intepret the expression:
#' \preformatted{
#' withinHumdrum(humData,
#'          do ~ myFunction(Token[Spine == 1], # first argument when Spine == 1
#'                          Token[Spine == 2], # second argument when Spine == 2
#'                          Token[Spine == 3], # etc.
#'                          Token[Spine == 4])) 
#' }
#' 
#' @section Argument interpolation:
#' Any named arguments to \code{withinHumdrum} are \code{\link[humdrumR:interpolateNamedValues]{interpolated}} into the
#' \code{do} expressions. This is useful if you've already created a list of formulas that like, but would like
#' to make small changes to a function call within the \code{do} expressions, without starting from scratch.
#' Examples:
#' \preformatted{
#' mycommand <- c(do ~ mean(., na.rm = TRUE), by ~ Spine ~ File)
#' withinHumdrum(humdata,
#'               mycommand,
#'               na.rm = FALSE)
#' # mycommand is executed with na.rm changed to FALSE              
#' }
#' 
#' @section Piping:
#' For calls to \code{withinHumdrum}, the result of each \code{do} expression
#' is insterted back into the \code{\link[humtable]{humdrum table}}. The results
#' are put into new field(s) labeled Pipe1, PipeX, ..., PipeN. If the results
#' of the expression are shorter than the rows in the \link[humtable]{humdrum table},
#' or an \code{object}, the humdrum table is shrunk to fit them.
#'     
#' @param humdrumR A \code{\linkS4class{humdrumR}} data object.
#' @param ...  \code{...} arguments to \code{withinHumdrum} are divided into either named or unnamed arguments.
#' Unnamed arguments must be formulas, functions---lists of formulas/functions, no matter how deeply nested, are flattened
#' to a single list of functions/formulas.
#' All functions are coerced to a formula as \code{~foo(.)}. The far left-hand side of each formula
#' must be a name/symbol. Named arguments are \link[humdrumR:interpolateNamedValues]{interpolated} into and \code{do~X} formulas.

#' 
#' @param ... Additional formulas/functions, or lists of formulas/functions.
#' These are all simply appended to the \code{formulae} argument.
#' 
#' @examples 
#' humdata <- readHumdrum('directorywithdata/*.krn')
#' 
#' withinHumdrum(humdata, ~nchar(.)) # counts characters in each data token.
#' withinHumdrum(humdata, ~table(.), by ~ Spine) # Tabulates data tokens in each Spine.
#' 
#' @return From \code{withinHumdrum} and \code{inHumdrum}, a new humdrumR data object.
#' From \code{withHumdrum}, whatever value is returned by expression.
#' 
#' @name with-in-Humdrum
NULL

#' withinHumdrum
#'
#' @name with-in-Humdrum
#' @export
withinHumdrum <- function(humdrumR,  ...) {
          checkhumdrumR(humdrumR, 'withinHumdrum')
          
          #### Preprocessing ... argument
          elips <- list(...)
          formulae <- if (is.null(names(elips))) elips else unlist(elips[names(elips) == ''])
          namedArgs <- elips[names(elips) != '']
          
          # turn  any functions into formula
          formulae <- anyfuncs2forms(formulae, parent.env(environment()))
          if (any(!sapply(formulae, rlang::is_formula))) stop('In withinHumdrum(...) unnamed arguments must be formulas or functions.')
          
          #### Processing formulae list
          #parseKeywords returns a list with the following names:
          # graphics, recordtypes, doexpressions, ngrams, partitions
          # also WhichAreDo (a logical vector)
          parsedFormulae <- parseKeywords(formulae) 
          if (length(parsedFormulae$doexpressions) == 0L) {
                  warning(call. = FALSE,
                       "No do-expression in call to withinHumdrum, humdrumR object is returned unchanged.")
                 return(humdrumR)
          }
          
          ### graphical options
          if (length(parsedFormulae$graphics) > 0) {
                    oldpar <- par(no.readonly = TRUE)
                    on.exit(par(oldpar))
                    
                    rlang::eval_tidy(rlang::quo(par(!!!parsedFormulae$graphics)))
          }
          
          #### Preprocess humdrumR and humtab
          
          # Getting the humtab with the right record types.
          humtab <- rlang::eval_tidy(rlang::quo(getHumtab(humdrumR, !!parsedFormulae$recordtypes)))
          
          #### Create main expression
          currentdo <- parsedFormulae$doexpressions[[1]]
          currentdo <- interpolateNamedValues(currentdo, namedArgs)
          
          funcQuosure <- prepareForm(humtab, currentdo, humdrumR@Active, parsedFormulae$ngrams)
          
          sideeffectonly <- grepl('p', names(parsedFormulae$doexpressions)[1]) # do~ names with 'p' as in 'doplot'
          humtabFunc  <- humtableApplier(funcQuosure, 
                                         captureOutput = !sideeffectonly,
                                         reHumtab = TRUE)
          
          #### evaluate expression 
          
          newhumtab <- if (length(parsedFormulae$partitions) == 0) {
                    humtabFunc(humtab)
          } else {
                    partApply(humtab, parsedFormulae$partitions, humtabFunc)
          }
          
          if (!sideeffectonly) {
              #### Put new humtable back into humdrumR object
              
              newfields <- colnames(newhumtab)[!colnames(newhumtab) %in% colnames(humtab)]
              
              # This section should be improved (Nat, 07/16/2019)
              local({
                  d    <- newhumtab$Type == 'd'
                  null <- Reduce('&', lapply(newhumtab[ , newfields, with = FALSE],
                                             function(col) is.na(col) | col == '.'))

                  newhumtab[ , Null:= Null & d & null]
                  # newhumtab[ , Type:= IfElse(Null, Type, 'D')]
              })
              
              if (any(is.na(newhumtab$Type))) {
                  newtype <- if ('D' %in% parsedFormulae$recordtypes) 'D' else parsedFormulae$recordtypes[1]
                  newhumtab$Type[is.na(newhumtab$Type)] <- newtype
              }
              putHumtab(humdrumR, drop = TRUE) <- newhumtab
              ########### Update other slots in humdrumR object
              
              # Now that the Humtable is taken care of,
              # tell the humdrumR object about the new fields and set the Active formula.
              if (length(newfields) > 0) {
                  ## Add fields to humtab
                  addFields(humdrumR) <- newfields
                  
                  ## Create new Active quosure
                  humdrumR <- if (any(names(parsedFormulae$partitions) == 'where')) {
                      act <- ifelsecalls(parsedFormulae$partitions['where'], 
                                         c(humdrumR@Active, lapply(newfields, 
                                                                   function(nf) rlang::as_quosure(as.symbol(nf), environment(humdrumR)))))
                      putActive(humdrumR, act)
                  } else {
                      setActiveFields(humdrumR, newfields) 
                  }
              }
              # humdrumR <- indexGLIM(humdrumR)
              
          }
          
          if (length(parsedFormulae$doexpressions) > 1) {
                    do.call('Recall', c(humdrumR,  
                                        formulae[-parsedFormulae$WhichAreDo[1]], #remove first do expression and Recall
                                        namedArgs))
          } else {
                    humdrumR 
                    
          }
}
          
          


#' withHumdrum
#'
#' @name with-in-Humdrum
#' @export
withHumdrum <- function(humdrumR,  ...) {
          checkhumdrumR(humdrumR, 'withHumdrum')
          
          #### Preprocessing ... argument
          elips <- list(...)
          formulae <- if (is.null(names(elips))) elips else unlist(elips[names(elips) == ''])
          namedArgs <- elips[names(elips) != '']
          
          # turn  any functions into formula
          formulae <- anyfuncs2forms(formulae, parent.env(environment()))
          if (any(!sapply(formulae, rlang::is_formula))) stop('In withHumdrum(...) unnamed arguments must be formulas or functions.')
         
          #### Processing formulae list
          #parseKeywords returns a list with the following names:
          # graphics, recordtypes, doexpressions, ngrams, partitions
          # also WhichAreDo (a logical vector)
          parsedFormulae <- parseKeywords(formulae) 
          if (length(parsedFormulae$doexpressions) == 0L) {
              warning(call. = FALSE,
                      "No do-expression in call to withHumdrum, humdrumR object is returned unchanged.")
              return(humdrumR)
          }
          
          # graphical options
          if (length(parsedFormulae$graphics) > 0) {
                    oldpar <- par(no.readonly = TRUE)
                    on.exit(par(oldpar))
                    
                    rlang::eval_tidy(rlang::quo(par(!!!parsedFormulae$graphics)))
          }
          
          #### Preprocess humdrumR and humtab
          # withHumdrum can't handle multiple do expressions, because it doesn't return a
          # humdrumR object, it can't pipe the output of one do expression to the next.
          # Instead, if there are more than one do expressions, we call all but the last do expression
          # using withinHumdrum (which does pipe one do to the next) and only call withHumdrum on the last do expression.
          # The call to withinHumdrum is identical to the call to withHumdrum, except missing the initial do expressions.
          if (length(parsedFormulae$doexpressions) > 1) {
                    
                    formulaeSubset <- formulae[head(parsedFormulae$WhichAreDo, -1)]  #call all but last doexpression
                    humdrumR <- do.call('withinHumdrum', c(humdrumR,
                                                           formulaeSubset,
                                                           namedArgs))
                    parsedFormulae$doexpressions <- tail(parsedFormulae$doexpressions, 1)       
          } 
          
          # Getting the humtab with the right record types.
          humtab <- rlang::eval_tidy(rlang::quo(getHumtab(humdrumR, !!parsedFormulae$recordtypes)))
          
          
          #### Create main expression
          currentdo <- parsedFormulae$doexpressions[[1]]
          currentdo <- interpolateNamedValues(currentdo, namedArgs)

          funcQuosure <- prepareForm(humtab, currentdo, humdrumR@Active, parsedFormulae$ngrams)

          humtabFunc  <- humtableApplier(funcQuosure, 
                                         captureOutput = !grepl('p', names(parsedFormulae$doexpressions)[1]),
                                         reHumtab = FALSE)
          
          #### evaluate expression 
          
          output <- if (length(parsedFormulae$partitions) == 0) {
                    humtabFunc(humtab)
          } else {
                    if (length(parsedFormulae$partitions) > 1) {
                              # If there are more than one partitions, need to run the initial ones (all but last)
                              # first with reHumtab output = TRUE. Only with last partition do we drop the humdrumR
                              # data format.
                              humtabFunc_pre  <- humtableApplier(funcQuosure, 
                                                                 captureOutput = !grepl('p', names(doexpressions)[1]),
                                                                 reHumtab = TRUE)
                              humtab <- partApply(humtab, head(parsedFormulae$partitions, -1), humtabFunc_pre)
                              
                    } 
                    partApply(humtab, tail(parsedFormulae$partitions, 1), humtabFunc)
                              
          }
          
          if (is.list(output) && length(output) == 1) output <- output[[1]]
          
          output
}
          
#' inHumdrum
#' @name with-in-Humdrum
#' @export
inHumdrum <- withinHumdrum
          

anyfuncs2forms <- function(fs, parentenv) {
          areFuncs <- sapply(fs, is.function)
          
          fs[areFuncs] <- lapply(fs[areFuncs],
                                 function(func) {
                                           expenv <- list2env(list(.func = func))
                                           parent.env(expenv) <- parentenv
                                           formula <- ~ .func(.)
                                           rlang::f_env(formula) <- expenv
                                           formula
                                 }) 
          fs
}

parseKeywords <- function(formulae) {
 formulargs <- parseArgFormulae(formulae) # output is named list of quosures
 formulargnames <- names(formulargs)
 
 #
 knownKeywords <- list(doexpressions   = c('d', 'do', 'dop', 'dopl', 'doplo', 'doplot'),
                       graphics        = names(par()),
                       ngrams          = c('ngrams', 'ngram', 'ngra', 'ngr', 'ng'),
                       partitions      = c('by', 'where', 'groupby', 'group_by'),
                       recordtypes     = c('recordtype', 'recordtypes'),
                       windows         = c('windows', 'window'))

 boolean <- lapply(knownKeywords, `%in%`, x = formulargnames)
 
 if (!any(boolean$doexpressions)) stop(call. = FALSE,
                                   "The with(in)Humdrum formulae argument doesn't include any do[plot] expressions to apply to the data!
                                   These expressions should be either in a formula with no left hand side (~Expr) or with 'do' or 'doplot' on the left hand side (do~Expr, doplot~Expr).")
 
 values  <- lapply(boolean, function(b) formulargs[b])
 values$WhichAreDo <- which(boolean$doexpressions)
 
 # These keywords must always have exactly one evaluated value.
 #        If there are more than one, take the first.
 #        If there are none, some categories have defaults
 values$ngrams      <- if (length(values$ngrams)      == 0) NULL else values$ngrams[[1]]
 values$recordtypes <- if (length(values$recordtypes) == 0) rlang::quo('D') else values$recordtypes[[1]]
 
 # Other keywords (e.g., windows and partitions) can be of any length, including 0
 
 values
}




parseArgFormulae <- function(l) {
          # This function parsed formula into [Keyword ~ Expression] pairs.
          # It takes a list of formulas and applies parseArgFormula.
          # Returns a named list of rlang::quosures.
          names(l) <- NULL
          unlist(lapply(l, parseArgFormula), recursive = FALSE)
}

parseArgFormula <- function(form) {
          # This function parsed formula into [Keyword ~ Expression] pairs.
          # This function takes a formula formatted
          # Keyword ~ expr1 [~ expr2 ~ expr3 ...] and translates it to a list
          # like list(Keyword = ~ c(expr1[, expr2, expr3, ...])).
          # If the keyword is missing (there is no left side of formula), it defaults to "by".
          # The function takes the input formula and turns it into a rlang::quosure.
          exprs <- splitFormula(form) # divides formula into list of expressions
          
          if (grepl('^~', deparse(form)[1])) { 
                    keyword <- quote(do) # if keyword is empty, default to 'do'
          } else {
                    keyword <- exprs[[1]]
                    exprs <- exprs[-1]
          }
          
          # check that keyword is actually a single legal R name, not an expression
          if (length(keyword) != 1L || !is.name(keyword)) stop("Your formula (in a call to withinHumdrum) contains a 
                                                  keyword (far left side of formula) which can't be parsed as a name.")
          
          # collapse to one expression
          expr <- if (length(exprs) == 1) exprs[[1]] else  rlang::expr(list(!!!exprs))
          quosure <- rlang::new_quosure(expr, env = rlang::f_env(form))
          
          setNames(list(quosure), as.character(keyword))
}


splitFormula <- function(form) {
          # Takes a formula which contains one or more formula,
          # and separates each expression, as if the ~ symbol is a boundary.
          if (length(form) == 1 || deparse(form[[1]]) != '~') return(form)      
          
          if (!rlang::is_formula(form)) form <- eval(form)
          
          lhs <- Recall(rlang::f_lhs(form))
          rhs <- Recall(rlang::f_rhs(form))
          
          c(unlist(lhs), unlist(rhs))
}



###########- Applying withinHumdrum's expression to a data.table

humtableApplier <- function(funcquo, captureOutput = TRUE, reHumtab = TRUE) {
          #' This function operates within withinHumdrum.
          #' It takes a quosure created by prepareForm 
          #' and turns it into a function which can be applied 
          #' to a Humtable (i.e., a data.table). 
          #' It's second argument determines if the result of the 
          #' expression application is put back into the Humtable.  
          
          function(humtab) {
                    output <- rlang::eval_tidy(funcquo, data = humtab)
                    if (!captureOutput) return(humtab)
                    if (reHumtab) {
                              pipeIn(humtab) <- output
                              humtab
                    } else {
                              output
                    }
          }
}


####################- Parsing expressions fed to withinHumdrum


prepareForm <- function(humtab, funcQuosure, active, ngram = NULL) {
  #' This is the main function used by \code{\link{withinHumdrum}} to prepare the current
  #' do expression argument for application to a \code{\linkS4class{humdrumR}} object.
  
  # turn . to active formula
  funcQuosure <- activateForm(funcQuosure, active)
  
  # tandem interpretations
  funcQuosure <- tandemsForm(funcQuosure)
  
  # splats
  funcQuosure <- splatForm(funcQuosure)
  
  # find what fields (if any) are used in formula
  usedInExpr <- unique(fieldsInExpr(humtab, funcQuosure))
  
  if (length(usedInExpr) == 0L) stop("The do expression in your call to withinHumdrum doesn't reference any fields in your humdrum data.
                                        Add a field somewhere or add a dot (.), which will automatically grab the default, 'Active' expression.",
                                        call. = FALSE)

  # if the targets are lists, Map
  lists <- vapply(humtab[1 , usedInExpr, with = FALSE], class, FUN.VALUE = character(1)) == 'list' 
  if (any(lists)) funcQuosure <- mapifyForm(funcQuosure, usedInExpr, depth = 1L)
    
  # if ngram is present
  if (!is.null(ngram) && rlang::eval_tidy(ngram) > 1L) {
            funcQuosure <- ngramifyForm(funcQuosure, 
                                        ngram, usedInExpr, 
                                        depth = 1L + any(lists))
  }
  
  funcQuosure
}

####################### Functions used inside prepareForm

activateForm <- function(funcQuosure, active) {
  #' This function takes the \code{expression} argument
  #' from the parent \code{\link{withinHumdrum}} call and 
  #' inserts the \code{Active} expression from the 
  #' target \code{\linkS4class{humdrumR}} object in place 
  #' of any \code{.} subexpressions.
  active <- rlang::f_rhs(active)
  substituteName(funcQuosure, list(. = active))
}

#### Interpretations in expressions

tandemsForm <- function(funcQuosure) {
 # This function inserts calls to getTandem
 # into an expression, using any length == 1 subexpression
 # which begins with `*` as a regular expression.
 # If input is a quosure (it should be), it keeps the quosure intact,
 # (i.e., keeps it's environment).
          
 applyExpr(funcQuosure,
           function(ex) {
             exstr <- deparse(ex)
             interp <- grepl('^\\*[^*]', exstr)
             
             if (interp) {
               regex <- stringr::str_sub(exstr, start = 2L)
               rlang::expr(getTandem(Tandem, !!regex))
             } else {
               ex
             }
           }) 
}


#' Get tandem interpretation information from humdrum data.
#' 
#' The every \code{\linkS4class{humdrumR}} object has a field called
#' \code{Tandem} which is a vector of strings which accumulates
#' tandem interpretations in each Spine. This function (\code{getTandem}) 
#' extracts tandem interpretations from this field, based on a matching
#' regular expression. The obligatory \code{'*'} \emph{does not} need to 
#' be included in the \code{regex}, as it is added automatically. Thus,
#' if you want to find tandem interpretations that match '*clef..', you
#' just have to write \code{regex = 'clef..'}.
#' 
#' @export
getTandem <- function(tandem, regex) {
  # Tandem data in a humdrumR object is stored as a 
  # cummulative list of tandem interpretations separated by commas.
  # This does some toying with tandem and regex to make
  # it so the user doesn't have to think about these commas,
  # but they get taken into account...I'm not sure if it 
  # will always work the way we want.
          
  tandem <- paste0(',', tandem, ',')
  
  regex <- gsub('\\.\\*$', '[^,]*', regex)
  regex <- gsub('\\.$', '[^,]', regex)
  regex <- paste0(',\\*', regex, '[^,]*,')
  
  matches <- stringr::str_extract(tandem, pattern = regex)
  unique(stringr::str_sub(matches, 2L, -2L))
  
}


#### Splatting in expressions
# "splatting" refers to giving a function which takes
# multiple arguments a list or vector which is interpreted
# as a list/vector of arguments, rather than a single argument.
# For instance, the function log takes two arguments, x and base.
# Imagine that I have a vector like this 
# myvector <- list(x = 3, base = 2), which
# I want to apply log to. The traditional R way is 
# do.call('log', as.list(myvector)).
# This is essentially splatting. What I want is syntactic sugar to
# go log(myvector) and have it work. splatForm allows exactly this in a 
# call to withinHumdrum. We can group one field by another field, then
# feed each group as an argument to a function.
#

splatForm <- function(funcQuosure) {
  #' This function takes an expression,
  #' and replaces any subexpression of the form \code{funccall(TargetExpr@GroupingExpr)},
  #' with \code{do.call('funccall', tapply(TargetExpr, GroupingExpr, c))}.
  #' The result is that \code{TargetExpr} is broken into a list of vectors by the
  #' \code{GroupingExpr}, and each group is fed to \code{funccall} as a separate
  #' argument. See the docementation for \code{\link{withinHumdrum}}.
  #' This does not look for \code{@} sub expression within branches of a \code{@} expression!
  #' 
  if (!is.call(funcQuosure)) return(funcQuosure)
          
  atArgs <- vapply(funcQuosure[-1], function(ex) is.call(ex) && deparse(ex[[1]]) == '@', FUN.VALUE = logical(1))
  
  if (!any(atArgs)) {
           for (i in 2:length(funcQuosure)) funcQuosure[[i]] <- Recall(funcQuosure[[i]]) 
           funcQuosure
  } else {
           for (i in (1L + which(atArgs))) funcQuosure[[i]] <- parseAt(funcQuosure[[i]])   
           callname <- funcQuosure[[1]]
           funcQuosure[[1]] <- quote(c)
           rlang::quo(do.call(!!callname, !!funcQuosure ))
            
  }
 
}

parseAt <- function(atExpr) {
 #' This function is used by splatForm
 #' It replaces an expression of the form \code{TargetExpr@GroupingExpr}
 #' with \code{tapply(TargetExpr, GroupingExpr, c)}.

 expr  <- atExpr[[2]]
 group <- atExpr[[3]]
 
 rlang::expr(tapply(!!expr, !!group, c))
}

########## Mapping expression across list fields.

xifyForm <- function(expression, usedInExpr, depth = 1L) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which creates an lambda function which takes those names
          #' as arguments and calls the expression with them.
          #' This lambda function is appropriate for calling with
          #' Map, lapply, ngramApply, etc.
          #' This is used by listifyForm and ngramifyForm.
          #' 
          #' Argnames within the newly generated lambda expressions are changed
          #' to lower case versions of usedInExpr strings, but with depth "_" appended
          #' to make sure there's no accidental overlap (just a precaution).
          fargs <- as.pairlist(alist(x = )[rep(1, length(usedInExpr))])
          names(fargs) <- paste0('.', tolower(usedInExpr), strrep('_', depth))
          
          expression <- substituteName(expression,
                                       setNames(lapply(names(fargs), as.symbol), usedInExpr))
          
          lambdaexpression      <- quote(function() {} )
          lambdaexpression[[2]] <- fargs
          lambdaexpression[[3]] <- rlang::quo_get_expr(expression)
          
          rlang::quo_set_expr(expression, lambdaexpression)
          
}


mapifyForm <- function(funcQuosure, usedInExpr, depth = 1L) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which uses Map to call this expression across these named objects.
          #' (It presumes that the named objects are actually lists).
          #' It first uses xifyForm to put the expression in the form of a 
          #' lambda function.
  funcQuosure <- xifyForm(funcQuosure, usedInExpr, depth)
  
  rlang::quo_set_expr(funcQuosure, 
                      rlang::expr(Map(f = !!rlang::quo_get_expr(funcQuosure), 
                                      !!!lapply(usedInExpr, rlang::sym))))

}

ngramifyForm <- function(funcQuosure, ngramQuosure, usedInExpr, depth = 1L) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which uses applyNgram on these named objects.
          #' It first uses xifyForm to put the expression in the form of a 
          #' lambda function.
          #' 
          #' 
  funcQuosure <- xifyForm(funcQuosure, usedInExpr, depth)
  
  # rlang::quo_set_expr(funcQuosure,
                      # rlang::expr(applyNgram(n = !!rlang::quo_get_expr(ngramQuosure), 
                                             # vecs = list(!!!lapply(usedInExpr, rlang::sym)), 
                                             # f = !!rlang::quo_get_expr(funcQuosure))))
  rlang::quo(
            applyNgram(n = !!ngramQuosure, 
                       vecs = list(!!!lapply(usedInExpr, rlang::sym)),
                       f = !!funcQuosure))
  
}



################################## Applying expressions across partitions


partApply <- function(humtab, partitions, humfunc) {
  if (length(partitions) == 0L) return(humfunc(humtab))
          
  partQuosure <- if (length(partitions) == 1L) {
                    rlang::quo(humfunc(.SD)) 
          } else  {
                    rlang::quo(partApply(.SD, partitions[-1], humfunc))
          }
  
  
  curpart <- partitions[[1]]
  curpart <- tandemsForm(curpart)
  # curpart <- rlang::eval_tidy(curpart, data = humtab) # getting part evaluated in it's own environment
  
  if (names(partitions)[1] %in% c('by', 'groupby', 'group_by')) {
    dtcall <- rlang::quo(humtab[ , !!partQuosure,  by = !!curpart])#, .SDcols = colnames(humtab)])
    
    eval(rlang::quo_squash(dtcall))  # do it!
    
  } else {
            
    dtcall <- rlang::quo(humtab[curpart, !!partQuosure])
    output <- eval(rlang::quo_squash(dtcall))  # do it!
    
    # get unchanged part
    negatecall      <- rlang::expr(humtab[!curpart])
    rest <- eval(negatecall)
    
    rbind(output, rest, fill = TRUE)
  
  }
  
}

#' Change or insert values in an expression
#' 
#' This function can be used to modify arguments to a functions
#' within an existing expression (or quosure/formula).
#' 
#' \code{interpolateNamedValues} inteprets named value in its \code{namedArgs} 
#' argument in one of two ways: If the named value is a list, it interprets
#' the name of the list as a function call, and inserts/swaps any arguments
#' in that list into any instances of that function call within the \code{expr}.
#' Named arguments are inserted or substituted if already present in expression.
#' Unnamed argmuments are simply added to the call.
#' Examples:
#' \preformatted{
#' myexpr <- quote(dnorm(x, mean = 5))
#' interpolateNamedValues(myexpr, list(dnorm = list(mean = 2, sd = 5, TRUE)))
#' 
#' # result is new expresson: dnorm(x, mean = 2, sd = 5, TRUE)
#' }
#' If a named valued in the \code{namedArgs} argument is not a list,
#' that name/value pair is substituted anywhere it is present in the expression.
#' This approach is often more conscise, but arguments cannot be added to an 
#' expression this way, only substituted if already present.
#' Examples:
#' \preformatted{
#' myexpr <- quote(dnorm(x, mean = 5))
#' interpolateNamedValues(myexpr, mean = 2)
#' 
#' # result is new expression: dnorm(x, mean = 2)
#' 
#' }
#' @examples
#' myexpr2 <- quote(A + b*x + rnorm(length(a), mean(Z), sd = 2))
#' 
#' interpolateNamedValues(myexpr2,
#'                        list(sd = 10, mean = list(na.rm = TRUE)))
#'                        
#' # result is new expression: 
#' # a + b*x + rnorm(length(a), mean(Z, na.rm = TRUE), sd = 10)
#' 
#' 
#' @param expr A unevaluated expression object.
#' @param namedArgs A list of named arguments. Unnamed arguments are simply ignored.
#' 
#' @export
interpolateNamedValues <- function(expr, namedArgs) {
 if (length(namedArgs) == 0 || !is.call(expr)) return(expr)
          
 argNames <- names(namedArgs)
 
 callname <- deparse(expr[[1]])
 if (callname %in% argNames) {
           callArgs <- namedArgs[[which(argNames == callname)[1]]]
           callargNames <- names(callArgs)
           if (!is.null(callargNames) && !is.null(names(expr))) {
                    alreadythere <- names(expr) %in% callargNames & callargNames != ''
                    expr <- expr[!alreadythere]
           }
           
           for (i in seq_along(callArgs)) {
                     expr[[length(expr) + 1]] <- callArgs[[i]]
                     if (!is.null(callargNames) && callargNames[i] != '') names(expr)[length(expr)] <- callargNames[i]
           }
           namedArgs <- namedArgs[argNames != callname]
           argNames <- argNames[argNames != callname]
           if (length(namedArgs) == 0) return(expr)
 }
 #       
 named <- if (is.null(names(expr))) logical(length(expr)) else names(expr) != ''
 named[1] <- FALSE
 if (any(named) && any(argNames %in% names(expr[named]))) {
           for (name in argNames[argNames %in% names(expr[named])]) {
                     expr[[name]] <- namedArgs[[name]] 
           }
 }     
 
 expr[!named] <- lapply(expr[!named], interpolateNamedValues, namedArgs = namedArgs)      
           
 expr
 
}


#######################################################-
################################# Reassembling humtable ----
#######################################################-

#This is the hard part, putting pipeout output back into data.table

# options:
# Output are either length 1, same length as original, short (<21), or some length in between, or some other object.
#     If they are same length, put them back where they came, or into new column, no collapsing.
#     If they are length 1, put them back, but collapse everything else to 1
#     If they are short (<21) AND named, place them as n appropriately named columns (nrow = 1), collapse everything else
#     If they are less than 0, do nothing
#     In any other case, put into list (to make it singleton) and collapse everything else.

#     inputs are 1 or more vectors.
#     Outputs, may be single vectors, or lists of vectors.
#     Outpurs are either placed back in original column, or into new dummy columns
#          if output is list of vectors of matching length, create new columns.
#     Multiple input columns


`pipeIn<-` <- function(object, value) {
          #' This is the main function for taking the output of a
          #' function or expression applied to humtable data and putting
          #' it back into the humtable. If the output and input are the same length,
          #' it's easy. The hard part is what to do when the output is different than
          #' the input. This function hard codes a few common possibilities,
          #' but deserves thorough updating (11/29/2018, Nat Condit-Schultz).
          
          humtab <- object # R requires the names object and value for x<- functions
          
          curpipen <- curPipeN(humtab) 
          
          lenvalue <- length(value)
          if (lenvalue > nrow(humtab)) stop("Sorry, withinHumdrum doesn't currently support functions/expressions that return values that are longer than their input.", .call = FALSE)
          
          if (is.object(value)) value <- list(list(value))
          
          if (is.atomic(value) && allnamed(value) && lenvalue < 10L) value <- as.list(value)
          
          if (is.atomic(value) || (is.list(value) & length(value) == nrow(humtab))) value <- list(value)
          
          ### value should be a list now
          if (!is.list(value)) stop(glue::glue("Nat, pipeIn just recieved an input of class {class(value)} which doesn't fit your conditions."), .call = TRUE)
          
          ####
          if (!allsame(lengths_(value))) value <- list(value)
          
          
          ## at this point, value should:
          # be a list with all elements of the same length.
          # may or may not be named
          # each element in this list will be a new field in the humtable
          
          lenvalue <- lengths_(value)[1]
          if (lenvalue < nrow(humtab)) humtab <- collapseHumtab(humtab, n = lenvalue)
          
          # What will the pipes be named
          nnewfields   <- length(value)
          targetfields <- if (allnamed(value)) names(value) else paste0('Pipe', (curpipen + seq_len(nnewfields)))
          
          humtab[ , targetfields] <- value
          
          #humtab[ , newfields := paste(targetfields, collapse = ' ')]
          
          humtab
          
} 




ifelsecalls <- function(calls, fields) {
          # function used within withinHumdrum, as part of 
          # humdrumR (re)assembly process.
          call <- call('ifelse', rlang::f_rhs(calls[[1]]), 
                       quote(.), fields[[1]])
          if (len1(calls)) {
                    call[[3]] <- fields[[2]]
          } else {
                    call[[3]] <- Recall(calls[-1], fields[-1])
          }
          
          call
}

pipeFields <- function(humtab) {
  #' Another function used by pipeIn<-
  #' Takes a humtab and identifies the pipe fields (columns), if any,
  #' are in it.
  colnms <- colnames(humtab)
  
  pipefields  <- colnms[grepl('Pipe', colnms)]
  
  if (length(pipefields) != 0L) pipefields <- pipefields[order(as.numeric(stringr::str_extract(pipefields, '[0-9]+')))]
  
  pipefields
}

curPipeN <- function(humtab) {
          #' A function used by pipeIn<-
          #' identifies how many pipe fields (if any)
          #' are already in a humtable.
          length(pipeFields(humtab))   
          
}

lengths_ <- function(ls) {
          #' Used by pipeIn<-
          #' 
          #' Acts just like base::lengths function,
          #' except it knows to treat any argument where
          #' is.object == TRUE as length 1.
          ifelse(sapply(ls, is.object), 1, sapply(ls, length))
          
}



collapseHumtab <- function(humtab, n = 1L) {
  #' This is the biggest part of pipeIn<-'s work.
  #' It's the trickiest part of the process, shrinking
  #' the existing humtable to match the size of the new fields.
  #' 
  #' This function really only passes information into collapse2n, which
  #' is the really workhorse.
  #' 
  humtab[ , Map(f = collapse2n, 
            .SD, # Each field/column
            colnames(humtab), # the field name
            lapply(humtab, class), # the class of each field (important because data.table doesn't like columns to change class
            n = n
            )] 
  }

collapse2n <- function(x, colname, class, n = 1) {
  #' This is the function that actually collapses a field
  #' (column in humtable) to a smaller length. 
  #' Certain structural fields are treated differently,
  #' so that (hopefully) things like record and spine numbers
  #' still make sense after collapse.  
  uniqx <- unique(x)
  uniqx <- uniqx[!is.na(uniqx)]
  if (length(uniqx) == 0) return(rep(as(NA, class), n))
  
  if (colname %in% c('Record', 'Spine', 'Path', 'ColumnNumber', 
                     'StopNumber', 'Bar', 'DoubleBarline', 'NData', 'File')) {
    uniqx[1:n]
  } else {
    rep( if (length(uniqx) == 1) uniqx else as(NA, class) , n)
  }
  
}


################################humApply ----
# 
#' 
#' \code{humApply} is just a wrapper for 
#' \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}},
#' included to parallel the \code{R} family of \code{\link[base:lapply]{_apply}} functions.
#' \code{humApply} uses \href{http://adv-r.had.co.nz/Computing-on-the-language.html}{non-standard evaluation}
#' to capture arguments fed to it without the user needing to make explicit 
#' \code{\link[base:tilde]{formula}} using \code{~}. This is only guaranteed to work 
#' in the \code{\link[base:environment]{global environment}}, so be careful. If you run into
#' problems, switch over to \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} and use
#' explicit \code{\link[base:tilde]{X~formulas}}.
#' 
#' @param humdrumR A \code{\linkS4class{humdrumR}} data object.
#' @param FUN A function to apply to the \code{\link[humdrumR:humdrumR]{Active}} field(s)
#' in the \code{humdrumR} object.
#' @param ... Any arguments which can be fed to 
#' \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} as formulae (except for
#' \code{do} expressions, which are replaced by the \code{FUN} argument!). 
#' However, rather that writinging formula in the format \code{Keyword ~ Expression},
#' \code{humApply} arguments should be written as normal \code{R} arguments: 
#' \code{Keyword = Expression}.
#' Unnamed arguments are ignored.
#' #' @param within A logical. If \code{TRUE} (the default), 
#' \code{\link[humdrumR:with-in-Humdrum]{withinHumdrum}} is used to apply the 
#' function---meaning that the output is reconstituted into a new field in the 
#' \code{humdrumR} object. If \code{within == FALSE},
#' \code{\link[humdrumR:with-in-Humdrum]{withHumdrum}} is used instead,
#' which results in the function's output being returned inprocessed.
#' @param doplot Boolean. If \code{TRUE} the \code{FUN} argument is treated
#' as a \code{doplot} expression by \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}},
#' so the result is ignored (for plotting or side-effects purposes).
#' @export
humApply <- function(humdrumR, FUN, ..., within = TRUE, doplot = FALSE) {
          exprs <- rlang::quos(...)
          keywords <- names(exprs)
          
          if (is.null(keywords)) exprs <- list()
          exprs    <- exprs[keywords != '']
          keywords <- keywords[keywords != '']
          
          formulae <- Map(function(qu, kw) rlang::new_formula(rhs = rlang::quo_get_expr(qu),
                                                              lhs = as.symbol(kw), 
                                                              env = rlang::quo_get_env(qu)), exprs, keywords) 
          formulae <- unname(formulae)
          # do expression
          do <- rlang::new_formula(rhs = quote(FUN(.)),
                                   lhs = if (doplot) quote(doplot) else quote(do),
                                   env = environment())
          
          do.call(if (within) 'withinHumdrum' else 'withHumdrum',
                  c(humdrumR, do, formulae))
          
          
          
          
}
