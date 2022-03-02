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
#'   \item{recordtypes}{A string or vector of characters drawn from \code{c("D", "d", "I", "L", "M","G")}. These characters
#'   correspond to types of humdrum records: \strong{D}ata, null \strong{d}ata, \strong{I}nterpretations, 
#'   \strong{M}easures, \strong{L}ocal comments, and \strong{G}lobal comments respectively. The expression
#'   is only evaluated on data drawn from the specified record types (defaults to \code{"D"}).}
#'   \item{pre}{An expression to evaluate once before evaluating the do expression(s). Useful, for instance, for taking logs
#'   or opening a graphing window. The \code{pre} expression is evaluated in the global environment.}
#'   \item{post}{An expression evaluate once after evaluating the do expression(s). Always evaluated in the global environment.}
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
#' 
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
#' 
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
#' 
#' Any named arguments to \code{withinHumdrum} are \code{\link[humdrumR:interpolateArguments]{interpolated}} into the
#' \code{do} expressions. This is useful if you've already created a list of formulas that you like, but would like
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
#' 
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
#' must be a name/symbol. Named arguments are \link[humdrumR:interpolateArguments]{interpolated} into and \code{do~X} formulas.

#' 
#' @param ... Additional formulas/functions, or lists of formulas/functions.
#' These are all simply appended to the \code{formulae} argument.
#' 
#' @param drop This argument is concetually similar to the \code{drop} argument in R matrices and data.frames.
#' If \code{drop = TRUE}, the output of \code{withHumdrum} is simplified as much as possible (trying to return
#' the "raw" vector, list, table, etc. within it). If \code{drop = FALSE}, the result is \emph{always}
#' a \code{data.table}. The default value (\code{drop = TRUE}) is usually what we want because it is more
#' intuitive, but in more complex code, it can be helpful to set \code{drop = FALSE} so that 
#' the output is consistent.
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
#' @name withinHumdrum
NULL

#' withinHumdrum
#'
#' @rdname withinHumdrum
#' @export
withinHumdrum <- function(humdrumR,  ...) {
          list2env(.withHumdrum(humdrumR, ..., withfunc = 'withinHumdrum'), envir = environment())
         
          ###
          if (!all(names(parsedArgs$formulae$doexpressions) == 'dofx')) {
              local({
              #### if any new fields have same name as old, remove old
              # ADD A CHECK TO PREVENT OVERWRITING STRUCTURAL FIELDS?
                overwrittenfields <- colnames(result)[colnames(result) %in% colnames(humtab)]
                overwrittenfields <- overwrittenfields[overwrittenfields != '_rowKey_']
                if (length(overwrittenfields)) humtab[ , eval(overwrittenfields) := NULL]
              })
              
              ## put result into new humtab
              newhumtab <- result[humtab, on ='_rowKey_'] 
              newhumtab[ , `_rowKey_` := NULL]
              
              # number new pipes
              unnamedresult <- colnames(newhumtab) == 'Pipe'
              colnames(newhumtab)[unnamedresult] <- paste0('Pipe', curPipeN(humtab) + seq_len(sum(unnamedresult)))
              
              
              #### Put new humtable back into humdrumR object
              newfields <- setdiff(colnames(newhumtab), colnames(humtab))
              
              notnull <- Reduce(`|`, lapply(newhumtab[, newfields, with = FALSE], function(field) if (is.logical(field)) logical(length(field)) else  !(is.na(field) | field == '.')))
              newhumtab$Null[notnull] <- FALSE
              newhumtab$Type[newhumtab$Type == 'd' & notnull] <- 'D'
              
              # What do do if d is in recordtypes
              recordtypes <- checkTypes(rlang::eval_tidy(parsedArgs$formulae$recordtypes), 'withinHumdrum')
              if (all(recordtypes == 'd') && all(newhumtab$Type == 'D')) {
                humtab[ , `_rowKey_` := NULL]
                newhumtab <- rbind(newhumtab, getHumtab(humdrumR, 'D'), fill = TRUE)
              }
              if (any(grepl('d', recordtypes))) {
                humdrumR@Humtable$d <- humdrumR@Humtable$d[FALSE]
              }
              
              putHumtab(humdrumR, drop = TRUE) <- newhumtab
              ########### Update other slots in humdrumR object
              
              # Now that the Humtable is taken care of,
              # tell the humdrumR object about the new fields and set the Active formula.
              if (length(newfields)) {
                  ## Add fields to humtab
                  addFields(humdrumR) <- newfields
                  
                  ## Create new Active quosure
                  # humdrumR <- if (any(names(parsedArgs$formulae$partitions) == 'where') && is.null(ordoQuosure)) {
                  #     act <- ifelsecalls(parsedArgs$formulae$partitions['where'], 
                  #                        c(humdrumR@Active, 
                  #                          lapply(newfields, 
                  #                                 function(nf) rlang::as_quosure(as.symbol(nf), 
                  #                                                                environment(humdrumR)))))
                  #    
                  #      putActive(humdrumR, act)
                  # } else {
                  #     setActiveFields(humdrumR, newfields) 
                  # }
                  humdrumR <- setActiveFields(humdrumR, newfields) 
              }
              # humdrumR <- indexGLIM(humdrumR)
              
          }
          
          humdrumR 
}
          
          


#' withHumdrum
#'
#' @rdname withinHumdrum
#' @export
withHumdrum <- function(humdrumR,  ..., drop = TRUE) {
    list2env(.withHumdrum(humdrumR, ..., withfunc = 'withHumdrum'), envir = environment())
  
    result[ , `_rowKey_` := NULL]
    
    ####-
    ### Do we want extract the results from the data.table? 
    if (drop && ncol(result) == 1L) {
        result <- result[[1L]]
        if (is.list(result) && length(result) == 1L) result <- result[[1]]
    } 
    
    result
}
    

.withHumdrum <- function(humdrumR,  ..., withfunc)  {
    # this function does most of the behind-the-scences work for both 
    # withinHumdrum and withHumdrum.
    
    checkhumdrumR(humdrumR, withfunc)
    
    #### Processing formulae list
    # parseArgs returns a list with the following names:
    # formulae, oldpar (which may be NULL), namedArgs
    parsedArgs <- parseArgs(..., withfunc = withfunc)
    
    # parseArgs MAY have changed graphical parameters.
    on.exit(par(parsedArgs$oldpar)) # if old par is NULL, nothing happens
    
    ### pre and post formulae 
    # these are evaluated before and after (respectively) everything else, in the global environment
    if (length(parsedArgs$formulae$pre)  > 0L) rlang::eval_tidy(parsedArgs$formulae$pre[[1]], env = .GlobalEnv)
    if (length(parsedArgs$formulae$post) > 0L) on.exit(rlang::eval_bare(rlang::quo_expr(parsedArgs$formulae$post[[1]]), 
                                                                        env = .GlobalEnv),
                                                       add = TRUE)
    
    #### Preprocess humdrumR and humtab
    # Getting the humtab with the right record types.
    humtab <- rlang::eval_tidy(rlang::quo(getHumtab(humdrumR, !!parsedArgs$formulae$recordtypes)))
    
    ###########################-
    ### Preparing the "do" expression
    doQuosure <- prepareQuo(humtab, parsedArgs$formulae$doexpressions, 
                            humdrumR@Active, parsedArgs$formulae$ngram)
    ordoQuosure <- prepareQuo(humtab, parsedArgs$formulae$ordoexpression,
                              humdrumR@Active, parsedArgs$formulae$ngram)
    
    ###########################-
    #### evaluate "do" expression! 
    humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
    
    result <- evalDoQuo(doQuosure, humtab, 
                        parsedArgs$formulae$partitions, 
                        ordoQuosure)
    data.table::setorder(result, `_rowKey_`)
    as.list(environment())
    
}
    
    
##### Parsing Args (Formulae) ----

parseArgs <- function(..., withfunc) {
    #### Preprocessing ... argument for with(in)Humdrum
    
    elips <- list(...)
    formulae <- if (is.null(names(elips))) elips else unlist(elips[names(elips) == ''])
    namedArgs <- elips[names(elips) != '']
    
    # turn  any functions into formula
    formulae <- anyfuncs2forms(formulae, parent.env(parent.env(environment())))
    if (any(!sapply(formulae, rlang::is_formula))) stop(call. = FALSE,
                                                        glue::glue('In {withfunc}(...) unnamed arguments must be formulas or functions.'))
    
    #### Processing formulae list
    #parseKeywords returns a list with the following names:
    # graphics, recordtypes, doexpressions, ngrams, partitions
    parsedFormulae <- parseKeywords(formulae, withfunc) 
    
    #### interpolate named args into do formulae
    if (length(namedArgs) > 0L) parsedFormulae$doexpressions <- lapply(parsedFormulae$doexpressions,  
                                                                       interpolateArguments, namedArgs = namedArgs)
    
    ### graphical options
    oldpar <- NULL
    if (length(parsedFormulae$graphics) > 0) {
        oldpar <- par(no.readonly = TRUE)
        rlang::eval_tidy(rlang::quo(par(!!!parsedFormulae$graphics)))
    }
    
    
    list(formulae   = parsedFormulae, 
         oldpar     = oldpar,
         namedArgs  = namedArgs)
}
 
#' @rdname withinHumdrum
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

parseKeywords <- function(formulae, withfunc) {
 formulargs <- parseArgFormulae(formulae) # output is named list of quosures
 
 names(formulargs) <- partialMatchKeywords(names(formulargs))
 formulargnames <- names(formulargs)
 
 #
 
 knownKeywords <- list(doexpressions   = c('do', 'dofx', 'dofill', 'domemory'),
                       ordoexpression  = c('ordo', 'ordofill'),
                       partitions      = c('by', 'where'),
                       graphics        = names(par()),
                       ngram           = 'ngram',
                       recordtypes     = 'recordtypes',
                       windows         = 'windows',
                       pre             = 'pre',
                       post            = 'post')
 
 
 #
 boolean <- lapply(knownKeywords, `%in%`, x = formulargnames)
 
 unknownKeys <- colSums(do.call('rbind', boolean)) == 0L
 if (any(unknownKeys)) {
     unknownKeynames <- glue::glue_collapse(paste0(formulargnames[unknownKeys], '~'), sep = ', ', last = ' and ')
     stop(call. = FALSE, glue::glue("In your call to {withfunc}, the formula {plural(sum(unknownKeys), 'keywords', 'keyword')} {unknownKeynames} {plural(sum(unknownKeys), 'do', 'does')} not match any known formula keys." ))
 }
 
 values  <- lapply(boolean, function(b) formulargs[b])
 
 if (!any(boolean$doexpressions)) stop(call. = FALSE,
                                   glue::glue("Your call to {withfunc} doesn't include any do expressions to apply to the data!
                                   These expressions should be either in a formula with no left hand side (~Expr) or with 'do' or 'doplot' on the left hand side (do~Expr, doplot~Expr)."))
 
 #
 if (any(boolean$ordoexpressions) && 
     (!any(names(values$partitions) == 'where') 
      || !any(boolean$doexpressions)) ) stop(call. = FALSE,
                                             glue::glue("In your call to {withfunc} you included an 'ordo' expression improperly. 
                                                        An 'ordo' expression can only be used in combination with BOTH a where expression AND a do expression."))
 
 # These keywords must always have exactly one evaluated value.
 #        If there are more than one, take the first.
 #        If there are none, some categories have defaults
 values$ngram       <- if (length(values$ngram)       == 0L) NULL else values$ngram[[1]]
 values$recordtypes <- if (length(values$recordtypes) == 0L) rlang::quo('D') else values$recordtypes[[1]]
 
 # Other keywords (e.g., windows and partitions) can be of any length, including 0

 
 values
}

partialMatchKeywords <- function(keys) {
    # this function matches partial matches off keywords
    # to the master standard
    keys <- gsub('_', '', tolower(keys))
    
    # define standard keys and alternatives
    standardkeys <- list(do          = c('do', 'd', 'eval', 'apply'),
                         dofill      = c('dofill', 'fill', 'evalfill', 'filleval', 'applyfill'),
                         domemory    = c('domemoizeDispatch', 'domem'),
                         dofx     = c('dofxs', 'dosideeffects', 'dosidefxs', 'fxs', 
                                      'doplots', 'plots',
                                      'evalfxs', 'evalsidefxs', 'evalsideeffects',
                                      'applyfxs', 'applysidefxs', 'applysideffects'),
                         by       = c('by', 'groupby', 'groups', 'across', 'groupacross'),
                         where    = c('where', 'when'),
                         ordo     = c('ordo', 'orelsedo', 'elsedo'),
                         ordofill = c('ordofill', 'orelsedofill', 'orelsefill', 'elsefill'),
                         recordtypes = 'recordtypes',
                         ngram    = 'ngrams',
                         windows  = 'windows',
                         pre      = 'pre', 
                         post     = 'post')
    
    matches <- pmatch(keys, unlist(standardkeys), duplicates.ok = TRUE)
    
    rep(names(standardkeys), lengths(standardkeys))[matches] %|% keys
    
    
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
          expr <- if (length(exprs) == 1L) exprs[[1]] else  rlang::expr(list(!!!exprs))
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


##### Preparing doQuo ----

prepareQuo <- function(humtab, doQuos, active, ngram = NULL) {
  # This is the main function used by \code{\link{withinHumdrum}} to prepare the current
  # do expression argument for application to a \code{\linkS4class{humdrumR}} object.
  if (length(doQuos) == 0L) return(NULL)
  
  # do fill 
  usedInExprs <- lapply(doQuos, fieldsInExpr, humtab = humtab)
  dofills <- names(doQuos) %in% c('dofill', 'ordofill')
  doQuos[dofills] <- Map(fillQuo, doQuos[dofills], usedInExprs[dofills])
  

  # collapse doQuos to a single doQuo
  doQuo <- concatDoQuos(doQuos)

  # turn . to active formula
  doQuo <- activateQuo(doQuo, active)
  
  # lagged vectors
  doQuo <- laggedQuo(doQuo)
  
  # add in arguments that are already fields
  doQuo <- fieldsArgsQuo(doQuo, colnames(humtab))
  
  # unnest nested formulae
  # funcQuosure <- unnestQuo(funcQuosure)
  
  # tandem interpretations
  doQuo <- tandemsQuo(doQuo)
  
  # splats
#  doQuo <- splatQuo(doQuo)
  
  # find what fields (if any) are used in formula
  usedInExpr <- unique(fieldsInExpr(humtab, doQuo))
  
  # if (length(usedInExpr) == 0L) stop("The do expression in your call to withinHumdrum doesn't reference any fields in your humdrum data.
  #                                       Add a field somewhere or add a dot (.), which will automatically grab the default, 'Active' expression.",
  #                                       call. = FALSE)

  # if the targets are lists, Map
  lists <- vapply(humtab[1 , usedInExpr, with = FALSE], class, FUN.VALUE = character(1)) == 'list' 
  if (any(lists)) doQuo <- mapifyQuo(doQuo, usedInExpr, depth = 1L)
    
  # if ngram is present
  if (!is.null(ngram) && rlang::eval_tidy(ngram) > 1L) {
            doQuo <- ngramifyQuo(doQuo, 
                                        ngram, usedInExpr, 
                                        depth = 1L + any(lists))
  } 
  
  
  # do memory
  if (any(names(doQuos) == 'domemory')) doQuo <- memoizeDispatch.quosure(doQuo, usedInExpr)

  doQuo
}


fillQuo <- function(doQuo, usedInExpr) {
    # this takes a do(fill) quosure and makes sure its
    # results expands to be the same size as its input
    if (length(usedInExpr) == 0L) usedInExpr <- '.'
    usedInExpr <- rlang::syms(usedInExpr)
    
    rlang::quo({
        targetlen <- max(lengths(list(!!!usedInExpr)))
        rep(!!doQuo, length.out = targetlen)
    } )
    
}

concatDoQuos <- function(doQuos) {
    ## this function takes a named list of "do" quosures and creates a single quosure
    # which applies each of them in turn.
    # the doQuos must have names either do or doplot
    
    sideEffects <- names(doQuos) == 'dofx'
    
    
    if (tail(sideEffects, 1)) doQuos <- c(doQuos, rlang::quo(.))

    temp <- quote(.)
    for (i in 1:length(doQuos)) {
        if (i > 1L) doQuos[[i]] <- substituteName(doQuos[[i]], list(. = temp))
        
        
        expr <- rlang::quo_get_expr(doQuos[[i]])
        
        if (i < length(doQuos) && 
                           (length(expr) > 1 && expr[[1]] != '<-') &&
                           !sideEffects[i]) {
            
            temp <- as.symbol(tempfile('xxx', tmpdir = ''))
            doQuos[[i]] <- rlang::new_quosure(expr(!!temp <- !!doQuos[[i]]), 
                                              env = rlang::quo_get_env(doQuos[[i]]))
            
        } 
    }
    
    
    quo({!!!doQuos})
}

####################### Functions used inside prepareQuo

activateQuo <- function(funcQuosure, active) {
  # This function takes the \code{expression} argument
  # from the parent \code{\link{withinHumdrum}} call and 
  # inserts the \code{Active} expression from the 
  # target \code{\linkS4class{humdrumR}} object in place 
  # of any \code{.} subexpressions.
  active <- rlang::f_rhs(active)
  substituteName(funcQuosure, list(. = active))
}

#### nested formulae in expressions
# 
# unnestQuo <- function(funcQuosure) {
#     ## THIS DOESNT WORK...NEED TO EVALUATE the symbols to figure out if they are formulae 
#     if (length(funcQuosure) < 2L) return(funcQuosure)
#     lhs <- rlang::f_lhs(funcQuosure)
#     if (identical(funcQuosure[[1]], as.symbol('~')) &&
#         (is.null(lhs) || 
#          identical(lhs, as.symbol('do')))) {
#         
#         return(Recall(funcQuosure[[length(funcQuosure)]]))
#         
#     } else 
#         
#     for (i in seq_along(funcQuosure)) {
#         funcQuosure[[i]] <- Recall(funcQuosure[[i]])
#         
#     }
#     funcQuosure
# }

#### Insert arguments which are fields automatically!

fieldsArgsQuo <- function(funcQuosure, fields) {
    
    predicate <- function(quo) !is.null(fargs(rlang::eval_tidy(quo[[2]][[1]], env = quo_get_env(quo))))
      
    
    do <- function(quo) {
        formNames <- names(fargs(rlang::eval_tidy(quo[[2]][[1]], env = quo_get_env(quo))))
        
        hits <- fields %in% formNames
        
        if (!any(hits)) return(quo)
        
        namedArgs <- .names(quo[[2]][-1])
        
        formNames <- formNames[!formNames %in% namedArgs] 
        
        usedArgs <- c(namedArgs, head(head(formNames, 
                                           min(which(formNames == '...'), 
                                               length(formNames))), 
                                      sum(namedArgs == "")))
        
        hits <- hits & !(fields %in% usedArgs)
        hits <- fields[hits]
        quo[[2]][hits] <- rlang::syms(hits)

        quo
    }
    
    recurseQuosure(funcQuosure, predicate, do, stopOnHit = FALSE)
    
}

#### Lag/Led vectors

laggedQuo <- function(funcQuosure) {
  
  predicate <- function(quo) { 
    quo <- rlang::quo_squash(quo)
    rlang::is_call(quo) && quo[[1]] == sym('[') && any(names(quo) == 'n')
  }
  
  do <- function(quo) {
    exp <- quo[[2]]
    
    args <- as.list(exp)[-1:-2]
    if (!'windows' %in% names(args)) args$windows <- expr(list(File, Spine))
    
    indexedObject <- exp[[2]]
    n <- eval_tidy(args$n)
    args <- args[names(args) != 'n']
    
    exprs <- lapply(n, \(N) { if (N == 0L) expr(!!indexedObject) else expr(lag(!!indexedObject, n = !!N, !!!args)) })
    
    quo[[2]] <- if (length(n) == 1L) {
      exprs[[1]]
    } else {
      expr(.SPLAT.(!!!exprs))
    }
    quo
  
  }
  
  funcQuosure <- recurseQuosure(funcQuosure, predicate, do, stopOnHit = FALSE)
  
  # check for .SPLAT.
  predicate <- function(quo) {
    quo <- quo_squash(quo)
    rlang::is_call(quo) &&
      any(sapply(quo[-1], \(arg) rlang::is_call(arg) && arg[[1]] == sym('.SPLAT.')))
  }
  do <- function(quo) {
    expr <- as.list(quo[[2]])
    splats <- sapply(expr[-1], \(arg) rlang::is_call(arg) && arg[[1]] == sym('.SPLAT.'))
    
    for (i in which(splats)) expr[[i + 1L]] <- as.list(expr[[i + 1L]][-1])
    expr <- unlist(expr, recursive = FALSE)
    
    quo[[2]] <- as.call(expr)
    quo
  }
  
  recurseQuosure(funcQuosure, predicate, do)
   
}

#### Interpretations in expressions

tandemsQuo <- function(funcQuosure) {
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
# go log(myvector) and have it work. splatQuo allows exactly this in a 
# call to withinHumdrum. We can group one field by another field, then
# feed each group as an argument to a function.
#

splatQuo <- function(funcQuosure) {
  # This function takes an expression,
  # and replaces any subexpression of the form \code{funccall(TargetExpr@GroupingExpr)},
  # with \code{do.call('funccall', tapply(TargetExpr, GroupingExpr, c))}.
  # The result is that \code{TargetExpr} is broken into a list of vectors by the
  # \code{GroupingExpr}, and each group is fed to \code{funccall} as a separate
  # argument. See the docementation for \code{\link{withinHumdrum}}.
  # This does not look for \code{@} sub expression within branches of a \code{@} expression!
  # 
  
  
  predicate <- function(expr) any(sapply(expr, is.givenCall, call = '@'))
  
  transform <- function(expr) {
      expr <- recurseQuosure(expr, 
                     function(quo) is.givenCall(quo, '@'), 
                     function(quo) {rlang::quo(unname(tapply(!!quo[[2]], !!quo[[3]], list)))
                         })
      expr <- as.list(expr)
      rlang::quo(do.call(!!(as_string(expr[[1]])), !!!expr[-1]))
  }
  
  recurseQuosure(funcQuosure, predicate, transform)
 
}

parseAt <- function(atExpr) {
 # This function is used by splatQuo
 # It replaces an expression of the form \code{TargetExpr@GroupingExpr}
 # with \code{tapply(TargetExpr, GroupingExpr, c)}.

 expr  <- atExpr[[2]]
 group <- atExpr[[3]]
 
 rlang::expr(tapply(!!expr, !!group, c))
}

########## Mapping expression across list fields.

xifyQuo <- function(expression, usedInExpr, depth = 1L) {
          # This function takes an expression and a vector of strings representing
          # names used in that expression and creates an expression
          # which creates an lambda function which takes those names
          # as arguments and calls the expression with them.
          # This lambda function is appropriate for calling with
          # Map, lapply, ngramApply, etc.
          # This is used by listifyQuo and ngramifyQuo.
          # 
          # Argnames within the newly generated lambda expressions are changed
          # to lower case versions of usedInExpr strings, but with depth "_" appended
          # to make sure there's no accidental overlap (just a precaution).
          fargs <- as.pairlist(alist(x = )[rep(1, length(usedInExpr))])
          names(fargs) <- paste0('.', tolower(usedInExpr), strrep('_', depth))
          
          expression <- substituteName(expression,
                                       setNames(lapply(names(fargs), as.symbol), usedInExpr))
          
          lambdaexpression      <- quote(function() {} )
          lambdaexpression[[2]] <- fargs
          lambdaexpression[[3]] <- rlang::quo_squash(expression)
          # 
          # lambdaexpression <- rlang::new_quosure(quote(function() {}))
          # lambdaexpression[[2]][[2]] <- fargs
          # lambdaexpression[[2]][[3]] <- expression
          
          # rlang::quo_set_expr(expression, lambdaexpression)
          rlang::quo(!!lambdaexpression)
          # 
}


mapifyQuo <- function(funcQuosure, usedInExpr, depth = 1L) {
          # This function takes an expression and a vector of strings representing
          # names used in that expression and creates an expression
          # which uses Map to call this expression across these named objects.
          # (It presumes that the named objects are actually lists).
          # It first uses xifyQuo to put the expression in the form of a 
          # lambda function.
  funcQuosure <- xifyQuo(funcQuosure, usedInExpr, depth)
  
  rlang::quo_set_expr(funcQuosure, 
                      rlang::expr({Map(f = !!rlang::quo_get_expr(funcQuosure), 
                                      !!!lapply(usedInExpr, rlang::sym))}))

}

ngramifyQuo <- function(funcQuosure, ngramQuosure, usedInExpr, depth = 1L) {
          # This function takes an expression and a vector of strings representing
          # names used in that expression and creates an expression
          # which uses applyNgram on these named objects.
          # It first uses xifyQuo to put the expression in the form of a 
          # lambda function.
          # 
          # 
  funcQuosure <- xifyQuo(funcQuosure, usedInExpr, depth)
  
  # rlang::quo_set_expr(funcQuosure,
                      # rlang::expr(applyNgram(n = !!rlang::quo_get_expr(ngramQuosure), 
                                             # vecs = list(!!!lapply(usedInExpr, rlang::sym)), 
                                             # f = !!rlang::quo_get_expr(funcQuosure))))
  rlang::quo(
            applyNgram(n = !!ngramQuosure, 
                       vecs = list(!!!lapply(usedInExpr, rlang::sym)),
                       f = !!funcQuosure))
  
}






#' Change or insert values in an expression
#' 
#' This function can be used to modify arguments to a functions
#' within an existing expression (or quosure/formula).
#' 
#' \code{interpolateArguments} inteprets named value in its \code{namedArgs} 
#' argument in one of two ways: If the named value is a list, it interprets
#' the name of the list as a function call, and inserts/swaps any arguments
#' in that list into any instances of that function call within the \code{expr}.
#' Named arguments are inserted or substituted if already present in expression.
#' Unnamed argmuments are simply added to the call.
#' Examples:
#' \preformatted{
#' myexpr <- quote(dnorm(x, mean = 5))
#' interpolateArguments(myexpr, list(dnorm = list(mean = 2, sd = 5, TRUE)))
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
#' interpolateArguments(myexpr, mean = 2)
#' 
#' # result is new expression: dnorm(x, mean = 2)
#' 
#' }
#' @examples
#' myexpr2 <- quote(A + b*x + rnorm(length(a), mean(Z), sd = 2))
#' 
#' interpolateArguments(myexpr2,
#'                      list(sd = 10, mean = list(na.rm = TRUE)))
#'                        
#' # result is new expression: 
#' # a + b*x + rnorm(length(a), mean(Z, na.rm = TRUE), sd = 10)
#' 
#' 
#' @param expr A unevaluated expression object.
#' @param namedArgs A list of named arguments. Unnamed arguments are simply ignored.
#' 
#' @export
interpolateArguments <- function(quo, namedArgs) {
    expr <- rlang::quo_get_expr(quo)
    expr <- .interpolateArguments(expr, namedArgs)
    
    rlang::new_quosure(expr, rlang::quo_get_env(quo))
    
}
    
.interpolateArguments <- function(expr, namedArgs) {    
    # the use interpolateArguments takes quosures
    # under the hood, .interpolateArguments works with raw expressions.
    # this is necessarry because .interpolateArguments is recursive.
 if (!is.call(expr)) return(expr)
          
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
 
 expr[!named] <- lapply(expr[!named], .interpolateArguments, namedArgs = namedArgs)      
           
 expr
}




##### Evaluating do quo in humtab ----
###########- Applying withinHumdrum's expression to a data.table





evalDoQuo <- function(doQuo, humtab, partQuos, ordoQuo) {
    if (length(partQuos) == 0L) {
        result <- rlang::eval_tidy(doQuo, data = humtab)
        parseResult(result, humtab$`_rowKey_`)
        
    } else {
        result <- evalDoQuo_part(doQuo, humtab, partQuos, ordoQuo)
        if (is.data.frame(result)) result else data.table::rbindlist(result)
    }
}
evalDoQuo_part <- function(doQuo, humtab, partQuos, ordoQuo) {
    ### evaluation partition expression and collapse results 
    ## to a single factor
    partType <- names(partQuos)[1]
    partition <- rlang::eval_tidy(partQuos[[1]], humtab)
    
    if (!is.list(partition)) partition <- list(partition)
    partition <- lapply(partition, rep, length.out = nrow(humtab))
    
    partition <- Reduce(switch(partType, by = paste, where = `&`), partition)
    
    partEval <- switch(partType,
                       by    = evalDoQuo_by,
                       where = evalDoQuo_where)
    
    partEval(doQuo, humtab, partition, partQuos, ordoQuo)
    
}

evalDoQuo_by <- function(doQuo, humtab, partition, parts, ordoQuo) {
    # this function doesn't use reHum because data.table 
    # pretty much already does (some) of whats needed.
    targetFields <- namesInExprs(colnames(humtab), c(doQuo, parts[-1]))
    targetFields <- c(targetFields, '_rowKey_')
    
    partition <- as.factor(partition)
    
    result <- humtab[ , 
                      list(list(evalDoQuo(doQuo, .SD, parts[-1], ordoQuo))), 
                      by = partition, .SDcols = targetFields]
    
    result$V1
}
evalDoQuo_where <- function(doQuo, humtab, partition, parts, ordoQuo) {
    if (!is.logical(partition)) stop(call. = FALSE,
                                     "In your call to with(in)Humdrum with a 'where ~ x' expression, 
                                     your where expression must evaluate to a logical (TRUE/FALSE).")
    result <- evalDoQuo(doQuo, humtab[partition], parts[-1], ordoQuo)
    
    if (!is.null(ordoQuo)) {
        orresult <- evalDoQuo(ordoQuo, humtab[!partition], parts[-1], NULL)
        result <- rbind(result, orresult)
    }
    
   # result[humtab[, '_rowKey_'], on ='_rowKey_'] 
   result
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

parseResult <- function(result, rowKey) {
    # this takes a nested list of results with associated
    # indices and reconstructs the output object.
    if (length(result) == 0L || all(lengths(result) == 0L)) return(data.table())
    
    parseFunc <- switch(class(result)[1],
                        data.table = force,
                        data.frame = as.data.table,
                        table      = parseResult_table,
                        integer    = ,
                        numeric    = ,
                        factor     = ,
                        character  = ,
                        logical    = parseResult_vector,
                        list       = parseResult_list,
                        parseResult_other)
    
    result <- parseFunc(result)
    
    colnames(result)[colnames(result) == ""] <- "Pipe"
    colnames(result) <- gsub('^V{1}[0-9]+', "Pipe", colnames(result))
    
    #
    lenRes <- nrow(result)
    lenKey <- length(rowKey)
    
    if (lenRes > lenKey) stop("Sorry, withinHumdrum doesn't currently support functions/do-expressions
                              that return values that are longer than their input.", .call = FALSE)
    
    result[ , `_rowKey_` := rowKey[1:lenRes]]
    result
    
}

parseResult_vector <- function(result) {
    if (!is.table(result) && allnamed(result) && length(result) < 15) {
        # If it's a short vector and ALL the ellements are named,
        # we'd like to make them each their own (named) field
        # in a data.table
        as.data.table(as.list(result))
    } else {
        data.table(Pipe = result)
    }
}

parseResult_table <- function(result) {
   names(dimnames(result)) <- NULL
   data.table(list(result))
}

parseResult_list  <- function(result) data.table(result) 
parseResult_other <- function(result) data.table(list(result))

`pipeIn<-` <- function(object, value) {
          # This is the main function for taking the output of a
          # function or expression applied to humtable data and putting
          # it back into the humtable. If the output and input are the same length,
          # it's easy. The hard part is what to do when the output is different than
          # the input. This function hard codes a few common possibilities,
          # but deserves thorough updating (11/29/2018, Nat Condit-Schultz).
          humtab <- object # R requires the names object and value for x<- functions
          
          curpipen <- curPipeN(humtab) 
          
          lengths_ <- function(ls) sapply(ls, function(v) if (is.object(v)) 1L else length(v)) # objects are length 1
          
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
    
          conditionexpr <- calls[[1]]
          
          ifexpr <- if (length(calls) == 1L) {
              fields[[2]]
          } else {
              Recall(calls[-1], fields[-1])
          }
          
          elseexpr <- fields[[1]]
          
          #
          rlang::quo(ifelse(!!conditionexpr, !!ifexpr, !!elseexpr))
}

pipeFields <- function(humtab) {
  # Another function used by pipeIn<- (withing curePipeN)
  # Takes a humtab and identifies the pipe fields (columns), if any,
  # are in it.
  colnms <- colnames(humtab)
  
  pipefields  <- colnms[grepl('Pipe', colnms)]
  
  if (length(pipefields) != 0L) pipefields <- pipefields[order(as.numeric(stringr::str_extract(pipefields, '[0-9]+')))]
  
  pipefields
}

curPipeN <- function(humtab) {
          # A function used by pipeIn<-
          # identifies how many pipe fields (if any)
          # are already in a humtable.
          length(pipeFields(humtab))   
          
}




collapseHumtab <- function(humtab, n = 1L) {
  # This is the biggest part of pipeIn<-'s work.
  # It's the trickiest part of the process, shrinking
  # the existing humtable to match the size of the new fields.
  # 
  # This function really only passes information into collapse2n, which
  # is the really workhorse.
  # 
  humtab[ , Map(f = collapse2n, 
            .SD, # Each field/column
            colnames(humtab), # the field name
            lapply(humtab, class), # the class of each field (important because data.table doesn't like columns to change class
            n = n
            )] 
  }

collapse2n <- function(x, colname, class, n = 1) {
  # This is the function that actually collapses a field
  # (column in humtable) to a smaller length. 
  # Certain structural fields are treated differently,
  # so that (hopefully) things like record and spine numbers
  # still make sense after collapse.  
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
#' \code{\link[humdrumR:withinHumdrum]{with(in)Humdrum}},
#' included to parallel the \code{R} family of \code{\link[base:lapply]{_apply}} functions.
#' \code{humApply} uses \href{http://adv-r.had.co.nz/Computing-on-the-language.html}{non-standard evaluation}
#' to capture arguments fed to it without the user needing to make explicit 
#' \code{\link[base:tilde]{formula}} using \code{~}. This is only guaranteed to work 
#' in the \code{\link[base:environment]{global environment}}, so be careful. If you run into
#' problems, switch over to \code{\link[humdrumR:withinHumdrum]{with(in)Humdrum}} and use
#' explicit \code{\link[base:tilde]{X~formulas}}.
#' 
#' @param humdrumR A \code{\linkS4class{humdrumR}} data object.
#' @param FUN A function to apply to the \code{\link[humdrumR:humdrumR]{Active}} field(s)
#' in the \code{humdrumR} object.
#' @param ... Any arguments which can be fed to 
#' \code{\link[humdrumR:withinHumdrum]{with(in)Humdrum}} as formulae (except for
#' \code{do} expressions, which are replaced by the \code{FUN} argument!). 
#' However, rather that writinging formula in the format \code{Keyword ~ Expression},
#' \code{humApply} arguments should be written as normal \code{R} arguments: 
#' \code{Keyword = Expression}.
#' Unnamed arguments are ignored.
#' #' @param within A logical. If \code{TRUE} (the default), 
#' \code{\link[humdrumR:withinHumdrum]{withinHumdrum}} is used to apply the 
#' function---meaning that the output is reconstituted into a new field in the 
#' \code{humdrumR} object. If \code{within == FALSE},
#' \code{\link[humdrumR:withinHumdrum]{withHumdrum}} is used instead,
#' which results in the function's output being returned inprocessed.
#' @param doplot Boolean. If \code{TRUE} the \code{FUN} argument is treated
#' as a \code{doplot} expression by \code{\link[humdrumR:withinHumdrum]{with(in)Humdrum}},
#' so the result is ignored (for plotting or side-effects purposes).
#' 
#' @rdname withinHumdrum
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
