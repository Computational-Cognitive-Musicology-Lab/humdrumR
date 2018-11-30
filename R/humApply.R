# This file defines the function humApply, which is used to apply
# expressions to fields in a humdrumR data object.
#
# Only humApply itself is exported for users to use.
# All other functions are just used by humApply.
#
# humApply has two major parts to it:
#   1: Applying the desired expression to the humdrumR object,
#   with various specical options specified by the desired arguments (e.g., partition, ngram).
#   2: Taking the results of the expression and (re)assembling/inserting
#   it into the humdrumR object. This second part, is primarily
#   accomplished by the function putHumtab. 

#################################################-
############################ humApply ----
##################################################-



#' humApply
#' 
#' Apply arbitrary expressions to \code{\linkS4class{humdrumR}} data.
#' 
#' @section Overview:
#' This function is the primary means of working with
#' humdrumR data. It allows you to specify arbitrary 
#' expressions involving fields in a humdrumR data object.
#' It also includes a number of special application options:
#' \itemize{
#'   \item Applying an expression to separate subsets of the data.
#'   \item Applying an expression in only matching parts of the data.
#'   \item Applying an expression across windows in the data (e.g., ngrams, rolling windows).
#' }
#' 
#' @section Applying expressions:
#' The \code{expression} argument should be a formula. The expression on the 
#' right side of this formula is evaluate within the humdrumR data object. The expression
#' can, thus, refer to any field in the humdrumR object (Record, Token, File, etc.). 
#' You can also include a \code{.} anywhere in the expression, which will be 
#' interpreted as the humdrumR object's default \code{\link[dest=humdrumR]{Active}} 
#' expression.
#' Examples:
#' \preformatted{
#' humdata <- readHumdrum('directorywithdata/*.krn') # read some data
#' 
#' humApply(humdata, ~getPitch(Token)) # The most basic pattern
#' humApply(humdata, ~getPitch(.)) # Same as previous (unless \code{Active} field has been changed))
#' 
#' humApply(humdata, ~solfa(getPitch(Token), key = Key)) 
#' # Assumes that the Key field was parsed during the call to \code{\link{readHumdrum}}
#' 
#' humApply(humdata, ~getSemits(Token) - mean(getSemits(Token))) 
#' }
#' 
#' @section Partitioning:
#' The \code{partition} argument is used to either apply the expression separately to 
#' subsets of the data (for instance, applying a function to each file in the data) or
#' to apply the expression to only a part of the data (for instance, only Spine 3).
#' The first case is specified with the keyword \code{by}; the second, with the 
#' keyword \code{where}. Desired partitions are specified as a list of formulae, 
#' each formatted 
#' 
#' \preformatted{Keyword ~ Expression [~ Expr2 ~ Expr3 ~ ...]}
#' 
#' Each partition expression is evaluated with in the \code{humdrumR} data object. 
#' Most commonly, the partition expression is simply a field in the data: 
#' for instance, 
#' \preformatted{humApply(humdata,
#'                        expression,
#'                        partition = list(by ~ File))} 
#' will cause the \code{expression}
#' \code{humApply} is applying to be applied separately to each file in the 
#' humdrumR data. Similarly, 
#' \preformatted{humApply(humdata, expression,
#'                        partition = list(by ~ File ~ Spine))} 
#' will cause application across each file \emph{and} each spine. We can also do 
#' creative things like
#' 
#' \preformatted{humApply(humdata, expression,
#'                        partition = list(by ~ Spine > 3 | Record \%\% 2 == 0))}
#'                        
#' which will break the data into two groups, one where either the spine number is 
#' three or less \emph{or} the record number is even, and another group where the opposite is true. 
#' 
#' The \code{where} keyword differs from the \code{by} keyword as so: 
#' the partition expression associated with the \code{where} keyword must 
#' evaulate to a boolean (TRUE or FALSE), and the function is only applied 
#' to data fields where the where expression is \code{TRUE}. Wherever the 
#' where expression evaluates to \code{FALSE}, the original humdrumR data is 
#' returned unchanged.
#' 
#' The list of partitionaing formulae in the \code{partition} argument are applied
#' recursively, in order from left to right. Thus if you specify
#' \preformatted{humApply(humdata,
#'                        expression,
#'                        partition = list(by ~ File, where ~ semits > mean(semits)))}
#' the application \code{expression} will only be applied to data tokens where
#' the \code{semits} field is greater than the mean \code{semits} value
#' \emph{within that file}. Contrast this with this call:
#' \preformatted{humApply(humdata,
#'                        expression,
#'                        partition = list(where ~ semits > mean(semits), by ~ File))} 
#' wherein the \code{expression} is applied, again, across files, but only
#' where the semits field is greater than the mean value \emph{across all the data}.
#' 
#' @section Tandem interpretations:
#' The function \code{\link{readHumdrum}} automatically parses
#' tandem interpretations (that it recognizes) in read humdrum data into
#' their own fields. For instance, data with a \code{'*clefF4'} will show
#' up as a \code{Clef} field. However, users might read humdrum data with their
#' own custom tandem interpretations that are not built into \code{humdrumR}.
#' \code{humdrumR} includes the function \code{\link{getTandem}} to help us
#' extract arbitrary tandem intrpretation data.
#' Luckily, \code{humApply} knows some
#'  \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
#' which makes it easy to do this anywhere in our \code{expression} argument.
#' In your expression, simply put a named object beginning with the 
#' symbol \code{*}. Of course, R doesn't normally 
#' allow names to begin with symbols like \code{*}, but you can force it by
#' placing grave symbols around the name \code{`*name`}. If you do this in your
#' \code{expression} argument, \code{humApply} will treat this name as a 
#' regular expression and create a call \code{getTandem(Tandem, regular expression)}.
#' This means you can could do something like 
#' \preformatted{humApply(humdata, ~myFunction(Token, `*mytandempattern`))}
#' and \code{myFunction} will be called with the first argument being the 
#' \code{Token} field, and the second argument being tandem interpretations
#' which match \code{'mytandempattern'} (extracted from the \code{Tandem} field).
#' 
#' @section Splatting:
#' "Splatting" refers to feeding a function a list or vector of arguments.
#' Sometimes we want to divide our data into pieces (a l\'a \code{partition} option), but
#' rather than applying the same expression to each piece, we want to feed
#' the separate pieces as separate arguments to the same function.
#' 
#' In \code{humApply} you can use some 
#' \href{https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic}{syntactic sugar}
#' to do just this, using the \code{@} symbol in the format \code{myFunction(TargetExpr@GroupingExpr)}.
#' If we make this call
#' \preformatted{
#' humApply(humData, 
#'          ~myFunction(Token@Spine))
#' }
#' and  there are four spines
#' this is how \code{humApply} will intepret the \code{expression}:
#' \preformatted{
#' humApply(humData,
#'          ~myFunction(Token[Spine == 1], # first argument when Spine == 1
#'                      Token[Spine == 2], # second argument when Spine == 2
#'                      Token[Spine == 3], # etc.
#'                      Token[Spine == 4])) 
#' }
#' 
#' #' @section Argument specification:
#' If desired, all arguments to humApply except for \code{humdrumR} and \code{expression}
#' can be specified in the left hand side of the \code{expression} formula.
#' For this to work, the left hand side of \code{expression} must contain a list
#' of formulas, each specifying a keyword (argument) and value/expression.
#' For instance,
#' \preformatted{
#' humApply(humdata, ~table(.), partition = list(by ~ Spine))
#' }
#' and
#' \preformatted{
#' humApply(humdata, list(by ~ Spine) ~ table(.))
#' }
#' are equivalent.
#' If arguments are specified in the formula, they supercede arguments fed to the function.
#' This feature is useful for saving more complex combinations of expressions and arguments.
#' 
#'     
#' @param humdrumR A humdrumR data object.
#' @param expression A formula (or a function). If fed a function this function
#' is coerced to a formula as \code{~func(.)}.
#' The expression on the right side of the formula
#' is evaluated inside the humdrumR internal data.table (humtable). The left side of the formula
#' can be use as an alternative way of specifying other arguments to humApply (see details). 
#' There are a number of useful options and syntactic
#' treats which make these expressions extra powerful (see details).
#' @param partition A list of formulae in the format 
#' \preformatted{Keyword ~ Expression [~ Expr2 ~ Expr3 ~ ...]} (see details).
#' Known keywords are \code{by} and \code{where}.
#' @param tee A boolean. This controls whether the results of the expression application are
#' captured and incorporated into a new humdrumR data object. If \code{tee = TRUE}, the results are not
#' captured. This can be if you want to call a command for it's side-effects (i.e., plotting, writing a file), 
#' but don't want to keep the output.
#' @param ngrams An integer. This argument causes humApply to apply it's expression across ngrams in the data.
#' The integer specified is interpreted as the desired ngram length. If NULL, ngrams are not applied (the default).
#' @param windows An argument which allows you to specify more specific windows to apply within.
#' @param applyTo A string. Controls which type of tokens to apply expression to. Defaults to "D" (non-null data tokens).
#' Other options include "I" (interpretation tokens), "M" (measure tokens), and "C" (comment tokens).
#' @param graphics A list. Changes plotting settings within the scope of the humApply call. 
#' Useful when plotting within humApply. For instance, to specify that you want to create four plots in a 2x2
#' grid, specify \code{graphics = list(mfcol = c(2,2))}.
#' 
#' @examples 
#' humdata <- readHumdrum('directorywithdata/*.krn')
#' 
#' humApply(humdata, ~nchar(.)) # counts characters in each data token.
#' humApply(humdata, ~table(.), partition = list(by ~ Spine)) # Tabulates data tokens in each Spine.
#' 
#' @return A new humdrumR data object.
#' 
#' @export
humApply <- function(humdrumR, 
                     expression,
                     partition = list(),
                     tee = FALSE,
                     ngrams = NULL,
                     windows = NULL,
                     applyTo = 'D', 
                     graphics = list()) {
          
          if (class(humdrumR) != "humdrumR") stop('humApply(humdrumR = ) argument must be a humdrumR object.')
          if (is.function(expression)) {
                    expenv <- list2env(list(.func = expression))
                    parent.env(expenv) <- parent.env(environment())
                    expression <- ~ .func(.)
                    lazyeval::f_env(expression) <- expenv
          }
          if (!lazyeval::is_formula(expression)) stop('In humApply(formula = ) argument must be a formula or a function.')
          
          
          
          humtab <- getHumtab(humdrumR, applyTo)
          
          ### parse option arguments
          # read them from formula, if they are there
          formulaArgs <- lazyeval::f_eval_lhs(expression)
          if (!is.null(formulaArgs) && is.list(formulaArgs) && all(sapply(formulaArgs, is.formula))) {
                    formulaArgs <- parsePartitionList(formulaArgs)
                    if (any(names(formulaArgs) %in% c('by', 'where'))) {
                              partition <- formulaArgs[names(formulaArgs) %in% c('by', 'where')]
                              formulaArgs <- formulaArgs[!names(formulaArgs) %in% c('by', 'where')]
                    }
                    
                    if (length(formulaArgs) != 0) {
                     list2env(formulaArgs, envir = environment()) 
                     # remaining arguments from formula are saved into environment,
                     # overwriting any arguments (like ngrams) that might have been there).
                              
                     ngrams <- eval(ngrams) 
                    }
          }
          
          
          if (lennot0(graphics)) {
                    oldpar <- par(no.readonly = TRUE)
                    on.exit(par(oldpar))
                    do.call('par', graphics)
          }
          
          partition <- c(humdrumR@Partition, parsePartitionList(partition))
          
          #### create main expresion
          funcform <- parseForm(humtab, expression, humdrumR@Active, ngrams)
          humtabFunc  <- humtableApplier(funcform, captureOutput = !tee)
          
          #### evaluate expression 
          
          newhumtab <- if (len0(partition)) {
                    humtabFunc(humtab)
          } else {
                    partApply(humtab, partition, humtabFunc)
          }
          
          
          #### Put new humtable back into humdrumR object
          
          # if we mixed record types, the newhumtab will not know
          # where to be put back
          # This section should be improved (Nat, 11/29/2018)
          if (any(is.na(newhumtab$Type))) {
                    newtype <- if ('D' %in% applyTo) 'D' else applyTo[1]
                    newhumtab$Type[is.na(newhumtab$Type)] <- newtype
          }
          
          putHumtab(humdrumR, drop = TRUE) <- newhumtab
          
          ########### Update other slots in humdrumR object
          
          # Now that the Humtable is taken care of,
          # tell the humdrumR object about the new layers and set the Active formula.
          newlayers <- colnames(newhumtab)[!colnames(newhumtab) %in% colnames(humtab)]
          # newlayers <- newhumtab[['newlayers']]
          # if (!is.null(newlayers)) newhumtab[ , newlayers := NULL]
          if (length(newlayers) > 0) {
                    addLayers(humdrumR)  <- newlayers
                    if (any(names(partition) == 'where')) {
                              act <- ifelsecalls(partition['where'], c(lazyeval::f_rhs(humdrumR@Active), lapply(newlayers, as.symbol)))
                              humdrumR@Active <- lazyeval::f_new(act)
                    } else {
                              humdrumR <- setActiveString(humdrumR, newlayers) 
                    }
          }
          
          humdrumR 
}

###########- Applying humApply's expression to a data.table

humtableApplier <- function(funcform, captureOutput = TRUE) {
          #' This function operates within humApply.
          #' It takes an expression created by parseForm 
          #' and turns it into a function which can be applied 
          #' to a Humtable (a data.table). 
          #' It's second argument determines if the result of the 
          #' expression application is put back into the Humtable.  #' table.
          function(humtab) {
                    # output <- eval(funccall, envir = humtab)
                    output <- lazyeval::f_eval_rhs(funcform, data = humtab)
                    if (captureOutput) pipeIn(humtab) <- output
                    humtab
          }
}


####################- Parsing expressions fed to humApply


parseForm <- function(humtab, funcform, active, ngram = NULL) {
  #' This is the main function used by \code{\link{humApply}} to prepare
  #' the \code{expression} argument for application to a \code{\linkS4class{humdrumR}}
  #' object.
  
  funccall <- lazyeval::f_rhs(funcform)
  
  # turn . to active formula
  funcform <- activateForm(funcform, active)
  
  # tandem interpretations
  funcform <- tandemsForm(funcform)
  
  # splats
  funcform <- splatForm(funcform)
  
  # find what layers (if any) are used in formula
  usedInExpr <- unique(layersInFormula(humtab, funcform))
  
  if (len0(usedInExpr)) { stop("The humformula argument in your call to humApply doesn't reference any layers in your humdrum data.\n A
                               dd a layer somewhere or add a dot (.), which will automatically grab the default, 'active' layer.",
                                call. = FALSE)}

  # if the targets are lists, Map
  lists <- sapply(humtab[ , usedInExpr, with = FALSE], class) == 'list'
  if (any(lists)) funcform <- mapifyForm(funcform, usedInExpr)
    
  # if ngram is present
  if (!is.null(ngram)) funcform <- ngramifyForm(funcform, ngram, usedInExpr)
  
  funcform
}

####################### Functions used inside parseForm

activateForm <- function(funcform, active) {
  #' This function takes the \code{expression} argument
  #' from the parent \code{\link{humApply}} call and 
  #' inserts the \code{Active} expression from the 
  #' target \code{\linkS4class{humdrumR}} object in place 
  #' of any \code{.} subexpressions.
  active <- lazyeval::f_rhs(active)
  swapIn(list(. = active), funcform)
}

#### Interpretations in expressions

tandemsForm <- function(funcform) {
 # This function inserts calls to getTandem
 # into an expression, using any length == 1 subexpression
 # which begins with `*` as a regular expression.
 applyExpr(lazyeval::f_rhs(funcform),
           function(ex) {
             exstr <- deparse(ex)
             interp <- grepl('^\\*[^*]', exstr)
             
             if (interp) {
               regex <- stringr::str_sub(exstr, start = 2L)
               call('getTandem', quote(Tandem), regex)
             } else {
               ex
             }
           }) -> newcall
  
  lazyeval::f_new(newcall, env = lazyeval::f_env(funcform))
  
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
  stringr::str_sub(matches, 2L, -2L) %>% unique
  
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
# call to humApply. We can group one field by another field, then
# feed each group as an argument to a function.
#

splatForm <- function(funcform) {
  #' This function takes an expression,
  #' and replaces any subexpression of the form \code{funccall(TargetExpr@GroupingExpr)},
  #' with \code{do.call('funccall', tapply(TargetExpr, GroupingExpr, c))}.
  #' The result is that \code{TargetExpr} is broken into a list of vectors by the
  #' \code{GroupingExpr}, and each group is fed to \code{funccall} as a separate
  #' argument. See the docementation for \code{\link{humApply}}.
  funccall <- lazyeval::f_rhs(funcform)
  
  funccall <- parseAts(funccall)
  
  lazyeval::f_new(funccall, env = lazyeval::f_env(funcform))
  
}

parseAts <- function(expr) {
 #' This is used by \code{\link{splatsForm}}.
 #' It identifies subexpressions which contain \code{@}
 #' and applies \code{\link{parseAt}} appropriately.
  if (len1(expr)) return(expr)
  
 callhead <- deparse(expr[[1]])
  
 if (callhead == '@') {
   list(as.list(expr[-1]) %class% 'splat')
   
 } else {
   new <- list()
   for (i in 2:length(expr)) {
     new <- c(new,  Recall(expr[[i]]))
   }
   
   names(new) <- names(as.list(expr))[-1]
   
   ats <- sapply(new, inherits, what = 'splat')
   if (any(ats)) {
     parseAt(callhead, new[ats], new[!ats])
   } else {
     do.call('call', c(callhead, new), quote = TRUE)
   }
 }
}

parseAt <- function(funccall, splats, otherargs) {
 #' This does the real work for \code{\link{splatForm}}.
 #' It replaces the \code{funccall(TargetExpr@GroupingExpr)} form
 #' with the \code{do.call('funccall', tapply(TargetExpr, GroupingExpr, c)} form.
 #' It also passed additional (non-splatted) arguments (\code{otherargs}) in.
  splats <- lapply(splats,
                   function(splat) funccall('unname', do.call('call', quote = TRUE,
                                           c('tapply', splat, quote(c)))))
  call('do.call', funccall, do.call('call', c('c', splats, otherargs), quote = TRUE))
 }

########## Mapping expression across list fields.

xifyForm <- function(expression, usedInExpr) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which creates an lambda function which takes those names
          #' as arguments and calls the expression with them.
          #' This lambda function is appropriate for calling with
          #' Map, lapply, ngramApply, etc.
          #' This is used by listifyForm and ngramifyForm.
          fargs <- as.pairlist(alist(x = )[rep(1, length(usedInExpr))])
          names(fargs) <- '.' %str+% tolower(usedInExpr)
          
          expression <- swapIn(setNames(lapply(names(fargs), as.symbol), usedInExpr), 
                               expression)
          
          lambdaexpression      <- quote(function() {} )
          lambdaexpression[[2]] <- fargs
          lambdaexpression[[3]] <- lazyeval::f_rhs(expression)
          
          lazyeval::f_new(lambdaexpression, env = lazyeval::f_env(expression))
          
}


mapifyForm <- function(expression, usedInExpr) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which uses Map to call this expression across these named objects.
          #' (It presumes that the named objects are actually lists).
          #' It first uses xifyForm to put the expression in the form of a 
          #' lambda function.
  expression <- xifyForm(expression, usedInExpr)
  
  mappedexpression <- lazyeval::f_rhs(expression)
  mappedexpression <- do.call('call', quote = TRUE,
                      as.list(c(list('Map', mappedexpression), 
                                parse(text = usedInExpr))))
  
  lazyeval::f_new(mappedexpression, env = lazyeval::f_env(expression))
}

ngramifyForm <- function(expression, n, usedInExpr) {
          #' This function takes an expression and a vector of strings representing
          #' names used in that expression and creates an expression
          #' which uses applyNgram on these named objects.
          #' It first uses xifyForm to put the expression in the form of a 
          #' lambda function.
  expression <- xifyForm(expression, usedInExpr)
  
  ngramexpression <- lazyeval::f_rhs(expression)
  
  ngramexpression <- call('applyNgram', 
                      n, 
                      do.call('call', quote = TRUE,
                              c('list', lapply(usedInExpr, as.symbol))),
                      ngramexpression)
  
  lazyeval::f_new(ngramexpression, env = lazyeval::f_env(expression))
  
}



################################## Applying expressions across partitions


partApply <- function(humtab, partitions, humfunc) {
  if (len0(partitions)) return(humfunc(humtab))
          
  funccall <- if (len1(partitions)) quote(humfunc(.SD)) else call('partApply', quote(humtab), quote(partitions[-1]), quote(humfunc))
  
  curpart <- partitions[[1]]
  
  partcall <- call('[', quote(humtab), alist(i=)[[1]], funccall)
  if (names(partitions)[1] %in% c('by', 'groupby', 'each')) {
    partcall[['by']]      <- curpart
    partcall[['.SDcols']] <- quote(colnames(humtab))
    # get environment from formulae
    env <- environment()
    # parent.env(env) <- lazyeval::f_env(partitions[[1]])
    
    # do it!
    output <- eval(partcall, envir = env)
    
    output <- output[ , !duplicated(colnames(output)), with = FALSE]
  } else {
    partcall[[3]] <- curpart
    output <- eval(partcall)
    
    negatecall <- partcall[1:3]
    negatecall[[3]] <- call('!', call('(', curpart))
    
    rest <- eval(negatecall)
    
    output <- rbind(output, rest, fill = TRUE)
  
  }
  
  output
}



parsePartitionList <- function(l) {
          # This function is simply syntactic sugar to
          # make specifying partitions in a call to humApply
          # a little bit more concise.
          # It takes a list of formulas and applies parsePartitionFormula.
          names(l) <- NULL
          unlist(lapply(l, parsePartitionFormula))
}

parsePartitionFormula <- function(form) {
          # This function is simply syntactic sugar to
          # make specifying partitions in a call to humApply
          # a little bit more concise.
          # This function takes a formula formatted
          # Keyword ~ expr1 [~ expr2 ~ expr3 ...] and translates it to a list
          # like list(Keyword = ~ .(expr1[, expr2, expr3, ...]).
          # If the keyword is missing (there is no left side of formula), it defaults to "by".
          exprs <- unwrapFormula(form)
          
          if (length(exprs) == 1) { 
                    keyword <- quote(by) # if keyword is empty, default to 'by.'
          } else {
                    keyword <- exprs[[1]]
                    exprs <- exprs[-1]
          }
          # check that keyword is actually a single legal R name, not an expression
          if (length(keyword) != 1L || !is.name(keyword)) stop("Your partition formula (in a call to humApply) contains a 
                                                  keyword (far left side of formula) which can't be parsed as a name.")
          
          exprs <- do.call('call', c('.', exprs), quote = TRUE)
          setNames(list(exprs), as.character(keyword))
          
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
          if (lenvalue > nrow(humtab)) stop("Sorry, humApply doesn't currently support functions/expressions that return values that are longer than their input.", .call = FALSE)
          
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
          nnewlayers   <- length(value)
          targetlayers <- if (allnamed(value)) names(value) else paste0('Pipe', (curpipen + seq_len(nnewlayers)))
          
          humtab[ , targetlayers] <- value
          
          #humtab[ , newlayers := paste(targetlayers, collapse = ' ')]
          
          humtab
          
} 




ifelsecalls <- function(calls, layers) {
          # function used within humApply, as part of 
          # humdrumR (re)assembly process.
          call <- call('ifelse', lazyeval::f_rhs(calls[[1]]), 
                       quote(.), layers[[1]])
          if (len1(calls)) {
                    call[[3]] <- layers[[2]]
          } else {
                    call[[3]] <- Recall(calls[-1], layers[-1])
          }
          
          call
}

pipeLayers <- function(humtab) {
  #' Another function used by pipeIn<-
  #' Takes a humtab and identifies the pipe fields (columns), if any,
  #' are in it.
  colnms <- colnames(humtab)
  
  pipelayers  <- colnms[grepl('Pipe', colnms)]
  
  if (lennot0(pipelayers)) pipelayers <- pipelayers[order(as.numeric(stringr::str_extract(pipelayers, '[0-9]+')))]
  
  pipelayers
}

curPipeN <- function(humtab) {
          #' A function used by pipeIn<-
          #' identifies how many pipe fields (if any)
          #' are already in a humtable.
          length(pipeLayers(humtab))   
          
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
                     'StopNumber', 'Bar', 'DoubleBarline', 'NData', 'NFile')) {
    uniqx[1:n]
  } else {
    rep( if (length(uniqx) == 1) uniqx else as(NA, class) , n)
  }
  
}
