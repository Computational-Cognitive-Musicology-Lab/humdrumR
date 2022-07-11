# This file defines the functions with.humdrumR and within.humdrumR, which are used to apply
# expressions to fields in a humdrumR data object in a manner analogous to the base
# with and within functions (as applied to data.frames). There is also the function apply2Humdrum
# which acts more like R lapply, but is really just using within.humdrumR.
#
# apply2Humdrum, with.humdrumR, within.humdrumR, and inHumdrum (an alias of withinHumdrum) 
# are each exported for users to use.
# getTandem is also exported.
# All other functions are just used by with/within.humdrumR.
#
# within.humdrumR has two major parts to it:
#   1: Applying the desired expression to the humdrumR object,
#   with various specical options specified by the desired arguments (e.g., partition, ngram).
#   2: Taking the results of the expression and (re)assembling/inserting
#   it into the humdrumR object. This second part, is primarily
#   accomplished by the function putHumtab. 
# with.humdrumR skips the second step!

#################################################-
# within.humdrumR ----
##################################################-



#' with(in)Humdrum
#' 
#' Apply arbitrary expressions to fields within `[humdrumR][humdrumRclass]` data.
#' 
#' @section Overview:
#' 
#' These functions are the primary means of working with
#' humdrumR data. (They are analogous to the base functions
#' `[base][with]` and `[base][within]`
#' as applied to `[base:data.frame][data.frames]`.)
#' Specifically they allow you to evaluate arbitrary
#' expressions involving fields in a humdrumR data object.
#' They also includes a number of special evaluation options:
#' 
#' * Evaluate an expression in only matching parts of the data.
#' * Evaluate an expression separately in subsets of the data.
#' * Evaluate an expression across windows in the data (e.g., ngrams, rolling windows).
#' * Evaluate an expression which produces a plot, with particular plotting parameters set using `[graphics][par]`.
#' 
#' 
#' The difference between `with.humdrumR` and `within.humdrumR` is
#' analogous to the difference between `[base][with]` and `[base][within]`.
#' `with.humdrumR` evaluates your expression(s) and then simply returns the result of
#' the evaluation. `within.humdrumR` evaluates your expression(s) and then
#' (attempts) to insert the results back into the humdrumR object, generating new
#' fields called `ResultX` (see details).
#' 
#' `inHumdrum` is simply a short hand for `within.humdrumR`.
#' 
#' @section `Formulae`:
#' Every formula in the `formulae` argument 
#' is treated as a `Keyword ~ Expression(s)`
#' pairing. Multiple expressions can be input using multiple `~` operators:
#' `Keyword ~ Expression1 [~ Expression2 ~ ... ~ ExpressionN]`
#' (the leftmost expression is treated as the keyword.)
#' If there is no leftmost expression (i.e., `~ Expression`), the Keyword
#' defaults to "`do`." The keyword expression must be a single, simple name/symbol, following 
#' standard R rules (i.e., "`.foobar`" is acceptable but "`3 + foobar`" is not).
#' 
#' Legal keywords, and their meanings are:
#' 
#' 1. `do` An expression to be evaluated within the `humdrumR` data object (see "Expression evaluation").
#' 2. `doplot` An expression to be evaluated within the `humdrumR` data object while ignoring the result of the expression (see "Expression evaluation" and "Plotting".
#' 3. `by` An expression used to break the data into groups, with the `do` expression(s) evaluated 
#'   separately in each group (see "Partitioning").
#' 4. `where` An expression indicating a subset of the data in which to evaluate the `do` expression (see "Partitioning").
#' 5. `ngrams` A positive number *n*. The expression is evaluated across overlapping length-*n* windows.
#' 6.  `recordtypes` A string or vector of characters drawn from `c("D", "d", "I", "L", "M","G")`. These characters
#' correspond to types of humdrum records: **D**ata, null **d**ata, **I**nterpretations, 
#'   **M**easures, **L**ocal comments, and **G**lobal comments respectively. The expression
#'   is only evaluated on data drawn from the specified record types (defaults to `"D"`).
#' 7. `pre` An expression to evaluate once before evaluating the do expression(s). Useful, for instance, for taking logs
#'   or opening a graphing window. The `pre` expression is evaluated in the global environment.
#' 8. `post` An expression evaluate once after evaluating the do expression(s). Always evaluated in the global environment.
#' 
#' 
#' @section Expression evaluation:
#'
#' The right-hand side of any formula in the `formulae` argument with the keyword `do` or `doplot` 
#' (or with no keyword specified) is evaluated within the `humdrumR` data object.
#' The expression can, thus, refer to any field in the humdrumR object (Record, Token, File, etc.). 
#' You can also include a `.` anywhere in the expression, which will be 
#' interpreted as the humdrumR object's current `[dest=humdrumR][Active]` 
#' expression.
#' 
#' ```
#' humdata <- readHumdrum('directorywithdata/*.krn') # read some data
#' 
#' within(humdata, ~getPitch(Token)) # The most basic pattern
#' within(humdata, ~getPitch(.)) # Same as previous (unless `Active` field has been changed))
#' 
#' within(humdata, ~solfa(getPitch(Token), key = Key)) 
#' # Assumes that the Key field was parsed during the call to `[readHumdrum][readHumdrum]`
#' 
#' within(humdata, ~getSemits(Token) - mean(getSemits(Token))) 
#' 
#' ```
#' 
#' If multiple `do` expressions are provided, they are each evaluated one at a time,
#' with the result of each piped into the next. Other, non-`do`, formulae (like `by~` or 
#' `ngrams~`) are reused for each expression evaluated.
#' 
#' @section Partitioning:
#' 
#' A `by` expression is used to break the data into subsets, with the `do` expression(s) evaluated 
#' separately within each subset. This works the similarly to the `by` argument in 
#' `[data.table][data.table]`s, the `INDEX` 
#' argument of `[base][tapply]`, or the `INDICES` argument of `[base][by]`.
#' Each `by` expression must evaluate, within the `humdrumR` data object, to a vector (or a list of vectors 
#' of equal length) of categories to group the data by.
#' Most commonly, the `by` expression(s) are simply field(s) in the data: 
#' for instance, 
#' 
#' ```
#' within(humdata,
#'          do ~ table(Token),
#'          by ~ File)
#' ```
#'
#' will apply the function `[base][table]` to the `Token` field
#' *separately* for each file in the `humdrumR` data. 
#' However, we can also use more complex expressions like
#' 
#' ```
#' within(humdata,
#'          do ~ table(Token), 
#'          by ~ Spine > 3 | Record \%\% 2 == 0)
#' ```
#'
#' which will evaluate the do expression in two groups, one where either the spine number is 
#' three or less *or* the record number is even, and another group where the opposite is true. 
#' 
#' If the `by` expression evaluates to a list of grouping vectors,
#' the `do` expressions are evaulated across every combination of categories in all the vectors.
#' Thus,
#' within(humdata, 
#'          do ~ table(Token),
#'          by ~ list(File, Spine))
#' will apply `table` to `Token` across each spine *in* each file.
#' As some [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic), if the 
#' `by` expression has more than two parts, all parts except 
#' the (leftmost) keyword part are combined in a list (i.e., `by ~ File ~ Spine` 
#' becomes `by ~ list(File, Spine)}`.
#' Thus the previous example can also be written:
#' within(humdata, 
#'          do ~ table(Token),
#'          by ~ File ~ Spine)
#'                      
#' A `where` expression is used to identify a subset of the data and evaluate
#' the `do` expression(s) *only* in that subset. 
#' `where` expressions must evaluated, within the `humdrumR` data object, to
#' a single logical vector. The `do` expression(s) are only evaluated where this logical
#' vector is `TRUE`.
#' Wherever the `where` expression evaluates to `FALSE`, the original `humdrumR` data is 
#' kept unchanged.
#' 
#' If multiple partitioning formulae (i.e, `by` and `where`) expressions
#' are evaluated recursively, in order from left to right. Thus if you specify
#' within(humdata,
#'          do ~ sd(semits),
#'          by ~ File, 
#'          where ~ semits > mean(semits))
#' a the standard deviation of the `semits` field will be calculated only in each file,
#' but only where the `semits` field is greater than the mean `semits` value
#' *within that file*. Contrast this with this call:
#' within(humdata,
#'          do ~ sd(semits)
#'          where ~ semits > mean(semits), 
#'          by ~ File) 
#' wherein the standard deviation of `semits` is, again, calculated for each file,
#' but this time wherever the `semits` field is greater than the mean value *across all the data*.
#' 
#' @section Plotting:
#' The `doplot` keyword behaves exactly like the `do` keyword, except that the result of the
#' evaluation is ignored. This is useful for plotting *as well as* other side-effects (like writing to a file).
#' If `doplot` is used with `with.humdrumR`, the function simply returns `NULL` (after executing the `doplot`
#' expression
#' If `doplot` is used with `within.humdrumR` (or `inHumdrum`), the function simply returns the unaltered
#' `humdrumR` argument.
#' 
#' `within.humdrumR` also allows you to specify plotting options in line, without having to make a separate call
#' to `[graphics][par]`. Any `[graphics][par]` argument can be specified as a `Keyword ~ Expression` pair
#' in the `formulae` argument. For instance, if you call a `doplot` expression with a `by` expression
#' that creates four groups, R will create four plots---but you will only see the last one! Normally, you would need to
#' call `par(mfcol = c(2,2))` *before* calling your plotting function. However, with `within.humdrumR` you can
#' soecific `mfcol = c(2,2)` right in a `formulae` formula:
#'              within(humdata,
#'                       doplot ~ fooplot(.),
#'                       by ~ list(Two, byTwo),
#'                       mfcol ~ c(2, 2))
#' The best part is `within.humdrumR` will reset `par` to it's previous state after `withinHumdrum` is done.
#' 
#' 
#' @section Tandem interpretations:
#' 
#' The function `[readHumdrum][readHumdrum]` automatically parses
#' tandem interpretations (that it recognizes) into
#' their own fields in the resulting `[humdrumR][humdrumRclass]` data.
#' For instance, data with a `'*clefF4'` will show
#' up as a `Clef` field. However, users might read humdrum data with their
#' own custom tandem interpretations that are not built into `humdrumR`.
#' `humdrumR` includes the function `[getTandem][getTandem]` to help us
#' extract arbitrary tandem intrpretation data.
#' Luckily, `within.humdrumR` knows some
#'  [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic)
#' which makes it easy to do this anywhere in our expressions, simply by putting a 
#' named object beginning with the symbol `*`. Of course, R doesn't normally 
#' allow names to begin with symbols like `*`, but you can force it by
#' placing grave symbols around the name ``*name``. If you do this in a `within.humdrumR`
#' expression, `within.humdrumR` will treat this name as a 
#' regular expression and substitute a call `getTandem(Tandem, 'regular expression')` in the expression.
#' This means you can could do something like 
#' within(humdata, 
#'          do ~ myFunction(Token, `*mytandempattern`))
#' and `myFunction` will be called with the first argument being the 
#' `Token` field, and the second argument being tandem interpretations
#' which match `'mytandempattern'` (extracted from the `Tandem` field).
#' 
#' @section Splatting:
#' 
#' ("Splatting" refers to feeding a function a list/vector of arguments.)
#' Sometimes we want to divide our data into pieces (a l\'a `partition` option), but
#' rather than applying the same expression to each piece, we want to feed
#' the separate pieces as separate arguments to the same function.
#' In `within.humdrumR` you can use some 
#' [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic)
#' to do just this, using the `@` symbol in the format `myFunction(TargetExpr@GroupingExpr)`.
#' If we make this call
#'
#' within(humdata, 
#'          do ~ myFunction(Token@Spine))
#'          
#' and there are four spines
#' this is how `within.humdrumR` will intepret the expression:
#' 
#' within(humData,
#'          do ~ myFunction(Token[Spine == 1], # first argument when Spine == 1
#'                          Token[Spine == 2], # second argument when Spine == 2
#'                          Token[Spine == 3], # etc.
#'                          Token[Spine == 4])) 
#' 
#' 
#' @section Argument interpolation:
#' 
#' Any named arguments to `within.humdrumR` are `[humdrumR:interpolateArguments][interpolated]` into the
#' `do` expressions. This is useful if you've already created a list of formulas that you like, but would like
#' to make small changes to a function call within the `do` expressions, without starting from scratch.
#' Examples:
#' 
#' ```
#' mycommand <- c(do ~ mean(., na.rm = TRUE), by ~ Spine ~ File)
#' within(humdata,
#'               mycommand,
#'               na.rm = FALSE)
#' # mycommand is executed with na.rm changed to FALSE              
#' ```
#' 
#' 
#' @section Piping:
#' 
#' For calls to `within.humdrumR`, the result of each `do` expression
#' is insterted back into the `[humtable][humdrum table]`. The results
#' are put into new field(s) labeled Result1, ResultX, ..., ResultN. If the results
#' of the expression are shorter than the rows in the [humtable][humdrum table],
#' or an `object`, the humdrum table is shrunk to fit them.
#'     
#' @param humdrumR A `[humdrumR][humdrumRclass]` data object.
#' @param ...  `...` arguments to `within.humdrumR` are divided into either named or unnamed arguments.
#' Unnamed arguments must be formulas, functions---lists of formulas/functions, no matter how deeply nested, are flattened
#' to a single list of functions/formulas.
#' All functions are coerced to a formula as `~foo(.)`. The far left-hand side of each formula
#' must be a name/symbol. Named arguments are [humdrumR:interpolateArguments][interpolated] into and `do~X` formulas.

#' 
#' @param ... Additional formulas/functions, or lists of formulas/functions.
#' These are all simply appended to the `formulae` argument.
#' 
#' @param drop This argument is concetually similar to the `drop` argument in R matrices and data.frames.
#' If `drop = TRUE`, the output of `with.humdrumR` is simplified as much as possible (trying to return
#' the "raw" vector, list, table, etc. within it). If `drop = FALSE`, the result is *always*
#' a `data.table`. The default value (`drop = TRUE`) is usually what we want because it is more
#' intuitive, but in more complex code, it can be helpful to set `drop = FALSE` so that 
#' the output is consistent.
#' 
#' @examples 
#' humdata <- readHumdrum('directorywithdata/*.krn')
#' 
#' within(humdata, ~nchar(.)) # counts characters in each data token.
#' within(humdata, ~table(.), by ~ Spine) # Tabulates data tokens in each Spine.
#' 
#' @return From `within.humdrumR` and `inHumdrum`, a new humdrumR data object.
#' From `with.humdrumR`, whatever value is returned by expression.
#' 
#' @name withinHumdrum
NULL


#' @rdname withinHumdrum
#' @export
with.humdrumR <- function(data, ..., 
                          drop = TRUE,
                          variables = list()) {
  
  checkhumdrumR(data, 'with.humdrumR')
  list2env(withHumdrum(data, ..., variables = variables, withFunc = 'with.humdrumR'), envir = environment())
  
  visible <- attr(result, 'visible') 
  result[ , `_rowKey_` := NULL]
  
  ####-
  ### Do we want extract the results from the data.table? 
  if (drop && ncol(result) == 1L) {
    result <- result[[1L]]
    if (is.list(result) && length(result) == 1L) result <- result[[1]]
  } 
  
  attr(result, 'visible') <- NULL
  
  if (visible) result else invisible(result)
  
}

#' @rdname withinHumdrum
#' @export
within.humdrumR <- function(data, ..., variables = list()) {
  checkhumdrumR(data, 'within.humdrumR')
  list2env(withHumdrum(data, ..., variables = variables, withFunc = 'within.humdrumR'), envir = environment())
  
  
  if (all(quoTab[KeywordType == 'do', Keyword == 'dofx'])) return(data)
  
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
  
  # number new results
  unnamedresult <- which(colnames(newhumtab) == 'Result')
  assign <- tail(quoTab[KeywordType == 'do' & Keyword != 'dofx' & !is.na(AssignTo), AssignTo], 1)
  if (length(assign)) {
    colnames(newhumtab)[unnamedresult[1]] <- assign[[1]]
    unnamedresult <- unnamedresult[-1]
  }
  
  if (length(unnamedresult)) colnames(newhumtab)[unnamedresult] <- paste0('Result', curResultN(humtab) + seq_along(unnamedresult))
  
  
  #### Put new humtable back into humdrumR object
  newfields <- setdiff(colnames(newhumtab), colnames(humtab))
  
  notnull <- Reduce(`|`, lapply(newhumtab[, newfields, with = FALSE], \(field) if (is.logical(field)) logical(length(field)) else  !(is.na(field) | field == '.')))
  newhumtab$Null[notnull] <- FALSE
  newhumtab$Type[newhumtab$Type == 'd' & notnull] <- 'D'
  
  # What do do if d is in recordtypes
  if (all(recordtypes == 'd') && all(newhumtab$Type == 'D')) {
    humtab[ , `_rowKey_` := NULL]
    newhumtab <- rbind(newhumtab, getHumtab(humdrumR, 'D'), fill = TRUE)
  }
  if (any(grepl('d', recordtypes))) {
    humdrumR@Humtable$d <- humdrumR@Humtable$d[FALSE]
  }
  
  putHumtab(humdrumR, drop = TRUE) <- newhumtab
  
  # Now that the Humtable is taken care of,
  # tell the humdrumR object about the new fields and set the Active formula.
  if (length(newfields)) {
    ## Add fields to humtab
    addFields(humdrumR) <- newfields
    
    humdrumR <- setActiveFields(humdrumR, newfields) 
    # humdrumR <- fillFields(humdrumR, from = 'Token', newfields)
  }
  
  humdrumR
  
}

withHumdrum <- function(humdrumR, ..., variables = list(), withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  
  # interpret ... arguments
  quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  
  # "pre" stuff
  oldpar <- par(no.readonly = TRUE) 
  quoTab <- evalPrePost(quoTab, 'pre')
 
  
  # Getting the humtab with the right record types.
  recordtypes <- if (any(quoTab$Keyword == 'recordtypes')) quoTab[Keyword == 'recordtypes']$Quo[[1]] else 'D'
  quoTab <- quoTab[Keyword != 'recordtypes']
                      
  humtab <- getHumtab(humdrumR, recordtypes)
  
  ### Preparing the "do" expression
  do   <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = FALSE)
  ordo <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = TRUE)
  # 

  
  #### evaluate "do" expression! 
  humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
  
  
  result <- evalDoQuo(do, humtab, 
                        quoTab[KeywordType == 'partitions'], 
                        ordo)
  
  if (nrow(result) > 0L) data.table::setorder(result, `_rowKey_`)
  
  # "post" stuff
  evalPrePost(quoTab, 'post')
  par(oldpar[names(oldpar) != 'new'])
  
  list(humdrumR = humdrumR, 
       humtab = humtab,
       quoTab = quoTab,
       recordtypes = recordtypes,
       result = result)
}

#' @rdname withinHumdrum
#' @export
In <- within.humdrumR
    
## Parsing Args ----

parseArgs <- function(..., variables = list(), withFunc) {
  quos <- rlang::enquos(...)
  argnames <- .names(quos)
  
  quos <- lapply(quos, \(quo) {
    quoA <- analyzeExpr(quo, stripBrackets = TRUE)
    if (quoA$Head == '{' && length(quoA$Args) == 1L) {
      quo <- quoA$Args[[1]]
      quoA <- analyzeExpr(quo, stripBrackets = TRUE)
    }
    
    keyword <- 'do'
    assign <- NA
    
    if (quoA$Type == 'symbol' ) {
      evaled <- try(rlang::eval_tidy(quo), silent = TRUE)
      
      if (class(evaled) == 'function') {
        quoA$Args[[1]] <- quote(.)
        quoA$Type <- 'call'
        quo <- unanalyzeExpr(quoA)
      }
      if (class(evaled) == 'formula') {
        if (!is.null(rlang::f_lhs(evaled))) keyword <- as.character(rlang::f_lhs(evaled))
        quo <- rlang::new_quosure(rlang::f_rhs(evaled), rlang::f_env(evaled))
        quoA <- analyzeExpr(quo, stripBrackets = TRUE)
      }
    } 
    
    if (quoA$Type == 'call') {
      if (quoA$Head == '<-') { # this block needs to happen first
        quo <- rlang::new_quosure(quoA$Args[[2]], env = quoA$Environment)
        assign <- deparse(quoA$Args[[1]])
        quoA <- analyzeExpr(quo)
        if (as.character(assign)[1] == '~') {
          assign <- assign[[3]]
        }
      }
      
      if (quoA$Head == '~') {
        quo <- rlang::new_quosure(quoA$Args[[length(quoA$Args)]], env = quoA$Environment)
        if (length(quoA$Args) > 1L) keyword <- as.character(quoA$Args[[1]])
      }
      
      if (quoA$Head == 'function') {
        argname <- quoA$Args[[1]][1]
        argname[[1]] <- quote(.)
        quo <- rlang::new_quosure(substituteName(quoA$Args[[2]], argname), quoA$Environment)
        
      }
      
      if (pmatch(quoA$Head, c('graphics'), nomatch = 0)) {
        quoA$Head <- 'par'
        quo <- unanalyzeExpr(quoA)
        keyword <- 'pre'
      }
      
      if (pmatch(quoA$Head, c('windows'), nomatch = 0)) {
        keyword <- 'windows'
      }
    }
    
    if (length(variables)) quo <- interpolateArguments(quo, variables)
    
    quoenv <- rlang::quo_get_env(quo)
    for (name in names(variables)) assign(name, variables[[name]], envir = quoenv)
    
    list(Quo = quo, Keyword = keyword, AssignTo = assign)
  })
  
  quoTab <- as.data.table(do.call('rbind', quos))
  quoTab$Keyword[argnames != ""] <- argnames[argnames != ""]
  
  quoTab$AssignTo[quoTab$Keyword != 'do'] <- NA
  
  quoTab <- parseKeywords(quoTab, withFunc)
  

  quoTab
  
}

 

          


parseKeywords <- function(quoTab, withFunc) {
  
  keywords <- quoTab[ , partialMatchKeywords(Keyword)]
  
  if (any(is.na(keywords))) {
    badwords <- glue::glue_collapse(paste0("'", quoTab$Keyword[is.na(keywords)], "'"),
                                    sep = ",", last = " and ")
    .stop('In your call to {withFunc},',
          "{badwords} <are|is> not <|a> legal keyword<s|>.",
          "See ?withHumdrum for help.",
          ifelse = sum(is.na(keywords)) > 1L)
  }
  
  quoTab$Keyword <- keywords
  
  # classify keywords
  knownKeywords <- list(do              = c('do', 'dofx', 'dofill', 'ordo', 'ordofill'),
                        partitions      = c('by', 'where'),
                        ngram           = 'ngram',
                        recordtypes     = 'recordtypes',
                        windows         = 'windows',
                        pre             = 'pre',
                        post            = 'post')
  quoTab[ , KeywordType := rep(names(knownKeywords), lengths(knownKeywords))[match(Keyword, unlist(knownKeywords))]]
  
  # check for validity
  if (!any(quoTab[ , KeywordType == 'do'])) {
    .stop("Your call to {withFunc} doesn't include any expressions with a 'do' keyword,",
          "so we don't know what you want to do to your data.",
          "These expressions must by input either as an argument with no name,",
          "a formula with no left hand side (~Expr)",
          "a formula or with 'do', 'doplot', 'dofx', or 'dofill', on the left hand side (do~Expr, doplot~Expr, etc.).")
  }
  
  if (any(quoTab[, KeywordType == 'ordo']) && !any(quoTab[, Keyword == 'where'])) {
    .stop("In your call to {withFunc} you've included an 'ordo' expression improperly.", 
          "An 'ordo' expression can only be used in combination with BOTH a where expression AND a do expression.",
          "See ?withinHumdrum for help.")
  }
  
  if (any(quoTab[ , Keyword == 'ngram'])) {
    i <- which(quoTab$Keyword == 'ngram')
    if (length(i) > 1L) .stop("In a call to {withFunc},",
                              "you can't have multiple 'ngram'-keyword arguments.")
    
    ngram <- rlang::eval_tidy(quoTab$Quo[[i]])
    checkLooseInteger(ngram, 'ngram', withFunc)
    quoTab$Quo[[i]] <- ngram
  }
  
  if (any(quoTab[ , Keyword == 'recordtypes'])) {
    i <- which(quoTab$Keyword == 'recordtypes')
    if (length(i) > 1L) .stop("In a call to {withFunc},",
                              "you can't have multiple 'recordtypes'-keyword arguments.")
    
    recordtypes <- rlang::eval_tidy(quoTab$Quo[[i]])
    quoTab$Quo[[i]] <- checkTypes(recordtypes, withFunc)
  }
  
  quoTab
}

partialMatchKeywords <- function(keys) {
    # this function matches partial matches off keywords
    # to the master standard
    keys <- gsub('_', '', tolower(keys))
    
    # define standard keys and alternatives
    standardkeys <- list(do          = c('do', 'd', 'eval', 'apply'),
                         dofill      = c('dofill', 'fill', 'evalfill', 'filleval', 'applyfill'),
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
                         windows  = c('windows', 'context', 'window'),
                         pre      = 'pre', 
                         post     = 'post')
    
    matches <- pmatch(keys, unlist(standardkeys), duplicates.ok = TRUE)
    
    rep(names(standardkeys), lengths(standardkeys))[matches]
    
    
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


## Preparing doQuo ----

prepareDoQuo <- function(humtab, quoTab, active, ordo = FALSE) {
  # This is the main function used by [.withinmHumdrum] to prepare the current
  # do expression argument for application to a [humdrumR][humdrumRclass] object.
  
  # do fill 
  doQuoTab <- quoTab[KeywordType == 'do']
  doQuoTab <- if (ordo) doQuoTab[grepl('ordo', Keyword)] else doQuoTab[!grepl('ordo', Keyword)]
  
  if (nrow(doQuoTab) == 0L) return(NULL)
  
  usedInExprs <- lapply(doQuoTab$Quo, fieldsInExpr, humtab = humtab)
  
  dofills <- grepl('fill', doQuoTab$Keyword)
  doQuoTab$Quo[dofills] <- Map(fillQuo, doQuoTab$Quo[dofills], usedInExprs[dofills])
  

  # collapse doQuos to a single doQuo
  doQuo <- concatDoQuos(doQuoTab)

  # turn . to active formula
  doQuo <- activateQuo(doQuo, active)
  
  # lagged vectors
  doQuo <- laggedQuo(doQuo)
  
  # add in arguments that are already fields
  doQuo <- fieldsArgsQuo(doQuo, colnames(humtab))
  
  # unnest nested formulae
  # funcQuosure <- unnestQuo(funcQuosure)
  
  # tandem interpretations
  #doQuo <- tandemsQuo(doQuo)
  
  # update what fields (if any) are used in formula
  
  # splats
  doQuo <- splatQuo(doQuo, humtab)
  
  
  # if (length(usedInExpr) == 0L) stop("The do expression in your call to within.humdrumR doesn't reference any fields in your humdrum data.
  #                                       Add a field somewhere or add a dot (.), which will automatically grab the default, 'Active' expression.",
  #                                       call. = FALSE)
  usedInExpr <- unique(fieldsInExpr(humtab, doQuo))

  # if the targets are lists, Map
  lists <- vapply(humtab[1 , usedInExpr, with = FALSE], class, FUN.VALUE = character(1)) == 'list' 
  if (any(lists)) doQuo <- mapifyQuo(doQuo, usedInExpr, depth = 1L)
    
  # if ngram is present
  if (any(quoTab$Keyword == 'ngram')) {
    doQuo <- ngramifyQuo(doQuo, 
                         quoTab[Keyword == 'ngram']$Quo[[1]], usedInExpr, 
                         depth = 1L + any(lists))
  } 
  
  if (any(quoTab$Keyword == 'windows')) {
    doQuo <- windowfyQuo(doQuo,  
                         quoTab[Keyword == 'windows']$Quo[[1]],
                         usedInExpr, 
                         depth = 1L + any(lists))
  }
  

  rlang::quo({
    result <- withVisible(!!doQuo)
    
    output <- result$value
    attr(output, 'visible') <- result$visible
    output
    
  })
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

concatDoQuos <- function(quoTab) {
    ## this function takes a named list of "do" quosures and creates a single quosure
    # which applies each of them in turn.
    # the doQuos must have names either do or doplot
    
    doQuos <- quoTab$Quo
    
    sideEffects <- grepl('fx', quoTab$Keyword)
    assignOut <- unlist(quoTab$Assign)
    
    if (tail(sideEffects, 1)) doQuos <- c(doQuos, rlang::quo(.))

    varname <- quote(.)
    for (i in 1:length(doQuos)) {
        if (i > 1L) doQuos[[i]] <- substituteName(doQuos[[i]], list(. = varname))
        
        expr <- rlang::quo_get_expr(doQuos[[i]])
        
        if ((i < length(doQuos) || any(!is.na(assignOut))) && 
                           (length(expr) > 1 && expr[[1]] != '<-') &&
                           !sideEffects[i]) {
            
          varname <- if (is.na(assignOut[i])) {
            rlang::sym(tempfile('xxx', tmpdir = ''))
          } else {
            rlang::sym(assignOut[i])
          }
     
            doQuos[[i]] <- rlang::new_quosure(expr(!!varname <- !!doQuos[[i]]), 
                                              env = rlang::quo_get_env(doQuos[[i]]))
            
        } 
    }
    doQuo <- quo({!!!doQuos})
    
    if (any(!is.na(assignOut))) {
      assignOut <- c(assignOut[!is.na(assignOut)],  if (is.na(tail(assignOut, 1L))) as.character(varname))
      assignOut <- setNames(rlang::syms(assignOut), assignOut)
      doQuo <- rlang::quo({
        !!doQuo
        list(!!!assignOut)
        })
    }
    doQuo
}

####################### Functions used inside prepareQuo

activateQuo <- function(funcQuosure, active) {
  # This function takes the `expression` argument
  # from the parent [withinHumdrum] call and 
  # inserts the `Active` expression from the 
  # target [humdrumRclass] object in place 
  # of any `.` subexpressions.
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

#### Insert exclusive/keyed args where necessary

fieldsArgsQuo <- function(funcQuosure, fields) {
  funcQuosure <- exclusiveArgsQuo(funcQuosure, fields)
  funcQuosure <- keyedArgsQuo(funcQuosure, fields)
  funcQuosure
}


exclusiveArgsQuo <- function(funcQuosure, fields) {
  if (!'Exclusive' %in% fields) return(funcQuosure)
  
  predicate <- \(Head) Head %in% exclusiveFunctions 
  do <- \(exprA) {
    if (!'Exclusive' %in% names(exprA$Args)) exprA$Args$Exclusive <- quote(Exclusive)
    exprA
  }
  
  modifyExpression(funcQuosure, predicate, do, stopOnHit = FALSE)
  
}

keyedArgsQuo <- function(funcQuosure, fields) {
  # functions that require a Key argument
  if (!'Key' %in% fields) return(funcQuosure)
  
  predicate <- \(Head) Head %in% keyedFunctions
  
  do <- \(exprA) {
    if (!'Key' %in% names(exprA$Args)) exprA$Args$Key <- quote(Key)
    exprA
  }
  
  modifyExpression(funcQuosure, predicate, do, stopOnHit = FALSE)
}

#### Lag/Led vectors

laggedQuo <- function(funcQuosure) {
  
  predicate <- \(Head, Args) Head == '[' && any(names(Args) %in% c('n', 'N')) 
  
  do <- \(exprA) {
    
    args <- exprA$Args
    if (!'windows' %in% .names(args)) args$windows <- expr(list(File, Spine))
    
    names(args)[names(args) == 'N'] <- 'n'
    n <- rlang::eval_tidy(args$n)
    if (!is.numeric(n) || ((n %% 1) != 0)) .stop('Invalid [n = ] lag expression.')
    args$n <- NULL
    
    lagExprs <- lapply(n, \(curn) rlang::expr(lag(!!!args, n = !!curn)))

    exprA$Head <- 'splat'
    exprA$Args <- lagExprs
    
    exprA
  }
  
  modifyExpression(funcQuosure, predicate, do, stopOnHit = TRUE)
  
}

#### Interpretations in expressions

#tandemsQuo <- function(funcQuosure) {
 # This function inserts calls to getTandem
 # into an expression, using any length == 1 subexpression
 # which begins with `*` as a regular expression.
 # If input is a quosure (it should be), it keeps the quosure intact,
 # (i.e., keeps it's environment).
          
# applyExpr(funcQuosure,
#           \(ex) {
#             exstr <- deparse(ex)
#             interp <- grepl('^\\*[^*]', exstr)
#             
#             if (interp) {
#               regex <- stringr::str_sub(exstr, start = 2L)
#               rlang::expr(getTandem(Tandem, !!regex))
#             } else {
#               ex
#             }
#           }) 
#}


#' Get tandem interpretation information from humdrum data.
#' 
#' Every [humdrumRclass] object has a field called
#' `Tandem` which is a vector of strings which accumulates
#' tandem interpretations in each Spine. This function (`getTandem`) 
#' extracts tandem interpretations from this field, based on a matching
#' regular expression. The obligatory `'*'` *does not* need to 
#' be included in the `regex`, as it is added automatically. Thus,
#' if you want to find tandem interpretations that match '*clef..', you
#' just have to write `regex = 'clef..'`.
#' 
#' @export
getTandem <- function(tandem, regex) {
  # Tandem data in a humdrumR object is stored as a 
  # cummulative list of tandem interpretations separated by commas.
  # This does some toying with tandem and regex to make
  # it so the user doesn't have to think about these commas,
  # but they get taken into account...I'm not sure if it 
  # will always work the way we want.
  
  checkArg(tandem, classes = c('list'))
          
  tandem <- paste0(',', tandem, ',')
  
  regex <- gsub('\\.\\*$', '[^,]*', regex)
  regex <- gsub('\\.$', '[^,]', regex)
  regex <- paste0(',\\*', regex, '[^,]*,')
  
  matches <- stringr::str_extract(tandem, pattern = regex)
  unique(stringr::str_sub(matches, 2L, -2L))
  
}


#### Splatting in expressions
# "splatting" refers to spreading a list of values expressions 
# into arguments to of a call.
# This is usually done in R using do.call, and can also be done useing
# rlang's unquoting operator !!!.
# humdrumR has another syntactic sugar for this:

splatQuo <- function(funcQuosure, humtab) {
  # This function takes an expression,
  # and replaces any subexpression of the form `func(splat(x, y, z))`
  # with `func(x, y, z)`.
  # This is basically unquoting, like rlang's !!!.
  # Splat also understands some special syntactic sugar, which is called BEFORE the unquoting:
  # Index expressions of the form `func(atomic[Field == x])`
  # are converted to `func(splat(atomic[Field == x[1], atomic[Field == x[2], etc.]))`

  predicate <- \(Head, Args) Head == '[' && all(.names(Args)[-1L] == 'splat')
  
  do <- \(exprA) {
    inExprA <- analyzeExpr(exprA$Args[[2]])
    
    if (inExprA$Head != '%in%') .stop('splat expression must use %in%')
    if (length(fieldsInExpr(humtab, inExprA$Args[[1]])) == 0L) .stop('splat expression must reference an existing field')
    
    inVals <- eval(inExprA$Args[[2]])
    if (!is.atomic(inVals)) .stop('splat %in% expression must be atomic values.')
    
    indexed <- exprA$Args[[1]]
    field <- inExprA$Args[[1]]
    splatArgs <- lapply(inVals, \(val) rlang::expr((!!indexed)[!!field == !!val]))
    
    exprA$Head <- 'splat'
    exprA$Args <- splatArgs
    exprA
  }
  
  funcQuosure <- modifyExpression(funcQuosure, predicate, do)

  # turn splat(x,y,z) to x,y,z
  predicate <- \(Args) any(sapply(Args, \(arg) analyzeExpr(arg)$Head == 'splat'))

  do <- \(exprA) {
      args <- exprA$Args
      
      args <- lapply(args, \(arg) {
        argA <- analyzeExpr(arg)
        if (argA$Head == 'splat') argA$Args else arg
      })
      exprA$Args <- unlist(args, recursive = FALSE)
      exprA
  }

  modifyExpression(funcQuosure, predicate, do)
  
}

splat <- list

parseAt <- function(atExpr) {
 # This function is used by splatQuo
 # It replaces an expression of the form `TargetExpr@GroupingExpr`
 # with `tapply(TargetExpr, GroupingExpr, c)`.

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
          
          lambdaexpression      <- quote(\() {} )
          lambdaexpression[[2]] <- fargs
          lambdaexpression[[3]] <- rlang::quo_squash(expression)
          # 
          # lambdaexpression <- rlang::new_quosure(quote(\() {}))
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

windowfyQuo <- function(funcQuosure, windowQuosure, usedInExpr, depth = 1L) {
  funcQuosure <- xifyQuo(funcQuosure, usedInExpr, depth)
  
  if (!'boundaries' %in% .names(windowQuosure[[2]])) windowQuosure[[2]][['boundaries']] <- quote(list(File,Spine))
  if (!'x' %in% .names(windowQuosure[[2]]) && .names(windowQuosure[[2]])[2] != '' ) windowQuosure[[2]][['x']] <- rlang::sym(usedInExpr[1])
  
  applyArgs <- as.list(windowQuosure[[2]][c('leftEdge', 'rebuild', 'passOutside')])
  windowQuosure[[2]] <- windowQuosure[[2]][!.names(windowQuosure[[2]]) %in% c('leftEdge', 'rebuild', 'passOutside')]
  
  
  rlang::quo(
    windowApply(func = !!funcQuosure,
                x = !!rlang::sym(usedInExpr[1]),
                windows = !!windowQuosure,
                !!!applyArgs))
  
}




#' Change or insert values in an expression
#' 
#' This function can be used to modify arguments to a functions
#' within an existing expression (or quosure/formula).
#' 
#' `interpolateArguments` inteprets named value in its `namedArgs` 
#' argument in one of two ways: If the named value is a list, it interprets
#' the name of the list as a function call, and inserts/swaps any arguments
#' in that list into any instances of that function call within the `expr`.
#' Named arguments are inserted or substituted if already present in expression.
#' Unnamed argmuments are simply added to the call.
#' Examples:
#'
#' ```
#' myexpr <- quote(dnorm(x, mean = 5))
#' interpolateArguments(myexpr, list(dnorm = list(mean = 2, sd = 5, TRUE)))
#' 
#' # result is new expresson: dnorm(x, mean = 2, sd = 5, TRUE)
#' ```
#'
#' If a named valued in the `namedArgs` argument is not a list,
#' that name/value pair is substituted anywhere it is present in the expression.
#' This approach is often more conscise, but arguments cannot be added to an 
#' expression this way, only substituted if already present.
#' Examples:
#' 
#' ```
#' myexpr <- quote(dnorm(x, mean = 5))
#' interpolateArguments(myexpr, mean = 2)
#' 
#' # result is new expression: dnorm(x, mean = 2)
#' }
#' ```
#'
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
    checkArg(namedArgs)
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




## Evaluating do quo in humtab ----

###########- Applying within.humdrumR's expression to a data.table



evalPrePost <- function(quoTab, which = 'pre') {
  
  if (any(quoTab$Keyword == which)) lapply(quoTab[Keyword == which]$Quo, rlang::eval_tidy, env = .GlobalEnv)
  
  quoTab[Keyword != which]
}

evalDoQuo <- function(doQuo, humtab, partQuos, ordoQuo) {
    if (nrow(partQuos) == 0L) {
        result <- rlang::eval_tidy(doQuo, data = humtab)
        
        do_attr(parseResult, result, humtab$`_rowKey_`)
        # parseResult(result, humtab$`_rowKey_`)
        
    } else {
        result <- evalDoQuo_part(doQuo, humtab, partQuos, ordoQuo)
        
        if (!is.data.frame(result)) {
          visible <- any(sapply(result, attr, which = 'visible'))
          result <- data.table::rbindlist(result)
          attr(result, 'visible') <- visible
        }
        
        result
    }
}
evalDoQuo_part <- function(doQuo, humtab, partQuos, ordoQuo) {
    ### evaluation partition expression and collapse results 
    ## to a single factor
    partType <- partQuos$Keyword[1]
    partition <- rlang::eval_tidy(partQuos$Quo[[1]], humtab)
    
    if (!is.list(partition)) partition <- list(partition)
    partition <- lapply(partition, rep, length.out = nrow(humtab))
    
    partition <- Reduce(switch(partType, by = paste, where = `&`), partition)
    
    partEval <- switch(partType,
                       by    = evalDoQuo_by,
                       where = evalDoQuo_where)
    
    partEval(doQuo, humtab, partition, partQuos, ordoQuo)
    
}

evalDoQuo_by <- function(doQuo, humtab, partition, partQuos, ordoQuo) {
    # this function doesn't use reHum because data.table 
    # pretty much already does (some) of whats needed.
    targetFields <- namesInExprs(colnames(humtab), c(doQuo, partQuos[-1]$Quo))
    targetFields <- c(targetFields, '_rowKey_')
    
    partition <- as.factor(partition)
    
    nparts <- max(as.integer(partition))
    if (nparts > 1L && all(par()$mfcol == c(1, 1))) {
      oldpar <- par(no.readonly = TRUE,
                    mfcol = find2Dlayout(nparts))
      on.exit(par(oldpar))
    }
    
    
    result <- humtab[ , 
                      list(list(evalDoQuo(doQuo, .SD, partQuos[-1], ordoQuo))), 
                      by = partition, .SDcols = targetFields]
    
    result$V1
}
evalDoQuo_where <- function(doQuo, humtab, partition, partQuos, ordoQuo) {
    if (!is.logical(partition)) stop(call. = FALSE,
                                     "In your call to with(in)Humdrum with a 'where ~ x' expression, 
                                     your where expression must evaluate to a logical (TRUE/FALSE).")
    result <- evalDoQuo(doQuo, humtab[partition], partQuos[-1], ordoQuo)
    
    if (!is.null(ordoQuo)) {
        orresult <- evalDoQuo(ordoQuo, humtab[!partition], partQuos[-1], NULL)
        result <- rbind(result, orresult)
    }
    
   # result[humtab[, '_rowKey_'], on ='_rowKey_'] 
   result
}





#######################################################-
## Reassembling humtable ----
#######################################################-

#This is the hard part, putting resultout output back into data.table

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
    
    colnames(result)[colnames(result) == ""] <- "Result"
    colnames(result) <- gsub('^V{1}[0-9]+', "Result", colnames(result))
    
    #
    lenRes <- nrow(result)
    lenKey <- length(rowKey)
    
    if (lenRes > lenKey) stop("Sorry, within.humdrumR doesn't currently support functions/do-expressions
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
        data.table(Result = result)
    }
}

parseResult_table <- function(result) {
   names(dimnames(result)) <- NULL
   data.table(list(result))
}

parseResult_list  <- function(result) {
  if (length(unique(lengths(result))) == 1L) as.data.table(result) else data.table(result)
}
parseResult_other <- function(result) data.table(list(result))






resultFields <- function(humtab) {
  # Another function used by resultIn<- (withing cureResultN)
  # Takes a humtab and identifies the result fields (columns), if any,
  # are in it.
  colnms <- colnames(humtab)
  
  resultfields  <- colnms[grepl('Result', colnms)]
  
  if (length(resultfields) != 0L) resultfields <- resultfields[order(as.numeric(stringr::str_extract(resultfields, '[0-9]+')))]
  
  resultfields
}

curResultN <- function(humtab) {
          # A function used by resultIn<-
          # identifies how many result fields (if any)
          # are already in a humtable.
          length(resultFields(humtab))   
          
}




###########################################-
# happly ----
###########################################-

# 

#' Apply functions to humdrum data
#'  
#' `apply2Humdrum` is just a wrapper for 
#' `[humdrumR:with-in-Humdrum][with(in)Humdrum]`,
#' included to parallel the `R` family of `[base:lapply][_apply]` functions.
#' `apply2Humdrum` uses [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html)
#' to capture arguments fed to it without the user needing to make explicit 
#' `[base:tilde][formula]` using `~`. This is only guaranteed to work 
#' in the `[base:environment][global environment]`, so be careful. If you run into
#' problems, switch over to `[humdrumR:with-in-Humdrum][with(in)Humdrum]` and use
#' explicit `[base:tilde][X~formulas]`.
#' 
#' @param humdrumR A [humdrumRclass] data object.
#' @param FUN A function to apply to the [humdrumR:humdrumR][Active]` field(s)
#' in the `humdrumR` object.
#' @param ... Any arguments which can be fed to 
#' `[humdrumR:with-in-Humdrum][with(in)Humdrum]` as formulae (except for
#' `do` expressions, which are replaced by the `FUN` argument!). 
#' However, rather that writinging formula in the format `Keyword ~ Expression`,
#' `apply2Humdrum` arguments should be written as normal `R` arguments: 
#' `Keyword = Expression`.
#' Unnamed arguments are ignored.
#' #' @param within A logical. If `TRUE` (the default), 
#' `[humdrumR:with-in-Humdrum][withinHumdrum]` is used to apply the 
#' function---meaning that the output is reconstituted into a new field in the 
#' `humdrumR` object. If `within == FALSE`,
#' `[humdrumR:with-in-Humdrum][withHumdrum]` is used instead,
#' which results in the function's output being returned inprocessed.
#' @param doplot Boolean. If `TRUE` the `FUN` argument is treated
#' as a `doplot` expression by `[humdrumR:with-in-Humdrum][with(in)Humdrum]`,
#' so the result is ignored (for plotting or side-effects purposes).
#' 
#' @export
apply2Humdrum <- function(humdrumR, FUN, ..., within = TRUE, doplot = FALSE) {
          checkhumdrumR(humdrumR, 'apply2Humdrum')
          exprs <- rlang::quos(...)
          keywords <- names(exprs)
          
          if (is.null(keywords)) exprs <- list()
          exprs    <- exprs[keywords != '']
          keywords <- keywords[keywords != '']
          
          formulae <- Map(\(qu, kw) rlang::new_formula(rhs = rlang::quo_get_expr(qu),
                                                              lhs = as.symbol(kw), 
                                                              env = rlang::quo_get_env(qu)), exprs, keywords) 
          formulae <- unname(formulae)
          # do expression
          do <- rlang::new_formula(rhs = quote(FUN(.)),
                                   lhs = if (doplot) quote(doplot) else quote(do),
                                   env = environment())
          
          do.call(if (within) 'within.humdrumR' else 'with.humdrumR',
                  c(humdrumR, do, formulae))
          
          
          
          
}

#' @rdname apply2Humdrum
#' @export 
happly <- apply2Humdrum

########################################-
# with methods ----
########################################-
