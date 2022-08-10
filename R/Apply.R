# This file defines the functions with.humdrumR and within.humdrumR, which are used to apply
# expressions to fields in a humdrumR data object in a manner analogous to the base
# with and within functions (as applied to data.frames).
#
# with.humdrumR, and within.humdrumR 
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
#' Apply arbitrary expressions to fields within [humdrumR][humdrumRclass] data.
#' 
#' @section Overview:
#' 
#' These functions are the primary means of working with
#' humdrumR data. They are analogous to the base functions
#' [with and within][base::with()]
#' methods for [data.frames][base::data.frame].
#' Specifically they allow you to evaluate arbitrary
#' expressions involving fields in a [humdrumR data object][humdrumRclass].
#' They also includes a number of special evaluation options:
#' 
#' * Evaluate an expression in a subset of the data.
#' * Evaluate the same expression separately in different subsets of the data.
#' * Evaluate an expression across windows in the data (e.g., ngrams, rolling windows).
#' * Evaluate an expression which produces a plot, with particular plotting parameters set using [graphics::par()].
#' 
#' 
#' The difference between `with.humdrumR` and `within.humdrumR` is
#' analogous to the difference between [base::with()] and [base::within()].
#' `with.humdrumR` evaluates your expression(s) and then simply returns the result of
#' the evaluation. `within.humdrumR` evaluates your expression(s) and then
#' inserts the results back into the humdrumR object (if possible), generating new
#' fields called `ResultX` (see details).
#'
#'
#' @section Expression evaluation:
#'
#' An "expression" is a legal bit of R code, like `2 + 2` or `x - mean(x)`. 
#' Each call to `with`/`within.humdrumR` must have at least one expression to evaluate,
#' or "*do*"  which we call "do" expressions.
#' These expressions are evaluated (executed) within the `humdrumR` object's humdrum table.
#' which means the expression can, refer to any field in the humdrumR object (`Record`, `Token`, `File`, etc.).
#' Since all the fields in a data.table are all vectors of the same length, expressions should usually be 
#' vectorized.
#' 
#' A number of special [syntactic sugars](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic) 
#' can be used in the "do" expressions.
#' The most basic, is that a `.` anywhere in the expression, will be 
#' interpreted as the humdrumR object's current [active expression][humActive].
#' More syntactic sugars are described below.
#' 
#' If multiple do expressions are provided, each expression is evaluated in order, and 
#' can refer to results of the previous do expression as `.` (variables assigned in previous expressions
#' can also be used.)
#' Other syntactic sugars are described in sections below.
#'
#' ```
#' humdata <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn') # read some data
#' 
#' within(humdata, pitch(Token))
#' within(humdata, pitch(.)) # Same as previous (unless `Active` field has been changed))
#' 
#' with(humdata, table(solfa(Token, Key = Key)))
#' # Assumes that the Key field was parsed during the call to `[readHumdrum][readHumdrum]`
#' 
#' within(humdata, semits(Token) - mean(semits(Token))) 
#' 
#' ```
#' 
#' Unnamed arguments (or formulae without a left-hand side) are interpreted as `do` expressions.
#' Do expressions can also be explicitly labeled by naming an argument "do" or with `do` on the left side
#' of a formula:
#' 
#' ```
#' with(chorales, table(Token))
#' with(chorales, do = table(Token))
#' with(chorales, do ~ table(Token))
#' # These all return the same result
#' 
#' ```
#'
#' ### Special Do
#' 
#' A few special versions of the `do` expression can be used.
#' 
#' + `doplot`:
#'   + The expression is evaluated, but the original `humdrumR` input
#'   if returned unchanged. This can be used for achieving a side effect (like making a plot)
#'   without saving the result.
#' + `dofill`:
#'   + The result is evaluated, and the result is recycled to the length of the input, making
#'   sure the result is the same length as the input.
#'   Basically, do fill is the equivalent of `rep(do ~ f(x), length.out = length(x))`.
#' 
#' 
#' @section Special Evaluation Keywords:
#'
#' `with.humdrumR` and `within.humdrumR` can be provided with
#' additional keyword expressions which modify how the main "do" expressions are evaluated.
#' The complete list of options are:
#' 
#' + `by` (group by)
#' + `where` (apply to subset)
#' + `windows`
#' + `ngrams`
#' + `pre` and `post`
#' 
#' These special expressions can be specified either as named arguments
#' (`with(data, do(x), by = group)`) or as formula, with the keyword on the left side 
#' (`with(data, do(x), by ~ group`).
#' Any unnamed argument (or formula without a left-side) is interpreted as a `do` expression,
#' not a special expression.
#' 
#' + A `by` expression is used to break the data into groups, with the `do` expression(s) evaluated 
#'   separately in each group (see "Group by", below).
#' + A `where` expression indicates a subset of the data 
#'   in which to evaluate the `do` expression (see "Partitioning", below).
#' + `ngrams` A positive number *n*. The expression is evaluated across overlapping length-*n* windows.
#' +. `pre` An expression to evaluate once before evaluating the do expression(s). Useful, for instance, for taking logs
#'   or opening a graphing window. The `pre` expression is evaluated in the global environment.
#' +. `post` An expression evaluate once after evaluating the do expression(s). Always evaluated in the global environment.
#' 
#' 
#' 
#' @section Group by:
#' 
#' A `by` expression is used to break the data into subsets, with the `do` expression(s) evaluated 
#' separately within each subset. This works the similarly to the `by` argument in 
#' `[data.table][data.table]`s, the `INDEX` 
#' argument of `[base][tapply]`, or the `INDICES` argument of `[base][by]`.
#' Each `by` expression must evaluate, within the `humdrumR` data object, to a vector (or a list of vectors 
#' of equal length) of categories to group the data by.
#' 
#' Most commonly, the `by` expression(s) are simply field(s) in the data: 
#' for instance, 
#' 
#' ```
#' with(humdata,
#'      do = table(Token),
#'      by = File)
#' ```
#'
#' will apply the function `[base][table]` to the `Token` field
#' *separately* for each file in the `humdrumR` data. 
#' However, we can also use more complex expressions like
#' 
#' ```
#' with(humdata,
#'      do = table(Token), 
#'      by = Spine > 3 | Record \%\% 2 == 0)
#' ```
#'
#' which will evaluate the do expression in two groups, one where either the spine number is 
#' three or less *or* the record number is even, and another group where the opposite is true. 
#' 
#' If the `by` expression evaluates to a list of grouping vectors,
#' the `do` expressions are evaluated across every combination of categories in all the vectors.
#' Thus,
#' 
#' ```
#' with(humdata, 
#'      do = table(Token),
#'      by = list(File, Spine))
#' ````
#'
#' will apply `table` to `Token` across each spine *in* each file.
#'
#' As some [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic), if the 
#' `by` expression is input as a formulae, lists of grouping expressions
#' can be created by separating each expression by a `~`. 
#' Thus, `by = list(File ~ Spine)` can be written 
#' `by = File ~ Spine` or `by ~ File ~ Spine`.
#'
#' @section Apply where TRUE:
#'                      
#' A `where` expression is used to identify a subset of the data and evaluate
#' the `do` expression(s) *only* in that subset. 
#' `where` expressions must evaluate, within the `humdrumR` data object, to
#' a single logical vector. The `do` expression(s) are only evaluated where this logical
#' vector is `TRUE`.
#' kept unchanged.
#' 
#' @section Advanced partitioning:
#' 
#' If multiple `by` or `where` expressions, or combinations of the two, are specified,
#' each is evaluated recursively, in order from left to right.
#' If `where` is specified after `by`, the `where` expression is evaluated within each `by` group
#' If `by` is specified after `where`, the grouping `by` expression is evaluated only where `where == TRUE`.
#' Thus, if you specify
#'
#' ````
#' within(humdata,
#'          do = sd(Semits),
#'          by = File, 
#'          where = Semits > mean(Semits))
#' ```
#' 
#' the standard deviation of the `semits` field will be calculated in each file,
#' but only where the `semits` field is greater than the mean `semits` value
#' *within that file*. Contrast this with this call:
#' 
#' ```
#' within(humdata,
#'          do = sd(Semits)
#'          where = Semits > mean(Semits), 
#'          by = File) 
#' ```
#' 
#' wherein the standard deviation of `semits` is, again, calculated for each file,
#' but this time wherever the `semits` field is greater than the mean value *across all the data*.
#' 
#' @section Plotting:
#'
#' The `doplot` keyword behaves exactly like the `do` keyword, except that the result of the
#' evaluation is ignored. This is useful for plotting as well as *other* side-effects (like writing to a file).
#' If `doplot` is used with `with.humdrumR`, the function simply returns `NULL` (after executing the `doplot`
#' expression.
#' If `doplot` is used with `within.humdrumR`, the function simply returns the unaltered
#' `humdrumR` argument.
#' 
#' `within.humdrumR` also allows you to specify plotting options inline, without having to make a separate call
#' to [par()]. Any [par()] argument can be specified by providing a named list to the `graphics` keyword.
#' For example, we can set the plot margins with the `mar` argument:
#' 
#' ```
#' within(data, 
#'        doplot = plot(sort(table(Token))), 
#'        graphics = list(mar = c(4, 4, 4, 4)))
#' 
#' ```
#' The best part is `within.humdrumR` will reset `par` to it's previous state after its done.
#' 
#' You can also use the syntactic sugar, `graphics(parargs = ...)`:
#' 
#' ```
#' within(data,
#'        doplot = plot(sort(nchar(Token))),
#'        graphics(mar = c(4, 4, 4, 4)))
#' ```
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
#' In `with(in).humdrumR` you can use some 
#' [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic)
#' to do just this.
#' We can index any field in our call with a `splat` argument, which must be a `Field %in% x`.
#' For example,
#'
#' ```
#' within(humdata, list(Token[splat = Spine %in% 1:2])) 
#' ```
#' 
#' In this call, the `Token` field will be divided into two groups, one where `Spine == 1` and the other where
#' `Spine == 2`; the first group (`Spine == 1`) will be used as the first argument to `list`, and the second group
#' (`Spine == 2`) as the second argument.
#' Thus, `wihtin.humdrumR` translates the previous expression to this:
#' 
#' ```
#' within(humdata,
#'        list(Token[Spine == 1], Token[Spine == 2]))
#' ```
#' 
#' @section Argument interpolation:
#' 
#' The `variables` argument is a list of named arguments which are `[humdrumR:interpolateArguments][interpolated]` into the
#' `do` expressions. This is useful if you've already created a list of formulas that you like, but would like
#' to make small changes to a function call within the `do` expressions, without starting from scratch.
#' Examples:
#' 
#' ```
#' mycommand <- c(do ~ mean(., na.rm = x), by ~ Spine ~ File)
#' within(humdata,
#'               mycommand,
#'               variables(x = TRUE))
#' ```
#' 
#' @section N grams:
#' 
#' @section Results:
#' 
#' The difference between `with.humdrumR` and `within.humdrumR` is in what they do with the results
#' of the evaluated do expression(s).
#' 
#' ### With
#' 
#' For calls to `with.humdrumR`, the result is simply returned as is.
#' This is what you want when you want to get out of thue humdrumR object and drop back into "normal" R,
#' often in the last stages of an analysis.
#' However, you may optionally specify `drop = FALSE`, in which case the result is returned
#' in a [data.table()].
#' 
#' 
#' ### Within
#' 
#' 
#' For calls to `within.humdrumR`, the result of each `do` expression
#' is inserted back into the `[humtable][humdrum table]`. 
#' Usually, `do` expressions should evaluate to atomic output which is the same length as the input:
#' in other words, every data point in the input field(s) correspond to a single data point in the new field.
#' However, if the results are shorter than input fields [humtable][humdrum table],
#' they are padded with null tokens to match the full (original) length.
#' 
#' #### Naming results 
#' 
#' If you don't explicitely name the results, they are put into new field(s) labeled 
#' `Result1`, `ResultX`, `...`, `ResultN`. 
#' You can name the new fields in one of two ways: 
#'
#' + normal R assignment, using `<-` or `->` anywhere in a do expression.
#'   + For example, `within(data, Semits <- semits(Token))`.
#' + end the expression with a named list.
#'   + For example, `within(data, list(Semits = semits(Token)))`
#' 

#'     
#' @param humdrumR A `[humdrumR][humdrumRclass]` data object.
#' @param ...  `...` arguments to `within.humdrumR` are divided into either named or unnamed arguments.
#'
#' Unnamed arguments must be formulas, functions---lists of formulas/functions, no matter how deeply nested, are flattened
#' to a single list of functions/formulas.
#' All functions are coerced to a formula as `~foo(.)`. The far left-hand side of each formula
#' must be a name/symbol. Named arguments are [humdrumR:interpolateArguments][interpolated] into and `do~X` formulas.

#' 
#' @param ... Any number of do expressions or evaluation modifying expressions.
#' @param dataTypes A string or vector of characters drawn from `c("D", "d", "I", "L", "M","G")`. 
#' These characters  correspond to types of humdrum records: **D**ata, null **d**ata, **I**nterpretations, 
#' **M**easures, **L**ocal comments, and **G**lobal comments respectively. The expression
#' is only evaluated on data drawn from the specified record types (defaults to `"D"`).
#' @param drop This argument is conceptually similar to the `drop` argument in R matrices.
#' If `drop = TRUE`, the output of `with.humdrumR` is simplified as much as possible (trying to return
#' the "raw" vector, list, table, etc. within it). If `drop = FALSE`, the result is *always*
#' a `data.table`. The default value (`drop = TRUE`) is usually what we want because it is more
#' intuitive, but in more complex code, it can be helpful to set `drop = FALSE` so that 
#' the output is consistent.
#' 
#' @examples 
#' humdata <- readHumdrum('directorywithdata/*.krn')
#' 
#' within(humdata, nchar(.)) # counts characters in each data token.
#' within(humdata, table(.), by ~ Spine) # Tabulates data tokens in each Spine.
#' 
#' @return From `within.humdrumR`  a new humdrumR data object.
#' From `with.humdrumR`, whatever value is returned by the expression or, if `drop = TRUE`,
#' a `data.table`.
#' 
#' @name withinHumdrum
NULL



#' @rdname withinHumdrum
#' @export
with.humdrumR <- function(data, ..., 
                          dataTypes = 'D',
                          drop = TRUE,
                          variables = list()) {
  
  checkhumdrumR(data, 'with.humdrumR')
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, variables = variables, withFunc = 'with.humdrumR'), envir = environment())
  
  result[ , `_rowKey_` := NULL]
  ### Do we want extract the results from the data.table? 
  if (drop) {
    if (nrow(result) == 0L) return(NULL)
    result <- result[[length(result)]]
    if (is.list(result) && length(result) == 1L) result <- result[[1]]
    
    visible <- attr(result, 'visible') %||% TRUE
    attr(result, 'visible') <- NULL
  } else {
    visible <- TRUE
  }
  
  
  if (visible) result else invisible(result)
  
}

#' @rdname withinHumdrum
#' @export
within.humdrumR <- function(data, ..., dataTypes = 'D', variables = list()) {
  checkhumdrumR(data, 'within.humdrumR')
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, variables = variables, 
                       withFunc = 'within.humdrumR'), 
           envir = environment())
  
  if (all(quoTab[KeywordType == 'do', Keyword == 'dofx'])) return(data)
  
  # any fields getting overwritten
  overWrote <- setdiff(colnames(result)[colnames(result) %in% colnames(humtab)], '_rowKey_')
  
  bad <- overWrote %in% c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Piece', 'Column', 'Spine', 'Path', 'Stop', 'Record', 'NData', 'Global', 'Null', 'Filter', 'Type')
  #fields(humdrumR, 'S')$Name
  if (any(bad)) {
    if ('Token' %in% overWrote[bad]) {
      .stop("In your call to withinHumdrum, you can't overwrite the 'Token' field.",
            "This field should always keep the original humdrum data you imported.")
    }
    .stop("In your call to withinHumdrum, you can't overwrite 'structural' fields.",
          ifelse = sum(bad) > 1L, 
          "You are attempting to overwrite the {harvard(overWrote[bad], 'and', quote = TRUE)} <fields|field>.",
          "For a complete list of structural fields, use the command fields(mydata, 'S').")
  }
  
 
  ## put result into new humtab
  newhumtab <- result[humtab[ , !colnames(humtab) %in% overWrote, with = FALSE], on ='_rowKey_'] 
  humtab[ , `_rowKey_` := NULL] # this is needed, because humtab was changed inPlace, inside the original object
  newhumtab[ , `_rowKey_` := NULL]
  
  
  #### Put new humtable back into humdrumR object
  newfields <- setdiff(colnames(newhumtab), colnames(humtab))
  
  notnull <- Reduce(`|`, lapply(newhumtab[, newfields, with = FALSE], \(field) if (is.logical(field)) logical(length(field)) else  !(is.na(field) | field == '.')))
  newhumtab$Null[notnull] <- FALSE
  newhumtab$Type[newhumtab$Type == 'd' & notnull] <- 'D'
  
  # What do do if d is in recordtypes
  if (any(grepl('d', recordtypes))) {
    humdrumR@Humtable <- humdrumR@Humtable[Type != 'd'] 
  }
  
  newhumtab <- update_humdrumR.data.table(newhumtab, field = c(newfields, overWrote))
  humdrumR@Humtable <- newhumtab
  
  # tell the humdrumR object about the new fields and set the Active formula.
  if (length(newfields)) {
    addFields(humdrumR) <- newfields
    humdrumR@Active <- rlang::quo(list(!!!(rlang::syms(newfields))))
  }
  humdrumR
  # update_humdrumR(humdrumR, field = c(newfields, overWrote))


}

withHumdrum <- function(humdrumR, ..., dataTypes = 'D', variables = list(), withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  humtab <- getHumtab(humdrumR)
  humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
  
  # interpret ... arguments
  quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  
  # "pre" stuff
  oldpar <- par(no.readonly = TRUE) 
  quoTab <- evalPrePost(quoTab, 'pre')
 
  
  # Getting the humtab with the right record types.
  recordtypes <- checkTypes(dataTypes, withFunc)
                      
  
  ### Preparing the "do" expression
  do   <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = FALSE)
  ordo <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = TRUE)
  # 

  #evaluate "do" expression! 
  result <- evalDoQuo(do, humtab[Type %in% recordtypes],  quoTab[KeywordType == 'partitions'],  ordo)
  
  if (nrow(result) > 0L) data.table::setorder(result, `_rowKey_`)
  
  #### number the unnamed new results
  ## This is done here because if we call `with.humdrumR(drop = FALSE)`
  ## we want the same colnames as the new fields we would get.
  unnamedresult <- colnames(result) == 'Result'
  if (sum(unnamedresult)) colnames(result)[unnamedresult] <- paste0('Result', curResultN(humtab) + seq_len(sum(unnamedresult)))
  
  # "post" stuff
  evalPrePost(quoTab, 'post')
  curmfg <- par('mfg')
  par(oldpar[!names(oldpar) %in% c('mai', 'mar', 'pin', 'plt','pty', 'new')])
  par(mfg = curmfg, new = FALSE)
  
  list(humdrumR = humdrumR, 
       humtab = humtab,
       quoTab = quoTab,
       recordtypes = recordtypes,
       result = result)
}


## Parsing Args ----

parseArgs <- function(..., variables = list(), withFunc) {
  quos <- rlang::enquos(...)
  if (length(quos) == 0L) .stop("You called {withFunc}, but gave it no commands to execute.")
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
        # quo <- rlang::new_quosure(quoA$Args[[2]], env = quoA$Environment)
        assign <- deparse(quoA$Args[[1]])
        # quoA <- analyzeExpr(quo)
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
  
  quoTab$AssignTo[!quoTab$Keyword %in% c('do', 'dofill')] <- NA
  
  quoTab <- parseKeywords(quoTab, withFunc)
  

  quoTab
  
}

 

          


parseKeywords <- function(quoTab, withFunc) {
  
  keywords <- quoTab[ , partialMatchKeywords(Keyword)]
  
  if (any(is.na(keywords))) {
    badwords <- harvard(quoTab$Keyword[is.na(keywords)], "and", quote = TRUE)
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
  

  # rlang::quo({
    # result <- withVisible(!!doQuo)
    # 
    # output <- result$value
    # if (!is.null(output)) attr(output, 'visible') <- result$visible
    # output
    
  # })
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

concatDoQuos <- function(quoTab) {
    ## this function takes a named list of "do" quosures and creates a single quosure
    # which applies each of them in turn.
    # the doQuos must have names either do or doplot
    
    doQuos <- quoTab$Quo
    
    sideEffects <- grepl('fx', quoTab$Keyword)
    whichResult <- last(which(!sideEffects))
    
    if (tail(sideEffects, 1)) {
      # this might not seem optimal, but actually gets around a lot of problems.
      doQuos <- c(doQuos, 
                  if (any(!sideEffects)) quote(.) else list(quote(NULL)))
      sideEffects <- c(sideEffects, FALSE)
    }
    
    assignOut <- list()
    resultName <- "Result"
    varname <- quote(.)
    for (i in 1:length(doQuos)) {
        if (is.null(doQuos[[i]])) next
        if (i > 1L) doQuos[[i]] <- substituteName(doQuos[[i]], list(. = varname))
        
        if (sideEffects[i]) next
        
        exprA <- analyzeExpr(doQuos[[i]])
        
        if (exprA$Head == '<-') {
          varname <- exprA$Args[[1]]
          if (i != whichResult) {
            assignOut <- c(assignOut, varname)
          } else {
            resultName <- as.character(varname)
          }
        } else {
          if (i  < length(doQuos)) {
            varname <- rlang::sym(tempfile('xxx', tmpdir = ''))
            doQuos[[i]] <- rlang::new_quosure(expr(!!varname <- !!doQuos[[i]]), 
                                              env = rlang::quo_get_env(doQuos[[i]]))
          }
        }
    }
    # if (is.null(assignOut[[length(assignOut)]]))  assignOut$Result <- quote(result)
    names(assignOut) <- sapply(assignOut, as.character)

    quo({
      
      result <- visible(withVisible({!!!doQuos}))
      if (!is.null(result) && !is.list(result)) {
        result <- list(result)
        names(result) <- !!resultName
      }
      results <- c(list(!!!assignOut), result)
      results
      }) 
    
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
  funcQuosure <- boundedArgsQuo(funcQuosure)
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


boundedArgsQuo <- function(funcQuosure) {
  # functions that require a Key argument
  
  predicate <- \(Head) Head %in% boundedFunctions
  
  do <- \(exprA) {
    if (!'boundaries' %in% names(exprA$Args)) exprA$Args$boundaries <- quote(list(File, Spine, Path))
    exprA
  }
  
  modifyExpression(funcQuosure, predicate, do, stopOnHit = FALSE)
}

#### Lag/Led vectors

laggedQuo <- function(funcQuosure) {
  
  predicate <- \(Head, Args) Head == '[' && any(names(Args) %in% c('n', 'N')) 
  
  do <- \(exprA) {
    
    args <- exprA$Args
    if (!'boundaries' %in% .names(args)) args$boundaries <- expr(list(File, Spine, Path))
    
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
  predicate <- \(Args) any(sapply(Args, \(arg) identical(analyzeExpr(arg)$Head, 'splat')))

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
        parseResult(result, humtab$`_rowKey_`)
        
    } else {
        evalDoQuo_part(doQuo, humtab, partQuos, ordoQuo)
        
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
    
    if (nparts > 1000L) message("You're 'by' argument is making ", num2print(nparts), " groups.", 
                                " This could take a while!")
    
    if (nparts > 1L && nparts <= 16 && all(par()$mfcol == c(1, 1))) {
      oldpar <- par(no.readonly = TRUE,
                    mfcol = find2Dlayout(nparts))
      on.exit(par(oldpar[!names(oldpar) %in% c('mai', 'mar', 'pin', 'plt','pty', 'new')]))
    }
    
    
    results <- humtab[ , {
                          evaled <- evalDoQuo(doQuo, .SD, partQuos[-1], ordoQuo)
                          evaled$`_partitionKey_` <- partition
                          list(list(evaled)) 
                          },
                      by = partition, .SDcols = targetFields]
    
    result <- data.table::rbindlist(results$V1)
    
    
    if (nrow(result) == nrow(results)) {
      lists <- colnames(result)[sapply(result, is.list) & !grepl('^_.*_$', colnames(result))]
      
      for (col in lists) result[[col]] <- setNames(result[[col]], result$`_partitionKey_`) 
    }
    result[, `_partitionKey_` := NULL]
    
}
evalDoQuo_where <- function(doQuo, humtab, partition, partQuos, ordoQuo) {
    if (!is.logical(partition)) stop(call. = FALSE,
                                     "In your call to with(in)Humdrum with a 'where ~ x' expression, 
                                     your where expression must evaluate to a logical (TRUE/FALSE).")
    result <- evalDoQuo(doQuo, humtab[partition], partQuos[-1], ordoQuo)
    
    if (!is.null(ordoQuo)) {
        orresult <- evalDoQuo(ordoQuo, humtab[!partition], partQuos[-1], NULL)
        result <- list(result, orresult)
    }
   # result[humtab[, '_rowKey_'], on ='_rowKey_'] 
   result
}





#######################################################-
## Reassembling humtable ----
#######################################################-


parseResult <- function(result, rowKey) {
    # this takes a nested list of results with associated
    # indices and reconstructs the output object.
    if (length(result) == 0L || all(lengths(result) == 0L)) return(cbind(as.data.table(result), `_rowKey_` = 0L)[0])
    
    objects <- sapply(result, \(res) !is.atomic(res) || (!is.factor(res) && is.object(res)))
    result[objects] <- lapply(result[objects], 
                              \(x) {
                                attr <- humdrumRattr(x)
                                x <- list(x)
                                humdrumRattr(x) <- attr
                                x
                                })
    
    if (length(unique(lengths(result))) > 1L) result <- list(result)
    
    result <- as.data.table(result)
    
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


