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
#' Evaluate arbitrary expressions using the fields within [humdrumR][humdrumRclass] data,
#' while employing split/apply/combine, windowing, and other analysis options.
#' 
#' @section Overview:
#' 
#' These functions are the primary means of working with
#' humdrumR data. They are analogous to the base-R
#' [with and within][base::with()]
#' methods for [data.frames][base::data.frame].
#' Specifically they allow you to evaluate arbitrary
#' expressions using the fields of a [humdrumR data object][humdrumRclass].
#' This means that you can drop "inside" your [humdrumR data object][humdrumRclass] and run whatever commands 
#' you want using the fields of the [humdrum data table][humTable], 
#' while keeping the data safely "encapsulated" within the humdrum object---at least, until you *want*
#' to pull it out.
#' 
#' The difference between `with.humdrumR` and `within.humdrumR` is
#' analogous to the difference between [base::with()] and [base::within()].
#' `with.humdrumR` evaluates your expression(s) and then simply returns the result of
#' the evaluation. `within.humdrumR` evaluates your expression(s) and then
#' inserts the results back into the humdrumR object, generating new
#' fields called `ResultX` (see details).
#' 
#' In addition, `with` and `within` offer a number of powerful options that make working with 
#' humdrum data easier:
#' *evaluation control arguments* can be used to control
#' how your expressions are evaluated.
#' You can evaluate expressions...
#' 
#' + In a subset of the data using `subset`...
#'   + either ignoring the rest of the data or evaluating a *different* expression in the other part.
#' + Separately in different subsets of the data, which are then recombined (split-apply-combine) using `by`.
#' + Across windows in the data (e.g., ngrams, rolling windows).
#' + Which produces a plot, with particular [plotting parameters][graphics::par()], and/or without 
#'   returning anything using `sidefx`.
#' + "Fill" short results to match the original field size using `fill`.
#' + Only in certain record types (defaulting only data records) using `dataTypes`.
#' 
#' These arguments are specified as named arguments to `with`/`within` calls.
#' Even though they aren't formal arguments, they are [partially matched][partialMatching],
#' so if write `grou` instead of `groupby`, you won't get an error!
#' In some cases, you can specify more than one of the same type of control argument (details below).
#'
#'
#' @section Expression evaluation:
#'
#' An "expression" is a legal bit of R code, like `2 + 2` or `x - mean(x)`. 
#' Each call to `with`/`within.humdrumR` must have at least one expression to evaluate.
#' We will refer to these as "within-expressions."
#' These expressions are passed to `with`/`within.humdrumR` as unnamed arguments: for example,
#' `with(humData, myExpressionHere)`.
#' 
#' Within expressions are evaluated within the `humdrumR` object's humdrum table,
#' which means the expression can refer fields in the humdrumR object by name (`Record`, `Token`, `File`, etc.)
#' just like any other variables.
#' Since all the fields in a humdrum object are vectors of the same length, within expressions are easily
#' (and generally should be) vectorized.
#' Note that the within-expression value is only evaluated over data-points/records that match the type
#' indicated in the `dataTypes` argument.
#' By default, only non-null data tokens (`"D"`) are used.
#' 
#' If multiple within-expressions are provided, each expression is evaluated in order (left to right).
#' Each expression can refer to the results of the last expression (as `.`), or 
#' to variables defined in previous expressions.
#' 
#' A number of special [syntactic sugars](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic) 
#' can be used in within expressions.
#' 
#' + The `.` placeholder.
#' + Side effects
#' + Recycled ("filled") results
#' + Lagged vectors
#' + etc.
#' 
#' Each of these is explained below.
#' 
#' ### The . placeholder
#' 
#' The `.` variable can be used as a special placeholder in within expressions.
#' In the first within expression, `.` is interpreted as the humdrumR object's 
#' current [active expression][humActive].
#' If multiple within expressions are given, beyond the first expression,  `.` refers to result of 
#' the *previous* expression.
#' For example, if `Token` is the [active expression][humActive], then:
#' 
#' ```
#' with(humData, nchar(.), mean(.), .^2)
#' 
#' ```
#' 
#' would return the same result as:
#' 
#' ```
#' with(humData, mean(nchar(Token))^2)
#' ```
#' 
#' 
#'
#' 
#' ### Side effects:
#' 
#' In some cases, you want to evaluate a within-expression for its 
#' "[side effect](https://en.wikipedia.org/wiki/Side_effect_(computer_science))";
#' This means that the expression *does* something you want (the "side effect") but doesn't actually
#' evaluate to (return) a result that you want.
#' The most common "side effect" is creating a plot.
#' Other examples might be printing text to the console using [base::cat()] or [base::print()], or 
#' writing to a file.
#' 
#' Side effects can be achieved by naming your expression `sidefx` or `fx`---as usual,
#' these arguments can be [partially matched][partialMatching], so `side` also works, and is commonly used.
#' Side-effect expressions are executed, but their result (if any) is ignored.
#' This means that if you call something like `newData <- within(humData, side = plot(x))`, the plot is made
#' but the result (`newData`) is identical to `humData`.
#' 
#' Side-effects can also be used in combination with other within expressions.
#' Their result is ignored, and *not* fed to the next expression as `.`.
#' For example the command
#' 
#' ```
#' with(humData, nchar(Token), side = hist(.), mean(.))
#' ```
#' 
#' creates a histogram of `nchar(Token)` and also returns the mean of `nchar(Token)`.
#' (Note that variables explicitly assigned in a `side` call *are* visible in later calls,
#' which is confusing, so don't do it!)
#' 
#' 
#' ### Recycling ("filling") results:
#' 
#' The result of your within expression may be shorter than the input vectors ([humtable fields][humTable]).
#' However, in some calls to `within.humdrumR` in particular, you'd like to return a single number and 
#' recycle it to "fill" the original data field.
#' In other words, you'd like the output (result) of your expression to be repeated until it matches the
#' length of the input field(s).
#' You could do this manually be using the [base::rep()] function, but `with`/`within.humdrumR` provide a
#' syntactic sugar for this:
#' You can name your expression `recycle` or `fill` ([partially matched][partialMatching]),
#' which will cause the result to be recycled.
#' All this does is take `yourExpression(field, ...)` and wrap it in 
#' `rep(yourExpression(field, ...), length.out = length(field))`.
#' 
#' ### Lagged vectors
#' 
#' We very often want to work with "[lagged][lag()]" vectors of data.
#' For example, we want to look at the relationship between a vector and the previous values of the 
#' same vector---e.g., the vector offset or "lagged" by one index.
#' The `humdrumR` [lag()] function is useful for this, as it gives us several options for lagging vectors,
#' always keeping them the same length so vectorization is never hindered.
#' `with` and `within.humdrumR` give us a very convenient short cut to using `lag`.
#' In a within-expression, any vector can be indexed with an `integer` argument named `lag` (case insensitive),
#' causing it to be lagged by that integer.
#' (A vector indexed with `lag = 0` returns the unchanged vector.)
#' For example, the following two calls are the same:
#' 
#' ```
#' with(humData, Token[lag = 1])
#' with(humData, lag(Token, 1))
#' ```
#' 
#' If the `lag` index has *multiple* values and the indexed object appears within a higher function call,
#' each lag is inserted as a *separate* argument to that call.
#' Thus, *these* two calls are also the same:
#' 
#' ```
#' with(humData, table(Token[lag = 1:2])
#' 
#' with(humData, table(lag(Token, 1), lag(Token, 2))
#' ```
#' 
#' [lag()] is a function with a `groupby` argument, which `with`/`within.humdrumR`
#' will automatically feed the fields `list(File, Spine, Path)`.
#' This is the default "melodic" behavior in most music.
#' If you'd like to turn this off, you need to override it by adding your own
#' `groupby` argument to the lagged index, like `Token[lag = 1, groupby = list(...)]`.
#' 
#' 
#' Using lagged vectors, since they are vectorized, is the fastest (computationally) and easiest way of working with n-grams.
#' For example, if you want to create character-string 5-grams of your data, you could call:
#' 
#' ```
#' with(humData, paste(Token[lag = 0:5], sep = '-'))
#' ```
#' 
#' Note that, since `with`/`within.humdrumR` passes `groupby = list(File, Spine, Path)`
#' to [lag()], these are true "melodic" n-grams, only created within spine-paths within each file.
#' 
#' 
#' @section Results:
#' 
#' The difference between `with.humdrumR` and `within.humdrumR` is in what they do with the results
#' of the evaluated within-expression(s).
#' 
#' 
#' ### Within
#' 
#' 
#' For calls to `within.humdrumR`, the result of the final within-expression
#' is inserted back into the `[humtable][humdrum table]`. 
#' Most results should be `atomic` vectors, though `list`s can also be placed as fields.
#' If results are shorter than the input fields of the [humtable][humdrum table] 
#' they are padded with null values to fill the same length as the input.
#' Non-vector results (`object`s) and tables are put into lists, and treated like any other list.
#' In the result, data/record-types that are not indicated by the `dataTypes` argument are also 
#' returned padded as null values.
#' 
#' If you don't explicitly name the result of your within-expression, it is put into a new field labeled 
#' `ResultX`, where `X` is the lowest number of "ResultX" fields that is not already taken.
#' You can explicitly name the result field with a top-level assignment in the expression.
#' For example, `within(data, Semits <- semits(Token))` will put the result of 
#' `semits(Token)` into a new field called `Semits`.
#' In fact, if you assign the results of multiple within-expressions, 
#' each result that is the same length as the last result will be put into a new field.
#' Thus, you can create multiple new fields in a single call:
#' for example,
#' 
#' ```
#' within(humData, 
#'        Semits <- semits(Token),
#'        Recip <- recip(Token))
#' ```
#' 
#' creates *two* new fields, `Semits` *and* `Recip`.
#' (As explained above, the second within-expression could also refer to the `Semits` variable
#' created in the previous expression.)
#' 
#' 
#' ### With
#' 
#' For calls to `with.humdrumR`, the result is simply returned as is.
#' This is what you want when you want to get out of the [humdrumR object][humdrumRclass] and drop back 
#' into "normal" R, often in the last stages of an analysis.
#' 
#' `with.humdrumR` has a `drop` argument, which defaults to `TRUE`.
#' If `drop = FALSE`, the result is returned in a [data.table()].
#' The column names of this results table are generated as described in the previous section:
#' i.e., defaulting to `ResultX` but allowing explicit naming with through one or more
#' explicit assignments in the within-expressions.
#' In addition, if `subset` or `groupby` arguments are used, columns are included which indicate
#' the value of the evaluated `subset`/`groupby` factor for each row.
#' 
#' 
#' 
#' @section Partitioning data:
#'
#' `groupby` (e.g., `by`) and `subset` expression control arguments all you to evaluate
#' your within-expressions within specific subsets of the data.
#' A `subset` argument can be used to evaluate your within-expression only within a subset of the data.
#' A `groupby` argument breaks the data into groups,
#' evaluating the within-expression(s) *separately* within each group.
#' The results of the grouped evaluations are then returned in a list (`with`) or recombined
#' into the original data `within`---this is a form of the "split-apply-combine" routine that is 
#' key to `R` data analysis.
#' 
#' `subset` and `groupby` arguments are themselves arbitrary expressions which are evaluated within
#' the [humdrum table][humTable], so they can (and usually do) refer to fields in the table.
#' Any `with`/`within.humdrumR` call can include zero, one, *or more* `subset` and/or `groupby` arguments, 
#' including combinations of both.
#' If more than one `subset`/`groupy` by argument is included, they are evaluated in order (left to right),
#' *recursively*: each one evaluated within the partition(s) established by the previous expression.
#' The normal *within* expression(s) are then, all evaluated within the partition(s) established by the
#' last  `subset`/`groupby` argument.
#' The "Advanced" partitioning section below explores this in more detail.
#' 
#' ### Apply in subset:
#'                      
#' A `subset` argument is an arbitrary expression which identifies a subset of the humdrum data.
#' `subset` expressions must evaluate to
#' a single logical vector, 
#' The `subset` result, if short, will be automatically recycled to the full length of the [humdrum table][humTable].
#' The within expression(s) are only evaluated where the `subset` argument(s) return `TRUE`.
#' 
#'   
#' In a call to `with`, only the result evaluated where `subset == TRUE` is returned.
#' However, in a call to `within`, we must decide what to
#' do with rest of the data: the [complement](https://en.wikipedia.org/wiki/Complement_(set_theory)) 
#' of the subset.
#' By default, `within` pads the returned values with null data
#' where ever `subset == FALSE` (in the complement).
#' So if you, for example, run the command `within(humData, kern(Token), subset = Spine == 1)`
#' the new field created by `within` will be filled with `kern` data where `Spine == 1`,
#'  but the remaining spines (if any) will all by null.
#'  
#' If you want to explicitly control what is put into the complement
#' part of a new field, you can specifying alternate
#' within-expression(s) to evaluate where `subset == FALSE`.
#' These must be named `complement`, or the aliases
#' `rest` or `otherwise` (these are all [partially matched][partialMatching]).
#' A `complement` expression can only be specified in combination with a `subset` argument,
#' and must be *in addition* to a normal within-expression.
#' The idea is that you evaluate the "normal" within-expression where `subset == TRUE`, *or else*
#' you specify evaluate the `complement` expression.
#' The results of Complement expressions are always recycled to fill the whole complement (see the "recycle")
#' 
#' A common use case for a `complement` expression is to use the within expression to change the data in one spine
#' but return the data unchanged in other spines.
#' For example, we could specify `within(humData, kern(Token), subset = Spine == 1, complement = Token)`.
#' Spine 1 will (as before) have `kern` applied to it.
#' However, instead of return a new field with null values in the other spine(s),
#' this call will return the original (unaltered) values from the `Token` field in the other spines.
#' 
#' 
#' 
#' ### Group by:
#' 
#' A `groupby` expression (use `by` for short) partitions your data exhaustively into 
#' (possibly non-contiguous) groups, and evaluates your within-expression(s) *separately* 
#' within each group.
#'  This works the similarly to the `by` argument in 
#' `[data.table][data.table]`s, the `INDEX` 
#' argument of `[base][tapply]`, or the `INDICES` argument of `[base][by]`.
#' Each `groupby` expression must evaluate, within the `humdrumR` data object, to a vector (or a list of vectors 
#' of equal length) to group the data by.
#' Each unique combination of values across these vectors becomes one group.
#' 
#' Most commonly, the `groupby` expression(s) are simply field(s) in the data: 
#' for instance, 
#' 
#' ```
#' with(humdata,
#'      table(Token),
#'      by = File)
#' ```
#'
#' will apply the function `[base][table]` to the `Token` field
#' *separately* for each file in the `humdrumR` data. 
#' However, we can also use more complex expressions like
#' 
#' ```
#' with(humdata,
#'      table(Token), 
#'      by = Spine > 3 | Record \%\% 2 == 0)
#' ```
#'
#' which will evaluate the do expression in two groups, one where either the spine number is 
#' three or less *or* the record number is even, and another group where the opposite is true. 
#' 
#' If the `groupby` expression evaluates to a list of grouping vectors,
#' the within expressions are evaluated across every combination of categories across all the vectors.
#' Thus,
#' 
#' ```
#' with(humdata, 
#'      table(Token),
#'      by = list(File, Spine))
#' ````
#'
#' will apply `table` to `Token` across each spine *in* each file.
#'
#'
#' 
#' ### Advanced partitioning:
#' 
#' If multiple `groupby` or `subset` expressions, or combinations of the two, are specified,
#' each is evaluated recursively, in order from left to right.
#' If `subset` is specified after `groupby`, the `subset` expression is evaluated within each `groupby` group
#' If `groupby` is specified after `subset`, the grouping `by` expression is evaluated only where `subset == TRUE`.
#' Thus, if you specify
#'
#' ````
#' within(humdata,
#'          sd(Semits),
#'          by = File, 
#'          subset = Semits > mean(Semits))
#' ```
#' 
#' the standard deviation of the `semits` field will be calculated in each file,
#' but only where the `semits` field is greater than the mean `semits` value
#' *within that file*. Contrast this with this call:
#' 
#' ```
#' within(humdata,
#'          sd(Semits)
#'          subset = Semits > mean(Semits), 
#'          by = File) 
#' ```
#' 
#' wherein the standard deviation of `semits` is, again, calculated for each file,
#' but this time wherever the `semits` field is greater than the mean value *across all the data*.
#' 
#' @section Windowing data:
#' 
#' XXXX
#' 
#' @section Plotting parameters:
#'
#' As mentioned above, plots in within-expressions should (often) be called using the `sidefx` argument name.
#' When plotting, `with`/`within.humdrumR` also allows you to specify plotting options inline,
#'  without having to make a separate call
#' to [par()]. Any [par()] argument can be specified by providing a named list to the `graphics` keyword.
#' For example, we can set the plot margins with the `mar` argument:
#' 
#' ```
#' within(data, 
#'        side = plot(sort(table(Token))), 
#'        graphics = list(mar = c(4, 4, 4, 4)))
#' 
#' ```
#' The best part is `with`/`within.humdrumR` will reset `par` to it's previous state after its done.
#' 
#' You can also use the syntactic sugar, `graphics(parargs = ...)`:
#' 
#' ```
#' within(data,
#'        side = plot(sort(nchar(Token))),
#'        graphics(mar = c(4, 4, 4, 4)))
#' ```
#' 
#' 
#' 
#' @section Splatting:
#' 
#' ("Splatting" refers to feeding a function a list/vector of arguments.)
#' Sometimes we want to divide our data into pieces (a l\'a `partition` option), but
#' rather than applying the same expression to each piece, we want to feed
#' the separate pieces as separate arguments to the same function.
#' In `with`/`within.humdrumR` you can use some 
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
#' Thus, `within` translates the previous expression to this:
#' 
#' ```
#' within(humdata,
#'        list(Token[Spine == 1], Token[Spine == 2]))
#' ```
#' 
#' @section N grams:
#' 
#' XXXX
#' 
#' 
#' 
#' @section Advanced scripting:
#' 
#' `with.humdrumR` and `within.humdrumR` use
#' [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html)
#' of their expressions.
#' This is very useful on the command line or in a script running one command at a time.
#' However, if you want to more advanced scripting non-standard evaluation can be a problem.
#' For example, if you to loop through a list of within-expressions
#' or reuse a common combination coor arguments many times.
#' 
#' Fortunately, R has *formula*, which are a way of capturing ("quoting") expressions into a 
#' concrete object that you can manipulate. Better yet, `with`/`within.humdrumR` will 
#' interpret formulae passed to them as arguments.
#' Basically, the right-hand side of any formula is interpreted as an 
#' expression to evaluate.
#' If a formula is passed as a unnamed argument to `with`/`within`, the left-hand side
#' of the formula (if any) is treated as the argument name.
#' If you name a formula argument that has a left-hand side, the left-hand side is ignored.
#' 
#' ```
#' 
#' with(humdrumR, table(Token), by = Spine)
#' with(humdrumR, ~table(Token), by ~ Spine)
#' 
#' ````
#' 
#' This approach would allows us to, same `~table(Token)` or `by ~ Spine` as variables, 
#' allowing us to do things like:
#' 
#' ```
#' 
#' tabler <- ~table(.)
#' byspine <- by ~ Spine
#' 
#' with(humData, tabler, byspine)
#' 
#' ```
#' 
#' We can even make lists and loop through them:
#' 
#' ```
#' bys <- list(~ Spine, ~ File, ~ COM)
#' 
#' for (b in bys) with(humData, table(.), by = b)
#' 
#' ```
#' 
#' ### Variables
#' 
#' Another useful scripting option is to include free variables in your 
#' within-expressions, which can then be fed in using the `variables` argument.
#' `variables` must be a named list.
#' Variables in the within-expression which match a name of from the `variables`
#' list have that value interpolated.
#' This allows us, for example, to run commands like:
#' 
#' ```
#' nums <- c(2, 3, 4, 5)
#' expression <- ~.^N
#' 
#' for (n in nums) with(humData, expression, variables = list(N = n))
#' 
#' ```
#' 
#' Each time `with` is called, the `N` in `.^N` is replaced by the current value of `n`.
#'      
#' @param data ***A [humdrumR data object][humdrumRclass] to summarize.***
#' 
#' Must be `humdrumR`.
#' 
#' @param ...  Any number of expressions to evaluate. Unnamed expressions are interpreted as the
#' "main" *within-expressions*. Possible *evaluation control arguments* include `by`, `subset`, and `windows`.
#' Other evaluation options can be achieved with `recycle` or `side` arguments.
#' 
#' @param dataTypes ***Which types of humdrum records to include.***
#' 
#' Defaults to `"D"`.
#' 
#' Must be `string`. 
#' 
#' A string or vector of characters drawn from `c("D", "d", "I", "L", "M","G")`. 
#' These characters  correspond to types of humdrum records: **D**ata, null **d**ata, **I**nterpretations, 
#' **M**easures, **L**ocal comments, and **G**lobal comments respectively. The expression 
#' is only evaluated on data drawn from the specified record types.
#' 
#' @param variables ***A named `list` of values, which are interpolated into the within-expression(s) wherever a variable name matches a named from the list.***
#' 
#' Defaults to `list()`.
#' 
#' Must be `list`.
#' 
#' @param drop ***Whether to return a simplified data structure.***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be `logical`; must be length `1`.
#' 
#' This argument is conceptually similar to the `drop` argument in R matrices.
#' If `drop = TRUE`, the output of `with.humdrumR` is simplified as much as possible (trying to return
#' the "raw" vector, list, table, etc. within it). If `drop = FALSE`, the result is *always*
#' a `data.table`. The default value (`drop = TRUE`) is usually what we want because it is more
#' intuitive, but in more complex code, it can be helpful to set `drop = FALSE` so that 
#' the output is consistent.
#' 
#' 
#' @return From `within.humdrumR`  a new humdrumR data object.
#' From `with.humdrumR`, whatever value is returned by the expression or, if `drop = TRUE`,
#' a `data.table`.
#' 
#' @aliases within with
#' @name withinHumdrum
NULL



#' @rdname withinHumdrum
#' @export
with.humdrumR <- function(data, ..., 
                          dataTypes = 'D',
                          expandPaths = FALSE,
                          drop = TRUE,
                          variables = list()) {
  
  checks(data, xclass('humdrumR'))
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, expandPaths = expandPaths, variables = variables, withFunc = 'with.humdrumR'), 
           envir = environment())
  

  
  result[ , `_rowKey_` := NULL][]
  ### Do we want extract the results from the data.table? 
  
  if (drop) {
    parts <- grepl('^_(by|subset)=..*_$', colnames(result))
    if (any(parts)) partNames <- do.call('paste', c(result[ , parts, with = FALSE], list(sep = ';')))
    
    result <- if (any(!parts)) result[[max(which(!parts))]]
    if (length(result) == 0L) return(result)
    
    if (is.list(result) && length(result) == 1L) {
      result <- result[[1]]
    } else {
      if (any(parts)) names(result) <- partNames
    }
  } else {
    visible <- TRUE
  } 
  
  attr(result, 'visible') <- NULL
  
  if (visible) result else invisible(result)
  
}

#' @rdname withinHumdrum
#' @export
within.humdrumR <- function(data, ..., dataTypes = 'D', expandPaths = FALSE, variables = list()) {
  checks(data, xclass('humdrumR'))
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, expandPaths = expandPaths, variables = variables, 
                       withFunc = 'within.humdrumR'), 
           envir = environment())
  
  if (all(quoTab[KeywordType == 'do', Keyword == 'dofx'])) return(data)
  
  # any fields getting overwritten
  overWrote <- setdiff(colnames(result)[colnames(result) %in% colnames(humtab)], '_rowKey_')
  
  bad <- overWrote %in% c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal',
                          'Piece', 'Spine', 'Path', 'Stop', 'Record', 'NData', 'Global', 'Null', 'Filter', 'Type')
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
  newhumtab <- newhumtab[ , !grep('_by=..*_$|_subset=..*_', colnames(newhumtab)), with = FALSE]
  
  #### Put new humtable back into humdrumR object
  newfields <- setdiff(colnames(newhumtab), colnames(humtab))
  
  notnull <- Reduce(`|`, lapply(newhumtab[, newfields, with = FALSE], \(field) if (is.logical(field)) logical(length(field)) else  !(is.na(field) | field == '.')))
  newhumtab$Null[notnull] <- FALSE
  newhumtab$Type[newhumtab$Type == 'd' & notnull] <- 'D'
  
  # What do do if d is in dataTypes
  if (any(grepl('d', dataTypes))) {
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

withHumdrum <- function(humdrumR, ..., dataTypes = 'D', expandPaths = FALSE, variables = list(), withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  
  if (expandPaths) humdrumR <- expandPaths(humdrumR, asSpines = FALSE)
  
  humtab <- getHumtab(humdrumR)
  humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
  
  # interpret ... arguments
  quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  
  # "pre" stuff
  oldpar <- par(no.readonly = TRUE) 
 
  
  # Getting the humtab with the right record types.
  dataTypes <- checkTypes(dataTypes, withFunc)
                      
  
  ### Preparing the "do" expression
  do   <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = FALSE)
  ordo <- prepareDoQuo(humtab, quoTab, humdrumR@Active, ordo = TRUE)
  # 

  #evaluate "do" expression! 
  result <- evalDoQuo(do, humtab[Type %in% dataTypes],  quoTab[KeywordType == 'partitions'],  ordo)
  
  visible <- attr(result, 'visible')
  attr(result, 'visible') <- NULL
  if (nrow(result) > 0L) data.table::setorder(result, `_rowKey_`)
  
  #### number the unnamed new results
  ## This is done here because if we call `with.humdrumR(drop = FALSE)`
  ## we want the same colnames as the new fields we would get.
  unnamedresult <- colnames(result) == 'Result'
  if (sum(unnamedresult)) colnames(result)[unnamedresult] <- paste0('Result', curResultN(humtab) + seq_len(sum(unnamedresult)))
  
  # "post" stuff
  curmfg <- par('mfg')
  par(oldpar[!names(oldpar) %in% c('mai', 'mar', 'pin', 'plt', 'pty', 'new')])
  par(mfg = curmfg, new = FALSE)
  
  if (expandPaths) {
    result <- result[!humtab[is.na(ParentPath)], on = '_rowKey_']
    humtab <- humtab[!is.na(ParentPath)]
  }
  
  list(humdrumR = humdrumR, 
       humtab = humtab,
       quoTab = quoTab,
       dataTypes = dataTypes,
       visible = visible,
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
    
    list(Quo = quo, Keyword = keyword, AssignTo = assign, Environment = quoA$Environment)
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
  knownKeywords <- list(do              = c('do', 'fx', 'fill', 'ordo'), #, 'ordofill'),
                        partitions      = c('by', 'subset'),
                        ngram           = 'ngram',
                        windows         = 'windows')
  quoTab[ , KeywordType := rep(names(knownKeywords), lengths(knownKeywords))[match(Keyword, unlist(knownKeywords))]]
  
  # check for validity
  if (!any(quoTab[ , KeywordType == 'do'])) {
    .stop("Your call to {withFunc} doesn't include any unnamed expressions to evaluate,",
          "so we don't know what you want to do to your data.")
  }
  
  if (any(quoTab[, KeywordType == 'or']) && !any(quoTab[, Keyword == 'subset'])) {
    .stop("In your call to {withFunc} you've included an 'complement' expression improperly.", 
          "An 'complement' expression can only be used in combination with BOTH a subset expression AND a normal within expression.",
          "See ?withinHumdrum for help.")
  }
  
  if (any(quoTab[ , Keyword == 'ngram'])) {
    i <- which(quoTab$Keyword == 'ngram')
    if (length(i) > 1L) .stop("In a call to {withFunc},",
                              "you can't have multiple 'ngram'-keyword arguments.")
    
    ngram <- rlang::eval_tidy(quoTab$Quo[[i]])
    checks(ngram, xpnatural)
    quoTab$Quo[[i]] <- ngram
  }
  
  
  quoTab
}

partialMatchKeywords <- function(keys) {
    # this function matches partial matches off keywords
    # to the master standard
    keys <- gsub('_', '', tolower(keys))
    
    # define standard keys and alternatives
    standardkeys <- list(do        = c('do', 'eval', 'apply'),
                         fill      = c('fill', 'recycle'),
                         fx        = c('fxs', 'sidefxs'),
                         by        = c('by', 'groupby'),
                         subset     = c('subset', 'where'),
                         ordo        = c('complement', 'rest', 'otherwise'),
                         # ordofill    = c('compfill', 'restfill', 'otherfill'),
                         ngram     = c('ngrams'),
                         windows   = c('windows', 'context'))
    
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
  
  dofills <- grepl('fill', doQuoTab$Keyword) | ordo
  doQuoTab$Quo[dofills] <- Map(fillQuo, doQuoTab$Quo[dofills], usedInExprs[dofills])
  
  
  doQuoTab$Quo <- Map(\(quo, used) {
    lists <- vapply(humtab[1 , used, with = FALSE], \(x) class(x)[1], FUN.VALUE = character(1)) == 'list' 
    if (any(lists)) mapifyQuo(quo, used, depth = 1L) else quo
  }, doQuoTab$Quo, usedInExprs)
  
  
  
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
    
    analE <- analyzeExpr(doQuo)
    
    if (analE$Head == '<-') {
      
      analE$Args[[2]] <- rlang::quo({
        targetlen <- max(lengths(list(!!!usedInExpr)))
        rep(!!analE$Args[[2]], length.out = targetlen)
      } )
      unanalyzeExpr(analE)
    } else {
      rlang::quo({
        targetlen <- max(lengths(list(!!!usedInExpr)))
        rep(!!doQuo, length.out = targetlen)
      } )
    }
    
    
    
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
    
    doQuo <- rlang::quo({
      
      result <- list(list(visible(withVisible({!!!doQuos}))))
      
      results <- c(lapply(list(!!!assignOut), \(assign) list(list(assign))), use.names = TRUE,
                   setNames(list(result), !!resultName),
                   list(`_rowKey_` = list(list(`_rowKey_`))))
      results
    }) 
    
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
  funcQuosure <- fieldArgQuo(funcQuosure, fields)
  funcQuosure <- byArgsQuo(funcQuosure)
  funcQuosure
}


fieldArgQuo <- function(funcQuosure, fields) {
  targetFields <- intersect(names(withinFields), fields)
  
  for (field in targetFields) {
    predicate <- \(Head) Head %in% withinFields[[field]]
    do <- \(exprA) {
      if (!field %in% names(exprA$Args)) exprA$Args[[field]] <- rlang::sym(field)
      exprA
    }
    
    funcQuosure <- withinExpression(funcQuosure, predicate, do, stopOnHit = FALSE)
    
  }
  
  funcQuosure
}


byArgsQuo <- function(funcQuosure) {
  predicate <- \(Head) Head %in% byTable$Function
  do <- \(exprA) {
    byTab <- byTable[Function %in% exprA$Head & !Argument %in% names(exprA$Args)]
    args <- setNames(byTab$Expression, byTab$Argument)
    exprA$Args <- c(exprA$Args, args)
    exprA
  }
  withinExpression(funcQuosure, predicate, do, stopOnHit = FALSE)
}


#### Lag/Led vectors

laggedQuo <- function(funcQuosure) {
  
  predicate <- \(Head, Args) Head == '[' && any(tolower(names(Args)) == 'lag') 
  
  do <- \(exprA) {
    
    args <- exprA$Args
    if (!'groupby' %in% .names(args)) args$groupby <- expr(list(File, Spine, Path))
    
    names(args)[tolower(names(args)) == 'lag'] <- 'n'
    n <- rlang::eval_tidy(args$n)
    if (!is.numeric(n) || ((n %% 1) != 0)) .stop('Invalid [lag = ] lag expression.')
    args$n <- NULL
    
    lagExprs <- lapply(n, \(curn) rlang::expr(lag(!!!args, n = !!curn)))

    exprA$Head <- 'splat'
    exprA$Args <- lagExprs
    
    exprA
  }
  
  withinExpression(funcQuosure, predicate, do, stopOnHit = TRUE)
  
}

#### Interpretations in expressions

#tandemsQuo <- function(funcQuosure) {
 # This function inserts calls to extractTandem
 # into an expression, using any length == 1 subexpression
 # which begins with `*` as a regular expression.
 # If input is a quosure (it should be), it keeps the quosure intact,
 # (i.e., keeps it's environment).
          
# withExpression(funcQuosure,
#           \(ex) {
#             exstr <- deparse(ex)
#             interp <- grepl('^\\*[^*]', exstr)
#             
#             if (interp) {
#               regex <- stringr::str_sub(exstr, start = 2L)
#               rlang::expr(extractTandem(Tandem, !!regex))
#             } else {
#               ex
#             }
#           }) 
#}


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
  
  funcQuosure <- withinExpression(funcQuosure, predicate, do)

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

  withinExpression(funcQuosure, predicate, do)
  
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
                      rlang::expr({
                        result <- Map(f = !!rlang::quo_get_expr(funcQuosure), 
                                              !!!lapply(usedInExpr, rlang::sym))
                        if (all(lengths(result) == 1L)) unlist(result) else result
                        
                        }))

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
  
  if (!'groupby' %in% .names(windowQuosure[[2]])) windowQuosure[[2]][['groupby']] <- quote(list(File,Spine))
  if (!'x' %in% .names(windowQuosure[[2]]) && .names(windowQuosure[[2]])[2] != '' ) windowQuosure[[2]][['x']] <- rlang::sym(usedInExpr[1])
  
  applyArgs <- as.list(windowQuosure[[2]][c('leftEdge', 'rebuild', 'passOutside')])
  windowQuosure[[2]] <- windowQuosure[[2]][!.names(windowQuosure[[2]]) %in% c('leftEdge', 'rebuild', 'passOutside')]
  
  
  rlang::quo(
    windowApply(func = !!funcQuosure,
                x = !!rlang::sym(usedInExpr[1]),
                windows = !!windowQuosure,
                !!!applyArgs))
  
}




# Change or insert values in an expression
# 
# This function can be used to modify arguments to a functions
# within an existing expression (or quosure/formula).
# 
# `interpolateArguments` inteprets named value in its `namedArgs` 
# argument in one of two ways: If the named value is a list, it interprets
# the name of the list as a function call, and inserts/swaps any arguments
# in that list into any instances of that function call within the `expr`.
# Named arguments are inserted or substituted if already present in expression.
# Unnamed argmuments are simply added to the call.
# Examples:
#
# ```
# myexpr <- quote(dnorm(x, mean = 5))
# interpolateArguments(myexpr, list(dnorm = list(mean = 2, sd = 5, TRUE)))
# 
# # result is new expresson: dnorm(x, mean = 2, sd = 5, TRUE)
# ```
#
# If a named valued in the `namedArgs` argument is not a list,
# that name/value pair is substituted anywhere it is present in the expression.
# This approach is often more conscise, but arguments cannot be added to an 
# expression this way, only substituted if already present.
# Examples:
# 
# ```
# myexpr <- quote(dnorm(x, mean = 5))
# interpolateArguments(myexpr, mean = 2)
# 
# # result is new expression: dnorm(x, mean = 2)
# }
# ```
#
# @examples
# myexpr2 <- quote(A + b*x + rnorm(length(a), mean(Z), sd = 2))
# 
# interpolateArguments(myexpr2,
#                      list(sd = 10, mean = list(na.rm = TRUE)))
#                        
# # result is new expression: 
# # a + b*x + rnorm(length(a), mean(Z, na.rm = TRUE), sd = 10)
# 
# 
# @param expr A unevaluated expression object.
# @param namedArgs A list of named arguments. Unnamed arguments are simply ignored.
# 
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




## Evaluating do quo in humtab ----

###########- Applying within.humdrumR's expression to a data.table




evalDoQuo <- function(doQuo, humtab, partQuos, ordoQuo) {
    result <- if(nrow(partQuos) == 0L) {
        as.data.table(rlang::eval_tidy(doQuo, data = humtab))
        
    } else {
        evalDoQuo_part(doQuo, humtab, partQuos, ordoQuo)
    }
   
    parseResult(result)
}
evalDoQuo_part <- function(doQuo, humtab, partQuos, ordoQuo) {
    ### evaluation partition expression and collapse results 
    ## to a single factor
    partType <- partQuos$Keyword[1]
    partition <- rlang::eval_tidy(partQuos$Quo[[1]], humtab)
    
    if (!is.list(partition)) partition <- list(partition)
    partition <- lapply(partition, rep, length.out = nrow(humtab))
    
    partition <- Reduce(switch(partType, by = paste, subset = `&`), partition)
    
    partEval <- switch(partType,
                       by    = evalDoQuo_by,
                       subset = evalDoQuo_subset)
    
    partEval(doQuo, humtab, partition, partQuos, ordoQuo)
    
}

evalDoQuo_by <- function(doQuo, humtab, partition, partQuos, ordoQuo) {
    # this function doesn't use reHum because data.table 
    # pretty much already does (some) of whats needed.
    targetFields <- namesInExprs(colnames(humtab), c(doQuo, partQuos[-1]$Quo))
    targetFields <- unique(c(targetFields, '_rowKey_'))
    
    partition <- as.factor(partition)
    
    nparts <- max(as.integer(partition), na.rm = TRUE)
    
    if (nparts > 1e5L) message("Your group-by expression {by = ",
                               rlang::as_label(partQuos$Quo[[1]]),
                               "} is evaluating to ", 
                               num2print(nparts), " groups.", 
                                " If your within-expression is complex, this could take a while!")
    
    if (nparts > 1L && nparts <= 16 && all(par()$mfcol == c(1, 1))) {
      oldpar <- par(no.readonly = TRUE,
                    mfcol = find2Dlayout(nparts))
      on.exit(par(oldpar[!names(oldpar) %in% c('mai', 'mar', 'pin', 'plt','pty', 'new')]))
    }
    partitionName <- paste0('_by=', gsub('  *', '', rlang::as_label(partQuos$Quo[[1L]])), '_')
    
    result <- if (nrow(partQuos) > 1) {
      results <- humtab[ , {
        evaled <- evalDoQuo_part(doQuo, .SD, partQuos[-1], ordoQuo)
        evaled[[partitionName]] <- partition
        list(list(evaled)) 
      },
      by = partition, .SDcols = targetFields]
      data.table::rbindlist(results$V1)
    } else {
      # quoEnv <- rlang::new_environment(list(humtab = humtab, partition = partition),
                                       # rlang::get_env(doQuo))
      # result <- eval(rlang::quo_squash(rlang::expr( humtab[ , !!doQuo, by = partition])),
                     # envir = quoEnv)
      result <- humtab[ , rlang::eval_tidy(doQuo, data = .SD), by = partition]
      colnames(result)[colnames(result) == 'partition'] <- partitionName
      result
    }
    
    result
    

    
}
evalDoQuo_subset <- function(doQuo, humtab, partition, partQuos, ordoQuo) {
  if (!is.logical(partition)) .stop("In your call to with(in).humdrumR with a 'subset = x' expression,",
                                    "your subset expression must evaluate to a logical (TRUE/FALSE) vector.",
                                   "The expression you've provided {{ {rlang::as_label(partQuos$Quo[[1]])} }}",
                                   " evaluates to something of class {class(partition)}.")
  
    
  if (!any(partition)) warning(call. = FALSE, 
                               "In your call to with(in).humdrumR, your subset never evaluates TRUE.",
                               " Your within-expression is being evaluated on nothing.")
  
    if (nrow(partQuos) > 1L) {
      
      result <- evalDoQuo_part(doQuo, humtab[partition], partQuos[-1], ordoQuo)
    
    } else {
      result <- as.data.table(rlang::eval_tidy(doQuo, data = humtab[partition]))
    }
    
    partitionName <- paste0('_subset=', gsub('  *', '', rlang::as_label(partQuos$Quo[[1L]])), '_')
    result[[partitionName]] <- TRUE
    
    if (!is.null(ordoQuo)) {
        complement <- as.data.table(rlang::eval_tidy(ordoQuo, data = humtab[!partition]))
        complement[[partitionName]] <- FALSE
        
        if (ncol(complement) > ncol(result)) complement <- complement[ , tail(seq_len(ncol(complement)), ncol(result)), with = FALSE]
        
        mismatch <-  !colnames(complement) %in% colnames(result)
        colnames(complement)[mismatch] <- tail(head(colnames(result), -2L), sum(mismatch))
        
        result <- data.table::rbindlist(list(result, complement), use.names = TRUE, fill = TRUE)
    }
    
   result
}





#######################################################-
## Reassembling humtable ----
#######################################################-



parseResult <- function(results) {
    # this takes a nested list of results with associated
    # indices and reconstructs the output object.
  # if (length(result) == 0L || all(lengths(result) == 0L)) return(cbind(as.data.table(result), `_rowKey_` = 0L)[0])
  
  resultCols <- !grepl('^_..*_$|partition', colnames(results))

  lastResult <- max(which(resultCols))
  firstResult <- results[[lastResult]][[1]][[1]]
  
  keyLengths <- lengths(unlist(results[['_rowKey_']], recursive = FALSE))
  resultLengths <- if (length(firstResult) && 
                       (is.table(firstResult) || 
                       (is.object(firstResult) && !is.atomic(firstResult)))) lengths(results[[lastResult]]) else sapply(results[[lastResult]], lengths) 
  
  
  
  # results <- results[ , !grepl('^_..*_$|partition', colnames(results)), with = FALSE]
  
  if (any(resultLengths > keyLengths)) {
    pairs <- subset(unique(data.frame(resultLengths, keyLengths)), resultLengths > keyLengths)
    
    .stop("Sorry, with(in).humdrumR doesn't currently support within-expressions", 
          "which return values that are longer than their input field(s).",
          "Your expression has returned results of lengths {harvard(pairs$resultLengths, 'and')} evaluated from",
          "inputs of length {harvard(pairs$keyLengths, 'and')}<, respectively|>.",
          ifelse = nrow(pairs) > 1L)
  }
  
  
  results <- lapply(results, 
                    \(result) {
                      if (!is.list(result)) return(rep(result, resultLengths)) # this should only be partitition columns
                      first <- result[[1]][[1]]
                      
                      humattr <- humdrumRattr(first)
                      class <- class(first)
                      object <- length(first) && (is.table(first) || (is.object(first) && !is.atomic(first)))
                      
                      # if (is.table(result) && length(result) == 0L) result <- integer(0)
                      result <- unlist(result, recursive = FALSE)
                      
                      if (!object) {
                        factors <- sum(sapply(result, is.factor))
                        if (factors > 0L && factors < length(result)) result <- lapply(result, as.character)
                        result <- Map(\(r, l) r[seq_len(l)], result, pmin(lengths(result), resultLengths))
                        result <- unlist(result, recursive = FALSE)
                      }
                      
                      humdrumRattr(result) <- humattr
                      attr(result, 'visible') <- NULL
                      if (!is.null(result)) class(result) <- if (object) 'list' else class
                      result 
                    })
  

  
  
 
    
  result <- as.data.table(results)
    
  colnames(result)[colnames(result) == ""] <- "Result"
  colnames(result) <- gsub('^V{1}[0-9]+', "Result", colnames(result))
    
  
  attr(result, 'visible') <- attr(firstResult, 'visible') %||% TRUE
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


`resultFields<-` <- function(object, value) {
  humtab <- getHumtab(object)
  
  results <- rev(resultFields(humtab))[seq_along(value)]
  
  for (i in seq_along(value)) {
    colnames(humtab)[colnames(humtab) == results[i]] <- value[i]
    removeFields(object) <- results[i]
  }
  
  putHumtab(object) <- humtab
  
  addFields(object) <- value
  object
}

