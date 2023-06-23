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
#' + Across contextual windows in the data (e.g., ngrams, rolling windows).
#' + Which produce a plots with particular [plotting parameters][graphics::par()], and/or without 
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
#' which means the expression can refer fields in the humdrumR object by name (`Record`, `Token`, `Piece`, etc.)
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
#' will automatically feed the fields `list(Piece, Spine, Path)`.
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
#' Note that, since `with`/`within.humdrumR` passes `groupby = list(Piece, Spine, Path)`
#' to [lag()], these are true "melodic" n-grams, only created within spine-paths within each piece.
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
#'      by = Piece)
#' ```
#'
#' will apply the function `[base][table]` to the `Token` field
#' *separately* for each piece in the `humdrumR` data. 
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
#'      by = list(Piece, Spine))
#' ````
#'
#' will apply `table` to `Token` across each spine *in* each piece.
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
#'          by = Piece, 
#'          subset = Semits > mean(Semits))
#' ```
#' 
#' the standard deviation of the `semits` field will be calculated in each piece,
#' but only where the `semits` field is greater than the mean `semits` value
#' *within that file*. Contrast this with this call:
#' 
#' ```
#' within(humdata,
#'          sd(Semits)
#'          subset = Semits > mean(Semits), 
#'          by = Piece) 
#' ```
#' 
#' wherein the standard deviation of `semits` is, again, calculated for each piece,
#' but this time wherever the `semits` field is greater than the mean value *across all the data*.
#' 
#' @section Windowing data:
#' 
#' `with`/`within.humdrumR` can work in tandem with the `context()` function to evaluate do expressions 
#' within arbitrary contextual windows.
#' To do so, simply place a call to `context()` in an unnamed argument to `with`/`within.humdrumR()`;
#' you must provide `open` and `close` arguments.
#' The `overlap`, `depth`, `rightward`, `duplicate_indices`, `min_length`, and `max_length` arguments can be used here,
#' just as they are in a separate call to `context()` --- see the `context()` man page.
#' Do not give the `context()` command inside a `with`/`within.humdrumR` call a `x` or `reference` argument:
#' The [active field][humActive] is used as the `reference` vector for `context()` --- e.g., if `open`
#' or `close` is a `character` string, they are matched as regular expressions against the [active field][humActive].
#' `list(Piece, Spine, Path)` is also automatically passed as the `groupby` argument to `context()`.
#' 
#' 
#' When using `context()` inside `with`/`within.humdrumR`, the `alignToOpen` argument will have no effect.
#' If you want output to align with the right side of each window, use `alignLeft = FALSE` as an argument to 
#' **with**/**within**, not as an argument to `context()`.
#' The `inPlace`, `collapse`, `sep`, and `stripRegex` arguments will have no effect when `context()` is used
#' as syntactic sugar in a `with`/`within.humdrumR` call.
#' 
#' 
#' As with a `subset` expression, you can provide a `complement` expression to go along with a `context()`.
#' (Note that the `complement` argument to `context()` itself is ignored.)
#' The `complement` expression will be evaluated on any data points that aren't captured within *any* contextual windows.
#' 
#' 
#' Any `by` or `subset` expressions placed before a `context()` expression are evaluated first, in order from left to right.
#' This means you can *first* run subset or `groupby`, then calculate context within the subset/partition.
#' However, any `subset` or `groupby` expressions that are passed in an argument *after* `context()` are ignored.

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
#' bys <- list(~ Spine, ~ Piece, ~ COM)
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
#' @param data ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Any number of expressions to evaluate.*** 
#'
#' Unnamed expressions are interpreted as the "main" *within-expressions*. 
#' Possible *evaluation control arguments* include `by`, `subset`, and `context`.
#' Other evaluation options can be achieved with `recycle` or `side` arguments.
#' 
#' @param dataTypes ***Which types of humdrum records to include.***
#' 
#' Defaults to `"D"`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation **Fields** section for explanation.)
#'
#' @param .by ***Optional grouping fields; an alternative to using [group_by()].***
#'
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or `character` strings which [partially match][partialMatching] one or more
#' [fields()] in the `data`.
#'
#' If not `NULL`, these fields are used to group the data.
#' If grouping fields have already been set by a call to [group_by()],
#' the `.by` argument overrides them.
#'
#' @param variables ***A named `list` of values, which are interpolated into the within-expression(s) wherever a variable name matches a named from the list.***
#' 
#' Defaults to `list()`.
#' 
#' Must be `list`.
#' 
#' @param alignLeft ***Should output that is shorter than input be aligned to the left?***
#'
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param drop ***Whether to return a simplified data structure.***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
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
                          recycle = c("pad", "scalar", "even", "always", "never", "summarize"),
                          drop = TRUE,
                          .by = NULL,
                          variables = list()) {
  withFunc <- paste0(if (any(as.character(sys.call(1)[[1]]) %in% c('summarize', 'pull')))  as.character(sys.call(1)[[1]]) else 'with', '.humdrumR')
  
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, expandPaths = expandPaths, recycle = recycle, 
                       .by = .by, variables = variables, withFunc = withFunc), 
           envir = environment())
  
  result[ , `_rowKey_` := NULL][]
  ### Do we want extract the results from the data.table? 
  
  if (drop) {
    
    result <- result[[max(which(!colnames(result) %in% groupFields))]]
    if (length(result) == 0L) return(result)
    
    if (is.list(result) && length(result) == 1L) {
      result <- result[[1]]
    }
  } else {
    visible <- TRUE
  } 
  
  attr(result, 'visible') <- NULL
  
  if (visible) result else invisible(result)
  
}


#' @rdname withinHumdrum
#' @export
within.humdrumR <- function(data, ..., 
                            dataTypes = 'D', 
                            alignLeft = TRUE,
                            expandPaths = FALSE, 
                            recycle = c("pad", "scalar", "even", "always", "never", "summarize"),
                            .by = NULL,
                            variables = list()) {
  withFunc <- paste0(if (any(as.character(sys.call(1)[[1]]) %in% c('mutate', 'reframe', 'subset', 'filter')))  as.character(sys.call(1)[[1]]) else 'within', '.humdrumR')
  
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, alignLeft = alignLeft,
                       expandPaths = expandPaths, recycle = recycle, .by = .by, variables = variables, 
                       withFunc = withFunc), 
           envir = environment())
  
  result <- result[ , !names(result) %in% groupFields, with = FALSE]
  
  # any fields getting overwritten
  overWrote <- setdiff(colnames(result)[colnames(result) %in% colnames(humtab)], '_rowKey_')
  
  bad <- overWrote %in% c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal',
                          'Piece', 'Spine', 'Path', 'Stop', 'Record', 'DataRecord', 'Global', 'Type')

  #fields(humdrumR, 'S')$Name
  if (any(bad)) {
    if ('Token' %in% overWrote[bad]) {
      .stop("In your call to withinHumdrum, you can't overwrite the 'Token' field.",
            "This field should always keep the original humdrum data you imported.")
    }
    .stop("In your call to withinHumdrum, you can't overwrite structural fields.",
          ifelse = sum(bad) > 1L, 
          "You are attempting to overwrite the {harvard(overWrote[bad], 'and', quote = TRUE)} <fields|field>.",
          "For a complete list of structural fields, use the command fields(mydata, 'S').")
  }
  
 
  ## put result into new humtab
  newhumtab <- result[humtab[ , !colnames(humtab) %in% overWrote, with = FALSE], on ='_rowKey_'] 
  newhumtab[ , `_rowKey_` := NULL]
  
  #### Put new humtable back into humdrumR object
  newFields <- setdiff(colnames(newhumtab), colnames(humtab))

  newhumtab <- addExclusiveFields(newhumtab, newFields)
  newhumtab <- update_humdrumR.data.table(newhumtab, field = c(newFields, overWrote))
  humdrumR@Humtable <- newhumtab
  
  if (length(newFields)) {
    humdrumR <- updateFields(humdrumR)
  }
  humdrumR
  # update_humdrumR(humdrumR, field = c(newfields, overWrote))


}


withHumdrum <- function(humdrumR, ..., dataTypes = 'D', recycle = c("pad", "scalar", "even", "always", "never", "summarize"), 
                        alignLeft = TRUE, expandPaths = FALSE, variables = list(), .by = NULL, withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  dataTypes <- checkTypes(dataTypes, withFunc) # use this to index humtab later
  recycle <- match.arg(recycle)
  
  if (expandPaths) humdrumR <- expandPaths(humdrumR, asSpines = FALSE)
  
  humtab <- data.table::copy(getHumtab(humdrumR))
  humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
  
  # quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  quosures <- rlang::enquos(...)
  if (length(quosures) == 0L) .stop("In a call to {withFunc}, you must provide an expression to evaluate!")
  
  
  ### Preparing the "do" quosure
  fields <- fields(humdrumR)
  dotField <- fields[Selected == 1L]$Name
  
  ##### grouping
  groupFields <- getGroupingFields(humdrumR, .by, withFunc) 
  
  doQuo  <- prepareDoQuo(humtab, quosures, dotField, recycle)
  
  ## Evaluate "do" expression! 
  result <- evaluateDoQuo(doQuo, humtab[Type %in% dataTypes], groupFields, humdrumR@Context)
  result <- parseResult(result, groupFields, recycle, alignLeft, withFunc)
  
  
  visible <- attr(result, 'visible')
  attr(result, 'visible') <- NULL
  if (nrow(result) > 0L) data.table::setorder(result, `_rowKey_`)
  
  #### rename unnamed results
  ## This is done here because if we call `with.humdrumR(drop = FALSE)`
  ## we want the same colnames as the new fields we would get.
  result <- renameResults(result, quosures, groupFields, colnames(humtab))

  
  # "post" stuff
  # curmfg <- par('mfg')
  # par(oldpar[!names(oldpar) %in% c('mai', 'mar', 'pin', 'plt', 'pty', 'new')])
  # par(mfg = curmfg, new = FALSE)
  
  if (expandPaths) {
    result <- result[!humtab[is.na(ParentPath)], on = '_rowKey_']
    humtab <- humtab[!is.na(ParentPath)]
  }
  
  list(humdrumR = humdrumR, 
       humtab = humtab,
       dataTypes = dataTypes,
       visible = visible,
       groupFields = groupFields,
       result = result)
}



 

          

## Preparing doQuo ----

prepareDoQuo <- function(humtab, quosures, dotField, recycle) {
  # This is the main function used by [.withinmHumdrum] to prepare the current
  # do expression argument for application to a [humdrumR][humdrumRclass] object.
  
  # what fields are used, and what are their classes?
  usedInExprs <- lapply(quosures, fieldsInExpr, humtab = humtab)
  usedClasses <- vapply(humtab[ , unique(unlist(usedInExprs)), with = FALSE], \(x) class(x)[1], FUN.VALUE = character(1)) 
  
  # if the targets are lists, Map
  quosures <- Map(quosures, usedInExprs, 
                  f = \(quo, curUsed) { 
                    if (any(usedClasses[curUsed] == 'list')) {
                      mapifyQuo(quo, curUsed, depth = 1L) 
                    } else {
                      quo
                    }
                  })
  
  
  # collapse doQuos to a single doQuo
  doQuo <- concatDoQuos(quosures)

  # turn . to selected field
  doQuo <- activateQuo(doQuo, dotField)
  
  # lagged vectors
  doQuo <- laggedQuo(doQuo)
  
  # add in arguments that are already fields
  doQuo <- autoArgsQuo(doQuo, humtab)
  
  
  # splats
  doQuo <- splatQuo(doQuo, humtab)
  
  
  # We may have added to the expr, so update what fields (if any) are used in formula
  attr(doQuo, 'usedFields') <- unique(fieldsInExpr(humtab, doQuo))

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

concatDoQuos <- function(quosures) {
    ## this function takes a named list of quosures and creates a single quosure
    # which applies each of them in turn.
   
    
    assignOut <- list()
    resultName <- "_Result_"
    varname <- quote(.)
    
    whichResult <- length(quosures)
    
    for (i in 1:length(quosures)) {
        if (is.null(quosures[[i]])) next
        if (i > 1L) quosures[[i]] <- substituteName(quosures[[i]], list(. = varname))
        
        # if (sideEffects[i]) next
        
        exprA <- analyzeExpr(quosures[[i]])
        
        if (exprA$Head == '<-') {
          varname <- exprA$Args[[1]]
          if (i != whichResult) {
            assignOut <- c(assignOut, varname)
          } else {
            resultName <- as.character(varname)
          }
        } else {
          if (i  < length(quosures)) {
            varname <- rlang::sym(tempfile('xxx', tmpdir = ''))
            quosures[[i]] <- rlang::new_quosure(expr(!!varname <- !!quosures[[i]]), 
                                              env = rlang::quo_get_env(quosures[[i]]))
          }
        }
    }
    # if (is.null(assignOut[[length(assignOut)]]))  assignOut$Result <- quote(result)
    names(assignOut) <- sapply(assignOut, as.character)
    
    doQuo <- rlang::quo({
      
      result <- list(list(visible(withVisible({!!!quosures}))))
      
      results <- c(lapply(list(!!!assignOut), \(assign) list(list(assign))), use.names = TRUE,
                   setNames(list(result), !!resultName),
                   list(`_rowKey_` = list(list(`_rowKey_`))))
      results
    }) 
    
    doQuo
}

####################### Functions used inside prepareQuo

activateQuo <- function(funcQuosure, dotField) {
  # This function takes the `expression` argument
  # from the parent [withinHumdrum] call and 
  # inserts the first selected field (dotField) from the 
  # target [humdrumRclass] object in place 
  # of any `.` subexpressions.
  dotField <- rlang::sym(dotField)
  substituteName(funcQuosure, list(. = dotField))

  # dotField <- rlang::syms(dotField)
  # funcQuosure <- substituteName(funcQuosure, list(. = quote(!!!dotField)))
  
  # funcQuosure <- rlang::eval_tidy(rlang::quo(rlang::quo(!!funcQuosure)))
  # funcQuosure
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


autoArgsQuo <- function(funcQuosure, humtab) {
  predicate <- \(Head) Head %in% c(autoArgTable$Function, paste0(autoArgTable$Function, '.default'))
  do <- \(exprA) {
    tab <- autoArgTable[(Function == exprA$Head | paste0(Function, '.default') == exprA$Head) & 
                          !Argument %in% names(exprA$Args) &
                          sapply(Expression, \(expr) length(fieldsInExpr(humtab, expr)) > 0L)]
    args <- setNames(tab$Expression, tab$Argument)
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
    if (!'groupby' %in% .names(args)) args$groupby <- expr(list(Piece, Spine, Path))
    
    names(args)[tolower(names(args)) == 'lag'] <- 'n'
    n <- rlang::eval_tidy(args$n)
    if (!is.numeric(n) || any((n %% 1) != 0)) .stop('Invalid [lag = ] lag expression.')
    args$n <- NULL
    
    lagExprs <- lapply(n, \(curn) rlang::expr(lag(!!!args, n = !!curn)))

    exprA$Head <- 'splat'
    exprA$Args <- lagExprs
    
    exprA
  }
  
  withinExpression(funcQuosure, predicate, do, stopOnHit = TRUE)
  
}

#### Interpretations in expressions


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
  
  if (!'groupby' %in% .names(windowQuosure[[2]])) windowQuosure[[2]][['groupby']] <- quote(list(Piece, Spine))
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


prepareContextQuo <- function(contextQuo, dotField) {
  exprA <- analyzeExpr(contextQuo)
  
  exprA$Head <- 'findWindows'
  exprA$Args$field <- dotField
  
  passAsExprs <- .names(exprA$Args) %in% c('open', 'close', '')
  exprA$Args[passAsExprs] <- lapply(exprA$Args[passAsExprs], \(expr) call('quote', expr))
  
  if (!'groupby' %in% names(exprA$Args)) exprA$Args$groupby <- quote(list(Piece, Spine, Path)) 
  exprA$Args$x <- quote(humtab)
  
  unanalyzeExpr(exprA)
}



## Evaluating do quo in humtab ----

###########- Applying within.humdrumR's expression to a data.table


evaluateDoQuo <- function(doQuo, humtab, groupFields, windowFrame) {
  usedFields <- union(attr(doQuo, 'usedFields'), groupFields)
  
  if (nrow(windowFrame)) {
    humtab <- windows2groups(humtab, windowFrame)
    usedFields <- c(usedFields, 'contextWindow')
    groupFields <- c(groupFields, 'contextWindow')
  }
  
  result <- if (length(groupFields)) {
    humtab[ , rlang::eval_tidy(doQuo, data = .SD), by = groupFields, .SDcols = usedFields]
  } else {
    humtab[ ,  rlang::eval_tidy(doQuo, data = .SD), .SDcols = usedFields] # outputs a data.table this way
  }
 
  if (nrow(windowFrame)) result[, contextWindow := NULL]
  
  result 
}


evalDoQuo_context <- function(doQuo, humtab, partQuos, ordoQuo) {
  
  contextQuo <- partQuos$Quo[[1]]
  
  windowFrame <- eval(rlang::quo_squash(contextQuo), envir = humtab)
  
  result <- if (nrow(windowFrame)) {
    humtab_extended <- windows2groups(humtab, windowFrame)
    humtab_extended[ , rlang::eval_tidy(doQuo, data = .SD), by = contextWindow]
  } else {
    data.table(contextWindow = integer(0L), Result = list(), `_rowKey_` = list())
  }

  
  
  if (!is.null(ordoQuo)) {
    # notused <- setdiff(seq_len(nrow(humtab)))
    
    complement <- as.data.table(rlang::eval_tidy(ordoQuo, data = humtab[!`_rowKey_` %in% unlist(result[['_rowKey_']])]))
    complement[ , contextWindow := 0L]
    
    if (ncol(complement) > ncol(result)) complement <- complement[ , tail(seq_len(ncol(complement)), ncol(result)), with = FALSE]
    
    mismatch <-  !colnames(complement) %in% colnames(result)
    colnames(complement)[mismatch] <- tail(head(colnames(result), -2L), sum(mismatch))
    
    result <- data.table::rbindlist(list(result, complement), use.names = TRUE, fill = TRUE)
  }
  
  result
  # doQuo <- rlang::quo_squash(doQuo)
  # humtab_extended[ , eval(doQuo), by = contextWindow]
  
}





#######################################################-
## Reassembling humtable ----
#######################################################-



parseResult <- function(results, groupFields, recycle, alignLeft, withFunc) {
    # this takes a nested list of results with associated
    # indices and reconstructs the output object.
  # if (length(result) == 0L || all(lengths(result) == 0L)) return(cbind(as.data.table(result), `_rowKey_` = 0L)[0])
  
  grouping <- results[ , groupFields, with = FALSE]
  rowKey <- unlist(results[['_rowKey_']], recursive = FALSE)
  results <- results[ , setdiff(names(results), c(groupFields, '_rowKey_')), with = FALSE]
  results[] <- lapply(results, unlist, recursive = FALSE)
  
  visibleResult <- attr(results[[length(results)]][[1]], 'visible') %||% TRUE
  
  
  results <- as.matrix(results) # allows me to lapply across all all at once
  # results <- do.call('cbind', results)
  
  # "objects" are treated like length 1, and not vectorized
  objects <- array(FALSE, dim(results), dimnames(results))
  objects[] <- sapply(results, 
                      \(resultGroup) {
                        length(resultGroup) &&
                          (is.table(resultGroup) || (is.object(resultGroup) && !is.atomic(resultGroup)))
                        })
  
  # pad results and/or trim rowKey to make them match in size
  c('results', 'rowKey') %<-% recycleResults(results, objects, rowKey, recycle, alignLeft, withFunc)
  
  # need to rbind result groups
  results <- as.list(as.data.frame(results))
  objects <- colSums(objects) > 0L
  
  groupLengths <- lengths(rowKey)
  results <- Map(results, objects,
                 f = \(group, isObject) {
                   # need to 
                   # 1) pad results which are shorter than the longest result
                   # 2) concatinate vectorized results
                   if (isObject) {
                     result <- vector('list', sum(groupLengths))
                     ind <- if (alignLeft) 1L + head(cumsum(c(0L, groupLengths)), -1L) else cumsum(groupLengths)
                     result[ind] <- group
                   } else {
                     if (hasdim(group[[1]])) {
                       short <- sapply(group, nrow) < groupLengths
                       group[short] <- Map(group[short], groupLengths[short], f = \(g, n) g[seq_len(n), , drop = FALSE])
                     } else {
                       short <- lengths(group) < groupLengths
                       group[short] <- Map(group[short], groupLengths[short], f = \(g, n) g[seq_len(n)])
                     }
                     result <- .unlist(group, recursive = FALSE) 
                   }
                   attr(result, 'visible') <- NULL
                   result
                   
                 })
  
  arrays <- sapply(results, is.array)
  result <- as.data.table(results)
  # arrays are split into new columns
  # for (array in names(results)[arrays]) {
    # result[[array]] <- results[[array]]
  # }
  
  result <- cbind(`_rowKey_` = unlist(rowKey), as.data.table(lapply(grouping, rep, lengths(rowKey))), result)
  
  attr(result, 'visible') <- visibleResult 
  
  result
    
}



recycleResults <- function(results, objects, rowKey, recycle, alignLeft, withFunc) {
  
  # length of each result group
  resultLengths <- array(as.integer(objects), dim(objects), dimnames(objects))
  resultLengths[!objects] <- unlist(lapply(results[!objects], \(resultGroup) if (hasdim(resultGroup)) nrow(resultGroup) else length(resultGroup)))
  
  keyLengths <- lengths(rowKey)
  keyLengths <- do.call('cbind', rep(list(keyLengths), ncol(results)))
  
  diff <- resultLengths - keyLengths
  
  if (any(diff > 0L)) .stop("Sorry, {withFunc}() doesn't currently support expressions",
                            "which return values that are longer than their input field(s).",
                            "Your expression has returned results of lengths {harvard(head(resultLengths[diff > 0L], 5), 'and')} evaluated from",
                            "inputs of length {harvard(head(keyLengths[diff > 0L], 5), 'and')}<, respectively|>.",
                            ifelse = length(results) > 1L)
  
    
  match  <- diff == 0L
  scalar <- resultLengths == 1L
  
  if (recycle == 'summarize' && !(all(scalar))) .stop("When using summarize() on humdrumR data, the result of each expression must be a scalar (length 1).")
  
  if (!recycle %in% c('pad', 'summarize') && any(!match & !objects)) {
    bad <- !match & switch(recycle,
                           scalar    = resultLengths != 1L,
                           even      = keyLengths %% resultLengths != 0L,
                           never     = TRUE,
                           FALSE)
    if (any(bad)) .stop("The {withFunc} command won't recycle these results because the recycle argument is set to '{recycle}.'",
                        switch(recycle, 
                               scalar = "Only scalar (length 1) results will be recycled.",
                               even = "Only results of a length that evenly divides the input will be recycled."),
                        "See the 'recycling' section of ?withHumdrum for explanation.")
    
    results[!match & !objects] <- Map(results[!match & !objects], keyLengths[!match & !objects], 
                                      f = \(result, keyLength) {
                                        if (hasdim(result)) {
                                          result[rep(1:nrow(result), length.out = keyLength), , drop = FALSE]
                                        } else {
                                          rep(result, length.out = keyLength)
                                        }
                                        
                                      })
    resultLengths[!match & !objects] <- keyLengths[!match & !objects]
  }
  
  
    
  # to make the (later) padding work, we may need to shorten the rowKey to match the result length
  maxGroupLength <- apply(resultLengths, 1L, max)
  keyLengths <- keyLengths[ , 1]
  keyTrim <- keyLengths > maxGroupLength
  
  if (any(keyTrim)) {
    rowKey[keyTrim] <- Map(rowKey[keyTrim], maxGroupLength[keyTrim], 
                           f = if (alignLeft) {
                             \(keys, len) {
                               keys[seq_len(min(length(keys), max(len, 0L)))]
                             }
                           } else {
                             \(keys, len) {
                               l <- length(keys)
                               ind <- (l - len + 1) : l
                               ind <- ind[ind > 0]
                               keys[ind]
                             }
                           })
    
  }
  list(results = results, rowKey = rowKey)
  
}


renameResults <- function(result, quosures, groupFields, allFields) {
  
  if (any(names(result) == "_Result_")) {
    names(result)[names(result) == '_Result_'] <- rlang::as_label(quosures[[length(quosures)]])
  }

  result
}

addExclusiveFields <- function(humtab, fields) {
  newcols <- lapply(humtab[ , fields, with = FALSE],
         \(field) {
           exclusive <- getExclusive(field)
           if (!is.null(exclusive)) {
             ifelse(is.na(field), NA_character_, exclusive)
           }
         })
  names(newcols) <- paste0('Exclusive.', fields)
  newcols <- Filter(length, newcols)
  cbind(humtab, as.data.frame(newcols))
}
