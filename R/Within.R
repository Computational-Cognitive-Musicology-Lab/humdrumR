
#################################################-
# within.humdrumR ----
##################################################-


#' Working *with* humdrum data fields
#' 
#' These functions are the primary means of working with
#' humdrumR data. 
#' They allow us to perform arbitrary (free form) manipulation of data [fields][fields()]
#' held within a [humdrumR data object][humdrumRclass], with convenient functionality
#' for ignoring null data, [lagging][lag()] data, [grouping][groupHumdrum] data, [windowing][context()], and more.
#' The `with()` and `within()` functions, which come from [base R][base], are the core functions.
#' However, the [dplyr] "verbs" `mutate()`, `summarize()`, and `reframe()` can be used as well---they
#' are equivalent to using `with()`/`within()` with particular arguments.
#' 
#' 
#' @section Overview:
#' 
#' These functions are the primary means of working with
#' [humdrumR data][humdrumRclass]. 
#' They all allow you to write code that accesses and manipulates the raw [fields()]
#' in our data.
#' The main differences between them are what they do with the *results* of our code:
#' `with()` and `summarize()` return results in normal, "raw" R formats, **removed** from the [humdrumR data][humdrumRclass];
#' In contrast, `within()`, `mutate()`, and `reframe()` always insert the results of your code into
#' new [fields()] **within** your humdrum data.
#' The other distinctions between these functions have to do with how they recycle/pad results (see below).
#' 
#' 
#'
#' @section Expression evaluation:
#'
#' The `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()` methods for [humdrumR data][humdrumRclass]
#' all perform "[non-standard evalation](http://adv-r.had.co.nz/Computing-on-the-language.html)" of 
#' any [expressions][evaluatingExpressions] you provide them as arguments.
#' Basically, when you use a function like `with(...)` or `mutate(...)`, the expressions you write inside
#' the function call aren't [evaluated][evaluatingExpressions] right then and there---instead, R takes those expressions
#' into the "[environment][evaluatingExpressions]" of your [humdrum table][humTable], where
#' all your fields are "visible" to the expression.
#' This means you can write code (expressions) that refer to your [fields()], like `Token` or `Spine`.
#' For example:
#' 
#' ```
#' with(humData, 
#'      ifelse(Spine > 2, 
#'             kern(Token), 
#'             recip(Token)))
#' ```
#' 
#' Since all the fields in a [humdrum table][humTable] are the same length, the expressions you write
#' can be, and generally should be, [vectorized][vectorization].
#' 
#' By default, `with()`, `within()`, etc. don't use the whole [humdrum table][humTable],
#' but instead only evaluate their expressions using rows correspoing to non-null data tokens (`Type == "D"`).
#' This means that interpretations, comments, barlines, and null data tokens are automatically ignored for you!
#' This feature is controlled by the `dataTypes` argument:
#' you can choose to work with the other token types by providing a `character` string containing combinations
#' of the characters `G` (global comments), `L` (local comments), `I` (interpretations), 
#' `M` (barlines), `D` (non-null data), or `d` (null data).
#' For example, `dataTypes = 'MDd'` will evaluate your expressions on barline tokens (`=`), as well as both null
#' and non-null data.
#' See the [ditto()] manual for an example application of using `dataTypes = 'Dd'`.
#' 
#' If multiple expression arguments are provided, each expression is evaluated in order (left to right).
#' Each expression can refer variables assigned in the previous expression (examples below).
#' 
#' ### Expression Pre-processing
#' 
#' The `with()`, `within()`, `mutate()`, `summarize()`, or `reframe()` methods all do some
#' pre-processing of your expressions before evaluatinng thing.
#' This pre-processing provides some convenient "[syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar)" 
#' for working with humdrum data.
#' There are currently six pre-processes:
#' 
#' + Variable interpolation.
#' + The `.` placeholder for selected fields.
#' + Automatic argument insertion.
#' + "Lagged"-vectors shorthand.
#' + "Splatted" arguments.
#' + List mapping.
#' 
#' Each of these is explained below.
#' 
#' #### Manual variable interpolation
#'  
#' The `variable` argument can be provided as an (option) `list` of named values.
#' If any of the names in the `variable` list appear as symbols (variable names)
#' in any expression argument, their value is interpolated in place of that symbol.
#' For example, in
#'
#' ```
#' within(humData, kern(Token, simple = x), variable(x = TRUE))
#' 
#' ```
#' 
#' the variable `x` will be changed to `TRUE`, resulting in:
#' 
#' ```
#' within(humData, kern(Token, simple = TRUE))
#' 
#' ```
#' 
#' This feature is most useful for programmatic purposes, like if you'd like
#' to run the same expression many times but with slightly different parameters.
#' 
#' 
#' 
#' #### The . placeholder
#' 
#' The `.` variable can be used as a special placeholder representing the data's first
#' [selected field][selectedFields].
#' For example, in
#' 
#' ```
#' humData |>
#'   select(Token) |>
#'   with(tally(.))
#' 
#' ```
#' 
#' will run [tally()] on the `Token` field.
#' 
#' Because new fields created by `within()`/`mutate()`/`reframe()` become the [selected fields][selectedFields]
#' (details below), the `.` makes it easy to refer to the *last* new field in pipes.
#' For example, in
#' 
#' 
#' ```
#' humData |>
#'    mutate(kern(Token, simple = TRUE)) |>
#'    with(tally(.))
#' 
#' ```
#' 
#' the `tally()` function is run on the output of the `mutate(kern(Token, simpe = TRUE))` expression.
#
#' #### Automatic argument insertion
#' 
#' Many [humdrumR] functions are designed to work with certain common fields in [humdrumR data][humdrumRclass].
#' For example, many [pitch functions][pitchFunctions] have a `Key` argument, which (can) take the 
#' content of the `Key` field, which is automatically created by [readHumdrum()] if there are key interpretations,
#' like `*G:` in the data.
#' When an expression argument uses one of these functions, but doesn't explicitly set the argument, humdrumR
#' will *automatically* insert the appropriate field into the call (if the field is present).
#' So, for example, if you run
#' 
#' ```
#' humData |> 
#'    mutate(Solfa = solfa(Token))
#' ```
#'
#' on a data set that includes a `Key` field, the expression will be changed to:
#' 
#' ```
#' humData |> 
#'    mutate(Solfa = solfa(Token, Key = Key))
#' ```
#' 
#' If you *don't* want this to happen, you need to explicitly give a different `Key` argument, like:
#' 
#' ```
#' humData |> 
#'    mutate(Solfa = solfa(Token, Key = 'F:'))
#' ```
#' 
#' (The `Key` argument can also be set to `NULL`).
#' 
#' Another common/important automatic argument insertion is for functions with a `groupby` argument.
#' These functions will automatically have appropriate grouping fields inserted into them.
#' For example, the [mint()] (melodic intervals) command will *automatically* by applied using `groupby`
#' `groupby = list(Piece, Spine, Path)`, which makes sure that melodic intervals are only calculated within
#' spine paths...not between pieces/spines/paths (which wouldn't make sense!).
#' 
#' All `humdrumR` functions which use automatic argument interpolation will mention it in their own documentation.
#' For example, the [?solfa] documentation mentions the treatment of `Key` in it's "Key" section.
#' 
#' #### Lagged vectors
#' 
#' In music analysis, we very often want to work with "[lagged][lag()]" vectors of data.
#' For example, we want to look at the relationship between a vector and the previous values of the 
#' same vector---e.g., the vector offset or "lagged" by one index.
#' The [lag()] and [lead()] functions are useful for this,
#' always keeping them the same length so vectorization is never hindered.
#' 
#' In expression arguments, we can use a convenient shorthand to call `lag()` (or `lead`).
#' In an expression, any vector can be indexed with an `integer` argument named `lag` or `lead` (case insensitive),
#' causing it to be lagged/ead by that integer ammount.
#' (A vector indexed with `lag = 0` returns the unchanged vector.)
#' For example, the following two calls are the same:
#' 
#' ```
#' with(humData, Token[lag = 1])
#' with(humData, lag(Token, 1))
#' ```
#' 
#' This is most useful if the `lag`/`lead` index has *multiple* values:
#' if the indexed object appears within a higher function call,
#' each lag is inserted as a *separate* argument to that call.
#' Thus, *these* two calls are also the same:
#' 
#' ```
#' with(humData, table(Token[lag = 1:2])
#' 
#' with(humData, table(lag(Token, 1), lag(Token, 2))
#' ```
#' 
#' Note that the lagging will also be automatically be grouped within the fields `list(Piece, Spine, Path)`,
#' which is the default "melodic" structure in most data.
#' This assures that a vector is "lagged" from one piece to another, or from one spine to the next.
#' If you'd like to turn this off or change the grouping, you need to override it by adding a
#' `groupby` argument to the lagged index, like `Token[lag = 1, groupby = list(...)]`.
#' 
#' 
#' Using lagged vectors, since they are vectorized, is the fastest (computationally) and easiest way of working with n-grams.
#' For example, if you want to create `character`-string 5-grams of your data, you could call:
#' 
#' ```
#' with(humData, paste(Token[lag = 0:5], sep = '-'))
#' ```
#' 
#' Since the lagging is grouped by `list(Piece, Spine, Path)`, 
#' these are true "melodic" n-grams, only created within spine-paths within each piece.
#'  
#' 
#' @section Parsing expression results:
#' 
#' The main differences between the `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()` humdrumR methods
#' are what they do with the *results* of code passed to them.
#' The major difference is that `within()`, `mutate()`, and `reframe()` put results into new [fields]
#' in a [humdrumR data][humdrumRclass], while `with()` and `summarize()` just return their results in "normal" R.
#' The other differences between the functions simply relate to how the `recycle` and `drop` arguments are used (details below).
#' 
#' The `recycle` argument controls how the results of your code are, or aren't, [recycled (or padded)][recycling].
#' When you write code using your [humdrumR data][humdrumRclass]'s [fields()]
#' as input, your results are inspected to see how long they are compared to the length of the input field(s).
#' If any of your results are *longer* than the input, you'll get an error message---`humdrumR` can't (yet) handle that case.
#' If any of your results are *shorter* than the input, the `recycle` argument controls what happens to that result.
#' There are seven options:
#' 
#' + `"no"`: The result is not recycled. For calls to `within()`, `mutate`, or `reframe()`, this option is not allowed.
#' + `"yes"`: the result is recycled, no matter how long it is.
#' + `"pad"`: the result is padded with `NA` values.
#' + `"ifscalar"`: if the result is scalar (length 1), it is recycled; otherwise you see an error.
#' + `"ifeven"`: if the result length evenly divides the input length, it is recycled; otherwise you see an error.
#' + `"never"`: The result is not recycled. If the result does not match the input length, you see an error.
#' + `"summarize"`: if the result is not scalar, *even if it matches the input length*, you see an error. The result is not recycled.
#' 
#' The result of padding/recycling also depends on the `alignLeft` argument:
#' If `alignLeft = TRUE`, results are padded to the right: like `c(result, NA, NA, ...)`;
#' If `alignLeft = FALSE`, results are padded on the left: like `c(..., NA, NA, results)`.
#' Recycling is also affected if the result's length does not evenly divide the input length.
#' For example, consider a result `c(1, 2, 3)` which needs to be recycled to length `10`:
#' If `alignLeft = TRUE`, the result is recycled `c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)`;
#' If `alignLeft = FALSE`, the result is recycled `c(3, 1, 2, 3, 1, 2, 3, 1, 2, 3)`.
#' 
#' 
#' #### with() and summarize()
#' 
#' The humdrumR `with()` and `summarize()` methods return "normal" R data objects.
#' The only difference between the `with()` and `summarize()` methods is their default  `drop` and `recycle` arguments:
#' 
#' + `with(..., drop = TRUE, recycle = 'no')`
#' + `summarize(..., drop = FALSE, recycle = 'summarize')`
#'
#' If `drop = TRUE`, they return whatever your code's result is, with no parsing.
#' This can be *any* kind of R data, 
#' including [vectors][vector] or objects like [lm fits][lm]
#' or [tables][base::table];
#' If `drop = FALSE`, the results will instead be returned in a [data.table].
#' This `data.table` will include the columns for each result, but also any [grouping][group_by()] columns as well.
#' 
#'
#' #### within(), mutate(), and reframe().
#' 
#' The humdrumR `within()`, `mutate()`, and `reframe()` methods always return a new [humdrumR data object][humdrumRclass],
#' with new [fields] created from your code results.
#' The only differences between these methods is their default `recycle` argument, and the types of `recycle` argument they allow:
#' 
#' + `within(..., recycle = 'pad')`
#'   + Can accept any `recycle` option except `"no"`.
#' + `mutate(..., recycle = 'ifscalar')`
#'   + Can only accept `"ifscalar"` or `"never"`.
#' + `reframe(..., recycle = 'pad')`
#'   + Can only accept `"pad"` or `"yes"`.
#' 
#' ### New field names
#' 
#' When running `within()`, `mutate()`, or `reframe()` new [fields()] are 
#' added to the output [humdrumR data][humdrumRclass].
#' These new fields become the [selected fields][selectedFields] in the output.
#' You can explicitly name these fields (recommended), or allow `humdrumR` to automatically name them.
#' When using `with(..., drop = FALSE)` or `summarize(..., drop = FALSE)`, the column names of the output [data.table]
#' are determined in the same way.
#' 
#' If you don't explicitly name the code expressions you provide, the new fields are named
#' by capturing the expression code itself as a `character` string.
#' However, it is generally a better idea to explicitly name your new fields.
#' This can be done in two ways:
#' 
#' + Base-R [within][base::within] style: Use the `<-` assignment operator inside your expression.
#'   + Example: `within(humData, Kern <- kern(Token))`.
#' + Tidyverse [mutate][dplyr::mutate] style: provide the expression as a named argument with `=`.
#'   + Example: `mutate(humData, Kern = kern(Token))`.
#'   
#' Either style can be used with any of the `humdrumR` methods.
#' Only top-level assignment will create a new field, which means only one field can be assigned per expression.
#' For example, 
#' 
#' ```
#' within(humData, 
#'        Semits <- semits(Token),
#'        Recip <- recip(Token))
#' ```
#' 
#' will create two fields (`Semits` and `Recip`).
#' However, 
#' 
#' ```
#' within(humData,
#'        { 
#'          Semits <- semits(Token)
#'          Recip <- recip(Token)
#'         })
#' ```
#' 
#' will not.
#' The [result of expressions][evaluatingExpressions] grouped by `{}` is always the *last* expression in the brackets.
#' Thus, the last example above will only create one new field, corresponding to the result of `recip(Token)`.
#' However, the resulting field won't be called `Recip`!
#' This is because only *top-level* assignments are used to name an expression:
#' To name a multi-expression expression (using `{}`), you could do something like this:
#' 
#' ```
#' within(humData,
#'        Recip <- { 
#'          Semits <- semits(Token)
#'          recip(Token)
#'         })
#' 
#' ```
#' 
#' Of course, only the result of `recip(Token)` would be saved to `Recip`, so the `Semits <- semits(Token)` expression is doing nothing here.
#' 
#' #### Self references
#' 
#' All expressions provided to the `with()`/`within()` methods are evaluated in order, from left to right,
#' so any assignments in a previous expression will be visible to the next expression.
#' This means we could, for example do this:
#' 
#' ```{r}
#' within(humData, 
#'        Kern <- kern(Token),
#'        Kern2 <- paste0(Kern, nchar(Kern)))
#' 
#' ```
#'  
#' the use of `Kern` in the second expression will refer to the `Kern` assigned in the previous expression.
#' 
#' 
#' 
#' 
#' @section Group by:
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
#' @param .by ***Optional grouping fields; an alternative to using [group_by()][groupHumdrum].***
#'
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or `character` strings which [partially match][partialMatching] one or more
#' [fields()] in the `data`.
#'
#' If not `NULL`, these fields are used to group the data.
#' If grouping fields have already been set by a call to [group_by()][groupHumdrum],
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
                          recycle = "no",
                          alignLeft = TRUE,
                          drop = TRUE,
                          .by = NULL,
                          variables = list()) {
  withFunc <- paste0(if (any(as.character(sys.call(1)[[1]]) %in% c('summarize', 'pull')))  as.character(sys.call(1)[[1]]) else 'with', '.humdrumR')
  
  recycle <- checkRecycle(recycle)
  
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, expandPaths = expandPaths, recycle = recycle, alignLeft = alignLeft,
                       .by = .by, variables = variables, withFunc = withFunc), 
           envir = environment())
  
  if (recycle == 'pad') result <- result[humtab[Type %in% dataTypes, list(`_rowKey_`)], on ='_rowKey_'] 
  
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
                            recycle = "pad",
                            .by = NULL,
                            variables = list()) {
  withFunc <- paste0(if (any(as.character(sys.call(1)[[1]]) %in% c('mutate', 'reframe', 'subset', 'filter')))  as.character(sys.call(1)[[1]]) else 'within', '.humdrumR')
  
  recycle <- checkRecycle(recycle, c("yes", "pad", "ifscalar", "ifeven", "never", "summarize"))
  
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, alignLeft = alignLeft,
                       expandPaths = expandPaths, recycle = recycle, 
                       .by = .by, variables = variables, 
                       withFunc = withFunc), 
           envir = environment())
  
  result <- result[ , !names(result) %in% groupFields, with = FALSE]
  
  # any fields getting overwritten
  overWrote <- setdiff(colnames(result)[colnames(result) %in% colnames(humtab)], '_rowKey_')
  
  bad <- overWrote %in% c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal',
                          'Exclusive',
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


withHumdrum <- function(humdrumR, ..., dataTypes = 'D', recycle = 'never', 
                        alignLeft = TRUE, expandPaths = FALSE, variables = list(), .by = NULL, withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  dataTypes <- checkTypes(dataTypes, withFunc) # use this to index humtab later
  
  if (expandPaths) humdrumR <- expandPaths(humdrumR, asSpines = FALSE)
  
  humtab <- data.table::copy(getHumtab(humdrumR))
  humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
  
  # quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  quosures <- rlang::enquos(...)
  quosures <- tidyNamer(quosures)
  if (length(quosures) == 0L) .stop("In a call to {withFunc}, you must provide an expression to evaluate!")
  
  
  ### Preparing the "do" quosure
  fields <- fields(humdrumR)
  dotField <- fields[Selected == 1L]$Name
  
  ##### grouping
  groupFields <- getGroupingFields(humdrumR, .by, withFunc) 
  
  doQuo  <- prepareDoQuo(humtab, quosures, dotField, recycle, variables)
  
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

prepareDoQuo <- function(humtab, quosures, dotField, recycle, variables) {
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
  
  # insert variables
  doQuo <- interpolateVariablesQuo(doQuo, variables)
  
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

#### adding/manipulating variables ----

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



interpolateVariablesQuo <- function(quo, variables) {
  
  withinExpression(quo, predicate = \() TRUE, applyTo = 'symbol',
                   \(exprA)
                   
                   if (exprA$Head %in% names(variables)) {
                     analyzeExpr(variables[[exprA$Head]])
                   } else {
                     exprA
                   })
                   
}

#### Lag/Led vectors ----

laggedQuo <- function(funcQuosure) {
  
  predicate <- \(Head, Args) Head == '[' && any(tolower(names(Args)) %in% c('lag', 'lead')) 
  
  do <- \(exprA) {
    
    args <- exprA$Args
    if (!'groupby' %in% .names(args)) args$groupby <- expr(list(Piece, Spine, Path))
    
    lagorlead <- names(args)[tolower(names(args)) %in% c('lag', 'lead')]
    
    names(args)[tolower(names(args)) == lagorlead] <- 'n'
    n <- rlang::eval_tidy(args$n)
    if (!is.numeric(n) || any((n %% 1) != 0)) .stop('Invalid [{lagorlead} = ] {lagorlead} expression.')
    args$n <- NULL
    
    lagExprs <- lapply(n, \(curn) rlang::expr((!!rlang::sym(lagorlead))(!!!args, n = !!curn)))

    exprA$Head <- 'splat'
    exprA$Args <- lagExprs
    
    exprA
  }
  
  withinExpression(funcQuosure, predicate, do, stopOnHit = TRUE)
  
}




#### Splatting in expressions ----
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

# ngramifyQuo <- function(funcQuosure, ngramQuosure, usedInExpr, depth = 1L) {
#           # This function takes an expression and a vector of strings representing
#           # names used in that expression and creates an expression
#           # which uses applyNgram on these named objects.
#           # It first uses xifyQuo to put the expression in the form of a 
#           # lambda function.
#           # 
#           # 
#   funcQuosure <- xifyQuo(funcQuosure, usedInExpr, depth)
#   
#   # rlang::quo_set_expr(funcQuosure,
#                       # rlang::expr(applyNgram(n = !!rlang::quo_get_expr(ngramQuosure), 
#                                              # vecs = list(!!!lapply(usedInExpr, rlang::sym)), 
#                                              # f = !!rlang::quo_get_expr(funcQuosure))))
#   rlang::quo(
#             applyNgram(n = !!ngramQuosure, 
#                        vecs = list(!!!lapply(usedInExpr, rlang::sym)),
#                        f = !!funcQuosure))
#   
# }

# windowfyQuo <- function(funcQuosure, windowQuosure, usedInExpr, depth = 1L) {
#   funcQuosure <- xifyQuo(funcQuosure, usedInExpr, depth)
#   
#   if (!'groupby' %in% .names(windowQuosure[[2]])) windowQuosure[[2]][['groupby']] <- quote(list(Piece, Spine))
#   if (!'x' %in% .names(windowQuosure[[2]]) && .names(windowQuosure[[2]])[2] != '' ) windowQuosure[[2]][['x']] <- rlang::sym(usedInExpr[1])
#   
#   applyArgs <- as.list(windowQuosure[[2]][c('leftEdge', 'rebuild', 'passOutside')])
#   windowQuosure[[2]] <- windowQuosure[[2]][!.names(windowQuosure[[2]]) %in% c('leftEdge', 'rebuild', 'passOutside')]
#   
#   
#   rlang::quo(
#     windowApply(func = !!funcQuosure,
#                 x = !!rlang::sym(usedInExpr[1]),
#                 windows = !!windowQuosure,
#                 !!!applyArgs))
#   
# }




    





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
  
  if (!recycle %in% c('pad', 'summarize', 'no') && any(!match & !objects)) {
    bad <- !match & switch(recycle,
                           ifscalar  = resultLengths != 1L,
                           ifeven    = keyLengths %% resultLengths != 0L,
                           never     = TRUE,
                           FALSE)
    if (any(bad)) .stop("The {withFunc} command won't recycle these results because the recycle argument is set to '{recycle}.'",
                        switch(recycle, 
                               ifscalar = "Only scalar (length 1) results will be recycled.",
                               ifeven = "Only results of a length that evenly divides the input will be recycled."),
                        "See the 'recycling' section of ?withHumdrum for explanation.")
    
    results[!match & !objects] <- Map(results[!match & !objects], keyLengths[!match & !objects], 
                                      f = \(result, keyLength) {
                                        i <- if (hasdim(result)) 1:nrow(result) else seq_along(result)
                                        
                                        i <- if (alignLeft) { 
                                          rep(i, length.out = keyLength) 
                                        } else { 
                                            rev(rep(rev(i), length.out = keyLength))
                                          }
                                        
                                        if (hasdim(result)) {
                                          result[i, , drop = FALSE]
                                        } else {
                                          result[i]
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


# Methods for tidyverse ----

## dplyr ----

tidyNamer <- function(quosures) {
  names <- .names(quosures)
  
  assigned <- sapply(quosures, 
                        \(quo) {
                          exprA <- analyzeExpr(quo)
                          if (exprA$Head == '<-')  rlang::as_label(exprA$Args[[1]]) else  '' 
                        })
  
  doubleAssign <- names != '' & assigned != ''
  if (any(doubleAssign)) {
    sameName <- names[doubleAssign] == assigned[doubleAssign]
    if (all(sameName)) {
      example <- names[doubleAssign][which(sameName)[1]]
      .warn("You are using '=' AND '<-' at the same time; you only need one or the other.",
            "For example, '{example} = {example} <-' could just be '{example} ='.")
    } else {
      bad <- paste0(names[doubleAssign][!sameName], ' = ', assigned[doubleAssign][!sameName], ' <- ...')
      .stop("You are using '=' AND '<-' to assign contradictory field names.",
            "For example, '{bad[1]}' is contradictory.")
    }
  }
  
  names <- ifelse(names == '', assigned, names)
  names[names == ''] <- sapply(quosures[names == ''], rlang::as_label)
  # if (any(duplicated(names))) .stop("You can't run summarize.humdrumR() and give {num2word(max(table(names)))} columns the same name!")
  
  names <- rlang::syms(names)
  
  Map(quosures, names, f = \(quo, name) rlang::quo(!!name <- !!quo))
  
}




#' HumdrumR using Tidyverse "verbs"
#' 
#' These methods for [dplyr] verbs are all shorthand calls for [with/within/subset.humdrumR()][withHumdrum].
#' 
#' @rdname withinHumdrum
#' @export
mutate.humdrumR <- function(.data, ..., dataTypes = 'D', recycle = 'ifscalar', alignLeft = TRUE, expandPaths = FALSE, .by = NULL) {
  recycle <- checkRecycle(recycle, options = c('ifscalar', 'never'))
  
  within.humdrumR(.data, ..., recycle = recycle, dataTypes = dataTypes, 
                  alignLeft = alignLeft, expandPaths = expandPaths, .by = .by)
  
}



#' @rdname withinHumdrum
#' @export
summarise.humdrumR <- function(.data, ..., dataTypes = 'D', expandPaths = FALSE, drop = FALSE, .by = NULL) {
  
  with.humdrumR(.data, ..., recycle = 'summarize', 
                dataTypes = dataTypes, drop = drop, expandPaths = expandPaths, .by = .by)
  
}




#' @rdname withinHumdrum
#' @export
reframe.humdrumR <- function(.data, ..., dataTypes = 'D', alignLeft = TRUE, expandPaths = FALSE, recycle = 'pad', .by = NULL) {
  recycle <- checkRecycle(recycle, options = c('yes', 'pad'))
  
  within.humdrumR(.data, ..., recycle = recycle, dataTypes = dataTypes, 
                  alignLeft = alignLeft, expandPaths = expandPaths, .by = .by)
  
}




## ggplot2  -----


#' @rdname withinHumdrum
#' @export
ggplot.humdrumR <- function(data = NULL, mapping = aes(), ..., dataTypes = 'D') {
  humtab <- getHumtab(data, dataTypes = dataTypes)
  
  ggplot(humtab, mapping = mapping, ...) + theme_humdrum()
}

#' @rdname withinHumdrum
#' @export
ggplot.humdrum.table <- function(data = NULL, mapping = aes(), ...) {
  
  ggplot(as.data.frame(data), mapping = mapping, ...) + theme_humdrum()
}


### Treatment of token ----

#' @export
scale_type.token <- function(x) if (class(x@.Data) %in% c('integer', 'numeric', 'integer64')) 'continuous' else 'discrete'


#' @export
scale_x_token <- function(..., expand = waiver(), guide = waiver(), position = "bottom") {
  sc <- ggplot2::discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                                # limits = c("c", "c#", "d-", "d", "d#", "e-", "e", "e#", "f", "f#", "f##", "g-", "g", "g#", "a-", "a", "a#", "b-", "b", "b#"),
                                expand = expand, guide = guide, position = position, super = ScaleDiscretePosition)
  
  sc$range_c <- scales::ContinuousRange$new()
  sc
}



### humdrumR plot style ----

#### Colors ----

scale_color_humdrum <- ggplot2::scale_fill_manual(values = flatly)
# scale_color_continuous(type = colorRamp(flatly[2:3]))

options(ggplot2.continuous.fill = ggplot2::scale_color_gradientn(colors = flatly_continuous(100)))
options(ggplot2.continuous.color = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))
options(ggplot2.continuous.colour = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))

# options(ggplot2.continuous.colour = 'humdrum')

#### Theme ----


theme_humdrum <- function() {
  ggplot2::update_geom_defaults("point", list(size = .5, color = flatly[1], fill = flatly[2]))
  ggplot2::update_geom_defaults("line", list(size = .5, color = flatly[4], fill = flatly[3]))
  ggplot2::update_geom_defaults("rect", list(fill = flatly[1]))
  
  theme(panel.background = element_blank(), axis.ticks = element_blank(),
        strip.background = element_blank(), 
        # panel.border = element_rect(linetype = 'dashed', fill = NA),
        legend.key = element_rect(fill = NA),
        title = element_text(family = 'Lato', color = flatly[5], size = 16),
        plot.title.position = 'plot', plot.title = element_text(hjust = .5),
        line = element_line(color = flatly[1]),
        rect = element_rect(color = flatly[2]),
        text = element_text(family = 'Lato', color = flatly[4]),
        axis.text = element_text(color = flatly[5], size = 7),
        axis.title = element_text(color = flatly[4], size = 11)
        )
}


