
#################################################-
# within.humdrumR ----
##################################################-


#' Working *with* humdrum data fields
#' 
#' These functions are the primary means of working with
#' humdrumR data. 
#' They allow us to perform arbitrary (free form) manipulation of data [fields][fields()]
#' held within a [humdrumR data object][humdrumRclass], with convenient functionality
#' for ignoring null data, [lagging][lag()] data, [grouping][groupHumdrum] data, 
#' [windowing][context()], and more.
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
#' The main differences between them are what they do with the *results* of your code:
#' `with()` and `summarize()` return results in normal, "raw" R formats, **removed** 
#' from the [humdrumR data][humdrumRclass];
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
#' 
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
#' but instead only evaluate their expressions using rows containing non-null data tokens (`Type == "D"`).
#' This means that interpretations, comments, barlines, and null data tokens are automatically ignored for you!
#' This feature is controlled by the `dataTypes` argument:
#' you can choose to work with the other token types by providing a `character` string containing combinations
#' of the characters `G` (global comments), `L` (local comments), `I` (interpretations), 
#' `M` (barlines), `D` (non-null data), or `d` (null data).
#' For example, `dataTypes = 'MDd'` will evaluate your expressions on barline tokens (`=`), non-null data,
#' and null data.
#' See the [ditto()] manual for an example application of using `dataTypes = 'Dd'`.
#' Keep in mind that `humdrumR` dynamically updates what tokens are considered "null" (`"d"`) based on what fields
#' are [selected][selectedFields].
#' 
#' If multiple expression arguments are provided, each expression is evaluated in order, from left to right.
#' Each expression can refer variables assigned in the previous expression (examples below).
#' 
#' ### Expression pre-processing
#' 
#' These functions all do some
#' pre-processing of expressions arguments before evaluating them.
#' This pre-processing provides some convenient "[syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar)" 
#' for working with humdrum data.
#' There are currently five pre-processing steps:
#' 
#' 1. Explicit variable interpolation.
#' 2. The `.` placeholder for selected fields.
#' 3. Automatic argument insertion.
#' 4. "Lagged"-vectors shorthand.
#' 5. "Splatted" arguments.
#' 
#' Each of these is explained below.
#' 
#' #### Explicit variable interpolation
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
#' For example, many [pitch functions][pitchFunctions] have a `Key` argument which (can) take the 
#' content of the `Key` which [readHumdrum()] creates when there are key interpretations,
#' like `*G:`, in the data.
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
#' For example, the [?solfa] documentation mentions the treatment of `Key` in its "Key" section.
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
#' causing it to be lagged/led by that integer amount.
#' (A vector indexed with `lag = 0` returns the unchanged vector.)
#' For example, the following two calls are the same:
#' 
#' ```
#' humData |> with(Token[lag = 1])
#' humData |> with(lag(Token, 1))
#' ```
#' 
#' This is most useful if the `lag`/`lead` index has *multiple* values:
#' if the indexed object appears within a higher function call,
#' each lag is inserted as a *separate* argument to that call.
#' Thus, *these* two calls are also the same:
#' 
#' ```
#' humData |> with(tally(Token[lag = 1:2]))

#' humData |> with(tally(lag(Token, 1), lag(Token, 2)))
#' ```
#' 
#' Note that the lagging will also be automatically grouped within the fields `list(Piece, Spine, Path)`,
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
#' humData |> with(paste(Token[lag = 0:5], sep = '-'))
#' ```
#' 
#' Since the lagging is grouped by `list(Piece, Spine, Path)`, 
#' these are true "melodic" n-grams, only created within spine-paths within each piece.
#'  
#' 
#' #### Splatted arguments
#' 
#' "Splatting" refers to feeding a function a list/vector of arguments.
#' Sometimes we want to divide our data into pieces (a l\'a [group_by()][groupHumdrum]), but
#' rather than applying the same expression to each piece, we want to feed
#' the separate pieces as separate arguments to the same function.
#' You can use some 
#' [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugarsyntactic)
#' to do just this.
#' We can index any field in our call with a `splat` argument, which must be a `Field %in% x`.
#' For example,
#'
#' ```
#' humData |> with(list(Token[splat = Spine %in% 1:2]))
#' ```
#' 
#' In this call, the `Token` field will be divided into two groups, one where `Spine == 1` and the other where
#' `Spine == 2`; the first group (`Spine == 1`) will be used as the first argument to `list`, and the second group
#' (`Spine == 2`) as the second argument.
#' Thus, `within` translates the previous expression to this:
#' 
#' ```
#' humData |> within(list(Token[Spine == 1], Token[Spine == 2]))
#' ```
#' 
#' Splatting can be little weird, because there is nothing to assure that the splatted arguments 
#' are all the same length, which we usually want ([vectorization]).
#' For example, in the previous example, there is no guarantee that `Token[Spine == 1]` and `Token[Spine == 2]` are the same length.
#' This just means we should only use splatting if we really understand the groups we are splatting.
#' For example, *if* there are [no spine paths or stops in our data][anyPaths()], *then* we can know that all spines
#' have the same number of data records, but only including **all** data records (null *and* non-null).      
#' So, if I know there are no stops/paths in our data, we can run something like this:
#' 
#' ```
#' humData |> within(dataTypes = 'Dd', 
#'                   tally(Token[splat = Spine %in% 1:2]))
#' ```
#'       
#' ### Saving expressions for later
#' 
#' In some cases you may find that there are certain arguments expressions that you use repeatedly.
#' You can store expressions as variables by "quoting" them: the most common way to 
#' quote an expression in R is using the [~][base::tilde], which creates what is called a
#'  "formula"---essentially a quoted expression.
#' You can also quote expressions, using [quote()].
#' Once you've quoted an expression you can pass it to 
#' `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()`.
#'
#' Image that you have three different datasets (`humData1`, `humData2`, and `humData3`),
#' and you'd like to evaluate the expression `tally(kern(Token, simple = TRUE))` in all three.
#' Use the `~` operator to quote and save that expression to variable, then use it with `with()`:
#' 
#' ```
#' tallyKern <- ~tally(kern(Token, simple = TRUE))
#' 
#' humData1 |> with(tallyKern)
#' humData2 |> with(tallyKern)
#' humData3 |> with(tallyKern)
#' 
#' ```
#' 
#' 
#' ### Expanding paths
#' 
#' For data that includes spine paths (which you can check with [anyPaths()]),
#' some analyses may require that spine paths are treated as contiguous "melodies."
#' The [expandPaths()] function can be used to "expand" spine paths into new spines.
#' The `expandPaths` *argument* to `with()`/`within()` will cause [expandPaths()]
#' to be run on your data before evaluating your argument expressions.
#' After evaluation, the expanded parts of the data are then removed from the output.
#'
#' @section Parsing expression results:
#' 
#' The only differences between the `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()` humdrumR methods
#' are what they do with the *results* of expressions passed to them.
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
#' + `"no"`: The result is not recycled or padded. For calls to `within()`, `mutate`, or `reframe()`, this option is not allowed.
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
#' If `drop = TRUE`, these methods return whatever your code's result is, with no parsing.
#' This can be *any* kind of R data, 
#' including [vectors][vector] or objects like [lm fits][lm]
#' or [tables][base::table].
#' If `drop = FALSE`, the results will instead be returned in a [data.table()].
#' 
#' If you are working with [grouped data][groupHumdrum],
#' the `drop = FALSE` output (`data.table`) will include all grouping columns as well
#' as the results of your expressions.
#' If `drop = TRUE` *and* there is only one result per group, the grouping fields will be
#' used to generate names for the output vector.
#'
#' #### within(), mutate(), and reframe().
#' 
#' The humdrumR `within()`, `mutate()`, and `reframe()` methods always return a new [humdrumR data object][humdrumRclass],
#' with new [fields] created from your code results.
#' The only differences between these methods is their default `recycle` argument and the types of `recycle` argument they allow:
#' 
#' + `within(..., recycle = 'pad')`
#'   + Can accept any `recycle` option except `"no"`.
#' + `mutate(..., recycle = 'ifscalar')`
#'   + Can only accept `"ifscalar"` or `"never"`.
#' + `reframe(..., recycle = 'pad')`
#'   + Can only accept `"pad"` or `"yes"`.
#' 
#' ### Creating new humdrumR fields
#' 
#' When running `within()`, `mutate()`, or `reframe()`, new [fields()] are 
#' added to the output [humdrumR data][humdrumRclass].
#' These new fields become the [selected fields][selectedFields] in the output.
#' You can explicitly name newly created fields (recommended), or allow `humdrumR` to automatically name them (details below).
#' When using `with(..., drop = FALSE)` or `summarize(..., drop = FALSE)`, the column names of the output [data.table]
#' are determined in the same way.
#' 
#' Note that `within()`, `mutate()`, and `reframe()` will (attempt to) put *any* result back into your 
#' [humdrumR data][humdrumRclass]...even if it doesn't make much sense.
#' Things will work well with [vectors][base::vector].
#' Atomic vectors are usually the best to work with (i.e., numbers, `character` strings, or `logical` values),
#' but `list`s will work well too---just remember that you'll need to treat those fields as lists 
#' (e.g., you might need to use [lapply()] or [Map()] to work with `list` fields.)
#' Any *non-vector* result will be put into a list as well, padded as needed.
#' For example, if you use [lm()] to compute a linear-regression in a call to `within()` 
#' the result will be a new field containing a `list`, with first element in the 
#' list being a single `lm` fit object, and the rest of the list empty (padded to the length of the field).
#' 
#' 
#' #### Naming new fields
#' 
#' If you don't explicitly name the code expressions you provide, the new fields are named
#' by capturing the expression code itself as a `character` string.
#' However, it is generally a better idea to explicitly name your new fields.
#' This can be done in two ways:
#' 
#' + Base-R [within()][base::within] style: Use the `<-` assignment operator inside your expression.
#'   + Example: `within(humData, Kern <- kern(Token))`.
#' + Tidyverse [mutate()][dplyr::mutate] style: provide the expression as a named argument with `=`.
#'   + Example: `mutate(humData, Kern = kern(Token))`.
#'   
#' Either style can be used with any of the `humdrumR` methods.
#' When using `<-`, only top-level assignment will create a new field, which means only one field can be assigned per expression.
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
#' Of course, only the result of `recip(Token)` would be saved to `Recip`, 
#' so the `Semits <- semits(Token)` expression is doing nothing useful here.
#' 
#' ### Piped references
#' 
#' All argument expressions passed to the `with()`/`within()` methods are evaluated in order, from left to right,
#' so any assignments in a previous expression will be visible to the next expression.
#' This means we can, for example, do this:
#' 
#' ```
#' within(humData, 
#'        Kern <- kern(Token),
#'        Kern2 <- paste0(Kern, nchar(Kern)))
#' 
#' ```
#'  
#' the use of `Kern` in the second expression will refer to the `Kern` assigned in the previous expression.
#' 
#' 
#' @section Evaluating expressions in groups or windows:
#' 
#' The `with()`, `within()`, `mutate()`, `summarize()`, and `reframe()` functions all
#' work with [grouped][groupHumdrum] data, or data with [contextual windows][context()] defined.
#' When groups or windows are defined, all argument expressions are evaluated independently
#' within each and every group/window.
#' Results are then processed (including recycling/padding) within each group/window.
#' Finally, the results are then pieced back together in locations corresponding to the
#' original data locations.
#' Since [groups][groupHumdrum] are necessarily exhaustive and non-overlapping, the results
#' location are easy to understand.
#' On the other hand [contextual windows][context()] may overlap, which means and non-scalar results
#' could potentially overlap as well;
#' in these cases, which result data lands where may be hard to predict.
#'
#' 
#' 
#' @param data ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Any number of expressions to evaluate.*** 
#'
#' These expressions can reference [fields()] in the data by name,
#' as well as variables outside the data.
#' 
#' If the expressions are named, the names are used to name the new fields
#' (or column names for `with(..., drop = FALSE)`.
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
#' @param recycle ***How should results be "recycled" (or padded) to relative to the input length?***
#' 
#' `within()` and `reframe()` default to `"pad"`; `mutate()` defaults to `"ifscalar"`; `with()` defaults to `"no"`.
#' 
#' Must be a single `character` string.
#' The full list of options are `"no"`, `"yes"`, `"pad"`, `"ifscalar"`, `"ifeven"`, `"never"`, and 
#' `"summarize"`, though not all functions accept all options.
#' See the *Parsing expression results* section below.

#' @param variables ***A named `list` of values, to interpolate into your expressions.***
#' 
#'  
#' Defaults to `list()`.
#' 
#' Must be a named `list`. 
#' These values are interpolated into the `...` expression arguments wherever 
#' a variable name matches a name from the list.
#' 
#' @param alignLeft ***Should output that is shorter than input be aligned to the left?***
#'
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param expandPaths ***Should spine paths be expanded before evaluating expressions?***
#'
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' If `TRUE`, the [expandPaths()] function is run on the data
#' before evaluating the expressions.
#' After evaluation, the expanded locations are removed from the output.
#' 
#' @param drop ***Whether to return a simplified data structure.***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' This argument is conceptually similar to the `drop` argument in R matrices.
#' If `drop = TRUE`, the output of `with()`/`summarize()` is simplified as much as possible (trying to return
#' the "raw" vector, list, table, etc. within it). If `drop = FALSE`, the result is *always*
#' a [data.table]. 
#' 
#' @examples
#' 
#' 
#' # with/within style:
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData |> with(tally(kern(Token, simple = TRUE), Spine))
#' 
#' humData |> within(Kern <- kern(Token), 
#'                   Recip <- recip(Token),
#'                   Semits <- semits(Token)) -> humData
#'                   
#' humData |> 
#'     group_by(Spine) |>
#'     with(mean(Semits))
#'     
#' humData |> 
#'     group_by(Piece, Spine) |>
#'     with(mean(Semits), drop = FALSE)
#'     
#' # tidyverse (dplyr) style:
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData |> mutate(Kern = kern(Token), 
#'                   Recip = recip(Token),
#'                   Semits = semits(Token)) -> humData
#'                   
#' humData |> 
#'     group_by(Spine, Bar) |>
#'     summarize(mean(Semits))
#'       
#' # dataTypes argument
#' 
#' humData |>
#'    group_by(Piece, Spine) |>
#'    within(paste(Token, seq_along(Token)))
#'    
#' humData |>
#'    group_by(Piece, Spine) |>
#'    mutate(Enumerated = paste(Token, seq_along(Token)),
#'           dataTypes = 'Dd')
#'           
#' # recycle argument
#' 
#' humData |>
#'    group_by(Piece, Bar, Spine) |>
#'    mutate(BarMean = mean(Semits), recycle = 'ifscalar')
#'    
#' humData |>
#'    group_by(Piece, Bar, Spine) |>
#'    within(BarMean = mean(Semits), recycle = 'pad')  
#' 
#' 
#' 
#' 
#' @seealso {These functions are most useful in combination with the 
#'  [subset()][subset.humdrumR], [group_by()][groupHumdrum], and [context()] commands.}
#' @aliases within with
#' @name withinHumdrum
NULL



#' @rdname withinHumdrum
#' @export
with.humdrumR <- function(data, ..., 
                          dataTypes = 'D',
                          recycle = "no",
                          alignLeft = TRUE,
                          expandPaths = FALSE,
                          drop = TRUE,
                          .by = NULL,
                          variables = list()) {
  withFunc <- paste0(if (any(as.character(sys.call(1)[[1]]) %in% c('summarize', 'pull')))  as.character(sys.call(1)[[1]]) else 'with', '.humdrumR')
  
  recycle <- checkRecycle(recycle)
  
  list2env(withHumdrum(data, ..., dataTypes = dataTypes, expandPaths = expandPaths, recycle = recycle, alignLeft = alignLeft,
                       .by = .by, variables = variables, withFunc = withFunc), 
           envir = environment())
  
  if (recycle == 'pad') result <- result[humtab[Type %in% dataTypes, list(`_rowKey_`)], on ='_rowKey_'] 
  
  result[ , `_rowKey_` := NULL]
  
  ### Do we want extract the results from the data.table? 
  if (drop) {
    
    groups <- result[ , groupFields, with = FALSE]
    groupNames <- if (length(groups)) do.call('paste', c(Map(groupFields, groups, f = paste0), list(sep = ',')))
    
    result <- result[[max(which(!colnames(result) %in% groupFields))]]
    if (length(result) == 0L) return(result)
    
    if (is.list(result) && length(result) == 1L) {
      result <- result[[1]]
    } else {
      if (!any(duplicated(groupNames))) names(result) <- groupNames
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
  
  putHumtab(data) <- humtab
  
  data <- updateFields(data)

  addExclusiveFields(humtab, newFields) # in place
  
  update_humdrumR.data.table(humtab, field = newFields) # in place
  
  
  data

}


withHumdrum <- function(humdrumR, ..., dataTypes = 'D', recycle = 'never', 
                        alignLeft = TRUE, expandPaths = FALSE, variables = list(), .by = NULL, withFunc) {
  # this function does most of the behind-the-scences work for both 
  # with.humdrumR and within.humdrumR.
  dataTypes <- checkTypes(dataTypes, withFunc) # use this to index humtab later
  
  if (expandPaths) humdrumR <- expandPaths(humdrumR, asSpines = FALSE)
  
  humtab <- data.table::copy(getHumtab(humdrumR))
  
  # quoTab <- parseArgs(..., variables = variables, withFunc = withFunc)
  quosures <- rlang::enquos(...)
  if (length(quosures) == 0L) .stop("In a call to {withFunc}, you must provide an expression to evaluate!")
  
  ### Preparing the "do" quosure
  fields <- fields(humdrumR)
  dotField <- fields[Selected == 1L]$Name
  
  ##### grouping
  groupFields <- getGroupingFields(humdrumR, .by, withFunc) 
  
  quosures <- prepareQuosures(humtab, quosures, dotField, recycle, variables, withFunc, alignLeft)
  
  
  # Check that structural fields aren't getting overwritten
  checkOverwrites(quosures, humtab, withFunc)
 
  ## Evaluate quosures
  # new fields are created in place
  visible <- evaluateDoQuo(quosures, humtab, dataTypes, groupFields, humdrumR@Context)
  
  humtab <- checkRecycling(humtab, recycle, names(quosures), withFunc)
  
  if (expandPaths) {
    humtab <- humtab[!is.na(ParentPath)]
  }
  
  list(humtab = humtab, visible = visible, groupFields = groupFields, newFields = names(quosures))
}



 

          

## Preparing doQuo ----

prepareQuosures <- function(humtab, quosures, dotField, recycle, variables, withFunc, alignLeft) {
  # This is the main function used by [.withinmHumdrum] to prepare the current
  # do expression argument for application to a [humdrumR][humdrumRclass] object.
  
  # if the targets are lists, Map
  # quosures <- Map(quosures, usedInExprs, 
  #                 f = \(quo, curUsed) { 
  #                   if (any(usedClasses[curUsed] == 'list')) {
  #                     mapifyQuo(quo, curUsed, depth = 1L) 
  #                   } else {
  #                     quo
  #                   }
  #                 })
  
  quosures <- unformula(quosures)
  
  quosures <- quoFieldNames(quosures)
  
  
  # insert variables
  quosures <- lapply(quosures, interpolateVariablesQuo, variables = variables)
  
  # turn . to selected field
  quosures <- lapply(quosures, activateQuo, dotField = dotField)
  
  # lagged vectors
  quosures <- lapply(quosures, laggedQuo, fields = colnames(humtab))
  
  # add in arguments that are already fields
  quosures <- lapply(quosures, autoArgsQuo, fields = colnames(humtab))

  # splats
  quosures <- lapply(quosures, splatQuo, fields = colnames(humtab))
  
  quosures <- lapply(quosures, quosureParseResult, recycle = recycle, withFunc = withFunc, alignLeft = alignLeft)

  quosures
}

unformula <- function(quosures) {
  lapply(quosures,
         \(quo) { 
           if (rlang::quo_is_symbol(quo)) {
             quochar <- as.character(rlang::quo_squash(quo))
             
             if (exists(quochar, mode = 'language', envir = rlang::quo_get_env(quo))) {
               quo <- rlang::as_quosure(rlang::quo_get_env(quo)[[quochar]], 
                                        env = rlang::quo_get_env(quo))
             }
           } else {
             if (as.character(rlang::quo_get_expr(quo)[[1]]) %in% c('~', 'quote', 'expression')){ 
               quo <- rlang::as_quosure(eval(rlang::quo_get_expr(quo), 
                                             rlang::quo_get_env(quo)),
                                        env = rlang::quo_get_env(quo))
             } 
             
           }
           quo
         })
}

quoFieldNames <- function(quosures) {
  
  quoNames <- .names(quosures)
  
  for (i in seq_along(quosures)) {
    exprA <- analyzeExpr(quosures[[i]])
    if (exprA$Head == '<-') {
      assigned <- rlang::as_label(exprA$Args[[1]])
      
      if (quoNames[i] != '') { # this means we have assignment AND naming
        if (quoNames[[i]] == assigned) {
          .warn("You are using '=' AND '<-' at the same time; you only need one or the other.",
                "For example, '{quoNames[[i]]} = {assigned} <-' could just be '{quoteNames[[i]]} ='.")
        } else {
          bad <- paste0(quoNames[[i]], ' = ', assigned, ' <- ...')
          .stop("You are using '=' AND '<-' to assign contradictory field names.",
                "For example, '{bad}' is contradictory.")
        }
      }
      
      quosures[[i]] <- rlang::new_quosure(exprA$Args[[2]], exprA$Environment)
      quoNames[i] <- rlang::as_label(exprA$Args[[1]])
      
    } else {
      if (quoNames[i] == '') quoNames[i] <- rlang::as_label(quosures[[i]])
    }
  }
  setNames(quosures, quoNames)
  
}

checkOverwrites <- function(quosures, humtab, withFunc) {
  overWrote <- intersect(names(quosures), colnames(humtab))
  
  bad <- overWrote %in% c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal',
                          'Exclusive',
                          'Piece', 'Spine', 'Path', 'Stop', 'Record', 'DataRecord', 'Global', 'Type')
  bad <- bad | grepl('^Exclusive\\.', overWrote) | overWrote == '_rowKey_'
  
  if (any(bad)) {
    if ('Token' %in% overWrote[bad]) {
      .stop("In your call to {withFunc}, you can't overwrite the 'Token' field.",
            "This field should always keep the original humdrum data you imported.")
    }
    .stop("In your call to {withFunc}, you can't overwrite structural fields.",
          ifelse = sum(bad) > 1L, 
          "You are attempting to overwrite the {harvard(overWrote[bad], 'and', quote = TRUE)} <fields|field>.",
          "For a complete list of structural fields, use the command fields(mydata, 'S').")
  }
  
  overWrote
}

checkRecycling <- function(humtab, recycle, fields, withFunc) {
  if (recycle == 'no') {
    
    recycled <- Reduce('&', humtab[ , grepl('\\.recycled_$', colnames(humtab)), with = FALSE])
    humtab <- humtab[!recycled]
    
  }  else {
    
    for (field in fields) {
      recycled <- humtab[[paste0('_', field, '.recycled_')]]
      inOutRatio <- humtab[[paste0('_', field, '.inOutRatio_')]]
      scalar <- humtab[[paste0('_', field, '.isScalar_')]]
      
      switch(recycle,
             pad =  humtab[recycled == TRUE, (field) := NA],
             ifscalar = if (!all(scalar | inOutRatio == 1, na.rm = TRUE)) {
               .stop("The {withFunc} command won't recycle these results",
                     "because the recycle argument is set to 'ifscalar'.",
                     "Your result is not scalar (i.e., length(result) != 1) and does not match the input length.")
             },
             summarize = if (!all(scalar, na.rm = TRUE)) {
               .stop("When using summarize() on humdrumR data, the result of each expression must be a scalar (length 1).")
             },
             ifeven = if (!all(inOutRatio %% 1 == 0, na.rm = TRUE)) {
               .stop("The {withFunc} command won't recycle these results",
                     "because the recycle argument is set to 'ifeven'.",
                     "The length of your result does not evenly divide the input length.")
             },
             never = if (!all(inOutRatio == 1, na.rm = TRUE)) {
               .stop("The {withFunc} command won't recycle these results",
                     "because the recycle argument is set to 'never',",
                     "but length of your result does not match the input length.")
             })
      
    }
    
  }
  
  humtab[ , grep('\\.(recycled|isScalar|inOutRatio)_$', colnames(humtab), value = TRUE) := NULL]
  
  
  humtab
}


quosureParseResult <- function(quosure, recycle, withFunc, alignLeft) {
  env <- rlang::quo_get_env(quosure)
  
  quosure <- rlang::quo({
    result <- withVisible(!!quosure)
    visible <- result$visible
    result <- wrapObjects(result$value)
    
    inlen <- length(Token)
    outlen <- length(result)
    
    result <- rep_len(result, inlen)
    attr(result, 'visible') <- visible
    
    list(result, orig = seq_len(inlen) >= outlen, inlen / outlen, outlen == 1)
    
    })
  
  rlang::quo_set_env(quosure, env)
  
}

concatDoQuos <- function(quosures) {
  assigned <- names(quosures)
   rlang::quo({
    {!!!quosures}
    
    results <- setNames(lapply(list(!!!rlang::syms(assigned)), \(x) list(list(x))), !!assigned) 
    # Need three lists here, and two for _rowKey_ 
    
    results[['_rowKey_']] <- list(list(`_rowKey_`))
    
    results
    
    })
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


autoArgsQuo <- function(funcQuosure, fields) {
  predicate <- \(Head) Head %in% c(autoArgTable$Function, paste0(autoArgTable$Function, '.default'))
  do <- \(exprA) {
    tab <- autoArgTable[(Function == exprA$Head | paste0(Function, '.default') == exprA$Head) & 
                          !Argument %in% names(exprA$Args) &
                          sapply(Expression, \(expr) length(namesInExpr(fields, expr)) > 0L)]
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

laggedQuo <- function(funcQuosure, fields) {
  
  predicate <- \(Head, Args) Head == '[' && any(tolower(names(Args)) %in% c('lag', 'lead')) 
  
  do <- \(exprA) {
    
    args <- exprA$Args
    if (!'groupby' %in% .names(args)) args$groupby <- expr(list(Piece, Spine, Path))
    
    lagorlead <- names(args)[tolower(names(args)) %in% c('lag', 'lead')]
    n <- args[[lagorlead]]
    args[[lagorlead]] <- NULL
    
    # do/can we evaluate n now?
    n <- try(eval(n, exprA$Environment))
    if (class(n) == 'try-error') .stop("In your use of '{lagorlead} = ', the expression '{rlang::as_label(n)}' can't be evaluated.",
                                          "It might refer to undefined variables?")
    
    
    lagExprs <- lapply(n, \(curn) rlang::expr((!!rlang::sym(lagorlead))(!!!args, n = !!curn)))
    # this SHOULD be expr(), not quo()

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

splatQuo <- function(funcQuosure, fields) {
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
    if (length(namesInExpr(fields, inExprA$Args[[1]])) == 0L) .stop('splat expression must reference an existing field')
    
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


evaluateDoQuo <- function(quosures, humtab, dataTypes, groupFields, windowFrame) {
  
  if (nrow(windowFrame)) {
    humtab <- windows2groups(humtab, windowFrame)
  }
  
  for (assign in names(quosures)) {
    assigned <- c(assign, paste0('_', assign, c('.recycled_', '.inOutRatio_', '.isScalar_')))
    humtab[Type %in% dataTypes, (assigned) := rlang::eval_tidy(quosures[[assign]], data = .SD), by = groupFields]
  }
 
  if (nrow(windowFrame)) humtab[, contextWindow := NULL]
  
  # assign is still last assign from loop
  visible <- attr(humtab[[assign]], 'visible')
  visible
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
  
  visibleResult <- is.visible(results[[length(results)]][[1]][[1]]) # determined by last result
  results[] <- lapply(results, unlist, recursive = FALSE)
  
  
  
  results <- as.matrix(results) # allows me to lapply across all all at once
  # results <- do.call('cbind', results)
  
  # "objects" are treated like length 1, and not vectorized
  objects <- array(FALSE, dim(results), dimnames(results))
  objects[] <- sapply(results, 
                      \(result) {
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

wrapObjects <- function(result) {
  if (length(result) &&  
      (is.table(result) || 
       (is.object(result) && !is.atomic(result)))) {
    list(result)
  } else {
    result
  }
}


recycle_summarize <- function(result) {
  if (length(result) != 1L) .stop("When using summarize() on humdrumR data, the result of each expression must be a scalar (length 1).")
  result
}


recycle_pad <- function(result, length, alignLeft) {
  if (alignLeft) result[seq_len(length)] else result[c(setdiff(seq_len(length), seq_along(result)), seq_along(result))]
}

recycle_ifscalar <- function(result, length, withFunc, alignLeft) {
  if (!(length(result) == 1L || length(result) == length)) .stop("The {withFunc} command won't recycle these results",
                                                                 "because the recycle argument is set to 'ifscalar'.",
                                                                 "Your result is not scalar (length == 1) and does not match the input length.")
  if (alignLeft) rep(result, length.out = length) else result[rev(rep(rev(seq_along(result)), length.out = length))]
}


recycle_ifeven <- function(result, length, withFunc, alignLeft) {
  if ((length(result) %% length) != 0) .stop("The {withFunc} command won't recycle these results",
                                              "because the recycle argument is set to 'ifeven'.",
                                              "The length of your result does not evenly divide the input length.")
  
  if (alignLeft) rep(result, length.out = length) else result[rev(rep(rev(seq_along(result)), length.out = length))]
}

recycle_yes <- function(result, length, alignLeft) {
  if (alignLeft) rep(result, length.out = length) else result[rev(rep(rev(seq_along(result)), length.out = length))]
}

recycle_never <- function(result, length, withFunc) {
  if (length(result) != length) .stop("The {withFunc} command won't recycle these results",
                                      "because the recycle argument is set to 'never',",
                                      "but length of your result does not match the input length.")
  result
}


recycleResults <- function(results, objects, rowKey, recycle, alignLeft, withFunc) {
  
  # length of each result group
  resultLengths <- array(as.integer(objects), dim(objects), dimnames(objects))
  resultLengths[!objects] <- unlist(lapply(results[!objects], \(result) if (hasdim(result)) nrow(result) else length(result)))
  
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



addExclusiveFields <- function(humtab, fields) {
  lapply(fields,
         \(fieldName) {
           field <- humtab[[fieldName]]
           exclusive <- getExclusive(field)
           if (!is.null(exclusive)) {
             exclusive <- ifelse(is.na(field), NA_character_, exclusive)
             
             newFieldName <- paste0('Exclusive.', fieldName)
             humtab[ , (newFieldName) := exclusive]
           }
         })
}


# Methods for tidyverse ----

## dplyr ----





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


  


 

          
