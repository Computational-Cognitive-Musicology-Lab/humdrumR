#################################-
# Grouping data  ############----
#################################-


### grouping ----

#' Divide humdrumR data into groups
#'
#' The [group_by()] method for [humdrumR objects][humdrumRclass]
#' is used to define [grouping factors][groupingFactors] in your data fields.
#' Note that groups created by grouping factors 1) are not necessarily contiguous and 
#' 2) always exhaustively partition the data.
#' The [context()] function can be used, as an alternative, to generate groups ("windows")
#' which are always contiguous and/or exclude some data.
#' 
#' 
#' @details
#' 
#' The `group_by()` method for [humdrumR objects][humdrumRclass] takes 
#' any number of expressions as `...` arguments.
#' These expressions may simply be `character` strings or symbols
#' indicating existing [fields()] in the data---
#' For example, `group_by(Piece, Spine)`.
#' However, the [expressions][expressionEvaluation] can also be arbitrary "expression arguments"
#' which are passed to [within()][withHumdrum] to generate new fields for grouping.
#' For example, you could group spines into even and odd groups with `group_by(Spine %% 2)`.
#' 
#' The `group_by()` function returns a new [humdrumR data object][humdrumRclass]
#' with grouping fields activated.
#' The grouping fields, and the number of groups, are show when the humdrumR data
#' is printed.
#' The `groups()` can be used to gather more information about groups: 
#' `group()` returns a `data.table` with one row representing each group,
#' the value of each grouping field indicated, and
#' with one or more columns indicating the number of tokens of each type in the group 
#' (the desired types are indicated by the `dataTypes` argument).
#' 
#' By default, each call to `group_by.humdrumR()` *adds* groups to
#' any groups already existing in the data.
#' If `.add = FALSE`, any preexisting groups are removed before creating new groups.
#' Groups can be explicitly removed using `ungroup()`.
#' 
#' When `.add = TRUE`, each call to `group_by()` computes new fields *using* the preexisting groups,
#' just like any normal call to [within()][withHumdrum].
#' This means that you can, in some cases, create different groupings depending on the order
#' you create groups.
#' For example, imagine we want to divide each piece in our data into two groups: 
#' all pitches higher than average in one group and all pitches lower than average in the other.
#' Consider a `humData` corpus with a numeric `Semits` field, and we run
#' these two different calls:
#' 
#' ```
#' humData |> 
#'    group_by(Piece) |>
#'    group_by(Semits > mean(Semits))
#' 
#' humData |>
#'    group_by(Semits > mean(Semits)) |>
#'    group_by(Piece)
#' 
#' ```
#' 
#' In the first call, we first group by `Piece`, then divide each piece by the *piece's*
#' average.
#' In the second example, we divide the corpus into two halves based on the *overall*
#' (cross-piece) average, *then* we divide it into pieces.
#' 
#' @param .data,x,humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Any number of expressions to evaluate.*** 
#'
#' These expressions can reference [fields()] in the data by name,
#' as well as variables outside the data.
#' 
#' If the expressions are named, the names are used to name the new fields.
#' 
#' @param dataTypes ***Which types of humdrum records to include.***
#' 
#' Defaults to `"D"`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation **Fields** section for explanation.)
#'
#' @param .add ***Should groups be added to existing groups?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a single `logical` value: an on/off switch.
#'
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData |> 
#'    group_by(Piece, Spine) |>
#'    groups()
#' 
#' humData |> 
#'    group_by(Piece, Spine %% 2) |>
#'    groups()
#'    
#' humData |> 
#'    group_by(Piece, Bar) |>
#'    mutate(NotesPerBar = length(Token)) |>
#'    ungroup()
#'    
#' humData |> 
#'    semits() |>
#'    group_by(Piece, Spine) |>
#'    with(mean(Semits))
#' 
#' @family {Contextual grouping functions.}
#' @name groupHumdrum
#' @aliases group_by
#' @export
group_by.humdrumR <- function(.data, ..., .add = TRUE) {
  .data <- uncontextMessage(.data, 'group_by')
  
  if (!.add) .data <- ungroup(.data)
  
  selectedFields <- selectedFields(.data)
  exprs <- rlang::enquos(...)
  calls <- sapply(exprs, rlang::quo_is_call)
  
  groupFields <- sapply(exprs[!calls], rlang::as_name)
  if (length(groupFields)) groupFields <- fieldMatch(.data, groupFields, 'group_by')
  
  
  fields <- fields(.data)
  groupn <- max(fields$GroupedBy)
  
  if (any(calls)) {
    oldfields <- fields$Name
    
    .data <- rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!(exprs[calls]))))
    fields <- fields(.data)
    
    newfields <- fields[ , !Name %in% oldfields]
    fields$Type[newfields] <- 'Grouping'
    groupFields <- c(fields$Name[newfields], groupFields)
  }
  
  fields[ , GroupedBy := Name %in% groupFields | GroupedBy]
  .data@Fields <- fields
  
  selectFields(.data, selectedFields)
  
  
  
  
}

#' Remove groups from humdrumR data
#' 
#' The `ungroup()` function removes grouping from a [humdrumR data object][humdrumRclass].
#' 
#' @rdname groupHumdrum
#' @aliases ungroup
#' @export
ungroup.humdrumR <- function(x, ...) {
  fields <- fields(x)
  fields[ , GroupedBy := FALSE]
  remove <- fields[Type == 'Grouping', Name]
  if (length(remove)) {
    fields <- fields[Type != 'Grouping']
    for (field in remove) x@Humtable[[field]] <- NULL
  }
  
  x@Fields <- fields
  x
  
}

### group information queries ----

#' Tabulate tokens in groups
#' 
#' Once groups are created, the `groups()` function can be used to tabulate 
#' the number of tokens in each group, and find their indices in the [humdrum table][humTable].
#' 
#' @rdname groupHumdrum
#' @export
groups <- function(humdrumR, dataTypes = 'D') {
  checks(humdrumR, xhumdrumR)
  dataTypes <- setdiff(checkTypes(dataTypes, 'groups'), c('S', 'E'))
  
  groupFields <- fields(humdrumR)[GroupedBy == TRUE, Name]
  humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
  
  groupTable <- humtab[ ,
                        as.list(table(factor(ifelse(Type %in% c('S', 'E'), 'I', Type),
                                             levels = dataTypes))), 
                        by = groupFields]
  
  if (length(dataTypes) > 1L) groupTable[ , Total := Reduce('+', .SD[ , !colnames(.SD) %in% groupFields, with = FALSE])]
  
  groupTable
   
  
  
}


#################################-
# Finding context ############----
#################################-


## Parsing context expressions ----




parseContextExpression <- function(expr, other) {
  if (is.null(expr)) expr <- quote('.')
  
  expr <- withinExpression(expr, applyTo = 'call', stopOnHit = TRUE,
                         \(Head) Head == 'hop',
                         \(exprA) {
                           
                           exprA$Args$along.with <- exprA$Args$along.with %||% quote(.)
                           exprA$Args$groupby <- quote(groupby)
                           exprA
                           
                         })
  
  expr <- substituteName(expr, list(nextopen = rlang::expr(lead(!!other, 1L)),
                                    end  = rlang::expr(length(.)),
                                    prevclose = rlang::expr(lag(!!other, 1L)),
                                    lastclose = rlang::expr(lag(!!other, 1L))))
  
  
  
  
  exprA <- analyzeExpr(expr)
  
  regexes <- c()
  if(exprA$Type == 'atomic' && exprA$Class == 'character') {
    regexes <- c(regexes, exprA$Args[[1]])
    expr <- rlang::expr(grepi_multi(., !!exprA$Args[[1]]))
  } else {
    if (exprA$Head == '|') {
      exprA$Head <- 'c'
      expr <- unanalyzeExpr(exprA)
    }
  }
  
  attr(expr, 'regexes') <- regexes
  expr
}


## Actual window finding ----

#' Group vectors into contextual windows 
#' 
#' The `context()` command can be used to group input data (vectors or [fields])
#' into arbitrary contextual windows.
#' Unlike [grouping vectors][groupHumdrum],
#' `context()` windows 1) are always contiguous relative to the reference vector(s)/field(s)
#' (which can depend on [order][order_by()]); 2) can *overlap*; and 3) don't necesarily exhaustively
#' divide the data.
#' The `context()` function should generally be called on 
#' [humdrumR data][humdrumRclass], but it can also be called directly on vectors.
#'  
#' @details 
#'
#' The `context()` function determines where contextual windows will begin and end based on 
#' [expressions][expressionEvaluation] in its `open` and `close` arguments.
#' These `open` and `close` expressions are evaluated using a **reference** [vector], or set of vectors/[fields()] that are all
#' the same length.
#'
#' In most cases, we'll apply `context()` to a [humdrumR data object], so windows are defined by
#' evaluating the `open` and `close` arguments using the [fields()] of the humdrum table as the reference.
#' Once this has been done, the humdrumR object will [show][humdrumRclass] how many windows
#' have been identified when printed in the console.
#' If we then use [with/within/mutate/summarize/reframe][withinHumdrum] on our data, these methods 
#' will evaluate their argument expressions within each and every contextual window defined by `context()`.
#' This means we can do basically anything we want to/with our contextual windows.
#'
#' We can also apply `context()` directly to a single input vector `x`, providing a [vector] or [list]/[data.frame] of 
#' equal-length vectors as the `reference` for the `open` and `close` arguments.
#' By default, `x` itself is reused as the `reference`, so windows are based on the input `x` itself.
#' When applied to a vector, `context()` will simply group the elements of `x` into the windows we defined,
#' with a few arguments to control how this is done:
#' 
#' + `complement`: The "complement" refers to elements of the input vector that don't fall inside
#'   any indicated windows: if `complement = FALSE` (the default), these "outside" values are
#'   dropped; if `complement = TRUE`, they are retained.
#' + `inPlace`: If `inPlace = TRUE`, windows are output in a vector of the same length as the input,
#'   padded with `NA` as needed---otherwise (the default), only the windows are returned
#' + `collapse`: If `collapse = TRUE`, the windows are collapsed to strings (separated by `sep`), otherwise,
#'   a `list()` of windows is returned.
#'   + `sep` the separator used if `collapse = TRUE`.
#' + `alignToOpen`: Should padded output (`inPlace = TRUE`) by aligned to the openning (left-side)
#'   of each window?
#' + `stripRegex`: Should regular expressions used to identify windows (details below) be stripped from the output?
#'
#' In the rest of this man page, we will apply `context()` to simple vectors (like the [letters] vector)
#' to illustrate how windows are defined.
#' In actual analyses, you'll be more likely to apply `context()` to [humdrumR data][humdrumRclass].
#' 
#' Note that, when using `context()` inside [with, within, etc.][withinHumdrum], 
#' the `alignToOpen` argument will have no effect.
#' Instead, use `alignLeft = FALSE` as an argument to 
#' `with()`/`within()`, not as an argument to `context()`.
#' 
#' #### groupby
#' 
#' The `groupby` argument is an optional [list] of grouping vectors,
#' all the same length as `x`/`reference`.
#' Contextual windows cannot cross boundaries indicated in `groupby`.
#' When applying `context()` to [humdrumR data][humdrumRclass],
#' `groupby` is automatically passed `list(Piece, Spine, Path)`, which prevents 
#' windows from crossing normal "melodic" boundaries in the data.
#'
#' @section Defining windows:
#' 
#' The system `context()` uses to define/identify windows in the data is quite sophisticated,
#' and can take some time to master!
#' The basic idea is that you must indicate where you want windows to start ("*open*") and 
#' where you want them to end ("*close*"):
#' you indicate this using the `open` and `close` arguments.
#' To introduce their usage, we will first do some simple examples applying 
#' `context()` to the built-in [letters] vector, which (by default) will act as our `reference`
#' vector *and* the target vector `x` to contextualize.
#' We will then show how these techniques can be used with multiple vectors/[fields()].
#'
#' The `open` and `close` arguments are [expressions][evaluatingExpressions] which, when evaluated, 
#' must indicate indices in the `reference` vector(s)/[field(s)][fields()];
#' For example, if we want a window to open at the 4th and 11th indices, 
#' and close at the 15th and 24th index,
#' we can write:
#' 
#' ```
#' context(letters, open = c(4, 11), close = c(15, 24))
#'
#' ```
#' 
#' This is quite trivial.
#' However, the `open` and `close` expressions can do a number of special tricks, 
#' including refering to each other.
#' For example, if either argument includes a call to [hop()],
#' `hop()` will automatically be applied along the input vector.
#' Consider this example:
#' 
#' ```
#' context(letters, open = hop(2), close = open + 3)
#' ```
#' 
#' In this example, the [hop()] command generates `open` indices for every odd
#' number from `1` to `25`.
#' The `close` argument then references these `open` indices, and adds `3` to each---
#' the result is pairs like `1:4`, `2:5`, `3:6`, `4:7`, etc.
#' If we give [hop()] different arguments (like `by` or `from`), we can modify this process.
#' In fact, if we use the default `by` value for `hop()` (`1`), we can use this approach to
#' create standard N-grams.
#' 
#' We can also indicate open/closes by providing `logical` vectors (the same length as `x`/`reference`).
#' For example:
#' 
#' ```
#' context(letters, open = letters %in% c('e', 'j', 'l'), close = open + 2)
#' 
#' ```
#' 
#' ### Regular Expressions
#' 
#' If either `open` or `close` are provided a `character` string, this string is treated
#' as a regular expression and is matched against the `reference` vector.
#' For example, we could make windows in the alphabet starting or ending on each vowel:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = open + 4)
#' context(letters, open = close - 4, close = '[aeiou]', alignToOpen = FALSE)
#' 
#' ```
#' 
#' If the `stripRegex = TRUE` (not the default), the matching `open` or `close` regular expressions are removed
#' from the output.
#' This can be useful if the character/tokens used to indicate windows are no longer needed
#' once windowing is done.
#' 
#' ### Special variables
#' 
#' The `open` and `close` expressions will understand a few special variable names:
#' 
#' + `nextopen`: represents the index of the *next* open---can only be used in the `close` argument.
#' + `prevclose`: represents the index of the *previous* close---can only be used in the `open` argument.
#' + `end`: represents the last index of the `reference` vector(s).
#' + `|`: As in "OR"---specify alternative window `open`/`close` criteria.
#' 
#' `
#' What if we'd like each of our windows to close right before the next window opens?
#' We can do this by making the `close` argument refer to the *next* `open`, by
#' referring to the `nextopen` variable:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L)
#' ```
#' 
#' Conversely, `open` can refer to the `prevclose` close:
#' 
#' ```
#' context(letters, open = prevclose + 1, close = '[aeiou]', alignToOpen = FALSE)
#' ```
#' 
#' Notice that when we called `context(letters, open = '[aeiou]', close = nextopen - 1L)`,
#' the window opening on `"u"` is not returned.
#' This is because there is no "`nextopen`" open to close on.
#' We can instead provide `context()` an alternative, using `|` (or):
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L | 26)
#' 
#' ```
#' 
#' Here we are saying, close a window 1 index before the next open *OR* at index 26.
#' What if we don't know exactly how long our input vector is?
#' Refer to the `end` variable:
#' 
#' ```
#' context(letters, open = '[aeiou]', close = nextopen - 1L | end)
#' ```
#' 
#' ### Separating context reference from application
#' 
#' The previous examples illustrate the basic concepts of using `open`/`close`;
#' to grasp how they work, study these examples and play around with them.
#' We can also define `open` and `close` expressions that reference more than one vector(s)/[field](s),
#' and which aren't necessarily the thing we want to apply the windowing to.
#' To illustrate this last point, let's take the last command from the previous section
#' and make it so the `x` argument is different than the `reference` argument:
#' 
#' ```
#' context(LETTERS, reference = letters, open = '[aeiou]', close = nextopen - 1L | 26)
#' ````
#' 
#' Now, `letters` is still being used as the windowing reference, but the contextual windowing is being
#' applied to `LETTERS`.
#' When we use `context()` on a [humdrumR dataset][humdrumRclass], the data's [fields()] can be used as the reference,
#' then [with(), within(), mutate()][withinHumdrum] can be used to manipulate other fields.
#' 
#' ### Multi-vector/field reference
#' 
#' The `open` and `close` arguments can reference more than one reference vector.
#' When applying `context()` to a vector `x`, we can provide a named `list()` or `data.frame()`
#' as the `reference` argument---so long as all the vectors they contain are the same length as `x`.
#' We can then refer to these vectors by name:
#'
#' ```
#' reference.frame <- data.frame(Threes = rep(1:3, length.out = 26), 
#'                                Fours = rep(4:1, length.out = 26))
#' 
#' context(letters, 
#'         reference = reference.frame,
#'         open = Threes == Fours, close = Fours == 1)
#' ```
#' 
#' So we created a data.frame with columns `Threes` and `Fours`.
#' We referenced *both* of these columns when defining where windows `open` and `close`.
#'
#' #### Using humdrumR data
#'
#' When we apply `context()` to [humdrumR data][humdrumRclass], we can refer to *any* of the 
#' data's [fields()] in `open` or `close`.
#' We can also use all of `open`/`close`'s special tricks (described above), like `hop()`, `nextopen`, `prevclose`,
#' and `end`.
#' For example, to create 4-grams in a humdrum dataset:
#' 
#' ```
#' humData |> context(open = hop(), open + 3)
#' 
#' ```
#' 
#' As mentioned above, when we apply `context()` to [humdrumR data][humdrumRclass],
#' `groupby` is automatically passed `list(Piece, Spine, Path)`, which prevents 
#' windows from crossing normal "melodic" boundaries in the data.
#' This can be overrriden by providing your own explicit `groupby` argument.
#' [Grouping fields][groupHumdrum] that have already been defined in the data, are *also* used.
#' 
#' 
#' 
#' @section Filtering windows:
#' 
#' Once `open` and `close` have identified where windows can start and end, there is still
#' some options for which open and close indices to associate with each other to create a window.
#' For example, as mentioned above, the `groupby` argument can be used to make sure windows to cross 
#' grouping boundaries---even if one group has and extra open index and the next an extra close index.
#' The minimum and maximum length of windows can also be controlled using the `min_length` and `max_length` arguments.
#' The `overlap`, `depth`, `rightward`, and `duplicate_indices` arguments provide a number of additional options,
#' which are useful for some use cases (details below).
#' 
#'
#' ### Nested windows
#' 
#' A common use-case for `context()` is analyzing phrases indicated in music.
#' In `**kern`, phrases are indicated with opening (`(`) and close (`)`) parentheses,
#' which we can capture with regular expressions for `open` and `close`.
#' Here is an example:
#' 
#' ```
#' nesting1 <- c('(a', 'b)', '(c', 'd', 'e)', '(d', 'e', 'f)', '(e', 'f', 'f#', 'g', 'g#', 'a)')
#' 
#' context(nesting1, open = '(', close = ')')
#' ```
#' 
#' Perfect.
#' However, what if there are nested phrasing indicators?
#' 
#' ```
#' nesting2 <- c('(a', 'b)', '(c', '(d', 'e)',  '(d', 'e)', 'f)', '(e', '(f', '(f#', 'g)', 'g#)', 'a)')
#' 
#' context(nesting2, open = '(', close = ')')
#' ```
#' 
#' That's not what we want!
#' By default, `context()` "pairs" each `open` with the next `close`, which often makes the most sense.
#' But in this case, we want different behavior.
#' We can get what we want by specifying `overlap = 'nested'`:
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested')
#' ```
#' 
#' Now context aligns each `open` with the corresponding `close` at the same *nesting level*.
#' What if we are only interested in the highest (or lowest) level of nesting?
#' Use the `depth` argument, which can be non-zero integers: the highest level is `1`,
#' with "deeper" levels incrementing up.
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 1)
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2)
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2:3)
#' ```
#' 
#' You can also use negative `depth` to specify from the deepest levels outward.
#' For example, in this case  `depth == -1` should get us that deepest level:
#' 
#' ```
#' context(nesting2, open = '(', close = ')', overlap = 'nested', depth = -1)
#' ```
#' 
#' If `depth` is `NULL` (the default), all depths are returned.
#' 
#' 
#' 
#' 
#' ### Controlling overlap
#'
#' There are some other options for controlling how windows can, or cannot, overlap.
#' Perhaps we'd like to look at every melodic phrase moving from *So* (dominant) to *Do* (tonic).
#' 
#' ```
#' melody <- c('so', 'la', 'ti', 'do', 'so', 'fi', 'so', 'la', 'ti', 're', 'do', 'so', 'la', 're', 'do')
#' 
#' context(melody, open = 'so', close = 'do')
#' 
#' ```
#' 
#' This output is probably not what we want.
#' Again, `context()` (by default) pairs each opening with the next close *which hasn't already been paired*.
#' In this case, that means the third *So* is getting pairs with the third *Do*, 
#' even though there is another *Do* in between!
#' We might want to try either the `"edge"` or `"none"` options for the `overlap` argument:
#' 
#' ```
#' context(melody, open = 'so', close = 'do', overlap = 'edge')
#' context(melody, open = 'so', close = 'do', overlap = 'none')
#' ```
#' 
#' The `"edge"` option allows the closing edge of windows to share a `close`---in this case,
#' the second and third *So* (`open`) are paired with the same *Do*.
#' On the other hand, with `overlap = "none"`, overlapping windows are simply not allowed, so the third `open` 
#' simply doesn't get paired with anything.
#' 
#' What if you would like to pair windows on their left (opening) edge?
#' If you specify `rightward = FALSE`, the overlap argument works backwards (right-to-left) through
#' the input vector, starting on each `close` and ending on each `open`.
#' By combining `righward = FALSE` with various `overlap` options, you can achieve a lot of windowing 
#' options you might need.
#' 
#' ### Repeated indices
#' 
#' Note that if `duplicates_indices = TRUE` (the default) the `open` and `close` arguments can 
#' incorporate repeated indices, including multiple matches to a regular expression in the same index.
#' This is useful with, for example, nested phrases:
#' 
#' ```
#' nesting3 <- c('(a', 'b)', '((c', 'd', 'e)',  '(d', 'e', 'f))', '(e', 'f', '((f#', 'g)', 'g#)', 'a)')
#' 
#' context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 1)
#' context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 2)
#' ````
#'
#' In some cases, you might want to turn `duplicate_indices = FALSE`. 
#' 
#' @param x ***Input data to group into windows.***
#' 
#' Must be an atomic vector.
#'
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param open ***Where to "open" (start) windows.***
#' 
#' Can be natural numbers, `logical` vectors (of the same length as `x`),
#' a single `character` string (interpreted as a regular expression).
#' May also be an arbitrary expression which returns natural numbers;
#' the expression can refer to named elements of `reference`, to `end` (last index),
#' to `close`, or to `prevclose` (the previous close).
#' 
#' @param close ***Where to "close" (end) windows.***
#' 
#' Can be natural numbers, `logical` vectors (of the same length as `x`),
#' a single `character` string (interpreted as a regular expression).
#' May also be an arbitrary expression which returns natural numbers;
#' the expression can refer to named elements of `reference`, to `end` (previous index),
#' to `open`, or to `nextopen` (the next open).
#' 
#' @param reference ***Vector(s) to use to identify window open/closes.***
#' 
#' Defaults to `x`.
#' 
#' Must be either an atomic vector of the same length as `x`, or a `list()`/`data.frame`
#' of such vectors, all [named][names()].
#' 
#' If `context()` is applied to a [humdrumR dataset][humdrumRclass],
#' all the fields of the data's [humdrum table][humTable] are used as `reference`.
#'  
#' @param overlap ***How are overlapping windows treated/created?***
#' 
#' Defaults to `'paired'`.
#' 
#' Must be a single `character`, [partially matching][partialMatching]
#' either `"paired"`, `"nested"`, `"edge"`, or `"none"`.
#' 
#' @param depth ***How "deep" can windows overlap?***
#'
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or a vector of non-zero whole numbers.
#' 
#' @param rightward ***Should window alignment/overlap be determined from left to right?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param duplicate_indices ***Can the same index open/close multiple windows?***
#'
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param min_length,max_length ***The minimum/maximum lengths of output windows.***
#' 
#' Default to two and infinity (no maximum) respectively.
#' 
#' Must be single, positive whole numbers.
#'
#' @param inPlace ***Should output be padded to same length as input?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param complement ***Should input "outside" any windows, be output?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param alignToOpen ***Should '`inPlace`' output be aligned to the open of each window?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param collapse ***Should output windows be collapsed to single `character` strings?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param sep ***Separator for collapsed output.***
#' 
#' Defaults to a comma (`","`).
#' 
#' Must be a single `character` string.
#' 
#' @param stripRegex ***Should regular expressions matched by the `open`/`close` arguments be removed from the output?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch. 
#' 
#' @param groupby ***Optional vectors to group windows within.***
#' 
#' Defaults to empty `list()`.
#' 
#' Must be a [list()], which is either empty or contains vectors which are all the same length as `x`.
#' In calls to [with/within.humdrumR][withinHumdrum], `groupby` is passed `list(Piece, Spine, Path)` by default.
#' 
#' Windows cannot cross group boundaries.
#' 
#' @examples 
#' 
#' # use the built-in 'letters' vector
#'
#' context(letters, open = hop(4), close = open + 3)
#' 
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end)
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end, inPlace = TRUE)
#' context(letters, open = "[aeiou]", close = nextopen - 1 | end, collapse = FALSE)
#' 
#' 
#' \dontrun{
#' # within.humdrumR
#' chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")
#' 
#' # 4-grams
#' chorales |>
#'   context(open = hop(), open + 3) |>
#'   within(paste(Token, collapse = ','))
#'        
#' # phrases leading to fermatas
#' chorales |>
#'   context(open = 1 | prevclose + 1, close = ';', overlap = 'none') |>
#'   within(paste(Token, collapse = ','), alignLeft = FALSE)
#'   
#' }
#' 
#' @export
context <- function(x, open, close, ...) UseMethod('context')
#' @family {Contextual grouping functions.}
#' @rdname context
#' @export
context.default <- function(x, open, close, reference = x, 
                    overlap = 'paired', depth = NULL, rightward = TRUE, duplicate_indices = TRUE, 
                    min_length = 2L, max_length = Inf,
                    inPlace = FALSE, complement = FALSE, alignToOpen = TRUE, 
                    collapse = TRUE, sep = ',', stripRegex = FALSE,  
                    groupby = list()
                    ) {
  
  checks(x, xatomic)
  # checks(reference, xmatch(x) | xnrowmatch(x))
  checks(overlap, xplegal(c('paired', 'nested', 'edge', 'none')))
  checks(depth, xnull | (xwholenum & xnotzero))
  checks(rightward, xTF)
  checks(duplicate_indices, xTF)
  checks(min_length, xpnatural)
  checks(max_length, xpnatural)
  checks(inPlace, xTF)
  checks(complement, xTF)
  checks(alignToOpen, xTF)
  checks(collapse, xTF)
  checks(sep, xcharacter & xlen1)
  checks(stripRegex, xTF)
  
  open  <- rlang::enexpr(open)
  close <- rlang::enexpr(close)
  windowFrame <- findWindows(reference, open, close, 
                             duplicate_indices = duplicate_indices,
                             overlap = overlap, depth = depth, rightward = rightward,
                             min_length = min_length, max_length = max_length)
  
  regexes <- attr(windowFrame, 'regexes')
  if (stripRegex) for (re in escapebraces(regexes)) x <- gsub(re, '', x)
  
  expr <- if (collapse) rlang::expr(paste(.x., collapse = !!sep)) else rlang::expr(list(.x.))
  
  .applyWindows(data.table(.x. = x), windowFrame, expr, field = '.x.',
                inPlace = inPlace, complement = complement, alignToOpen = alignToOpen)
  
  
  
  
}

#' @rdname context
#' @export
context.humdrumR <- function(humdrumR, open,  close, 
                             overlap = 'paired', depth = NULL,
                             rightward = TRUE, duplicate_indices = TRUE, 
                             min_length = 2L, max_length = Inf, groupby) {
  
  # checks(reference, xmatch(x) | xnrowmatch(x))
  checks(overlap, xplegal(c('paired', 'nested', 'edge', 'none')))
  checks(depth, xnull | (xwholenum & xnotzero))
  checks(rightward, xTF)
  checks(duplicate_indices, xTF)
  checks(min_length, xpnatural)
  checks(max_length, xpnatural)
  
  open  <- rlang::enexpr(open)
  close <- rlang::enexpr(close)
  
  humtab <- getHumtab(humdrumR, 'D')
  
  if (missing(groupby)) groupby <- humtab[ , c('Piece', 'Spine', 'Path'), with = FALSE]
  
  groupFields <- setdiff(getGroupingFields(humdrumR, .by = c(), 'context.humdrumR'), c('Piece', 'Spine', 'Path'))
  if (length(groupFields)) groupby <- cbind(groupby, humtab[ , groupFields, with = FALSE])
  
  windowFrame <- findWindows(humtab, open, close, 
                             groupby = groupby,
                             duplicate_indices = duplicate_indices,
                             overlap = overlap, depth = depth, rightward = rightward,
                             min_length = min_length, max_length = max_length)
  
  humdrumR@Context <- windowFrame
  
  humdrumR
  
}

#' Remove context
#'
#' The `uncontext()` function removes contextual windows
#' from a [humdrumR data object][humdrumRclass].
#'
#' @export
#' @rdname context
uncontext <- function(humdrumR) {
  checks(humdrumR, xhumdrumR)
  
  humdrumR@Context <- humdrumR@Context[0]
  humdrumR
}

uncontextMessage <- function(humdrumR, func) {
  if (nrow(humdrumR@Context)) {
    .warn("Using {func} requires removing contextual windows from your humdrumR data.",
             "{num2print(nrow(humdrumR@Context))} windows are being removed.\n",
             "To avoid this warning, manually remove the windows using uncontext() before calling {func}.\n")
    
    uncontext(humdrumR)
  } else {
    humdrumR
  }
}

#' View contextual windows
#' 
#' Once contextual windows are created, the `windows()` function can be used to
#' view a [data.table] representing these windows.
#' The `Open` and `Close` columns indicate row indices in the [humdrum table][humTable].
#' 
#' @rdname context
#' @export
windows <- function(humdrumR) {
  windows <- humdrumR@Context
  windows[ , RevDepth := NULL]
  windows[ , Length := Close - Open + 1L]
  windows[]
  
}

findWindows <- function(x, open, close = quote(nextopen - 1), ..., 
                        field = 'Token',
                        overlap = 'paired', depth = NULL,  rightward = TRUE, duplicate_indices = TRUE,
                        groupby = list(),
                        min_length = 2L, max_length = Inf) {
  
  
  if (!is.data.frame(x)) x <- setNames(data.table::data.table(. = x), field %||% '.')
  
  open <- parseContextExpression(open, other = quote(close))
  close <- parseContextExpression(close, other = quote(open))
  regexes <- union(attr(open, 'regexes'), attr(close, 'regexes'))
  if (!is.null(field)) {
    open  <- substituteName(open, list('.' = rlang::sym(field)))
    close <- substituteName(close, list('.' = rlang::sym(field)))
  }
  
  # do the expressions reference each other?
  
  openDepends <- 'close' %in% namesInExpr('close', open) 
  closeDepends <- 'open' %in% namesInExpr('open', close)
  
  if (openDepends && closeDepends) .stop("In your call to context, your open and clsoe arguments are mutually referential in a circular manner.")
  
  
  open_indices <- close_indices <- NULL
  for (i in order(c(openDepends, closeDepends))) { # this is all just to make sure any the independent expressions are evaluated first
    val <- rlang::eval_tidy(list(open, close)[[i]], 
                            data = c(x, list(open = open_indices, close = close_indices, groupby = groupby)))
    if (is.logical(val)) val <- which(val)
    
    assign(c('open_indices', 'close_indices')[i], val)
  }
  if (!duplicate_indices) {
    open_indices <- unique(open_indices)
    close_indices <- unique(close_indices)
  }


  # 
  windowFrame <- if (length(open_indices) && length(close_indices)) {
   align(open_indices, close_indices, fullLength = nrow(x),
                       overlap = overlap, rightward = rightward, depth = depth, groupby = groupby,
                       min_length = min_length, max_length = max_length)
  } else {
    data.table(Open = integer(0L), Close = integer(0L), Length = integer(0L), 
               Depth = integer(0L), RevDepth = integer(0L))
  }
  
  #                      
  windowFrame <- windowFrame[Open >= 1L & Open <= nrow(x) & Close >= 1L & Close <= nrow(x)]
  windowFrame <- windowFrame[Reduce('&', lapply(windowFrame, Negate(is.na)))]
  
  attr(windowFrame, 'regexes') <- regexes
  attr(windowFrame, 'vector') <- x
  windowFrame
  
}


print.windows <- function(x) {
  plot.new()
  vec <- attr(x, 'vector')[[1]]
  
  plot.window(ylim = c(0, max(x$Depth)),xlim = c(0, length(vec)))
  
  text(1:length(vec), rep(0, length(vec)), vec)
  y <- x$Depth
  y <- as.numeric(make.unique(as.character(y)))
  graphics::arrows(x$Open, x$Close, y0 = y, y1= y, angle = 90, code = 3)
  
  print(x)
}

### Window finding rules ----


# 
align <- function(open, close, fullLength, groupby = list(),
                  overlap = 'paired', rightward = TRUE, depth = NULL, 
                  min_length = 2L, max_length = Inf) {

  overlap <- pmatches(overlap, c('nested', 'paired', 'edge', 'none'))
  groupby <- squashGroupby(groupby)
  
  if (!rightward) {
    openx <- open
    open  <- (fullLength + 1L) - close
    close <- (fullLength + 1L) - openx
    groupby <- rev(groupby)
    
  }
  
  open <- sort(open)
  close <- sort(close)
  
  if (overlap == 'paired' && 
      (length(open) == length(close)) && 
      all((close - open + 1L) >= min_length & (close - open + 1L) <= max_length, na.rm = TRUE)) {
    
    windowFrame <- data.table(Open = open, Close = close) 
    windowFrame <- windowFrame[!is.na(Open) & !is.na(Close) & Open > 0L & Close > 0L]
    
    if (length(groupby)) {
      windowFrame <- windowFrame[groupby[Open] == groupby[Close]]
    }
    
  } else {
    open <- open[!is.na(open)]
    close <- close[!is.na(close)] 
    
    if (length(groupby)) {
      groupbyopen <- groupby[open]
      groupbyclose <- groupby[close]
    } else {
      groupbyopen <- integer(length(open))
      groupbyclose <- integer(length(close))
    }
    
    openframe  <- data.table(Open = open,   OpenInd = seq_along(open),   Group = groupbyopen)
    closeframe <- data.table(Close = close, CloseInd = seq_along(close), Group = groupbyclose)
    
    windowFrame <- openframe[closeframe, on = 'Group', allow.cartesian = TRUE]
    #
    windowFrame[ , Length := Close - Open + 1L]
    windowFrame <- windowFrame[Length >= min_length & Length <= max_length]
  
    
    if (overlap == 'nested') {
      
      windowFrame <- nest(windowFrame)
    } else {
      windowFrame[ , Hit := CloseInd == min(CloseInd), by = OpenInd]
      windowFrame <- windowFrame[Hit == TRUE]
      windowFrame <- switch(overlap,
                            paired = shunt(windowFrame, open, close),
                            edge = windowFrame,
                            none = { 
                              i <- 1
                              while(i <= nrow(windowFrame)) {
                                windowFrame <- windowFrame[Open > Close[i] | Close <= Close[i]]
                                i <- i + 1
                              }
                              windowFrame[ , Hit := OpenInd == min(OpenInd), by = CloseInd]
                              windowFrame[Hit == TRUE]
                              }
                            )
    }
    
    setorder(windowFrame,  OpenInd, CloseInd)
    windowFrame[ , c('OpenInd', 'CloseInd', 'Group', 'Hit') := NULL]
  }
  
  # update depth
  windowFrame <- depth(windowFrame, depth = depth)

  if (!rightward) windowFrame[ , c('Open', 'Close') := list((fullLength + 1L) - Close, 
                                                            (fullLength + 1L) - Open)]

  
  windowFrame
  
}


### Sorting, filtering, or modifying windows ----
nest <- function(windowFrame) {
  # setorder(windowFrame, Open, Close)
  open <- windowFrame[!duplicated(OpenInd), Open]
  close <- windowFrame[!duplicated(CloseInd), Close]
  
  steps <- rep(c(1L, -1L), c(length(open), length(close)))[order(c(open, close))]
  stepdepth <- sigma(steps)
  
  opendepth  <- stepdepth[steps == 1L][windowFrame$OpenInd]
  closedepth <- stepdepth[steps == -1L][windowFrame$CloseInd]
  

  windowFrame <- windowFrame[opendepth == (closedepth + 1L)]
  
  windowFrame[ , Hit := CloseInd == min(CloseInd), by = OpenInd]
  windowFrame[Hit == TRUE]
  
}
shunt <- function(windowFrame, open, close) {
  # "shunt" looks for window Close positions that have aleady been matched
  # with a previous Open, and "shunts" them to the next Close.

  closeinds <- unique(windowFrame$CloseInd)
  
  while(any(duplicated(windowFrame$CloseInd))) {
    windowFrame[ , CloseInd := CloseInd + (seq_along(Open) - 1), by = CloseInd]
    windowFrame <- windowFrame[CloseInd %in% closeinds]
  }
  windowFrame[ , Open := open[OpenInd]]
  windowFrame[ , Close := close[CloseInd]]
  
  windowFrame
  
}



depth <- function(windowFrame, depth = NULL, ...) {
  if (nrow(windowFrame) == 0L) {
    windowFrame[ , c('Depth', 'RevDepth') := list(integer(), integer())]
    return(windowFrame)
  }
  
  windowFrame[ , Depth := {
    steps <- rep(c(1L, -1L), c(length(Open), length(Close)))[order(c(Open, Close))]
    stepdepth <- sigma(steps)
    stepdepth[steps == 1L]
  }]
  
  windowFrame[ , RevDepth := Depth - max(Depth) - 1L]

  if (!is.null(depth))   windowFrame <- windowFrame[Depth %in% depth | RevDepth %in% depth]
  
  windowFrame
}
# 
# removeCrossing <- function(windowFrame, groupby) {
#   groupby <- do.call('paste', groupby)
#   windowFrame[groupby[Open] == groupby[Close]]
# }

##########################################-
# Applying functions in context #######----
##########################################-

# contextApply <- function(x, open, close, func = c, ..., groupby = list(), inPlace = FALSE, reference = x, vectorize = TRUE, openIndex = TRUE) {
#   
#   open  <- rlang::enexpr(open)
#   close <- rlang::enexpr(close)
#   
#   windowFrame <- findWindows(reference, open, close, ..., groupby = groupby)
#   func
#   .applyWindows(data.table(.x. = x), windowFrame, rlang::quo(func(.x.)), ..., activeToken = '.x.',
#                 inPlace = inPlace, openIndex = openIndex)
# }


windows2groups <- function(dt, windowFrame) {
  # expands overlapping windows into "groups" appropriate for use with data.table[ , , by = group]
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  
  dt <- rbind(dt[Type != 'D'], 
              dt[Type == 'D'][unlist(indices)])
  dt[Type == 'D', contextWindow := rep(seq_along(indices), lengths(indices))]
  
  dt
  # 
  # 
  # 
  # xindices <- which(dt$Type == 'D')[unlist(indices)]
  # contextWindow <- rep(seq_along(indices), lengths(indices))[order(xindices)]
  # xindices <- sort(xindices)
  # 
  # allindices <- c(which(dt$Type != 'D'), xindices)
  # context <- integer(length(allindices))
  # context[allindices %in% xindices] <- contextWindow
  # context <- context[order(allindices)]
  # allindices <- sort(allindices)
  # 
  # dt <- dt[allindices]
  # 
  # 
  # dt[, contextWindow := context] 
  # 
}

.applyWindows <- function(dt, windowFrame, expr, field = 'Token', ..., 
                          inPlace = TRUE, complement = TRUE, alignToOpen = TRUE) {
  indices <- windowFrame[ , list(list(Open:Close)), by = seq_len(nrow(windowFrame))]$V1
  
  dt_extended <- windows2groups(dt, windowFrame)
  
  results <- rlang::eval_tidy(rlang::quo({ dt_extended[ , list(list(!!expr)), by = contextWindow]}))$V1
  # should be a list of results, one result per window
  
  
  edges <- windowFrame[ , if (alignToOpen) Open else Close]
  edge <- if (alignToOpen) head else tail
  
  
  result_lengths <- lengths(results)
  edgeindices <- Map(indices, result_lengths, f = \(i, l) edge(i, n = l))
  
  
  output <- if (complement) dt[[field]] else rep(NA, nrow(dt))
  output[.unlist(edgeindices)] <- .unlist(results, recursive = FALSE)
  
  
  if (inPlace) {
    output[setdiff(unlist(indices), unlist(edgeindices))] <- NA
    output
  } else {
    if (complement) {
      output[-setdiff(unlist(indices), unlist(edgeindices))]
    } else {
      output[unlist(edgeindices)]
      
    }
  }
  
} 

applyNgram <- function(n = 2, vecs, f = c, by = NULL, pad = TRUE, 
                       fill = NA, splat = !is.null(by), ...) {
  # vecs is list of vectors  of same length
  if (!is.null(by)) vecs <- lapply(vecs, split, f = by)
  
  if (n == 0) stop("You called applyNgram with n = 0, but you can't make an 0-gram!", call. = FALSE)
  if (!allsame(lengths(vecs))) stop("You have tried to applyNgram across multiple vectors of different lengths, but this is not supported.", .call = FALSE)
  
  n <- n - (sign(n))
  
  
  if (n == 0) { 
    output <- do.call('Map', c(f, vecs))
  } else {
    
    starts <- seq_along(vecs[[1]])
    if (pad) {
      vecs <- lapply(vecs, \(vec) c(rep(NA, abs(n)), vec, rep(NA, abs(n))))
      starts <- starts + abs(n)
    } else {
      starts <- starts[ (starts + n) <= length(vecs[[1]]) & starts + n >= 1]
    }
    
    inds   <- if (sign(n) == 1) Map(`:`, starts, starts + n) else Map(`:`, starts + n, starts)
    
    #
    
    ngs <- lapply(vecs, 
                  function(vec) {
                    lapply(inds, \(i) vec[i])
                  })
    .f <- if (splat) { 
      function(...) {do.call('f', list(...)) }
    } else {
      f
    }
    output <- do.call('Map', c(.f, ngs))
    
    if (pad && !is.na(fill)) output <- lapply(output,
                                              function(out) {
                                                if (is.character(out)) gsub('NA', fill, out) else `[<-`(out, is.na(out), fill)
                                              })
    
  } #end of if(n == 0) else
  
  if (all(lengths(output) == 1)) output <- unlist(output)
  
  output
  
}


# context tools ----



grepi_multi <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  ns <- x %~n% pattern %|% 0L
  rep(x %~i% pattern, ns[ns > 0L])
  
}

grepn <- function(x, pattern) {
  pattern <- escapebraces(pattern)
  
  x %~n% pattern %|% 0L
  
}


windowsSum <- function(x, windowFrame, na.rm = FALSE, cuttoff = 10) {
  # takes elements of x and a window frame and quickly sums x within windows
  
  lengths <- table(windowFrame[Length > 1L, Length])
  
  vectorize <- lengths >= cuttoff & as.integer(names(lengths)) < 20L
  
  if (any(vectorize)) {
    maxsize <- max(as.integer(names(lengths)[vectorize]))
    
    na <- as(NA, class(x))
    for (l in 1:(maxsize - 1L)) {
      
      windowFrame[Length == l, 
                  {
                    curClose <- Open + l
                    x[Open] <<- x[Open] + x[curClose]
                    x[curClose] <<- na
                    
                  }]
      windowFrame <- windowFrame[Length != l]
    }
  }
  

  if (nrow(windowFrame)) x <- .applyWindows(data.table(.x. = x), field = '.x.', 
                                            windowFrame,
                                            expr = rlang::expr(sum(.x., na.rm = !!na.rm)),
                                            inPlace = TRUE, complement = TRUE)
  
  x
  
  
}


#' Generate regular sequence "along" input
#' 
#' `hop()` is similar to base R's [seq()], but with some additional features, including
#' special sugar when used with humdrumR's [context()] command.
#' `hop()` is used to create customizable sequences of indices for a vector;
#' for example, if you want to index every third value from a vector.
#' This is useful for, as when used with [context()], defining the start points of "rolling-window"
#' analyses along a vector; the "hop size" is the gap between the start of each window, 
#' defined by `hop()`'s `by` argument.
#' 
#' @details 
#' 
#' `hop()` has similar arguments to [base::seq()], but focused on the `along.with` argument,
#' a vector which you'd like to generate indices for.
#' If you simply call `hop(myvector)`, the output will be the same as `1:length(myvector)`.
#' The `by` argument can be used to specify a different "hop" pattern: `by = 2` will get you *every other*
#' index, `1`, `3`, `5`, `7`, etc.
#' Unlike [base::seq()], `hop()`'s `by` argument can be a vector of numbers, allowing you to specify a pattern of hops.
#' For example, `by = c(2, 3)` will first hop `2`, then hop `3`, then repeat---so the output would be `1`, `3`, `6`, `8`, `11`, `13`, etc.
#' 
#' The by pattern can be comprised of negative numbers, or a mix of negative and positive numbers.
#' If `by` mixes negative and positive numbers, the pattern can hop up and down, as it climbs.
#' For example, you could go up two, then down one, then repeat using `by = c(2,-1)`.
#' If the pattern is *overall* (sums) negative, the `from` argument must be greater than the `to` argument (see next section);
#' if the pattern sums to zero, an error occurs because the pattern would never end!
#' If a pattern changes directions, it is possible for pattern to hop outside the bounds of the vector;
#' if this happens, the outside indices return `NA`.
#' 
#'
#' @returns
#' 
#' By default, `hop()` returns an `integer` vector, appropriates indices for the `along.with` vector.
#' However, there are two other options:
#' 
#' + If `logical = TRUE`, the indices are returned as a logical vector, the same length as `along.with`,
#'   with `TRUE` values indicating indices. Note that any ordering in the output (due to a mix of positive and negative
#'   values in the `by` argument) is lost.
#' + If `value = TRUE`, the actual indixed elements of the `along.with` vector are returned:
#'   Thus, `hop(myvector, ..., value = TRUE)` is simply the same as `myvector[hop(myvector, ...)]`.
#'   If `value = TRUE`, the `logical` argument is ignored.
#' 
#' 
#' @section Starting and Ending:
#' 
#' By default, `hop()` builds indices from `1` to the end of the `along.with` vector.
#' The `from` and `to` arguments can be used to control this.
#' Either argument can simply be a natural number, indicating where to start and end the output sequences.
#' (If `to` is `NULL`, it is set to `length(along.with)`.)
#' An alternate approach is to provide either argument a single `character` string, which is treated as a regular 
#' expression and matched against `along.with`, or  a `logical` vector the same length as `along.with`.
#' The first match/`TRUE` is used for the `from` index and the last match/`TRUE` for the `to` index.
#' This means you can say things like `from = Record == 33` in a [within()][withinHumdrum] call.
#' 
#' If the `by` argument is *overall* (sums) positive, `from` must be less than `to`.
#' If the `by` argument is *overall* (sums) negative, `from` must be greater than `to`.
#' 
#' If the `by` pattern doesn't ever actually the actual `to` index---perhaps jumping over it---
#' the output stops when it would *pass* the `to`.
#' 
#' @section Grouping:
#' 
#' In many cases we want to how along vectors, but *not across certain boundaries*.
#' For example, if we want all even numbered indices, we can set `by = 2` and `from = 2`.
#' However, if your vector includes data from multiple pieces, and some of the pieces have an odd number of data points,
#' our "even" sequence would end up hitting odd numbers in some pieces.
#' To get around this, the `groupby` argument indicates one, or more, grouping vectors, which break the `x` (input) argument
#' into groups.
#' If more than `groupby` vectors are given, a change in *any* vector indicates a boundary.
#' Each grouped segement of `along.with` is treated just like a separate call to `hop()`;
#' for example, if `from = 2`, the hop sequence will start on the second index of *each* group.
#' However, the output indices still represent the original `along.with` indices.
#' 
#' Since `hop()` is usually used with [context()] to create rolling windows within musical parts,
#' we want typically want to apply `hop()` using `groupby = list(Piece, Spine, Path)`.
#' In fact, `humdrumR` [with(in)][withinHumdrum] calls will *automatically* feed these 
#' three fields as `groupby` arguments to `hop()`.
#' So any use of `hop()` in a call to [with(in)][withinHumdrum], will automatically generate the hop sequence
#' in a "melodic" way, within each spine path of each piece.
#'
#' @param along.with ***The vector you want indices to "hop" along.***
#' 
#' Must be a vector (either atomic, or a `list()`).
#' 
#' @param by ***The pattern of "hops" to to use.***
#' 
#' Defaults to `1`: returning all indices `from:to`.
#' 
#' Must be one or more whole numbers.
#' 
#' `sum(by)` must non-zero.
#'
#' @param from ***Where to start the sequence.***
#' 
#' Defaults to `1`: starting from the first index.
#'
#' Must be either a single natural number, a single `character` string, or 
#' a `logical` vector the same length as `along.with`.
#' 
#' A `character`-string input is treated as a regular expression,
#' which is matched against `along.with` using [grepl()] to generate a `logical` vector.
#' The index of the first `TRUE` is used.
#' 
#' @param to ***Where to end the sequence.***
#' 
#' Defaults to `NULL`.
#'
#' Must be either `NULL`, a single natural number, a single `character` string, or 
#' a `logical` vector the same length as `along.with`.
#' 
#' If `NULL`, `to` is set to the last index of `along.with` (or of each group in `groupby`).
#' A `character`-string input is treated as a regular expression,
#' which is matched against `along.with` using [grepl()] to generate a `logical` vector.
#' The index of the last `TRUE` is used.
#' 
#' @param value ***Should actual values from `along.with` be returned?***
#'
#' Defaults to `FALSE`.  
#'
#' Must be a singleton `logical` value; an on/off switch.
#' 
#' @param value ***Should indices be returned as logical `TRUE`s?***
#'
#' Defaults to `FALSE`.  
#'
#' Must be a singleton `logical` value; an on/off switch.
#'
#' @param groupby ***Optional vectors to group hop sequences within.***
#' 
#' Defaults to empty `list()`.
#'
#' Must be a [list()], which is either empty or contains vectors which are all the same length as `along.with`.
#' In calls to [with/within.humdrumR][withinHumdrum], `groupby` is passed `list(Piece, Spine, Path)` by default.
#'
#' @examples 
#' # use the built-in 'letters' vector
#' 
#' hop(letters)
#' 
#' hop(letters, by = 3)
#' 
#' hop(letters, by = 2, from = 4)
#' 
#' hop(letters, by = 2, from = 'e', to = 'x')
#'
#' hop(letters, by = c(-1, 2), from = 'e', to = 'w', value = TRUE)
#' 
#' hop(letters, by = -1, from = 'z', to = 3)
#' 
#' @export
hop <- function(along.with, by = 1, from = 1L, to = NULL, value = FALSE, logical = FALSE, groupby = list()) {
  if (length(along.with) == 0L) return(integer())
  
  checks(along.with, xvector)
  checks(by, xwholenum & xminlength(1))
  checks(from, ((xwholenum | xcharacter) & xlen1) | (xlogical & xmatch(along.with)))
  checks(to, xnull | ((xwholenum | xcharacter) & xlen1) | (xlogical & xmatch(along.with)))
  checks(value, xTF)
  checks(logical, xTF)
  

  along <- if (length(groupby)) tapply(along.with, groupby, c, simplify = FALSE) else list(along.with)
  
  dt <- data.table(i = seq_along(along.with),
                   Group = if (length(groupby)) squashGroupby(groupby) else 1)
  dt[, igrouped := seq_along(i), by = Group]
  dt[ , Max := rep(max(i), length(i)), by = Group]
  
  # from
  starts <- if (is.numeric(from)) {
    dt[igrouped == from]
  } else {
    if (is.character(from)) from <- grepl(from, along.with)
    dt[from, .SD[1], by = Group]
  }
  
  # to
  if (is.null(to)) to <- max(dt$igrouped)
  ends <- if (is.numeric(to)) {
    dt[igrouped <= to, .SD[.N], by = Group]
  } else {
    if (is.character(to)) to <- grepl(to, along.with)
    dt[to, .SD[.N], by = Group]
  }
  ranges <- starts[ends, on = 'Group']
  ranges[ , Length := i.i - i]
  ranges[ , Length := Length + sign(Length)]
  
  
  ranges <- ranges[!is.na(i) & !is.na(i.i)]
  
  # actual sequences!
  interval <- sum(by)
  if (interval < 0) {
    if (any(ranges$Length > 0)) .stop("In your call to hop(), your 'by' pattern is negative overall, but your `from` argument is less than your `to` argument.",
                                      "There is no way to hop down from a lower number to a higher number.")
    ranges <- ranges[nrow(ranges):1]
  } else {
    if (any(ranges$Length < 0)) .stop("In your call to hop(), your 'by' pattern is positive overall, but your `from` argument is greater than your `to` argument.",
                                      "There is no way to hop up from higher number to a lower number.")
  }
  
  if (interval == 0L) .stop("In call to hop(), the by argument cannot sum to zero")
  
  fullpattern <- rep_len(by, (ceiling(max(abs(ranges$Length)) / abs(interval))) * length(by))
  fullpattern <- cumsum(c(0L, fullpattern))
  
  
  result <- ranges[ , {
    pat <- fullpattern + i
    endon <- pat == i.i
    if (any(endon)) {
      pat <- pat[1:which(endon)[1]]
    } else {
      past <- if (interval > 0L) pat > i.i else pat < i.i
      if (any(past)) pat <- pat[1:(which(past)[1] - 1L)]
    }
    pat[ pat <= (i - igrouped) | pat > Max] <- NA
    pat
  }, by = seq_len(nrow(ranges))]$V1
  
  if (value) {
    result <- along.with[result]
  } else { 
    if (logical) result <- seq_along(along.with) %in% result
    
  }
  
  result
  
}


first <- function(x) nth(x, 1L)
last  <- function(x) lth(x, 1L)
nth <- first <- function(x, n) {
  if (n > length(x)) return(vectorNA(1L, class(x)))
  x[n]
}
lth  <- function(x, n) {
  if (n > length(x)) return(vectorNA(1L, class(x))) 
  x[(length(x) + 1L) - n]
}

Next <- function(x) lead(x, 1L)
Prev <- function(x) lag(x, 1L)


`%before%` <- function(hits, anchors) {
  i <- findInterval(anchors, hits, left.open = T)
  i[i == 0 | i > length(hits)] <- NA_integer_
  hits[i]
}
`%after%` <- function(hits, anchors) {
  -rev(sort(-hits) %before% sort(-anchors))
}

