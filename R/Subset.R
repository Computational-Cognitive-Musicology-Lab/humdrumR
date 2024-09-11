
# Filtering humdrumR----


#' Filter humdrum data
#' 
#' HumdrumR defines [subset()][base::subset] (base R) and [filter()][dplyr::filter] (tidyverse) methods 
#' for [humdrumR data][humdrumR::humdrumRclass]---these two `.humdrumR` methods are synonymous,
#' working exactly the same.
#' They are used to "filter" the contents of the underlying [humdrum table][humTable].
#' R's standard indexing operators (`[]` and `[[]]`) can also be used to filter data---
#' you can read about these indexing options [here][indexHumdrum]---however, 
#' the `subset()`/`filter()` can accomplish much more sophisticated filtering commands than the indexing
#' methods.
#' 
#' @details
#' 
#' `subset()` and `filter()` are passed one or more expressions which are using the 
#' fields of the [humdrum table][humTable] using a call to [within][withinHumdrumR].
#' This evaluation can thus include all of [within.humdrumR()]'s functionality (and arguments)
#' including group-apply.
#' The only requirement is that the expressions/functions fed to `subset()`/`filter()`
#' *must* be return a logical (`TRUE`/`FALSE`) vector (`NA` values are treated as `FALSE`).
#' The returned vector must either be scalar (length `1`), or be the same length as the input data (the number
#' of rows in the [humdrum table][humdrumR::humTable]).
#' If the logical result is scalar, it will be recycled to match the input length: this is useful
#' in combination with `group_by()`; for example, you can split the data into groups, then
#' return a single `TRUE` or `FALSE` for each group, causing the whole group to be filtered or not.
#' 
#' Note that `subset()`/`filter()` are incompatible with [contextual windows][context()]; if
#' your data has contextual windows defined, they will be removed (with a warning message) before filtering.
#' 
#' @section Nullifying data:
#' 
#' When using `subset()`/`filter()`, humdrumR doesn't actually delete the data you filter out.
#' Instead, what these functions do is set all filtered data fields to `NA` (null) values, and
#' changing their data type to `"d"`.
#' This ensures that the humdrum-syntax of the data is not broken by filtering!
#' Thus, when you print a 
#' filtered [humdrumR object][humdrumR::humdrumRclass] you'll see all the filtered data points 
#' turned to null data (`.`).
#' Since, most `humdrumR` functions ignore null data (`d`) by default, the data is effectively filtered out 
#' for most practical purposes.
#' However, if you need to use those null (`'d'`) data points (like, with [ditto()]), they
#' can be accessed by setting `dataTypes = 'Dd'` in many functions.
#' See the [ditto()] documentation for examples.
#' 
#' @section Truly removing data:
#' 
#' In many cases, filtering out large parts of your data leaves a bunch of empty null
#' data points (`"."`) in your printout...which maybe be difficult to read.
#' If you *want* to **actually** remove these filtered data points, you can call `removeEmptyFiles()`, 
#' `removeEmptyPieces()`, `removeEmptySpines()`,  `removeEmptyPaths()`, `removeEmptyRecords()`, or `removeEmptyStops()`.
#' These functions will safely remove null data without breaking the humdrum syntax;
#' They do this by going through each piece/spine/path/record and checking if *all* the data in that region
#' is null; if, and only if, *all* the data is null, that portion of data will be removed.
#' 
#' By default, `subset.humdrumR()` automatically calls `removeEmptyPieces()` before returning.
#' However, you can stop this by specifying  `removeEmptyPieces = FALSE`.
#' 
#' @section Renumbering:
#'
#' If filtered pieces, files, or spines are removed from a corpus 
#' (using `removeEmptyPieces()` or `removeEmptySpines()`)
#' the `File`, `Piece`, `Record` and/or `Spine` fields are renumbered to represented the remaining regions,
#' starting from `1`.
#' For example, if you have a corpus of 10 pieces and remove the first piece (`Piece == 1`),
#' the remaining pieces are renumbered from `2:10` to `1:9`.
#' Spine/record renumbering works the same, except it is done independently *within* each piece.
#' 
#' @param x,.data,humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Arbitrary expressions passed to [with(in)][withinHumdrum].***
#'
#' The "within" expression(s) must evaluate to either scalar or full-length `logical` values.
#' 
#' @param removeEmptyPieces ***Should empty pieces be removed?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
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
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' # remove spine 1 (non destructive)
#' humData |> subset(Spine > 1)
#' 
#' # remove spine 1 (destructive)
#' humData |> subset(Spine > 1) |> removeEmptySpines()
#' 
#' # remove odd numbered bars
#' 
#' humData |> group_by(Bar) |> subset(Bar[1] %% 2 == 1)
#'
#' # unfiltering and complement
#' 
#' humData |> filter(Spine %in% 1:2) |> complement()
#' 
#' humData |> filter(Spine %in% 1:2) |> unfilter()
#' 
#' humData |> filter(Spine %in% 1:2) |> solfa() |> unfilter(complement = 'Token')
#' 
#' @seealso {The [indexing operators][indexHumdrum] `[]` and `[[]]` can be used as shortcuts for common `subset` calls.}
#' @export
#' @aliases subset
subset.humdrumR <- function(x, ..., dataTypes = 'D', .by = NULL, removeEmptyPieces = TRUE) {
  checks(removeEmptyPieces, xTF)
  x <- uncontextMessage(x, 'subset')
  
  checkTypes(dataTypes, 'subset.humdrumR')
  
  quosures <- rlang::enquos(...)
  
  subsets <- local({
    groupFields <- getGroupingFields(x, .by, 'subset.humdrumR') 
    subsets <- rlang::eval_tidy(rlang::quo(with.humdrumR(x, !!!quosures, recycle = 'ifscalar',
                                                         dataTypes = !!dataTypes,
                                                         .by = !!.by, drop = FALSE)))
    subsets[ , setdiff(names(subsets), groupFields), with = FALSE]
  })

  if (any(!sapply(subsets, is.logical))) .stop('In call to subset.humdrumR/filter.humdrumR,', 
                                               'the subsetting expression(s) must evaluate to logical (TRUE/FALSE) vectors.')
  
  subset <- Reduce('&', subsets)
  subset[is.na(subset)] <- FALSE
  # NA values are results of NA values in the evaluated expression, 
  # and should be treated as FALSE, same as to base::subset() and dplyr::filter()
  
  humtab <- getHumtab(x)
  humtab <- nullify(humtab, dataFields(x), subset, dataTypes)
  
  putHumtab(x) <- humtab 
  x <- updateFields(x, selectNew = FALSE)
  x <- update_Dd(x, selectedFields(x))
  
  if (removeEmptyPieces) x <- removeNull(x, 'Piece', 'GLIMd')
  x
  
  
}



#' @rdname subset.humdrumR
#' @aliases filter
#' @export
filter.humdrumR <- function(.data, ..., dataTypes = 'D', .by = NULL, removeEmptyPieces = TRUE) {
  exprs <- rlang::enquos(...)
  .data <- uncontextMessage(.data, 'filter')
  
  rlang::eval_tidy(rlang::quo(subset.humdrumR(.data, !!!exprs, 
                                              dataTypes = !!dataTypes, 
                                              .by = !!.by, 
                                              removeEmptyPieces = !!removeEmptyPieces))) 
}


nullify <- function(humtab, fields, subset, dataTypes) {
  if (all(subset) || length(fields) == 0L) return(humtab)
  
  null <- logical(nrow(humtab))
  targets <- humtab$Type %in% dataTypes
  null[targets] <- !subset
  
  complementFields <-  paste0('_complement_', fields)
  
  newFields <- Map(fields, 
                   complementFields, f = \(fieldName, complementName) {
                              field <- humtab[[fieldName]]
                              complementField <- if (complementName %in% colnames(humtab)) humtab[[complementName]] else field
                              
                              subset <- complement <- field[0][1:length(field)] # vector of NAs of right class
                              
                              # This is not backwards!
                              subset[!targets | !null]    <- field[!targets | !null] 
                              complement[!targets | null] <- complementField[!targets |  null]
                              
                              setNames(data.table(subset, complement), c(fieldName, complementName))
                            })
  
  complementFields
  humtab <- humtab[ , !colnames(humtab) %in% c(fields, complementFields), with = FALSE]
  
  for (j in seq_along(newFields)) humtab <- cbind(humtab, newFields[[j]])
 
  humtab
}

## Null indexing ----

# humdrumR filtering and application can result in lots of filtered tokens.
# These functions remove parts that are entirely filtered

removeNull <- function(hum, by, nulltypes, ...) {
  UseMethod("removeNull")
}
removeNull.humdrumR <- function(hum, by = 'Piece', nullTypes = 'd', ...) {
  nullTypes <- checkTypes(nullTypes, 'removeNull', 'nullTypes')
  
  humtab <- getHumtab(hum)
  putHumtab(hum) <- removeNull.data.table(humtab, by = by, nullTypes = nullTypes)
  updateFields(hum) # in case any complements have been deleted
 
}
removeNull.data.table <- function(hum, by = 'Piece', nullTypes = 'GLIMd', ...) {
  nullTypes <- checkTypes(nullTypes, 'removeNull', 'nullTypes')
  
  targets <- hum[ , by, with = FALSE]
  targets <- unique(targets[!hum$Type %in% nullTypes])
  
  hum <- hum[targets, on = by]
  
  if ('Piece' %in% by) hum <- renumberFiles.data.table(hum)
  if ('Spine' %in% by) hum <- renumberSpines(hum)
  
  if (any(grepl('^_complement_', colnames(hum)))) {
    complements <- grep('^_complement_', colnames(hum), value = TRUE)
    for (comp in complements) {
      
      sub <- gsub('^_complement_', '', comp)
      if (all(is.na(hum[[comp]]) |
              (!is.na(hum[[sub]]) & hum[[comp]] == hum[[sub]]),
              na.rm = TRUE)) hum[[comp]] <- NULL
    }
  }
  
  hum
  
}



#' @export
#' @rdname subset.humdrumR
removeEmptyFiles <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x, 'File', 'GLIMd')
}
#' @export
#' @rdname subset.humdrumR
removeEmptyPieces <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x, 'Piece', 'GLIMd')
}
#' @export
#' @rdname subset.humdrumR
removeEmptySpines <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x,  c('Piece', 'Spine'), 'LIMd')
}

#' @export
#' @rdname subset.humdrumR
removeEmptyPaths <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x,  c('Piece', 'Spine', 'Path'), 'LIMd')
}
#' @export
#' @rdname subset.humdrumR
removeEmptyRecords <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x, c('Piece', 'Record'), 'd')
}

#' @export
#' @rdname subset.humdrumR
removeEmptyStops <- function(x) {
  checks(x, xclass('humdrumR'))
  removeNull(x, c('Piece', 'Stop'), 'd')
}


# Unfiltering humdrumR ----

#' Unfilter data
#' 
#' Filtering with `subset()`/`filter()` is (by default) not destructive, 
#' allowing you to recover the filtered data
#' using `removeSubset()` or `unfilter()` (which are also synonyms).
#'
#' @section Complements (unfiltering):
#' 
#' When `subset()` is applied, `humdrumR` stores the complement of the subset of each data
#' field is retained (unless an explicit `removeEmpty...()` function is called).
#' The `removeSubset()` or `unfilter()` functions can be used to restore the original data,
#' by combining the subset with the complement.
#' The `fields` argument can be used to control which data fields are unfiltered---by default,
#' all data fields are unfiltered.
#' 
#' Normally, each data field is restored with its own complement data.
#' However, the `complement` argument can be used to specify an field to use as the complement.
#' This allows you to, for instance, different parts of separate fields into a single field.
#'
#' The `complement()` function will directly swap the data-field subsets with their complements.
#' 
#' 
#' @param fields **Which fields to unfilter or complement?**
#' 
#' Defaults to all data fields in the `humdrumR` data.
#'
#' Must be `character` strings, partially matching data [field][fields()] in the input data.
#' 
#' @param complement **Which field to use as the subset complement to restore?**
#' 
#' By default `NULL`, which means each data field's original complement is used.
#'
#' Must be a single `character` string, partially matching a [field][fields()] in the input data.
#' 
#' @export
#' @rdname subset.humdrumR
removeSubset <- function(humdrumR, fields = dataFields(humdrumR), complement = NULL) {
  checks(humdrumR, xhumdrumR)
  checks(complement, xnull | (xcharnotempty & xlen1))
  fields <- fieldMatch(humdrumR, fields, 'removeSubset', 'fields', fieldTypes = 'Data')
  
  
  humtab <- data.table::copy(getHumtab(humdrumR))
  fields <- fields(humdrumR)[Name %in% fields]
  
  if (is.null(complement)) {
    fields <- fields[Complement == TRUE]
    fields[ , complementField := paste0('_complement_', Name)]
    
  } else {
    complement <- fieldMatch(humdrumR, complement, 'removeSubset', 'complement', fieldTypes = 'Data')
    fields[ , complementField := paste0('_complement_', complement)]
  }
  
  fields[ , {
    
    field <- humtab[[Name]]
    complement <- humtab[[complementField]]
    
    null <- is.na(field)
    field[null] <- complement[null]
    humtab[ , (Name) := field]
    
  }, 
  by = seq_len(nrow(fields))]

  humtab[ , (unique(fields$complementField)) := NULL]
  
  humdrumR@Fields[Name %in% fields$Name, Complement := FALSE]
  
  putHumtab(humdrumR) <- humtab
  humdrumR <- update_Dd(humdrumR, fields$Name)
  
  humdrumR
  
}

#' @export
#' @rdname subset.humdrumR
unfilter <- removeSubset


## Complement ----

#' @export
#' @rdname subset.humdrumR
complement <- function(humdrumR, fields = dataFields(humdrumR)) {
  checks(humdrumR, xhumdrumR)
  fields <- fieldMatch(humdrumR, fields, 'complement', 'fields', fieldTypes = 'Data')
  
  fields <- fields(humdrumR)[Name %in% fields]
  
  fields <- fields[Complement == TRUE]
  if (nrow(fields) == 0L) {
    .warn("The field<s|> {harvard(fields$Name, 'and')} <have|has> no complement (has not been filterd).",
          "Your data is returned unchanged.", ifelse = nrow(fields) > 1L)
    return(humdrumR)
  }
  
  
  humtab <- getHumtab(humdrumR)
  fields[ , {
    
    field <- humtab[[Name]]
    complementField <- paste0('_complement_', Name)
    humtab[ , (Name) := humtab[[complementField]]]
    humtab[ , (complementField) := field]
    
    
  }, by = seq_len(nrow(fields))]
  
  humdrumR@Humtable <- humtab
  
  humdrumR <- update_Dd(humdrumR, fields$Name)
  
  humdrumR
    
}

#' Combine one or more fields into a new field
#' 
#' @seealso {You can do similar things using [subset.humdrumR() and complement()][subset.humdrumR]. }
#' @export
combineFields <- function(humdrumR, ...) {
  fieldLists <- list(...)
  newFields <- .names(fieldLists)
  
  if (any(newFields == '')) .stop("You must give a name for the newly combined fields.")
  
  humtab <- getHumtab(humdrumR)
  
  for (newField in newFields) {
    fields <- humtab[ , fieldLists[[newField]], with = FALSE]
    humtab[[newField]] <- Reduce(\(cur, nex) ifelse(is.na(cur), nex, cur), fields)
  }
  
  putHumtab(humdrumR) <- humtab
  
  humdrumR <- updateFields(humdrumR)
  update_Dd(humdrumR, newFields)
}



# Indexing humdrumR ----

#' Indexing humdrumR objects
#'
#' R's built-in indexing operators, `[]` (single brakcets) and `[[]]` (double brackets) can
#' be used to filter [humdrumR data][humdrumR::humdrumRclass], by removing specific
#' pieces, spines, or records from the [humdrum table][humTable].
#' Unlike the more flexible/powerful [subset()/filter()][subset.humdrumR()] methods,
#' the indexing operators are generally destructive (by default), meaning filtered data can no longer
#' be accessed after indexing.
#' The functions `index()` and `index2()` are synonyms for single and double brackets respectively, 
#' which can be used in pipes.
#' 
#' 
#' @details 
#'  
#' In R, the fundamental [indexing operators][base::Extract], `[]` and `[[]]`,
#' are used to select subsets of data.
#' For many data types (for instance, base R [lists][base::list])
#' the **`[`single brackets`]`** are used for "shallower" extraction while the 
#' **`[[`double brackets`]]`** are used for "deeper" extraction.
#' By rough analogy with this "shallow vs deep" dichotomy, [HumdrumR corpus][humdrumR::humdrumRclass] 
#' indexing brackets are used in two ways:
#' 
#' + **`[`single brackets`]`** are used to select *pieces* in your data.
#' + **`[[`double brackets`]]`** are used to select records or spines *within the pieces* in your data.
#' 
#' (Accidentally writing `[]` when you need
#' `[[]]` is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, `humdrumR` objects can use
#' two types of indexing arguments: `numeric` (ordinal integers) or `character` string 
#' (interpreted as regular expressions).
#' 
#' 
#' 
#' ### Numeric indexing:
#' 
#' 
#' Indexing `humdrumR` corpora with
#' **`[`single brackets`]`** will accept
#' one numeric argument---only whole numbers are accepted.
#' This argument will be used to pick pieces within the `humdrumR` object ordinally.
#' Thus, `humData[1:10]` will select the first ten pieces in the data while `humData[42]`
#' will select only the 42nd piece. 
#' 
#' Indexing `humdrumR` objects with
#'  **`[[`double brackets`]]`** will accept 
#' one or two numeric arguments, `i` and `j`, either of which can 
#' be used in isolation or in combination.
#' (If `j` is used in isolation, it must be named or placed after a comma, as in `humData[[ , j ]]`.)
#' 
#' + `i` is used to index records (i.e., based on the humtable `Record` field).
#'   Thus, `humData[[1:20]]` indexes the first twenty records *from each piece*
#'   in the corpus, and `humData[[42]]` extracts the 42nd record *from each piece*.
#'
#'   To avoid breaking the humdrum syntax, exclusive interpretations and spine-path 
#'   interpretations are not removed.
#' + `j` is used to index spines  (i.e., based on the `Spine` field).
#'   Thus, `humData[[ , 3:4]]` returns the third and fourth spines *from each*
#'   piece in the corpus.
#' 
#' 
#' Pieces/spines/records are renumbered after indexing 
#' (see the **Renumbering** section of the [subset()/filter() docs][subset.humdrumR()] for explantion).
#' As a result, `humdrumR` indexing is entirely **ordinal**.
#' For example,
#' 
#' ```
#' humsubset <- humData[11:20]
#' humsubset[2]
#' ````
#' 
#' will return the 12th piece from the original `humData` object.
#' This is because the first call to `[]` returns the 11th through 20th pieces, which
#' are renumbered `1:10` and the second index call returns the *new* 2nd index, which was the 12th
#' originally.
#' Similarly,
#' 
#' ```
#' humsubset2 <- humData[[ , 2:4]]
#' humsubset2[[ , 2]]
#' ```
#
#' will return the third spine from the original data.
#' 
#' 
#' #### Negative numbers 
#' 
#' As in normal `R` indexing, negative numbers can be used, causing corresponding elements to be
#' *removed* instead of retained. Thus, `humData[-3:-5]` will remove the third, fourth, and fifth pieces from the data
#' while `humData[[ , -3:-5]]` will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
#' 
#' #### Out of bounds indices
#' 
#' In all cases, indices outside of bounds (or of value `0`) are ignored.
#' E.g., if you have a corpus of twenty pieces and you call `corpus[21]`, there is no 21st piece, so `21` is "out of bounds".
#' If all your input indices are `0` and error will result.
#' If *all* your input indices are out of bounds then 
#' an empty `humdrumR` object is returned.
#' For instance, `humData[[401:500, ]]` will return an empty
#' `humdrumR` object if there are no pieces with more than 400
#' data records.
#' 
#' 
#' ### Character indexing:
#' 
#' If you index a [humdrumR object][humdrumR:humdrumRclass]
#' with `character` strings, these strings are 
#' treated as [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) (regexes),
#' which are matched against non-null data tokens (`"D"`) in the object's first [selected field][selectedFields].
#' A match to **any** of the regular expressions considered a match.
#' 
#' Indexing with `[`single brackets`]` accepts one 
#' vector of `character` regular expressions.
#' Any piece that contains even a single match will be retained.
#' If no matches occur in any pieces, an empty `humdrumR` object is returned.
#' 
#' Indexing `humdrumR` objects with `[[`double brackets`]]` 
#' accepts one or two vectors of `character` strings, `i` and `j`, 
#' either of which can be used in isolation or in combination. 
#' (If `j` is used in isolation, it must be placed after a comma, 
#' as in `humData[[ , j]]`.)
#' Any data record which contains at least one match to the `i` regex(es)
#' will be retained.
#' Similarly, any spine which contains at least one match to the
#' `j` regex(es) is retained.
#' If `i` and `j` are used together,
#' matching spines (`j`) are indexed first, so that 
#' tokens matching the regular expression(s) in `i`
#' must be found in the matching spines.
#' 
#' ### Exclusive indexing:
#' 
#' Spines can also be indexed ordinally by exclusive interpretation.
#' To do this, provide a double-bracket index with a *named* numeric (whole number) argument,
#' with name(s) corresponding to exclusive interpretations in the data.
#' For example, if you want to index the 3rd `**kern` spine in each piece,
#' use `humData[[kern = 3]]`.
#' Note that *other* exclusive interpretations in each piece are unaffected---in 
#' this example, only the kern spines (if there are any) are indexed!
#' 
#' 
#' @section drop:
#' 
#' The `drop` argument to any humdrumR indexing controls whether
#' filtered data is completely removed from the data, or simply set to null 
#' This means the filtered data can be recovered using [unfilter()] (see the [subset()/filter()][subset.humdrumR()]
#' docs for an explanation).
#' By default, piece-indexing and spine-indexing have `drop = TRUE`,
#' but record-indexing defaults to `drop = FALSE`.
#' 
#' 
#' 
#' @param x ***HumdrumR data to index.***
#'
#' Must be a [humdrumR data object][humdrumRclass].
#'
#' @param i ***Index for vectors or matrix/data.frame rows.***
#'
#' A numeric vector or a `character` string treated as a regular expression.
#'
#' @param j ***Index for matrix/data.frame columns.***
#'
#' A numeric vector or a `character` string treated as a regular expression.
#'
#' @param drop ***Should empty records/spines/pieces be removed?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/RollingStoneCorpus/*.hum")
#' 
#' humData[1:2]
#' humData[-1]
#' 
#' humData[[ , 3:4]]
#' humData[[1:40 , ]]
#' 
#' # find all pieces which use a flat 3
#' humData['b3']
#' 
#' # find all records that use a flat 3
#' humData[['b3', ]]
#' humData[['b3', drop = TRUE]]
#' 
#' # Exclusive interpretation indexing
#' humData[[deg = 1]]
#'
#' # pipe indexing
#' humData |> index(1:3) |> index2(3:4)

#' 
#' @seealso {For more powerful/flexible indexing options, use [subset()/filter()][subset.humdrumR].}
#' @name indexHumdrum
NULL





numericIndexCheck <- function(i, argname = 'i') {
    
    
  
    checks(i, xwholenum & xposORneg, argname, seealso = '?indexHumdrum')
    # if (any(i < 0) && any(i > 0)) .stop("You can't mix negative and positive numbers when trying to index humdrumR objects.")
    if (all(i == 0)) .stop("You can't index humdrumR objects with just zeros.")
    
    if (any(i == 0)) {
        .warn("Your indexing of a humdrumR object is mixing zeros in with non-zero numbers. These zeros are simply ignored.")
        i <- i[i != 0]
    }
    
    if (any(duplicated(i))) {
        .warn("When indexing a humdrumR object with numeric values, duplicates are ignored.")
        i <- i[!duplicated(i)]
    }
    
    i
}


## Single brackets [] ----


#' @rdname indexHumdrum
#' @usage humData[] # returns unchanged
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing'),
          definition = force)

### numeric ----

#' @rdname indexHumdrum
#' @usage humData[x:y]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i, drop = TRUE) {
              i <- numericIndexCheck(i)
              
              if (drop) {
                humtab <- getHumtab(x)
                
                targets <- humtab[ , sort(unique(Piece))[i]]
                humtab <- humtab[Piece %in% targets]
               
                
                putHumtab(x) <- humtab
              } else {
                x <- subset(x, Piece %in% sort(unique(Piece))[!!i])
              }
             
              
              x

          })



### character ----

#' @rdname indexHumdrum
#' @usage humData['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i, drop = TRUE) {
            x <- subset(x, any(. %~l% !!i), .by = 'Piece')
            
            if (drop) x <- removeEmptyPieces(x)
            
            x
          })




## Double brackets [[]] ----
    

### numeric ----

#' @rdname indexHumdrum
#' @usage humData[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i, j, drop = TRUE) {
            
            i <- numericIndexCheck(i)    
            if (drop) {
              humtab <- getHumtab(x)
              
              humtab <- if (all(i > 0L)) {
                humtab[Record %in% i | Type %in% c('E', 'S')]
              } else {
                humtab[!(Record %in% abs(i)) | Type %in% c('E', 'S')]
              }
              
              putHumtab(x) <- humtab
             
            } else {
              x <- if (all(i > 0L)) {
                subset(x, Record %in% (!!i) | Type %in% c('E', 'S'), dataTypes = 'GLIMDd')
              } else {
                subset(x, !(Record %in% abs(!!i)) | Type %in% c('E', 'S'), dataTypes = 'GLIMDd')
              }
          
            }
            
            removeEmptyPieces(x)

          })


#' @rdname indexHumdrum
#' @usage humData[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j, drop = TRUE) {
              j <- numericIndexCheck(j, 'j')    
              
              if (drop) {
                humtab <- getHumtab(x)
                humtab <- humtab[is.na(Spine) | Spine %in% sort(unique(Spine))[j]]
                # sort(unique(Spine))[j] is needed so that negative indices are used!
                
                putHumtab(x) <- renumberSpines.data.table(humtab)
              } else {
                
                
                x <- subset(x, Spine %in% sort(unique(Spine))[!!j] | is.na(Spine), dataTypes = "D")
              }
              
              removeEmptyPieces(x)
              
              
          })



### character ----



#' @rdname indexHumdrum
#' @usage humData[['regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
function(x, i, drop = FALSE) {
    # gets any record which contains match
  
    x <- subset(x, Record %in% unique(Record[. %~l% !!i]), .by = 'Piece', dataTypes = "D")
    
    if (drop) x <- removeEmptyRecords(x)
    
    removeEmptyPieces(x)
})


#' @rdname indexHumdrum
#' @usage humData[[ , 'regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j, drop = TRUE) {
            #gets any spine which contains match
            
            exclusive <- all(grepl('^\\*\\*', j))
            expr <- quote(Spine %in% unique(Spine[. %~l% j]) | is.na(Spine))
            
            if (exclusive) {
              j <- gsub('^\\*\\**', '', j)
              expr <- substituteName(expr, list(. = quote(Exclusive)))
            }
            
            if (drop && exclusive) {
              humtab <- getHumtab(x)
              hits <- rlang::eval_tidy(rlang::expr(humtab[ , !!expr, by = Piece]))$V1
              humtab <- humtab[hits == TRUE]
              putHumtab(x) <- renumberSpines.data.table(humtab)
              
            } else {
              x <- rlang::eval_tidy(rlang::expr(subset(x, !!expr, .by = 'Piece', dataTypes = 'D')))
              if (drop) x <- removeEmptySpines(x)
            }
            removeEmptyPieces(x)

          })









#' @rdname indexHumdrum
#' @usage humData[[x:y, l:m]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'ANY', j = 'ANY'), 
          function(x, i, j, drop = FALSE) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            
            if (drop) x <- removeEmptyRecords(removeEmptySpines(x))
            x
          })


#   


#### Exclusive interpretation indexing ----

#' @rdname indexHumdrum
#' @usage humData[[ , , regex]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'missing'), 
          function(x, i, j, ..., drop = TRUE) {
           
            ldots <- list(...)
            names(ldots) <- gsub('^\\**', '', names(ldots))
            exclusives <- intersect(names(ldots), unique(x$Exclusive))
            
            ldots <- ldots[names(ldots) %in% exclusives]
            ldots <- Map(numericIndexCheck, ldots, names(ldots))
            
            if (length(exclusives)) {
              x <- subset(x, .by = 'File', {
                keepSpines <- unique(Spine)
                for (exclusive in exclusives) {
                  excSpines <- sort(unique(Spine[Exclusive == exclusive]))
                  keepSpines <- setdiff(keepSpines, setdiff(excSpines, excSpines[ldots[[exclusive]]]))
                }
                Spine %in% keepSpines
              })
              if (drop) x <- removeEmptySpines(x)
            }
            
            
           
            x
          })



## Indexing in pipes ----


#' @rdname indexHumdrum
#' @export
index <- function(x, i, j, drop = TRUE) {
  
  i <- rlang::enexpr(i)
  j <- rlang::enexpr(j)
  
  if (missing(i) && missing(j)) return(x)
  
  expr <- rlang::expr(x[])
  if (!missing(i)) expr[[3]] <- i
  if (!missing(j)) expr[[4]] <- j
  if (!is.null(dim)) expr$drop <- drop
  if (inherits(x, 'data.table') && !missing(j)) x$with = FALSE
  
  
  rlang::eval_tidy(expr)
  # 
  # pat <- paste0(missing(i), missing(j))
  # 
  # switch(pat,
  #        'TRUETRUE' = x,
  #        'TRUEFALSE' = x[  , j, drop = drop],
  #        'FALSETRUE' = if (length(dim(x)) > 1L) x[i ,  , drop = drop] else x[i],
  #        'FALSEFALSE' = x[i, j, drop = drop])
}

#' @rdname indexHumdrum
#' @export
index2 <- function(x, i, j, drop = TRUE) {
  
  pat <- paste0(missing(i), missing(j))
  
  if (!is.humdrumR(x)) return(x[[i]])
  switch(pat,
         'TRUETRUE' = x,
         'TRUEFALSE' = x[[  , j, drop]],
         'FALSETRUE' = x[[i ,  , drop]],
         'FALSEFALSE' = x[[i, j, drop]])
}

