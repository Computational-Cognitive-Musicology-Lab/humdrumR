
# Filtering humdrumR----


#' Filter humdrum data
#' 
#' `subset.humdrumR` is a command used to filter a [humdrumR corpus][humdrumR::humdrumRclass].
#' The standard indexing operators (`[]` and `[[]]`) actually work by calling `subset` under-the-hood;
#' you can read about these indexing options [here][indexHumdrum].
#' However, using `subset` directly can accomplish much more sophisticated filtering commands than the indexing
#' methods.
#' 
#' 
#' `subset.humdrumR` is used in a similar manner to [withinHumdrum],
#' taking any number of "within expressions" as arguments.
#' In fact, expression arguments are passed directly to an internal call to `withinHumdrum`, and 
#' other control expressions (like `by` or `subset`) can be used as well.
#' The only requirement is that the expressions/functions fed to `subset.humdrumR` 
#' *must* be [predicate](https://en.wikipedia.org/wiki/Predicate_(mathematical_logic)) expressions 
#' which return a logical (`TRUE`/`FALSE`) vector.
#' The returned vector must also be the same length as the input data (the number
#' of rows in the [humdrum table][humdrumR::humTable]).
#' (You can use a `fill` expression if you want to "expand" shorter outputs for filtering purposes.)
#' 
#' @section Filter field:
#' 
#' When using `subset`, `humdrumR` doesn't necessarily delete the data you filter out.
#' Rather, there is a `logical` field in the [humdrum table][humTable] called `Filter`.
#' `subset.humdrumR` updates the humdrum table's `Filter` field using an logical OR (`|`) between the 
#' existing `Filter` field and the negation of your predicate: `Filter | !Predicate`.
#' HumdrumR functions (mostly) ignore all data points where `Filter == TRUE`, treating them like `NULL` data.
#' For example, when you print a 
#' filtered `humdrumR` you'll see all the filtered data points turned to null data (`.`), and
#' any calls to [withinHumdrum][with(in)Humdrum] will ignore the filtered data.
#' This means that you can, recover the filtered data by calling `clearFilter` on your dataset.
#' 
#' In some cases you might filter out large parts of your data, which will leave a bunch of empty null
#' data points (`"."`).
#' If you *want* to remove these filtered data points, you can call `removeEmptyFiles`, `removeEmptySpines`, 
#' `removeEmptyPaths`, `removeEmptyRecords`, or `removeEmptyStops`.
#' These functions go through each piece/spine/path/record and check if *all* the data in that region
#' is null or filtered (i.e., `Null == TRUE | Filter == TRUE`); if so, that data will be removed.
#' You can only remove the data if *all* of it is null (within a region) because otherwise the humdrum syntax is broken.
#' 
#' By default, `subset.humdrumR` automatically calls `removeEmptyFiles` at the end.
#' However, you can stop this by specifying  `removeEmptyFiles = FALSE`.
#' 
#' @section Renumbering:
#'
#' If filtered files are removed from a corpus (using `removeEmptyFiles` or `removeEmptySpines`, in combination with `subset`)
#' the `File` and/or `Spine` fields are renumbered to represented the remaining regions,
#' starting from `1`.
#' For example, if you have a corpus of 10 files and remove the first file (`File == 1`),
#' the remaining files are renumbered from `2:10` to `1:9`.
#' Spine renumbering works the same, except it is done independently *within* each file.
#' 
#' @param x A [humdrumR data object][humdrumRclass].
#' @param humdrumR A [humdrumR data object][humdrumRclass].
#' @param ... Arbitrary expressions passed to [with(in)][withinHumdrum]---the "within" expression(s) must evaluate to
#'   full-length `logical` values.
#' 
#' @seealso {The [indexing operators][indexHumdrum] `[]` and `[[]]` can be used as shortcuts for common `subset` calls.}
#' @export
#' @aliases subset
subset.humdrumR <- function(x, ...) {
  
  
  oldActive <- getActive(x)
  oldActiveFields <- getActiveFields(x)
  x <- within.humdrumR(x, ...)
  resultFields(x) <- '.TmpFilter.'
  
  humtab <- getHumtab(x)
  if (humtab[ , class(.TmpFilter.)] != 'logical') .stop('In call to subset.humdrumR, the within-expression must evaluate to a logical vector.')
 
  humtab[ , .TmpFilter. := .TmpFilter. | is.na(.TmpFilter.)] 
  # NA values come in from record types we didn't use, which should NOT be filtered
  humtab[ , Filter := Filter | !.TmpFilter.]
  humtab[ , .TmpFilter. := NULL]
  
  humtab <- update_Null.data.table(humtab, oldActiveFields)
  putHumtab(x) <- removeNull(humtab, 'File', 'GLIMd')
  
  removeFields(x) <- '.TmpFilter.'
  x@Active <- oldActive
  
  
  x
  
}

#' @export
#' @rdname subset.humdrumR
clearFilter <- function(humdrumR) {
  humtab <-getHumtab(humdrumR)
  
  humtab[ , Filter := FALSE]
  
  putHumtab(humdrumR) <- update_Null.data.table(humtab)
  
  humdrumR
  
}

## Null indexing ----

# humdrumR filtering and application can result in lots of filtered tokens.
# These functions remove parts that are entirely filtered

removeNull <- function(hum, by, nulltypes, ...) {
  UseMethod("removeNull")
}
removeNull.humdrumR <- function(hum, by = 'File', nullTypes = 'd', ...) {
  nullTypes <- checkTypes(nullTypes, 'removeNull', 'nullTypes')
  
  humtab <- getHumtab(hum)
  putHumtab(hum) <- removeNull.data.table(humtab, by = by, nullTypes = nullTypes)
  
  hum
 
}
removeNull.data.table <- function(hum, by = 'File', nullTypes = 'GLIMd', ...) {
  nullTypes <- checkTypes(nullTypes, 'removeNull', 'nullTypes')
  
  targets <- hum[ , by, with = FALSE]
  targets <- unique(targets[!hum$Type %in% nullTypes])
  
  hum <- hum[targets, on = by]
  
  if ('File' %in% by) hum <- renumberFiles.data.table(hum)
  if ('Spine' %in% by) hum <- renumberSpines(hum)
  hum
}



#' @export
#' @rdname subset.humdrumR
removeEmptyFiles <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyFiles')
  removeNull(humdrumR, 'File', 'GLIMd')
}
#' @export
#' @rdname subset.humdrumR
removeEmptySpines <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptySpines')
  removeNull(humdrumR,  c('File', 'Spine'), 'LIMd')
}

#' @export
#' @rdname subset.humdrumR
removeEmptyPaths <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyPaths')
  removeNull(humdrumR,  c('File', 'Spine', 'Path'), 'LIMd')
}
#' @export
#' @rdname subset.humdrumR
removeEmptyRecords <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyRecords')
  removeNull(humdrumR, c('File', 'Record'), 'd')
}

#' @export
#' @rdname subset.humdrumR
removeEmptyStops <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyStops')
  removeNull(humdrumR, c('File', 'Stop'), 'd')
}


# Indexing ----

#' Indexing humdrumR objects
#'
#' R's built-in indexing operators, `[]` (single brakcets) and `[[]]` (double brackets) can
#' be used as shortcuts for common calls to [subset.humdrumR()],
#' allowing you to filter out specific files, spines, or records.
#' 
#' @details 
#'  
#' In `R`, the basic [indexing operators][base::Extract], `[]` and `[[]]`,
#' are used to select subsets of data.
#' For many data types (for instance, base R [lists][base::list])
#' the **`[`single brackets`]`** are used for "shallower" extraction while the 
#' **`[[`double brackets`]]`** are used for "deeper" extraction.
#' [HumdrumR corpus][humdrumR::humdrumRclass] indexing follows this same basic pattern:
#' **`[`single brackets`]`** are used to index `humdrumR` objects
#' *by piece* while **`[[`double brackets`]]`** are used to index
#' *within pieces*. (Accidentally writing `[]` when you need
#' `[[]]` is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, `humdrumR` objects can use
#' two types of indexing arguments:
#' 
#' + By `numeric` (ordinal integers)
#' + By `character` string (regular expressions)
#' 
#' For more powerful/flexible indexing options, use [subset][subset.humdrumR()] directly.
#' 
#' 
#' ### Numeric indexing:
#' 
#' 
#' Indexing `humdrumR` corpora with
#' **`[`single brackets`]`** will accept
#' one numeric argument.
#' This argument will be used to pick pieces within the `humdrumR` object ordinally.
#' Thus, `humdata[1:10]` will select the first ten pieces in the data while `humdata[42]`
#' will select only the 42nd piece. 
#' 
#' Indexing `humdrumR` objects with
#'  **`[[`double brackets`]]`** will accept 
#' one or two numeric arguments, `i` and `j`, either of which can 
#' be used in isolation or in combination.
#' (If `j` is used in isolation, it must be named or placed after a comma, as in `humdata[[ , j ]]`.)
#' 
#' + `i` is used to index data records (i.e., based on the humtable `Record` field).
#'   Thus, `humdata[[1:20]]` indexes the first twenty records *from each file*
#'   in the corpus, and `humdata[[42]]` extracts the 42nd record *from each file*.
#' + `j` is used to index spines  (i.e., based on the `Spine` field).
#'   Thus, `humdata[[ , 3:4]]` returns the third and fourth spines *from each*
#'   file in the corpus.
#' 
#' When indexing [humdrumR corpora][humdrumR::humdrumRclass] with numbers,
#' all `numeric` (double) inputs are converted to integers.
#' 
#' Since [subset][subset.humdrumR] always renumbers files/spines that remain after filtering/indexing,
#' `humdrumR` indexing is entirely **ordinal**.
#' For example,
#' 
#' ```
#' humsubset <- humdata[11:20]
#' humsubset[2]
#' ````
#' 
#' will return the 12th piece from the original `humdata` object.
#' This is because the first call to `[]` returns the 11th through 20th pieces, which
#' are renumbered `1:10` and the second index call returns the *new* 2nd index, which was the 12th
#' originally.
#' Similarly,
#' 
#' ```
#' humsubset2 <- humdata[[ , 2:4]]
#' humsubset2[[ , 2]]
#' ```
#
#' will return the third spine from the original data.
#' 
#' As in normal `R` indexing, negative numbers can be used, causing corresponding elements to be
#' *removed* instead of retained. Thus, `humdata[-3:-5]` will remove the third, fourth, and fifth pieces from the data
#' while `humdata[[ , -3:-5]]` will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
#' 
#' 
#' 
#' In all cases, indices outside of range (or of value `0`) are ignored.
#' E.g., if you have a corpus of twenty files and you call `corpus[21]`, there is no 21st piece, so `21` is "out of range".
#' If all your input indices are `0` and error will result.
#' If all your input indices are out of range then 
#' an empty `humdrumR` object is returned.
#' For instance, `humdata[[401:500, ]]` will return an empty
#' `humdrumR` object if there are no pieces with more than 400
#' data records.
#' 
#' 
#' ### Character indexing:
#' 
#' Indexing [humdrumR objects][humdrumR:humdrumRclass] with 
#' `[`single brackets`]` will accept one 
#' vector of `character` strings. These strings are 
#' treated as 
#' [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) (regexes).
#' 
#' The tokens from the humdrumR object's [active field(s)][humActive] are searched
#' for matches to any of the regular expressions you input. Any piece that contains
#' **any** match to **any** of the regular expressions is retained---all other pieces
#' are filtered out. Note that (because this is `[`single-bracket`]` indexing) the entire piece is 
#' retained, even if there is only one match.
#' If no matches occur in any pieces, an empty `humdrumR` object is returned.
#' 
#' Indexing `humdrumR` objects with `[[`double brackets`]]` will 
#' accept one or two vectors of `character` strings, `i` and `j`, 
#' either of which can 
#' be used in isolation or in combination. 
#' (If `j` is used in isolation, it must be placed after a comma, 
#' as in `humdata[[ , j]]`.)
#' These strings are 
#' treated as [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) (regexes).
#' The tokens from the humdrumR object's [active field(s)][humActive] are searched
#' for matches to any of the regular expressions you input.
#' Any record which contains at least one token matching any regex in `i`
#' will be retained.
#' Similarly, any spine which contains at least one token matching any
#' regex in `j` is retained.
#' If `i` and `j` are used together,
#' matching spines (`j`) are indexed first, so that 
#' tokens matching the regular expression(s) in `i`
#' must be found in the matching spines.
#' 
#' @section removeEmpty:
#' 
#' By default, calls to indexing operators will completely remove
#' data which you are filtering out.
#' However, if you set the `removeEmpty` argument to `FALSE`,
#' the filtered data is set to `NULL`, but not actually removed from the data object.
#' (See [subset.humdrumR()] for more details.)
#' 
#' @param x A [humdrumR data object][humdrumRclass].
#' @param i A `integer`/`numeric` value or a `character` string treated as a regular expression.
#' @param j A `integer`/`numeric` value or a `character` string treated as a regular expression.
#' 
#' @seealso {These indexing operators work through special calls to [subset.humdrumR()]}
#' @name indexHumdrum
NULL




numericIndexCheck <- function(i) {
    if (any(i < 0) && any(i > 0)) stop("You can't mix negative and positive numbers when trying to index humdrumR objects.")
    if (all(i == 0)) stop("You can't index humdrumR objects with just zeros.")
    
    if (any(i == 0)) {
        warning("Your indexing of a humdrumR object is mixing zeros in with non-zero numbers. These zeros are simply ignored.")
        i <- i[i != 0]
    }
    
    if (any(duplicated(i))) {
        warning("When indexing a humdrumR object with numeric values, duplicates are ignored.")
        i <- i[!duplicated(i)]
    }
    
    i
}


## Single brackets [] ----


#' @rdname indexHumdrum
#' @usage humdata[] # returns unchanged
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing'),
          definition = force)

### numeric ----

#' @rdname indexHumdrum
#' @usage humdata[x:y]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i, removeEmpty = TRUE) {
              i <- numericIndexCheck(i)
              
              if (removeEmpty) {
                humtab <- getHumtab(x)
                
                targets <- humtab[ , sort(unique(File))[i]]
                humtab <- humtab[File %in% targets]
               
                
                putHumtab(x) <- humtab
              } else {
                x <- subset(x, File %in% sort(unique(File))[!!i])
              }
             
              
              x

          })



### character ----

#' @rdname indexHumdrum
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i, removeEmpty = TRUE) {
            x <- subset(x, fill = any(. %~l% !!i),  by = File)
            
            if (removeEmpty) x <- removeEmptyFiles(x)
            
            x
          })




## Double brackets [[]] ----
    

### numeric ----

#' @rdname indexHumdrum
#' @usage humdata[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i, j, removeEmpty = TRUE) {
            
            i <- numericIndexCheck(i)    
            if (removeEmpty) {
              humtab <- getHumtab(x)
              
              humtab <- if (all(i > 0L)) {
                humtab[Record %in% i | Token %in% c('*-', '*v', '*^') | grepl('\\*\\*', Token)]
              } else {
                humtab[!(Record %in% abs(i)) | Token %in% c('*-', '*v', '*^') | grepl('\\*\\*', Token)]
              }
              
              putHumtab(x) <- humtab
             
            } else {
              x <- if (all(i > 0L)) {
                subset(x, Record %in% (!!i) | Token %in% c('*-', '*v', '*^') | grepl('\\*\\*', Token), dataTypes = 'GLIMDd')
              } else {
                subset(x, !(Record %in% abs(!!i)) | Token %in% c('*-', '*v', '*^') | grepl('\\*\\*', Token), dataTypes = 'GLIMDd')
              }
          
            }
            
            removeEmptyFiles(x)

          })


#' @rdname indexHumdrum
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j, removeEmpty = TRUE) {
              j <- numericIndexCheck(j)    
              
              if (removeEmpty) {
                humtab <- getHumtab(x)
                humtab <- humtab[is.na(Spine) | Spine %in% sort(unique(Spine))[j]]
                # sort(unique(Spine))[j] is needed so that negative indices are used!
                
                putHumtab(x) <- renumberSpines.data.table(humtab)
              } else {
                
                
                x <- subset(x, Spine %in% sort(unique(Spine))[!!j] | is.na(Spine), dataTypes = "D")
              }
              
              removeEmptyFiles(x)
              
              
          })



### character ----



#' @rdname indexHumdrum
#' @usage humdata[['regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
function(x, i, removeEmpty = FALSE) {
    # gets any record which contains match
  
    x <- subset(x, Record %in% unique(Record[. %~l% !!i]), by = File, dataTypes = "D")
    
    if (removeEmpty) x <- removeEmptyRecords(x)
    
    removeEmptyFiles(x)
})


#' @rdname indexHumdrum
#' @usage humdata[[ , 'regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j, removeEmpty = TRUE) {
            #gets any spine which contains match
            
            if (removeEmpty && all(grepl('^\\*\\*', j))) {
              humtab <- getHumtab(x)
              j <- gsub('^\\*\\**', '', j)
              hits <- humtab[ , Spine %in% unique(Spine[Exclusive %in% j]) | is.na(Spine), by = File]$V1
              humtab <- humtab[hits == TRUE]
              putHumtab(x) <- renumberSpines.data.table(humtab)
              
            } else {
              form <- do ~ Spine %in% unique(Spine[. %~l% j]) | is.na(Spine)
              
              if (all(grepl('^\\*\\*', j))) {
                
                j <- gsub('^\\*\\**', '', j)
                form <- substituteName(form, list(. = quote(Exclusive)))
                
              } 
              
              x <- subset(x, form, by = File, dataTypes = "D")
              
              if (removeEmpty) x <- removeEmptySpines(x)
            }
           
            
            
            removeEmptyFiles(x)

          })









#' @rdname indexHumdrum
#' @usage humdata[[x:y, l:m]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'ANY', j = 'ANY'), 
          function(x, i, j, removeEmpty = FALSE) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            
            if (removeEmpty) x <- removeEmptyRecords(removeEmptySpines(x))
            x
          })


#   



