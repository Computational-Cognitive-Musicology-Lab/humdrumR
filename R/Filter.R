
# Filtering humdrumR----


#' Filter humdrum data
#' 
#' `filterHumdrum` is a command used to filter a [humdrumR corpus][humdrumR::humdrumRclass]
#' Indexing a humdrumR corpus (using the `[]` or `[[]]` operators) 
#' uses calls to `filterHumdrum`!
#' 
#' `filterHumdrum` is used in a similar manner as [withinHumdrum],
#' taking any number of "do expressions" (or functions) as arguments.
#' (In fact, do expressions/function arguments are passed directly to an internal call to `withinHumdrum`.)
#' The only difference is that the expressions/functions fed to `filterHumdrum` 
#' *must* be [predicate](https://en.wikipedia.org/wiki/Predicate_(mathematical_logic)) expressions 
#' which return a logical (`TRUE`/`FALSE`) vector.
#' The returned vector must also be the same length as the input data (the number
#' of rows in the [humdrum table][humdrumR::humTable]).
#' (You can use a `dofill~` expression if you want to "expand" shorter outputs for filtering pusposes.)
#' `filterHumdrum` updates the humdrum table's `Filter` field using an logical OR (`|`) between the existing `Filter` field and the negation of your predicate: `Filter | !Predicate`.
#' HumdrumR functions (mostly) ignore all data points where `Filter == TRUE`: when you print a filtered `humdrumR` you'll see all the filtered data points turned to null data (`.`), and
#' any calls to [withinHumdrum][with(in)Humdrum] will ignore the filtered data.
#' 
#' By default, `filterHumdrum` completely removes any files in the corpus where *all* the data records are filtered out.
#' However, you can stop this by specifying the `removeEmptyFiles` argumet as `FALSE`.
#' If you *want* to remove empty files, spines, or records, you should call `removeEmptyFiles`, `removeEmptySpines`, or `removeEmptyRecords`.
#' 
#' @section Indexing:
#' 
#' In R, the basic [indexing operators][base::Extract], `[]` and `[[]]`,
#' are used to select subsets of data.
#' For many R data types (for instance, base R [lists][base::list])
#' the **`[`single brackets`]`** are used for "shallower" extraction while the 
#' **`[[`double brackets`]]`** are used for "deeper" extraction.
#' [HumdrumR corpus][humdrumR::humdrumRclass] indexing follows this same basic pattern:
#' **`[`single brackets`]`** are used to index `humdrumR` objects
#' *by piece* while **`[[`double brackets`]]`** are used to index
#' *within pieces*. (Accidentally writing `[]` when you need
#' `[[]]` is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, `humdrumR` objects can use
#' three types of indexing arguments:
#' 
#' + By `numeric` (ordinal integers)
#' + By `character` string (regular expressions)
#' + By `formula` (arbitrary expressions)
#' 
#' The last option (by `formula`) is the most powerful option,
#' and indeed, the first two options (`numeric` or `character` indexing)
#' are just convenient shorthands for indexing that can be accomplished using 
#' the `formula` method.
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
#' (If `j` is used in isolation, it must be placed after a comma, as in `humdata[[ , j ]]`.)
#' 
#' + `i` is used to index data records (i.e., based on the humtable `Record` field) ordinally.
#'   Thus, `humdata[[1:20]]` indexes the first twenty records *from each file*
#'   in the corpus, and `humdata[[42]]` extracts the 42nd record *from each file*.
#' + `j` is used to index spines  (i.e., based on the `Spine` field) ordinally.
#'   Thus, `humdata[[ , 3:4]]` returns the third and fourth spines *from each*
#'   file in the corpus.
#' 
#' When indexing [humdrumR corpora][humdrumR::humdrumRclass] with numbers,
#' all `numeric` (double) inputs are converted to integers.
#' 
#' 
#' Note that numeric `humdrumR` indexing is entirely **ordinal**, meaning 
#' that pieces/data records/spines are not matched based on their value in their
#' respective fields, but rather on their order among all existing values.
#' Thus, for `[`single-bracket`]` indexing the $i_{th}$ piece in the
#' corpus is taken, regardless of that `FileN` field associated
#' with that piece.
#' For example,
#' 
#' ```
#' humsubset <- humdata[11:20]
#' humsubset[2]
#' ````
#' 
#' will return the 12th piece from the original `humdata` object, *not* the second piece.
#' This is beacuse the first call to `[]` returns the 11th through 20th pieces, and the second call
#' returns the *second* piece that is still present (the 12th).
#' Similarly,
#' 
#' ```
#' humsubset2 <- humdata[[ , 2:4]]
#' humsubset2[[ , 2]]
#' ```
#' 
#' will return the third spine from the original data.
#' 
#' As in normal R indexing, negative numbers can be used, causing corresponding elements to be
#' *removed* instead of retained. Thus, `humdata[-3:-5]` will remove the third, fourth, and fifth pieces from the data
#' while `humdata[[ , -3:-5]]` will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
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
#' The tokens from the humdrumR object's `Active` fields are searched
#' for matches to any of the regular expressions you input. Any piece that contains
#' **any** match to **any** of the regular expressions is retained---all other pieces
#' are filtered out. Note that (because this is `[`single-bracket`]` indexing) the entire piece is retained, even if there is only one match.
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
#' The tokens from the humdrumR object's `Active` fields are searched
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
#' A third argument, `k`, can also be used, but only if 
#' both the `i` and `j` arguments are missing.
#' In order for this to work, you need to put two commas to mark the "missing" `i` and `j` arguments: 
#' e.g., `humdata[[ , , '[Ee]-']]`.
#' In the case of `k`, only matching tokens are retained,
#' regardless of their spine or record number(s).
#' 
#' 
#' ### Formula indexing:
#' 
#' Indexing [humdrumR objects][humdrumR:humdrumRclass] with 
#' `formulae` is the most powerful, flexible indexing option.
#' Either `[`single`]` or `[[`double`]]` brackets will accept
#' a (single) formula. The formula are fed directly as arguments to 
#' `filterHumdrum`---as such, they music evaluate to a logical vector of the same 
#' length as the input.
#' 
#' In the case of `[`single-bracket`]` indexing, only one `formula`
#' is accepted, and *every piece* that evalues with at least one 
#' `TRUE` will be retained.
#' For instance, `humdata[~Spine > 4]` will return all pieces
#' which contain five (or more) spines.
#' `[`single-bracket`]` formula indexing is especially useful for indexing
#' meta-data properties like reference records:
#' for instance, `humdata[~COM == "Paul McCartney"]` will return
#' all pieces with a `!!!COM: Paul McCartney` reference record.
#' 
#' In the case of `[[`double-bracket`]]` indexing, one or two formulas are accepted, 
#' in arguments `i` and `j`, either of which can 
#' be used in isolation or in combination. 
#' (If `j` is used in isolation, it must be placed after a comma, 
#' as in `humdata[[ , j]]`.)
#' In the case of `i` formulae, any record which evaluates to
#' at least one `TRUE` value is retained.
#' In the case of `j`, any spine which evaluates to
#' at least one `TRUE` value is retained.
#' Any piece which contains no matches is dropped entirely.
#' 
#' For `[[`double-bracket`]]` formula indexing, a third argument, `k`
#' may be used in the absence of `i` and `j`.
#' In order for this to work, you need to put two commas to mark the "missing" `i` and `j` arguments: 
#' e.g., `humdata[[ , , ~formula]]`.
#' In the case of `k` all tokens which evaluate to `TRUE`
#' are retained, regardless of piece/spine/record.
#' Pieces, spines, or records with no `TRUE` values
#' are simply dropped.
#' Using the `k` argument is exactly the same a "plain" call to `filterHumdrum`.
#' 
#' @export
filterHumdrum <- function(humdrumR, ...) { 
    checkhumdrumR(humdrumR, 'filterHumdrum')
    
    
    oldActive <- getActive(humdrumR)
    oldActiveFields <- activeFields(humdrumR)
    
    humdrumR <- within.humdrumR(humdrumR, ...)

    humtab <- getHumtab(humdrumR, 'GLIMDd')
    
    filterResult <- tail(resultFields(humtab), 1L)
    removeFields(humdrumR) <- filterResult
    
    colnames(humtab)[colnames(humtab) == filterResult] <- '__TmpFilter__'
    
    if (humtab[ , class(`__TmpFilter__`)] != 'logical') stop('In call to filterHumdrum, the do-expression must evaluate to a logical vector.')
    
    humtab[ , `__TmpFilter__` := `__TmpFilter__` | is.na(`__TmpFilter__`)]
    humtab[ , Filter := Filter | !`__TmpFilter__`]
    humtab[ , `__TmpFilter__` := NULL]
    
    humtab <- update_Null(humtab, oldActiveFields)
    putHumtab(humdrumR) <- humtab
    
    humdrumR <- setActive(humdrumR, oldActive)

    
    humdrumR
    
}


#' @export
subset.humdrumR <- function(x, ...) {
  
  
  oldActive <- getActive(x)
  oldActiveFields <- activeFields(x)
  
  x$.TmpFilter. <- within.humdrumR(x, ...)
  
  humtab <- getHumtab(x)
  
  if (humtab[ , class(.TmpFilter.)] != 'logical') .stop('In call to subset.humdrumR, the do-expression must evaluate to a logical vector.')
 
  humtab[ , .TmpFilter. := .TmpFilter. | is.na(.TmpFilter.)] 
  # NA values come in from record types we didn't use, which should NOT be filtered
  humtab[ , Filter := Filter | !.TmpFilter.]
  humtab[ , .TmpFilter. := NULL]
  
  humtab <- update_Null.data.table(humtab, oldActiveFields)
  putHumtab(x) <- humtab
  
  removeFields(x) <- '.TmpFilter.'
  x <- setActive(x, oldActive)
  
  x
  
}

## Null indexing ----

# humdrumR filtering and application can result in lots of filtered tokens.
# These functions remove parts that are entirely filtered

removeNull <- function(hum, by, nulltypes, ...) {
  UseMethod("removeNull")
}
removeNull.humdrumR <- function(hum, by = 'File', nullTypes = 'd', ...) {
  nullTypes <- checkTypes(nullTypes, 'removeNull', 'nullTypes')
  
  humtab <- getHumtab(hum, 'GLIMDd')
  putHumtab(hum, overwriteEmpty = "GLIMDdP") <- removeNull.data.table(humtab, by = by, nullTypes = nullTypes)
  
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
#' @rdname filterHumdrum
removeEmptyFiles <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyFiles')
  renumberFiles(removeNull(humdrumR,'File', 'GLIMd'))
}
#' @export
#' @rdname filterHumdrum
removeEmptySpines <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptySpines')
  removeNull(humdrumR,  c('File', 'Spine'), 'GLIMd')
}
#' @export
#' @rdname filterHumdrum
removeEmptyRecords <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyRecords')
  removeNull(humdrumR, c('File', 'Record'), 'd')
}

#' @export
#' @rdname filterHumdrum
removeEmptyStops <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'removeEmptyStops')
  removeNull(humdrumR, c('File', 'Stop'), 'd')
}


# Indexing ----

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



#' @rdname filterHumdrum
#' @usage humdata[] # returns unchanged
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing'),
          definition = force)

### numeric ----

#' @rdname filterHumdrum
#' @usage humdata[x:y]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i, removeEmpty = TRUE) {
              i <- numericIndexCheck(i)
              
              if (removeEmpty) {
                humtab <- getHumtab(x, 'GLIMDdP')
                
                targets <- humtab[ , sort(unique(File))[i]]
                humtab <- humtab[File %in% targets]
               
                
                putHumtab(x, overwriteEmpty = "GLIMDdP") <- humtab
              } else {
                x <- subset(x, File %in% sort(unique(File))[!!i])
              }
             
              
              x

          })



### character ----

#' @rdname filterHumdrum
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i, removeEmpty = TRUE) {
            x <- subset(x, dofill ~ any(. %grepl% !!i),  by = File)
            
            if (removeEmpty) x <- removeEmptyFiles(x)
            
            x
          })

### formula ----



#' @rdname filterHumdrum
#' @usage humdata[~expression]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'formula'),
          function(x, i, removeEmpty = TRUE) {
              i <- wrapInCall('any', i)
              rlang::f_lhs(i) <- quote(dofill)
              
              x <- filterHumdrum(x, i, by ~ File, recordtypes ~ "D")
              
              if (removeEmpty) x <- removeEmptyFiles(x)
              
              x
          })

## Double brackets [[]] ----
    

### numeric ----

#' @rdname filterHumdrum
#' @usage humdata[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i, removeEmpty = TRUE) {
            i <- numericIndexCheck(i)    
            
            if (removeEmpty) {
              humtab <- getHumtab(x, 'GLIMDdP')
              
              humtab <- humtab[Record %in% i | Token == '*-' | grepl('\\*\\*', Token)]
              
              putHumtab(x, overwriteEmpty = c()) <- humtab
            } else {
              form <- do ~ Record %in% sort(unique(Record))[i]
              x <- filterHumdrum(x, form, recordtypes ~ "GLIMDdP")
            }
          
            
            
            x

          })


#' @rdname filterHumdrum
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j, removeEmpty = TRUE) {
              j <- numericIndexCheck(j)    
              
              if (removeEmpty) {
                humtab <- getHumtab(x, 'GLIMDdP')
                humtab <- humtab[is.na(Spine) | Spine %in% j]
                
                putHumtab(x, overwriteEmpty = c()) <- humtab
              } else {
                
                form <- do ~ Spine %in% sort(unique(Spine))[j] | is.na(Spine)
                
                x <- filterHumdrum(x, form, recordtypes ~ "D")
              }
              
              x
              
              
          })



### character ----

# grepingind <- function(humdrumR, ind, func) {
#           Dd <- getHumtab(humdrumR, dataTypes = c('D', 'd'))
#           Dd[ , .indhits := grepl(pattern = ind, evalActive(humdrumR, dataTypes = c('D', 'd')))]
#           
#           Dd <- Dd[ , func(.SD), by = File]
#           putHumtab(humdrumR, overwriteEmpty = "GLIMDdP") < Dd[ , '.indhits' :=  NULL]
#           humdrumR
# }


#' @rdname filterHumdrum
#' @usage humdata[['regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
function(x, i, removeEmpty = FALSE) {
    # gets any record which contains match
  
    form <- do ~ Record %in% unique(Record[. %grepl% i])
    x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
    
    if (removeEmpty) x <- removeEmptyRecords(x)
    
    x
})

# setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
#           function(x, i) {
#             # gets any record which contains match
#             grepingind(x, i,  \(sd) { 
#               recn <- unique(sd$Record[sd$.indhits])
#               sd[Record %in% recn]
#             })
#           })


#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @name humdrumR-class
#' @rdname humdrumRclass
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
              putHumtab(x, overwriteEmpty = "GLIMDdP") <- humtab
              
            } else {
              form <- if (all(grepl('^\\*\\*', j))) {
                j <- gsub('^\\*\\**', '', j)
                do ~ Spine %in% unique(Spine[Exclusive %in% j])
              } else {
                do ~ Spine %in% unique(Spine[. %grepl% j])
              }
              x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
              
              if (removeEmpty) x <- removeEmptySpines(x)
            }
           
            
            
            x

          })





#' @rdname humdrumRclass
#' @usage humdata[[ , , ~expression]] or humdata [[ , , 'regex']] or humdata[[z = ~expression]] or humdata[[z = 'regex']]
#' @export
setMethod('[[',
          signature = c(x = 'humdrumR', i = 'missing', j = 'missing'),
          function(x, i, j, k, ..., removeEmpty = FALSE) {
              if (missing(k)) return(x)
              
              if (!(rlang::is_formula(k) || is.character(k)))  {
                  stop('When indexing humdrumR objects using [[]], third argument (k) must be a formula or character string.')
              }
              
              if (is.character(k)) {
                  x <- filterHumdrum(x, do ~ . %grepl% k, recordtypes ~ "D")
              }
              if (rlang::is_formula(k)) {
                  # x <- do.call('filterHumdrum', c(x, k,  recordtypes ~ "D", list(...)))
                  x <- filterHumdrum(x, k, recordtypes ~ 'D', ...)
              }
            
              if (removeEmpty) x <- removeEmptyFiles(x)
            
              x
              
          })





### formula ----



#' @rdname humdrumRclass
#' @usage humdata[[~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula', j = 'missing'), 
          function(x, i, removeEmpty = FALSE) {
            
                    i <- rlang::as_quosure(i)
                    form <- rlang::new_formula(quote(dofill), rlang::expr(Record %in% unique(Record[!!i])))
                    
                    x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
                    
                    if (removeEmpty) x <- removeEmptyRecords(x)
                    
                    x
          })

#' @rdname humdrumRclass
#' @usage humdata[[ , ~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'formula'), 
          function(x, j, removeEmpty = TRUE) {
            j <- rlang::as_quosure(j)
            
            form <- rlang::new_formula(quote(dofill), rlang::expr(Spine %in% unique(Spine[!!j])))
            
            x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
            
            if (removeEmpty) x <- removeEmptySpines(x)
            
            x
          })



#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @rdname filterHumdrum
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



