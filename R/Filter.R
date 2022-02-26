
############## Filtering humdrumR #######


#' Filter humdrum data
#' 
#' `filterHumdrum` is a command used to filter a [humdrumR corpus][humdrumR::humdrumR-class]
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
#' [HumdrumR corpus][humdrumR::humdrumR-class] indexing follows this same basic pattern:
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
#' When indexing [humdrumR corpora][humdrumR::humdrumR-class] with numbers,
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
#' Indexing [humdrumR objects][humdrumR:humdrumR-class] with 
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
#' Indexing [humdrumR objects][humdrumR:humdrumR-class] with 
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
    
    formulae <- list(...)
    formulae <- anyfuncs2forms(formulae, parent.env(environment()))
    if (any(!sapply(formulae, rlang::is_formula))) stop('In filterHumdrum(...) unnamed arguments must be formulas or functions.')
    
    oldActive <- getActive(humdrumR)
    
    humdrumR <- withinHumdrum(humdrumR, ...)

    humtab <- getHumtab(humdrumR, 'GLIMDd')
    
    filterPipe <- tail(pipeFields(humtab), 1L)
    removeFields(humdrumR) <- filterPipe
    
    colnames(humtab)[colnames(humtab) == filterPipe] <- '__TmpFilter__'
    
    if (humtab[ , class(`__TmpFilter__`)] != 'logical') stop('In call to filterHumdrum, the do-expression must evaluate to a logical vector.')
    
    humtab[ , `__TmpFilter__` := `__TmpFilter__` | is.na(`__TmpFilter__`)]
    humtab[ , Filter := Filter | !`__TmpFilter__`]
    humtab[ , `__TmpFilter__` := NULL]
    
    putHumtab(humdrumR) <- humtab
    humdrumR <- update_d(humdrumR)
    
    humdrumR <- setActive(humdrumR, oldActive)

    
    humdrumR
    
}




############## Null indexing ----

# humdrumR filtering and application can result in lots of filtered tokens.
# These functions remove parts that are entirely filtered

removeNull <- function(humdrumR, recordTypes = 'GLIMDd', ...) {
  checkhumdrumR(humdrumR, 'removeNull')
  # checkTypes(recordTypes, 'removeNull', 'recordTypes')
  
  # recordtypesform <- rlang::new_formula(quote(recordtypes), rlang::expr(!!recordTypes))
  remove <- withHumdrum(humdrumR, recordTypes ~ 'GLIMDd', dofill ~ !(any(Type == 'D')), ...)
  
  
  humtab <- getHumtab(humdrumR, recordTypes)
  humtab <- humtab[remove == FALSE]
  
  putHumtab(humdrumR, drop = FALSE) <- humtab
  
  renumberSpines(humdrumR)
}



#' @export
#' @rdname filterHumdrum
removeEmptyFiles <- function(humdrumR, fillfromTypes = 'D') {
  fillfromTypes <- checkTypes(fillfromTypes, 'removeEmptyFiles', 'fillfromTypes')
  removeNull(humdrumR, 'GLIMDd', by ~ File)
}
#' @export
#' @rdname filterHumdrum
removeEmptySpines <- function(humdrumR, fillfromTypes = 'D') {
  fillfromTypes <- checkTypes(fillfromTypes, 'removeEmptySpines', 'fillfromTypes')
  removeNull(humdrumR, 'GLIMDd', by ~ File ~ Spine)
}
#' @export
#' @rdname filterHumdrum
removeEmptyRecords <- function(humdrumR, fillfromTypes = 'D') {
  fillfromTypes <- checkTypes(fillfromTypes, 'removeEmptyRecords', 'fillfromTypes')
  removeNull(humdrumR, 'GLIMDd', by ~ File ~ Record)
}

#########################Indexing ----

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


####[]



#' @name filterHumdrum
#' @usage humdata[] # returns unchanged
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing'),
          definition = force)

##[numeric]

#' @name filterHumdrum
#' @usage humdata[x:y]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i, removeEmpty = TRUE) {
              i <- numericIndexCheck(i)
              
              form <- do ~ File %in% sort(unique(File))[i]
              
              x <- filterHumdrum(x, form, recordtypes ~ "GLIMDdP")
              
              if (removeEmpty) x <- removeEmptyFiles(x)
              
              x
              # removeNull(x, by ~ File)
          })



##[character]

#' @name filterHumdrum
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i, removeEmpty = TRUE) {
            x <- filterHumdrum(x, dofill ~ any(. %~% i),  by ~ File, recordtypes ~ "D")
            
            if (removeEmpty) x <- removeEmptyFiles(x)
            
            x
          })

##[formula]



#' @name filterHumdrum
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

####[[]]
    

##[[numeric]]

#' @name filterHumdrum
#' @usage humdata[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i, removeEmpty = TRUE) {
            i <- numericIndexCheck(i)    
            
            form <- do ~ Record %in% sort(unique(Record))[i]
            
            x <- filterHumdrum(x, form, recordtypes ~ "GLIMDdP")
            
            if (removeEmpty) x <- removeEmptyRecords(x)
            
            x

          })


#' @name filterHumdrum
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j, removeEmpty = TRUE) {
              j <- numericIndexCheck(j)    
              
              form <- do ~ Spine %in% sort(unique(Spine))[j] | is.na(Spine)
              
              x <- filterHumdrum(x, form, recordtypes ~ "D")
              
              if (removeEmpty) x <- removeEmptySpines(x)
              
              x
              
              
          })



#### [[character]]

# grepingind <- function(humdrumR, ind, func) {
#           Dd <- getHumtab(humdrumR, dataTypes = c('D', 'd'))
#           Dd[ , .indhits := grepl(pattern = ind, evalActive(humdrumR, dataTypes = c('D', 'd')))]
#           
#           Dd <- Dd[ , func(.SD), by = File]
#           putHumtab(humdrumR, drop = TRUE) < Dd[ , '.indhits' :=  NULL]
#           humdrumR
# }


#' @name filterHumdrum
#' @usage humdata[['regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
function(x, i, removeEmpty = TRUE) {
    # gets any record which contains match
  
    form <- do ~ Record %in% unique(Record[. %~% i])
    x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
    
    if (removeEmpty) x <- removeEmptyRecords(x)
    
    x
})

# setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
#           function(x, i) {
#             # gets any record which contains match
#             grepingind(x, i,  function(sd) { 
#               recn <- unique(sd$Record[sd$.indhits])
#               sd[Record %in% recn]
#             })
#           })


#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @name humdrumR-class
#' @usage humdata[[ , 'regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j, removeEmpty = TRUE) {
            #gets any spine which contains match
            form <- do ~ Spine %in% unique(Spine[. %~% j])
            
            x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
            
            if (removeEmpty) x <- removeEmptySpines(x)
            
            x

          })





#' @name humdrumR-class
#' @usage humdata[[ , , ~expression]] or humdata [[ , , 'regex']] or humdata[[z = ~expression]] or humdata[[z = 'regex']]
#' @export
setMethod('[[',
          signature = c(x = 'humdrumR', i = 'missing', j = 'missing'),
          definition = function(x, i, j, k, ..., removeEmpty = TRUE) {
              if (missing(k)) return(x)
              
              if (!(rlang::is_formula(k) || is.character(k)))  {
                  stop('When indexing humdrumR objects using [[]], third argument (k) must be a formula or character string.')
              }
              
              if (is.character(k)) {
                  x <- filterHumdrum(x, do ~ . %~% k, recordtypes ~ "D")
              }
              if (rlang::is_formula(k)) {
                  # x <- do.call('filterHumdrum', c(x, k,  recordtypes ~ "D", list(...)))
                  x <- filterHumdrum(x, k, recordtypes ~ 'D', ...)
              }
            
              if (removeEmpty) x <- removeEmptyFiles(x)
            
              x
              
          })


##[[logical]]

# setMethod('[[',  signature = c(x = 'humdrumR', i = 'logical', j = 'missing'), 
#           function(x, i) {
#                     #gets rows which are TRUE
#                     D <- get(D)
#                     
#                     putD(x) <- D[i]
#                     x
#           })

##[[formula]]



#' @name humdrumR-class
#' @usage humdata[[~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula', j = 'missing'), 
          function(x, i, removeEmpty = TRUE) {
            
                    i <- rlang::as_quosure(i)
                    form <- rlang::new_formula(quote(dofill), rlang::expr(Record %in% unique(Record[!!i])))
                    
                    x <- filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
                    
                    if (removeEmpty) x <- removeEmptyRecords(x)
                    
                    x
          })

#' @name humdrumR-class
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
#' @name filterHumdrum
#' @usage humdata[[x:y, l:m]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'ANY', j = 'ANY'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
          })


#   
# indexGLIM <- function(humdrumR, dataTypes = c('G', 'L', 'I', 'M', 'd', 'P')) {
#   #####ReIndex GLIMd humdrum tables to match indexing of D tables
#   # To do this we need to:
#   #   
#   #    2) Make sure that pieces which are missing from the D table (because they've been indexed away) 
#   #       are removed from GLIMd table (most likely)
#   #    3) Make sure, on a piece-by-piece basis that
#   #         a) Spines which are missing from D are removed from GLIMd
#   #         b) That there are no records which are all d (null)
#   #         c) That there are no "bunched up" barlines at the beggining, end, or anywherei in the middle.
#   #         d) That there are no tandem interpretations AFTER the last data token.
#           
#   GLIMDdP <- getHumtab(humdrumR, dataTypes = dataTypes)
#   D       <- getD(humdrumR)
#   # first add missing fields (columns)
#   missingfields <- colnames(D)[!colnames(D) %in% colnames(GLIMDdP)]
#   missingfieldsTypes <- vapply(D[ , missingfields, with = FALSE], class, FUN.VALUE = character(1)) 
#   
#   if (length(missingfields) > 0L) padGLIMfields(GLIMDdP) <- missingfieldsTypes
#   
#   GLIMDdP <- GLIMDdP[ , colnames(D), with = FALSE] # match order of columns
#   
#   # next  remove missing pieces (i.e., File or Filename fields)
#   GLIMDdP <- GLIMDdP[File %in% unique(unlist(D$File))]
#   
#   # then, do indexing by piece:
#   # (GLIMd and D records are combined into one table, then split by piece)
#   GLIMDdP <- rbindlist(list(D, GLIMDdP), fill = TRUE, use.names = TRUE)
#   GLIMDdP <- GLIMDdP[ , indexGLIM_piece(.SD), by = File, .SDcols = colnames(D)[colnames(D) != 'File']]
#   
#   # resplit and put back in to humdrumR object
#   putHumtab(humdrumR, drop = FALSE) <- GLIMDdP
#   humdrumR
# }
# 
# 
# indexGLIM_piece <- function(humtab) {
#   ###called by indexGLIM on each individual piece
#   D <- humtab[Type == 'D']
#   # remove missing spines
#   humtab <- humtab[Spine %in% unique(D$Spine) | is.na(Spine)]
#   
#   # remove all except last barline before first data record
#   prebarline <- unique(humtab$Record[humtab$Type == 'M' & humtab$Record < min(D$Record, na.rm = TRUE)])
#   if (length(prebarline) != 0L)   humtab <- humtab[!(Record < prebarline[length(prebarline)] & Type == 'M')]
#   
#   #remove everything after last data record, except global stuff, '*-' or '=='
#   # humtab <- humtab[!(Record > max(D$Record, na.rm = TRUE) & !(is.na(Spine) | Token %in% c('*-', '==', '*v', '*^')))]
#   
#   
#   # if (any(humtab$Type == 'd')) {
#     #remove records containing only d
#     # rectab <- table(humtab$Record, humtab$Type)
#     # recs   <- as.numeric(rownames(rectab))
#     # humtab <- humtab[Record %in% recs[(!(rectab[ , 'd'] > 0 & rectab[ , 'D'] == 0))]]
#   # }
#   
#   
#   
#   if (any(humtab$Type == 'M')) {
#     #remove consecutive barlines
#     rectab <- table(humtab$Record, humtab$Type)
#     recs   <- as.numeric(rownames(rectab))
#     humtab <- humtab[Record %in% recs[!(rectab[ ,'M'] == rotate(rectab[ ,'M'], rotation = -1, pad = -1) & rectab[ ,'M'] > 0)]]
#   }
#   #
#   humtab
#   
# }





