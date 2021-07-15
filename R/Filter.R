
############## Filtering humdrumR #######


#' Filter humdrum data
#' 
#' `filterHumdrum` is a command used to filter a [humdrumR-class][humdrumR] corpus.
#' Indexing (using the `[]` or `[[]]` operators) is a special case---indeed,
#' the `[]` and `[[]]` methods for `\linkS4class{humdrumR`} objects are 
#' simply wrappers to calls to `filterHumdrum`.
#' 
#' `filterHumdrum` is used in the same way as [humdrumR:with-in-Humdrum][withinHumdrum]:
#' Any number of formulae or functions can be fed as arguments to `filterHumdrum`: these arguments
#' are passed directly to a call to `\link[humdrumR:with-in-Humdrum]{withinHumdrum`}.
#' The only difference is that the do expressions/functions fed to `filterHumdrum` 
#' must be [predicate](https://en.wikipedia.org/wiki/Predicate_(mathematical_logic)) expressions---
#' i.e., expressions that return a logical vector.
#' The returned vector must either be the same length as the input data (the number
#' of rows in the `\link[humdrumR::humtable]{humdrum table`}) *or* be of length `1`. 
#' 
#' If a predicate expression *is* of length `1`, the entire `\link[humdrumR::humtable]{humdrum table`}
#' is either returned (`TRUE`) or filtered out (`FALSE`).
#' This can be combined with a `by~` expression to filter out entire segments:
#' for instance, the command
#' \preformatted{
#' filterHumdrum(humdata, ~any(. \%~\% '\eE]-'), by ~ File ~ Spine)
#' }
#' will look in each file and remove any spine which contains an Eb token (the whole spine is removed
#' if it contains even one Eb).
#' 
#' @section Indexing:
#' To learn how to
#' In `R`, the basic `\link[base:Extract]{indexing operators`}
#' are `[]` and `[[]]`.
#' 
#' These are used to filter out subsets of data.
#' In many `R` data types (for instance, base `R`'s `\link[base:list]{list`}),
#' the `[`single brackets`]`
#' are used for "shallower" extraction while the `[[`double brackets`]]`
#' are used for "deeper" extraction.
#' `humdrumR` object indexing follows this same basic pattern:
#' `[`single brackets`]` are used to index `humdrumR` objects
#' *by piece* while `[[`double brackets`]]` are used to index
#' *within pieces*. (Accidentally writing `[]` when you need
#' `[[]]` is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, `humdrumR` objects can use
#' three types of indexing arguments:
#' \itemize{
#' \item By `numeric` (ordinal integers)
#' \item By `character` string (regular expressions)
#' \item By `formula` (arbitrary expressions)
#' }
#' The last option (by `formula`) is the most powerful option,
#' and indeed, the first two options (`numeric` or `character` indexing)
#' are just convenient shorthands for indexing that can be accomplished using 
#' the `formula` method.
#' 
#' 
#' **Numeric indexing:** Indexing `humdrumR` objects with
#'  **`[`single brackets`]`** will accept
#' one numeric argument. (Non-integer arguments will be converted to integers.)
#' This argument will be used to pick pieces within the `humdrumR` object ordinally.
#' Thus, `humdata[1:10]` will select the first ten pieces in the data while `humdata[42]`
#' will select only the 42nd piece. 
#' 
#' 
#' Indexing `humdrumR` objects with
#'  **`[[`double brackets`]]`** will accept 
#' one or two numeric arguments, `i` and `j`, either of which can 
#' be used in isolation or in combination.
#' If `j` is used in isolation, it must be placed after a comma, as in `humdata[[ , j ]]`.
#' (Non-integer arguments will be converted to integers.)
#' `i` is used to index data records (i.e., based on the `NData` field) ordinally.
#' Thus, `humdata[[1:20]]` indexes the first twenty data records *from each file*
#' in the corpus, and `humdata[[42]]` extracts the 42nd data record *from each file*.
#' `j` is used to index spines  (i.e., based on the `Spine` field) ordinally.
#' Thus, `humdata[[ , 3:4]]` returns the third and fourth spines *from each*
#' file in the corpus.
#' 
#' Note that numeric `humdrumR` indexing is entirely **ordinal**, meaning 
#' that pieces/data records/spines are not matched based on their value in their
#' respective fields, but rather on their order among all existing values.
#' Thus, for `[`single-bracket`]` indexing the \eqn{ith} piece in the
#' corpus is taken, regardless of that `FileN` field associated
#' with that piece:
#' 
#' \preformatted{
#' humsubset <- humdata[11:20]
#' humsubset[2]
#' }
#' 
#' will return the 12th piece from the original `humdata` object, not the second piece.
#' Similarly,
#' \preformatted{
#' humsubset2 <- humdata[[ , 2:4]]
#' humsubset2[[ , 2]]
#' }
#' will return the third spine from the original data.
#' 
#' As in traditional `R` indexing, negative numbers are allowed as well, causing corresponding elements to be
#' removed instead of retained. Thus, `humdata[-3:-5]` will remove the third, fourth, and fifth pieces from the data
#' while `humdata[[ , -3:-5]]` will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
#' 
#' In all cases, indices outside of range (or of value `0)` are ignored.
#' If all indices are `0` or outside of range then 
#' an empty `humdrumR` object is returned.
#' For instance, `humdata[[401:500, ]]` will return an empty
#' `humdrumR` object if there are no pieces with more than 400
#' data records.
#' 
#' 
#' **Character indexing:** Indexing `humdrumR` objects with 
#' `[`single brackets`]` will accept one 
#' vector of `character` strings. These strings are 
#' treated as 
#' \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the `humdrumR` object's `Active` fields are searched
#' for matches to all the regular expressions you input. Any piece that contains
#' _**any**_ match to _**any**_ of the regular expressions is retained---all other pieces
#' are dropped. Note that (because this is `[`single-bracket`]` indexing) the entire piece is retained, even if there is only one match.
#' If no matches occur in any pieces, an empty `humdrumR` object is returned.
#' 
#' Indexing `humdrumR` objects with `[[`double brackets`]]` will 
#' accept one or two vectors of `character` strings, `i` and `j`, 
#' either of which can 
#' be used in isolation or in combination. 
#' If `j` is used in isolation, it must be placed after a comma, 
#' as in `humdata[[ , j]]`.
#' These strings are 
#' treated as \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the `humdrumR` object's `Active` fields are searched
#' for matches to all the regular expressions you input.
#' Any record which contains at least one token matching any regex in `i`
#' will be retained.
#' Similarly, any spine which contains at least one token matching any
#' regex in `j` is retained.
#' If `i` and {j} are used together,
#'  matching spines are indexed first, so that 
#' tokens matching the regular expression(s) in `i`
#' must be found in matching spines.
#' 
#' A third argument, `k`, can also be used, but only if 
#' both `i` and `j` arguments are missing.
#' In the case of `k`, only matching tokens are retained,
#' regardless of their spine or record number(s).
#' Any pieces, spines, or records with no matches are dropped entirely.
#' 
#' 
#' **Formula indexing:** Indexing `humdrumR` objects with 
#' `formulae` is the most powerful, flexible indexing option.
#' Either `[`single`]` or `[`double`]` brackets will accept
#' a formula. The right-hand side of each formula will be evaluated
#' within the `humdrumR` objects internal 
#' `\link[humdrumR:humTable]{humdrum table`}.
#' Each formula must evaluate to a `logical` vector of the same 
#' length as the total number of tokens (rows in the humdrum table).
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
#' If `j` is used in isolation, it must be placed after a comma, 
#' as in `humdata[[ , j]]`.
#' In the case of `i` formulae, any record which evaluates to
#' at least one `TRUE` value is retained.
#' In the case of `j`, any spine which evaluates to
#' at least one `TRUE` value is retained.
#' Any piece which contains no matches is dropped entirely.
#' 
#' For `[[`double-bracket`]]` formula indexing, a third argument, `k`
#' may be used in the absence of `i` and `j`.
#' In the case of `k` all tokens which evaluate to `TRUE`
#' are retained, regardless of piece/spine/record.
#' Pieces, spines, or records with no `TRUE` values
#' are simply dropped.
#' 
#' @section Assignment:
#' `R` objects often have ways of assigning new values to 
#' *part* of the object using `\link[base:Extract]{indexing operators`}.
#' `humdrumR` objects are no different.
#' 
#' A new field can be inserted in a `humdrumR` object in two ways:
#' \enumerate{
#' \item A field can be copied from one humdrumR object to another if the humdrumR objects'
#' `\link[humdrumR:humTable]{humdrum tables`} have the same number of data tokens (i.e., rows).
#' This is actually most useful for renaming fields within a humdrumR object (explained below).
#' \item A `\link[base:vector]{vector`} or `\link[base:list]{list`} can be instered as a 
#' new field in a `humdrumR`---but again, it must be the same length as the number of tokens
#' in the object's `\link[humdrumR:humTable]{humdrum table`}.
#' }
#' 
#' Fields can be assigned using two syntaxes:
#' \preformatted{
#' humdata['fieldname'] <- x
#' # or
#' humdata[c('field1', 'field2')] <- x
#' }
#' or 
#' \preformatted{
#' humdata$fieldname <- x
#' }
#' 
#' **`humdrumR$fieldname <- humdrumR` assignment**: Assigning a field in one `humdrumR`
#' object from another `humdrumR` object works like this. First of call, as a reminder, the two `humdrumR`
#' objects must have the exact same numbers of data tokens in their `\link[humdrumR:humTable]{humdrum tables`}.
#' This means, that this is most useful for assigning field names from one `humdrumR` object to itself.
#' The name(s) given in the indexing expression on the left side of the assignment (i.e., `humdata[c('name1', 'name2')]` or
#' `humdata$name`) are used as new field names.
#' How fields are extracted from the right side of the assignment is a little trickier:
#' Any fields in the right-side `humdrumR` object which are named \eqn{PipeN} (where \eqn{N} is an integer) are copied
#' in descending order into the named fields on the left side.
#' If there are no \eqn{PipeN} fields on the right side, any fields used in the current Active formula (on the right side)
#' are copied instead.
#' 
#' This system might seem odd at first, but it is very useful in combination with the `\link[humdrumR:with-in-Humdrum]{withinHumdrum`} function,
#' and its convenient pipe operator `\link[humdrumR:humPipe]{\%hum>\%`}.
#' The `withinHumdrum` command always creates new fields that are called \eqn{Pipe1 ... Pipe2 ... PipeN}.
#' By using `humdata$name` we can immediately assign these pipe fields more meaningful names!
#' 
#' Examples:
#' \preformatted{
#' humdata \%hum>\% ~ semit(Token) -> humdata$Semits
#' }
#' 
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
    
    removeEmptyFiles(humdrumR)
    
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
  humdrumR
}



#' @export
removeEmptyFiles <- function(humdrumR, fillfromTypes = 'D') {
  fillfromTypes <- checkTypes(fillfromTypes, 'removeEmptyFiles', 'fillfromTypes')
  removeNull(humdrumR, 'GLIMDd', by ~ File)
}
#' @export
removeEmptySpines <- function(humdrumR, fillfromTypes = 'D') {
  fillfromTypes <- checkTypes(fillfromTypes, 'removeEmptySpines', 'fillfromTypes')
  removeNull(humdrumR, by ~ File ~ Spine)
}
#' @export
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
          function(x, i) {
              i <- numericIndexCheck(i)
              
              form <- do ~ File %in% sort(unique(File))[i]
              
              x <- filterHumdrum(x, form, recordtypes ~ "GLIMDdP")
              # removeNull(x, by ~ File)
          })



##[character]

#' @name filterHumdrum
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i) {
            filterHumdrum(x, dofill ~ any(. %~% i),  by ~ File, recordtypes ~ "D")
          })

##[formula]



#' @name filterHumdrum
#' @usage humdata[~expression]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'formula'),
          function(x, i) {
              i <- wrapInCall('any', i)
              rlang::f_lhs(i) <- quote(dofill)
              
              filterHumdrum(x, i, by ~ File, recordtypes ~ "D")
          })

####[[]]
    

##[[numeric]]

#' @name filterHumdrum
#' @usage humdata[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i) {
            i <- numericIndexCheck(i)    
            
            form <- do ~ Record %in% sort(unique(Record))[i]
            
            filterHumdrum(x, form, recordtypes ~ "GLIMDdP")

          })


#' @name filterHumdrum
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j) {
              j <- numericIndexCheck(j)    
              
              form <- do ~ Spine %in% sort(unique(Spine))[j] | is.na(Spine)
              
              filterHumdrum(x, form, recordtypes ~ "D")
              
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
function(x, i) {
    # gets any record which contains match
  
    form <- do ~ Record %in% unique(Record[. %~% i])
    filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
})

# setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
#           function(x, i) {
#             # gets any record which contains match
#             grepingind(x, i,  function(sd) { 
#               recn <- unique(sd$Record[sd$.indhits])
#               sd[Record %in% recn]
#             })
#           })


#' @name humdrumR-class
#' @usage humdata[[ , 'regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j) {
            #gets any spine which contains match
            form <- do ~ Spine %in% unique(Spine[. %~% j])
            
            filterHumdrum(x, form, by ~ File, recordtypes ~ "D")

          })





#' @name humdrumR-class
#' @usage humdata[[ , , ~expression]] or humdata [[ , , 'regex']] or humdata[[z = ~expression]] or humdata[[z = 'regex']]
#' @export
setMethod('[[',
          signature = c(x = 'humdrumR', i = 'missing', j = 'missing'),
          definition = function(x, i, j, k, ...) {
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
          function(x, i) {
            
                    i <- rlang::as_quosure(i)
                    form <- rlang::new_formula(quote(dofill), rlang::expr(Record %in% unique(Record[!!i])))
                    
                    filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
          })

#' @name humdrumR-class
#' @usage humdata[[ , ~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'formula'), 
          function(x, j) {
            j <- rlang::as_quosure(j)
            form <- rlang::new_formula(quote(dofill), rlang::expr(Spine %in% unique(Spine[!!j])))
            
            filterHumdrum(x, form, by ~ File, recordtypes ~ "D")
          })




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





