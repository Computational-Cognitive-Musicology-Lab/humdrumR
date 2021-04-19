
############## Filtering humdrumR #######


#' Filter humdrum data
#' 
#' \code{filterHumdrum} is a command used to filter a \code{\linkS4class{humdrumR}} corpus.
#' Indexing (using the \code{[]} or {\code{[[]]}} operators is a special case---indeed,
#' the \code{[]} and \code{[[]]} methods for \code{\linkS4class{humdrumR}} objects are 
#' simply wrappers to calls to \code{filterHumdrum}.
#' 
#' \code{filterHumdrum} is used in the same way as \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}}:
#' Any number of formulae or functions can be fed as arguments to \code{filterHumdrum}: these arguments
#' are passed directly to a call to \code{\link[humdrumR:with-in-Humdrum]{withinHumdrum}}.
#' The only caveat difference is that the do expressions / functions fed to \code{filterHumdrum} 
#' must be \href{https://en.wikipedia.org/wiki/Predicate_(mathematical_logic)}{predicate} expressions---
#' expressions that return a logical vector.
#' The returned vector must either be the same length as the input data (the number
#' of rows in the \code{\link[humdrumR::humtable]{humdrum table}}) \emph{or} be of length \code{1}. 
#' 
#' If a predicate expression \emph{is} of length \code{1}, the entire \code{\link[humdrumR::humtable]{humdrum table}}
#' is either returned (\code{TRUE}) or filtered out (\code{FALSE}).
#' This can be combined with a \code{by~} expression to filter out entire segments:
#' for instance, the command
#' \preformatted{
#' filterHumdrum(humdata, ~any(. \%~\% '\eE]-'), by ~ File ~ Spine)
#' }
#' will look in each file and remove any spine which contains an Eb token (the whole spine is removed
#' if it contains even one Eb).
#' 
#' @section Indexing:
#' To learn how to
#' In \code{R}, the basic \code{\link[base:Extract]{indexing operators}}
#' are \code{[]} and \code{[[]]}.
#' 
#' These are used to filter out subsets of data.
#' In many \code{R} data types (for instance, base \code{R}'s \code{\link[base:list]{list}}),
#' the \code{[}single brackets\code{]}
#' are used for "shallower" extraction while the \code{[[}double brackets\code{]]}
#' are used for "deeper" extraction.
#' \code{humdrumR} object indexing follows this same basic pattern:
#' \code{[}single brackets\code{]} are used to index \code{humdrumR} objects
#' \emph{by piece} while \code{[[}double brackets\code{]]} are used to index
#' \emph{within pieces}. (Accidentally writing \code{[]} when you need
#' \code{[[]]} is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, \code{humdrumR} objects can use
#' three types of indexing arguments:
#' \itemize{
#' \item By \code{numeric} (ordinal integers)
#' \item By \code{character} string (regular expressions)
#' \item By \code{formula} (arbitrary expressions)
#' }
#' The last option (by \code{formula}) is the most powerful option,
#' and indeed, the first two options (\code{numeric} or \code{character} indexing)
#' are just convenient shorthands for indexing that can be accomplished using 
#' the \code{formula} method.
#' 
#' 
#' \strong{Numeric indexing:} Indexing \code{humdrumR} objects with
#'  \strong{\code{[}single brackets\code{]}} will accept
#' one numeric argument. (Non-integer arguments will be converted to integers.)
#' This argument will be used to pick pieces within the \code{humdrumR} object ordinally.
#' Thus, \code{humdata[1:10]} will select the first ten pieces in the data while \code{humdata[42]}
#' will select only the 42nd piece. 
#' 
#' 
#' Indexing \code{humdrumR} objects with
#'  \strong{\code{[[}double brackets\code{]]}} will accept 
#' one or two numeric arguments, \code{i} and \code{j}, either of which can 
#' be used in isolation or in combination.
#' If \code{j} is used in isolation, it must be placed after a comma, as in \code{humdata[[ , j ]]}.
#' (Non-integer arguments will be converted to integers.)
#' \code{i} is used to index data records (i.e., based on the \code{NData} field) ordinally.
#' Thus, \code{humdata[[1:20]]} indexes the first twenty data records \emph{from each file}
#' in the corpus, and \code{humdata[[42]]} extracts the 42nd data record \emph{from each file}.
#' \code{j} is used to index spines  (i.e., based on the \code{Spine} field) ordinally.
#' Thus, \code{humdata[[ , 3:4]]} returns the third and fourth spines \emph{from each}
#' file in the corpus.
#' 
#' Note that numeric \code{humdrumR} indexing is entirely \strong{ordinal}, meaning 
#' that pieces/data records/spines are not matched based on their value in their
#' respective fields, but rather on their order among all existing values.
#' Thus, for \code{[}single-bracket\code{]} indexing the \eqn{ith} piece in the
#' corpus is taken, regardless of that \code{FileN} field associated
#' with that piece:
#' 
#' \preformatted{
#' humsubset <- humdata[11:20]
#' humsubset[2]
#' }
#' 
#' will return the 12th piece from the original \code{humdata} object, not the second piece.
#' Similarly,
#' \preformatted{
#' humsubset2 <- humdata[[ , 2:4]]
#' humsubset2[[ , 2]]
#' }
#' will return the third spine from the original data.
#' 
#' As in traditional \code{R} indexing, negative numbers are allowed as well, causing corresponding elements to be
#' removed instead of retained. Thus, \code{humdata[-3:-5]} will remove the third, fourth, and fifth pieces from the data
#' while \code{humdata[[ , -3:-5]]} will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
#' 
#' In all cases, indices outside of range (or of value \code{0)} are ignored.
#' If all indices are \code{0} or outside of range then 
#' an empty \code{humdrumR} object is returned.
#' For instance, \code{humdata[[401:500, ]]} will return an empty
#' \code{humdrumR} object if there are no pieces with more than 400
#' data records.
#' 
#' 
#' \strong{Character indexing:} Indexing \code{humdrumR} objects with 
#' \code{[}single brackets\code{]} will accept one 
#' vector of \code{character} strings. These strings are 
#' treated as 
#' \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the \code{humdrumR} object's \code{Active} fields are searched
#' for matches to all the regular expressions you input. Any piece that contains
#' \emph{\strong{any}} match to \emph{\strong{any}} of the regular expressions is retained---all other pieces
#' are dropped. Note that (because this is \code{[}single-bracket\code{]} indexing) the entire piece is retained, even if there is only one match.
#' If no matches occur in any pieces, an empty \code{humdrumR} object is returned.
#' 
#' Indexing \code{humdrumR} objects with \code{[[}double brackets\code{]]} will 
#' accept one or two vectors of \code{character} strings, \code{i} and \code{j}, 
#' either of which can 
#' be used in isolation or in combination. 
#' If \code{j} is used in isolation, it must be placed after a comma, 
#' as in \code{humdata[[ , j]]}.
#' These strings are 
#' treated as \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the \code{humdrumR} object's \code{Active} fields are searched
#' for matches to all the regular expressions you input.
#' Any record which contains at least one token matching any regex in \code{i}
#' will be retained.
#' Similarly, any spine which contains at least one token matching any
#' regex in \code{j} is retained.
#' If \code{i} and {j} are used together,
#'  matching spines are indexed first, so that 
#' tokens matching the regular expression(s) in \code{i}
#' must be found in matching spines.
#' 
#' A third argument, \code{k}, can also be used, but only if 
#' both \code{i} and \code{j} arguments are missing.
#' In the case of \code{k}, only matching tokens are retained,
#' regardless of their spine or record number(s).
#' Any pieces, spines, or records with no matches are dropped entirely.
#' 
#' 
#' \strong{Formula indexing:} Indexing \code{humdrumR} objects with 
#' \code{formulae} is the most powerful, flexible indexing option.
#' Either \code{[}single\code{]} or \code{[}double\code{]} brackets will accept
#' a formula. The right-hand side of each formula will be evaluated
#' within the \code{humdrumR} objects internal 
#' \code{\link[humdrumR:humTable]{humdrum table}}.
#' Each formula must evaluate to a \code{logical} vector of the same 
#' length as the total number of tokens (rows in the humdrum table).
#' 
#' In the case of \code{[}single-bracket\code{]} indexing, only one \code{formula}
#' is accepted, and \emph{every piece} that evalues with at least one 
#' \code{TRUE} will be retained.
#' For instance, \code{humdata[~Spine > 4]} will return all pieces
#' which contain five (or more) spines.
#' \code{[}single-bracket\code{]} formula indexing is especially useful for indexing
#' meta-data properties like reference records:
#' for instance, \code{humdata[~COM == "Paul McCartney"]} will return
#' all pieces with a \code{!!!COM: Paul McCartney} reference record.
#' 
#' In the case of \code{[[}double-bracket\code{]]} indexing, one or two formulas are accepted, 
#' in arguments \code{i} and \code{j}, either of which can 
#' be used in isolation or in combination. 
#' If \code{j} is used in isolation, it must be placed after a comma, 
#' as in \code{humdata[[ , j]]}.
#' In the case of \code{i} formulae, any record which evaluates to
#' at least one \code{TRUE} value is retained.
#' In the case of \code{j}, any spine which evaluates to
#' at least one \code{TRUE} value is retained.
#' Any piece which contains no matches is dropped entirely.
#' 
#' For \code{[[}double-bracket\code{]]} formula indexing, a third argument, \code{k}
#' may be used in the absence of \code{i} and \code{j}.
#' In the case of \code{k} all tokens which evaluate to \code{TRUE}
#' are retained, regardless of piece/spine/record.
#' Pieces, spines, or records with no \code{TRUE} values
#' are simply dropped.
#' 
#' @section Assignment:
#' \code{R} objects often have ways of assigning new values to 
#' \emph{part} of the object using \code{\link[base:Extract]{indexing operators}}.
#' \code{humdrumR} objects are no different.
#' 
#' A new field can be inserted in a \code{humdrumR} object in two ways:
#' \enumerate{
#' \item A field can be copied from one humdrumR object to another if the humdrumR objects'
#' \code{\link[humdrumR:humTable]{humdrum tables}} have the same number of data tokens (i.e., rows).
#' This is actually most useful for renaming fields within a humdrumR object (explained below).
#' \item A \code{\link[base:vector]{vector}} or \code{\link[base:list]{list}} can be instered as a 
#' new field in a \code{humdrumR}---but again, it must be the same length as the number of tokens
#' in the object's \code{\link[humdrumR:humTable]{humdrum table}}.
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
#' \strong{\code{humdrumR$fieldname <- humdrumR} assignment}: Assigning a field in one \code{humdrumR}
#' object from another \code{humdrumR} object works like this. First of call, as a reminder, the two \code{humdrumR}
#' objects must have the exact same numbers of data tokens in their \code{\link[humdrumR:humTable]{humdrum tables}}.
#' This means, that this is most useful for assigning field names from one \code{humdrumR} object to itself.
#' The name(s) given in the indexing expression on the left side of the assignment (i.e., \code{humdata[c('name1', 'name2')]} or
#' \code{humdata$name}) are used as new field names.
#' How fields are extracted from the right side of the assignment is a little trickier:
#' Any fields in the right-side \code{humdrumR} object which are named \eqn{PipeN} (where \eqn{N} is an integer) are copied
#' in descending order into the named fields on the left side.
#' If there are no \eqn{PipeN} fields on the right side, any fields used in the current Active formula (on the right side)
#' are copied instead.
#' 
#' This system might seem odd at first, but it is very useful in combination with the \code{\link[humdrumR:with-in-Humdrum]{withinHumdrum}} function,
#' and its convenient pipe operator \code{\link[humdrumR:humPipe]{\%hum>\%}}.
#' The \code{withinHumdrum} command always creates new fields that are called \eqn{Pipe1 ... Pipe2 ... PipeN}.
#' By using \code{humdata$name} we can immediately assign these pipe fields more meaningful names!
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
    indexfield <- tail(pipeFields(getHumtab(humdrumR)), 1)
    
    if (fields(humdrumR)[Name == indexfield, Class] != 'logical') stop('In call to filterHumdrum, the do-expression must evaluate to a logical vector.')
    
    nullifyIndex(humdrumR, indexfield, oldActive)
    
    
}

nullifyIndex <- function(humdrumR, indexfield, newActive = ~Token) {
  # This function takes a humdrumR object with a logical indexfield  
  # field and changes the Null field to Null | !indexfield
  # In other words, it used the index field to make null data.
  
  if (!indexfield  %in% fields(humdrumR)$Name) return(humdrumR)
  humtab <- getHumtab(humdrumR)
  removeFields(humdrumR) <- indexfield
  
  humtab$Null <-  (!humtab[ , indexfield, with = FALSE]  & !is.na(humtab[ , indexfield, with = FALSE])) | humtab$Null
  humtab <- humtab[ , colnames(humtab) != indexfield, with = FALSE]
  
  putHumtab(humdrumR, drop = TRUE) <- humtab
  humdrumR <- setActive(humdrumR, newActive)
  humdrumR
  
  
}



############## Null indexing ----

# humdrumR filtering and application can result in lots of null tokens.
# These functions remove parts that are entirely null

removeNull <- function(humdrumR, ..., recordTypes = 'GLIMDdP') {
  checkhumdrumR(humdrumR, 'removeNull')
  checkTypes(recordTypes, 'removeNull', 'recordTypes')
  
  recordtypesform <- rlang::new_formula(quote(recordtypes), rlang::expr(!!recordTypes))
  remove <- withHumdrum(humdrumR, recordtypesform, dofill ~ all(Null, na.rm=T), ...)
  
  
  humtab <- getHumtab(humdrumR, recordTypes)
  humtab <- humtab[remove == FALSE]
  
  putHumtab(humdrumR, drop = FALSE) <- humtab
  humdrumR
}


unfillNull <- function(humdrumR, ..., fillfromTypes = 'D', recordtypes = recordtypes ~ 'GLIMDdP') {
  checkhumdrumR(humdrumR, 'unfillNull')
  fillfromTypes <- checkTypes(fillfromTypes, 'unfillNull', 'fillfromTypes')
  
  humdrumR$Null <- withinHumdrum(humdrumR, dofill ~ any(!Null[Type %in% fillfromTypes]), recordtypes, ...)
  humdrumR
  
  
}
fillNull <- function(humdrumR, ..., fillfromTypes = 'D', recordtypes = recordtypes ~ 'GLIMDdP') {
    checkhumdrumR(humdrumR, 'fillNull')
    fillfromTypes <- checkTypes(fillfromTypes, 'fillNull', 'fillfromTypes')
  
    filterHumdrum(humdrumR, dofill ~ !all(Null[Type %in% fillfromTypes], na.rm = TRUE), recordtypes, ...)
  
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
              removeNull(x, by ~ File)
          })



##[character]

#' @name filterHumdrum
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i) {
            x <- filterHumdrum(x, dofill ~ any(. %~% i),  by ~ File, recordtypes ~ "D")
            removeNull(fillNull(x, by ~ File), by ~ File)
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
              
              x <- filterHumdrum(x, i, by ~ File,
                            recordtypes ~ "D")
              removeNull(fillNull(x, by ~ File), by ~ File)
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
            
            filterHumdrum(x, form, by ~ File,
                          recordtypes ~ "GLIMDdP")
          })


#' @name filterHumdrum
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j) {
              j <- numericIndexCheck(j)    
              
              form <- do ~ Spine %in% sort(unique(Spine))[j] | is.na(Spine)
              
              filterHumdrum(x, form,
                            recordtypes ~ "GLIMDdP")
          })

#' @name filterHumdrum
#' @usage humdata[[x:y, l:m]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'numeric'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
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
function(x, i) {
    # gets any record which contains match
    x <- filterHumdrum(x, do ~ any(. %~% i), by ~ File,
                       recordtypes ~ "D")
  
    filterHumdrum(x, do ~ any(. %~% i), by ~ File ~ Record,
                  recordtypes ~ "D")
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
              filterHumdrum(x, do ~ any(. %~% i), by ~ File ~ Spine,
                            recordtypes ~ "D")
          })
# setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
#           function(x, j) {
#               #gets any spine which contains match
#               grepingind(x, j,  function(sd) { 
#                   recn <- unique(sd$Spine[sd$.indhits])
#                   sd[Spine %in% recn]
#               })
#           })



#' @name humdrumR-class
#' @usage humdata[['regex1', 'regex2']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'character'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
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
                  x <- filterHumdrum(x, do ~ . %~% k)
              }
              if (rlang::is_formula(k)) {
                  x <- do.call('filterHumdrum', c(x, k, 
                                                  recordtypes ~ "D",
                                                  list(...)))
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
                    i <- wrapInCall('any', i)
           
                    filterHumdrum(x, i, by ~ File ~ Record,
                                  recordtypes ~ "D")
          })

#' @name humdrumR-class
#' @usage humdata[[ , ~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'formula'), 
          function(x, j) {
              j <- wrapInCall('any', j)
              
              filterHumdrum(x, j, by ~ File ~ Spine,
                            recordtypes ~ "D")
          })

#' @name humdrumR-class
#' @usage hudmata[[~expression1, ~expression2]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula', j = 'formula'), 
          function(x, i, j) {
                    x <- x[[ , j]]
                    x[[i, ]]
          })





  ############## GLIMdP indexing ----
  
#' @export
indexGLIM <- function(humdrumR, dataTypes = c('G', 'L', 'I', 'M', 'd', 'P')) {
  #####ReIndex GLIMd humdrum tables to match indexing of D tables
  # To do this we need to:
  #   
  #    2) Make sure that pieces which are missing from the D table (because they've been indexed away) 
  #       are removed from GLIMd table (most likely)
  #    3) Make sure, on a piece-by-piece basis that
  #         a) Spines which are missing from D are removed from GLIMd
  #         b) That there are no records which are all d (null)
  #         c) That there are no "bunched up" barlines at the beggining, end, or anywherei in the middle.
  #         d) That there are no tandem interpretations AFTER the last data token.
          
  GLIMDdP <- getHumtab(humdrumR, dataTypes = dataTypes)
  D       <- getD(humdrumR)
  # first add missing fields (columns)
  missingfields <- colnames(D)[!colnames(D) %in% colnames(GLIMDdP)]
  missingfieldsTypes <- vapply(D[ , missingfields, with = FALSE], class, FUN.VALUE = character(1)) 
  
  if (length(missingfields) > 0L) padGLIMfields(GLIMDdP) <- missingfieldsTypes
  
  GLIMDdP <- GLIMDdP[ , colnames(D), with = FALSE] # match order of columns
  
  # next  remove missing pieces (i.e., File or Filename fields)
  GLIMDdP <- GLIMDdP[File %in% unique(unlist(D$File))]
  
  # then, do indexing by piece:
  # (GLIMd and D records are combined into one table, then split by piece)
  GLIMDdP <- rbindlist(list(D, GLIMDdP), fill = TRUE, use.names = TRUE)
  GLIMDdP <- GLIMDdP[ , indexGLIM_piece(.SD), by = File, .SDcols = colnames(D)[colnames(D) != 'File']]
  
  # resplit and put back in to humdrumR object
  putHumtab(humdrumR, drop = FALSE) <- GLIMDdP
  humdrumR
}


indexGLIM_piece <- function(humtab) {
  ###called by indexGLIM on each individual piece
  D <- humtab[Type == 'D']
  # remove missing spines
  humtab <- humtab[Spine %in% unique(D$Spine) | is.na(Spine)]
  
  # remove all except last barline before first data record
  prebarline <- unique(humtab$Record[humtab$Type == 'M' & humtab$Record < min(D$Record, na.rm = TRUE)])
  if (length(prebarline) != 0L)   humtab <- humtab[!(Record < prebarline[length(prebarline)] & Type == 'M')]
  
  #remove everything after last data record, except global stuff, '*-' or '=='
  # humtab <- humtab[!(Record > max(D$Record, na.rm = TRUE) & !(is.na(Spine) | Token %in% c('*-', '==', '*v', '*^')))]
  
  
  # if (any(humtab$Type == 'd')) {
    #remove records containing only d
    # rectab <- table(humtab$Record, humtab$Type)
    # recs   <- as.numeric(rownames(rectab))
    # humtab <- humtab[Record %in% recs[(!(rectab[ , 'd'] > 0 & rectab[ , 'D'] == 0))]]
  # }
  
  
  
  if (any(humtab$Type == 'M')) {
    #remove consecutive barlines
    rectab <- table(humtab$Record, humtab$Type)
    recs   <- as.numeric(rownames(rectab))
    humtab <- humtab[Record %in% recs[!(rectab[ ,'M'] == rotate(rectab[ ,'M'], rotation = -1, pad = -1) & rectab[ ,'M'] > 0)]]
  }
  #
  humtab
  
}





