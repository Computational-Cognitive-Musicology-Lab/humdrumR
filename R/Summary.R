#######################################################---
## Functions for summarizing humdrumR objects----
######################################################---

#' Summarize humdrumR corpora
#' 
#' \code{\link[humdrumR:humdrumR]{humdrumR}} includes a number of built in
#' functions for creating quick summaries of \code{\linkS4class{humdrumR}}
#' corpora:
#' \describe{
#' \item{\link[humdrumR:humCensus]{census}}{Tabulates the raw size of the humdrumR corpus. An example of the first item of such output is below (taken from MCFlow corpus):
#' ###### humdrumR census of GLIMDd records in mcf object (124 Filenames)
###### By Filename :
#'                                 Records    Tokens  (unique)  Characters (per token)
#' 2pac_CaliforniaLove.rap [  1]       491      3802    (592)         7514      (1.98)
#' ...                                 ...       ...      ...          ...         ...
#' \item{\link[humdrumR:humReference]{reference}}{Tabulates reference records (metadata) for each file. An example of the first item of such output is below (taken from MCFlow corpus):
#' File Filename                OTL             RTL            RRD        RRM       RC#    BPP BPD        COC  COL           COM                                     CDT                                         ENC                      EED                      RDT
#' 1    2pac_CaliforniaLove.rap California Love All Eyez on Me 1995/12/28 Death Row 854652 6   1996/06/22 2Pac Dr. Dre; 2Pac Andre Romelle Young; Tupac Amaru Shakur 1965/02/18->2016/01/; 1971/06/16-1996/09/13 Nathaniel Condit-Schultz Nathaniel Condit-Schultz 2014/-2016/
#' ...                     ...             ...            ...        ...       ...    ... ...        ...  ...           ...                                     ...                                         ...                      ...                      ... 
#' }
#' \item{\link[humdrumR:humPaths]{spines}}{Tabulates the number of spines and spine paths in files in the corpus. An example of the first item of such output is below 
#' (taken from MCFlow corpus):
#' File Filename                Spines Columns Splits Slices Where
#' 1    2pac_CaliforniaLove.rap 8      8       0      0      c(0, 0, 0, 0, 0, 0, 0, 0)
#' ...  ...                     ...    ...     ...    ...    ...
#' }
#' \item{\link[humdrumR:humInterpretations]{interpretations}}{Tabulates the types of exclusive and tandem interpretations in the corpus. An example of such output is below
#' (taken from MCFlow corpus):
#' }
#' \item{\link[humdrumR:humSections]{sections}}{Tabulates any formal data (\code{"*>"}) in the corpus. An example of such output is below (taken from MCFlow corpus):
#' }
#' \item{summary}{Calls all of the above functions, and prints
#' a condensed version of each.}
#'                 Summary of humdrumR object
#'         Token Census:
#' 
#' ###### humdrumR census of GLIMDd records in object object (124 Filenames)
#' ###### Totals:
#'                                  Records    Tokens (unique)   Characters (per token)
#'                                   82,643   646,744 (13,177)    1,178,769      (1.82)
#'         Reference Records:
#'         ...
#'         ...
#'         ...
#' }
#' Each function takes a \code{\linkS4class{humdrumR}} object and returns a
#' special class of data.table.
#' @name humSummary
#' @export 
setMethod('summary', 'humdrumR',
          function(object, ...) {
            cat('\t\tSummary of humdrumR object:\n\n')
            
            
            funcs <- c(`Token Census` = census, `Reference Records` = reference, 
                       `Spine Structure` = spines, `Intepretation Content` = interpretations) #, sections)
            
            summaries <- lapply(funcs, function(f) f(object, ...))
            
            for (i in seq_along(summaries)) {
              cat('\t', names(funcs)[i], ':\n', sep ='')
              print(summaries[[i]], showall = FALSE)
              cat('\n')
            }
            invisible(summaries)
          })


######## Census ----

#' Tabulate records and tokens in a humdrumR corpus
#' 
#' \code{census} is one of \code{\link[humdrumR:humdrumR]{humdrumR's}}
#' \code{\link[humdrumR:humSummary]{summary functions}}, used to
#' tabulate the raw size of a \code{\linkS4class{humdrumR}} corpus.
#' \code{census} takes a \code{\linkS4class{humdrumR}} object
#' and and returns a \code{\strong{humCensus}} table.
#' The \code{dataType} argument controls what types of records to tabulate:
#' legal values are \code{'G', 'L', 'I', 'M', 'D', 'd'} 
#' or any combination of these (e.g., \code{"LIM"}).
#' The default is \code{"D"}.
#' 
#' A \code{humCensus} table has five columns of information:
#' \describe{
#' \item{Records}{The total number of records.}
#' \item{Tokens}{The total number of tokens.}
#' \item{(unique)}{The number of \strong{unique} tokens}
#' \item{Characters}{The total numbder of characters. This includes
#' humdrum control characters like \code{"*"} and \code{"!!"}.}
#' \item{(per token)}{This is simply \code{Characters / Tokens},
#' indicating the mean length of each token.}
#' }

#' A \code{humCensus} table has one row for each file in the corpus.
#' Rows are labeled with each file's corresponding 
#' number (from the \code{\link[humdrumR:humTable]{humTable's}} \strong{File} field)
#'  and name (the \strong{Filename} field).
#' In addition, when a \code{humCensus} object is printed,
#' the totals across all files are printed as well---(unique) and (per token)
#' across all files are calculated across all files as well, not summed.
#'  
#' @section Indexing:
#' Rows of a \code{humCensus} object can be selected with a single argument \code{i}:
#' e.g., \code{censusTable[i]}.
#' If \code{i} is \code{numeric}, the corresponding rows are selected ordinally (not by 
#' \code{File} number).
#' If \code{i} is a \code{character} string, this string is mached 
#' as a regular expression against file names.
#' If \code{i} is a formula, the right-hand side of the formula
#' is evaluated within the table---if it evaluates to a logical vector,
#' files are selected accordingly. For instance,
#' \code{censusTable[~Tokens > 100]} will select all files
#' with more than 100 tokens. (The '(unique)' and '(per token)' columns
#' must be referred to with their names enclosed in \code{``}---for example,
#' \code{censusTable[~`(unique)` > 100]} will return all files with
#' more than 100 unique tokens.
#' 
#' A \code{drop} argument is also available. If \code{TRUE}, a plain 
#' \code{\link[data.table]{data.table}} is returned.
#' 
#' @name humCensus
#' @export
census <- function(humdrumR, dataTypes = 'GLIMDd', by = 'Filename') {
  corpusName <- substitute(humdrumR)
  corpusName <- if (is.call(corpusName))  NULL else deparse(corpusName)
  
  checkhumdrumR(humdrumR, 'census')
  dataTypes <- checkTypes(dataTypes, 'census')
  
  humdrumR <- removeEmptyFiles(humdrumR)
  
  ## This function creates a data.table of class humCensus
  humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
  
  
  ##ADD MULTISTOPS, MOVE BARS TO sections FUNCTION
  
  censusTable <- humtab[ , .(
    Records          = length(unique(Record[Stop == 1L | is.na(Stop)])),
    Tokens           = nrow(.SD),
    `(unique)`       = list(unique(Token)),
    Characters       = sum(nchar(Token)),
    `(per token)`    = round(mean(nchar(Token)), 2)
  ),
  # Bars             = length(unique(Bar))),
  by = by]
  
  
  attr(censusTable, 'corpusName') <- corpusName
  attr(censusTable, 'dataTypes')  <- paste(dataTypes, collapse = '')
  attr(censusTable, 'by') <- by
  censusTable %class% 'humCensus'
}

#' @name humCensus
#' @usage census(humdata)[i]
#' @export
`[.humCensus` <- function(censusTable, i, drop = FALSE) {
  if (missing(i)) return(if (drop) popclass(censusTable) else censusTable)
  
  corpusName <- attr(censusTable, 'corpusName')          
  dataTypes  <- attr(censusTable, 'dataTypes')          
  
  if (rlang::is_formula(i)) expr <- rlang::f_rhs(i)
  if (is.character(i)) expr <- call('grepl', quote(i), as.symbol(attr(censusTable, 'by')))
  if (is.numeric(i)) expr <- quote(i)
  
  censusTable <- popclass(censusTable)
  indexpr <- call('[', quote(censusTable), expr)
  censusTable <- eval(indexpr)
  
  
  if (!drop) censusTable <- censusTable %class% 'humCensus' 
  
  
  corpusName <- paste0(corpusName, '[i]')
  attr(censusTable, 'corpusName') <- corpusName
  attr(censusTable, 'dataTypes')  <- dataTypes
  attr(censusTable, 'by') <- by
  censusTable
}


#' @name humCensus
#' @export
print.humCensus <- function(censusTable, showall = TRUE) {
  
  corpusName <- attr(censusTable, 'corpusName')
  dataTypes  <- attr(censusTable, 'dataTypes')
  
  censusTable <- data.table::copy(popclass(censusTable))
  
  nfiles <- nrow(censusTable)
  
  if (nfiles < 1L) {
    cat('Empty humdrum corpus.\n') 
    return(invisible(NULL))
  }
  
  ##
  by <- attr(censusTable, 'by')
  files <- censusTable[ , paste0(get(by), ' [', num2str(seq_along(get(by)), pad = TRUE), ']')]
  censusTable[ , get('by') := NULL] # in place!
  
  #
  sums <- censusTable[, lapply(.SD,
                               function(col) {
                                 if (is.list(col)) { # for Unique Column
                                   length(unique(unlist(col)))
                                 } else {
                                   sum(col, na.rm = TRUE)
                                 }
                               })] 
  pertoken <- paste0('(', round(sums$Characters / sums$Tokens, 2), ')')
  sums   <- c("", num2str(unlist(sums[ , 1:4, with = FALSE]), pad = FALSE), pertoken)
  sums['(unique)'] <- paste0("(", sums['(unique)'], ")")
  
  ## append unique counts as parenthetical to total counts
  censusTable[ , '(unique)'    := paste0('(', lengths(`(unique)`), ')')] # in place!
  censusTable[ , '(per token)' := paste0('(', `(per token)`, ')')] # in place!
  
  #
  censusTable <- cbind(files, censusTable)
  colNames    <- colnames(censusTable)
  colNames[1] <- "" # don't print "Filename" as header
  
  ## how wide does each column need to be to accomodate longest strings in each column?
  lenCol <- pmax(nchar(colNames),
                 sapply(censusTable, max %.% nchar),
                 nchar(sums))
  lenCol[colNames %in% c("Records", "Tokens", "Characters")] <- lenCol[colNames %in% c("Records", "Tokens", "Characters")] + 3L
  lenCol[colNames %in% c('(unique)', '(per token)')] <- lenCol[colNames %in% c('(unique)', '(per token)')] + 1L
  
  
  
  # Corpus message (name and n files)
  corpusMessage <- paste0("\n###### humdrumR census of ",
                          dataTypes, ' records',
                          if (is.null(corpusName)) "" else glue::glue( " in {corpusName} object"), 
                          glue::glue(" ({num2print(nfiles, by)})"),
                          '\n')
  
  colNames_str <- padder(colNames, lenCol)
  ## PRINTING BEGINS:
  
  if (showall) {
    cat(corpusMessage)
    cat("###### By", by, ":\n")
    cat(colNames_str, '\n', sep = '')
    # 
    censusTable[, cat(paste(padder(unlist(.SD), lenCol), collapse = ''), '\n', sep = ''), by = seq_len(nfiles)]
    if (nfiles > 10L ) cat(colNames_str, '\n', sep = '')
    
  }
  
  if (!showall || nfiles > 10L) cat(corpusMessage) else cat('\n')
  cat("###### Totals:\n")
  if (!showall) cat(colNames_str, '\n', sep = '')
  cat(padder(sums, lenCol), '\n', sep = '') #sums
  
  invisible(NULL)
}


#### Reference ----

#' Summarize reference records in a humdrumR corpus
#' 
#' \code{reference} is one of \code{\link[humdrumR:humdrumR]{humdrumR's}}
#' \code{\link[humdrumR:humSummary]{summary functions}}, used to
#' tabulate the reference records
#' present in a \code{\linkS4class{humdrumR}} corpus.
#' \code{reference} takes a \code{\linkS4class{humdrumR}} object
#' and and returns a \code{\strong{humReference}} table.
#' Alternatively, \code{reference} can take a \code{character} string,
#' which it will check against known reference codes and print a
#' information about matching codes. For instance, \code{reference('OTL')}
#' returns a description of the standard humdrum \code{!!!OTL} reference record
#' (original title metadata).
#' 
#' A \code{humReference} table has one column for 
#' each reference code that appears in a \code{\linkS4class{humdrumR}} corpus.
#' Since reference records can be long (too much to print on one screen),
#' and humdrum files can have multiple of the same type of reference code
#' (for instance multiple composers annotated with "!!!COM"),
#' by default, a \code{humReference} only prints the number of each type of 
#' reference record to appear in each file.
#' However, if only one type of reference code is present in a
#' \code{humReference} table, the complete reference records for that code
#' will be printed for each file. Likewise, if only one file is present
#' in the table, all of that file's complete reference records are printed.
#' Thus, if you want to see actualy reference records, try indexing the
#' \code{humReference} table down to one column or row (see below).
#' 
#' A \code{humReference} table has one row for each file in the corpus.
#' Rows are labeled with each file's corresponding 
#' number (from the \code{\link[humdrumR:humTable]{humTables}} \strong{File} field)
#'  and name (the \strong{Filename} field).
#' In addition, when a \code{humReference} object is printed,
#' three different summary totals are printed for each reference code:
#' \strong{Any} indicates how many files in the corpus have at least
#' one example of each code in them. 
#' \strong{Sum} indicates the total number of each reference code to appear
#' in the corpus, including multiple appearances in one file (like multiple "!!!COM"
#' records).
#' Finally, \strong{Unique} tabulates the number of unique tokens in each reference
#' code---if your corpus only hase two unique composers (encoded in "!!!COM"),
#' the \strong{Unique} total will be \code{2}.
#' 
#' @section Indexing:
#' 
#' \code{humReference} tables can be indexed much like base \code{R}
#' \code{\link[base:data.frame]{data.frames}}, with two arguments: \code{i} (rows)
#' and \code{j} (columns).
#' If \code{i} or {j} are \code{numeric}, they select
#' rows or columns respectively, ordinally.
#' If \code{i} is a \code{character}, it is matched as a regular expression
#' against filenames in the corpus.
#' If \code{j} is a \code{character}, it is \code{\link[base:pmatch]{partially-matched}}
#'  against column names.
#' 
#' A \code{drop} argument is also available. If \code{TRUE}, a plain 
#' \code{\link[data.table]{data.table}} is returned.
#'       
#' @name humReference
#' @export
reference <- function(x) UseMethod('reference')


#' @name humReference
#' @usage reference('OTL')
#' @export
reference.character <- function(str) {
  str <- gsub('^!*', '', str)
  
  ReferenceCodes[] <- lapply(ReferenceCodes, as.character)
  hits <- ReferenceCodes[rownames(ReferenceCodes) %in% str, ]
  
  if (nrow(hits) == 0) {
    cat("Unknown reference code\n")
    return(invisible(NULL))
  } 
  
  lapply(unique(hits$Category),
         function(Cat) {
           curhits <- hits[hits$Category == Cat, ]
           
           cat('\n(', Cat, ')\n\n', sep = '')
           
           for(i in seq_len(nrow(curhits))) {
             cat('\t!!!', curhits$Code[i], 
                 '  =  ', curhits$Brief[i], '\n', sep = '')
             if (length(curhits$Description[[i]]) > 0) {
               cat('\n\t', 'Details:\n', sep = '')
               descrip <- curhits$Description[[1]]
               descrip <- strsplit(descrip, split = '[.] ')[[1]]
               
               cat(paste0('\t\t', descrip, '.\n'), sep = '')
               cat('\n')
             }
             
             if (length(curhits$Examples[[i]]) > 0) {
               cat('\n\t', 'Examples:\n', sep = '')
               cat(paste0('\t\t\t', curhits$Examples[[i]], '\n'), sep = '')
               cat('\n')
             }
             
           }
         })
  
  return(invisible(hits))
}

#' @name humReference
#' @usage reference(humdata)
#' @export
reference.humdrumR <- function(humdrumR) {
  # This funtion simply extracts the refernence columns from a humdrumR object
  corpusName <- substitute(humdrumR)
  corpusName <- if (is.call(corpusName))  NULL else deparse(corpusName)
  
  humtab <- getHumtab(humdrumR)
  fieldtable <- fields(humdrumR, fieldTypes = 'Reference')
  
  refTable <- humtab[ , c('File', 'Filename', fieldtable$Name), with = FALSE]
  # 
  
  refTable <- refTable[!duplicated(Filename)]
  
  attr(refTable, 'corpusName') <- corpusName
  refTable %class% 'humReference'
}


#' @name humReference
#' @export
`[.humReference` <- function(refTable, i, j, drop = FALSE) {
  if (missing(i) && missing(j)) return(if (drop) popclass(refTable) else refTable)
  
  corpusName <- attr(refTable, 'corpusName')          
  
  
  # The first two columns of the refTable are the file number name,
  # but for users we want them to be treated like rownames...
  # Thus j is +- 2 
  
  corpusName <- paste0(corpusName, 
                       '[', 
                       if (!missing(i)) 'i', 
                       if (!missing(j)) ', j',
                       ']')
  if (missing(j)) j <- seq_len(ncol(refTable) - 2L)
  if (missing(i)) i <- seq_len(nrow(refTable)) 
  
  if (is.character(j)) j <- pmatch(j, colnames(refTable)) - 2L
  if (is.character(i)) i <- grep(i, refTable$Filename)
  
  j <- j[j <= (ncol(refTable) - 2L)]
  
  refTable <- popclass(refTable)
  refTable <- refTable[i, c(1, 2, j + 2), with = FALSE]
  
  if (!drop) refTable <- refTable %class% 'humReference' 
  
  attr(refTable, 'corpusName') <- corpusName
  
  refTable
  
}


#' @name humReference
#' @usage NULL
#' @export
print.humReference <- function(refTable, showall = TRUE) {
  corpusName <- attr(refTable, 'corpusName')
  
  refTable <- data.table::copy(popclass(refTable))
  
  nfiles <- nrow(refTable)
  
  if (nfiles < 1L) {
    cat('Empty humdrumR object.\n') 
    return(invisible(NULL))
  }
  
  ##
  files <- paste0(refTable$Filename, " [", num2str(refTable$File, pad = TRUE), "]")
  refTable[ , c('File', 'Filename') := NULL] # in place!
  
  ######### -
  corpusMessage <- paste0("\n###### Reference records in humdrumR corpus ",
                          if (is.null(corpusName)) "" else glue::glue( "{corpusName}"), 
                          glue::glue(" ({num2print(nfiles, 'file')})"),
                          '\n')
  
  # If only one file, show actual reference records,
  # as they appear in the file
  # and then return invisible
  if (nrow(refTable) == 1L) {
    cat(corpusMessage)
    cat(files, '\n', sep = '')
    
    refTable <- refTable[ , lapply(.SD, function(col) {
      if (is.list(col)) { col <- do.call('paste', 
                                         c(col, 
                                           collapse = paste0('\n\t', 
                                                             stringr::str_dup(' ', max(nchar(colnames(refTable)))),
                                                             '   ')))
      }
      if (is.null(col) || all(is.na(col))) NULL else col
    })]
    
    colNames <- colnames(refTable)
    colNames <- padder(colNames, max(nchar(colNames)) + 1L)
    
    cat(paste(paste0('\t', colNames, ': ', unlist(refTable))), sep = '\n')
    return(invisible(NULL))
  } 
  
  # If more than one file, print the number of reference records (by code)
  # in each file
  codeCounts <- refTable[ , lapply(.SD, 
                                   function(col) {
                                     col <- if (is.list(col)) lengths(col) else ifelse(is.na(col), 0, 1)
                                     col
                                   })]
  
  colNames <- colnames(codeCounts)
  
  ###Totals
  Totals <- list(`Any:` = sapply(codeCounts, function(col) num2str(sum(!is.na(col) & col > 0L))),
                 `Sum:` = sapply(codeCounts, num2str %.% sum, na.rm = TRUE),
                 `Unique:` = sapply(refTable, function(col) num2str(length(unique(col[!is.na(col)])))))
  
  ### Column widths
  lenCol <- do.call('pmax',
                    c(nchar(colNames),
                      sapply(codeCounts, max %.% nchar),
                      lapply(Totals, nchar))) + 1L # plus one to add space between lines
  
  # append filename, plus totals categories
  colNames   <- c("", colNames) # don't print "Filename" as a header
  lenCol     <- c(max(nchar(c(files, names(Totals)))), lenCol)
  
  ## If there is only one column
  oneColumn <- ncol(codeCounts) == 1L
  if (oneColumn) {
    lenCol <- c(lenCol[1] + 1L, 0L)
    files <- paste0(files, '  ')
    names(Totals) <- paste0(names(Totals), '  ')
  }
  
  colNames_str <- padder(colNames, lenCol)
  ## PRINTING BEGINS:
  
  if (showall) {
    cat(corpusMessage)
    cat("###### By file:\n")
    
    cat(colNames_str, '\n', sep = '')
    tab <- cbind(files, if (oneColumn) refTable else codeCounts)
    tab[, cat(paste(padder(sapply(.SD, paste, collapse = ', '), lenCol), collapse = ''), '\n', sep = ''), by = seq_len(nfiles)]
    
    if (nfiles > 10L) cat(colNames_str, '\n', sep = '')
  }
  
  if (!showall || nfiles > 10L) {
    cat(corpusMessage) 
  } else {
    cat('\n')
  }
  cat("###### Totals:\n")
  if (!showall) cat(colNames_str, '\n', sep = '')
  
  Map(function(tot, totname) {
    cat(padder(c(totname, tot), lenCol), '\n', sep = '')
  },
  Totals, names(Totals)) 
  invisible(NULL)
}

#### Spines ----

spines  <- function(humdrumR) {
  #' Summarize humdrum corpus spine paths.
  #' 
  #' This function provides summary of the spines and spine paths in the pieces of a humdrumR corpus.
  #' @export
  humtab <- getHumtab(humdrumR)
  
  spins <- humtab[Global == FALSE , 
                  .(File            = unique(File),
                    Spines           = length(unique(Spine)),
                    Columns          = length(unique(Column)),
                    Splits           = sum(grepl('\\*\\^', Token)),
                    Splices          = length(unique(Record[grepl('\\*v', Token)])),
                    Where            = list(.SD[ , length(unique(Path)) - 1, by = Spine]$V1)),
                  by = Filename]
  
  setcolorder(spins, c('File', 'Filename', 'Spines', 'Columns', 'Splits', 'Splices', 'Where'))
  
  spins %class% 'humSpines'
}


#' @export
`[.humSpines` <- function(spines, i, j) {
  if (missing(i) && missing(j)) return(spines)
  
  if (missing(j)) j <- seq_len(ncol(spines) - 2)
  if (missing(i)) i <- seq_len(nrow(spines)) 
  
  if (is.character(j)) j <- pmatch(j, colnames(spines)) - 2
  if (is.character(i)) i <- pmatch(i, spines$Filename)
  
  spines <- popclass(spines)
  spines <- spines[i, c(1, 2, j + 2), with = FALSE]
  
  spines %class% 'humSpines'
}



print.humSpines <- function(spinemat, showall = TRUE) {
  #' @export
  if (nrow(spinemat) < 1) {cat('Empty humdrumR object.\n') ; return(invisible(NULL))}
  
  spinemat <- popclass(spinemat)
  spinemat$File <- paste0(num2str(spinemat$File), ":")
  spinemat[ , In := sapply(Where, function(x) sum(x > 0))]
  where <- spinemat$Where
  spinemat[ , 'Where' := NULL]
  spinemat[ , Columns := Columns - Spines]
  anypaths <- any(spinemat$Columns > 0)
  
  
  if (nrow(spinemat) == 1) {
    cat(spinemat$Filename, ': ', spinemat$Spines, ' spines', if (anypaths) paste0(' : ', spinemat$Columns, {if ( spinemat$Columns > 1) ' paths' else ' path'}) else '', '\n', sep = '')
    where <- where[[1]]
    
    if (anypaths) {
      cat(paste0('\tSpine ', 1:spinemat$Spines, ' : ', ifelse(where == 0, '', where), '\n'), sep = '')
    }
    
  } else {
    setcolorder(spinemat,
                c('File', 'Filename', 'Spines',
                  'Columns', 'In', 'Splits', 'Splices'))
    
    
    cols <- if (anypaths) 1:7 else 1:3
    colNames <- c('', 'Filename', 'Spines', '+ Paths', 'In', '*^', '*v')[cols]
    
    lenCol <- pmax(c(0,8,0,0,0,0,0)[cols], #Tallies: is 8 long
                   nchar(colNames), sapply(spinemat[ , cols, with = FALSE], max %.% nchar)) + 2
    
    if (showall) {
      cat(padder(colNames, lenCol), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
      spinemat[ , { row <- unlist(.SD)
      if (Columns == 0) row[4:7] <- ' '
      paste(padder(row[cols], lenCol), collapse = '') 
      }, 
      by = 1:nrow(spinemat)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
    }
    
    cat(padder(colNames, lenCol), '\n\n', sep = '')
    
    #
    cat(padder(c('Tallies:'), sum(lenCol[1:2])), '\n', sep = '')
    
    tab <- spinemat[ , table(Spines, Columns)]
    for (i in 1:nrow(tab)) {
      row <- c('', sum(tab[i, ]), paste0('with ', (rownames(tab)[i])))
      row <- padder(row, lenCol[1:3])
      if (anypaths) {
        notzero <- which(tab[i, ] > 0)
        row <- c(row, ' (', glue::collapse(paste0(tab[i, notzero], '*',  colnames(tab)[notzero]), sep = ' paths, ', last = ', and '), ')')
      }
      cat(row, '\n', sep = '')
    }
    
  }
  invisible(NULL)
}


#### Interpretations ----



#' Summarize humdrum corpus interpretations.
#' 
#' This function provides a summary of the interpretations in the pieces of a humdrumR corpus.
#' @name humInterpretations
#' @export
interpretations <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'interpretations')
  
  # humdrumR <- indexGLIM(humdrumR, dataTypes = 'I')
  humtab <- getHumtab(humdrumR, dataTypes = 'I')
  
  # Tandem
  tandem <- humtab[!grepl('^\\*\\*', Token) &   
                     !Token %in% c('*', '*-', '*v', '*^') &
                     !grepl('\\*>.*', Token)]
  tandem[ , ID := factor(idTandem(Token))]
  tandemN  <- do.call('rbind', tandem[, .(list(table(ID))), by = File]$V1)
  rownames(tandemN) <- unique(humtab$Filename)
  tandemUN <- do.call('rbind', tandem[, .(list(tapply(Token, ID, length %.% unique))), by = File]$V1)
  tandemUN[is.na(tandemUN)] <- 0L
  
  tandIDs  <- levels(tandem$ID)
  tandemIn <- do.call('rbind', tandem[, .(list(colSums(tapply(ID, list(Spine,ID), length), na.rm = TRUE))), by = File]$V1)
  
  
  # Exclusive
  exclusive <- humtab[grepl('^\\*\\*', Token)]
  exclusive$Token <- factor(exclusive$Token)
  exclusiveN <- do.call('rbind', exclusive[ , .(list(table(Token))), by = File]$V1)
  rownames(exclusiveN) <- unique(humtab$File)
  
  list(Exclusive = exclusiveN, 
       Tandem    = list(Number    = tandemN, 
                        NUnique   = tandemUN,
                        InNSpines = tandemIn)) %class% 'humInterpretations'
  
}

print.humInterpretations <- function(interps, showall = TRUE) {
  #' @export
  if (nrow(interps[[1]]) < 1 || any(sapply(interps[[2]], nrow) < 1)) { cat('No interpretations.\n') ; return(invisible(NULL))}
  
  tandems <- interps$Tandem[[1]]
  tandems[] <- do.call('paste', c(sep = '.', interps$Tandem))
  
  Nexclusive <- ncol(interps$Exclusive)
  
  interpmat <- data.table(interps$Exclusive, tandems)
  interpmat$File <- paste0(num2str(rownames(interps$Exclusive)), ":")
  interpmat$Filename  <- rownames(interps$Tandem$Number)
  setcolorder(interpmat, c('File', 'Filename', head(colnames(interpmat), -2)))
  
  if (nrow(interpmat) == 1) {
    cat(interpmat$File, '\n')
    cat("Exclusive: ")
    interps$Exclusive[ , cat(Exclusive), by = Spine]
    cat('\n')
    cat("Tandems:")
    interps$Tandem[ , cat(paste0(colnames(.SD), '\n')), by = Spine]
    
    
  } else  {
    colNames <- c('', 'Filename', colnames(interpmat)[-1:-2])
    colKeys <- character(length(colNames))
    colKeys[3 + Nexclusive] <-   '(Total.Unique.Spines)'
    
    lenCol <- pmax(interpmat[ , sapply(.SD, max %.% nchar)], 
                   nchar(colnames(interpmat))) + 2
    # lenCol[2] <- max(8, lenCol[2]) # to make enough room for "Unique:"
    # lenCol[3] <- max(9, lenCol[3]) # to make enough room for "Unique:"
    
    if (showall) {
      cat(padder(colNames, lenCol), '\n', sep = '')
      cat(padder(colKeys, lenCol), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
      interpmat[ , { row <- unlist(.SD)
      paste(padder(row, lenCol), collapse = '') 
      }, 
      by = 1:nrow(interpmat)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
    }
    
    cat(padder(colNames, lenCol), '\n', sep = '')
    cat(padder(colKeys, lenCol), '\n\n', sep = '')
    cat(padder(c('',
                 'Hits:',
                 sapply(as.list(interpmat)[2 + seq_len(Nexclusive)],   function(col) sum(col > 0)),
                 colSums(interps$Tandem$NUnique  > 0)),
               lenCol), '\n', sep = '')
    # cat(padder(c('', 'Total:', sapply(as.list(interpmat)[-1:-2], sum)), lenCol), '\n', sep = '')
    # cat(padder(c('', '', 'Unique:', sapply(interps$Tandem, function(col) length(unique(col[!is.na(col)])))), lenCol), '\n', sep = '')
    
    
    
    
  }
  
}