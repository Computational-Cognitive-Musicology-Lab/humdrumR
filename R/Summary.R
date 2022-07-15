#######################################################---
## Functions for summarizing humdrumR objects----
######################################################---

#' Summarize humdrumR corpora
#' 
#' [humdrumR] includes a number of built in functions for creating quick summaries of [humdrumR][humdrumRclass]
#' corpora:
#'
#' 1. [census][humCensus()]
#'     + Tabulates the raw size of the humdrumR corpus.
#' 2. [reference][humReference()]
#'     + Tabulates reference records (metadata) for each file.
#' 3. [spines][humSpines()]
#'     + Tabulates the number of spines and spine paths in files in the corpus.
#' 4. [interpretations][humInterpretations()]
#'     + Tabulates the types of exclusive and tandem interpretations in the corpus.
#' 5. [sections][humSections()]
#'     + Tabulates any formal data (`*>`) in the corpus.
#'
#'
#' Each function takes a [humdrumR][humdrumRclass] object and returns a
#' special class of data.table.
#' The `summary` method for [humdrumR objects][humdrumRclass] calls all of the above functions and prints a condensed version of each.
#'
#' @name humSummary
#' @family humdrum data summary functions
#' @export 
setMethod('summary', 'humdrumR',
          function(object, ...) {
            cat('\t\tSummary of humdrumR object:\n\n')
            
            
            funcs <- c(`Token Census` = census, `Reference Records` = reference, 
                       `Spine Structure` = spines, `Intepretation Content` = interpretations) #, sections)
            
            summaries <- lapply(funcs, \(f) f(object, ...))
            
            for (i in seq_along(summaries)) {
              # cat('\t', names(funcs)[i], ':\n', sep ='')
              print(summaries[[i]], showEach = FALSE)
              cat('\n')
            }
            invisible(summaries)
          })


######## Census ----

#' Tabulate records and tokens in a humdrumR corpus
#' 
#' `census` is one of [humdrumR]'s
#' [summary functions][humSummary], used to
#' tabulate the raw size of a [humdrumR][humdrumRclass] corpus.
#' `census` takes a [humdrumR][humdrumRclass] object
#' and and returns a `humCensus` table.
#' The `dataType` argument controls what types of records to tabulate:
#' legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' The default is `"D"`.
#' 
#' A `humCensus` table has five columns of information:
#' 
#' 1. Records
#'     + The total number of records.
#' 2. Tokens
#'     + The total number of tokens.
#' 3. (unique)
#'     + The number of **unique** tokens
#' 4. Characters
#'     + The total numder of characters. This includes humdrum control characters like `*` and `!!`.
#' 5. (per token)
#'     + This is simply `Characters / Tokens`, indicating the mean length of each token.
#' 
#' A `humCensus` table has one row for each file in the corpus.
#' Rows are labeled with each file's corresponding 
#' number (from the [humTable's][humTable] **File** field) and name (the **Filename** field).
#' In addition, when a `humCensus` object is printed,
#' the totals across all files are printed as well---(unique) and (per token)
#' across all files are calculated across all files as well, not summed.
#'  
#' @param humdrumR A humdrumR object
#' @param dataTypes A `character` string of length 1. 
#' @param by A `character` string of length 1. Must be a [field][humdrumRclass] in the humdrumR object.
#'  
#'  
#' @section Indexing:
#'
#' Rows of a `humCensus` object can be selected with a single argument `i`: e.g., `censusTable[i]`.
#' If `i` is `numeric`, the corresponding rows are selected ordinally (not by `File` number).
#' If `i` is a `character` string, this string is mached as a regular expression against file names.
#' If `i` is a formula, the right-hand side of the formula is evaluated within the table---if it evaluates to a logical vector,
#' files are selected accordingly. For instance,
#' `censusTable[~Tokens > 100]` will select all files
#' with more than 100 tokens. (The '(unique)' and '(per token)' columns
#' must be referred to with their names enclosed in `\``---for example,
#' `censusTable[~\`(unique)\` > 100]` will return all files with
#' more than 100 unique tokens.
#' 
#' A `drop` argument is also available. If `TRUE`, a plain 
#' [data.table::data.table()] is returned.
#' 
#' @name humCensus
#' @family humdrum data summary functions
#' @export
census <- function(humdrumR, dataTypes = 'GLIMDd', by = 'Filename', removeEmpty = FALSE) {
  ## This function creates a data.table of class humCensus
  corpusName <- substitute(humdrumR)
  corpusName <- if (is.call(corpusName))  NULL else deparse(corpusName)
  
  ## ARGUMENT CHECKS:
  checkhumdrumR(humdrumR, 'census')
  dataTypes <- checkTypes(dataTypes, 'census')
  checkTF(removeEmpty, 'removeEmpty', 'census')
  
  ##
  
  if (removeEmpty) humdrumR <- removeEmptyFiles(humdrumR)
  
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
  by <- attr(censusTable, 'by')
  
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
print.humCensus <- function(censusTable, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  
  
  censusTable <- data.table::copy(popclass(censusTable))
  nfiles <- nrow(censusTable)
  
  # Corpus message (name and n files)
  corpusMessage <- paste0("\n###### humdrumR census of ",
                          attr(censusTable, 'dataTypes'), ' records in humdrumR corpus "',
                          attr(censusTable, 'corpusName'), 
                          '" (', num2print(nfiles, attr(censusTable, 'by')), ")\n")
  
  
  
  if (nfiles < 1L) {
    cat('Empty humdrum corpus.\n') 
    return(invisible(NULL))
  }
  
  ##
  by <- attr(censusTable, 'by')
  files <- censusTable[ , paste0(trimTokens(get(by), 70L), ' [', num2str(seq_along(get(by)), pad = TRUE), ']')]
  censusTable[ , get('by') := NULL] # in place!
  
  #
  sums <- censusTable[, lapply(.SD,
                               \(col) {
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
                 sapply(censusTable, \(x) max(nchar(x))),
                 nchar(sums))
  lenCol[colNames %in% c("Records", "Tokens", "Characters")] <- lenCol[colNames %in% c("Records", "Tokens", "Characters")] + 3L
  lenCol[colNames %in% c('(unique)', '(per token)')] <- lenCol[colNames %in% c('(unique)', '(per token)')] + 1L
  
  
  colNames_str <- padder(colNames, lenCol)
  
  # shrink to screenWidth
  screen <- cumsum(lenCol) <= screenWidth
  colNames <- colNames[screen]
  lenCol <- lenCol[screen]
  colNames_str <- padder(colNames, lenCol)
  stars <- if (any(!screen)) "    ***" else ""
  
  ## PRINTING BEGINS:
  
  cat(corpusMessage)
  if (showEach) {
    cat("###### By", by, ":\n")
    cat(colNames_str, stars, '\n', sep = '')
    # 
    censusTable[, cat(paste(padder(unlist(.SD)[screen], lenCol), collapse = ''), stars, '\n', sep = ''), by = seq_len(nfiles)]
    if (nfiles > 10L ) cat(colNames_str, stars, '\n', sep = '')
    
  }
  
  
  cat("###### Totals:\n")
  if (!showEach) cat(colNames_str, stars, '\n', sep = '')
  cat(padder(sums[screen], lenCol), stars,  '\n', sep = '') #sums
  
  if (showEach && nfiles > 10L) cat(corpusMessage) else cat('\n')
  
  if (stars != '') {
    cat('\n') 
    extraCols <- paste0('(***', 
                        num2word(sum(!screen)),
                        plural(sum(!screen), ' columns', ' column'), ' not displayed due to screensize',
                        '***)')
    extraCols <- stringr::str_pad(extraCols, width = sum(lenCol) + 8L, side = 'left')
    cat(extraCols, '\n', sep = '')
  }
  
  invisible(NULL)
}


#### Reference ----

#' Summarize reference records in a humdrumR corpus
#' 
#' `reference` is one of [humdrumR]'s
#' [summary functions][humSummary], used to
#' tabulate the reference records
#' present in a [humdrumR][humdrumRclass] corpus.
#' `reference` takes a [humdrumR][humdrumRclass] object
#' and and returns a `humReference` table.
#' Alternatively, `reference` can take a `character` string,
#' which it will check against known reference codes and print a
#' information about matching codes. For instance, `reference('OTL')`
#' returns a description of the standard humdrum `!!!OTL` reference record
#' (original title metadata).
#' 
#' A `humReference` table has one column for 
#' each reference code that appears in a [humdrumR][humdrumRclass] corpus.
#' Since reference records can be long (too much to print on one screen),
#' and humdrum files can have multiple of the same type of reference code
#' (for instance multiple composers annotated with "!!!COM"),
#' by default, a `humReference` only prints the number of each type of 
#' reference record to appear in each file.
#' However, if only one type of reference code is present in a
#' `humReference` table, the complete reference records for that code
#' will be printed for each file. Likewise, if only one file is present
#' in the table, all of that file's complete reference records are printed.
#' Thus, if you want to see actualy reference records, try indexing the
#' `humReference` table down to one column or row (see below).
#' 
#' A `humReference` table has one row for each file in the corpus.
#' Rows are labeled with each file's corresponding 
#' number (from the [humTable]'s **File** field) and name (the **Filename** field).
#' In addition, when a `humReference` object is printed,
#' three different summary totals are printed for each reference code:
#' **Any** indicates how many files in the corpus have at least
#' one example of each code in them. 
#' **Sum** indicates the total number of each reference code to appear
#' in the corpus, including multiple appearances in one file (like multiple "!!!COM"
#' records).
#' Finally, **Unique** tabulates the number of unique tokens in each reference
#' code---if your corpus only hase two unique composers (encoded in "!!!COM"),
#' the **Unique** total will be `2`.
#' 
#' @section Indexing:
#' 
#' `humReference` tables can be indexed much like base `R` `[data.frames][base::data.frame()], 
#' with two arguments: `i` (rows) and `j` (columns).
#' If `i` or `j` are `numeric`, they select rows or columns respectively, ordinally.
#' If `i` is a `character`, it is matched as a regular expression
#' against filenames in the corpus.
#' If `j` is a `character`, it is [partially matched][base::pmatch()]
#'  against column names.
#' 
#' A `drop` argument is also available. If `TRUE`, a plain
#' `[data.table][data.table::data.table()]` is returned.
#'       
#' @name humReference
#' @export
reference <- function(x) UseMethod('reference')


#' @rdname humReference
#' @usage reference('OTL')
#' @export
reference.character <- function(str) {
  checkArg(str, classes = c('string'))
  str <- gsub('^!*', '', str)
  
  ReferenceCodes[] <- lapply(ReferenceCodes, as.character)
  hits <- ReferenceCodes[rownames(ReferenceCodes) %in% str, ]
  
  if (nrow(hits) == 0) {
    cat("Unknown reference code\n")
    return(invisible(NULL))
  } 
  
  lapply(unique(hits$Category),
         \(Cat) {
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

#' @rdname humReference
#' @usage reference(humdata)
#' @export
reference.humdrumR <- function(humdrumR) {

  checkhumdrumR(humdrumR, 'reference.humdrumR')
    
  # This function simply extracts the refernence columns from a humdrumR object
  
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
print.humReference <- function(refTable, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  
  refTable <- data.table::copy(popclass(refTable))
  nfiles <- nrow(refTable)
  
  ### 
  corpusMessage <- paste0('\n###### Reference records in humdrumR corpus "',
                          attr(refTable, 'corpusName'),
                          '" (', num2print(nfiles, 'file'), ")\n")
  
  if (nfiles < 1L) {
    cat('Empty humdrumR object.\n') 
    return(invisible(NULL))
  }
  
  ##
  files <- paste0(refTable$Filename, " [", num2str(refTable$File, pad = TRUE), "]")
  refTable[ , c('File', 'Filename') := NULL] # in place!
  
  
  
  # If only one file, show actual reference records,
  # as they appear in the file
  # and then return invisible
  if (nrow(refTable) == 1L) {
    cat(corpusMessage)
    cat(files, '\n', sep = '')
    
    refTable <- refTable[ , lapply(.SD, \(col) {
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
                                   \(col) {
                                     col <- if (is.list(col)) lengths(col) else ifelse(is.na(col), 0, 1)
                                     col
                                   })]
  
  colNames <- colnames(codeCounts)
  
  ###Totals
  Totals <- list(`Any:` = sapply(codeCounts, \(col) num2str(sum(!is.na(col) & col > 0L))),
                 `Sum:` = sapply(codeCounts, \(x) num2str(sum(x, na.rm = TRUE))),
                 `Unique:` = sapply(refTable, \(col) num2str(length(unique(col[!is.na(col)])))))
  
  ### Column widths
  lenCol <- do.call('pmax',
                    c(list(nchar(colNames), sapply(codeCounts, \(x) max(nchar(x)))), lapply(Totals, nchar))) + 2L # plus one to add space between lines
  
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
  
  # shrink to screenWidth size
  screen <- cumsum(lenCol) <= screenWidth
  colNames <- colNames[screen]
  lenCol <- lenCol[screen]
  colNames_str <- padder(colNames, lenCol)
  stars <- if (any(!screen)) "    ***" else ""
  
  ## PRINTING BEGINS:
  cat(corpusMessage)
  if (showEach) {
    cat("###### By file:\n")
    cat(colNames_str, stars, '\n', sep = '')
    
    tab <- cbind(files, if (oneColumn) refTable else codeCounts)[, screen, with = FALSE]
    tab[, cat(paste(padder(unlist(.SD), lenCol), collapse = ''), stars, '\n', sep = ''), by = seq_len(nfiles)]
    # tab[, cat(paste(padder(sapply(.SD, paste, collapse = ', '), lenCol), collapse = ''), '\n', sep = ''), by = seq_len(nfiles)]
    
    if (nfiles > 10L) cat(colNames_str, stars, '\n', sep = '')
    cat('\n')
  }
  
  
  cat("###### Totals:\n")
  if (!showEach) cat(colNames_str, stars, '\n', sep = '')
  
  Map(function(tot, totname) {
    cat(padder(c(totname, tot)[screen], lenCol), stars, '\n', sep = '')
  },
  Totals, names(Totals)) 
  
  if (showEach && nfiles > 10L) cat(corpusMessage) else cat('\n')
  
  
  if (stars != "") {
    cat('\n') 
    extraCodes <- paste0('(', 
                         num2word(sum(!screen)),
                         plural(sum(!screen), ' columns', ' column'), ' not displayed due to screensize',
                         '***)')
    extraCodes <- stringr::str_pad(extraCodes, width = sum(lenCol) + 8L, side = 'left')
    cat(extraCodes, '\n', sep = '')
  }
  
  
  invisible(NULL)
}

#### Spines ----

#' Summarize spines in humdrum dataset
#'
#' This function provides summary of the spines and spine paths in the pieces of a humdrumR corpus.
#'
#' @name humSpines
#' @family humdrum data summary functions
#' @export
spines  <- function(humdrumR) {

  checkhumdrumR(humdrumR, 'spines')
  corpusName <- substitute(humdrumR)
  corpusName <- if (is.call(corpusName))  NULL else deparse(corpusName)
  
  humtab <- getHumtab(humdrumR)
  
  spines <- humtab[Global == FALSE , 
                   .(File            = unique(File),
                     Spines           = length(unique(Spine)),
                     Columns          = length(unique(Column)),
                     Splits           = sum(grepl('\\*\\^', Token)),
                     Splices          = length(unique(Record[grepl('\\*v', Token)])),
                     Where            = list(.SD[ , length(unique(Path)) - 1, by = Spine]$V1)),
                   by = Filename]
  
  setcolorder(spines, c('File', 'Filename', 'Spines', 'Columns', 'Splits', 'Splices', 'Where'))
  
  
  attr(spines, 'corpusName') <- corpusName
  spines %class% 'humSpines'
}


#' @rdname humSpines
#' @export
`[.humSpines` <- function(spines, i, j) {
  if (missing(i) && missing(j)) return(spines)
  
  if (missing(j)) j <- seq_len(ncol(spines) - 2)
  if (missing(i)) i <- seq_len(nrow(spines)) 
  
  if (is.character(j)) j <- pmatch(j, colnames(spines)) - 2
  if (is.character(i)) i <- pmatch(i, spines$Filename)
  
  corpusName <- attr(spines, 'corpusName')
  spines <- popclass(spines)
  spines <- spines[i, c(1, 2, j + 2), with = FALSE]
  
  attr(spines, 'corpusName') <- corpusName
  spines %class% 'humSpines'
}



#' @rdname humSpines
#' @export
print.humSpines <- function(spineTable, showEach = TRUE) {
  nfiles <- nrow(spineTable)
  
  if (nfiles < 1L) {cat('Empty humdrumR object.\n') ; return(invisible(NULL))}
  
  ### 
  corpusMessage <- paste0('\n###### Spine structure in in humdrumR corpus "',
                          attr(spineTable, 'corpusName'),
                          '" (', num2print(nfiles, 'file'), ")\n")
  
  spineTable <- popclass(spineTable)
  spineTable$File <- paste0(num2str(spineTable$File), ":")
  spineTable[ , In := sapply(Where, \(x) sum(x > 0))]
  where <- spineTable$Where
  spineTable[ , 'Where' := NULL]
  spineTable[ , Columns := Columns - Spines]
  anypaths <- any(spineTable$Columns > 0)
  
  
  cat(corpusMessage)
  if (nrow(spineTable) == 1) {
    cat(spineTable$Filename, ': ', spineTable$Spines, ' spines', if (anypaths) paste0(' + ', spineTable$Columns, {if ( spineTable$Columns > 1) ' paths:' else ' path:'}) else '', '\n', sep = '')
    where <- where[[1]]
    
    if (anypaths) {
      cat(paste0('\tSpine ', 1:spineTable$Spines, ' : ', ifelse(where == 0, '', where), '\n'), sep = '')
    }
    
  } else {
    setcolorder(spineTable,
                c('File', 'Filename', 'Spines',
                  'Columns', 'In', 'Splits', 'Splices'))
    
    
    cols <- if (anypaths) 1:7 else 1:3
    colNames <- c('', 'Filename', 'Spines', '+ Paths', 'In', '*^', '*v')[cols]
    
    lenCol <- pmax(c(0,8,0,0,0,0,0)[cols], #Tallies: is 8 long
                   nchar(colNames), sapply(spineTable[ , cols, with = FALSE], \(x) max(nchar(x)))) + 2L
    
    if (showEach) {
      cat(padder(colNames, lenCol), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
      spineTable[ , { row <- unlist(.SD)
      if (Columns == 0) row[4:7] <- ' '
      paste(padder(row[cols], lenCol), collapse = '') 
      }, 
      by = 1:nrow(spineTable)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
    }
    
    
    cat(padder(colNames, lenCol), '\n\n', sep = '')
    
    #
    cat(padder(c('Tallies:'), sum(lenCol[1:2])), '\n', sep = '')
    
    tab <- spineTable[ , table(Spines, Columns)]
    for (i in 1:nrow(tab)) {
      row <- c('', sum(tab[i, ]), paste0('with ', (rownames(tab)[i])))
      row <- padder(row, lenCol[1:3])
      if (anypaths) {
        notzero <- which(tab[i, ] > 0)
        row <- c(row, ' (', harvard(paste0(tab[i, notzero], '*',  colnames(tab)[notzero], ' paths'), 'and'), ')')
      }
      cat(row, '\n', sep = '')
    }
    
    if (showEach && nfiles > 10L) cat(corpusMessage) else cat('\n')
    
  }
  invisible(NULL)
}


#### Interpretations ----



#' Summarize humdrum corpus interpretations.
#' 
#' This function provides a summary of the interpretations in the pieces of a humdrumR corpus.
#' @name humInterpretations
#' @family humdrum data summary functions
#' @export
interpretations <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'interpretations')
  
  corpusName <- substitute(humdrumR)
  corpusName <- if (is.call(corpusName))  NULL else deparse(corpusName)
  
  # humdrumR <- indexGLIM(humdrumR, dataTypes = 'I')
  humtab <- getHumtab(humdrumR, dataTypes = 'I')
  
  # Tandem
  tandem <- humtab[!grepl('^\\*\\*', Token) &   
                     !Token %in% c('*', '*-', '*v', '*^') &
                     !grepl('\\*>.*', Token)]
  tandem[ , ID := factor(idTandem(Token))]
  tandemN  <- do.call('rbind', tandem[, .(list(table(ID))), by = File]$V1)
  
  tandemUN <- do.call('rbind', tandem[, .(list(tapply(Token, ID, \(x) length(unique(x))))), by = File]$V1)
  tandemUN[is.na(tandemUN)] <- 0L
  
  tandIDs  <- levels(tandem$ID)
  tandemIn <- do.call('rbind', tandem[, .(list(colSums(tapply(ID, list(Spine,ID), length), na.rm = TRUE))), by = File]$V1)
  
  
  # Exclusive
  exclusive <- humtab[grepl('^\\*\\*', Token)]
  exclusive$Token <- factor(exclusive$Token)
  exclusiveN <- do.call('rbind', exclusive[ , .(list(table(Token))), by = File]$V1)
  rownames(exclusiveN) <- unique(humtab$Filename)
  
  output <- list(Filename = unique(humtab$Filename),
                 File = unique(humtab$File),
                 Exclusive = exclusiveN, 
                 Tandem    = list(Number    = tandemN, 
                                  NUnique   = tandemUN,
                                  InNSpines = tandemIn)) %class% 'humInterpretations'
  
  attr(output, 'corpusNames') <- corpusName
  
  output
  
}

#' @rdname humInterpretations
#' @export
print.humInterpretations <- function(interps, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  if (nrow(interps$Exclusive) < 1 || any(sapply(interps$Tandem, nrow) < 1)) { cat('No interpretations.\n') ; return(invisible(NULL))}
  
  
  tandems <- interps$Tandem[[1]]
  tandems[] <- do.call('paste', c(sep = '.', interps$Tandem))
  
  interpmat <- data.table(File = paste0(num2str(interps$File), ':'), Filename = interps$Filename, interps$Exclusive, tandems)
  nfiles <- nrow(interpmat)
  
  corpusMessage <- paste0('\n###### Interpretation content in humdrumR corpus "',
                          attr(interps, 'corpusName'), 
                          '" (', num2print(nfiles, 'file'), ")\n")
  
  
  exclusive <- grepl('^\\*\\*', colnames(interpmat))
  colNames <- c('', 'Filename', colnames(interpmat)[-1:-2])
  
  lenCol <- pmax(interpmat[ , sapply(.SD, \(x) max(nchar(x)))],  
                 nchar(colnames(interpmat)), 
                 na.rm = TRUE) + 2
  
  
  
  # shrink to screenWidth size
  screen <- cumsum(lenCol) <= screenWidth
  interpmat <- interpmat[ , screen, with = FALSE]
  colNames <- colNames[screen]
  lenCol <- lenCol[screen]
  colNames_str <- padder(colNames, lenCol)
  stars <- if (any(!screen)) "    ***" else ""
  
  key <- if (ncol(tandems) > 0L && max(which(screen)) > max(which(exclusive))) paste0(stringr::str_pad('(Total.Unique.Spines)', width = sum(lenCol), side = 'left'), '\n') else ""
  
  
  ## PRINTING BEGINS:
  cat(corpusMessage)
  
  if (nfiles == 1L || showEach) {
    cat(padder(colNames, lenCol), stars, '\n', sep = '')
    cat(key)
    
    interpmat[ , { 
      row <- unlist(.SD)[screen]
      paste(padder(row, lenCol), collapse = '') 
    }, 
    by = 1:nrow(interpmat)]$V1 -> strs
    
    cat(paste0(strs, stars), sep = '\n')
    
  }
  
  if (nfiles > 1L) {
    if (showEach) cat(key)
    cat("###### Totals:\n")
    cat(padder(colNames, lenCol), stars, '\n', sep = '')
    cat(padder(c('',
                 'Hits:',
                 sapply(as.list(interpmat)[exclusive & screen], \(col) sum(col > 0)),
                 colSums(interps$Tandem$NUnique[ , seq_len(max(0L, max(which(screen)) - max(which(exclusive)))), drop = FALSE]  > 0)),
               lenCol), stars,  '\n', sep = '')
  }
  
  if (showEach && nfiles > 10L) cat(corpusMessage) else cat('\n')
  
  if (stars != "") {
    cat('\n') 
    message <- paste0('(***',
                      num2word(sum(!screen)),
                      plural(sum(!screen), ' columns', ' column'), ' not displayed due to screensize',
                      '***)')
    message <- stringr::str_pad(message, width = sum(lenCol) + 8L, side = 'left')
    cat(message, '\n', sep = '')
  }
  
  
  
}

#### Sections ----

#' @export
sections <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'sections')
  
  cat('The sections function is under construction ;p')
  
  invisible(NULL)
}
