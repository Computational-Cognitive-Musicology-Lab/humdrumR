#######################################################---
## Functions for summarizing humdrumR objects----
######################################################---

#' Summarize humdrumR corpora
#' 
#' Summarizes the content of a [humdrumR corpus][humdrumRclass], by calling five different
#' corpus summary functions and printing their results.
#' 
#' @details 

#' [humdrumR] includes a number of separate functions for summarizing different
#' aspects of [humdrumR data objects][humdrumRclass]:
#'
#' * [census()]
#'     + Tabulates the raw size of the humdrumR corpus.
#' * [reference()]
#'     + Tabulates reference records (metadata) for each file.
#' * [spines()]
#'     + Tabulates the number of spines and spine paths in files in the corpus.
#' * [interpretations()]
#'     + Tabulates the types of exclusive and tandem interpretations in the corpus.
#' * [sections()]
#'     + Tabulates any formal data (`*>`) in the corpus, including barlines.
#'
#'
#' Each function takes a [humdrumR][humdrumRclass] object and returns a data.table.
#' The `summary` method for [humdrumR objects][humdrumRclass] simply calls all of the above 
#' functions and prints a condensed version of each.
#'
#' @name humSummary
#' @aliases summary
#' @family corpus summary functions
#' @export 
summary.humdrumR <- function(object) {
            quoted <- substitute(object) # enexpr won't work!
            corpusName <- rlang::as_label(quoted)
            cat('\t\tSummary of humdrumR corpus "', corpusName, '":\n', sep = '')
            
            funcs <- c(`Token Census` = quote(census), 
                       `Reference Records` = quote(reference), 
                       `Spine Structure` = quote(spines), 
                       `Intepretation Content` = quote(interpretations)) #, sections)
            
            summaries <- lapply(funcs, 
                                \(f) {
                                  eval(rlang::expr((!!f)(!!quoted))) 
                                  })
            
            for (i in seq_along(summaries)) {
              # cat('\t', names(funcs)[i], ':\n', sep ='')
              print(summaries[[i]], showEach = FALSE)
            }
            invisible(summaries)
          }


######## Census ----

#' Tabulate records and tokens in a humdrumR corpus
#' 
#' `census` tabulates the raw "size" of a [humdrumR corpus][humdrumRclass],
#' including the total number of records and tokens.
#' `census` is one of [humdrumR]'s
#' basic [corpus summary functions][humSummary].
#' 
#' 
#' @details 
#' 
#' `census` returns a special `data.frame` called a `humCensus` table.
#' A `humCensus` table has five columns of information:
#' 
#' 1. Records
#'     + The total number of records.
#' 2. Tokens
#'     + The total number of tokens.
#' 3. (unique)
#'     + The number of **unique** tokens
#' 4. Characters
#'     + The total number of characters. 
#'       (This includes humdrum control characters like `*` and `!!`.)
#' 5. (per token)
#'     + This is simply `Characters / Tokens`, indicating the mean length of each token.
#' 
#' By default, `census` tabulates data within files in the corpus,
#' with each file tabulated in a row of the `humCensus` table.
#' Rows are labeled with each filename.
#' When a `humCensus` object is printed,
#' the totals across all files are printed as well---(unique) and (per token)
#' values are calculated across all files as well, not summed.
#' The `by` argument can be used to tabulate data across other divisions in the data (see next section).
#' 
#' 
#' 
#' @section Tabulate "by" other groups:
#' 
#' The `by` argument to `census` indicates groupings in the data to tabulate within, grouping 
#' across filenames in the corpus by default.
#' `by` can be an arbitrary expression which is evaluated inside the [humdrum table][humTable],
#' like the `groupby` argument to a [with/within][withHumdrum] call.
#' The by expression must be the full length of the humdrum table.
#'  
#' @param humdrumR A [humdrumR object][humdrumRclass]
#' @param dataTypes Which types of humdrum records to include in the census. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#'    or any combination of these (e.g., `"LIM"`).
#'    (see the [humdrum table][humTable] documentation **Fields** section for explanation.).
#' @param by An arbitrary expression which indicates how to group the data.
#' @param i If `numeric`, selects rows by index. If `character`, the string is matched
#'     as a regular expression against the "by-group" names.
#' @param removeEmpty (`logical`, `length == 1`) If set `TRUE`, any groups that have zero tokens are 
#' not included in the `humCensus` table.
#' @param drop If `drop = TRUE`, a normal [data.table][data.table::data.table()] is returned instead of 
#' a `humCensus` table.
#'
#'
#' @family corpus summary functions
#' @export
census <- function(humdrumR, dataTypes = 'GLIMDd', by = Filename, removeEmpty = FALSE, drop = FALSE) {
  corpusName <- rlang::as_label(rlang::enexpr(humdrumR))
  by <- rlang::enexpr(by)
  
  ## ARGUMENT CHECKS:
  checkhumdrumR(humdrumR, 'census')
  dataTypes <- checkTypes(dataTypes, 'census')
  checkTF(removeEmpty, 'removeEmpty', 'census')
  checkTF(drop, 'drop', 'census')
  
  humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
  
  ##get the partitioning vector
  partition <- rlang::eval_tidy(by, data = humtab)
  if (!is.list(partition)) partition <- list(partition)
  partition <- lapply(partition, rep, length.out = nrow(humtab))
  levels <- Reduce(paste, do.call('expand.grid', lapply(partition, unique)))
  partition <- as.factor(Reduce(paste, partition))
  
  if (length(partition) != nrow(humtab)) .stop("In your call to census, your by expression doesn't evaluate to a vector matching the length of",
                                               "the humdrum table.")
  
  ## create the table
  censusTable <- humtab[ , 
      .(
        Records          = length(unique(Record[Stop == 1L | is.na(Stop)])),
        Tokens           = nrow(.SD),
        `(unique)`       = list(unique(Token)),
        Characters       = sum(nchar(Token)),
        `(per token)`    = round(mean(nchar(Token)), 2),
        Files            = list(unique(File))
      )
  ,
  by = partition]
  
  # levels that aren't present
  emptylevels <- setdiff(levels, censusTable$partition)
  if (!removeEmpty && length(emptylevels) > 0L) {
    censusTable <- rbind(censusTable, 
                         data.table(partition = emptylevels,
                                    Records = 0L, Tokens = 0L, `(unique)` = list(), Characters = 0L, `(per token)` = NA_integer_))
  }

  censusTable$partition <- as.character(censusTable$partition)
  setorder(censusTable, partition)
  
  attr(censusTable, 'corpusName') <- corpusName
  attr(censusTable, 'dataTypes')  <- paste(dataTypes, collapse = '')
  attr(censusTable, 'by') <- rlang::as_label(by)
  attr(censusTable, 'nfiles') <- length(unique(humtab$Filename))
  
  if (drop) {
    censusTable$`(unique)` <- lengths(censusTable$`(unique)`)
    censusTable
  } else {
    censusTable %class% 'humCensus'
  }
}

#' @rdname census
#' @usage census(humdata)[i]
#' @export
`[.humCensus` <- function(censusTable, i, drop = FALSE) {
  if (missing(i)) return(if (drop) popclass(censusTable) else censusTable)
  
  corpusName <- attr(censusTable, 'corpusName')          
  dataTypes  <- attr(censusTable, 'dataTypes')      
  
  censusTable <- popclass(censusTable)
  if (is.character(i)) i <- grepl(i, censusTable$partition)
  
  censusTable <- censusTable[i]
  
  
  if (!drop) censusTable <- censusTable %class% 'humCensus' 
  
  
  corpusName <- paste0(corpusName, '[i]')
  attr(censusTable, 'corpusName') <- corpusName
  attr(censusTable, 'dataTypes')  <- dataTypes
  censusTable
}


#' @rdname census
#' @export
print.humCensus <- function(censusTable, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  
  
  censusTable <- data.table::copy(popclass(censusTable))
  ngroups <- nrow(censusTable)
  nfiles <- length(unique(unlist(censusTable$Files)))
  
  by <- attr(censusTable, 'by')
  by <- if (grepl('[^a-zA-Z0-9]', by)) paste0(num2print(ngroups), " unique results of ", by) else num2print(ngroups, by)
  
  # Corpus message (name and n files)
  corpusMessage <- paste0("\n###### Census of ",
                          attr(censusTable, 'dataTypes'), ' records', if (!showEach) ':\n')
  
  if (showEach) corpusMessage <- paste0(corpusMessage, 
                                        ' in humdrumR corpus "',  attr(censusTable, 'corpusName'), '" ',
                                        '(', num2print(nfiles, 'file'), '):\n')
  
  
  
  if (ngroups < 1L) {
    cat('Empty humdrum corpus.\n') 
    return(invisible(NULL))
  }
  
  ##
  partition <- censusTable[ , paste0(trimTokens(as.character(partition), 70L), ' [', num2str(seq_along(partition), pad = TRUE), ']')]
  censusTable[ , partition := NULL] # in place!
  censusTable[ , Files := NULL] # in place!
  
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
  censusTable <- cbind(partition, censusTable)
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
    cat("###### Grouped by ", by, ":\n", sep = '')
    cat(colNames_str, stars, '\n', sep = '')
    # 
    censusTable[, cat(paste(padder(unlist(.SD)[screen], lenCol), collapse = ''), stars, '\n', sep = ''), by = seq_len(ngroups)]
    if (ngroups > 10L ) cat(colNames_str, stars, '\n', sep = '')
    
  }
  
  
  if (showEach) cat("###### Totals:\n")
  if (!showEach) cat(colNames_str, stars, '\n', sep = '')
  cat(padder(sums[screen], lenCol), stars,  '\n', sep = '') #sums
  
  if (showEach && ngroups > 10L) cat(corpusMessage) else cat('\n')
  
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
#' `reference` is used to
#' tabulate the reference records
#' present in a [humdrumR][humdrumRclass] corpus.
#' `reference` is one of [humdrumR]'s
#' basic [corpus summary functions][humSummary].
#' 
#' @details 
#' 
#' 
#' `reference` can be used to look up information about common reference
#' codes: supply a reference code as a `character` string to `reference`
#' and it will check it against known reference codes and print
#' information about matching codes (if there is one). For instance, `reference('OTL')`
#' returns a description of the standard humdrum `!!!OTL` reference record
#' (original title metadata).
#' 
#' 
#' When applied to a [humdrumR corpus][humdrumRclass] 
#' `reference` returns a special `data.frame` called a `humReference` table.
#' A `humReference` table has one column for 
#' each reference code that appears in the corpus.
#' Since reference records can be too long to print on one screen,
#' and humdrum files can have multiple of the same type of reference code,
#' a `humReference` table normally prints only the number of each type of 
#' reference record to appear in each file.
#' However, if only one type of reference code is present in a
#' `humReference` table, the complete reference records for that code
#' *will* be printed for each file. Likewise, if only one file is present
#' in the table, all of that file's complete reference records are printed.
#' Thus, if you want to see actual reference records, try indexing the
#' `humReference` table down to one column or row (see below).
#' 
#' A `humReference` table has one row for each file in the corpus.
#' Rows are labeled with each file name.
#' In addition, when a `humReference` object is printed,
#' three different summary totals are printed for each reference code:
#' 
#' + **Any** indicates how many files in the corpus contain at least
#'    one example of each code.
#' + **Sum** indicates the total number of each reference code to appear
#'    in the corpus, including multiple appearances in one file (like multiple `"!!!COM"`
#' records).
#' + **Unique** tabulates the number of unique token in the corpus, for each code. 
#'    If your corpus only has two unique composers (encoded in "!!!COM"),
#'    the **Unique** total will be `2`.
#'    This assumes that tokens are *exactly* identical, including white space;
#'    so `"!!!COM: J.S. Bach"` and `"!!!COM: JS Bach"` will be counted as two 
#'    unique reference records.
#' 
#' @param x A `character` string (to look up a reference code) or a [humdrumR object][humdrumRclass].
#' @param i If `numeric`, selects rows by index. If `character`, the string is matched
#'     as a regular expression against filenames in the corpus.
#' @param j if `numeric` selects columns by index. If `character`, [partially matched][base::pmatch()]
#'     against column names (reference codes).
#' @param drop If `drop = TRUE`, a normal [data.table][data.table::data.table()] 
#'     is returned instead of `humReference` table.
#' 
#'       
#' @examples 
#' 
#' reference('COM')
#' 
#' reference(humData)[ , 'COM']
#' 
#' reference(humData)[3, ]
#'        
#' @family corpus summary functions
#' @export
reference <- function(x, ...) UseMethod('reference')


#' @rdname reference
#' @usage reference('OTL')
#' @export
reference.character <- function(str) {
  str <- gsub('^!*', '', str)
  
  ReferenceCodes$Examples <- sapply(ReferenceCodes$Examples, paste, collapse = '\n\t\t\t')
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
             
             if (curhits$Examples[[i]] != "") {
               cat('\n\t', 'Examples:\n', sep = '')
               cat(paste0('\t\t\t', curhits$Examples[[i]], '\n'), sep = '')
               cat('\n')
             }
             
           }
         })
  
  return(invisible(hits))
}

#' @rdname reference
#' @usage reference(humdata)
#' @export
reference.humdrumR <- function(humdrumR, drop = FALSE) {
  corpusName <- rlang::as_label(substitute(humdrumR)) # enexpr won't work!

  
  humtab <- getHumtab(humdrumR)
  fieldtable <- fields(humdrumR, fieldTypes = 'Reference')
  
  refTable <- humtab[ , c('File', 'Filename', fieldtable$Name), with = FALSE]
  # 
  
  refTable <- refTable[!duplicated(Filename)]
  
  attr(refTable, 'corpusName') <- corpusName
  if (drop) refTable else refTable %class% 'humReference'
}


#' @rdname reference
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


#' @rdname reference
#' @usage NULL
#' @export
print.humReference <- function(refTable, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  
  refTable <- data.table::copy(popclass(refTable))
  nfiles <- nrow(refTable)
  
  ### 
  corpusMessage <- paste0('\n###### Reference records', if (!showEach) ':\n')
  
  if (showEach) corpusMessage <- paste0(corpusMessage, 'in humdrumR corpus "',
                                        attr(refTable, 'corpusName'),
                                        '" (', num2print(nfiles, 'file'), "):\n")
  
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
  
  
  if (showEach) cat("###### Totals:\n")
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

#' Summarize spines in humdrum dataset.
#'
#' `spines` tabulates the spines and spine paths within the files in a
#' [humdrumR corpus][humdrumRclass].
#' `spines` is one of [humdrumR]'s
#' basic [corpus summary functions][humSummary].
#' 
#' @details 
#'
#' `spines` returns a special `data.frame` called a `humSpines` table.
#' A `humSpines` table has five columns of information about each file:
#' 
#' 1. Spines
#'     + The number of spines.
#' 2. Paths
#'     + The total number of spine paths.
#' 3. In
#'     + The number of spines which contain any spine paths.
#' 4. *^
#'     + The total number of spine splits (`"*^"`).
#' 5. *v
#'     + The total number of spine joins (`"*v"`).
#'     
#' When `humSpine` table prints on the command line, "tallies"
#' of the unique combinations of spines and paths in the files are also printed.
#' 
#' @param humdrumR A [humdrumR object][humdrumRclass]
#' @param i If `numeric`, selects rows by index. If `character`, the string is matched
#'     as a regular expression against filenames in the corpus.
#' @param drop If `drop = TRUE`, a normal [data.table][data.table::data.table()] is returned instead of a `humSpine` table.
#' 
#' @family corpus summary functions
#' @export
spines  <- function(humdrumR, drop = FALSE) {

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
  if (drop) spines else spines %class% 'humSpines'
}


#' @rdname spines
#' @export
`[.humSpines` <- function(spines, i, drop = FALSE) {
  if (missing(i)) return(if (drop) popclass(spines) else spines)
  
  
  corpusName <- attr(spines, 'corpusName')
  spines <- popclass(spines)
  spines <- spines[i]
  
  attr(spines, 'corpusName') <- corpusName
  if (drop) spines else spines %class% 'humSpines'
}



#' @rdname spines
#' @export
print.humSpines <- function(spineTable, showEach = TRUE) {
  nfiles <- nrow(spineTable)
  
  if (nfiles < 1L) {cat('Empty humdrumR object.\n') ; return(invisible(NULL))}
  
  ### 
  corpusMessage <- paste0('\n###### Spine structure', if (!showEach) ':\n')
  
  if (showEach) corpusMessage <- paste0(corpusMessage,
                                        ' in in humdrumR corpus "',
                                        attr(spineTable, 'corpusName'),
                                        '" (', num2print(nfiles, 'file'), "):\n")
  
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
    
    spineTable[ , Filename := paste0(trimTokens(Filename, 70L), ' [', num2str(seq_along(Filename), pad = TRUE), ']')]
    spineTable[ , File := NULL]
    
    cols <- 1:6
    colNames <- c('', 'Spines', '+ Paths', 'In', '*^', '*v')[cols]
    
    
    lenCol <- pmax(c(8,0,0,0,0,0)[cols], #Tallies: is 8 long
                   nchar(colNames), sapply(spineTable[ , cols, with = FALSE], \(x) max(nchar(x)))) + 2L
    
    if (showEach) {
      cat(padder(colNames, lenCol), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
      spineTable[ , { 
        row <- unlist(.SD)
        if (Columns == 0) row[4:7] <- ' '
        paste(padder(row[cols], lenCol), collapse = '') 
        }, by = 1:nrow(spineTable)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat(stringr::str_dup('#', sum(lenCol)), '\n', sep = '')
      
      cat(padder(colNames, lenCol), '\n\n', sep = '')
      cat(padder(c('Tallies:'), sum(lenCol[1:2])), '\n', sep = '')
    }
    
    
    
    #
    
    tab <- spineTable[ , table(Spines, Columns)]
    tab <- tab[order(tab[ , 1], decreasing = TRUE), , drop = FALSE]
    for (i in 1:nrow(tab)) {
      row <- c('', sum(tab[i, ]), paste0(' files with ', (rownames(tab)[i]), ' spines'))
      row <- padder(row, lenCol[1:3])
      if (anypaths) {
        notzero <- which(tab[i, ] > 0)
        row <- c(row, ' (', harvard(paste0(tab[i, notzero], 
                                           ' with ',  
                                           colnames(tab)[notzero], 
                                           plural(as.numeric(colnames(tab)[notzero]), ' paths', ' path')), 'and'), ')')
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
#' `interpretations` is used to summarize the interpretations in the pieces of a humdrumR corpus,
#' including *exclusive* (`**`) and *tandem* (`*`) interpretations.
#' `interpretations` is one of [humdrumR]'s
#' basic [corpus summary functions][humSummary].
#' 
#' @details
#' 
#' `interpretations` returns a special `data.frame` called a `humInterpretations` table.
#' Each row in the table represents a single piece in the corpus.
#' The first column (`{X}`) is a variable indicating a unique "exclusive pattern" associated with
#' each file---the exclusive patterns are tallied at the bottom of the printout.
#' The remaining columns indicate how many of each interpretation (indicated by column name)
#' appear in each piece.
#' 
#' For tandem interpretations, counts are returned in the format `Total.Unique.Spines`:
#' 
#' + `Total`: The total instances of this interpretation, across all spines.
#' + `Unique`: The number of unique versions of this interpretation.
#' + `Spines`: The number of spines that this interpretation appears in.
#'     
#'     
#'  For example, consider the following file:
#'  
#'  ```
#'  **kern   **kern   **silbe
#'     *C:      *C:         *
#'       c        e        La
#'       d        f        la
#'       e        g        la
#'     *e:      *e:         *
#'      f#       d#        la
#'       g        e         _
#'       a        b         _
#'     *G:      *G:         *
#'      f#        a       doo
#'       g        b       wop
#'      *-       *-        *-
#'  ```
#'  
#'  In this file, there is several tandem key interpretations,
#'  which `humdrumR` will call `Key`.
#'  The tabulation by `interpretations` will return a `Key` column with the value
#'  `6.3.2` for this file:
#'  
#'  + `6` because there are six key interpretations in total.
#'  + `3` because there are three unique keys: `*C:`, `*e:` and `*G:`.
#'  + `2` because the key interpretations only occur in two spines.
#'  
#'     
#'     
#' @param humdrumR A [humdrumR][humdrumR-class] data object.
#' @param i If `numeric`, selects rows by index. If `character`, the string is matched
#'     as a regular expression against the filenames in the corpus.
#' 
#' @family corpus summary functions
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
  
  exclusivePats <- exclusive[, harvard(Token), by = File]$V1
  output <- list(Filename = unique(humtab$Filename),
                 File = unique(humtab$File),
                 Exclusive = exclusiveN, 
                 ExclusivePat = exclusivePats,
                 Tandem    = list(Number    = tandemN, 
                                  NUnique   = tandemUN,
                                  InNSpines = tandemIn)) 
  
  attr(output, 'corpusNames') <- corpusName
  
  output %class% 'humInterpretations'
  
}

#' @rdname interpretations
#' @export
`[.humInterpretations` <- function(interps, i) {
  if (missing(i)) return(interps)
  
  corpusName <- attr(interps, 'corpusName')
  if (is.character(i)) i <- grepl(i, interps$Filename)
  
  interps <- rapply(interps, how = 'replace',
                    \(x) {
                      if (hasdim(x)) x[i , , drop = FALSE] else x[i]
                    })
  attr(interps, 'corpusName') <- corpusName
                        
  interps %class% 'humInterpretations'
}


#' @rdname interpretations
#' @export
print.humInterpretations <- function(interps, showEach = TRUE, screenWidth = options('width')$width - 10L) {
  if (nrow(interps$Exclusive) < 1 || any(sapply(interps$Tandem, nrow) < 1)) { cat('No interpretations.\n') ; return(invisible(NULL))}
  
  
  tandems <- interps$Tandem[[1]]
  tandems[] <- do.call('paste', c(sep = '.', interps$Tandem))
  
  tallies <- sort(table(interps$ExclusivePat), decreasing = TRUE)
  
  interpmat <- data.table(File = paste0(trimTokens(interps$Filename, 70L), ' [', num2str(seq_along(interps$Filename), pad = TRUE), ']'), 
                          "{X}" = paste0('{', LETTERS[match(interps$ExclusivePat, names(tallies))], '}'), 
                          interps$Exclusive,
                          tandems)
  nfiles <- nrow(interpmat)
  
  corpusMessage <- paste0('\n###### Interpretation content', if (!showEach) ':\n')
                          
  if (showEach) corpusMessage <- paste0(corpusMessage,
                                        ' in humdrumR corpus "',
                                        attr(interps, 'corpusName'), 
                                        '" (', num2print(nfiles, 'file'), "):\n")
                                        
                       
  
  
  exclusive <- grepl('^\\*\\*', colnames(interpmat))
  colNames <- c('', colnames(interpmat)[-1])
  
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
  
  key <- if (ncol(tandems) > 0L && max(which(screen)) > max(which(exclusive))) paste0(stringr::str_pad('(Total.Unique.Spines)', 
                                                                                                       width = sum(lenCol), side = 'left'), '\n') else ""
  
  
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
    if (showEach) {
      cat(key)
      cat("###### Totals:\n")
    }
    cat(padder(colNames, lenCol), stars, '\n', sep = '')
    cat(padder(c('Hits:',
                 '',
                 sapply(as.list(interpmat)[exclusive & screen], \(col) sum(col > 0)),
                 colSums(interps$Tandem$NUnique[ , seq_len(max(0L, max(which(screen)) - max(which(exclusive)))), drop = FALSE]  > 0)),
               lenCol), stars,  '\n', sep = '')
  }
  
  #
  if (stars != "") {
    cat('\n') 
    message <- paste0('(***',
                      num2word(sum(!screen)),
                      plural(sum(!screen), ' columns', ' column'), ' not displayed due to screensize',
                      '***)')
    message <- stringr::str_pad(message, width = sum(lenCol) + 8L, side = 'left')
    cat(message, '\n', sep = '')
  }
  
  
  ## tallying patterns
  cat(padder(c('Tallies:'), 12), '\n', sep = '')
  tab <- cbind(paste0('{', LETTERS[seq_along(tallies)], '} = '), 
               paste0(names(tallies), ':'),  
               tallies)
  lenCol <- c(12, apply(nchar(tab), 2, max) + 2L)
  for (i in 1:nrow(tab)) {
    row <- padder(c('', tab[i, ]), lenCol[1:4])
    cat(row, '\n', sep = '')
  }
  
  
  if (showEach && nfiles > 10L) cat(corpusMessage) else cat('\n')
}

#### Sections ----

#' @family corpus summary functions
#' @export
sections <- function(humdrumR) {
  checkhumdrumR(humdrumR, 'sections')
  
  cat('The sections function is under construction ;p')
  
  invisible(NULL)
}
