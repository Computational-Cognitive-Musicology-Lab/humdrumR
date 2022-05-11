#######################################################
## Methods for humdrum.table class
######################################################

#####initialize

#' @export
setClass('humdrum.table', slots = c(D = 'data.table', GLIM = 'data.table')) -> enHum

#' @export
setMethod('initialize', 'humdrum.table',
          function(.Object, tab) {
            .Object@D    <- tab[Type == 'Data']
            .Object@GLIM <- tab[Type != 'Data']
            .Object
          })

#' @export
is.humdrum.table <- function(x) inherits(x, 'humdrum.table')

#' @export
unHum <- function(humtab) {
  # takes a humdrum.table object, or the data.table content of a humdrum.table, and returns a data.table.
  # Also adds an object D and and object GLIM to the environment, representing the Data and non Data portions of the output table
  if (is.humdrum.table(humtab)) {
    humtabs <- list(D    = humtab@D, 
                    GLIM = humtab@GLIM)
    # humtab  <- data.table::rbindlist(humtabs) 
    humtab <- do.call('rbind', humtabs)
    
    setorder(humtab, FileNumber, Spine, Path, RecordNumber, StopNumber)
    
  } else {
    humtabs <- list(D    = humtab[Type == 'Data'],
                    GLIM = humtab[Type != 'Data'])
  }
  
  list2env(humtabs,  envir = parent.frame(1L))

  return(humtab)
}

reHum <- function(D, GLIM) enHum(rbind(D, GLIM))





#####################################
## indexing 
########################################
#' Index humdrum.table object.
#'
#' Indexing methods (\code{[}, and \code{[[}) for humdrum.table objects.
#' \code{[]} is used to index \emph{pieces} in a humdrum corpus.
#' \code{[]} is used to index within pieces (i.e., spines, records).
#'
#' @rdname humdrum.table.indexing


##################################
#### [
#################################

#' @export
setMethod('[', 
          signature = c(x = 'humdrum.table', i = 'missing'), 
          force)

#' @export
setMethod('[', 
          signature = c(x = 'humdrum.table', i = 'numeric'),
          function(x, i) {
            i <- unique(x@D$FileNumber)[i]
            
            x@D <- x@D[FileNumber %in% i]
            x
            # unHum(x)
            
            # D    <- D[FileNumber %in% i]
            # GLIM <- reHum_index(GLIM, D)

            # reHum(D, GLIM)
          })

#' @export
setMethod('[', 
          signature = c(x = 'humdrum.table', i = 'logical'),
          function(x, i) {
            i <- unique(x@D$FileNumber[i])
            x[i]
          })

#####################################
#### [[ 
#####################################

#' @rdname humdrum.table.indexing
#' @export
setMethod('[[', 
          signature = c(x = 'humdrum.table', i = 'missing', j = 'missing'), 
          force)

#' @export
setMethod('[[', 
          signature = c(x = 'humdrum.table', i = 'logical', j = 'missing'),
          function(x, i) {
            unHum(x)
            
            D    <- D[i]
            # GLIM <- reHum_index(GLIM, D)
            
            reHum(D, GLIM)
          })

#' @export
setMethod('[[', 
          signature = c(x = 'humdrum.table', i = 'numeric', j = 'missing'),
          function(x, i) {
            unHum(x)
            
            if (!allsame(sign(i))) i <- abs(i)
            
            D    <- if (sign(i[1]) == 1)  D[DataRecordNumber %in% i] else D[!DataRecordNumber %in% abs(i)]
            # GLIM <- reHum_index(GLIM, D)
            
            reHum(D, GLIM)
          })

#' @export
setMethod('[[', 
          signature = c(x = 'humdrum.table', i = 'missing', j = 'numeric'),
          function(x, j) {
            ### NEED TO: add support for spine paths
            unHum(x)
            
            if (!allsame(sign(j))) j <- abs(j)
            
            D <- if (sign(j[1]) == 1)  D[Spine %in% j] else D[!Spine %in% abs(j)]
            # GLIM <- reHum_index(GLIM, D)
            
            reHum(D, GLIM)
          })

#' @export
setMethod('[[', 
          signature = c(x = 'humdrum.table', i = 'numeric', j = 'numeric'),
          function(x, i, j) {
            x <- x[[ , j]]
            
            x[[i , ]]
          })


############################
### $
#############################


#' @export
setMethod('$',
          signature = c(x = 'humdrum.table'),
          function(x, name) {
            name <- as.character(name)
            x@D[[name]]
          })

reHum_index <- function(GLIM, D) {
#####ReIndex GLIM tables to match indexing of D tables
  
    # first remove missing pieces
    GLIM <- GLIM[FileName %in% unique(D$FileName)]
    
    # by piece:
    GLIM <- rbind(D, GLIM)[ , reHum_index_piece(.SD), by = FileName]
    GLIM
    
}


reHum_index_piece <- function(humtab) {
  ###called by reHum_index on each individual piece
  D    <- humtab[Type == 'Data']
  GLIM <- humtab[Type != 'Data']
  
  # remove missing spines
  GLIM <- GLIM[Spine %in% unique(D$Spine) | is.na(Spine)]
  
  # remove all except last barline before first data record
  prebarline <- GLIM$RecordNumber[GLIM$Type == 'Barline' & GLIM$RecordNumber < min(D$RecordNumber, na.rm = TRUE)]
  if (lennot0(prebarline))   GLIM <- GLIM[!(RecordNumber < last(prebarline) & Type == 'Barline')]
  
  #remove everything after last data record, except global stuff, '*-' or '=='
  GLIM <- GLIM[!(RecordNumber > max(D$RecordNumber, na.rm = TRUE) & !(is.na(Spine) | Token %in% c('*-', '==')))]
  
  GLIM
  
}


#######ACTIVE COLIMBS SHOULD BE PROPERTY OF humdrum.tables, not humdrumR

############################################################
#############Data transformation methods
############################################################

#' @export
as.RecordVectors <- function(humtab, global = FALSE) {
  # takes a humdrum.table and returns a list (each element == piece) of lists of vectors (each vector == tokens making up a record)
  humtab <- unHum(humtab)
  
  if (!global) humtab <- humtab[!is.na(Spine)]
  if (nrow(humtab) == 0) return(list()) 
  
  # collapse multi-stop tokens
  humtab[ , Token := paste(Token, collapse = ' '), by = list(RecordNumber, Path, Spine)]
  humtab <- humtab[humtab[j = !duplicated(paste(RecordNumber, Spine, Path))]]
  
  #
  cols <- unique(humtab$ColumnNumber)
  cols <- cols[!is.na(cols)]
  humtab$ColumnNumber <- match(humtab$ColumnNumber, cols, nomatch = NA_integer_)
  cols <- seq_along(cols)
  
  paths <- tapply(humtab$Path, humtab$ColumnNumber, unique)
  columnlabels <- as.vector(by(humtab, humtab$ColumnNumber, function(dat) paste0(unique(dat$Spine), '.', unique(dat$Path))))
  
  func <- function(record) {
    if (is.na(record$Spine[1])) {
      setNames(record$Token, '0.0')
    } else {
      # tokens <- rep(switch(record$Type[1], 'Interpretation' = '*', 'Data' = '.', 'Comment' = '!', 'Barline' = '='), length(cols))
      tokens <- rep('', length(cols))
      tokens[record$ColumnNumber] <- record$Token
      # if (any(paths != 0) && any(tokens in% c('*', '!', '=', '.'))) tokens[tokens %in% c('*', '!', '=', '.') & paths != 0] <- NA_character_
      # tokens[tokens %in% c('*', '!', '=', '.')] <- NA_character_
      names(tokens) <- columnlabels
      tokens
    }
  }
  output <- humtab[ , .(Tokens = list(func(.SD)), 
                        Type = unique(Type), 
                        FileName = unique(FileName),
                        Global = is.na(Spine[1])), by = RecordNumber]
  output[order(output$RecordNumber)]
}

padRecordVectors <- function(recvecdt) {
  recvecs <- recvecdt$Tokens
  # recvecs <- lapply(recvecs, trimLongString)
  
  strlengths <- lapply(recvecs, str_length)
  spineMaxN <- unlist(lapply(seq_len(max(lengths(recvecs))), 
                            function(j) {  max(unlist(lapply(strlengths, '[', j)[!recvecdt$Global])) }
                            )
                     )
  recvecs[!recvecdt$Global] <- lapply(recvecs[!recvecdt$Global], 
                                     function(recvec) { stri_pad_right(recvec, spineMaxN + 3) })
  
  recvecdt$Tokens <- recvecs
  recvecdt
}




#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrum.table'),
          function(x) as.data.frame(as.matrix(x)))

#' @export
setMethod('as.matrix',
          signature = c(x = 'humdrum.table'),
          function(x) { 
            humtab <- unHum(x) 
            if (nrow(humtab) == 0) return(matrix(data = character(0), ncol = 0, nrow = 0))
            
            output <- unclass(by(data = humtab, INDICES = humtab$FileNumber, humtab2mat_piece))
            names(output) <- unique(humtab$FileName)
            
            output
            })

humtab2mat_piece <- function(humtab) {
  # called by humdrum.table as.matrix method on each individual piece
  as.RecordVectors(humtab, global = FALSE) -> records
  
  mat <- stri_list2matrix(records$Tokens, byrow = T)
  
  rownames(mat) <- records$RecordNumber
  colnames(mat) <- names(records$Tokens[[1]])
  
  mat[is.na(mat)] <- ''
  
  mat
}
############################



#' @export
as.RecordStrings <- function(humtab, pad = FALSE, trim = NA, global = TRUE) {
  # takes a humdrum table a list (each element == one piece) of vectors of strings (each string == one record)
  humtab <- unHum(humtab)
  if (nrow(humtab) == 0) return(character(0))
  
  output <- unclass(by(data = humtab, INDICES = humtab$FileNumber, as.RecordStrings_piece, 
                       global = global, trim = trim, pad = pad))
  names(output) <- unique(humtab$FileName)
  
  output
}

as.RecordStrings_piece <- function(humtab, global, pad, trim) {
  # called by as.RecordStrings on each piece
  as.RecordVectors(humtab, global = global) -> records
  
  if (!is.na(trim)) {
    records$Tokens[!records$Global] <- lapply(records$Tokens[!records$Global], trimLongString, n = trim)
  }
  records$Tokens[ records$Global] <- lapply(records$Tokens[ records$Global], trimLongString, n = max(trim, 80L, na.rm = TRUE))
  
  if (pad) records <- padRecordVectors(records)
  
  recordstrings <- unlist(lapply(records$Tokens, 
                                 function(toks) {
                                   toks[is.na(toks)] <- ''
                                   paste(toks, collapse = '\t')
                                 }
  ))
  
  names(recordstrings) <- records$RecordNumber
  recordstrings[order(records$RecordNumber)]
}

##################################################
###############PRINTING RECORDS from humdrum.tables
#########################################################

printrecords <- function(humtab, printnames = FALSE, n = 0, global = TRUE) {
  #' @export
  humtab@GLIM <- reHum_index(humtab@GLIM, humtab@D)
   
  pieces <- as.RecordStrings(humtab, pad = TRUE, global = global) 
  if (length(pieces) == 0 || (!is.na(n) && n == 0)) invisible(list())
  
  Map(function(recs, filename) {  
    # what to do if empty
    if (length(recs) == 0) return(recs)
    # n is how many records to print?
    


    datarecn <- which(!grepl('^[!=*]', recs))
    if (is.na(n) || n >= length(datarecn)) {
      n <- length(recs)
      ellips <- FALSE
    } else {
      n <- datarecn[min(n, length(datarecn))]
      ellips <- TRUE
    }
    
    if (n < 0) n <- length(datarecn) - n
  
    datarecn <- datarecn[seq_len(min(n, length(datarecn)))]
    lastrec <- last(recs)
    lastrecn <- last(names(recs))
    recs <- recs[seq_len(n)]
    
    maxnchars <- max(stri_length(recs))
    
    recn <- names(recs)
    recn <- paste0(stri_pad_left(recn, max(stri_length(c(recn, lastrecn)))), ':')
    
    # print spine names above
    if (printnames) cat('\n', rep(' ', maxnchars / 2), filename, ':\n\n', sep = '') 
    
    cat(paste0(recn, ifelse(grepl('^!!', recs, useBytes = TRUE), ' ', '    '), recs, '\n'), sep = '') 
    
    # print trailing elipses if n is less than length recs
    if (ellips) {
      cat('-' %str*% maxnchars, '\n', sep = '')
      cat(lastrecn,  ':    ', lastrec, '\n', sep = '')
    } 
    }, 
    pieces, 
    gsub('^.*/', '', names(pieces))
    )
  invisible(pieces)
}

#' @export
setMethod('show', signature = c(object = 'humdrum.table'),
          function(object) { 
            humtab <- object
            if (nrow(humtab) == 0) {
              cat('Empty Humdrum corpus.') 
              
              return(invisible(NULL))
            }  
            filen <- length(unique(humtab@D$FileName))
            
            if (filen == 1) { 
              printrecords(humtab, n = NA, printnames = TRUE, global = TRUE) 
              } else { 
                whichfiles <- unique(unHum(humtab)$FileNumber) 
                
                whichfiles <- whichfiles[1]  
                
                printrecords(humtab[whichfiles], printnames = TRUE, n = 10, global = FALSE) 
                cat('\n')
                cat('\tCorpus of ', filen, ' humdrum files.\n', sep = '') 
              }  
            
            invisible(NULL) 
            
            })

#####################################################################
######################################Miscalaneus methods
#####################################################################
###

nrecords <- function(humtab, data.only = TRUE) {
  #' Count records in Humdrum corpus
  #' 
  #' @export
  humtab <- unHum(humtab)
  
  if (data.only)  {
    n <- humtab[Type == 'Data' & !Null, .(NR = length(unique(RecordNumber))), by = FileName] 
    
    } else { 
      
    n <- humtab[ , .(NR = length(unique(RecordNumber))), by = FileName]
  }
  sum(n$NR)
}

#' @export
setMethod('nrow', 
          signature = c(x = 'humdrum.table'), 
          function(x) nrecords(x))

##################
###corpus summaries
############

  #' @export 
summary.humdrum.table <- function(humdrum) {
  invisible(
    setNames(lapply(list(census, interpretations, reference),
                    function(func) func(humdrum)),
             c('Census', 'Interpretations', 'Reference')
    )
  )
}


### tokens ("census")

census <- function(humdrum) {
  #' Summarize humdrum corpus data tokens.
  #' 
  #' This function provides summary counts of the data tokens in a Humdrum corpus.
  #' @export
  unhumdrum <- unHum(humdrum)
  
  counts <- unhumdrum[ , .(Records          = length(unique(RecordNumber)),
                           DataRecords      = sum(!is.na(unique(DataRecordNumber))),
                           DataTokens       = sum(Type == 'Data' & !Null),
                           UniqueDataTokens = length(unique(Token[Type == 'Data' & !Null])),
                           Bars         = length(unique(Bar[Type == 'Data']))),
                       by = FileName]
  
  attr(counts, 'unique.tokens') <- unhumdrum[Type == 'Data' & !Null, list(list(unique(Token))), by = FileName]$V1
  
  counts %class% 'humdrum.table_census'
}

`[.humdrum.table_census` <- function(counts, i, j) {
  #' @export
  unique.tokens <- attr(counts, 'unique.tokens')
  counts <- popclass(popclass(counts))
  
  counts <- counts[i, j, drop = FALSE]
  if (!missing(i))  unique.tokens <- unique.tokens[i]
  
  attr(counts, 'unique.tokens') <- unique.tokens
  
  as.data.table(counts) %class% 'humdrum.table_census' 

}
  
print.humdrum.table_census <- function(counts, showall = TRUE) {
  #' @export
  #' 
  unique.tokens <- attr(counts, 'unique.tokens')
  counts <- popclass(counts)
  nrows <- nrow(counts)
  
  # if (nrows > 1) cat(nrows, 'files:\n')
  summable <- c('Records', 'DataRecords', 'DataTokens', 'Bars')
  
  sums <- colSums(counts[ , summable[summable %in% colnames(counts)], with = FALSE])
  if (any(colnames(counts) == 'UniqueDataTokens')) {
    sums <- append(sums,  
                   setNames(length(unique(unlist(unique.tokens))), 'UniqueDataTokens'),
                   after = which(colnames(counts) == 'UniqueDataTokens') - 1)
  }
  if (any(colnames(counts) == 'FileName')) counts$FileName <- paste0(gsub('\\.[^.][^.]*$', '',  
                                                                          gsub('.*/', '', counts$FileName)), 
                                                                     ':')
  
  lenColNms <- nchar(colnames(counts))
  # lenColCon <- c(max(nchar(counts$FileName)), nchar(num2str(sums)))
  lenColCon <- c(5, nchar(num2str(sums)))
  lenCols <- unlist(Map(max, 
                        lenColNms,
                        lenColCon)) + 2
  
  # cat(padder(c('' , colnames(counts)[-1]), lenCols), '\n')
  if (showall) {
    cat(padder(colnames(counts) %str-% 'FileName', lenCols), '\n', sep = '')
    cat('-' %str*% sum(lenCols), sep = '','\n')
    
    lapply(seq_len(nrows),
           function(i) {
             currow <- unlist(counts[i])
             cat(padder(c(currow[1], num2str(currow[-1], pad = FALSE)), lenCols), '\n', sep = '')
           })
    cat(rep('-', sum(lenCols)), sep = '','\n')
  }
  if (nrow(counts) > 1) {
    cat(padder(c('Corpus:', num2str(sums)), lenCols), '\n', sep = '') #sums
    cat(padder(colnames(counts) %str-% 'FileName', lenCols), '\n', sep = '')
    cat(stri_pad_both(paste0('in ', nrows, ' files.'), sum(lenCols)), '\n')
  }
  

  invisible(counts)
}


## interpretations
  
interpretations <- function(humdrum) {
  #' @export
  unhum <- unHum(humdrum)
  unhum <- unhum[Type == 'Interpretation']

  unhum <- unhum[!grepl('^\\*$|^\\*[*>+-]', Token)]
  
  `**cols` <- unique(unhum$Exclusive)
  
  `**s` <- lapply(`**cols`, function(`**`) unhum[, unique(Exclusive) == `**`, by = .(FileNumber, Spine)][ , sum(V1), by = FileNumber]$V1)
  names(`**s`) <- gsub('^\\*\\*', '', `**cols`)
  
  intmat <- as.data.table(`**s`)
  rownames(intmat) <- unhum[ , unique(FileName), by = FileNumber]$V1
  
  
  byfile <- unhum[ , .(Tandem = list(unique(unlist(Tandem)))), by = .(FileNumber, Exclusive)]
  intmat$Tandems <- byfile[ , list(Tandem = list(setNames(Tandem, Exclusive))), by = FileNumber]$Tandem
  
  intmat
  # popclass(intmat)
}
  

## reference
reference <- function(x) { 
  #' @export
  UseMethod('reference')
}
reference.character <- function(str) {
  #' @export
  str <- gsub('^!*', '', str)
  hits <- ReferenceCodes[rownames(ReferenceCodes) %in% str, ]
  
  if (nrow(hits) == 0) {
    cat("Unknown code\n")
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
               cat(curhits$Description[[i]], sep = '')
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

reference.humdrum.table <- function(humdrum) {
  #' Query reference records
  #' 
  #' @export
  refmat <- as.data.frame(unHum(humdrum))
  filenames <- paste0(gsub('\\.[^.][^.]*$', '', gsub('.*/', '', refmat$FileName)))
  
  refmat <- refmat[ , !colnames(refmat) %in% c('Token', 'Type', 'RecordNumber', 'Null', 'Spine',
                       'Path', 'Exclusive', 'Tandem', 'ColumnNumber', 'Bar', 'DoubleBarline',
                       'Barline', 'DataRecordNumber', 'Section', 'FileNumber', 'FileName')]
  # 
  
  refmat <- refmat[ !duplicated(filenames), ]
  rownames(refmat) <- filenames[!duplicated(filenames)]
  refmat[] <- lapply(refmat, `names<-`, rownames(refmat))
  
  refmat %class% 'humdrum.table_reference' 
}
  

print.humdrum.table_reference <- function(refmat) {
  #' @export
  refmat <- popclass(refmat)
  
  refis <- refmat
  refis[] <- lapply(refmat, lengths)
  
  colsizes = c(max(max(nchar(rownames(refmat))), 11),  nchar(colnames(refmat))) + 2
 

  cat(padder(c('File', colnames(refmat)), colsizes), '\n')
  lapply(1:nrow(refis),
         function(i) {
          cat(padder(c(rownames(refis)[i], ifelse(refis[i, ] > 0, refis[i, ], '.')), colsizes), '\n')
         })
  cat(padder(c('File', colnames(refmat)), colsizes), '\n')
  
  cat('\n')
  cat(padder(c('Hits:       ', lapply(refis, function(col) sum(col != 0))), colsizes), '\n')
  cat(padder(c('Total:      ', colSums(refis)), colsizes), '\n')
  cat(padder(c('Unique:     ', lapply(refmat, function(col) length(unique(unlist(col))))), colsizes), '\n')

  invisible(NULL)
}

  #' @export
`[.humdrum.table_reference` <- function(x, i, j, drop = FALSE) {
  if (missing(i) && missing(j)) return(x)
  
  x <- popclass(x)
  
  if (!missing(i)) {
    x <- if (!missing(j)) x[i, j, drop = drop] else x[i, , drop = drop]
    if (i %l!=% 1) x <- x %class% 'humdrum.table_reference'
  } else {
    x <- x[ , j, drop = drop]
  }
  
  x
}


