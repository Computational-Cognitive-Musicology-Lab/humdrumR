#######################################################---
## Functions for summarizing humdrumR objects----
######################################################---



#' @export 
setMethod('summary', 'humdrumR',
          function(object, ...) {
            funcs <- c(census, reference, spines, interpretations) #, interpretations, sections)
            
            summaries <- lapply(funcs, function(f) f(object, ...))
            names(summaries) <- c('Census', 'Reference', 'Spines', 'Interpretations')
            names <- '\n#####' %str+%  names(summaries) %str+% ":\n\n"
            
            for (i in seq_along(summaries)) {
              cat(names[i], sep ='')
              print(summaries[[i]], showall = FALSE)
            }
            invisible(summaries)
          })


#### Census ----

census <- function(humdrumR) {
  #' Summarize humdrum corpus data tokens and records.
  #' 
  #' This function provides summary counts of the data tokens in a Humdrum corpus.
  #' @export
  humtab <- getHumtab(humdrumR)
  
  ##ADD MULTISTOPS, MOVE BARS TO sections FUNCTION
  
  cens <- humtab[ , .(NFile            = unique(NFile),
                      Records          = length(unique(Record)),
                      DataRecords      = sum(!is.na(unique(NData))),
                      DataTokens       = sum(Type == 'D' & !Null),
                      UniqueDataTokens = length(unique(Token[Type == 'D' & !Null])),
                      Bars             = length(unique(BarN))),
                      by = File]
  
  setcolorder(cens, c('NFile', 'File', 'Records', 'DataRecords', 'DataTokens', 'UniqueDataTokens', 'Bars'))
  
  attr(cens, 'unique.tokens') <- humtab[Type == 'D' & !Null, list(list(unique(Token))), by = File]$V1
  
  cens %class% 'humCensus'
}

`[.humCensus` <- function(cens, i, j) {
  #' @export
  unique.tokens <- attr(cens, 'unique.tokens')
  
  
  if (missing(i) && missing(j)) return(cens)
  if (missing(j)) j <- seq_len(ncol(cens) - 2)
  if (missing(i)) i <- seq_len(nrow(cens)) else unique.tokens <- unique.tokens[i]
  
  if (is.character(j)) j <- grep(j, colnames(cens)) - 2
  if (is.character(i)) i <- grep(i, cens$File)
  
  cens <- popclass(cens)
  cens <- cens[i, c(1, 2, j + 2), with = FALSE]
  
  attr(cens, 'unique.tokens') <- unique.tokens
  
  cens %class% 'humCensus' 
}
  
print.humCensus <- function(cens, showall = TRUE) {
  #' @export
  #' 
  unique.tokens <- attr(cens, 'unique.tokens')
  cens <- popclass(cens)
  nrows <- nrow(cens)
  if (nrows < 1) {cat('Empty humdrum corpus.\n') ; return(invisible(NULL))}
  
  
  
  # if (nrows > 1) cat(nrows, 'files:\n')
  summable <- c('Records', 'DataRecords', 'DataTokens', 'Bars')
  sums <- c(FileN = '', File = 'All', num2str(colSums(cens[ , summable[summable %in% colnames(cens)], with = FALSE])))
  if (any(colnames(cens) == 'UniqueDataTokens')) {
    sums <- c(sums, UniqueDataTokens = num2str(length(unique(unlist(unique.tokens)))))[colnames(cens)]
  }
  sums[1] <- num2str(nrows)
  
  
  sums <- num2str(sums, pad = FALSE)
  cens <- cens[ , lapply(.SD, num2str, pad = FALSE)]
  cens$NFile <- cens$NFile %str+% ":"
  colNms <- c('File#', colnames(cens)[-1])
  lenCol <- pmax(nchar(colNms),
                 sapply(cens, max %.% nchar),
                 nchar(sums)) + 2
  
  
  if (showall) {
    cat(padder(colNms, lenCol), '\n', sep = '')
    cat('#' %str*% sum(lenCol), '\n', sep = '')
  
    cens[, cat(paste(padder(unlist(.SD), lenCol), collapse = ''), '\n', sep = ''), by = 1:nrow(cens)]
    
    cat(rep('#', sum(lenCol)), '\n', sep = '')
  }
  if (nrows > 1) {
    cat(padder(colNms, lenCol), '\n\n', sep = '')
    cat(padder(sums, lenCol), '\n', sep = '') #sums
    # cat(stri_pad_both(paste0('in ', nrows, ' files.'), sum(lenCol)), '\n')
  }
  invisible(NULL)
}


#### Reference ----

#' @export
reference <- function(x) UseMethod('reference')

reference.character <- function(str) {
  #' @export
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
               
               cat('\t\t' %str+% descrip %str+% '.\n', sep = '')
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

reference.humdrumR <- function(humdrumR) {
  #' Query reference records
  #' 
  #' @export
  humtab <- getHumtab(humdrumR)
  lay <- fields(humdrumR, reference = TRUE)
  
  humtab <- humtab[ , c('NFile', 'File', lay$Name[lay$Type == 'Reference']), with = FALSE]
  # 
  
  humtab <- humtab[ !duplicated(File), ]
  
  humtab %class% 'humReference'
}
  

print.humReference <- function(refmat, showall = TRUE) {
  #' @export
  if (nrow(refmat) < 1) {cat('Empty humdrumR object.\n') ; return(invisible(NULL))}
  
  refmat <- popclass(refmat)
  refmat$NFile <- num2str(refmat$NFile) %str+% ":"
  
  if (nrow(refmat) == 1) {
    cat(refmat$File, ':\n\t', sep = '')
    
    rowNms <- colnames(refmat)[-1:-2]
    rowNms <- padder(rowNms, max(nchar(rowNms)))
    
    refmat <- refmat[ , lapply(.SD,  function(col) { sapply(col, paste, collapse = '\n\t\t') })]
    
    cat(paste(collapse = '\n\t', rowNms %str+% ': ' %str+% unlist(refmat[, -1:-2])))
  }  else {
    
    refNs <- refmat[, -1:-2, with = FALSE]
    refNs <- refNs[ , lapply(.SD, 
                             function(col) {
                               col <- if (is.list(col)) lengths(col) else ifelse(is.na(col), 0, 1)
                               col
                             })]
    
    refnames <- colnames(refNs)
    
    refNs$File = refmat$File
    refNs$NFile <- refmat$NFile
    refmat <- refmat[ , lapply(.SD, 
                               function(col) {
                                 sapply(col, function(ref) paste(collapse = '\n\t\t', '  ' %str+% trimLongString(ref, n = 80L)))
                               })]
    
    setcolorder(refNs, c('NFile', 'File', refnames))
    
    
    colNms <- c('', 'File', refnames)
    
    
    lenCol <- pmax(refNs[ , sapply(.SD, max %.% nchar)], 
                   nchar(colNms)) + 2
    lenCol[2] <- max(9, lenCol[2]) # to make enough room for "Unique:"
    
    if (showall) {
      cat(padder(colNms, lenCol), '\n', sep = '')
      
      cat('#' %str*% sum(lenCol), '\n', sep = '')
      if (ncol(refmat) == 3) {
        refmat[ , cat(c(padder(unlist(.SD)[1:2], lenCol[1:2]), {texts <- unlist(.SD)[3] ; gsub('^  NA$', '  .', texts)} ), '\n', sep = ''), by = 1:nrow(refmat)]
      } else {
        refNs[ , cat(gsub('^( *)0$', '\\1.', padder(unlist(.SD), lenCol)), '\n', sep = ''), by = 1:nrow(refmat)]
      }
      cat('#' %str*% sum(lenCol), '\n', sep = '')
    }
    cat(padder(colNms, lenCol), '\n\n', sep = '')
    
    cat(padder(c('', 'Hits:'  , sapply(refNs[ ,-1:-2], sum %.% GT(0))), lenCol), '\n', sep = '')
    cat(padder(c('', 'Total:' , colSums(refNs[ , -1:-2])), lenCol), '\n', sep = '')
    cat(padder(c('', 'Unique:', sapply(refmat[ , -1:-2], length %.% unique %.% unlist)), lenCol), '\n', sep = '')
    
  }
  invisible(NULL)
}

  #' @export
`[.humReference` <- function(ref, i, j) {
  if (missing(i) && missing(j)) return(ref)
  
  if (missing(j)) j <- seq_len(ncol(ref) - 2)
  if (missing(i)) i <- seq_len(nrow(ref)) 
  
  if (is.character(j)) j <- grep(j, colnames(ref)) - 2
  if (is.character(i)) i <- grep(i, ref$File)
  
  ref <- popclass(ref)
  ref <- ref[i, c(1, 2, j + 2), with = FALSE]
  
  ref %class% 'humReference'
}


#### Spines ----

spines  <- function(humdrumR) {
  #' Summarize humdrum corpus spine paths.
  #' 
  #' This function provides summary of the spines and spine paths in the pieces of a humdrumR corpus.
  #' @export
  humtab <- getHumtab(humdrumR)
  
  spins <- humtab[Global == FALSE , 
                    .(NFile            = unique(NFile),
                      Spines           = length(unique(Spine)),
                      Columns          = length(unique(Column)),
                      Splits           = sum(grepl('\\*\\^', Token)),
                      Splices          = length(unique(Record[grepl('\\*v', Token)])),
                      Where            = list(.SD[ , length(unique(Path)) - 1, by = Spine]$V1)),
                  by = File]
  
  setcolorder(spins, c('NFile', 'File', 'Spines', 'Columns', 'Splits', 'Splices', 'Where'))
  
  spins %class% 'humSpines'
}


#' @export
`[.humSpines` <- function(spines, i, j) {
  if (missing(i) && missing(j)) return(spines)
  
  if (missing(j)) j <- seq_len(ncol(spines) - 2)
  if (missing(i)) i <- seq_len(nrow(spines)) 
  
  if (is.character(j)) j <- pmatch(j, colnames(spines)) - 2
  if (is.character(i)) i <- pmatch(i, spines$File)
  
  spines <- popclass(spines)
  spines <- spines[i, c(1, 2, j + 2), with = FALSE]
  
  spines %class% 'humSpines'
}



print.humSpines <- function(spinemat, showall = TRUE) {
  #' @export
  if (nrow(spinemat) < 1) {cat('Empty humdrumR object.\n') ; return(invisible(NULL))}
  
  spinemat <- popclass(spinemat)
  spinemat$NFile <- num2str(spinemat$NFile) %str+% ":"
  spinemat[ , In := sapply(Where, sum %.% GT(0))]
  where <- spinemat$Where
  spinemat[ , 'Where' := NULL]
  spinemat[ , Columns := Columns - Spines]
  anypaths <- any(spinemat$Columns > 0)
  
  
  if (nrow(spinemat) == 1) {
    cat(spinemat$File, ': ', spinemat$Spines, ' spines', if (anypaths) ' : ' %str+% spinemat$Columns %str+% {if ( spinemat$Columns > 1) ' paths' else ' path'} else '', '\n', sep = '')
    where <- where[[1]]
    
    if (anypaths) {
      cat('\tSpine ' %str+% 1:spinemat$Spines %str+% ' : ' %str+% ifelse(where == 0, '', where) %str+% '\n', sep = '')
    }
    
  } else {
    setcolorder(spinemat,
                c('NFile', 'File', 'Spines',
                  'Columns', 'In', 'Splits', 'Splices'))
  

    cols <- if (anypaths) 1:7 else 1:3
    colNms <- c('', 'File', 'Spines', '+ Paths', 'In', '*^', '*v')[cols]
      
    lenCol <- pmax(c(0,8,0,0,0,0,0)[cols], #Tallies: is 8 long
                   nchar(colNms), sapply(spinemat[ , cols, with = FALSE], max %.% nchar)) + 2
    
    if (showall) {
      cat(padder(colNms, lenCol), '\n', sep = '')
      cat('#' %str*% sum(lenCol), '\n', sep = '')
      
      spinemat[ , { row <- unlist(.SD)
                    if (Columns == 0) row[4:7] <- ' '
                    paste(padder(row, lenCol), collapse = '') 
                  }, 
                by = 1:nrow(spinemat)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat('#' %str*% sum(lenCol), '\n', sep = '')
      
    }
  
    cat(padder(colNms, lenCol), '\n\n', sep = '')
  
    #
    cat(padder(c('Tallies:'), sum(lenCol[1:2])), '\n', sep = '')
    
    tab <- spinemat[ , table(Spines, Columns)]
    for (i in 1:nrow(tab)) {
      row <- c('', sum(tab[i, ]), 'with ' %str+% (rownames(tab)[i]))
      row <- padder(row, lenCol[1:3])
      if (anypaths) {
        notzero <- which(tab[i, ] > 0)
        row <- c(row, ' (', glue::collapse(tab[i, notzero] %str+% '*' %str+% colnames(tab)[notzero], sep = ' paths, ', last = ', and '), ')')
      }
      cat(row, '\n', sep = '')
    }
  
  }
  invisible(NULL)
}


#### Interpretations ----





interpretations <- function(humdrumR) {
  #' Summarize humdrum corpus interpretations.
  #' 
  #' This function provides a summary of the interpretations in the pieces of a humdrumR corpus.
  #' @export
  humdrumR <- indexGLIM(humdrumR, targets = 'I')
  humtab <- getHumtab(humdrumR, types = 'I')
                           
  
  # Tandem
  tandem <- humtab[!grepl('^\\*\\*', Token) &   
                   !Token %in% c('*', '*-', '*v', '*^') &
                   !grepl('\\*>.*', Token)]
  tandem[ , ID := factor(idTandem(Token))]
  tandemN  <- do.call('rbind', tandem[, .(list(table(ID))), by = NFile]$V1)
  rownames(tandemN) <- unique(humtab$File)
  tandemUN <- do.call('rbind', tandem[, .(list(tapply(Token, ID, length %.% unique))), by = NFile]$V1)
  tandemUN[is.na(tandemUN)] <- 0L
  
  tandIDs  <- levels(tandem$ID)
  tandemIn <- do.call('rbind', tandem[, .(list(colSums(tapply(ID, list(Spine,ID), length), na.rm = TRUE))), by = NFile]$V1)
  
  
  # Exclusive
  exclusive <- humtab[grepl('^\\*\\*', Token)]
  exclusive$Token <- factor(exclusive$Token)
  exclusiveN <- do.call('rbind', exclusive[ , .(list(table(Token))), by = NFile]$V1)
  rownames(exclusiveN) <- unique(humtab$NFile)
  
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
  interpmat$NFile <- num2str(rownames(interps$Exclusive)) %str+% ":"
  interpmat$File  <- rownames(interps$Tandem$Number)
  setcolorder(interpmat, c('NFile', 'File', head(colnames(interpmat), -2)))
  
  if (nrow(interpmat) == 1) {
    cat(interpmat$NFile, '\n')
    cat("Exclusive: ")
    interps$Exclusive[ , cat(Exclusive), by = Spine]
    cat('\n')
    cat("Tandems:")
    interps$Tandem[ , cat(colnames(.SD) %str+% '\n'), by = Spine]
    
    
  } else  {
    colNms <- c('', 'File', colnames(interpmat)[-1:-2])
    colKeys <- character(length(colNms))
    colKeys[3 + Nexclusive] <-   '(Total.Unique.Spines)'
    
    lenCol <- pmax(interpmat[ , sapply(.SD, max %.% nchar)], 
                   nchar(colnames(interpmat))) + 2
    # lenCol[2] <- max(8, lenCol[2]) # to make enough room for "Unique:"
    # lenCol[3] <- max(9, lenCol[3]) # to make enough room for "Unique:"
    
    if (showall) {
      cat(padder(colNms, lenCol), '\n', sep = '')
      cat(padder(colKeys, lenCol), '\n', sep = '')
      cat('#' %str*% sum(lenCol), '\n', sep = '')
      
      interpmat[ , { row <- unlist(.SD)
      paste(padder(row, lenCol), collapse = '') 
      }, 
      by = 1:nrow(interpmat)]$V1 -> strs
      
      cat(paste(strs, collapse = '\n'), '\n', sep = '')
      cat('#' %str*% sum(lenCol), '\n', sep = '')
      
    }
 
    cat(padder(colNms, lenCol), '\n', sep = '')
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

