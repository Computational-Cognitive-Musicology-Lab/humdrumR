####################################################
####Reading files
####################################################

readLinesFast = function(fname, x) {
  s = file.info(fname)$size 
  buf = readChar(fname, s, useBytes = TRUE)
  buf = stri_enc_tonative(buf)
  stri_split_fixed(buf, "\n", omit_empty = TRUE)[[1]] 
}


pickFiles = function(pattern, regex = FALSE, recursive = FALSE) {
  if (grepl('/$', pattern)) pattern = paste0(pattern, '*')
  
  dir_file = strsplit(pattern, '/', fixed = TRUE, useBytes = TRUE)[[1]]
  
  if(!regex) {
    dir_file = str_replace(dir_file, '\\*', '.*')
    dir_file = str_replace(dir_file, '\\.', '\\.')
    dir_file[!dir_file %in% c('.', '..', '~')] = paste0('^', dir_file[!dir_file %in% c('.', '..', '~')], '$')
  }
  if (length(dir_file) == 1) dir_file = c('.', dir_file)
  
  # Find matching directories
  dirpattern = head(dir_file, -1)
  
  if(dirpattern[1] == '~') {
    dirpattern = dirpattern[-1]
    initial = '~'
  } else { initial = '.' }
  
  
  
  Reduce(
    function(cur, rest) {
      unlist(
        lapply(cur, 
               function(curdir) { 
                 if(rest %in% c('..', '.')) {
                   hits = rest
                 } else {
                   hits = grep(rest[1], useBytes = TRUE,
                               list.dirs(path = curdir, recursive = FALSE, full.names = FALSE), 
                               value = TRUE)
                 }
                 paste(curdir, hits, sep = '/') 
                 } 
               )
        )
      },
  x = dirpattern, 
  init = initial
  ) -> matchingdirs
  
  matchingdirs = matchingdirs[!str_detect(matchingdirs, '//')]
  
  if(length(matchingdirs) == 1 && matchingdirs == './') { stop('\n\tNo directories match search\n\n') }
  
  # Find matching files in those directories
  filepattern = tail(dir_file, 1)
  
  unlist(
    lapply(matchingdirs, 
           function(curdir) {
             Filter(function(x) !x %in% list.dirs(curdir, recursive = FALSE, full.names = FALSE),
                    list.files(path = curdir, pattern = filepattern, recursive = recursive)
                    ) -> files
             
            if(length(files) > 0) paste(curdir, files, sep = '/') else NA
    })
  ) -> matchingfiles
  
  matchingfiles = matchingfiles[!is.na(matchingfiles)]
  
  if (length(matchingfiles) == 0) return(character(0)) 
  
  matchingfiles = str_replace(matchingfiles, '^\\./', '')
  
  unique(unlist(Filter(Negate(is.na), matchingfiles))) -> output
  
  #
  
  if (any(grepl('^\\.\\./|\\.\\./', output))) {
    curd = getwd()
    output = paste0(curd, '/', output)
    output = unlist(lapply(output, function(path) { 
      path = strsplit(path, '/')[[1]]
      while(any(path == '..')) {
        hit = which(path[-1] == '..' & head(path, -1) != '..')
        path = path[-hit:(-(hit + 1))] 
        }
      paste(path, collapse = '/')
    }
    ))
  }
  
  if (length(output) == 0) character(0) else output
}


readFiles = function(patterns, ..., regex = FALSE, recursive = FALSE) {
  if(!missing(...)) patterns = c(patterns, unlist(list(...)))
  filenames = unique(unlist(lapply(patterns, pickFiles, regex = FALSE, recursive = recursive)))
  
  if( length(filenames) == 0) {
    cat('\n\t', 'No files match search pattern. None loaded.\n')
    return(NULL)
  }

  setNames(lapply(filenames, readLinesFast), filenames)
}


#################################################





#' Read humdrum files into R
#' 
#' This function reads one or more humdrum files into a humdrumR "humdrum.table" object.
#' 
#' 
#' @param pattern 
#' Chracter: Each directory (separated by '/') and the final pattern are treated as regular expressions.
#' 
#' @param recursive 
#' Logical: Should each search term (directories, and files) be searched for recursively through all sub directories? 
#' \code{recursive = TRUE} should only be used with a simple search pattern.
#' 
#' @param validate
#' Logical: Should each humdrum file be validated using the function \code{\link{humdrumValidate}}?
#' 
#' @examples 
#' readHumdrum('*krn$') # loads all files ending with 'krn' in the currect directory
#' 
#' ## UNIX style---no regexes
#' readHumdrum('Composers/Be*/*/Joined/*krn') 
#' # Goes inside the directory "Composers."
#' # Inside "Composers" looks for directories that start with "Be".
#' # If there are any "Be*" matching directories within "Composers" matches all directories within them.
#' # Within these directories, looks for directories called "Joined."
#' # If there are any directories that match "Joined$", loads all files (if any) that end with "krn."
#' 
#' 
#' ## with regexes---notice the use of ^ and $ to make sure we get exact matches
#' #' readHumdrum('^Composers$/^Be|^Mo/.*/^Joined$/.*krn$') 
#' # Goes inside the directory "Composers."
#' # Inside "Composers" looks for directories that start with "Be" or "Mo".
#' # If there are any "Be|Mo" matching directories within "Composers" matches all directories within them.
#' # Within these directories, looks for directories called "Joined."
#' # If there are any directories called "Joined", loads all files (if any) that end with "krn".
#' 
readHumdrum = function(pattern, ..., regex = FALSE, recursive = FALSE, validate = FALSE) {
#' @export
 files <- readFiles(pattern, ..., regex = FALSE, recursive = recursive)
   
 if (validate) Map(humdrumValidate, files, names(files))
 
 data <- lapply(files, parseRecords)
 
 data <- Map(data, seq_along(data), names(files),
             f = function(dt, n, fn) {
               dt$FileNumber = n
               dt$FileName = fn
               dt
             })
 
 humtab <- enHum(data.table::rbindlist(data, fill = TRUE) )
 
 
 new('humdrumR',
     Data = humtab,
     Files = list(Search = pattern, Names = unique(humtab@D$FileName)),
     LoadTime = Sys.time(),
     Apply = list(Token) ~ .)
 
}




parseRecords = function(records) {
  
  global = parseGlobal(records)
  local  = parseLocal(records)
  
  data = if (length(global) == 1 && is.na(global)) local else rbind(global$Data, local, fill = TRUE)
  
  data$DataRecordNumber = rep(NA, nrow(data))
  data$DataRecordNumber[data$Type == 'Data'] = match(data$RecordNumber[data$Type == 'Data'], 
                                                     sort(unique(data$RecordNumber[data$Type == 'Data'])))
  
  if (!(length(global) == 1 && is.na(global))) cbind(data, global$Table) else data
  
}

parseGlobal = function(records) {
  globalr = grep('^!!', records, useBytes = TRUE)
  if (length(globalr) == 0) return(NA)
  
  globalrecords = records[globalr]
  
  refind = grepl('^!!!', globalrecords, useBytes = TRUE)
  
  list(Data  = data.table(Token = globalrecords, 
                          Type = c('Comment', 'Reference')[1 + refind], 
                          RecordNumber = globalr,
                          Null = FALSE),
       Table = parseReference(globalrecords[refind]))
}

parseReference <- function(refrecords) {
  refrecords <- stri_split_fixed(refrecords, ':', n = 2)
  refKeys    <- unlist(lapply(refrecords, function(r) sub('^!!!', '', r[1])))
  refVals    <- stri_trim_both(unlist(lapply(refrecords, '[', 2)))
  
  
  #multiple keys
  refNums <- as.numeric(stri_match_last_regex(refKeys, '[0-9][0-9]*$'))
  refNums[is.na(refNums)] <- '1'
  refKeys <- stri_replace_all_regex(refKeys, '[0-9]*$', '')
  
  refVals <- tapply(refVals, refKeys, c, simplify = FALSE)
  refVals <- Map(`names<-`, refVals, tapply(refNums, refKeys, c, simplify = FALSE))
  
  as.data.table(setNames(lapply(refVals, list), names(refVals)))
}


parseLocal = function(records) {
  ###records is vector of strings 
  ###(each string = one record)
  recordn <- grep('^!!', records, invert = TRUE, useBytes = TRUE)
  localrecords <- records[recordn]
  
  ###local is list of vectors of strings 
  ###(each string = one record)
  local <- splitColumns(localrecords)
  
  #spine paths
  if (any(grepl('*^', localrecords, fixed = TRUE), 
          grepl('*v', localrecords, fixed = TRUE), 
          grepl('*+', localrecords, fixed = TRUE),
          grepl('*-', localrecords[-1], fixed = TRUE))) {
    
    local = padSpinePaths(local)
  } 
    
  ###mat is character matrix 
  ###(row = record, col = spine/subspine)
  mat <- stri_list2matrix(local, byrow = TRUE)
  rownames(mat) <- recordn
  
  #sections
  sections <- parseSections(mat[ , 1])
  measures <- parseMeasures(mat[ , 1])
  
  #iterpretations 
  c('exclusives', 'tandems') %<-% parseInterpretations(mat)
  
  ###spines is list of vectors 
  ###(each vector = spine/subspine)
  spines <- lapply(seq_len(ncol(mat)), function(j) mat[ , j])
  ditto.spines <- lapply(spines, ditto)
  
  SpineNumbers <- cumsum(sapply(spines, function(spine) any(spine != 'xxx') && grepl('^\\*\\*', spine[spine != 'xxx'][1]))) 
  SpinePaths   <- unlist(tapply(SpineNumbers, SpineNumbers, seq_along, simplify = TRUE)) - 1
  
  ####### Multistop tokens
  #flatten spines and get new recordns, and stopNs, for each.
  spines   <- lapply(spines, parseMultiStops) #outputs newspines with names in the format recordn.stopnumber (i.e., 23.01)
  #If there are multistops, the spines are no longer the same lengths, and have different recordns.
  # get rid of stuff before period, leaving only stop number
  stopNs   <- lapply(spines, Compose(asN, strrid('^.*\\.'), names)) 
  # get rid of stuff after period, leaving just record number
  recordns <- lapply(spines, Compose(strrid('\\..*$'), names))           
  
  # expand objects to match new multistop recordn
  # recordns are still characters. This is necessarry, because we use them as names to index other objects.
  tandems     <- Map('[', tandems, recordns)
  ditto.spines <- Map('[', ditto.spines, recordns) # FIX: multistops are not being dittoed separately
  
  measures    <- lapply(recordns, function(ind) lapply(measures, '[', ind))
  sections    <- lapply(recordns, function(ind) lapply(sections, '[', ind))
  
  # Don't need recordns to be characters anymore.
  recordns <- lapply(recordns, asN) 
  
  # token types
  types <- lapply(spines, parseTokenType)
  ###DaTa is list of data.tables
  ###(each data.table = spine/subspine)
  Map(spines, ditto.spines, 
      SpineNumbers, SpinePaths, 
      exclusives,  tandems, 
      types, 
      seq_along(spines), recordns, stopNs,
      measures, sections,
      f = function(spine, ditto.spine,
                   spinenumber, spinepath, 
                   exclusive, tandem, 
                   type,
                   colnumber, recn, stopn,
                   measure, section
                   ) {  
        
           data.table(Token = spine, Ditto = ditto.spine,
                      Spine = spinenumber, Path = spinepath,
                      Exclusive = exclusive,  Tandem = tandem, 
                      Type = type,
                      ColumnNumber = colnumber, RecordNumber = recn, StopNumber = stopn,
                      Bar = measure$BarN,
                      DoubleBarline = measure$DoubleBars,
                      Barline = measure$Labels) -> dt
                         
           for (j in seq_along(section)) { dt[[paste0('Section', names(section)[j])]] <- section[[j]]  }
                         
           dt
    }
  ) -> DaTa
  
  DaTa <- rbindlist(DaTa)
  DaTa <- DaTa[Token != 'xxx']
  # DaTa <- DaTa[!Token %in% c('.', '*', '!')]
  DaTa$Null <- DaTa$Token %in% c('*', '!', '.')
  DaTa

}


splitColumns <- function(records) { stri_split_fixed(records, pattern = '\t') }


ditto <- function(spine) {
 datainds <- grep('^[^!=*]', spine)
 
 data <- spine[datainds]
 
 notnull <- data != '.'
 grps <- cumsum(notnull)
 if (grps[1] == 0) grps <- grps + 1
 
 spine[datainds] <- data[notnull][grps]
 spine
 
}


padSpinePaths <- function(local) {
  minpath <- min(sapply(local, Position, f = function(x) x %in% c('*^', '*v', '*+', '*-')), na.rm = TRUE)
  
  lapply(minpath:max(lengths(local)),
         function(j) {
           open  <- sapply(local, function(row) length(row) >= j && (row[j] == '*^' || row[j] == '*+'))
           close <- sapply(local, function(row) length(row) > j && all(row[j:(j + 1)] == '*v'))
           if (any(open) | any(close)) {
             
             open <- c(FALSE, head(open, -1))
             close <- c(FALSE, head(close, -1))
             pad <- cumsum(as.numeric(open) + -as.numeric(close)) 
             pad <- abs(pad - max(pad))
             local[pad > 0] <<- Map(append, 
                                    x = local[pad > 0], 
                                    values = lapply(pad[pad > 0], function(n) rep('xxx', n)), 
                                    after = j)
           }
           
           close <- sapply(local, function(row) length(row) > j && all(row[j] == '*-'))
           if (any(head(close, -1))) {
             pad <- cumsum(c(FALSE, head(close, -1)))
             local[pad > 0] <<- Map(append, x = local[pad > 0], values = 'xxx', after = j - 1)
           }
         }
  )
  local
  
  ####NEED TO ADD OPTIONS FOR ** AND *-
}


parseInterpretations <- function(spinemat) {
  spinemat[is.na(spinemat)] <- 'xxx'
  
  lapply(1:ncol(spinemat), 
         function(j) { 
           if (j > 1 && any(spinemat[ , j] == 'xxx')) {
             spinemat[spinemat[ , j] == 'xxx', j] <<- spinemat[spinemat[ , j] == 'xxx', j - 1] 
           }
           spine <- spinemat[ , j]
           
           interpind <- grep('^\\*[^*->^v]', spine)
           interpind <- interpind[interpind > tail(stri_startswith_fixed(spine, '**'), 1)]
           interps <- spine[interpind]
           
           setNames(lapply(seq_along(spine), function(i) interps[i >= interpind]), 
                    rownames(spinemat))
         } 
  ) -> tandemIs
  
  exclusiveI <- apply(spinemat, 2, function(spine) tail(spine[stri_startswith_fixed(spine, '**')], 1))
  
  list(Exclusive = exclusiveI, 
       Tandems   = tandemIs)
}

parseMultiStops <- function(spine) {
  nspaces <- stri_count_fixed(spine, ' ') #count multistops 
  nspaces[grepl('^[!*=]', spine)] <- 0 #except not in non-data tokens
  
  if (!any(nspaces > 0)) return(setNames(spine, names(spine) %str+% '.01'))
    
  multiRecordn <- sort(c(names(spine) %str+% '.01', 
                         unlist(Map(nspaces[!is.na(nspaces) & nspaces > 0], which(nspaces > 0),  
                                    f = function(n, ind) { ind + .01 + (.01 * seq_len(n)) }))))
     
  spine[!grepl('^[!=*]', spine)] <- strsplit(spine[!grepl('^[!=*]', spine)], split = ' ')
  
  setNames(unlist(spine), multiRecordn)
  # list(Spine = unlist(spine), MultiIndices = multiIndices)
}

parseSections <- function(vec) {
  if (!(any(grepl('^\\*>', vec)))) return(list(setNames(vector('list', length(vec)), names(vec))))
  
  sectionTypes <- unique(stri_extract_all_regex(grep('\\*>[^>]*>[^>]', vec, value = TRUE), '\\*>[^>]*>'))
  sectionTypes <- paste0('^\\', c('*>[^>]*$', sectionTypes))
  
  nesting <- max(stri_count_fixed(str_sub(grep('^\\*>', vec, value = TRUE), 3L), '>'))
  
  matrices <- lapply(sectionTypes, 
                     function(type) {
                       hits  <- grepl(type, vec)
                       if (!any(hits)) return(NULL)
                       
                       depth <- rep(NA_integer_, length(vec))
                       depth[hits] <- stri_count_fixed(str_sub(vec[hits], 3L), '>')
                       
                       typemat <- if (any(depth != 0, na.rm = TRUE)) { 
                         sapply(sort(unique(depth[depth != 0])), 
                                function(N) { 
                                  cats = c(NA_character_, vec[hits & depth <= N])
                                  cats[which((hits & depth <  N)[hits & depth <= N]) + 1] <- NA_character_
                                  cats[cumsum(hits & depth <= N) + 1]
                                } ) 
                       } else {
                          matrix(c(NA_character_, vec[hits])[cumsum(hits) + 1], ncol = 1)
                       }
                       
                       colnames(typemat) <- str_dup('>', seq_len(ncol(typemat)) - 1)
                       if (type == '^\\*>[^>]*$') gsub('^\\*>', '', typemat) else gsub(paste0(type, '>*'), '', typemat)
                     } )
  
  names(matrices) <- c('', gsub('[\\^>*]' , '', sectionTypes)[-1])
  matrices <- matrices[sapply(matrices, Negate(is.null))]
  columns  <- lapply(matrices, function(mat) setNames(unlist(apply(mat, 1, list), recursive = FALSE), names(vec)))
  
  columns
  
  
}


parseMeasures <- function(vec) {
  Bars = 1 + cumsum(grepl('^=', vec))
  Doubles  = 1 + cumsum(grepl('^==', vec))
  
  BarLabels = str_sub(grep('^=', vec, value = TRUE), 2L)[Bars]
  
  lapply(list(BarNs = Bars, DoubleBars = Doubles, Labels = BarLabels), 
         function(b) setNames(b, names(vec)))
}


parseTokenType <- function(spine) {
  type <- rep('Data', length(spine))
  type[grepl('^!', spine)] <- 'Comment'
  type[grepl('^\\*', spine)] <- 'Interpretation'
  type[grepl('^=', spine)] <- 'Barline'
  type
}

