#### reading files ----

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
  filepattern <- tail(dir_file, 1)
  
  unlist(
    lapply(matchingdirs, 
           function(curdir) {
             Filter(function(x) !x %in% list.dirs(curdir, recursive = FALSE, full.names = FALSE),
                    list.files(path = curdir, pattern = filepattern, recursive = recursive)
                    ) -> files
             
            if(length(files) > 0) paste(curdir, files, sep = '/') else NA
    })
  ) -> matchingfiles
  
  matchingfiles <- matchingfiles[!is.na(matchingfiles)]
  
  if (length(matchingfiles) == 0) return(character(0)) 
  
  matchingfiles <- str_replace(matchingfiles, '^\\./', '')
  
  unique(unlist(Filter(Negate(is.na), matchingfiles))) -> output
  
  #
  
  if (any(grepl('^\\.\\./|\\.\\./', output))) {
    curd   <- getwd()
    output <- paste0(curd, '/', output)
    output <- unlist(lapply(output, function(path) { 
      path <-  strsplit(path, '/')[[1]]
      while(any(path == '..')) {
        hit  <- which(path[-1] == '..' & head(path, -1) != '..')
        path <- path[-hit : (-(hit + 1))] 
      }
      paste(path, collapse = '/')
    }
    ))
  }
  
  if (length(output) == 0) character(0) else output
}


readFiles = function(patterns, ..., regex = FALSE, recursive = FALSE, verbose = TRUE) {
  if(!missing(...)) patterns = c(patterns, unlist(list(...)))
  filenames = unique(unlist(lapply(patterns, pickFiles, regex = FALSE, recursive = recursive)))
  
  if (length(filenames) == 0) {
    if (verbose) cat('\n\t', 'No files match search pattern. None loaded.\n')
    return(NULL)
  } 
  
  if (verbose)  cat(glue('Reading {num2str(length(filenames))} files...'))

  setNames(lapply(filenames, readLinesFast), filenames)
}


shortFileNames <- function(fns) {
  stepin <- str_replace(fns, '[^/]*/', '')
  
  duples <- duplicated(stepin) | rev(duplicated(rev(stepin))) | 
    stepin == fns | stepin == '' | is.na(stepin)
  ifelse(duples, fns, Recall(stepin))
  
}

########################### readHumdrum ----





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
readHumdrum = function(pattern, ..., regex = FALSE, recursive = FALSE, validate = FALSE, verbose = TRUE) {
#' @export
 files <- readFiles(pattern, ..., regex = FALSE, recursive = recursive, verbose = verbose)
 if (is.null(files)) return(NULL)
 
 if (validate) Map(humdrumValidate, files, names(files))
 
 data <- lapply(files, parseRecords)
 
 #
 shortfilenames <- shortFileNames(names(files))
 
 data <- Map(data, seq_along(data), names(files), shortfilenames,
             f = function(dt, n, fn, sfn) {
               dt$NFile = n
               dt$File = sfn
               dt$FullFileName = fn
               dt
             })
 
 humtab <- data.table::rbindlist(data, fill = TRUE)
 humtab[ , Type := parseTokenType(Token)]
 humtab[ , Null := Token %in% c('.', '!', '*', '=')]
 humtab[ , Global := is.na(Spine)]
 #
 tandemTab <- tandemTable(humtab$Tandem)
 humtab <- cbind(humtab, tandemTab)
 
 cat('Done!\n')
 
 makeHumdrumR(humtab, pattern)

 
}




parseRecords = function(records) {
  
  global = parseGlobal(records)
  local  = parseLocal(records)
  
  data = if (length(global) == 1 && is.na(global)) local else rbind(global$Data, local, fill = TRUE)
  
  data$NData = rep(NA, nrow(data))
  D <- !grepl('^[!=*]', data$Token)
  data$NData[D] = match(data$Record[D],  sort(unique(data$Record[D])))
  
  if (!(length(global) == 1 && is.na(global))) cbind(data, global$Table) else data
  
}

parseGlobal = function(records) {
  globalr = grep('^!!', records, useBytes = TRUE)
  if (length(globalr) == 0) return(NA)
  
  globalrecords = records[globalr]
  
  types <- parseTokenType(globalrecords)
  refind = grepl('^!!!', globalrecords)
  list(Data  = data.table(Token = globalrecords, Record = globalr, Type  = types),
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
  
  # refVals <- tapply(refVals, refKeys, function(x) paste(x, collapse = '\n'), simplify = FALSE)
  refVals <- tapply(refVals, refKeys, c, simplify = FALSE)
  refVals <- Map(`names<-`, refVals, tapply(refNums, refKeys, c, simplify = FALSE))
  
  
  # as.data.table(setNames(lapply(refVals, list), names(refVals)))
  as.data.table(lapply(refVals, function(ref) if (len1(ref)) ref else list(ref)))
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
          grepl('*-', init(localrecords), fixed = TRUE))) {
    local = padSpinePaths(local)
  }

  ###mat is character matrix
  ###(row = record, col = spine/subspine)
  mat <- stri_list2matrix(local, byrow = TRUE)
 
  ####### Multistop tokens
  #flatten spines and get new recordns, and stopNs, for each.
  tokens   <- setNames(as.vector(mat), rep(recordn, ncol(mat)))

  ## get spine and path numbers of appropriate length
  SpineNumbers <- cumsum(apply(mat, 2,  function(spine) any(spine != 'xxx') && grepl('^\\*\\*', spine[spine != 'xxx'][1])))
  SpinePaths   <- unlist(tapply(SpineNumbers, SpineNumbers, seq_along, simplify = TRUE)) - 1
  Columns      <- seq_along(SpineNumbers)

  #sections
  sections <- parseSections(mat[ , 1])
  sections <- lapply(sections, setNames, nm = recordn)
  measures <- parseMeasures(mat[ , 1])
  measures <- lapply(measures, setNames, nm = recordn)

  #interpretations
  exclusivestandems <- parseInterpretations(mat)
  exclusives <- exclusivestandems$Exclusive
  tandems <- exclusivestandems$Tandem

  tandems <- setNames(unlist(tandems), names(tokens))

  #If there are multistops, the spines are no longer the same lengths, and have different recordns.
  tokens <- parseMultiStops(tokens)
  # get rid of stuff before period, leaving only stop number
  stopNs <- as.numeric(gsub('^.*\\.', '', names(tokens)))
  # get rid of stuff after period, leaving just record number (don't make numeric yet)
  recordns <- gsub('\\..*$', '', names(tokens))

  # expand objects to match recordn
  # recordns are still characters. This is necessarry, because we use them as names to index other objects.
  tandems <- tandems[recordns]
  measures <- as.data.table(lapply(measures, function(m) m[recordns]))
  
  spineLengths <- nrow(mat) + apply(mat[!grepl('^[*!=]', mat[ , 1]), ], 2, function(col) sum(stringi::stri_count_fixed(col, ' ')))
  Columns <- rep(Columns, spineLengths) 
  # Don't need recordns to be characters anymore.
  recordns <- as.numeric(recordns)

  DaTa <- data.table(Token = tokens,
                     Column = Columns,
                     Spine = SpineNumbers[Columns],
                     Path  = SpinePaths[Columns],
                     Record = recordns,
                     Stop = stopNs,
                     Exclusive = exclusives[Columns],
                     Tandem = tandems,
                     Section = NA,
                     measures)

  # if (!is.null(sections)) {
            # sections <- as.data.table(lapply(sections, function(s) s[match(recordns, rep(names(s), ncol(mat)))]))
            # DaTa <- cbind(DaTa, sections)
  # }


  DaTa <- DaTa[Token != 'xxx'] # | is.na(Token)]

  DaTa

}


parseTokenType <- function(spine) {
  type <- rep('D', length(spine))
  type[grepl('^!'  , spine)]    <- 'L'
  type[grepl('^!!' , spine)]    <- 'G'
  type[grepl('^\\*', spine)]    <- 'I'
  type[grepl('^=', spine)]      <- 'M'
  type[spine == '.']            <- 'd'
  type
}

  

splitColumns <- function(records) { stri_split_fixed(records, pattern = '\t') }


ditto <- function(spine) {
 datainds <- grep('^[^!=*]', spine)
 
 data <- spine[datainds]
 
 notnull <- data != '.' & !is.na(data)
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
           
           interpind <- rev(grep('^\\*[^*->^v]', spine))
           interpind <- interpind[interpind > tail(stri_startswith_fixed(spine, '**'), 1)]
           interps <- spine[interpind]
           
           setNames(sapply(seq_along(spine), function(i) paste(interps[i >= interpind], collapse = ',')), 
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
    
  Map(function(recn, n) {gsub('0\\.', '.', recn %str+% (.01*seq_len(n)))}, 
      names(spine), 
      nspaces + 1) -> multiRecordn
  spine[!grepl('^[!=*]', spine)] <- strsplit(spine[!grepl('^[!=*]', spine)], split = ' ')
  
  setNames(unlist(spine), unlist(multiRecordn))
  # list(Spine = unlist(spine), MultiIndices = multiIndices)
}

parseSections <- function(vec) {
  if (!(any(grepl('^\\*>', vec)))) return(NULL)
  
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
  
  lapply(list(BarN = Bars, DoubleBarN = Doubles, BarLabel = BarLabels), 
         function(b) setNames(b, names(vec)))
}




