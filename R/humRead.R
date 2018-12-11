#### reading files ----
# These are helper functions used by readHumdrum

readLinesFast <- function(fname, x) {
  s <- file.info(fname)$size 
  buf <- readChar(fname, s, useBytes = TRUE)
  buf <- stringi::stri_enc_tonative(buf)
  stringi::stri_split_fixed(buf, "\n", omit_empty = TRUE)[[1]] 
}


pickFiles <- function(pattern, recursive = FALSE) {
          # This function searches for files within directories using regular expressions.
          # These are kind of like Unix "glob" patterns, but instead use regex syntax (to be consistent with rest of package).
          # See the documentation for readHumdrum for use details.
 
  cursep <- .Platform$file.sep
  
  dir_file <- strsplit(pattern, cursep, fixed = TRUE, 
                       useBytes = TRUE)[[1]]
  
  if (length(dir_file) == 1) dir_file <- c('.', dir_file)
  
  # Find matching directories
  dirpattern <- head(dir_file, -1)
  
  if (dirpattern[1] == '~') {
            dirpattern <- dirpattern[-1]
            initial <- path.expand('~')
  } else { 
            initial <- '.' 
  }
  
  Reduce( function(cur, rest) {
            unlist( lapply(cur, 
                           function(curdir) { 
                                     hits <- if (rest %in% c('..', '.')) {
                                               rest
                                     } else {
                                               grep(rest[1], useBytes = TRUE,
                                                    list.dirs(path = curdir, recursive = FALSE, full.names = FALSE), 
                                                    value = TRUE)
                                     }
                                     paste(curdir, hits, sep = cursep) 
                           } 
              )
              )
            },
          x = dirpattern, 
          init = initial
  ) -> matchingdirs
  
  matchingdirs <- matchingdirs[!stringr::str_detect(matchingdirs, paste0(cursep, cursep))]
  
  if (length(matchingdirs) == 1 && matchingdirs == paste0('.', cursep)) stop('\n\tNo directories match search\n\n')
  
  # Find matching files in those directories
  filepattern <- tail(dir_file, 1)
  
  unlist(
            lapply(matchingdirs, 
                   function(curdir) {
                             Filter(function(x) !x %in% list.dirs(curdir, recursive = FALSE, full.names = FALSE),
                                    list.files(path = curdir, pattern = filepattern, recursive = recursive)
                             ) -> files
                              
                             if (length(files) > 0) paste(curdir, files, sep = '/') else NA
                   })
  ) -> matchingfiles
  
  matchingfiles <- matchingfiles[!is.na(matchingfiles)]
  
  if (length(matchingfiles) == 0) return(character(0)) 
  
  matchingfiles <- str_replace(matchingfiles, paste0("^\\.", cursep), '')
  
  unique(unlist(Filter(Negate(is.na), matchingfiles))) -> output
  
  #
  
  if (any(grepl(paste0("^\\.\\.", cursep, "|", "\\.\\.", cursep), output))) {
    curd   <- getwd()
    output <- paste0(curd, cursep, output)
    output <- unlist(lapply(output, function(path) { 
      path <-  strsplit(path, cursep)[[1]]
      while(any(path == '..')) {
        hit  <- which(path[-1] == '..' & head(path, -1) != '..')
        path <- path[-hit : (-(hit + 1))] 
      }
      paste(path, collapse = cursep)
    }
    ))
  }
  
  if (length(output) == 0) character(0) else output
}


readFiles <- function(patterns, ..., recursive = FALSE, verbose = TRUE) {
          # takes in a regex-glob pattern(s) and reads (quickly) all the matching files as lines.
          # If verbose is true, it warns how many files are going to be read before starting.
          # recursive is passed as argument to pickFiles
          
  patterns  <- c(patterns, unlist(list(...)))
  filenames <- unique(unlist(lapply(patterns, pickFiles, recursive = recursive)))
  
  if (length(filenames) == 0) {
    if (verbose) cat('\n\t', 'No files match search pattern. None loaded.\n')
    return(NULL)
  } 
  
  cat(glue('Reading {num2str(length(filenames))} files...'))
  
  if (verbose) {
            cat('\n')
            cat(filenames, sep = '\n')
  }
  
  files <- lapply(filenames, readLinesFast)
  names(files) <- filenames
  
  cat("Done!\n")
  
  files
}

shortFileNames <- function(fns) {
          # This function takes a list of full directory paths
          # and finds the shortest unique version of each one.
          # In most cases, the final final name is returned,
          # but if two (or more directores) contain files 
          # with the same names, their directory names are retained
          # to distinguish them
          stepin <- stringr::str_replace(fns, '[^/]*/', '')
          
          duples <- duplicated(stepin) | rev(duplicated(rev(stepin))) | 
                    stepin == fns | stepin == '' | is.na(stepin)
          ifelse(duples, fns, Recall(stepin))
}


########################### readHumdrum ----





#' Read humdrum files into R
#' 
#' This function reads one or more humdrum files to create a \code{\link[humdrumR:humtable]{humdrum table}}
#' then builds a \code{\linkS4class{humdrumR}} data object around the table.
#' 
#' 
#' @param pattern 
#' character: One or more patterns used to identify files to read. Each pattern represents
#' a file path, using normal (system appropriate) conventions (i.e., directories separated by \code{"/"}, 
#' \code{'~'} at beginning to indicate home, \code{".."} to indicate directory above working directory, etc.). Each directory
#' and the final file pattern are treated as regular expressions. Thus, we can say things like \code{"../^A.*/.*krn"}, which would
#' match any kern files in any directory beginning with a capital \code{"A"} in the directory above the current working directory.
#' 
#' @param ... Any additional arguments are interpreted as additional directory regular-expression patterns, appended to the
#' \code{pattern} argument.
#' 
#' @param recursive 
#' logical: If \code{TRUE}, the final part of the serach pattern (i.e., the file search) is searched for recursively through all sub directories.
#' 
#' @param validate
#' logical: If \code{TRUE}, the function \code{\link{validateHumdrum}} is called on each file before parsing, to check
#' if the files are valid humdrum.
#' 
#' @param verbose
#' logical: If \code{TRUE}, the names of matching files are printed before parsing begins.
#' 
#' @examples 
#' readHumdrum(".*krn$") # loads all files ending with "krn" in the currect directory
#' 
#' readHumdrum('^Composers$/^Be|^Mo/.*/^Joined$/.*krn$') 
#' # Goes inside the directory "Composers".
#' # Inside "Composers" looks for directories that start with "Be" or "Mo".
#' # If there are any "Be|Mo" matching directories within "Composers", matches all directories within them.
#' # Within these directories, looks for directories called "Joined".
#' # If there are any directories called "Joined", loads all files (if any) that end with "krn".
#' 
#' @export
readHumdrum = function(pattern, ..., recursive = FALSE, validate = FALSE, verbose = FALSE) {
 files <- readFiles(pattern, ..., recursive = recursive, verbose = FALSE)
 if (is.null(files)) return(NULL)
 
 if (validate) Map(validateHumdrum, files, names(files))
 
 cat(paste0('Parsing ', num2str(length(names(files))), ' files...'))
 
 humtabs <- if (verbose) {
          Map(function(file, filename) { cat(filename, '\n') ; parseRecords(file)}, 
              files, names(files)) 
 } else {
          lapply(files, parseRecords)
 }
 
 #
 shortfilenames <- shortFileNames(names(files))
 
 humtabs <- Map(function(dt, n, fn, sfn) {
               dt$NFile = n
               dt$File = sfn
               dt$FullFileName = fn
               dt
             },
             humtabs, seq_along(humtabs), names(files), shortfilenames)
 
 humtab <- data.table::rbindlist(humtabs, fill = TRUE)
 humtab[ , Type := parseTokenType(Token)]
 humtab[ , Null := Token %in% c('.', '!', '*', '=')]
 humtab[ , Global := is.na(Spine)]
 #
 tandemTab <- tandemTable(humtab$Tandem)
 if (!is.null(tandemTab)) humtab <- cbind(humtab, tandemTab)
 
 cat('Done!\n')
 
 makeHumdrumR(humtab, pattern)

 
}




parseRecords <- function(records) {
          # This function is the biggest part of readHumdrum
          # It takes a character vector representing the records 
          # in a single humdrum file, and outputs a data.table (an incomplete humdrum table).
  
  global <- parseGlobal(records)
  local  <- parseLocal(records)
  
  humtab <- if (length(global) == 1 && is.na(global)) local else rbind(global$Data, local, fill = TRUE)
  
  humtab$NData <- rep(NA, nrow(humtab))
  D <- !grepl('^[!=*]', humtab$Token)
  humtab$NData[D] <- match(humtab$Record[D],  sort(unique(humtab$Record[D])))
  
  if (!(length(global) == 1 && is.na(global))) cbind(humtab, global$Table) else humtab
  
}

parseGlobal <- function(records) {
          # This function words for parseRecords
          # It takes a character vectors of records 
          # from a single humdrum file and outputs 
          # two things: 
          #         Data, which is a data.table (the beginnings of a humdrum table), representing the tokens.
          #         Reference, (also a data.table, which will fold into a humdrum table) representing the reference records.
  globalr <- grep('^!!', records, useBytes = TRUE)
  if (length(globalr) == 0) return(NA)
  
  globalrecords <- records[globalr]
  
  types <- parseTokenType(globalrecords)
  refind <- grepl('^!!!', globalrecords)
  list(Data  = data.table(Token = globalrecords, Record = globalr, Type  = types),
       Table = parseReference(globalrecords[refind]))
}

parseReference <- function(refrecords) {
          # This parses a character vector of reference records into
          # a data.table with columns indicating reference keys
          # and rows holdingtheir values
  refrecords <- stringi::stri_split_fixed(refrecords, ':', n = 2)
  refKeys    <- unlist(lapply(refrecords, function(r) sub('^!!!', '', r[1])))
  refVals    <- stringi::stri_trim_both(unlist(lapply(refrecords, '[', 2)))
  
  
  #multiple keys
  refNums <- as.numeric(stringi::stri_match_last_regex(refKeys, '[0-9][0-9]*$'))
  refNums[is.na(refNums)] <- '1'
  refKeys <- stringi::stri_replace_all_regex(refKeys, '[0-9]*$', '')
  
  # refVals <- tapply(refVals, refKeys, function(x) paste(x, collapse = '\n'), simplify = FALSE)
  refVals <- tapply(refVals, refKeys, c, simplify = FALSE)
  refVals <- Map(`names<-`, refVals, tapply(refNums, refKeys, c, simplify = FALSE))
  
  
  # as.data.table(setNames(lapply(refVals, list), names(refVals)))
  as.data.table(lapply(refVals, function(ref) if (length(ref) == 1) ref else list(ref)))
}


parseLocal <- function(records) {
  # This is the most substantive chunk of 
  # parseRecords (and thus, read Humdrum)
  # Takes a character vector of local records
  # returns a data.table (the makings of a humdrum table)
          
  recordn <- grep('^!!', records, invert = TRUE, useBytes = TRUE)
  localrecords <- records[recordn]

  ###local is list of vectors of strings
  ###(each string = one record)
  local <- stringi::stri_split_fixed(localrecords, pattern = '\t')
  
  
  #spine paths
  if (any(grepl('*^', localrecords, fixed = TRUE),
          grepl('*v', localrecords, fixed = TRUE),
          grepl('*+', localrecords, fixed = TRUE),
          grepl('*-', init(localrecords), fixed = TRUE))) { 
            # big if statement decides whether parseSpinePaths needs to be called.          
            # It's worthwhile to waste a little time to do it here.
    local <- padSpinePaths(local)
  }

  ###mat is character matrix
  ###(row = record, col = spine/subspine)
  mat <- stringi::stri_list2matrix(local, byrow = TRUE)
 
 
  #flatten spines and get recordns for each.
  rownames(mat) <- recordn
  tokens   <- setNames(as.vector(mat), rep(recordn, ncol(mat)))

  ## get spine and path numbers of appropriate length
  SpineNumbers <- cumsum(apply(mat, 2,  function(spine) any(spine != 'xxx') && grepl('^\\*\\*', spine[spine != 'xxx'][1])))
  SpinePaths   <- as.vector(tapply(SpineNumbers, SpineNumbers, seq_along, simplify = TRUE)) - 1L
  Columns      <- seq_along(SpineNumbers)

  #sections
  sections <- parseSections(mat[ , 1])
  barlines <- parseBarlines(mat[ , 1])

  #interpretations
  exclusivestandems <- parseInterpretations(mat)
  exclusives <- exclusivestandems$Exclusive
  tandems    <- exclusivestandems$Tandem

  tandems    <- setNames(unlist(tandems), names(tokens))

  tokens <- parseMultiStops(tokens)
  #If there are multistops, the spines are no longer the same lengths, and have different recordns.
  stopNs <- as.integer(gsub('^.*\\.', '', names(tokens)))
  # get rid of stuff before period, leaving only stop number
  recordns <- gsub('\\..*$', '', names(tokens))
  # get rid of stuff after period, leaving just record number (don't make numeric yet)

  # expand objects to match recordn, which may have changed when multistops were introduced
  # recordns are still characters. This is necessarry, because we use them as names to index other objects.
  sections <- sections[recordns, , drop = FALSE]
  barlines <- barlines[recordns, , drop = FALSE]
  tandems  <- tandems[recordns]
  
  spineLengths <- nrow(mat) + apply(mat[!grepl('^[*!=]', mat[ , 1]), , drop = FALSE], 2, function(col) sum(stringi::stri_count_fixed(col, ' ')))
  Columns      <- rep(Columns, spineLengths)
  
  # Don't need recordns to be characters anymore.
  recordns  <- as.integer(recordns)
  
  humtab <- data.table::data.table(Token = tokens,
                                 Column = Columns,
                                 Spine = SpineNumbers[Columns],
                                 Path  = SpinePaths[Columns],
                                 Record = recordns,
                                 Stop = stopNs,
                                 Exclusive = exclusives[Columns],
                                 Tandem = tandems,
                                 barlines)

  if (ncol(sections) > 0) humtab <- cbind(humtab, sections)

  humtab <- humtab[Token != 'xxx'] # "xxx" tokens were inserted as padding by parseSpinePaths

  humtab

}


parseTokenType <- function(spine) {
  # This is called by parseRecords
  # simply categories records by spine type,
  # to create the humdrum tables Type field.
  type <- rep('D', length(spine))
  type[grepl('^!'  , spine)]    <- 'L'
  type[grepl('^!!' , spine)]    <- 'G'
  type[grepl('^\\*', spine)]    <- 'I'
  type[grepl('^=', spine)]      <- 'M'
  type[spine == '.']            <- 'd'
  type
}

  

padSpinePaths <- function(local) {
  # Used by parseLocal to make sense of spine paths.
  # This function takes a list of character vectors (each representing a list of tokens from one record)
  # identifies spine paths in them, and pads the records with "xxx" such that 
  # subspines (spine paths) are grouped within their spine, and all rows are the same length.
  ####NEED TO ADD OPTIONS FOR ** AND *-
          
  minpath <- min(sapply(local, Position, f = function(x) x %in% c('*^', '*v', '*+', '*-')), na.rm = TRUE)
  
  lapply(minpath:max(lengths(local)),
         function(j) {
           open  <- sapply(local, function(row) length(row) >= j && (row[j] == '*^' || row[j] == '*+'))
           close <- sapply(local, function(row) length(row) > j && all(row[j:(j + 1)] == '*v'))
           if (any(open) | any(close)) {
             
             open  <- c(FALSE, head(open, -1))
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
  
}


parseInterpretations <- function(spinemat) {
  # This function is called by parseLocal to create the basic
  # Exclusive and Tandem interpretation Field for the humdrum table.
  # It takes a spinematrix (a character matrix representing
  # humdrum records and padded columns)
  # It returns 1) a list of character vectors representing exclusive interpretations
  # and 2) a list of character vectors representing comma-delineated, backwards-cumulative tandem interpretaions
  # i.e.  c("*clefF4", 
  #         "*F:,*clefF4", 
  #         "*k[b-],*F:,*clefF4")
  spinemat[is.na(spinemat)] <- 'xxx' # Not sure why this is necassary (Nat, December 2018)
  
  lapply(1:ncol(spinemat), 
         function(j) { 
           if (j > 1 && any(spinemat[ , j] == 'xxx')) {
             spinemat[spinemat[ , j] == 'xxx', j] <<- spinemat[spinemat[ , j] == 'xxx', j - 1] 
           }
           spine <- spinemat[ , j]
           
           interpind <- rev(grep('^\\*[^*->^v]', spine))
           interpind <- interpind[interpind > tail(stringi::stri_startswith_fixed(spine, '**'), 1)]
           interps <- spine[interpind]
           
           setNames(sapply(seq_along(spine), function(i) paste(interps[i >= interpind], collapse = ',')), 
                    rownames(spinemat))
         } 
  ) -> tandemIs
  
  exclusiveI <- apply(spinemat, 2, function(spine) tail(spine[stringi::stri_startswith_fixed(spine, '**')], 1))
  
  list(Exclusive = exclusiveI, 
       Tandems   = tandemIs)
}

parseMultiStops <- function(spine) {
  # This function is used by parseLocal to stretch out spines 
  # containing multi stops (i.e., tokens separate by spaces) represented by their own tokens.
  # This takes a character vector representing a single
  # spine of tokens, with the names of the vector representing
  # record numbers.
  # It returns a new character vector of tokens, with multi stops 
  # split into their on character strings, and with new record numbers enumerating
  # any multi stops "X.01", "X.02", "X.03", etc.
  # Single-stop tokens get labellex "X.01", while non-data tokens get "X.0"
  nspaces <- stringi::stri_count_fixed(spine, ' ') #count multistops 
  nspaces[grepl('^[!*=]', spine)] <- 0 #except not in non-data tokens
  
  if (!any(nspaces > 0)) return(setNames(spine, paste0(names(spine), '.01'))) # if there are no multistops, just as ".01" to names
    
  Map(function(recn, n) gsub('0\\.', '.', paste0(recn, .01 * seq_len(n))), 
      names(spine), 
      nspaces + 1) -> multiRecordn
  spine[!grepl('^[!=*]', spine)] <- strsplit(spine[!grepl('^[!=*]', spine)], split = ' ')
  
  setNames(unlist(spine), unlist(multiRecordn))
}

parseSections <- function(spine) {
  # This function is called by parseLocal in order
  # to parse section information in the humdrum data
  # and make Section fields (for the humdrum table) as appropriate.
  # It takes a character vector representing a spine
  # and returns a data.frame of section fields (or NULL if none)
  if (!(any(grepl('^\\*>', spine)))) return(data.frame(row.names = names(spine)))
  
  sectionTypes <- unique(stringi::stri_extract_all_regex(grep('\\*>[^>]*>[^>]', spine, value = TRUE), '\\*>[^>]*>'))
  sectionTypes <- paste0('^\\', c('*>[^>]*$', sectionTypes))
  
  nesting <- max(stringi::stri_count_fixed(str_sub(grep('^\\*>', spine, value = TRUE), 3L), '>'))
  
  matrices <- lapply(sectionTypes, 
                     function(type) {
                       hits  <- grepl(type, spine)
                       if (!any(hits)) return(NULL)
                       
                       depth <- rep(NA_integer_, length(spine))
                       depth[hits] <-stringi::stri_count_fixed(str_sub(spine[hits], 3L), '>')
                       
                       typemat <- if (any(depth != 0, na.rm = TRUE)) { 
                         sapply(sort(unique(depth[depth != 0])), 
                                function(N) { 
                                  cats <- c(NA_character_, spine[hits & depth <= N])
                                  cats[which((hits & depth <  N)[hits & depth <= N]) + 1] <- "" # NA_character_
                                  cats[cumsum(hits & depth <= N) + 1]
                                } ) 
                       } else {
                          matrix(c(NA_character_, spine[hits])[cumsum(hits) + 1], ncol = 1)
                       }
                       name <- paste0('Formal', if (type == '^\\*>[^>]*$') '' else gsub('[\\^>*]' , '', type))
                       colnames(typemat) <- rep(name, ncol(typemat))
                       if (ncol(typemat) > 1L) colnames(typemat) <- paste0(colnames(typemat), c("", paste0("_", 1:(ncol(typemat) - 1))))
                      
                       rownames(typemat) <- names(spine)
                       
                       if (type == '^\\*>[^>]*$') gsub('^\\*>', '', typemat) else gsub(paste0(type, '>*'), '', typemat)
                     } )
  as.data.frame(matrices, stringsAsFactors = FALSE)
}


parseBarlines <- function(spine) {
  # This function is used by parseLocal
  # to extract the three barline related fields from 
  # a humdrum spine.
  # It takes a character vector representing a humdrum spine
  # and returns a data.frame with three columns representing the three barline fields (BarN, DoubleBarN, BarLabel)
          
  Singles <- cumsum(grepl('^=', spine))
  Doubles <- cumsum(grepl('^==', spine))
  
  BarLabels <- str_sub(grep('^=', spine, value = TRUE), 2L)[Singles + 1L]
  
  data.frame(BarN = Singles, 
             DoubleBarN = Doubles, 
             BarLabel = BarLabels, 
             row.names = names(spine), stringsAsFactors = FALSE)
}




