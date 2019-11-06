
############################################################################## ---
############## Finding files ----


### These functions identify files on the local disc that we want to read.
### As such, they are helpers for readFiles and readHumdrum.
### There is one exported (public-facing) function: findHumdrum.


findFiles <- function(..., recursive = FALSE) {
    # This function is the workhorse behind the readFiles function,
    # (which itself support readHumdrum and findHumdrum).
    
    # The function is used to identify files (excludes directories) on the local system, 
    # based their filenames/paths, using a unique type of regex-based path search pattern. 
    # Details of how these search patterns work can be found in the findHumdrum user documentation.
    # As a part of this process, the recursive argument is (eventually) passed to the base::list.files function.
    #
    #
    # findFiles returns a named list of filepaths.
    
    #### Parse ... arguments into a cartesian collection of patterns, ordered left to right
    patterns <- assemblePatterns(list(...))
    patterns[patterns == ""] <- ".*"
    
    ##### split paths into directories (all but rightmost) and files (rightmost)
    dirpaths  <- dirname(patterns)
    filepaths <- basename(patterns)
    
    #### find matches
    ## dirs
    matchingdirs <- lapply(dirpaths, matchDirs)
    
    ## full paths (incl file)
    matchingpaths <- if (sum(lengths(matchingdirs)) == 0L) {
        replicate(length(matchingdirs), character(0), simplify = FALSE)
        
    } else {
        Map(matchFiles, filepaths, matchingdirs,
            recursive    = recursive)
    }
    
    data.table(Filepath    = matchingpaths,
               Directories = matchingdirs,
               Pattern     = patterns,
               Label       = names(patterns))
    
}

assemblePatterns <- function(patternargs) {
    # This function does work for findFiles
    # It translates one or more regular expression "patternargs" into
    # a cartesian product of specific file-path regular expressions to look for.
    # See the findHumdrum documentation for more explanation.
    
    if (length(patternargs) == 0L) return(c(`_1` = ""))
    
    
    # Cartesian product patterns
    patgrid  <- do.call('expand.grid', c(patternargs, stringsAsFactors = FALSE))
    patterns <- apply(patgrid, 1, paste, collapse = .Platform$file.sep)
    patterns <- gsub(paste0(strrep(.Platform$file.sep, 3), '*'), .Platform$file.sep, patterns)
    
    ## pattern labels
    patlabs <- if (length(unlist(lapply(patternargs, names))) > 0L) {
        patgridlabs <- patgrid
        patgridlabs[] <- sapply(1:ncol(patgrid), 
                                function(i) names(unlist(patternargs))[match(patgrid[,i], unlist(patternargs))] )
        
        patlabs <- apply(patgridlabs, 1, paste, collapse = '')
        patlabs
    } else {
        rep("", length(patterns))
    }
    
    patlabs[patlabs == ""] <- paste0("_", seq_len(sum(patlabs == "")))
    names(patterns) <- patlabs
    #
    patterns
    
}



matchDirs <- function(dirpattern) {
    # This function does work for findFiles.
    # Given directory-path regular expressions (dirpattern) it finds matching directories on the system.
    # See the findHumdrum documentation for more explanation.
    dirpattern <- strsplit(dirpattern, split = .Platform$file.sep)[[1]]
    
    dirpattern <- dirpattern[dirpattern != "."]
    
    if (length(dirpattern) && dirpattern[1] == "") {
        dirpattern <- dirpattern[-1]
        initial <- "/"
    } else { 
        initial <- "." 
    }
    
    Reduce( function(cur, nex) {
        unlist( lapply(cur, 
                       function(curdir) {
                           hits <- if (nex %in% c('..', '.')) {
                               return(paste(curdir, nex, sep = .Platform$file.sep))
                           } else {
                               grep(nex, useBytes = TRUE,
                                    list.dirs(path = curdir, recursive = FALSE, full.names = FALSE), 
                                    value = TRUE)
                           }
                           if (length(hits) > 0L) {
                               paste(curdir, hits, sep = .Platform$file.sep) 
                           } else {
                               character(0)
                           }
                       } 
        )
        )
    },
    x = dirpattern, 
    init = initial
    ) -> matchingdirs
    
    ## remove paths containing //
    matchingdirs <- gsub(paste0(strrep(.Platform$file.sep, 3), '*'), .Platform$file.sep, matchingdirs)   

    ##
    matchingdirs
}

matchFiles <- function(filepattern, matchingdirs, recursive) {
    # This function does work for findFiles.
    # Given file-path regular expressions (filepattern), AND the matchingdirs produced by matchDirs,
    # it finds matching files in all the matched directories.
    # See the findHumdrum documentation for more explanation.
    
    matchingfiles <- 
        lapply(matchingdirs, 
               function(curdir) {
                   paths <- list.files(path = curdir, pattern = filepattern, recursive = recursive)
                   files <- Filter(function(x) !x %in% list.dirs(curdir, recursive = FALSE, full.names = FALSE),
                                   paths)
                 
                   if (length(files) > 0) paste(curdir, files, sep = .Platform$file.sep) else NULL
               })
    
    
    if (length(matchingfiles) == 0L) return(character(0)) 
    
    matchingfiles <- unlist(matchingfiles, use.names = FALSE)
    
    matchingfiles <- stringr::str_replace(matchingfiles, pattern = paste0("^\\.", .Platform$file.sep), replacement = '')
    
    Filter(Negate(is.na), matchingfiles)
    
    
}

istextFile <- function(fpaths) {
    # given file path, asks if the file is a text file
    # returns logical
    # findFiles already checks that files are not directories, so this function doesn't
    
    # directories <- list.dirs(dirname(fpath), full.names = FALSE, recursive = FALSE)
    # if (fpath %in% directories) return(FALSE)
    
    raw <- lapply(fpaths, stringi::stri_read_raw)
    enc <- data.table::rbindlist(stringi::stri_enc_detect2(raw))
    
    !is.na(enc$Encoding) & enc$Confidence > 0.8
}

filterFilesByContent <- function(fileFrame, patterns, combine = `&`) {
    # takes a data.table with filenames 
    # and removes those that don't contain matches to (regex) patterns args.
    # By default, files much match ALL patterns 
    # THIS COULD BE UPDATED to allow ALL (&) or ANY (|)
    matches <- lapply(fileFrame$Files,
                      function(files) {
                          if (length(files) == 0L) return(files)
                          matches <- lapply(patterns, stringr::str_detect, str = files)
                          if (length(matches) > 1L) Reduce(combine, matches) else matches[[1]]
                          
                      })
    
    fileFrame[ , Filepath := Map(`[`, Filepath, matches)]
    fileFrame[ , Files     := Map(`[`, Files    , matches)]
    
    
    fileFrame
}



#' @name readHumdrum
#' @export
findHumdrum <- function(..., contains = NULL, recursive = FALSE, allowDuplicates = FALSE, verbose = FALSE) {
    
    ## First call findFiles with exactly the same arguments
    fileFrame <- readFiles(..., contains = contains, recursive = recursive,  
                           allowDuplicates = allowDuplicates, verbose = verbose)
    
    fileFrame <- isValidHumdrum(fileFrame, errorReport.path = NULL)
    
    fileFrame <- fileFrame[Valid == TRUE]
    fileFrame[ , Valid := NULL]
    
    fileFrame
    
}


###################################################################---
############## Reading files ----

# These are helper functions used by readHumdrum
# They do the actual work of reading in text from files.
# There is no exported (public-facing) function for users.



readFiles <- function(..., contains = NULL, recursive = FALSE, allowDuplicates = FALSE, validate = TRUE, verbose = FALSE) {
    # readFiles is a major workhorse for readHumdrum and findHumdrum.
    # It uses findFiles (and it's associated regex-based path search patterns) to ID files.
    # It then reads any text files, filters for files that contain matches to regexes in "contains" argument.
    # Finally, it checks for duplicates,
    # Details of how these search patterns work can be found in the findHumdrum user documentation.
    # As a part of this process, the recursive argument is (eventually) passed to the base::list.files function.
    #
    
    cat("Finding and reading files...\n")
    
    ### Filenames
    fileFrame <- findFiles(..., recursive = recursive)
    
    # Duplicate matches?
    matchTable <- fileFrame[ , table(unlist(Filepath), rep(Pattern, lengths(Filepath)))] # this is used later, for verbose printing
    if (!allowDuplicates) fileFrame[ , Filepath := .(remove.duplicates(Filepath))]  
    
    ### Files
    fileFrame[ , Files := .(lapply(Filepath, readTextFiles))]
    
    fileFrame <- as.list(fileFrame) # this stupidy (translatign to list and back) 
    # is because of annoying data.table assignment behavior, which acts differently
    # depending assiging columns of lists if nrow == 1.
    fileFrame$Filepath <- lapply(fileFrame$Files, function(files) if (length(files)) names(files) else files)
    fileFrame <- as.data.table(fileFrame)
    
    if (!is.null(contains)) fileFrame <- filterFilesByContent(fileFrame, contains)
    
    ### print warnings if there are duplicates
    # this is delayed to here, because earlier, we don't know how many files will be removed by contains
    duplicateWarnings(matchTable[rownames(matchTable) %in% unlist(fileFrame$Filepath), , drop = FALSE], 
                      allowDuplicates, verbose)
    
    #### print matches per pattern
    with(fileFrame,
          {
             Label <- IfElse(grepl('^_[1-9]', Label), "", paste0(Label, ' = '))
             
             # prepare string to print if contains
             contains <- if(!is.null(contains)) {
                 glue::glue(" (containing ", 
                            glue::glue_collapse(paste0("'", contains, "'"), sep = ', ', last = ' and '),
                            ")")
                 } else ""
             
             # prepare string to print if verbose
             filepaths <- if (verbose) {
                 sapply(Filepath,
                        function(fps) {
                            paste(paste0('\t\t', fps), collapse = '\n')
                        }) 
             } else ""
             
             cat(glue::glue("\tREpath-pattern {Label}'{Pattern}'",
                            " matches {lengths(Filepath)} text files",
                            contains,
                            " in {lengths(Directories)} {plural(lengths(Directories), 'directories', 'directory')}",
                            if (verbose) ":\n" else ".",
                            "{filepaths}",
                            .trim = FALSE ), sep = '\n')
         })
    
    # Unwrap nested lists
    fileFrame[ , Directories := NULL]
    
    fileFrame <- as.data.table(lapply(fileFrame, 
                                      function(col) {
                                          if (is.list(col)) {
                                              unlist(col) 
                                              } else {
                                              rep(col, lengths(fileFrame$Filepath))
                                              }
                                          }))
    
    # Split lines
    fileFrame[ , FileLines := list(splitFiles2Lines(Files))]
    
    # Number files
    fileFrame[ , File := match(Filepath, unique(Filepath))]
    
    # return
    cat(glue::glue("{num2print(sum(lengths(fileFrame$Filepath)), capitalize = TRUE)} files read from disk."), '\n')
    
    fileFrame
}


readLinesFast <- function(fpath, lines = TRUE) {
    # This function is awesomely fast, but doesn't check if the file are proper text files
    
    s <- file.info(fpath)$size 
      
    buf <- readChar(fpath, s, useBytes = TRUE)
    buf <- stringi::stri_enc_tonative(buf)
    if (lines) stringi::stri_split_fixed(buf, "\n", omit_empty = TRUE)[[1]] else buf
}


readTextFiles <- function(fpaths) {
    # This function reads files, 
    # skipping and leaving out non-text files
    # returns a vector of strings (one per file)
    if (length(fpaths) == 0L) return(character(0))
    
    raw <- lapply(fpaths, stringi::stri_read_raw)
    enc <- data.table::rbindlist(lapply(stringi::stri_enc_detect2(raw), head, n = 1))
    
    text <- !is.na(enc$Encoding) & enc$Confidence > 0.8
    
    files <- unlist(Map(stringi::stri_encode, raw[text], enc$Encoding[text]))
    
    names(files) <- fpaths[text]
    
    files
}

    
splitFiles2Lines <- function(filestrs) {
    lines <- stringi::stri_split_lines(filestrs)
    lapply(lines, head, n = -1L)
}


duplicateWarnings <- function(matchTable, allowDuplicates, verbose) {
    duplicates <- rowSums(matchTable) > 1L
    
    if (!any(duplicates)) return(NULL)
    
    duptable <- matchTable[duplicates, , drop = FALSE]
    
    warning <- c('Regarding findHumdrum/readHumdrum file-search patterns:\n\t\t',
                 glue::glue("{num2print(sum(duplicates), capitalize = TRUE)} ",
                            "{plural(sum(duplicates), 'files match', 'file matches')} ",
                            "match more than one search-pattern. \n"))
    
    warning(warning, call. = FALSE, immediate. = FALSE, noBreaks. = FALSE)
    
    cat(strrep('-', options('width')$width),
        '\nhumdrumR WARNING\n\t',
        warning,
        sep = '')
    
    if (verbose) {
        uniqcombos <- unique.matrix(duptable)
        for (i in 1:nrow(uniqcombos)) {
            curdup <- rownames(duptable)[apply(duptable, 1, function(row) all(row == uniqcombos[i,]))]
            cat('\n',
                glue::glue("The following files match patterns ", 
                                    glue::glue_collapse(colnames(duptable)[uniqcombos[i, ] > 0L],
                                                        sep = ', ', last = ' AND '),
                                    ':\n\t',
                                    glue::glue_collapse(curdup, sep = '\n\t'),
                                    '\n'))
            
        cat('\n')
        }
    }
    
    
    cat('\n\t', sep = '',
        if (allowDuplicates) {
                     "If this is a problem, be more specific in your searches or specify allowDuplicates = FALSE 
                       to force humdrumR to only match each file once.\n"
                 }  else {
                     "Since allowDuplicates = FALSE, these files are only being read once and 
                     associated with the first pattern they match.\n"
                 },
        strrep('-', options('width')$width),
        '\n')
    
    
    
    return(NULL)
}

shortFilenames <- function(fns) {
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




#############################################################################################---
########################### readHumdrum ----
################################################################--

## This is the main event: the function which parses text files (found and read already by findHumdrum and readFiles)
## readHumdrum itself is the only exported (public-facing) function.




#' Find and read humdrum files into R
#' 
#' These functions find valid humdrum files on your local machine and read them into \code{humdrumR}.
#' 
#' \code{findHumdrum} does the work of finding and reading the text files into R.
#' \code{readHumdrum} utilizes \code{findHumdrum} to read files, then parses them to
#' create a \code{\link[humdrumR:humtable]{humdrum table}} and build
#' a \code{\linkS4class{humdrumR}} data object around the table.
#' 
#' 
#' @param ... character: One or more patterns used to identify files to read.
#' For details: see the "REpath-patterns" section below.
#' 
#' @param recursive
#' logical: If \code{TRUE}, the final part of the search pattern (i.e., the file search) is searched for
#' recursively through all sub directories.
#' 
#' @param allowDuplicates \code{logical} or length one, indicating what should happen if multiple search patterns match the same files.
#' If \code{allowDuplicates = TRUE},
#' any such files are read multiple times, grouped into their respective corpora by the \code{Label} field. 
#' If \code{allowDuplicates = FALSE}, any redundant files are only read into the corpus of the first pattern they 
#' match.
#' 
#' @param contains \code{character}. If \code{!is.null(contains)}, the \code{contains} argument is
#' is treated as regular expressions: only files which contain matches to
#'  \emph{all} of these regular expressions are read.
#' Thus, \code{readHumdrum('.*krn$', contains = "EEE")} will only read kern files which contain matches 
#' to \code{"EE"}---which is kern for the E two octaves below middle C (or lower).
#' 
#' 
#' @param verbose
#' logical: If \code{TRUE}, the names of matching files are printed before parsing begins. This is very
#' useful as a check to make sure you aren't reading the wrong files!
#' 
#' @section REpath-patterns:
#' 
#' "REpath-patterns" are specified using \code{...} arguments. 
#' In combination, all the \code{...} arguments are used to search for file paths.
#' Each part of the search path you specify (\code{"dirpart/dirpart/filepart"}, etc) are matched as regular expressions
#' against directories/files on your disc.
#' Thus, we can say things like \code{findHumdrum("../^A.*/.*krn$")}, which would
#' match any kern files in any directory beginning with a capital \code{"A"} in the 
#' directory above the current working directory.
#' For conveniance, you can break the path across multiple arguments instead of using delimited strings: For example, the code
#' \code{findHumdrum("..", "^A.*", ".*krn$")} will give an identical result as the previous example 
#' (\code{findHumdrum("../^A.*/,*krn$")}).
#' This is useful when searching for more than one pattern (see next paragraph) in the same directory.
#' 
#' If you want to search for \emph{more than one} pattern, you can input them as a character vector:
#' For instance, \code{readHumdrum(c("mozart", "beethoven")}---this command will search for
#' filenames containing "mozart" OR "beethoven."
#' This works for directories too: \code{readHumdrum(c("Mozart", "Beethoven"), ".*krn$")} will
#' look for any file kern files in directories containing "Mozart" OR "Beethoven."
#' If patterns are named, these names will show up as identifying patterns in the \code{\linkS4class{humdrumR}} object's
#' \code{Label} field. Unnamed patterns are simply labeled with numbers.
#' 
#' Normal (system appropriate) conventions (i.e., directories separated by \code{"/"}, 
#' \code{'~'} at beginning to indicate home, \code{".."} to indicate directory above working directory, etc.)
#' are followed.
#' If a pattern contains a solo dot followed by a file sep---e.g., \code{"./"}, \code{"x/./y"}---this is 
#' treated as the current directory, not a regular expression.
#' If a pattern contains two dots---e.g., \code{"../"}---this is treated as the directory above, not a regular expression.
#' If you want to create a regular expression to match any directory, use \code{".*/"}.
#' 
#' The regex pattern \code{""} matches any file (it is changed to \code{".*"}). If you don't specifiy any \code{...} argument,
#' \code{findHumdrum} (or \code{readHumdrum}) will default to \code{".*"} as well.
#' Thus, \code{readHumdrum()} will read any humdrum files in the working directory.
#' 
#' (If two or more files in different directories share the same name, a unique name is created for 
#' each file by appending the names of the directories they occupy, recursively
#' until the names are unique.)
#' 
#' If a single humdrum file has multiple pieces in it---meaning that all spine paths close with `*-`, then
#' open again with `**`---then they are parsed separetely.
#' They are distinguished in the `Piece` field.
#' If there are no multi-piece files, `Piece` and `File` will be identical.
#' 
#' @section Validity:
#' 
#' \code{findHumdrum} and \code{readHumdrum} automatically ignore non-text files.
#' Whatsmore, any files which contain humdrum syntax errors (checked by \code{\link{validateHumdrum}}) are automatically
#' skipped. If you want to see specifically what errors occured, call \code{\link{validateHumdrum}} 
#' directly and its \code{errorReport.path} argument.
#' 
#' @section Result:
#' 
#' \code{findHumdrum} returns a "fileFrame" (\code{data.table}), listing all file names,
#' the patterns they match, the directories they were found in, \emph{and} the raw text content of these files.
#' 
#' \code{readHumdrum} returns a fully parsed \code{humdrumR} object.
#' 
#' @examples 
#' readHumdrum() # loads all valid humdrum files in the current directory.
#' 
#' readHumdrum(".*krn$") # loads all files ending with "krn" in the currect directory
#' 
#' readHumdrum("^Composers$/^Be|^Mo/.*/^Joined$/.*krn$") 
#' # Goes inside the directory "Composers".
#' # Inside "Composers" looks for directories that start with "Be" or "Mo".
#' # If there are any "Be|Mo" matching directories within "Composers", matches all directories within them.
#' # Within these directories, looks for directories called "Joined".
#' # If there are any directories called "Joined", loads all files (if any) that end with "krn".
#' 
#' readHumdrum("^Composers$", "^Be|^Mo", ".*", "^Joined$", ".*krn$")
#' # exactly the same as the previous!
#' 
#' readHumdrum("^Composers$", c(Beethoven = "^Be", Mozart = "^Mo"), ".*", "^Joined$", ".*krn$") 
#' # exactly the same as the previous, except now the two matching patterns ("^Be", or "^Mo") will be grouped
#' in the Label field as "Beethoven" and "Mozart" respectively.
#' 
#' @name readHumdrum
#' @export
readHumdrum = function(..., recursive = FALSE, contains = NULL, allowDuplicates = FALSE, verbose = FALSE, parseTandem =TRUE, parseGlobal = TRUE) {
    
    fileFrame <- findHumdrum(..., contains = contains, recursive = recursive, 
                             allowDuplicates = allowDuplicates, verbose = verbose)
    
    #
    if (nrow(fileFrame) == 0L) return(NULL)
    
    #
    cat(glue::glue("Parsing {num2print(nrow(fileFrame))} files..."))
    
    if (verbose) cat ('\n')
    
    
    ## Divide out any pieces within files
    fileFrame <- separatePieces(fileFrame)
    
    ## Parse records
    humtabs <- Map(
        function(file, piece) { 
            humtab <- parseRecords(file, piece, parseGlobal)
            if (verbose) cat(filename, '\n') 
            humtab
        }, 
        fileFrame$FileLines, fileFrame$Piece) 
    
    ##########-
    ### Assembling local record information with corpus metadata (filenames, reference records, etc.)
    ##########-
    cat("Assembling corpus...")
    
    ###### consolidate files into one data.table
    humtab  <- data.table::rbindlist(humtabs, fill = TRUE) # fill = TRUE because some pieces have different reference records
   
    # file/piece info
    fileFrame[ , Filename := shortFilenames(Filepath)]
    fileFrame[ , FileLines := NULL]
    
    # combine with parsed data
    humtab  <- humtab[fileFrame,  on = "Piece"]
    
    
    ## Other general information about tokens
    humtab[ , Type := parseTokenType(Token)]
    humtab[ , Null := Token %in% c('.', '!', '*', '=', '_P')]
    humtab[ , Global := is.na(Spine)]
    
    #
    if (parseTandem) {
        tandemTab <- tandemTable(humtab$Tandem)
        if (!is.null(tandemTab)) humtab <- cbind(humtab, tandemTab)
    }
    cat('Done!\n')
    
    makeHumdrumR(humtab, unique(fileFrame$Pattern))
    
    
}




parseRecords <- function(records, piece, parseGlobal = TRUE) {
          # This function is the biggest part of readHumdrum
          # It takes a character vector representing the records 
          # in a single humdrum file, and outputs a data.table (an incomplete humdrum table).
  
  global <- if (parseGlobal) parseGlobal(records) else NA
  local  <- parseLocal(records)
  
  humtab <- if (length(global) == 1 && is.na(global)) local else rbind(global$Data, local, fill = TRUE)
  
  # Data record numbers
  humtab$NData <- rep(NA, nrow(humtab))
  D <- !grepl('^[!=*]', humtab$Token)
  humtab$NData[D] <- match(humtab$Record[D],  sort(unique(humtab$Record[D])))
  
  #
  humtab <- if (!(length(global) == 1 && is.na(global))) cbind(humtab, global$Table) else humtab
  

  humtab[ , Piece := piece]
  humtab
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
  data.table::as.data.table(lapply(refVals, function(ref) if (length(ref) == 1) ref else list(ref)))
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
    local  <- padSpinePaths(local)
  }

  ###mat is character matrix
  ###(row = record, col = spine/subspine)
  mat <- stringi::stri_list2matrix(local, byrow = TRUE, fill = '_P')
 
  #flatten spines and get recordns for each.
  rownames(mat) <- recordn
  tokens   <- setNames(as.vector(mat), rep(recordn, ncol(mat)))
  
  ## get spine and path numbers of appropriate length
  SpineNumbers <- cumsum(apply(mat, 2,  function(spine) any(spine != '_P') && grepl('^\\*\\*', spine[spine != '_P'][1])))
  SpinePaths   <- unlist(use.names = FALSE, tapply(SpineNumbers, SpineNumbers, seq_along, simplify = TRUE)) - 1L
  Columns      <- seq_along(SpineNumbers)
  
  #sections
  sections <- parseSections(mat[ , 1])
  barlines <- parseBarlines(mat[ , 1])

  #interpretations
  exclusivestandems <- parseInterpretations(mat)
  exclusives <- exclusivestandems$Exclusive
  tandems    <- unlist(exclusivestandems$Tandem)
  
  ##################### tokens
  tokens <- parseMultiStops(tokens)
  #If there are multistops, the spines are no longer the same lengths, and have different recordns.
  stopNs <- as.integer(gsub('^.*\\.', '', names(tokens)))
  # get rid of stuff before period, leaving only stop number
  recordns <- gsub('\\..*$', '', names(tokens))
  # get rid of stuff after period, leaving just record number (don't make numeric yet)

  # expand objects to match recordn, which may have changed when multistops were introduced
  # recordns are still characters. This is necessarry, because we use them as names to index other objects.
  sections <- sections[recordns, , drop = FALSE] # these two should always be the same in each spine,
  barlines <- barlines[recordns, , drop = FALSE] # so I'm really just copying the first spine...maybe change this to work like tandems (below)?
 
  
  spineLengths <- nrow(mat) + apply(mat[!grepl('^[*!=]', mat[ , 1]), , drop = FALSE], 2, function(col) sum(stringi::stri_count_fixed(col, ' ')))
  Columns      <- rep(Columns, spineLengths)
  
  tandems  <- tandems[paste0(SpineNumbers[Columns], '_', recordns)] # These are different in each spine
  
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

  if (ncol(sections) > 0L) humtab <- cbind(humtab, sections)
  # humtab <- humtab[Token != '_P'] # "_P" tokens were inserted as padding by parseSpinePaths

  humtab

}


separatePieces <- function(fileFrame) {
    # Takes a file frame.
    # It looks at each file and breaks it into subfiles if necessarry
    # it returns a new file frame, expanded to include newly split files 
    # and with new "Piece" column
    
    filelines <- fileFrame$FileLines
    
    containmultiple <- sapply(filelines, 
                              function(lines) {
                                  sum(stringi::stri_detect_regex(lines, '^\\*\\*')) > 1L
                              })
    
    filelines[containmultiple] <-
        lapply(filelines[containmultiple], 
           function(lines) {
               open  <- stringi::stri_count_regex(lines,'\\*\\*')
               close <- stringi::stri_count_regex(lines,'\\*-')
               nspines <- open - close
               
               newpiece <- head(nspines, -1) > 0L & tail(nspines, -1) == 0L
               if (nspines[1] == 0L) {
                   newpiece[which(newpiece)[1]] <- FALSE
                   newpiece[1] <- TRUE
               }
               pieces <- cumsum(c(newpiece, FALSE))
               split(lines, f = pieces) 
           })
    
    filelines[!containmultiple] <- lapply(filelines[!containmultiple], list)
    
    ## spread out 
    newFrame <- data.table(FileLines = unlist(filelines, recursive = FALSE),
                           Filepath  = rep(fileFrame$Filepath, lengths(filelines)))
    newFrame[ , Piece := 1:nrow(newFrame)]
    
    fileFrame <- fileFrame[ , c('Filepath', 'File', 'Label')]
    
    newFrame[fileFrame, on = 'Filepath']
    
}
# 
# parseTokenType <- function(spine) {
#   # This is called by parseRecords
#   # simply categories records by spine type,
#   # to create the humdrum tables Type field.
#   type <- rep('D', length(spine))
#   type[spine == '_P'] <- 'P'
#   type[grepl('^!'  , spine)]    <- 'L'
#   type[grepl('^!!' , spine)]    <- 'G'
#   type[grepl('^\\*', spine)]    <- 'I'
#   type[grepl('^=', spine)]      <- 'M'
#   type[spine == '.']            <- 'd'
#   type
# }

parseTokenType <- function(spine) {
    # This is called by parseRecords
    # simply categories records by spine type,
    # to create the humdrum tables Type field.
    
    out <- rep('D', length(spine))
    
    out[spine == '_P'] <- 'P'
    out[spine == '.'] <- 'd'
    out[stringi::stri_detect_regex(spine, '^\\*')] <- 'I'
    out[stringi::stri_detect_regex(spine, '^=')] <- 'M'
    out[stringi::stri_detect_regex(spine, '^!')] <- 'L'
    out[stringi::stri_detect_regex(spine, '^!!')] <- 'G'
    out
}



padSpinePaths <- function(local) {
  # Used by parseLocal to make sense of spine paths.
  # This function takes a list of character vectors (each representing a list of tokens from one record)
  # identifies spine paths in them, and pads the records with "_P" such that 
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
                                    values = lapply(pad[pad > 0], function(n) rep('_P', n)), 
                                    after = j)
           }
           
           close <- sapply(local, function(row) length(row) > j && all(row[j] == '*-'))
           if (any(head(close, -1))) {
             pad <- cumsum(c(FALSE, head(close, -1)))
             local[pad > 0] <<- Map(append, x = local[pad > 0], values = '_P', after = j - 1)
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
  spinemat[is.na(spinemat)] <- '_P' # Not sure why this is necassary (Nat, December 2018)
  
  lapply(1:ncol(spinemat), 
         function(j) { 
           if (j > 1 && any(spinemat[ , j] == '_P')) {
             spinemat[spinemat[ , j] == '_P', j] <<- spinemat[spinemat[ , j] == '_P', j - 1] 
           }
           spine <- spinemat[ , j]
           
           interpind <- rev(grep('^\\*[^*->^v]', spine))
           interpind <- interpind[interpind > tail(stringi::stri_startswith_fixed(spine, '**'), 1)]
           interps <- spine[interpind]
           
           setNames(sapply(seq_along(spine), function(i) paste(interps[i >= interpind], collapse = ',')), 
                    paste0(j, '_', rownames(spinemat)))
         } 
  ) -> tandemIs
  
  exclusiveI <- apply(spinemat, 2, function(spine) tail(spine[stringi::stri_startswith_fixed(spine, '**')], 1))
  
  
  
  list(Exclusive = exclusiveI, 
       Tandems   = tandemIs)
}

tandemTable <- function(tandems) {
          # This function takes the parsed, cummulative
          # Tandem fields produced by parseInterpretations
          # and, using functions from the file humInterpretations.R
          # identifies any "known" (i.e., standard) interpretations
          # and parses these into a table of new Interpretation fields
          # for the humdrum table.
          if (all(tandems == "")) return(NULL)
          
          uniqueTandem <- unique(unlist(stringr::str_split(unique(tandems), ',')))
          uniqueTandem <- uniqueTandem[!is.na(uniqueTandem) & uniqueTandem != '']
          
          areKnownTandems <- isKnownTandem(uniqueTandem)
          knownTandems   <- uniqueTandem[areKnownTandems]
          # unknownTandems <- uniqueTandem[!areKnownTandems]
          
          tandemPatterns <- unique(generalizeTandem(knownTandems))
          tandemMat <- lapply(tandemPatterns,
                              function(tan) stringr::str_match(tandems, tan)[ ,1])
          
          # for (t in knownTandems) tandems <- stringi::stri_replace_all_fixed(tandems, pattern = t, '')
          # tandems <- stringi::stri_replace_all(tandems, regex = ',,*', ',')
          # tandems[tandems %in% c(',', '')] <- NA_character_
          
          names(tandemMat) <- unique(idTandem(knownTandems))
          
          # tandemMat$Tandem <- tandems
          
          as.data.table(tandemMat)
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
  
  nesting <- max(stringi::stri_count_fixed(stringr::str_sub(grep('^\\*>', spine, value = TRUE), 3L), '>'))
  
  matrices <- lapply(sectionTypes, 
                     function(type) {
                       hits  <- grepl(type, spine)
                       if (!any(hits)) return(NULL)
                       
                       depth <- rep(NA_integer_, length(spine))
                       depth[hits] <- stringi::stri_count_fixed(stringr::str_sub(spine[hits], 3L), '>')
                       
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
  BarLabels <- stringr::str_sub(grep('^=', spine, value = TRUE), 2L)[IfElse(Singles == 0L, NA_integer_, Singles)]
  
  data.frame(BarN = Singles, 
             DoubleBarN = Doubles, 
             BarLabel = BarLabels, 
             row.names = names(spine), stringsAsFactors = FALSE)
}




