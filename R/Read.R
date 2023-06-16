
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
    
    fileFrame[ , Filepath := list(Map(`[`, Filepath, matches))]
    fileFrame[ , Files    := list(Map(`[`, Files   , matches))]
    
    
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
    
    message("Finding and reading files...")
    
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
    fileFrame$Filepath <- lapply(fileFrame$Files, \(files) if (length(files)) names(files) else files)
    fileFrame <- as.data.table(fileFrame)
    
    if (!is.null(contains)) fileFrame <- filterFilesByContent(fileFrame, contains)
    
    ### print warnings if there are duplicates
    # this is delayed to here, because earlier, we don't know how many files will be removed by contains
    duplicateWarnings(matchTable[rownames(matchTable) %in% unlist(fileFrame$Filepath), , drop = FALSE], 
                      allowDuplicates, verbose)
    
    #### print matches per pattern
    with(fileFrame,
          {
             Label <- ifelse(grepl('^_[1-9]', Label), "", paste0(Label, ' = '))
             
             # prepare string to print if contains
             contains <- if(!is.null(contains)) {
                 glue::glue(" (containing ", 
                            harvard(contains, 'and', quote = TRUE), 
                            ")")
                 } else ""
             
             # prepare string to print if verbose
             filepaths <- if (verbose) {
                 sapply(Filepath,
                        function(fps) {
                            paste(paste0('\t\t', fps), collapse = '\n')
                        }) 
             } else ""
             
             message(glue::glue("\tREpath-pattern {Label}'{Pattern}'",
                            " matches {lengths(Filepath)} text files",
                            contains,
                            " in {lengths(Directories)} {plural(lengths(Directories), 'directories', 'directory')}",
                            if (verbose) ":\n" else ".\n",
                            "{filepaths}",
                            .trim = FALSE ))
         })
    
    # Unwrap nested lists
    fileFrame[ , Directories := NULL]
    
    fileFrame <- as.data.table(lapply(fileFrame, 
                                      \(col) {
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
    message(glue::glue("{num2print(sum(lengths(fileFrame$Filepath)), capitalize = TRUE, label = 'file')} read from disk."))
    
    fileFrame
}


# readLinesFast <- function(fpath, lines = TRUE) {
#     # This function is awesomely fast, but doesn't check if the file are proper text files
#     
#     s <- file.info(fpath)$size 
#       
#     buf <- readChar(fpath, s, useBytes = TRUE)
#     buf <- stringi::stri_enc_tonative(buf)
#     if (lines) stringi::stri_split_fixed(buf, "\n", omit_empty = TRUE)[[1]] else buf
# }
# 

readTextFiles <- function(fpaths) {
    # This function reads files, 
    # skipping and leaving out non-text files
    # returns a vector of strings (one per file)
    if (length(fpaths) == 0L) return(character(0))
    
    raw <- lapply(fpaths, stringi::stri_read_raw)
    enc <- data.table::rbindlist(lapply(stringi::stri_enc_detect2(raw), head, n = 1))
    
    text <- !is.na(enc$Encoding) & enc$Confidence >= 0.75
    
    files <- unlist(Map(stringi::stri_encode, raw[text], enc$Encoding[text]))
    
    names(files) <- fpaths[text]
    
    files
}

    
splitFiles2Lines <- function(filestrs) {
    lines <- stringi::stri_split_lines(filestrs)
    lapply(lines, \(ls) if (ls[length(ls)] == "") ls[-length(ls)] else ls)
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
    
    message(strrep('-', options('width')$width),
        '\nhumdrumR WARNING\n\t',
        warning,
        sep = '')
    
    if (verbose) {
        uniqcombos <- unique.matrix(duptable)
        for (i in 1:nrow(uniqcombos)) {
            curdup <- rownames(duptable)[apply(duptable, 1, \(row) all(row == uniqcombos[i,]))]
            message('\n',
                glue::glue("The following files match patterns ", 
                                    paste0('\t', glue::glue_collapse(colnames(duptable)[uniqcombos[i, ] > 0L],
                                                        sep = ', ', last = ' AND ')),
                                    '\t:\n\t',
                                    paste0('\t', glue::glue_collapse(curdup)),
                                    '\n'))
            
        # cat('\n')
        }
    }
    
    
    message('\n\t', appendLF = FALSE,
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

shortFilenames <- function(fileFrame) {
    # This function takes a list of full directory paths
    # and finds the shortest unique version of each one.
    # In most cases, the final final name is returned,
    # but if two (or more directores) contain files 
    # with the same names, their directory names are retained
    # to distinguish them
  
    runs <- rle(fileFrame$Filepath)
    
    shorten <- function(fns) {
      stepin <- stringr::str_replace(fns, '[^/]*/', '')
      
      duples <- duplicated(stepin) | rev(duplicated(rev(stepin))) | 
        stepin == fns | stepin == '' | is.na(stepin)
      ifelse(duples, fns, Recall(stepin))
    }
    
    runs$values <- shorten(runs$values)
    
    fileFrame[ , Filename := inverse.rle(runs)]
    fileFrame
}




#############################################################################################---
########################### readHumdrum ----
################################################################--

## This is the main event: the function which parses text files (found and read already by findHumdrum and readFiles)
## readHumdrum itself is the only exported (public-facing) function.




#' Find and read humdrum files into R
#' 
#' These functions find valid humdrum files on your local machine and read them into `humdrumR`.
#' 
#' `findHumdrum` does the work of finding and reading the text files into `R`.
#' `readHumdrum` utilizes `findHumdrum` to read files, then parses them to
#' create a [humdrum table][humTable] and build
#' a [humdrumR data object][humdrumRclass] around the table.
#' 
#' 
#' @param ... ***One or more patterns used to identify files to read.***
#' 
#' Must be `character` strings.
#'
#' For details: see the "REpath-patterns" section below.
#' 
#' @param recursive ***Should files be found recursively through sub directories?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the final part of the search pattern (i.e., the file search) is searched for recursively through all sub directories.
#' 
#' @param allowDuplicates ***Indicating what should happen if multiple search patterns match the same files.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `allowDuplicates = TRUE`,
#' any such files are read multiple times, grouped into their respective corpora by the `Label` field. 
#' If `allowDuplicates = FALSE`, any redundant files are only read into the corpus of the first pattern they 
#' match.
#' 
#' @param contains ***REGEX for filtering files.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be `character`. 
#' 
#' If `!is.null(contains)`, the `contains` argument is
#' is treated as regular expressions: only files which contain matches to
#'  *all* of these regular expressions are read.
#' Thus, `readHumdrum('.*krn$', contains = "EEE")` will only read kern files which contain matches 
#' to `"EE"`---which is kern for the E two octaves below middle C (or lower).
#' 
#' 
#' @param verbose ***Whether to print filename while reading or not.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the names of matching files are printed before parsing begins. This is very
#' useful as a check to make sure you aren't reading the wrong files!
#' 
#' @param tandems ***Controls which, if any, tandem interpretations are parsed into their own fields.***
#' 
#' Defaults to `"known"`. 
#' 
#' Must be `character`.
#' 
#' @param reference ***Which reference records should be parsed into fields.***  
#' 
#' Defaults to `"all"`. 
#' 
#' Must be `character`.
#' 
#' @section REpath-patterns:
#' 
#' "REpath-patterns" are specified using `...` arguments. 
#' In combination, all the `...` arguments are used to search for file paths.
#' Each part of the search path you specify (`"dirpart/dirpart/filepart"`, etc) are matched as regular expressions
#' against directories/files on your disc.
#' Thus, we can say things like `findHumdrum("../^A.*/.*krn$")`, which would
#' match any kern files in any directory beginning with a capital `"A"` in the 
#' directory above the current working directory.
#' For conveniance, you can break the path across multiple arguments instead of using delimited strings: For example, the code
#' `findHumdrum("..", "^A.*", ".*krn$")` will give an identical result as the previous example 
#' (`findHumdrum("../^A.*/,*krn$")`).
#' This is useful when searching for more than one pattern (see next paragraph) in the same directory.
#' 
#' If you want to search for *more than one* pattern, you can input them as a character vector:
#' For instance, `readHumdrum(c("mozart", "beethoven")`---this command will search for
#' filenames containing "mozart" OR "beethoven."
#' This works for directories too: `readHumdrum(c("Mozart", "Beethoven"), ".*krn$")` will
#' look for any kern files in directories containing "Mozart" OR "Beethoven."
#' If patterns are named, these names will show up as identifying patterns in the `[humdrumR][humdrumR]` object's
#' `Label` field. Unnamed patterns are simply labeled with numbers.
#' We refer to files matched from regex patterns to be "subcorpora" of the total corpus.
#' 
#' Normal (system appropriate) conventions (i.e., directories separated by `"/"`, 
#' `'~'` at beginning to indicate home, `".."` to indicate directory above working directory, etc.)
#' are followed.
#' If a pattern contains a solo dot followed by a file sep---e.g., `"./"`, `"x/./y"`---this is 
#' treated as the current directory, not a regular expression.
#' If a pattern contains two dots---e.g., `"../"`---this is treated as the directory above, not a regular expression.
#' If you want to create a regular expression to match any directory, use `".*/"`.
#' 
#' The regex pattern `""` matches any file (it is changed to `".*"`). If you don't specifiy any `...` argument,
#' `findHumdrum` (or `readHumdrum`) will default to `".*"` as well.
#' Thus, `readHumdrum()` will read any humdrum files in the working directory.
#' 
#' (If two or more files in different directories share the same name, a unique name is created for 
#' each file by appending the names of the directories they occupy, recursively
#' until the names are unique.)
#' 
#' If a single humdrum file has multiple pieces in it---meaning that all spine paths close with `*-`, then
#' open again with `**`---then they are parsed separately.
#' They are distinguished in the `Piece` field.
#' If there are no multi-piece files, `Piece` and `File` will be identical.
#' 
#' @section Validity:
#' 
#' `findHumdrum` and `readHumdrum` automatically ignore non-text files.
#' What's more, any files which contain humdrum syntax errors (checked by [validateHumdrum()]) are automatically
#' skipped. If you want to see specifically what errors occurred, call [validateHumdrum()]
#' directly and use its `errorReport.path` argument.
#' 
#' @section Tandem Interpretations:
#' 
#' All tandem interpretations in a humdrum dataset are summarized in the [humdrum table's][humTable]
#' `Tandem` field, which is described in detail [here][extractTandem()].
#' In addition, certain "known" tandem interpretations are parsed into their *own* fields automatically.
#' For example, `*clefG4` and "`*clefF2` are parsed as `Clef` data, while `*k[b-]` is parsed as a `KeySignature`.
#' The "known" tandem interpretations that `humdrumR` recognizes are encoded in a built-in
#' table called `knownInterpretations`. 
#' Each interpretation has a humdrumR name (`"Clef"`, `"TimeSignature"`, etc.) as well as a regular expression
#' associated with it.
#' 
#' The `tandems` argument to `readHumdrum` controls which tandem interpretations are
#' parsed into their own fields. This can be helpful to either save processing time and memory
#' by *not* parsing interpretations you won't need, or to parse interpretations that 
#' humdrumR doesn't recognize.
#' The default value for the `tandems` argument is `"known"`. If the `tandems` argument
#' contains `"known"` *all* tandem interpretations in the built-in `knownInterpretations` 
#' table are parsed.
#' Users may specify different interpretations to parse in two ways: 
#' 
#' 1) character strings 
#'    matching one of the name values from the `Name` column of `knownInterpretations`.
#'    For instance, if you specify `tandems = c('Clef', 'TimeSignature')`, only clef (e.g., `"*clefG2"`),
#'    and time signature (e.g., `"*M3/4"`) intepretations will be parsed.
#' 2) if the character string(s) in `tandem` do not exactly match one of the names in 
#'    `knownInterpretations$Name`, they are treated as regular expressions and used to match
#'    tandem interpretations in the data. This allows users to parse non-standard tandem interpretations
#'    that humdrumR doesn't already know about.
#' 
#' If any values in `tandems` are named, these names will be used for resulting fields.
#' If no matches to an given interpretation are found, no field is created for that interpretation.
#' If `tandems = NULL`, then no tandem interpretations are parsed.
#' 
#' @section Reference Records:
#' 
#' By default (`reference = "all"`), humdrumR reads all reference records in the data.
#' The reference code for each record (e.g, the "OTL", in "!!!OTL: xxx") is used as the name of 
#' an associated field.
#' (If a reference record has no reference code (i.e., it lacks a colon), the field is called "Unkeyed.")
#' In large datasets with many reference records, the reference data can actually make up a large portion 
#' of the humdrum table, and eat up a lot of memory. In these cases, we might not want to read
#' all (or any) reference records---we can instead read only the reference records that we are planning to use 
#' in our analyses (if any).
#' If `reference = NULL`, no reference records are parsed.
#' Otherwise, the character values of `reference` are treated as reference codes and only
#' matching reference records are parsed.
#' For instance, `readHumdrum(_, reference = "OTL")` will *only* parse OTL reference records.
#' If the values of `reference` are named, these names are used to name associated fields.
#' Thus, by specifing `reference = c(Title = 'OTL')`, you can use "OTL" reference records to populate
#' a field called "Title".
#' 
#' If there are more than one reference records with the same reference code,
#' either explicitely numbered (e.g., "!!!COM1:", "!!!COM2:") all are read and rather than making two 
#' or more fields, a single field is created ("COM" in this) with the multiple values separated by ";".
#' 
#' If your humdrum data includes files containing multiple pieces, special consideration is 
#' needed to determine (or guess) which reference records (or other global comments) "go with" which piece.
#' Obviously, reference records at the beginning and end of each file are grouped with the first
#' and last pieces respectively.
#' However, reference records that are between pieces in any multi-piece file require some guess work.
#' `readHumdrum()` will look at reference codes and attempt to group in-between reference records
#' into pieces in a logical way by avoiding duplicated reference codes.
#' 
#' 
#' @section Spines and Paths:
#' 
#' 
#' In the [humdrum syntax](http://www.humdrum.org/guide/ch05/), data is placed in "spines,"
#' which are not the same as "columns" in a spreadsheet. A "column" refers to a 
#' tab-delineated group of values.
#' "Spines" can be a single column, or they may (at any time) split into multiple columns,
#' which can in turn split again, using the `"*^"` interpretation token. The reverse can happen as well,
#' with two or more columns merging into a single column, using the `"v"` token.
#' This means that, while humdrum data at first glance looks like a simple two-dimensional table,
#' it is actually a flexible tree structure. As spines split and merge, the total number of columns
#' can change during a piece, creating a "ragged" edge.
#' Another similar issue is that a corpus of humdrum files may have varying numbers of spines/columns, between pieces.
#' ("Global" comment/reference records are also a special case, as that are always a single value, even if interspersed with
#' multi-column local records.)
# 
#' `readHumdrum` assumes a slightly more strict version of the humdrum syntax:
#' that all the spines which appear at the beginning of a file (headed with exclusive interpretations
#' like `"**kern"`) can never merge into each other. Thus, a humdrum file read into `humdrumR`
#' must not end with fewer columns than it starts.
#' Spine merges (`"*v"`) can only happen within spine paths that originally split off the same spine.
#' This extra-strict specification of spine paths in the humdrum syntax is, fortunately, something that has been
#' informally followed in most humdrum datasets.
#' 
#' Our strict spine-path definition makes everything work fairly simply: 
#' Within a piece, the spines which appear at the beginning of the piece are the "true" spines throughout the piece, numbered
#' from left to right, starting from `1L`.
#' For each local token, the value in the `Spine` field is an integer indicating which of these
#' "true" spines it belongs to---global tokens have a `NA` value in their `Spine` field, because they do not belong to any spine.
#' Any spine path splits (`"*^"`) from the main spines form **spine paths**.
#' Every spine's paths are numbered in the `Path` field, from right to left, starting from `0L`.
#' A spine with no splits will have all `0L`s in its `Path` field.
#' 
#' 
#' @section Result:
#' 
#' `findHumdrum` returns a "fileFrame" (`data.table`), listing all file names,
#' the patterns they match, the directories they were found in, *and* the raw text content of these files.
#' 
#' `readHumdrum` returns a fully parsed [humdrumR object][humdrumRclass].
#' 
#' @examples 
#' 
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
#' # in the Label field as "Beethoven" and "Mozart" respectively.
#' 
#' @name readHumdrum
#' @export
readHumdrum <- function(..., recursive = FALSE, contains = NULL, allowDuplicates = FALSE, verbose = FALSE, 
                       tandems = 'known', reference = 'all') {
    
    fileFrame <- findHumdrum(..., contains = contains, recursive = recursive, 
                             allowDuplicates = allowDuplicates, verbose = verbose)
    
    #
    if (nrow(fileFrame) == 0L) return(NULL)
    
    #
    message(glue::glue("Parsing {num2print(nrow(fileFrame), label = 'file')}..."), appendLF = FALSE)
    if (verbose) message()
    
    
    ## Divide out any pieces within files
    fileFrame <- separatePieces(fileFrame)
    
    ## Parse records
    humtabs <- Map(
        function(file, filename, piece) { 
            humtab <- parseRecords(file, piece, reference)
            if (verbose) message(strrep(' ', 17L), '...', filename) 
            humtab
        }, 
        fileFrame$FileLines, fileFrame$Filepath, fileFrame$Piece) 
    
    ##########-
    ### Assembling local record information with corpus metadata (filenames, reference records, etc.)
    ##########-
    message("Assembling corpus...", appendLF = FALSE)
    
    ###### consolidate files into one data.table
    humtab  <- data.table::rbindlist(humtabs, fill = TRUE) # fill = TRUE because some pieces have different reference records
   
    # file/piece info
    fileFrame <- shortFilenames(fileFrame)
    fileFrame[ , FileLines := NULL]
    
    # combine with parsed data
    humtab  <- humtab[fileFrame,  on = "Piece"]
    
    
    ## Other general information about tokens
    humtab[ , Type := parseTokenType(Token)]
    humtab <- humtab[Type != 'P']
    humtab[ , Global := is.na(Spine)]
    humtab[ , Pattern := NULL]
    
    #
    tandemTab <- parseTandem(humtab$Tandem, tandems)
    if (!is.null(tandemTab)) humtab <- cbind(humtab, tandemTab)
        
    #
    message('Done!\n')
    
    makeHumdrumR(humtab, 
                 pattern = setNames(unique(fileFrame$Pattern), unique(fileFrame$Label)), 
                 tandemFields = colnames(tandemTab))
    
    
}




parseRecords <- function(records, piece, reference) {
          # This function is the biggest part of readHumdrum
          # It takes a character vector representing the records 
          # in a single humdrum file, and outputs a data.table (an incomplete humdrum table).
  
  global <- parseGlobal(records, reference) 
  local  <- parseLocal(records)
  
  humtab <- if (length(global) == 1 && is.na(global)) local else rbind(global$Data, local, fill = TRUE)
  
  # Data record numbers
  humtab$DataRecord <- rep(NA, nrow(humtab))
  D <- !grepl('^[!=*]', humtab$Token)
  humtab$DataRecord[D] <- match(humtab$Record[D],  sort(unique(humtab$Record[D])))
  
  #
  humtab <- if (!(length(global) == 1 && is.na(global))) cbind(humtab, global$Table) else humtab
  

  humtab[ , Piece := piece]
  humtab
}

parseGlobal <- function(records, reference) {
          # This function works for parseRecords
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
       Table = parseReference(globalrecords[refind], reference))
}

parseReference <- function(refTable, reference) {
          # This parses a character vector of reference records into
          # a data.table with columns indicating reference keys
          # and rows holdingtheir values
  if (is.null(reference)) return(NULL)    
    
  refrecords <- stringr::str_sub(refTable, start = 4L) # rid of !!!
  
  refTable <- as.data.table(stringi::stri_split_regex(refrecords, ': *', n = 2, simplify = TRUE))
  colnames(refTable) <- c('refKeys', 'refVals')
  
  # if there is no colon, there is no key so need to shift the first column over and insert "Unkeyed"
  refTable <- refTable[refKeys != '' | refVals != '']
  refTable[refKeys == '', refKeys := 'Unkeyed']
  
  #multiple keys
  refTable[ , refKeys := stringi::stri_replace_all_regex(refKeys, '[0-9]+$', '')]
  if (any(duplicated(refTable$refKeys)))  {
      refTable <- refTable[ , .(refVals =  paste(refVals, collapse = '; ')), by = refKeys]
      }

  # filter by reference argument
  if (!(length(reference) == 1L && reference == 'all')) {
      refTable <- refTable[refKeys %in% reference]
      names <- .names(reference)
      names[names == ""] <- reference[names == ""]
      refTable[ , refKeys := names[match(refKeys, reference)]]
  }
  
  if (nrow(refTable) == 0L) {
      NULL
  } else {
      as.data.table(as.list(setNames(refTable$refVals, refTable$refKeys)))
  }
  
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
          grepl('*-', head(localrecords, -1), fixed = TRUE))) { 
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
  
  ParentPaths  <- integer(length(SpineNumbers)) 
  if (any(SpinePaths) > 0L) {
    ind <- which(mat != '_P', arr.ind = TRUE)
    ind <- ind[!duplicated(ind[ , 2]), ]
    
    # targets <- unique(which(mat[ind[SpinePaths > 0L, 'row'] - 1L, , drop = FALSE] == '*^', arr.ind = TRUE))[ , 'col']
    splitrows <- mat[ind[SpinePaths > 0L, 'row'] - 1L, , drop = FALSE]
    splitrows <- which(splitrows == '*^' | splitrows == '*+', arr.ind = TRUE)
    targets <- tapply(splitrows[ , 'col'], splitrows[ , 'row'], max)
    
    ParentPaths[SpinePaths > 0L] <- SpinePaths[targets]
  }
  
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
 
  
  spineLengths <- nrow(mat) + apply(mat[!grepl('^[*!=]', mat[ , 1]), , drop = FALSE], 2, \(col) sum(stringi::stri_count_fixed(col, ' ')))
  Columns      <- rep(Columns, spineLengths)
  
  tandems  <- tandems[paste0(SpineNumbers[Columns], '_', recordns)] # These are different in each spine
  
  # Don't need recordns to be characters anymore.
  recordns  <- as.integer(recordns)
  humtab <- data.table::data.table(Token = tokens,
                                   Spine = SpineNumbers[Columns],
                                   Path  = SpinePaths[Columns],
                                   ParentPath = ParentPaths[Columns],
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
                              \(lines) {
                                  sum(stringi::stri_detect_regex(lines, '^\\*\\*')) > 1L
                              })
    
    filelines[containmultiple] <-
        lapply(filelines[containmultiple], 
           \(lines) {
               starts <- which(stringi::stri_detect_regex(lines, '^\\*\\*'))
               
               # find global comments which are "OUTSIDE" of pieces.
               nopen  <- stringi::stri_count_regex(lines,'^\\*\\*|\t\\*\\*')
               nclose <- stringi::stri_count_regex(lines,'^\\*-|\t\\*-')
               nspines <- cumsum(nopen - c(0L, head(nclose, -1L)))
               global <- which(nspines == 0L) 
               # we can't just grep for ^!! because that would also capture global comments
               # which are mid-piece
               
               #
               codes <- stringi::stri_extract_first_regex(lines[global], '!!![^:]+:')
               groups <-  findInterval(global, starts)
               
               
               # At blocks of global records between pieces and see if we can improve alignment
               # by shifting some reference records between adjacent blocks.
               blocks <- table(groups = factor(groups, levels = 0L:length(starts)), codes)
               
               blocks[2L, ] <- blocks[2L, ] + blocks[1L, ]
               blocks[length(starts), ] <- blocks[length(starts), ] + blocks[nrow(blocks), ]
               blocks <- head(tail(blocks, -1L), -1L)
               
               shift <- which(blocks > 1L & rbind(blocks[-1 , , drop = FALSE], FALSE) == 0L, arr.ind = TRUE)
               if (nrow(shift)) {
                 shift <- Map(shift[ , 'groups'], shift[ , 'codes'],
                              f = \(group, code) groups == rownames(blocks)[group] & codes == colnames(blocks)[code])
                 shift <- Reduce('|', shift)
                 shift <- tapply_inplace(shift, 
                                         groups, 
                                         \(s) {
                                           not <- which(!s)
                                           if (any(not)) seq_along(s) > max(which(!s)) else !logical(length(s))
                                           }) # makes sure only block of TRUE on right edge are shifted
                 groups <- ifelse(shift, groups + 1L, groups)
                 
               }
               
               # get records
               groups[groups == 0L] <- 1L
               records <- tapply(global[groups <= length(starts)], 
                                 factor(groups[groups <= length(starts)], levels = 1L:(length(starts))),
                                 min)
               
               starts <- pmin(records,starts, na.rm = TRUE)
               
               # divide into pieces
               pieces <- cumsum(seq_along(lines) %in% starts)
               
               split(lines, f = pieces) 
           })
    
    filelines[!containmultiple] <- lapply(filelines[!containmultiple], list)
    
    ## spread out 
    newFrame <- data.table(FileLines = unlist(filelines, recursive = FALSE),
                           Pattern   = rep(fileFrame$Pattern, lengths(filelines)),
                           Filepath  = rep(fileFrame$Filepath, lengths(filelines)))
    newFrame[ , Piece := 1:nrow(newFrame)]
    # newFrame[ , Piece := unlist(lapply(lengths(filelines), seq_len))]
    
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

parseTokenType <- function(spine, E = FALSE) {
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
    
    if (E) out[stringi::stri_detect_regex(spine, '^\\*\\*')] <- 'E'
    
    out
}



padSpinePaths <- function(local) {
  # Used by parseLocal to make sense of spine paths.
  # This function takes a list of character vectors (each representing a list of tokens from one record)
  # identifies spine paths in them, and pads the records with "_P" such that 
  # subspines (spine paths) are grouped within their spine, and all rows are the same length.
  ####NEED TO ADD OPTIONS FOR ** AND *-
  minpath <- min(sapply(local, Position, f = \(x) x %in% c('*^', '*v', '*+', '*-')), na.rm = TRUE)
  lapply(minpath:max(lengths(local)),
         \(j) {
           open  <- sapply(local, \(row) length(row) >= j && (row[j] == '*^' || row[j] == '*+'))
           close <- sapply(local, \(row) length(row) > j && all(row[j:(j + 1)] == '*v'))
           if (any(open) | any(close)) {
             
             open  <- c(FALSE, head(open, -1))
             close <- c(FALSE, head(close, -1))
             
             pad <- cumsum(as.numeric(open) + -as.numeric(close)) 
             pad <- abs(pad - max(pad))
             
             local[pad > 0] <<- Map(append, 
                                    x = local[pad > 0], 
                                    values = lapply(pad[pad > 0], \(n) rep('_P', n)), 
                                    after = j)
           }
           
           close <- sapply(local, \(row) length(row) > j && all(row[j] == '*-'))
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
         \(j) { 
           if (j > 1 && any(spinemat[ , j] == '_P')) {
             spinemat[spinemat[ , j] == '_P', j] <<- spinemat[spinemat[ , j] == '_P', j - 1] 
           }
           spine <- spinemat[ , j]
           
           interpind <- rev(grep('^\\*[^*->^v]', spine))
           interpind <- interpind[interpind > tail(stringi::stri_startswith_fixed(spine, '**'), 1)]
           interps <- stringr::str_sub(spine[interpind], start = 2L)
           
           setNames(sapply(seq_along(spine), \(i) paste(interps[i >= interpind], collapse = ',')), 
                    paste0(j, '_', rownames(spinemat)))
         } 
  ) -> tandemIs
  
  # exclusive
  exclusiveI <- apply(spinemat, 2, \(spine) tail(spine[stringi::stri_startswith_fixed(spine, '**')], 1))
  exclusiveI <- stringr::str_sub(exclusiveI, start = 3) # strip away **
  
  list(Exclusive = exclusiveI, 
       Tandems   = tandemIs)
}

parseTandem <- function(tandems, known) {
    # This function takes the parsed, cummulative
    # Tandem fields produced by parseInterpretations
    # and, using functions from the file humInterpretations.R
    # identifies any "known" (i.e., standard) interpretations
    # and parses these into a table of new Interpretation fields
    # for the humdrum table.
    # Known indicates
    if (is.null(known) || all(tandems == "", na.rm = TRUE)) return(NULL)
    
    # which are the tandems we want it to recognize
    REs <- getRE(known)
    names(REs)[names(known) != ''] <- names(known)[names(known) != '']
    if (any(REs == 'known')) {
        REs <- c(REs[REs != 'known'],  
                 knownInterpretations[Type == 'Tandem', getRE(Name, types = 'Tandem')])
    }
    
    # parse the stacked tandem vector
    uniqueTandem <- unique(unlist(stringr::str_split(unique(tandems), ',')))
    uniqueTandem <- uniqueTandem[!is.na(uniqueTandem) & uniqueTandem != '']
    
    # Reduce REs to only those that appear in data
    REs <- REs[apply(do.call('cbind', lapply(REs, stringr::str_detect, string = uniqueTandem)), 2, any)]
    
    tandemMat <- lapply(REs,
                        \(re) stringr::str_match(tandems, re)[ ,1]) #finds first instance
    
    names(tandemMat) <- names(REs)
    
    as.data.table(tandemMat)
}



#' Get tandem interpretation information from humdrum data
#' 
#' `extractTandem` extracts tandem interpretations from the raw `Tandem`
#' spine in [humdrumR object][humdrumRclass].
#' 
#' @details 
#'
#' Every [humdrumR][humdrumRclass] object has a field called
#' `Tandem`, which is a vector of strings which accumulates
#' tandem interpretations in each Spine.
#' At each record, all the previous tandems that occured in each spine
#' are listed (comma separated), with the most recent appearing first.
#' 
#' For example, consider this file:
#' 
#' ```
#' **kern **kern
#' *C:    *C:
#' *Ibass *Isoprn
#' 2G     4g
#' .      4f
#' 2C     2e
#' *G:    *G:
#' 2D     2f#
#' 2G     2g
#' *-     *-
#' ```
#' 
#' The `Tandem` field for these two spines would look like this:
#' 
#' ```
#' ""
#' "C:"                 "C:"
#' "Ibass,C:"           "Isoprn,C:"
#' "Ibass,C:"           "Isoprn,C:"
#' "Ibass,C:"           "Isoprn,C:"
#' "Ibass,C:"           "Isoprn,C:"
#' "G:,Ibass,C:"        "G:,Isoprn,C:"
#' "G:,Ibass,C:"        "G:,Isoprn,C:"
#' "G:,Ibass,C:"        "G:,Isoprn,C:"
#' "G:,Ibass,C:"        "G:,Isoprn,C:"
#' ```
#'
#' Notice that the `"C:"` isn't erased by the appearance of `"G:"`---this is a naive parser
#' that doesn't "know" that `"C:"` and `"G:"` are related.
#' The earlier tandem (`"C:"`) is just pushed further back onto the stack.
#' 
#' 
#' Don't worry, the [humdrumR data parser][readHumdrum()] *does* recognize many
#' common tandem interpretations (like `*C:` and `*G:`) and will automatically parse
#' them if they are present---in this case, they are put into the `Key` field automatically.
#' However, the `Tandem` field is retained in case your data contains any novel tandem intepretations
#' that `humdrumR` does not recognize.
#' 
#' @section extractTandem:
#' 
#' If your data *does* contain novel/unknown tandem interpretations, you can use the
#' `extractTandem` function to pull them out of the `Tandem` field.
#' The first argument to `extractTandem` must be the `Tandem` field from a 
#' [humdrumR object][humdrumRclass].
#' The second argument (`regex`) is a regular expression which is matched against
#' the the tandem interpretations.
#' For each token in `Tandem`, the most recent match (if any) is retained.
#' 
#' For example, if we wanted to manually extract the key information from the `Tandem` field 
#' (which `humdrumR` automatically does for you), we could call `extractTandem(Tandem, "[A-Ga-g][#-]*:")`.
#' 
#' @param Tandem ***Parsed tandem interpretation data.***
#' 
#' Must be `atomic`.
#' 
#' Should always be the `Tandem` field from a [humdrumR object][humdrumRclass].
#' 
#' @param regex ***A regular expression to match tandem interpretations.***
#'   
#' Must be a single `character` string.
#'   
#' You should not include a `*` at the beginning---the `*` marker for tandem interpretations are already removed from the `Tandem` field.
#' 
#' @seealso {Read more about tandem interpretations in `humdrumR` in the 
#' [humdrum table docs][humTable]. [readHumdrum()] also preprocesses
#'  some "known" tandem interpretations. }
#' @export
tandem <- function(regex, Tandem) {
  
  
  checks(regex, xcharacter & xlen1)
  
  Tandem <- paste0(',', Tandem, ',')
  
  regex <- gsub('\\^', '', regex)
  regex <- gsub('\\$', '', regex)
  regex <- paste0(',', regex, ',')
  
  matches <- stringr::str_extract(Tandem, pattern = regex)
  stringr::str_sub(matches, 2L, -2L) # get rid of commas
  
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
                     \(type) {
                       hits  <- grepl(type, spine)
                       if (!any(hits)) return(data.frame(row.names = names(spine)))
                       
                       depth <- rep(NA_integer_, length(spine))
                       depth[hits] <- stringi::stri_count_fixed(stringr::str_sub(spine[hits], 3L), '>')
                       
                       typemat <- if (any(depth != 0, na.rm = TRUE)) { 
                         sapply(sort(unique(depth[depth != 0])), 
                                \(N) { 
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
  # and returns a data.frame with three columns representing the three barline fields (Bar, DoubleBar, BarLabel)
          
  Singles <- cumsum(grepl('^=', spine))
  Doubles <- cumsum(grepl('^==', spine))
  BarLabels <- stringr::str_sub(grep('^=', spine, value = TRUE), 2L)[ifelse(Singles == 0L, NA_integer_, Singles)]
  
  data.frame(Bar = Singles, 
             DoubleBar = Doubles, 
             BarLabel = BarLabels, 
             row.names = names(spine), stringsAsFactors = FALSE)
}


#############################################################################################---
########################### readTSV ----
################################################################--




readTSV <- function(paths) {
  if (dir.exists('.humdrumR_tmp')) {
    content <- dir('.humdrumR_tmp', full.names = TRUE)
    for (file in content) file.remove(file)
  } else {
    dir.create('.humdrumR_tmp')
  }
  
  message("Naively converting ", num2str(length(paths)), ' tsv file', 
          plural(length(paths), 's', ''), ' to basic humdrum files...', 
          appendLF = FALSE)
  newfilepaths <- lapply(paths,
                  \(path) {
                    tsv <- fread(file = path)
                    
                    colnames(tsv) <- paste0('**', colnames(tsv))
                    
                    tsv[] <- lapply(tsv, as.character)
                    tsv[] <- lapply(tsv, \(col) {col[col == '' | is.na(col)] <- '.'; col})
                    ender <- replicate(ncol(tsv), '*-')
                    ender <- rlang::eval_tidy(rlang::expr(data.table(!!!ender)))
                    colnames(ender) <- colnames(tsv)
                    tsv <- rbind(tsv, ender)
                    newpath <- paste0('.humdrumR_tmp', .Platform$file.sep, basename(path), '.humdrumR')
                    fwrite(tsv,  sep = '\t', file = newpath)
                    newpath
                  })
  
  message('done!')
  
  corpus <- readHumdrum('.humdrumR_tmp/.*')
  
  for (file in newfilepaths) file.remove(file)
  file.remove('.humdrumR_tmp')
  
  corpus
}
