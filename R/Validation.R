#' Validate humdrum files
#' 
#' This function checks files on your local machine for violations of the 
#' [humdrum syntax](http://www.humdrum.org/guide/ch05/).
#' Detailed error reports can be generated, pointing to specific problematic records in files.
#'
#' 
#' @details 
#'
#' Only violations of the general [humdrum syntax](http://www.humdrum.org/guide/ch05/) are identified.
#' For example, missing exclusive interpretations, `*-` spine enders, or null data tokens.
#' The `validateHumdrum` function *does not* check for ill-formed data content---for example, a `**kern`
#' spine containing the token `"Lsharp"` will not be rejected.
#' Note that `validateHumdrum` is quite picky about details! 
#' "Hanging white space," even in global records, will be marked as invalid files!
#'
#' `validateHumdrum` is called in the same manner as [readHumdrum()], by providing
#' one or more regex search patterns to match files on your machine.
#' (The `...`, `recursive`, and `contains` arguments are all simply passed to [findHumdrum()].)
#' When called, `validateHumdrum` prints basic messages informing you about the result of
#' the file matching and validity testing.
#'
#' The function also returns ([invisibly][base::invisible()]) the [findHumdrum()] "file info"
#' `data.table`, with additional `Valid` and `Errors` columns:
#' The `Valid` column is `logical`, with `TRUE` for all valid humdrum files.
#' The `Error` column is a list of `data.table`s, describing syntax errors in the files
#' if any.
#' The `Error` `data.table`s have one row for each error in a file (if any), and three columns:
#' 
#' + `Filepath`: The file name.
#' + `Record`: Which record contains the error.
#' + `Message`: A description of the error.
#' 
#' 
#' @section Error reports:
#' 
#' If desired, the contents of the `validateHumdrum` can be written files.
#' This is most useful, as we can print the errors tagged right alongside the original raw data.
#' To write an error report, set the `errorReport.path` argument to a non-`NULL` string,
#' pointing to a directory path on your machine.
#' If the directory doesn't exist, `R` will (attempt to) create it.
#' 
#' In the `errorReport.path` directory, the complete error report(s) for all
#' the files (same as in the returned "fileFrame", see above) are written into a single file named
#' `'humdrumR_syntaxErrorReport_DATE.txt'` (with the `date` coming from [Sys.Date][base::Sys.Date()]`). 
#' In addition, a sub directory called `AnnotatedFiles` is created.
#' In this directory, copies of all the files which contain errors are written, with `_errorAnnotations` appended
#' to their names.
#' In each file, individual errors are directly indicated in the record where they occur.
#' The output looks like this:
#' 
#' ```
#'                                              | The original records from the input file
#'                                              | appear on the right side.
#' Error message for record three printed here. | Exactly as they did
#'                                              | in the input file.
#' ````
#' 
#' @param ... ***Arguments passed to [findHumdrum()].*** 
#'
#' Used to identify files on the local machine to test for humdrum validity.
#' This is mainly used to pass [regex file-path search patterns][readHumdrum()], but may also be used to pass
#' the `recursive` and/or `contains` arguments to [findHumdrum()].
#'
#' @param errorReport.path ***A directory path where to write error report files.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be a single `character` string.
#' 
#' If `NULL` (the default), no error report files are written.
#'
#' @examples 
#' validateHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' @export
validateHumdrum <- function(..., errorReport.path = NULL) {

  fileFrame <- readFiles(..., allowDuplicates = FALSE)
  
  fileFrame <- isValidHumdrum(fileFrame, errorReport.path = errorReport.path)
  
  invisible(fileFrame)
}


isValidHumdrum <- function(fileFrame, errorReport.path = NULL) {
    ## This function does the actual work for validateHumdrum
    ## It takes a character of filestrings (not lines) with names representing the filepath

    if (nrow(fileFrame) == 0L) {
        message("No files to validate.")
        fileFrame[ , Valid := logical(0)]
        return(fileFrame)
    }
    
    ##
    emptyFiles <- fileFrame[ , lengths(FileLines) == 0L]
    if (any(emptyFiles)) message(glue::glue("{num2print(sum(emptyFiles))} files are simply empty..."))
    
    files     <- fileFrame$FileLines
    filepaths <- fileFrame$Filepath
    
    
    message(glue::glue("Validating {if (any(emptyFiles)) 'remaining ' else ''}{num2print(sum(!emptyFiles), label = 'file')}..."), appendLF = FALSE)
    # If files are empty everything gets thrown off...this is a hacky fix
    # Just pad empty files with a single empty record
    
    filevec  <- rep(filepaths, lengths(files))
    recordNs <- unlist(lapply(files, seq_along), use.names = TRUE) 
    records  <- unlist(fileFrame$FileLines)
    
    local   <- !grepl('^!!', records)
    
    ## Prepare validity reports
    reports <- list()
    # 
    funcs <- list(validate_File,
                  validate_Characters,
                  validate_Records,
                  validate_recordTypes,
                  validate_whiteSpace,
                  validate_spinePaths)
    reports <- data.table::rbindlist(lapply(funcs, do.call, 
                                            args = list(records, local, filevec)))
    reports <- reports[ , c("RecordN", "Filepath") := .(recordNs[Location], filevec[Location])]
    if (any(emptyFiles)) {
      reports <- rbind(reports, 
                       fileFrame[emptyFiles == TRUE, list(Filepath = Filepath, RecordN = 0, Message = 'Empty', Location = NA)])
      setorder(reports, Filepath)
    }
    # reports <- shortFilenames(reports)
    
    if (nrow(reports) == 0L) {
        message("all valid.")
        fileFrame[ , Valid := TRUE]
        return(fileFrame)
    }
    
    badFiles  <- reports[ , unique(Filepath)]
    fileFrame[ , Valid := !Filepath %in% badFiles]
    message(glue::glue("{num2print(nrow(reports))} errors in {num2print(length(badFiles))} files{if (any(emptyFiles)) ' (including empty files)' else ''}..."), 
            appendLF = FALSE)
    
    
    if (!is.null(errorReport.path)) {
        file.sep <- .Platform$file.sep
        if (!dir.exists(errorReport.path)) dir.create(path = errorReport.path)
        
        # Summary file
        summary <- reports[ , fileErrorSummary(.SD, unique(Filepath)) , by = Filepath, .SDcols = colnames(reports)]$V1
        summary.file <- paste0(errorReport.path, file.sep,
                               "humdrumR_syntaxErrorReport_", 
                               Sys.Date(), ".txt")
        writeLines(summary, summary.file)
        
        # Annotated files
        recordTable <- data.table::data.table(Record = records, Location = seq_along(records),
                                              RecordN = recordNs, Filepath = filevec)
        recordReports <- reports[recordTable, on = c('Location', 'Filepath', 'RecordN')]
        recordReports <- recordReports[ , if (any(!is.na(Message))) .SD else NULL, by = Filepath]
        
        annotation.path <- paste0(errorReport.path,  file.sep,
                                  "AnnotatedFiles", file.sep)
        if (!dir.exists(annotation.path)) dir.create(annotation.path)
        
        uniqFiles <- gsub(file.sep, '_', unique(recordReports$Filepath))
        recordReports[ , Filepath := paste0(annotation.path, uniqFiles[match(Filepath, unique(Filepath))], '_errorAnnotations')]
        recordReports[ , Message := Message %|% ""]
        recordReports <- recordReports[ , .(Filepath = unique(Filepath), Message = paste(Message, collapse = ' and '), 
                                            RecordN = unique(RecordN), Record = unique(Record)), by = Location]
        recordReports[ , Message := padder(recordReports$Message, sizes = max(nchar(recordReports$Message)))]
        recordReports[ , writeLines(paste0(Message, ' | ', Record), unique(Filepath)), by = Filepath]
        
        message(glue::glue("report written in directory '{errorReport.path}'..."), appendLF = FALSE) 
    }
    
    message(glue::glue("{num2print(sum(fileFrame$Valid))} valid files.\n", .trim = FALSE))
    

    fileFrame <- reports[, list(Errors = list(data.table(Filepath = Filepath, Record = RecordN, Error = Message))), by = Filepath][fileFrame, on = 'Filepath']
    fileFrame$Errors <- lapply(fileFrame$Errors,
                               \(errors) {
                                 if (is.null(errors)) {
                                   data.table(Filepath = character(0), Record = integer(0), Message = character(0))
                                 } else {
                                   errors
                                 }
                                 
                                 })
    
    fileFrame
}

fileErrorSummary <- function(.SD, filename) {

 file.message <- glue::glue("In file '{filename}':\n", .trim = FALSE)
 
 .SD <- .SD[, .(Messages = paste(Message, sep = '\n\t\t')), by = RecordN] #may be multiple messages on one record
 
 messages <- glue::glue("\trecord {.SD$RecordN}\n\t\t{.SD$Messages}\n", .trim = FALSE)
 
 paste(file.message, paste(messages, collapse = '\n'), '\n', sep = '')
}

# first character of each token stringi::stri_replace_all_regex(records[hits], '([^\t])[^\t]*\t', '$1\t')

hitsTable <- function(hits, indices, messages) {
 if (any(hits)) {
  data.table(Message = as.character(messages), 
             Location = indices)         
 } else {
  data.table(Message = 1, Location = NA)[Message < 0]
 }
}

validate_File <- function(records, local, filevec) {
    ## Could improve this to give more specific messages?
 tapply(records[local], filevec[local],
        \(file) {
                  opens  <- grepl('^\\*\\*', file)
                  closes <- grepl('^\\*-', file)
                  
                  which(opens)[1] != 1L ||
                  !any(opens) ||
                            !any(closes) ||
                            min(which(opens)) > min(which(closes)) ||
                            max(which(opens)) > max(which(closes))
        }) -> hits
 
  firstrecs <- tapply(which(local), filevec[local], '[', i = 1)
  
  hitsTable(hits, firstrecs[hits], "** and *- records in file are missing or don't add up")
          
}
 

validate_Records <- function(records, ...){
 
 output <- list()

 # any empty lines
 hits_empty <- stringi::stri_detect_regex(records, '^$|^\\s$')
 output$Empty <- hitsTable(hits_empty, which(hits_empty), 'is empty')
 
 data.table::rbindlist(output)
 
}

validate_Characters <- function(records, local, ...) {
          output <- list()
          
          # carriage returns
          hits_carriage <- stringi::stri_detect_fixed(records[local], '\r')
          output$Carriage <- hitsTable(hits_carriage, which(local)[hits_carriage], 'illegal carriage return')
          
          data.table::rbindlist(output)
}

validate_whiteSpace <- function(records, local, ...) {
          local <- which(local)
          output <- list()
          
          
          
          # startwith 
          hits_start <- stringi::stri_detect_regex(records, '^\\s')
          output$Start <- hitsTable(hits_start, which(hits_start), 'starts with whitespace')
          
          # double 
          hits_double   <- stringi::stri_detect_fixed(records[local], '  ')
          output$Double <- hitsTable(hits_double, local[hits_double], 'contains consecutive spaces')
          
          # hanging space 
          hits_hanging   <- stringi::stri_detect_regex(records[local], ' \t|\t | $|\t$') & 
                    !stringi::stri_detect_regex(records[local], '^!') # ok in comments
          output$Hanging <- hitsTable(hits_hanging, local[hits_hanging], 'contains hanging white space')
          
          # missing token
          hits_missing <- stringi::stri_detect_fixed(records[local], '\t\t')
          output$Missing <- hitsTable(hits_missing, local[hits_missing], 'contains consecutive tabs')
          
          data.table::rbindlist(output)
}

validate_recordTypes <- function(records, local, ...) {
          hits <- stringi::stri_detect_regex(records[local],  '\t([!=*]).*\t(?!\\1)|^([!=*]).*\t(?!\\2)') 
          # if (!any(hits)) return(list(Message = c(), Location = which(local)[hits]))
          
          toks <- strsplit(records[local][hits], split = '\t')
          firsttoks <- lapply(toks, stringr::str_sub, end = 1L)
          
          types <- c("*", "!", "=")
          types <- sapply(firsttoks, 
                          \(toks) {
                                    typ <- c("interpretation", "comment", "barline", "data")[match(unique(toks), c("*","!","="), nomatch = 4)]
                                    harvard(typ, 'and')
                                    })
          
          toks <- sapply(firsttoks, \(x) paste0("(", glue::glue_collapse(x, sep = ' '), ')'))
          
          hitsTable(hits, which(local)[hits], glue::glue("mixes {types} tokens {toks}"))
}


validate_spinePaths <- function(records, local, filevec) {
          locrecords <- records[local]
          
          ntab <- stringi::stri_count_fixed(locrecords, '\t') + 1L
          diffs <- unlist(tapply(ntab, filevec[local], \(nt) (diff(c(0L, nt, 0L)))))
          
          locrecords <- unlist(tapply(locrecords, filevec[local], append, values = 'XXX_padding', after = 0L))
          
          opens  <- stringi::stri_count_fixed(locrecords, '*^')
          opens  <- opens + c(stringi::stri_count_regex(locrecords[-1], '\\*[+*]'), 0)
          closes <- stringi::stri_count_fixed(locrecords, '*v')
          closes <- ifelse(closes == 0, 0, closes - 1)
          closes <- closes + stringi::stri_count_fixed(locrecords, '*-')
          
          changes <- opens - closes
          
          # output <- list()
          
          #
          hits <- changes != diffs #& c(!singleDrop, FALSE) & c(!singleAdd, FALSE)
          
          padding <- locrecords == 'XXX_padding'
          hits <- hits[!padding]
          diffs <- diffs[!padding]
          
          hits[diffs == 0L] <- FALSE
          
          addsordrops <- ifelse(diffs[hits] > 0, 'adds', 'drops')
          columns <- ifelse(abs(diffs[hits]) > 1L, 'columns', 'column')
          hitsTable(hits, which(local)[hits],
                    glue::glue("{addsordrops} {num2print(abs(diffs[hits]))} {columns}"))
}



