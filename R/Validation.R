#' Validate humdrum files
#' 
#' This function checks files for violations of the 
#' humdrum syntax.
#' 
#' @param patterns `character` vector. Search pattern(s) for identifying files 
#' (see `[readHumdrum][readHumdrum]`).
#' @param recursive `logical`. If `TRUE`, the final part of the search pattern (i.e., the file search) 
#' is searched for recursively through all sub directories.
#' @param errorReport.path `character`. A directory path which, if not `NULL`, an error report is written 
#' in the file `'humdrumR_syntaxErrorReport_date.txt'`. In addition, all files with errors
#' are written to this directory (with `'errorMarkup'` appended to their names), with 
#' errors annotated inline.
#' 
#' @param files A list of character strings, each representing a record in a file.
#' 
#' @name humValidation
#' 
#' @export
validateHumdrum <- function(..., contains = NULL, recursive = FALSE, errorReport.path = NULL) {

  fileFrame <- readFiles(..., contains = contains, recursive = recursive, allowDuplicates = FALSE)
  
  fileFrame <- isValidHumdrum(fileFrame, errorReport.path = errorReport.path)
  
  invisible(fileFrame)
}


isValidHumdrum <- function(fileFrame, errorReport.path = NULL) {
    ## This function does the actual work for validateHumdrum
    ## It takes a character of filestrings (not lines) with names representing the filepath

    if (nrow(fileFrame) == 0L) {
        cat("No files to validate.\n")
        fileFrame[ , Valid := logical(0)]
        return(fileFrame)
    }
    
    ##
    files     <- fileFrame$FileLines
    filepaths <- fileFrame$Filepath
    
    cat(glue::glue("Validating {num2print(length(files))} files..."))
    
    # If files are empty everything gets thrown off...this is a hacky fix
    # Just pad empty files with a single empty record
    files <- ifelse(lengths(files) == 0L, "", files)
    
    filevec  <- rep(filepaths, lengths(files))
    recordNs <- unlist(lapply(files, seq_along), use.names = TRUE) 
    records  <- unlist(fileFrame$FileLines)
    
    local   <- !grepl('^!!', records)
    
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
    if (nrow(reports) == 0L) {
        cat("all valid.\n")
        fileFrame[ , Valid := TRUE]
        return(fileFrame)
    }
    
    badFiles  <- unique(filevec[reports$Location])
    hits      <- !filepaths %in% badFiles
    goodFiles <- filepaths[hits]
    
    cat(glue::glue("{num2print(nrow(reports))} errors in {num2print(length(badFiles))} files..."))
    
    if (!is.null(errorReport.path)) {
        file.sep <- .Platform$file.sep
        if (!dir.exists(errorReport.path)) dir.create(path = errorReport.path)
        
        reports <- reports[ , c("RecordN", "Filepath") := .(recordNs[Location], filevec[Location])]
        reports <- shortFilenames(reports)
        
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
        
        cat(glue::glue("report written in directory '{errorReport.path}'...")) 
    }
    
    cat(glue::glue("{num2print(length(goodFiles))} valid files.\n", .trim = FALSE))
    
    #
    fileFrame[ , Valid := hits]
    
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
                                    glue::glue_collapse(typ, sep = ', ', last = ' and ')
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
          hitsTable(hits, 1L + which(local)[hits],
                    glue::glue("{addsordrops} {num2print(abs(diffs[hits]))} {columns}"))
}



