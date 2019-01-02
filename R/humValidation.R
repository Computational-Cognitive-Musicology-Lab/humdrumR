#' Validate humdrum files
#' 
#' This function checks files for violations of the 
#' humdrum syntax.
#' 
#' @param patterns \code{character} vector. Search pattern(s) for identifying files 
#' (see \code{\link{readHumdrum}).
#' @param recursive \code{logical}. If \code{TRUE}, the final part of the serach pattern (i.e., the file search) 
#' is searched for recursively through all sub directories.
#' @param errorReport.path \code{character}. A directory path which, if not \code{NULL}, an error report is written 
#' in the file \code{'humdrumR_syntaxErrorReport_date.txt'). In addition, all files with errors
#' are written to this directory (with \code{'errorMarkup'} appended to their names), with 
#' errors annotated inline.
#' 
#' @param files A list of character strings, each representing a record in a file.
#' 
#' @name humValidation
#' 
#' @export
validateHumdrum <- function(pattern = NULL, recursive = FALSE, errorReport.path = NULL, files = NULL) {

  if (!is.null(pattern)) files <- readFiles(pattern, recursive = recursive, verbose = FALSE)
  if (is.null(files)) stop("validateHumdrum needs either a pattern or files argument to be specified", call. = FALSE)
  
  cat(glue::glue("Validating {length(files)} files..."))
  
  filenames <- names(files)
  
  filevec <- rep(filenames, lengths(files))
  recordNs <- unlist(lapply(files, seq_along), use.names = TRUE) 
  records  <- unlist(files)
          
  local   <- !grepl('^!!', records)
  
  reports <- list()
  # 
  funcs <- list(validate_Characters,
                validate_Records,
                validate_recordTypes,
                validate_whiteSpace,
                validate_spinePaths)
  reports <- data.table::rbindlist(lapply(funcs, do.call, args = list(records, local, filevec)))
  
  if (nrow(reports) == 0L) {
   cat("all valid.\n")
   return(invisible(files))
  }
  
  badFiles  <- unique(filevec[reports$Location])
  goodFiles <- filenames[!filenames %in% badFiles]
  
  cat(glue::glue("{num2print(nrow(reports))} errors in {num2print(length(badFiles))} files..."))
  
  if (!is.null(errorReport.path)) {
            file.sep <- .Platform$file.sep
            if (!dir.exists(errorReport.path)) dir.create(path = errorReport.path)
            
            reports <- reports[ , c("RecordN", "File") := .(recordNs[Location], filevec[Location])]

            # Summary file
            summary <- reports[ , fileErrorSummary(.SD, unique(File)) , by = File]$V1
            summary.file <- paste0(errorReport.path, file.sep,
                                   "humdrumR_syntaxErrorReport_", 
                                   Sys.Date(), ".txt")
            writeLines(summary, summary.file)
            
            # Annotated files
            recordTable <- data.table::data.table(Record = records, Location = seq_along(records),
                                                  RecordN = recordNs, File = filevec)
            recordReports <- reports[recordTable, on = c('Location', 'File', 'RecordN')]
            recordReports <- recordReports[ , if (any(!is.na(Message))) .SD else NULL, by = File]
            
            annotation.path <- paste0(errorReport.path,  file.sep,
                                      "AnnotatedFiles", file.sep)
            if (!dir.exists(annotation.path)) dir.create(annotation.path)
            
            uniqFiles <- gsub(file.sep, '_', shortFileNames(unique(recordReports$File)))
            recordReports[ , File := paste0(annotation.path, uniqFiles[match(File, unique(File))], '_errorAnnotations')]
            recordReports[ , Message := ifelse(is.na(Message), "", Message)]
            recordReports <- recordReports[ , .(File = unique(File), Message = paste(Message, collapse = ' and '), 
                                                RecordN = unique(RecordN), Record = unique(Record)), by = Location]
            recordReports[ , Message := padder(recordReports$Message, sizes = max(nchar(recordReports$Message)))]
            recordReports[ , writeLines(paste0(Message, ' | ', Record), unique(File)), by = File]
                        
            cat(glue::glue("report written in directory '{errorReport.path}'...")) 
  }
  
  cat(glue::glue("{num2print(length(goodFiles))} valid files.\n", .trim = FALSE))
      
  return(invisible(files[goodFiles]))
}

fileErrorSummary <- function(.SD, filename) {

 file.message <- glue::glue("In file '{shortFileNames(filename)}':\n", .trim = FALSE)
 
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

validate_Records <- function(records, ...){
 hits <- stringi::stri_detect_regex(records, '^$|^\\s$')
 
 hitsTable(hits, which(hits), 'is empty')
 
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
          hits_hanging   <- stringi::stri_detect_regex(records[local], ' \t|\t | $|\t$')
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
                          function(toks) {
                                    typ <- c("interpretation", "comment", "barline", "data")[match(unique(toks), c("*","!","="), nomatch = 4)]
                                    glue::glue_collapse(typ, sep = ', ', last = ' and ')
                                    })
          
          toks <- sapply(firsttoks, function(x) paste0("(", glue::glue_collapse(x, sep = ' '), ')'))
          
          hitsTable(hits, which(local)[hits], glue::glue("mixes {types} tokens {toks}"))
}


validate_spinePaths <- function(records, local, filevec) {
          locrecords <- records[local]
          
          ntab <- stringi::stri_count_fixed(locrecords, '\t') + 1L
          diffs <- unlist(tapply(ntab, filevec[local], function(nt) (diff(c(0L, nt, 0L)))))
          
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
          
          addsordrops <- ifelse(diffs[hits] > 0, 'adds', 'drops')
          columns <- ifelse(abs(diffs[hits]) > 1L, 'columns', 'column')
          hitsTable(hits, 1L + which(local)[hits],
                    glue::glue("{addsordrops} {num2print(abs(diffs[hits]))} {columns}"))
}



