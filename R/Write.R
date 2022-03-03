#' Write `humdrumR` data to humdrum files.
#' 
#' ----------NEEDS DOCUMENTATION------------
#' @export
writeHumdrum <- function(humdrumR, fieldname = NULL,
                         affix = "_humdrumR", prefix = "", rename = NULL, extension = NULL, 
                         directory = NULL, 
                         EMD = paste0("Edited in humdrumR ", packageVersion('humdrumR'), ' on ', Sys.date()),
                         overwrite = FALSE, verbose = TRUE) {
    # This function will have bugs if the input files are read on different file systems, with different directory separators.
    checkhumdrumR(humdrumR, 'writeHumdrum')
    
    if (!is.null(fieldname)) humdrumR <- setActiveFields(humdrumR, fieldname)
    
    
    cat('Writing humdrum data...\n')
    cat('Determining validity of new filenames...')
    #
    filenameTable <- getFields(humdrumR, c('File', 'Filename', 'Filepath'), dataTypes = 'GLIMDd')
    filenameTable[ , Directory := dirname(Filepath)]
    filenameTable[ , Filename      := stringi::stri_replace_all_regex(Filename, '/', ':')] # in case filenames contain dir/file
    
    ## File extensions
    re.ext <- '\\.[A-Za-z0-9]{1,4}$'
    filenameTable[ , Extension := stringi::stri_extract_last_regex(Filename, re.ext)]
    filenameTable[ , Filename      := stringi::stri_replace_last_regex(Filename, re.ext, '')]
    
    if (!is.null(extension)) {
        filenameTable[ , Extension := extension]
    }
    
    #
    ### POTENTIAL SPEED UPS HERE< IF PROCESSING IS ONLY APPLIED TO UNIQUE VALUES
    # Right now, one method of processFixer replies on withHumdrum, and thus can't be a reduced version of the table.
    rename    <- processFixer(rename, filenameTable, humdrumR)
    prefix    <- processFixer(prefix, filenameTable, humdrumR)
    affix     <- processFixer(affix,  filenameTable, humdrumR)
    directory <- processFixer(directory, filenameTable, humdrumR)
    
    filenameTable[ , Directory := if (is.null(directory)) Directory else directory]
    filenameTable[ , NewFile := paste0(Directory, .Platform$file.sep, 
                                       prefix, 
                                       if (is.null(rename)) Filename else rename, 
                                       affix, 
                                       Extension %|% "")]
    
    
    filenameTable <- filenameTable[ , .SD[1], by = Filename] # get unique value for each file!
    if (any(duplicated(filenameTable$NewFile))) {
        warning(call. = FALSE, noBreaks. = FALSE, immediate. = FALSE,
                "In your call to writeHumdrum, your arguments are resulting in non-unique names ",
                "for the output files. This is only possible through (mis)use of the rename argument. ",
                "HumdrumR will automatically append '.n' (where n is a number) to any duplicated file names ",
                "using base::make.unique.")
        cat("\n\t...the target filenames are not unique (see warnings())...")
        filenameTable[ , NewFile := base::make.unique(NewFile)]
    }
    

    
    ###Do file names already exist?
   
    alreadyexist <- filenameTable[ , file.exists(NewFile) ]
    if (any(alreadyexist)) {
        cat(sep = '',
            '\n\t', sum(alreadyexist), ' of your target filenames already exist.\n')
        
        if (overwrite) {
            cat('\tYou specified the writeHumdrum(overwrite = TRUE), but are you sure you want to overwrite these files?\n')
            answer <- readline(prompt = '\t\tType "y" and press ENTER if you DO want to overwrite existing files.\n')
            if (answer != 'y') {
                cat('writeHumdrum cancelled.\n', 'No files written.\n', sep = '')
                return(invisible(NULL))
            }
            
        } else {
            cat('\tSince you specified writeHumdrum(overwrite = FALSE), writeHumdrum has been cancelled.\n',
                'No files written.\n', sep = '')
            return(invisible(NULL))
        }
        
    }
    
    ########## Do we need to create directories?
    local({
        dontexist <- Filter(Negate(dir.exists), unique(filenameTable$Directory))
        if (length(dontexist) > 0L) {
            cat('\nCreating ', 
                glue::glue_collapse(dontexist, sep = ', ', last = ', and '), 
                if (length(dontexist) == 1L) ' directory.' else ' directories.',
                sep = '')
            
            sapply(dontexist, dir.create)
        }
    })
    
    #### Get lines
    cat(sep = '', '\nPreparing text...')
    
    # humdrumR <- indexGLIM(humdrumR)
    humdrumR <- fields.as.character(humdrumR)
    lines <- as.lines(humdrumR, dataTypes = 'GLIMDd', fieldname = fieldname, padPaths = TRUE, alignColumns = FALSE)
    filestrs <- tapply(lines, 
                       as.numeric(stringi::stri_extract_first_regex(names(lines), # as.numeric makes them sort numerically
                                                                    '^[1-9][0-9]*')), 
                       paste, collapse = '\n')
    
    ### Write~
    cat(sep = '', 'Writing ', nrow(filenameTable), ' files...')
    Map(\(str, path) {
        if (verbose) {
            cat('\n\t\t', path, sep = '')
            
        }
        writeLines(str, con = path)
        }, filestrs, filenameTable$NewFile)
    
    if (verbose) cat('\n')
    
    cat('done!\n')
    
    invisible(filenameTable)
}


processFixer <- function(fix, origfilenames, humdrumR) {
    if (is.null(fix)) return(fix)
    
    UseMethod('processFixer')
}

processFixer.character <- function(fix, origfilenames, humdrumR) return(fix)

processFixer.function <- function(fix, origfilenames, humdrumR) {
    if (length(formals(args(fix))) < 1L) return(fix())
    
    uniqfilenames <- unique(origfilenames)
    uniqfilenames[ , Fixed := fix(Filename)]
    
    uniqfilenames[origfilenames, on = 'File']$Fixed
    
}
processFixer.formula <- function(fix, origfilenames, humdrumR) {
    as.character(withHumdrum(humdrumR, fix, recordtypes ~ 'GLIMDd'))
}
processFixer.default <- function(fix, origfilenames, humdrumR) as.character(fix)

