
#' Write [humdrumR data][humdrumRclass] to humdrum files
#' 
#' `writeHumdrum` writes `humdrumR` data into humdrum-syntax text files.
#' The current [selected field(s)][selectedFields] are evaluated to generate the humdrum output
#' data.
#' The written output should match the printout if printing the data in the `R` terminal.
#' 
#' @section File names:
#' 
#' The main option to control with `writeHumdrum` is what files to write to.
#' `writeHumdrum` uses the original names of the files, as [read by readHumdrum][readHumdrum()],
#' as the basis for *new* file names.
#' By default, `writeHumdrum` will refuse to overwrite the original files---overwriting
#' will only be allowed if you specify `overwrite == TRUE` *and* respond with `"y"` to a prompt.
#' 
#' `writeHumdrum` generates *new* file names by modifying the original read file names.
#' The `renamer` argument must be a function which takes the original names as an input `character` vector
#' (excluding the directory path and the file extension)
#' and returns a new `character` vector of the same length (the default is `R`'s identity function `force`).
#' After running `renamer`, the `character`-string `affix` and `prefix` arguments 
#' are appended/prepended to the renamed names.
#' (The `affix` is affixed *before* the extension.)
#' Finally, the `extension` argument can be used to specify a different file extension.
#' 
#' If any files in your data set contain multiple pieces, you can either write these
#' pieces into the same files (`seperateFiles = FALSE`), as they were originally, or write each piece into its 
#' own new file (`separateFiles = TRUE`); if writing pieces into separate files,
#' each piece's file name is appended with `_pieceN`, where `N` is the number of the piece within the file.
#' 
#' The `directory` argument indicates a file path where to write the files.
#' If the `directory` doesn't exist, it is created.
#' If `directory` is `NULL`, files are written to their original input directory (or directories).
#' 
#' The `EMD` argument specifies a character string to put into a new `!!!EMD` reference record
#' at the end of each file.
#' `EMD` (*Document modification description*) records keep track of modifications to humdrum data.
#' The default behavior is to print a string indicating the `humdrumR` version number and date.
#' If `EMD` is set to `NULL`, it is not appended to the files.
#' 
#' @param humdrumR ***HumdrumR data to write.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param renamer ***A `function` to modify output filenames.***
#' 
#' Defaults to `force` (keep the original filenames).
#' 
#' Must be a `function` which accepts a `character` vector and returns a new `charater` vector
#' of the same length. 
#' 
#' @param prefix ***A prefix to add to output filenames.***
#' 
#' Defaults to `"humdrumR_"`.
#' 
#' Must be a single `character` string.
#' 
#' @param affix ***An affix to add to output filenames.***
#' 
#' Defaults to `""`.
#' 
#' Must be a single `character` string.
#'
#' Affix is appended at end of filename, but before the extension.
#'
#' @param extension ***What extension to use for new files.***
#' 
#' Defaults to `NULL`, which means the file extension of the original files is used.
#'
#' Must be `NULL`, or a single, non-empty `character` string.
#'  
#' @param directory ***A directory to write the files into.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be a single `character` string.
#' 
#' If `NULL`, files are written to the same directory (or directories) they were [read from][readHumdrum()].
#' 
#' @param overwrite ***Whether to overite existing files.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `FALSE`, `writeHumdrum` will refuse to overwrite
#' any files. If `TRUE`, `writeHumdrum` will overwrite files, but only after an additional prompt from the user.
#' 
#' @param verbose ***Whether to show file names while writing.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, each *new* output file name is printed on the console
#' as the writing happens.
#' 
#' @param EMD ***A string to write to a new `!!!EMD:` record in each file.***
#' 
#' Defaults to "Edited using humdrumR, version X.X.X.XXX, on current data/time."
#' 
#' Must be a single `character` string.
#' 
#' If `NULL`, not appended.
#' 
#' 
#' 
#' @export
writeHumdrum <- function(humdrumR, 
                         prefix = "humdrumR_", renamer = force, affix = "", extension = NULL, 
                         separatePieces = FALSE,
                         directory = NULL, 
                         overwrite = FALSE, verbose = FALSE,
                         EMD = paste0("Edited in humdrumR ", packageVersion('humdrumR'), ' on ', Sys.Date())
                         ) {
    # This function will have bugs if the input files are read on different file systems, with different directory separators.
    checks(humdrumR, xhumdrumR)
    checks(renamer, xclass('function'))

    checks(affix,  xcharacter & xlen1)
    checks(prefix, xcharacter & xlen1)
    checks(extension, xnull | (xcharacter & xlen1 & xcharnotempty))
    checks(EMD,    xnull | (xcharacter & xlen1 & xcharnotempty))

    checks(directory, xnull | (xcharacter & xlen1))
    checks(extension, xnull | (xcharacter & xlen1))

    checks(overwrite, xTF)
    checks(verbose, xTF)
    
    
    cat('Writing humdrum data...\n')
    cat('Determining validity of new filenames...')
    #
    filenameTable <- pullFields(humdrumR, c('File', 'Piece', 'Filepath'), dataTypes = 'GLIMDd')
    filenameTable <- if (separatePieces) {
        filenameTable[ , .SD[1], by = Piece] 
    } else {
        filenameTable[ , list(File = File[1], Filepath = Filepath[1], Piece = list(unique(Piece))), by = File]
    }
    
    # parse filenames and paths
    filenameTable[ , Directory := if (is.null(directory)) dirname(Filepath) else directory]
    filenameTable[ , Filename  := basename(Filepath)]
    ## File extensions
    re.ext <- '\\.[A-Za-z0-9]{1,4}$'
    filenameTable[ , Extension := if (is.null(extension)) {
        stringi::stri_extract_last_regex(Filename, re.ext) 
        }else {
        stringi::stri_replace_all_regex(extension, '^\\.*', '.')
            }]
    filenameTable[ , Filename  := stringi::stri_replace_last_regex(Filename, re.ext, '')]
    
    
    #
    ### POTENTIAL SPEED UPS HERE< IF PROCESSING IS ONLY APPLIED TO UNIQUE VALUES
    # Right now, one method of processFixer replies on withHumdrum, and thus can't be a reduced version of the table.
    filenameTable[ , Filename := renamer(Filename)]
    filenameTable[ , PieceLabel := if (length(unique(Piece)) > 1L) paste0('_piece', num2order(Piece)) else '', by = Filename]
    
    filenameTable[ , NewFile := paste0(Directory, .Platform$file.sep, 
                                       prefix, 
                                       Filename,
                                       PieceLabel,
                                       affix, 
                                       Extension)]
    
    
    if (any(duplicated(cbind(filenameTable$NewFile, filenameTable$Piece)))) {
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
                harvard(dontexist, 'and'), 
                plural(length(dontexist), ' directories.', ' directory.'),
                sep = '')
            
            sapply(dontexist, dir.create)
        }
    })
    
    #### Get lines
    cat(sep = '', '\nPreparing text...')
    
    # humdrumR <- indexGLIM(humdrumR)
    humdrumR <- printableSelectedField(humdrumR, 'DGLIMDd', 'GLIMd', 'NA2dot')
    lines <- as.lines(humdrumR, dataTypes = 'GLIMDd', padPaths = 'dont')
    filestrs <- tapply(lines, # collapse across files (using names())
                       as.numeric(stringi::stri_extract_first_regex(names(lines), # as.numeric makes them sort numerically
                                                                    '^[1-9][0-9]*')), 
                       paste, collapse = '\n')
    
    if (!separatePieces) filestrs <- lapply(filenameTable$Piece, \(i) paste(filestrs[i], collapse = '\n'))
    
    if (!is.null(EMD)) {
        filestrs <- paste0(filestrs, '\n!!!EMD: ', EMD)
    }
    ### Write~
    cat(sep = '', 'Writing ', nrow(filenameTable), ' files...')
    Map(filestrs, 
        filenameTable$NewFile,
        f = \(str, path) {
        if (verbose) {
            cat('\n\t\t', path, sep = '')
            
        }
        writeLines(str, con = path)
        })
    
    if (verbose) cat('\n')
    
    cat('done!\n')
    
    invisible(filenameTable)
}

