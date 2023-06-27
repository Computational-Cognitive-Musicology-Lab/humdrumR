
# Renumbering ----

renumberFiles <- function(hum) UseMethod('renumberFiles')
renumberFiles.humdrumR <- function(hum) {
    humtab <- getHumtab(hum)
    putHumtab(hum) <- renumberSpines.data.table(humtab)
    hum
}
renumberFiles.data.table <- function(hum) {
    hum[ , File := match(File, sort(unique(File)))]
    hum[ , Piece := match(Piece, sort(unique(Piece)))]
    hum
}

renumberSpines <- function(hum) UseMethod('renumberSpines')
renumberSpines.humdrumR <- function(hum) {
    humtab <- getHumtab(hum, 'GLIMDd')
    putHumtab(hum, overwriteEmpty = c()) <- renumberSpines.data.table(humtab)
    hum
    
}
renumberSpines.data.table <- function(hum) {
    hum[ , Spine := match(Spine, sort(unique(Spine))), by = Piece]
    
    hum
}

# mergeHumdrum ----


#' Merge two (or more) humdrumR datasets
#'
#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @export
#' @name humMerge
mergeHumdrum <- function(...) {
    .stop("mergeHumdrum has not been implemented yet")
    
}


#' Expand paths into new spines
#' 
#' This function takes a [humdrumR object][humdrumRclass]
#' and "expands" the content of any spine paths by filling them in with
#' the content of their parent path(s).
#' 
#' @details 
#' 
#' For example, imagine that in humdrum representation of a eight-measure
#' piano score, the annotator included an [ossia](https://en.wikipedia.org/wiki/Ossia)
#' passage in the seventh measure.
#' If we want to simply ignore the ossia passage, we can just specify a [subset()] where `Path == 0`.
#' If we want to study *only* the ossia passage, we can grab a [subset()] where `Path == 1`.
#' However, what if we want to study the ossia as it would be performed, with the ossia measure
#' swapped in for measure 7, but still using measures 1-6 and 8 from the main path?
#' `expandPaths()` will help us do just this:
#' `expandPaths()` will copy the contents of measure 1-6 and 8 into the second path and,
#' if `asSpines = TRUE`, then copy the path into it's own new spine.
#' We can then treat that new "full" path/spine just like any other path/spine.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param asSpines ***Should paths expanded into new spines?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the expanded paths are copied into their
#' own new spines (shifting higher spines over as needed).
#' 
#' 
#' @family {Humdrum data reshaping functions}
#' @export
expandPaths <- function(x, asSpines) UseMethod('expandPaths')
#' @export
expandPaths.humdrumR <- function(x, asSpines = TRUE) {
    checks(asSpines, xTF)
    
    if (!anyPaths(x)) return(x)
    
    putHumtab(x) <- expandPaths.data.table(getHumtab(x), asSpines = asSpines)
    
    x
}
expandPaths.data.table <- function(humtab, asSpines = TRUE) {
    if (!any(humtab$Path > 0L, na.rm = TRUE)) return(humtab)
    
    humtab[ , Piece.Spine.Record := paste(Piece, Spine, Record, sep = ':')]
    humtab[ , Piece.Spine := paste(Piece, Spine, sep = ':')]
    
    paths <- unique(humtab[!is.na(Path) & Path > 0L, c('ParentPath', 'Path'), with = FALSE])
    
    for(path in unique(paths$Path)) {
        for (parent in paths[Path == path, ParentPath]) {
            
            recordsWithPaths <- humtab[Path == path & ParentPath == parent, unique(Piece.Spine.Record)]
            spinesWithPaths <- humtab[Path == path & ParentPath == parent, unique(Piece.Spine)]
            
            new <- humtab[Piece.Spine %in% spinesWithPaths & 
                              !Piece.Spine.Record %in% recordsWithPaths &
                              Path == parent ]
            new[ , Path := path]
            new[ , ParentPath := NA_integer_]
            
            humtab <- rbind(new, humtab)
        }
    }
    
    
    humtab[ , Piece.Spine.Record := NULL]
    humtab[ , Piece.Spine := NULL]

    humtab <- orderHumtab(humtab)
    
    
    
    if (asSpines) {
        humtab[ , Spine := 1L + Path + ((Spine - 1L) * (1L + max(Path, na.rm = TRUE))), by = Piece]
        humtab[ , Path := 0L]
        renumberSpines.data.table(humtab)
        
    }
    
    if ('I' %in% humtab$Type) {
        if (asSpines) {
            humtab[Type == 'I', Token := stringr::str_replace(Token, '\\*[v^+]', '*')]
        } else {
            humtab[Type == 'I' & is.na(ParentPath), Token := stringr::str_replace(Token, '\\*[v^+]', '*')]
        }
    }
    
    
    humtab
}


contractPaths <- function(humtab) {
    humtab[!is.na(parentPath)]
}


# collapseHumdrum ----

#' "Collapse" humdrumR data into a field
#' 
#' `collapseHumdrum` allows you collapse a data field across
#' across groups within the data indicated by the `by` argument.
#' Data is "collapsed" either by [pasting][base::paste()] the data into a string,
#' or by putting them into [list][base::list()].
#' `collapseStops`, `collapsePaths`, and `collapseRecords` are built-in
#' calls to `collapseHumtab`, with some additional optimizations.
#'
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param by ***Fields to collapse data within.***
#' 
#' Must be `character`. 
#' 
#' Must be a `character` string [partially][partialMatching] matching the name(s) of a data field(s) in the `humdrumR` input.
#'
#' Data in the `collapseField` will be collapsed within these fields.
#' 
#' @param collapseField ***The target field in the `humdrumR` data to collapse.***
#' 
#' Defaults to `selectedFields(humdrumR)[1]`.
#' 
#' Must be a single `character` string.
#' 
#' @param dataTypes ***Which types of humdrum record(s) to collapse.***
#' 
#' Defaults to `"GLIMDd"`.
#' 
#' Must be `character`. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation **Fields** section for explanation.)
#'
#' @param collapseAtomic ***Whether to collapse the data into `character` strings.***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, data is collapsed into a single `character` string. If `FALSE`, data is conctanated in a `list`.
#'
#' @param sep ***A separator for collapsed strings.***
#' 
#' Defaults to `" "` (space).
#' 
#' Must be a single `character` string.
#'
#' Only has an effect if `collapseAtomic == TRUE`.
#' 
#' @family {Humdrum data reshaping functions}
#' @seealso The humdrum [folding functions][cleave()] serve a similar function,
#' "folding" data into *new* fields, rather than collapsing it within a field.
#' @export
collapseHumdrum <- function(humdrumR, by,
                            collapseField = selectedFields(humdrumR)[1], 
                            dataTypes = 'GLIMDd', 
                            collapseAtomic = TRUE, sep = ' ') {
 
    checks(humdrumR, xhumdrumR)
    by <- fieldMatch(humdrumR, by, 'collapseHumdrum', 'by')
    collapseField <- fieldMatch(humdrumR, collapseField, 'collapseHumdrum', 'collapseField')
    dataTypes <- checkTypes(dataTypes, 'collapseHumdrum', argname = 'dataTypes')
    checks(collapseAtomic, xTF)
    checks(sep, xcharacter & xlen1)
    
    humtab <- getHumtab(humdrumR, dataTypes)
    
    humtab <- collapseHumtab(humtab, by = by, collapseField = collapseField, sep = sep, collapseAtomic = collapseAtomic)
    
    putHumtab(humdrumR, dataTypes) <- humtab
    
    humdrumR
    
}


collapseHumtab <- function(humtab, by, target = humtab, collapseField, collapseAtomic = TRUE, sep = ' ') {
    if (nrow(target) == 0L) return(humtab)
    
    collapser <- switch(paste0(is.list(humtab[[collapseField]]), collapseAtomic),
                        TRUETRUE   = \(x) paste(unlist(x), collapse = sep),
                        TRUEFALSE  = \(x) list(list(unlist(x, recursive = FALSE))), # there could be bugs here if we collapse data that is objects.
                        FALSETRUE  = \(x) paste(x, collapse = sep),
                        FALSEFALSE = \(x) list(list(x)))
    
    target <- humtab[unique(target[ , by , with = FALSE]), on = by]
    collapsed <- rlang::eval_tidy(rlang::expr(target[ , collapser(!!rlang::sym(collapseField)), by = by]))
    humtab <- humtab[ , .SD[1], by = by]

    collapsed <- collapsed[humtab, on = by, V1]
    field <- humtab[[collapseField]]
    if (collapseAtomic) {
        replacements <- !is.na(collapsed) 
        
        if (is.list(field)) field <- sapply(field, collapser)
        
    } else {
        replacements <- !sapply(collapsed, is.null)
    }
    field[replacements] <- collapsed[replacements]
    humtab[[collapseField]] <- field
    humtab$Type[replacements] <- 'D'
    
    humtab
    
}


#' @rdname collapseHumdrum
#' @export 
collapseStops <- function(humdrumR, collapseField = selectedFields(humdrumR)[1], collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    collapseField <- fieldMatch(humdrumR, collapseField, 'collapseStops', 'collapseStops')
    checks(collapseAtomic, xTF)
    checks(sep, xcharacter & xlen1)
    
    
    humtab <- getHumtab(humdrumR, 'D')
    humtab <- collapseHumtab(humtab, by = c('Piece', 'Spine', 'Path', 'Record'),
                             target = humtab[Stop > 1L & !is.na(Stop)],
                             collapseField = collapseField,
                             collapseAtomic = collapseAtomic, sep = sep)
    
    putHumtab(humdrumR, overwriteEmpty = 'D') <- humtab
    removeEmptyStops(humdrumR)

}



#' @rdname collapseHumdrum
#' @export
collapsePaths <- function(humdrumR, collapseField = selectedFields(humdrumR)[1], collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    checks(collapseAtomic, xTF)
    collapseField <- fieldMatch(humdrumR, collapseField, 'collapsePaths', 'collapseField')
    checks(sep, xcharacter & xlen1)
    
    humtab <- getHumtab(humdrumR)
    humtab <- collapseHumtab(humtab, by = c('Piece', 'Spine', 'Record'),
                             target = humtab[Path > 0L & !is.na(Path)],
                             collapseField = collapseField,
                             collapseAtomic = collapseAtomic, sep = sep)
    
    putHumtab(humdrumR) <- humtab
    humdrumR
    
}

#' @rdname collapseHumdrum
#' @export
collapseRecords <- function(humdrumR, collapseField = selectedFields(humdrumR)[1], collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    checks(collapseAtomic, xTF)
    collapseField <- fieldMatch(humdrumR, collapseField, 'collapseRecords', 'collapseField')
    checks(sep, xcharacter & xlen1)
    
    collapseHumdrum(humdrumR, dataTypes = 'GLIMDd', 
                    by = c('Piece', 'Record'),
                    collapseField = collapseField,
                    collapseAtomic = collapseAtomic, sep = sep)

}



# Cleave and Rend ----

## Cleave ----

#' "Fold" humdrumR data into new fields
#'
#' Many humdrum datasets encode data across multiple spines, spine-paths, or stops.
#' By default, `humdrumR` parses each separate spine, spine-path, and stop as their own individual
#' data points, taking up one row in the [humdrum table][humTable].
#' If we want to treat data in multiple spines/paths/stops as different aspects of the same data
#' it is easiest to reshape the data so that the information is in different humdrumR [fields][fields()]
#' rather than separate spines/paths/stops.
#' We "fold" the data from one structural location over "on top" of other data using `cleave`.
#' 
#' @section From where to where:
#' 
#' The `numeric` `fold` and `onto` arguments specify where to fold from/to.
#' `fold` indicates the Spine/Path/Stop to fold *from*, "*on to*" the Spine/Path/Stop
#' indicated by `onto`.
#' For example, if you specify `cleave(mydata, fold = 2, onto = 1, what = 'Spine')`
#' spine 2 will be folded "on top of" spine 1.
#' The `fold` and `onto` targets may not overlap.
#' 
#' 
#' The `fold` and `onto` arguments can be vectors of any length, which are interpreted in parallel:
#' for example, the combination `fold = 1:2` and `onto = 3:4` would map the first spine
#' to the third spine (`1 -> 3`) and the second spine to the 4th spine (`2 -> 4`).
#' If the `onto` targets are duplicated, the `fold` spines will be folded onto
#' multiple new fields: for example, the combination `fold = 1:2` and `onto = c(3, 3)` will
#' map first spine *and* the second spine on to *two* new fields of the third spine.
#' If the `fold` target is duplicated, the same `fold` spines can be copied onto multiple
#' `onto` spines: for example, the combination `fold = 1` and `onto = 2:3` will map the contents 
#' of the first spine onto the second *and* third spine, duplicating the spine-1 data.
#' 
#' The lengths of `fold` and `onto` are automatically matched, so
#' arguments like `fold = 1:2` and `onto = 3` are equivalent to `fold = 1:2, onto = c(3, 3)`.
#' This makes it easy to do things like "copy all four spines onto spine 1": 
#' just write `fold = 2:4, onto = 1`.
#' 
#' 
#' To specify what structural field you want to fold across, 
#' use the `what` argument (`character`, `length == 1`).
#' The default `what` value is `"Spine"`; other common fold options are `"Path"`,
#' and `"Stop"`, though you might want to use the convenient `foldPaths()` and `foldStops()`
#' functions directly (details below).
#' (You may also fold across `"Record"` or `"DataRecord"`), but these are advanced/tricky!)

#' @section Which fields:
#' 
#' The `fromField` (`character`, `length == 1`) controls which field in the `fold` 
#' spine/path/stop is folded into a new field.
#' The `fromField` argument defaults to the (first) [selected fields][selectedFields],
#' and must match (or [partially match][partialMatching]) a field in the `humdrumR` argument data set.
#' In some cases, the `fold` data is smaller than the `onto` data---for instance,
#' spine paths often only exist for part of a spine, so there is less data in the path 
#' than in the full spine.
#' In these cases, it can be helpful to set `fillFromField == TRUE`,
#' which causes the missing parts of `fold` to be filled with data from the `from`
#' field. `foldPaths` does this by default.
#' 
#' The resulting new fields will automatically be named as appropriate `Result`s fields.
#' The `newFieldNames` argument (`character`) can be used to control the output names:
#' one for each new field created by the fold.
#' If you specify too many `newFieldNames`, the later names are ignored.
#' If you specify too few `newFieldNames`, the later names will be given result names, 
#' consistent with the default behavior.
#' 
#' 
#' @section Piece-Specific Folding:
#' 
#' By default, the same "fold" is carried out in each piece in the input corpus 
#' (`humdrumR` argument).
#' If you need to specify different folds in different pieces, you have to specify the `Piece`
#' argument (`numeric`, whole number).
#' For *every* piece in the corpus you want to apply folds to, you must specify all the `fold`
#' and `onto` arguments in parallel vectors with the `Piece` argument (even if this is reduendant 
#' for some files).
#' For example, if we specify the combinations,
#' 
#' |  `fold`  |  `onto`  | `Piece` |
#' |:--------:|:--------:|:------:|
#' | `1`      | `2`      | `1`    |
#' | `3`      | `4`      | `1`    |
#' | `1`      | `2`      | `2`    |
#' | `4`      | `3`      | `2`    |
#' 
#' then
#' 
#' + In `Piece` one: 
#'   + the first spine is mapped to the second spine
#'   + the third spine is mapped to the fourth spine
#' + In `Piece` two: 
#'   + the first spine is mapped to the second spine
#'   + the fourth spine is mapped to the third spine
#' 
#' If any files in the corpus are not included, they will not be affected at all!
#' 
#' @section Predefined folds:
#' 
#' The convenient `foldStops()` and `foldPaths()` functions automatically fold *all* stops/paths in a dataset 
#' onto the first stop/path, creating new fields named, e.g., `Path1`, `Path2`, etc.
#' Another extremely useful function is [foldExclusive()], which automatically folds spines 
#' based on their exclusive interpretation.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param fold ***Which target structure (spine, path, etc.) to "fold" onto another structural position.***
#' 
#' Must be natural numbers.
#' 
#' @param onto ***Which target structure (spine, path, etc.) is the data "folded" onto.***
#'   
#' Must be natural numbers.
#' 
#' @param what ***The structural (spine, path, etc.) which is folded across.***
#' 
#' Defaults to `"Spine"`.
#' 
#' Must be a single `character` string. Valid options are `"Spine"`, `"Path"`, `"Stop"`, `"Record"`,and `"DataRecord"`.
#'
#' @param Piece ***Which pieces in the corpus should be folded (see "Piece-Specific Folding" section, below).***
#' 
#' Defaults to `NULL`.
#' 
#' Must be natural numbers; must be length `length(onto)`.
#' 
#' @param fromField ***Which field to "fold."***
#' 
#' Defaults to `selectedFields(humdrumR)[1]`.
#' 
#' Must be a `character` string [partially][partialMatching] matching the name of a data field in the `humdrumR` input.
#' For example, `"Tok"` would match the `Token` field.
#' This is the field which is "folded" into a new field.
#'   
#' @param fillFromField ***Should the content of the `fromField` be to copied unfolded sections?***
#' 
#' Defaults to `FALSE` for `cleave()` and `foldStops()`; `TRUE` for `foldPaths()`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' This only comes into play if the folding field is smaller than the `to` field.
#'
#' @param newFieldNames ***Names to use for new fields created by the folding.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be `character`.
#' 
#' @seealso The complement/opposite of `cleave()` is [rend()].
#' The [collapse family of functions][collapseHumdrum()] serves a somewhat
#' similar function to `cleave()`.
#' @family {Humdrum data reshaping functions}
#' @export
cleave <- function(humdrumR, ...,
                   field = selectedFields(humdrumR)[1], 
                   newFieldNames = NULL) {
    # argument checks
    checks(humdrumR, xhumdrumR)
    checks(field, xcharacter & xlen1)

    field <- fieldMatch(humdrumR, field, 'cleave', 'fromField')
    
    
    
    # humtab and fields
    humdrumR <- selectFields(humdrumR, field)
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDd')
    
    # what are we doing?
    c('groupDT', 'what') %<-% cleaveParseGroups(...)
    groupDT <- cleaveMoves(humtab, groupDT, what, newFieldNames)
    

    # 
    fromHits <- humtab[ , list(Piece, get(what)) %ins% groupDT[, c('Piece', 'From'), with = FALSE]]
    fromTable <- humtab[fromHits == TRUE, c(field, 'Exclusive', fields(humdrumR, c('S', 'F', 'R'))$Name), with = FALSE]
    
    if (all(is.na(fromTable[[field]]))) {
        .warn("Your fromField doesn't have any non-null data where {what} %in% {harvard(unique(groupDT$To)}.",
              "Your humdrumR data is being returned unchanged.")
        return(humdrumR)
    }
    
    humtab[[field]][fromHits & humtab$Type == 'D'] <- NA
    #
    whichMatch <- fromTable[ ,  matches(list(Piece, get(what)), groupDT[ , c('Piece', 'From'), with = FALSE], multi = TRUE)]
    
    #
    fromTable <- do.call('rbind', lapply(1:ncol(whichMatch),
           \(j) {
               i <- whichMatch[, j]
               fromTable <- fromTable[!is.na(i),]
               i <- i[!is.na(i)]
               switch(what,
                      Record = fromTable$DataRecord  <- fromTable[ , DataRecord  + (groupDT$To[i] - Record)],
                      DataRecord  = fromTable$Record <- fromTable[ , Record + (groupDT$To[i] - DataRecord)])
               
               fromTable[[what]] <-  groupDT$To[i]
               fromTable$FieldName <- groupDT$FieldName[i]
               fromTable
           }))
   
   
    
    # data fields in old rows need to be renamed, because they will now be columns
   
    fromTables <- split(fromTable, by = 'FieldName', keep.by = FALSE)
    fromTables <- Map(\(ftab, fname) {
                             colnames(ftab)[colnames(ftab) == field] <- fname
                             colnames(ftab)[colnames(ftab) == 'Exclusive'] <- paste0('Exclusive.', fname)
                             ftab

                         }, fromTables, names(fromTables))
 
    newfields <- names(fromTables)
    mergeFields <- fields(humdrumR, c('S', 'F', 'R'))$Name
    humtab <- Reduce(\(htab, ftab) {
        # htab <- ftab[htab, on = mergeFields]
        htab <- rbind(ftab[htab, on = mergeFields],
                      # This is necessary if the from spines have extra paths or stops,
                      ftab[!htab, on = setdiff(mergeFields, 'Type')],
                      # or vice versa
                      # htab[!ftab, on = mergeFields],
                      fill = TRUE)
        # 
        # if (complement) {
            # for (field in newfields) {
                # na <- is.na(htab[[field]])
                # hits <- na & htab$Spine %in% unique(htab$Spine[!na])
                # htab[[field]][na] <- htab[[field]][na]
            # }
        # }
        # htab
        
    }, fromTables, init = humtab)
 
    
    humtab <- update_humdrumR(humtab, field = c(newfields, fields(humdrumR, 'D')$Name))
    humtab <- removeNull(humtab, by = c('Piece', what), nullTypes = 'LIMd')
    humtab <- update_Dd(humtab, field = newfields)
    
    putHumtab(humdrumR, overwriteEmpty = c()) <- orderHumtab(humtab)
    
    humdrumR <- updateFields(humdrumR) 
    
    selectFields(humdrumR, c(field, newfields))
    
    

}

# pivotHumdrum <- function(humdrumR, fold,  onto, what = 'Spine', Piece = NULL, 
#                         fromField = selectedFields(humdrumR)[1], fillFromField = FALSE,
#                         newFieldNames = NULL) {
#     
#     # argument checks
#     checks(humdrumR, xhumdrumR)
#     checks(fold, xnatural)
#     checks(onto, xnatural)
#     
#     checks(fromField, xcharacter & xlen1)
#     fromField <- fieldMatch(humdrumR, fromField, 'cleave', 'fromField')
#     checks(what, xcharacter & xlen1 & xlegal(c('Spine', 'Path', 'Stop', 'Record', 'DataRecord')))
#    
#     
#     humtab <- data.table::copy(getHumtab(humdrumR))
#     moves <- cleaveMoves(humtab, fold, onto, what, Piece, newFieldNames)
#     humtab[ , "_pivot_" := {
#         pivot <- rep(fromField, length(Token))
#         pivot[Spine %in% moves$From] <- moves$FieldNames[match(Spine[Spine %in% moves$From], moves$From)]
#         pivot
#     }]
#     
#     humtab[, Spine := ifelse(Spine %in% moves$From, moves$To[match(Spine, moves$From)], Spine)]
#     
#     humtab <- as.data.table(tidyr::pivot_wider(humtab, names_from = '_pivot_', values_from = Token, values_fill = NA))
#     
#     humtab <- renumberSpines.data.table(humtab)
#     
#     
#     putHumtab(humdrumR) <- humtab
#     
#     updateFields(humdrumR)   
# }


cleaveParseGroups <- function(...) {
  groups <- list(...)
  if (length(groups) == 0L) .stop("The cleave() function requires at least one group of structural locations to cleave together.")
  
  ## what
  what <- local({
    what <- .names(groups)
    what[what == ''] <- 'Spine'
    what <- unique(what)
    options <- c('Spine', 'Path', 'Stop', 'Record', 'DataRecord')
    
    matches <- unique(pmatch(what, options, duplicates.ok = TRUE))
    if (any(is.na(matches))) {
      .stop("In a call to cleave(), your arguments must (partially) match one of the following field names:",
            "{harvard(options, 'or', quote = TRUE)}.", 
            "{harvard(what[is.na(matches)], 'and', quote = TRUE)} <do|does> not match.", 
            ifelse = sum(is.na(matches)) > 1L)
    }
    what <- options[matches]
    if (length(what) > 1L) .stop("The cleave() function can only cleave one type of location at a time.",
                                 "Your argument names (or lack of them) are indicating location fields {harvard(what, 'AND', quote = TRUE)}.")
    what
  })
  
  ## targets
  lists <- sapply(groups, is.list)
  if (any(lists)) {
    if (any(!lists)) .stop("In a call to cleave(), Piece-wise cleave groups must be indicated with lists of groups,", 
                           "one per piece. In your call, {num2print(sum(lists))} of your grouping arguments is",
                           "a list, but the other {num2print(sum(!lists))} <aren't|isn't>;",
                           "They must be ALL Piece-wise (lists) (or have no Piece-wise arguments).",
                           ifelse = sum(!lists) > 1L)
    
    classes <- unique(unlist(lapply(groups, \(group) unlist(lapply(group, class)))))
    groups <- lapply(groups, \(group) lapply(group, unique))
    lengths <- unlist(lapply(groups, lengths))
    
    groupDT <- cleaveParseGroups_list(groups)
  } else {
    classes <- sapply(groups, class)
    
    groups <- lapply(groups, unique)
    lengths <- lengths(groups)
    
    to   <- unlist(lapply(groups, '[', i = 1L))
    from <- lapply(groups, '[', i = -1L)
    to <- rep(to, lengths(from))
    
    groupDT <- data.table(From = unlist(from), To = to)
    setorder(groupDT, To, From)
  }
  
  if (any(lengths == 1L)) .stop("In a call to cleave(), groups to cleave must have more than one unique target {what} to cleave together.",
                                "{num2print(sum(lengths == 1L), capitalize = TRUE)} of the groups you have provided <are|is> length 1.",
                                ifelse = sum(lengths == 1L) > 1L)
  
  if (any(!classes %in% c('character', 'numeric', 'integer'))) {
    .stop("When using cleave(), the groups to cleave must be indicated with character strings",
          "(exclusive interpretatios) or natural numbers (spines).",
          "You have provided<| a> {harvard(setdiff(classes, c('character', 'numeric', 'integer')), 'and', quote = TRUE)} argument<s|>.",
          ifelse = sum(!classes %in% c('character', 'numeric', 'integer')) > 1L)
  }
  
  if (!(all(classes == 'character') || all(classes %in% c('numeric', 'integer')))) {
    .stop("When using cleave(), all cleave grouping arguments must be the same class.",
          "They must either ALL be 'character' (exclusive interpretations) or ALL be natural numbers (spines).")
  } 
  
  if (classes[1] == 'character') {
    groupDT[ , c('From', 'To') := lapply(list(From, To), gsub, pattern = '^\\*\\**', replacement = '')]
    groupDT <- unique(groupDT)
  }
  
  
  
  list(groupDT = groupDT, what = what)
  
}


cleaveParseGroups_list <- function(groups) {
  lens <- lengths(groups)
  if (length(unique(lens)) > 1L) .stop("Using cleave(), Piece-wise (list) group arguments must all be the same length.")
  
  names <- lapply(groups, .names)
  if (length(groups) > 1L && any(!Reduce('==', names))) .stop("Using cleave(), Piece-wise list grouping arguments must all have identical names, or no names.")
  
  unnamed <- names[[1]] == ''
  if (!(all(unnamed) | all(!unnamed))) .stop("Using cleave(), Piece-wise list grouping arguments must have no names, or ",
                                             "have EVERY index named.",
                                             "Your call includes {num2print(sum(unnamed))} unnamed indices and {num2print(sum(!unnamed))} named indices.")
  if (!all(unnamed) && any(!grepl('[1-9][0-9]*', names[[1]]))) .stop("Using cleave(), the names of Piece-wise (list) grouping arguments must",
                                                    "either be absent (no names) or by positive whole numbers.",
                                                    "Your list includes invalid names like",
                                                    "{harvard(grep('[1-9][0-9]*', value = TRUE, invert = TRUE, head(unique(names[[1]]), 5)), 'and', quote = TRUE)}.")
  
  piece <- if (all(unnamed)) {
    seq_along(groups[[1]])
  } else {
    as.integer(names[[1]])
  }
  
  dt <- data.table::rbindlist(lapply(groups, 
                                     \(group) {
                                       from <- lapply(group, '[', i = -1L)
                                       to   <- unlist(lapply(group, '[', i = 1L))
                                       
                                       to <- rep(to, lengths(from))
                                       piece <- rep(piece, lengths(from))
                                       data.table(From = unlist(from), To = to, Piece = piece)
                                       
                                       
                                     }))
  
  setorder(dt, Piece, To, From)
}

cleaveMoves <- function(humtab, groupDT, what, newFieldNames = NULL) {
    
    if (is.null(groupDT$Piece)) {
      piece <- rep(unique(humtab$Piece), each = nrow(groupDT))
      groupDT <- groupDT[rep(1:nrow(groupDT), length(unique(humtab$Piece)))]
      groupDT[ , Piece := piece]
    }    
    
    
    if (class(groupDT$From) == 'character') {
      # this turns exclusives into integers, but also creates fieldNames
      groupDT <- cleaveParseExclusives(humtab, groupDT, newFieldNames)
    } else {
      # name fields
      groupDT[ , newFieldN := seq_along(From), by = .(Piece, To)]
      NnewFields <- length(unique(groupDT$newFieldN))
      
      if (is.null(newFieldNames)) {
         groupDT[ , FieldNames := paste0(what, paste(unique(From), collapse = '|')), by = newFieldN]
        
      } else {
        newFieldNames <- make.unique(rep(newFieldNames, length.out = max(groupDT$newFieldN)), sep = '')
        groupDT[ , FieldName := newFieldNames[newFieldN]]
      }
    }
    
    groupDT$From <- as.integer(groupDT$From) 
    groupDT$To <- as.integer(groupDT$To) 
    
    groupDT
}

cleaveParseExclusives <- function(humtab, groupDT, newFieldNames = NULL) {
  uniqueExclusives <- union(groupDT$From, groupDT$To) # solely used for error message below

  
  groupDT <- humtab[ , {
    curPiece <- Piece
    To <- groupDT[Piece == curPiece, unique(To)]
    From <- groupDT[Piece == curPiece, unique(From)]
    
    toSpine <- setdiff(unique(Spine[Exclusive == To]), NA)
    if (length(toSpine)) {
      do.call('rbind', lapply(From, 
                              \(fromExclusive) {
                                fromSpine <- setdiff(unique(Spine[Exclusive == fromExclusive]), NA)
                                if (length(fromSpine) == 0L) return(data.table(From = integer(0), 
                                                                               To = integer(0), 
                                                                               Exclusive = character(0)))
                                if (!(length(fromSpine) == 1L && length(toSpine) == 1L && fromSpine == toSpine)) {
                                  
                                  if (length(fromSpine) == length(toSpine) && all(fromSpine == toSpine)) {
                                    fromSpine <- fromSpine[-1]
                                    toSpine <- toSpine[1]
                                  }   
                                  as.data.table(match_size(From = fromSpine, To = toSpine,  Exclusive = fromExclusive))
                                }  
                                
                              })) -> groupDT_piece
      # if one exclusive is collapsing onto itself (like kern -> kern),
      # other spines collapsing onto the collapsed one will result in overlaps
      # there's no way to see this before now
      overlaps <- groupDT_piece[ , To %in% From]
      if (any(overlaps)) {
        groupDT_piece$To[overlaps] <- groupDT_piece[overlaps == FALSE][groupDT_piece[overlaps == TRUE], on ='Exclusive']$To
        groupDT_piece <- groupDT_piece[!duplicated(groupDT_piece)]
      }
      groupDT_piece
    }
  }, by = Piece]
  
  if (nrow(groupDT) == 0L) {
    uniqueExclusives <- gsub('^\\**', '**', uniqueExclusives)
    .stop("cleave() has found no matching files with which contain any combinations of the exclusive interpretations",
          "{harvard(uniqueExclusives, 'and', quote = TRUE)}.")

  }
  groupDT <- groupDT[, list(From, newFieldN = seq_along(From)), by = .(Piece, To, Exclusive)]
  groupDT[ , FieldName := stringr::str_to_sentence(paste0(Exclusive, if (any(newFieldN > 1)) newFieldN)), by = Exclusive]
  groupDT[ , newFieldN := match(FieldName, unique(FieldName))]
  
  if (!is.null(newFieldNames)) {
    newFieldNames <- make.unique(rep(newFieldNames, length.out = max(groupDT$newFieldN)), sep = '')
    groupDT[ , FieldName := newFieldNames[newFieldN]]
  } 
 
  groupDT
}
#    


### Predefined cleaves ----

#' @rdname cleave
#' @export
cleaveSpines <- cleave

#' @rdname cleave
#' @export
cleavePaths <- function(humdrumR, field = selectedFields(humdrumR)[1]) {
    
    paths <- sort(unique(getHumtab(humdrumR)$Path))
    paths <- paths[!is.na(paths)]
    
    if (all(paths == 0L)) return(humdrumR)
    
    dataFields <- fields(humdrumR, fieldTypes = 'Data')

    
    cleave(humdrumR, Path = paths, field = field,
              newFieldNames = paste0(field, '_Path', paths[-1]))
    

    
}

#' @rdname cleave
#' @export
cleaveStops <- function(humdrumR, fromField = selectedFields(humdrumR)[1], fillFromField = FALSE) {
    checks(humdrumR, xhumdrumR)
           
   stops <- unique(getHumtab(humdrumR)$Stop)
   stops <- stops[!is.na(stops)] 
   
   if (all(stops == 1L)) return(humdrumR)
   
   dataFields <- fields(humdrumR, fieldTypes = 'Data')
   
   
   cleave(humdrumR, Stop = stops, field = field,
          newFieldNames = paste0(field, '_Stop', paths[-1]))
   
   
}


#' "Fold" grace notes into neighbos
#' 
#' 
#' @seealso `foldGraceNotes` makes use of the more general [cleave()].
#' @export
cleaveGraceNotes <- function(humdrumR) {
    warn("foldGraceNotes has not been implemented yet!")
    humdrumR
}

## rend ----


#' Separate data fields into new spines, paths, or stops.
#' 
#' @export
#' @seealso The complement/opposite of `rend()` is [cleave()].
#' @family {Humdrum data reshaping functions}
rend <- function(humdrumR, field = 'Kern') {
    humtab <- getHumtab(humdrumR, 'IMDd')
    
    spines <- humtab[, {
      list(list(if (any(!is.na(Kern))) c(Token = 1, Kern = 1) else c(Token = 1)))
    }, by = Spine]
    spines <- spines[ , list(oldSpine = rep(Spine, lengths(V1)), newSpine = cumsum(unlist(V1)))]
    
    old <- data.table::copy(humtab)
    new <- data.table::copy(humtab)
    
    
    old[ , Kern := NULL]
    new[ , Token := NULL]
    colnames(new)[colnames(new) == 'Kern'] <- 'Token'
    
    old[ , Spine := spines[!duplicated(oldSpine), newSpine[match(Spine, oldSpine)]]]
    new[ , Spine := spines[ duplicated(oldSpine), newSpine[match(Spine, oldSpine)]]]
    
    
    embeddedExclusive <- getExclusive(new$Token)
    
    new$Token <- as.character(new$Token)
    new$Token[is.na(new$Token)] <- old$Token[is.na(new$Token)]
    if (!is.null(embeddedExclusive)) {
        new$Token[grepl('^\\*\\*', new$Token)] <- paste0('**', embeddedExclusive)
        new$Exclusive <- embeddedExclusive
    }
    
    humtab <- rbind(old, new)
    
    putHumtab(humdrumR) <- humtab
    humdrumR <- updateFields(humdrumR)
    selectFields(humdrumR, 'Token')
    
    
    
    
}



#' "Unfold" data into multiple stops
#' 
#' If some record/spine/path locations have different numbers of
#' stops in different fields, this function spreads the data from the 
#' smaller fields into multiple stops.
#' 
#' @seealso The opposite (kinda) of [foldStops()]
#' @export
unfoldStops <- function(humdrumR, fromFields = fields(humdrumR, 'D')$Name) {
  checks(humdrumR, xhumdrumR)
  if (!anyStops(humdrumR)) return(humdrumR)
  checks(fromFields, xcharacter & xlen0)
  
  #
  humtab <- getHumtab(humdrumR, 'D')
  
  multistopRecords <- humtab[ , list(Record = unique(Record)[rowSums(table(Record,Stop)) > 1]), by = Piece]
  multiHumtab <- humtab[multistopRecords, on = c('Record', 'Piece')]
  fromFields <- fromFields[multiHumtab[, sapply(fromFields, \(field) any(is.na(get(field))))]]
  for (field in fromFields) {
    
    multiHumtab[, eval(field) := rep_len(get(field)[!is.na(get(field))], length(Token)), by = list(Piece, Record)]   
  }
  humtab <- orderHumtab(rbind(multiHumtab, humtab[!multistopRecords, on = c('Record', 'Piece')]))
  humtab <- update_Dd(humtab, field = fromFields)
  putHumtab(humdrumR, overwriteEmpty = c()) <- humtab
  humdrumR
}
