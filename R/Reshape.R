
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
#' @family {Humdrum table reshaping functions}
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
#' Must be `character` strings [partially][partialMatching] matching the name(s) of a [field(s)][fields()] in the `humdrumR` input.
#'
#' Data in the `fields` will be collapsed within these fields.
#' 
#' @param fields ***The target field(s) in the `humdrumR` data to collapse.***
#' 
#' Defaults to `selectedFields(humdrumR)`.
#' 
#' Must be `character` strings [partially][partialMatching] matching the name(s) of a data [field(s)][fields()] in the `humdrumR` input.
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
#' @family {Humdrum table reshaping functions}
#' @seealso The humdrum [folding functions][cleave()] serve a similar function,
#' "folding" data into *new* fields, rather than collapsing it within a field.
#' @export
collapseHumdrum <- function(humdrumR, by,
                            fields = selectedFields(humdrumR), 
                            dataTypes = 'GLIMDd', 
                            collapseAtomic = TRUE, sep = ' ') {
 
    checks(humdrumR, xhumdrumR)
    by <- fieldMatch(humdrumR, by, 'collapseHumdrum', 'by')
    fields <- fieldMatch(humdrumR, fields, 'collapseHumdrum', 'fields')
    dataTypes <- checkTypes(dataTypes, 'collapseHumdrum', argname = 'dataTypes')
    checks(collapseAtomic, xTF)
    checks(sep, xcharacter & xlen1)
    
    humtab <- getHumtab(humdrumR, dataTypes)
    
    humtab <- collapseHumtab(humtab, by = by, fields = fields, sep = sep, collapseAtomic = collapseAtomic)
    
    putHumtab(humdrumR, dataTypes) <- humtab
    
    humdrumR
    
}


collapseHumtab <- function(humtab, by, target = humtab, fields, collapseAtomic = TRUE, sep = ' ') {
    if (nrow(target) == 0L) return(humtab)
    target <- humtab[unique(target[ , by , with = FALSE]), on = by]
  
    for (fieldname in fields) {
      collapser <- switch(paste0(is.list(humtab[[fieldname]]), collapseAtomic),
                          TRUETRUE   = \(x) .paste(unlist(x), collapse = sep),
                          TRUEFALSE  = \(x) list(list(unlist(x, recursive = FALSE))), # there could be bugs here if we collapse data that is objects.
                          FALSETRUE  = \(x) .paste(x, collapse = sep),
                          FALSEFALSE = \(x) list(list(x)))
      
      
      collapsed <- rlang::eval_tidy(rlang::expr(target[ , collapser(!!rlang::sym(fieldname)), by = by]))
      humtab <- humtab[ , .SD[1], by = by]
      
      collapsed <- collapsed[humtab, on = by, V1]
      field <- humtab[[fieldname]]
      if (collapseAtomic) {
        replacements <- !is.na(collapsed) 
        
        if (is.list(field)) field <- sapply(field, collapser)
        
      } else {
        replacements <- !sapply(collapsed, is.null)
      }
      field[replacements] <- collapsed[replacements]
      humtab[[fieldname]] <- field
      humtab$Type[replacements] <- 'D'
    }
    
    
    humtab
    
}


#' @rdname collapseHumdrum
#' @export 
collapseStops <- function(humdrumR, fields = selectedFields(humdrumR), collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    fields <- fieldMatch(humdrumR, fields, 'collapseStops', 'fields')
    checks(collapseAtomic, xTF)
    checks(sep, xcharacter & xlen1)
    
    
    humtab <- getHumtab(humdrumR, 'D')
    humtab <- collapseHumtab(humtab, by = c('Piece', 'Spine', 'Path', 'Record'),
                             target = humtab[Stop > 1L & !is.na(Stop)],
                             fields = fields,
                             collapseAtomic = collapseAtomic, sep = sep)
    
    putHumtab(humdrumR, overwriteEmpty = 'D') <- humtab
    removeEmptyStops(humdrumR)

}



#' @rdname collapseHumdrum
#' @export
collapsePaths <- function(humdrumR, fields = selectedFields(humdrumR), collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    checks(collapseAtomic, xTF)
    fields <- fieldMatch(humdrumR, fields, 'collapsePaths', 'fields')
    checks(sep, xcharacter & xlen1)
    
    humtab <- getHumtab(humdrumR)
    humtab <- collapseHumtab(humtab, by = c('Piece', 'Spine', 'Record'),
                             target = humtab[Path > 0L & !is.na(Path)],
                             fields = fields,
                             collapseAtomic = collapseAtomic, sep = sep)
    
    putHumtab(humdrumR) <- humtab
    humdrumR
    
}

#' @rdname collapseHumdrum
#' @export
collapseRecords <- function(humdrumR, fields = selectedFields(humdrumR), collapseAtomic = TRUE, sep = ' ') {
    checks(humdrumR, xhumdrumR)
    checks(collapseAtomic, xTF)
    fields <- fieldMatch(humdrumR, fields, 'collapseRecords', 'fields')
    checks(sep, xcharacter & xlen1)
    
    collapseHumdrum(humdrumR, dataTypes = 'GLIMDd', 
                    by = c('Piece', 'Record'),
                    fields = fields,
                    collapseAtomic = collapseAtomic, sep = sep)

}



# Cleave and Rend ----

## cleave ----

#' Align data from separate spines into new fields.
#' 
#' Cleave, as in "to cleave together," moves data from separate spines (or paths) into 
#' new fields in the *same* spine(s).
#' Under the hood, `cleave()` essentially runs a specialized call to make the [humdrum table][humTable]
#' "wider," similar to R functions like [cast()][reshape2], [spread()][tidyr], or [pivot_wider()][tidyr].
#' In fact, a humdrumR method for [pivot_wider()][tidyr] is defined, which is equivalent to `cleave()`.
#' The `cleave()` function is essentially the inverse of [rend()].
#' 
#' @details
#' 
#' Many humdrum datasets encode data across multiple spines, spine-paths, or stops.
#' By default, `humdrumR` parses each separate spine, spine-path, and stop as their own individual
#' data points, taking up one row in the [humdrum table][humTable].
#' If we want to treat data in multiple spines/paths/stops as different aspects of the same data
#' it is easiest to reshape the data so that the information is in different humdrumR [fields][fields()]
#' rather than separate spines/paths/stops.
#' In the humdrum syntax view, the spines (or path/stops) are moved "on top" of each other, cleaving them
#' together.
#' 
#' The convenient `cleaveSpines()`, `cleaveStops()`, and `cleavePaths()` functions automatically cleave *all* stops/paths in a dataset 
#' onto the first spine/stop/path, creating new fields named, e.g., `Path1`, `Path2`, etc.
#' 
#' 
#' ## Syntax
#' 
#' The `cleave()` function takes any number of `...` arguments specifying *groups* of spines/paths/stops
#' to cleave together. 
#' 
#' + `cleave(humData, Spine = 1:2)` will cleave the first and second spine.
#' + `cleave(humData, Spine = 1:4)` will cleave the first four spines.
#' + `cleave(humData, Spine = 1:2, Spine = 3:4)` will cleave spine 1 with spine 2, and separately, spine 3 with spine 4.
#' 
#' The default is to cleave spines, so you can actually omit the `Spine = ` part: e.g., `cleave(humData, 1:2)` is the same
#' as `cleave(humData, Spine = 1:2)`.
#' If you want to cleave spine paths, you'll need to explicitly call something like `cleave(humData, Path = 0:1)`.
#' The *first* element in each group is used as the original location which all other spines/paths/stops are cleaved into.
#' The ordering of the remaining elements is irrelevant.
#' 
#' ### Piece-specific cleaving
#'
#' By default, the same cleaving will be applied in all pieces (if the pieces *have* the target spines/paths/stops).
#' However, you can use an alternate argument structure to apply differerent cleaves to different pieces.
#' To do so, provide groups to cleave arguments in a list, with each element in the list representing cleave group
#' in one piece.
#' If the listed groups are unnamed, the groups are mapped to pieces by index.
#' For example, the call `cleave(humData, list(1:2, 2:3, 3:4, NULL, 1:3))` will result in the following cleaves:
#' 
#' + In piece 1, cleave spines 1 and 2.
#' + In piece 2, cleave spines 2 and 3.
#' + In piece 3, cleave spines 3 and 4.
#' + In piece 4, no cleave (no changes).
#' + In piece 5, cleave the first three spines.
#' + In any remaining pieces (6 or greater), no cleaves.
#' 
#' Alternatively, you can name the list elements with integers corresponding to pieces.
#' For example, `cleave(humData, Path = list("1" = 0:1, 5 = 0:2"))` will cleave paths 0 and 1 in
#' piece 1, and paths `0:2` in piece 5.
#' 
#' 
#' ### Exclusive interpretations
#' 
#' When cleaving spines, you can specify spines using character strings representing exclusive interpretations.
#' So you can call `cleave(humData, c('kern', 'silbe'))`, which will cause `**kern` and `**silbe` spines in each piece to
#' be cleaved.
#' Note that any exclusive interpretations which aren't "mentioned" in the call remain in their original field.
#' 
#' The behavior of exclusive cleaving depends on the relative number of each target exclusive interpretation in each piece.
#' If there are equal numbers of each interpretation, the spines are grouped in parallel.
#' For example, if a piece has the spines `**kern **silbe **kern **silbe`, the command `cleave(humData, c('kern', 'silbe'))`
#' will see that there are two `**kern`/`**silbe` pairs, and cleave them just like `cleave(humData, 1:2, 3:4)`.
#' 
#' If there are different numbers of spines matching each exclusive interpretation, the cleave behavior depends on which
#' field is the first field in your input argument---we'll call that the "target" exclusive.
#' Consider a file with spines `**kern **kern **harm`.
#' If we specify a cleave call `cleave(humData, c('kern', 'harm'))` that means we want `**kern` to be the "target" exclusive.
#' Since there are fewer `**harm` spines than `**kern` spines, the data in the `**harm` spine will be *duplicated*, can cleaved
#' to **both** `**kern` spines in parallel.
#' If we instead call `cleave(humData, c('harm', 'kern'))`, making `**harm` the "target", the two `**kern` spines will both be "piled" atop
#' the single `**harm` spine, making two new `**kern` fields.
#' 
#' 
#' @section Fields:
#' 
#' Cleaving can (for now) only be applied to one field in our data, which defaults to the first [selected field][selectedFields];
#' You can change the target field with the `field` argument.
#' 
#' Cleaving will always introduce new fields into your data.
#' The first spine/path/stop in each cleave group is left in it's original (target) field.
#' Other spine/path/stops will be put into new fields.
#' So if you call `humData |> select(Token) |> cleave(humData, 1:2)`, spine 1 will remain in the `Token`
#' field and spine 2 data will be put in a new field.
#' By default, the new field(s) will be automatically named by appending the type of cleave (spine vs path vs stop)
#' to the number.
#' In the `cleave(humData, 1:2)` case, the new field will be called `Spine2`.
#' You can control the name with the `newFields` argument, which must be a `character` vector.
#' You can provide as many new field names as there will be new fields.
#' If you provide too few field names, your name(s) will have numbers appended as necceasary to cover all the new fields;
#' If you provide too many field names, the extra names will simply be ignored.
#' 
#' When cleaving by exclusive interpretation `newFields` can be used in the exact same way.
#' However, by default (if `newFields` is `NULL`), `cleave()` will names fields by their exclusive interpretation.
#' Note that the original target field (specified by the `field`) argument will not have it's name changed.
#' So for example, `humData |> select(Token) |> cleave(humData, c('kern', 'silbe'))` will result in the spines `Token`
#' and `Silbe`. 
#' No `Kern` field is created, because the `Kern` data is left in the `Token` field.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***What to cleave?***
#' 
#' Must be natural numbers, `character` strings representing exclusive interpretations, or lists of either.
#' 
#' @param field ***Which field cleave data from.***
#' 
#' Defaults to first [selected field][selectedFields]. 
#' 
#' Must be a `character` string [partially][partialMatching] matching the name of a data field in the `humdrumR` input.
#' For example, `"Tok"` would match the `Token` field.
#'   
#' @param newFields ***Names to use for new fields created by the cleave.***
#' 
#' By default generates names by structure/number (like `Spine2`) or exclusive interpretation (like `Silbe`).
#' 
#' Must be non-empty `character` string.
#' 
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/MozartVariations/.*.krn")
#' 
#' 
#' humData |> cleave(3:4)
#' humData |> cleave(Path = 0:1, newFields = 'Ossia')
#' humData |> cleavePaths()
#'
#' humData |> cleave(c('function', 'harm'), newFields = 'Harmony')
#' 
#' humData |> cleave(c('kern', 'function', 'harm'))
#' 
#' @seealso {The complement/opposite of `cleave()` is [rend()].
#'           The [collapse family of functions][collapseHumdrum()] serves a somewhat
#'            similar function to `cleave()`.}
#' @family {Humdrum table reshaping functions}
#' @family {Humdrum table pivoting functions}
#' @export
cleave <- function(humdrumR, ...,
                   field = selectedFields(humdrumR)[1], 
                   newFields = NULL) {
    # argument checks
    checks(humdrumR, xhumdrumR)
    checks(field, xcharacter & xlen1)
    checks(newFields, xnull | (xcharnotempty & xlen))

    field <- fieldMatch(humdrumR, field, 'cleave', 'field')
    
    
    
    # humtab and fields
    humdrumR <- selectFields(humdrumR, field)
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDd')
    
    # what are we doing?
    c('groupDT', 'what') %<-% cleaveParseGroups(...)
    groupDT <- cleaveMoves(humtab, groupDT, what, newFields)
    

    # 
    fromHits <- humtab[ , list(Piece, get(what)) %ins% groupDT[, c('Piece', 'From'), with = FALSE]]
    fromTable <- humtab[fromHits == TRUE, c(field, 'Exclusive', fields(humdrumR, c('S', 'F', 'R'))$Name), with = FALSE]
    
    if (all(is.na(fromTable[[field]]))) {
        .warn("Your fromField doesn't have any non-null data where {what} %in% {.show_vector(unique(groupDT$To))}.",
              "Your humdrumR data is being returned unchanged.")
        return(humdrumR)
    }
    
    humtab[[field]][fromHits & humtab$Type == 'D'] <- NA # the spine/whatever isn't removed if a DIFFERENT field is non-null, so need to set it null.
    humtab$Type[fromHits & humtab$Type == 'D'] <- 'd'
    # humtab <- humtab[fromHits == FALSE]
    #
    whichMatch <- fromTable[ ,  matches(list(Piece, get(what)), groupDT[ , c('Piece', 'From'), with = FALSE], multi = TRUE)]
    
    # change numbers (i.e., change spine numbers)
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
    mergeFields <- setdiff(fields(humdrumR, c('S', 'F', 'R'))$Name,'Type')
    humtab <- Reduce(\(htab, ftab) {
        # htab <- ftab[htab, on = mergeFields]
        htab <- rbind(ftab[htab, on = mergeFields],
                      # This is necessary if the from spines have extra paths or stops,
                      ftab[!htab, on = mergeFields],
                      # or vice versa
                      # htab[!ftab, on = mergeFields],
                      fill = TRUE)
        htab[ , Type := ifelse(is.na(Type), i.Type, Type)]
        htab[ , i.Type := NULL]
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
    humtab <- removeNull(humtab, by = c('Piece', what), nullTypes = 'LIMd') # this is where empty spines are removed, if ALL fields are empty.
    humtab <- update_Dd(humtab, field = newfields)
    
    putHumtab(humdrumR, overwriteEmpty = c()) <- orderHumtab(humtab)
    
    humdrumR <- updateFields(humdrumR) 
    
    selectFields(humdrumR, c(field, newfields))
    
    

}

# pivotHumdrum <- function(humdrumR, fold,  onto, what = 'Spine', Piece = NULL, 
#                         fromField = selectedFields(humdrumR)[1], fillFromField = FALSE,
#                         newFields = NULL) {
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
#     moves <- cleaveMoves(humtab, fold, onto, what, Piece, newFields)
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
  groups <- lapply(groups, \(group) ifelse(lengths(group) == 0L, list(integer(0L)), group))
  
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
  

  
  
  dt <- data.table::rbindlist(lapply(groups, 
                                     \(group) {
                                       piece <-  if (is.null(names(group))) which(lengths(group) > 0L) else as.integer(names(group))
                                       group <- group[piece]
                                       
                                       from <- lapply(group, '[', i = -1L)
                                       to   <- sapply(group, '[', i = 1L)
                                       
                                       to <- rep(to, lengths(from))
                                       piece <- rep(piece, lengths(from))
                                       data.table(From = unlist(from), To = to, Piece = piece)
                                       
                                       
                                     }))
  
  setorder(dt, Piece, To, From)
}

cleaveMoves <- function(humtab, groupDT, what, newFields = NULL) {
    
    if (is.null(groupDT$Piece)) {
      piece <- rep(unique(humtab$Piece), each = nrow(groupDT))
      groupDT <- groupDT[rep(1:nrow(groupDT), length(unique(humtab$Piece)))]
      groupDT[ , Piece := piece]
    }    
    
    
    if (class(groupDT$From) == 'character') {
      # this turns exclusives into integers, but also creates fieldNames
      groupDT <- cleaveParseExclusives(humtab, groupDT, newFields)
    } else {
      # name fields
      groupDT[ , newFieldN := seq_along(From), by = .(Piece, To)]
      NnewFields <- length(unique(groupDT$newFieldN))
      
      if (is.null(newFields)) {
         groupDT[ , FieldNames := paste0(what, paste(unique(From), collapse = '|')), by = newFieldN]
        
      } else {
        newFields <- make.unique(rep(newFields, length.out = max(groupDT$newFieldN)), sep = '')
        groupDT[ , FieldName := newFields[newFieldN]]
      }
    }
    
    groupDT$From <- as.integer(groupDT$From) 
    groupDT$To <- as.integer(groupDT$To) 
    
    groupDT
}

cleaveParseExclusives <- function(humtab, groupDT, newFields = NULL) {
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
  
  if (!is.null(newFields)) {
    newFields <- make.unique(rep(newFields, length.out = max(groupDT$newFieldN)), sep = '')
    groupDT[ , FieldName := newFields[newFieldN]]
  } 
 
  groupDT
}
    
#' @rdname cleave
#' @export
pivot_wider.humdrumR <- function(data, names_from = 'Spine', values_from = selectedFields(data)[1]) {
  checks(names_from, xcharacter & xlen1)
  checks(values_from, xcharacter & xlen1)
  
  names_from <- fieldMatch(data, names_from, 'pivot_wider.humdrumR()', argname = 'names_from')
  values_from <- fieldMatch(data, values_from, 'pivot_wider.humdrumR()', argname = 'values_from')
  
  which <- sort(unique(pullFields(data, names_from, 'GLIMDd', drop = TRUE, null = 'asis')))
  
  args <- list(which, humdrumR = data, field = values_from)
  names(args)[1] <- names_from
  
  do.call('cleave', args)
}


### Predefined cleaves ----

#' @rdname cleave
#' @export
cleaveSpines <- function(humdrumR, field = selectedFields(humdrumR)[1]) {
  spines <- sort(unique(getHumtab(humdrumR)$Spine))
  spines <- spines[!is.na(spines)]
  
  if (all(spines == 0L)) {
    .warn('No pieces with multiple spines to cleave.')
    return(humdrumR)
  }
  
  cleave(humdrumR, Spine = spines, field = field)
}

#' @rdname cleave
#' @export
cleavePaths <- function(humdrumR, field = selectedFields(humdrumR)[1]) {
    
    paths <- sort(unique(getHumtab(humdrumR)$Path))
    paths <- paths[!is.na(paths)]
    
    if (all(paths == 0L)) {
      .warn('No paths to cleave.')
      return(humdrumR)
    }
    
    cleave(humdrumR, Path = paths, field = field)
    
}

#' @rdname cleave
#' @export
cleaveStops <- function(humdrumR, field = selectedFields(humdrumR)[1]) {
   checks(humdrumR, xhumdrumR)
           
   stops <- unique(getHumtab(humdrumR)$Stop)
   stops <- stops[!is.na(stops)] 
   
   if (length(unique(stops)) == 1L) {
     .warn('No multi-stops to cleave.')
     return(humdrumR)
   } 

   cleave(humdrumR, Stop = stops, field = field)
   
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


#' Separate data fields into new spines.
#' 
#' Rend, as in "to rend apart," splits data in separate fields into separate spines or paths.
#' Under the hood, `rend()` essentially runs a specialized call to make the [humdrum table][humTable]
#' "longer"/"taller," similar to R functions like [melt()][reshape2], [gather()][tidyr], or [pivot_longer()][tidyr].
#' In fact, a humdrumR method for [pivot_longer()][tidyr] is defined, which is equivalent to `rend()`.
#' The `rend()` function is essentially the inverse of [cleave()].
#' 
#' @details
#' 
#' The `rend()` function takes any number of `...` arguments to [select][selectedFields] fields in the `humdrumR` data.
#' The identified fields are then split into new spines.
#' If no fields are provided, the data's [selected fields][selectedFields] are rended.
#' New spines are generated from existing spines; if we start with spines 1, 2, 3, and rend *two* fields...
#' 
#' + the original spine 1 will be rended into new spines 1 and 2;
#' + the original spine 2 will be rended into new spines 3 and 4;
#' + the original spine 3 will be rended into new spines 5 and 6.
#'  
#' However, by default, spines are only rended if they contain non-null data points
#' in the target fields.
#' If for example, the original spine 2 had no non-null data in one of the rended fields, 
#' if would not be rended into two spines.
#' However, if `rendEmpty` is set to `TRUE`, 
#' *all* spines will be rended even if empty (all null data).
#' 
#' Note that, since different fields may be different data types, `rend()` will generally coerce the result to `character`.
#' 
#' ### Fields
#' 
#' When you rend fields, a new field is generated.
#' The name of the new field is specified by `newField`---by default, `newField` is `NULL` and the names of the rended
#' fields are simply pasted together.
#' If `removeRended = TRUE` (the default), the original fields are removed from the data.
#' However, certain fields, like `Token` and any [structural fields][humTable] cannot be removed from the data.
#' Therefore, if you rend these fields, they will not be deleted, even if `removeRended = TRUE`.
#' 
#' If you only provide one field name to rend, is automatically take to be `Token`.
#' Thus, `rend(humData, 'Solfa')` is equivalent to `rend(humData, 'Token', 'Solfa')`.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Which fields to rend?***
#' 
#' These arguments can be any combination of `character` strings, numbers, or symbols used
#' to match fields in the `humdrumR` input using [tidyverse][dplyr::select()] semantics.
#' See the [select()][selectedFields] docs for details.
#' 
#' @param fieldName ***A name for the newly rended field.***
#'
#' Defaults to pasting the names of selected fields (`...`) together, separated by `.`.
#' 
#' Must be either `NULL`, or a single non-empty `character` string.
#' 
#' @param removeRended ***Should rended fields be removed from the output?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param rendEmpty ***Empty spines be rended?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' 
#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData |> 
#'    mutate(Recip = recip(Token), 
#'           Solfa = solfa(Token, simple = TRUE)) -> humData
#' 
#' humData |> rend(c('Recip', 'Solfa'))
#' 
#' humData |> select(c('Recip', 'Solfa')) |> rend()
#' 
#' @seealso {The complement/opposite of `rend()` is [cleave()].}
#' @family {Humdrum table reshaping functions}
#' @family {Humdrum table pivoting functions}
#' @export
rend <- function(humdrumR, ..., fieldName = NULL, removeRended = TRUE, rendEmpty = FALSE) {
  checks(humdrumR, xhumdrumR)
  checks(fieldName, xnull | (xlen1 & xcharnotempty))
  checks(removeRended, xTF)
  checks(rendEmpty, xTF)
  
  humtab <- getHumtab(humdrumR, 'LIMDd')
  
  exprs <- rlang::enexprs(...)
  fields <- if (length(exprs)) tidyselect_humdrumRfields(humdrumR, exprs, fieldTypes = 'Data', callname = 'rend()') else selectedFields(humdrumR)
  
  if (length(fields) == 1L) {
    if (fields == 'Token') .stop("You haven't provide rend() and fields besides Token. You can't rend Token from itself.")
    fields <- unique(c('Token', fields))
  }
  
  fieldName <- fieldName %||% paste(fields, collapse = '.')
  
  spines <- humtab[ , list(Field = fields, nonNull = sapply(.SD, \(field) any(!is.na(field)))), by = list(Piece, Spine), .SDcols = fields]
  if (!rendEmpty) {
    spines[ , newSpine := cumsum(nonNull | (!nonNull & !duplicated(Spine))), by = Piece]
    spines <- spines[nonNull == TRUE |  (!nonNull & !duplicated(cbind(Piece, Spine)))]
  } else {
    spines[ , newSpine := seq_along(nonNull), by = Piece]
  }
  
  # what classes are different fields?
  classes <- lapply(humtab[ , fields, with = FALSE], class)
  if (length(unique(classes)) > 1L) humtab[ , (fields) := lapply(fields, \(field) as.character(humtab[[field]]))]
  
  cantRemove <- c('Token', 'Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal', 
                  'Exclusive', 
                  'Piece', 'Spine', 'Path', 'Stop', 'Record', 'DataRecord', 'Global', 'Type')
  
  Exclusive.fields <- intersect(paste0('Exclusive.', fields), colnames(humtab))
  humtabs <- lapply(fields, 
                    \(field) {
                      htab <- data.table::copy(humtab)
                      htab[ , (setdiff(fields, field)) := NULL]
                      
                      if (removeRended && !field %in% cantRemove) {
                        colnames(htab)[colnames(htab) == field] <- fieldName
                      } else {
                        htab[[fieldName]] <- htab[[field]]
                        
                      }
                      
                      Exclusive.field <- paste0('Exclusive.', field)
                      if (Exclusive.field != 'Exclusive.Token' && Exclusive.field %in% Exclusive.fields) {
                        if (length(setdiff(Exclusive.fields, Exclusive.field))) htab[ , (setdiff(Exclusive.fields, Exclusive.field)) := NULL]
                        colnames(htab)[colnames(htab) == Exclusive.field] <- paste0('Exclusive.', fields[1]) # need to make sure this gets created
                      }
                      
                      spineMatches <- matches(htab[ , list(Piece, Spine)], spines[Field == field, list(Piece, Spine)])
                      htab <- htab[!is.na(spineMatches)]
                      htab$Spine <- spines[Field == field]$newSpine[spineMatches[!is.na(spineMatches)]]
                      # humtab[ , Spine := ifelse(is.na(spineMatches), Spine,
                      # spines[Field == field]$newSpine[spineMatches])]
                      htab
                      
                    })
  
  humtab <- data.table::rbindlist(humtabs, fill = TRUE)
  putHumtab(humdrumR, overwriteEmpty = 'LIMDd') <- humtab
  
  humdrumR <- updateFields(humdrumR)
  
  humdrumR <- reKey(humdrumR)
  
  selectFields(humdrumR, fieldName)
  
}

#' @rdname rend
#' @export
pivot_longer.humdrumR <- function(data, cols, ...) {
  fields <- tidyselect_humdrumRfields(data, list(rlang::enexpr(cols)), 
                                      ...,
                                      callname = 'pivot_longer.humdrumR')
  
  rend(data, fields, ...)
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
  checks(fromFields, xcharacter & xlen1)
  
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
