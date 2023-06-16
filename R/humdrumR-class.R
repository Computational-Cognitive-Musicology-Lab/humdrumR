#' Humdrum tables (and their Fields)
#' 
#' In the [humdrumR] package, the fundamental data structure is called a **Humdrum Table**.
#' A humdrum table encodes all the information in a collection of one or more humdrum-syntax files
#' as a single [data.table][data.table::data.table] 
#' (A `data.table` is an "enhanced" version of R's standard [data.frame]).
#' Humdrum tables are stored "inside" every [humdrumRclass] object that you will work with, and various `humdrumR`
#' functions allow you to study or manipulate the them.
#' If you want to directly access the humdrum table within a [humdrumRclass] object, use the [getHumtab()] function.
#' 
#' 
#' In a humdrum table, by default, humdrum data is organized in a maximally "long" (or "tall")
#' format, with each and every single "token" in the original data represented by a single row in the table.
#' Even multistops---tokens separated by spaces---are broken onto
#' their own rows. Meanwhile, each column in the humdrum table represents a single
#' piece of information associated with each token, which we call a **field**.
#' Throughout this documentation, you should keep in mind that a "token" refers
#' to a *row* in the humdrum table while a "field" refers to a *column*:
#' 
#' * Token = row
#' * Field = column
#' 
#' # Fields:
#' 
#' There are six types of fields in a humdrum table: 
#' 
#' 1. Data fields
#' 2. Structure fields
#' 3. Interpretation fields
#' 4. Formal fields
#' 5. Reference fields
#' 6. Grouping fields
#' 
#' When first created by a call to [readHumdrum()] every
#' humdrum table has at least nineteen fields: one data field (`Token`), two interpretation 
#' fields (`Tandem` and `Exclusive`), three formal fields, and thirteen structure fields. Additional
#' formal, interpretation, or reference fields
#' may be present depending on the content of the humdrum file(s), and users can create additional data fields
#' by using [within.humdrumR()][withinHumdrum], [mutate.humdrumR()], or some other functions.
#' 
#' ### Data fields:
#' 
#' *Data* fields are used to describe individual data points
#' in humdrum data (as opposed to groups of points). 
#' Every humdrum table starts with a data
#' field called **Token**, which
#' contains character strings representing the original strings read from the humdrum files. 
#' Users can create as many additional data fields as they like. Every call to
#' [withinHumdrum()] generates new data fields. 
#' 
#' 
#' 
#' ### Structure fields:
#' 
#' Every humdrum table has thirteen *Structure* fields,
#' which describe where each data token was "located" in the original humdrum data: 
#' which file, which spine, which record, etc.
#' See the vignette on humdrum syntax to fully understand the terms here.
#' 
#' + *File info*:
#'     + `Filename` :: `character`
#'         + The unique name of the humdrum file. This may include an appended path 
#'           if more than one file with the same name were read from different directories 
#'           (see the [readHumdrum()] docs).
#'     + `Filepath` :: `character`
#'         + The full file name (always includes its full path).
#'     + `Label` :: `character`
#'         + A label specified during the call to [readHumdrum()], associated with a particular
#'          `readHumdrum` "REpath-pattern." If no label was specified, patterns are just labeled `"_n"`, where "`n`" is the 
#'          number of the pattern. 
#'     + `File` :: `integer`
#'         + A unique number associated with each file (ordered alphabetically, starting from `1`).
#'     + `Piece` :: `integer`
#'         + A number specifying the number of the *piece* in the corpus. 
#'           This is identical to the `File` field except when
#'           more than one piece were read from the same file.
#' + *Location info*:
#'     + `Spine` :: `integer`
#'         + The spine, numbered (from left-to-right) starting from `1`. 
#'         + This field is `NA` wherever `Global == TRUE`.
#'     + `Path` :: `integer`
#'         + The "spine path." Any time a `*^` spine path split occurs in
#'           the humdrum data, the right side of the split becomes a new "path." The original path
#'           is numbered `0` with additional paths numbered with integers to the right.
#'           (If there are no spine path splits, the `Path` field is all `0`s.)
#'         + This field is always `NA` when `Global == TRUE`. 
#'     + `ParentPath` :: `integer`
#'         + For spine paths (i.e., where `Path > 0`), which path was the parent from
#'           which this path split? Where `Path == 0`, parent path is also `0`.
#'     + `Record` :: `integer`
#'         + The record (i.e., line) number in the original file.
#'     + `DataRecord` :: `integer`
#'         + The *data* record enumeration in the file, starting from `1`.
#'     + `Stop` :: `integer`
#'         + Which token in a multistop token, numbered starting from `1`.
#'         + In files with no multistops, the `Stop` field is all `1`s.
#'         + This field is always `NA` when `Global == TRUE`.
#'     + `Global` :: `logical`
#'         + Did the token come from a global record (as opposed to a local record)?
#'         + When `Global == TRUE`, the `Spine`, `Path`, and `Stop` fields are always `NA`.
#' + *Token info*:
#'     + `Type` :: `character`
#'         + What type of record is it? 
#'             + `"D"` = non-null data 
#'             + `"d"` = null data
#'             + `"I"` = interpretation
#'             + `"M"` = measure/barline 
#'             + `"L"` = local comment
#'             + `"G"` = global comment. 
#'         
#' 
#' 
#' 
#' ### Interpretation fields:
#' 
#' *Interpretation* fields describe interpretation metadata in the humdrum file(s).
#' Humdrum interpretations are tokens that "carry forward" to data points after them, unless cancelled out by a
#' subsequent interpretation. (See the humdrum syntax vignette for a detailed explanation.)
#' *All* humdrum data must have an *exclusive* interpretation
#' so humdrum tables always have an `Exclusive` (:: `character`) field indicating the
#' exclusive interpretation associated with each token/row of the `Token` field.
#' 
#' Humdrum data may, or may not, include additional *tandem* interpretations. A universal rule for parsing
#' tandem interpretations is impossible, because A) tandem interpretations can "overwrite" each other and B)
#' users can create their own tandem interpretations. The best we can do in all cases is 
#' identify *all* tandem interpretations that have appeared previously in the spine
#' (counting most recent first). All these previous interpretations are encoded in a single
#' character string in the `Tandem` field (see the [tandem()] docs for details). 
#' If working with non-standard interpretations, users can parse the `Tandem` field using the
#' [tandem()] function. 
#' If no tandem interpretations occur in a file, the `Tandem` field is full of empty strings (`""`).
#' 
#' Fortunately, many tandem interpretations are widely used and standardized, and these 
#' interpretations are known by `humdrumR`. Recognized interpretations (such as `*clefG4` and `*k[b-]`)
#' are automatically parsed into their own fields by a call to [readHumdrum()].
#' See the [readHumdrum()] documentation for more details.
#' 
#' 
#' ### Formal fields:
#' 
#' *Formal* fields indicate musical sections, or time windows within
#' a piece, including formal designations ("verse", "chorus", etc.) and measures/bars.
#' Humdrum data may or may not include formal metadata fields, indicated by the token `"*>"`.
#' Classified formal marks are put into fields matching their name.
#' Unclassified formal marks are placed in a field called `Formal` as a default.
#' Nested formal categories are appended with an underscore and a number for each level of descent:
#' `Formal_1, Formal_2, ..., Formal_N`.
#' If part of a section is not given a name in a lower hierarchical level, the field is simply
#' empty (`""`) at that point.
#' 
#' Humdrum data may, or may not, also include barlines (tokens beginning `"="`).
#' However, humdrum tables *always* include three formal fields related to barlines:
#' 
#' + `Bar` :: `integer` 
#'     + How many barline records (single or double) have passed before this token?
#'     + If no `"="` tokens occur in a file, `Bar` is all zeros.
#'     + Note that this field is independent of whether the barlines are labeled with numbers in the humdrum file!
#' + `DoubleBar` :: `integer`
#'     + How many *double*-barline records have passed before this token?
#'     + If no `"=="` tokens occur in a file, `DoubleBar` is all zeros.
#' + `BarLabel` :: `character`
#'     + Any characters that occur in a barline-token *after* an initial `"="` or `"=="`.
#'       These include the `"-"` in the common "implied barline" token `"=-"`,
#'       repeat tokens (like `"=:||"`), and also any *explicit* bar numbers.
#'     + Note that the `Bar` field always enumerate *every* bar record, while
#'       measure-number labels in humdrum data (which appear in the `BarLabel` field) may
#'       do weird things like skipping numbers, repeating numbers, or having suffixes (e.g., `"19a"`).
#'       If no barline tokens appear in the file, `BarLabel` is all empty strings (`""`).
#' 
#' If no barline tokens are present in a file, `Bar` and `DoubleBar` will be nothing but `0`s,
#' and `BarLabel` will be all `NA`.
#' 
#' ### Reference fields:
#' 
#' *Reference* fields describe any **Reference Records**
#' in the humdrum data. Every reference record (records beginning `"!!!"`) in any
#' humdrum file in a corpus read by [readHumdrum] is parsed into a field named
#' by the reference code: `"XXX"` in `"!!!XXX"`.
#' Reference tokens are all identical throughout
#' any humdrum piece. If a reference code appears in one file but not another, the field is
#' `NA` in the file which does not have the code. If no reference records appear in any
#' files read by [readHumdrum()], no reference fields are created.
#' 
#' Examples of common reference records are `"!!!COM:"` (composer) and `"!!!OTL:"` (original title).
#' Any humdrum data with these records will end up having `COM` and `OTL` fields in its humdrum table.
#' 
#' @section Null data:
#' 
#' In humdrum syntax, there is no requirement that every spine-path contains data
#' in every record. Rather, spines are often padded with *null tokens*.
#' In some cases, entire records may be padded with null tokens.
#' Each type of humdrum record uses a different null token:
#' 
#' + *Intepretation*: `*`
#' + *Comment*: `!`
#' + *Barline*: `=`
#' + *Data*: `.`
#' 
#' Many `humdrumR` functions automatically ignore null data, unless you specifically tell them not to 
#' (usually, using `dataTypes` argument).
#' Whenever different [fields()] are created or [selected][selectedFields], `humdrumR` reevaluates
#' what data locations it considers null.
#' Note that `humdrumR` considers data locations to be "null" when
#' 
#' + the selected fields are all `character` data *and* the token is a single `"."`, `"!"`, `"="`, or `"*"`; **or**
#' + the selected fields are all `NA` (including `NA_character_`).
#' 
#' When `humdrumR` reevaluates null data, the `Type` field is updated, setting data records to `Type == "d"`
#' for null data and `Type == "D"` for non-null data. 
#' This is the main mechanism `humdrumR` functions use to ignore null data: most functions
#' only look at data where `Type == "D"`.
#' 
#' Whenever you print or [export][writeHumdrum()] a [humdrumR object[humdrumRclass], null data in the selected fields
#' prints as `"."`---thus `NA` values print as `.`.
#' Thus, if you are working with numeric data with `NA` values, these `NA` values will print as `"."`.
#' 
#' ### Grouping fields:
#' 
#' Grouping fields are special feels which may be created by calls to [group_by()].
#' These fields are deleted by calls to [ungroup()].
#' 
#' 
#' @section Reshaping:
#' 
#' Breaking the complex syntax of humdrum data into the "flat" structure of a humdrum table, with every single token on one line
#' of a `data.table`, makes humdrum data easier to analyze.
#' Of course, thanks to the structure fields, we can easily
#' regroup and reform the original humdrum data or use the structure of the data (like spines) in our analyses.
#' However, in some cases, you might want to work with humdrum data in a different structure or "shape."
#' `humdrumR` has several options for ["collapsing"][collapseHumdrum()] tokens within humdrum tables, 
#' ["folding"][foldHumdrum()] different parts of the data into new fields,
#' or otherwise [reshaping humdrum data][humCoercion] into basic R data structures you might prefer.
#' 
#'
#' @family Core humdrum data representation
#' @name humTable
NULL




getColumn <- function(humtab, pad = 'corpus') {
    
    switch(pad,
           corpus = humtab[  , frank(list(Spine, Path), ties.method = 'dense', na.last = 'keep')],
           piece  = humtab[  , frank(x = list(Spine, Path), ties.method = 'dense', na.last = 'keep'), by = Piece]$V1,
           dont   = {
               humtab[ , `_rowKey_` := seq_len(nrow(humtab))]
               column <- humtab[  , list(Column = cumsum(Stop == 1L), `_rowKey_` = `_rowKey_`), by = .(Piece, Record)]
               setorder(column, `_rowKey_`)
               
               column$Column
               })
        
   
    
}


#####Humtable methods


orderHumtab <- function(humtab) {
    if (nrow(humtab) == 0L) return(humtab)
    orderingcols <- c('File', 'Piece', 'Spine', 'Path', 'Record', 'Stop') 
    
    # can't sort by lists
    
    setorderv(humtab, cols = orderingcols)
    
}

#######################################################-
#################################### humdrumR S4 class ----
######################################################-

#' `humdrumR` class
#' 
#' This `S4` class is the basic unit of the 
#' [humdrumR] package.
#' Each `humdrumR object` represents data [read][readHumdrum()] from one or 
#' more humdrum files.
#' In the documentation, we refer to these objects interchangeably as 
#' "`humdrumR` corpora", "`humdrumR` objects," or `humdrumR` data(sets).
#' In coding examples we name them "`humData`."
#' 
#' The most important part of a `humdrumR` object is the 
#' [humdrum tables][humTable] it holds within it;
#' In essence, a `humdrumR` object is simply a wrapper around its
#' humdrum table, which helps users to
#' to visualize, [index][subset.humdrumR()], [summarize][humSummary], and [manipulate][withinHumdrum]
#' the table in a variety of ways.
#' 
#' Basic information about the size and shape of `humdrumR` objects can be
#' obtained with calls to [nrecord, npiece, length, ncol, etc.][humSize].
#' More detailed summary information can be obtained with the humdrumR [corpus summary functions][humSummary].
#' `humdrumR` data can also be coerced to more basic R data types using [as.matrix, as.data.frame, etc.][humCoercion].
#' A number of helpful functions are also defined to "reshape" or reorganize the
#'  data (e.g., [foldHumdrum()], [collapseHumdrum()]).
#' 
#' The most powerful features of [humdrumR] are the tools it gives you to...
#' 
#' + Print a readable view of the data in shorthand/curtailed humdrum syntax.
#' + Filter `humdrumR` data, using [subset.humdrumR()] and the standard `R` [indexing operators][base::Extract]: `[]` and `[[]]`.
#' + Apply arbitrary commands to [humtable][humTable] fields using the [with(in)Humdrum][withinHumdrum] routines.
#' 
#' @section Printing: 
#' 
#' If you type the name of an object on the R command line, R will "print" the object in the console.
#' A `humdrumR` object will, by default, print a humdrum-syntax score representation.
#' The print/view settings for humdrumR can be manipulated with the [humdrumR()] function.
#' Most notably, by calling `humdrumR('table')`, `humdrumR` will switch or printing a view of the underlying [humdrum table][humTable],
#' rather than the humdrum-syntax score.
#' You can return to the score view by calling `humdrumR('humdrum')`.
#' For `**kern` data, you can also view the notation of the score using `humdrumR('score')`.
#' If there are more than one pieces in the object, the beginning of the first piece is printed, followed by the end of the last piece.
#' For more details on printing options, read the [humdrumR()] manual.
#' 
#' When `humdrumR(view = 'table')`, the long [humdrum table][humTable] is printed.
#' The `Piece`, `Spine`, and `Record` fields will always print in the output table, as well as `Path` and `Stop` if any
#' paths/stops are present.
#' After these stuctural fields, any/all selected fields are shown.
#' When `humdrumR(view = 'humdrum')` (the default) a humdrumR score is printed, with record numbers enumerated at the left edge.
#' 
#' 
#' 
#' @slot Humtable A [humdrum tables][humTable]---i.e, a [data.table::data.table()] with particular fields.
#' @slot Files A list of two elements. The first, "`Search`", contains a single character representing
#' the `pattern` used in the call to [readHumdrum()] which created this humdrumR object.
#' The second, "`Names`," is a vector of strings representing all the files which matched the `pattern`
#' and were read into the `humdrumR` object, with [names()] corresponding to their "subcorpora" labels (`Label`).
#' @slot Fields A [data.table] indicating the existing fields in the `humdrumR` object's
#' [humdrum table][humTable].
#' The fields are divided into five categories: "Data", "Structure", "Interpretation", "Formal", and "Reference."
#' @slot LoadTime A [POSIXct][base::DateTimeClasses] value, indicating the time at which [readHumdrum()] was
#' called to create this `humdrumR` object.
#
#' @name humdrumRclass
#' @seealso humdrumR
#' @family Core humdrum data representation
#' @aliases humdrumRS4
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'data.table',
                   Files = 'list',
                   Fields = 'data.frame',
                   LoadTime = 'POSIXct'
                   )) -> makeHumdrumR

setMethod('initialize', 'humdrumR',
          function(.Object, humtab, pattern, tandemFields) {
            # humtab = a humdrum table
            # pattern = the original file search pattern (string)
            # tandem col a logical vector indicating which columns are tandem fields

            fieldTable <- initFields(humtab, tandemFields)
            setcolorder(humtab, fieldTable$Name)
            
            .Object@Humtable  <- humtab    
            .Object@Fields    <- fieldTable
            .Object@Files     <- list(Search = pattern, Names = unique(humtab$Filepath))
            .Object@LoadTime  <- Sys.time()
            .Object
          })


structureTab <- function(..., groupby = list()) {
    
    fields <- as.data.frame(list(...))
    
    groupby <- checkWindows(fields[[1]], groupby)
    
    fields$Piece <- fields$Spine <- fields$Stop <- fields$File <- 1L
    fields$Record <- seq_len(nrow(fields))
    fields$Path <- fields$ParentPath <- 0L
    fields[names(groupby)] <- groupby
    
    as.data.table(fields)
}


# humdrumR core methods ####


## $ methods ----

#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
              name <- as.character(name)
              
              match <- fieldMatch(x, name, callfun = '$', argname = 'name')
              
              getHumtab(x, 'D')[[match[1]]]
          })

#' @export
setMethod('$<-', signature = c(x = 'humdrumR'),
          function(x, name, value) {
              checks(value, (xvector | xinherits('token')) & xlen)
              
              humtab <- getHumtab(x, 'D')
              if (!(length(value) == 1L | length(value) == nrow(humtab))) .stop("When using humdrumR$<- value, the value must either be length 1",
                                                                                "or exactly the same length as the number of non-null data tokens in the",
                                                                                "humdrumR object's selected fields.")
              name <- as.character(name)
              
              if (name == 'Token') .stop("In your use of humdrumR$<-, you are trying to overwrite the 'Token' field, which is not allowed.",
                                         "This field should always keep the original humdrum data you imported.")
              
              structural <- c('Filename', 'Filepath', 'File', 'Label', 'Bar', 'DoubleBar', 'BarLabel', 'Formal',
                              'Piece', 'Spine', 'Path', 'Stop', 'Record', 'DataRecord', 'Global', 'Type')

              if (name %in% structural) .stop("In your use of humdrumR$<-, you are trying to overwrite the structural field '{match}', which is not allowed.",
                                              "For a complete list of structural fields, use the command fields(mydata, 'S').")
              isnew <- !name %in% colnames(humtab)
              humtab[[name]] <- value
              
              putHumtab(x, overwriteEmpty = c()) <- humtab
              x <- updateFields(x)
              
              x
              
          })


## As/Is ####

#' @rdname humdrumRclass
#' @export
is.humdrumR <- function(x){
    inherits(x, 'humdrumR')  
} 


#' humdrumR coercion
#' 
#' Many users may wish to work with humdrum data,
#' without having to rely on `humdrumR`'s [with(in).humdrumR][withinHumdrum] functionality.
#' Rather, you'd like to just get "normal" `R` objects out of your humdrum data.
#' `humdrumR` defines a number of functions/methods for "coercing" [humdrum data][humdrumRclass] into
#' basic `R` data types.
#' 
#' @details
#' 
#' Generally, coercion works by evaluating a humdrumR object's the 
#' [selected fields][selectedFields] and forcing the result to be an atomic vector.
#' When multiple field are selected, they are pasted together, separated by `", "`.
#' If a field is not atomic (like a `list`, or `lm` object), a concise representation of the
#' list or object class is printed.
#' The [as.vector(humdrumR)][humCoercion] has the additional
#' option of coercing the resulting vector to a particular type using the `mode` argument.
#' 
#' The [as.matrix(humdrumR)][base::as.matrix()] method take things a step further by putting the evaluated
#' fields into a two-dimensional matrix, with rows representing records and columns indicating 
#' spine paths (see Padding section below).
#' [as.data.frame(humdrumR)][base::as.data.frame()] first calls `as.matrix` then converts the matrix to a
#' `data.frame`.
#' Note that `as.matrix(humdrumR)` places the *entire* corpus object into one matrix, even if there are multiple pieces.
#' In contrast, the plural `as.matrices` and `as.data.frames` call their respective singular versions 
#' separately on each individual file in a [humdrumR corpus][humdrumRclass] and return them all in a list.
#' The [row names][base::rownames()]  of the `matrix`/`data.frame`(s) consist of two integer values, 
#' separated by a `.`, representing: `Piece.Record`.
#'
#' The `as.lines` function converts a [humdrumR object][humdrumRclass] into a `character` vector of text lines,
#' with columns separated by the `sep` argument (defaults to `"\t"`), just as you'd see in a humdrum-syntax file.
#' Each line is a single row from a `as.matrix.humdrumR`, with padded values at the right side removed.
#' The matrix's `Piece.Record` [row names][base::rownames()] are preserved as the lines' [names][base::names()].
#'  
#' Note that multiple-stop token (where `Stop > 1L`) cannot by incorporated into the two 
#' dimensional `matrix`/`data.frame`. Thus, `as.matrix(humdrumR)` calls 
#' [collapseStops(collapseAtomic = TRUE, sep = " ")]
#' on the [humdrumR object][humdrumRclass] before creating a matrix.
#' 
#' @section Padding:
#' 
#' Different pieces in a single [humdrumR object][humdrumRclass]
#' often differ in the number of spines and/or spine paths they contain.
#' To squish them into a two dimensional object (`matrix` or `data.frame`) they must necessarily be padded to the same number of columns.
#' (Global comments---which actually have `NA` spines---are also padded, placing the record in column 1.)
#' The `pad` argument is a single atomic value which is used to pad the matrix.
#' 
#' Another consideration is the behavior of spine paths.
#' In the humdrum syntax, a spine path in a leftward spine "bumps" data in higher spines into new columns, as in this example:
#' 
#' ```
#' **kern  **kern
#' A       E
#' *^      *
#' A       C       E
#' G       B       D
#' *v      *v      *
#' A       C        
#' *-      *-
#' ```
#' 
#' At the beginning and end of the file, the second column holds data for the second spine.
#' However, in the middle of the file, the second column holds data from the second spine path of the first spine.
#' To make the spine structure clearer, `as.matrix(humdrumR)` has the option to pad spine paths.
#' For example, using `"_"` as our `pad` argument:
#' 
#' ```
#' **kern   _        **kern
#' A        _        E
#' *^       _        *
#' A        C        E
#' G        B        D
#' *v       *v       *
#' A        _        C        
#' *-       _        *-
#' ```
#' 
#' This aspect of the matrix padding behavior can be controlled with the `padPaths` argument, with three possible values/behaviors:
#' 
#' + `"corpus"`: Paths are padded such that spine-paths across all pieces in the corpus all align in the same columns.
#'   If even one file has a spine path, all the other files are padded so their spines stay aligned.
#'   This is the default behavior for `as.matrix(humdrumR)`.
#' + `"piece"`: Paths are padded, but only *within* each piece. The spines/paths between different pieces may not align.
#' + `"dont"`: Paths are not padded at all. 
#' 
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param dataTypes ***Which types of humdrum record(s) to include.***
#' 
#' Defaults to `"GLIMDd"` for `as.lines()` and `as.matrix()`; `"Dd"` for `as.data.frame()`;
#' `"LIMDd"` for `as.matrices()` and `as.data.frames()`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation for explanation.)
#' 
#' @param padPaths ***Determines how spine-paths are aligned in the output.***
#' 
#' Defaults to `"dont"` for `as.lines()`; `"corpus"` for `as.matrix()` and `as.data.frame()`;
#' `"piece"` for `as.matrices()` and `as.data.frames()`
#' 
#' Must be a single `character` string, `"corpus"`, `"piece"`, or `"dont"`.
#'
#' See the details for an explanation.
#'   
#' @param padder ***Used to fill in differences in the number of columns between files and/or spine paths.***
#' 
#' Defaults to `NA`.
#' 
#' Must be a single `atomic` value.
#' 
#' @param sep ***Separator to place between columns in collapsed lines.***
#' 
#' Defaults to `"\t"` (tab).
#' 
#' Must be a single `character` string.
#' 
#' @param mode ***The desired output class.***
#'   
#' Defaults to `"any"`.
#' 
#' Must be a single `character` string naming an [atomic vector type][base::vector] to coerce the output to (i.e., `logical` or `numeric`).
#'   
#' If set to `"any"`, the output type is simply whatever the type of the [selected field][selectedFields] is.
#' 
#' 
#' 
#' 
#' @name humCoercion
#' @export
setMethod('as.vector', 
          signature = c(x = 'humdrumR'),
          function(x, mode = 'any') {
	  checks(mode, xcharacter & xlen1 & xlegal(c('any', 'logical', 'numeric', 'integer', 'character')))

                    if (is.empty(x)) return(vector(mode, 0L))
                    
                    vec <- pullSelectedField(x, 'D')
                    if (mode != 'any') vec <- as(vec, mode)
                    vec
                    
                    })

#' @name humCoercion
#' @export
as.lines <- function(humdrumR, dataTypes = 'GLIMDd', padPaths = 'dont', padder = '', sep = '\t') {
    
          # dataTypes <- checkTypes(dataTypes, 'as.lines')
          checks(dataTypes, xrecordtypes)
          checks(humdrumR, xhumdrumR)
          checks(padPaths, xcharacter & xlen1 & xlegal(c('corpus', 'piece', 'dont', "don't")))
          checks(padder, xatomic & xlen1)
          checks(sep, xatomic & xlen1)
          
          mat <- as.matrix(humdrumR, dataTypes, padPaths = padPaths, padder = padder)
          lines <- applyrows(mat, paste, collapse = sep)
          
          
          lines <- stringr::str_replace_all(lines, paste0('(', sep, padder, ')+$'), '')
          # lines <- stringr::str_remove(lines, '\t+$')
          # 
          names(lines) <- rownames(mat)
          
          lines
                           
          
}

### As single matrix(like) ----

#' @name humCoercion
#' @aliases as.matrix
#' @export
as.matrix.humdrumR <- function(x, dataTypes = 'GLIMDd', padPaths = 'corpus', padder = NA) { 
    
    checks(x, xhumdrumR)
    dataTypes <- checkTypes(dataTypes, 'as.matrix.humdrumR')
    checks(padPaths, xcharacter & xlegal(c('corpus', 'piece', 'dont', "don't")))
    checks(padder, xatomic & xlen1)

    x <- collapseStops(x)
    humtab <- getHumtab(x, dataTypes)
    
    
    i <- data.table::frank(humtab[ , list(Piece, Record)], ties.method = 'dense')
    j <- getColumn(humtab, padPaths)
    j[is.na(j)] <- 1L
    
    
    field <- pullSelectedField(x, dataTypes = dataTypes)
    if (is.factor(field)) field <- as.character(field) # R does't allow factors in matrices
    # padder <- as(padder, class(field))
    
    output <- matrix(padder, nrow = max(i), ncol = max(j))
    
    output[cbind(i, j)] <- field
    rownames(output) <- unique(humtab[,c('Piece', 'Record')])[order(Piece, Record), paste0(Piece, '.', Record)]
    
    output
    
    
}             








#' @name humCoercion
#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrumR'),
          function(x, dataTypes = 'Dd', padPaths = 'corpus', padder = NA) {
              
              as.data.frame(as.matrix.humdrumR(x, dataTypes = dataTypes, padPaths = padPaths, padder = padder), stringsAsFactors = FALSE)
          })

#' @name humCoercion
#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrumR'),
          function(x, dataTypes = 'Dd', padPaths = 'corpus', padder = NA) {
              
              as.data.frame(as.matrix.humdrumR(x, dataTypes = dataTypes, padPaths = padPaths, padder = padder), stringsAsFactors = FALSE)
          })

### As (list of) matrix-like ####

#' @name humCoercion
#' @export
as.matrices <- function(humdrumR, dataTypes = 'LIMDd', padPaths = 'piece', padder = NA) {
    checks(humdrumR, xhumdrumR)
    dataTypes <- checkTypes(dataTypes, 'as.matrices')
    checks(padPaths, xcharacter & xlegal(c('corpus', 'piece', 'dont', "don't")))
    checks(padder, xatomic & xlen1)
    
    
    mat <- as.matrix.humdrumR(humdrumR, dataTypes = dataTypes, padPaths = padPaths, padder = padder)
    dontpad <- padPaths != 'corpus'
    
    piece <- as.integer(gsub('\\..*', '', rownames(mat)))
    lapply(unique(piece),
           \(p) {
               submat <- mat[piece == p, , drop = FALSE] 
               if (dontpad) {
                   submat <- submat[ , colSums(!is.na(submat)) > 0L, drop = FALSE]
               }
               
               submat
           })
}

#' @name humCoercion
#' @export 
as.data.frames <- function(humdrumR, dataTypes = 'LIMDd', padPaths = 'piece', padder = NA) {
    checks(humdrumR, xhumdrumR)
    dataTypes <- checkTypes(dataTypes, 'as.data.frames')
    checks(padPaths, xcharacter & xlegal(c('corpus', 'piece', 'dont', "don't")))
    checks(padder, xatomic & xlen1)
    
    
    lapply(as.matrices(humdrumR,dataTypes = dataTypes, padPaths = padPaths, padder = padder), 
           as.data.frame, stringsAsFactors = FALSE)
}





# Shape ####

## Size ----

#' [humdrumR data][humdrumRclass] size and shape
#' 
#' These functions can be used to quickly
#' get basic information about the size and "shape" of
#' a [humdrumR corpus objects][humdrumRclass].
#' For more details, use the [census()] or [spines()] functions instead.
#' 
#' @details 
#' 
#' The following functions are defined.
#' 
#' 
#' + `nfile` : The number of input files in the corpus. 
#'   + [length][base::length()]`(humdrumR)` is a synonym.
#' + `npiece`: The number of pieces in the corpus. (There may be multiple pieces per file.)
#' + `nrecord`: The number of records in the corpus. 
#'   + [nrow][base::nrow()]`(humdrumR)` is a synonym.
#' + `ntoken`: The number of tokens in the corpus.
#' + `ncol(humdrumR)`: Returns the maximum number of "columns" need to represent the data in a 2d matrix.
#'    Matches the default output from [as.matrix(humdrumR)][humCoercion].
#' + `dim(humdrumR)`: the same as `c(nrow(humdrumR), ncol(humdrumR))`.
#' 
#' @section Is/Any:
#' 
#' A few additional functions return quick `TRUE`/`FALSE` answers regarding a [humdrumR corpus][humdrumRclass]:
#' 
#' + `is.empty`: Returns `TRUE` is a corpus contains no *non-null* data tokens (`D` tokens).
#' + `anyPaths`: Returns `TRUE` if there are any spine paths (`Path > 0`) in any pieces in the corpus.
#' + `anyStops`: Returns `TRUE` if there are any multi-stops (`Stop > 1`) in any pieces in the corpus.
#' + `anySubcorpora`: Returns `TRUE` if the corpus was [read][readHumdrum()] with different regex patterns
#'     matching "subcorpora" labels.
#'    + `namesSubcorpora` returns the names of the subcorpora labels (`Label` field).
#' + `anyMultiPieceFiles`: Returns `TRUE` if any files contain more than one piece (`Piece != File`).
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param dataTypes ***Which types of humdrum record(s) to include in the census.***
#' 
#' Defaults to `"GLIMDd"`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation **Fields** section for explanation.)
#'     
#' @name humSize
#' @export
nrecord <- function(humdrumR, dataTypes = 'GLIMDd') {
          checks(humdrumR, xhumdrumR)
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'nrecord')
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)

          nrow(unique(humtab[ , list(Piece, Record)]))
          
}

#' @name humSize
#' @export
setMethod('nrow',  signature = c(x = 'humdrumR'), \(x) nrecord(x))



#' @name humSize
#' @export
ntoken <- function(humdrumR, dataTypes = 'GLIMDd') {
          checks(humdrumR, xhumdrumR)
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'ntoken')
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
          
          nrow(humtab)
}

#' @name humSize
#' @export
npieces <- function(humdrumR) {
          checks(humdrumR, xhumdrumR)
          
          length(unique(getHumtab(humdrumR)$Piece))
}

#' @name humSize
#' @export
nfiles <- function(humdrumR) {
    checks(humdrumR, xhumdrumR)
    
    length(unique(getHumtab(humdrumR)$File))
}

#' @name humSize
#' @export
setMethod('length', signature = c(x = 'humdrumR'), \(x) npieces(x))


#' @name humSize
#' @export
setMethod('ncol', 
          signature = c(x = 'humdrumR'), 
          \(x) max(getColumn(getHumtab(x)), na.rm = TRUE))

#' @name humSize
#' @export
setMethod('dim',  signature = c(x = 'humdrumR'),  \(x) c(nrecord(x), ncol(x)))


#' @name humSize
#' @export
is.empty <- function(humdrumR){
    checks(humdrumR, xhumdrumR)
    nrow(getHumtab(humdrumR, 'D')) == 0L
} 



## Structure ----

#' @rdname humSize
#' @export
anyMultiPieceFiles <- function(humdrumR) {
    checks(humdrumR, xhumdrumR)
    nfiles(humdrumR) != npiece(humdrumR)
}

#' @rdname humSize
#' @export
anyPaths <- function(humdrumR) {
    checks(humdrumR, xhumdrumR)
    humtab <- getHumtab(humdrumR)
    
    any(humtab$Path > 0L, na.rm = TRUE)
    
}

#' @rdname humSize
#' @export
anyStops <- function(humdrumR) {
    checks(humdrumR, xhumdrumR)
    
    humtab <- getHumtab(humdrumR)
    any(humtab$Stop > 1L, na.rm = TRUE)
    
}





#' Does humdrumR corpus contain subcorpora?
#' 
#' [HumdrumR][humdrumRclass] objects can be divided into "subcorpora."
#' `anySubcorpora` and `namesSubcorpora` functions tell us if there are any subcorpora and, if so, what they are called.
#'
#' @name humSize
#' @export
anySubcorpora <- function(humdrumR){
    checks(humdrumR, xhumdrumR)
    
    humtab <- getHumtab(humdrumR)
    
    humtab[ , length(unique(Label)) > 1L]
}

#' @name humSize
#' @export
namesSubcorpora <- function(humdrumR) {
    checks(humdrumR, xhumdrumR)
    
    humtab <- getHumtab(humdrumR)
    
    humtab[ , unique(Label)]
}


is.ragged <- function(humdrumR) {
    # Do the pieces in the corpus vary in number of spines?
    
    humtab <- getHumtab(humdrumR, 'D')
    
    humtab$Column <- getHumtab(humtab, 'piece')
    ncols   <- humtab[!is.na(Column) , max(Column) , by = Filename]$V1
    nspines <- humtab[!is.na(Spine)  , max(Spine)  , by = Filename]$V1
    
    length(unique(ncols)) > 1L || length(unique(nspines)) > 1L
    
}


## Renumbering ----

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

## Reshaping ----


#' Merge two (or more) humdrumR datasets
#'
#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @export
#' @name humMerge
mergeHumdrum <- function(...) {
    
    
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


### collapseHumdrum ----

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
#' @seealso The humdrum [folding functions][foldHumdrum()] serve a similar function,
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



### foldHumdrum ----

#' "Fold" humdrumR data into new fields
#'
#' Many humdrum datasets encode data across multiple spines, spine-paths, or stops.
#' By default, `humdrumR` parses each separate spine, spine-path, and stop as their own individual
#' data points, taking up one row in the [humdrum table][humTable].
#' If we want to treat data in multiple spines/paths/stops as different aspects of the same data
#' it is easiest to reshape the data so that the information is in different humdrumR [fields][fields()]
#' rather than separate spines/paths/stops.
#' We "fold" the data from one structural location over "on top" of other data using `foldHumdrum`.
#' 
#' @section From where to where:
#' 
#' The `numeric` `fold` and `onto` arguments specify where to fold from/to.
#' `fold` indicates the Spine/Path/Stop to fold *from*, "*on to*" the Spine/Path/Stop
#' indicated by `onto`.
#' For example, if you specify `foldHumdrum(mydata, fold = 2, onto = 1, what = 'Spine')`
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
#' Defaults to `FALSE` for `foldHumdrum()` and `foldStops()`; `TRUE` for `foldPaths()`.
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
#' @seealso The [collapse family of functions][collapseHumdrum()] serves a somewhat
#' similar function, "collapsing" data *within* a field.
#' @family {Folding functions}
#' @family {Humdrum data reshaping functions}
#' @export
foldHumdrum <- function(humdrumR, fold,  onto, what = 'Spine', Piece = NULL, 
                        fromField = selectedFields(humdrumR)[1], fillFromField = FALSE,
                        newFieldNames = NULL) {
    # argument checks
    checks(humdrumR, xhumdrumR)
    checks(fold, xnatural)
    checks(onto, xnatural)
    
    checks(fromField, xcharacter & xlen1)
    fromField <- fieldMatch(humdrumR, fromField, 'foldHumdrum', 'fromField')
    checks(what, xcharacter & xlen1 & xlegal(c('Spine', 'Path', 'Stop', 'Record', 'DataRecord')))
    
    # start work
    humdrumR <- selectFields(humdrumR, fromField)
    
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDd')
    moves <- foldMoves(humtab, fold, onto, what, Piece, newFieldNames)

    # 
    fromHits <- humtab[ , list(Piece, get(what)) %ins% moves[, c('Piece', 'From'), with = FALSE]]
    fromTable <- humtab[fromHits == TRUE, c(fromField, fields(humdrumR, c('S', 'F', 'R'))$Name), with = FALSE]
    
    if (all(is.na(fromTable[[fromField]]))) {
        .warn("Your fromField doesn't have any non-null data where {what} == {harvard(fold, 'or')}.",
              "Your humdrumR data is being returned unchanged.")
        return(humdrumR)
    }
    
    humtab[[fromField]][fromHits & humtab$Type == 'D'] <- NA
    #
    whichMatch <- fromTable[ ,  matches(list(Piece, get(what)), moves[ , c('Piece', 'From'), with = FALSE], multi = TRUE)]
    
    #
    fromTable <- do.call('rbind', lapply(1:ncol(whichMatch),
           \(j) {
               i <- whichMatch[, j]
               fromTable <- fromTable[!is.na(i),]
               i <- i[!is.na(i)]
               switch(what,
                      Record = fromTable$DataRecord  <- fromTable[ , DataRecord  + (moves$To[i] - Record)],
                      DataRecord  = fromTable$Record <- fromTable[ , Record + (moves$To[i] - DataRecord)])
               
               fromTable[[what]] <-  moves$To[i]
               fromTable$FieldNames <- moves$FieldNames[i]
               fromTable
           }))
   
   
    
    # data fields in old rows need to be renamed, because they will now be columns
   
    fromTables <- split(fromTable, by = 'FieldNames', keep.by = FALSE)
    fromTables <- Map(\(ftab, fname) {
                             colnames(ftab)[colnames(ftab) == fromField] <- fname
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
        
        if (fillFromField) {
            for (field in newfields) {
                na <- is.na(htab[[field]])
                # hits <- na & htab$Spine %in% unique(htab$Spine[!na])
                htab[[field]][na] <- htab[[fromField]][na]
            }
        }
        htab
        
    }, fromTables, init = humtab)
 
    
    humtab <- update_humdrumR(humtab, field = c(newfields, fields(humdrumR, 'D')$Name))
    humtab <- removeNull(humtab, by = c('Piece', what), nullTypes = 'LIMd')
    humtab <- update_Null(humtab, field = newfields)
    
    putHumtab(humdrumR, overwriteEmpty = c()) <- orderHumtab(humtab)
    
    humdrumR <- updateFields(humdrumR) 
    
    selectFields(humdrumR, c(fromField, newfields))
    
    

}

foldMoves <- function(humtab, fold, onto, what, Piece = NULL, newFieldNames = NULL) {
    checks(fold, xwholenum)
    
    
    if (!is.null(Piece)) {
        if (length(unique(lengths(list(fold, onto, Piece)))) > 1){
            .stop("In your call to foldHumdrum, ",
                  "if the 'Piece' argument is not NULL,",
                  "the 'Piece', 'fold', and 'onto' arguments must all be the same length.")
        }
    } else {
        Piece <- rep(unique(humtab$Piece), each = max(length(fold), length(onto)))
        
        match_size(Piece = Piece, fold = fold, onto = onto, toEnv = TRUE)
    }    
    
    moves <- unique(data.table(Piece = Piece, From = fold, To = onto))
    moves[] <- lapply(moves, as.integer)
    
    # Check for errors
    moves[ ,  if (any(To %in% From)) .stop("In your call to foldHumdrum, the 'fold' and 'onto' {what}s can't overlap within any 'Piece'.") , by = Piece]
    
    
    # name fields
    moves[ , NewField := seq_along(From), by = .(Piece, To)]
    NnewFields <- length(unique(moves$NewField))

    
    newFieldNames <- if (is.null(newFieldNames)) {
        newFieldNames <- moves[ , paste0('Fold{', what, paste(unique(From), collapse = ','), '->', what, paste(unique(To), collapse = ','), '}'), by = NewField]$V1
        tail(n = length(newFieldNames), make.unique(c(colnames(humtab), newFieldNames), sep = '.'))
        
    } else {
        if (length(newFieldNames) < NnewFields) {
            newFieldNames <- c(head(newFieldNames, -1L),
                               paste0(tail(newFieldNames, 1L), 
                                      seq_len(NnewFields - length(newFieldNames) + 1)))
        } 
        newFieldNames[1:length(unique(moves$NewField))]
        
        
    }
    
    moves[ , FieldNames := newFieldNames[NewField]]
    
    
    
    moves
}
#    

#' "Fold" exclusive interpretations into new fields
#' 
#' 
#' `foldExclusive()` is a special version of [foldHumdrum()], which 
#' "folds" spines based on their exclusive interpretations.
#' For instance, we can "fold" all the `**silbe` spines in a corpus
#' onto their respective `**kern` spines.
#' 
#' 
#' 
#' @details 
#' 
#' The `fold` and `onto` arguments (`character`, `length == 1`)
#' must match exclusive interpretations in the `humdrumR` object input.
#' Within each file, mismatches in the number of matching `onto` and `fold` spines
#' are handled "in parallel," just like [foldHumdrum()].
#' Multi-matching spines are matched from left-to-right.
#'
#' If no matching exclusive interpetation pairs are found, 
#' the unchanged `humdrumR` object is returned with a warning.
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param fold,onto ***Which exclusive interpretation(s) to "fold" from/to.***
#'    
#' Must be non-empty `character` vectors. `onto` must be a single string; `from` may contain multiple exclusive strings.
#' 
#' Must be specified *without* the `**` prefix: `"kern"` not `"**kern"`.
#' 
#' @family {Folding functions}
#' @export
foldExclusive <- function(humdrumR, fold, onto, fromField = selectedFields(humdrumR)[1]) {
    checks(humdrumR, xhumdrumR)
    checks(fold, xcharnotempty)
    checks(onto, xcharnotempty & xlen1)
    
    fold <- unique(gsub('^\\*\\*', '', fold))
    onto <- unique(gsub('^\\*\\*', '', onto))
    
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDd')
    moves <- humtab[,{
        toSpine <- setdiff(unique(Spine[Exclusive == onto]), NA)
        if (length(toSpine)) {
            do.call('rbind', lapply(fold, 
                   \(fromExclusive) {
                       fromSpine <- setdiff(unique(Spine[Exclusive == fromExclusive]), NA)
                       if (length(fromSpine) == 0L) return(data.table(From = integer(0), 
                                                                      To = integer(0), 
                                                                      Exclusive = character(0)))
                       if (!(length(fromSpine) == 1L && length(toSpine) == 1L && fromSpine == toSpine)) {
                        
                           if (all(fromSpine == toSpine)) {
                               fromSpine <- fromSpine[-1]
                               toSpine <- toSpine[1]
                           }   
                           
                           as.data.table(match_size(From = fromSpine, To = toSpine,  Exclusive = fromExclusive))
                       }  
                       
                   })) -> moves
            # if one exclusive is collapsing onto itself (like kern -> kern),
            # other spines collapsing onto the collapsed one will result in overlaps
            # there's no way to see this before now
            overlaps <- moves[ , To %in% From]
            if (any(overlaps)) {
                moves$To[overlaps] <- moves[overlaps == FALSE][moves[overlaps == TRUE], on ='Exclusive']$To
                moves <- moves[!duplicated(moves)]
            }
            moves
        }
    }, by = Piece]
    
    if (nrow(moves) == 0L) {
        .warn("foldExclusive found no matching files with both '{fold}' and '{onto}'",
              'exclusive interpretations.',
              "Your humdrumR data is returned unchaged.")
        return(humdrumR)
    }
    
    moves <- moves[, list(From, N = seq_along(From)), by = .(Piece, To, Exclusive)]
    moves[ , Group := paste0(Exclusive, if (any(N > 1)) N), by = Exclusive]
    
    newexclusives <- unique(moves$Group)
    newFieldNames <- stringr::str_to_sentence(newexclusives)
    
    humdrumR <- foldHumdrum(humdrumR, 
                            fold = moves$From, 
                            onto = moves$To, 
                            Piece = moves$Piece, what = 'Spine',
                            fromField = fromField,
                            newFieldNames = newFieldNames)
    
    # add exclusives to data
    humtab <- getHumtab(humdrumR)
    for (i in seq_along(newexclusives)) {
        humtab[[newFieldNames[i]]] <- token(humtab[[newFieldNames[i]]], Exclusive = newexclusives[i]) 
    }
    putHumtab(humdrumR) <- humtab
    
    humdrumR
    
    
}

#' @rdname foldHumdrum
#' @export
foldPaths <- function(humdrumR, fromField = selectedFields(humdrumR)[1], fillFromField = TRUE) {
    checks(humdrumR, xhumdrumR)
    
    paths <- unique(getHumtab(humdrumR)$Path)
    paths <- paths[!is.na(paths)]
    
    if (all(paths == 1L)) return(humdrumR)
    
    dataFields <- fields(humdrumR, fieldTypes = 'Data')
    minPath <- min(paths)
    
    paths <- setdiff(paths, minPath)
    
    foldHumdrum(humdrumR, paths, minPath, what = 'Path', 
                fromField = fromField, fillFromField = fillFromField,
                newFieldNames = paste0(fromField, '_Path', paths))
    

    
}

#' @rdname foldHumdrum
#' @export
foldStops <- function(humdrumR, fromField = selectedFields(humdrumR)[1], fillFromField = FALSE) {
    checks(humdrumR, xhumdrumR)
           
   stops <- unique(getHumtab(humdrumR)$Stop)
   stops <- stops[!is.na(stops)] 
   
   if (all(stops == 1L)) return(humdrumR)
   
   dataFields <- fields(humdrumR, fieldTypes = 'Data')
   minStop <- min(stops)
   
   stops <- setdiff(stops, minStop)
   
   foldHumdrum(humdrumR, stops, minStop, what = 'Stop', 
               fromField = fromField, fillFromField = fillFromField,
               newFieldNames = paste0(fromField, '_Stop', stops))
   
   
}

#' "Unfold" data into multiple stops
#' 
#' If some record/spine/path locations have different numbers of
#' stops in different fields, this function spreads the data from the 
#' smaller fields into multiple stops.
#' 
#' @family {Folding functions}
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
    humtab <- update_Null(humtab, field = fromFields)
    putHumtab(humdrumR, overwriteEmpty = c()) <- humtab
    humdrumR
}

#' "Fold" grace notes into neighbos
#' 
#' 
#' @family {Folding functions}
#' @seealso `foldGraceNotes` makes use of the more general [foldHumdrum()].
#' @export
foldGraceNotes <- function(humdrumR) {
    warn("foldGraceNotes has not been implemented yet!")
    humdrumR
}


#################################-
# Humtable manipulation and access ####
###############################-



#' Access a Humdrum Table
#' 
#' `getHumtab` extracts the hudrum table from a [humdrumR object][humdrumRclass].
#' 
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param dataTypes ***Which types of humdrum record(s) to include in the output.***
#' 
#' Defaults to `"GLIMDd"`.
#' 
#' Must be a `character` string, which specifies which types of data tokens/records to extract.
#' Legal values are: `"G"` (global comments), `"L"` (local comments), `"I"` (interpretations),
#' `"M"` (barlines), `"D"` (non-null data), or `"d"` (null data).
#' Multiple types can be specified in a single string: e.g., `"GLIMD"`.
#' 
#' 
#' @rdname humTable
#' @export
getHumtab <- function(humdrumR, dataTypes = "GLIMDd") {
          humtab <- humdrumR@Humtable
          
          checks(humdrumR, xhumdrumR)
          dataTypes <- checkTypes(dataTypes, 'getHumtab')
          
          if (length(setdiff(c('G', 'L', 'I', 'M', 'D', 'd'), dataTypes))) {
              humtab <- humtab[Type %in% dataTypes]
          }
          
          humtab

}


`putHumtab<-` <- function(humdrumR, value, overwriteEmpty = 'GLIMDd') {
          # adds humtab into humdrumR
          # Drop determines whether record dataTypes that are 
          # absent from value are left unchanged (drop = FALSE)
          # or replaced with empty data tables (drop = TRUE)
          # If drop indicates a record type (i.e., GLIM) those types are dropped only
          if (!data.table::is.data.table(value)) .stop("putHumtab()<- requires a data.table value.")
    
    
          if (length(overwriteEmpty)) {
              if (overwriteEmpty == 'GLIMDd') {
                  humdrumR@Humtable <- value
                 return(humdrumR)
                }
                  overwriteEmpty <- checkTypes(overwriteEmpty, 'putHumtab<-')
              
          }
          
          overwriteTypes <- unique(value$Type)
          overwriteTypes <- union(overwriteTypes, overwriteEmpty)
          
          # humtab <- rbind(humdrumR@Humtable[!Type %in% overwriteTypes], value, fill = TRUE) 
          # This works the same as the whole forloop bs bellow, but it breaks with token...which only matters for $<-
          
          
          oldhumtab <- humdrumR@Humtable[!Type %in% overwriteTypes]
          
          newcol <- setdiff(colnames(value), colnames(oldhumtab))
          for(col in newcol) {
              class <- class(value[[col]])
              insert <- if (class == 'token') {
                 token(as(NA, class(value[[col]]@.Data)), Exclusive = getExclusive(value[[col]]))
              } else {
                  as(NA, class)
              }
              if (nrow(oldhumtab) == 0L) insert <- insert[0]
              oldhumtab <- cbind(oldhumtab, setNames(as.data.frame(insert), col))
          }
          humtab <- rbind(oldhumtab, value)
          #
          
          humtab <- orderHumtab(humtab)
          humdrumR@Humtable <- humtab
          
          humdrumR
}







##

is.nullToken <- function(tokens) {
    if (is.list(tokens)) {
        lengths(tokens) == 0L
    } else {
        is.na(tokens) | tokens %in% c('*', '=', '!', '.')
    }
}

nullFields <- function(hum, fields, reduce = '&') {
    nulls <- lapply(hum[ , fields, with = FALSE], is.nullToken)
    Reduce('&', nulls)
}

update_humdrumR <- function(hum, Exclusive, Null, ...) UseMethod('update_humdrumR')
update_humdrumR.humdrumR <- function(hum,  Exclusive = TRUE, Null = TRUE , ...) {
    humtab <- getHumtab(hum, 'GLIMDd')
    
    humtab <- update_humdrumR.data.table(humtab, Exclusive, Null, ...)
    putHumtab(hum, overwriteEmpty = c('d')) <- humtab
    hum
}
update_humdrumR.data.table <- function(hum, Exclusive = TRUE, Null = TRUE, ...) {
    
    if (Exclusive) hum <- update_Exclusive(hum, ...)
    if (Null) hum <- update_Null(hum, ...)
    hum
    
}


#
update_Exclusive <- function(hum, ...) UseMethod('update_Exclusive')
update_Exclusive.humdrumR <- function(hum, ...) {
    humtab <- getHumtab(hum, 'ID')
    
    field <- selectedFields(hum)[1]
    putHumtab(hum, overwriteEmpty = 'ID') <- update_Exclusive.data.table(humtab, field)
    
    hum
}
update_Exclusive.data.table <- function(hum, field = 'Token', ...) {
    field <- field[1]
    Exclusive <- getExclusive(hum[[field]]) 
    
    exclusives <- hum[, Type == 'I' & grepl('^\\*\\*', Token)]
    
    if (!is.null(Exclusive)) {
        
        hum[['Token']][exclusives] <- paste0('**', Exclusive)
    } else {
        hum[['Token']][exclusives] <- paste0('**', hum$Exclusive[exclusives])
    }
    hum
}

#
update_Null <- function(hum, field, ...) UseMethod('update_Null')
update_Null.humdrumR <- function(hum, field = selectedFields(hum),  allFields = FALSE, ...) {
    
    if (allFields) field <- fields(hum, 'D')$Name
    humtab <- getHumtab(hum, 'GLIMDd')
    putHumtab(hum, overwriteEmpty = "GLIMDd") <- update_Null.data.table(humtab, field = field)
    hum
}
update_Null.data.table <- function(hum, field = 'Token', ...) {
    null <- nullFields(hum, field)
    
    hum$Type[hum$Type %in% c('d', 'D')] <-  ifelse(null[hum$Type %in% c('d', 'D')], 'd', 'D')
    hum
}



####################################################-
# Fields ----
####################################################-


## Manipulating the @Fields slot ----

checkFieldTypes <- function(types, argname, callname) {
    valid <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping', 'selected')
    types <- matched(types, valid, nomatch = types)
    checks(types, xcharacter & xmaxlength(7) & xplegal(c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping', 'selected')))
}

initFields <- function(humtab, tandemFields) {
    fields <- colnames(humtab)
    
    fieldTable <- data.table(Name = fields, Class = sapply(humtab, class), Type = 'Reference')
    
    fieldTable[ , Type := {
        Type <- Type
        Type[Name == 'Token'] <- 'Data'
        Type[Name %in% c('Filename', 'Filepath', 'File', 'Label', 'Piece',
                         'Spine', 'Path', 'ParentPath', 'Stop',
                         'Record', 'DataRecord', 'Global', 'Type')] <- 'Structure'
        Type[Name %in% c('Exclusive', 'Tandem', tandemFields)] <- 'Interpretation'
        Type[grepl('^Formal', Name) | Name %in% c('Bar', 'DoubleBar', 'BarLabel')] <- 'Formal'
        Type                 
    }]
    
    setorder(fieldTable, Type, Class)
    fieldTable[ , Selected := Name == 'Token']
    fieldTable[ , GroupedBy := FALSE]
    fieldTable
}




fieldClass <- function(x) {
    xclass <- class(x)
    
    if (xclass == 'token') {
        xclass <- paste0(class(x@.Data), 
                        ' (', if (!is.null(x@Exclusive)) paste0('**', x@Exclusive, ' '), 
                        'tokens)')
    }
    if (xclass == 'list') {
        classes <- unique(sapply(x, class))
        xclass <-  paste0('list (of ',  harvard(paste0(setdiff(classes, 'NULL'), "s"), 'and'), ')')
    }
    
    xclass
}

updateFields <- function(humdrumR, selectNew = TRUE) {
    humtab <- humdrumR@Humtable
    fieldTable <- humdrumR@Fields
    fieldTable <- fieldTable[Name %in% colnames(humtab)] # removes fields that don't exist in humtab
    
    new <- setdiff(colnames(humtab), fieldTable$Name)
    if (length(new)) {
        fieldTable <- rbind(fieldTable, 
                            data.table(Name = new, Type = 'Data', 
                                       Class = '_tmp_', Selected = selectNew, GroupedBy = FALSE))
    }
    
    setorder(fieldTable, Type, Class)
    setcolorder(humtab, fieldTable$Name)
    
    fieldTable$Class <- sapply(humtab, fieldClass)
    
    if (length(new) && selectNew) fieldTable[ , Selected := Name %in% new]
    humdrumR@Fields <- fieldTable
    
    humdrumR
    
}


## Querying fields ----



naDots <- function(field, null, types) {
    if (null == 'asis') return(field)
    na <- is.na(field)
    
    nulltoken <- c(G = '!!', I = '*', L = '!', d = '.', D = '.', M = '=')[types]
    
    
    if (null == 'dot2NA') {
        na <- na | field == nulltoken
        field[na] <- NA
    } else {
        if (null == 'charNA2dot') na <- is.character(field) & na
        field[na] <- nulltoken[na]
    }
    
    field   
}


#' @export
names.humdrumR <- function(humdrumR) names(getHumtab(humdrumR))

fieldMatch <- function(humdrumR, fieldnames, callfun = 'fieldMatch', argname = 'fieldnames') {
          fields <- fields(humdrumR)$Name
          target <- pmatch(fieldnames, fields)
          
          nomatch <- is.na(target)
          
          if (all(nomatch)) {
              .stop("In the '{argname}' argument of your call to humdrumR::{callfun},", 
                    ifelse = length(fieldnames),
                    harvard(fieldnames, 'and', quote = TRUE),
                    '<is not the name of a field|are not names of fields>',
                    'in your humdrumR object.')
          }
          
          if (any(nomatch)) {
              .warn('In the "{argname}" argument of your call to humdrumR::{callfun}, ',
                    ifelse = length(argname),
                    harvard(fieldnames[is.na(target)],  'and'),
                    '<is not the name of a field|are not names of fields>',
                    'in your humdrumR object.')
                      
              target <- target[!nomatch]
              
          }
          

          fields[target]

}


#' Individual fields from the humdrum table can be extracted using `pull()`, which
#' returns a [data.table()][data.table::data.table()] with each column corresponding to one field. 
#' (The `data.table` is a column-subset of the humdrum table).
#' By default, `pullFields()` pulls the [selected fields][selectedFields].
#' If only one field is pulled, and `drop = TRUE`, the field is extracted from the `data.table` and returned
#' as a vector.
#' 
#' @param fields ***Which fields to output.***
#' 
#' Defaults to `selectedFields(humdrumR)`.
#' 
#' Must be a `character` string [partially][partialMatching] matching the name of a data field in the `humdrumR` input.
#' For example, `"Tok"` would match the `Token` field.
#'   
#' @param dataTypes ***Which types of humdrum record(s) to include.***
#' 
#' Defaults to `"GLIMDd"` for `as.lines()` and `as.matrix()`; `"Dd"` for `as.data.frame()`;
#' `"LIMDd"` for `as.matrices()` and `as.data.frames()`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation for explanation.)
#' 
#' @param null ***How should null data points be output?***
#' 
#' Default is `"NA2dot"`.
#' 
#' Must be a single character string, [partially matching][partialMatchng] `"NA2dot"`, `"dot2NA"`, `'charNA2dot"`, or `"asis"`.
#' `"NA2dot"` means all `NA` values are converted to `"."`; `"dot2NA` means all `"."` are converted to `NA`; `charNA2dot` means `NA` values
#' in `character` vectors are converted to `NA`, but not in other atomic types; `"asis"` means either `NA` or `"."` values may print, depending
#' on what is in the field.
#' 
#' @param drop ***Should single fields be extracted from the `data.table`?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'   
#' @rdname humTable
pullFields <- function(humdrumR, fields = selectedFields(humdrumR), dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis'), drop = FALSE) {
    checks(humdrumR, xhumdrumR)
    checks(drop, xTF)
    dataTypes <- checkTypes(dataTypes, 'pullFields')
    
    null <- match.arg(null)
    
    
    humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
    selectedTable <- humtab[ , fields, with = FALSE]
    

    # selectedTable[] <- selectedTable[ , lapply(.SD, 
    #                                            \(field) {
    #                                                if (is.list(field)) {
    #                                                   field[filter] <- lapply(field[filter], '[', i = 0)
    #                                                } else {
    #                                                   field[filter] <- NA
    #                                                }
    #                                                field
    #                                            })]
    
    # decide how NA/null values are shown
    fieldTypes <- fields(humdrumR)[ , Type[match(colnames(selectedTable), Name)]]
    fieldTypes <- lapply(as.list(fieldTypes), 
                         \(fieldType) if (fieldType == 'Data') humtab$Type else rep(c(Interpretation = 'I', Formal = 'I',
                                                                                      Structure = 'D',Reference = 'G')[fieldType],
                                                                                      fieldType, length = nrow(selectedTable)))
    
    selectedTable[] <- Map(naDots, selectedTable, types = fieldTypes, null = null)
    
    # return
    if (length(fields) == 1L && drop) selectedTable[[1]] else selectedTable
    
}


#' Tabulate current fields in a [humdrumR corpus][humdrumRclass]
#'
#' Use the `fields()` function list the current fields in 
#' a [humdrumRclass] object.
#' It returns a [data.table()] with five columns:
#' 
#' + `Name`
#' + `Class`
#' + `Type`
#'   + `Data`/`Structure`/`Interpretation`/`Formal`/`Reference`/`Grouping`
#' + `Selected`,
#'   + A `logical` indicating which fields are [selected][selectedFields()].
#' + `GroupedBy`
#'   + A `logical` indicating which, if any fields, are currently grouping the data.
#'
#' 
#' @param fieldTypes ***Which types of fields to list.***
#' 
#' Shows all fields by default.
#' 
#' Must be a `character` vector. Legal options are `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`,
#' and `"Grouping"`.
#' You can also pass `"selected"` to extract only the [selected fields][selectedFields()].
#' Types can be [partially matched][partialMatching]---for example, `"S"` for `"Structure"`.
#'   
#' @rdname humTable
#' @export
fields <- function(humdrumR, fieldTypes = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping', 'selected')) { 
  #

  checks(humdrumR, xhumdrumR)
  fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', 'fields')
            
  humdrumR@Fields[Type %in% fieldTypes | ('selected' %in% fieldTypes & Selected == TRUE)]

}

fieldsInExpr <- function(humtab, expr) {
  ## This function identifies which, if any,
  ## fields in a humtable are referenced in an expression (or rhs for formula).
  if (is.humdrumR(humtab)) humtab <- getHumtab(humtab)          
  
  namesInExpr(colnames(humtab), expr)
}



fillFields <- function(humdrumR, from = 'Token', to, where = NULL) {
    humtab <- getHumtab(humdrumR, 'GLIMDd')
    
    where <- if (!is.null(where)) eval(where, envir = humtab) else TRUE
        
    for (field in to) {
        if (class(humtab[[from]]) == class(humtab[[field]])) {
            
            hits <- is.na(humtab[[field]]) & where
            
            
            humtab[[field]][hits] <- humtab[[from]][hits]
        }
    }
    
    putHumtab(humdrumR) <- humtab
    
    update_Null(humdrumR)
    
}





####################################################-
## Selected fields ----
####################################################-


#' The "selected" fields of a [humdrumR object][humdrumRclass]
#' 
#' Every `humdrumR` object will have, at any given time, one or more of its
#' fields "selected."
#' Selected fields are show in the console printout.
#' The first selected field is also passed as the hidden `.` variable in calls to `with`, `within`,
#' `mutate`, `summarize`, or `reframe`.
#' 
#' @details
#' 
#' The currently selected fields are shown when a [humdrumR object][humdrumRclass] prints on the console.
#' At the bottom of the printout, the selected fields are also marked by an `*`.
#' The currently selected fields can also be queried directly using the `selectedFields()` function.
#' The selected fields can be set by calls to the tidyverse `select()` function.
#' As a shorthand, the keywords `"Data"`, `"Structure"`, `"Interpretation"`, `"Reference"`, or `"Formal"` can
#' be used to select *all* fields of each [field types][fields()].
#' 
#' The selected fields play an important role in defining/redefining "null" data.
#' Whenever new fields are selected, they are checked for `NA` values or null
#' tokens---`"."`, `"*"`, `"!"`, or `"="`---and the `Null` structural field is set to `TRUE`
#' wherever they appear.
#' The `Type` field is also set to `"d"` wherever `Null == TRUE & Type == 'D'`.
#' If more than one field is selected, all fields must be null to be marked as such.
#' 
#' 
#' 
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' selectedFields(humData)
#' 
#' humData |> select(Spine, Record, Token) |> selectedFields()
#'
#' humData |> select(Structure)
#'
#' @export
selectedFields <- function(humdrumR) fields(humdrumR)[Selected == TRUE]$Name

pullSelectedField <- function(humdrumR, dataTypes = 'D', drop = TRUE, null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    fieldInTable <- pullSelectedFields(humdrumR, dataTypes = dataTypes, null = null)[ , 1L, with = FALSE]
    
    if (drop) fieldInTable[[1]] else fieldInTable
    
}

pullSelectedFields <- function(humdrumR, dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {

    pullFields(humdrumR, selectedFields(humdrumR), dataTypes = dataTypes, null = null)
    
}

selectFields <- function(humdrumR, fields) {
    checks(humdrumR, xhumdrumR)
    
    fields <- fieldMatch(humdrumR, fields, 'selectFields', 'fields')
    
    fieldTable <- humdrumR@Fields
    
    fieldTable[ , Selected := Name %in% fields]
    
    humdrumR@Fields <- fieldTable #data.table::copy(fieldTable)
    
    humdrumR <- update_humdrumR.humdrumR(humdrumR, field = fields)
    humdrumR
}



pullPrintable <- function(humdrumR, fields, dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis'), useTokenGLIM = TRUE, collapse = TRUE){
    
    fields <- pullFields(humdrumR, fields, dataTypes = dataTypes, null = null)
    
    fields[] <- lapply(fields, 
                               \(field) {
                                   if (is.list(field)) return(list2str(field))
                                   if (is.token(field)) field <- field@.Data
                                   field[] <- as.character(field)
                                   if (is.matrix(field)) {
                                       matrix[] <- str_pad(c(matrix), width = max(nchar(matrix)))
                                       field <- paste0('[', do.call('paste', as.data.frame(matrix)),  ']')
                                   }
                                   field <- gsub('\t\t*', '', field)
                                   field
                                   
                               }) # need[] in case there are matrices
    if (!collapse) return(fields)                  
      
    field <- do.call('paste', c(fields, list(sep = ', ')))
    ## fill from token field
    TokenType <- pullFields(humdrumR, c('Token', 'Type'), dataTypes = dataTypes, null = 'NA2dot')
    if (useTokenGLIM && any(grepl('[GLIM]', dataTypes))) {
        # humtab[, !Type %in% c('D', 'd')]
        field[TokenType$Type %in% c('G', 'L', 'I', 'M')] <- TokenType[Type %in% c('G', 'L', 'I', 'M'), Token]
        
    } else {
        # always get ** exclusive
        field[grepl('\\*\\*', TokenType$Token)] <- TokenType[grepl('\\*\\*', Token), Token]
    }
    
    field <- stringr::str_replace(field, '^\\.[ ,.]*\\.$', '.')
    
    field[field == ''] <- "'"
    
    
    data.table(Printable = field)
   
}



printableSelectedField <- function(humdrumR, dataTypes = 'D', null =  c('charNA2dot', 'NA2dot', 'dot2NA', 'asis'), useTokenGLIM = TRUE) {
    printableField <- pullPrintable(humdrumR, fields = selectedFields(humdrumR), dataTypes = dataTypes, 
                                    null = null, useTokenGLIM = useTokenGLIM, collapse = TRUE)
    
    humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
    humtab$Printable <- printableField[[1]]
    putHumtab(humdrumR) <- humtab
    
    updateFields(humdrumR)
    
    
    
}


getGroupingFields <- function(humdrumR, .by = NULL, withFunc = 'within.humdrumR') {
    if (is.null(.by)) {
        fields(humdrumR)[GroupedBy == TRUE]$Name 
    } else {
        fieldMatch(humdrumR, unlist(.by), callfun = withFunc)
    }
    
}



####################################################-
# Print methods ----
#########################################################-

setMethod('show', signature = c(object = 'humdrumR'),
          function(object) {
                    print.humdrumR(object)
                    return(invisible(NULL))
          })


#' @export
print.humdrumR <- function(humdrumR, view = humdrumRoption('view'), 
                           dataTypes = if (view %in% c('score', 'humdrum')) "GLIMDd" else 'D', 
                           firstAndLast = TRUE, 
                           screenWidth = options('width')$width - 10L,
                           null = humdrumRoption('nullPrint'), 
                           syntaxHighlight =  humdrumRoption('syntaxHighlight'),
                           maxRecordsPerFile = if (length(humdrumR) == 1L) 800L else humdrumRoption('maxRecordsPerFile'), 
                           maxTokenLength = humdrumRoption('maxTokenLength'), 
                           censorEmptyRecords = humdrumRoption('censorEmptyRecords')) {
    
  checks(humdrumR, xhumdrumR)
  dataTypes <- checkTypes(dataTypes, "print_humdrumR")
  checks(view, xcharacter & xlen1 & xplegal(c('humdrum', 'score', 'table', 'tibble', 'data.frame')))
  checks(firstAndLast, xTF)
  checks(screenWidth, xwholenum & xpositive)
  checks(null, xcharacter & xlen1 & xlegal(c('NA2dot', 'dot2NA', 'charNA2dot', 'asis')))
  checks(syntaxHighlight, xTF)
  checks(maxRecordsPerFile, xwholenum & xpositive)
  checks(maxTokenLength, xwholenum & xpositive)
  checks(censorEmptyRecords, xwholenum & xpositive)
  
    
  
  if (is.empty(humdrumR)) {
    cat("\nEmpty humdrumR object\n")
    return(invisible(humdrumR))
  }
  
  Npieces <- npieces(humdrumR)
  Nfiles <- nfiles(humdrumR) # needs to be done before firstLast indexing
  if (Npieces > 2L && firstAndLast) humdrumR <- humdrumR[c(1, Npieces)]
  
  if (view == 'score') return(print_score(humdrumR, maxRecordsPerFile))
  
  tokmat <- if (view == 'humdrum') {
      tokmat_humdrum(humdrumR, dataTypes, null = null, censorEmptyRecords = censorEmptyRecords)
  } else {
      tokmat_humtable(humdrumR, dataTypes, null = null)
  }
  
  print_tokmat(tokmat, Nmorefiles = Npieces - length(humdrumR), maxRecordsPerFile, maxTokenLength, 
               screenWidth = screenWidth, showCensorship = view == 'humdrum', syntaxHighlight = syntaxHighlight)

  
  
  if (length(humdrumR) > 1L) {
      cat('\n')
      cat('\thumdrumR corpus of', num2print(Npieces), 'pieces',
          if (Nfiles != Npieces) c('(in', num2word(Nfiles), plural(Nfiles, 'files)', 'file)')))
      
      
      if (anySubcorpora(humdrumR)) {
          subnames <- namesSubcorpora(humdrumR)
          cat(' (', num2word(length(subnames)), 
              " subcorpora: ", 
              paste(subnames, collapse = ', '), 
              ')', sep = '')
      }
      cat('.\n')
      
  }
  
  ## Fields
  showFields(humdrumR)
  
  
  return(invisible(humdrumR))
  
}


tokmat_humtable <- function(humdrumR, dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    structureFields <- c('Piece', 'Filename', 'Spine', 'Path', 'Record', 'Stop')
    selectedFields <- selectedFields(humdrumR)
    tokenTable <- pullPrintable(humdrumR, unique(c(structureFields, selectedFields)),
                                dataTypes = dataTypes,
                                null = null, useTokenGLIM = FALSE, collapse = FALSE) 
   
    
    
    setcolorder(tokenTable, unique(c(structureFields, selectedFields)))
    
    lastPiece <- max(tokenTable$Piece)
    Filenames <- tokenTable[ , unique(Filename)]
    if (!'Filename' %in% selectedFields) tokenTable[ , Filename := NULL]
    if (!'Path' %in% selectedFields && all(tokenTable$Path == 0, na.rm = TRUE)) tokenTable[, Path := NULL]
    if (!'Stop' %in% selectedFields && all(tokenTable$Stop == 1, na.rm = TRUE)) tokenTable[, Stop := NULL]
    

    tokmat <- do.call('cbind', as.list(tokenTable))
    tokmat[is.na(tokmat)] <- '<NA>'
    
    # syntax highlighting prep
    types <- fields(humdrumR)[, setNames(Type, Name)[colnames(tokenTable)]]
    types <- c(Data = 'D', Interpretation = 'I', Structure = 'N', 'Reference' = 'G', Formal = 'I')[types]
    types[colnames(tokenTable) %in% selectedFields & types == 'N'] <- 'n' # don't italicize structural fields that are selected
    syntax <- col(tokenTable)
    syntax[] <- types[syntax]
    syntax[tokmat == '.' | is.na(tokmat)] <- 'd'
    
    
    # add header/footer
    tokmat <- rbind(names(tokenTable), tokmat, names(tokenTable))
    # syntax <- rbind(syntax[1, ], syntax, syntax[1, ])
    syntax <- rbind('N', syntax, 'N')
    
    # output
    list(Tokmat = tokmat,  
         Piece = c(NA, tokenTable$Piece, NA), 
         Record = c(NA, tokenTable$Record, NA), 
         Filenames = Filenames,
         Global = logical(nrow(tokmat)), 
         Syntax = syntax)
    

    
}

tokmat_humdrum <- function(humdrumR, dataTypes = 'GLIMDd', censorEmptyRecords = Inf, null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {

  humdrumR <- printableSelectedField(humdrumR, dataTypes = dataTypes, null = null)
    
  tokmat <- as.matrix(humdrumR, dataTypes = dataTypes, padPaths = 'corpus', padder = '')
  
  # removes "hanging stops" like "a . ." -> "a"
  # if (anyStops(humdrumR)) tokmat[] <- stringr::str_replace(tokmat, '( \\.)+$', '')
  #
  if (censorEmptyRecords < Inf) tokmat <- censorEmptySpace(tokmat, collapseNull = censorEmptyRecords)
  
  Filenames <- getHumtab(humdrumR)[ , unique(Filename)]
  Piece   <- gsub('\\..*$', '', rownames(tokmat))
  NRecord <- gsub('^[0-9]*\\.', '', rownames(tokmat))
  
  global <- stringr::str_detect(tokmat[ , 1], '^!!')
  
  
  # syntax highlighting
  syntax <- array(parseTokenType(tokmat, E = TRUE), dim = dim(tokmat))
  
  # add rownames (records)
  tokmat <- cbind(paste0(NRecord, ':  '), tokmat)
  syntax <- cbind('N', syntax)
  
  #output
  list(Tokmat = tokmat, 
       Piece = Piece, 
       Record = NRecord, 
       Filenames = Filenames, 
       Global = global, 
       Syntax = syntax)
  
}

print_tokmat <- function(parsed, Nmorefiles = 0, maxRecordsPerFile, maxTokenLength,
                         screenWidth = options('width')$width - 10, showCensorship = TRUE, syntaxHighlight = TRUE) {
   
    tokmat <- parsed$Tokmat
    Record <- parsed$Record
    Piece <- parsed$Piece
    global <- parsed$Global
    Filenames <- parsed$Filenames
    syntax <- parsed$Syntax
    
    ## censor lines beyond maxRecordsPerFile

    # 
    uniqRec <- tapply_inplace(Record, Piece, seq_along)
    censored <- ifelse(length(unique(Piece)) == 1L | Piece != max(Piece, na.rm = TRUE),
                       uniqRec >  maxRecordsPerFile,
                       uniqRec <= (max(uniqRec[Piece == max(Piece, na.rm = TRUE)], na.rm = TRUE) - maxRecordsPerFile)) & !is.na(Piece)
    tokmat <- tokmat[!censored, , drop = FALSE]
    syntax <- syntax[!censored, , drop = FALSE]
    global <- global[!censored]
    
    
    
    ## Trim and align columns, and collopse to lines
    tokmat[!global, ] <- trimTokens(tokmat[!global, , drop = FALSE], maxTokenLength = maxTokenLength)
    
    lines <- padColumns(tokmat, global, screenWidth, if (syntaxHighlight) syntax)
    
    starMessage <- attr(lines, 'message')
  
    # put in Piece indicators
    maxwidth <- min(screenWidth, sum(attr(lines, 'trueColWidth')))
    firsts <- tapply(seq_along(lines), Piece[!censored], min)
    lasts <- tapply(seq_along(lines), Piece[!censored], max)
    
    if (showCensorship) {
        # records of first and last non-censored lines of each file
        
        #  censored ranges (if any)
        ranges <- tapply(Record[censored], factor(Piece)[censored], 
                         \(nr) {
                             if (length(nr) > 1L) paste0(nr[1], '-', nr[length(nr)], ':') else paste0(nr[1], ':')
                         })
        anycensored <- !is.na(ranges)
        ranges <- ranges[anycensored]
        # ranges[is.na(ranges)] <- ":"
        # align : (colon)
        if (length(ranges)) {
            line_colon <- stringr::str_locate(lines, ':')[ , 'start'] - 7L
            range_colon <- stringr::str_locate(ranges, ':')[ , 'start']
            largest_colon <- max(line_colon, range_colon)
            lines <- paste0(strrep(' ', largest_colon - line_colon), lines)
            ranges <- paste0(strrep(' ', largest_colon - range_colon), ranges)
        }
        
        # 
        # 
        
        
        ranges <- stringr::str_pad(ranges, width = maxwidth, pad = ':', side = 'right')
        if (syntaxHighlight) ranges <- textstyle(ranges, style = 'italic')
        ranges <- paste0('\n', ranges)
        
        if (any(anycensored)) lines[lasts[-length(lasts[anycensored])]] <- paste0(lines[lasts[-length(lasts[anycensored])]], ranges[-length(ranges)])
        
        
        if (length(unique(Piece)) > 1L && tail(ranges, 1) != '') {
            lines[tail(firsts, 1)] <- paste0(gsub('^\n', '', tail(ranges, 1)), '\n', lines[tail(firsts, 1)])
        }
        
        # put filenames in
     
    }
    lines[firsts] <- paste0(stringr::str_pad(paste0(' vvv ', Filenames, ' vvv '), width = maxwidth, pad = '#', side = 'both'), '\n', lines[firsts])
    lines[lasts] <- paste0(lines[lasts], '\n', stringr::str_pad(paste0(' ^^^ ', Filenames, ' ^^^ '), width = maxwidth, pad = '#', side = 'both'))
    
    # if any lines have been censored due to screen size, put message at the end
    if (!is.null(starMessage)) {
        lines[length(lines)] <- paste0(lines[length(lines)], '\n', smartPadWrap(starMessage, maxwidth + 1L))
    }
    
    ##
    if (Nmorefiles > 0L) {
        
        message <- c('',
                     paste0('\t\t', .glue("({num2print(Nmorefiles)} <more pieces|other piece>...)", ifelse = Nmorefiles > 1L)),
                     '')
        lines <- append(lines, message, after = tail(firsts, 1) - 1L)
    }
    
    cat(lines, sep = '\n')
    
}


print_score <- function(humdrumR, maxRecordsPerFile) {
    selectedFields <- selectedFields(humdrumR)
    
  humdrumR <- printableSelectedField(humdrumR, dataTypes = 'GLIMDd', null = 'NA2dot', useTokenGLIM = TRUE)
    
  lines <- as.lines(humdrumR[1])
  
  toHNP(lines, .glue("Viewing the '{paste(selectedFields, collapse = '/')}' <fields|field>",
                     "using the PLUGIN.", ifelse = length(selectedFields) > 1L))
  
  invisible(NULL)
}


censorEmptySpace <- function(tokmat, collapseNull = 10L) {
    if (nrow(tokmat) < 50) return(tokmat)
    null <- apply(matrix(grepl('^\\.( \\.)*$', tokmat) | grepl('^=', tokmat), nrow = nrow(tokmat)), 1, all, na.rm = TRUE)
    
    chunks <- segments(!null)
    
    # newRN <- unlist(tapply(rownames(tokmat), chunks, \(x) if (length(x) <= collapseNull) x else c(x[1], paste0(x[2], '-', tail(x, 1)))))
    
    tokmat <- tapply(seq_len(nrow(tokmat)), chunks, simplify = FALSE, 
                                   \(i) {
                                       nbars <- sum(grepl('^=', tokmat[i, 1]))
                                       
                                       if (nbars == 1 || length(i) <= collapseNull) return(tokmat[i, , drop = FALSE])
                                       
                                       fill <- if (nbars > 0L) {
                                           bars <- tokmat[i, , drop = FALSE][grepl('^=', tokmat[i, 1]), 1]
                                           barnums <- stringr::str_extract(bars, '[0-9a-zA-Z]+')
                                           base <- strrep('=', length(bars))
                                           
                                           if (any(!is.na(barnums))) {
                                               barnums <- barnums[!is.na(barnums)]
                                               barnums <- paste(unique(c(barnums[1], tail(barnums, 1))), collapse = '-')
                                           } else {
                                               barnums <- ""
                                           }
                                           newRN <- paste(rownames(tokmat[i[c(2, length(i))], , drop = FALSE]), collapse = '-')
                                           
                                           paste0(base, barnums)
                                           
                                       } else {
                                           newRN <- paste(rownames(tokmat[i[c(2, length(i))], ]), collapse = '-')
                                           
                                           strrep('.', length(i) - 1)
                                       }
                                       newRN <- c(rownames(tokmat)[i[1]], newRN)
                                       # rbind(tokmat[i[1], , drop = FALSE], paste0('(', fill, ')'))
                                       tokmat <- rbind(tokmat[i[1], , drop = FALSE], fill)
                                       rownames(tokmat) <- newRN
                                       
                                       tokmat
                                       
                     })
    tokmat <- do.call('rbind', tokmat)
    rownames(tokmat) <- stringr::str_replace(rownames(tokmat), '-[0-9]+\\.', '-') # replace redundant fileNumber
    
    tokmat
}

padColumns <- function(tokmat, global, screenWidth = options('width')$width - 10L, syntax) {
    # This function takes a token matrix
    # and pads each token with the appropriate number of spaces
    # such that the lines will print as nicely aligned columns.
    # it also adds "***" where there are too many columns to fit on the screen.
    # Finally it collapses each row to a single line.

    toklen <- nchar(tokmat)
    
    lenCol <- sapply(as.data.frame(toklen[!global, ]), max) + 2L
    # lenCol <- apply(toklen[!global, ], 2, max) + 2L
    
    screen <- cumsum(lenCol) <= screenWidth
    lenCol <- lenCol[screen]
    tokmat <- tokmat[ , screen, drop = FALSE]

    # do padding
    tokmat[!global,  ] <- padder(tokmat[!global, , drop = FALSE], lenCol)
    tokmat[global, 1L] <- padder(tokmat[global, 1L], lenCol[1]) # column 1 is record number!
    
    # colorize
   
    if (!is.null(syntax)) tokmat[] <- syntaxHighlight(tokmat, syntax)

    
    # collapse to lines
    tokmat[global, -1:-2L] <- ''
    lines <- do.call('paste0', as.data.frame(tokmat))
    lines[global] <- gsub('\t', ' ', lines[global])

    longGlobal <- global & nchar(lines) > screenWidth
    longColumn <- !screen
    

    if (any(longColumn) || any(longGlobal)) {
        message <- if (any(!screen)) {
            lines[!global] <- paste0(lines[!global], '    ***')
            lines[ global] <- stringr::str_trunc(lines[global], width = sum(lenCol) + 7L, ellipsis = '***')
            
            paste0('(***', num2word(sum(!screen)), plural(sum(longColumn), ' spines/paths ' ,' spine/path '),  'not displayed due to screen size***)')
        } else {
            lines[ global] <- stringr::str_trunc(lines[global], width = screenWidth + 7L, ellipsis = '***')
            paste0('(***', num2word(sum(longGlobal)), ' global ', plural(sum(longGlobal), 'comments ' ,'comment '),  'truncated due to screen size***)')
        }
       
        attr(lines, 'message') <- message
        # lines[length(lines)] <- paste0(lines[length(lines)], '\n', message)
    } 
    
    
    attr(lines, 'trueColWidth') <- lenCol + 1L
    lines
    
}



showFields <-  function(humdrumR) {
          # This function is used to produce the human readable 
          # list fields used by print_humdrumR
          fields <- fields(humdrumR)[Type == 'Data' | Selected == TRUE | GroupedBy == TRUE]

          ## prep for printing
          selected <- fields$Selected
          
          fields[ , Name := paste0(ifelse(Selected, '*', ' '), Name)]
          fields[ , Name := stringr::str_pad(Name, width = max(nchar(Name)), side = 'right')]
          
          fields[ , Print := paste0(Name, ' :: ', Class)]

          
          ## Print fields
          cat('\n')
          fields[(Type == 'Data' | Selected == TRUE) & Type != 'Grouping',
                  { cat('  ', Type[1], 'fields:', '\n\t        ')
                    cat(Print, sep = '\n\t        ')
                    cat('\n')
                            }, 
                  by = Type]

          
          # grouping fields
          groupFields <- fields[GroupedBy == TRUE]
          
          if (nrow(groupFields)) {
              ngroups <- nrow(unique(getHumtab(humdrumR, 'D')[ , gsub('[ *]*', '', groupFields$Name), with = FALSE]))
              
              groupFields[ ,
                           { cat('   Grouping fields: (', num2print(ngroups), plural(ngroups, ' groups', ' only one "group"...'), ')\n\t        ', sep = '')
                               cat(Print, sep = '\n\t        ')
                               cat('\n')
                           }]
          }
          
          # cat('\t\tFields: ', paste(fieldprint, collapse = '\n\t\t        '), '\n', sep = '')
          
          invisible(fields)
}
