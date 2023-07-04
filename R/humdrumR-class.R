# Humdrum table  ----

#' Humdrum tables (and their "fields")
#' 
#' In the [humdrumR] package, the fundamental data structure is called a **humdrum table**.
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
#' Even multiple-stops---tokens separated by spaces---are broken onto
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
#' When first created by a call to [readHumdrum()], every
#' humdrum table has at least nineteen fields: one data field (`Token`), two interpretation 
#' fields (`Tandem` and `Exclusive`), three formal fields, and thirteen structure fields. Additional
#' formal, interpretation, or reference fields
#' *may* be present depending on the content of the humdrum file(s), and you can create additional data fields
#' by using [within.humdrumR()][withinHumdrum], [mutate.humdrumR()], or other functions.
#' 
#' ### Data fields:
#' 
#' *Data* fields are used to describe individual data points
#' in humdrum data (as opposed to groups of points). 
#' Every humdrum table starts with a data
#' field called **Token**, which
#' contains `character` strings representing the original strings read from the humdrum files. 
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
#'         + Which token in a multi-stop token, numbered starting from `1`.
#'         + In files with no multi-stops, the `Stop` field is all `1`s.
#'         + This field is always `NA` when `Global == TRUE`.
#'     + `Global` :: `logical`
#'         + Did the token come from a global record (as opposed to a local record)?
#'         + When `Global == TRUE`, the `Spine`, `Path`, and `Stop` fields are always `NA`.
#' + *Token info*:
#'     + `Type` :: `character`
#'         + What type of record is it? 
#'             + `"G"` = global comment. 
#'             + `"L"` = local comment
#'             + `"I"` = interpretation
#'             + `"M"` = measure/barline 
#'             + `"D"` = non-null data 
#'             + `"d"` = null data
#'             + `"E"` = exclusive interpretation
#'             + `"S"` = spine-control tokens (`*^`, `*v`, `*-`)
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
#' ### Grouping fields:
#' 
#' Grouping fields are special fields which may be created by calls to [group_by()].
#' These fields are deleted by calls to [ungroup()].
#' These fields are generally hidden/inaccessible to users.
#' 
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
#' + the selected fields are all `character` data *and* the token is a one of `c(".", "!", "!!", "=", "*", "**")`; **or**
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
#' 
#' @section Reshaping:
#' 
#' Breaking the complex syntax of humdrum data into the "flat" structure of a humdrum table, with every single token on one line
#' of a `data.table`, makes humdrum data easier to analyze.
#' Of course, thanks to the structure fields, we can easily
#' regroup and reform the original humdrum data or use the structure of the data (like spines) in our analyses.
#' However, in some cases, you might want to work with humdrum data in a different structure or "shape."
#' `humdrumR` has several options for ["collapsing"][collapseHumdrum()] tokens within humdrum tables, 
#' ["cleaving"][cleave()] different parts of the data into new fields,
#' or otherwise [reshaping humdrum data][humCoercion] into basic R data structures you might prefer.
#' 
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' fields(humData)
#' 
#' getHumtab(humData)
#' getHumtab(humData, dataTypes = 'D')
#' 
#' @family {Core humdrum data representation}
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
# humdrumR S4 class ----
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
#' Test is an object/variable is a `humdrumR` dataset using `is.humdrumR()`.
#' 
#' The most important part of a `humdrumR` object is the 
#' [humdrum table][humTable] it holds within it;
#' In essence, a `humdrumR` object is simply a wrapper around its
#' humdrum table, which helps users to
#' to visualize, [filter][subset.humdrumR()], [summarize][humSummary], and [manipulate][withinHumdrum]
#' the table in a variety of ways.
#' 
#' Basic information about the size and shape of `humdrumR` objects can be
#' obtained with calls to [nrecord, npiece, length, ncol, etc.][humSize].
#' More detailed summary information can be obtained with the humdrumR [corpus summary functions][humSummary].
#' `humdrumR` data can also be coerced to more basic R data types using [as.matrix, as.data.frame, etc.][humCoercion].
#' A number of helpful functions are also defined to "reshape" or reorganize the
#'  data (e.g., [cleave()], [rend()], [collapseHumdrum()]).
#' 
#' The most powerful features of [humdrumR] are the tools it gives you to...
#' 
#' + Print a readable view of the data in shorthand/curtailed humdrum syntax.
#' + Filter `humdrumR` data, using [subset.humdrumR()] and the standard `R` [indexing operators][base::Extract]: `[]` and `[[]]`.
#' + Apply arbitrary commands to [humtable][humTable] fields using the [with(in)Humdrum][withinHumdrum] routines,
#' and related tidyverse commands (`mutate()`, `summarize()`, etc.).
#' 
#' @section Viewing your Data: 
#' 
#' If you type the name of an object on the R command line, R will "print" the object in the console.
#' This can be also be done explicitly using the `humdrumR` method of the [print()] function.
#' The humdrumR print method contains a number of arguments, which can be manipulated directly in calls to `print()`.
#' However, the `humdrumR` print argument draws its defaults from global `humdrumR` options
#' which can be also controlled with the [humdrumR()] function.
#' Generaly, changing print options with [humdrumR()] is the best option, as once you change them,
#' all automatic printing will follow your new settings---this means you can avoid explicitly calling [print()].
#'
#' When printing, only the [selected fields][selectedFields] in the data are shown.
#' 
#' #### View types
#' 
#' There are three options for how to view `humdrumR` data, which can be toggled between 
#' using the `view` argument to [print()] or [humdrumR()].
#' Since `view` is the first argument to the [humdrumR()] function, you can switch between views by simply calling
#' `humdrumR('humdrum')` or `humdrumR('score')` or `humdrumR('table')`.
#' The options are:
#' 
#' + `"humdrum"` (the default): prints a humdrum-syntax score representation, with record numbers enumerated at the left side.
#' 
#'   When the `Token` field is selected, and you haven't applied any [filters][subset.humdrumR()],
#'   this view will show your original data as it was in the files you [read][readHumdrum()].
#'   
#'   If more than one field is selected, they are pasted together in the printed output.
#'   
#' + `"table"`: prints a view of the underlying [humdrum table][humTable].
#' 
#'   In addition to the [selected fields][selectedFields], the `Piece`, `Spine`, and `Record` fields will
#'   always print in the output table, as well as `Path` and `Stop` if any paths/stops are present.
#'   
#' + `"score"`: will (attempt to) show a rendered score of (the first piece) in your data.
#' 
#'   This view is only likely to work correctly if you are using Rstudio and connected to the internet.
#'   Score rendering is accomplished using Craig Sapp's [Humdrum Notation Plugin](https://plugin.humdrum.org/).
#'    
#' For `table` and `humdrum` views, if there are more than one pieces in the object, 
#' the beginning of the first piece is printed, followed by the end of the last piece;
#' How *much* of the first/last piece are printed is controlled by the `maxRecordsPerFile` print argument.
#' Both of these views also highlight the output with different colors representing different types of data tokens:
#' this can be disabled using `syntaxHighlight = FALSE`.
#' For `score` view, only the first piece is shown.
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
#' @slot Context A [data.table] with two columns: `Open` and `Close`.
#' Each row represents a contextual window to apply to the data.
#'
#' @param humdrumR ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#'
#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData 
#' 
#' humData |> print(view = 'table')
#' 
#' @inheritParams humdrumR
#' @name humdrumRclass
#' @seealso {The actual data is stored in the internal [humdrum table][humTable].
#' You can set printing options globally using the [humdrumR()] function.}
#' @family Core humdrum data representation
#' @aliases humdrumRS4 humData
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'data.table',
                   Files = 'list',
                   Fields = 'data.frame',
                   LoadTime = 'POSIXct',
                   Context = 'data.table'
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
            .Object@Context   <- data.table(Open = integer(0), Close = integer(0))
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
    
    
    field <- pullPrintable(x, selectedFields(x), dataTypes = dataTypes)[[1]]
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


#################################-
# Humtable manipulation and access ####
###############################-



#' Access a Humdrum Table
#' 
#' The `getHumtab()` function extracts the humdrum table from a [humdrumR object][humdrumRclass].
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
#' Note that `"I"` also grabs `"E"` (exclusive) and `"S"` (spine-control) tokens.
#' 
#' 
#' @rdname humTable
#' @export
getHumtab <- function(humdrumR, dataTypes = "GLIMDd") {
          humtab <- humdrumR@Humtable
          
          checks(humdrumR, xhumdrumR)
          dataTypes <- checkTypes(dataTypes, 'getHumtab')
          
          
          if (length(setdiff(c('G', 'L', 'I', 'M', 'D', 'd', 'S', 'E'), dataTypes))) {
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

update_humdrumR <- function(hum, Exclusive, Null, ...) UseMethod('update_humdrumR')
update_humdrumR.humdrumR <- function(hum,  Exclusive = TRUE, Dd = TRUE , ...) {
    humtab <- getHumtab(hum, 'GLIMDd')
    humtab <- update_humdrumR.data.table(humtab, Exclusive, Dd, ...)
    
    putHumtab(hum, overwriteEmpty = c('d')) <- humtab
    hum
}
update_humdrumR.data.table <- function(hum, Exclusive = TRUE, Dd = TRUE, ...) {
    
    if (Exclusive) hum <- update_Exclusive(hum, ...)
    if (Dd) hum <- update_Dd(hum, ...)
    hum
    
}


#
update_Exclusive <- function(hum, ...) UseMethod('update_Exclusive')
update_Exclusive.humdrumR <- function(hum, ...) {
    humtab <- getHumtab(hum, 'ID')
    
    fields <- selectedFields(hum)
    putHumtab(hum, overwriteEmpty = 'ID') <- update_Exclusive.data.table(humtab, fields)
    
    hum
}
update_Exclusive.data.table <- function(hum, fields = 'Token', ...) {
    
    exclusiveFields <- Reduce('|', lapply(paste0('^Exclusive\\.', fields), stringi::stri_detect_regex, str = colnames(hum)))
    if (any(exclusiveFields)) {
        hum[ , Exclusive := do.call('paste0', hum[ , exclusiveFields, with = FALSE])]
    }
    
    
    hum
}

#
update_Dd <- function(hum, field, ...) UseMethod('update_Dd')
update_Dd.humdrumR <- function(hum, field = selectedFields(hum),  allFields = FALSE, ...) {
    
    if (allFields) field <- fields(hum, 'D')$Name
    humtab <- getHumtab(hum, 'GLIMDd')
    putHumtab(hum, overwriteEmpty = "GLIMDd") <- update_Dd.data.table(humtab, field = field)
    hum
}
update_Dd.data.table <- function(hum, field = 'Token', ...) {
    null <- nullFields(hum, field)
    
    hum$Type[hum$Type %in% c('d', 'D')] <-  ifelse(null[hum$Type %in% c('d', 'D')], 'd', 'D')
    hum
}

is.nullToken <- function(tokens) {
    if (is.list(tokens)) {
        lengths(tokens) == 0L
    } else {
        is.na(tokens) | tokens %in% c('*', '=', '!', '!!', '.', '**')
    }
}

nullFields <- function(hum, fields, reduce = '&') {
    nulls <- lapply(hum[ , fields, with = FALSE], is.nullToken)
    Reduce('&', nulls)
}



####################################################-
# Fields ----
####################################################-


## Manipulating the @Fields slot ----

checkFieldTypes <- function(types, argname, callname, includeSelected = TRUE) {
    valid <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping', if (includeSelected) 'selected')
    types <- matched(types, valid, nomatch = types)
    checks(types, #argname = argname, 
           xcharacter & xmaxlength(7) & xplegal(valid))
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
    
    humtab[ , Exclusive.Token := Exclusive] # changes this in place
    
    setorder(fieldTable, Type, Class)
    fieldTable[ , Selected := as.integer(Name == 'Token')]
    fieldTable[ , GroupedBy := FALSE]
    fieldTable[ , Complement := FALSE]
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
    humtab <- getHumtab(humdrumR)
    
    fieldTable <- humdrumR@Fields
    fieldTable <- fieldTable[Name %in% colnames(humtab)] # removes fields that don't exist in humtab
    
    new <- setdiff(colnames(humtab), fieldTable$Name)
    new <- new[!grepl('^_complement_|Exclusive\\.', new)]
    if (length(new)) {
        fieldTable <- rbind(fieldTable, 
                            data.table(Name = new, 
                                       Type = 'Data', 
                                       Class = '_tmp_', 
                                       Selected = 0L, 
                                       GroupedBy = FALSE,
                                       Complement = FALSE))
    }
    
    
    fieldTable$Class <- sapply(humtab[ , fieldTable$Name, with = FALSE], 
                               fieldClass)
    fieldTable$Complement <- paste0('_complement_', fieldTable$Name) %in% colnames(humtab)
    
    setorder(fieldTable, Type, Name)
    setcolorder(humtab, fieldTable$Name)
    
    if (length(new) && selectNew) fieldTable[ , Selected := match(Name, new, nomatch = 0L)]
    humdrumR@Fields <- fieldTable
    
    humdrumR
    
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
    
    update_Dd(humdrumR)
    
}

## Querying fields ----







#' List fields in a humdrumR object
#' 
#' Use the `fields()` function to list the current fields in 
#' a [humdrumRclass] object.
#' 
#' @section Querying Fields:
#' 
#' The `fields()` function takes a [humdrumR object][humdrumRclass]
#' and returns a [data.table()], with each
#' row describing an available field in the humdrum table.
#' The output table has five columns:
#' 
#' + `Name`
#'   + The field name.
#' + `Class`
#'   + The [class()] of the data in the field.
#' + `Type`
#'   + The type of field (described above). 
#'     Can be `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`, or `"Grouping"`.
#' + `Selected`,
#'   + A `logical` indicating which fields are [selected][selectedFields()].
#' + `GroupedBy`
#'   + A `logical` indicating which, if any, fields are currently [grouping][withinHumdrum] the data.
#'
#' Using the [names()] function on a [humdrumR object][humdrumRclass] will
#' get just the field names, the same as `fields(humData)$Name`.
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
#' @seealso {To actually extract fields from [humdrumR data], see the [pull()] family of functions.}
#' @rdname humTable
#' @export
fields <- function(humdrumR, fieldTypes = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping', 'selected')) { 

  checks(humdrumR, xhumdrumR)
  fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', 'fields')
            
  humdrumR@Fields[Type != 'Complement' & (Type %in% fieldTypes | ('selected' %in% fieldTypes & Selected == TRUE))]

}

fieldsInExpr <- function(humtab, expr) {
  ## This function identifies which, if any,
  ## fields in a humtable are referenced in an expression (or rhs for formula).
  if (is.humdrumR(humtab)) humtab <- getHumtab(humtab)          
  
  namesInExpr(colnames(humtab), expr)
}

fieldMatch <- function(humdrumR, fieldnames, callfun = 'fieldMatch', argname = 'fields') {
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


#' @rdname humTable
#' @export
names.humdrumR <- function(humdrumR) fields(humdrumR)[ , Name]



## Selecting fields ----


#' The "selected" fields of a [humdrumR object][humdrumRclass]
#' 
#' Every `humdrumR` object will have, at any given time, one or more of its
#' fields "selected."
#' The selected fields are the fields that are shown when a 
#' [humdrumR object][humdrumRclass] prints on the console.
#' (At the bottom of the printout, the selected fields are also marked by a `*`.)
#' The selected fields can also be queried directly using the `selectedFields()` function, or 
#' by inspecting the output of [fields()].
#' The selected fields also play other important roles in `humdrumR` 
#' (see details).
#' 
#' @details
#'
#' The "selected" fields play an important role in [humdrumR].
#' In addition to controlling what [fields()] you "see" in the console printout, 
#' the select fields are the fields that many [humdrumR][humdrumR] functions will automatically
#' apply themselves to.
#' For example, if you call [ditto()], [tally()], or [kern()] on a [humdrumR data object][humdrumRclass],
#' these functions will be applied the selected field(s).
#' (However, most such functions are only applied to the *first* selected field, 
#' if there is more than one; see their own manuals for details.)
#' The first selected field is also passed as the hidden `.` variable in calls to [with()/within()/,
#' mutate()/summarize()/reframe()][withinHumdrum]---so if you don't remember what fields are selected
#' you can just put a `.`!
#' 
#' The selected fields also have a role in identifying "null" data.
#' Whenever new fields are selected, their data tokens are checked for `NA` values or null
#' tokens (`"."`).
#' Anywhere where *all* the selected fields are null, the `Type` field is updated to `"d"`;
#' wherever *any* field is **not** null, the `Type` field is updated to `"D"`.
#' Many functions ignore `d` (null data) tokens by default, so selecting fields can be a way controlling which data you
#' want to analyze and which you don't.
#'
#'
#' ## Selecting fields
#' 
#' Fields can be selected using the tidyverse `select()` function, 
#' and can use any of `select()`'s [special select features][dplyr::select()].
#' If you call `select()` with no argument, the original `Token` field is selected by default.
#'
#' If you use `select()` with a numeric selections or, like `select(1:3)`, fields are numbered in the (row) order
#' shown in call to [fields()].
#' Fields are always sorted first by `Type` (`Data` first), then by name.
#' If you provide a `fieldTypes` argument, the numeric selection is reduced to only those fields you choose,
#' matching with the row-numbers you'd see if you call [fields(humData, fieldTypes = ...)][fields()].
#' So, for example, `select(humData, 1:3, fieldTypes = 'Structure')` will select the first three structural fields.
#' You can also simply provide the keywords `"Data"`, `"Structure"`, 
#' `"Interpretation"`, `"Reference"`, or `"Formal"`
#' to select *all* fields of each [field type][fields()].
#' 
#' Note that when you call `select()` on [humdrumR data][humdrumRclass], 
#' the selected field(s) change **in place**,
#' meaning that the selection changes *even if you don't (re)assign the output*!
#' 
#' @param humdrumR,.data ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Which fields to output.***
#' 
#' If no arguments are provided, the `Token` field is selected.
#' 
#' These arguments can be any combination of `character` strings, numbers, or symbols used
#' to match fields in the `humdrumR` input using [tidyverse][dplyr::select()] semantics.
#' 
#' Unlike in tidyverse `select()`, field names can be [partially matched][partialMatching].
#' You can also include `character` strings [partially matching][partialMatching] 
#' `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"` or `"Grouping"`,
#' which will select all fields of those types (see [fields()] for further explanation).
#' 
#' @param fieldTypes ***Which field types are available for numeric selecting?***
#' 
#' Defaults to `"any"`, so all fields are counted for numeric selection.
#' 
#' Must be a `character` vector. Legal options are `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`,
#' `"Grouping"`, and `"any"`, corresponding to the `Type` column in the output of [fields()].
#' Types can be [partially matched][partialMatching]---for example, `"S"` for `"Structure"`.
#' 
#'
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' # see what is selected
#' selectedFields(humData)
#' 
#' # change selection
#' humData |> select(Spine, Record, Token) |> selectedFields()
#'
#' humData |> select(Structure)
#' 
#' humData |> select(4)
#' humData |> select(1:3, fieldTypes = 'Structure')
#' 
#' # effect of selection
#' 
#' humData |> select(Token) |> tally()
#' humData |> select(Spine) |> tally()
#'
#' @seealso {Use [fields()] to see what fields are available, and how they are ordered.
#' To actually *extract* fields, see [pullFields()].}
#' @export
selectedFields <- function(humdrumR) {
    fields(humdrumR)[Selected > 0L][order(Selected)]$Name
} 

selectFields <- function(humdrumR, fields = 'Token') {
    checks(humdrumR, xhumdrumR)
    
    fields <- fieldMatch(humdrumR, fields, 'selectFields', 'fields')
    
    fieldTable <- humdrumR@Fields
    
    fieldTable[ , Selected := match(Name, fields, nomatch = 0L)]
    
    # humdrumR@Fields <- #data.table::copy(fieldTable)
    
    humdrumR <- update_humdrumR.humdrumR(humdrumR, field = fields)
    humdrumR
}



#' @rdname selectedFields
#' @aliases select
#' @export 
select.humdrumR <- function(.data, ..., fieldTypes = "any") {
 
    exprs <- rlang::enexprs(...)
    fields <- if (length(exprs) == 0L) {
      'Token'
    } else {
      tidyselect_humdrumRfields(.data, exprs, fieldTypes, 'select.humdrumR')
    }
    selectFields(.data, fields)
}

#### select helpers ----

tidyselect_humdrumRfields <- function(humdrumR, exprs, fieldTypes, callname) {
  fields <- fields(humdrumR)
  
  types <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping')
  
  fieldTypes <- if ('any' %in% tolower(fieldTypes)) {
    types
  } else {
    fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', callname, includeSelected = FALSE)
  }
 
  
  # select by field type (character string only, partially matched)
  typeSelections <- lapply(exprs, 
                           \(expr) {
                             if (is.character(expr)) {
                               type <- types[pmatch(expr, types, nomatch = 0L)]
                               if (length(type)) fields[Type == type, Name]
                             }
                           })
  
  fieldExprs <- exprs[lengths(typeSelections) == 0L]
  
  # select by field names
  
  fieldExprs <- lapply(fieldExprs, withinExpression,
                       stopOnHit = TRUE,
                       applyTo = c('atomic', 'symbol'),
                       predicate = \(Type, Class) (Type == 'atomic' && Class =='character') | Type == 'symbol',
                       func = \(exprA) {
                           
                           name <- if (exprA$Type == 'symbol') exprA$Head else exprA$Args[[1]]
                           
                           pname <- pmatch(name, fields$Name)
                           if (!is.na(pname)) name <- fields$Name[pname]
                           
                           if (exprA$Type == 'symbol') exprA$Head <- name else exprA$Args[[1]] <- name
                           
                           exprA
                           
                       })
  
  fieldSelections <- local({
    fields <- fields[order(!Type %in% fieldTypes)]
    options <- fields$Name
    expr <- rlang::expr(c(!!!(fieldExprs)))
    tried <- try(options[tidyselect::eval_select(expr, setNames(options, options), strict = FALSE)], silent = TRUE)
    if (class(tried) == 'try-error') .stop('In a call to {callname}(), you can ONLY provide the names of humdrumR fields,',
                                           'or special tidyverse "select features" expressions involving those fields.',
                                            "You can't provide more complex/arbitrary expressions.")
    
    
    tried
  })
 
  ##
  selections <- union(fieldSelections, unlist(typeSelections))
  
  
  if (length(selections) == 0L) {
    exprs <- do.call('harvard', c(lapply(exprs, rlang::as_label), conjunction = '', quote = TRUE))
    .stop("The <expressions {exprs} don't|expression {exprs} doesn't> match any {harvard(fieldTypes, 'or')} fields in your humdrumR data.",
          ifelse = length(exprs) > 1)
  }
  
  selections
}



printableSelectedField <- function(humdrumR, 
                                   dataTypes = 'D', useToken = 'GLIM',
                                   null =  c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    dataTypes <- checkTypes(dataTypes, 'printableSelectField')
    useToken <- checkTypes(useToken, 'printableSelectField', 'useToken')
    
    printableField <- pullPrintable(humdrumR, fields = selectedFields(humdrumR), 
                                    dataTypes = dataTypes,  useToken = useToken,
                                    collapse = TRUE)
    
    humtab <- getHumtab(humdrumR, dataTypes = 'GLIMDd')
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


## Extracting ("pull") fields ----




pullFields <- function(humdrumR, fields, dataTypes = 'D', 
                       null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis'),
                       drop = FALSE) {

    
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
                                                                                      Structure = 'D', Reference = 'G')[fieldType],
                                                                                    fieldType, length = nrow(selectedTable)))
    
    selectedTable[] <- Map(naDots, selectedTable, types = fieldTypes, null = null)
    
    # return
    if (length(fields) == 1L && drop) selectedTable[[1]] else selectedTable
    
}



pullSelectedField <- function(humdrumR, dataTypes = 'D', drop = TRUE, null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    fieldInTable <- pullSelectedFields(humdrumR, dataTypes = dataTypes, null = null)[ , 1L, with = FALSE]
    
    if (drop) fieldInTable[[1]] else fieldInTable
    
}

pullSelectedFields <- function(humdrumR, dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    
    pullFields(humdrumR, selectedFields(humdrumR), dataTypes = dataTypes, null = null)
    
}


pullPrintable <- function(humdrumR, fields, 
                          dataTypes = 'D', null = 'NA2dot',
                          useToken = c('G', 'L', 'I', 'M', 'S', 'E'), collapse = TRUE){
    
    fieldTable <- pullFields(humdrumR, union(fields, c('Token', 'Type')), dataTypes = dataTypes, null = if (collapse) 'dot2NA' else null)
    
    Exclusives <- Filter(length, lapply(fieldTable[ , fields, with = FALSE], getExclusive))
    tandems <- unlist(unique(lapply(fieldTable, getTandem)))
    
    # change fields to character
    fieldTable[ , (fields) := lapply(fields, 
                                     \(field) {
                                         field <- fieldTable[[field]]
                                         
                                         if (is.list(field)) {
                                             field <- list2str(field)
                                         } else {
                                             if (is.token(field)) field <- field@.Data
                                             field[] <- as.character(field)
                                         }
                                         
                                         if (is.matrix(field)) {
                                             matrix[] <- str_pad(c(matrix), width = max(nchar(matrix)))
                                             field <- paste0('[', do.call('paste', as.data.frame(matrix)),  ']')
                                         }
                                         field <- gsub('\t\t*', '', field)
                                         field
                                         
                                     })] 
    if (!collapse) return(fieldTable[ , fields, with = FALSE])                  
    
    # field <- do.call('.paste', c(fieldTable[, fields, with = FALSE], list(sep = '')))
    field <- Reduce(\(a, b) {
        ifelse(fieldTable$Type %in% c('I', 'M', 'd', 'S') & a == b, a, .paste(a, b, sep = ''))
        
    }, fieldTable[, fields, with = FALSE])
    
    Type <- fieldTable$Type
    ## Do we need to grab any interpretations from the Token field?
    if (length(useToken) && any(grepl(captureRE(useToken), dataTypes))) {
        # humtab[, !Type %in% c('D', 'd')]
        fill <- Type %in% useToken & (is.na(field) | is.nullToken(field))
        
        if (length(tandems)) {
            tandemRE <- knownInterpretations[Name %in% tandems, RE]
            fill[Type == 'I'] <- fill[Type == 'I']  & Reduce('|', lapply(tandemRE, 
                                                                         stringi::stri_detect_regex, 
                                                                         str = fieldTable$Token[Type == 'I'] ))
        }
        field[fill] <- fieldTable$Token[fill]
    } 
    
    if (length(Exclusives)) {
        field[Type == 'E'] <- paste0('**', do.call('paste', c(Exclusives[fields], list(sep = '**'))))
    }
    
    

    field <- stringr::str_replace(field, '^\\.[ ,.]*\\.$', '.')
    field <- naDots(field, null, fieldTable$Type)
    field[field == ''] <- "'"
    
    
    data.table(Printable = field)
    
}

### Exported pull functions ----

#' Extract field(s) from [humdrumR data][humdrumRclass]
#' 
#' Individual fields from the [humdrum table][humTable] can be extracted using `pull()`.
#' Multiple fields can be extracted using `pull_data.frame()`, `pull_data.table`, or `pull_tibble()`
#' ---the resulting data.frames are a column-subset of the humdrum table.
#' You can also use the `$` operator to extract a single field, just like `pull()`.
#'
#'
#' @details
#' 
#' The functions `pull()`, `pull.data.xxx()`, `pull.tibble()`, and `$` are 
#' the "escape hatch" to pull your 
#' data out of the [humdrumR data world][humdrumRclass] into "normal" R.
#' Use the `pull()` function or the `$` to access the actual vector content of a single field.
#' The other functions *always* return a `data.frame`/`data.table`/`tibble`, even if it has only one column.
#' 
#' Choose which field(s) to return using the `...`, `var`, or `name` arguments.
#' The `var` and `...` options use tidyverse style select semantics (see [select()][selectedFields]).
#' If no fields are indicated, the data's [selected fields][selectedFields] are pulled; in the case of `pull()` and `$`,
#' only the *first* selected field is pulled.
#' 
#' The `dataTypes` argument controls which *types* of data are pulled---by default, 
#' only non-null data (`Type == "D"`) is pulled.
#' The `$` operator can only grab non-null data.
#' 
#' The `null` argument controls how null data is returned, with four options: 
#' 
#' + `"NA2dot"` means all `NA` values are converted to `"."`; note that this will cause all output to be coerced to `character`.
#' + `"dot2NA"` means all `"."` are converted to `NA`.
#' + `"charNA2dot"` means `NA` values in `character` vectors are converted to `NA`, but not in other atomic types.
#' + `"asis"` means either `NA` or `"."` values may print, depending on what is in the field.
#' 
#' Note that `pull_tibble()` won't work if you don't independently load the `tibble` (or `tidyverse`) package---
#' i.e., call `library(tibble)`.
#' 
#' @param humdrumR,.data,x ***HumdrumR data.***
#' 
#' Must be a [humdrumR data object][humdrumRclass].
#' 
#' @param ... ***Which fields to output.***
#' 
#' If no arguments are provided, the object's [selected fields][selectFields] are pulled.
#' 
#' These arguments can be any combination of `character` strings, numbers, or symbols used
#' to match fields in the `humdrumR` input using [tidyverse][dplyr::select()] semantics.
#' 
#' Unlike in tidyverse `select()`, field names can be [partially matched][partialMatching].
#' You can also include `character` strings [partially matching][partialMatching] 
#' `"Data"`, `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"` or `"Grouping"`,
#' which will select all fields of those types (see [fields()] for further explanation).
#' 
#' @param var ***Which field to output.***
#' 
#' Defaults to `selectedFields(humdrumR)[1]`.
#' 
#' Must be either a single `character` string or `symbol` which [partially matches][partialMatching] 
#' a field name, or a single whole-number, which selects the field by the row index of the [fields()] output.
#' If a negative number is provided, nth-to-last index is used---for example, `-1` would grab the last field.
#'   
#' @param dataTypes ***Which types of humdrum record(s) to include.***
#' 
#' Only non-null data tokens (`"D"`) are returned by default.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation for explanation.)
#' 
#' @param null ***How should null data points be output?***
#' 
#' Default is `"charNA2dot"`.
#' 
#' Must be a single character string, [partially matching][partialMatchng] `"NA2dot"`, `"dot2NA"`, `'charNA2dot"`, or `"asis"`.
#' 
#' @seealso {To know what fields are available to pull, use [fields()].
#'           To know what fields are selected---the default fields to pull---use [selectedFields()].}
#' @examples
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' humData |> pull(Token)
#' humData$Token
#' 
#' humData |> pull_data.table(Token, Spine)
#' humData |> pull_tibble(everything())
#' 
#' @name pullHumdrum
#' @export
pull_data.table <- function(humdrumR, ..., dataTypes = 'D', null = 'charNA2dot') {
    checks(humdrumR, xhumdrumR)
    dataTypes <- checkTypes(dataTypes, 'pull_data.table')
    checks(null, xplegal(c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')))
    
    exprs <- rlang::enexprs(...)
    fields <- if (length(exprs)) {
        tidyselect_humdrumRfields(humdrumR, exprs, fieldTypes = 'any', callname = 'pull.humdrumR')
    } else {
        selectedFields(.data)
    }
    
    pullFields(humdrumR, fields, dataTypes = dataTypes, null = null)
}

#' @name pullHumdrum
#' @export
pull_data.frame <- function(humdrumR, ..., dataTypes = 'D', null = 'charNA2dot') {
    as.data.frame(pull_data.table(humdrumR, ..., dataTypes = dataTypes, null = null))
}

#' @name pullHumdrum
#' @export
pull_tibble <- function(humdrumR, ..., dataTypes = 'D', null = 'charNA2dot') {
    tibble::as_tibble(pull_data.table(humdrumR, ..., dataTypes = dataTypes, null = null))
}

#' @rdname pullHumdrum
#' @aliases pull
#' @export
pull.humdrumR <- function(.data, var, dataTypes = 'D', null = 'asis') {
    
    checks(.data, xhumdrumR)
    dataTypes <- checkTypes(dataTypes, 'pull.humdrumR', argname = 'var')
    checks(null, xplegal(c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')))
    
    var <- rlang::ensym(var)
    
    if (is.atomic(var)) checks(var, ((xcharacter | xwholenum) & xlen1))
    
    if (is.symbol(var)) var <- as.character(var)
    if (is.numeric(var)) {
        fields <- fields(.data)$Name
        if (var == 0 | abs(var) > length(fields)) .stop("Your numeric 'var' argument is larger",
                                                        "than the number of fields available to pull.")
        if (var < 0) var <- max(0, length(fields) + 1 + var)
        var <- fields[var]
    } else {
        var <- fieldMatch(.data, var, 'pull')
    }
    
    pullFields(.data, fields = var, dataTypes = dataTypes, null = null)[[1]]
    
    
}

#### $ methods ----

#' @rdname pullHumdrum
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




#### pull helpers ----


naDots <- function(field, null, types) {
    if (null == 'asis') return(field)
    na <- is.na(field)
    
    nulltoken <- c(G = '!!', I = '*', L = '!', d = '.', D = '.', M = '=', E = '**', S = '*')[types]
    
    
    if (null == 'dot2NA') {
        na <- na | field == nulltoken
        field[na] <- NA
    } else {
        if (null == 'charNA2dot') na <- is.character(field) & na
        field[na] <- nulltoken[na]
    }
    
    field   
}


####################################################-
# Print methods ----
#########################################################-

setMethod('show', signature = c(object = 'humdrumR'),
          function(object) {
                    print.humdrumR(object)
                    return(invisible(NULL))
          })

#' @rdname humdrumRclass
#' @export
print.humdrumR <- function(humdrumR, view = humdrumRoption('view'), 
                           dataTypes = humdrumRoption('dataTypes'), 
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
  
  showWindows(humdrumR)
  
  return(invisible(humdrumR))
  
}


tokmat_humtable <- function(humdrumR, dataTypes = 'D', null = c('charNA2dot', 'NA2dot', 'dot2NA', 'asis')) {
    structureFields <- c('Piece', 'Filename', 'Spine', 'Path', 'Record', 'Stop')
    selectedFields <- selectedFields(humdrumR)
    tokenTable <- pullPrintable(humdrumR, unique(c(structureFields, selectedFields)),
                                dataTypes = dataTypes, null = 'charNA2dot',
                                useToken = FALSE, collapse = FALSE) 
   
    
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

  # humdrumR <- printableSelectedField(humdrumR, dataTypes = dataTypes, null = null)
    
  tokmat <- as.matrix(humdrumR, dataTypes = union(c('S', 'E'), dataTypes), padPaths = 'corpus', padder = '')
  
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
    
    lines <- padColumns(tokmat, global, maxTokenLength, screenWidth, if (syntaxHighlight) syntax)
    
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
    
  humdrumR <- printableSelectedField(humdrumR, dataTypes = 'GLIMDd', null = 'NA2dot')
    
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

padColumns <- function(tokmat, global, maxTokenLength, screenWidth = options('width')$width - 10L, syntax) {
    # This function takes a token matrix
    # and pads each token with the appropriate number of spaces
    # such that the lines will print as nicely aligned columns.
    # it also adds "***" where there are too many columns to fit on the screen.
    # Finally it collapses each row to a single line.

    toklen <- nchar(tokmat)
    
    lenCol <- sapply(as.data.frame(toklen[!global, ]), max) + 2L
    # lenCol <- apply(toklen[!global, ], 2, max) + 2L
    
    if (sum(lenCol) < (screenWidth - 5L)) {
        # if there is extra space, fill it (up to maxTokenLength)
        lenCol[-1] <- pmin(lenCol[-1] + ((screenWidth - sum(lenCol)) %/% (length(lenCol) - 1L)), 
                           maxTokenLength)
    }
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
          fields <- fields(humdrumR)[Type == 'Data' | Selected > 0 | GroupedBy == TRUE]

          ## prep for printing
          fields[ , Name := paste0(ifelse(Selected, '*', ' '), Name)]
          fields[ , Name := stringr::str_pad(Name, width = max(nchar(Name)), side = 'right')]
          
          fields[ , Print := paste0(Name, ' :: ', Class)]
          
          ## Print fields
          cat('\n')
          fields[(Type == 'Data' | Selected > 0L) & Type != 'Grouping',
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


showWindows <- function(humdrumR) {
    windowFrame <- humdrumR@Context
    
    if (nrow(windowFrame)) {
        overlap <- any(windowFrame$Depth > 1L)
        
        cat('   With ', num2print(nrow(windowFrame)),
        if (overlap) ' (overlapping)', ' contextual windows:\n', 
        sep = '')
        
        lengths <- windowFrame[ , Close - Open + 1L]
        quants <- round(c(Shortest = min(lengths), Median = median(lengths), Longest = max(lengths)))
        if (length(unique(quants)) == 1L) {
            cat("\t\tAll windows length ==", quants[1],'\n')
        } else {
            quants <- setNames(unique(quants), tapply(names(quants), quants, paste, collapse = '/'))
            cat(paste0('\t\t', 
                        str_pad(paste0(names(quants), ' '), 
                                max(nchar(names(quants)) + 1L)), 
                        'length == ', 
                       quants), 
                 sep = '\n')
        }
    }
}
