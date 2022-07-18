#' Humdrum Tables
#' 
#' In the [humdrumR] package, the fundamental data structure is called a **Humdrum Table**.
#' A humdrum table encodes all the information in a collection of one or more humdrum-syntax files
#' as a single [data.table][data.table::data.table] 
#' (A `data.table` is an "enhanced" version of R's standard [data.frame]).
#' Humdrum tables are stored "inside" every [humdrumRclass] object that you will work with, and various `humdrumR`
#' functions allow you to study or manipulate the them.
#' If you want to directly access the humdrum table within a [humdrumRclass] object, use the [getHumtab] function.
#' 
#' 
#' In a humdrum table, each row represents a single "token"
#' in the original humdrum data. Even multistops---tokens separated by spaces---are broken onto
#' their own rows. Meanwhile, each column in the humdrum table represents a single
#' piece of information associated with each token, which we call a **field**.
#' Throughout this documentation, you should keep in mind that a "token" refers
#' to a *row* in the humdrum table while a "field" refers to a *column*:
#' 
#' * Token = Row
#' * Field = Column
#' 
#' # Fields:
#' 
#' There are five types of fields in a humdrum table: 
#' 
#' 1. Data fields
#' 2. Structure fields
#' 3. Interpretation fields
#' 4. Formal fields
#' 5. Reference fields
#' 
#' When first created by a call to [readHumdrum] every
#' humdrum table has at least nineteen fields: one data field (`Token`), two interpretation 
#' fields (`Tandem` and `Exclusive`), three formal fields, and fifteen structure fields. Additional
#' interpretation or reference fields
#' may be present depending on the content of the humdrum file(s), and users can create additional data fields
#' by [assigning to the object][humAssignment].
#' 
#' ### Data fields:
#' 
#' Data fields are used to describe individual data points
#' in humdrum data (as opposed to groups of points). 
#' Every humdrum table starts with a data
#' field called **Token**, which
#' contains character strings representing the original strings read from the humdrum files. 
#' Users can create as many additional data fields as they like. Every call to
#' [withinHumdrum] generates one or N new data fields named {Result1, Result2, ..., ResultN}. 
#' These fields can be renamed using the `$<-` operator.
#' 
#' 
#' ### Structure fields:
#' 
#' Every humdrum table starts with fifteen Structure fields,
#' which describe where each data token was "located" in the original humdrum data: which file, which spine, which record, etc.
#' See the vignette on humdrum syntax to fully understand the terms here.
#' 
#' + *File info*:
#'     + `Filename` :: `character`
#'         + The unique name of the humdrum file. This may include an appended path 
#'           if more than one file with the same name were read from different directories (see the [readHumdrum] docs).
#'     + `Filepath` :: `character`
#'         + The full file name (always includes its full path).
#'     + `Label` :: `character`
#'         + A label specified during the call to [readHumdrum], associated with a particular
#'          `readHumdrum` "REpath-pattern." If no label was specified, patterns are just labeled `"_n"`, where "`n`" is the 
#'          number of the pattern. (Labels can also be created when [merging two humdrumR objects][humMerge].)
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
#'           (If there are no spine path splits, the `Path` field is all zeros.)
#'         + This field is always `NA` when `Global == TRUE`. 
#'         + Read the [humdrum columns documentation][humColumns] for a more thorough explanation
#'           of spine paths.
#'     + `Column` :: `integer`
#'         + The tab-delineated column in the humdrum file---irrespective of Spine/Paths---, numbered starting from `1`.
#'         + This field is always `NA` when `Global == TRUE`. 
#'         + See this [explanation of columns in humdrumR][humColumns].
#'     + `Record` :: `integer`
#'         + The record (i.e., line) number in the original file.
#'     + `NData` :: `integer`
#'         + The *data* record enumeration in the file, starting from `1`.
#'     + `Stop` :: `integer`
#'         + Which token in a multistop token, numbered starting from `1`.
#'         + In files with no multistops, the `Stop` field is all `1`s.
#'         + This field is always `NA` when `Global == TRUE`.
#'     + `Global` :: `logical`
#'         + Did the token come from a global record (as opposed to a local record)?
#'         + When `Global == TRUE`, the `Spine`, `Column`, and `Stop` fields are always `NA`.
#' + *Token info*:
#'     + `Type` :: `character`
#'         + What type of record is it? 
#'             + `"D"` = non-null data 
#'             + `"d"` = null data
#'             + `"I"` = interpretation
#'             + `"M"` = measure/barline 
#'             + `"L"` = local comment
#'             + `"G"` = global comment. 
#'             + `"P"` = null "non-tokens" (see the [humdrumR columns][humColumns] documentation for an explanation.)
#'     + `Null` :: `logical` 
#'         + Is the [active][humdrumR] data field null? 
#'         + See the detailed discussion below, in the section of this documentation called "Null Data."
#'     + `Filter` :: `logical`
#'         + Has this record/token been [filtered out][filterHumdrum]? 
#'         
#' 
#' 
#' 
#' ### Interpretation fields:
#' 
#' Interpretation fields describe interpretation metadata in the humdrum file(s).
#' Humdrum interpretations are tokens that "carry forward" to data points after them, unless cancelled out by a
#' subsequent interpretation. (See the humdrum syntax vignette for a detailed explanation.)
#' *All* humdrum data must have an *exclusive* interpretation
#' so humdrum tables always have an `Exclusive` (:: `character`) field indicating the
#' exclusive interpretation associated with each token/row of the [active][humdrumR] field.
#' 
#' Humdrum data may, or may not, include additional *tandem* interpretations. A universal rule for parsing
#' tandem intepretations is impossible, because A) tandem interpretations can "overwrite" each other and B)
#' users can create their own tandem interpretations. The best we can do in all cases is 
#' identify *all* tandem interpretations that have appeared previously in the spine
#' (counting most recent first). All these previous interpretations are encoded in a single
#' character string in the `Tandem` field. 
#' If working with non-standard intrepretations, users can parse the `Tandem` field using the
#' [getTandem] function. 
#' If no tandem interpretations occur in a file, the `Tandem` field is full of empty strings (`""`).
#' 
#' Fortunately, many tandem interpretations are widely used and standardized, and these 
#' interpretations are known by `humdrumR`. Recognized interpretations (such as `*clefG4` and `*k[b-]`)
#' are automatically parsed into their own fields by a call to [readHumdrum].
#' See the [readHumdrum] documentation for more details.
#' 
#' 
#' ### Formal fields:
#' 
#' Formal fields indicate musical sections, or time windows within
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
#' Humdrum tables *always* include three formal fields related to barlines:
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
#'       These include the `"-"` in the common "implied barline token `"=-"`,
#'      repeat tokens (like `"=:||"`), and also any *explicit* bar numbers.
#'     + Note that the `Bar` field always enumerate *every* bar record, while
#'      measure-number labels in humdrum data (which appear in the `BarLabel` field) may
#'      do weird things like skipping numbers, repeating numbers, or having suffixes (e.g., `"19a"`).
#'      If no barline tokens appear in the file, `BarLabel` is all empty strings (`""`).
#' 
#' ### Reference fields:
#' 
#' Reference fields describe any *Reference Records*
#' in the humdrum data. Every reference record (records beginning `"!!!"`) in any
#' humdrum file in a corpus read by [readHumdrum] is parsed into a field named
#' by the reference code: `"XXX"` in `"!!!XXX"`.
#' Reference tokens are all identical throughout
#' any humdrum piece. If a reference code appears in one file but not another, the field is
#' `NA` in the file which does not have the code. If no reference records appear in any
#' files read by [readHumdrum], no reference fields are created.
#' 
#' Examples of common reference records are `"!!!COM:"` (composer) and `"!!!OTL:"` (original title).
#' Any humdrum data with these records will end up having `COM` and `OTL` fields in its humdrum table.
#' 
#' # Null Data:
#' 
#' In humdrum syntax, there is no requirement that every spine-path contains data
#' in every record. Rather, spines are often padded with *null tokens*.
#' In some cases, entire records may be padded with null tokens.
#' Each type of humdrum record uses a different null token:
#' 
#' + *Intepretation*: `*`
#'  *Comment*: `!`
#' + *Barline*: `=`
#' + *Data*: `.`
#' 
#' Null tokens in a humdrum table are identified in the logical `Null` field.
#' The `Null` field is set when a humdrum table is created (by [readHumdrum]) and is updated everytime 
#' a new [active][humdrumR] field is set.
#' `Null` is set to `TRUE` wherever, either 
#' 
#' + the active field is `character` data and the token is a single `"."`, `"!"`, `"="`, or `"*"`;
#' + the active field is `NA` (including `NA_character_`).
#' 
#' In parallel to the `Null` field, null *data* tokens (`"."`) are identified as their own record type: `"d"`.
#' All updates/changes to the `Null` field are also propogated to the `Type` field---i.e., setting `Type == d` wherever
#' a data record is `Null`.
#' This is important/useful because [withinHumdrum] routines are, by default, only applied to `"D"` data, ignoring `"d"`.
#' 
#' Whenever you [print][humPrint] or [export][writeHumdrum] a [humdrumRclass] object, null data in the active field 
#' (i.e., `Null == TRUE`) print as `"."`.
#' Thus, if you are working with numeric data, with `NA` values, these `NA` values will print as `"."`.
#' 
#' 
#' 
#' # Reshaping:
#' 
#' Breaking the complex syntax of humdrum data into the "flat" structure of a humdrum table, with every single token on one line
#' of a `data.table`, makes humdrum data easier to analyze.
#' Of course, thanks to the structure fields, we can easily
#' regroup and reform the original humdrum data or use the structure of the data (like spines) in our analyses.
#' However, in some cases, you might want to work with humdrum data in a different structure or "shape."
#' HumdrumR has several options for ["collapseing"][humShape] tokens within humdrum tables,
#' or otherwise [reshaping humdrum data][humCoercion] into data formats/structures you might prefer.
#' 
#' 
#'
#' @name humTable
NULL


#' Spines vs Paths vs Columns 
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
#' 
#' In [humdrumR], spines, columns, and spine paths work like this.
#' First of all, we actually assume a slightly more strict version of the humdrum syntax:
#' we assume that all the spines which appear at the beginning of a file (headed with exlusive interpretations
#' like `"**kern"`) can never merge into each other. Thus, a humdrum file read into `humdrumR`
#' must not end with fewer columns than it starts.
#' Spine merges (`"*v"`) can only happen within spine paths that originally split off the same spine.
#' This extra-strict specification of spine paths in the humdrum syntax is, fortunately, something that has been
#' informally followed in most humdrum datasets.
#' 
#' Our strict spine-path definition makes everything work fairly simply: 
#' Within a piece, the spines which appear at the beginning of the piece are the "true" spines through the rest of the piece, numbered
#' from left to right, starting from `1L`.
#' For each local token, the value in the `Spine` field is an integer indicating which of these
#' "true" spines it belongs to---global tokens have a `NA` value in their `Spine` field, because they are considerd to not belong to any spine.
#' Any spine path splits (`"*^"` from the main spines form subspines, which we call **Paths**.
#' Every spine's paths are numbered, from right to left, starting from `0L`.
#' A spine with no splits will have all `0L`s in its `Path` field.
#' 
#' @section Columns:
#'
#' It is very useful to sometimes turn humdrum data into a true two dimensional structure, with no ragged edges.
#' (This always requires removing global records.)
#' In order to do this, while maintaining a sensible relationship between spine which have spine paths,
#' [humRead] automatically *pads* humdrum data into a complete, non-ragged 2d table.
#' For instance, given this file
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
#' [humRead] pads the file as so:
#' 
#' ```
#' **kern   _P       **kern
#' A        _P       E
#' *^       _P       *
#' A        C        E
#' G        B        D
#' *v       *v       *
#' A        _P       C        
#' *-       _P       *-
#' 
#' ##########################################
#' 1        1        2        Spine
#' 0        1        0        Path
#' 1        2        3        Column
#' ```
#' (In this example, the `Spine`, `Path`, and `Column` values are shown below the data.)
#' The `"_P"` tokens stand for "padded path."
#' This appraoch assures that every **Spine** is a contiguous block of tokens, of constant width.
#' In most humdrumR use cases, these padding tokens (and the `Column` field) can be safely ignored.
#' 
#' @section Corpus padding:
#'
#' [humRead] automatically pads spine paths *within pieces*.
#' However, as mentioned above, there is also (sometimes) a need to pad across pieces, in order
#' to create a logical, clean 2d structure.
#' Consider this example, with humdrum data from two pieces:
#' 
#' + *Piece 1*:
#'    ```
#'    **kern   **kern  **kern
#'    E        D       C
#'    D        .       .
#'    C        C       E
#'    *-       *-      *-
#'    ```
#' + *Piece 2*:
#'    ```
#'    **kern   **kern
#'    A        A
#'    .        B
#'    C        C
#'    *-       *-
#'    ```
#' 
#' In this example, we have two pieces, one with three spines, the other with two.
#' There is no way to squish these two pieces into one regular 2d table.
#' But we *could* pad any missing columns, as so:
#' 
#' + *Piece 1*:
#'    ```
#'    **kern   **kern  **kern
#'    E        D       C
#'    D        .       .
#'    C        C       E
#'    *-       *-      *-
#'    ```
#' + *Piece 2*:
#'    ```
#'    **kern  **kern   _C
#'    A        A       _C
#'    .        B       _C
#'    C        C       _C
#'    *-       *-      _C
#'    ```
#'
#' The function `alignColumns` is used to achieve just this effect.
#' In this example, the `"_C"` token stands for "padded column."
#' 
#' The presence of spine paths makes padding columns across pieces a bit more complicated.
#' What `alignColumns` will do, is match up all pieces in a corpus so that
#' every **Spine**/**Path** field pair allign in the same column.
#' Here is an example, with its paths already padded: 
#' 
#' + *Piece 1*:
#'    ```
#'    **kern      _P        **kern
#'    A           _P        E
#'    B           _P        D
#'    *^          _P        *
#'    A           C         E
#'    G#          B         E
#'    *v          *v        *
#'    A           _P        E
#'    *-          _P        *-
#'    #################################################
#'    1           1         2         Spine
#'    0           1         0         Path
#'    1           2         3         Column
#'    ```
#' + *Piece 2*:
#'    ```
#'    **kern     **kern    _P
#'    A          E         _P
#'    *          *^        _P
#'    G#         D         F
#'    A          C         E
#'    *          *v        *v
#'    E          D         _P
#'    *-         *-        _P        
#'    #################################################
#'    1          2         2        Spine
#'    0          0         1        Path
#'    1          2         3        Column
#'    ```
#' We have two pieces, each with two spines,
#' but in the first piece, the first spine splits, while in the second piece, the
#' second spine splits. Thus, the padded output will have four columns:
#' 
#' + *Piece 1*:
#'    ```
#'    **kern    _P        **kern   _C
#'    A         _P        E        _C
#'    B         _P        D        _C
#'    *^        _P        *        _C
#'    A         C         E        _C
#'    G#        B         E        _C
#'    *v        *v        *        _C
#'    A         _P        E        _C
#'    *-        _P        *-       _C 
#'    ###########################################################
#'    1         1         2        2         Spine
#'    0         1         0        1         Path
#'    1         2         3        4         Column
#' + *Piece 2*:
#'    ```
#'    **kern    _C        **kern   _P
#'    A         _C        E        _P
#'    *         _C        *^       _P
#'    G#        _C        D        F
#'    A         _C        C        E
#'    *         _C        *v       *v
#'    E         _C        D        _P
#'    *-        _C        *-       _P 
#'    #########################################################       
#'    1         1         2        2         Spine
#'    0         1         0        1         Path
#'    1         2         3        4         Column
#'    ```
#' Note that `alignColumns` actually adds rows to the [humdrumRclass] object's
#' internal [humdrum tables][humTable].
#' @name humColumns
NULL

#####Humtable methods


splitHumtab <- function(humtab, drop = FALSE) { 
          # Splits a humtable by type
          # drop determines whether absent dataTypes
          # are returned as empty data.tables (drop = FALSE)
          # or simply ommited (drop = TRUE).
          # If the humtab is empty, an empty table for each type is produced, regardless of drop
    if (nrow(humtab) == 0L ) {
        output <- replicate(7, humtab, simplify = FALSE)
        names(output) <- c('G', 'L', 'I', 'M', 'D', 'd', 'P')
        output
    } else {
        split(humtab, 
              # by = 'Type', sorted = FALSE,
              f = factor(humtab$Type, levels = c('G', 'L', 'I', 'M', 'D', 'd', 'P')),
              drop = drop) # this seems wrong but it actually makes sense
    }
}

spliceHumtab <- function(humtab) {
          # This combines the components of a humtab list into a single data.table
          # it also sorts them by default values
          humtab <- if (length(humtab) == 1L) humtab[[1]] else rbindlist(humtab, fill = TRUE)
          
          orderHumtab(humtab)
}

orderHumtab <- function(humtab) {
    if (nrow(humtab) == 0L) return(humtab)
    orderingcols <- c('File', 'Column', 'Record', 'Stop')
    
    # can't sort by lists
    orderingcols <- orderingcols[sapply(humtab[ , orderingcols, with = FALSE], class) == 'integer']
    
    setorderv(humtab, cols = orderingcols)
    
}

#######################################################-
#############################humdrumR S4 class ----
######################################################-

#' HumdrumR class
#' 
#' This `S4` class is the basic unit of the 
#' [humdrumR] package.
#' Each `humdrumR` object represents data [read][readHumdrum] from one or 
#' more humdrum files.
#' In the documentation we refer to the collection of files within a [humdrumRclass] object
#' as a "**corpus**," and each file as a "**piece**."
#' However, though humdrum data is *usually* encoded as one "piece" per file, this is not necessarily the case:
#' files might represent movements within a piece, or even just a part of a score. Still, we tend to refer
#' to them as "pieces."
#' In coding examples, we name humdrumR objects "`humdata`."
#' 
#' The most imporant part of a `humdrumR` object is the 
#' [humdrum tables][humTable] it holds within it.
#' In essence, an `humdrumR` object is simply a wrapper around these
#' humdrum tables, which helps users to
#' to visualize, index, [summarize][humSummary], and [manipulate][humdrumR::withinHumdrum]
#' the table in a variety of ways.
#' 
#' Basic information about the size and shape of `humdrumR` data can be
#' obtained with calls to [nrecords, npieces, length, ncol, etc.][humSize].
#' More detailed summary information can be obtained with the humdrumR [corpus summary functions][humSummary].
#' HumdrumR data can also be coerced to more basic R data types using [as.matrix, as.data.frame, etc.][humCoercion].
#' A number of helpful functions are also defined to [reshape][humShape] humdrumR data.
#' 
#' The most powerful features of [humdrumR] are the tools it gives you to
#' 
#' 1. Filter humdrum data, using [filterHumdrum] and the standard R [indexing operators][base::Extract]: `[]` and `[[]]`.
#' 2. Apply functions and arbitrary commands to humdrum data using the [with(in)Humdrum][humdrumR::withinHumdrum] routines.
#' 
#' 
#' 
#' @section Active field:
#' 
#' The `Active` slot of a [humdrumR] object contains an [expression][rlang::quosure]
#' which refers to fields in the internal [humdrum table][humTable].
#' Go to the dedicated [active field][humActive] documentation to learn more about this important slot!
#' 
#' 
#' @slot Humtable A list of [humdrum tables][humTable], each having the same fields
#' but containing data from different types of records (e.g., interpretations, data, barlines, comments).
#' @slot Files A list of two elements. The first, "Search", contains a single character representing
#' the `pattern` used in the call to [readHumdrum] which created this humdrumR object.
#' The second, "Names", is a vector of strings representing all the files which matched the `pattern`
#' and were read into the `humdrumR` object.
#' @slot Fields A list containing strings corresponding to the existing fields in the `humdrumR` object.
#' The fields are divided into five categories: "Data", "Structure", "Interpretation", "Formal", and "Reference"---see 
#' the [humdrum table][humTable] documentation.
#' @slot Active A quosure expression which 
#' extracts data from field(s) in the [humdrum table][humTable]: the "active expression."
#' @slot LoadTime A [POSIXct][base::DateTimeClasses] value, indicating the time at which [readHumdrum] was
#' called to create this `humdrumR` object.
#' @slot Patterns A character vector of the original search patterns used to match files in the system.
#
#' 
#' @name humdrumRclass
#' @aliases humdrumRS4
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'list',
                   Files = 'list',
                   Fields = 'list',
                   Active = 'quosure',
                   LoadTime = 'POSIXct'
                   )) -> makeHumdrumR

setMethod('initialize', 'humdrumR',
          function(.Object, humtab, pattern, tandemcol) {
            # humtab = a humdrum table
            # pattern = the original file search pattern (string)
            # tandem col a logical vector indicating which columns are tandem fields

            fields <- colnames(humtab)
            fieldcategories <- list(Data = 'Token',
                                    Structure = c('Filename', 'Filepath', 'File', 'Label', 'Piece',
                                                  'Column', 'Spine', 'Path', 'Stop',
                                                  'Record', 'NData', 'Global', 'Null', 'Filter', 'Type'),
                                    Interpretation   = c('Exclusive', 'Tandem',
                                                         fields[tandemcol]),
                                    Formal    = c(grep('^Formal', fields, value = TRUE),
                                                  'Bar', 'DoubleBar', 'BarLabel'))
            fieldcategories$Reference <- fields[!fields %in% unlist(fieldcategories)]
         
            
            .Object@Humtable  <- splitHumtab(humtab)        
            .Object@Fields    <- fieldcategories
            .Object@Active    <- rlang::quo(Token)
            .Object@Files     <- list(Search = pattern, Names = unique(humtab$Filepath))
            .Object@LoadTime  <- Sys.time()
            .Object
          })



######humdrumR core methods ####

####As/Is ####

#' @rdname humdrumRclass
#' @export
is.humdrumR <- function(x){
    inherits(x, 'humdrumR')  
} 
is.humdrumR <- \(x) inherits(x, 'humdrumR')


#' humdrumR Coercion.
#' 
#' Many users may wish to work with humdrum data without
#' using the `[humdrumR:humdrumR][humdrumR]` API, instead using 
#' basic `R` data types.
#' For this purpose, [humdrumRclass] data objects can be coerced to 
#' basic `R` data types.
#' 
#' [as.vector(humdata)][base::as.vector()] evaluates the [humdrumRclass] object's
#' **Active** expression, and (attempts) to force the result to a vector of mode. This
#' method is essentially a wrapper for [evalActive].
#' 
#' [as.matrix(humdata][base::as.matrix()] also evaluates the [humdrumRclass] object's
#' **Active** expression, but wraps it into a matrix of dimensions `c([humdrumR:humSize][nrow(humdata), ncol(humdata)])`.
#' Note that "`[humdrumR:humTable][Columns]`" in humdrum data are not necesarily the same as spines. 
#' 
#' [as.data.frame(humdata)][base::as.data.frame()] first calls `as.matrix` then converts the matrix to a [data.frame][base::data.frame()].
#' [as.data.table(humdata)][data.table::as.data.table()] first calls `as.matrix` then converts the matrix to a [data.table::data.table()].
#' 
#' `as.matrices`, `as.data.frames`, and `as.data.tables` call `as.matrix`/`as.data.frame`/`as.data.table`
#' on each individual file in a [humdrumRclass] corpus, returning a list of matices/data.frames/data.tables.
#' 
#' 
#' @param dataTypes Which types of humdrum records to include. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (see the [humdrum table][humTable] documentation **Fields** section for explanation.).
#' 
#' @param pad.files `logical` (default `TRUE`). If any pieces in the [humdrumRclass] corpus have fewer 
#' `[humdrumR:humTable][spines/columns]` than the maximum, should they be padded with the `padder` argument (`par.files == TRUE`) or
#' should an an error occur (`pad.files == FALSE`)? Note that these "padded" points are not represented in the original humdrum data.
#' 
#' @param pad.paths `logical` If any spine path splits (`'*^'`) occur in the humdrumR data, should they be padded 
#' with the `padder` argument (`par.files == TRUE`) or
#' should an an error occur (`pad.paths == FALSE`)? 
#' Note that these "padded" points are not represented in the original humdrum data.
#' 
#' @param padder An atomic value of length one. If `par.files` or `pad.paths` are true, the `padder`
#' argument is used to fill in the desired gaps.
#' 
#' 
#' @param mode If the `mode` argument is not `'any'`, it can be a single `character`
#' string naming an atomic mode---the output will be coerced to this mode (if possible).
#' 
#' @param field(s) If the `field` argument is *not* `NULL`, it can instead be a `character` string matching
#' the [humdrumRclass] object's fields. If so, these fields are extracted instead of the
#' [Active expression][humActive].
#' For calls to `as.vector` and `as.data.frame`, only one field can be extracted.
#' However, for calls to `as.matrix`, multiple fields can be extraced---these fields will be
#' returned in a third matrix dimension, each field forming one rectangular slice.
#' 
#' 
#' 
#' 
#' @name humCoercion
#' @export
setMethod('as.vector', 
          signature = c(x = 'humdrumR'),
          function(x, mode = 'any') {
                    if (is.empty(x)) return(vector(mode, 0L))
                    
                    as.vector(evalActive(x, dataTypes = 'D', nullAs = '.', forceVector = TRUE), mode)
                    })

#' @name humCoercion
#' @export
as.lines <- function(humdrumR, dataTypes = 'GLIMDd', fieldname = NULL, 
                     alignColumns = FALSE, padPaths = FALSE, padder = '.') {
    
          checkhumdrumR(humdrumR, 'as.lines')
            
          dataTypes <- checkTypes(dataTypes, 'as.lines')
          
          mats <- as.matrices(humdrumR, dataTypes = dataTypes, padder = padder,
                              fieldnames = fieldname[1], alignColumns = alignColumns,
                              path.collapse = !padPaths)
          
         
          lines <- unlist(lapply(mats, 
                                  \(mat) {
                                      mat[is.na(mat)] <- "."
                                      apply(mat, 1, \(row) paste(row, collapse = '\t'))
                                  }))
          lines[grepl('^!!', lines)] <- stringr::str_remove_all(lines[grepl('^!!', lines)], '\t\\.')
          
          names(lines) <- unlist(lapply(mats, rownames))
          
          lines
                           
          
}


#' @name humCoercion
#' @export
as.matrix.humdrumR <- function(x, dataTypes = 'D', fieldnames = NULL, 
                   alignColumns = TRUE, padder = NA,  path.collapse = TRUE) { 
                    
                    checkhumdrumR(x, 'as.matrix.humdrumR')
    
                    dataTypes <- checkTypes(dataTypes, 'as.matrix')
                    
                    if (!is.null(fieldnames)) x <- setActiveFields(x, fieldnames)
                    
                    if (is.empty(x)) return(matrix(character(0L), ncol = 0, nrow = 0))
                    
                    if (anyStops(x)) x <- collapseStops(x, collapseAtomic = TRUE, sep = ' ')
                    
                    paths  <- anyPaths(x)
                    if (paths && path.collapse) x <- collapsePaths(x, collapseAtomic = TRUE, sep = '\t')
                    
                    ragged <- is.ragged(x)
                    #if (ragged && !alignColumns) stop("In call as.matrix(humdrumR, pad = FALSE): This humdrumR object has different numbers
                     #                                # of spines or paths across files, so it can't by made into a matrix unless pad = TRUE")
                    
                    if (ragged && alignColumns) x <- alignColumns(x, "_C")
                    
                    dataTypes <- c(dataTypes, 'P')
                    x <- collapseRecords(x, collapseAtomic = FALSE, padPaths = TRUE)
                    
                    records <- getFields(x, fieldnames = fieldnames, dataTypes = dataTypes)
                    records  <- lapply(records, as.list) # stri_list2matrix needs lists! If column is not a list-column, we're getting errors.
                    matrices <- lapply(records, stringi::stri_list2matrix, byrow = TRUE)
                    
                    if (length(matrices) == 1L) {
                              outMat <- matrices[[1]]         
                    } else {
                              outMat <- do.call(abind::abind, c(args = matrices, along = 3))
                    }
                    
                    outMat[outMat == '_C'] <- padder
                    outMat[outMat == '_P'] <- padder
                    outMat[is.na(outMat)] <- padder
                    
                    ## dimnames and sort
                    humtab <- getHumtab(x, dataTypes = dataTypes)
                    
                    dimnames(outMat) <- c(list(File.Record = humtab[ , paste0(File, '.', Record)],
                                               Column = 1:ncol(outMat)),
                                          if (length(dim(outMat)) == 3L) list(Field = colnames(records)) else NULL)
                    outMat <- outMat[order(humtab$File, humtab$Record), , drop = FALSE]
                        
                    
                    outMat
                    
                    
                    
}                   

#' @name humCoercion
#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrumR'),
          function(x, dataTypes = 'D', fieldname = NULL, padder = NA, collapse.path = TRUE) {
                    if (!is.null(fieldname) && length(fieldname) != 1L) stop("Can only coerce one field in a humdrumR object to a data.frame.")
                    
                    as.data.frame(as.matrix(x, dataTypes, fieldname, padder, collapse.path), stringsAsFactors = FALSE)
          })




#' @name humCoercion
#' @export
as.matrices <- function(humdrumR, dataTypes = 'D', fieldnames = NULL, padder = NA, path.collapse = TRUE, alignColumns = FALSE) {
          checkhumdrumR(humdrumR, 'as.matrices')

          dataTypes <- checkTypes(dataTypes, 'as.matrices')
          mat <- as.matrix(humdrumR, dataTypes = dataTypes,
                           padder = padder, fieldnames = fieldnames, path.collapse = path.collapse, alignColumns = alignColumns)
          
          file <- as.integer(gsub('\\..*', '', rownames(mat)))
          lapply(tapply(seq_along(file), file, list),
                 \(i) {
                     m <- mat[i, , drop = FALSE]
                     m[ , colSums(if (is.na(padder)) {is.na(m)} else {m == padder}) != nrow(m), drop = FALSE]
                     
                 })
          
}
#' @name humCoercion
#' @export 
as.data.frames <- function(humdrumR, dataTypes = 'D', fieldnames = NULL, padder = NA, path.collapse = TRUE) {
          checkhumdrumR(humdrumR, 'as.data.frames')
          lapply(as.matrices(humdrumR, dataTypes = 'D', fieldnames = NULL, 
                             padder = NA, path.collapse = TRUE), as.data.frame)
}



# A humdrumR object is treated differently depending on whether its
# active columns contain atomic data ("isActiveAtomic") or not (tables, lists, matrices, etc.).
# this function tests if the active column is a vector or not
isActiveAtomic <- function(humdrumR) {
          checkhumdrumR(humdrumR, 'isActiveAtomic')
          act <- evalActive(humdrumR)
          !is.object(act) && !is.list(act) 
}



####Shape ####

#' humdrumR size and shape
#' 
#' These functions can be used to quickly
#' get basic information about the size and "shape" of
#' a [humdrumRclass] corpus.
#' For more details, use the [census][humSummary()] function.
#' 
#' A few common base `R` methods are defined
#' as synonyms for the humdrumR-specific sizing functions:
#' [length(humdata)][base::length()] is equivalent to `npieces(humdata)`;
#' [nrow(humdata)][base::nrow()] is shortand for `nrecords(., dataTypes = 'LIMDd')` (i.e., local records only).
#' [ncol(humdata)][base::ncol()] returns the *maximum* value of the [Column][humTable] field---the maximum number of
#' tab-delineated columns in the humdrum files (irrespective of Spines/Paths).
#' The results of `nrow` and `ncol` will match
#' up with the dimensions of matrices/data.frames produced by calls to [as.matrix/as.data.frame][humdrumR::as.matrix()].
#' [dim(humdata)][base::dim()] returns `c(nrow(humdata), ncol(humdata))`, as usual.
#' 
#' `is.empty(humdata)` asks if `ntokens(humdata, dataTypes = 'D') == 0L`.
#' 
#' @name humSize
#' @export
nrecords <- function(humdrumR, dataTypes = 'D') {
          checkhumdrumR(humdrumR, 'nrecords')
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)

          n <- humtab[Stop == 1L | is.na(Stop) , .(NR = length(unique(Record))), by = Filename] 
          
          sum(n$NR)
}

#' @name humSize
#' @export
ntokens <- function(humdrumR, dataTypes = 'D') {
          checkhumdrumR(humdrumR, 'ntokens')
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
          
          nrow(humtab)
}

#' @name humSize
#' @export
npieces <- function(humdrumR) {
          checkhumdrumR(humdrumR, 'npieces')
          humtab <- getHumtab(humdrumR, 'D')
          
          if (nrow(humtab) == 0L) 0L else length(unique(humtab$File))
}

#' Does humdrumR corpus contain subcorpora?
#' 
#' [HumdrumR][humdrumRclass] objects can be divided into "subcorpora."
#' `anySubcorpora` and `namesSubcorpora` functions tell us if there are any subcorpora and, if so, what they are called.
#'
#' @name humSize
#' @export
anySubcorpora <- function(humdrumR){
    checkhumdrumR(humdrumR, 'anySubcorpora')
    
    humtab <- getHumtab(humdrumR)
    
    length(unique(humtab$SubCorpus)) > 1L
}

#' @name humSize
#' @export
namesSubcorpora <- function(humdrumR) {
    checkhumdrumR(humdrumR, 'namesSubcorpora')
    
    humtab <- getHumtab(humdrumR)
    
    unique(humtab$SubCorpus)
}

#' @name humSize
#' @export
nfiles <- npieces

#' @name humSize
#' @export
setMethod('length',
          signature = c(x = 'humdrumR'),
          \(x) npieces(x))

#' @name humSize
#' @export
setMethod('nrow', 
          signature = c(x = 'humdrumR'), 
          \(x) nrecords(x, dataTypes = 'LIMDd'))

#' @name humSize
#' @export
is.empty <- function(humdrumR){
    checkhumdrumR(humdrumR, 'is.empty')
    ntokens(humdrumR, 'D') == 0L 
} 



#' @name humSize
#' @export
anyPaths <- function(humdrumR) {
    checkhumdrumR(humdrumR, 'anyPaths')
    humtab <- getHumtab(humdrumR, 'Dd')
    
    any(humtab$Path > 0L, na.rm = TRUE)
    
}

#' @name humSize
#' @export
anyStops <- function(humdrumR) {
    checkhumdrumR(humdrumR, 'anyStops')
    
    humtab <- getHumtab(humdrumR, 'Dd')
    any(humtab$Stop > 1L, na.rm = TRUE)
    
}

is.ragged <- function(humdrumR) {
    # Do the pieces in the corpus vary in number of spines?
    
    humtab <- getD(humdrumR)
    
    ncols   <- humtab[!is.na(Column) , length(unique(Column)), by = Filename]$V1
    nspines <- humtab[!is.na(Spine)  , length(unique(Spine)) , by = Filename]$V1
    
    length(unique(ncols)) > 1L || length(unique(nspines)) > 1L
    
}

renumberSpines <- function(hum) UseMethod('renumberSpines')
renumberSpines.humdrumR <- function(hum) {
    humtab <- getHumtab(humdrumR, 'GLIMDdP')
    putHumtab(humdrumR, drop = FALSE) <- renumberSpines.data.table(humtab)
    humdrumR
    
}
renumberSpines.data.table <- function(hum) {
    NewSpine <- hum[ , match(Spine, sort(unique(Spine))), by = Piece]$V1
    hum[ , Column := Column + (NewSpine - Spine)]
    hum$Spine <- NewSpine
    hum
}

#### Reshaping ----

#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @name humColumns
#' @export
alignColumns <- function(humdrumR, padder = '_C') {
    
    checkhumdrumR(humdrumR, 'alignColumns')
    
    humtab <- getHumtab(humdrumR, c('LIMDdP'))
    
    
    ### Figuring out new column for each Spine/Path combination
    allSpinePathcombs <- which(humtab[!is.na(Spine) , table(Spine, Path) > 0L], arr.ind = TRUE) # !is.na(Spine) should be unecessary
    allSpinePathcombs[, 'Spine'] <- as.numeric(rownames(allSpinePathcombs))
    allSpinePathcombs <- allSpinePathcombs[order(allSpinePathcombs[ , 'Spine']), ]
    
    cols <- seq_len(nrow(allSpinePathcombs))
    
    maxP <- max(humtab$Path,  na.rm = TRUE) + 1L
    maxS <- max(humtab$Spine, na.rm = TRUE)
    
    # Initialize empty matrix
    PSmat <- matrix(NA_integer_, 
                    nrow = maxS, ncol = maxP, 
                    dimnames = list(Spines = seq_len(maxS), Paths = seq_len(maxP)))
    PSmat[allSpinePathcombs] <- cols # Fill it
    
    humtab$Column <- PSmat[cbind(humtab$Spine, humtab$Path + 1)]
    
    
    ### Creating new "P" humdrum table
    copyfields <- fields(humdrumR, c('Structure', 'Reference'))$Name
    humtabPadded <- humtab[ , {
        missingColumns <- setdiff(cols,Column)
        newtab <- merge(.SD, all = TRUE,
                        expand.grid(Column = missingColumns, Record = unique(Record)))
        copyfields <- copyfields[sapply(.SD[ , copyfields, with = FALSE], 
                                        \(col) length(unique(col[!is.na(col)])) == 1L)]
        
        newtab[ , copyfields] <- .SD[1, copyfields, with = FALSE]
        newtab
    }
    , by = Filename, .SDcols =  colnames(humtab)]
    
    newrows <- is.na(humtabPadded$Type)
    humtabPadded$Stop[newrows]  <- 1L
    humtabPadded$Type[newrows]  <- "P"
    humtabPadded$Token[newrows] <- padder
    
    humtabPadded$Spine <- allSpinePathcombs[humtabPadded$Column, 'Spine'] 
    humtabPadded$Path  <- allSpinePathcombs[humtabPadded$Column, 'Path' ] - 1
    
    orderHumtab(humtabPadded)
    
    putHumtab(humdrumR, drop = TRUE) <- humtabPadded
    humdrumR
    
}

#' Merge two (or more) humdrumR datasets
#'
#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @export
#' @name humMerge
mergeHumdrum <- function(...) {
    
    
}

# padRecord <- function(record) {
#           ## This is used by alignColumns
#           columnlabels <- record$ColumnLabels[[1]]
#           if (is.na(record$Spine[1])) { # if it's a global record!
#                     list(setNames(record$Temp, '0.0'))
#                     
#           } else {
#                     
#                     tokens <- rep('', record$MaxColumn[1])
#                     tokens[record$Column] <- tapply(record$Temp, record$Column, paste, collapse = ' ') 
#                     names(tokens) <- columnlabels
#                     list(tokens)
#           }
# }



#########################################collapseHumdrum ----

#' HumdrumR data "Shape"
#'
#' These functions are used to change the "shape"
#' of data stored in [humdrum tables][humTable]
#' (held within [humdrumRclass] objects of course).
#' 
#' The `collapseXXX` family allows you collapse all 
#' [user fields][humTable]
#' across groups in another field.
#'
#' @param humdrumR A [humdrumRclass] data object.
#' (see the [humdrum table][humTable] documentation **Fields** section for explanation.).
#' @param collapseAtomic `logical`. If `collapseAtomic == TRUE`, each stop is collapsed to a single string
#' `collapseAtomic == FALSE`, each stop is collapsed to a list of tokens. 
#' @param sep `character`. If `collapseAtomic == TRUE`, collapsed tokens are separated by this string.
#' @param pad `logical`. Should [path/column padding tokens][humColumns] be included?
#' 
#' @family {Humdrum data "reshaping" functions.}
#' @export
collapseHumdrum <- function(humdrumR, byfields, 
                            collapseAtomic = TRUE, sep = ' ', padPaths = FALSE) {
    # This function is the primary function for "collapsing"
    # tokens across groups in another field.
    # Most of the arguments are described in the user documentation
    # (because users use them).
    # byfields determines what fields to collapse across.
    # byfields should be a character vector.
    # suitable for the "by" argument in a data.table[].
    checkhumdrumR(humdrumR, 'collapseHumdrum')
    
    humtab   <- getHumtab(humdrumR, dataTypes = if (padPaths) "GLIMDdP" else "GLIMDd")
    
    # What fields do apply to?
    fieldnames <- unique(c(fields(humdrumR, "Data")$Name, activeFields(humdrumR)))
    fieldtypes <- sapply(humtab[ , fieldnames, with = FALSE], class)
    
    if (collapseAtomic) humtab[ , fieldnames] <- lapply(humtab[, fieldnames, with = FALSE],
                                                     \(field) {
                                                         field[is.na(field)] <- '.'
                                                         field
                                                     })
    
    # What fields to apply across
    byfields <- fieldMatch(humdrumR, byfields, callfun = 'collapseHumdrum', argname = 'byfields')
    
    #### Construct the expressions which will do the work
    #This is a list of expressions, one to collapse each field.
    collapseExprs <- Map(\(name, type) {
        if (collapseAtomic) {
            rlang::expr(paste(!!name, collapse = !!sep))
        } else {
            call <- if (type == 'list') quote(catlists) else quote(list)
            rlang::expr((!!call)(!!name))
        }
        
        },
        rlang::syms(fieldnames), fieldtypes)
    
    ## Expressions will be first saved into tmpfieldnames, 
    # because data.table doesn't allow in place changes if the type changes
    collapsedhumtab <- eval(rlang::expr(humtab[ , c(.SD[1], 
                                                    setNames(list(!!!collapseExprs), 
                                                             !!fieldnames)), 
                                                by = list(!!!(rlang::syms(byfields))),
                                                .SDcols = setdiff(colnames(humtab), c(byfields, fieldnames))]))
    
    
    ## Make sure that null tokens which have been grouped with non-null tokens (d with D, or P with anything)
    ## are now marked as non-null typ
    
    collapsedhumtab$Type <- collapsedhumtab[ , {
        if (Type[1] == 'P' && any(Type != 'P')) Type[1] <- Type[Type != 'P'][1]
        
        if (any(Type == 'D') && any(Type == 'd')) 'D' else Type[1]}, 
        by = byfields]$V1
    
    ## Rename temp colnames
    # newhumtab[ , eval(fieldnames) := NULL] # inplace
    # colnames(newhumtab) <- gsub('_xxxcollapseedxxx$', '', colnames(newhumtab))
    
    if (anyPaths(humdrumR) && !padPaths) collapsedhumtab <- rbindlist(list(collapsedhumtab,
                                                                           getHumtab(humdrumR, 'P')), use.names = TRUE,
                                                                      fill = TRUE) 
    putHumtab(humdrumR, drop = TRUE) <- collapsedhumtab
    humdrumR
}


#' @rdname collapseHumdrum
#' @export 
collapseStops <- function(humdrumR, collapseAtomic = TRUE, sep = ' ') {
    checkhumdrumR(humdrumR, 'collapseStops')
    
    humtab <- getHumtab(humdrumR)
    if (!any(humtab$Stop > 1L & !is.na(humtab$Stop))) return(humdrumR)
    
    collapseHumdrum(humdrumR, byfields = c('Filename', 'Spine', 'Record', 'Path'), 
                    collapseAtomic = collapseAtomic, sep = sep)
}

#' @rdname collapseHumdrum
#' @export
collapsePaths <- function(humdrumR, collapseAtomic = TRUE, sep = ' ') {
    checkhumdrumR(humdrumR, 'collapsePaths')
    # First some necessary preprocessing
    
    if (!anyPaths(humdrumR)) return(humdrumR)
    
    output <- collapseHumdrum(humdrumR, byfields = c('Filename', 'Record', 'Spine'), 
                              collapseAtomic = collapseAtomic, sep = sep, padPaths = FALSE)
    
    output@Humtable$P <- output@Humtable$P[0]
    
    output
    
}

#' @rdname collapseHumdrum
#' @export
collapseRecords <- function(humdrumR, collapseAtomic = TRUE, sep = ' ', padPaths = FALSE) {
    checkhumdrumR(humdrumR, 'collapseRecords')
    humtab <- getHumtab(humdrumR)
    if (!any(humtab$Column > 1L & !is.na(humtab$Column))) return(humdrumR)
    
    
    collapseHumdrum(humdrumR, byfields = c('Filename', 'Record'), 
                    collapseAtomic = collapseAtomic, sep = sep, padPaths = padPaths)
    
    
}


### reshape to fields ----

#' "Fold" data into new fields
#'
#' Many humdrum datasets encode data spread across multiple spines, spine-paths, or stops.
#' By default, `humdrumR` parses each separate spine, spine-path, and stop as their own individual
#' data points, taking up one row in the [humdrum table][humTable].
#' If we want to treat data in multiple spines/paths/stops as different aspects of the same data
#' it is easiest to reshape the data so that the information is in different humdrumR [fields][fields()]
#' rather than separate spines/paths/stops.
#' We "fold" the data from structural field over "on top" of other data using `foldHumdrum`.
#' The convenient `foldStops()` and `foldPaths()` functions automatically fold *all* stops/paths in a dataset onto the first stop/path,
#' creating new fields named, e.g., `Path1`, `Path2`, etc.
#' 
#' @details
#' 
#' The `numeric` `fold` and `onto` arguments, specify where to fold from/to.
#' `fold` indicates the Spine/Path/Stop to fold *from*, "**on to**" the Spine/Path/Stop
#' indicated by `onto`.
#' For example, if you specify `foldHumdrum(mydata, fold = 2, onto = 1, what = 'Spine')`
#' spine 2 will be folded "on top of spine 1.
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
#' of the first spine onto the second *and* third spine, duplicating the spine one data.
#' 
#' The lengths of `fold` and `onto` are automatically matched, so
#' arguments like `fold = 1:2` and `onto = 3` are equivalent to `(fold = 1:2, onto = c(3, 3))`.
#' This makes it east to do things like "copy all four spines onto spine 1": 
#' just write `(fold = 2:4, onto = 1)`.
#' 
#' To specify what structural field you want to fold across, 
#' use the `what` argument (`character`, `length == 1`).
#' The default `what` value is `"Spine"`; other common fold options are `"Path"`,
#' and `"Stop"`, though you might want to use the convenient `foldPaths()` and `foldStops()`
#' functions directly (details below).
#' (You may also fold across `"Record"` or `"NData"`), but these are advanced/tricky!)
#' 
#' The `fromField` (`character`, `length == 1`) controls which field in the `fold` 
#' spine/path/stop is folded into a new field.
#' The `fromField` argument defaults to the (first) [active field][humActive],
#' and must match (or partially match) a field in the `humdrumR` argument data set.
#' In some cases, the `fold` data is smaller than the `onto` data---for instance,
#' spine paths often only exist for part of a spine, so there is less data in the path 
#' than in the full spine.
#' In these cases, it can be helpful to set `fillFromField == TRUE`,
#' which causes the missing parts of `fold` to be filled with data from the `from`
#' field. `foldPaths` does this by default.
#' 
#' The resulting new fields will automatically be named as appropriate results.
#' The `newFieldNames` argument (`character`) can be used to control the output names:
#' one for each new field created by the fold.
#' If you specify too many `newFieldNames`, the later names are ignored.
#' If you specify too few `newFieldNames`, the later names will be given result names, 
#' consistent with the default behavior.
#' 
#' 
#' @section File-Specific Folding:
#' 
#' By default, the same "fold" is carried out in each file of the input corpus 
#' (`humdrumR` argument).
#' If you need to specify different folds in different files, you have to specify the `File`
#' argument (`numeric`, whole number).
#' For *every* file in the corpus you want to apply folds to, you must specify all the `fold`
#' and `onto` arguments in parallel vectors with the `File` argument (even if this is reduendant 
#' for some files).
#' For example, if we specify the combinations,
#' 
#' |  `fold`  |  `onto`  | `File` |
#' |:--------:|:--------:|:------:|
#' | `1`      | `2`      | `1`    |
#' | `3`      | `4`      | `1`    |
#' | `1`      | `2`      | `2`    |
#' | `4`      | `3`      | `2`    |
#' 
#' then
#' 
#' + In `File` one: 
#'   + the first spine is mapped to the second spine
#'   + the third spine is mapped to the fourth spine
#' + In `File` two: 
#'   + the first spine is mapped to the second spine
#'   + the fourth spine is mapped to the third spine
#' 
#' If any files in the corpus are not included, they will not be affected at all!
#' 
#' @param humdrumR A [humdrumR data object][humdrumR-class].
#' @param fold (`numeric`, whole number) The target structure (spine, path, etc.) *from which*
#'   to "fold" data to another structural position and field(s).
#' @param onto (`numeric`, whole number) The target structure (spine, path, etc.) *to which*
#'   the "fold" data is moved.
#' @param what (`character`, `length == 1`) The structural field which is folded across.
#'   Valid options are `"Spine"`, `"Path"`, `"Stop"`, `"Record"`,and `"NData"`.
#' @param File (`NULL` or `numeric`, `length == length(onto)`, whole number) Used to specify
#'   specific folds for different files in the corpus (see "File-Specific Folding" section, below).
#' @param fromField (`character`, `length == 1`) A string (partially) matching the 
#'    name of a data field in the `humdrumR`-object input. This field is the field which is 
#'   "folded" to a new field.
#' @param fillFromField (`logical`, `length == 1`) If the folding field is 
#' smaller than the `to` field, should the content of the `fromField` be copied
#' into the `NA` sections?
#' @param newFieldNames (`character`) Names to use for new fields created by the folding.
#' 
#' @seealso [foldExclusive()] is a particularly useful application of folding. [collapseHumdrum()] also serves a similar function.
#' @family {Humdrum data "reshaping" functions.}
#' @export
foldHumdrum <- function(humdrumR, fold,  onto, what = 'Spine', File = NULL, 
                        fromField = 'Token', fillFromField = FALSE,
                        newFieldNames = NULL) {
    # argument checks
    checkhumdrumR(humdrumR, 'foldHumdrum')
    checkLooseInteger(fold)
    checkLooseInteger(onto)
    
    checkCharacter(fromField, 'fromField', 'foldHumdrum', max.length = 1L)
    fromField <- fieldMatch(humdrumR, fromField, 'foldHumdrum', 'fromField')
    checkArg(what, 'what', 'foldHumdrum', max.length = 1L,
             validoptions = c('Spine', 'Path', 'Stop', 'Record', 'NData'))
    
    # start work
    humdrumR <- setActiveFields(humdrumR, fromField)
    
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDdP')
    moves <- foldMoves(humtab, fold, onto, what, File, newFieldNames)

    # 
    fromHits <- humtab[ , list(File, get(what)) %ins% moves[, c('File', 'From'), with = FALSE]]
    fromTable <- humtab[fromHits == TRUE, c(fromField, fields(humdrumR, c('S', 'F', 'R'))$Name), with = FALSE]
    
    if (all(is.na(fromTable[[fromField]]))) {
        .warn("Your fromField doesn't have any non-null data where {what} == {harvard(fold, 'or')}.",
              "Your humdrumR data is being returned unchanged.")
        return(humdrumR)
    }
    
    humtab[[fromField]][fromHits & humtab$Type == 'D'] <- NA
    #
    whichMatch <- fromTable[ ,  matches(list(File, get(what)), moves[ , c('File', 'From'), with = FALSE], multi = TRUE)]
    
    #
    fromTable <- do.call('rbind', lapply(1:ncol(whichMatch),
           \(j) {
               i <- whichMatch[, j]
               fromTable <- fromTable[!is.na(i),]
               i <- i[!is.na(i)]
               switch(what,
                      Path   = fromTable$Column <- fromTable[ , Column + (moves$To[i] - Path)],
                      Spine  = fromTable$Column <- fromTable[ , Column + (moves$To[i] - Spine)],
                      Record = fromTable$NData  <- fromTable[ , NData  + (moves$To[i] - Record)],
                      NData  = fromTable$Record <- fromTable[ , Record + (moves$To[i] - NData)])
               
               fromTable[[what]] <-  moves$To[i]
               fromTable$FieldNames <- moves$FieldNames[i]
               fromTable
           }))
   
   
    
    # data fields in old rows need to be renamed, because they will now be columns
   
    fromTable[ , Null := NULL]
    fromTable[ , Filter := NULL]
    fromTables <- split(fromTable, by = 'FieldNames', keep.by = FALSE)
    dataFields <- fields(humdrumR, 'D')$Name
    fromTables <- Map(\(ftab, fname) {
                             colnames(ftab)[colnames(ftab) == fromField] <- fname
                             ftab

                         }, fromTables, names(fromTables))
 
    newfields <- names(fromTables)
    mergeFields <- setdiff(fields(humdrumR, c('S', 'F', 'R'))$Name, c('Null', 'Filter'))
    humtab <- Reduce(\(htab, ftab) {
        htab <- rbind(ftab[htab, on = mergeFields], 
                      # This is necessary if the from spines have extra paths or stops,
                      ftab[!htab, on = mergeFields], 
                      # or vice versa
                      # htab[!ftab, on = mergeFields],
                      fill = TRUE) 
        
        htab$Filter[is.na(htab$Filter)] <- FALSE
        htab$Null[is.na(htab$Null)] <- FALSE
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
    humtab <- removeNull(humtab, by = c('File', what), nullTypes = 'LIMd')
    humtab <- update_Null(humtab, field = newfields)
    
    putHumtab(humdrumR, drop = FALSE) <- orderHumtab(humtab)
    
    addFields(humdrumR) <- newfields
    
    humdrumR <- setActiveFields(humdrumR, newfields)
    
    
    humdrumR

    
    
}

foldMoves <- function(humtab, fold, onto, what, File = NULL, newFieldNames = NULL) {
    checkNumeric(fold, 'fold', 'foldHumdrum')
    
    
    if (!is.null(File)) {
        if (length(unique(lengths(list(fold, onto, File)))) > 1){
            .stop("In your call to foldHumdrum, ",
                  "if the 'File' argument is not NULL,",
                  "the 'File', 'fold', and 'onto' arguments must all be the same length.")
        }
    } else {
        File <- rep(unique(humtab$File), each = max(length(fold), length(onto)))
        
        match_size(File = File, fold = fold, onto = onto, toEnv = TRUE)
    }    
    
    moves <- unique(data.table(File = File, From = fold, To = onto))
    moves[] <- lapply(moves, as.integer)
    
    # Check for errors
    moves[ ,  if (any(To %in% From)) .stop("In your call to foldHumdrum, the 'fold' and 'onto' {what}s can't overlap within any 'File'.") , by = File]
    
    
    # name fields
    moves[ , NewField := seq_along(From), by = .(File, To)]
    NnewFields <- length(unique(moves$NewField))

    
    newFieldNames <- if (is.null(newFieldNames)) {
        paste0('Result', seq_len(NnewFields) +  curResultN(humtab))
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
#' @param fold (`character`) The target exclusive interpretation(s) *from which*
#'    to "fold" spines to new fields.
#'    Must be specified *without* the `**` prefix: `"kern"` not `"**kern"`.
#' @param onto (`character`, whole number) The target exclusive interpretation (must be only one) *to which*
#'    the "fold" data is moved.
#' 
#' @family {Humdrum data "reshaping" functions.}
#' @seealso `foldExclusive` makes use of the more general [foldHumdrum()].
#' @export
foldExclusive <- function(humdrumR, fold, onto, fromField = 'Token') {
    checkhumdrumR(humdrumR, 'foldExclusive')
    
    checkCharacter(fold, 'fold', 'foldExclusive', allowEmpty = FALSE)
    checkCharacter(onto, 'onto', 'foldExclusive', max.length = 1L, allowEmpty = FALSE)
    
    fold <- unique(gsub('^\\*\\*', '', fold))
    onto <- unique(gsub('^\\*\\*', '', onto))
    
    humtab <- getHumtab(humdrumR, dataTypes = 'LIMDdP')
    
    moves <- humtab[,{
        toSpine <- unique(Spine[Exclusive == onto])
        if (length(toSpine)) {
            do.call('rbind', lapply(fold, 
                   \(fromExclusive) {
                       fromSpine <- unique(Spine[Exclusive == fromExclusive])
                       if (length(fromSpine) &&
                           !(length(fromSpine) == 1L && length(toSpine) == 1L && fromSpine == toSpine)) {
                        
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
    }, by = File]
    
    if (nrow(moves) == 0L) {
        .warn("foldExclusive found no matching files with both '{fold}' and '{onto}'",
              'exclusive interpretations.',
              "Your humdrumR data is returned unchaged.")
        return(humdrumR)
    }
    
    moves <- moves[, list(From, N = seq_along(From)), by = .(File, To, Exclusive)]
    moves[ , Group := paste0(Exclusive, if (any(N > 1)) N), by = Exclusive]
    
    newFieldNames <- stringr::str_to_sentence(unique(moves$Group))
    
    humdrumR <- foldHumdrum(humdrumR, 
                            fold = moves$From, 
                            onto = moves$To, 
                            File = moves$File, what = 'Spine',
                            fromField = fromField,
                            newFieldNames = newFieldNames)
    humdrumR
    
    
}

#' @rdname foldHumdrum
#' @export
foldPaths <- function(humdrumR, fromField = 'Token', fillFromField = TRUE) {
    checkhumdrumR(humdrumR, 'foldPaths')
    
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
foldStops <- function(humdrumR, fromField = 'Token', fillFromField = FALSE) {
    checkhumdrumR(humdrumR, 'foldStops')
           
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
#' @seealso The opposite (kinda) of [foldStops()]
#' @export
unfoldStops <- function(humdrumR, fromFields = fields(humdrumR, 'D')$Name) {
    checkhumdrumR(humdrumR, 'unfoldStops', 'humdrumR')
    if (!anyStops(humdrumR)) return(humdrumR)
    checkCharacter(fromFields, 'fromFields', 'unfoldStops')
    fromFields <- fieldMatch(humdrumR, fromFields, 'unfoldStops', 'fromFields')
    
    #
    humtab <- getHumtab(humdrumR)
    
    
    multistopRecords <- as.data.table(humtab[, which(apply(table(Record,Stop,File), c(1,3), sum) > 1, arr.ind = TRUE)])
    multiHumtab <- humtab[multistopRecords, on = c('Record', 'File')]
    fromFields <- fromFields[multiHumtab[, sapply(fromFields, \(field) any(is.na(get(field))))]]
    for (field in fromFields) {
        
        multiHumtab[, eval(field) := rep_len(get(field)[!is.na(get(field))], length(Token)), by = list(File, Record)]   
    }
    humtab <- orderHumtab(rbind(multiHumtab, humtab[!multistopRecords, on = c('Record', 'File')]))
    humtab <- update_Null(humtab, field = fromFields)
    putHumtab(humdrumR, drop = TRUE) <- humtab
    humdrumR
}

#' "Fold" grace notes into neighbos
#' 
#' 
#' @family {Humdrum data "reshaping" functions.}
#' @seealso `foldGraceNotes` makes use of the more general [foldHumdrum()].
#' @export
foldGraceNotes <- function(humdrumR) {
    warn("foldGraceNotes has not been implemented yet!")
    humdrumR
}


#################################-
############Humtable manipulation and access ####
###############################-



#' Access a Humdrum Table
#' 
#' `getHumtab` extracts a [humdrum table][humTable] from a [humdrumRclass] object.
#' 
#' @param humdrumR A `humdrumR` object.
#' @param dataTypes A `character` vector. Specifies which types of data tokens/records to extract.
#'     See the [humTable] documentation and/or the humdrum syntax vignette for clarification!
#' 
#' For `dataTypes`, values can be:
#' 
#' + `"G"`: global comments 
#' + `"L"`: local comments 
#' + `"I"`: interpretations
#' + `"M"`: barlines
#' + `"D"`: non-null data
#' + `"d"`: null data 
#' 
#' Multiple types can be specified as a vector, or smooshed into a single string: e.g., `"GLIMD"`.
#' 
#' @rdname humTable
#' @export
getHumtab <- function(humdrumR, dataTypes = c('G', 'L', 'I', 'M', 'D', 'd')) {
          checkhumdrumR(humdrumR, 'getHumtab')
          dataTypes <- checkTypes(dataTypes, 'getHumtab')
          
          humtab <- humdrumR@Humtable[dataTypes]

          spliceHumtab(humtab)
}

getD <- function(humdrumR) getHumtab(humdrumR, dataTypes = 'D')

`putHumtab<-` <- function(humdrumR, value, drop = FALSE) {
          # adds humtab into humdrumR
          # Drop determines whether record dataTypes that are 
          # absent from value are left unchanged (drop = FALSE)
          # or replaced with empty data tables (drop = TRUE)
          # If drop indicates a record type (i.e., GLIM) those types are dropped only
          if (data.table::is.data.table(value)) {
              value <- if (is.character(drop)) {
                  dataTypes <- checkTypes(drop, 'putHumtab')
                  value <- splitHumtab(value, drop = FALSE)
                  value[!names(value) %in% dataTypes]
              } else {
                  splitHumtab(value, drop = !drop)
              }
          }
          humdrumR@Humtable[names(value)] <- value
          humdrumR
}



# `addNulld<-` <- function(humdrumR, value) {
#     old <- getHumtab(humdrumR, 'd')
#     value <- value[ , colnames(old), with = FALSE]
#     humdrumR@Humtable$d <- rbind(old, value)
#     humdrumR
#     
# }


`putD<-` <- function(humdrumR, value) { 
  # This is a shortcut to replacing non-dull data records
  # in a humdrumR object. I also forces these
  # replacement values to be non-null D
  value$Type <- 'D'
  value$Null <- FALSE
  humdrumR@Humtable[['D']] <- value
  humdrumR
}

##

update_humdrumR <- function(hum, Exclusive, Null, ...) UseMethod('update_humdrumR')
update_humdrumR.humdrumR <- function(hum,  Exclusive = TRUE, Null = TRUE , ...) {
    humtab <- getHumtab(hum, 'GLIMDd')
    
    humtab <- update_humdrumR.data.table(humtab, Exclusive, Null, ...)
    putHumtab(hum, drop = FALSE) <- humtab
    hum
}
update_humdrumR.data.table <- function(hum,Exclusive = TRUE, Null = TRUE, ...) {
    
    if (Exclusive) hum <- update_Exclusive(hum, ...)
    if (Null) hum <- update_Null(hum, ...)
    hum
    
}


#
update_Exclusive <- function(hum, ...) UseMethod('update_Exclusive')
update_Exclusive.humdrumR <- function(hum, ...) {
    humtab <- getHumtab(hum, 'ID')
    
    field <- activeFields(hum)[1]
    putHumtab(hum, drop = 'ID') <- update_Exclusive.data.table(humtab, field)
    
    hum
}
update_Exclusive.data.table <- function(hum, field = 'Token', ...) {
    field <- field[1]
    excluder <- attr(hum[[field]], 'Exclusive')
    
    if (!is.null(excluder)) {
        if (!is.character(hum[[field]])) field <- 'Token'
        
        exclusives <- hum[, Type == 'I' & grepl('^\\*\\*', Token)]
        
        hum[[field]][exclusives] <- paste0('**', excluder(gsub('\\*\\*', '', hum[['Token']][exclusives])))
        hum$Null[exclusives] <- FALSE
    }
    hum
}

#
update_Null <- function(hum, field, ...) UseMethod('update_Null')
update_Null.humdrumR <- function(hum, field = activeFields(hum),  allFields = FALSE, ...) {
    
    if (allFields) field <- fields(hum, 'D')$Name
    humtab <- getHumtab(hum, 'GLIMDd')
    putHumtab(hum, drop = TRUE) <- update_Null.data.table(humtab, field = field)
    hum
}
update_Null.data.table <- function(hum, field = 'Token', ...) {
    dataFields <- hum[ , field, with = FALSE]
    dataFields <- lapply(dataFields, \(x) is.na(x) | x %in% c('.', '!', '*', '=', '_P'))
    null <- Reduce('&', dataFields)
    
    hum[, Null := null]
    
    hum$Type[hum$Type %in% c('d', 'D')] <- hum[Type %in% c('d', 'D'),  ifelse( (Null | Filter), 'd', 'D')]
    hum
}



####### Active slot ----
##### Manipulating the Active slot

#' The "Active expression" of a humdrumR object.
#' 
#' This "Active" expression is used as the default value in a lot of humdrumR code.
#' For one, it is the data which is printed by [show][methods::show()] calls,
#' i.e., whenever you return a `humdrumR` object in the terminal.
#' In any expression within a call to 
#' [with(in)Humdrum][withinHumdrum]
#' `.` is automatically replaced with the `Active` expression.
#' 
#' The active expression can be changed with the commands 
#' [setActive or the $ operator][humdrumRclass].
#' This is a handy way to quickly look at different fields in your data.
#' 
#' The `Active` expression is often just the name of a 
#' [field][humTable]:
#' for instance, the default value is `Token`.
#' However, it can actually be any complex expression which evaluates
#' within the [humdrum table][humTable].
#' For instance, the `Active` expression could be:
#' `paste0(Token, " ", Record)`, which would automatically 
#' print each Token with its record number pasted to it.
#' 
#' @section Null data:
#' 
#' HumdrumR identifies "null data" based on the active field.
#' Anywhere the current active field evaluates to `"."` or `NA` is considered Null data, and assigned the type `"d"` in the internal
#' [humdrum table][humTable].
#' As you work, there will often be data tokens which are null in one field, but not in another field.
#' For example, if you load `**kern` data, a token like `"4r"` (quarter-note rest) token will be `NA` if you call `pitch`, but 
#' not `NA` if you call `recip` (rhythm).
#' 
#' ```
#' 
#' kerndata <- readHumdrum(...)
#' 
#' within(kerndata$Token,
#'        Pitch  <- pitch(.),
#'        Rhythm <- recip(.)) -> kerndata
#' 
#' ```
#' 
#' Now, if you change the active field between `Pitch` and `Rhythm` you'll see that there
#' are different numbers of (non-null) data tokens: `ntokens(kerndata$Pitch)` vs `ntokens(kerndata$Rhythm)` will return different numbers!
#' (The different would be the number of rest tokens.)
#' Similarly, if you apply functions/expressions to this data (using [withinHumdrum] for example), the result will depend on 
#' what the active field is:
#' 
#' ```
#' 
#' with(kerndata$Pitch, length(.))
#' with(kerndata$Rhythm, length(.))
#' ```
#' 
#' Once again, we'll get different numbers here! (Assuming there are rests in the data.)
#' This is the case even though the do-expression isn't actually using the `Pitch` or `Rhythm` fields!
#' If `Pitch` is the active field the rest tokens are null-data and will be ignored!
#' 
#' 
#' @name humActive
NULL

#' `evalActive` evaluates the active expression in a
#' [humdrumR object][humdrumR::humdrumRclass].
#' 
#' 
#' @param humdrumR A [humdrumRclass] data object.
#' @param dataTypes Which dataTypes of humdrum records to include. Legal values are `'G', 'L', 'I', 'M', 'D', 'd', 'P'` 
#'        or any combination of these in a single string (e.g., `"LIM"`).
#'        (see the [humdrum table][humTable] documentation **Fields** section for an explanation.).
#' @param forceVector `logical`. If `TRUE`, the result is forced to be an atomic vector.
#' @param sep A length-one `character` string. If `forceVector == TRUE` this value is used as a separator 
#'        between tokens that are collapsed.
#' @param nullAsDot A single `atomic` value. Any null tokens are coerced to this value (default is `.`).
#' @rdname humActive
#' @export
evalActive <- function(humdrumR, dataTypes = 'D', forceVector = FALSE, sep = ', ', nullAs = NA, humtab)  {
  checkhumdrumR(humdrumR, 'evalActive')
    
  dataTypes <- checkTypes(dataTypes, 'evalActive')
  
  humtab <- if (missing(humtab)) getHumtab(humdrumR, dataTypes = dataTypes) else humtab[Type %in% dataTypes]
  # locnames <- humtab[ , paste(File, Spine, Path, Stop, Record, sep = '.')]
  
  values <- rlang::eval_tidy(getActive(humdrumR), data = humtab)
  
  if (is.atomic(values)) {
    values[is.na(values)] <- nullAs
  } else {
    values[] <- lapply(values, 
                       \(col) {
                                 col[is.na(col)] <- nullAs
                                 col
                                 })
  }
  
  if (!is.atomic(values) && forceVector) {
      if (is.factor(values)) values <- as.character(values)
      if (is.list(values)) {     
          vectors <- sapply(values, is.atomic)
          
          values <- if (all(vectors) && all(lengths(values) == nrow(humtab))) {
              do.call('.paste', c(values, sep = sep))
          } else {
              sapply(values, \(x) if (is.object(x)) object2str(x) else paste(x, collapse = sep))
          }
      }
      if (is.matrix(values)) {
          out <- character(nrow(humtab))
          out[humtab$Type != 'D'] <- apply(values[humtab$Type != 'D', ], 1, \(row) paste(unique(row), collapse = sep))
          out[humtab$Type == 'D'] <- apply(values[humtab$Type == 'D', ], 1, \(row) paste(       row , collapse = sep))
          # rownames(out) <- locnames
          values <- out
      }   
      if (is.object(values)) values <- object2str(values)
      
  }
  
  values
}



#' `getActive(humdata)` is simply an accessor for the humdrumR object's Active quosure.
#' @rdname humActive
#' @export
getActive <- function(humdrumR){
    checkhumdrumR(humdrumR)
    humdrumR@Active 
} 


#' `setActive` takes a [humdrumRclass] object and a formula
#' and sets the right side of formula as the object's Active expression.
#' @rdname humActive
#' @export
setActive <- function(humdrumR, form) {
  checkhumdrumR(humdrumR, 'setActive')
  putActive(humdrumR, rlang::as_quosure(form))
}



#' `setActiveFields` takes a character vector of strings representing current
#' [field][humTable] names
#' and sets the [humdrumRclass] object's active expression
#' to simply return those fields (as a list, if there are more than one).
#' @rdname humActive
#' @export
setActiveFields <- function(humdrumR, fieldnames) {
  checkhumdrumR(humdrumR, 'setActiveFields')
  fieldnames <- fieldMatch(humdrumR, fieldnames, callfun = 'setActiveFields', argname = 'fieldnames')
  actquo <- if (length(fieldnames) > 1L) {
            rlang::quo(list(!!!lapply(fieldnames, as.symbol)))
            } else {
            rlang::new_quosure(as.symbol(fieldnames), env = rlang::get_env(humdrumR@Active))
            }
  putActive(humdrumR, actquo)
}

putActive <- function(humdrumR, actquo) {
    # This does the dirty work for 
    # setActive and setActiveFields.
    humtab <- getD(humdrumR)
    usedInExpr <- fieldsInExpr(humtab, actquo)
    
    
    if (length(usedInExpr) == 0L) .stop("The 'active'-field formula for a humdrumR object must refer to some field.\n
Add a reference to some field, for instance Token.")
    
    humdrumR <- update_Null(humdrumR, field = usedInExpr)
    humdrumR@Active <- actquo
    
    act <- evalActive(humdrumR, humtab = humtab)
    nrows <- nrow(humtab)
    if ((is.atomic(act) && length(act) == nrows)
        || (!is.null(dim(act)) && dim(act)[1] == nrows)
        || (is.list(act) && length(act) == nrows)
        || (is.list(act) && all(lengths(act) == nrows))) {
        return(humdrumR)
    } else {
        stop("The 'active-field formula for a humdrumR object cannot be a different size from the raw fields.", call. = FALSE)
    }
    
    
}




####Fields ----


checkFieldTypes <- function(types, argname, callname) {
          checkArg(types,
                   valid = \(arg) arg %in%  c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference'),
                   validoptions = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference'),
                   argname, callname, 
                   min.length = 0L, max.length = 5L,
                   classes = 'character')
}

#' The `$` operator controls which humdrumR data are printed and default target for result.
#' @rdname humdrumRclass
#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
            name <- as.character(name)
            matches <-  if (name == 'All') {
                fields(x, 'D')$Name
            } else {
                fieldMatch(x, name, callfun = '$', argname = 'name')
            }
            
            
            
            
            setActiveFields(x, matches)
          })

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

#' Use `fields` to list the current fields in 
#' a [humdrumRclass] object.
#'
#' @rdname humdrumRclass
#' @export
fields <- function(humdrumR, fieldTypes = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')) { 
  #

  checkhumdrumR(humdrumR, 'fields')
    
  D <- getHumtab(humdrumR, 'Dd')
 
  valid <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')
  valid <- valid[pmatch(fieldTypes, valid)]
  fieldTypes[!is.na(valid)] <- valid[!is.na(valid)]
  fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', 'fields')
            
  fields <- unlist(humdrumR@Fields[fieldTypes])
  
  D <- D[1L, fields, with = FALSE]
  classes <- sapply(D, class)
  
  if (any(lists <- classes == 'list')) {
    classes[lists] <- paste0('list (of ',
                             sapply(D[ , lists, with = FALSE],
                                                \(col) {
                                                  harvard(paste0(unique(sapply(col, class)), "s"), 'and')
                                                }),
                             ")")
  }
 
  output <- data.table(Name = fields, Class = classes, Type = gsub('[0-9]*$', '', names(fields)))
  output <- output[output$Type %in% fieldTypes]
  
  output
}


showFields <-  function(humdrumR, fieldTypes = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')) {
          # This function is used to produce the human readable 
          # list fields used by print_humtab
          fields <- fields(humdrumR, fieldTypes)

          activefield <- fields$Name %in% activeFields(humdrumR)
          fields$Name <- paste0(' ', fields$Name)
          fields$Name[activefield] <- gsub('^ ', '*', fields$Name[activefield])
          fields$Name <- stringr::str_pad(fields$Name, width = max(nchar(fields$Name)), side = 'right')

          fields$Print <- paste0(fields$Name, ' :: ', fields$Class)

          fields[ ,
                  { cat('\t', Type[1], 'fields:', '\n\t        ')
                    cat(Print, sep = '\n\t        ')
                    cat('\n')
                            }, 
                  by = Type]

          
          # cat('\t\tFields: ', paste(fieldprint, collapse = '\n\t\t        '), '\n', sep = '')
          invisible(fields)
}

fieldsInExpr <- function(humtab, expr) {
  ## This function identifies which, if any,
  ## fields in a humtable are referenced in an expression (or rhs for formula).
  if (is.humdrumR(humtab)) humtab <- getHumtab(humtab)          
  
  namesInExpr(colnames(humtab), expr)
}

activeFields <- function(humdrumR) {
          # Identifies which fields are used in
          # the current `Active` expression.
          fieldsInExpr(getD(humdrumR), getActive(humdrumR))
}




#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' Get named 
#' @export
getFields <- function(humdrumR, fieldnames = NULL, dataTypes = 'D') {
          
          checkhumdrumR(humdrumR, 'getFields')
    
          dataTypes <- checkTypes(dataTypes, 'getFields')
          
          if (is.null(fieldnames)) fieldnames <- activeFields(humdrumR)
          
          fieldnames <- fieldMatch(humdrumR, fieldnames, callfun = 'getFields', argname = 'fieldnames')
          
          humtab <- getHumtab(humdrumR, dataTypes)
          
          humtab[ , fieldnames, with = FALSE]
          
}

fields.as.character <- function(humdrumR, useToken = TRUE) {
# This takes the active humdrumR fields (any field used in the Active expression)
# and coerceds them to characters, filling in null tokens (! * = .) where there are 
# NAs.
# is useToken is true, the Token field is used to fill-in (instead of null tokens).
 humtab <- getHumtab(humdrumR, 'GLIMDdP') 
 
 nulltypes <- c(G = '!!', I = '*', L = '!', d = '.', D = NA_character_, M = '=', P = "_P")
 
 active <- activeFields(humdrumR)
 humtab <- humtab[ , 
                   Map(\(field, act) {
                             if (!act) return(field)
                             field <- as.character(field)
                             na <- is.na(field)
                             field[na] <- if (useToken) Token[na] else nulltypes[Type[na]]
                             field
                   }, 
                   .SD, colnames(humtab) %in% active)]
         
 
 putHumtab(humdrumR, drop = FALSE) <- humtab
 humdrumR
}


`addFields<-` <- function(object, value) {
 ## This function simply adds field names to
 ## the Fields slot in a humdrumR object. 
 ## It DOESN'T actually do the job of adding fields
 ## (column) of data to a humdrum table.
 object@Fields$Data <- unique(c(object@Fields$Data, value))
 object
}

`removeFields<-` <- function(object, value) {
  ## This function removes field names from 
  ## the Fields slot in a humdrumR object. 
  ## It DOESN'T actually do the job of removing fields
  ## (columns) of data from a humdrum table.
  object@Fields$Data <- object@Fields$Data[!object@Fields$Data %in% value]
  object
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

############## Assigning to humdrumR #######
#' Assigning new fields
#' 
#' R objects often have ways of assigning new values to 
#' *part* of the object using [indexing operators][base::Extract].
#' [HumdrumR objects][humdrumR:humdrumRclass] objects are no different, as they allow us to insert
#' new fields into them!
#' 
#' A new field can be inserted into a `humdrumR` object in two ways:
#' 
#' 1. A field can be copied from one humdrumR object to another if their
#'    internal [humdrum tables][humTable] have the exact same number of data tokens (i.e., rows).
#'    It might not seem obvious, but this mechanism is very useful because it can be used to *rename* existing fields
#'     within a humdrumR object (explained below).
#' 2. A [vector][base::vector] or [list of vectors][base::list] can be inserted as 
#'    new fields in a humdrumR object.
#' 
#' Fields can be assigned using two syntaxes:
#' 
#' ```
#' humdata['fieldname'] <- x
#' # or
#' humdata[c('fieldname1', 'fieldname2')] <- x
#' ```
#' 
#' or
#' 
#' ```
#' humdata$fieldname <- x
#' ````
#' 
#' where "fieldname" can be whatever you want it to be, of course!
#' 
#' ### `humdrumR -> humdrumR` assignment:
#' 
#' Assigning a field from one [humdrumR object][humdrumR:humdrumRclass]
#' to another works like this:
#' (Recall that the two objects must have the exact same numbers of data tokens.)
#' The name(s) given in the indexing expression on the left side of the assignment (i.e., `humdata[c('name1', 'name2')]` or
#' `humdata$name`) are used as new field names.
#' How fields are extracted from the right side of the assignment is a little trickier:
#' Any fields in the right-side `humdrumR` object which are named $ResultN$ (where $N$ is an integer) are copied
#' in descending order into the named fields on the left side.
#' If there are no $ResultN$ fields on the right side, any fields used in the current Active formula (on the right side)
#' are copied instead.
#' This system might seem odd at first, but it is very useful in combination with the [withinHumdrum] function.
#' When `withinHumdrum` creates new fields, it calls them $Result1 \ldots Result2 \ldots ResultN$.
#' Since the output of `withinHumdrum` is always the same as the input except with these new "Result" fields,
#' Byou can use `humdrumR <- humdrumR` assignment to immediately assign these result fields more meaningful names in the original object.
#' This makes the most sense with an example:
#' 
#' ```
#' humdata <- within(humdata, Semits <- semits(Token))
#' ````
#' 
#' 
#' Calls to [within.humdrumR()]  keep producing new result fields.
#' If there are more than one result fields, you can assign multiple fields at once using the `[]<-` syntax:
#' 
#' 
#' 
#' #' **IMPORTANT NOTE!**: Any "ResultN" fields in the humdrumR object you assign from
#' that you don't assign field names are simply dropped.
#' This is nice, because often you might proceed through a serious of resultd steps, but you only
#' want the last one (or two).
#' If you want to keep all your result fields either don't re-assign them at all (i.e., keep the "ResultN" names)
#' or assign them all names using the `->[c("name1", "name2", "name3", ...)]` syntax.
#' 
#' 
#' ### `humdrumR -> vector` assignment:
#' 
#' You can assign vectors or lists of vectors straight into a [humdrumR object][humdrumR:humdrumRclass].
#' All vectors must be the same length as the number of data tokens
#' in the target object.
#' If you provide multiple vectors to assign (as a `list` or `data.frame` of vectors) 
#' you must provide the same number of fieldnames using the `->[c('name1', 'name2', ...)]` syntax.
#' You can use the `ntokens` command to determine the right length of vectors you need!
#' 
#' @name humAssignment
NULL

###$<- simply calls []<- indexing!
#' @rdname humdrumRclass
#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'vector'), function(x, name, value) { x[name] <- value ; x  })

#' @rdname humdrumRclass
#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'humdrumR'), function(x, name, value) { x[name] <- value ; x  })


#' @rdname humdrumRclass
#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', j = 'ANY', value = 'vector'),
          function(x, i, j, value) {
                    D <- getD(x)
                    
                    if (!is.list(value)) value <- list(value)
                    
                    multi <- length(value) > 1L
                    
                    if (any(lengths(value) != nrow(D))) {
                        
                        .stop("Can't assign", 
                              plural(length(value), 'these vectors ', 'this vector'),
                              "into this humdrumR object because",
                              plural(length(value), 'they are ', 'it is '),
                              "not the same length as the number of data tokens in the object.")
                    }
                        
                    if (length(value) != length(i)) {
                        .stop("To assign vector(s) to a humdrumR object, the number of new field names you provide",
                              "must be the same as the number of vectors.")
                    }
                        
                    D[ , i] <- value
                    
                    putD(x) <- D
                    humtab <- update_humdrumR(humtab, field = i)
                    
                    addFields(x) <- i
                    x <- setActiveFields(x, i)
                    
                    x

          })


#' @rdname humdrumRclass
#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', j = 'ANY', value = 'humdrumR'),
          function(x, i, j, value) {
                    # This function copies one or more ResultN fields from one humdrumR object
                    # into named fields in a different (or the same) humdrumR object of the same size.
                    # If these named fields don't exist, they are created.
                    # If there are no ResultN fields, the active field(s) are copied.
                    if (any(i %in% fields(x, c('Structure', 'Interpretation', 'Formal', 'Reference'))$Name)) {
                        builtin <- i[i %in% fields(x, c('Structure', 'Interpretation', 'Formal', 'Reference'))$Name]
                        .stop("You can't overwrite built-in fields of a humdrumR object. In this case,",
                              harvard(builtin, 'and'), 
                              ifelse = length(builtin), 
                              '<is a built-in field|are built-in fields>.')
                    }
              
                    humtab <- getHumtab(value)
                    results <- resultFields(humtab)
                    removeFields(value) <- results
                    
                    if (length(results) == 0L) results <- activeFields(value)
   
                    results <- tail(results, n = length(i))
                    
                    if (any(i %in% colnames(humtab))) humtab[ , eval(i[i %in% colnames(humtab)]) := NULL]
                    
                    colnames(humtab)[colnames(humtab) %in% results] <- i
                    
                    if (any(grepl('Result', colnames(humtab)))) humtab[ , eval(grep('Result', colnames(humtab), value = TRUE)) := NULL]
                    
                    putHumtab(value, drop = FALSE) <- humtab
                    humtab <- update_humdrumR(humtab, field = i)
                    
                    addFields(value) <- i
                    
                    value@Active <- substituteName(value@Active, setNames(rlang::syms(i), results))
                   
                    value
          })




####################################################-
#########################Print methods ----
#########################################################-

setMethod('show', signature = c(object = 'humdrumR'),
          function(object) {
                    len  <- length(object)
                    trim <- if (len == 1L) 800L else 40L
                    print_humtab(object, firstAndLast = TRUE, max.records.file = trim)
                    
                    if (len > 1L) {
                              cat('\n')
                              cat('\thumdrumR corpus of', 
                                  ifelse(len <= 100L, num2word(len), num2str(len)), 
                                  'files') 
                              if (anySubcorpora(object)) {
                                  subnames <- namesSubcorpora(object)
                                  cat(' (', num2word(length(subnames)), 
                                      " subcorpora: ", 
                                      paste(subnames, collapse = ', '), 
                                      ')', sep = '')
                              }
                              cat('.\n')
                                  
                    }
                    
                    ## Fields
                    showFields(object, 'Data')
                    
          })

print_humtab <- function(humdrumR, dataTypes = "GLIMDd", firstAndLast = TRUE,
                         max.records.file = 40L, max.token.length = 30L, collapseNull = 10L) {
    
  checkhumdrumR(humdrumR, 'print_humtab')
    
  dataTypes <- checkTypes(dataTypes, "print_humtab")
  
  
  if (is.empty(humdrumR)) {
    cat("\nEmpty humdrumR object\n")
    return(invisible(NULL))
  }
  
  Nfiles <- length(humdrumR)          
  if (Nfiles > 2 && firstAndLast) humdrumR <- humdrumR[unique(range(getFields(humdrumR, 'File')$File))] 
  
  humdrumR <- printableActiveField(humdrumR, dataTypes = 'D') 
  
  .print_humtab(humdrumR, dataTypes, Nmorefiles = Nfiles - length(humdrumR),
                max.records.file, max.token.length, collapseNull)

  invisible(NULL)
  
}


printableActiveField <- function(humdrumR, dataTypes = 'D', useTokenNull = TRUE, sep = ', '){
    # evaluates the active expression into something printable, and puts it in a 
    # field called "Print"
    dataTypes <- checkTypes(dataTypes, "printableActiveField")
    
    humtab <- getHumtab(humdrumR, dataTypes = 'GLIMDdP') 
    
    active <- as.character(evalActive(humdrumR, dataTypes = 'GLIMDdP', 
                                      forceVector = TRUE, nullAs = ".", humtab = humtab))
    humtab$Null[humtab$Type == 'P'] <- FALSE
    humtab$Filter[humtab$Type == 'P'] <- FALSE

    ## fill in null data
    nulltypes <- c(G = '!!', I = '*', L = '!', d = '.', D = NA_character_, M = '=', P = "_P")
    active[humtab[, Filter | Null]] <- nulltypes[humtab[Filter | Null, Type]]    

    ## fill from token field
    tokenFill <- if (useTokenNull) {
        # humtab[, !Type %in% c('D', 'd')]
        humtab[ , !Type %in% c('D', 'd') & Null]
    } else {
        # always get ** exclusive
        humtab[ , (is.na(active) | active == '*') & grepl('\\*\\*', Token)]
    }
    
    active[tokenFill] <- humtab[tokenFill == TRUE, Token]
    
    active <- gsub('\\.(, )+\\.', '.', active)

    
    humtab[ , Print := active]
    humtab$Type[humtab$Type == 'd'] <- 'D'
    humtab$Type[humtab$Type == 'P'] <- 'D'
    
    putHumtab(humdrumR, drop = TRUE) <- humtab
    
    addFields(humdrumR) <- 'Print'
    setActive(humdrumR, ~Print)
}



.print_humtab <- function(humdrumR, dataTypes = 'GLIMDd', Nmorefiles = 0L,
                          max.records.file = 40L, max.token.length = 12L, collapseNull = 10L,
                          screenWidth = options('width')$width - 10L) {
  tokmat <- as.matrix(humdrumR, dataTypes = dataTypes, path.collapse = FALSE, alignColumns = TRUE)
  
  # removes "hanging stops" like "a . ." -> "a"
  # if (anyStops(humdrumR)) tokmat[] <- stringr::str_replace(tokmat, '( \\.)+$', '')
  #
  if (collapseNull < Inf) tokmat <- censorEmptySpace(tokmat, collapseNull = collapseNull)
  
  Filenames <- getHumtab(humdrumR)[ , unique(Filename)]
  File   <- gsub('\\..*$', '', rownames(tokmat))
  NRecord <- gsub('^[0-9]*\\.', '', rownames(tokmat))
  
  #
  global <- stringr::str_detect(tokmat[ , 1], '^!!')
  tokmat <- cbind(paste0(NRecord, ':  '), tokmat)
  
  
  ## censor lines beyond max.records.file
  filei <- tapply_inplace(File, File, seq_along)
  i <- ifelse(length(unique(File)) == 1L | File != max(File), filei <= max.records.file, filei > (tail(filei, 1) - max.records.file))
  tokmat <- tokmat[i, , drop = FALSE]
  global <- global[i]
  

  
  ## Trim and align columns, and collopse to lines
  tokmat[!global, ] <- trimTokens(tokmat[!global, , drop = FALSE], max.token.length = max.token.length)
  lines <- padColumns(tokmat, global, screenWidth)
  starMessage <- attr(lines, 'message')
  lines[global] <- gsub('\t', ' ', lines[global])
  
  # records of first and last non-censored lines of each file
  firsts <- tapply(seq_along(lines), File[i], min)
  lasts <- tapply(seq_along(lines), File[i], max)

  
  #  censored ranges (if any)
  ranges <- tapply(NRecord[!i], factor(File)[!i], 
                   \(nr) {
                       if (length(nr) > 1L) paste0(nr[1], '-', nr[length(nr)], ':') else paste0(nr[1], ':')
                   })
  ranges[is.na(ranges)] <- ""
  
  # align :
  if (any(ranges != '')) {
      line_colon <- stringr::str_locate(lines, ':')[ , 'start']
      range_colon <- stringr::str_locate(ranges, ':')[ , 'start']
      largest_colon <- max(line_colon, range_colon)
      lines <- paste0(strrep(' ', largest_colon - line_colon), lines)
      ranges <- paste0(strrep(' ', largest_colon - range_colon), ranges)
  }

  # 
  # 
  maxwidth <- max(nchar(lines))
  
  ranges[ranges != ''] <- stringr::str_pad(paste0('\n', ranges[ranges != '']), width = maxwidth, pad = ':', side = 'right')
  
  lines[lasts[-length(lasts)]] <- paste0(lines[lasts[-length(lasts)]], ranges[-length(ranges)])
  
  
  if (length(unique(File)) > 1L && tail(ranges, 1) != '') {
      lines[tail(firsts, 1)] <- paste0(gsub('^\n', '', tail(ranges, 1)), '\n', lines[tail(firsts, 1)])
  }
  
  # put filenames in
  lines[firsts] <- paste0(stringr::str_pad(paste0(' vvv ', Filenames, ' vvv '), width = maxwidth, pad = '#', side = 'both'), '\n', lines[firsts])
  lines[lasts] <- paste0(lines[lasts], '\n', stringr::str_pad(paste0(' ^^^ ', Filenames, ' ^^^ '), width = maxwidth, pad = '#', side = 'both'))
  
  # if any lines have been censored due to screen size, put message at the end
  if (!is.null(starMessage)) {
      lines[length(lines)] <- paste0(lines[length(lines)], '\n', smartPadWrap(starMessage, maxwidth + 1L))
  }
  
  ##
  if (Nmorefiles > 0L) {
   
   message <- c('',
                paste0('\t\t', glue::glue("({num2str(Nmorefiles)} more files...)")),
                '')
   lines <- append(lines, message, after = tail(firsts, 1) - 1L)
  }
  
  cat(lines, sep = '\n')
  
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

padColumns <- function(tokmat, global, screenWidth = options('width')$width - 10L) {
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

    
    tokmat[!global,  ] <- padder(tokmat[!global, , drop = FALSE], lenCol)
    tokmat[global, 1L] <- padder(tokmat[global, 1L], lenCol[1]) # column 1 is record number!
    

    
    # collapse to lines
    tokmat[global, -1:-2L] <- ''
    lines <- do.call('paste0', as.data.frame(tokmat))
    

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
    
    
                                           
    lines
    
}
