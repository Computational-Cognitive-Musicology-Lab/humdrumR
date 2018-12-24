#' Humdrum Tables
#' 
#' In \code{\link{humdrumR}}, humdrum data is stored (within \code{\linkS4class{humdrumR}} objects)
#' in a data structure called a \strong{Humdrum Table}. A humdrum table
#' is actually a \code{\link[data.table]{data.table}}, from the 
#' R package of the same name. \code{\link[data.table:data.table]{data.tables}}
#' are simply enhanced R \code{\link[base:data.frame]{data.frames}}, with a few
#' handy optimizations.
#' 
#' In a humdrum table, each row represents a single 'token'
#' in the original humdrum data. (Multistops---tokens separated by spaces---are even broken into
#' their own rows). Each column represents a single
#' piece of information associated with the token, which we call a \strong{field}.
#' Throughout this documentation, you should keep in mind that a "token" refers
#' to a row in the humdrum table while a "field" refers to a column.
#' 
#' @section Fields:
#' There are five types of fields in a humdrum table: 
#' \enumerate{
#' \item Data fields
#' \item Structure fields
#' \item Interpretation fields
#' \item Form fields
#' \item Reference fields
#' }
#' When first created by a call to \code{\link{readHumdrum}} every
#' humdrum table has at least eighteen fields: one data field (\code{Token}), two interpretation 
#' fields (\code{Tandem} and \code{Exclusive}), three section fields, and twelve structure fields. Additional fields
#' may be present depending on the content of the humdrum file(s), and even more fields can be created
#' by users.
#' 
#' \strong{1. Data fields:} Data fields are used to describe individual data points
#' in humdrum data (as opposed to groups of points). 
#' Every humdrum table starts with a data
#' field called \strong{Token}, which
#' contains character strings representing the original strings read from the humdrum files. 
#' Users can create as many additional data fields as they like. Every call to
#' \code{\link{withinHumdrum}}---which can also be called using the 
#' \code{\link[humdrumR:humPipe]{\%hum>\%}} piping 
#' operator---generates one or \eqn{N} new data fields named \eqn{{Pipe1, Pipe2, ..., PipeN}}. 
#' (These fields can then be renamed using the \code{$<-} operator, if you want.)

#' 
#' \strong{2. Structure fields:} Structure fields describe where each data point
#' came from---which file, which spine, which record, etc.
#' Every humdrum table starts with twelve Structure fields, describing where
#' the token came from:
#' \describe{
#' \item{File}{\code{character} - The name of the humdrum file.}
#' \item{FullFileName}{\code{character} - The full file name, including it's path.}
#' \item{NFile}{\code{integer} - A unique number associated with each read file (files are numbered alphabetically).}
#' \item{Record}{\code{integer} - The record (i.e., line) number.}
#' \item{NData}{\code{integer} - An enumeration of \strong{data records} specifically.}
#' \item{Global}{\code{logical} - Did the token come from a glocal record (i.e., a record with no spine)?}
#' \item{Type}{\code{character} - What type of record is it? \code{"D"} = non-null data; \code{"d"} = null data;
#'    \code{"I"} = interpretation; \code{"M"} = measure/barline; \code{"L"} = local comment;
#'    \code{"G"} = global comment. There is also a \code{"P"} type, which indicates null "non-tokens"
#'     (see the \code{\link[humdrumR:humColumns]{humdrumR columns}} documentation for an explanation). }
#' \item{Null}{\code{logical} - Is this a null record (i.e., is the token ".", "*", "!", "!!", "!!!", or "=")?}
#' \item{Spine}{\code{integer} - The spine. This field is \code{NA} when \code{Global == TRUE}.}
#' \item{Path}{\code{integer} - The "spine path." Any time a \code{*^} spine path split occurs in
#'       the humdrum data, the right side of the split becomes a new "path." The original path
#'       is numbered \code{0}---if there are no spine path splits, the \code{Path} field is all zeros. 
#'       This field is always \code{NA} when \code{Global == TRUE}. (Check out
#'       the \code{\link[humdrumR:humColumns]{humdrum columns}} documentation for a more thorough explanation
#'       of spine paths.)}
#' \item{Column}{\code{integer} - Which tab-delineated column in the humdrum data---irrespective of Spine/Paths. 
#' See this \code{\link[humdrumR:humColumns]{explanation of columns in humdrumR}}.}
#' \item{Stop}{\code{integer} - Which token in a multistop token. Single tokes are numbered \code{1}.
#'       This field is always \code{NA} when \code{Global == TRUE}.}
#' }
#' 
#' 
#' 
#' \strong{Interpretation fields:} Interpretation fields describe interpretation metadata in the humdrum file(s).
#' Humdrum interpretations are tokens that "carry forward" to data points after them, unless cancelled out by a
#' subsequent interpretation. 
#' All humdrum data must have an \emph{exclusive} interpretation, marked
#' with two asterisks ("**x")---thus, all humdrum tables have an \code{Exclusive} field indicating the
#' exclusive interpretation associated with that token.
#' Humdrum may or may not include \emph{tandem} interpretations. A universal rule for parsing
#' tandem intepretations is impossible, because A) tandem interpretations can "overwrite" each other and B)
#' users can create their own tandem interpretation. The best we can do in all cases is 
#' identify \emph{all} tandem interpretations that have appeared previously in the spine
#' (counting most recent first). All these previous interpretations are encoded in a single
#' character string in the \code{Tandem} field. Users can parse this field using the
#' \code{\link{getTandem}} function. If no tandem interpretations occur in a file,
#' the \code{Tandem} field is still created, but is simply full of empty strings (\code{""}).
#' 
#' Fortunately, many tandem interpretations are widely used and standardized, and these 
#' interpretations are known by \code{humdrumR}. Recognized interpretations (such as "\*clef_" and "\*k[x]")
#' are automatically parsed into their own fields by a call to \code{\link{readHumdrum}}.
#' See the \code{\link{readHumdrum}} documentation for more details.
#' 
#' 
#' \strong{Form fields:} Form fields indicate musical sections, or time windows within
#' a piece, including formal designations ("verse", "chorus", etc.) and measures/bars.
#' Humdrum data may or may not include formal metadata fields, indicated by the token \code{"*>"}.
#' Classified formal marks are put into fields matching their name.
#' Unclassified formal marks are placed in a field called \code{Formal} as a default.
#' Nested formal categories are appended with an underscore and a number for each level of descent:
#' \code{Formal_1, Formal_2, ..., Formal_N}.
#' If part of a section is not given a name in a lower hierarchical level, the field is simply
#' empty (\code{""}) at that point.
#' 
#' Humdrum data may or may not also include barlines (tokens beginning \code{'='}).
#' \code{\link{readHumdrum}} always Three section fields are 
#' \describe{
#'   \item{BarN}{\code{integer} - How many single barline records have passed before this token?
#'     If no '=' tokens occur in the file, \code{BarN} is all zeros.}
#'   \item{DoubleBarN}{\code{integer} - How many double barlines have passed before this token?
#'      If no \code{'=='} tokens occur in the file, \code{DoubleBarN} is all zeros.}
#'   \item{BarLabel}{\code{character} - Any characters that occur after initial \code{'='} or
#'      \code{'=='} of previous bar token. These include the \code{"-"} in 
#'      the \code{'=-'} pickup barline,
#'      repeat tokens (like \code{"=:\|\|"}), and also explicit \emph{bar numbers}. Note that
#'      the \code{BarN} field always enumerate every single \code{'='} bar record, while
#'      measure number labels in humdrum data (which appear in the \code{BarLabel} field) may
#'      do weird things like skipping numbers, repeating numbers, and having suffixes (e.g., "19a")
#'      If no barline tokens appear in the file, \code{BarLabel} is all empty strings (\code{""}).}
#' }
#' 
#' \strong{Reference fields:} Reference fields describe any \emph{Reference Records}
#' in the humdrum data. Every reference record (records beginning \code{"!!!"}) in any
#' humdrum file in a corpus read by \code{\link{readHumdrum}} is parsed into a field named
#' by the reference code: \code{"XXX"} in \code{"!!!XXX"}. Reference tokens are all identical throughout
#' any humdrum piece. If a reference code appears in one file but not another, the field is
#' \code{NA} in the file which does not have the code. If no reference records appear in any
#' files, no Reference fields are created.
#' 
#' @section Philosophy:
#' Why break humdrum data into this "flat" structure, destroying the spreadsheet-like
#' grid structure of the original humdrum data? The Humdrum Table structure affords
#' maximum data analysis flexibility. Thanks to the Structure fields, we can easily
#' regroup and reform the structures of humdrum data (like spines). 
#' However, if you would like to change the "flat" structure of a humdrum table,
#' check out the \code{\link[humdrumR:humShape]{humdrumR reshaping funcitons}}.
#' 
#' ...
#' @name Humtable
NULL

#' Spines vs Paths vs Columns 
#' 
#' In the \href{http://www.humdrum.org/guide/ch05/}{humdrum syntax}, data is placed in "spines,"
#' which are not the same as "columns" in a spreadsheet. A "column" refers to a 
#' tab-delineated group of values.
#' "Spines" can be a single column, or they may (at any time) split into multiple columns,
#' which can in turn split again, using the \code{"*^"} interpretation token. The reverse can happen as well,
#' with two or more columns merging into a single column, using the \code{"v"} token.
#' This means that, while humdrum data at first glance looks like a simple two-dimensional table,
#' it is actually a flexible tree structure. As spines split and merge, the total number of columns
#' can change during a piece, creating a "ragged" edge.
#' Another similar issue is that a corpus of humdrum files may have varying numbers of spines/columns, between pieces.
#' ("Global" comment/reference records are also a special case, as that are always a single value, even if interspersed with
#' multi-column local records.)
#' 
#' In \code{\link{humdrumR}}, spines, columns, and spine paths work like this.
#' First of all, we actually assume a slightly more strict version of the humdrum syntax:
#' we assume that all the spines which appear at the beginning of a file (headed with exlusive interpretations
#' like \code{"**kern"}) can never merge into each other. Thus, a humdrum file read into \code{humdrumR}
#' must not end with fewer columns than it starts.
#' Spine merges (\code{"*v"}) can only happen within spine paths that originally split off the same spine.
#' This extra-strict specification of spine paths in the humdrum syntax is, fortunately, something that has been
#' informally followed in most humdrum datasets.
#' 
#' Our strict spine-path definition makes everything work fairly simply: 
#' Within a piece, the spines which appear at the beginning of the piece are the "true" spines through the rest of the piece, numbered
#' from left to right, starting from \code{1L}.
#' For each local token, the value in the \code{Spine} field is an integer indicating which of these
#' "true" spines it belongs to---global tokens have a \code{NA} value in their \code{Spine} field, because they are considerd to not belong to any spine.
#' Any spine path splits (\code{"*^"} from the main spines form subspines, which we call \strong{Paths}.
#' Every spine's paths are numbered, from right to left, starting from \code{0L}.
#' A spine with no splits will have all \code{0L}s in its \code{Path} field.
#' 
#' @section Columns:
#' It is very useful to sometimes turn humdrum data into a true two dimensional structure, with no ragged edges.
#' (This always requires removing global records.)
#' In order to do this, while maintaining a sensible relationship between spine which have spine paths,
#' \code{\link{humRead}} automatically \emph{pads} humdrum data into a complete, non-ragged 2d table.
#' For instance, given this file
#' \preformatted{
#' **kern  **kern
#' A       E
#' *^      *
#' A       C       E
#' G       B       D
#' *v      *v      *
#' A       C        
#' *-      *-
#' }
#' \code{\link{humRead}} pads the file as so:
#' \preformatted{
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
#' }
#' (In this example, the \code{Spine}, \code{Path}, and \code{Column} values are shown below the data.)
#' The \code{"_P"} tokens stand for "padded path."
#' This appraoch assures that every \strong{Spine} is a contiguous block of tokens, of constant width.
#' In most \code{\link{humdrumR}} use cases, these padding tokens (and the \code{Column} field) can be safely ignored.
#' 
#' @section Corpus padding:
#' \code{\link{humRead}} automatically pads spine paths \emph{within pieces}.
#' However, as mentioned above, there is also (sometimes) a need to pad across pieces, in order
#' to create a logical, clean 2d structure.
#' Consider this example, with humdrum data from two pieces:
#' \preformatted{
#' (From piece 1:)
#' **kern   **kern  **kern
#' E        D       C
#' D        .       .
#' C        C       E
#' *-       *-      *-
#' (From piece 2:)
#' **kern   **kern
#' A        A
#' .        B
#' C        C
#' *-       *-
#' }
#' In this example, we have two pieces, one with three spines, the other with two.
#' There is no way to squish these two pieces into one regular 2d table.
#' But we \emph{could} pad any missing columns, as so:
#' \preformatted{
#' (From piece 1:)
#' **kern   **kern  **kern
#' E        D       C
#' D        .       .
#' C        C       E
#' *-       *-      *-
#' (From piece 2:)
#' **kern  **kern   _C
#' A        A       _C
#' .        B       _C
#' C        C       _C
#' *-       *-      _C
#' }
#' The function \code{alignColumns} is used to achieve just this effect.
#' In this example, the \code{"_C"} token stands for "padded column."
#' 
#' The presence of spine paths makes padding columns across pieces a bit more complicated.
#' What \code{alignColumns} will do, is match up all pieces in a corpus so that
#' every \strong{Spine}/\strong{Path} field pair allign in the same column.
#' Here is an example, with its paths already padded: 
#' \preformatted{
#' (From piece 1:)
#' **kern      _P        **kern
#' A           _P        E
#' B           _P        D
#' *^          _P        *
#' A           C         E
#' G#          B         E
#' *v          *v        *
#' A           _P        E
#' *-          _P        *-
#' #################################################
#' 1           1         2         Spine
#' 0           1         0         Path
#' 1           2         3         Column
#' 
#' (From piece 2:)
#' **kern     **kern    _P
#' A          E         _P
#' *          *^        _P
#' G#         D         F
#' A          C         E
#' *          *v        *v
#' E          D         _P
#' *-         *-        _P        
#' #################################################
#' 1          2         2        Spine
#' 0          0         1        Path
#' 1          2         3        Column
#' }
#' We have two pieces, each with two spines,
#' but in the first piece, the first spine splits, while in the second piece, the
#' second spine splits. Thus, the padded output will have four columns:
#' \preformatted{
#' (From piece 1:)
#' **kern    _P        **kern   _C
#' A         _P        E        _C
#' B         _P        D        _C
#' *^        _P        *        _C
#' A         C         E        _C
#' G#        B         E        _C
#' *v        *v        *        _C
#' A         _P        E        _C
#' *-        _P        *-       _C 
#' ###########################################################
#' 1         1         2        2         Spine
#' 0         1         0        1         Path
#' 1         2         3        4         Column
#' 
#' (From piece 2:)
#' **kern    _C        **kern   _P
#' A         _C        E        _P
#' *         _C        *^       _P
#' G#        _C        D        F
#' A         _C        C        E
#' *         _C        *v       *v
#' E         _C        D        _P
#' *-        _C        *-       _P 
#' #########################################################       
#' 1         1         2        2         Spine
#' 0         1         0        1         Path
#' 1         2         3        4         Column
#' }
#' Note that code{alignColumns} actually adds rows to the \code{\linkS4class{humdrumR}} object's
#' internal \code{\link[humdrumR:humTable]{humdrum tables}}.
#' @name humColumns
NULL

#####Humtable methods


splitHumtab <- function(humtab, drop = FALSE) { 
          # Splits a humtable by type
          # drop determines whether absent dataTypes
          # are returned as empty data.tables (drop = FALSE)
          # or simply ommited (drop = TRUE).
          split(humtab, 
                f = factor(humtab$Type, levels = c('G', 'L', 'I', 'M', 'D', 'd', 'P')), 
                drop = drop) 
}

spliceHumtab <- function(humtab) {
          # This combines the components of a humtab list into a single data.table
          # it also sorts them by default values
          humtab <- rbindlist(humtab, fill = TRUE)
          
          # sortby <- c('NFile', 'Spine', 'Path', 'Record', 'Stop')
          # sortby <- sortby[sapply(humtab[ , sortby, with = FALSE], class) != 'list'] 
          # Can't sort by lists
          # These should never be lists...but still
          # if (length(sortby) > 0L) do.call('setorder', c(humtab, lapply(sortby, as.symbol))) # data.table::setorder
          setorder(humtab, NFile, Record, Column, Stop)
          
          humtab
}

#######################################################-
#############################humdrumR S4 class ----
######################################################-

#' HumdrumR class
#' 
#' This \code{S4} class is the basic unit of the 
#' \code{\link[humdrum:humdrumR]{humdrumR}} package.
#' Each \code{humdrumR} object represents data \code{\link[humdrumR:readHumdrum]{read}} from one or 
#' more humdrum files.
#' In the documentation we refer to the collection of files within a \code{\linkS4class{humdrumR}} object
#' as a "\strong{corpus}, and each file as a "\strong{piece}."
#' However, though humdrum data is \emph{usually} encoded as one "piece" per file, this is not necessarily the case:
#' files might represent movements within a piece, or even just a part of a score. Still, we tend to refer
#' to them as "pieces."
#' In coding examples, we name \code{humdrumR} objects \code{\strong{humdata}}.
#' 
#' The most imporant part of a \code{humdrumR} object is the 
#' \code{\link[humdrumR:humTable]{humdrum table(s)}} it holds within it.
#' In essence, an \code{humdrumR} object is simply a wrapper around the 
#' \code{\link[humdrumR:humTable]{humdrum table}} which helps users to
#' to visualize, index, \code{\link[humdrumR:humSummary]{summarize}}, and manipulate
#' the table in a variety of ways.
#' 
#' Basic information about the size and shape of \code{humdrumR} data can be
#' obtained with calls to \code{\link[humdrumR:humSize]{nrecords, npieces, length, ncol, etc.}}
#' More detailed summary information can be obtained with the humdrumR \code{\link[humSummary]{corpus summary functions}}.
#' \code{humdrumR} data can also be coerced to more basic \code{R} data types using \code{\link[humdrumR:humCoersion]{as.matrix, as.data.frame, etc.}}
#' 
#' A number of helpful functions are also defined to \code{\link[humdrumR:humShape]{reshape}} humdrumR data.
#' 
#' 
#' @section Indexing:
#' In \code{R}, the basic \code{\link[base:Extract]{indexing operators}}
#' are \code{[]} and \code{[[]]}.
#' 
#' These are used to filter out subsets of data.
#' In many \code{R} data types (for instance, base \code{R}'s \code{\link[base:list]{list}}),
#' the \code{[}single brackets\code{]}
#' are used for "shallower" extraction while the \code{[[}double brackets\code{]]}
#' are used for "deeper" extraction.
#' \code{humdrumR} object indexing follows this same basic pattern:
#' \code{[}single brackets\code{]} are used to index \code{humdrumR} objects
#' \emph{by piece} while \code{[[}double brackets\code{]]} are used to index
#' \emph{within pieces}. (Accidentally writing \code{[]} when you need
#' \code{[[]]} is a very common error, so watch out!)
#' 
#' Whether, indexing by piece or within, \code{humdrumR} objects can use
#' three types of indexing arguments:
#' \itemize{
#' \item By \code{numeric} (ordinal integers)
#' \item By \code{character} string (regular expressions)
#' \item By \code{formula} (arbitrary expressions)
#' }
#' The last option (by \code{formula}) is the most powerful option,
#' and indeed, the first two options (\code{numeric} or \code{character} indexing)
#' are just convenient shorthands for indexing that can be accomplished using 
#' the \code{formula} method.
#' 
#' 
#' \strong{Numeric indexing:} Indexing \code{humdrumR} objects with
#'  \strong{\code{[}single brackets\code{]}} will accept
#' one numeric argument. (Non-integer arguments will be converted to integers.)
#' This argument will be used to pick pieces within the \code{humdrumR} object ordinally.
#' Thus, \code{humdata[1:10]} will select the first ten pieces in the data while \code{humdata[42]}
#' will select only the 42nd piece. 
#' 
#' 
#' Indexing \code{humdrumR} objects with
#'  \strong{\code{[[}double brackets\code{]]}} will accept 
#' one or two numeric arguments, \code{i} and \code{j}, either of which can 
#' be used in isolation or in combination.
#' If \code{j} is used in isolation, it must be placed after a comma, as in \code{humdata[[ , j ]]}.
#' (Non-integer arguments will be converted to integers.)
#' \code{i} is used to index data records (i.e., based on the \code{NData} field) ordinally.
#' Thus, \code{humdata[[1:20]]} indexes the first twenty data records \emph{from each file}
#' in the corpus, and \code{humdata[[42]]} extracts the 42nd data record \emph{from each file}.
#' \code{j} is used to index spines  (i.e., based on the \code{Spine} field) ordinally.
#' Thus, \code{humdata[[ , 3:4]]} returns the third and fourth spines \emph{from each}
#' file in the corpus.
#' 
#' Note that numeric \code{humdrumR} indexing is entirely \strong{ordinal}, meaning 
#' that pieces/data records/spines are not matched based on their value in their
#' respective fields, but rather on their order among all existing values.
#' Thus, for \code{[}single-bracket\code{]} indexing the \eqn{ith} piece in the
#' corpus is taken, regardless of that \code{FileN} field associated
#' with that piece:
#' 
#' \preformatted{
#' humsubset <- humdata[11:20]
#' humsubset[2]
#' }
#' 
#' will return the 12th piece from the original \code{humdata} object, not the second piece.
#' Similarly,
#' \preformatted{
#' humsubset2 <- humdata[[ , 2:4]]
#' humsubset2[[ , 2]]
#' }
#' will return the third spine from the original data.
#' 
#' As in traditional \code{R} indexing, negative numbers are allowed as well, causing corresponding elements to be
#' removed instead of retained. Thus, \code{humdata[-3:-5]} will remove the third, fourth, and fifth pieces from the data
#' while \code{humdata[[ , -3:-5]]} will remove the third, fourth, and fifth spines from each piece.
#' Positive and negative indices cannot be mixed in a single argument.
#' 
#' In all cases, indices outside of range (or of value \code{0)} are ignored.
#' If all indices are \code{0} or outside of range then 
#' an empty \code{humdrumR} object is returned.
#' For instance, \code{humdata[[401:500, ]]} will return an empty
#' \code{humdrumR} object if there are no pieces with more than 400
#' data records.
#' 
#' 
#' \strong{Character indexing:} Indexing \code{humdrumR} objects with 
#' \code{[}single brackets\code{]} will accept one 
#' vector of \code{character} strings. These strings are 
#' treated as 
#' \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the \code{humdrumR} object's \code{Active} fields are searched
#' for matches to all the regular expressions you input. Any piece that contains
#' \emph{\strong{any}} match to \emph{\strong{any}} of the regular expressions is retained---all other pieces
#' are dropped. Note that (because this is \code{[}single-bracket\code{]} indexing) the entire piece is retained, even if there is only one match.
#' If no matches occur in any pieces, an empty \code{humdrumR} object is returned.
#' 
#' Indexing \code{humdrumR} objects with \code{[[}double brackets\code{]]} will 
#' accept one or two vectors of \code{character} strings, \code{i} and \code{j}, 
#' either of which can 
#' be used in isolation or in combination. 
#' If \code{j} is used in isolation, it must be placed after a comma, 
#' as in \code{humdata[[ , j]]}.
#' These strings are 
#' treated as \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expressions} (regexes).
#' The tokens from the \code{humdrumR} object's \code{Active} fields are searched
#' for matches to all the regular expressions you input.
#' Any record which contains at least one token matching any regex in \code{i}
#' will be retained.
#' Similarly, any spine which contains at least one token matching any
#' regex in \code{j} is retained.
#' If \code{i} and {j} are used together,
#'  matching spines are indexed first, so that 
#' tokens matching the regular expression(s) in \code{i}
#' must be found in matching spines.
#' 
#' A third argument, \code{k}, can also be used, but only if 
#' both \code{i} and \code{j} arguments are missing.
#' In the case of \code{k}, only matching tokens are retained,
#' regardless of their spine or record number(s).
#' Any pieces, spines, or records with no matches are dropped entirely.
#' 
#' 
#' \strong{Formula indexing:} Indexing \code{humdrumR} objects with 
#' \code{formulae} is the most powerful, flexible indexing option.
#' Either \code{[}single\code{]} or \code{[}double\code{]} brackets will accept
#' a formula. The right-hand side of each formula will be evaluated
#' within the \code{humdrumR} objects internal 
#' \code{\link[humdrumR:humTable]{humdrum table}}.
#' Each formula must evaluate to a \code{logical} vector of the same 
#' length as the total number of tokens (rows in the humdrum table).
#' 
#' In the case of \code{[}single-bracket\code{]} indexing, only one \code{formula}
#' is accepted, and \emph{every piece} that evalues with at least one 
#' \code{TRUE} will be retained.
#' For instance, \code{humdata[~Spine > 4]} will return all pieces
#' which contain five (or more) spines.
#' \code{[}single-bracket\code{]} formula indexing is especially useful for indexing
#' meta-data properties like reference records:
#' for instance, \code{humdata[~COM == "Paul McCartney"]} will return
#' all pieces with a \code{!!!COM: Paul McCartney} reference record.
#' 
#' In the case of \code{[[}double-bracket\code{]]} indexing, one or two formulas are accepted, 
#' in arguments \code{i} and \code{j}, either of which can 
#' be used in isolation or in combination. 
#' If \code{j} is used in isolation, it must be placed after a comma, 
#' as in \code{humdata[[ , j]]}.
#' In the case of \code{i} formulae, any record which evaluates to
#' at least one \code{TRUE} value is retained.
#' In the case of \code{j}, any spine which evaluates to
#' at least one \code{TRUE} value is retained.
#' Any piece which contains no matches is dropped entirely.
#' 
#' For \code{[[}double-bracket\code{]]} formula indexing, a third argument, \code{k}
#' may be used in the absence of \code{i} and \code{j}.
#' In the case of \code{k} all tokens which evaluate to \code{TRUE}
#' are retained, regardless of piece/spine/record.
#' Pieces, spines, or records with no \code{TRUE} values
#' are simply dropped.
#' 
#' @section Assignment:
#' \code{R} objects often have ways of assigning new values to 
#' \emph{part} of the object using \code{\link[base:Extract]{indexing operators}}.
#' \code{humdrumR} objects are no different.
#' 
#' A new field can be inserted in a \code{humdrumR} object in two ways:
#' \enumerate{
#' \item A field can be copied from one humdrumR object to another if the humdrumR objects'
#' \code{\link[humdrumR:humTable]{humdrum tables}} have the same number of data tokens (i.e., rows).
#' This is actually most useful for renaming fields within a humdrumR object (explained below).
#' \item A \code{\link[base:vector]{vector}} or \code{\link[base:list]{list}} can be instered as a 
#' new field in a \code{humdrumR}---but again, it must be the same length as the number of tokens
#' in the object's \code{\link[humdrumR:humTable]{humdrum table}}.
#' }
#' 
#' Fields can be assigned using two syntaxes:
#' \preformatted{
#' humdata['fieldname'] <- x
#' # or
#' humdata[c('field1', 'field2')] <- x
#' }
#' or 
#' \preformatted{
#' humdata$fieldname <- x
#' }
#' 
#' \strong{\code{humdrumR$fieldname <- humdrumR} assignment}: Assigning a field in one \code{humdrumR}
#' object from another \code{humdrumR} object works like this. First of call, as a reminder, the two \code{humdrumR}
#' objects must have the exact same numbers of data tokens in their \code{\link[humdrumR:humTable]{humdrum tables}}.
#' This means, that this is most useful for assigning field names from one \code{humdrumR} object to itself.
#' The name(s) given in the indexing expression on the left side of the assignment (i.e., \code{humdata[c('name1', 'name2')]} or
#' \code{humdata$name}) are used as new field names.
#' How fields are extracted from the right side of the assignment is a little trickier:
#' Any fields in the right-side \code{humdrumR} object which are named \eqn{PipeN} (where \eqn{N} is an integer) are copied
#' in descending order into the named fields on the left side.
#' If there are no \eqn{PipeN} fields on the right side, any fields used in the current Active formula (on the right side)
#' are copied instead.
#' 
#' This system might seem odd at first, but it is very useful in combination with the \code{\link[humdrumR:with-in-Humdrum]{withinHumdrum}} function,
#' and its convenient pipe operator \code{\link[humdrumR:humPipe]{\%hum>\%}}.
#' The \code{withinHumdrum} command always creates new fields that are called \eqn{Pipe1 ... Pipe2 ... PipeN}.
#' By using \code{humdata$name} we can immediately assign these pipe fields more meaningful names!
#' 
#' Examples:
#' \preformatted{
#' humdata \%hum>\% ~ as.semits(Token) -> humdata$Semits
#' }
#' 
#' @section Active field:
#' The \code{Active} slot contains a formula, the right side of which is an expression
#' refering to fields in the \code{\link[humdrumR:humTable]{Humtable}} (the left side
#' of the formula is ignored). 
#' This expression is used as the "default" value in a lot of \code{humdrumR} code.
#' For one, it is the data which is printed by \code{\link[methods:show]{show}} calls,
#' i.e., whenever you return a \code{humdrumR} object in the terminal.
#' In any expression within a call to 
#' \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} 
#' \code{.} is automatically replaced with the \code{Active} expression.
#' 
#' The active expression can be changed with the commands 
#' \code{\link[humdrumR:setActive]{setActive or the $ operator}}.
#' This is a handy way to quickly look at different fields in your data.
#' 
#' The \code{Active} expression is often just the name of a 
#' \code{\link[humdrumR:humTable]{field}}:
#' for instance, the default value is \code{~Token}.
#' However, it can actually be any complex expression which evaluates
#' within the \code{\link[humdrum:humTable]{humdrum table}}.
#' For instance, the \code{Active} expression could be:
#' \code{~paste0(Token, " ", Record)}, which would automatically 
#' print each Token with its record number pasted to it.
#' 
#' @slot Humtable A list of \code{\link[humTable]{humdrum tables}}, each having the same fields
#' but containing data from different types of records (e.g., interpretations, data, barlines, comments).
#' @slot Files A list of two elements. The first, "Search", contains a single character representing
#' the \code{pattern} used in the call to \code{\link{readHumdrum}} which created this \code{humdrumR} object.
#' The second, "Names", is a vector of strings representing all the files which matched the \code{pattern}
#' and were read into the \code{humdrumR} object.
#' @slot Fields A list containing strings corresponding to the existing fields in the \code{humdrumR} object.
#' The fields are divided into five categories: "Data", "Structure", "Interpretation", "Formal", and "Reference"---see (\code{\link{fields}}).
#' @slot Active A formula. The right side of this formula is an expression which 
#' extracts data from field(s) in the \code{\link[humdrum:humTable]{humdrum table}}: the "active expression."
#' @slot LoadTime A \code{\link[base:DataTimeClasses]{POSIXct}} value, indicating the time at which \code{\link{readHumdrum}} was
#' called to create this \code{humdrumR} object.
#' 
#
#' 
#' @name humdrumR-class
#' @aliases humdrumRS4
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'list',
                   Files = 'list',
                   Fields = 'list',
                   Active = 'formula',
                   LoadTime = 'POSIXct'
                   )) -> makeHumdrumR

setMethod('initialize', 'humdrumR',
          function(.Object, humtab, pattern) {
            # humtab = a humdrum table
            # pattern = the original file search pattern (string)

            fields <- colnames(humtab)
            fieldcategories <- list(Data = 'Token',
                                    Structure = c('File', 'FullFileName', 'NFile',
                                                  'Column', 'Spine', 'Path', 'Stop',
                                                  'Record', 'NData', 'Global', 'Null', 'Type'),
                                    Interpretation   = c('Exclusive', 'Tandem',
                                                         fields[fields %in% knownInterpretations[knownInterpretations$Type == 'Tandem', ]$Name]),
                                    Formal    = c(grep('^Formal', fields, value = TRUE),
                                                  'BarN', 'DoubleBarN', 'BarLabel'))
            fieldcategories$Reference <- fields[!fields %in% unlist(fieldcategories)]
         
            
            .Object@Humtable  <- splitHumtab(humtab)        
            .Object@Fields    <- fieldcategories
            .Object@Active    <- ~Token
            .Object@Files     <- list(Search = pattern, Names = unique(humtab$FullFileName))
            .Object@LoadTime  <- Sys.time()
            .Object
          })



######humdrumR core methods ####

####As/Is ####

#' @name humdrumR-class
#' @export
is.humdrumR <- function(x) inherits(x, 'humdrumR')


#' \code{\linkS4class{humdrumR}} coersion.
#' 
#' Many users may wish to work with humdrum data without
#' using the \code{\link[humdrumR:humdrumR]{humdrumR}} API, instead using 
#' basic \code{R} data types.
#' For this purpose, \code{\linkS4class{humdrumR}} data objects can be coerced to 
#' basic \code{R} data types.
#' 
#' \code{\link[base:vector]{as.vector(humdata, types, mode, fields)}} evaluates the \code{\linkS4class{humdrumR}} object's
#' \strong{Active} expression, and (attempts) to force the result to a vector of mode. This
#' method is essentially a wrapper for \code{\link{evalActive}}.
#' 
#' \code{\link[base:matrix]{as.matrix(humdata, types, pad.files, pad.paths)}} also evaluates the \code{\linkS4class{humdrumR}} object's
#' \strong{Active} expression, but wraps it into a matrix of dimensions \code{c(\link[humdrumR:humSize]{nrow(humdata), ncol(humdata)}}.
#' Note that "\code{\link[humdrumR:humTable]{Columns}}" in humdrum data are not necesarrily the same as spines. 
#' 
#' \code{\link[base:data.frame]{as.data.frame(humdata)}} first calls \code{as.matrix} then converts the matrix to a \code{\link[base:data.frame]{data.frame}}.
#' \code{\link[data.table:data.table]{as.data.table(humdata)}} first calls \code{as.matrix} then converts the matrix to a \code{\link[data.table:data.table]{data.table}}.
#' 
#' \code{as.matrices}, \code{as.data.frames}, and \code{as.data.tables} call \code{as.matrix}/\code{as.data.frame}/\code{as.data.table}
#' on each individual file in a \code{\linkS4class{humdrumR}} corpus, returning a list of matices/data.frames/data.tables.
#' 
#' 
#' @param dataTypes Which types of humdrum records to include. Legal values are \code{'G', 'L', 'I', 'M', 'D', 'd'} 
#' or any combination of these (e.g., \code{"LIM"}).
#' (see the \code{\link[humdrumR:humTable]{humdrum table}} documentation \strong{Fields} section for explanation.).
#' 
#' @param pad.files \code{logical} (default \code{TRUE}). If any pieces in the \code{\linkS4class{humdrumR}} corpus have fewer 
#' \code{\link[humdrumR:humTable]{spines/columns}} than the maximum, should they be padded with the \code{padder} argument (\code{par.files == TRUE}) or
#' should an an error occur (\code{pad.files == FALSE})? Note that these "padded" points are not represented in the original humdrum data.
#' 
#' @param pad.paths \code{logical} If any spine path splits (\code{'*^'}) occur in the \code{\linkS4class{humdrumR}} data, should they be padded 
#' with the \code{padder} argument (\code{par.files == TRUE}) or
#' should an an error occur (\code{pad.paths == FALSE})? 
#' Note that these "padded" points are not represented in the original humdrum data.
#' 
#' @param padder An atomic value of length one. If \code{par.files} or \code{pad.paths} are true, the \code{padder}
#' argument is used to fill in the desired gaps.
#' 
#' 
#' @param mode If the \code{mode} argument is not \code{'any'}, it can be a single \code{character}
#' string naming an atomic mode---the output will be coerced to this mode (if possible).
#' 
#' @param field(s) If the \code{field} argument is \emph{not} \code{NULL}, it can instead be a \code{character} string matching
#' the \code{\linkS4class{humdrumR}} object's fields. If so, these fields are extracted instead of the
#' \code{\link[humdrumR:humdrumR-class]{Active expression}}.
#' For calls to \code{as.vector} and \code{as.data.frame}, only one field can be extracted.
#' However, for calls to \code{as.matrix}, multiple fields can be extraced---these fields will be
#' returned in a third matrix dimension, each field forming one rectangular slice.
#' 
#' 
#' 
#' 
#' @name humCoersion
#' @export
setMethod('as.vector', 
          signature = c(x = 'humdrumR'),
          function(x, mode = 'any') {
                    if (is.empty(x)) return(vector(mode, 0L))
                    
                    as.vector(evalActive(x, dataTypes = dataTypes, nullAs = '.', forceVector = TRUE), mode)
                    })

#' @name humCoersion
#' @export
as.lines <- function(humdrumR, dataTypes = 'GLIMDd', field = NULL, 
                     alignColumns = FALSE, padPaths = FALSE, padder = '') {
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'as.lines')
          
          
          mat <- as.matrix(humdrumR, dataTypes = dataTypes, padder = padder,
                           fields = field[1], alignColumns = alignColumns,
                           path.fold = !padPaths)
          
          lines <- apply(mat, 1, function(row) glue::glue_collapse(row[!is.na(row)], sep = '\t'))
          names(lines) <- rownames(mat)
          
          lines
                           
          
}


#' @name humCoersion
#' @usage as.matrix(humdata)
#' @export
as.matrix.humdrumR <- function(x, dataTypes = 'D', fields = NULL, 
                   alignColumns = TRUE,
                   padder = NA, 
                   path.fold = TRUE) { 
                    
                    dataTypes <- checkTypes(dataTypes, 'dataTypes', 'as.matrix')
                    if (!is.null(fields)) x <- setActiveFields(x, fields)
                    
                    if (is.empty(x)) return(matrix(character(0L), ncol = 0, nrow = 0))
                    
                    if (anyStops(x)) x <- foldStops(x, foldAtomic = TRUE, sep = ' ')
                    
                    
                    
                    paths  <- anyPaths(x)
                    if (paths && path.fold) x <- foldPaths(x, foldAtomic = TRUE, sep = '\t')
                    
                    ragged <- ragged(x)
                    #if (ragged && !alignColumns) stop("In call as.matrix(humdrumR, pad = FALSE): This humdrumR object has different numbers
                     #                                # of spines or paths across files, so it can't by made into a matrix unless pad = TRUE")
                    
                    if (ragged && alignColumns) x <- alignColumns(x, "_C")
                    
                    dataTypes <- c(dataTypes, 'P')
                    
                    x <- foldRecords(x, foldAtomic = FALSE, padPaths = TRUE)
                    
                    records <- getFields(x, fields = fields, dataTypes = dataTypes)
                    matrices <- lapply(records, stringi::stri_list2matrix, byrow = TRUE)
                    
                    if (length(matrices) == 1L) {
                              outMat <- matrices[[1]]         
                    } else {
                              outMat <- do.call(abind::abind, c(args = matrices, along = 3))
                    }
                    
                    outMat[outMat == '_C'] <- padder
                    outMat[outMat == '_P'] <- padder
                    outMat[outMat == 'NA'] <- padder
                    dimnames(outMat) <- c(list(File.Record = with(getHumtab(x, dataTypes = dataTypes), paste0(NFile, '.', Record)),
                                               Column = 1:ncol(outMat)),
                                          if (length(dim(outMat)) == 3L) list(Field = colnames(records)) else NULL)
                    
                    outMat
                    
                    
                    
}                   

#' @name humCoersion
#' @usage as.data.frame(humdata)
#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrumR'),
          function(x, dataTypes = 'D', field = NULL, padder = NA, fold.path = TRUE) {
                    if (!is.null(field) && length(field) != 1L) stop("Can only coerce one field in a humdrumR object to a data.frame.")
                    
                    as.data.frame(as.matrix(x, dataTypes, field[1], padder, fold.path), stringsAsFactors = FALSE)
          })




#' @name humCoersion
#' @export
as.matrices <- function(humdrumR, dataTypes = 'D', fields = NULL, padder = NA, path.fold = TRUE) {
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'as.matrices')
          n <- length(humdrumR)
          lapply(1:n,
                 function(i) as.matrix.humdrumR(humdrumR[i], dataTypes = dataTypes, 
                                       fields = fields, 
                                       padder = padder, 
                                       path.fold = path.fold))
          
}
#' @name humCoersion
#' @export 
as.data.frames <- function(humdrumR) {
          
}



# A humdrumR object is treated differently depending on whether its
# active columns contain atomic data ("isActiveVector") or not (tables, lists, matrices, etc.).
# this function tests if the active column is a vector or not
#' @export
isActiveVector <- function(humdrumR) {
          act <- evalActive(humdrumR)
          !is.list(act)
}




####Shape ####

#' humdrumR size and shape
#' 
#' These functions can be used to quickly
#' get basic information about the size and "shape" of
#' a \code{\linkS4class{humdrumR}} corpus.
#' For more details, use the \code{\link[humdrumR:humSummary]{census}} function.
#' 
#' A few common base \code{R} methods are defined
#' as synonyms for the humdrumR-specific sizing functions:
#' \code{\link[base:length]{length(humdata)}} is equivalent to \code{npieces(humdata)};
#' \code{\link[base:nrow]{nrow(humdata)}} is shortand for \code{nrecords(., dataTypes = 'LIMDd')} (i.e., local records only).
#' \code{\link[base:ncol]{ncol(humdata)}} returns the \emph{maximum} value of the \code{\link[humdrumR:humTable]{Column}} fields---the maximum number of
#' tab-delineated columns in the humdrum files (irrespective of Spines/Paths).
#' The results of \code{\link[base:nrow]{nrow}} and \code{\link[base:ncol]{ncol}} will match
#' up with the dimensions of matrices/data.frames produced by calls to \code{\link[humdrumR:humAs]{as.matrix/as.data.frame}}.
#' \code{\link[base:dim]{dim(humdata)}} returns \code{c(nrow(humdata), ncol(humdata))}, as usual.
#' 
#' \code{is.empty(humdata)} asks if \code{ntokens(humdata, dataTypes = 'D') == 0L}.
#' 
#' @name humSize
#' @export
nrecords <- function(humdrumR, dataTypes = 'D') {
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)

          n <- humtab[ , .(NR = length(unique(Record))), by = File] 
          
          sum(n$NR)
}

#' @name humSize
#' @export
ntokens <- function(humdrumR, dataTypes = 'D') {
          humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
          
          nrow(humtab)
}

#' @name humSize
#' @export
npieces <- function(humdrumR) {
          humtab <- getD(humdrumR)
          
          length(unique(humtab$NFile))
}

#' @name humSize
#' @export
nfiles <- npieces

#' @name humSize
#' @usage length(humdata)
#' @export
setMethod('length',
          signature = c(x = 'humdrumR'),
          function(x) npieces(x))

#' @name humSize
#' @export
setMethod('nrow', 
          signature = c(x = 'humdrumR'), 
          function(x) nrecords(x, dataTypes = 'LIMDd'))

#' @name humSize
#' @export
is.empty <- function(humdrumR) ntokens(humdrumR, 'D') == 0L



#' @export
anyPaths <- function(humdrumR) {
          humtab <- getD(humdrumR)
          
          any(humtab$Path > 0L, na.rm = TRUE)
          
}

#' @export
anyStops <- function(humdrumR) {
          humtab <- getD(humdrumR)
          any(humtab$Stop > 1L, na.rm = TRUE)
          
}


ragged <- function(humdrumR) {
          # Do the pieces in the corpus vary in number of columns?
          
          humtab <- getD(humdrumR)
          
          ncols   <- humtab[!is.na(Column) , length(unique(Column)), by = File]$V1
          nspines <- humtab[!is.na(Spine)  , length(unique(Spine)) , by = File]$V1
          
          length(unique(ncols)) > 1L || length(unique(nspines)) > 1L
          
}

#### Reshaping ----

#' @name humColumns
#' @export
alignColumns <- function(humdrumR, padder = '_C') {
          # This function pads and columns and/or paths
          # which appear don't appear in all pieces
          # and renumbers the Column field, such that
          # all pieces have the same number of columns.

          checkhumdrumR(humdrumR, 'alignColumns')
          
          humtab <- getHumtab(humdrumR, c('LIMDdP'))
          
          
          ### Figuring out new column for each Spine/Path combination
          allSpinePathcombs <- which(humtab[!is.na(Spine) , table(Spine, Path) > 0L], arr.ind = TRUE) # !is.na(Spine) should be unecessary
          allSpinePathcombs <- allSpinePathcombs[order(allSpinePathcombs[ , 'Spine']), ]
          
          cols <- seq_len(nrow(allSpinePathcombs))
        
          maxP <- max(humtab$Path) + 1L
          maxS <- max(humtab$Spine)
          
          # Initialize empty matrix
          PSmat <- matrix(NA_integer_, nrow = maxS, ncol = maxP, dimnames = list(Spines = unique(humtab$Spine), Paths = unique(humtab$Path)))
          PSmat[allSpinePathcombs] <- cols # Fill it
          
          humtab$Column <- PSmat[cbind(humtab$Spine, humtab$Path + 1)]
          
          
          ### Creating new "P" humdrum table
          copyfields <- fields(humdrumR, c('Structure', 'Reference'))$Name
          humtabPadded <- humtab[ , {
                    missingColumns <- cols[-unique(Column)]
                    newtab <- merge(.SD, all = TRUE,
                                    expand.grid(Column = missingColumns, Record = unique(Record)))
                    copyfields <- copyfields[sapply(.SD[ , copyfields, with = FALSE], 
                                                    function(col) length(unique(col[!is.na(col)])) == 1L)]
                    
                    newtab[ , copyfields] <- .SD[1, copyfields, with = FALSE]
                    newtab
                    }
                    , by = File, .SDcols =  colnames(humtab)]
          
          newrows <- is.na(humtabPadded$Type)
          humtabPadded$Stop[newrows]  <- 1L
          humtabPadded$Type[newrows]  <- "P"
          humtabPadded$Token[newrows] <- padder
          
          humtabPadded$Spine <- allSpinePathcombs[humtabPadded$Column, 'Spine'] 
          humtabPadded$Path  <- allSpinePathcombs[humtabPadded$Column, 'Path' ] - 1
          
          setorder(humtabPadded, NFile, Column, Record, Stop)
          
          putHumtab(humdrumR, drop = TRUE) <- humtabPadded
          humdrumR
          
}
# 
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

#########################################foldHumdrum ----

#' These functions are used to change the "shape"
#' of data stored in \code{\link[humdrumR:humTable]{humdrum tables}}
#' (held within \code{\linkS4class{humdrumR}} objects of course).
#' 
#' The \code{foldXXX} family allows you collapse all 
#' \code{\link[humdrumR:humTable]{user fields}}
#' across groups in another field.

#' @param humdrumR A \code{\linkS4class{humdrumR}} data object.
#' (see the \code{\link[humdrumR:humTable]{humdrum table}} documentation \strong{Fields} section for explanation.).
#' @param foldAtomic \code{logical}. If \code{foldAtomic == TRUE}, each stop is collapsed to a single string
#' \code{foldAtomic == FALSE}, each stop is collapsed to a list of tokens. 
#' @param sep \code{character}. If \code{foldAtomic == TRUE}, collapsed tokens are separated by this string.
#' @param pad \code{logical}. Should \code{\link[humdrumR:humColumns]{path/column padding tokens}} be included?
#' 
#' @name humShape
#' @export
foldHumdrum <- function(humdrumR, byfields, 
                        foldAtomic = TRUE, sep = ' ', padPaths = FALSE) {
          # This function is the primary function for "folding"
          # tokens across groups in another field.
          # Most of the arguments are described in the user documentation
          # (because users use them).
          # byfields determines what fields to fold across.
          # byfields should be a character vector.
          # suitable for the "by" argument in a data.table[].
          # funcname is just the name of the use function that is being called
          # (it is only used in case of an error).
          # passfunc is a function which lookas at a humtable
          # and determines if the humtable should be returned with no changes.
          checkhumdrumR(humdrumR, 'foldHumdrum')
          
          dataTypes <- if (padPaths) "GLIMDdP" else "GLIMDd"
          humtab   <- getHumtab(humdrumR, dataTypes)
          
          # What fields do apply to?
          fieldnames <- unique(c(fields(humdrumR, "Data")$Name, activeFields(humdrumR)))
          fieldtypes <- sapply(humtab[ , fieldnames, with = FALSE], class)
          
          # What fields to apply across
          byfields <- fieldMatch(humdrumR, byfields)
          byfields <- do.call('call', c(".", lapply(byfields, as.symbol)), quote = TRUE)
          
          #### Construct the expressions which will do the work
          #This is a list of expressions, one to collapse each field.
          foldexprs <- Map(function(name, type) {
                    name <- as.symbol(name) 
                    
                    if (type == 'list') return(call('do.call', 'c', name))
                    
                    if (foldAtomic) call('paste', name, collapse = sep) else call('list',  name)},
                    fieldnames, fieldtypes)
          #This puts the pasteexprs into a single expression
          foldexpr <- do.call('call', quote = TRUE,
                               c('list', foldexprs))
          
          ## Expressions will be first saved into tmpfieldnames, 
          # because data.table doesn't allow in place changes if the type changes
          tmpfieldnames <- paste0(fieldnames, '_xxxfoldedxxx')
          
          ## Build the final expression that is evaluated
          datatableExpr <- quote(humtab[ , X := Y]) # X and Y are just tmp dummies
          datatableExpr <- substituteName(datatableExpr, subs = list(X = call('c', tmpfieldnames), 
                                                                     Y = foldexpr))
          datatableExpr[['by']] <- byfields
          
          ### EVALUATE IT!
          eval(datatableExpr) # in place!
          
          
          ### Get rid of rows we don't need anymore
          reduceExpr <- quote(humtab[ , .SD[1]])
          reduceExpr[['by']] <- byfields
          newhumtab <- eval(reduceExpr)
          
          ## Make sure that null tokens which have been grouped with non-null tokens (d with D, or P with anything)
          ## are now marked as non-null type
          typeExpr <- quote(humtab[ , {
                    if (Type[1] == 'P' && any(Type != 'P')) Type[1] <- Type[Type != 'P'][1]
                    output <- if (any(Type == 'D') && any(Type == 'd')) 'D' else Type[1]
                    output
          }])
          typeExpr[['by']] <- byfields
          newhumtab$Type <- eval(typeExpr)$V1
          
          ## Rename temp colnames
          newhumtab[ , eval(fieldnames) := NULL] # inplace
          colnames(newhumtab) <- gsub('_xxxfoldedxxx$', '', colnames(newhumtab))
          
          if (!padPaths) newhumtab <- rbindlist(list(newhumtab, getHumtab(humdrumR, 'P')), use.names = TRUE)
          putHumtab(humdrumR, drop = FALSE) <- newhumtab
          humdrumR
}

          
#' @name humShape
#' @export 
foldStops <- function(humdrumR, foldAtomic = TRUE, sep = ' ') {
          checkhumdrumR(humdrumR, 'foldStops')
          
          humtab <- getHumtab(humdrumR)
          if (!any(humtab$Stop > 1L & !is.na(humtab$Stop))) return(humdrumR)
          
          foldHumdrum(humdrumR, byfields = c('File', 'Spine', 'Record', 'Path'), 
                      foldAtomic = foldAtomic, sep = sep)
}

#' @name humShape
#' @export
foldPaths <- function(humdrumR, foldAtomic = TRUE, sep = ' ') {
          checkhumdrumR(humdrumR, 'foldPaths')
          # First some necessary preprocessing
          
          if (!anyPaths(humdrumR)) return(humdrumR)
          
          output <- foldHumdrum(humdrumR, byfields = c('File', 'Record', 'Spine'), 
                                foldAtomic = foldAtomic, sep = sep, padPaths = FALSE)
          
          output@Humtable$P <- output@Humtable$P[0]
          
          output
          
}

#' @name humShape
#' @export
foldRecords <- function(humdrumR, foldAtomic = TRUE, sep = ' ', padPaths = FALSE) {
          checkhumdrumR(humdrumR, 'foldRecords')
          
          humtab <- getHumtab(humdrumR)
          if (!any(humtab$Column > 1L & !is.na(humtab$Column))) return(humdrumR)
          
          
          foldHumdrum(humdrumR, byfields = c('File', 'Record'), 
                      foldAtomic = foldAtomic, sep = sep, padPaths = padPaths)
          
}



# collapseRecords <- function(humdrumR, pad = FALSE, trim = NA, global = TRUE) {
#           # takes a humdrumR object, and collapses each record to a string, returning a new data.table
#           # this relies heavily on foldRecords
#           
#           humtab_rv <- foldRecords(humdrumR, global = global)
#           
#           #trim
#           if (!is.na(trim)) {
#                     humtab_rv[ , Active := Map(Vectorize(trimLongString, vectorize.args = 'strs'),
#                                                Active,
#                                                ifelse(global, max(80L, trim), trim))]
#           }
#           
#           #pad
#           if (pad) humtab_rv[ , Active := list(padRecords(Active, Global)), by = .(NFile)]
#           
#           #collapse
#           sep <- if (pad) '' else '\t'
#           humtab_rv[['Active']]  <- humtab_rv[ , sapply(Active, paste, collapse = sep)]
#           
#           humtab_rv
# }


# padRecords <- function(recvecs, global) {
#           # this is a tool used by collapseRecords
#           strlengths <- lapply(recvecs, stringr::str_length)
#           
#           mat <- do.call('rbind', strlengths[!global])
#           colsizes <- apply(mat, 2, max, na.rm = TRUE) + 3
#           
#           recvecs[!global] <- lapply(recvecs[!global], 
#                                      function(recvec) { stri_pad_right(recvec, colsizes) })
#           
#           recvecs
# }


#################################-
############Humtable manipulation and access ####
###############################-

checkTypes <- function(dataTypes, argname, callname) {
          dataTypes <- unique(unlist(strsplit(dataTypes, split = '')))
          checkArgs(dataTypes,
                    c('G', 'L', 'I', 'M', 'D', 'd', 'P'),
                    argname, callname, warnSuperfluous = TRUE, 
                    min.length = 1L, max.length = 7L,
                    classes = "character")
}

#' @name Humtable
#' @usage getHumtab(humdrumR, dataTypes = 'GLIMdD')
#' @export
getHumtab <- function(humdrumR, dataTypes = c('G', 'L', 'I', 'M', 'D', 'd')) {
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'getHumtab')
          
          humtab <- humdrumR@Humtable[dataTypes]
          # if (!allsame(sapply(humtab, ncol))) {
          # humtab <- indexGLIM(humdrumR)@Humtable[dataTypes]
          # }
          spliceHumtab(humtab)
}

getD <- function(humdrumR) getHumtab(humdrumR, dataTypes = 'D')

`putHumtab<-` = function(humdrumR, value, drop = FALSE) {
          # adds humtab into humdrumR
          # Drop determines whether record dataTypes that are 
          # absent from value are left unchanged (drop = TRUE)
          # or replaced with empty data tables (drop = FALSE)
          if (is.data.table(value)) value <- splitHumtab(value, drop = drop)
          humdrumR@Humtable[names(value)] <- value
          
          humdrumR
}

`putD<-` <- function(humdrumR, value) { 
  # This is a shortcut to replacing non-dull data records
  # in a humdrumR object. I also forces these
  # replacement values to be non-null D
  value$Type <- 'D'
  value$Null <- FALSE
  humdrumR@Humtable[['D']] <- value
  humdrumR
}




####### Active slot ----
##### Manipulating the Active slot

#'
#' \code{evalActive} evaluates the active expression in a
#' \code{\linkS4class{humdrumR}} object.
#' 
#' 
#' @param humdrumR A \code{\linkS4class{humdrumR}} data object.
#' @param dataTypes Which dataTypes of humdrum records to include. Legal values are \code{'G', 'L', 'I', 'M', 'D', 'd', 'P'} 
#' or any combination of these in a single string (e.g., \code{"LIM"}).
#' (see the \code{\link[humdrumR:humTable]{humdrum table}} documentation \strong{Fields} section for explanation.).
#'  (see the \code{\link[humdrumR:humTable]{humdrum table}} documentation \strong{Fields} section for an explanation.).
#' @param forceVector \code{logical}. If \code{TRUE}, the result is forced to be an atomic vector.
#' #' @param sep A length-one \code{character} string. If \code{forceVector == TRUE} this value is used as a separator 
#' between tokens that are collapsed.
#' @param nullAsDot A single \code{atomic} value. Any null tokens are coerced to this value (default is \code{.}).
#' @export
evalActive <- function(humdrumR, dataTypes = 'D', forceVector = FALSE, sep = ', ', nullAs = NA)  {
  humtab <- getHumtab(humdrumR, dataTypes = dataTypes)
  locnames <- humtab[ , paste(NFile, Spine, Path, Stop, Record, sep = '.')]
  
  values <- lazyeval::f_eval(humdrumR@Active, data = humtab)
  
  
  if (is.atomic(values)) {
    values[is.na(values) | values == '.'] <- nullAs
  } else {
    values[] <- lapply(values, 
                       function(col) {
                                 col[is.na(col) | col == '.'] <- nullAs
                                 xnames(col) <- locnames
                                 col
                                 })
  }
  
  if (forceVector) {
            if (is.list(values)) {         
                      if (length(unique(lengths(values))) == 1L) {
                                values <- values <- do.call('paste', c(values, sep = sep))
                      } else {
                                values <- sapply(values, paste, collapse = sep)
                      }
            }
            if (is.matrix(values)) {
                      out <- character(nrow(humtab))
                      out[humtab$Type != 'D'] <- apply(values[humtab$Type != 'D', ], 1, function(row) paste(unique(row), collapse = sep))
                      out[humtab$Type == 'D'] <- apply(values[humtab$Type == 'D', ], 1, function(row) paste(       row , collapse = sep))
                      rownames(out) <- locnames
                      values <- out
            }   
            if (is.object(values)) values <- as.character(values)
  }
  
  values
}

#' \code{getActive(humdata)} is simply a wrapper for \code{evalActive(humdata, dataTypes = 'D', forceVector = TRUE, nullAsDot = TRUE)}.
#' @name humActive
#' @export
getActive <- function(humdrumR) lazyeval::f_rhs(humdrumR@Active)


#' \code{setActive} takes a \code{\linkS4class{humdrumR}} object and a formula
#' and sets that formula as the object's Active expression.
#' @name humActive
#' @export
setActive <- function(humdrumR, form) {
  # form <- lazyeval::f_capture(expr)
  putActive(humdrumR, form)
}



#' \code{setActiveFields} takes a character vector of strings representing current
#' \code{\link[humdrumR:humTable]{field}} names
#' and sets the \code{\linkS4class{humdrumR}} object's active expression
#' to simply return those fields (as a list, if there are more than one).
#' @name humActive
#' @export
setActiveFields <- function(humdrumR, fieldnames) {
  fieldnames <- fieldMatch(humdrumR, fieldnames)
  scall <- if (length(fieldnames) > 1) {
            do.call('call', c('list', lapply(fieldnames, as.symbol)), quote = TRUE)
            } else {
                      as.symbol(fieldnames)
            }
  form <- f_new(rhs = scall)
  putActive(humdrumR, form)
}

putActive <- function(humdrumR, form) {
          # This does the dirty work for 
          # setActive and setActiveFields
          humtab <- getD(humdrumR)
          usedInExpr <- fieldsInFormula(humtab, form)
          if (length(usedInExpr) == 0) stop("The 'active'-field formula for a humdrumR object must refer to some field.\n
Add a reference to some field, for instance Token.", call. = FALSE)
          
          humdrumR@Active <- form
          
          act <- evalActive(humdrumR)
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
          checkArgs(types,
                    c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference'),
                    argname, callname, warnSuperfluous = TRUE,
                    min.length = 0L, max.length = 5L,
                    classes = 'character')
}

#' This controls which humdrumR data are printed and default target for pipe.
#' @name humdrumR-class
#' @usage humdata$Field
#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
            name <- as.character(name)
            
            matches <- fieldMatch(x, name)
            
            if (is.na(matches))  stop(glue::glue("You are trying to use the $ operator to make a field within a humdrumR object active,
                                                 but there is no field called '{name}'"), call. = FALSE)
            
            setActiveFields(x, matches)
          })

fieldMatch <- function(humdrumR, names) {
          fields <- fields(humdrumR)$Name
          target <- pmatch(names, fields)

          fields[target]

}

#' Use \code{fields} to list the current fields in 
#' a \code{\linkS4class{humdrumR} object.
#' @name humdrumR-class
#' @export
fields <- function(humdrumR, fieldTypes = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')) { 
  #
  D <- getD(humdrumR)
 
  valid <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')
  fieldTypes <- valid[pmatch(fieldTypes, valid)]
  fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', 'field')
            
  fields <- unlist(humdrumR@Fields[fieldTypes])
  
  D <- D[ , fields, with = FALSE]
  classes <- sapply(D, class)
  
  if (any(lists <- classes == 'list')) {
    classes[lists] <- paste0('list (of ',
                             sapply(D[ , lists, with = FALSE],
                                                function(col) {
                                                  glue::glue_collapse(paste0(unique(sapply(col, class)), "s"), sep = ', ', last = ', and ')
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

fieldsInFormula <- function(humtab, form) {
  ## This function identifies which, if any,
  ## fields in a humtable are referenced in a
  ## formula (rhs expression).
  if (is.humdrumR(humtab)) humtab <- getHumtab(humtab)          
          
  expr  <- lazyeval::f_rhs(form)
  colnms  <- colnames(humtab)
  
  applyExpr(expr, rebuild = FALSE,
            function(ex) {
              exstr <- deparse(ex)
              match <- colnms[pmatch(exstr, colnms)]
              if (is.na(match)) NULL else match
            }) -> usedInExpr
  unique(unlist(usedInExpr))
}

activeFields <- function(humdrumR) {
          # Identifies which fields are used in
          # the current \code{Active} expression.
          fieldsInFormula(getD(humdrumR), humdrumR@Active)
}


isField <- function(humdrumR, names) {
          ## checks if character strings in "names"
          ## argument match the names of any fields
          ## in the humdrumR argument
          names %in% fields(humdrumR)$Name 
}

#' Get named 
#' @export
getFields <- function(humdrumR, fields = NULL, dataTypes = 'D') {
          dataTypes <- checkTypes(dataTypes, 'dataTypes', 'getFields')
          
          if (is.null(fields)) fields <- activeFields(humdrumR)
          
          fields <- fieldMatch(humdrumR, fields)
          
          humtab <- getHumtab(humdrumR, dataTypes)
          
          humtab[ , fields, with = FALSE]
          
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

`padGLIMfields<-` <- function(object, copyField = NULL, value)  {
  # This function is used be indexGLIM
  # It is used to add empty fields to a humtable.
  # withinHumdrum (or humApply) may add new fields to the 'D' (data)
  # humtable, and we need to add these same fields to the other 
  # humtables (i.e., GLIMdP)
  # By default, these blank fields are null tokens.
  # The argument copyField can be the name of another field, which is 
  # copied instead of null tokens.
  humtab <- object
  nulltypes <- c(G = '!!', I = '*', L = '!', d = '.', D = NA, M = '=', P = "_P")
  for (name in value) {
    newfield <- if (is.null(copyField)) nulltypes[humtab$Type] else humtab[[copyField]]
    humtab[[name]] <- newfield
  }
  
  humtab
}

############## Indexing humdrumR #######



numericIndexCheck <- function(i) {
          if (any(i < 0) && any(i > 0)) stop("You can't mix negative and positive numbers when trying to index humdrumR objects.")
          if (any(i == 0) && any(i != 0)) warning("Your indexing of a humdrumR object is mixing zeros in with non-zero numbers. These zeros are simply ignored.")
}

formulaIndexCheck <- function(i, humdrumR) {
          if (!is.null(lazyeval::f_lhs(i))) warning("When indexing humdrumR objects with formulae, the left-hand side of formulae are ignored.")
          if (length(fieldsInFormula(humdrumR, i)) == 0L) stop("When indexing humdrumR objects with formulae, the formulae must refer to at least one field in the object.")
}


####[]

#' @name humdrumR-class
#' @usage humdata[] # returns unchanged
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing'),
          definition = force)

##[numeric]

#' @name humdrumR-class
#' @usage humdata[x:y]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i) {
            numericIndexCheck(i)          
            # Gets files with numeric index of unique file numbers
            humtab <- getHumtab(x, 'GLIMDdP')
            i <- sort(unique(humtab$NFile))[i]
              
            putHumtab(x, drop = FALSE) <- humtab[NFile %in% i]
            x
          })

##[character]

#' @name humdrumR-class
#' @usage humdata['regex']
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i) {
            # Gets files which contain matches to i
            humtab <- getHumtab(x, 'GLIMDdP')
            
            matches <- grepls(i, evalActive(x))
            
            i <- unique(humtab$NFile[matches])
            
            putHumtab(x, drop = FALSE) <- humtab[NFile %in% i]
            x
          })

##[formula]

#' @name humdrumR-class
#' @usage humdata[~expression]
#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'formula'),
          function(x, i, .f = any) {
            # Evaluates formula (must return a boolean)
            # within the humdrumR object, and returns
            # all files which contain ANY TRUE.
            
            humtab <- evalIndexFormula(x, i)
            humtab <- humtab[ , if (.f(.indhits, na.rm = TRUE)) .SD, by = File]
            
            putHumtab(x, drop = FALSE) <- humtab[ , .indhits := NULL]
            x
          })

####[[]]



##[[numeric]]

#' @name humdrumR-class
#' @usage humdata[[x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i) {
            numericIndexCheck(i)    
            D <- getD(x)
            
            # In each piece, identify existing Records and index according to them ordinally.
            D <- D[,
                   {ipiece <- sort(unique(Record))[i]
                    .SD[Record %in% ipiece] }, 
                   by = File]
            
            putD(x) <- D
            x
          })


#' @name humdrumR-class
#' @usage humdata[[ , x:y]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j) {
            numericIndexCheck(j) 
            D <- getD(x)
            
            # In each piece, identify existing Spines and index according to them, ordinally.
            D <- D[,
                   { ipiece <- sort(unique(Spine))[j]
                     .SD[Spine %in% ipiece] }, 
                   by = File]
            
            putD(x) <- D
            x
          })

#' @name humdrumR-class
#' @usage humdata[[x:y, l:m]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'numeric'), 
          function(x, i, j) {
            numericIndexCheck(i) 
            numericIndexCheck(j)
            
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
          })

#### [[character]]

grepingind <- function(humdrumR, ind, func) {
          Dd <- getHumtab(humdrumR, dataTypes = c('D', 'd'))
          Dd[ , .indhits := grepl(pattern = ind, evalActive(humdrumR, dataTypes = c('D', 'd')))]
          
          Dd <- Dd[ , func(.SD), by = NFile]
          putHumtab(humdrumR, drop = TRUE) < Dd[ , '.indhits' :=  NULL]
          humdrumR
}


#' @name humdrumR-class
#' @usage humdata[['regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
          function(x, i) {
            # gets any record which contains match
            grepingind(x, i,  function(sd) { 
              recn <- unique(sd$Record[sd$.indhits])
              sd[Record %in% recn]
            })
          })


#' @name humdrumR-class
#' @usage humdata[[ , 'regex']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j) {
            #gets any spine which contains match
            grepingind(x, j,  function(sd) { 
              recn <- unique(sd$Spine[sd$.indhits])
              sd[Spine %in% recn]
            })
          })



#' @name humdrumR-class
#' @usage humdata[['regex1', 'regex2']]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'character'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
          })

#' @name humdrumR-class
#' @usage humdata[[] , , ~expression]] or humdata [[ , , 'regex']] or humdata[z = ~expression] or humdata[[z = 'regex']]
#' @export
setMethod('[[',
          signature = c(x = 'humdrumR', i = 'missing', j = 'missing'),
          definition = function(x, i, j, k) {
                    if (missing(k)) return(x)
                    
                    if (is.character(k)) {
                              x <- grepingind(x, k,  function(sd) { 
                                        sd[sd$.indhits]
                              })
                    }
                    if (is.formula(k)) {
                              D <- evalIndexFormula(x, k)
                              D <- D[.indhits]
                              putD(x) <- D[ , .indhits := NULL]
                    }
                    
                    x
          })


##[[logical]]

setMethod('[[',  signature = c(x = 'humdrumR', i = 'logical', j = 'missing'), 
          function(x, i) {
                    #gets rows which are TRUE
                    D <- get(D)
                    
                    putD(x) <- D[i]
                    x
          })

##[[formula]]

evalIndexFormula <- function(humdrumR, form) {
          formulaIndexCheck(form, humdrumR)   
          
          humtab <- getHumtab(humdrumR, 'GLIMDdP')
          
          expr   <- lazyeval::f_rhs(form)
          humtab[ , .indhits := eval(expr)] 
}

#' @name humdrumR-class
#' @usage humdata[[~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula', j = 'missing'), 
          function(x, i) {
                    D <- evalIndexFormula(x, i)
                    
                    D <- D[ , 
                            {irecords <- unique(Record[.indhits])
                            .SD[Record %in% irecords]}, 
                            by = File]
           
                    putD(x) <- D[ , .indhits = NULL]
                    x
          })

#' @name humdrumR-class
#' @usage humdata[[ , ~expression]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'formula'), 
          function(x, j) {
                    D <- evalIndexFormula(x, j)
                    
                    D <- D[ , 
                            {ispines <- unique(Spine[.indhits])
                            .SD[Spine %in% irecords]}, 
                            by = File]
                    
                    putD(x) <- D[ , .indhits = NULL]
                    x
          })

#' @name humdrumR-class
#' @usage hudmata[[~expression1, ~expression2]]
#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula', j = 'formula'), 
          function(x, i, j) {
                    x <- x[[ , j]]
                    x[[i, ]]
          })


####GLIMd indexing 

# All humdrumR indexing relates to data records.
# However, in order to reassemble and show humdrum data
# after indexing humdrum data records, we also need to index 
# non-data records.
# The following functions, which I call GLIMd indexing functions,
# are not used by users, but are called (when necessary) to try
# and strip away in GLIMd records that are defunct (because data
# records have been stripped away).

            


indexGLIM <- function(humdrumR, dataTypes = c('G', 'L', 'I', 'M', 'd', 'P')) {
  #####ReIndex GLIMd humdrum tables to match indexing of D tables
  # To do this we need to:
  #    1) Make sure the GLIMd tables have the same field as D tables
  #    2) Make sure that pieces which are missing from the D table (because they've been indexed away) 
  #       are removed from GLIMd table (most likely)
  #    3) Make sure, on a piece-by-piece basis that
  #         a) Spines which are missing from D are removed from GLIMd
  #         b) That there are no records which are all d (null)
  #         c) That there are no "bunched up" barlines at the beggining, end, or anywherei in the middle.
  #         d) That there are no tandem interpretations AFTER the last data token.
          
  GLIMDdP <- getHumtab(humdrumR, dataTypes = dataTypes)
  D     <- getD(humdrumR)
  
  # first add missing fields (columns)
  missingfields <- colnames(D)[!colnames(D) %in% colnames(GLIMDdP)]
  if (length(missingfields) > 0L) padGLIMfields(GLIMDdP) <- missingfields
  
  GLIMDdP <- GLIMDdP[ , colnames(D), with = FALSE] # match order of columns
  
  # next  remove missing pieces (i.e., File or NFile fields)
  GLIMDdP <- GLIMDdP[NFile %in% unique(unlist(D$NFile))]
  
  # then, do indexing by piece:
  # (GLIMd and D records are combined into one table, then split by piece)
  GLIMDdP <- rbindlist(list(D, GLIMDdP), fill = TRUE)[ , indexGLIMd_piece(.SD), by = NFile, .SDcols = colnames(D)]
  
  # resplit and put back in to humdrumR object
  putHumtab(humdrumR, drop = TRUE) <- GLIMDdP
  humdrumR
}


indexGLIMd_piece <- function(humtab) {
  ###called by indexGLIM on each individual piece
  D <- humtab[Type == 'D']
  # remove missing spines
  humtab <- humtab[Spine %in% unique(D$Spine) | is.na(Spine)]
  
  # remove all except last barline before first data record
  prebarline <- unique(humtab$Record[humtab$Type == 'M' & humtab$Record < min(D$Record, na.rm = TRUE)])
  if (lennot0(prebarline))   humtab <- humtab[!(Record < prebarline[length(prebarline)] & Type == 'M')]
  
  #remove everything after last data record, except global stuff, '*-' or '=='
  humtab <- humtab[!(Record > max(D$Record, na.rm = TRUE) & !(is.na(Spine) | Token %in% c('*-', '==', '*v', '*^')))]
  
  
  if (any(humtab$Type == 'd')) {
    #remove records containing only d
    rectab <- table(humtab$Record, humtab$Type)
    recs   <- as.numeric(rownames(rectab))
    humtab <- humtab[Record %in% recs[(!(rectab[ , 'd'] > 0 & rectab[ , 'D'] == 0))]]
  }
  
  if (any(humtab$Type == 'M')) {
    #remove consecutive barlines
    rectab <- table(humtab$Record, humtab$Type)
    recs   <- as.numeric(rownames(rectab))
    humtab <- humtab[Record %in% recs[!(rectab[ ,'M'] == rotate(rectab[ ,'M'], rotation = -1, pad = -1) & rectab[ ,'M'] > 0)]]
  }
  #
  humtab
  
}



############## Assigning to humdrumR #######


###$<- simply calls []<- indexing!
#' @name humdrumR-class
#' @usage humdata$name <- vector
#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'vector'), function(x, name, value) { x[name] <- value ; x  })

#' @name humdrumR-class
#' @usage humdata$name <- humdata2
#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'humdrumR'), function(x, name, value) { x[name] <- value ; x  })


#' @name humdrumR-class
#' @usage humdata['character'] <- vector
#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', j = 'ANY', value = 'vector'),
          function(x, i, j, value) {
                    D <- getD(x)
                    
                    if (length(value) == nrow(D)) {
                              D[i] <- value
                    } else {
                              stop(glue::glue("Can't assign this value to '{name}' field, because it is the wrong length.
                                              It must be the same length as the number of data tokens (rows) in the Data humdrum table."))
                    }
                    
                    putD(x) <- D
                    padGLIMfields(x) <- i
                    
                    x <- setActiveFields(x, i)
                    return(x)
          })


#' @name humdrumR-class
#' @usage humdata['character'] <- humdata2
#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', j = 'ANY', value = 'humdrumR'),
          function(x, i, j, value) {
                    # This function copies one or more PipeN fields from one humdrumR object
                    # into named fields in a different (or the same) humdrumR object of the same size.
                    # If these named fields don't exist, they are created.
                    # If there are no PipeN fields, the active field(s) are copied.
                    humtab <- getD(value)
                    
                    removeFields(value) <- grep('Pipe', colnames(humtab), value = TRUE)
                    pipes <- pipeFields(humtab)
                    
                    if (length(pipes) == 0L) pipes <- activeFields(value)
                    
                    pipes <- tail(pipes, n = length(i))
                    
                    colnames(humtab)[colnames(humtab) %in% pipes] <- i
                    
                    if (any(grepl('Pipe', colnames(humtab)))) humtab[ , eval(grep('Pipe', colnames(humtab), value = TRUE)) := NULL]
                    
                    putHumtab(value, drop = TRUE) <- humtab
                    addFields(value) <- i
                    
                    value@Active <- substituteName(value@Active, setNames(list(as.symbol(i)), pipes))
                    
                    value
          })




####################################################-
#########################Print methods ----
#########################################################-

#' @export
print_humtab <- function(humdrumR, dataTypes = "GLIMDd", firstAndLast = TRUE,
                         max.records.file = 40L, max.token.length = 12L) {
  dataTypes <- checkTypes(dataTypes, 'dataTypes', "print_humtab")
  
  if (is.empty(humdrumR)) {
    cat("\nEmpty humdrumR object\n")
    return(invisible(NULL))
  }
  
  Nfiles <- length(humdrumR)          
  if (firstAndLast) humdrumR <- humdrumR[c(1, length(humdrumR))]
  
  if (isActiveVector(humdrumR)) {
    print_humtab_isActiveVector(humdrumR, dataTypes, Nmorefiles = Nfiles - length(humdrumR),
                                max.records.file, max.token.length)
  }  else {
    print_humtab_notActiveVector(humdrumR, firstAndLast)
  }
  invisible(NULL)
  
}


padColumns <- function(lines, max.token.length) {
 # This function takes a bunch of lines, separated by tabs,
 # and replaces the tabs with appropriate numbers of spaces
 # such that the lines will print as nicely aligned columns.
 # It also trims strings that are too long, replacing the last
 # three characters before the cuttoff with "..."
          
 local <- !grepl('[0-9]:  \t!!', lines)          
 
 lines[!local] <- paste0('  ', gsub('\t', '', lines[!local]))
 tokmat <- stringi::stri_list2matrix(strsplit(lines[local], split = '\t'), byrow = TRUE)
 
 # trim long tokens
 toklen  <- nchar(tokmat)
 toklen[is.na(toklen)] <- 0L
 toolong <- toklen > max.token.length
 tokmat[toolong] <- stringi::stri_sub(tokmat[toolong], from = 0L, max.token.length)
 tokmat[toolong] <- stringi::stri_replace_last_regex(tokmat[toolong], pattern = '...', replacement = '...')
 tokmat[is.na(tokmat)] <- ''
 
 # pad columns
 colMaxs <- apply(toklen, 2, max, na.rm = TRUE)
 lines[local] <- apply(tokmat, 1, paste %.% padder, sizes = colMaxs + 2L, collapse = '')
 
 lines <- gsub('\t*$', '', lines)
 
 lines

}

print_humtab_isActiveVector <- function(humdrumR, dataTypes = 'GLIMDd', Nmorefiles = 0L,
                                        max.records.file = 40L, max.token.length = 12L) {
  lines <- as.lines(humdrumR, dataTypes = dataTypes,
                    padPaths = TRUE, alignColumns = TRUE)
  
  NRecord <- num2str(as.numeric(gsub('^.*\\.', '', names(lines))), pad = TRUE)
  NFile   <- gsub('\\..*$', '', names(lines))
  FileNames <- getHumtab(humdrumR)[ , unique(File)]
  
  lines <- paste0(NRecord, ':  \t', lines)
  
  ## Trim an space lines
  lines <- padColumns(lines, max.token.length = max.token.length)
  ellipsis <- pander::repChar('#', max(nchar(lines[!grepl(':   *!!', lines)])))
  
  
  ##
  lines   <- split(lines, f = NFile)
  NRecord <- split(NRecord, f = NFile)
  
  lines <- Map(f = function(l, rn, last) {
            if (length(l) <= max.records.file) return(l)
            
            # lines
            l <- do.call(if(last) tail else head, list(l, n = max.records.file))
            
            # record numbers
            rn <- stringi::stri_trim_left(rn)
            restRN <- do.call(if(last) head else tail, list(rn, n = -max.records.file))
            if (length(restRN) > 1L) restRN <- paste0(restRN[1], '-',restRN[length(restRN)])
            restRN <- paste0("[", restRN, ']')
            
            elips  <- pander::repChar('.', max(nchar(l)))
            elips  <- paste0(restRN, stringi::stri_sub(elips, from = nchar(restRN) + 1L)) 
            
            append(l, elips, after = if (last) 0L else length(l))
            

            
            },
               lines, NRecord, seq_along(lines) == length(lines))
  
  
  
  ellipses <- paste0(ellipsis, ' ', FileNames)
  lines    <- Map(append, lines, ellipses, after = 0L)
  
  ##
  if (Nmorefiles > 0L) {
   
   message <- c(ellipsis,
                '',
                paste0('\t\t', glue::glue("({num2str(Nmorefiles)} more files...)")),
                '')
   lines <- append(lines, message, after = length(lines) - 1L)
  }
  
  cat(unlist(lines), sep = '\n')
  
}


print_humtab_notActiveVector <- function(humdrumR, cutMiddle = FALSE) {
  act <- evalActive(humdrumR)
  D <- getD(humdrumR)
  
  lays <- fields(humdrumR)
  refs <- lays$Name[lays$Type == 'Reference']
  
  notact <- D[ , !colnames(D) %in% activeFields(humdrumR), with = FALSE ]
  notact <- notact[ , colnames(notact) %in% c(refs, 'File', 'Spine', 
                                              'Record', 'Path', 
                                              'Exclusive', 'BarN', 'DoubleBarN'), with = FALSE]
  
  notact <- notact[ , sapply(notact, function(col) !all(duplicated(col)[-1]) & !any(is.na(col))), with = FALSE]
  printnotact <- nrow(notact) > 0 && nrow(notact) == length(act)
  if (printnotact) {
    ellipsis <- '####'
    collabs <- lapply(colnames(notact), function(col) paste0(gsub('File: ', '', paste0(col, ": ")), notact[[col]]))
    collabs <- apply(do.call('cbind',collabs), 1, 
                     function(row) paste0('               ', ellipsis, paste(row, collapse = ', '), ellipsis))
  }
  for (i in 1:nrow(humdrumR)) {
    if (printnotact) cat(collabs[i], '\n', sep = '')
    print(act[[i]])
    cat('\n')
  }
}

#' @export
setMethod('show', signature = c(object = 'humdrumR'),
          function(object) {
            len  <- length(object)
            trim <- if (len == 1L) 400L else 40L
            
            print_humtab(object, firstAndLast = TRUE, 
                         max.records.file = trim)
            
            if (len > 1L) {
              cat('\n')
              cat('\thumdrumR corpus of ', 
                  ifelse(len <= 100L, num2word(len), num2str(len)), 
                  ' files.\n', sep = '') 
            }
            
            ## Fields
            showFields(object, 'Data')
        
          })

