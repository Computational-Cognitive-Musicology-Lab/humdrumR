##Methods for humdrumR class ####



###
####################################################humdrumR Class definition ####
##

#' Humdrum data table
#' 
#' In \code{humdrumR}, humdrum data is stored
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
#' to a row in the humdrum table, while a "field" refers to a column.
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
#'    \code{"G"} = global comment.}
#' \item{Null}{\code{logical} - Is this a null record (i.e., is the token ".", "*", "!", "!!", "!!!", or "=")?}
#' \item{Spine}{\code{integer} - The spine. This field is \code{NA} when \code{Global == TRUE}.}
#' \item{Path}{\code{integer} - The "spine path." Any time a \code{*^} spine path split occurs in
#'       the humdrum data, the right side of the split becomes a new "path." The original path
#'       is numbered \code{0}---if there are no spine path splits, the \code{Path} field is all zeros. 
#'       This field is always \code{NA} when \code{Global == TRUE}.}
#' \item{Column}{\code{integer} - Which column (tab-separated) in the humdrum data---irespective of Spine/Paths.}
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
#' 
#' ...
#' @name humtable
NULL

#' HumdrumR class
#' 
#' @slot Humtable A list of \code{\link[humtable]{humdrum tables}}, each having the same fields
#' but containing data from different types of records (e.g., interpretations, data, barlines, comments).
#' @slot Files A list of two elements. The first, "Search", contains a single character representing
#' the \code{pattern} used in the call to \code{\link{readHumdrum}} which created this \code{humdrumR} object.
#' The second, "Names", is a vector of strings representing all the files which matched the \code{pattern}
#' and were read into the \code{humdrumR} object.
#' @slot Fields A list containing strings corresponding to the existing fields in the \code{humdrumR} object.
#' The fields are divided into five categories: "Data", "Structure", "Interpretation", "Formal", and "Reference."
#' See (\code{\link{fields}})
#' @slot Active A formula. The right side of this formula is an expression which 
#' extracts data from field(s) in the \code{humdrumR} data. The active expression
#' is the "default" data which is printed be \code{\link{show}} calls, and when \code{.}
#' is used in a call to \code{\link{humApply}}.
#' and others.
#' @slot LoadTime A POSIXct value, indicating the time at which \code{\link{readHumdrum}} was
#' called to create this \code{humdrumR} object.
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'list',
                   Files = 'list',
                   Fields = 'list',
                   Active = 'formula',
                   LoadTime = 'POSIXct'
                   )) -> makeHumdrumR

 #' @export
setMethod('initialize', 'humdrumR',
          function(.Object, dtab, pattern) {
            # dtab = takes a data.table of humdrum data as input
            # pattern = the original file search glob (string)

            fields <- colnames(dtab)
            fieldcategories <- list(Data = 'Token',
                                    Structure = c('File', 'FullFileName', 'NFile',
                                                  'Column', 'Spine', 'Path', 'Stop',
                                                  'Record', 'NData', 'Global', 'Null', 'Type'),
                                    Interpretation   = c('Exclusive', 'Tandem',
                                                         fields[fields %in% knownInterpretations[knownInterpretations$Type == 'Tandem', ]$Name]),
                                    Formal    = c(grep('^Formal', fields, value = TRUE),
                                                  'BarN', 'DoubleBarN', 'BarLabel'))
            fieldcategories$Reference <- fields[!fields %in% unlist(fieldcategories)]
         
            
            .Object@Humtable  <- splitHumtab(dtab)        
            .Object@Fields    <- fieldcategories
            .Object@Active    <- ~Token
            .Object@Files     <- list(Search = pattern, Names = unique(dtab$FullFileName))
            .Object@LoadTime  <- Sys.time()
            .Object
          })

#' @export
is.humdrumR <- function(x) inherits(x, 'humdrumR')



############Important Methods ####

#getting humdrum data out of humdrumR


#' @export
splitHumtab <- function(humtab, drop = FALSE) { split(humtab, f = factor(humtab$Type, levels = c('G', 'L', 'I', 'M', 'D', 'd')), drop = drop) }

#' @export
spliceHumtab <- function(humtab) {
  # This combines the components of a humtab list into a single data.table
  humtab <- rbindlist(humtab, fill = TRUE)
  if (all(sapply(humtab[ ,c('NFile', 'Spine', 'Path', 'Record', 'Stop')], class) != 'list')) {
    setorder(humtab, NFile, Spine, Path, Record, Stop)
  }
  
  humtab
}

getD <- function(humdrumR) { humdrumR@Humtable[['D']] }

`setD<-` <- function(humdrumR, value) { 
  humdrumR@Humtable[['D']] <- value
  humdrumR
}

#' @export
getHumtab <- function(humdrumR, types = c('G', 'L', 'I', 'M', 'D', 'd')) {
  # takes a humdrumR object, extracts D and and GLIM to the environment
  types <- unique(unlist(strsplit(types, split = '')))
  humtab <- humdrumR@Humtable[types]
  # if (!allsame(sapply(humtab, ncol))) {
   # humtab <- indexGLIM(humdrumR)@Humtable[types]
  # }
  spliceHumtab(humtab)
}

#' @export
`putHumtab<-` = function(humdrumR, value, drop = FALSE) {
  # adds humtab into humdrumR
  if (is.data.table(value)) value <- splitHumtab(value, drop = drop)
  humdrumR@Humtable[names(value)] <- value
  
  humdrumR
}


            




# A humdrumR object is treated differently depending on whether its
# active columns contain atomic data ("humdrumable") or not (tables, lists, matrices, etc.).
# this function tests if the active column is humdrumable or not


#' @export
getActive <- function(humdrumR, types = 'D', atomize = FALSE, nullAsDot = FALSE)  {
  humtab <- getHumtab(humdrumR, types = types)
  act <- lazyeval::f_eval(humdrumR@Active, data = humtab)
  
  
  if (nullAsDot) {
   if (is.atomic(act)) {
     act[is.na(act)] <- '.' 
   } else {
     act <- lapply(act, function(a) `[<-`(a, is.na(a), '.'))
   }
  }
  
  if (atomize && is.list(act)) {
    act <- as.matrix(as.data.frame(act))
  }
  if (atomize && is.matrix(act)) {
   out <- character(nrow(humtab))
   out[humtab$Type != 'D'] <- apply(act[humtab$Type != 'D', ], 1, function(row) paste(unique(row), collapse = ' <> '))
   out[humtab$Type == 'D'] <- apply(act[humtab$Type == 'D', ], 1, function(row) paste(       row , collapse = ' <> '))
   
   act <- out
  }
  
  act
}

#' @export
setActive <- function(humdrumR, expr) {
  form <- lazyeval::f_capture(expr)
  putActive(humdrumR, form)
}

putActive <- function(humdrumR, form) {
  humtab <- getD(humdrumR)
  usedInExpr <- fieldsInFormula(humtab, form)
  if (length(usedInExpr) == 0) stop("The 'active'-field formula for a humdrumR object must refer to some field.\n
Add a reference to some field, for instance Token.", call. = FALSE)
  
  humdrumR@Active <- form
  
  act <- getActive(humdrumR)
  if ((is.atomic(act) && length(act) == nrow(humtab))
      || (is.list(act) && length(act) == nrow(humtab))
      || (is.list(act) && all(lengths_(act) == nrow(humtab)))) {
   return(humdrumR) 
  } else {
    stop("The 'active-field formula for a humdrumR object cannot be a different size from the raw fields.", call. = FALSE)
  }
  
}

#' @export
activeString <- function(humdrumR) fieldsInFormula(getD(humdrumR), humdrumR@Active)

setActiveString <- function(humdrumR, str) {
  scall <- if (length(str) > 1) do.call('call', c(list('list'), str)) else as.symbol(str)
  form <- f_new(rhs = scall)
  putActive(humdrumR, form)
}


# `active<-` <- function(humdrumR, value, types = 'D') {
#   acts <- activeString(humdrumR)
#   
#   humtab <- getHumtab(humdrumR, types = types)
#   
#   if (is.list(value) && length(value) == length(acts)) {
#     for (i in seq_along(acts)) {
#       humtab[[acts[i]]] <- value[[i]]
#     }
#   } 
#   if (!is.list(value) && length(value) == nrow(humtab)) {
#     humtab[[acts[1]]] <- value
#   }
#  
#  putHumtab(humdrumR) <- humtab
#  humdrumR
# }

#' @export
humdrumAble <- function(humdrumR) {
  act <- getActive(humdrumR)
  !is.list(act)
}

#' @export
anyPaths <- function(humdrumR) {
 humtab <- getHumtab(humdrumR)
 
 any(humtab$Path > 0, na.rm = TRUE)
 
}

####Fields ----

#' This controls which humdrumR data are printed and default target for pipe.
#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
            name <- as.character(name)
            fields <- fields(x)$Name
            target <- pmatch(name, fields)
            if (is.na(target)) stop(glue::glue("No field called '{name}'"), call. = FALSE)
            
            setActiveString(x, fields[target])
          })



#' List fields in a \code{\linkS4class{humdrumR} object
#' @export
fields <- function(humdrumR, types = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')) { 
  #
  D <- getD(humdrumR)
  
  fields <- unlist(humdrumR@Fields[types])
  
  D <- D[ , fields, with = FALSE]
  classes <- sapply(D, class)
  
  if (any(lists <- classes == 'list')) {
    classes[lists] <- paste0('list (of ',
                             sapply(D[ , lists, with = FALSE],
                                                function(col) {
                                                  glue::collapse(paste0(unique(sapply(col, class)), "s"), sep = ', ', last = ', and ')
                                                }),
                             ")")
  }
 
  output <- data.table(Name = fields, Class = classes, Type = gsub('[0-9]*$', '', names(fields)))
  output <- output[Type %in% types]
  
  output
}


showFields <-  function(humdrumR, types = c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')) {
          fields <- fields(humdrumR, types)

          activefield <- fields$Name %in% activeString(humdrumR)
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




isField <- function(humdrumR, names) names %in% fields(humdrumR)$Name 

nulltypes <- c(G='!!', I = '*', L= '!', d = '.', D = NA, M = '=')

`addFields<-` <- function(object, value) {
 object@Fields$Data <- unique(c(object@Fields$Data, value))
 object
}

`removeFields<-` <- function(object, value) {
  object@Fields$Data <- object@Fields$Data[!object@Fields$Data %in% value]
  object
}

`putFields<-` <- function(object, copyField = NULL, value)  {
  humtab <- object
  for (name in value) {
    newfield <- if (is.null(copyField)) nulltypes[humtab$Type] else humtab[[copyField]]
    humtab[[name]] <- newfield
  }
  
  humtab
}

#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', value = 'vector'),
          function(x, i, value) {
            
            if (length(value) == nrow(x)) {
              x@Humtable$D[i] <- value
            } else {
              stop(glue::glue("Can't assign this value to '{name}' field, because it is the wrong length."))
            }
            
            x <- setActiveString(x, i)
            return(x)
          })

#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'vector'), function(x, name, value) { x[name] <- value ; x  })

#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', value = 'humdrumR'),
          function(x, i, value) {
            humtab <- getD(value)
            
            removeFields(value) <- grep('Pipe', colnames(humtab), value = TRUE)
            pipes <- pipeFields(humtab)
            
            if (len0(pipes)) pipes <- activeString(value)
            
            pipes <- tail(pipes, n = length(i))
            
            colnames(humtab)[colnames(humtab) %in% pipes] <- i
            
            if (any(grepl('Pipe', colnames(humtab)))) humtab[ , eval(grep('Pipe', colnames(humtab), value = TRUE)) := NULL]
            
            putHumtab(value, drop = TRUE) <- humtab
            addFields(value) <- i
            
            value@Active <- substituteName(value@Active, setNames(list(as.symbol(i)), pipes))
            
            value
          })
#' @export
setMethod('$<-',  signature = c(x = 'humdrumR', value = 'humdrumR'), function(x, name, value) { x[name] <- value ; x  })

############################################ Indexing humdrumR #######



# [] ####


#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric'),
          function(x, i) {
            # Gets files with numeric index of unique file numbers
            D <- getD(x)
            i <- sort(unique(D$NFile))[i]
              
            setD(x) <- D[NFile %in% i]
            x
          })

#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'character'),
          function(x, i) {
            # Gets files which contain matches to i
            D <- getD(x)
            
            matches = grepls(i, getActive(x))
            
            i <- unique(D$NFile[matches])
            
            setD(x) <- D[NFile %in% i]
            x
          })

#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'formula'),
          function(x, i) {
            # Gets files which match formula
            form <- lazyeval::f_rhs(i)
            
            D <- getD(x)
            # i <- unique(D[eval(form), NFile])
            i <- unique(D[ , eval(form), by = NFile][get('V1')]$NFile)
            
            setD(x) <- D[NFile %in% i]
            x
          })

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'logical', j = 'missing'), 
          function(x, i) {
            #gets rows which are TRUE
            D <- get(D)
            
            setD(x) <- D[i]
            x
          })

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'formula'), 
          function(x, i) {
            #evaluate formula
            formu <- i[[-1]]
            D       <- getD(x)
            setD(x) <- D[eval(formu)]
            x
          })



##################################################[[numeric]] ####

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i) {
            #gets data record numbers
            D <- getD(x)
            
            neg <- all(sign(i) == -1) 
            i <- abs(i)
            
            setD(x) <- if (neg)  D[!NData %in% i] else D[NData %in% i]
            x
          })


#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j) {
            #gets spines
            D <- getD(x)
            
            neg <- all(sign(j) == -1)
            j <- abs(j)
            
            setD(x) <- if (neg)  D[!Spine %in% j] else D[Spine %in% j]
            x
          })

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'numeric', j = 'numeric'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
          })

##################################################[[character]] ####

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'missing'), 
          function(x, i) {
            # gets any record which contains match
            grepingind(x, i,  function(sd) { 
              recn <- unique(sd$Record[sd$hits])
              sd[Record %in% recn]
            })
          })


#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'character'), 
          function(x, j) {
            #gets any spine which contains match
            grepingind(x, j,  function(sd) { 
              recn <- unique(sd$Spine[sd$hits])
              sd[Spine %in% recn]
            })
          })

grepingind <- function(humdrumR, ind, func) {
  Dd <- getHumtab(humdrumR, types = c('D', 'd'))
  Dd$hits <- grepl(pattern = ind, getActive(humdrumR, types = c('D', 'd')))
  
  Dd <- Dd[ , func(.SD), by = NFile]
  Dd[ , 'hits' :=  NULL]
  putHumtab(humdrumR, drop = TRUE) <- Dd
  humdrumR
}


#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'character', j = 'character'), 
          function(x, i, j) {
            x <- x[[ , j]]
            x <- x[[i, ]]
            x
          })

#####
####GLIMDd indexing ####


            
index_withinPiece <- function(D, expr) {
              func <- function(sd) sd[eval(expr)]
              D[ , func(.SD), by = NFile]
              
}
            
            

indexGLIM <- function(humdrumR, targets = c('G', 'L', 'I', 'M', 'd')) {
  #####ReIndex GLIM tables to match indexing of D tables
  GLIMd <- humdrumR@Humtable[targets]
  D     <- getD(humdrumR)
  # first add missing fields (columns)
  GLIMd <- lapply(GLIMd, 
                  function(tab) {
                    missingfields <- colnames(D)[!colnames(D) %in% colnames(tab)]
      
                    if (len0(missingfields)) return(tab)
                    
                    putFields(tab, 'Token') <- missingfields
                    
                    tab
                  })
  
  GLIMd <- lapply(GLIMd, function(tab) tab[ , colnames(D), with = FALSE]) # match order of columns
  GLIMd <- rbindlist(GLIMd)
  
  # next  remove missing pieces
  GLIMd <- GLIMd[NFile %in% unique(unlist(D$NFile))]
  
  # then, do indexing by piece:
  GLIMd <- rbindlist(list(D, GLIMd), fill = TRUE)[ , indexGLIMd_piece(.SD), by = NFile, .SDcols = colnames(D)]
  
  # resplit and put back in to humdrumR object
  putHumtab(humdrumR, drop = FALSE) <- GLIMd
  humdrumR
}


indexGLIMd_piece <- function(humtab) {
  ###called by reHum_index on each individual piece
  D     <- humtab[Type == 'D']
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

#### Data transformation and printing ----

# Standard as.x ----

#' @export
setMethod('as.data.frame', 
          signature = c(x = 'humdrumR'),
          function(x) lapply(as.matrix(x), as.data.frame))

#' @export
setMethod('as.matrix',
          signature = c(x = 'humdrumR'),
          function(x, d.only = FALSE) { 
            if (length(x) == 0 || nrow(x) == 0) return(list(matrix(data = character(0), ncol = 0, nrow = 0)))
            folded <- foldRecords(x, global = FALSE)
            
            mat <- folded[ , stringi::stri_list2matrix(Active, byrow = TRUE, fill = '.')]
            
            if (d.only) {
             mat <- mat[!grepl('^[!=*]', mat[ , 1]), ]
            }
            
            mat
          })

#' @export
as.matrices <- function(humdrumR, d.only = FALSE) {
  if (length(humdrumR) == 0 || nrow(humdrumR) == 0) return(list(matrix(data = character(0), ncol = 0, nrow = 0)))
  folded <- foldRecords(humdrumR, global = FALSE)
  
  matrices <- folded[ , list(list(stringi::stri_list2matrix(Active, byrow = TRUE, fill = '.'))), by = NFile]$V1
  
  if (d.only) {
    matrices <- lapply(matrices, function(mat) mat[!grepl('^[!=*]', mat[ , 1]), ])
  }
  
  
  names(matrices) <- unique(folded$File)
  
  matrices
}


#### Special as.xxxx ####

#' @export 
foldStops <- function(humdrumR, tokenize = FALSE) {
 humtab <- getD(humdrumR)
 
 if (!any(humtab$Stop > 1 & !is.na(humtab$Stop))) return(humdrumR)
 
 actives <- activeString(humdrumR)
 actives <- actives[!actives %in% c('Spine', 'Record', 'NData', 'BarN', 'DoubleBarN', 'BarLabel',
                                    'NFile', 'Type')]
 
 if (tokenize) humtab[[actives]] <- lapply(humtab[[actives]], as.character)
 
 pasteexprs <- lapply(actives, 
                      function(a) {
                        a <- as.symbol(a)
                        if (tokenize) call('paste', a, collapse = ' ') else call('list',  a)
                        })
 pasteexpr <- do.call('call', quote = TRUE,
                      c('list', pasteexprs))
 
 expr <- ~humtab[ , X := Y, by = .(File, Spine, Record)]
 
 activecall <- call('c', paste0(actives, '_new_foldstopxxx'))

 expr <- substituteName(expr, subs = list(X = activecall, Y = pasteexpr))
 
 suppressWarnings(lazyeval::f_eval(expr)) # inplace)
 
 humtab <- humtab[Stop == 1]
 humtab[ , eval(actives) := NULL]
 colnames(humtab) <- gsub('_new_foldstopxxx$', '', colnames(humtab))
 
 putHumtab(humdrumR, drop = TRUE) <- humtab
 humdrumR
}

#' @export
foldRecords <- function(humdrumR, global = FALSE) {
  # takes a humdrumR and, if it's humdrumAble, collapses the humdrum table active column by record, creating vectors for each record.
  
  if (!humdrumAble(humdrumR)) return(NULL) # doesn't work on unhumable active columns
  
  humdrumR <- foldStops(humdrumR, tokenize = TRUE)
  humdrumR <- indexGLIM(humdrumR)
  humtab <- getHumtab(humdrumR)
  
  active <- getActive(humdrumR, types = c('G','L','I','M','D','d'), atomize = TRUE, nullAsDot = TRUE)
  activecolname <- paste(activeString(humdrumR), collapse = '.')
  humtab$Temp <- active
  
  
  if (!global) humtab <- humtab[!humtab$Global]
  if (nrow(humtab) == 0) return(list()) 
  
  ### ID which "column" each token belongs to
  # these need to be recomputed (using match), because some columns may have been removed
  
  humtab[ , {
          cols <- unique(Column)
          cols <- cols[!is.na(cols)]
          match(Column, cols, nomatch = NA_integer_)
          },
          by = NFile]$V1 -> humtab$Column
  
  humtab[ , MaxColumn := max(Column, na.rm = TRUE), by = .(NFile)]
  humtab$ColumnLabels <- humtab[ , list(rep(list(unique(paste0(Spine[!is.na(Spine)], '.', Path[!is.na(Spine)]))), nrow(.SD))), by = .(NFile)]$V1

  # Temp becomes Active because data.table := can't change the string ttype to a list type
  humtab[ ,  Active := list(parseRecord(.SD)), by = .(NFile, Record) ] 
  humtab <- humtab[humtab[j = !duplicated(paste(NFile, Record))]]
  
  humtab[ , 'Temp' := NULL]
  humtab[ , 'ColumnLabels' := NULL]
  humtab[ , 'MaxColumn' := NULL]
  # for (act in c('Temp', activeString(humdrumR))) humtab[ , eval(act) := NULL]
  setorder(humtab, NFile, Record)
  
  humtab
}

parseRecord <- function(record) {
  columnlabels <- record$ColumnLabels[[1]]
  ## This is used by foldRecords
  if (is.na(record$Spine[1])) { # if it's a global record!
    list(setNames(record$Temp, '0.0'))

  } else {
            
    tokens <- rep('', record$MaxColumn[1])
    tokens[record$Column] <- tapply(record$Temp, record$Column, paste, collapse = ' ') 
    names(tokens) <- columnlabels
    list(tokens)
  }
}



#' @export
collapseRecords <- function(humdrumR, pad = FALSE, trim = NA, global = TRUE) {
  # takes a humdrumR object, and collapses each record to a string, returning a new data.table
  # this relies heavily on foldRecords
  
  humtab_rv <- foldRecords(humdrumR, global = global)
  
  #trim
  if (!is.na(trim)) {
            humtab_rv[ , Active := Map(Vectorize(trimLongString, vectorize.args = 'strs'),
                                       Active,
                                       ifelse(global, max(80L, trim), trim))]
  }
  
  #pad
  if (pad) humtab_rv[ , Active := list(padRecords(Active, Global)), by = .(NFile)]
  
  #collapse
  sep <- if (pad) '' else '\t'
  humtab_rv[['Active']]  <- humtab_rv[ , sapply(Active, paste, collapse = sep)]
  
  humtab_rv
}


padRecords <- function(recvecs, global) {
  # this is a tool used by collapseRecords
  strlengths <- lapply(recvecs, stringr::str_length)
  
  mat <- do.call('rbind', strlengths[!global])
  colsizes <- apply(mat, 2, max, na.rm = TRUE) + 3
  
  recvecs[!global] <- lapply(recvecs[!global], 
                             function(recvec) { stri_pad_right(recvec, colsizes) })
  
  recvecs
}

####standard methods ----

#' @export
setMethod('length',
          signature = c(x = 'humdrumR'),
          function(x) { length(unique(getD(x)$NFile))})

#' @export
nrecords <- function(humdrumR, data.only = TRUE) {
  if (data.only) {
    humtab <- getD(humdrumR)
  } else {
    humtab <- getHumtab(humdrumR)
  }
  
  n <- humtab[ , .(NR = length(unique(Record))), by = File] 
  
  sum(n$NR)
}

#' @export
setMethod('nrow', 
          signature = c(x = 'humdrumR'), 
          function(x) nrow(getD(x)))

####Print methods ----

#' @export
print_humtab <- function(humdrumR, cutMiddle = FALSE, global = FALSE) {
  if (len0(humdrumR)) {
    cat("\nEmpty humdrumR object\n")
    return(invisible(NULL))
  }
  if (cutMiddle) humdrumR <- humdrumR[c(1, length(humdrumR))]
  
  if (humdrumAble(humdrumR)) {
    print_humtab_humdrumAble(humdrumR, cutMiddle, global)
  }  else {
    print_humtab_nothumdrumAble(humdrumR, cutMiddle)
  }
  invisible(NULL)
  
}

local_path <- function(path) gsub('^.*/', '', path)

print_humtab_humdrumAble <- function(humdrumR, cutMiddle = FALSE, global = FALSE) {
  humtab_rs <- collapseRecords(humdrumR, pad = TRUE, trim = 16L, global = global)
  recordn <- num2str(humtab_rs$Record, pad = TRUE)
  records <- humtab_rs[['Active']]
  records <- paste(recordn, ':', 
                   ifelse(grepl('^!!', records, useBytes = TRUE), ' ', '    '),
                   records, sep = '')
  
  ellipsis <- paste(rep('#', max(nchar(records))), collapse = '')
  FNs <- humtab_rs$File
  # if (filenames) FNs <- (' ' %str*% (max(nchar(recordn)) + 1)) %str+% (humtab_rs$FileName %str-% '^.*/') else FNs <- c()
  records <- split(records, FNs)
  
  if (cutMiddle && any(table(FNs) > 40) ) {
    
    
    lapply(head(records, -1),
           function(recs) {
             if (recs %len>% 39L) head(recs, 20) else recs
           }) -> records[1:(length(records) - 1)]
    
    if (records[[length(records)]] %len>% 39L) records[[length(records)]] <- tail(records[[length(records)]], 20)
    
  }
    
  records <- unlist(Map(append, records, paste0(ellipsis, ' ', names(records)), after = 0L))
  
  records <- paste0(records, "\n")
  
  
  cat(records, sep = '')
  
}


print_humtab_nothumdrumAble <- function(humdrumR, cutMiddle = FALSE) {
  act <- getActive(humdrumR)
  D <- getD(humdrumR)
  
  lays <- fields(humdrumR)
  refs <- lays$Name[lays$Type == 'Reference']
  
  notact <- D[ , !colnames(D) %in% activeString(humdrumR), with = FALSE ]
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
            len    <- length(object)
            
            
            if (len == 1L) {
              print_humtab(object, cutMiddle = FALSE, global = TRUE)
            } else {
              print_humtab(object, cutMiddle = len > 1L, global = FALSE)
              cat('\n')
              cat('\tCorpus of ', ifelse(len <= 100, num2word(len), num2str(len)), ' humdrum files.\n', sep = '') 
            }
            
            ## Fields
            showFields(object, 'Data')
        
          })





#### String manip ----
### NEED TO: add methods for > < >= <= == != ^



#' @export
setMethod('-', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            # browser()
            # newtokens <- getActive(humdrumR)[[1]] %str-% e2
            # newtokens[newtokens == ''] <- '.'
            e1 | RErid(e2)
            # active(humdrumR) <- newtokens
            
          })

#' @export
setMethod('-', signature = c(e1 = 'humdrumR', e2 = 'numeric'),
          function(e1, e2) {
            
            
            if (all(sapply(getActive(e1), is.numeric))) {
              e1 | subtract(e2)
            } else {
              whichnot <- activeString(e1)[!sapply(getActive(e1), is.numeric)]
              plural <- if (len1(whichnot)) " is" else "s are"
              stop(glue::glue("'{whichnot}' field{plural} not numeric."),  call. = FALSE)
            }
          
            humdrumR
          })
              


#' @export
setMethod('%%', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            e1 | REkeep(e2)
            # humdrumR <- e1
            # 
            # newtokens <- getActive(humdrumR)[[1]] %strkeep% e2
            # newtokens[newtokens == ''] <- '.'
            # active(humdrumR) <- newtokens
            # 
            # humdrumR
          })

## +

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            humdrumR <- e1
            
            newtokens <- getActive(humdrumR)[[1]] %str+% e2
            
            active(humdrumR) <- newtokens
          
            humdrumR
          })


#' @export
setMethod('+', signature = c(e1 = 'character', e2 = 'humdrumR'),
          function(e1, e2) {
            humdrumR <- e2
            
            newtokens <- paste0(e1, getActive(humdrumR)[[1]])
            active(humdrumR) <- newtokens
          
            humdrumR
          })

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'numeric'),
          function(e1, e2) {
            humdrumR <- e1
            
            oldtokens <- getActive(humdrumR)[[1]]
            if (is.numeric(oldtokens)) {
              newtokens <- oldtokens + e2
            } else {
              stop(glue::glue("'{activeString(humdrumR)[[1]]}' field is not numeric."),  call. = FALSE)
            }
            active(humdrumR) <- newtokens
          
            humdrumR
          })

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'function'),
          function(e1, e2) {
            humdrumR <- e1
            
            oldtokens <- getActive(humdrumR)[[1]]
            additions <- e2(oldtokens)
            
            if (is.numeric(oldtokens)) {
              newtokens <- oldtokens + additions
            } else {
              newtokens <- oldtokens %str+% additions
            }
            
            active(humdrumR) <- newtokens
          
            humdrumR
          })

#' @export
setMethod('+', signature = c(e1 = 'function', e2 = 'humdrumR'),
          function(e1, e2) {
            humdrumR <- e2
            
            D <- getD(humdrumR)
            oldtokens <- getActive(humdrumR)[[1]]
            additions <- e1(oldtokens)
            
            if (is.numeric(oldtokens)) {
              newtokens <- additions     +  oldtokens
            } else {
              newtokens <- additions %str+% oldtokens
            }
            
            active(humdrumR) <- newtokens
          
            humdrumR
          })

## >



# #' @export
# setMethod('>', signature = c(e1 = 'humdrumR', e2 = 'numeric'),
#           function(e1, e2) {
#             humdrumR <- e1
#             
#             D <- humdrumR@D
#             oldtokens <- D[[humdrumR@Active]]
#             if (is.numeric(oldtokens)) {
#               newtokens <- oldtokens > e2
#             } else {
#               newtokens <- oldtokens %str>% e2
#             }
#             
#             D[[humdrumR@Active]] <- newtokens
#             
#             humdrumR@D <- D
#             
#             humdrumR
#           })

# #' @export
# setMethod('>', signature = c(e1 = 'humdrumR', e2 = 'function'),
#           function(e1, e2) {
#             humdrumR <- e1
#             
#             D <- humdrumR@D
#             oldtokens <- humdrumR@D[[humdrumR@Active]]
#             additions <- e2(oldtokens)
#             
#             if (is.numeric(oldtokens)) {
#               newtokens <- D[[humdrumR@Active]] > additions
#             } else {
#               newtokens <- D[[humdrumR@Active]] %str>% additions
#             }
#             
#             newtokens <- D[[humdrumR@Active]] %str+% additions
#             
#             D[[humdrumR@Active]] <- newtokens
#             
#             humdrumR@D <- D
#             
#             humdrumR
#           })

# #' @export
# setMethod('>', signature = c(e1 = 'function', e2 = 'humdrumR'),
#           function(e1, e2) {
#             humdrumR <- e2
#             
#             D <- humdrumR@D
#             oldtokens <- humdrumR@D[[humdrumR@Active]]
#             additions <- e1(oldtokens)
#             
#             if (is.numeric(oldtokens)) {
#               newtokens <- additions     >  D[[humdrumR@Active]] 
#             } else {
#               newtokens <- additions %str>% D[[humdrumR@Active]] 
#             }
#             
#             
#             D[[humdrumR@Active]] <- newtokens
#             
#             humdrumR@D <- D
#             
#             humdrumR
#           })
# 


####INTERPRETATION
