
##Methods for Humdrum class ####



###
####################################################humdrumR Class definition ####
###
structlayernames <- c('File', 'FullFileName', 'NFile', 'Global',
                      'Record', 'Type', 'Spine', 'Path', 'Stop',
                      'Exclusive', 'Tandem', 'Column', 
                      'BarN', 'DoubleBarN', 'BarLabel', 'Section', 
                      'NData', 'Null')


#' HumdrumR class
#' @export
setClass('humdrumR', 
         slots = c(Humtable = 'list',
                    # D = 'data.table',
                   # GLIM = 'data.table',
                   # Other = 'list',
                   Files = 'list',
                   Layers = 'list',
                   Active = 'formula',
                   Partition = 'list',
                   LoadTime = 'POSIXct'
                   )) -> makeHumdrumR

 #' @export
setMethod('initialize', 'humdrumR',
          function(.Object, dtab, pattern) {
            # dtab = takes a data.table of humdrum data as input
            # pattern = the original file search glob (string)

            layers <- colnames(dtab)
            
            .Object@Humtable  <- splitHumtab(dtab)        
            .Object@Layers    <- list(User = 'Token',
                                      Structure = structlayernames,
                                      Tandem    = layers[layers %in% knownTandemInterpretations$Name],
                                      Reference = layers[!layers %in% c('Token', structlayernames, knownTandemInterpretations$Name)])
            .Object@Active    <- ~Token
            .Object@Files     <- list(Search = pattern, Names = unique(dtab$FullFileName))
            .Object@LoadTime  <- Sys.time()
            .Object@Partition <- list()
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
  usedInExpr <- layersInFormula(humtab, form)
  
  if (len0(usedInExpr)) stop("The 'active'-layer formula for a humdrumR object must refer to some layer.\n
Add a reference to some layer, for instance Token.", call. = FALSE)
  
  humdrumR@Active <- form
  
  act <- getActive(humdrumR)
  if ((is.atomic(act) && length(act) == nrow(humtab))
      || (is.list(act) && length(act) == nrow(humtab))
      || (is.list(act) && all(lengths_(act) == nrow(humtab)))) {
   return(humdrumR) 
  } else {
    stop("The 'active-layer formula for a humdrumR object cannot be a different size from the raw layers.", call. = FALSE)
  }
  
}

#' @export
activeString <- function(humdrumR) layersInFormula(getD(humdrumR), humdrumR@Active)

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

####Layers ----

#' This controls which humdrumR data are printed and default target for pipe.
#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
            name <- as.character(name)
            layers <- layers(x, struct = TRUE)$Name
            target <- pmatch(name, layers)
            if (is.na(target)) stop(glue::glue("No layer called '{name}'"), call. = FALSE)
            
            setActiveString(x, layers[target])
          })



#' @export
layers <- function(humdrumR, tandem = TRUE, reference = TRUE, struct = TRUE) { 
  
  #
  D <- getD(humdrumR)
  
  targets <- c('User',  
               if (tandem) 'Tandem' else NULL,
               if (reference) 'Reference' else NULL, 
               if (struct) 'Structure' else NULL)
  layers <- unlist(humdrumR@Layers[targets])
  
  D <- D[ , layers, with = FALSE]
  classes <- sapply(D, class)
  
  if (any(lists <- classes == 'list')) {
    classes[lists] <- 'list (of ' %str+% sapply(D[ , lists, with = FALSE],
                                                function(col) {
                                                  glue::collapse(unique(sapply(col, class)) %str+% "s", sep = ', ', last = ', and ')
                                                }) %str+% ")"
  }
 
  output <- data.table(Name = layers, Class = classes, Type = gsub('[0-9]*$', '', names(layers)))
  output <- output[Type %in% targets]
  
  output
}

#' @export
showLayers <-  function(humdrumR, tandem = TRUE, reference = TRUE, struct = TRUE) {
  layers <- layers(humdrumR, tandem, reference, struct)
  
  activelayer <- layers$Name %in% activeString(humdrumR)
  layers$Name <- ' ' %str+% layers$Name
  layers$Name[activelayer] <- gsub('^ ', '*', layers$Name[activelayer])
  layers$Name <- str_pad(layers$Name, width = max(nchar(layers$Name)), side = 'right')
  
  layprint <- layers$Name %str+% ' :: ' %str+% layers$Class
  
  
  cat('\t\tLayers: ', layprint %str*% '\n\t\t        ', '\n', sep = '')
  invisible(layers)
}

layersInFormula <- function(humtab, form) {
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




isLayer <- function(humdrumR, names) names %in% layers(humdrumR)$Name 

nulltypes <- c(G='!!', I = '*', L= '!', d = '.', D = NA, M = '=')

`addLayers<-` <- function(object, value) {
 object@Layers$User <- unique(c(object@Layers$User, value))
 object
}

`removeLayers<-` <- function(object, value) {
  object@Layers$User <- object@Layers$User[!object@Layers$User %in% value]
  object
}

`putLayers<-` <- function(object, copyLayer = NULL, value)  {
  humtab <- object
  for (name in value) {
    newlayer <- if (is.null(copyLayer)) nulltypes[humtab$Type] else humtab[[copyLayer]]
    humtab[[name]] <- newlayer
  }
  
  humtab
}

#' @export
setMethod('[<-', signature = c(x = 'humdrumR', i = 'character', value = 'vector'),
          function(x, i, value) {
            
            if (length(value) == nrow(x)) {
              x@Humtable$D[i] <- value
            } else {
              stop(glue::glue("Can't assign this value to '{name}' layer, because it is the wrong length."))
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
            
            removeLayers(value) <- colnames(humtab) %~x% 'Pipe'
            pipes <- pipeLayers(humtab)
            
            if (len0(pipes)) pipes <- activeString(value)
            
            pipes <- tail(pipes, n = length(i))
            
            colnames(humtab)[colnames(humtab) %in% pipes] <- i
            
            if (any(colnames(humtab) %~% 'Pipe')) humtab[ , eval(colnames(humtab) %~x% 'Pipe') := NULL]
            
            putHumtab(value, drop = TRUE) <- humtab
            addLayers(value) <- i
            
            value@Active <- swap.(setNames(list(as.symbol(i)), pipes), value@Active)
            
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

# #' @export
# setMethod('[',  signature = c(x = 'humdrumR', i = 'missing'), 
#           function(x, ...) {
#             factors <- as.list(match.call(expand.dots = TRUE))[-1:-2]
#             if (len0(factors)) return(x)
#             
#             if (any(sapply(factors, is.null))) {
#               x@Partition <- list() 
#             } else {
#               
#               if (lennot0(factors) && is.null(names(factors))) names(factors) <- rep('each', length(factors))
#               names(factors) <- gsub('by|partition|split|divide|each', 'eachfile', names(factors))
#               names(factors) <- gsub('if|when|onlyif|onlywhere|only|where', 'filewhere', names(factors))
#               
#               factors <- factors[names(factors) %in% c('eachfile', 'filewhere')]
#               
#               x@Partition <- c(x@Partition, factors)
#             }
#             x
#           })

##################################################[[]] ####

# #' @export
# setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'missing'), 
#           function(x, ...) {
#             factors <- as.list(match.call(expand.dots = TRUE))[-1:-2]
#             if (len0(factors)) return(x)
#             
#             if (any(sapply(factors, is.null))) {
#              x@Partition <- list() 
#             } else {
#               
#               if (lennot0(factors) && is.null(names(factors))) names(factors) <- rep('each', length(factors))
#               names(factors) <- gsub('by|partition|split|divide', 'each', names(factors))
#               names(factors) <- gsub('if|when|onlyif|onlywhere|only', 'where', names(factors))
#               
#               factors <- factors[names(factors) %in% c('each', 'where')]
#               
#               x@Partition <- c(x@Partition, factors)
#             }
#             x
#             })

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
  # first add missing layers (columns)
  GLIMd <- lapply(GLIMd, 
                  function(tab) {
                    missinglayers <- colnames(D)[!colnames(D) %in% colnames(tab)]
      
                    if (len0(missinglayers)) return(tab)
                    
                    putLayers(tab, 'Token') <- missinglayers
                    
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
  if (lennot0(prebarline))   humtab <- humtab[!(Record < last(prebarline) & Type == 'M')]
  
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
 
 activecall <- call('c', actives %str+% '_new_foldstopxxx')
 
 expr <- expr <= swap(X = activecall)
 expr <- expr <= swap(Y = pasteexpr)
 
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
  humtab_rv[['Active']]  <- humtab_rv[ , sapply(Active, collapsestr(sep))]
  
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

local_path <- function(path) path %str-% '^.*/'

print_humtab_humdrumAble <- function(humdrumR, cutMiddle = FALSE, global = FALSE) {
  humtab_rs <- collapseRecords(humdrumR, pad = TRUE, trim = 16L, global = global)
  recordn <- num2str(humtab_rs$Record, pad = TRUE)
  records <- humtab_rs[['Active']]
  records <- paste(recordn, ':', 
                   ifelse(grepl('^!!', records, useBytes = TRUE), ' ', '    '),
                   records, sep = '')
  
  ellipsis <- '#' %str*% max(nchar(records))
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
    
  records <- unlist(Map(append, records, ellipsis %str+% ' ' %str+% names(records), after = 0L))
  
  records <- records %str+% "\n"
  
  
  cat(records, sep = '')
  
}


print_humtab_nothumdrumAble <- function(humdrumR, cutMiddle = FALSE) {
  act <- getActive(humdrumR)
  D <- getD(humdrumR)
  
  lays <- layers(humdrumR)
  refs <- lays$Name[lays$Type == 'Reference']
  
  notact <- D[ , !colnames(D) %in% activeString(humdrumR), with = FALSE ]
  notact <- notact[ , colnames(notact) %in% c(refs, 'File', 'Spine', 
                                              'Record', 'Path', 'Section', 
                                              'Exclusive', 'BarN', 'DoubleBarN'), with = FALSE]
  
  notact <- notact[ , sapply(notact, function(col) !all(duplicated(col)[-1]) & !any(is.na(col))), with = FALSE]
  printnotact <- nrow(notact) > 0 && nrow(notact) == length(act)
  if (printnotact) {
    ellipsis <- '#' %str*% 4
    collabs <- lapply(colnames(notact), function(col) gsub('File: ', '', col %str+% ": ") %str+% notact[[col]])
    collabs <- apply(do.call('cbind',collabs), 1, 
                     function(row) (' ' %str*% 15) %str+% ellipsis %str+% paste(row, collapse = ', ') %str+% ellipsis)
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
            
            ## Layers
            showLayers(object, tandem = FALSE, struct = FALSE, reference = FALSE)
        
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
              stop(glue::glue("'{whichnot}' layer{plural} not numeric."),  call. = FALSE)
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
            
            newtokens <- e1 %str+% getActive(humdrumR)[[1]]
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
              stop(glue::glue("'{activeString(humdrumR)[[1]]}' layer is not numeric."),  call. = FALSE)
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
