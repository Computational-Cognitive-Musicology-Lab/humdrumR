#' @export
humApply <- function(humdrumR, 
                     applyTo = 'D', 
                     partition = list(),
                     tee = FALSE,
                     ngrams = 1,
                     windows = NULL,
                     graphics = list()) {
          
            if (class(humdrumR) != "humdrumR") stop('humApply can only be applied to humdrumR objects.')
            
            ### meta expressions
            humtab <- getHumtab(humdrumR, applyTo)
            
            ###
            if (lennot0(graphics)) {
              oldpar <- par(no.readonly = TRUE)
              on.exit(par(oldpar))
              do.call('par', graphics)
            }
            
            #
            partition <- c(humdrumR@Partition, partition)
            #### main expresion
            funcform <- parseForm(humtab, humformula@Formula, humdrumR@Active, ngrams)
            humfunc  <- exprAsFunc(funcform, captureOutput = !tee)
            
            ##################
            #### do it
            ##################
           
            
            newhumtab <- if (len0(partition)) {
              humfunc(humtab)
            } else {
              partApply(humtab, partition, humfunc)

            }
            newlayers <- newhumtab[['newlayers']]
            if (!is.null(newlayers)) newhumtab[ , newlayers := NULL]
            
            # if we mixed record types, the newhumtab will not know
            # where to be put back
            if (any(is.na(newhumtab$Type))) {
              newtype <- if ('D' %in% applyTo) 'D' else applyTo[1]
              newhumtab$Type[is.na(newhumtab$Type)] <- newtype
            }
            
            putHumtab(humdrumR, drop = TRUE) <- newhumtab
            if (!is.null(newlayers)) {
              newlayers <- unique(strsplit(newlayers[1], split = ' ')[[1]])
              addLayers(humdrumR)  <- newlayers
              if (any(names(partition) == 'where')) {
                act <- ifelsecalls(partition['where'], c(lazyeval::f_rhs(humdrumR@Active), lapply(newlayers, as.symbol)))
                humdrumR@Active <- lazyeval::f_new(act)
              } else {
                humdrumR <- setActiveString(humdrumR, newlayers) 
              }
            }
            
            
            # if (len0(humformula@Pipe)) {
              humdrumR 
            # } else {
              # humApply(humdrumR, humformula@Pipe[[1]])
            # } 
              
          }

#' @export
exprAsFunc <- function(funcform, captureOutput = TRUE) {
  function(humtab) {
    # output <- eval(funccall, envir = humtab)
    output <- lazyeval::f_eval_rhs(funcform, data = humtab)
    if (captureOutput) pipeIn(humtab) <- output
    humtab
  }
}

ifelsecalls <- function(calls, layers) {
  call <- call('ifelse', lazyeval::f_rhs(calls[[1]]), 
               quote(.), layers[[1]])
 if (len1(calls)) {
   call[[3]] <- layers[[2]]
 } else {
   call[[3]] <- Recall(calls[-1], layers[-1])
 }
  
  call
}


#' @export
parseForm <- function(humtab, funcform, active, ngram = NULL) {
  funccall <- lazyeval::f_rhs(funcform)
  
  # turn dots to active formula
  funcform <- activateForm(funcform, active)
  
  # interpretations
  funcform <- tandemsForm(funcform, humtab$Tandem)
  
  # splats
  funcform <- splatForm(funcform)
  
  # find what layers (if any) are used in formula
  usedInExpr <- unique(layersInFormula(humtab, funcform))
  
  if (len0(usedInExpr)) { stop("Your humdrum pipe-formula doesn't reference any layers in your humdrum data.\n A
                               dd a layer somewhere or add a dot (.), which will automatically grab the default, 'active' layer.",
                                call. = FALSE)}

  # if the targets are lists, Map
  lists <- sapply(humtab[ , usedInExpr, with = FALSE], class) == 'list'
  if (any(lists)) funcform <- mapifyForm(funcform, usedInExpr)
    
  # if ngram is present
  if (!is.null(ngram)) funcform <- ngramifyForm(funcform, ngram, usedInExpr)
  
  
  funcform
}


activateForm <- function(funcform, active) {
  active <- lazyeval::f_rhs(active)
  funcform <= swap(. = active)
}

tandemsForm <- function(funcform, tandem) {
 applyExpr(lazyeval::f_rhs(funcform),
           function(ex) {
             exstr <- deparse(ex)
             interp <- grepl('^\\*[^*]', exstr)
             
             if (interp) {
               regex <- stringr::str_sub(exstr, start = 2L)
               call('getTandem', quote(Tandem), regex)
             } else {
               ex
             }
           }) -> newcall
  
  lazyeval::f_new(newcall, env = lazyeval::f_env(funcform))
  
}


splatForm <- function(funcform) {
  funccall <- lazyeval::f_rhs(funcform)
  
  funccall <- parseAts(funccall)
  
  lazyeval::f_new(funccall, env = lazyeval::f_env(funcform))
  
}

parseAts <- function(expr) {
  if (len1(expr)) return(expr)
  
 callhead <- deparse(expr[[1]])
  
 if (callhead == ':') {
   list(as.list(expr[-1]) %class% 'splat')
   
 } else {
   new <- list()
   for (i in 2:length(expr)) {
     new <- c(new,  Recall(expr[[i]]))
   }
   
   names(new) <- names(as.list(expr))[-1]
   
   ats <- sapply(new, inherits, what = 'splat')
   if (any(ats)) {
     parseAt(callhead, new[ats], new[!ats])
   } else {
     do.call('call', c(callhead, new), quote = TRUE)
   }
 }
}

parseAt <- function(call, splats, otherargs) {
  splats <- lapply(splats,
                   function(splat) call('unname', do.call('call', quote = TRUE,
                                           c('tapply', splat, quote(c)))))
  call('%splat|%', do.call('call', c('c', splats, otherargs), quote = TRUE), as.symbol(call))
  # call('do.call', call, do.call('call', c('c', splats, otherargs), quote = TRUE))
}

mapifyForm <- function(funcform, usedInExpr) {
  
  funcform <- xifyForm(funcform, usedInExpr)
  
  funccall <- lazyeval::f_rhs(funcform)
  funccall <- do.call('call', quote = TRUE,
                      as.list(c(list('Map', funccall), 
                                parse(text = usedInExpr))))
  
  lazyeval::f_new(funccall, env = lazyeval::f_env(funcform))
}

ngramifyForm <- function(funcform, n, usedInExpr) {
  
  funcform <- xifyForm(funcform, usedInExpr)
  
  funccall <- lazyeval::f_rhs(funcform)
  
  newfunccall <- call('applyNgram', 
                      lazyeval::f_rhs(n),
                      do.call('call', quote = TRUE,
                              c('list', lapply(usedInExpr, as.symbol))),
                      funccall)
  
  lazyeval::f_new(newfunccall, env = lazyeval::f_env(funcform))
  
}

xifyForm <- function(funcform, usedInExpr) {
  # this is used by listifyForm and ngramifyForm, etc.
  fargs <- as.pairlist(alist(x = )[rep(1, length(usedInExpr))])
  names(fargs) <- '.' %str+% tolower(usedInExpr)
 
  funcform <- funcform <= do.call('swap', setNames(lapply(names(fargs), as.symbol), usedInExpr))
  
  funccall      <- quote(function() {} )
  funccall[[2]] <- fargs
  funccall[[3]] <- lazyeval::f_rhs(funcform)
  
  lazyeval::f_new(funccall, env = lazyeval::f_env(funcform))
  
}



partApply <- function(humtab, partitions, humfunc) {
  if (len0(partitions)) return(humfunc(humtab))
  funccall <- if (len1(partitions)) quote(humfunc(.SD)) else call('partApply', quote(humtab), quote(partitions[-1]), quote(humfunc))
  
  curpart <- lazyeval::f_rhs(partitions[[1]])
  
  partcall <- call('[', quote(humtab), alist(i=)[[1]], funccall)
  if (names(partitions)[1] == 'each') {
    partcall[['by']]      <- curpart
    partcall[['.SDcols']] <- quote(colnames(humtab))
    # get environment from formulae
    env <- environment()
    parent.env(env) <- lazyeval::f_env(partitions[[1]])
    
    # do it!
    output <- eval(partcall, envir = env)
    
    output <- output[ , !duplicated(colnames(output)), with = FALSE]
    
    
    
  } else {
    partcall[[3]] <- curpart
    output <- eval(partcall)
    
    negatecall <- partcall[1:3]
    negatecall[[3]] <- call('!', call('(', curpart))
    
    rest <- eval(negatecall)
    
    output <- rbind(output, rest, fill = TRUE)
  
  }
  
  output
}


#### Reassembling ----
#This is the hard part, putting pipeout output back into data.table

# options:
# Output are either length 1, same length as original, short (<21), or some length in between, or some other object.
#     If they are same length, put them back where they came, or into new column, no collapsing.
#     If they are length 1, put them back, but collapse everything else to 1
#     If they are short (<21) AND named, place them as n appropriately named columns (nrow = 1), collapse everything else
#     If they are less than 0, do nothing
#     In any other case, put into list (to make it singleton) and collapse everything else.

#     inputs are 1 or more vectors.
#     Outputs, may be single vectors, or lists of vectors.
#     Outpurs are either placed back in original column, or into new dummy columns
#          if output is list of vectors of matching length, create new columns.
#     Multiple input columns

pipeLayers <- function(humtab) {
  colnms <- colnames(humtab)
  
  pipelayers  <- colnms[grepl('Pipe', colnms)]
  
  if (lennot0(pipelayers)) pipelayers <- pipelayers[order(as.numeric(stringr::str_extract(pipelayers, '[0-9]+')))]
  
  pipelayers
}

curPipeN <- function(humtab) length(pipeLayers(humtab))   



lengths_ <- function(ls) ifelse(sapply(ls, is.object), 1, sapply(ls, length))

`pipeIn<-` <- function(object, value) {
  humtab <- object
  curpipen <- curPipeN(humtab) 
  
  lenvalue <- length(value)
  if (lenvalue > nrow(humtab)) stop("Sorry, we don't currently support return values that are longer than their input.", .call = FALSE)
  
  if (is.object(value)) value <- list(list(value))
  
  if (is.atomic(value) && allnamed(value) && lenvalue < 10L) value <- as.list(value)
  
  if (is.atomic(value) || (is.list(value) & length(value) == nrow(humtab))) value <- list(value)
  
  ### value should be a list now
  if (!is.list(value)) stop(glue::glue("Nat, pipeIn just recieved an input of class {class(value)} which doesn't fit your conditions."), .call = TRUE)
  
  ####
  if (!allsame(lengths_(value))) value <- list(value)
  
  
  ## at this point, value should:
  # be a list with all elements of the same length.
  # may or may not be named
  # each element in this list will be a new layer in the humtable
  
  lenvalue <- lengths_(value)[1]
  if (lenvalue < nrow(humtab)) humtab <- collapseHumtab(humtab, n = lenvalue)
    
  #
  nnewlayers   <- length(value)
  targetlayers <- if (allnamed(value)) names(value) else 'Pipe' %str+% (curpipen + seq_len(nnewlayers))
  
  humtab[ , targetlayers] <- value
    
  humtab[ , newlayers := paste(targetlayers, collapse = ' ')]
  # need to let higher functions (humApply) know which are the new columns, so they can be set to the new active
  humtab

} 





#' @export
collapseHumtab <- function(dt, n = 1L) {
  dt[ , Map(collapse2n, .SD, colnames(dt), lapply(dt, class), n = n)] 
  }

#' @export
collapse2n <- function(x, colname, class, n = 1) {
  uniq <- unique(x)
  uniq <- uniq[!is.na(uniq)]
  if (len0(uniq)) return(rep(as(NA, class), n))
  
  if (colname %in% c('Record', 'Spine', 'Path', 'ColumnNumber', 
                     'StopNumber', 'Bar', 'DoubleBarline', 'NData', 'NFile')) {
    uniq[1:n]
  } else {
    rep( if (len1(uniq)) uniq else as(NA, class) , n)
  }
  
}

collapse2one <- function(x, colname) {
  
  uniq <- unique(x)
  
  if (colname %in% c('Record', 'Spine', 'Path', 'ColumnNumber', 
                     'StopNumber', 'Bar', 'DoubleBarline', 'NData', 'NFile')) {
    if (!all(is.na(uniq))) min(uniq, na.rm = TRUE) else NA
  } else {
    if (len1(uniq)) uniq else NA
  }
  
}


#### Interpretations ----

#' @export
getTandem <- function(tandem, regex) {
  
  regex <- '\\*' %str+% regex
  
  matches <- stringr::str_extract(tandem, pattern = regex)
  
  matches
  
}



####### Regex dispatch

#' @export
.do2substr_inplace <- function(.func, regex) {
          
          function(str) {
                    matches <- stringi::stri_extract_first(str = str, regex = regex)
                    
                    modified <- as.character(.func(matches))
                    stringi::stri_replace_first(str, regex = regex,  
                                                replacement = modified)
          }
}

#' @export
.do2substr <- function(.func, regex) {
          
          function(...) {
                    strs <- list(...)
                    matches <- lapply(strs, stringi::stri_extract_first, regex = regex)
                    
                    do.call('.func', matches)
          }
          
}


#' @export
regexDispatch <- function(..., inplace = FALSE) {
          .funcs <- list(...)
          if (length(.funcs) == 0) return(id)
          
          regexes <- names(.funcs)
          reFuncs <- Map(if (inplace) .do2substr_inplace else .do2substr, 
                         .funcs, regexes)
          function(str) {
                    Nmatches <- sapply(regexes,  
                                       function(re) sum(stringi::stri_detect(str, regex = re)))
                    
                    if (any(Nmatches > 0)) {
                              Ncharmatches <- sapply(regexes[Nmatches > 0],
                                                     function(re) sum(nchar(stringi::stri_extract_first(str, regex = re))))
                              reFuncs[[which(Nmatches > 0)[which.max(Ncharmatches)]]](str)
                    } else {
                              str
                    }
          }
}
