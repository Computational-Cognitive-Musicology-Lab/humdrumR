
#### humFormula ----

#' @export
setClass('humFormula', 
         slots = c(Meta = 'list', Formula = 'formula', Pipe = 'list')) -> humform

#' @export humform
NULL

tryform <- function(form) {
  try(lazyeval::f_eval(form), silent = TRUE)
}

#' @export
setMethod('initialize', 'humFormula',
          function(.Object, formula = ~., meta = list(), pipe = list()) {
            FormPipe <- splitFormulaPipe(formula)
            Form <- FormPipe$Form
            Pipe <- FormPipe$Pipe
            
            evaled <- tryform(Form)
            if (inherits(evaled, 'humFormula')) {
              .Object <- evaled
            } else {
              .Object@Formula <- Form
              meta <- meta[!duplicated(meta)]
              meta <- lapply(meta, tandemsForm)
              .Object@Meta    <- meta
            }
            .Object@Pipe <- if (is.null(Pipe) && len0(pipe)) {
               list()
            } else {
              if (!is.null(Pipe)) {
                evaledpipe <- tryform(Pipe)
                if (inherits(evaledpipe, 'humFormula')) {
                  list(evaledpipe)
                } else {
                  if (inherits(evaledpipe, 'function')) {
                    list(humform() * evaledpipe)
                  } else {
                    list(humform(formula = Pipe, 
                                 meta = list(), 
                                 pipe = pipe))
                  }
                }
               
              } else {
                 pipe 
              }
            }
            
            .Object
          })



splitFormulaPipe <- function(form) {
  expr <- lazyeval::f_rhs(form)
  
  exprstr <- deparse(expr)
  
  if (any(grepl('\\|', exprstr))) {
    split <- str_split(exprstr,  pattern = ' *\\| *', n = 2)[[1]]
    form <- lazyeval::f_new(parse(text = split[[1]])[[1]], env = lazyeval::f_env(form))
    pipe <- lazyeval::f_new(parse(text = split[[2]])[[1]], env = lazyeval::f_env(form))
  } else {
    pipe <- NULL
  }
  
  list(Form = form, Pipe = pipe)
  # if (expr %len>% 1L && deparse(expr[[1]]) == '|') {
  #   list(Form = lazyeval::f_new(expr[[2]], env = lazyeval::f_env(form)),
  #        Rest = lazyeval::f_new(expr[[3]], env = lazyeval::f_env(form)))
  # } else {
  #   list(Form = form, Rest = NULL)
  # }
}


splitFormula <- function(form) {
  if (is.null(form) || len1(form) || deparse(form[[1]]) !=  '~') return(form)
  
  form <- eval(form)  
  
  c(Recall(lazyeval::f_lhs(form)), 
    Recall(lazyeval::f_rhs(form)))
}



#' @export
setMethod('c', signature = c('humFormula'),
          function(x, y) {
            humform(formula = y@Formula, 
                    meta = c(x@Meta, y@Meta),
                    pipe = y@Pipe)
          })

#' @export
setMethod('*', signature = c('humFormula', 'humFormula'),
          function(e1, e2) {
            c(e1, e2)
          })
#' @export
setMethod('^', signature = c('humFormula', 'humFormula'),
          function(e1, e2) {
            pipe <- e2@Pipe
            if (lennot0(pipe)) e2@Pipe <- list(e1 ^ pipe[[1]])
            e1 * e2
          })

#' @export
setMethod('*', signature = c('humFormula', 'function'),
          function(e1, e2) {
            funcform <- lazyeval::f_capture(e2)
            
            if (!is.symbol(lazyeval::f_rhs(funcform))) {
              .pipefunc <- lazyeval::f_eval(funcform)
              newform <- quote(.pipefunc(.))
            } else {
              newform <- as.call(c(lazyeval::f_rhs(funcform), quote(.)))
            }
            
            funcform <- lazyeval::f_new(newform, 
                                        env = lazyeval::f_env(funcform))
      
            e1@Formula <- funcform
            e1
          })
#' @export
setMethod('^', signature = c('humFormula', 'function'),
          function(e1, e2) {
            e1 * e2
          })
#' @export
setMethod('*', signature = c('humFormula', 'formula'),
          function(e1, e2) {
            e1 * humform(e2)
          })
#' @export
setMethod('^', signature = c('humFormula', 'formula'),
          function(e1, e2) {
            e1 ^ humform(e2)
          })
#' @export
setMethod('show', 'humFormula',
          function(object) {
            
            cat(' Expression:\n\t')
            cat(paste(deparse(lazyeval::f_rhs(object@Formula)), collapse = '\n'), '\n') 
            
            metas <- sapply(object@Meta, function(form) deparse(lazyeval::f_rhs(form)))
            if (lennot0(metas)) {
              cat('\n Meta-expressions:\n\t')
              cat((names(metas) %str+% ' = ' %str+% metas) %str*% '\n\t')
            }
          })


#### Applying to humdrumR ----



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
            humtab <- getHumtab(hum, applyTo)
            
            ###
            if (lennot0(graphics)) {
              oldpar <- par(no.readonly = TRUE)
              on.exit(par(oldpar))
              do.call('par', graphics)
            }
            
            #
            partition <- c(hum@Partition, partition)
            #### main expresion
            funcform <- parseForm(humtab, humformula@Formula, hum@Active, ngrams)
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
            
            putHumtab(hum, drop = TRUE) <- newhumtab
            if (!is.null(newlayers)) {
              newlayers <- unique(strsplit(newlayers[1], split = ' ')[[1]])
              addLayers(hum) <- newlayers
              if (any(names(partition) == 'where')) {
                act <- ifelsecalls(partition['where'], c(lazyeval::f_rhs(hum@Active), lapply(newlayers, as.symbol)))
                hum@Active <- lazyeval::f_new(act)
              } else {
                hum <- setActiveString(hum, newlayers) 
              }
            }
            
            
            if (len0(humformula@Pipe)) {
              hum 
            } else {
              humApply(hum, humformula@Pipe[[1]])
            } 
              
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


####Piping ----



#' @export
setMethod('|', signature = c(e1 = 'humdrumR', e2 = 'function'),
          function(e1, e2) {
            funcform <- lazyeval::f_capture(e2)
            
            if (!is.symbol(lazyeval::f_rhs(funcform))) {
              .pipefunc <- lazyeval::f_eval(funcform)
              newform <- quote(.pipefunc(.))
            } else {
              newform <- as.call(c(lazyeval::f_rhs(funcform), quote(.)))
            }
            
            funcform <- lazyeval::f_new(newform)
            
            
            humApply(e1, funcform)
          })

#' @export
setMethod('|', signature = c(e1 = 'humdrumR', e2 = 'humFormula'),
          function(e1, e2) {
            humApply(e1, e2)
          })

#' @export
setMethod('|', signature = c(e1 = 'humdrumR', e2 = 'formula'),
          function(e1, e2) {
            form <- humform(e2)  
            humApply(e1, form)
          })
####humFormula building functions ----

#' @export
tee <- humform(meta = list(tee = ~TRUE))

#' @export
wind <- function(x11 = FALSE, ...) {
  dots <- lazyeval::dots_capture(...)
  meta <- do.call('call', quote = TRUE, 
                  c('par',  lapply(dots, lazyeval::f_rhs)))
  
  if (x11) meta <- call('{', quote(X11()), meta)
  
  meta <- list(par = lazyeval::f_new(rhs = meta))
  
  
  tee * humform(meta = meta)
  
}

#' @export
records <- function(x) {
  meta <- lazyeval::f_capture(x)
                
  humform(meta = list(types = meta))
}

#' @export
each <- function(...) {
  meta <- match.call(expand.dots = TRUE)
  
  meta[[1]] <- quote(.)
  meta <- lazyeval::f_new(rhs = meta, 
                          env = parent.env(environment()))
  humform(meta = list(each = meta))
}

#' @export
where <- function(...) {
  meta <- match.call(expand.dots = TRUE)
  
  if (meta %len>% 2L) {
    meta[[1]] <- quote(`&`)
    for (i in 2:length(meta)) {
     meta[[i]] <- call('(', meta[[i]]) 
    }
  } else { 
    meta <- meta[[-1]]
  }
  
  meta <- lazyeval::f_new(rhs = meta, 
                          env = parent.env(environment()))
  humform(meta = list(where = meta))
}

#' @export
ngram <- function(n = quote(2), pad = TRUE) {
  meta <- lazyeval::f_capture(n)
  humform(formula = ~ paste(., collapse = '-'), meta = list(ngram = meta))
}

#' @export
swap <- function(...) {
  newenv <- as.environment(lapply(dots_capture(...), f_rhs))
  
  func <- function(form) {
    oldenv <- f_env(form)
    
    parent.env(newenv) <- oldenv
    
    f_env(form) <- newenv
    
    newform <- f_unwrap(form)
    f_env(newform) <- oldenv
    newform
  } 
  
  new('formulaModifier', func)
}

swap. <- function(calls, form) {
  newenv <- as.environment(calls)
  
  oldenv <- f_env(form)
  
  parent.env(newenv) <- oldenv
  
  f_env(form) <- newenv
  
  newform <- f_unwrap(form)
  f_env(newform) <- oldenv
  newform
}


#' @export
setClass('formulaModifier', contains = 'function')

#' @export
setMethod('<=', c('formula', 'formulaModifier'),
          function(e1, e2) {
            e2(e1)
          })

#' @export
setMethod('<=', c('humFormula', 'formulaModifier'),
          function(e1, e2) {
            e1@Formula <- e1@Formula <= e2
            e1
            
          })


#########windowing


# apply2gram <- function(vecs, f = c, dir = 1, pad = TRUE, padder = NA, splat = FALSE, ...) {
#   cur   <- lapply(vecs, rotate, rotation =  1 - (dir ==  1))
#   other <- lapply(vecs, rotate, rotation = -1 + (dir == -1))
#   
#   
#   if (splat) {
#     .f <- function(...) do.call('f', unlist(list(...)))
#     output <- Map(.f, cur, other)
#     
#   } else {
#     ngs    <- Map(function(x, y) browser(), cur, other)
#     
#     if (!pad) { ngs <- ngs[!sapply(ngs, any %.% is.na)] } 
#     
#     output <- lapply(ngs, f, ...)
#   }
#   if (!pad) { 
#     output <- output[!sapply(output, any %.% is.na)] 
#   } else {
#     if (!is.na(padder)) output <- lapply(output, f.({.[is.na(.)] <- padder; .}))
#   }
#   if (all(lengths(output) == 1)) output <- unlist(output)
#   
#   output  
# }


#### Windowing functions ----

#' @export
applyNgram <- function(n = 2, vecs, f = c, by = NULL, pad = TRUE, 
                       padder = NA, splat = !is.null(by), ...) {
  # x is list of vectors of same length
  if (!is.null(by)) vecs <- lapply(vecs, split, f = by)
  
  if (n == 0) stop("You called applyNgram with n = 0, but you can't make an 0-gram!", call. = FALSE)
  if (!allsame(lengths(vecs))) stop("You have tried to applyNgram across multiple vectors of different lengths, but this is not supported.", .call = FALSE)
  
  n <- n - (sign(n))
  
  
  if (n == 0) { 
    output <- do.call('Map', c(f, vecs))
  } else {
    
    starts <- seq_along(vecs[[1]])
    if (pad) {
      vecs <- lapply(vecs, function(vec) c(rep(NA, abs(n)), vec, rep(NA, abs(n))))
      starts <- starts + abs(n)
    } else {
      starts <- starts[ (starts + n) <= length(vecs[[1]]) & starts + n >= 1]
    }
    
    inds   <- if (sign(n) == 1) Map(`:`, starts, starts + n) else Map(`:`, starts + n, starts)
    
    #
    
    ngs <- lapply(vecs, 
                  function(vec) {
                    lapply(inds, function(i) vec[i])
                  })
    .f <- if (splat) { 
      function(...) {do.call('f', unlist(list(...), use.names = FALSE, recursive = FALSE)) }
      } else {
        f
      }
    output <- do.call('Map', c(.f, ngs))
    
    if (pad && !is.na(padder)) output <- lapply(output,
                                                function(out) {
                                                  if (is.character(out)) gsub('NA', padder, out) else `[<-`(out, is.na(out), padder)
                                                })
                                                
  } #end of if(n == 0) else
  
  if (all(lengths(output) == 1)) output <- unlist(output)
  
  output
  
}


## On
## Last (last on)
##
## Off
## Prev (prev off)

### NextOn              ~      NextOff
### NextOn = last + x*n 
### NextOn = prev +- x*n
###                            NextOff = on + x*n
###                            NextOff = prev + x*n


windowEdges <- function(inds, start, end, type = 'exclude') {
          
          if (type == 'exclude') {
                    ok <- !unlist(do.call('Map', c(`|`, lapply(inds, function(x) x < start | x > end))))
          }
          if (type == 'trim') {
                    inds <- lapply(inds, 
                                  function(x) {
                                    x[x < start] <- start
                                    x[x > end] <- end
                                    x
                                  })
                    
                    ok <- !unlist(do.call('Map', c(`>=`, inds)))
          }
          return(lapply(inds, '[', ok))
          
          
}

#'  @export 
setGeneric('windowIndices',  function(vec, open, close, start = 1) standardGeneric('windowIndices') )
setMethod('windowIndices', signature = c(vec = 'vector', open = 'numeric', close = 'numeric'),
           function(vec, open, close, start = 1) {
                     opens <- cumsum(c(start, rep(open, length(vec) / sum(open))))
                     
                     closes <- opens + close
                     
                     list(opens, closes)
                    
           })

setMethod('windowIndices', signature = c(vec = 'vector', open = 'numeric', close = 'character'),
          function(vec, open, close, start = 1) {
                    opens <- cumsum(c(start, rep(open, length(vec) / sum(open))))
                    
                    closes <- grep(close, vec)
                    closes <- sapply(opens, function(o) closes[closes > o][1])
                    list(opens, closes)
                    
                    
          })

setMethod('windowIndices', signature = c(vec = 'vector', open = 'character', close = 'character'),
          function(vec, open, close, start = 1) {
                    opens <- grep(open, vec)
                    opens <- opens[opens >= start]
                    
                    closes <- grep(close, vec)
                    closes <- sapply(opens, function(o) closes[closes > o][1])
                    list(opens, closes)
                    
                   
          })

setMethod('windowIndices', signature = c(vec = 'vector', open = 'character', close = 'numeric'),
          function(vec, open, close, start = 1) {
                    opens <- grep(open, vec)
                    opens <- opens[opens >= start]
                    
                    closes <- opens + close
                    list(opens, closes)
          })
