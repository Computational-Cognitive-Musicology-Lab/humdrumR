
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
            .Object@Pipe <- if (is.null(Pipe) && length(pipe) == 0L) {
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
