

######### Function composition ----

compose <- function(...) UseMethod('compose')
compose.default <- function(..., fenv = parent.frame()) {
# accepts a NAMED ... of functions
    fs <- rev(list(...))
    fnames <- names(fs)
    ### arguments
    fargs <- lapply(fs, fargs)
    fargs[-1] <- lapply(fargs[-1], 
                        function(farg) {
                           if (names(farg)[[1]] == '...') farg <- c(alist(tmp = ), farg)
                         farg })
    
    fargNames <- lapply(fargs, 
                        function(farg) {
                          names <- names(farg)
                          names(names) <- ifelse(names == '...', '', names)
                          rlang::syms(names)
                          })
    
    
    args <- do.call('c', c(fargs[1], 
                           lapply(fargs[-1], function(farg) farg[-1]),
                           use.names = FALSE))
    args <- args[!duplicated(names(args)) & names(args) != 'tmp']
    
    # firstArg <- rlang::sym(names(args)[[1]])
    pipeArgs <- rlang::syms(sapply(fargs, function(arg) names(arg)[[1]]))
    
    
    
    ### body
    body <- rlang::expr({
        !!!Map(function(bod, arg, argnames) {
            rlang::expr(!!arg <- stickyApply(!!bod, !!!argnames))
            }, 
            rlang::syms(head(fnames, -1)), 
            pipeArgs[-1], 
            head(fargNames, -1))
      
        
        # stickyApply(!!(rlang::sym(tail(fnames,1))), !!!fargNames[[length(fargNames)]])
        (!!tail(fnames,1))(!!!fargNames[[length(fargNames)]])
        })
    
    body <- predicateDispatch.expr('is.na', body, names(args)[1], negate = TRUE)
    body <- memoizeDispatch.expr(body, names(args))
    
    ### environment
    # fenv <- new.env() # parent.env(parent.frame())
    Map(function(fname, f) assign(fname, f, envir = fenv), 
        c(fnames, 'memoiseParse', 'predicateParse'), 
        c(fs, memoiseParse, predicateParse))
    
    
    # Create the new function
    newfunc <- rlang::new_function(args, body, fenv)
    
    attr(newfunc, 'composition') <- list(...)
    
    
    newfunc %class% 'composed'
}

compose.composed <- function(...) {
    fs <- list(...)
    
    fs <- c(attr(fs[[1]], 'composition'), fs[-1])
    do.call('compose.default', fs)
    
}

#' @export
print.composed <- function(x) {
    attributes(x) <- NULL
    print(x)
    
}

#' @export
`%.%` <- function(e1, e2) { 
    f1name <- rlang::quo_text(rlang::enquo(e1))
    f2name <- rlang::quo_text(rlang::enquo(e2))
    
    fs <- setNames(c(e1, e2), c(f1name, f2name))
    do.call('compose', c(fs, list(fenv = parent.frame())))
    }



# "sticky attributes" 

#' @export
stickyApply <- function(func, ...) {
    pipe <- stickyAttrs(list(...)[[1]])
    result <- func(...)
    stickyAttrs(result) <- pipe
    result
}


stickyAttrs <- function(x) attr(x, 'sticky')

`stickyAttrs<-` <- function(x, value) {
    sticky <- stickyAttrs(x)
    sticky <- sticky[!names(sticky) %in% names(value)]
  
    attr(x, 'sticky') <- c(sticky, value)
    x
}

unstick <- function(x) {
    attr(x, 'sticky') <- NULL
    x
}

passargs <- function(x) {
  # this COULD be incorporated into compose, but I'd rather not.
  # It allows one function in the chain to change an input argument for functiosn later in the chain.
  attrs <- attributes(x)
  pass <- attrs$pass
  if (length(pass) == 0L) return(x)
  
  attr(x, 'pass') <- NULL
  
  parentargs <- names(formals(sys.function(1L)))
  
  pass <- pass[names(pass) %in% parentargs]
  
  for (arg in names(pass)) {
    assign(arg, pass[[arg]], parent.frame())
    
  }

  x  
}
    

##### "restoring" ----

inPlace <- function(result, orig, regex) {
  
    if (is.null(stickyAttrs(result)$replace)) {
      stickyAttrs(result) <- c(replace = inPlacer(orig, getRE(regex)))
    } 
    
    if (is.character(result)) return(stickyAttrs(result)$replace(result))
    
    result
    
}

inPlacer <- function(orig, regex) {
    function(result) {
        .ifelse(is.na(result), 
                orig,
                stringi::stri_replace_first(str = orig,
                                            replacement = result,
                                            regex = regex))
    }
}

as.re <- function(x, as) {
    stickyAttrs(x) <- list(as = names(as))
   x
}

`%re.place%` <- function(e1, e2) {
    stickyAttrs(e1) <- list(replace = e2)
    e1
}

re.as <- function(vector) {
    sticky <- stickyAttrs(vector)
    asfunc <- sticky$as
    if (is.null(asfunc)) return(vector)
    
    match_size(vector = vector, asfunc = asfunc, toEnv = TRUE)
    
    splitvector <- split(vector, asfunc)
    
    splitvector <- Map(stickyApply, lapply(names(splitvector), match.fun), splitvector)
    
    output <- setNames(unlist(splitvector), names(vector))
    
    sticky$as <- NULL
    stickyAttrs(output) <- sticky
    
    output %dim% vector
    
    
    
}


#' @export
re.place <- function(vector) {
    asfunc <- stickyAttrs(vector)$replace
    if (is.null(asfunc)) return(unstick(vector))
    
    asfunc(vector) %dim% vector
}


################## Predicate function generators ----


setClass('predicate.function', contains = 'function', 
         slots = c(string = 'character'))

setMethod('show', 'predicate.function',
          function(object) cat(c('f(x) =', object@string)) )

setMethod('&', c('predicate.function', 'predicate.function'),
          function(e1, e2) {
              f1 <- e1@.Data
              f2 <- e2@.Data
              func <- function(x) {
                  f1(x) & f2(x)
              }
              
              s1 <- e1@string
              s2 <- e2@string
              
              if (grepl('&|\\|', s1)) s1 <- paste0('(', s1, ')')
              if (grepl('&|\\|', s2)) s2 <- paste0('(', s2, ')')
              new('predicate.function', func,
                  string = paste(s1, s2, sep = ' & '))
          })

setMethod('|', c('predicate.function', 'predicate.function'),
          function(e1, e2) {
              f1 <- e1@.Data
              f2 <- e2@.Data
              func <- function(x) {
                  f1(x) | f2(x)
              }
              
              s1 <- e1@string
              s2 <- e2@string
              
              if (grepl('&|\\|', s1)) s1 <- paste0('(', s1, ')')
              if (grepl('&|\\|', s2)) s2 <- paste0('(', s2, ')')
              new('predicate.function', func,
                  string = paste(s1, s2, sep = ' | '))
          })

#' @export
EQ <- function(pat) {
  func <- function(x) {
    match_size(pat = pat,x = x, toEnv = TRUE)
    ifelse(is.na(pat), is.na(x), x == pat)
  }


  new('predicate.function', func, string = glue::glue('x == {deparse(pat)}'))
}

#' @export
LEN <- function(p.f) {
    func <- function(x) p.f(length(x))
 
    new('predicate.function', func,
        string = gsub('x', 'length(x)', p.f@string))
}

#' @export
ANY <- function(p.f) {
    func <- unclass(any %.% p.f)
    
    new('predicate.function', func, string = paste0('any(', p.f@string, ')'))
}
#' @export
ALL <- function(p.f) {
    func <- unclass(all %.% p.f)
    
    new('predicate.function', func, string = paste0('all(', p.f@string, ')'))
}

#' @export
GT <- function(n) {
  func <- function(x) x > n

  new('predicate.function', func, string = glue::glue('x > {deparse(n)}'))
}

#' @export
GTET <- function(n) {
  func <- function(x) x >= n

  new('predicate.function', func, string = glue::glue('x >= {deparse(n)}'))
}


#' @export
LT <- function(n) {
  func <- function(x) x < n

  new('predicate.function', func, string = glue::glue('x < {deparse(n)}'))
}

#' @export
LTET <- function(n) {
  func <- function(x) x <= n

  new('predicate.function', func, string = glue::glue('x <= {deparse(n)}'))
}

#' @export
RE <- function(pat) {
  func <- function(x)  grepl(pat, x) 

  new('predicate.function', func, string = glue::glue('x ~ {deparse(pat)}'))
}

#' @export
na <- new('predicate.function', function(x) is.na(x), string = "x == NA")
#' @export
notna <- new('predicate.function', function(x) !is.na(x), string = "x != NA")



############### Predicate dispatch ----

#' @name regexDispatch
#' @export
`%predate%` <- function(func, predicate) {
    predicateExpr <- rlang::expr_text(rlang::enexpr(predicate))
    if (grepl('function\\(', predicateExpr)) predicateExpr <- 'lambda'
    
    predicateDispatch(rlang::expr_text(rlang::enexpr(func)), predicate, predicateExpr)
}



#' @export
predicateDispatch <- function(func, predicateFunc, negate = FALSE) {
  
    fbody <- if (is.character(func)) {
       func <- match.fun(fname)
       funcCall(fname)
    } else {
       body(func)
    }
    
    #argnames
    fargs <- fargs(func)
    argnames <- names(fargs)
    
    if (length(argnames) == 0L) stop(call. = FALSE, "predicateDispatch (%predicate%) can't add a predicate to a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "predicateDispatch (%predicate%) doesn't work if the first argument of the method is ..." )
    argnames <- argnames[argnames != "..."]
    
    newenv <- new.env(parent = parent.frame())
    #### 
    if (is.function(predicateFunc)) {
      assign('predicate', predicateFunc, envir = newenv)
      predicateFunc <- 'predicate'
    }

    body <- predicateDispatch.expr(predicateFunc, fbody, argnames, negate = negate)

    
    rlang::new_function(fargs, body, newenv)
}

predicateDispatch.expr <- function(predicateFuncName, expr, argnames, negate = FALSE) {
  if (argnames[1] == '...') return(expr)
  
  argnames <- argnames[argnames != '...']
  args <- setNames(rlang::syms(argnames), argnames)
  
  predicateFuncName <- rlang::sym(predicateFuncName)
  
  rlang::expr({
    rebuild <- predicateParse(!!predicateFuncName, !!!args, negate =  !!negate, allargs = FALSE)
    predicateResult <- {!!expr}
    rebuild(predicateResult)
  })
  
}
funcCall <- function(fname) {
    # this takes the name of a function (as a string)
    # and creates an expression of this function calling
    # its arguments
    
    func <- match.fun(fname)
    fname <- rlang::sym(fname)
    ftext <- rlang::quo_text(func)
    
    
    # args
    argnames <- names(fargs(func))
    if (argnames[[1]] == '...') argnames[[1]] <- 'tmp'
    argnames <- rlang::syms(argnames)
    
    rlang::expr((!!fname)(!!!argnames))
    
}



# predicateParse <- function(predicate, argnames, inPlace, ...) {
#     args <- setNames(list(...), argnames)
#     
#     lengths <- lengths(args)
#     targets <- args[lengths == lengths[1]]
#     bool <- apply(sapply(targets, predicate), 1, any)
#     list2env(lapply(targets, '[', i = bool), 
#              envir = parent.frame())
#     
#     output <- if (inPlace) args[[1]] else vectorNA(length(args[[1]]), class(args[[1]]))
#     
#     function(result) {
#         if (length(result) != sum(bool, na.rm = TRUE)) return(if (inPlace) result else output)
#         output[bool] <- result
#         output
#     }
# }


#' @export
predicateParse <- function(predicateFunc, ..., inPlace = TRUE, allargs = FALSE, negate = FALSE, onlymatch = FALSE) {
  args <- list(...)
  
  if (is.null(names(args)) || any(names(args) == "")) .stop("predicateParse requires that all arguments are named.")
  
  # only atomic/struct args are affected
  args <- args[sapply(args, function(arg) is.atomic(arg) || is.struct(arg))]
  
  if (length(args) == 0L) return(force)
  
  target <- args[[1]]

  # "parallel" args are the same length as the target arg
  args <- args[lengths(args) == length(target)]
  
  argnames <- names(args)
  
  bool <- if (allargs) Reduce('&', lapply(args, predicateFunc)) else predicateFunc(target)
  if (negate) bool <- !bool
  
  matchingArgs <- lapply(args, '[', i = bool)
  
  
  mapply(assign, argnames, matchingArgs, MoreArgs = list(envir = parent.frame())) 
  # THIS mapply DOES THE WORK OF SHRINKING THE ARGUMENTS IN THE PARENT FUNCTION
  
  function(result) {
    if (!(is.struct(result) || is.atomic(result)) || length(result) != sum(bool)) return(result)
    if (onlymatch) return (result)
    output <- if (inPlace && class(target) == class(result)) target else vectorNA(length(target), class(result))
    
    output[bool] <- result
    stickyAttrs(output) <- stickyAttrs(result)
    
    output
    
  }
}



###### "Memoify" ----

#' @export
memoizeDispatch <- function(fname) {
    func <- match.fun(fname)
    #argnames
    argnames <- names(fargs(func))
    
    if (length(argnames) == 0L) stop(call. = FALSE, "Can't memoizeDispatch a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "Can't memoizeDispatch a function if the first argument is ..." )
    argnames <- argnames[argnames != "..."]
    
    #
    fbody <- funcCall(fname)
    
    body <- rlang::quo({
        rebuild <- memoiseParse(argnames, !!!rlang::syms(argnames[argnames != '...']))
        result <- {!!fbody}
        rebuild(result)
    })
    body(func) <- rlang::quo_squash(body)
    environment(func) <- new.env(parent = environment(func))
    
    assign('argnames', argnames, envir = environment(func))
    
    func
    
}


memoizeDispatch.expr <- function(expr, argnames) {
  if (argnames[1] == '...') return(expr)
  
  argnames <- argnames[argnames != '...']
  args <- setNames(rlang::syms(argnames), argnames)
       
  
  rlang::expr({
    rememoise <- memoiseParse(!!!args)
    memoizeResult <- {!!expr}
    rememoise(memoizeResult)
  })
  
}

memoizeDispatch.quosure <- function(quosure, argnames) {
  rlang::quo_set_expr(quosure, memoizeDispatch.expr(rlang::quo_get_expr(quosure), argnames))
}

memoiseParse <- function(..., minN = 100L) {
    args <- list(...)
    
    if (is.null(names(args)) || any(names(args) == "")) .stop("memoiseParse requires that all arguments are named.")
    
    # only atomic/struct args that are longer than minN are affected
    args <- args[sapply(args, function(arg) (is.atomic(arg) || is.struct(arg)) && length(arg) >= minN)]
    
    if (length(args) == 0L) return(force)
    
    target <- args[[1]]
    
    # "parallel" args are the same length as the first arg, atomic or stuct, and longer than minN
    args <- args[lengths(args) == length(target)]
    argnames <- names(args)
   
    if (length(args) > 1L) target <- list2dt(args)  
    # target may now be a data.table of parallel arguments OR a single vector
    
    bool <- duplicated(target) 
    uniqueArgs <- lapply(args, '[', i = !bool) # always the original arguments (structs are not squashed)
    
    mapply(assign, argnames, uniqueArgs, MoreArgs = list(envir= parent.frame())) 
    # THIS mapply DOES THE WORK OF SHRINKING THE ARGUMENTS IN THE PARENT FUNCTION
    
    function(result) {
        if (is.table(result) || length(result) != sum(!bool)) return(result)
        
        uniqueVals <- target[!bool] # may or may not be data.table, with structs squashed
      
        if (is.data.table(target)) {
          uniqueVals$i <- seq_len(nrow(uniqueVals))
          
          result[merge(target, uniqueVals, on = colnames(uniqueVals), sort = FALSE)$i]
        } else {
          if (is.struct(target)) target <- as.atomic(target)
          if (is.struct(uniqueVals)) uniqueVals <- as.atomic(uniqueVals)
          result[match(target, uniqueVals)]
          
        }
        
    }
    
}



#### Humdrum Dispatch (exclusive dispatch AND regex dispatch) ----

exclusiveFunction <- function(...) {
    exprs <- rlang::exprs(...)
    
    body <- .exclusiveDispatch(exprs)
    
    arguments <- c(attr(body, 'arguments'))
    rlang::new_function(arguments, body, env = parent.env(environment()))
    
}

.exclusiveDispatch <- function(exprs) {
    rlang::expr({
        result <- .switch(x, Exclusive, !!!exprs, #inPlace = inPlace,  
                          parallel = list(Exclusive = Exclusive))
        if (inPlace) {
            result <- inPlace(result, x, regexes[Exclusive])
        } 
        
        result
    }) -> dispatchExpr
    
    dispatchExpr
    
}

regexGeneric <- function(...) {
    exprs <- rlang::exprs(...)
    
    body <- .regexGeneric(exprs)
    
    arguments <- attr(body, 'arguments')
    rlang::new_function(arguments, body, env = parent.frame())
    
}

.regexDispatch <- function(exprs) {
    rlang::expr({
        dispatchn <- regexFindMethod(x, c(regexes))
        switch(dispatchn,  
               !!! exprs )
    }) -> dispatchExpr
    
    
    dispatchExpr
    
}


# regexDispatch <- function(...) {
#   exprs <- rlang::enexprs(...)
#   
#   regexDispatch     <- .regexGeneric(exprs[!sapply(exprs, rlang::is_missing)])
#   
#   arguments <- attr(regexDispatch, 'arguments')
#   arguments <- arguments[!duplicated(names(arguments))]
#   arguments <- c(arguments[names(arguments) != '...'], arguments[names(arguments) == '...'])
#   
#   body <- rlang::expr({
#     !!regexDispatch 
#   })
#   
#   rlang::new_function(arguments, body)
# }

##### humdrum dispatch ----



humdrumDispatch <- function(..., doExclusiveDispatch = TRUE) {
    exprs <- rlang::enexprs(...)
    
    
    dispatchcode <- names(exprs)
    dispatchRE <- gsub('^[^ ]+(: )?', '', dispatchcode)
    dispatchExclusive <- gsub(': .*', '', dispatchcode)
    
    REexprs <- rlang::parse_exprs(dispatchRE)
    RElist <- lapply(dispatchExclusive, function(REname) rlang::expr(`$`(regexes, !!(rlang::sym(REname)))))
    
    
    arguments <- getAllArgs(exprs)
    exprs <- lapply(exprs, makeCall)
    exprs <- Map(REcall, RElist, exprs)
    
    names(exprs) <- names(REexprs) <- dispatchExclusive
    
    #
    regexDispatch     <- .regexDispatch(unname(exprs))
    exclusiveDispatch <- .exclusiveDispatch(exprs)
   
    ## 
    body <- if (doExclusiveDispatch) {
      rlang::expr({
        regexes <- list(!!!REexprs)
        
        if (missing(Exclusive) || is.null(Exclusive)) !!regexDispatch else  !!exclusiveDispatch
        
      })
      
    } else {
      rlang::expr({
        regexes <- list(!!!REexprs)
        
        !!regexDispatch
      })
    }
    
    
    # body <- predicateDispatch.expr('is.na', body, 'x', negate = TRUE)
    # body <- memoizeDispatch.expr(body, 'x')
    #
    arguments <- c(x = rlang::missing_arg(), 
                   if (doExclusiveDispatch) list(Exclusive = NULL),
                   arguments,
                   inPlace = FALSE)
    
    rlang::new_function(arguments, body)
    
    
    
}


getAllArgs <- function(exprs) {
    
    # arguments
    arguments <- lapply(exprs, callArgs)
    arguments <- unlist(unname(arguments), recursive = FALSE)
    arguments <- arguments[!duplicated(names(arguments))]
    arguments <- arguments[order(names(arguments) == '...')]
    arguments
}

callArgs <- function(call) {
    call <- if (is.call(call)) call[[1]] else call
    func <- rlang::eval_tidy(call)
    fargs <- fargs(func)
    fargs[-1]
    
}
makeCall <- function(expr, arg1 = rlang::sym('x')) {
    if (rlang::is_missing(expr)) return(expr)
    
    
    func <- rlang::eval_tidy(expr)
    fargs <- names(fargs(func))
                   
    args <- setNames(rlang::syms(fargs), fargs)
    args[[1]] <- arg1
    names(args)[names(args) == "..."] <- ""
    rlang::expr((!!expr)(!!!args))
    
}


REcall <- function(regex, expr) {
    if (rlang::is_missing(expr)) return(expr)
    fun <- expr[[1]]
    rlang::expr(REapply(!!expr[[2]], !!regex, !!fun, !!!as.list(expr[-1:-2]), inPlace = inPlace))
}

regexFindMethod <- function(str, regexes) {
    # this takes a str of character values and a string of REs
    # and returns a single interger value representing the 
    # index (of regexes) to dispatch.
    
    regexes <- getRE(regexes)
    
    Nmatches <- sapply(regexes, function(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
    if (!any(Nmatches > 0L)) return(0L)
    
    #which function to dispatch
    Ncharmatches <- sapply(regexes[Nmatches > 0],
                           function(re) {
                               nchars <- nchar(stringi::stri_extract_first_regex(str, re))
                               nchars[is.na(nchars)] <- 0L
                               sum(nchars)
                           })
    which(Nmatches > 0L)[which.max(Ncharmatches)]
}


partialApply <- function(func, ...) {
    fargs <- fargs(func)
    newargs <- list(...)
    
    hits <- names(fargs)[names(fargs) %in% names(newargs)]
    fargs[hits] <- newargs[hits]
    
    formals(func) <- fargs
    func
    
}

REcompose <- function(regex, func, ...) {
    funcname <- rlang::enexpr(func)
    
    fargs <- fargs(partialApply(func, ...))
    # regex <- getRE(regex)
    
    body <- rlang::expr({
        .REapply(!!rlang::sym(names(fargs)[1]), !!regex, !!funcname, !!!fargs[-1], inPlace = inPlace)
    })
    
    newenv <- new.env(parent.frame())
    assign(rlang::expr_text(funcname), func, newenv )
    
    rlang::new_function(c(fargs, inPlace = TRUE), body, newenv)    
}

