

######### Function composition ----

compose <- function(...) UseMethod('compose')
compose.default <- function(..., fenv = parent.frame()) {
# accepts a NAMED ... of functions
    fs <- rev(list(...))
    fnames <- names(fs)
    ### arguments
    fargs <- lapply(fs, fargs)
    fargs[-1] <- lapply(fargs[-1], 
                        \(farg) {
                           if (names(farg)[[1]] == '...') farg <- c(alist(tmp = ), farg)
                         farg })
    
    fargNames <- lapply(fargs, 
                        \(farg) {
                          names <- names(farg)
                          names(names) <- ifelse(names == '...', '', names)
                          rlang::syms(names)
                          })
    
    
    args <- do.call('c', c(fargs[1], 
                           lapply(fargs[-1], \(farg) farg[-1]),
                           use.names = FALSE))
    args <- args[!duplicated(names(args)) & names(args) != 'tmp']
    
    # firstArg <- rlang::sym(names(args)[[1]])
    pipeArgs <- rlang::syms(sapply(fargs, \(arg) names(arg)[[1]]))
    
    
    
    ### body
    body <- rlang::expr({
        !!!Map(\(bod, arg, argnames) {
            rlang::expr(!!arg <- stickyApply(!!bod, !!!argnames))
            }, 
            rlang::syms(head(fnames, -1)), 
            pipeArgs[-1], 
            head(fargNames, -1))
      
        
        # stickyApply(!!(rlang::sym(tail(fnames,1))), !!!fargNames[[length(fargNames)]])
        (!!tail(fnames,1))(!!!fargNames[[length(fargNames)]])
        })
    
    # body <- predicateDispatch.expr('is.na', body, names(args)[1], negate = TRUE)
    # body <- memoizeDispatch.expr(body, names(args))
    
    ### environment
    # fenv <- new.env() # parent.env(parent.frame())
    Map(\(fname, f) assign(fname, f, envir = fenv), 
        c(fnames, 'memoizeParse', 'predicateParse'), 
        c(fs, memoizeParse, predicateParse))
    
    
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

print.composed <- \(x) {
    attributes(x) <- NULL
    print(x)
    
}

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


stickyAttrs <- \(x) attr(x, 'sticky')

`stickyAttrs<-` <- function(x, value) {
    sticky <- stickyAttrs(x)
    sticky <- sticky[!names(sticky) %in% names(value)]
  
    attr(x, 'sticky') <- c(sticky, value)
    x
}

unstick <- \(x) {
    attr(x, 'sticky') <- NULL
    x
}

passargs <- \(x) {
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
         stringi::stri_replace_first(str = orig, replacement = result, regex = regex) %|% orig
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


re.place <- function(vector, reference = vector) {
    asfunc <- stickyAttrs(reference)$replace
    if (is.null(asfunc)) return(unstick(vector))
    
    asfunc(vector) %dim% vector
}


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
                         lapply(fargs[-1], \(farg) farg[-1]),
                         use.names = FALSE))
  args <- args[!duplicated(names(args)) & names(args) != 'tmp']
  
  # firstArg <- rlang::sym(names(args)[[1]])
  pipeArgs <- rlang::syms(sapply(fargs, \(arg) names(arg)[[1]]))
  
  
  
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
  
  # body <- predicateDispatch.expr('is.na', body, names(args)[1], negate = TRUE)
  # body <- memoizeDispatch.expr(body, names(args))
  
  ### environment
  # fenv <- new.env() # parent.env(parent.frame())
  Map(function(fname, f) assign(fname, f, envir = fenv), 
      c(fnames, 'memoizeParse', 'predicateParse'), 
      c(fs, memoizeParse, predicateParse))
  
  
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


print.composed <- function(x) {
  attributes(x) <- NULL
  print(x)
  
}


`%.%` <- function(e1, e2) { 
  f1name <- rlang::quo_text(rlang::enquo(e1))
  f2name <- rlang::quo_text(rlang::enquo(e2))
  
  fs <- setNames(c(e1, e2), c(f1name, f2name))
  do.call('compose', c(fs, list(fenv = parent.frame())))
}



# "sticky attributes" 


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
re.place <- function(vector, reference = vector) {
  asfunc <- stickyAttrs(reference)$replace
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
              func <- \(x) {
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
              func <- \(x) {
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
  func <- \(x) {
    match_size(pat = pat,x = x, toEnv = TRUE)
    is.na(x) %|% x == pat
  }


  new('predicate.function', func, string = glue::glue('x == {deparse(pat)}'))
}

#' @export
LEN <- function(p.f) {
    func <- \(x) p.f(length(x))
 
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
  func <- \(x) x > n

  new('predicate.function', func, string = glue::glue('x > {deparse(n)}'))
}

#' @export
GTET <- function(n) {
  func <- \(x) x >= n

  new('predicate.function', func, string = glue::glue('x >= {deparse(n)}'))
}


#' @export
LT <- function(n) {
  func <- \(x) x < n

  new('predicate.function', func, string = glue::glue('x < {deparse(n)}'))
}

#' @export
LTET <- function(n) {
  func <- \(x) x <= n

  new('predicate.function', func, string = glue::glue('x <= {deparse(n)}'))
}

#' @export
RE <- function(pat) {
  func <- \(x)  grepl(pat, x) 

  new('predicate.function', func, string = glue::glue('x ~ {deparse(pat)}'))
}

#' @export
na <- new('predicate.function', \(x) is.na(x), string = "x == NA")
#' @export
notna <- new('predicate.function', \(x) !is.na(x), string = "x != NA")



############### Predicate dispatch ----

`%predate%` <- function(func, predicate) {
    predicateExpr <- rlang::expr_text(rlang::enexpr(predicate))
    if (grepl('function\\(', predicateExpr)) predicateExpr <- 'lambda'
    
    predicateDispatch(rlang::expr_text(rlang::enexpr(func)), predicate, predicateExpr)
}



#' @export
predicateDispatch <- function(predicateFuncName, negate = FALSE, dispatchArgs, all = TRUE, func) {
  if (length(dispatchArgs) == 0L) .stop("predicateDispatch can't add a predicate to a function with no arguments." )
  if (dispatchArgs[1] == '...') .stop("predicateDispatch doesn't work if the first argument of the method is ..." )
 
  body(func) <- predicateDispatch.expr(predicateFuncName, dispatchArgs, body(func), negate = negate, all = all)
  func 
}

predicateDispatch.expr <- function(predicateFuncName, dispatchArgs, expr, negate = FALSE, all = TRUE) {
  
  args <- setNames(rlang::syms(dispatchArgs), dispatchArgs)
  
  predicateFuncName <- rlang::sym(predicateFuncName)
  
  rlang::expr({
    rebuild <- predicateParse(!!predicateFuncName, !!!args, negate = !!negate, all = !!all)
    predicateResult <- if (length(!!args[[1]]) > 0L)  {!!expr} else {!!args[[1]]}
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
predicateParse <- function(predicateFunc, ..., negate = FALSE, inPlace = TRUE, all = TRUE) {
  args <- list(...)
  
  if (is.null(names(args)) || any(names(args) == "")) .stop("predicateParse requires that all arguments are named.")
  
  # only atomic/struct args are affected
  args <- args[sapply(args, \(arg) is.atomic(arg) || is.struct(arg))]
  
  if (length(args) == 0L) return(force)
  
  target <- args[[1]]

  # "parallel" args are the same length as the target arg
  args <- args[lengths(args) == length(target)]
  
  argnames <- names(args)
  
  if (negate) predicateFunc <- Negate(predicateFunc)
  
  bool <- Reduce(if (all) '&' else '|', lapply(args, predicateFunc)) 
  
  matchingArgs <- lapply(args, '[', i = bool)
  
  
  mapply(assign, argnames, matchingArgs, MoreArgs = list(envir = parent.frame())) 
  # THIS mapply DOES THE WORK OF SHRINKING THE ARGUMENTS IN THE PARENT FUNCTION
  
  \(result) {
    if (!(is.struct(result) || is.atomic(result)) || length(result) != sum(bool)) return(result)
    
    output <- if (inPlace && class(target) == class(result)) target else vectorNA(length(target), class(result))
    
    output[bool] <- result
    stickyAttrs(output) <- stickyAttrs(result)
    
    output
    
  }
}

dimParse <- function(x) {
  name <- as.character(rlang::enexpr(x))
  olddim <- dim(x) 
  
  x <- dropdim(x)
  
  assign(name, value = x, envir = parent.frame())
  
  \(newx) {
    if (length(newx) == prod(olddim))  dim(newx) <- olddim 
    
    newx
    
  }
}

dimParse2 <- function(args) {
  firstArg <- args[[1]]
  if (!hasdim(firstArg)) return(list(Restore = force, Args = args))
  
  olddim <- dim(firstArg)
  
  args[[1]] <- dropdim(firstArg)
  
  restorer <- function(result) {
    if (length(result) == prod(olddim))  dim(result) <- olddim 
    result
  }
  
  list(Restore = restorer, Args = args)
}

predicateParse2 <- function(predicateFunc, args, anyMatch = FALSE, 
                            dispatchArgs = c(), verbose = FALSE, ...) {
  
  firstArg <- args[[1]]
  
  if (length(firstArg) == 0L || !(is.atomic(firstArg) || is.struct(firstArg))) return(list(Restore = force, Args = args))
  
  # "target" args are the same length as the first arg, atomic or struct
  targets <- sapply(args,
                    \(arg) {
                      (is.atomic(arg) || is.struct(arg)) &&  length(arg) == length(firstArg)
                    })
  
  if (length(dispatchArgs) > 0L) targets <- targets & .names(args) %in% dispatchArgs
  
  if (!any(targets)) return(list(Restore = force, Args = args))
  
  hits <- list2dt(lapply(args[targets], predicateFunc))
  hits <- Reduce(if (anyMatch) `|` else `&`, hits)
  
  if (verbose) cat('predicateParse has removed ', 
                   num2word(sum(!hits)), 
                   ' argument ', plural(sum(!hits), 
                                      "combinations which don't", 
                                      "combination which doesn't"), 
                   ' match the predicate ', list(...)$verboseMessage, '.\n', sep = '')
  
  args[targets] <- lapply(args[targets], '[', i = hits)
  
  
  restorer <- function(result) { 
    if (is.table(result) || !(is.struct(result) || is.atomic(result)) || length(result) != sum(hits)) return(result)
    
    output <- vectorNA(length(firstArg), class(result))
    
    output[hits] <- result
    output
  }
  
  
  list(Restore = restorer, Args = args)
}


###### "Memoify" ----

#' @export
memoizeDispatch <- function(dispatchArgs, func) {
  if (length(dispatchArgs) == 0L) return(func)
  if (dispatchArgs[1] == '...') .stop("Can't memoizeDispatch functions where the first argument is ..." )
  
  
  body(func) <- memoizeDispatch.expr(dispatchArgs, body(func))
  formals(func) <- c(fargs(func), alist(memoize = TRUE))
  func
  
  
}

memoizeDispatch.expr <- function(dispatchArgs, expr) {
  
  args <- setNames(rlang::syms(dispatchArgs), dispatchArgs)
       
  
  rlang::expr({
    rememoize <- memoizeParse(!!!args, memoize = memoize)
    memoizeResult <- {!!expr}
    rememoize(memoizeResult)
  })
  
}

memoizeDispatch.quosure <- function(quosure, dispatchArgs) {
  rlang::quo_set_expr(quosure, memoizeDispatch.expr(rlang::quo_get_expr(quosure), dispatchArgs))
}

memoizeParse <- function(..., minN = 100L, memoize = TRUE) {
    if (!memoize) return(force)
    args <- list(...)
    
    if (is.null(names(args)) || any(names(args) == "")) .stop("memoizeParse requires that all arguments are named.")
    
    # only atomic/struct args that are longer than minN are affected
    args <- args[sapply(args, \(arg) (is.atomic(arg) || is.struct(arg)) && length(arg) >= minN)]
    
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
    
    \(result) {
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
    minN
    minN
}

memoizeParse2 <- function(args, dispatchArgs = c(), minMemoize = 100L, memoize = TRUE, verbose = FALSE, ...) {
  
  firstArg <- args[[1]]
  
  if (length(firstArg) < minMemoize || !memoize || !(is.atomic(firstArg) || is.struct(firstArg))) return(list(Restore = force, Args = args))
  
  # "target" args are the same length as the first arg, atomic or struct, and longer than minN
  targets <- sapply(args,
                    \(arg) {
                      (is.atomic(arg) || is.struct(arg)) && 
                        length(arg) >= minMemoize &&
                        length(arg) == length(firstArg)
                      })
  
  if (length(dispatchArgs) > 0L) targets <- targets & .names(args) %in% dispatchArgs
  
  if (!any(targets)) return(list(Restore = force, Args = args))
  
  memoizeArgs <- list2dt(args[targets])
  names(memoizeArgs)[.names(memoizeArgs) == ""] <- cumsum(.names(memoizeArgs) == "")
  duplicates <- duplicated(memoizeArgs) 
  
  if (verbose) cat('memoizeParse has removed', 
                   num2word(sum(duplicates)), 
                   'duplicate argument', plural(sum(duplicates), 'combinations.', 'combination.'), '\n')
  
  args[targets] <- lapply(args[targets], '[', i = !duplicates)
  
  uniqueArgs <- memoizeArgs[!duplicates]
  uniqueArgs$i <- seq_len(nrow(uniqueArgs))
  
  
  restorer <- function(result) {
    if (is.table(result) || length(result) != sum(!duplicates)) return(result)
    
    result[merge(memoizeArgs, uniqueArgs, on = colnames(uniqueArgs), sort = FALSE)$i]
    
  }
  
  list(Restore = restorer, Args = args)
}


#### Humdrum Dispatch (exclusive dispatch AND regex dispatch) ----

exclusiveFunction <- function(...) {
    exprs <- rlang::exprs(...)
    
    body <- .exclusiveDispatch(exprs)
    
    arguments <- c(attr(body, 'arguments'))
    rlang::new_function(arguments, body, env = parent.env(environment()))
    
}

.exclusiveDispatch <- function(exprs, defaultClass = 'character') {
    rlang::expr({
        result <- .switch(x, Exclusive, !!!exprs, #inPlace = inPlace,  
                          parallel = list(Exclusive = Exclusive),
                          defaultClass = !!defaultClass)
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

.regexDispatch <- function(exprs, defaultClass = 'character') {
    rlang::expr({
        dispatchn <- regexFindMethod(x, c(regexes))
        if (dispatchn == 0L) return(vectorNA(length(str), !!defaultClass))
        switch(dispatchn,  
               !!! exprs )
    }) -> dispatchExpr
    
    
    dispatchExpr
    
}


smartDispatch <- function(dispatchArgs, func) {
  predicateDispatch('is.na', TRUE, dispatchArgs, all = TRUE,
                    memoizeDispatch(dispatchArgs, func))
}

do... <- function(func, args) {
  # calls func on args, even if some named arguments in args are not arguments of func
  # (ignores those arguments)
  if (!'...' %in% names(fargs(func))) formals(func) <- c(fargs(func), alist(... = ))
  
  do.call(func, args)
  
  
}

do <- function(func, args, doArgs = c(), ..., ignoreUnknownArgs = TRUE, outputClass = class(args[[1]])) {
  firstArg <- args[[1]]
  if (is.vector(firstArg) && length(firstArg) == 0L) return(vectorNA(0L, outputClass))
  if (is.null(firstArg)) return(NULL)
  
  dimension <- dimParse2(args)
  
  memoize <- memoizeParse2(dimension$Args, dispatchArgs = doArgs, ...)
  
  naskip <- predicateParse2(Negate(is.na), memoize$Args, dispatchArgs = doArgs, 
                            verboseMessage = '"not NA"', ...)
  
  
  result <- if (ignoreUnknownArgs) do...(func, naskip$Args) else do.call(func, naskip$Args)
  humattr <- humdrumRattr(result)
  
  result <- dimension$Restore(memoize$Restore(naskip$Restore(result)))
  humdrumRattr(result) <- humattr
  
  result
  
  
  
}

`%do%` <- function(e1, e2) do(e1, e2)

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



# Humdrum dispatch ----

#' Regular expression method dispatch and function application
#' 
#' The [humdrumR] **regular-expression method dispatch system**
#' is a simple system for making new functions which can by smartly
#' applied to a variety of character strings.
#' Humdrum dispatch works like normal R method dispatch, but instead of dispatching specific methods
#' based on their class (`integer`, `character`, etc.) it dispatches based on regular expressions.
#' In addition, exclusive interpretations can be used to guide dispatch.
#' 
#' Many `humdrumR` functions are in fact, humdrum-dispatch functions: for example, [tonalInterval.character()].
#' If you call `tonalInterval('ee-')`, the function will recognize that the input string is a token in the  `**kern`
#' representation, and call the appropriate parser.
#' If you instead call `tonalInterval('me')`, the function will recognize that the input string is a token in the `**solfa`
#' representation, and call the appropriate parser for that.
#' 
#' 
#' ### dispatchDF
#' 
#' The `dispatchDF` must be a [data.table::data.table()] created using the `makeDispatchDF` function.
#' `makeDispatchDF` takes one or more arguments, each a list with three components (ordered, not nameed):
#' 
#' 1. A character vector of exclusive interpretations. (Specify `"any"` if you don't want exclusive dispatch).
#' 2. A regular expression (character string) or a function which can generate a regular expression, which accepts `...`
#'   arguments at the time of dispatch.
#' 3. A function to dispatch.
#' 
#' 
#' @param str The input `character` string, on which dispatch is called.
#' @param dispatchDF A data.frame which describes what function should be called for 
#'        which regex input. (See details).
#' @param Exclusive Defaults to `NULL`. If `NULL`, only the regexes are used for dispatch.
#' @param ... Arguments to pass to dispatch functions.
#' @param multiDispatch `logical`, length 1. If `FALSE` (the default) the "best" regex/exclusive match
#'      is dispatched for each Exclusive segment. If `TRUE`, differenet functions can be dispatched
#'      within the same input vector. 
#' @param outputClass Character string: the default output class which the function should return.
#'        
#'        
#' 
#' 
#' Generally, to make sense, all dispatched functions should return the same type, which you should explicitly 
#' indicate with the `outputClass` argument.
#' Dispatch functions should also be [vectorized][base::Vectorize()].
#' 
#' @section makeHumdrumDispatcher:
#' 
#' `makeHumdrumDispatcher` is a function which creates a new function which automatically performs humdrum-dispatch.
#' A number of important `humdrumR` functions are created with `makeHumdrumDispatcher`:
#' 
#' + `tonalInterval.character`
#' + `diatonicSet.character`
#' + `tertianSet.character`
#' + `rhythmInterval.character`
#' 
#' @examples 
#' 
#' u <- c('A', 'B', 'CD', 'E', 'F', 'gh', 'L', 'KX')
#' l <- c('a', 'b', 'cd', 'e', 'f', 'gh', 'l', 'kx')
#' 
#' lowercasefunc <- \(x) 5L - nchar(x)
#' 
#' humdrumDispatch(l, outputClass = 'integer',
#'                 makeDispatchDF(list('any', '[a-z]+',  lowercasefunc),
#'                                list('any', '[A-Z]+',  nchar)))
#'  # lowercasefunc will be called on l, nchar on u
#' 
#' @name humdrumDispatch
#' @export
humdrumDispatch <-  function(str, dispatchDF,  Exclusive = NULL, 
                             multiDispatch = FALSE, ..., outputClass = 'character') {
  if (is.null(str)) return(NULL)
  if (length(str) == 0L && is.character(str)) return(vectorNA(0L, outputClass))
  if (!is.character(str)) .stop(if (hasArg('funcName')) "The function '{funcName}'" else "humdrumDispatch", "requires a character-vector 'str' argument.")
  
  dispatchDF$regex <- lapply(dispatchDF$regex, \(re) if (rlang::is_function(re)) re(...) else getRE(re))
  
  if (is.null(Exclusive)) Exclusive <- rep('any', length(str))
  
  ### find places where str matches dispatch regex AND Exclusive matches dispatch exclusives
  matches <-  Map(\(regex, exclusives) {
    output <- character(length(str))
    target <- 'any' %in% exclusives | Exclusive == "any" | Exclusive %in% exclusives 
    
    output[target] <- stringi::stri_extract_first_regex(str[target], regex)
    output
  },
  dispatchDF$regex, dispatchDF$Exclusives)
  
  Lmatches <- as.data.frame(lapply(matches, \(m) nchar(m) %|% 0L))
  
  if (multiDispatch) {
    dispatch <- apply(Lmatches, 1L,\(row) {
      dispatch <- which.max(row)
      if (length(dispatch) == 0L || row[dispatch] == 0L) 0L else dispatch 
    })
    segments <- segments(dispatch)
    dispatch <- attr(segments, 'values')
    
  } else {
    segments <- segments(Exclusive)
    Mmatches <- as.data.frame(do.call('rbind', by(Lmatches, segments, colMeans, na.rm = TRUE)))
    
    #### Pick which dispatch is best
    dispatch <- apply(Mmatches, 1, \(row) {
      dispatch <- which.max(row)
      if (length(dispatch) == 0L || row[dispatch] == 0L) 0L else dispatch 
    })
  }
  
  ### Extract matching vectors
  matches <- Map(\(match, disp) if (disp > 0L) match[ , disp] else vectorNA(nrow(match), outputClass), 
                 split(as.data.frame(matches), segments), 
                 dispatch)
  
  ### call methods
  result <- Map(\(method, strs) {
    na <- is.na(strs)
    output <- vectorNA(length(strs), outputClass)
    args <- c(list(strs), list(...))
    if (any(na)) args <- lapply(args, \(arg) if (length(arg) == length(na)) arg[!na] else arg)
    
    output[!na] <- do...(method, args)
    output
  },
  c(list(force), dispatchDF$method)[dispatch + 1L], matches)
  
  result <- do.call('c', result)
  
  attr(result, 'dispatch') <-  list(Original = str, 
                                    Regexes = unlist(dispatchDF$regex[dispatch]),
                                    Segments = segments,
                                    Exclusives = sapply(dispatchDF$Exclusives, '[', 1)[dispatch])
  result
}


humdrumRattr <- function(x) {
  known <- c('dispatch', 'dispatched')
  attr <- attributes(x)
  
  attr[names(attr) %in% known]
}
`humdrumRattr<-` <- function(x, value) {
  
  for (attrname in names(value)) attr(x, attrname) <- value[[attrname]]
  x
}

rePlace <- function(result, dispatched = attr(result, 'dispatched')) {
  if (is.null(dispatched) || length(result) != length(dispatched$Original) || !is.atomic(result)) return(result)
  names <- names(result)
  
  result <- unlist(Map(stringi::stri_replace_first_regex, split(dispatched$Original, dispatched$Segments), dispatched$Regexes, split(result, dispatched$Segments)))
  names(result) <- names
  result
}

reParse <- function(result, dispatched = attr(result, 'dispatched'), reParsers) {
  if (is.null(dispatched) || length(result) != length(dispatched$Original) || is.character(result)) return(result)
  names <- names(result)
  exclusives <- dispatched$Exclusives
  
  result <- split(result, dispatched$Segments)
  
  result <- unlist(Map(\(res, excl) {
    reParser <- match.fun(if (excl %in% reParsers)  excl else reParsers[1])
    reParser(res, inPlace = FALSE)
  }, result, exclusives))
  
  names(result) <- names
  
  result
  
}
  


#' @rdname humdrumDispatch
#' @export
makeDispatchDF <- function(...) {
  quoted <- rlang::enexprs(...) 
  
  if (length(quoted) == 0L) .stop("You can't make a dispatchDF with zero dispatch options!")
  
  
  dispatchDF <- as.data.table(do.call('rbind', list(...)))
  colnames(dispatchDF) <- c('Exclusives', 'regex', 'method')
  
  dispatchDF$regexPrint <- sapply(quoted, \(row) as.character(row[[3]])[1])
  dispatchDF[ , regexPrint := unlist(Map(\(regex, print) if (rlang::is_function(regex)) paste0(print, '(...)') else print, regex, regexPrint))]
  
  dispatchDF$methodPrint <- sapply(quoted, \(row) as.character(row[[4]])[1])
  
  dispatchDF$Args <- lapply(dispatchDF$method, fargs)
  dispatchDF
}

#' @rdname humdrumDispatch
#' @export
makeHumdrumDispatcher <- function(..., funcName = 'humdrum-dispatch', outputClass = 'character', args = alist()) {

  dispatchDF <- makeDispatchDF(...)
                       
  
  # Assemble the new function's arguments
  genericArgs <- local({
    sharedArgNames <- Reduce('intersect', lapply(dispatchDF$Args, names))
    args <- c(alist(str = , Exclusive = NULL, ... = , multiDispatch = FALSE), args, unlist(dispatchDF$Args)[sharedArgNames])
    args <- args[!duplicated(names(args))]
    args <- args[names(args) != 'x']
    args

  })
  
  dispatchArgs <- genericArgs[!names(genericArgs) %in% c('str', 'Exclusive', '...', 'multiDispatch') ]
  
  
  ##################################################### #
  
  body <- rlang::expr({
    args <- list(str = str, dispatchDF = dispatchDF, Exclusive = Exclusive, 
                 !!!dispatchArgs, multiDispatch = multiDispatch,
                 outputClass = !!outputClass, funcName = !!funcName,
                 ...)
    do(humdrumDispatch, args, ...)
    
  })
  
  genericFunc <- rlang::new_function(genericArgs, body, 
                                     rlang::new_environment(list(dispatchDF = dispatchDF, dispatchArgs = dispatchArgs), parent = parent.frame()))
  
  attr(genericFunc, 'dispatch') <- dispatchDF
  genericFunc %class% 'humdrumDispatch'
  
  
}

#' @rdname humdrumDispatch
#' @export
print.humdrumDispatch <- function(x) {
  dispatchDF <- attr(x, 'dispatch')
  
  call <- gsub(' NULL$', '', deparse(call('function', fargs(x))))
  cat('humdrum-dispatch', call, '\n')
  
  exclusives <- sapply(dispatchDF$Exclusives, \(exc) paste(paste0('**', exc), collapse = '|'))
  exclusives <- if (all(exclusives == '**any')) "    " else paste0('    ', exclusives, ' :: ')
  exclusives <- stringr::str_pad(exclusives, max(nchar(exclusives)), side = 'left')
  
  regexes <- dispatchDF$regexPrint %|% ''
  
  dispatchDF$methodPrint <- stringr::str_pad(dispatchDF$methodPrint, max(nchar(dispatchDF$methodPrint)), side = 'left')
  
  dispatchcalls <- Map(\(call, args, regex) {
    argnames <- names(args)
    argnames[1] <- paste0(argnames[1], if (regex != '') paste0(' %~m% ', regex))
    if (length(argnames) > 1L ) argnames[1] <- stringr::str_pad(argnames[1], max(nchar(regexes)) + 10L, side = 'right')
    paste0(call, '(', paste(argnames, collapse = ', '), ')')
    
    }, dispatchDF$methodPrint, dispatchDF$Args, regexes)
  
  
  cat(paste0(exclusives, dispatchcalls), sep = '\n')
  
  cat(stringr::str_pad('(See ?humdrumDispatch for info on how this works!)', options('width')$width - 1, side = 'left'), '\n')
  
  
  # cat(deparse(body(x)), sep='\n')
  

  
  
  
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
    
    Nmatches <- sapply(regexes, \(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
    if (!any(Nmatches > 0L)) return(0L)
    
    #which function to dispatch
    Ncharmatches <- sapply(regexes[Nmatches > 0],
                           \(re) {
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

