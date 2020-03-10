######### Function composition ----


compose <- function(...) UseMethod('compose')
compose.default <- function(...) {
# accepts a NAMED list of functions.
    fs <- rev(list(...))
    fnames <- names(fs)
    
    ### arguments
    fargs <- lapply(fs, fargs)
    fargs[-1] <- lapply(fargs[-1], 
                        function(farg) {
                           if (names(farg)[[1]] == '...') farg <- c(alist(tmp = ), farg)
                         farg })
    
    args <- do.call('c', c(fargs[1], 
                           lapply(fargs[-1], function(farg) farg[-1]),
                           use.names = FALSE))
    args <- args[!duplicated(names(args)) & names(args) != 'tmp']
    
    # firstArg <- rlang::sym(names(args)[[1]])
    pipeArgs <- rlang::syms(sapply(fargs, function(arg) names(arg)[[1]]))
    
    ### body
    body <- rlang::expr({
        !!!Map(function(bod, arg, parg) {
            rlang::expr(!!arg <- stickyApply(!!bod, !!!rlang::syms(names(parg))))
            }, 
            rlang::syms(head(fnames, -1)), 
            pipeArgs[-1], 
            head(fargs, -1))
        
        (!!tail(fnames,1))(!!!rlang::syms(names(fargs[[length(fargs)]])))
        
        })
    
    ### environment
    fenv <- new.env()
    Map(function(fname, f) assign(fname, f, envir = fenv), fnames, fs)
    
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
    do.call('compose', fs)
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
    attr(x, 'sticky') <- c(stickyAttrs(x), value)
    x
}

unstick <- function(x) {
    attr(x, 'sticky') <- NULL
    x
}
    

##### "restoring" ----

`%re.as%` <- function(e1, e2) {
    stickyAttrs(e1) <- list(as = e2)
    e1
}

`%re.place%` <- function(e1, e2) {
    stickyAttrs(e1) <- list(replace = e2)
    e1
}

re.as <- function(vector) {
    asfunc <- stickyAttrs(vector)$as
    if (is.null(asfunc)) return(vector)
    
    asfunc <- match.fun(paste0('as.', asfunc))
    stickyApply(asfunc, vector)
}

re.place <- function(vector) {
    asfunc <- stickyAttrs(vector)$replace
    if (is.null(asfunc)) return(unstick(vector))
    
    asfunc <- match.fun(asfunc)
    asfunc(vector)
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

LEN <- function(p.f) {
    func <- function(x) p.f(length(x))
 
    new('predicate.function', func,
        string = gsub('x', 'length(x)', p.f@string))
}

ANY <- function(p.f) {
    func <- unclass(any %.% p.f)
    
    new('predicate.function', func, string = paste0('any(', p.f@string, ')'))
}
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

na <- new('predicate.function', function(x) is.na(x), string = "NA")



############### Predicate dispatch ----

#' @name regexDispatch
#' @export
`%predate%` <- function(func, predicate) {
    predicateExpr <- rlang::expr_text(rlang::enexpr(predicate))
    if (grepl('function\\(', predicateExpr)) predicateExpr <- 'lambda'
    
    predicateDispatch(rlang::expr_text(rlang::enexpr(func)), predicate, predicateExpr)
}






predicateDispatch <- function(fname, predicateFunc, predicateName) {
    func <- match.fun(fname)
    #argnames
    fargs <- fargs(func)
    argnames <- names(fargs)
    
    if (length(argnames) == 0L) stop(call. = FALSE, "predicateDispatch (%predicate%) can't add a predicate to a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "predicateDispatch (%predicate%) doesn't if the first argument of the method is ..." )
    argnames <- argnames[argnames != "..."]
    
    #
    fbody <- funcCall(fname)
    
    body <- rlang::expr({
        rebuild <- predicateParse(!!rlang::sym(predicateName), argnames, inPlace,
                                  !!!rlang::syms(argnames[argnames != '...']))
        result <- {!!fbody}
        rebuild(result)
    })
    fargs[['inPlace']] <- TRUE
    newenv <- new.env(parent = parent.frame())
    
    assign(predicateName, predicateFunc, envir = newenv)
    assign('argnames', argnames, envir = newenv)
    
    rlang::new_function(fargs, body, newenv)
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



predicateParse <- function(predicate, argnames, inPlace, ...) {
    args <- setNames(list(...), argnames)
    
    lengths <- lengths(args)
    targets <- args[lengths == lengths[1]]
    bool <- apply(sapply(targets, predicate), 1, any)
    list2env(lapply(targets, '[', i = bool), 
             envir = parent.frame())
    
    output <- if (inPlace) args[[1]] else vectorna(length(args[[1]]), class(args[[1]]))
    
    function(result) {
        if (length(result) != sum(bool, na.rm = TRUE)) return(if (inPlace) result else output)
        output[bool] <- result
        output
    }
}


###### "Memoify" ----

memoify <- function(fname) {
    func <- match.fun(fname)
    #argnames
    argnames <- names(fargs(func))
    
    if (length(argnames) == 0L) stop(call. = FALSE, "Can't memoify a function with no arguments." )
    if (argnames[1] == '...') stop(call. = FALSE, "Can't memoify a function if the first argument is ..." )
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

memoiseParse <- function(argnames, ...) {
    args <- setNames(list(...), argnames)
    
    target <- args[[1]]
    
    bool <- duplicated(target)
    
    uniq <- target[!bool]
    
    assign(argnames[1], uniq, parent.frame())
    
    matrix <- vapply(uniq, function(x) x == target, FUN.VALUE = integer(length(target)))
    i <- rowSums(matrix * col(matrix))
    
    function(result) {
        if (length(result) != ncol(matrix)) return(result)

        result[i]
    }
    
    
}
