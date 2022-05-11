

#' @export
print.composed <- function(x) {
    checkArg(x)
    attributes(x) <- NULL
    print(x)
    
}

#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @export
`%.%` <- function(e1, e2) {
    checkArg(e1)
    checkArg(e2)
    f1name <- rlang::quo_text(rlang::enquo(e1))
    f2name <- rlang::quo_text(rlang::enquo(e2))
    
    fs <- setNames(c(e1, e2), c(f1name, f2name))
    do.call('compose', c(fs, list(fenv = parent.frame())))
    }

# Smart dispatch/function application ----


# "sticky attributes" 
#' @export
stickyApply <- function(func, ...) {
    checkArg(func)
    pipe <- stickyAttrs(list(...)[[1]])
    result <- func(...)
    stickyAttrs(result) <- pipe
    result
  }
  
  list(Restore = restorer, Args = args)
}

predicateParse <- function(predicateFunc, args, anyMatch = FALSE, 
                            dispatchArgs = c(), verbose = FALSE, ...) {
  
  firstArg <- args[[1]]
  
  if (length(firstArg) == 0L || !(is.atomic(firstArg) || is.struct(firstArg))) return(list(Restore = force, Args = args))
  
  # "target" args are the same length as the first arg, atomic or struct
  targets <- sapply(args,
                    \(arg) {
                      (is.atomic(arg) || is.struct(arg)) &&  length(arg) == length(firstArg)
                    })
  
  if (length(dispatchArgs) > 0L) targets <- targets & .names(args) %in% dispatchArgs
  
<<<<<<< HEAD
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
    if(!is.vector(vector) return(c("you did not enter a vector, you entered a ", class(vector))))
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
  checkArg(pat)
  func <- function(x) {
    match_size(pat = pat,x = x, toEnv = TRUE)
    ifelse(is.na(pat), is.na(x), x == pat)
  }


  new('predicate.function', func, string = glue::glue('x == {deparse(pat)}'))
}

#' @export
LEN <- function(p.f) {
    checkArg(p.f)
    func <- function(x) p.f(length(x))
 
    new('predicate.function', func,
        string = gsub('x', 'length(x)', p.f@string))
}

#' @export
ANY <- function(p.f) {
    checkArg(p.f)
    func <- unclass(any %.% p.f)
    
    new('predicate.function', func, string = paste0('any(', p.f@string, ')'))
}
#' @export
ALL <- function(p.f) {
    checkArg(p.f)
    func <- unclass(all %.% p.f)
    
    new('predicate.function', func, string = paste0('all(', p.f@string, ')'))
}

#' @export
GT <- function(n) {
  checkNumeric(n)
  func <- function(x) x > n

  new('predicate.function', func, string = glue::glue('x > {deparse(n)}'))
}

#' @export
GTET <- function(n) {
  checkNumeric(n)
  func <- function(x) x >= n

  new('predicate.function', func, string = glue::glue('x >= {deparse(n)}'))
}


#' @export
LT <- function(n) {
  checkNumeric(n)
  func <- function(x) x < n

  new('predicate.function', func, string = glue::glue('x < {deparse(n)}'))
}

#' @export
LTET <- function(n) {
  checkNumeric(n)
  func <- function(x) x <= n

  new('predicate.function', func, string = glue::glue('x <= {deparse(n)}'))
}

#' @export
RE <- function(pat) {
  checkArg(pat)
  func <- function(x)  grepl(pat, x) 

  new('predicate.function', func, string = glue::glue('x ~ {deparse(pat)}'))
}

#' @export
na <- new('predicate.function', function(x) is.na(x), string = "x == NA")
#' @export
notna <- new('predicate.function', function(x) !is.na(x), string = "x != NA")



############### Predicate dispatch ----

#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @name regexDispatch
#' @export
`%predate%` <- function(func, predicate) {
    checkArg(func)
    checkArg(predicate)
    predicateExpr <- rlang::expr_text(rlang::enexpr(predicate))
    if (grepl('function\\(', predicateExpr)) predicateExpr <- 'lambda'
    
    predicateDispatch(rlang::expr_text(rlang::enexpr(func)), predicate, predicateExpr)
}



#' @export
predicateDispatch <- function(func, predicateFunc, negate = FALSE) {
    
    checkArg(func)
  
    checkArg(predicateFunc)
  
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
=======
  if (!any(targets)) return(list(Restore = force, Args = args))
>>>>>>> master
  
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

memoizeParse <- function(args, dispatchArgs = c(), minMemoize = 100L, memoize = TRUE, verbose = FALSE, ...) {
  
<<<<<<< HEAD
  checkArg(args)
  
  if (is.null(names(args)) || any(names(args) == "")) .stop("predicateParse requires that all arguments are named.")
=======
  firstArg <- args[[1]]
>>>>>>> master
  
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
  names(memoizeArgs)[.names(memoizeArgs) == ""] <- cumsum(.names(memoizeArgs) == "")[.names(memoizeArgs) == ""]
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




<<<<<<< HEAD
#' @export
memoizeDispatch <- function(fname) {
    checkArg(fname)
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
=======
>>>>>>> master


do... <- function(func, args, envir = parent.frame()) {
  # calls func on args, even if some named arguments in args are not arguments of func
  # (ignores those arguments)
  if (!'...' %in% names(fargs(func))) formals(func) <- c(fargs(func), alist(... = ))
  
  do.call(func, args, envir = envir)
  
  
}

do <- function(func, args, doArgs = c(), memoize = TRUE, ..., ignoreUnknownArgs = TRUE, outputClass = class(args[[1]])) {
  firstArg <- args[[1]]
  if (is.vector(firstArg) && length(firstArg) == 0L) return(vectorNA(0L, outputClass))
  if (is.null(firstArg)) return(NULL)
  
  dimension <- dimParse(args)
  
  memoize <- memoizeParse(dimension$Args, dispatchArgs = doArgs, memoize = memoize, ...)
  
  naskip <- predicateParse(Negate(is.na), memoize$Args, dispatchArgs = doArgs, 
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

  checkArg(str, classes = c('string'))
  
  if (is.null(str)) return(NULL)
  if (length(str) == 0L && is.character(str)) return(vectorNA(0L, outputClass))
  if (!is.character(str)) .stop(if (hasArg('funcName')) "The function '{funcName}'" else "humdrumDispatch", "requires a character-vector 'str' argument.")
  
  dispatchDF$regex <- lapply(dispatchDF$regex, \(re) if (rlang::is_function(re)) re(...) else getRE(re))
  
  if (is.null(Exclusive)) Exclusive <- rep('any', length(str))
  if (length(Exclusive) < length(str)) Exclusive <- rep(Exclusive, length.out = length(str))
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
    args <- c(alist(str = , Exclusive = NULL), 
              args, 
              unlist(dispatchDF$Args, recursive = FALSE)[sharedArgNames], 
              alist(... = , multiDispatch = FALSE))
    args <- args[!duplicated(names(args))]
    args <- args[names(args) != 'x']
    args

  })
  dispatchArgs <- genericArgs[!names(genericArgs) %in% c('str', 'Exclusive', '...', 'multiDispatch')]
  dispatchArgs[names(dispatchArgs) %in% names(args)] <- lapply(names(dispatchArgs[names(dispatchArgs) %in% names(args)]),
                                                               rlang::sym)
  
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









