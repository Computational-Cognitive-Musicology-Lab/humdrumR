



# Smart dispatch/function application ----



dimParse <- function(args) {
  firstArg <- args[[1]]
  # if (!hasdim(firstArg)) return(list(Restore = force, Args = args))
  
  
  if (hasdim(firstArg)) {
    names <- dimnames(firstArg) 
    olddim <- dim(firstArg)
    } else {
    names <- names(firstArg)
    olddim <- length(firstArg)
    }
  
  args[[1]] <- dropdim(firstArg)
  
  restorer <- function(result) {
    if (length(result) == prod(olddim)) {
      if (hasdim(firstArg) )  {
        dim(result) <- olddim 
        dimnames(result) <- names
      } else {
        names(result) <- names
      }
      
    }
    result
    
  }
  
  list(Restore = restorer, Args = args)
}

predicateParse <- function(predicateFunc, args, anyMatch = NULL, 
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
  
  hits <- if (is.null(anyMatch)) {
    predicateFunc(firstArg)
  } else {
    hits <- list2dt(lapply(args[targets], predicateFunc))
    hits <- Reduce(if (anyMatch) `|` else `&`, hits)
  }

  
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

memoizeParse <- function(args, dispatchArgs = c(), minMemoize = 100L, memoize = TRUE, verbose = FALSE, lag = 0L, ...) {
  
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
  
  if (lag != 0L) {
    args <- c(args, Lagged = list(lag(firstArg, lag)))
    targets <- c(targets, TRUE)
  }
  
  memoizeArgs <- list2dt(args[targets])
  names(memoizeArgs)[.names(memoizeArgs) == ""] <- cumsum(.names(memoizeArgs) == "")[.names(memoizeArgs) == ""]
  duplicates <- duplicated(memoizeArgs) 
  
  if (verbose) cat('memoizeParse has removed', 
                   num2word(sum(duplicates)), 
                   'duplicate argument', plural(sum(duplicates), 'combinations.', 'combination.'), '\n')
  
  args[targets] <- lapply(args[targets], '[', i = !duplicates)
  
  uniqueArgs <- memoizeArgs[!duplicates]
  
  if (lag != 0L) {
    args[[1]] <- c(rbind(args$Lagged, args[[1]]))
    args$Lagged <- NULL
    targets <- head(tail(which(targets), -1L), -1L)
    args[targets] <- lapply(args[targets], rep, each = 2)
  }
  
  restorer <- function(result) {
    if (lag) result <- result[seq_along(result) %% 2L == 0L]
    
    if (is.table(result) || length(result) != sum(!duplicates)) return(result)
    
    if (is.struct(result)) {
      uniqueArgs$i <- seq_len(nrow(uniqueArgs))
      result[merge(memoizeArgs, uniqueArgs, on = colnames(uniqueArgs), sort = FALSE)$i]
      
    } else {
      uniqueArgs[ , Result := result]
      merge(memoizeArgs, uniqueArgs, on = head(colnames(uniqueArgs), -1), sort = FALSE)$Result
      
    }
    
  }
  
  list(Restore = restorer, Args = args)
}






do... <- function(func, args = list(), ..., envir = parent.frame()) {
  # calls func on args, even if some named arguments in args are not arguments of func
  # (ignores those arguments)
  
  args <- c(args, list(...))
  if (!'...' %in% names(fargs(func))) formals(func) <- c(fargs(func), alist(... = ))
  
  do.call(func, args, envir = envir)
  
  
}



do <- function(func, args, doArgs = c(), memoize = TRUE, ..., ignoreUnknownArgs = TRUE, outputClass = class(args[[1]])) {
  firstArg <- args[[1]]
  
  if (is.vector(firstArg) && length(firstArg) == 0L) return(vectorNA(0L, outputClass))
  if (is.null(firstArg)) return(NULL)
  
  Exclusive <- attr(firstArg, 'Exclusive')
  
  dimension <- dimParse(args)
  
  memoize <- memoizeParse(dimension$Args, dispatchArgs = doArgs, memoize = memoize, ...)
  
  naskip <- predicateParse(Negate(is.na), memoize$Args, dispatchArgs = doArgs, 
                            verboseMessage = '"not NA"', ...)
  
  
  attr(naskip$Args[[1]], 'Exclusive') <- Exclusive
  
  result <- if (length(naskip$Args[[1]])) {
    if (ignoreUnknownArgs) do...(func, naskip$Args) else do.call(func, naskip$Args)
  } else {
    vectorNA(0L, mode = outputClass)
  }
  
  humattr <- humdrumRattr(result)
  
  result <- dimension$Restore(memoize$Restore(naskip$Restore(result)))
  if ('dispatch' %in% names(humattr)) humattr$dispatch$Original <- firstArg
  humdrumRattr(result) <- humattr
  
  result
  
  
  
}

dofunc <- function(doArgs = c(), .func) {
  formals <- formals(.func)
  args <- setNames(rlang::syms(names(formals)), names(formals))
  rlang::new_function(formals, 
                      rlang::expr({
                        do(.func, args = list(!!!args), doArgs = !!doArgs)
                      }))
  

}

`%do%` <- function(e1, e2) do(e1, e2)


partialApply <- function(func, ...) {
  
  fcall <- rlang::enexpr(func)
  fargs <- fargs(func)
  pargs <- rlang::enexprs(...)
  
  fargNames <- names(fargs)
  pargNames <- .names(pargs)
  
  ldots <- any(fargNames == "...")
               
  # partial matching
  hits <- pmatch(pargNames, fargNames, nomatch = 0L)
  pargNames[hits > 0L] <- fargNames[hits[hits > 0L]]
  
  namedPargs <- pargs[pargNames != ""]
  unnamedPargs <- pargs[pargNames == ""]
  
  # explicit partial args
  
  if (any(fargNames == '...')) {
    fargs[names(namedPargs)] <- namedPargs
  } else {
    hits <- names(namedPargs) %in% fargNames
    fargs[names(namedPargs[hits])] <- namedPargs[hits]
  }
  
  # other (positional) partial args
  unfilledPositions <- !names(fargs) %in% .names(namedPargs) & names(fargs) != '...'
  
  positionalN <- min(length(unnamedPargs), sum(unfilledPositions))
  
  fargs[fargNames[head(which(unfilledPositions), positionalN)]] <- head(unnamedPargs, positionalN)
  
  ### reorder args
  missing <- sapply(fargs, rlang::is_missing)
  
 if (ldots) {
   
   notmissing <- fargs[!missing  & .names(fargs) != '...']
   passed <- fargs[.names(fargs) != '...']
   passed <- setNames(rlang::syms(names(passed)), names(passed))
   fargs <- fargs#[missing ]
   body <- rlang::expr({
     
     passedArgs <- list(!!!passed)
     curArgs <- list(...)
     
     args <- c(passedArgs, curArgs)
     args <- args[!duplicated(names(args)) | .names(args) == ''] #curArgs (ldots) may need to overwrite default args
     
     do.call(!!(as.character(fcall)), args)
     
   })
   
 } else {
   fargs <- fargs[order(missing, decreasing = TRUE)]
   passed <- setNames(rlang::syms(names(fargs)), names(fargs)) 
   body <- rlang::expr({
     (!!fcall)(!!!passed)
     
   })
 } 

  
  
  rlang::new_function(fargs, body) %class% "partiallyApplied"
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
humdrumDispatch <-  function(x, dispatchDF,  Exclusive = NULL, funcName = NULL, 
                             regexApply = TRUE,
                             multiDispatch = FALSE, ..., outputClass = 'character') {
  if (is.null(x)) return(NULL)
  if (length(x) == 0L || (is.null(Exclusive) && !is.character(x))) return(vectorNA(0L, outputClass))
  
  if (!is.null(Exclusive)) {
    exclusiveDispatch(x, dispatchDF, regexApply = regexApply,
                      Exclusive = Exclusive, outputClass = outputClass,
                      ...)
  } else {
    regexDispatch(x, dispatchDF, multiDispatch = multiDispatch,
                  funcName = funcName, outputClass = outputClass, ...)
  }
  
}



regexDispatch <- function(str, dispatchDF, multiDispatch = FALSE, outputClass = 'character', ..., funcName = 'regexDispatch') {
  if (!is.character(str)) .stop("The function '{funcName %||% humdrumDispatch}' requires a character-vector 'x' argument.")
  
  dispatchDF$regex <- lapply(dispatchDF$regex, \(re) if (rlang::is_function(re)) re(...) else getRE(re))
  
  matches <- do.call('cbind', lapply(dispatchDF$regex, \(re) stringi::stri_extract_first_regex(str, pattern = re)))
  
  Lmatches <- nchar(matches) %|% 0L
  
  if (!multiDispatch) Lmatches <- rbind(colMeans(Lmatches))
  
  matches <- cbind(matches, NA_character_)
  j <- ncol(matches)
  dispatch <- apply(Lmatches, 1, \(row) {
      dispatch <- which.max(row)
      if (length(dispatch) == 0L || row[dispatch] == 0L) j else dispatch 
    })
  
  ### Extract matching vectors
  matches <- if (length(matches) > 1L) matches[cbind(1:nrow(matches), dispatch)] else matches[ , dispatch]
                 
  ### call methods
  result <- if (length(dispatch) == 1L) {
    do(dispatchDF$method[[dispatch]], c(list(matches), list(...)))
  } else {
    i <- tapply(seq_along(matches), dispatch, list)
    
    do.call('c', Map(\(method, strs) {
      do...(method, c(list(strs), list(...)))
    }, c(dispatchDF$method, list(force))[sort(unique(dispatch))], tapply(matches, dispatch, list)))[order(unlist(i))]
  }
  

  
  attr(result, 'dispatch') <-  list(Original = str, 
                                    Regexes = unlist(dispatchDF$regex[unique(dispatch)]),
                                    Segments = dispatch,
                                    Exclusives = sapply(dispatchDF$Exclusives, '[', 1)[unique(dispatch)])
  result
}

#' @rdname humdrumDispatch
#' @export
exclusiveDispatch <- function(x, dispatchDF, Exclusive, regexApply = TRUE, outputClass = 'character', inPlace = FALSE, ...) {
  if (!is.null(attr(x, 'Exclusive'))) Exclusive <- attr(x, 'Exclusive')(Exclusive)
  if (is.null(Exclusive)) Exclusive <- dispatchDF$Exclusives[[1]][1]
  if (length(Exclusive) < length(x)) Exclusive <- rep(Exclusive, length.out = length(x))
  
  Exclusive <- stringr::str_remove(Exclusive, '^\\*{1,2}')
  
  dispatchDF <- dispatchDF[sapply(dispatchDF$Exclusives, \(exc) any(Exclusive %in% exc)), ]
  
  

  dispatchDF$regex <- lapply(dispatchDF$regex, \(re) if (rlang::is_function(re)) re(...) else getRE(re))
  
  result <- vectorNA(length(x), outputClass)
  exclusives <- c()
  
  regexApply <- regexApply && is.character(x)
  if (nrow(dispatchDF)) {
    for (i in 1:nrow(dispatchDF)) {
      hits <- Exclusive %in% dispatchDF$Exclusives[[i]]
      args <- c(list(x), list(...))
      args[lengths(args) == length(args[[1]])] <- lapply(args[lengths(args) == length(args[[1]])], '[', i = hits)
      
      result[hits] <- if (regexApply && !is.na(dispatchDF$regex[[i]])) {
        REapply(args[[1]], dispatchDF$regex[[i]], dispatchDF$method[[i]], inPlace = inPlace, args = args[-1], outputClass = outputClass) 
        } else {
         do...(dispatchDF$method[[i]], args)
      }
      
      exclusives <- c(exclusives, unique(Exclusive[hits])) # some can have multiple exclusives associated!
    }
  }
  
  attr(result, 'dispatch') <-  list(Original = x, 
                                    Regexes = unlist(dispatchDF$regex),
                                    Segments = Exclusive, 
                                    Exclusives = exclusives)
  result
  
}


makeExcluder <- function(from, to) {
  from <- from[!from %in% to]
  
  if (length(from) == 0L) return(NULL)
  
  
  func <- rlang::new_function(alist(Exclusive = ), 
                      env = pkg_env('humdrumR'), 
                      rlang::expr({
                        if (is.null(Exclusive)) return(NULL)
                        Exclusive[!is.na(Exclusive) & Exclusive %in% !!from] <- !!to
                      }))
  
  attr(func, 'from') <- from
  attr(func, 'to') <- to
  func %class% "Excluder"
}

#' @export
print.Excluder <- function(x) {
  # from <- attr(x, 'from')
  to <- attr(x, 'to')
  cat('**', to, '\n', sep = '')
  
  # from <- harvard(paste0('**', from), 'or')
  # if (length(from) > 1L) from <- paste0('(', from, ')')
  # cat(from, ' -> ', '**', to, '\n', sep = '')
}

do_attr <- function(func, x, ...) {
  humattr <- humdrumRattr(x)
  
  result <- func(x, ...)
  humdrumRattr(result) <- humattr
  result
}

humdrumRattr <- function(x) {
  known <- c('dispatch', 'dispatched', 'visible', 'Exclusive')
  attr <- attributes(x)
  
  attr[names(attr) %in% known]
}
`humdrumRattr<-` <- function(x, value) {
  known <- c('dispatch', 'dispatched', 'visible', 'Exclusive')
  if (is.null(value)) attributes(x) <- attributes(x)[!names(attributes(x)) %in% known]
  for (attrname in names(value)) attr(x, attrname) <- value[[attrname]]
  x
}

rePlace <- function(result, dispatched = attr(result, 'dispatch')) {
  if (is.null(dispatched) || length(result) != length(dispatched$Original) || !is.atomic(result)) return(result)
  names <- names(result)
  
  result <- unlist(Map(stringi::stri_replace_first_regex, split(dispatched$Original, dispatched$Segments), dispatched$Regexes, split(result, dispatched$Segments)))
  names(result) <- names
  result
}

reParse <- function(result, dispatched = attr(result, 'dispatch'), reParsers) {
  # if (is.null(dispatched) || length(result) != length(dispatched$Original) || is.character(result)) return(result)
  if (is.null(dispatched) || is.character(result)) return(result)
  
  humAttr <- humdrumRattr(result)
  
  names <- names(result)
  exclusives <- dispatched$Exclusives
  
  result <- if (length(result) > 1L && length(result) == length(dispatched$Segments)) split(result, dispatched$Segments) else list(result)
  
  result <- unlist(Map(\(res, excl) {
    reParser <- match.fun(if (excl %in% reParsers)  excl else reParsers[1])
    reParser(res, inPlace = FALSE)
  }, result, exclusives))
  
  names(result) <- names
  
  humAttr$Exclusives <- NULL
  humdrumRattr(result) <- humAttr
  
  result
  
}
  


#' @rdname humdrumDispatch
#' @export
makeDispatchDF <- function(...) {
  quoted <- rlang::enexprs(...) 
  
  if (length(quoted) == 0L) .stop("You can't make a dispatchDF with zero dispatch options!")
  
  
  dispatchDF <- data.table::as.data.table(do.call('rbind', list(...)))
  colnames(dispatchDF) <- c('Exclusives', 'regex', 'method')
  
  dispatchDF$regexPrint <- sapply(quoted, \(row) as.character(row[[3]])[1])
  dispatchDF$regexPrint <-  unlist(Map(\(regex, print) if (rlang::is_function(regex)) paste0(print, '(...)') else print, dispatchDF$regex, dispatchDF$regexPrint))
  
  
  dispatchDF$methodPrint <- sapply(quoted, \(row) as.character(row[[4]])[1])
  
  dispatchDF$Args <- lapply(dispatchDF$method, fargs)
  
  if (all(is.na(dispatchDF$regex)) && any(duplicated(dispatchDF$Exclusive))) .stop("Can't make a DispatchDF with no regexes and duplicated Exclusive interpretations!")
  
  dispatchDF
}

#' @rdname humdrumDispatch
#' @export
makeHumdrumDispatcher <- function(..., funcName = 'humdrum-dispatch', outputClass = 'character', args = alist()) {

  dispatchDF <- makeDispatchDF(...)
                       
  if (all(is.na(dispatchDF$regex))) {
    regexApply <- FALSE
    dispatcher <-  quote(exclusiveDispatch) 
  } else {
    regexApply <- TRUE
    dispatcher <-  quote(humdrumDispatch)
  }
  
  
  # Assemble the new function's arguments
  genericArgs <- local({
    sharedArgNames <- Reduce('intersect', lapply(dispatchDF$Args, names))
    args <- c(alist(x = , ... =), 
              unlist(dispatchDF$Args, recursive = FALSE)[sharedArgNames], 
              args, 
              alist(Exclusive = NULL, multiDispatch = FALSE))
    args <- args[!duplicated(names(args))]
    args

  })
  
  formalSymbols <- setNames(rlang::syms(names(genericArgs)), names(genericArgs))
  formalSymbols <- formalSymbols[names(formalSymbols) != '...']
  ##################################################### #
  body <- rlang::expr({
    args <- list(!!!formalSymbols)
    result <- do(!!dispatcher, outputClass = !!outputClass, 
                 c(args,  
                   list(..., dispatchDF = dispatchDF, 
                        regexApply = !!regexApply, outputClass = !!outputClass, funcName = !!funcName)),
       ...)
    
    if (!(!!outputClass) %in% class(result)) result <- as(result, !!outputClass)
    
    result
  })
  
  genericFunc <- rlang::new_function(genericArgs, body, 
                                     rlang::new_environment(list(dispatchDF = dispatchDF), parent = parent.frame()))
  
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









