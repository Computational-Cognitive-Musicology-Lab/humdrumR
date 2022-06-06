



# Smart dispatch/function application ----



dimParse <- function(args) {
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
humdrumDispatch <-  function(str, dispatchDF,  Exclusive = NULL, 
                             multiDispatch = FALSE, ..., outputClass = 'character') {
  
  if (is.null(str)) return(NULL)
  if (length(str) == 0L && is.character(str)) return(vectorNA(0L, outputClass))
  if (!is.character(str)) .stop(if (hasArg('funcName')) "The function '{list(...)$funcName}'" else "humdrumDispatch", "requires a character-vector 'str' argument.")
  
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
  
  
  dispatchDF <- data.table::as.data.table(do.call('rbind', list(...)))
  colnames(dispatchDF) <- c('Exclusives', 'regex', 'method')
  
  dispatchDF$regexPrint <- sapply(quoted, \(row) as.character(row[[3]])[1])
  dispatchDF$regexPrint <-  unlist(Map(\(regex, print) if (rlang::is_function(regex)) paste0(print, '(...)') else print, dispatchDF$regex, dispatchDF$regexPrint))
  
  
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
                 multiDispatch = multiDispatch,
                 outputClass = !!outputClass, funcName = !!funcName,
                 ...)
    # dispatchArgs <- list(!!!dispatchArgs)
    # for (name in .names(dispatchArgs)) if (hasArg(name)) dispatchArgs[[name]] <- get(name0)
    do(humdrumDispatch, c(args, dispatchArgs), ...)
    
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









