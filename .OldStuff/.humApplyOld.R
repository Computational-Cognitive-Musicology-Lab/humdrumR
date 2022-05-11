
appendToCall <- function(thecall, args) {
  if (is.name(thecall)) thecall <- call(as.character(thecall))
  
 n <- length(thecall)
 for (i in seq_along(args)) {
  n <- n + 1
  thecall[[n]] <- args[[i]]
  if (!is.null(names(args))) {
   names(thecall)[n] <- names(args)[i]
  }
 }
 thecall
}
# 

examples <- c(~mean, # apply mean
              
              ~mean & median, # apply mean and median return in list
              ~(mean & median), # same
              ~list(mean(na.rm = TRUE), median), # same
              
              ~(mean : median), # apply mean, then median
              ~list(table, mean : median(na.rm = TRUE)), 
              ~list(table, mean : log : exp),
              
              ~mean & median[each = Spine], 
              ~list(mean, median)[each = File * Spine],
              ~mean(Ditto) - Ditto,
              ~mean - .,
              ~(mean(Ditto, na.rm = TRUE) - Ditto)[each = File],
              ~mean[each = Spine] & median[... = spine],
              ~mean[each = Spine] & median[... = spine](Ditto), 
              ~mean(na.rm = TRUE)[where = Spine == 1])


makeCall <- function(symbol) {
 if (is.call(symbol)) {
   return(symbol)
 }
  
 aschar <- as.character(symbol)
 evalit <- pander::evals(aschar)[[1]]
 if (evalit$type != 'error') {
   obj <- eval(symbol)
   if (is.function(obj) || is.primitive(obj)) {
    return(call(aschar, quote(Active)))
   }
 }
 return(symbol)
}


parseFcall <- function(fcall, layers = c('Token' ,'Ditto')) {
  if (len1(fcall)) {
    if (deparse(fcall) == '.') return(quote(Active))
    return(makeCall(fcall))
  }
  
  if (deparse(fcall[[1]]) ==  '(') fcall <- fcall[[-1]]
  
  callfun <- deparse(fcall[[1]]) %str-% ' '
  if (callfun == '*') {
    return(do.call('call', c('.', as.list(fcall[-1])), quote = TRUE))
  }
  
  if (callfun %in% c('==', '!=', '>', '<', '>=', '<=', '%%')) {
   return(fcall) 
  }
  
  if (callfun %in% c('&', 'c', '-', '+', '/', ':', 
                     'list', 'cbind', 'c', 'rbind', 'diff')) {
    combine <- fcall[[1]]
    calllist <- lapply(as.list(fcall[-1]), sys.function(), layers = layers)
    clattr <- lapply(calllist, attributes)
    
    if (callfun == '&') combine <- as.name('list')
    if (callfun == ':') {
      call <- Reduce(function(x,y) { y[[2]] <- x ; y}, calllist, right = TRUE)
    } else {
      call <- do.call('call' , append(as.character(combine), calllist), quote = TRUE)
      ## NEED TO FIX HERE, to include ... and where
      attr(call, 'subattr') <- clattr
      
    }
    return(call)
  }

  if (callfun  %in% c('[', '[[')) { 
    indexedcall <- Recall(fcall[[2]], layers)
    how <- names(fcall)[3]
    groups <- Recall(fcall[[3]], layers) 
    if (how == 'each') {
      attr(indexedcall, 'by')  <- do.call('call', c('=', quote(by), groups), quote = TRUE)
    }
    if (how == '...') {
      indexedcall <- splatCall(indexedcall, groups, layers)
    }
    if (how == 'where') {
      indexedcall <- whereCall(indexedcall, groups, layers)
    }
    # attr(indexedcall, 'by')  <- Recall(fcall[[3]])
    return(indexedcall)
  } 
  
  thecall <- Recall(fcall[[1]], layers)
  targets <- unlist(lapply(fcall[-1], sys.function(), layers = layers), recursive = FALSE)
  if (!any(sapply(targets, deparse) %in% c('Active', layers))) {
   targets <- c(list(quote(Active)), targets) 
  }
  if (targets %len>% 0L) {
    thecall[[2]] <- targets[[1]]
    targets <- targets[-1]
  }
  thecall <- appendToCall(thecall, targets)
  # attr(thecall, 'targets') <- targets
  
  thecall 

}



parsePipe <- function(form, layers = c('Ditto', 'Token'), active = 'Token') {
  fcall <- form[[-1]]
  if (fcall %len==% 3L && deparse(fcall[[1]]) == '|') {
    c('fcall', 'restofpipe') %<-% pipeHeadRest(fcall)
  } else {
    restofpipe <- NULL
  }
 
  parsedcall <- parseFcall(fcall, layers)
  ##
  if (is.list(parsedcall)) parsedcall <- parsedcall[[1]]
  output <- call('[', quote(D), alist(i = )[[1]], call('list', call('list', parsedcall)))
  
  # different attributes per call
  subattr <- attr(parsedcall, 'subattr')
  if (!is.null(subattr)) { 
    attrnames <- lapply(subattr, names)
    nullnames <- sapply(attrnames, is.null)
    if (sum(nullnames) == 1 && !last(nullnames)) {
     assign(last(attrnames), last(subattr))
    } 
  } else {
    by <- attr(parsedcall, 'by')
    where <- attr(parsedcall, 'where')
  }
  
  #

  if (!is.null(by)) { 
    output[[5]] <- by
  } 
  #


  if (!is.null(where)) { 
    output[[3]] <- where
  } 

  output <- eval(call('substitute', output, list(Active = as.name(active))))
  
  if (is.null(restofpipe)) {
   list(output )
  } else {
    c(output, Recall(restofpipe))
  }
  
  
}

pipeHeadRest <- function(form) {
  pipetree <- formulaSplit(form, split = '|')
  
  fcall      <- pipetree[[1]]
  restofpipe <- pipetree[-1]
  restofpipe <- call('~', Reduce(function(x, y) call('|', x, y), restofpipe))
  
  list(fcall, restofpipe)
}

splatCall <- function(call, groups, layers) {
  callhead <- call[[1]]
  call <- call[-1]
  targets = sapply(call, deparse) %in% c('Active', layers)
  target = call[targets][[1]]
  call <- call[-targets]
  splitcall <- call('unname', call('split', target, groups))
  
  if (call %len>% 1L) {
    args <- call[[-1]]
    splitcall <- call('c', splitcall, args)
    names(splitcall)[3:length(splitcall)] <- names(call)[-1]
    call <- call[[1]]
  } 

 call('%splat|%', splitcall,      callhead)
}

whereCall <- function(call, pred, layers) {
  func <- call[[1]]
  call <- call[-1]
  targets <- sapply(call, deparse) %in% c('Active', layers)
  target <- call[targets][[1]]
  
  if (!all(targets)){
    func <- do.call('call', c('pa', func, as.list(call)[!targets]), quote = TRUE)
  }
  
  newcall <- call('applyif', pred, func, target)
  
}

formulaSplit <- function(form, split) {
  if (form %len==% 3L && deparse(form[[1]]) == split) {
    append(Recall(form[[2]], split), Recall(form[[3]], split))
  } else {
    list(form)
  }
}

##

collapse2one <- function(x, cls) {
 
  print(cls)
 x[sapply(x, is.null)] <- as(NA, Class = cls)
 
 uniqs <- unique(unlist(x))
 
 if (len1(uniqs)) uniqs else list(uniqs)
 
}

collapseBy <- function(dt, byexpr) {
  classes <- sapply(dt, function(C) class(unlist(C)))
 dt[ , Map(collapse2one, .SD, classes), eval(byexpr)]
}