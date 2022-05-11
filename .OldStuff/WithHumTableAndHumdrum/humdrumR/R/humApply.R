

makeHumFunc <- function(pfcall) {
  # pfcall = parsed function/formula call
  
  attri <- attributes(pfcall)
  
  ### call by
  if (any(names(attri) == 'by')) {
     byfacs <- attri$by 
    if (attri$how == 'each') {
     # byform <- call('=', quote(by), byfacs)
     byform <- as.call(c(as.name('list'), byfacs))
    }
  } else {
   byform <- NULL 
  }
  
  ### targets
  if (any(names(attri) == 'targets')) {
    
    thecall <- as.call(c(pfcall[[1]], attri$targets))
  } else {
    thecall <- as.call(list(pfcall[[1]], as.name('Active')))
  }
  
  do.call('call', c(list('['), quote(SD), 
                    alist(x=)[[1]],
                    thecall,
                    byform), quote = TRUE) -> thefullcall
  
  function(SD) eval(thefullcall)
}


appendToCall <- function(thecall, args) {
 n <- length(thecall)
 for (arg in args) {
  n <- n + 1
 browser()
  thecall[[n]] <- arg
 }
 thecall
}
# 

examples <- c(~mean, # apply mean
              
              ~mean & median, # apply mean and median return in list
              ~(mean & median), # same
              ~list(mean, median), # same
              ~list(mean & median), # same
              
              ~(mean : median), # apply mean, then median
              
              ~mean & median[each = Spine], 
              ~list(mean, median)[each = File * Spine],
              ~mean(Recip) - Recip,
              ~(mean(Recip) - Recip)[each = File],
              ~mean[each = Spine] & median[splat = spine](Target), 
              
              ~diff(Target1, Target2 - 2))




parseFcall <- function(fcall) {
  if (len1(fcall)) return(list(fcall))
  
  if (deparse(fcall[[1]]) ==  '(') fcall <- fcall[[-1]]
  
  callfun <- deparse(fcall[[1]]) %str-% ' '
  
  if (callfun == '*') return(as.list(fcall[-1]))
  
  if (callfun %in% c('&', 'c', '-', '+', '/', ':', 
                     'list', 'cbind', 'c', 'rbind')) {
    combine <- fcall[[1]]
    if (callfun == '&') combine <- as.name('list')
    if (callfun == ':') combine <- as.name('compose')
    
    calllist <- lapply(as.list(fcall[-1]), sys.function())
    attr(calllist, 'HowToCombine') <- combine
    
    return(calllist)
  }

  if (callfun  %in% c('[', '[[')) { 
    indexedcall <- Recall(fcall[[2]])
    attr(indexedcall, 'by')  <- Recall(fcall[[3]])
    attr(indexedcall, 'how') <- names(fcall)[3]
    
    return(indexedcall)
  } 
  
  thecall <- Recall(fcall[[1]])
  attr(thecall, 'targets') <- unlist(lapply(fcall[-1], sys.function()), recursive = FALSE)
  
  thecall
}



parsePipe <- function(form) {
  fcall <- form[[-1]]
  
  if (fcall %len==% 3L && deparse(fcall[[1]]) == '|') {
    c('fcall', 'restofpipe') %<-% pipeHeadRest(fcall)
  } else {
    restofpipe <- NULL
  }
 
  parsedcall <- parseFcall(fcall)
  if (is.null(restofpipe)) {
   parsedcall 
  } else {
    list(parsedcall, Recall(restofpipe))
  }
  
}

pipeHeadRest <- function(form) {
  pipetree <- formulaSplit(form, split = '|')
  
  fcall      <- pipetree[[1]]
  restofpipe <- pipetree[-1]
  restofpipe <- call('~', Reduce(function(x, y) call('|', x, y), restofpipe))
  
  list(fcall, restofpipe)
}

formulaSplit <- function(form, split) {
  if (form %len==% 3L && deparse(form[[1]]) == split) {
    append(Recall(form[[2]], split), Recall(form[[3]], split))
  } else {
    list(form)
  }
}

