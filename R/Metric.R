#' Meter class
#' 
#' @name meter
#' @export
setClass('meter', contains = 'struct', slots = c(Levels = 'list', Tactus = 'integer'))


setValidity('meter', 
            \(object) {
              levels <- object@Levels
              tactus <- object@Tactus
              errors <- Map(\(ls, ts) {
                if (length(ls) == 1L && is.na(ls)) {
                   return (if (!is.na(ts)) "NA levels must have an NA tactus.")
                  
                }
                spans <- do.call('c', lapply(ls, sum))
                errors <- c(
                  if (any(diff(spans) > rational(0))) "Metric levels must get consecutively shorter.",
                  if (!all(sapply(ls, is.rational))) "All levels of meter object must be rational vectors.",
                  if (any(lengths(ls) < 1L)) "All levels of meter object must have at least length 1.",
                  if (length(ts) > 1L || ts <= 0 || ts > length(ls)) "Invalid tactus."
                )
                
              }, levels, tactus) 
              
              errors <- unlist(errors)
              
              if (length(errors)) errors else TRUE
              
              
              
            })


setMethod('initialize', 
          'meter',
          function(.Object, Levels, Tactus) {
            Tactus <- rep(Tactus, length.out = length(Levels))
            na <- lengths(Levels) == 0L | is.na(Tactus)
            
            Levels[na] <- NA
            Tactus[na] <- NA
            
            .Object <- callNextMethod()
            .Object@Levels <- Levels
            .Object@Tactus <- Tactus
            
            .Object
            
          })

#' Meter S4 class
#' 
#' @export
meter <- function(x, ...) UseMethod('meter')
#' @rdname meter
#' @export
meter.rational <- function(x, ...) {
  
  levels <- list(x, ...)
  
  ord <- order(sapply(levels, \(l) sum(as.double(l))),
               -lengths(levels), 
               decreasing = TRUE)
  
  new('meter', Levels = list(levels[ord]), Tactus = which(ord == 1L))
  
}
#' @rdname meter
#' @export
meter.list <- function(x, ...) {
  x <- x[sapply(x, class) == 'list']
  
  if (length(x) == 0L) return(new('meter', Levels = list(), Tactus = integer(0)))
  do.call('c', lapply(x, \(ls) do.call('meter.rational', ls)))
}




#' @rdname meter
#' @export
duple <- function(nlevels = 4, measure = rational(1), tactus = 3L) {
  checkLooseInteger(nlevels, 'nlevels', 'duple', minval = 1, min.length = 1L)
  
  measure <- rhythmInterval(measure)
  match_size(nlevels = nlevels, measure = measure, tactus = tactus, toEnv = TRUE)
  
  tactus <- pmin(nlevels, tactus)
  
  levels <- Map(\(n, m) lapply(2^((1:n) - 1), \(d) rational(1, d) * m), nlevels, as.list(measure))
  new('meter', Levels = levels,
      Tactus = as.integer(tactus))
  
  
}


###################################################################### ###
# Deparsing Meter Representations (meter2x) ##############################
###################################################################### ###

meter2timeSignature <- function(x) {
  unlist(Map(\(ls, ts) {
    if (length(ls) == 0L || is.na(ls[1]) || is.na(ts) ) return(NA_character_)
    
    tactus <- ls[[ts]]
    if (tactus@Numerator == 3L) tactus <- tactus / 3L
    
    spans <- do.call('c', lapply(ls, sum))
    span <- max(spans)
    numerator <- paste(span %/% tactus, collapse = '+')
    if (length(tactus) > 1L) numerator <- paste0('(', numerator, ')')
    
    
    denominator <- rint2recip(Reduce('gcd', as.list(tactus)))
    paste0('*M', numerator , '/', denominator)
    
  }, x@Levels, x@Tactus))
}
  
  

###################################################################### ### 
# Parsing Meter Representations (x2meter) ################################
###################################################################### ### 

timesignature2meter <- function(x, sep = '/') {
  x <- gsub('^\\*?M?', '', x)
  REparse(x, makeRE.timeSignature(sep = sep, collapse = FALSE),
          toEnv = TRUE)
  
  
  numerator <- lapply(strsplit(numerator, split = '\\+'), as.integer)
  denominator <- as.integer(denominator)
  
  #
  beats <- Map(rational, numerator, denominator)
  denominator <- as.list(rational(1L, denominator))
  
  results <- Map(\(den, bts) {
    levels <- if (length(bts) > 1) {
      list(den, bts, sum(bts))
    } else {
      if (3L %divides% bts@Numerator && (den@Numerator == 1L && 2L %divides% den@Denominator)) {
        c(list(den * 3L), den, bts)
      } else {
        list(den, bts)
      }
      
    }
    do.call('meter.rational', levels)
  }, denominator, beats) 
  
  do.call('c', results)
}


## Meter Parsing Dispatch ######################################

#' @rdname meter
#' @export
meter.character <- makeHumdrumDispatcher(list('any', makeRE.timeSignature, timesignature2meter),
                                         funcName = 'meter.character',
                                         outputClass = 'meter')

setMethod('as.character', 'meter', meter2timeSignature)

#### setAs meter ####
setAs('integer', 'meter', \(from) new('meter', 
                                      Levels = as.list(rep(NA, length(from))), 
                                      Tactus = rep(NA_integer_, length(from))))

## Meter ####

#' Tools for analyzing rhythm and meter.
#' 
#' [humdrumR] includes a number of useful
#' functions for working with rhythms and meter.
#'
#' + [rhythmDecompose()] decomposes a series of rhythms in terms of desired pulses.
#' + [rhythmOffset()] Calculates the cummulative offset of durations from a starting point.
#' 
#' 
#' @name humMeter
NULL


#' Calculate metric positions from duration data.
#' 
#' 
#' @family rhythm analysis tools
#' @export
metric <- function(ioi, meter = duple(5), ..., remainderSubdivides = TRUE ) {
  
  soi <- SOI(ioi)$Onset
  


  levels <- meter@Levels
  tatum <- .unlist(lapply(levels, sum))
  
  parent <- bottommost(outer(tatum, tatum, \(x, y) x > y & y %divides% x), TRUE)[ , 'row']
  
  ois <- counts <- vector('list', length(levels))
  
  for (i in seq_along(levels)) {
    higherLevel <- max(0L, which(tatum[i] < tatum & tatum[i] %divides% tatum))
    
    curMeasure <- measure(if (higherLevel == 0L) soi else ois[[higherLevel]], 
                          levels[[i]])
    
    counts[[i]] <- curMeasure$N
    ois[[i]] <- curMeasure$SOI
  }
  
  counts <- do.call('cbind', counts)
  ois <- do.call('cbind', ois)
  
  lowestLevel <- leftmost(ois == 0, which = TRUE)[,'col']
  
  remainders <- lowestLevel == 0L
  
  if (any(remainders)) {
    remains <- as.double(ois[remainders, ])
    
    ranked <- t(apply(remains, 1, order, decreasing = TRUE))
    
    if (remainderSubdivides) {
      subdivide <- sapply(as.list(tatum), \(tat) rhythmInterval(ioi)[remainders] %divides% tat)
      ranked[!subdivide] <- 1L
    }
    lowestLevel[remainders] <- max.col(ranked, ties.method = 'last')
  }
  
  
  remainder <- ois[cbind(seq_along(soi), lowestLevel)]
  
  # remove redundant counts
  counts[sweep(col(counts), 1L, lowestLevel, '>')] <- 0L
  counts[sweep(col(counts), 1L, parent[lowestLevel], '>') & !sweep(col(counts), 1L, lowestLevel, '==')] <- 0L
  
  
  beats <- lapply(seq_along(tatum), \(j) tatum[j] * counts[ , j])
  output <- do.call('cbind', c(beats, Remainder = remainder))
  
  colnames(output) <- c(sapply(levels, \(l) paste(recip(l), collapse = '+')), 'Remainder')
  rownames(output) <- ioi
  attr(output, 'meter') <- meter
  
  output <- recip(output)
  output[output == '1%0'] <- ''
  
  output
}


metricPlot <- function(metric) {
  metric[metric == ""] <- NA
  
  durations <- duration(metric)
  durations[is.na(durations)] <- 0
  y <- barplot(t(durations), horiz = TRUE, col = c(gray.colors(ncol(metric) - 1), 'red'))
  
  x <- t(apply(durations,1,cumsum))
  y <- replicate(ncol(x), y)
  text(x[x != 0], y[x != 0], metric[x != 0], pos = 2, cex=.5)
}


# normalizeMeasures <- function(dur, )

# Count the number of beats in a duration
# 
# how many beats have passed, and the offset between each attack and the nearest beat.
count <- function(durs, beat = rational(1L), 
                  phase = rational(0L), beat.round = floor) {
  
  durs <- rhythmInterval(durs)
  
  beat <- rhythmInterval(beat)
  totalTatum <- sum(beat)
  #

  
  mcount <- beat.round((durs + phase) / totalTatum) 
  
  if (length(beat) > 1L) {
    
    beatoff <- sigma(beat)
    mremain <- ((durs + phase) - totalTatum * mcount)
    
    subcount <-  outer(beatoff, mremain, '<=') |> colSums()
    mcount <- mcount * length(beat) + subcount
    
    
    # mremain <- mremain - c(rational(0), beatoff)[subcount + 1]
    
  }
  
  numerator(mcount)
}


subpos <- function(durs, beat = rational(1L), deparser = recip, 
                   phase = rational(0L), beat.round = floor, Bar = NULL) {
  
  durs <- rhythmInterval(durs)
  
  beat <- rhythmInterval(beat)
  totalTatum <- sum(beat)
  
  mcount <- beat.round((durs + phase) / totalTatum) 
  
  mremain <- ((durs + phase) - totalTatum * mcount)
  
  if (length(beat) > 1L) {
    
    beatoff <- sigma(beat)
    mremain <- ((durs + phase) - totalTatum * mcount)
    
    subcount <-  outer(beatoff, mremain, '<=') |> colSums()
    mcount <- mcount * length(beat) + subcount
    
    
    mremain <- mremain - c(rational(0), beatoff)[subcount + 1]
    
  }
  
  deparser(mremain)
  # 

  
}



# test <- data.table(Dur = c('4.','8','4','4','4','4','2','4.','8','4','4.','8','4','4','8','8','2'),
                   # Meter = c(1, 1, 1, 1, 1, 1, .75, .75, .75, .75, .75, .75, .75, 1,1,1,1))
# Meter extraction tools ----

## Find tatum

#' Metric units
#' 
#' @family {Metric functions}
#' @export
tatum <- function(x, ...) UseMethod('tatum')
#' @rdname tatum
#' @export
tatum.meter <- function(x) {
  do.call('c', lapply(x@Levels, \(ls) tatum.rational(do.call('c', ls))))
}
#' @rdname tatum
#' @export
tatum.default <- function(x) {
  if (is.character(x) && any(grepl('\\*M', x))) {
    recip(tatum.meter(meter.character(x)))
  } else {
    rint <- rhythmInterval(unique(x))
    result <- tatum.rational(rint)
    reParse(result, attr(rint, 'dispatch'), c('recip', 'duration'))
  }
}
#' @rdname tatum
#' @export
tatum.rational <- function(x)  do.call('gcd', as.list(unique(x)))
#' @rdname tatum
#' @export
tatum.NULL <- function(x) NULL

#' Properties of meters
#' 
#' @family {Metric functions}
#' @export
tactus <- function(x, deparser, ...) UseMethod('tactus') 
#' @rdname tactus
#' @export
tactus.meter <- function(x, deparser = recip) {
  
  result <- do.call('c', Map('[[', x@Levels, x@Tactus))
  if (!is.null(deparser)) deparser(result) else result
} 
#' @rdname tactus
#' @export
tactus.character <- function(x, deparser = recip) {
 tactus.meter(meter.character(x), deparser = deparser)
}
#' @rdname tactus
#' @export
tactus.NULL <- function(x) NULL

#' @rdname tactus
#' @export
measure <- function(x, deparser, ...) UseMethod('measure') 
#' @rdname tactus
#' @export
measure.meter <- function(x, deparser = recip) {
  result <- do.call('c', lapply(x@Levels, \(ls) max(do.call('c', lapply(ls, sum)))))
  if (!is.null(deparser)) deparser(result) else result
}
#' @rdname tactus
#' @export
measure.character <- function(x, deparser = recip) do.call('c', lapply(meter(x), measure.meter, deparser = deparser))
#' @rdname tactus
#' @export
measure.NULL <- function(x) NULL

#' Counting beats
#' 
#' @export
nbeats <- function(x, deparser, ...) UseMethod('nbeats') 
#' @rdname nbeats
#' @export
nbeats.meter <- function(x, deparser = result) {
  measure.meter(x, deparser = NULL) %/% tactus.meter(x, deparser = NULL)
  
}
#' @rdname nbeats
#' @export
nbeats.character <- function(x, deparser = recip) unlist(lapply(meter(x), beats.meter))
#' @rdname nbeats
#' @export
nbeats.NULL <- function(x) NULL

