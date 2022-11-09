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
  .unlist(lapply(x, \(ls) do.call('meter.rational', ls)))
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
    
    if (length(tactus) == 1L) {
      if (tactus@Numerator == 3L) tactus <- tactus / 3L
      measure <- max( do.call('c', lapply(ls, sum)))
      
      numerator <- measure %/% tactus
      denominator <- tactus
    } else {
      denominator <- Reduce('gcd', as.list(tactus))
      numerator <- paste(tactus %/% denominator, collapse = '+')
    }
   
  
    paste0('*M', numerator , '/', rint2recip(denominator))
    
  }, x@Levels, x@Tactus))
}
  
  

###################################################################### ### 
# Parsing Meter Representations (x2meter) ################################
###################################################################### ### 

timesignature2meter <- function(x, sep = '/') {
  REparse(x, makeRE.timeSignature(sep = sep, collapse = FALSE),
          toEnv = TRUE)
  
  numerator <- lapply(strsplit(numerator, split = '\\+'), as.integer)
  denominator <- as.integer(denominator)
  #
  beats <- Map(rational, numerator, denominator)
  denominator <- as.list(rational(1L, denominator))
  
  results <- Map(\(den, bts) {
    levels <- if (length(bts) > 1) {
      list(bts, den, sum(bts))
    } else {
      if (as.integer64(3L) %divides% bts@Numerator && 
          (den@Numerator == as.integer64(1L) && 
           as.integer64(2L) %divides% den@Denominator)) {
        c(list(den * 3L), den, bts)
      } else {
        list(den, bts)
      }
      
    }
    do.call('meter.rational', levels)
  }, denominator, beats) 

  .unlist(results)
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

count <- function(dur, beat = rational(1L), start = rational(0),
                  phase = rational(0L), beat.round = floor, groupby = list()) {
  
  dur <- rhythmInterval(dur)
  
  checkArg(beat, argname = 'beat', callname = 'count',
           valid = \(x) length(x) == 1L || length(x) == length(dur), min.length = 1L)
  # beat <- if (is.list(beat)) lapply(beat, rhythmInterval) else as.list(rhythmInterval(beat))
  
  if (is.list(beat)) {
    uniqueBeats <- valind(beat)
    
    uniqueBeats$values <- lapply(uniqueBeats$values, rhythmInterval)
    
    irregular <- lengths(uniqueBeats$values) > 1L
      
    tatum <- .unlist(lapply(uniqueBeats$values, tatum.rational))
    
    beat <- tatum[uniqueBeats$i]
    
  } else {
    irregular <- logical(length(dur))
  }

  dur <- dur / rhythmInterval(beat)
  
  timeline <- pathSigma(dur, groupby = groupby, start = start, callname = 'count')

  #
  
  mcount <- as.integer(numerator(beat.round((timeline + phase))))
  
  if (any(irregular)) {
    subcounts <- Map(\(bs, tat) cumsum(as.integer(numerator(bs / tat))), 
                     uniqueBeats$values, 
                     as.list(tatum))
    segments <- segments(uniqueBeats$indices)
    mcounts <- split(mcount, segments)
    
    mcounts <- Map(\(m, sc) {
      m <- m - min(m)
      
      if (length(sc) == 1L) return(m)
      
      sub <- findInterval(m %% max(sc), sc, rightmost.closed = TRUE, left.open = FALSE)
      
      (length(sc) * (m %/% max(sc))) + sub
      }, 
               mcounts, 
               subcounts[attr(segments, 'values')])
    
    mcount <- unlist(mcounts, use.names = FALSE)
    
    mcount <- delta.default(mcount)
    mcount[mcount < 0L] <- 1L
    mcount <- sigma(mcount)
  }

  
  mcount + 1L
}

# # count2 uses precomputed timeline
# count2 <- function(timeline, beat = rational(1L), 
#                   phase = rational(0L), beat.round = floor) {
#   
#   
#   if (length(beat) == length(timeline) && length(unique(beat)) > 1L) {
#     beatchange <- changes(beat)
#     groupsizes <- delta(timeline[beatchange], right = TRUE)
#     groupsizes[is.na(groupsizes)] <- 0
#     
#     # need to split timeline into separate parts, but subtract the missing parts from each place.
#     timelines <- lapply(unique(beat), 
#            \(b) { 
#              offsets <- cumsum(ifelse(attr(beatchange, 'values') == b, 0, -groupsizes))
#              offsets <- rep(offsets, rle(beat)$lengths)
#              (timeline + offsets)[beat == b]
#              }) 
#     
#     mcounts <- Map(\(t, b) count(t, b), timelines, unique(beat))
#     
#     
#     
#     mcount <- unlist(mcounts)[order(unlist(tapply(seq_along(timeline), beat, list)))]
#     diff <- delta(mcount)
#     diff[beatchange] <- 1L
#     mcount <- sigma(diff)
#     return(mcount)
#  
#   }
#   
#   timeline <- rhythmInterval(timeline)
#   beat <- rhythmInterval(beat)
#   totalTatum <- sum(beat)
#   #
# 
#   
#   mcount <- beat.round((timeline + phase) / totalTatum) 
#   
#   if (length(beat) > 1L) {
#     
#     beatoff <- sigma(beat)
#     mremain <- ((timeline + phase) - totalTatum * mcount)
#     
#     subcount <-  outer(beatoff, mremain, '<=') |> colSums()
#     mcount <- mcount * length(beat) + subcount
#     
#     
#     # mremain <- mremain - c(rational(0), beatoff)[subcount + 1]
#     
#   }
#   
#   numerator(mcount)
# }


subpos <- function(durs, beat = rational(1L), deparser = recip, ...,
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
  
  deparser(mremain, ...)
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
tatum.default <- dofunc('x', function(x, deparse = TRUE) {
  
  if (is.character(x) && any(grepl('\\*?M', x))) {
    result <- tatum.meter(meter.character(x))
    if (deparse) recip(result) else result
  } else {
    rint <- rhythmInterval(unique(x))
    result <- tatum.rational(rint)
    if (deparse) {
      reParse(result, attr(rint, 'dispatch'), c('recip', 'duration'))
    } else {
      result
    }
  }
})
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
tactus.meter <- function(x, deparser = recip, ...) {
  
  result <- Map('[[', x@Levels, x@Tactus)
  
  
  if (!is.null(deparser)) lapply(result, deparser, ...) else result
} 
#' @rdname tactus
#' @export
tactus.character <- dofunc('x', function(x, deparser = recip) {
 tactus.meter(meter.character(x, deparser = deparser))
})

#' @rdname tactus
#' @export
tactus.NULL <- function(x) NULL

#' @rdname tactus
#' @export
measure <- function(x, deparser, ...) UseMethod('measure') 
#' @rdname tactus
#' @export
measure.meter <- function(x, deparser = recip) {
  x <- valind(x)

  x$values <- lapply(x$values@Levels, \(ls) max(.unlist(lapply(ls, sum))))

  
  x$values <- .unlist(x$values)
  result <- inverse.valind(x)
  
  if (!is.null(deparser)) deparser(result) else result
}
#' @rdname tactus
#' @export
measure.character <- function(x, deparser = recip) measure.meter(meter(x), deparser = deparser)
#' @rdname tactus
#' @export
measure.NULL <- function(x) NULL

#' Counting beats
#
#' @family {Metric functions}' 
#' @export
nbeats <- function(x) UseMethod('nbeats') 
#' @rdname nbeats
#' @export
nbeats.meter <- function(x) {
  as.integer(measure.meter(x, deparser = NULL) %/% tactus.meter(x, deparser = NULL))
  
}
#' @rdname nbeats
#' @export
nbeats.character <- function(x) unlist(lapply(meter(x), nbeats.meter))
#' @rdname nbeats
#' @export
nbeats.NULL <- function(x) NULL

 
#' @export
beats <- function(x) UseMethod('beats') 
#' @rdname nbeats
#' @export
beats.meter <- function(x) {
  measure.meter(x, deparser = NULL) %/% tactus.meter(x, deparser = NULL)
  
}
#' @rdname nbeats
#' @export
beats.character <- function(x) unlist(lapply(meter(x), beats.meter))
#' @rdname nbeats
#' @export
beats.NULL <- function(x) NULL
