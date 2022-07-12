#' Meter class
#' 
#' @name humMetric
#' @export
setClass('meter', slots = c(Levels = 'list', Tactus = 'integer'))


meter <- function(...) {
  levels <- lapply(list(...), rhythmInterval)
  
  ord <- order(sapply(levels, \(l) sum(as.double(l))), decreasing = TRUE)
  
  
  new('meter', Levels = levels[ord], Tactus = ord[1])
  
}



setValidity('meter', 
            \(object) {
              levels <- object@Levels
              tactus <- object@Tactus
              spans <- do.call('c', lapply(levels, sum))
              errors <- c(
                if (any(diff(spans) > rational(0))) "Metric evels must get consecutively shorter.",
                if (!all(sapply(levels, is.rational))) "All levels of meter object must be rational vectors.",
                if (any(lengths(levels) < 1L)) "All levels of meter object must have at least length 1.",
                if (length(tactus) != 1L || tactus <= 0 || tactus > length(levels)) "Invalid tactus."
              )
              
            })



setMethod('show', 'meter',
          \(object) {
            levels <- object@Levels
            
            spans <- do.call('c', lapply(levels, sum))
            span <- max(spans)
            tactus <- levels[[object@Tactus]]
            
            numerator <- paste(span %/% tactus, collapse = '+')
            if (length(tactus) > 1) numerator <- paste0('(', numerator, ')')
      
            
            denominator <- rint2recip(Reduce('gcd', as.list(tactus)))
            
            cat(paste0('*M', numerator , '/', denominator), '\n')
            
            
            
          })


duple <- function(nlevels = 4, measure = rational(1), tactus = 3L) {
  
  new('meter', Levels = lapply(2^((1:nlevels) - 1), \(d) rational(1, d) * measure),
      Tactus = tactus)
  
  
}

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

#' Decompose durations in terms of other durations
#' 
#' 
#' @family rhythm analysis tools
#' @export
rhythmDecompose <- function(rhythmInterval, into = rint(c(1, 2, 4, 8, 16, 32))) {
          # into <- sort(into, decreasing = TRUE)
          
          lapply(as.list(rhythmInterval), 
                 \(rs) {
                           divs <- rs %/% into
                           parts <- into * divs
                           
                           for (i in 2:length(parts)) {
                                     parts[i] <- parts[i] - sum(parts[1:(i - 1)])       
                           }
                           parts
                 }) -> decompositions
          
          lapply(1:length(into),
                 \(j) {
                           do.call('c', lapply(decompositions, '[', j))
                 }) -> decompositions
          
          
          decompositions <- do.call('.data.frame', decompositions)
          colnames(decompositions) <- as.character(into)
          rownames(decompositions) <- make.unique(as.character(rhythmInterval))
          decompositions
}

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
      subdivide <- sapply(as.list(tatum), \(tat) ioi[remainders] %divides% tat)
      ranked[!subdivide] <- 1L
    }
    lowestLevel[remainders] <- max.col(ranked, ties.method = 'last')
  }
  
    # remainderLevel <- local({
    #   
    #   subind <- which(subdivide, arr.ind = TRUE)
    #   subind <- subind[order(subind[ , 'col'], subind[ , 'row'], decreasing = TRUE), ]
    #   subind <- subind[!duplicated(subind[ , 'row']), ]
    #   
    #   subind[order(subind[ , 'row']), 'col']
    # }) 
  
  remainder <- ois[cbind(seq_along(soi), lowestLevel)]
  
  # remove redundant counts
  counts[sweep(col(counts), 1L, lowestLevel, '>')] <- 0L
  counts[sweep(col(counts), 1L, parent[lowestLevel], '>') & !sweep(col(counts), 1L, lowestLevel, '==')] <- 0L
  
  
  # counts[!matrix(tatum[remainderJ] %divides% tatum[c(col(counts))],ncol=ncol(counts))] <- 0
  
  
  
  
  beats <- sapply(seq_along(tatum),
                  \(j) recip(tatum[j] * counts[ , j]))
  beats[counts == 0L] <- ""
  
  output <- cbind(beats, ifelse(remainder == 0, "", recip(remainder)))
  
  colnames(output) <- c(sapply(levels, \(l) paste(recip(l), collapse = '+')), 'Remainder')
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

#' Measure
#' 
#' Takes a sequence of rhythmic offsets and a regular or irregular beat unit, and counts
#' how many beats have passed, and the offset between each attack and the nearest beat.
measure <- function(soi, beat = rational(1L), start = as(0, class(dur)), phase = rational(0L), Bar = NULL) {
  # if correct meter is known (and aligned with dur)
  totalTatum <- sum(beat)
  
  
  # 
  if (!is.null(Bar) & any(Bar > 0, na.rm = TRUE)) {
    soi <- soi - soi[which(Bar > 0)[1]]
  }
  
  mcount <- ((soi + phase) %/% totalTatum) 
  mremain <- (soi - totalTatum * mcount)
  
  
  if (length(beat) > 1L) {
    
    beatoff <- soi(beat)
    
    subcount <-  outer(beatoff$Off, mremain, '<=') |> colSums()
    mcount <- mcount * length(beat) + subcount
    
    
    mremain <- mremain - (beatoff$On[1L + subcount])
    
  }
  
  output <- .data.frame(N = mcount, SOI = mremain)
  
  attr(output, 'beat') <- beat

  output
}



# test <- data.table(Dur = c('4.','8','4','4','4','4','2','4.','8','4','4.','8','4','4','8','8','2'),
                   # Meter = c(1, 1, 1, 1, 1, 1, .75, .75, .75, .75, .75, .75, .75, 1,1,1,1))