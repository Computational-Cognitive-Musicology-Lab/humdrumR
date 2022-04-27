#' @name humMetric
#' @export
setClass('meter', slots = c(Levels = 'list', Tactus = 'integer')) -> meter



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
          
          
          decompositions <- do.call('struct2data.frame', decompositions)
          colnames(decompositions) <- as.character(into)
          rownames(decompositions) <- make.unique(as.character(rhythmInterval))
          decompositions
}

#' Calculate metric positions from duration data.
#' 
#' 
#' @family rhythm analysis tools
#' @export
metricPosition <- function(offset, meter = duple(5), ...) {
  

  if (!inherits(offset, 'rhythmOffset')) offset <- offset(offset)
  
  

  levels <- meter@Levels
  
  do.call('cbind', Reduce( \(off, lev) {
           measure(off, lev)
          }, 
          levels, init = offset, accumulate = TRUE))
  
  

}



# normalizeMeasures <- function(dur, )

#' Measure
#' 
#' Takes a sequence of rhythmic offsets and a regular or irregular beat unit, and counts
#' how many beats have passed, and the offset between each attack and the nearest beat.
measure <- function(offset, beat = rational(1L), start = as(0, class(dur)), phase = rational(0L), Bar = NULL) {
  # if correct meter is known (and aligned with dur)
  offset <- offset$On
  totalTatum <- sum(beat)
  
  
  # 
  if (!is.null(Bar) & any(Bar > 0, na.rm = TRUE)) {
    offset <- offset - offset[which(Bar > 0)[1]]
  }
  
  mcount <- ((offset + phase) %/% totalTatum) 
  mremain <- (offset - totalTatum * mcount)
  
  
  if (length(beat) > 1L) {
    
    beatoff <- offset(beat)
    
    subcount <-  outer(beatoff$Off, mremain, '<=') |> colSums()
    mcount <- mcount * length(beat) + subcount
    
    
    mremain <- mremain - (beatoff$On[1L + subcount])
    
  }
  
  output <- struct2data.frame(Offset = offset, N = mcount + 1L, On = mremain)
  colnames(output)[colnames(output) == 'N'] <- paste(rint2recip(beat), collapse = '+')
  
  attr(output, 'beat') <- beat

  output
}



meterAnalyze <- function(dur, meter = c(.25, .25, .25, .25), subdiv = 1/16) {
 measure <- sum(meter)
 
 moff <- measureOffset(dur, measure)
 
 beats <- beats(moff, meter, subdiv = subdiv, measure = measure)
 
 beats$Dur <- dur
 beats$Moff <- moff
 beats
  
}
# test <- data.table(Dur = c('4.','8','4','4','4','4','2','4.','8','4','4.','8','4','4','8','8','2'),
                   # Meter = c(1, 1, 1, 1, 1, 1, .75, .75, .75, .75, .75, .75, .75, 1,1,1,1))