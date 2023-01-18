#################################### ###
# meter S4 class #######################
#################################### ###


## meter documentation ----


## Definition, validity, initialization ####

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

## Constructors ####

#' Meter S4 class
#' 
#' @export
meter <- function(x, ...) UseMethod('meter')
#' @rdname meter
#' @export
meter.meter <- function(x, ...) x
#' @rdname meter
#' @export
meter.rational <- function(x, ..., measure = NULL, tactus = NULL, tatum = '16', fill.levels = TRUE, subdiv = NULL, hyper = NULL) {
  levels <- list(x, ...)
  
  meter.list(levels, measure = measure, tactus = tactus, tatum = tatum, fill.levels = fill.levels, subdiv = subdiv, hyper = hyper, ...)
  
}

#' @rdname meter
#' @export
meter.list <- function(x, measure = NULL, tactus = NULL, tatum = '16', fill.levels = TRUE, hyper = NULL, subdiv = NULL, ...) {
  
  levels <- lapply(x, rhythmInterval)
  spans <- lapply(levels, sum)
  
  #
  measure <- if (is.null(measure)) do.call('lcm', spans) else rhythmInterval(measure)
  tactus  <- if (is.null(tactus))  levels[[1]] else rhythmInterval(tactus)
  tatum   <- if (is.null(tatum))   tatum.rational(.unlist(c(levels, lapply(levels, sum)))) else rhythmInterval(tatum)

  #
  if (fill.levels) levels <- c(levels,
                               as.list(harmonicInterpolate(sum(tactus), measure, ...)),
                               as.list(harmonicInterpolate(tatum, sum(tactus), ...)))
  
   
  if (!is.null(hyper)) levels <- c(levels, lapply(hyper, \(n) n * measure))
  if (!is.null(subdiv)) levels <- c(levels, lapply(sub, \(n) tactus / n))
  
  
  levels <- c(list(tactus, measure, tatum), levels)
  levels <- unique(lapply(levels, `humdrumRattr<-`, value = NULL))
  
  ord <- order(.unlist(lapply(levels, sum)),
               -lengths(levels), 
               decreasing = TRUE)
  new('meter', Levels = list(levels[ord]), Tactus = which(ord == 1L))
  
  
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


## Logic methods ####

setMethod('==', c('meter', 'meter'),
          function(e1, e2) {
            
            unlist(Map(e1@Levels, e2@Levels,
                       f = \(levs1, levs2) {
                         
                         length(levs1) == length(levs2) &&
                           all(.unlist(levs1) == .unlist(levs2))
                       }))
          })

###################################################################### ###
# Deparsing Meter Representations (meter2x) ##############################
###################################################################### ###

meter2timeSignature <- dofunc('x', function(x) {
  unlist(Map(\(ls, ts) {
    if (length(ls) == 0L || is.na(ls[1]) || is.na(ts) ) return(NA_character_)
    
    tactus <- ls[[ts]]
    
    if (length(tactus) == 1L) {
      if (tactus@Numerator == 3L) tactus <- tactus / 3L
      measure <- max(.unlist(lapply(ls, sum)))
      
      numerator <- measure %/% tactus
      denominator <- tactus
    } else {
      denominator <- Reduce('gcd', as.list(tactus))
      numerator <- paste(tactus %/% denominator, collapse = '+')
    }
   
  
    paste0('*M', numerator , '/', rint2recip(denominator))
    
  }, x@Levels, x@Tactus))
})
  
  

###################################################################### ### 
# Parsing Meter Representations (x2meter) ################################
###################################################################### ### 

timesignature2meter <- function(x, ..., sep = '/', compound = TRUE) {
  REparse(x, makeRE.timeSignature(sep = sep, collapse = FALSE),
          toEnv = TRUE)
  
  
  numerator <- lapply(strsplit(numerator, split = '\\+'), as.integer)
  denominator <- as.integer(denominator)
  #
  beats <- Map(rational, numerator, denominator)
  denominator <- as.list(rational(1L, denominator))
  
  levels <- lapply(list(...), rhythmInterval)
  results <- Map(\(den, bts) {
    
    measure <- sum(bts)
    
    
    tactus <- if (length(bts) > 1)  bts  else den
    
    if (compound && 
        as.integer64(3L) %divides% bts@Numerator && 
        (den@Numerator == as.integer64(1L) && 
         as.integer64(8L) %divides% den@Denominator)) {
      # compound meters
      tactus <- tactus * 3L
    } 
    do.call('meter.rational', list(tactus, ..., measure = measure))
    # levels <- c(levels, subdiv)
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
#' 
#' 
#' @name humMeter
NULL


metric <- function(dur, meter = duple(5), start = rational(0), value = TRUE, offBeats = TRUE, numeric = FALSE, deparser = recip, 
                   groupby = list(), ..., parseArgs = list(), remainderSubdivides = TRUE) {
  
  .metric(dur, meter = meter, start = start, groupby = groupby, 
          offBeats = offBeats, parseArgs = parseArgs, remainderSubdivides = remainderSubdivides, ...,
          callname = 'metric')
  
  
}

#' Calculate metric information
#' 
#' 
#' @export
metlev <- function(dur, meter = duple(5), start = rational(0), value = TRUE, offBeats = TRUE, numeric = FALSE, deparser = recip, 
                   groupby = list(), ..., parseArgs = list(), remainderSubdivides = TRUE ) {
  
  met <- .metric(dur = dur, meter = meter, start = start, groupby = groupby, parseArgs = parseArgs, 
                 remainderSubdivides = remainderSubdivides, callname = 'metlev', ...)
  
  
  metlev <- met$MetLev
  
  if (value) {
    metlev <- if (is.null(deparser)) {
      .unlist(lapply(met$Levels, sum))[metlev]
      
    } else {
      sapply(met$Levels, 
                       \(lev) {
                         output <- deparser(lev, ...)
                         paste(output, collapse = '+')
                         
                       })[metlev]
    } 
  }
  
  
  if (!offBeats) metlev[met$Remainder != rational(0L)] <- as(NA, class(metlev))
  
  metlev
  
}


#' @rdname metlev
#' @export
metcount <- function(dur, meter = duple(5), level = tactus(meter), ...,
                     offBeats = FALSE,
                     start = rational(0), groupby = list(), parseArgs = list(), remainderSubdivides = TRUE) {
  
  met <- .metric(dur = dur, meter = meter, start = start, groupby = groupby, parseArgs = parseArgs, 
                 remainderSubdivides = remainderSubdivides, callname = 'metcount', ...)
  
  
  counts <- met$Counts
  
  
  if (is.character(level) && any(!level %in% colnames(counts))) {
    .stop("In your call to metcount(), {harvard(unique(level[!level %in% colnames(counts)]), 'and', quote = TRUE)}",
          "<are not names of metric levels|is not a name of a metric level>",
          "in the input meter.",
          "These levels as {harvard(colnames(counts), 'and', quote = TRUE)}.",
          ifelse = length(level[!level %in% colnames(counts)]) == 1L)
  }
  
  if (is.numeric(level) && any(level < 1 || level > ncol(counts))) {
    .stop("In your call to metcount(), {harvard(unique(level[level < 1 || level > ncol(counts)]), 'and')}",
          "<are not valid metric levels|is not a valid metric level>",
          "as, the in the input meter only has {num2word(ncol(counts))} levels.",
          ifelse = length(level[level < 1 || level > ncol(counts)]) == 1L)
  }
  

  if (is.character(level)) {
    level <- match(level, colnames(counts))
  }
  
  mcount <- if (length(level) == 1L) {
    counts[, level]
  }  else {
    
    counts[cbind(seq_len(nrow(counts)), rep(level, length.out = nrow(counts)))]
  }
  
  
  if (!offBeats) mcount[!met$OnBeat[cbind(seq_len(nrow(counts)), rep(level, length.out = nrow(counts)))]] <- NA_integer_
  
  mcount
    
}


.metric <- function(dur, meter = duple(5), start = rational(0), groupby = list(), ..., 
                    parseArgs = list(), remainderSubdivides = TRUE, callname = '.metric') {
  
  if (length(unique(meter)) > 1L) {
    return(.metrics(dur, meter = meter, start = start, 
                    groupby = groupby, parseArgs = parseArgs, remainderSubdivides = remainderSubdivides,
                    callname = callname, ...))
  }
  
  dur <- do.call('rhythmInterval', c(list(dur), parseArgs))
  
  meter <- meter(meter, ...)
  
  timeline <- pathSigma(dur, groupby = groupby, start = start, callname = callname)
  

  levels <- meter@Levels[[1]]
  spans <- .unlist(lapply(levels, sum))
  nbeats <- lengths(levels)
  
  counts <- do.call('cbind', lapply(lapply(levels, \(l) if (length(l) > 1) list(l) else l), count, dur = dur, start = start, groupby = groupby))
  counts[counts >= 1L] <- counts[counts >= 1L] - 1L
  
  rounded_timelines <- lapply(seq_along(spans), \(i) spans[i] * counts[,i])
  remainders <- do.call('cbind', lapply(rounded_timelines, \(rt) timeline - rt))
  
  ## get counts
  parents <- unlist(Map(as.list(spans),  
                        seq_along(spans),
                        f = \(spn, i) {
                          hits <- seq_along(spans) < i & 
                            spn <= spans & 
                            spn %divides% spans &
                            !(nbeats[i] > 1 & nbeats > 1)
                          if (any(hits)) max(which(hits)) else 0L
                        }))
  
  counts <- do.call('cbind', 
                    Map(parents, seq_along(spans),
                        f = \(parent, self) {
                          if (parent == 0L) return(counts[ , self])
                          
                          beatsPerParent <- (spans[parent] %/% spans[self]) * nbeats[self]
                          count <- counts[,self] %% as.integer(beatsPerParent) # the %% method for integer64 is screwed up!
                          
                          if (nbeats[parent] > nbeats[self]) {
                            beats <- cumsum(c(0L, as.integer(levels[[parent]] %/% spans[self])))
                            count <- count - beats[(counts[ , parent] %% nbeats[parent]) + 1L]
                          }
                          count
                          
                        })) 

  ## figure out remainders
  onbeats <- remainders == rational(0L)
  lowestLevel <- leftmost(onbeats, which = TRUE)[ , 'col']
  onbeat <- lowestLevel > 0L
  
  
  if (any(!onbeat)) {
    
    offbeats <- as.double(remainders[!onbeat , ])
    
    if (remainderSubdivides) {
      subdivide <- do.call('cbind', lapply(as.list(spans), \(span) dur[!onbeat] %divides% span))
      offbeats[!subdivide] <- max(offbeats)
    }
    
    lowestLevel[!onbeat] <- max.col(-offbeats, ties.method = 'last')
    
  }
  remainder <- remainders[cbind(seq_len(nrow(remainders)), lowestLevel)]
    
  # remove redundant counts
  counts[sweep(col(counts), 1L, lowestLevel, '>')] <- 0L
  counts[sweep(col(counts), 1L, parents[lowestLevel], '>') & !sweep(col(counts), 1L, lowestLevel, '==')] <- 0L
  
  counts <- as.integer(counts) %<-dim% dim(counts)
  
  colnames(counts) <-  colnames(onbeats) <- sapply(levels, \(ls) paste(recip(ls), collapse = '+'))

  list(Counts = counts + 1L, 
       Remainder = remainder, 
       OnBeat = onbeats, 
       Levels = levels,
       MetLev = lowestLevel)
}

.metrics <- function(dur, meter = duple(5), start = rational(0), groupby = list(), ..., 
                     parseArgs = list(), remainderSubdivides = TRUE, callname = '.metric') {
  
  uniqmeters <- unique(meter)
  
  mets <- lapply(seq_along(uniqmeters), 
                \(i) {
                  targets <- meter == uniqmeters[i]
                  
                  met <- .metric(dur[targets], uniqmeters[i], 
                                 start = if (length(start) == length(dur)) start[targets] else start, 
                                 groupby = lapply(groupby, '[', i = targets),
                                 parseArgs = parseArgs, remainderSubdivides = remainderSubdivides,
                                 callname = callname, ...)
                  met$Indices <- which(targets)
                  met
                })
  
  ## get full counts table
  topLevels <- unique(unlist(lapply(mets, \(met) colnames(met$Count)[1])))
  allCols <- unique(unlist(lapply(mets, \(met) colnames(met$Count))))
  
  
  counts  <- matrix(NA_integer_, 
                    nrow = length(dur), ncol = length(allCols),
                    dimnames = list(NULL, allCols))
  onbeats <- matrix(NA, 
                    nrow = length(dur), ncol = length(allCols),
                    dimnames = list(NULL, allCols))
  
  
  for (met in mets) {
    counts[cbind(rep(met$Indices, ncol(met$Counts)), 
                 rep(match(colnames(met$Counts), allCols), each = length(met$Indices)))] <- c(met$Counts)
    onbeats[cbind(rep(met$Indices, ncol(met$OnBeat)), 
                  rep(match(colnames(met$Counts), allCols), each = length(met$Indices)))] <- c(met$OnBeat)
  }
  
  # need to make counts in the same beat accumulate across changes in meter
  counts[, colnames(counts) %in% topLevels] <- apply(counts[, colnames(counts) %in% topLevels],
                                                     2,
                                                     makeCumulative,
                                                     groupby = c(list(segments(meter)), groupby))
  
  # levels
  levels <- unique(.unlist(lapply(mets, '[[', 'Levels')))
  neworder <- order(as.double(.unlist(lapply(levels, sum))), lengths(levels), decreasing = TRUE)
  
  counts <- counts[, neworder, drop = FALSE]
  onbeats <- onbeats[ , neworder, drop = FALSE]
  levels <- levels[neworder]
  
  
  # get other stuff
  indices <- unlist(lapply(mets, '[[', 'Indices'))
  remainder <- .unlist(lapply(mets, '[[', 'Remainder'))[order(indices)]
  lowestLevel <- match(unlist(lapply(mets, \(met) colnames(met$Counts)[met$MetLev])), allCols[neworder])[order(indices)]
  
    
  
  list(Counts = counts, 
       Remainder = remainder, 
       OnBeat = onbeats, 
       Levels = levels,
       MetLev = lowestLevel)
}
# normalizeMeasures <- function(dur, )

## Count ----

#' Count the number of elapsed beats (or measures)
#' 
#' 
#' 
#' how many beats have passed, and the offset between each attack and the nearest beat.
#' @export
count <- function(dur, beat = rational(1L), start = 1L, offBeats = TRUE,
                  phase = rational(0L), beat.round = floor, groupby = list()) {
  

   
  checks(start,  
         #+ "This is because the first beat to count occurs at the starting instant, so there is no 'zeroth' beat"
         (...number & ...scalar & ...notzero ) |
           (...logical & ...match(dur, 'dur')))
         # ...match(dur, 'dur'), ...natural,
         # list(...notzero, "This is because the first beat to count occurs at the starting instant, so there is no 'zeroth' beat.")
         # )
 
  timeline <- scaled_timeline(dur, beat, start = if (is.logical(start)) start else rational(0L), groupby, callname = 'count')
  #
  
  mcount <- as.integer(numerator(beat.round((timeline$Timeline + phase))))
  
  if (any(timeline$Irregular)) {
    subcounts <- Map(timeline$values, 
                     as.list(timeline$tatum),
                     f = \(bs, tat) cumsum(as.integer(numerator(bs / tat))))
    
    segments <- segments(timeline$indices)
    mcounts <- split(mcount, segments)
    
    mcounts <- Map(mcounts, 
                   subcounts[attr(segments, 'values')],
                   f = \(m, sc) {
                     m <- m - min(m)
                     
                     if (length(sc) == 1L) return(m)
                     
                     sub <- m %% max(sc)
                     
                     if (!offBeats) sub[!sub %in% c(0, sc)] <- NA
                     
                     sub <- findInterval(sub, sc, rightmost.closed = TRUE, left.open = FALSE)
                       
                     (length(sc) * (m %/% max(sc))) + sub
                   })
    
    mcount <- unlist(mcounts, use.names = FALSE)
    
    mcount <- delta.default(mcount)
    mcount[mcount < 0L] <- 1L
    mcount <- sigma(mcount)
  }

  
  if (!offBeats) mcount[!timeline$Irregular & !(rational(1) %divides% (timeline$Timeline + 1))] <- NA
  
  if (!is.logical(start)) {
    mcount <- mcount + as.integer(start) 
    if (start < 0L) mcount[mcount >= 0L] <- mcount[mcount >= 0L] + 1L
    }
  
  mcount
}

#' @rdname count
#' @export
subpos <- function(dur, beat = rational(1L), start = rational(0), deparser = duration, 
                    phase = rational(0L), beat.round = floor, groupby = list(), ...) {
  
  scaled <- scaled_timeline(dur, beat, start, groupby, callname = 'subpos', sumBeats = TRUE)
  
  timeline <- (scaled$Timeline %% rational(1)) * scaled$Scale
  
  if (any(scaled$Irregular)) {
    irregular <- scaled$Irregular
    indices <- scaled$indices
    irregTimeline <- Map(split(timeline[irregular], indices[irregular]),
                         scaled$values[unique(indices[irregular])], 
                         f =  \(tl, bts) {
                           
                           subcount <- numerator(tl / sum(bts))
                           btcounts <- cumsum(numerator(bts / sum(bts)))
                           bts <- cumsum(c(rational(0L), bts))[findInterval(subcount, btcounts, rightmost.closed = FALSE, left.open = FALSE) + 1L]
                           tl - bts
                         })
    
    timeline[unlist(split(seq_along(indices)[irregular], indices[irregular]))] <- .unlist(irregTimeline)
  }
  
  if (is.null(deparser)) timeline else deparser(timeline, ...)
}

scaled_timeline <- function(dur, beat, start, groupby, callname, sumBeats = FALSE) {
  dur <- rhythmInterval(dur)
  
  checkArg(beat, argname = 'beat', callname = callname,
           min.length = 1L, max.length = 1L, alt.length = length(dur))
  
  if (is.list(beat)) {
    beat <- rep(beat, length.out = length(dur))
    uniqueBeats <- valind(beat)
    
    uniqueBeats$values <- lapply(uniqueBeats$values, rhythmInterval)
    
    irregular <- (lengths(uniqueBeats$values) > 1L)[uniqueBeats$i]
    
    tatum <- .unlist(lapply(uniqueBeats$values, if(sumBeats) sum else tatum.rational))
    
    beat <- tatum[uniqueBeats$i]
    
  } else {
    irregular <- logical(length(dur))
    beat <- rhythmInterval(beat)
  }
  
  
  dur <- dur / beat
  
  timeline <- pathSigma(dur, groupby = groupby, start = start, callname = 'count')
  
  c(list(Timeline = timeline, Scale = beat, Irregular = irregular, tatum = tatum), get0('uniqueBeats'))
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
tactus.meter <- function(x, deparser = recip, sep = '+', ...) {
  
  result <- Map('[[', x@Levels, x@Tactus)
  if (is.null(deparser)) {
    result
  } else {
    sapply(result, \(x) paste(deparser(x, ...), collapse = sep))
  }
} 
#' @rdname tactus
#' @export
tactus.character <- dofunc('x', function(x, deparser = recip) {
 tactus.meter(meter.character(x), deparser = deparser)
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



###################################################################### ###
# Metric analysis tools ##################################################
###################################################################### ###


syncopation <- function(dur, meter = duple(5), groupby = list()) {
  levs <- metlev(dur, meter = meter, groupby = groupby, deparser = duration)
  
  dur <- rhythmInterval(dur)
  
  dur > levs
  
  
}

