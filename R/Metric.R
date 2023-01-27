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
  checks(nlevels, xpnatural & xminlength(1))
  
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

#' Count beats or measures
#' 
#' 
#' The `count()` function takes a vector of rhythmic duration values and
#' counts (in the musical sense) the number of *beats* (or *measures*) which have occurred since the starting point, 
#' associating each rhythmic onsets with a beat.
#' The `subpos()` function is paired with `count()`, computing how far (in rhythmic time) each onset is from its
#' associated beat; if `subpos()` returns `0`, this means that an onset is *on* the beat.
#' Finally, `onbeat()` is simply a convenient shorthand for `subpos() == 0`, returning
#' a `logical` vector for indicating where onsets fall on or off beat.
#' 
#' @details
#' 
#' In many basic use cases, using `count()` is essentially the same as using `floor(timeline())`.
#' However, `count()` gives us a few additional options which add musicological power compared to [timeline()].
#' (`count()` also starts from `1` not `0`, as [timeline()] does.)
#' 
#' The first beat in an input vector is assigned the value of the `start` argument, which defaults to `start = 1L`.
#' There is no 'zeroth' count, as the first beat occurs at the instant of the starting time---i.e., the first onset in the input vector.
#' Every rhythmic onset is associated with one beat, but multiple onsets may occur within the same beat---thus
#' the output of `count()` assigns (rounds) each onset to the previous beat onset.
#' However, if `offBeats = FALSE`, only onsets that *land* on a beat are counted, with offbeat values returning `NA`.
#' 
#' The `phase` controls how offbeat onsets are associated with nearby beats.
#' `phase` is [parsed][rhythmParsing] as a rhythmic value and must be rhythmic values that are smaller than the smallest `beat` value.
#' The `phase` argument shifts the "boundary" between beats backwards, before the beat onset.
#' By default, `phase = 0` so the beat-association boundary lands on the beat: only onsets on or after each beat "belong" to that beat.
#' If `phase = '8'`, the beat boundary is pushed back to capture one eighth-note *before* the beat itself.
#' This can be used to, for example, associate the last 3/8s of a measure with the next measure (like pick ups);
#' This could be achieved with a command like `count(dur, beat = '1', phase = 3/8)`.
#' 
#'
#' 
#' @section "Beats":
#' 
#' The `beat` argument is used to indicate what size of beat you want to count.
#' The default `beat` is a whole note, equivalent to a measure of `M4/4` time.
#' The `beat` argument uses the [rhythm parser][rhythmInterval()], so it can understand beat values input in a variety of formats:
#' thus, you could specify quarter-note beats as either `beat = '4'` or `beat = 0.25`.
#' The parser also understands how to parse the (full) duration of time signature: for example, `beat = 'M3/4'` would use a dotted-half-note beat (`'2.'`).
#' 
#' ### Changing meter
#' 
#' If your data has changing meters (either between pieces, or within pieces), you can specify
#' the `beat` argument as a vector which is the same length as `dur`, indicating the
#' beat size at each moment/index. 
#' This feature is very easy to use with any dataset that includes time signature interpretations, like `"*M4/4"`;
#' these interpetations, if present, are automatically [read into][readHumdrum()] a field called `TimeSignature`.
#' For such a dataset, you can simply pass the `TimeSignature` field to the `beat` argument of `count()`, and 
#' the measures of the piece will be correctly counted (even when changing!): `count(x, beat = TimeSignature)`.
#' Alternatively, you can use the [tactus()] command to extract the tactus beat from a time signature, like `count(x, beat = tactus(TimeSignature))`.
#' 
#' ### Irregular meter
#' 
#' Some musical meters consist of a pattern of irregular beats.
#' For example, the meter `M7/8` is often realized as two "short" beats (two eigth-notes each) and one "long" beat (three eigth-notes), forming a 2 + 2 + 3 pattern.
#' If we want to count each eighth-note, we can simply specify `beat = '8'` and get the `M7/8` beats counted as c(`1`, `3`, `5`).
#' However, if we want to count each short *or* long beat as a single unit, we must specify the desired pattern as a `list` of beat durations: for example, `beat = list(c("4", "4", "4."))`.
#' Let's see what these two cases look like, applied to two `M7/8` measures of straight eighth-notes:
#' 
#' ```
#' rhythm <- rep('8', 14)
#' 
#' count(rhythm, beat = '8'),
#' 
#' # output is: 1  2  3  4  5  6  7  8  9 10 11 12 13 14
#' 
#' count(rhythm, beat = list(c('4', '4', '4.')))
#' 
#' # output is: 1 1 2 2 3 3 3 4 4 5 5 6 6 6
#' ```
#'
#' To accommodate changing meters, the `beat` argument can still accept `list` of such patterns, so long as the list is the same length as `dur`.
#'
#' @section Logical start:
#' 
#' Another option is to pass the `start` argument a logical vector of the same length as the input `dur`.
#' Within each piece, the the *first* index where the `start` logical is `TRUE` is used as the beat `1`:
#' all earlier points will be negative counts, counting backwards from the start.
#' In `humdrumR`, and datapoints before the first barline record (`=`) are labeled `Bar == 0` in the `Bar` [field][fields()].
#' Thus, a common use for a `logical` `start` argument is `within(humData, count(Token, start = Bar == 1)`, which makes the downbeat of
#' the first complete bar `1`---any notes in a pickup bar are give negative counts on the timeline.
#' 
#' **Note that there is never a 'beat zero'.**
#' Beats before the starting point progress directly from `-1` to `1` (the start).
#' As a result, doing arithmetic or other math with beat "counts" can be problematic when using a `logical` `start` argument.
#' It may be better to use `round(timeline())` in cases where you want to do much math with counts.
#'
#' @param dur An input vector which is parsed for duration information using the [rhythm parser][rhythmParsing].
#' @param beat An input vector of length 1, or the same length as `dur`, which is parsed as duration information using the [rhythm parser][rhythmParsing]---or a list of vectors.
#' @param start A whole-number value from which the counting begins, or a `logical` vector of same length as `x`.
#' @param phase (length 1 or the same length as `dur`) An atomic vector which can be parsed by the [rhythm parser][rhythmParsing]. `0` is the default. The largest `phase`
#' value must be smaller than the smallest rhythmic value in `beat`.
#' @param offBeats (`logical` TRUE/False) If `FALSE`, offbeat onsets return `NA`.
#' @param groupby A `list` of vectors, of the same length as `x`, which are used to group `x` into.
#'   To function as a by-record timeline, the `groupby` list music include a *named* `Piece` and `Record` fields.
#'   Luckily, these are automatically passed by [with(in).humdrumR][withinHumdrum], so you won't need to worry about it!
#'   
#' @examples 
#' 
#' 
#' `within(humdata, count(Token, beat = TimeSignature))`
#' `within(humdata, count(Token, beat = tactus(TimeSignature)))`
#'  
#'   
#' @seealso {`count()` and `subpos()` are closely related to the [timeline()] function. The [metcount()] function applies `count()` within a metric framework.}
#' @export
count <- function(dur, beat = rational(1L), start = 1L, phase = 0,  offBeats = TRUE,  groupby = list()) {
  
  checks(start,  
         (xnumber & xlen1 & (xnotzero + "The 'first' beat to count occurs at the starting instant, so there is no 'zeroth' beat" )) |
           (xlogical & xmatch(dur)), seealso = 'the rhythm vignette')
 
  checks(phase, (xnumeric | xcharacter) & (xlen1 | xmatch(dur)))
  
  
  scaled <- scaled_timeline(dur, beat, start = if (is.logical(start)) start else rational(0L), groupby, callname = 'count')
  
  # phase
  phase_rint <- rhythmInterval(phase) 
  checks(phase_rint, argname = 'phase',
         argCheck(\(arg) all(arg < min(.unlist(scaled$values))), "must be smaller than all beats in the 'beat' argument'", 
                  \(arg) paste0(.show_values(rep(phase, length(scaled$Scale))[arg >= min(.unlist(scaled$values))]), " but 'beat' includes ", .show_vector(unique(beat)))))
  
  phase_rint <- phase_rint / scaled$Scale
  #
  mcount <- as.integer(numerator(floor((scaled$Timeline + phase_rint))))
  
  if (any(scaled$Irregular)) {
    subcounts <- Map(scaled$values, 
                     as.list(scaled$tatum),
                     f = \(bs, tat) cumsum(as.integer(numerator(bs / tat))))
    
    segments <- segments(scaled$indices)
    mcounts <- split(mcount, segments)
    phases <- split(as.numeric(phase_rint), segments)
    
    mcounts <- Map(mcounts, 
                   subcounts[attr(segments, 'values')],
                   phases,
                   f = \(m, sc, ph) {
                     m <- m - min(m)
                     m <- m + ph
                     
                     if (length(sc) == 1L) return(m)
                     
                     sub <- m %% max(sc)
                     
                     if (!offBeats) sub[!sub %in% c(0, sc)] <- NA
                     
                     sub <- findInterval(sub, sc, rightmost.closed = TRUE, left.open = FALSE)
                       
                     (length(sc) * (m %/% max(sc))) + sub
                   })
    
    mcount <- unlist(mcounts, use.names = FALSE)
    
    mcount <- delta.default(mcount)
    mcount[mcount < 0L] <- 1L
    mcount <- as.integer(sigma(mcount))
  }

  
  if (!offBeats) mcount[!scaled$Irregular & !(rational(1) %divides% (scaled$Timeline + 1))] <- NA
  
  # start at 1
  if (!is.logical(start))  mcount <- mcount + as.integer(start - (start > 0L))
  mcount[!is.na(mcount) & mcount >= 0L] <- mcount[!is.na(mcount) & mcount >= 0L] + 1L
  
  
  
  mcount
}

#' @rdname count
#' @export
subpos <- function(dur, beat = rational(1L), phase = 0, deparser = duration,  groupby = list(), ...) {
  
  scaled <- scaled_timeline(dur, beat, start = rational(0L), groupby = groupby, callname = 'subpos', sumBeats = TRUE)
  
  # phase
  phase_rint <- rhythmInterval(phase) 
  checks(phase_rint, argname = 'phase',
         argCheck(\(arg) all(arg < min(.unlist(scaled$values))), "must be smaller than all beats in the 'beat' argument'", 
                  \(arg) paste0(.show_values(rep(phase, length(scaled$Scale))[arg >= min(.unlist(scaled$values))]), " but 'beat' includes ", .show_vector(unique(beat)))))
  phase_rint <- phase_rint / scaled$Scale

  
  timeline <- (((scaled$Timeline + phase_rint) %% rational(1)) - phase_rint) * scaled$Scale
  
  if (any(scaled$Irregular)) {
    irregular <- scaled$Irregular
    indices <- scaled$indices
    irregTimeline <- Map(split(timeline[irregular], indices[irregular]),
                         scaled$values[unique(indices[irregular])], 
                         split(phase_rint[irregular] * scaled$Scale, indices[irregular]),
                         f =  \(tl, bts, ph) {
                           
                           subcount <- as.numeric(tl + ph)
                           btcounts <- cumsum(as.numeric(bts ))
                           bts <- cumsum(c(rational(0L), bts))[findInterval(subcount, btcounts, rightmost.closed = FALSE, left.open = FALSE) + 1L]
                           tl - bts
                         })
    
    timeline[unlist(split(seq_along(indices)[irregular], indices[irregular]))] <- .unlist(irregTimeline)
  }
  
  if (is.null(deparser)) timeline else deparser(timeline, ...)
}

scaled_timeline <- function(dur, beat, start, groupby, callname, sumBeats = FALSE) {
  dur <- rhythmInterval(dur)
  
  checks(beat, (xatomic | xclass(c('list', 'rational'))) & (xlen1 | xmatch(dur)))
  
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
    uniqueBeats <- list(values = beat)
  }
  
  
  dur <- dur / beat
  
  timeline <- pathSigma(dur, groupby = groupby, start = start, callname = 'count')
  
  c(list(Timeline = timeline, Scale = beat, Irregular = irregular, tatum = tatum), uniqueBeats)
}


#' @rdname count
#' @export
onbeat <- function(dur, beat = rational(1L), groupby = list(), ...) {
  subpos(dur, beat = beat, groupby = groupby, deparser = NULL) == rational(0L)
}

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

