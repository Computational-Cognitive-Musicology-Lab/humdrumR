#################################### ###
# meter S4 class #######################
#################################### ###


## meter documentation ----


## Definition, validity, initialization ####

#' Musical Meter in humdrumR
#' 
#' `HumdrumR` represents musical meter, internally, using the S4-class `meter`.
#' A `meter` is simply a collection of regular or irregular beat "levels," with each
#' level represented as [musical durations][rhythmFunctions].
#' For example, the meter 4/4 could be represented as the [recip()] beat-levels 
#' `c("1", "2", "4", "8", "16")`---that is, 
#' whole-note, half-note, quater-note, eighth-note, and sixteenth note.
#' In addition, each meter has a *tactus*---the "main" beat level.
#'
#' @details 
#' 
#' Generating meters in `humdrumR` can be done easily with the `meter()` function.
#' If you pass a `character` string of a humdrum time signature, you'll get a meter object:
#' for example, `meter("M4/4")` or `meter("M12/8")`.
#' Additive time signatures, like `meter(M2+3/8)` are also parseable.
#'
#' ## Adding or removing levels
#'
#' Time signatures leave a lot of room for interpretation.
#' Even in "standard" 4/4 time, there are a number of questions you need to consider
#' when analyzing meter:
#' 
#' + What is the fastest level you want to count? 16ths? 32nds? 64ths?
#' + Do you want to count the half-note level, which is "in between" the full measure (whole-note)
#' and the tactus quarter-note?
#' + How do you want to treat triplets or other tuplets? What is a piece uses a lot of 
#' + Do you want to consider hypermeter, *above* the measure level?
#' triplet eighth-notes?
#' 
#' Fortunately, `humdrumR` and the `meter()` function give you options to precisely specify 
#' metric levels.
#' The most transparent way is to simply pass `meter()` a `list` of duration values, like `meter(list("1", "4", "8"))`.
#' However, `meter()` includes a number of helpful arguments that can be used to quickly streamline the process of defining a meter.
#' To understand these arguments, lets first clarify some metric defitions used in `humdrumR`:
#' 
#' + *Measure*: The duration of the "measure" of the meter. I.e., the highest metric level, or
#' the least common multiple of all levels.
#' + *Hypermeter*: Metric levels above the measure indicated by the time signature.
#' + *Tactus*: The "main,", usually central, level.
#' + *Subdivision*: The level directly below the tactus.
#' + *Tick*: The smallest/fastest metric level. (The "ticks" in the grid.)
#' + *Tatum*: The smallest common denominator between a set of beats/duration. 
#'   Note that a fully specified metric grid *should* have the tick and the tatum be identical.
#'   However, we don't require that the tick be the tatum.
#' 
#' ### Meter construction arguments
#'
#' The *measure* (largest) and the *tick* (smallest) levels capture the full range of a meter.
#' The tactus is typically somewhere in the middle between these two, but this is not required.
#' The `fill.levels` argument can be used to 'fill in' levels between the measure, tactus, and tick.
#' This means that if there is room to "fit" (geometrically) duple or triple subdivisions between the higher and lower level,
#' those subdivisions are added to the meter.
#' The `fill.levels` argument must be a single `character` string, [partially matching][partialMatching] either
#' `'above'`, `'below'`, `'both'`, or `'neither'`. 
#' "Above" means fill in any gap between the tactus and the measure;
#' "Below" means fill in any gap between the tactus and the tick.
#' As a shortcut, `logical` `TRUE` or `FALSE` can be used as an alternative way of specifiying
#' `'both'` or `'neither'`, respectively.
#' For example, if you start with levels measure-tactus-tick combination of `c("1", "4", "16")`, `fill.levels = TRUE`,
#' will fill in levels at `"2"` and `"8"`.
#' 
#' The `tick` argument can be used to directly specify a tick for the meter.
#' This is especially useful if you are parsing datasets with multiple meters, but want to force them all to use 
#' the same tick value. For example, `meter(TimeSignature, tick = '32')`.
#' The `tick` argument must be a single value which can be [parsed][rhythmInteval()] as a rhythmic duration.
#' 
#' The `subdiv` argument can be used to explicitly control how the tactus is subdivided.
#' `subdiv` should be a natural number (greater than 1), to divide the tactus by.
#' Similarly, the `hyper` argument is a natural number to explicitly multiply the measure by.
#' Thus, `hyper = 2` adds a two-measure hyper meter.
#' 
#' The `tatum` argument, if `TRUE`, causes the tatum of all other metric levels to be added to the meter (if not already present).
#' This is useful, for example, if you specify a meter with a mix of duple and triple levels and want to make sure the *tick*
#' of the meter is the tatum of the levels.
#' For example, if you have levels `c("4", "6", "8")`, `tatum = TRUE` will add the level `"24"` to the meter.
#' 
#' ### Constructing meters from time signatures
#' 
#' The "meter construction arguments" above can *also* be used when reading time signatures.
#' This allows you to use time signatures from your datasets, while maintaining precise control of the metric levels used.
#' For example, the command `meter("M4/4", fill.levels = FALSE)` generates a meter with levels `c("1", "4", "16")`.
#' We could add an eighth-note level by adding `subdiv = 2`, or triple-eighth-notes with `subdiv = 3`.
#' If we *did* add triplet-eighth-notes (`subdiv = 3`), we might want to add `tatum = TRUE`, which would automatically
#' calculate the common tatum of our levels---in this case, adding forty-eighth notes to the meter.
#' On the opposite end, `hyper = 4` could add a four-measure hyper meter on top of our 4/4 bar.
#' Finally, the `tactus` argument could be used to choose another tactus, like `tactus = "2"`.
#' 
#' Any additional, unnamed arguments to `meter()` will be parsed as durations, and added as levels to the meter.
#' 
#' @param x ***A time signature or list of beats used to construct a meter.***
#' 
#' Must be a `character` string or a `list` of either `numeric` or `character` values.
#' 
#' A `character` input is parsed as a time signature, which is used to extract tactus and measure levels.
#' The contents of `list` are parsed as durations using [rhythmInterval()]: these durations become the 
#' levels of the meter.
#' Failures to parse the input will result in an error.
#' 
#' @param measure,tick,tactus ***Durations to use longest (measure), shortest (tick), or tactus levels.***
#' 
#' Must be a singleton `character` or `numeric` value, or a `list` containing a single vector of such values.
#' 
#' These are parsed as durations using [rhythmInterval()]; parse failures lead to errors.
#' 
#' @param subdiv ***Subdivisions (fractions) of the `tactus` to add to the meter.***
#' 
#' Must be positive natural numbers.
#' 
#' @param hyper ***Multiples of the `measure` duration to add to the meter.***
#' 
#' Must be positive natural numbers.
#' 
#' @param tatum ***Should the least common denominator of all levels be added to the meter?***
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' @param fill.levels ***Should "gaps" between specified levels be added to the meter?***
#' 
#' Must be a singleton `logical` or `character` value; `character` values must be `'above'`, `'below'`, `'both'`, or `'neither'`.
#' 
#' `TRUE` is shorthand for `'both'`; `FALSE` is shorthand for `'neither'`.
#' 
#' 
#'
#' @name meter
#' @family {Metric functions}
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

#' @rdname meter
#' @export
meter <- function(x, ...) UseMethod('meter')
#' @rdname meter
#' @export
meter.meter <- function(x, ...) x
#' @rdname meter
#' @export
meter.rational <- function(x, ..., measure = NULL, tactus = NULL, tick = '16', fill.levels = 'both', subdiv = NULL, hyper = NULL, tatum = FALSE) {
  levels <- list(x, ...)
  
  meter.list(levels, measure = measure, tactus = tactus, tick = tick, fill.levels = fill.levels, subdiv = subdiv, hyper = hyper, tatum = tatum)
  
}

#' @rdname meter
#' @export
meter.list <- function(x, ..., measure = NULL, tactus = NULL, tick = '16', fill.levels = 'both', hyper = NULL, subdiv = NULL, tatum = FALSE) {
  checks(fill.levels, xlen1 & (xlogical | xcharacter &  xplegal(c('both', 'above', 'below', 'neither'))))
  if (is.logical(fill.levels)) fill.levels <- c('neither', 'both')[fill.levels + 1L]
  
  levels <- lapply(x, rhythmInterval)
  spans <- lapply(levels, sum)
  
  #
  measure <- if (is.null(measure)) do.call('lcm', spans) else rhythmInterval(measure)
  tactus  <- if (is.null(tactus))  levels[[1]] else rhythmInterval(tactus)
  tick   <-  rhythmInterval(tick)

  #
  if (fill.levels != 'neither') levels <- c(levels,
                                            if (fill.levels %in% c('above', 'both')) as.list(harmonicInterpolate(sum(tactus), measure, ...)),
                                            if (!is.null(tick) & fill.levels %in% c('below', 'both')) as.list(harmonicInterpolate(tick, sum(tactus), ...)))
  
  if (!is.null(hyper)) levels <- c(levels, lapply(hyper, \(n) n * measure))
  if (!is.null(subdiv)) levels <- c(levels, lapply(subdiv, \(n) tactus / n))
  
  
  levels <- c(list(tactus, measure, tick), levels)
  levels <- levels[lengths(levels) > 0L]
  if (tatum) levels <- c(levels, tatum(.unlist(levels)))
  levels <- unique(lapply(levels, `humdrumRattr<-`, value = NULL))
  
  
  ord <- order(.unlist(lapply(levels, sum)),
               -lengths(levels), 
               decreasing = TRUE)
  new('meter', Levels = list(levels[ord]), Tactus = which(ord == 1L))
  
  
}

#' Generate duple meters
#' 
#' This function generates a [meter()] objects representing duple meters.
#' The desired number of duple levels is controlled by the `nlevels` argument.
#' The span of the meter (i.e., the highest level) is indicated by the `measure` argument.
#' Finally, the `tactus` argument indicates which level (indexe from highest to lowest)
#' is the tactus.
#' The default arguments build a 4/4 meter with levels ranging from whole-notes down
#' to sixteenth-notes, and a quarter-note tactus.
#' 
#' @param nlevels ***The number of duple levels.***
#' 
#' Must be a singleton, positive natural number
#' 
#' 
#' @param measure ***The duration of the top level of the meter.***
#' 
#' Must be a singleton `numeric` or `character` value.
#' 
#' Is parsed as a duration by [rhythmInterval()]; a failure to parse leads to an error.

#' @param tactus ***Which level is the tactus?***
#' 
#' Must be a singleton, positive natural number; must be less than or equal to `nlevels`.
#'
#' 
#' @seealso {Use the [meter()] function to create abitrary meters.}
#' @export
duple <- function(nlevels = 4, measure = 1, tactus = 3L) {
  checks(nlevels, xpnatural & xminlength(1))
  checks(tactus, xpnatural)
  
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
  
  baddenom <- !denominator %in% as.integer(2^(0:12))
  if (any(baddenom)) {
    .stop("Time signatures must use denominators which are (small) powers of 2.",
          "Your time signature includes {.values(x[baddenom])}.")
  }
  #
  beats <- Map(rational, numerator, denominator)
  denominator <- as.list(rational(1L, denominator))
  
  levels <- lapply(list(...), rhythmInterval)
  results <- Map(\(den, bts) {
    
    measure <- sum(bts)
    
    
    tactus <- if (length(bts) > 1)  bts  else den
    if (compound && 
        all(as.integer64(3L) %divides% bts@Numerator) && 
        all(den@Numerator == as.integer64(1L)) && 
        all(as.integer64(8L) %divides% den@Denominator)) {
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


# Meter extraction tools ----

## Find tatum

#' Find common denominator of beats
#' 
#' In `humdrumR`, we define a *tatum* as the greatest common denominator of 
#' a set of durations.
#' In other words, given a set of durations, the largest duration that divides all the given beats is the tatum---a common unit
#' which can measure any of the other durations
#' 
#' @details 
#' 
#' `tatum()` is a generic function; it can read any input which can be parsed by the [rhythm parser][rhythmInterval].
#' If can also take a [meter()] object or `character` string of the form `"MX/Y"`.
#' 
#' The tatum of a [meter()] is the tatum of all that meters metric levels.
#' If meters *and* durations are provided---like `tatum(c('M4/4', '6')`---, the tatum of all the meters'
#' levels *and* all the durations is computed.
#' 
#' The `deparser` argument is a [rhythm function][rhythmFunction] which controls the output format.
#' If `deparser` is `NULL`, the tatum is returned as a [rational()] value.
#'
#' @param x ***The input to compute the tatum of.***
#' 
#' Must be a [meter()] object, a singleton `character` value, or vector of either `character`, `numeric`, or [rational()] values.
#' 
#' For `character` input, valuest that match the regular expression `"^\*?M"` are parsed as a time signature using [meter()], while other strings are 
#' parsed as durations using [rhythmInterval()]. 
#' `numeric` input is also parsed using [rhythmInterval()]; parse failures result in errors.
#' 
#' @param deparser ***What output format is desired?***
#' 
#' For `character` or `meter` input, the default is [recip()]; for `numeric` input,
#' the default is [duration()].
#'
#' Must be a [rhythm function][rhythmFunction] or `NULL`.
#' 
#' 
#' 
#' @examples 
#' 
#' tatum(c("4", "8", "12"))
#' 
#' tatum(c("M4/4"))
#' 
#' tatum("M4/4", '6')
#' 
#' @family {Metric functions}
#' @export
tatum <- function(x, ...) UseMethod('tatum')
#' @rdname tatum
#' @export
tatum.meter <- function(x, deparser = recip) {
  result <- do.call('c', lapply(x@Levels, \(ls) tatum.rational(do.call('c', ls))))
  
  if (is.null(deparser)) result else deparser(result)
}
#' @rdname tatum
#' @export
tatum.character <- dofunc('x', function(x, deparser = recip) {
  checks(deparser, xclass('rhythmFunction'))
  
  timesignatures <- grepl('^\\*?M', x)
  rint <- .unlist(c(if (any(timesignatures)) tatum.meter(meter.character(x[timesignatures]), deparser = NULL),
                  if (any(!timesignatures)) rhythmInterval.character(unique(x[!timesignatures]))))
  
  result <- tatum.rational(rint)
  
  if (is.null(deparser)) result else deparser(result)
  
})
    
#' @rdname tatum
#' @export
tatum.numeric <- dofunc('x', function(x, deparser = duration) {  
  checks(deparser, xclass('rhythmFunction'))
    
  rint <- rhythmInterval(unique(x))
  result <- tatum.rational(rint)
  
  if (is.null(deparser)) result else deparser(result)
})
#' @rdname tatum
#' @export
tatum.rational <- function(x)  do.call('gcd', as.list(unique(x[!is.na(x) & x > rational(0L)])))
#' @rdname tatum
#' @export
tatum.NULL <- function(x) NULL

#' Extract levels from meters
#'
#' These functions take [meter()] objects---or values parseable as meters---and return
#' specific levels from the meter.
#' `tactus()` extracts the tactus of a meter; `measure()` extracts the length of the full measure of a meter.
#' `nbeats()` counts the number of tactus beats in the meter.
#' These functions are particularly useful as arguments to the [count and subpos][count()] functions.
#' 
#' 
#' @details 
#' 
#' By default, `tactus()` and `measure()` deparse their output as [recip()];
#' an alternative deparser (output format) can be chosen using the `deparser` argument.
#' 
#' @param x ***The input to compute the desired duration from.***
#' 
#' Must be a [meter()] object or a `character` vector.
#' 
#' A `character` input is parsed using [meter()]; failures to parse result in errors.
#' 
#' @param deparser ***What output format is desired?***
#' 
#' The default is [recip()].
#'
#' Must be a [rhythm function][rhythmFunction] or `NULL`.
#'
#' @param sep ***Seperator between irregular beat patterns.***
#' 
#' Defaults to `"+"`.
#' 
#' A singleton `character` value.
#'
#' If the tactus is a pattern of irregular beats, they are pasted together using this separator.
#'
#' 
#' @examples 
#' 
#' tactus("M4/4")
#' 
#' tactus("M6/8")
#' 
#' measure("M4/4")
#' 
#' measure("M6/8")
#' 
#' measure("M6/8", deparser = duration)
#'  
#' @family {Metric functions}
#' @export
tactus <- function(x, deparser, ...) UseMethod('tactus') 
#' @rdname tactus
#' @export
tactus.meter <- function(x, deparser = recip, sep = '+', ...) {
  
  result <- Map('[[', x@Levels, x@Tactus)
  if (is.null(deparser)) {
    .unlist(result)
  } else {
    unlist(lapply(result, \(x) paste(deparser(x, ...), collapse = sep)))
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
nbeats.character <- function(x) unlist(lapply(as.list(meter(x)), nbeats.meter))
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
#' @section Pickups:
#' 
#' Another option is to pass the `pickup` argument a logical vector of the same length as the input `dur`.
#' Within each piece/group, any block of `TRUE` values at the *beginning* of the `pickup` vector 
#' indicate a pickup.
#' The *first* index where the `pickup` logical is `FALSE` is used as the location of beat `1`:
#' all the earlier (`pickup == TRUE`) points will be negative counts, counting backwards from the start.
#' In `humdrumR`, and datapoints before the first barline record (`=`) are labeled `Bar == 0` in the `Bar` [field][fields()].
#' Thus, a common use for the `pickup` argument is `within(humData, count(Token, pickup = Bar < 1)`, which makes the downbeat of
#' the first complete bar `1` the stating point---any notes in pickup bars are give negative counts.
#' 
#' **Note that there is never a 'beat zero'.**
#' Beats before the starting point progress directly from `-1` to `1` (the start).
#' As a result, doing arithmetic or other math with beat "counts" can be problematic when using a `pickup` argument.
#' It may be better to use `round(timeline())` in cases where you want to do much math with counts.
#'
#' @param dur ***An input vector of rhythmic durations.***
#'            
#' Must be a `character` or `numeric` vector.
#'      
#' Is parsed using [rhythmInterval()]; 
#' Wherever the input can't be parsed as a duration, 
#' that element is treated as a duration of zero.
#'             
#' @param beat ***The size of "beat" (or measure) to count.***
#' 
#' Defaults to a whole-note (one measure of 4/4 time).
#' 
#' Must be a `character` or `numeric` vector, or a list of such vectors; 
#' must be a singleton or the same length as `dur`.
#' 
#' Is parsed as a duration using [rhythmInterval()]; 
#' If the input can't be parsed as a duration, the output will be all `NA`.
#'
#' @param start ***The number to start counting from.***
#' 
#' Must be a single whole-number value (either `numeric` or `integer`).
#'
#' @param phase ***The phase offset between onsets and beats.***
#' 
#' Defaults to `0`.
#'
#' Must be a `character` or `numeric` vector; must be length `1` or the same length as `dur`;
#' The duration of `phase` must be smaller than the smallest duration value in `beat`.
#' 
#' Is parsed as a duration using [rhythmInterval()]; 
#' If the input can't be parsed as a duration, an error occurs.
#' 
#' @param pickup ***Indicates which leading values in the input are pickups, if any.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or a `logical` vector of the same length as `dur`.
#'
#' @param offBeats ***Should off-beat onsets be numbered in the output, or `NA`?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a single `logical` value: an on/off switch.
#'
#' @param groupby ***Optional vectors to group by and count within.***
#' 
#' Defaults to empty `list()`.
#'
#' Must be a [list()], which is either empty or contains vectors which are all the same length as `dur`.
#' To function as a by-record timeline, the `groupby` list must include a *named* `Piece` and `Record` vectors.
#' Luckily, these are automatically passed by [with(in).humdrumR][withinHumdrum], so you won't need to worry about it!
#'   
#' @examples 
#' 
#' humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#' 
#' show(within(humData, count(Token, beat = TimeSignature, pickup = Bar < 1)))
#' 
#' show(within(humData, count(Token, beat = tactus(TimeSignature))))
#'  
#'   
#' @seealso {`count()` and `subpos()` are closely related to the [timeline()] function. The [metcount()] function applies `count()` within a metric framework.}
#' @export
count <- function(dur, beat = rational(1L), start = 1L, phase = 0,  pickup = NULL, offBeats = TRUE,  groupby = list()) {
  
  checks(dur, xcharacter | xnumber)
  checks(start, (xnumber & xlen1 & (xnotzero + "The 'first' beat to count occurs at the starting instant, so there is no 'zeroth' beat" )))
  checks(pickup, xnull | (xlogical & xmatch(dur)), seealso = 'the rhythm vignette')
  checks(phase, (xnumeric | xcharacter) & (xlen1 | xmatch(dur)))
  checks(offBeats, xTF)
  
  
  scaled <- scaled_timeline(dur, beat, rational(0L), pickup, groupby, callname = 'count')
  
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
  
  # start at 1 (skip 0)
  mcount <- mcount + as.integer(start - (start > 0L))
  mcount[!is.na(mcount) & mcount >= 0L] <- mcount[!is.na(mcount) & mcount >= 0L] + 1L
  
  
  mcount
}

#' @rdname count
#' @export
subpos <- function(dur, beat = rational(1L), phase = 0, pickup = NULL, deparser = duration, ..., groupby = list()) {
  
  checks(dur, xcharacter | xnumber)
  checks(pickup, xnull | (xlogical & xmatch(dur)), seealso = 'the rhythm vignette')
  checks(phase, (xnumeric | xcharacter) & (xlen1 | xmatch(dur)))
  checks(deparser, xclass('rhythmFunction'))
  
  scaled <- scaled_timeline(dur, beat, rational(0L), pickup, groupby, callname = 'subpos', sumBeats = TRUE)
  
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

scaled_timeline <- function(dur, beat, start, pickup, groupby, callname, sumBeats = FALSE) {
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
  
  timeline <- pathSigma(dur, groupby = groupby, start = start, pickup = pickup, callname = 'count')
  
  c(list(Timeline = timeline, Scale = beat, Irregular = irregular, tatum = tatum), uniqueBeats)
}


#' @rdname count
#' @export
onbeat <- function(dur, beat = rational(1L), groupby = list(), ...) {
  subpos(dur, beat = beat, groupby = groupby, deparser = NULL) == rational(0L)
}


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

#' Count or measure metric position
#' 
#' These functions take vectors of rhythmic duration values and
#' compute the metric position of each rhythmic onset.
#' `metlev()` identifies the metric *level* of each onset;
#' `metcount()` counts beats within a measure;
#' `metsubpos()` measures the distance
#' between an onset and the nearest metric beat.
#' `metcount()` and `metsubpos()` parallel the more general `count()` and `subpos()` functions.
#' 
#' @details 
#' 
#' Watch out! These `met...()` functions require *meter* information and their output is
#' highly dependent on how you interpret meter from scores.
#' For a full discussion of how meter can represented, parsed, and created in `humdrumR`,
#' see the [meter()] manual.
#' Effective use of the [meter()] function is essential to use of `metlev()`, `metcount()`, and `metsubpos()`.
#' 
#' 
#' ## Metric levels
#' 
#' `metlev()` identifies the "highest" (longest) metric level that each onset lands in/on:
#' For example, in 4/4 time:
#' 
#' + An onset on the downbeat is at the highest level of all, the whole-note level;
#' + An onset on beat three of the 4/4 measure is on the half-note level;
#' + Onsets on the backbeats (beats two and two) fall on the quarter-note level; 
#' + The next level down is the eighth-note level, in between each quarter-note beat;
#' + etc.
#' 
#' The `metlev()` output expresses beat levels as the duration of
#' the the level, in [recip()] format by default.
#' So the whole-note level is `"1"` and the quarter-note level (backbeats) is `"4"`.
#' You can specify a different [deparsing][rhythmDeparsing] function 
#' (like `duration()`) using the `deparser` argument.
#' (If `deparser` is `NULL`, [rational()] numbers are returned.)
#' 
#' Another option is to express the metric levels simply as natural numbers, which you can achieve
#' with the argument `value = FALSE`.
#' In this case, the top level of the meter is `1`, which each next lower-level incrementing up:
#' i.e., the quarter-note level (of 4/4) would be `3`, while the sixteenth-note level would be `5`.
#' 
#' 
#' #### (Full) 4/4 meter levels
#' 
#' |                          | 1      | &      | 2      | &      | 3      | &      | 4      | &      | 
#' | ------------------------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ | 
#' | Level ([recip()])        | `"1"`  | `"8"`  | `"4"`  | `"8"`  | `"2"`  | `"8"`  | `"4"`  | `"8"`  | 
#' | Level (`value = FALSE`)  | `1`    | `4`    | `3`    | `4`    | `2`    | `4`    | `3`    | `4`    | 
#' 
#' #### 3/4 meter levels
#' 
#' |                          | 1      | &      | 2      | &      | 3      | &      | 
#' | ------------------------ | ------ | ------ | ------ | ------ | ------ | ------ | 
#' | Level ([recip()])        | `"1"`  | `"8"`  | `"4"`  | `"8"`  | `"4"`  | `"8"`  | 
#' | Level (`value = FALSE`)  | `1`    | `3`    | `2`    | `3`    | `2`    | `3`    | 
#' 
#' #### 6/8 meter levels
#' 
#' |                          | 1      | &      | a      | 2      | &      | a      | 
#' | ------------------------ | ------ | ------ | ------ | ------ | ------ | ------ | 
#' | Level ([recip()])        | `"1"`  | `"8"`  | `"8"`  | `"4."` | `"8"`  | `"8"`  | 
#' | Level (`value = FALSE`)  | `1`    | `3`    | `3`    | `2`    | `3`    | `3`    |  
#' 
#' 
#' ## Metric counts
#' 
#' The `metcount()` function counts one beat level in a metric hierarchy, *within* the next highest level.
#' In a full duple meter, the counts are always simply `1`, `2`, `1`, `2`, etc.
#' Meters with a triple level will get `1`, `2`, `3`, etc.
#' Why level you want to count is controlled by the `level` argument, which can be either a `character` string
#' in [recip()] format or a natural number (`1` is top level, `2` is next lowest level, etc.).
#' 
#' #### (Full) 4/4 meter counts:
#' 
#' |                  | 1   | &   | 2   | &   | 3   | &   | 4   | &   |
#' | ---------------- | --- | --- | --- | --- | --- | --- | --- | --- |
#' | `"1"` (whole)    | 1   | 1   | 1   | 1   | 1   | 1   | 1   | 1   |
#' | `"2"` (half)     | 1   | 1   | 1   | 1   | 2   | 2   | 2   | 2   | 
#' | `"4"` (quarter)  | 1   | 1   | 2   | 2   | 1   | 1   | 2   | 2   |
#' | `"8"` (eighth)   | 1   | 2   | 1   | 2   | 1   | 2   | 1   | 2   | 
#' 
#' #### 3/4 meter counts:
#' 
#' |                       | 1   | &   | 2   | &   | 3   | &   |
#' | --------------------- | --- | --- | --- | --- | --- | --- |
#' | `"2."` (dotted-half)  | 1   | 1   | 1   | 1   | 1   | 1   | 
#' | `"4"` (quarter)       | 1   | 1   | 2   | 2   | 3   | 3   |
#' | `"8"` (eighth)        | 1   | 2   | 1   | 2   | 1   | 2   | 
#' 
#'
#' #### 6/8 meter counts:
#' 
#' 
#' |                         | 1   | &   | a   | 2   | &   | a   |
#' | ----------------------- | --- | --- | --- | --- | --- | --- |
#' | `"2."` (dotted-half)    | 1   | 1   | 1   | 1   | 1   | 1   | 
#' | `"4."` (dotted-quarter) | 1   | 1   | 1   | 2   | 2   | 2   |
#' | `"8"` (eighth)          | 1   | 2   | 3   | 1   | 2   | 3   | 
#' 
#' 
#' 
#' 
#' In the case of 4/4, if you want to count `1`, `2`, `3`, `4`, you'll need to make your [meter()] object
#' *not* include a half-note level.
#' 
#' 
#' #### 4/4 meter with no half-note level:
#' 
#' |                  | 1   | &   | 2   | &   | 3   | &   | 4   | &   |
#' | ---------------- | --- | --- | --- | --- | --- | --- | --- | --- |
#' | `"1"` (whole)    | 1   | 1   | 1   | 1   | 1   | 1   | 1   | 1   |
#' | `"4"` (quarter)  | 1   | 1   | 2   | 2   | 3   | 3   | 4   | 4   |
#' | `"8"` (eighth)   | 1   | 2   | 1   | 2   | 1   | 2   | 1   | 2   | 
#' 
#' You can do this with `meter('M4/4', fill.levels = 'below')`.
#' 
#' ## Metric subpositions
#' 
#' In some cases, onsets may occur which do not land on any beat specified in the meter.
#' This could be very fast beat levels (e.g., 32nd notes), triplets, or other tuplets.
#' In these cases, you might consider adding these levels to the [meter()]; for example,
#' if you want to have a 32nd-note level in 4/4, you could use `meter('M4/4', tick = '32')`.
#' For `metlev()` and `metcount()`, the `offBeats` argument can be set to `FALSE` to cause 
#' offbeat onsets to return `NA`.
#' Another option is to use `metsubpos()`, which measures how far an onset is from the nearest
#' associated beat in the meter.
#' 
#' By default, off-beat onsets are always associated with the closets previous position in any level in the meter.
#' If the `remainderSubdivides` argument is `TRUE`, off-beat onsets are associated with the previous metric level
#' which the subposition makes an even subdivision of.
#' 
#' @param dur ***An input vector of rhythmic durations.***
#'
#' Must be a `character` or `numeric` vector.
#'      
#' Is parsed using [rhythmInterval()]; 
#' Wherever the input can't be parsed as a duration, 
#' that element is treated as a duration of zero.
#' 
#' @inheritParams timeline 
#' @inheritParams count
#' 
#' @param meter ***The meter(s) to compute levels from.***
#' 
#' Defaults to a standard, five-level duple (4/4) meter.
#' 
#' Must be a [meter()] object or a `character` vector.
#' 
#' For `character` input, the string is parsed using [meter()]; a
#' failure to parse will result in an error.
#' 
#' @param value ***Should the output levels be represented as rhythmic duration values?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param level ***Which metric level should be counted?***
#' 
#' Defaults to the tactus of the `meter`.
#' 
#' A single `character` value or positive natural number.
#' 
#' A `character` string input must be a [recip()] value, matching a beat level in the meter.
#' A numeric input directly indicates a level in the meter, starting from the highest level (`1`).
#' 
#' 
#' @param remainderSubdivides ***Should off-beat onsets only be associated with beat levels that they evenly subdivide?***
#'
#' Defaults to `FALSE`.
#' 
#' A singleton `logical` value: an on/off switch.
#' 
#' @inheritSection timeline Pickups
#'
#' @examples
#' 
#' rhythm <- c('4', '8', '8', '4', '8', '16', '16','4.', '8','2')
#' 
#' metlev(rhythm)
#' metlev(rhythm, meter = 'M6/8')
#'
#' metcount(rhythm)
#' metcount(rhythm, offBeats = FALSE)
#' metcount(rhythm, meter = 'M6/8', offBeats = FALSE)
#' 
#' # chorales
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn') 
#' 
#' within(chorales, metlev(Token, pickup = Bar < 1))
#' 
#' within(chorales, metcount(Token, pickup = Bar < 1, fill.levels = 'below'))
#'
#' @seealso {The [count()] and [subpos()] functions are more basic versions of `metcount()` and `metsubpos()`,
#' based only on counting a *single* beat level, rather then a hierarchy of beat levels.}
#' @export
metlev <- function(dur, meter = duple(5), pickup = NULL, value = TRUE, offBeats = TRUE, remainderSubdivides = FALSE, deparser = recip, 
                   groupby = list(), ..., parseArgs = list()) {
  
  checks(dur, xcharacter | xnumber)
  checks(pickup, xnull | (xlogical & xmatch(dur)))
  checks(offBeats, xTF)
  checks(value, xTF)
  checks(remainderSubdivides, xTF)
  checks(deparser, xnull | xclass('rhythmFunction'))
  
  met <- .metric(dur = dur, meter = meter, pickup = pickup, groupby = groupby, parseArgs = parseArgs, 
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
metcount <- function(dur, meter = duple(5), level = tactus(meter), pickup = NULL, ...,
                     offBeats = TRUE, remainderSubdivides = FALSE, groupby = list(), parseArgs = list()) {
  
  checks(dur, xcharacter | xnumber)
  checks(offBeats, xTF)
  checks(remainderSubdivides, xTF)
  checks(pickup, xnull | (xlogical & xmatch(dur)), seealso = 'the rhythm vignette')
  
  met <- .metric(dur = dur, meter = meter, pickup = pickup, groupby = groupby, parseArgs = parseArgs, 
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

#' @rdname metlev
#' @export
metsubpos <- function(dur, meter = duple(5), pickup = NULL, deparser = duration, ...,
                     remainderSubdivides = TRUE, groupby = list(), parseArgs = list()) {
  
  checks(dur, xcharacter | xnumber)
  checks(pickup, xnull | (xlogical & xmatch(dur)))
  checks(remainderSubdivides, xTF)
  
  met <- .metric(dur = dur, meter = meter, pickup = pickup, groupby = groupby, parseArgs = parseArgs, 
                 remainderSubdivides = remainderSubdivides, callname = 'metsubpos', ...)
  
  if (is.null(deparser)) met$Remainder else deparser(met$Remainder, ...)
}


.metric <- function(dur, meter = duple(5),  groupby = list(), pickup = NULL, ..., 
                    parseArgs = list(), remainderSubdivides = TRUE, callname = '.metric') {
  
  if (length(unique(meter)) > 1L) {
    return(.metrics(dur, meter = meter, pickup = pickup,
                    groupby = groupby, parseArgs = parseArgs, remainderSubdivides = remainderSubdivides,
                    callname = callname, ...))
  }
  
  dur <- do.call('rhythmInterval', c(list(dur), parseArgs))
  
  meter <- meter(meter, ...)
  
  timeline <- pathSigma(dur, groupby = groupby, pickup = pickup, start = rational(0), callname = callname)
  
  levels <- meter@Levels[[1]]
  spans <- .unlist(lapply(levels, sum))
  nbeats <- lengths(levels)
  
  counts <- do.call('cbind', lapply(lapply(levels, \(l) if (length(l) > 1) list(l) else l), 
                                    count, 
                                    pickup = pickup, dur = dur, groupby = groupby))
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
  remainder <- c(remainders[cbind(seq_len(nrow(remainders)), lowestLevel)])
    
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

.metrics <- function(dur, meter = duple(5), pickup = NULL, groupby = list(), ..., 
                     parseArgs = list(), remainderSubdivides = TRUE, callname = '.metric') {
  
  uniqmeters <- unique(meter)
  
  mets <- lapply(seq_along(uniqmeters), 
                \(i) {
                  targets <- meter == uniqmeters[i]
                  
                  met <- .metric(dur[targets], uniqmeters[i], pickup = if (!is.null(pickup)) pickup[targets],
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

### Syncopation ----

#' Identify syncopated rhythms
#' 
#' The `syncopation()` function takes a vector of rhythmic duration values and a meter
#' and identifies which durations are syncopated, return `TRUE` for synocopations and `FALSE`
#' otherwise.
#' The output syncopation depends a lot on how meter is specified/interpreted
#' so check out the [meter()] documentation if you are looking for more control of the output.
#' 
#' @details 
#' 
#' A syncopation occurs whenever a rhythmic duration is longer than the highest
#' metric level that it lands on.
#' 
#' 
#' In some cases, we might want to restrict our attention to syncopations that occur
#' at a specific metric level: for example, "eighth-note syncpations."
#' We can proved a set of metric levels to the `levels` argument, to do this restriction.
#' The `levels` must be parsable as durations which match the levels of the [meter()].
#' 
#' @inheritParams metlev
#' @param levels ***On which metrics levels should we identify syncopations?***
#' 
#' Defaults to `"all"`.
#' 
#' Must be a non-empty `character` or `numeric` vector.
#' 
#' If `levels` is simply the singleton string `"all"`, syncopations at any
#' metric level are identified.
#' Otherwise, the `levels` are parsed with [rhythmInterval()]; fail to parse may lead to an error.
#' The parsed levels must be levels of the given [meter()].
#' 
#' @export
syncopation <- function(dur, meter = duple(5), levels = 'all', groupby = list()) {
  checks(dur, xcharacter | xnumber)
  
  levs <- metlev(dur, meter = meter, groupby = groupby, deparser = NULL)
  
  dur <- rhythmInterval(dur)
 
  syncopation <- dur > levs
  
  if (!length(levels) == 1L || levels != 'all') {
    syncopation <- syncopation & Reduce('|', lapply(as.list(rhythmInterval(levels)), `==`, e2 = levs))
  }
  
  syncopation
  
}

