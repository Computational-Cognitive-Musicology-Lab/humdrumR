#########################tonality

#' Tonal Data Types
#' 
#' \code{\link[humdrumR:humdrumR]{humdrumR package}} contains a number of 
#' intertwined \code{S4} data types defined to represent tonal musical information,
#' from the traditional Western persective (i.e., diatonicism).
#' The most important is the \code{\linkS4class{tonalInterval}}.
#' There are also the \code{\linkS4class{tonalCenter}} and the \code{\linkS4class{tonalHarmony}}.
#' 
#' @name humTonality
NULL


#######################################-
##################tonalInterval S4 class ####
#######################################-

#' Representation of tonal pitch information
#' 
#' This \emph{S4} class is the core tonal pitch representation in the 
#' \code{\link[humdrumR:humdrumR]{humdrumR package}}.
#' The object is used to represent tonal pitch names ("C", "G#", "Db", etc.), tonal intervals 
#' and scale degrees ("Major 3rd", "Diminished 5th", etc.), and solfege ("Do", "Re", "Fi", etc.).
#' Each pitch/interval is represented as two integers: one representing octave, the other
#' position on the circle of fifths. These values are held in the slots \code{\@Octave} 
#' and \code{\@Fifth}. 
#' The interval/pitch represented is the result of the Octaves and Perfect 5ths (actually, Perfect
#' *12ths*) in the slots added together.
#' (\code{tonalInterval} interfaces with \code{\link[humdrumR:humdrumR]{humdrumR}}'s other 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}.)
#' 
#' Tonal intervals (which are rather abstract) can be translated to more concrete 
#' pitch representations using a variety of \code{\link[humdrumR:tonalInterval-write]{as.xxx}} 
#' methods. Tonal intervals can also be created by reading various inputs
#' using these \code{\link[humdrumR:tonalInterval-read]{read.xxx}} functions.
#' 
#' In the case of fifths, it is not difficult (for learned musicians at least) to 
#' see how the numbers related to tonal pitch names (C = 0, G = 1, D = 2, A = 3, 
#' A# = 10, Bb = -2, etc.), intervals (P1 = 0, P5 = 1, M2 = 2, M6 = 3, A6 = 10, m7 = -2, etc.),
#' solfege (Do = 0, So = 1, Re = 2, La = 3, Li = 10, Te = -2, etc.).
#' It's just the circle of fifths!
#' However, how the Fifth and Octave numbers relate to specific intervals/pitches
#' is not immediately intuitive, and not is not conducive to human reading. For instance,
#' given the pair \eqn{(Octave = -3, Fifth = 3)}{(Octave = -3, Fifth = 3)}, I can quicklty recognize that this \code{tonalInterval}
#' refers to the tonal name "A" (or the interval "Major 6th", or the solfege "La", etc.).
#' However, it is difficult (even for me) to immediately recognize *which* "A" this is.
#' This is ok! And it is intentional. The representation is meant to make coding easier, not for users
#' to actually interact with. (Functions to read/write \code{tonalInteval}s to more familiar
#' representations are provided with \code{humdrumR}.)
#' 
#' The way the integers in the \code{@Octave} and \code{@Fift
#' h} slots are interpreted is as follows:
#' We can think of each \code{Octave} as corresponding to 12 semitones, and each \code{Fifth} corresponding
#' to 19 semitones (Perfect 12th). Thus, \deqn{(Octave = -1, Fifth = 1) = -12 + 19 = 7}
#' or the example from previous paragraph:
#' \deqn{(O=-3, F=3) = -12*3 + 19*3 = -36 + 57 = 21}
#' which means the "Major 6" is actually a major 13th.
#' More generally can also think of the two integers as exponents of two bases: \deqn{(O, F) = 2^O * ~3^F}.
#' If the \code{Fifth} base is actually 3, this represents a "pure" 12th, and thus
#' the resulting numbers represent \strong{Pythagorean Tuning}.
#' \deqn{2^{-1} * 3^{1} = \frac{3^1}{2^1} = \frac{3}{2} = 1.5}
#' If we instead use as our fifth base \eqn{2^{\frac{19}{12}}} we 
#' will get equal temperement:
#' \deqn{2^{-1} * {2^{\frac{19}{12}}}^1 = 2^{\frac{7}{12}}}
#' 
#' @section Vectorization:
#' \code{tonalInterval} inherits from the virtual class 
#' \code{\linkS4class{humdrumVector}}.
#' This means you can apply normal vectorized commands to \code{tonalInterval}s, 
#' and even put them in \code{\link[base:data.frame]{data.frames}}.
#' 
#' @section Arithmetic:
#' \code{tonalInterval} objects have (some) arithmetic operations defined.
#' Addition and subtraction are straightword and intuitive (i.e., \eqn{M3 + M3 = P5}).
#' 
#' Multiplication and division are slightly more limited: 
#' \href{https://en.wikipedia.org/wiki/Scalar_multiplication}{scalar multiplication}
#' is defined \emph{for integers}: \eqn{M2 * 3 = A4} 
#' (the result is always a new \code{tonalInterval}).
#' Consequently, a \code{tonalInterval} can be divided by another \code{tonalInterval} to produce
#' an integer: \eqn{M4 / M2 = 2L}, but not non-integer values.
#' This means that only simple, \href{https://en.wikipedia.org/wiki/Euclidean_division}{Euclidean} 
#' division is possible: we divide
#' a \code{tonalInterval} by another \code{tonalInterval} to get \emph{both}
#' an integer \strong{quotient} and a \code{tonalInterval} \strong{remainder}.
#' Each of these values can be useful in different ways, 
#' so in \code{R} they are calculated with two separate operators: 
#' \code{\link[base:Arithmetic]{\%\% and \%/\%}}.
#' The remainder (a.k.a., \emph{modulo}) operator (\code{\%\%}) is especially
#' useful in pitch calculations, because tonal scales can be seen as 
#' a modulus operation (modulo 7 for diatonic). For instance,
#' \code{tonalint \%\% tint(-11, 7)} will always calculate the generic 
#' version of an interval.
#' 
#' @section Relational Operators:
#' 
#' \code{tonalInnterval}s can be compared using the standard
#' \code{\link[base:Comparison]{relational operators}}---\code{==},
#' \code{!=}, \code{>}, \code{>=}, etc.
#' \code{tonalIntervals}s are equal only if their \code{Octave} and
#' \code{Fifth} slots are identical. Thus, enharmonic notes are \emph{not}
#' equal.
#' Numeric comparisons (e.g., \code{>}, 
#' \code{<=}) are based on their semitone (equal temperament) size.
#' 
#' @slot Fifth Integers representing the intervals'
#' size on the circl of fifths. When considering absolute size
#' these fifths are actually treated like pure \emph{12ths} (i.e., one 
#' fifth + one octave): 19 semitones, or \deqn{3^1}.
#' @slot Octave Integers representing the octave offset of the intervals
#' (in addition to the fifth). See details for full explanation.
#' @slot Cent Real numbers representing the cents (2^(1/1200)) offset of the interval.
#' 
#' @seealso humTonality
#' @name tonalInterval
#' @export
setClass('tonalInterval', 
         contains = 'humdrumVector',
         slots = c(Fifth = 'integer', Octave = 'integer', Cent = 'numeric')) -> tonalInterval

setValidity('tonalInterval', 
            function(object) {
              all(abs(cents)) <= 1200
            })

setMethod("initialize", 
          "tonalInterval",
          function(.Object, Fifth = 0L, Octave = 0L, Cent = 0L) {
              .Object <- callNextMethod() # call the humdrumVector initialize

              Cent <- .Object@Cent
              if (any(abs(Cent) >= 1200L, na.rm = TRUE)) {
                  centOctaves <- IfElse(Cent == 0L, 
                                        0L,
                                        as.integer((Cent %/% (sign(Cent) * 1200)) * sign(Cent)))
                  .Object@Octave <- .Object@Octave + centOctaves 
                  Cent   <- Cent  - (1200 * centOctaves)
                 .Object@Cent   <- Cent
              }
              .Object
          }) 


######tonalInterval constructors ####


## Constructors

#' The basic constructor for \code{tonalInterval}s.
#' \code{tint} accepts integer values for octaves and fifths and numeric values for cent.
#' If the octave argument is missing a "simple" interval is constructed---i.e., an ascending interval less than one octave.
#' (When appropriate, we can think of these generically as an interval with no specific octave.)
#' @name tonalInterval
#' @export
tint <- function(octave, fifth = 0L, cent = numeric(length(octave))) {
    if (missing(octave)) return(fifthNsciOct2tonalInterval(fifth, 4L))
    cent[is.na(octave) | is.na(fifth)] <- NA_real_
    new('tonalInterval', 
        Octave = as.integer(octave), 
        Fifth  = as.integer(fifth), 
        Cent   = as.numeric(cent))
}




######tonalInterval vector (and other core) methods ####




#' @name tonalInterval
#' @export
is.tonalInterval <- function(x) inherits(x, 'tonalInterval')

#' @name tonalInterval
#' @export
setMethod('is.numeric', signature = c('tonalInterval'),
          function(x) { TRUE })


######tonalInterval order/relations methods ####


order.tonalInterval <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
              
              x <- do.call('c', list(x, ...))
              order(as.semit(x), 
                    na.last = na.last,
                    decreasing = decreasing,
                    method = method
              )
          }
          
#' @name tonalInterval
#' @export
setMethod('>', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
             as.semit(e1) > as.semit(e2)
          })

#' @name tonalInterval
#' @export
setMethod('>=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
              as.semit(e1) >= as.semit(e2)
          })

#' @name tonalInterval
#' @export
setMethod('Summary', signature = c('tonalInterval'),
          function(x) {
              read.semit2tonalInterval(callGeneric(as.semit(x)))
          })

#' @name tonalInterval
#' @export
setMethod('abs', signature = c('tonalInterval'),
          function(x) {
              IfElse(x < tint(0, 0), -x, x)
          })

#' @name tonalInterval
#' @export
setMethod('sign', signature = c('tonalInterval'),
          function(x) {
              sign(as.semit(x))
          })


######tonalinterval formatting methods ####


#' @name tonalInterval
#' @export
setMethod('as.character', signature = c('tonalInterval'), 
          function(x) as.kernPitch(x))

#' @name tonalInterval
#' @export
as.double.tonalInterval <- function(x, ...) as.decimal(x, ...)


######tonalInterval arithmetic methods ####

####Addition

#' @name tonalInterval
#' @export
setMethod('+', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(e1@Octave + e2@Octave,
                         e1@Fifth  + e2@Fifth, 
                         e1@Cent + e2@Cent)
          })

#' @name tonalInterval
#' @export
setMethod('+', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- as.tonalInterval(e1, inPlace = TRUE)
              e3 <- stickyApply(`+`, e1, e2)
              
              re.place(re.as(e3))
              
          })

#' @name tonalInterval
#' @export
setMethod('-', signature = c('tonalInterval', 'missing'),
          function(e1) {
              tint(-e1@Octave, -e1@Fifth,  -e1@Cent)
              })

#' @name tonalInterval
#' @export
setMethod('-', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- as.tonalInterval(e1)
              e3 <- stickyApply(`-`, e1, e2)
              
              re.as(e3)
          })


#' @name tonalInterval
#' @export
setMethod('sum', signature = c('tonalInterval'),
          function(x, ..., na.rm = TRUE) {
                    x <- list(x, ...)
                    x <- do.call('c', x)
                    tint(sum(x@Octave, na.rm), sum(x@Fifth, na.rm), sum(x@Cent, na.rm))
          })

#' @name tonalInterval
#' @export
setMethod('cumsum', signature = c('tonalInterval'),
          function(x) {
                    tint(cumsum(x@Octave), cumsum(x@Fifth), cumsum(x@Cent))
          })


####Subtraction

#' @name tonalInterval
#' @export
setMethod('diff', signature = c('tonalInterval'),
          function(x, lag = 1, differences = 1) {
                    tint(diff(x@Octave, lag, differences), 
                         diff(x@Fifth,  lag, differences), 
                         diff(x@Cent,   lag, differences))
          })



#### Euclidean division and modulo (remainder)

#The euclidean arithmatic for tonalInterval
# does not operate on linear frequency space, but on the 2d tonal space.
#' @name tonalInterval
#' @export
setMethod('%%', signature = c('tonalInterval', 'tonalInterval'),
          # To take the modulo of a tonalInterval, it doesn't make sense 
          # to take the modulo of the Fifth and the Octave separately.
          # Rather, we take the modulo of the Fifth normally, but for the octave
          # we look at how many time the Fifth values was divided by the Fifth modulus
          # and subtract that number multiplied by the octave modulus.
          # That way the change applied to the Octave matches the one applied to the Fifth.
          function(e1, e2) {
              if (length(e1) == 0L) return(e1)
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              match_size(f1 = f1, f2 = f2, toEnv = TRUE)
              
              fifthDivs <- ifelse(f2 == 0L, 0L, f1 %/% f2)
              fifthMods <- ifelse(f2 == 0L, f1, f1 %%  f2)
              
              tint(e1@Octave - (e2@Octave * fifthDivs),
                   fifthMods)
          })

#' @name tonalInterval
#' @export
setMethod('%/%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              match_size(f1 = f1, f2 = f2, toEnv = TRUE)
              ifelse(f2 == 0L, 0L, f1 %/% f2)
          })




############################################-
####### Writing pitch representations ----
############################################-
#' Writing \code{\link[humdrumR:tonalInterval]{tonalIntervals}} to various representations
#' 
#' These functions all translate \code{\link[humdrumR:tonalInterval]{tonalIntervals}} to 
#' various pitch representations. 
#' 
#' @name tonalInterval-write
NULL

###Writing from fifths (integers) to X
##To start, we need to be able to translate the fifths
##part of every tonal interval (i.e., integers on the circle-of-fifths) into various things.

genericFifth <- function(fifth) ((fifth + 1L) %% 7L) - 1L
fifth2genericinterval <- function(fifth) c(1L, 5L, 2L, 6L, 3L, 7L, 4L)[1 + (fifth %% 7)]
fifth2solfabase       <- function(fifth) c('d', 's', 'r', 'l', 'm', 't', 'f')[1 + (fifth %% 7)]
fifth2lettername      <- function(fifth) c('C', 'G', 'D', 'A', 'E', 'B', 'F')[1 + (fifth %% 7)]
fifth2qualityN        <- function(fifth) as.integer((fifth + 1) %/% 7)

fifth2accidental <- function(fifth, accidental.labels = c()) {
          setoptions(accidental.labels) <- c(sharp = '#', flat = '-')
          list2env(as.list(accidental.labels), envir = environment())
          
          accN <- fifth2qualityN(fifth)
          
          output <- character(length(fifth)) # vector of ""
          isna <- is.na(fifth)
          output[!isna & fifth >  5] <- strrep(sharp,  abs(accN[!isna & fifth > 5]))
          output[!isna & fifth < -1] <- strrep(flat,   abs(accN[!isna & fifth < -1]))
          output[isna] <- NA_character_
          output
}
fifth2alteration <- function(fifth, mode, cautionary = TRUE, alteration.labels = c()) {
    # fifth and mode must be centered to 0 (C major)
    setoptions(alteration.labels) <- c(sharp = '#', flat = 'b', natural = 'n')
    list2env(as.list(alteration.labels), envir = environment())
    
    output <- alteration <- fifth2accidental(fifth - mode, alteration.labels[c('sharp', 'flat')])
    notna <- !is.na(alteration)
    alteration <- alteration[notna]
    fifth <- fifth[notna]
    
    accidental <- fifth2accidental(fifth, alteration.labels[c('sharp', 'flat')])
    
    alteration[alteration != '' & fifth <= 5 & fifth >= -1] <- 'n'
    alteration[fifth >  5 & fifth - mode >=  6] <- accidental[fifth >  5 & fifth - mode >= 6]
    alteration[fifth < -1 & fifth - mode <= -2] <- accidental[fifth < -1 & fifth - mode <= -2]
    
    alteration[ alteration %~% '#' & fifth < -1] <- paste0('n', accidental[ alteration %~% '#' & fifth < -1] )
    alteration[ alteration %~% 'b' & fifth >  5] <- paste0('n', accidental[ alteration %~% 'b' & fifth >  5] )
    
    if (cautionary) {
        alteration[fifth >  5 & alteration == ""] <- paste0('(', accidental[fifth >  5 & alteration == ""], ')')
        alteration[fifth < -1 & alteration == ""] <- paste0('(', accidental[fifth < -1 & alteration == ""], ')')
    }
    
    output[notna] <- alteration
    output
}
fifth2tonalname <- function(fifth, accidental.labels = c()) {
          letternames <- fifth2lettername(fifth)
          accidentals <- fifth2accidental(fifth, accidental.labels)
          IfElse(!is.na(letternames) & !is.na(accidentals),
                 .paste(letternames, accidentals), 
                 NA_character_)
}

fifth2quality <- function(fifth, quality.labels = c()) {
          setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
          
          list2env(as.list(quality.labels), envir = environment()) 
          #puts major, minor, augment, diminish, perfect into environment
    
          qualityN <- abs(fifth2qualityN(fifth))
          qualities <- if (is.null(dim(fifth))) rep(major, length(fifth)) else array(major, dim = dim(fifth))
          na <- is.na(fifth)
          qualities[na] <- NA_character_
          
          qualities[] <- IfElse(!na & fifth < 2 & fifth > -2, rep(perfect, length(fifth)), qualities)
          qualities[] <- IfElse(!na & fifth > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- IfElse(!na & fifth > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- IfElse(!na & fifth > -6 & fifth < -1, rep(minor, length(fifth)), qualities)          
          qualities[] <- IfElse(!na & fifth <= -6, strrep(diminish, abs(fifth2qualityN(fifth + 4))), qualities)     
          qualities
          
}
fifth2simpleInterval <- function(fifth, quality.labels = c()) {
          setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
    
          generic <- fifth2genericinterval(fifth)
          qualities <- fifth2quality(fifth,  quality.labels)
          
          IfElse(is.na(generic) | is.na(qualities), NA_character_, .paste(qualities, generic))
}



##### As semit (i.e., 0, -11)

#' @name tonalInterval-write
#' @export
as.semit <- function(x, calculus = 0, ...) UseMethod('as.semit')

#' @name tonalInterval-write
#' @export as.semit.tonalInterval
as.semit.tonalInterval <- function(x, calculus = 0) {
    semits <- ((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L) 
    calculus(semits, calculus)
}

#' @name tonalInterval-write
#' @export
as.midi <- function(x, ...) UseMethod('as.midi')

#' @name tonalInterval-write
#' @export
as.midi.tonalInterval <- function(x) as.semit(x) + 60L


##### As tonal name (i.e., "Eb")

#' @name tonalInterval-write
#' @export
as.tonalname <- function(x, accidental.labels = c(), ...) UseMethod('as.tonalname')

#' @name tonalInterval-write
#' @export
as.tonalname.tonalInterval <- function(x, accidental.labels = c()) {
    fifth2tonalname(x@Fifth, accidental.labels)
}

#### As scientific pitch (i.e, C4)

sciOctave <- function(tint) {
          generic <- tint %% tint(-11L, 7L)
          as.integer(as.semit(generic) %/% 12L) + 4L
}

#' @name tonalInterval-write
#' @export
as.sciPitch <- function(x, ...) UseMethod('as.sciPitch')

#' @name tonalInterval-write
#' @export
as.sciPitch.tonalInterval <- function(x) {
                    octave <- sciOctave(x)
                    .paste(fifth2tonalname(x@Fifth, c(flat = 'b')), octave)
            }


#### As kern pitch (i.e., 'aaa', 'CC-')

#' @name tonalInterval-write
#' @export
as.kernPitch <- function(x, ...) UseMethod('as.kernPitch')

#' @name tonalInterval-write
#' @export
as.kernPitch.tonalInterval <- function(x) {
                    octaves <- sciOctave(x) - 4L
                    fifths <- x@Fifth
                    letternames <- fifth2lettername(fifths)
                    accidentals <- fifth2accidental(fifths)
                    
                    letternames <- IfElse(octaves >= 0L, tolower(letternames), letternames)
                    repn <- IfElse(octaves >= 0, octaves + 1L, -octaves)
                    repn[repn == 0L] <- 1L
                    
                    .paste(strrep(letternames, abs(repn)), accidentals)
}

#### As Lilypond

#' @name tonalInterval-write
#' @export
as.lilyPitch <- function(x, ...) UseMethod('as.lilyPitch')

###NEED THIS

#### As melodic contour

#' Get melodic contours
#' 
#' @rdname MelodicContour
#' @export
as.contour <- function(x, derive, threshold, octave, contour.labels) UseMethod('as.contour')

#' @rdname MelodicContour
#' @export
as.contour.tonalInterval <- function(x, derive = TRUE, 
                                     threshold = A4,
                                     octave = TRUE,
                                     contour.labels = c()) {
    setoptions(contour.labels) <- c(Down = '-', Same = '', Up = '+', Bound = "")
    
    if (derive) x <- derive(x)
    
    contour <- character(length(x))
    
    threshold <- as.tonalInterval(threshold)
    targets <- abs(x) >= threshold & !is.na(x)
    
    contour[targets] <- contour.labels[c('Down', 'Same', 'Up')[sign(x[targets]) + 2L]]
    if (octave) contour <- .ifelse(targets, 
                                   strrep(contour, 1 + (as.semit(abs(x) - threshold) %/% 12L)), 
                                   contour)
    if (derive) contour[1] <- paste0(contour.labels['Bound'], contour[1])
    contour
    
}

addcontour <- function(strs, tint, contour.options = list()) {
    setoptions(contour.options) <- list(derive = TRUE, threshold = 0L, octave = TRUE, after = FALSE)
    contour.options <- nestoptions(contour.options, contour.labels = c("Up", "Down", "Same", "Bound"))
    
    after <- contour.options$after
    contour.options$after <- NULL
    
    contour <- do.call('as.contour.tonalInterval', c(list(tint), contour.options))
    if (after) .paste(strs, contour) else .paste(contour, strs)
    
}



#### As interval (i.e., "M3", "-P11")

#' @name tonalInterval-write
#' @export
as.interval <- function(x, derive = FALSE, generic = FALSE, contour = TRUE, ...) UseMethod('as.interval')

#' @name tonalInterval-write
#' @export
as.interval.tonalInterval <- function(x, derive = FALSE, generic = FALSE, contour = TRUE, 
                                      quality.labels = c()) {
    
    setoptions(quality.labels) <- c(augment = 'A', diminish = 'd', major = 'M', minor = 'm', perfect = 'P')
    
    if (derive) x <- derive(x)
    
    octave <- sciOctave(x) - 4L
    
    fifth <- x@Fifth
    fifth <- IfElse(octave < 0L,   fifth * sign(octave), fifth) # invert interval if direction is down

    genericints <- fifth2genericinterval(fifth)
    octave <- abs(IfElse(octave < 0, octave + 1, octave)) # problem here?
    complexgeneric <- abs(genericints + (7 * octave))
    
    intervals <- .paste(if (!generic) fifth2quality(fifth, quality.labels), 
                        complexgeneric)
    
    ## contour
    if (logicalOption(contour)) {
        setoptions(contour) <- c(derive = FALSE)
        addcontour(intervals, x, contour.options = contour)
    } else {
        intervals 
    }
    
}



##### As scale degree (i.e., "do", "si")


#' @name tonalInterval-write
#' @export
as.scaleDegree <- function(x, key, cautionary, generic, contour, quality.labels = c(), ...) UseMethod('as.scaleDegree') 

#' @name tonalInterval-write
#' @export
as.scaleDegree.tonalInterval <- function(x, key = 0L, cautionary = TRUE, generic = FALSE, contour = FALSE, quality.labels = c(), ...) {
    deg  <- as.scaleDegree.integer(x@Fifth, key, cautionary, generic, quality.labels = quality.labels)
    
    if (logicalOption(contour)) {
        setoptions(contour) <- c(threshold = 6L)
        addcontour(deg, x, contour.options = contour)
    } else {
        deg
    }
    
}

#' @name tonalInterval-write
#' @export
as.scaleDegree.integer <- function(x, key = 0L, cautionary = TRUE,
                                   generic = FALSE, quality.labels = c()) {
    
    setoptions(quality.labels) <- c(perfect = 'n',  augment = '#',  diminish = 'b',  major = 'M',  minor = 'm')
    
    if (is.diatonicSet(key)) {
        mode <- getMode(key)
        key  <- getRoot(key)
    } else {
        mode <- 0L
    }
    
    x <- x - key
    
    if (generic) x <- genericFifth(x)
    
    intervals <- fifth2simpleInterval(x, quality.labels = quality.labels)
    if (!cautionary) {
        x <- x - mode
        inkey <- !is.na(x) & x <= 5L & x >= -1L
        intervals[inkey] <-  stringi::stri_sub(intervals[inkey], from = 2L)
    }
    intervals
    
}

##### As solfege (i.e., "do", "si")
#' @name tonalInterval-write
#' @export
as.solfa <- function(x, key, generic, contour, ...) UseMethod('as.solfa')

solfatab <- rbind(d = c("e", "o", "i"),
                  r = c("a", "e", "i"),
                  m = c("e", "i", "y"),
                  f = c("e", "a", "i"),
                  s = c("e", "o", "i"),
                  l = c("e", "a", "i"),
                  t = c("e", "i", "y"))

#' @name tonalInterval-write
#' @export
as.solfa.tonalInterval <- function(x, key = 0L, generic = FALSE, contour = FALSE, ...) {
    
    solfa <- as.solfa.numeric(x@Fifth, key, generic)
    
    if (logicalOption(contour)) {
        setoptions(contour) <- c(threshold = 6L)
        addcontour(solfa, x, contour.options = contour)
    } else {
        solfa 
    }
}

#' @name tonalInterval-write
#' @export
as.solfa.numeric <- function(x,  key = 0L, generic = FALSE) {
          # This is the function that does the real heavy lifting of the
          # as.solfa function
          if (!is.numeric(key)) key <- as.tonalInterval(key@Fifth)
          
          x <- x - key
          if (generic) x <- genericFifth(x)
                    
          bases <- fifth2solfabase(x)
          qualityN <- fifth2qualityN(x)
          qualSign <- sign(qualityN)    
                
          tails <- solfatab[cbind(fifth2genericinterval(x), 
                                 qualSign + 2)]
          
          residualQualityN <- qualityN - qualSign
          
          accidentals <- strrep(c('-', '', '#')[qualSign + 2], abs(residualQualityN))
          
          .paste(bases, tails, accidentals)
          
}

#### As ratio (i.e., "3/2")

#' @name tonalInterval-write
#' @export
as.ratio <- function(x,  ..., sep) UseMethod('as.ratio')

#' @name humCoerce
#' @export
as.ratio.numeric <- function(n, sep = '/') {
    frac <- numeric2fraction(n)
    .paste(frac$Numerator, frac$Denominator, sep = sep)
}

#' @name tonalInterval-write
#' @export
as.ratio.tonalInterval <-  function(x, twelfth = 2^(19/12), sep = '/') {
    frac <- numeric2fraction(as.decimal(x, twelfth = twelfth))
    
    .paste(frac$Numerator, frac$Denominator, sep = sep)
    
}

#' @name humCoerce
#' @export
as.fraction <- as.ratio


#### As decimal (i.e, "1.5")
#' @name humCoerce
#' @export
as.decimal <- function(...) UseMethod('as.decimal')

#' @name tonalInterval-write
#' @export
as.decimal.tonalInterval <-  function(x, twelfth = 2^(19/12)) {
    fifth <- x@Fifth
    oct   <- x@Octave
    cent  <- x@Cent
    
    IfElse(is.na(fifth), 
           NA_real_, 
           (2 ^ oct) * (twelfth ^ fifth) * 2^(cent / 1200))
}



#### As frequency (i.e., "440")

#' @name tonalInterval-write
#' @export
as.frequency <- function(x, reference.freq = 440L, reference.tint = tint(-4,3), twelfth = 2^(19/12), ...) UseMethod('as.frequency')

#' @name tonalInterval-write
#' @export
as.frequency.tonalInterval <- function(x, reference.freq = 440L, 
                                       reference.tint = tint(-4, 3), twelfth = 2^(19/12)) {
            x <- x - reference.tint
            
            ratio <- as.decimal(x, twelfth = twelfth)
            attributes(ratio) <- NULL
            
            reference.freq * ratio
          }


#####################################-
#### Reading pitch representations ----
#######################################-
#' Reading \code{\link[humdrumR:tonalInterval]{tonalIntervals}} from various representations
#' 
#' These functions all translate other pitch representations
#' into \code{\link[humdrumR:tonalInterval]{tonalIntervals}}.
#' 
#' These functions all assume that thheir string input is a well-formed
#' example of the target pitch representation, with no extra strings.
#' (The \code{\link[humdrumR:regexDispatch]{regex dispatch}} functions can be 
#' used to clean/filter inputs into these functions.
#' 
#' @name tonalInterval-read
NULL


#### Reading from X to fifths (integers)
##To start we need to be able to read various things
##and translate them to fifths (i.e., integers on the circle-of-fifths).
genericinterval2fifth   <- function(ints) {
          ints[ints ==  0] <- NA
          ints[ints == -1] <- 1
          simpleints <- (abs(ints - sign(ints)) %% 7L) # from 0
          fifths <- c(0, 2, 4, 6, 1, 3, 5)[simpleints + 1]
          ifelse(ints > 0, fifths, 7 - fifths) %% 7L
          
}
lettername2fifth <- function(ln) match(toupper(ln), c('F', 'C', 'G', 'D', 'A', 'E', 'B')) - 2L
accidental2fifth <- function(acc, accidental.labels = c()) {
          setoptions(accidental.labels) <- c(sharp = '#', flat = '-', flat = 'b')
          
          sharps <- colSums(do.call('rbind', 
                                    lapply(accidental.labels[names(accidental.labels) == 'sharp'], 
                                           stringi::stri_count_fixed, str = acc)))
          flats <- colSums(do.call('rbind', 
                                    lapply(accidental.labels[names(accidental.labels) == 'flat'], 
                                           stringi::stri_count_fixed, str = acc)))
          as.integer((7 * sharps) - (7 * flats))
          
}
tonalname2fifth <- function(tn) {
    fifth <- lettername2fifth(stringr::str_sub(tn, start = 0L, end = 1L))
    acc <- accidental2fifth(stringr::str_sub(tn, start = 2L),
                            accidental.labels = c())
    
    fifth + acc
}
solfa2fifth <- function(solfa) {
 base <- stringr::str_sub(solfa, start = 0L, end = 1L)        
 tail <- stringr::str_sub(solfa, start = 2L, end = 2L)        
 acc  <- stringr::str_sub(solfa, start = 3L, end = 3L)        
 
 isna <- is.na(base)
 
 basefifth <- match(base, c('f', 'd', 's', 'r', 'l', 'm', 't')) - 2L
 stackedbases <- solfatab[base[!isna], , drop = FALSE]
 
 tailfifth <- (apply(cbind(tail[!isna], stackedbases), 1, function(row) which(row[-1] == row[1])) - 2L) * 7L
 
 
 
 accfifth <- accidental2fifth(acc, c())
 fifth <- basefifth + accfifth
 fifth[!isna] <- fifth[!isna] + tailfifth 
 
 unname(fifth)
}

fifthNsciOct2tonalInterval <- function(fifth, sciOct) {
          tintWith0Octave <- tint(o = numeric(length(fifth)), f = fifth)
          octshift <- as.semit(tintWith0Octave %% tint(-11, 7)) %/% 12L
          
          tint(sciOct - 4 - octshift, fifth)
}


######### From semit/midi

#' @name tonalInterval-read
#' @export
read.semit2tonalInterval <- function(n, key = NULL, melodic = FALSE) {
          
          wholen <- as.integer(n)
          
          pitchclass <- wholen %% 12L
          
          fifths <- IfElse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- (wholen - (fifths * 19)) %/% 12
          tints <- tint(octaves, fifths)
          
          ##
          if (!is.null(key)) {
              
           if (!is.integer(key)) key <- as.tonalInterval(key@Fifth)
           fifths <- fifths - key
           tints[fifths > 8 & !is.na(fifths)]  <- tints[fifths > 8 & !is.na(fifths)] - pythagorean.comma
           tints[fifths < -5& !is.na(fifths)] <- tints[fifths < -5 & !is.na(fifths)] + pythagorean.comma
          }
          
          if (melodic) {
           chromatic <- fifths > 5 | fifths < -1
           ints <- c(diff(tints), tint(0, 0)) # tint(0,0) is just padding

           isA1 <- ints == tint(-11, 7)
           isD1 <- ints == tint(11, -7)
           
           tints[which(chromatic & isA1)] <- tints[which(chromatic & isA1)] + pythagorean.comma
           tints[which(chromatic & isD1)] <- tints[which(chromatic & isD1)] - pythagorean.comma
                    
          }
          
          tints # %re.as% 'as.semit.tonalInterval'
}

#' @name tonalInterval-read
#' @export
read.midi2tonalInterval <- function(n, key = NULL, melodic = FALSE) {
 midi <- read.semit2tonalInterval(n - 60L, key = key, melodic = melodic) 
 
 midi %re.as% 'as.midi.tonalInterval'
        
}

######## From kern pitch

read.kernPitch2components <- function(str) {
          letters     <- stringi::stri_extract_first(str, regex = '([A-Ga-g])\\1*')
          
          accidentals <- stringi::stri_extract_first(str, regex = '([#-])\\1*')
          accidentals[is.na(accidentals)] <- ''
          
          nletters <- nchar(letters)
          upper    <- letters == toupper(letters)
          sciOct <- IfElse(upper, 0 - nletters, nletters - 1) + 4
          
          letters <- toupper(substr(letters, 0, 1))
          
          list(Letters = letters, 
               Accidentals = accidentals,
               SciOctave = sciOct)
}

read.kernPitch2sciPitch <- function(str) {
          components <- read.kernPitch2components(str)
          
          components$Accidentals <- gsub('-', 'b', components$Accidentals)
          do.call('.paste', components) 
}

#' @name tonalInterval-read
#' @export
read.kernPitch2tonalInterval <- function(str) {
          components <- read.kernPitch2components(str)
          
          fifth <- with(components, 
                        lettername2fifth(Letters) + 
                            accidental2fifth(Accidentals,   
                                             accidental.labels = c(flat = '-')))
 
          fifthNsciOct2tonalInterval(fifth, components$SciOctave) %re.as% 'as.kernPitch.tonalInterval'
         
}




######## From scientific pitch

#' @name tonalInterval-read
#' @export
read.sciPitch2tonalInterval <- function(str) {
          letters     <- stringi::stri_extract_first(str, regex = '[A-G]')
          accidentals <- stringi::stri_extract_first(str, regex = '([#b])\\1*')
          accidentals[is.na(accidentals)] <- ''
          
          sciOct      <- as.numeric(stringi::stri_extract_first(str, regex = '[-+]?[0-9]+'))
          if (all(is.na(sciOct))) sciOct <- rep(4, length(sciOct))
          
          fifth <- lettername2fifth(letters) + accidental2fifth(accidentals, 
                                                                accidental.labels = c(flat = 'b'))
          
          fifthNsciOct2tonalInterval(fifth, sciOct) %re.as% 'as.sciPitch.tonalInterval'
          
}


#### From interval

#' @name tonalInterval-read
#' @export
read.interval2tonalInterval <- function(str) {
          direction <- stringi::stri_extract_first(str, regex = "^[-+]")
          quality   <- stringi::stri_extract_first(str, regex = "[MmP]|[AaDd]+")
          generic   <- as.integer(stringi::stri_extract_first(str, regex = "[1-9][0-9]*"))
          
          
          ## Fifth
          genericfifth <- genericinterval2fifth(generic)
          genericfifth[genericfifth == 6] <- (-1) # because P4 is -1 not 6
          qualshift <- sapply(quality,
                              function(qual) {
                                        qual1 <- stringr::str_sub(qual, end = 1L)
                                        switch(qual1,  P = 0, M = 0, m = -7, d = -7, D = -7, A = 7, a = 7) *  nchar(qual)
                              }) -  ifelse(grepl('^d', quality) & genericfifth > 1, 7, 0)
          
          fifth <- qualshift + genericfifth
          
          ## Octave
          octaveshift <- ((abs(generic) - 1) %/% 7)
          tints <- fifthNsciOct2tonalInterval(fifth, octaveshift + 4L)
          
          ## Direction
          
          direction <- 2 * ((is.na(direction) | direction == '+') - 0.5)
          
          (tints * direction) %re.as% 'as.interval.tonalInterval'
          
          
}



#### From scale degree

read.scaleDegree2tonalInterval <- read.interval2tonalInterval

#### From solfege

#' @name tonalInterval-read
#' @export
read.solfa2tonalInterval <- function(str, key = 0L) {
  fifths <- solfa2fifth(str) + key
  tint( , fifths) %re.as% 'as.solfa.tonalInterval'
  
}


#### From ratio

#' @name tonalInterval-read
#' @export
read.ratio2tonalInterval <- function(str, twelfth = 3) {
          if (is.character(str)) {
                    slashes <- grepl("[/%]", str)
                    num <- numeric(length(str))
                    num[!slashes] <- as.numeric(str[!slashes])
                    num[slashes] <- sapply(str[slashes], function(s) eval(parse(text = s)))
          } else {
                    num <- str         
          }
          
          fracs <- MASS::fractions(num)
          fracs <- strsplit(attr(fracs, 'fracs'), split = '/')
          numerators   <- as.integer(sapply(fracs, '[', 1))
          denominators <- as.integer(sapply(fracs, '[', 2))
          denominators[is.na(denominators)] <- 1L
          
          fracs <- cbind(numerators, denominators)
          
          # octaves
          octaves    <- log(fracs, base = 2)
          twelfths   <- log(fracs, base = twelfth)
          octaves [ , 2] <- -octaves[ , 2]
          twelfths[ , 2] <- -twelfths[ , 2] 
          is.octave  <- is.whole(octaves) & octaves != 0
          is.twelfth <- is.whole(twelfths) & twelfths != 0
          
          octs <- fifs <- numeric(nrow(fracs))
          # easy "pure" matches
          pure <- rowSums(is.whole(twelfths) | is.whole(octaves)) == 2
          pure12 <- rowSums(is.twelfth) > 0
          fifs[pure12] <- twelfths[pure12,][is.twelfth[pure12, ]]
          
          # 
          pure8 <- rowSums(is.octave) > 0
          octs[pure8] <- octaves[pure8,][is.octave[pure8, ]]
          
          # approximations
          # round to nearest fifth value
          if (any(!pure)) {
            impure <- !is.twelfth & !is.octave & twelfths != 0
            twelfths <- twelfths + log(2^(octaves), base = 3)
            
            fifs[rowSums(impure) > 0] <- round(twelfths[impure])
          }
          
          tint(octs, fifs) %re.as% 'as.ratio.tonalInterval'
}


#### From decimal

#' @name tonalInterval-read
#' @export
read.decimal2tonalInterval <- function(float, twelfth = 3, centmargin = 10) {
    octrange <- attr(centmargin, 'octrange')
    if (is.null(octrange)) octrange <- 5L
    if (octrange > 150) stop(.call = FALSE,
                            "read.decimal2tonalInterval can't find a note corresponding exactly to this frequency/ratio. ",
                            "Try raising the centmargin.")
    
    #
    octs <- -octrange:octrange
    
    allocts <- do.call('cbind', lapply(2^octs, '*', float))
    logged <- log(allocts, twelfth)
    
    whole <- round(logged)
    remain <- logged - whole
    
    whichhit  <- applyrows(remain, function(row) {
        hitind <- which(abs(row) == min(abs(row)))
        hitind[which.min(abs(octs[hitind]))]
    })
    
    fifth  <- whole[cbind(seq_along(float), whichhit)]
    remain <- remain[cbind(seq_along(float), whichhit)]
    octave <- round(log(float / twelfth ^ fifth, 2))
    
    # cents
    cents <- log(twelfth^remain,2) * 1200
    
    accept <- abs(cents) < centmargin
    
    output <- tint(octave, fifth, cent = cents)
    if (any(!accept)) output[!accept] <- Recall(float[!accept], twelfth, 
                                                data.table::setattr(centmargin, 'octrange', octrange + 5L))
    output
}

#### From frequency

#' @name tonalInterval-read
#' @export
read.frequency2tonalInterval <- function(float, reference.freq = 440L, 
                                         reference.tint = tint(-4, 3), twelfth = 3,
                                         centmargin = 10) {
    
    read.decimal2tonalInterval(float / reference.freq, twelfth, centmargin = 10) + reference.tint
}

#### From anything!

#' @name tonalInterval
#' @export
setAs('integer', 'tonalInterval', function(from) as.tonalInterval.integer(from))
#' @name tonalInterval
#' @export
setAs('numeric', 'tonalInterval', function(from) as.tonalInterval.numeric(from))
#' @name tonalInterval
#' @export
setAs('character', 'tonalInterval', function(from) as.tonalInterval.character(from))

#' @name tonalInterval
#' @export
as.tonalInterval <- function(...) UseMethod('as.tonalInterval')

as.tonalInterval.tonalInterval <- force

#' @name tonalInterval-read
#' @export
as.tonalInterval.integer <- read.semit2tonalInterval

#' @name tonalInterval-read
#' @export
as.tonalInterval.numeric <- read.decimal2tonalInterval

#' @name tonalInterval-read
#' @export
as.tonalInterval.character <- regexDispatch( 'kernPitch' = read.kernPitch2tonalInterval,
                                             'sciPitch' = read.sciPitch2tonalInterval,
                                             'interval'  = read.interval2tonalInterval,
                                             'solfege' = read.solfa2tonalInterval,
                                             'decimal' = read.semit2tonalInterval)



#############################################################################-
#### Translating pitch representations ----
####################################################################-

#' Pitch translations
#' 
#' These functions translate various pitch representations
#' between each other. Using the \code{humdrumR} \code{\link[humdrumR:regexDispatch]{regular-expression dispatch system}}
#' they will even (automatically) read parts of a string which represent a pitch,
#' and translate only that part (leaving the rest of the string unchanged).
#' They all have an option \code{inPlace} which can be set to \code{FALSE}
#' if you want them to discard non-pitch parts of the string(s).
#' 
#' Under the hood, these functions use the \code{\link{humdrumR}} 
#' \code{\link[humdrumR:tonalInterval]{tonalInterval}} \code{S4} class as the 
#' fundamental, \emph{lingua franca} representation of pitch.
#' 

#' @name humPitch
#' @export as.semit.character
as.semit.character <- re.place %.% as.semit.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.semit.numeric <- as.integer

#' @name humPitch
#' @export
as.midi.character <- as.midi.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.midi.numeric <- as.integer

#' @name humPitch
#' @export
as.tonalname.character <- as.tonalname.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.kernPitch.character <- as.kernPitch.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.kernPitch.integer <- as.kernPitch.tonalInterval %.% as.tonalInterval.integer

#' @name humPitch
#' @export
as.kernPitch.numeric <- as.kernPitch.tonalInterval %.% as.tonalInterval.numeric

#' @name humPitch
#' @export
as.sciPitch.character <- as.sciPitch.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.interval.character <- as.interval.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.scaleDegree.character <- as.scaleDegree.tonalInterval %.% as.tonalInterval


#' @name humPitch
#' @export
as.solfa.character <- as.solfa.tonalInterval %.% as.tonalInterval


#' @name humPitch
#' @export
as.frequency.character <- as.frequency.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.ratio.character <- as.ratio.tonalInterval %.% as.tonalInterval


#################################################-
######Special pitch functions ####
##################################################-

######Special methods

#' @name tonalInterval
#' @export invert invert.tonalInterval invert.default
invert <- function(tint, around = Unison) UseMethod('invert')
invert.tonalInterval <- function(tint, around = tint(0,0)) (around + around - tint)
invert.default <- re.as %.% invert.tonalInterval %.% as.tonalInterval


## Partition simple + octave = complex

#' Tonal interval partitions
#' 
#' @name tonalIntervalparts
#' @export simplepart simplepart.tonalInterval simplepart.default
#' @export octavepart octavepart.tonalInterval octavepart.default
#' @export is.simple is.simple.tonalInterval is.simple.default
octavepart <- function(tint, roundingMethod) UseMethod('octavepart')
simplepart <- function(tint, roundingMethod) UseMethod('simplepart')
octavepart.tonalInterval <- function(tint, roundingMethod = floor) {
    octshift <- as.integer(roundingMethod(as.semit.tonalInterval(tint) / 12))
    tint(octshift, 0L)
}
simplepart.tonalInterval <- function(tint, roundingMethod = floor) {
    octavepart <- octavepart.tonalInterval(tint, roundingMethod)
    tint - octavepart
}
simplepart.default <- re.as %.% simplepart.tonalInterval %.% as.tonalInterval
octavepart.default <- re.as %.% simplepart.tonalInterval %.% as.tonalInterval

is.simple <- function(tint) UseMethod('is.simple')
is.simple.tonalInterval <- function(tint) {
    semit <- abs(as.semit.tonalInterval(tint))
    semit < 12
}
is.simple.default <- is.simple %.% as.tonalInterval

### Partition generic + quality = specific

#' @name tonalIntervalparts
#' @export genericpart genericpart.tonalInterval genericpart.default
#' @export alterationpart alterationpart.tonalInterval alterationpart.default
genericpart <- function(tint, key) UseMethod('genericpart')
alterationpart <- function(tint, key) UseMethod('alterationpart')
genericpart.tonalInterval <- function(tint, key = dset(0L, 0L)) {
    key <- as.diatonicSet(key)
    mode <- tint( , key@Mode - key@Root)
    key  <- tint( , key@Root)

    gtint <- tint - key 
    
    gtint <- ((gtint + P5 - mode ) %% A1) - P5 + mode
    # offset by P5 because F (-1) is natural, not F# (6)
    
    gtint
}
alterationpart.tonalInterval <- function(tint, key = dset(0L, 0L)) {
    key <- as.diatonicSet(key)
    gtint <- genericpart.tonalInterval(tint, key)
    tint - gtint - tint( , key@Root)
}
genericpart.default <- re.as %.% genericpart.tonalInterval %.% as.tonalInterval
alterationpart.default <- re.as %.% alterationpart.tonalInterval %.% as.tonalInterval

### Partition enharmonic + comma = enharmonic

#' @rdname tonalIntervalparts
#' @export enharmonicpart enharmonicpart.tonalInterval 
#' @export commapart commapart.tonalInterval
enharmonicpart <- function(tint, wolf, key) UseMethod('enharmonicpart')
commapart      <- function(tint, wolf, key) UseMethod('commapart')
enharmonicpart.tonalInterval <- function(tint, wolf = "B#", key = dset(0L, 0L)) {
    key <- as.diatonicSet(key)
    key <- tint( , key@Root)
    tint <- tint - key
    
    comma <- commapart.tonalInterval(tint, wolf, key = dset(0L, 0L))
    
    (tint - comma) + key
    
}
commapart.tonalInterval <- function(tint, wolf = "B#", key = dset(0L, 0L)) {
    key <- as.diatonicSet(key)
    key <- tint( , key@Root)
    
    tint <- tint - key
    
    
    wolf <- as.tonalInterval(wolf)
    wolf[wolf@Fifth < 0L] <- wolf[wolf@Fifth < 0L] + pythagorean.comma
    
    tint <- (wolf - P5 - (tint))
    
    (tint %/% pythagorean.comma) * -pythagorean.comma
    
    
    
    
    
    
}
# genericpart.default <- re.as %.% genericpart.tonalInterval %.% as.tonalInterval
# qualitypart.default <- re.as %.% qualitypart.tonalInterval %.% as.tonalInterval

#' Transpose tonalIntervals
#' 
#' This function transposes tonalIntervals by other tonal intervals.
#' By default, does real transposition.
#' However, if a \code{key} argument is specified, tonal transposition
#' takes place in that (major) key.
#' @name humTranspose
#' @export
transpose <- function(x, interval = tint(0,0), generic = NULL, ...) UseMethod('transpose')

#' @name humTranspose
#' @export
transpose.tonalInterval <- function(x, interval = tint(0,0), generic = NULL) {
          if (!is.tonalInterval(interval)) interval <- as.tonalInterval(interval)
          
          output <- if (!is.null(generic)) {
              if (!is.tonalInterval(generic)) generic <- as.tonalInterval(generic)
              generic <- generic - tint(-1, 1)
              inkey <- x - generic
              chromaticshift <- tint( , inkey %/% tint(-11, 7))
              inkey <- inkey - chromaticshift
              result <- (inkey + interval) %% tint(-11, 7)
              result + generic + chromaticshift
          } else {
              x + interval
          }
          
          output
}

#' @name humTranspose
#' @export
transpose.character <- re.as %.% transpose.tonalInterval %.% as.tonalInterval

# transpose.character <- regexDispatch('KernPitch' = as.kernPitch %.% transpose.tonalInterval %.% read.kernPitch2tonalInterval,
                                     # 'Interval'    = as.interval  %.% transpose.tonalInterval %.% read.interval2tonalInterval,
                                     # 'ScientificPitch' = as.sciPitch %.% transpose.tonalInterval %.% read.sciPitch2tonalInterval,
                                     # 'Solfege'          = as.solfa %.% transpose.tonalInterval %.% read.solfa2tonalInterval,
                                     # 'Decimal' = as.semit %.% transpose.tonalInterval %.% read.semit2tonalInterval)


############# Known tonalIntervals ----
#' @name tonalInterval
#' @export dd16 dd9 dd13 dd6 dd10 dd14 dd3 dd7 dd11 dd15 dd4 dd8 dd12 d16
#' @export dd5 d9 d13 d2 d6 d10 d14 d3 d7 d11 d15 d4 d8 d12 d5 m9 m13 m2 m6
#' @export m10 m14 m3 m7 P11 P15 P4 P8 P12 P1 P5 M9 M13 M2 M6 M10 M14 M3 M7
#' @export A11 A4 A8 A12 A1 A5 A9 A13 A2 A6 A10 A14 A3 A7 AA11 d2 AA4 AA8 AA12
#' @export AA1 AA5 AA9 AA13 AA2 AA6 AA10 AA3 AA7 dd2 Unison pythagorean.comma
for (i in -30:32) {
    for (j in -19:19) {
        t <- tint(i, j)
        if (t >= tint(0, 0) & t <= tint(2, 0)) {
            curint <- as.interval(t, contour = FALSE, generic = FALSE)
            assign(curint, t)
        }
    }
}
Unison <- tint(0, 0)
pythagorean.comma <- tint(-19,12)

