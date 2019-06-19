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
#' using these \code{\link[humdrumR:tonalInterval-read]{xxx2tonalInterval}} functions.
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
#' size on the circle of fifths. When considering absolute size
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

              Cent <- getCent(.Object)
              if (any(abs(Cent) >= 1200L, na.rm = TRUE)) {
                  centOctaves <- IfElse(Cent == 0L, 
                                        0L,
                                        as.integer((Cent %/% (sign(Cent) * 1200)) * sign(Cent)))
                  .Object@Octave <- getOctave(.Object) + centOctaves 
                  Cent   <- Cent  - (1200 * centOctaves)
                 .Object@Cent   <- Cent
              }
              .Object
          }) 


######tonalInterval constructors and accessors ####


## Constructors

#' The basic constructor for \code{tonalInterval}s.
#' \code{tint} accepts integer values for octaves and fifths and numeric values for cent.
#' \code{simpletint} accepts only an integer (fifth) and creates
#' a "simple" interval---i.e., an ascending interval less than one octave.
#' @name tonalInterval
#' @export
tint <- function(octave = 0L, fifth = 0L, cent = numeric(length(octave))) {
            cent[is.na(octave) | is.na(fifth)] <- NA_real_
            new('tonalInterval', 
                Octave = as.integer(octave), 
                Fifth  = as.integer(fifth), 
                Cent   = as.numeric(cent))
}

#' @name tonalInterval
#' @export
simpletint <- function(fifth) {
 fifthNsciOct2tonalInterval(fifth, rep(4L, length(fifth)))         
}

## Accessors

#' @name tonalInterval
#' @export
getFifth  <- function(tint) tint@Fifth
#' @name tonalInterval
#' @export
getOctave <- function(tint) tint@Octave
#' @name tonalInterval
#' @export
getCent <- function(tint) tint@Cent

############# Special tonalIntervals ----
pythagorean.comma <- tint(-19, 12)


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



######tonalinterval formatting methods ####


#' @name tonalInterval
#' @export
setMethod('as.character', signature = c('tonalInterval'), 
          function(x) as.kernPitch(x))

#' @name tonalInterval
#' @export
as.double.tonalInterval <- function(x) as.double(as.ratio(x))


######tonalInterval arithmetic methods ####

####Addition

#' @name tonalInterval
#' @export
setMethod('+', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(getOctave(e1) + getOctave(e2),
                         getFifth(e1)  + getFifth(e2), 
                         getCent(e1) + getCent(e2))
          })

setMethod('-', signature = c('tonalInterval', 'missing'),
          function(e1) {
              tint(-getOctave(e1), -getFifth(e1),  -getCent(e1))
              })

#' @name tonalInterval
#' @export
setMethod('sum', signature = c('tonalInterval'),
          function(x, ..., na.rm = TRUE) {
                    x <- list(x, ...)
                    x <- do.call('c', x)
                    tint(sum(getOctave(x), na.rm), sum(getFifth(x), na.rm), sum(getCent(x), na.rm))
          })

#' @name tonalInterval
#' @export
setMethod('cumsum', signature = c('tonalInterval'),
          function(x) {
                    tint(cumsum(getOctave(x)), cumsum(getFifth(x)), cumsum(getCent(x)))
          })


####Subtraction

#' @name tonalInterval
#' @export
setMethod('diff', signature = c('tonalInterval'),
          function(x, lag = 1, differences = 1) {
                    tint(diff(getOctave(x), lag, differences), 
                         diff(getFifth(x),  lag, differences), 
                         diff(getCent(x),   lag, differences))
          })



####Division and modulo

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
            f1 <- getFifth(e1)
            f2 <- getFifth(e2)
            match_size(f1 = f1, f2 = f2, toEnv = TRUE)
            
            fifthDivs <- ifelse(f2 == 0L, 0L, f1 %/% f2)
            fifthMods <- ifelse(f2 == 0L, f1, f1 %%  f2)
            
            tint(getOctave(e1) - (getOctave(e2) * fifthDivs),
                 fifthMods)
          })

#' @name tonalInterval
#' @export
setMethod('%/%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    f1 <- getFifth(e1)
                    f2 <- getFifth(e2)
                    match_size(f1 = f1, f2 = f2, toEnv = TRUE)
                    ifelse(f2 == 0L, 0L, f1 %/% f2)
          })



######Special methods

#' @name tonalInterval
#' @export
invert <- function(tint, around = tint(0,0)) around + around - tint


#' @name tonalInterval
#' @export
simplify <- function(tint) {
          simpletint(getFifth(tint))
}

#' @name tonalInterval
#' @export
generalize <- function(tint) {
          tint %% tint(-11,7)
}

#' @export
derive <- function(x, pad = 'approach') {
    `x'` <- diff(x)
    
    if (!is.null(pad)) {
        `x'` <- if (pad == 'approach') { 
                    c(tint(rep(NA, order)), `x'`) 
                } else { 
                    c(`x'`, tint(rep(NA, order))) 
                }
    } 
    
    `x'`
}


#' @export
antiderive <- function(x, start = tint(0,0)) {
    cumsum(c(start, x))
}


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
as.semit <- function(x, ...) UseMethod('as.semit')

#' @name tonalInterval-write
#' @export
as.semit.tonalInterval <- function(x) {
                    ((getFifth(x) * 19L) + (getOctave(x) * 12L)) + (getCent(x) / 100L) 
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
    fifth2tonalname(getFifth(x), accidental.labels)
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
                    .paste(fifth2tonalname(getFifth(x), c(flat = 'b')), octave)
            }


#### As kern pitch (i.e., 'aaa', 'CC-')

#' @name tonalInterval-write
#' @export
as.kernPitch <- function(x, ...) UseMethod('as.kernPitch')

#' @name tonalInterval-write
#' @export
as.kernPitch.tonalInterval <- function(x) {
                    octaves <- sciOctave(x) - 4L
                    fifths <- getFifth(x)
                    letternames <- fifth2lettername(fifths)
                    accidentals <- fifth2accidental(fifths)
                    
                    letternames <- IfElse(octaves >= 0L, tolower(letternames), letternames)
                    repn <- IfElse(octaves >= 0, octaves + 1L, -octaves)
                    repn[repn == 0L] <- 1L
                    
                    .paste(strrep(letternames, abs(repn)), accidentals)
          }


#### As interval (i.e., "M3", "-P11")

#' @name tonalInterval-write
#' @export
as.interval <- function(x, specific = TRUE, directed = TRUE, ...) UseMethod('as.interval')

#' @name tonalInterval-write
#' @export
as.interval.tonalInterval <- function(x, specific = TRUE, directed = TRUE, 
                                      quality.labels = c(), contour.labels = c()) {
    
    setoptions(quality.labels) <- c(augment = 'A', diminish = 'd', major = 'M', minor = 'm', perfect = 'P')
    setoptions(contour.labels) <- c(Down = '-', Same = '', Up = '+')
    
    octave <- sciOctave(x) - 4
    
    fifth <- getFifth(x)
    fifth <- IfElse(octave < 0,   fifth * sign(octave), fifth) # invert interval if direction is down
    direction <- if (directed) {
        IfElse(fifth == 0, 
               rep('', length(fifth)),  
               contour.labels[c('Down', 'Same', 'Up')[sign(octave) + 2]])
    }  else {
        ""         
    }
    genericints <- fifth2genericinterval(fifth)
    octave <- abs(IfElse(octave < 0, octave + 1, octave)) # problem here?
    octaveshift <- abs(genericints + (7 * octave))
    
    if (specific) {
        qualities <- fifth2quality(fifth, quality.labels)
    } else {
        qualities <- ''
    }
    
    .paste(direction, qualities, octaveshift)
}


#### As melodic contour

#' Get melodic contours
#' 
#' @rdname MelodicContour
#' @export
as.contour <- function(tint, derive, threshold, octave, contour.labels) UseMethod('as.contour')

#' @rdname MelodicContour
#' @export
as.contour.tonalInterval <- function(tint, derive = TRUE, 
                                     threshold = 0,
                                     octave = TRUE,
                                     contour.labels = c()) {
    setoptions(contour.labels) <- c(Down = 'v', Same = '', Up = '^', Bound = "|")
    
    if (derive) tint <- diff(tint)
    
    cont <- character(length(tint))
    
    semits <- as.semit.tonalInterval(tint)
    
    targets <- abs(semits) >= threshold & !is.na(semits)
    
    cont[targets] <- contour.labels[c('Down', 'Same', 'Up')[sign(semits[targets]) + 2L]]
    
    if (octave) cont <- strrep(cont, 1L + ((abs(semits) - 1L) %/% 12)) 
    
    
    if (derive) c(contour.labels['Bound'], cont) else cont
    
}

addcontour <- function(strs, tint, threshold = 0L, octave = TRUE, contour.labels = c()) {
    cont <- as.contour.tonalInterval(tint, derive = TRUE, octave = octave,
                                     threshold = threshold, contour.labels = contour.labels)
    .paste(cont, strs)
    
}



##### As scale degree (i.e., "do", "si")


#' @name tonalInterval-write
#' @export
as.scaleDegree <- function(x, cautionary = FALSE, 
                           contour = FALSE, quality.labels = c(), ...) UseMethod('as.scaleDegree') 

#' @name tonalInterval-write
#' @export
as.scaleDegree.tonalInterval <- function(x, key = 0L, cautionary = FALSE, contour = FALSE, quality.labels = c(), ...) {
    fifths <- getFifth(x)        
    
    deg  <- as.scaleDegree.numeric(fifths, key, cautionary = FALSE, quality.labels = quality.labels)
    
    if (contour) addcontour(deg, x, threshold = 6L, ...) else deg
    
}

#' @name tonalInterval-write
#' @export
as.scaleDegree.numeric <- function(x, key = 0L, cautionary = TRUE, quality.labels = c()) {
    setoptions(quality.labels) <- c(perfect = 'n',  augment = '#',  diminish = 'b',  major = 'M',  minor = 'm')
    
    if (is.diatonicSet(key)) {
        mode <- getMode(key)
        key  <- getRoot(key)
    } else {
        mode <- 0L
        key  <- getFifth(as.tonalInterval(key))
    }
    
    x <- x - key
    
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
as.solfa <- function(x, key = 0L, contour = FALSE, ...) UseMethod('as.solfa')

solfatab <- rbind(d = c("e", "o", "i"),
                  r = c("a", "e", "i"),
                  m = c("e", "i", "y"),
                  f = c("e", "a", "i"),
                  s = c("e", "o", "i"),
                  l = c("e", "a", "i"),
                  t = c("e", "i", "y"))

#' @name tonalInterval-write
#' @export
as.solfa.tonalInterval <- function(x, key = 0L, contour = FALSE, ...) {
    fifths <- getFifth(x)        
    solfa <- as.solfa.numeric(fifths, key)
    
    if (contour) addcontour(solfa, x, threshold = 6L, ...) else solfa
}

#' @name tonalInterval-write
#' @export
as.solfa.numeric <- function(x,  key = 0L) {
          # This is the function that does the real heavy lifting of the
          # as.solfa function
          if (!is.numeric(key)) key <- getFifth(as.tonalInterval(key))
          
          x <- x - key
                    
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
as.ratio <- function(x, twelfth = 2^(19/12), ...) UseMethod('as.ratio')

#' @name tonalInterval-write
#' @export
as.ratio.tonalInterval <-  function(x, twelfth = 2^(19/12)) {
                    fifth <- getFifth(x)
                    oct   <- getOctave(x)
                    cent  <- getCent(x)
                    
                    IfElse(is.na(fifth), 
                           NA_real_, 
                           (2L ^ oct) * (twelfth ^ fifth) * 2^(cent / 1200))
          }

#### As frequency (i.e., "440")

#' @name tonalInterval-write
#' @export
as.frequency <- function(x, reference.freq = 440L, reference.tint = tint(-4,3),
                         twelfth = 2^(19/12), ...) UseMethod('as.frequency')

#' @name tonalInterval-write
#' @export
as.frequency.tonalInterval <- function(x, reference.freq = 440L, 
                                       reference.tint = tint(-4, 3), twelfth = 2^(19/12)) {
            x <- x - reference.tint
            
            ratio <- as.ratio(x, twelfth = twelfth)
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
          octshift <- as.semit(tintWith0Octave %% tint(-11, 7)) %/% 12
          
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
              
           if (!is.integer(key)) key <- getFifth(as.tonalInterval(key))
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
          
          tints
}

#' @name tonalInterval-read
#' @export
read.midi2tonalInterval <- function(n, key = NULL, melodic = FALSE) {
 read.semit2tonalInterval(n - 60L, key = key, melodic = melodic)      
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

#' @name tonalInterval-read
#' @export
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
 
          fifthNsciOct2tonalInterval(fifth, components$SciOctave)
         
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
          fifthNsciOct2tonalInterval(fifth, sciOct)
          
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
          tints * direction
          
          
}



#### From scale degree

read.scaleDegree2tonalInterval <- read.interval2tonalInterval

#### From solfege

#' @name tonalInterval-read
#' @export
read.solfa2tonalInterval <- function(str, key = 0L) {
  fifths <- solfa2fifth(str) + key
  simpletint(fifths)
  
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
          
          tint(octs, fifs)
}



#### From frequency


#### From anything!

#' @name tonalInterval
#' @export
setAs('numeric', 'tonalInterval', function(from) as.tonalInterval.numeric(from))
#' @name tonalInterval
#' @export
setAs('character', 'tonalInterval', function(from) as.tonalInterval.character(from))

#' @name tonalInterval
#' @export
as.tonalInterval <- function(...) UseMethod('as.tonalInterval')

#' @name tonalInterval-read
#' @export
as.tonalInterval.numeric <- read.semit2tonalInterval

#' @name tonalInterval-read
#' @export
as.tonalInterval.character <- regexDispatch( 'Kern Pitch' = read.kernPitch2tonalInterval,
                                             'Interval'  = read.interval2tonalInterval,
                                             'Scientific Pitch' = read.sciPitch2tonalInterval,
                                             'Solfege' = read.solfa2tonalInterval,
                                             'Decimal' = read.semit2tonalInterval)




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
#' @export
as.semit.character <- as.semit.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.midi.character <- as.midi.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.tonalname.character <- as.tonalname.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.kernPitch.character <- as.kernPitch.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.sciPitch.character <- as.sciPitch.tonalInterval %.% as.tonalInterval

#' @name humPitch
#' @export
as.interval.character <- as.interval.tonalInterval %.% as.tonalInterval

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
          
          if (!is.null(generic)) {
              if (!is.tonalInterval(generic)) generic <- as.tonalInterval(generic)
              generic <- generic - tint(-1, 1)
              inkey <- x - generic
              chromaticshift <- simpletint(inkey %/% tint(-11, 7))
              inkey <- inkey - chromaticshift
              result <- (inkey + interval) %% tint(-11, 7)
              result + generic + chromaticshift
          } else {
              x + interval
          }
}

#' @name humTranspose
#' @export
transpose.character <- regexDispatch('Kern Pitch' = as.kernPitch %.% transpose.tonalInterval %.% read.kernPitch2tonalInterval,
                                     'Interval'    = as.interval  %.% transpose.tonalInterval %.% read.interval2tonalInterval,
                                     'Scientific Pitch' = as.sciPitch %.% transpose.tonalInterval %.% read.sciPitch2tonalInterval,
                                     'Solfege'          = as.solfa %.% transpose.tonalInterval %.% read.solfa2tonalInterval,
                                     'Decimal' = as.semit %.% transpose.tonalInterval %.% read.semit2tonalInterval)



