
###############tonalInterval S4 class ----

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
#' The way the integers in the \code{@Octave} and \code{@Fifth} slots are interpreted is as follows:
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
#' The \code{tonalInterval} class has all the \code{\link[tonalInterval-asvector]{basic vector methods}} 
#' defined for it function like an \code{R} atomic vector (\code{\link[base]{c}}, \code{\link[base]{[}}, 
#' \code{\link[base]{length}}, etc.).
#' (Each slot can actually hold a vector of integers representing
#' Octaves/Fifths). This means you can apply normal vectorized commands to it, and even put
#' them in \code{\link[base:data.frame]{data.frames}}.
#' However, \code{R} is limited in this regard---users can't define
#' \code{S4} classes that really act like \code{R} atomics---, so you may 
#' run in to problems if you take this too far.
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
#' \code{tonalInnterval}s can be compared using the standard
#' \code{\link[base:Comparison]{relational operators}}---\code{==},
#' \code{!=}, \code{>}, \code{>=}, etc.
#' \code{tonalIntervals}s are equal only if their \code{Octave} and
#' \code{Fifth} slots are identical. Numeric comparisons are (i.e., \code{>}, 
#' \code{<=}) based on their semitone (equal temperament) size.
#' 
#' @slot Fifth An integer representing the intervals
#' size on the circle of fifths. When considering absolute size
#' these fifths are actually treated like pure \emph{12ths} (i.e., one 
#' fifth + one octave): 19 semitones, or \deqn{3^1}.
#' @slot Octave An integer representing the octave offset of the interval
#' (in addition to the fifth). See details for full explanation.
#' 
#' @export
setClass('tonalInterval', slots = c(Fifth = 'integer', Octave = 'integer'))

setValidity('tonalInterval', 
            function(object) {
              length(object@Fifth) == length(object@Octave) &&
                                is.integer(object@Fifth) &&
                                is.integer(object@Octave)
            })

######tonalInterval constructors

#' The basic constructor for \code{tonalIntervals}.
#' @name tonalInterval
#' @export
tint <- function(o, f) new('tonalInterval', Octave = as.integer(o), Fifth = as.integer(f))

#' A basic constructor for simple \code{tonalIntervals}
#' (i.e., within one octave).
#' @name tonalInterval
#' @export
simpletint <- function(f) {
 fifthNsciOct2tonalInterval(f, rep(4L, length(f)))         
}
#' @name tonalInterval
#' @export
getFifth  <- function(tint) tint@Fifth
#' @name tonalInterval
#' @export
getOctave <- function(tint) tint@Octave


#' tonalInterval vector methods
#' 
#' @name tonalInterval-asvector
NULL


## a few methods required to make tonalIntervals act like vectors
#' @name tonalInterval-asvector
#' @export
setMethod('[', signature = c('tonalInterval', i = 'missing'),
          function(x, i) x)  

#' @name tonalInterval-asvector
#' @export
setMethod('[', signature = c('tonalInterval'),
          function(x, i) {
                    
                    tint(getOctave(x)[i], getFifth(x)[i])        
          })

#' @name tonalInterval-asvector
#' @export
setMethod('[<-', signature = c(x = 'tonalInterval', i = 'ANY', j = 'missing', value = 'tonalInterval'),
          function(x, i, value) {
                    stopifnot(length(i) == length(value))
                    
                    x@Fifth[i] <- value@Fifth
                    x@Octave[i] <- value@Octave
                    
                    x
          })

#' @name tonalInterval-asvector
#' @export
setMethod('c', signature = c('tonalInterval'),
          function(x, ...) {
                    durs <- list(x, ...)
                    
                    ns <- unlist(sapply(durs, getFifth))
                    ds <- unlist(sapply(durs, getOctave))
                    
                    tint(ds, ns)
          })

#' @name tonalInterval-asvector
#' @export
setMethod('length', signature = c('tonalInterval'),
          function(x) {
                    length(getOctave(x))
          })


#' @name tonalInterval-asvector
#' @export
setMethod('as.vector', signature = c('tonalInterval'),
          function(x) { x })

#' @name tonalInterval-asvector
#' @export
setMethod('is.vector', signature = c('tonalInterval'),
          function(x) { TRUE })

#' @name tonalInterval-asvector
#' @export
is.atomic.tonalInterval <- function(x) TRUE


#' @name tonalInterval-asvector
#' @export
rep.tonalInterval <- function(x, ...) {
 tint(rep(getOctave(x), ...), rep(getFifth(x), ...))       
}

#' @name tonalInterval-asvector
#' @export
setMethod('dim', signature = 'tonalInterval',
          function(x) NULL) #c(length(x), 1))

#' @name tonalInterval-asvector
#' @export
setMethod('show', signature = c(object = 'tonalInterval'), function(object) { cat(as.interval(object)) })


#' @name tonalInterval-asvector
#' @export
as.data.frame.tonalInterval <- function(x, row.names = NULL, optional = FALSE, ...) {
          if (is.null(row.names)) row.names <- 1:length(x)
          
          value <- list(x)
          attr(value, 'row.names') <- row.names
          attr(value, 'names') <- 'tonalInterval'
          class(value) <- c('data.frame')
          value
}

#' @name tonalInterval-asvector
#' @export
format.tonalInterval <- function(x, ...) {
          as.interval(x)         
}



#' @name tonalInterval-asvector
#' @export
setMethod('as.character', signature = c('tonalInterval'), function(x) as.interval(x))



#### tonalInterval arithmetic ----

#' @name tonalInterval
#' @export
setMethod('+', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(getOctave(e1) + getOctave(e2),
                         getFifth(e1)  + getFifth(e2))
          })

#' @name tonalInterval
#' @export
setMethod('sum', signature = c('tonalInterval'),
          function(x, ..., na.rm = TRUE) {
                    x <- list(x, ...)
                    x <- do.call('c', x)
                    tint(sum(getOctave(x), na.rm), sum(getFifth(x), na.rm))
          })

#' @name tonalInterval
#' @export
setMethod('cumsum', signature = c('tonalInterval'),
          function(x) {
                    tint(cumsum(getOctave(x)), cumsum(getFifth(x)))
          })


#' @name tonalInterval
#' @export
setMethod('diff', signature = c('tonalInterval'),
          function(x) {
                    tint(diff(getOctave(x)), diff(getFifth(x)))
          })

#' @name tonalInterval
#' @export
setMethod('-', signature = c('tonalInterval', 'missing'),
          function(e1) {
                    invert(e1, around = tint(0, 0))
          })

#' @name tonalInterval
#' @export
setMethod('-', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(getOctave(e1) - getOctave(e2),
                         getFifth(e1)  - getFifth(e2))
                    
          })

#' @name tonalInterval
#' @export 
invert <- function(tint, around = tint(0,0)) around + around - tint


#' @name tonalInterval
#' @export
simplify <- function(tint) {
          simpletint(getFifth(tint))
}




#' 
#' To take the modulo of a tonalInterval, it doesn't make sense 
#' to take the modulo of the Fifth and the Octave separately.
#' Rather, we take the modulo of the Fifth normally, but for the octave
#' we look at how many time the Fifth values was divided by the Fifth modulus
#' and subtract that number multiplied by the octave modulus.
#' That way the change applied to the Octave matches the one applied to the Fifth.
#' @name tonalInterval
#' @export
setMethod('%%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
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

#' @name tonalInterval
#' @export
setMethod('*', signature = c('tonalInterval', 'numeric'),
          function(e1, e2) {
                    tint(getOctave(e1) * e2,
                         getFifth(e1) * e2)
          })

####tonalInterval comparisions ----

#' @name tonalInterval
#' @export
setMethod('==', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    getOctave(e1) == getOctave(e2) &  
                    getFifth(e1)  == getFifth(e2)
          })

#' @name tonalInterval
#' @export
setMethod('!=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    getOctave(e1) != getOctave(e2) &  
                              getFifth(e1)  != getFifth(e2)
          })

#' @name tonalInterval
#' @export
setMethod('>', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) > as.semits(e2)
          })

#' @name tonalInterval
#' @export
setMethod('>=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) >= as.semits(e2)
          })

#' @name tonalInterval
#' @export
setMethod('<', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) < as.semits(e2)
          })

#' @name tonalInterval
#' @export
setMethod('<=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) <= as.semits(e2)
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

fifth2genericinterval <- function(fifth) c(1, 5, 2, 6, 3, 7, 4)[1 + (fifth %% 7)]
fifth2solfabase       <- function(fifth) c('d', 's', 'r', 'l', 'm', 't', 'f')[1 + (fifth %% 7)]
fifth2lettername      <- function(fifth) c('C', 'G', 'D', 'A', 'E', 'B', 'F')[1 + (fifth %% 7)]
fifth2qualityN        <- function(fifth) (fifth + 1) %/% 7         

fifth2accidental <- function(fifth, sharp = '#', flat = '-') {
          accN <- fifth2qualityN(fifth)
          
          output <- character(length(fifth)) # vector of empty strings ""
          output <- IfElse(fifth >  5, strrep(sharp,  abs(accN)), output)
          output <- IfElse(fifth < -1, strrep(flat,   abs(accN)), output)
          output
}
fifth2tonalname <- function(fifth, kernFlats = TRUE) {
          letternames <- fifth2lettername(fifth)
          flatstyle <- if (kernFlats) '-' else 'b'
          accidentals <- fifth2accidental(fifth, '#', flatstyle)
          
          paste0(letternames, accidentals)
}
fifth2quality <- function(fifth, quality.labels) {
          list2env(quality.labels, envir = environment()) 
          #puts major, minor, augment, diminish, perfect into environment
          
          qualityN <- fifth2qualityN(fifth)
          qualities <- rep(major, length(fifth))
          qualities <- IfElse(fifth < 2 & fifth > -2, rep(perfect, length(fifth)), qualities)
          qualities <- IfElse(fifth > 5,  strrep(augment,  abs(qualityN)), qualities)
          qualities <- IfElse(fifth > 5,  strrep(augment,  abs(qualityN)), qualities)
          qualities <- IfElse(fifth > -6 & fifth < -1, rep(minor, length(fifth)), qualities)          
          qualities <- IfElse(fifth <= -6, strrep(diminish, abs(fifth2qualityN(fifth + 4))), qualities)          
          qualities
          
}
fifth2simpleInterval <- function(fifth, quality.labels = list(augment = 'A', diminish = 'd', major = 'M', minor = 'm')) {
          generic <- fifth2genericinterval(fifth)
          qualities <- fifth2quality(fifth, quality.labels)
          
          paste0(qualities, generic)
}


##### As semits (i.e., 0, -11)

#' @export
setGeneric('as.semits', function(x) standardGeneric('as.semits'))

#' @name tonalInterval-write
#' @export
setMethod('as.semits', signature = c(x = 'tonalInterval'),
          function(x) {
                    ((getFifth(x) * 19) + (getOctave(x) * 12)) 
            })

#' @export
setGeneric('as.midi', function(x) standardGeneric('as.midi'))

#' @name tonalInterval-write
#' @export
setMethod('as.midi', signature = c(x = 'tonalInterval'),
          function(x) {
                    as.semits(x) + 60L
          })


#### As scientific pitch (i.e, C4)

sciOctave <- function(tint) {
          generic <- tint %% tint(-11, 7)
          (as.semits(generic) %/% 12) + 4L
}

#' @export
setGeneric('as.sciPitch', function(x) standardGeneric('as.sciPitch'))

#' @name tonalInterval-write
#' @export
setMethod('as.sciPitch', signature = c(x = 'tonalInterval'),
          function(x) {
                    octave <- sciOctave(x)
                    paste0(fifth2tonalname(getFifth(x), kernFlats = FALSE), octave)

            })


#### As kern pitch (i.e., 'aaa', 'CC-')

#' @export
setGeneric('as.kernPitch', function(x) standardGeneric('as.kernPitch'))

#' @name tonalInterval-write
#' @export
setMethod('as.kernPitch', signature = c(x = 'tonalInterval'),
          function(x) {
                    octaves <- sciOctave(x)
                    fifths <- getFifth(x)
                    
                    letternames <- fifth2lettername(fifths)
                    accidentals <- fifth2accidental(fifths)
                    
                    letternames <- IfElse(octaves >= 4, tolower(letternames), letternames)
                    
                    repn <- octaves - 4L
                    repn <- IfElse(repn >= 0, repn + 1L, -repn)
                    
                    paste0(strrep(letternames, repn), accidentals)
          })


#### As interval (i.e., "M3", "-P11")

#' @export
setGeneric('as.interval', function(x, specific = TRUE, directed = TRUE, ...) standardGeneric('as.interval'))

#' @name tonalInterval-write
#' @export
setMethod('as.interval', signature = c(x = 'tonalInterval'),
          function(x, specific = TRUE, directed = TRUE, 
                   quality.labels = list(augment = 'A', diminish = 'd', major = 'M', minor = 'm', perfect = 'P')) {
                    octave <- sciOctave(x) - 4
                    
                    fifth <- getFifth(x)
                    fifth <- IfElse(octave < 0,   fifth * sign(octave), fifth) # invert interval if direction is down
                    direction <- if (directed) {
                              IfElse(fifth == 0, 
                                     rep('', length(fifth)),  
                                     c('-', '+', '+')[sign(octave) + 2])
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
                    
                    paste0(direction, qualities, octaveshift)
          })

##### As solfege (i.e., "do", "si")
#' @export
setGeneric('as.solfa', function(x, key = 0L, ...) standardGeneric('as.solfa'))

solfatab <- rbind(d = c("e", "o", "i"),
                  r = c("a", "e", "i"),
                  m = c("e", "i", "y"),
                  f = c("e", "a", "i"),
                  s = c("e", "o", "i"),
                  l = c("e", "a", "i"),
                  t = c("e", "i", "y"))

#' @name tonalInterval-write
#' @export
setMethod('as.solfa', signature = c(x = 'integer', key = 'integer'),
          function(x, key = 0L) {
          x <- x - key          
                    
          bases <- fifth2solfabase(x)
          qualityN <- fifth2qualityN(x)
          qualSign <- sign(qualityN)    
                
          tails <- solfatab[cbind(fifth2genericinterval(x), 
                                 qualSign + 2)]
          
          residualQualityN <- qualityN - qualSign
          
          accidentals <- strrep(c('-', '', '#')[qualSign + 2], abs(residualQualityN))
          
          paste0(bases, tails, accidentals)
          
          })

#' @name tonalInterval-write
#' @export
setMethod('as.solfa', signature = c(x = 'tonalInterval', key = 'integer'),
          function(x, key = 0L) {
            fifths <- getFifth(x)        
            as.solfa(fifths, key)
         
            
          })

#' @name tonalInterval-write
#' @export
setMethod('as.solfa', signature = c(x = 'ANY', key = "tonalInterval"),
          function(x, key = tint(0, 0)) {
                    as.solfa(x, getFifth(key))        
          })


#### As ratio (i.e., "3/2")

#' @export
setGeneric('as.ratio', function(x, twelfth = 2^(19/12)) standardGeneric('as.ratio'))

#' @name tonalInterval-write
#' @export
setMethod('as.ratio', signature = c(x = 'tonalInterval'),
          function(x, twelfth) {
                    fifth <- getFifth(x)
                    oct   <- getOctave(x)
                    
                    MASS::fractions((2 ^ oct) * (twelfth ^ fifth))
          })

#### As frequency (i.e., "440")

#' @export
setGeneric('as.frequency', function(x, 
                                    reference.freq = 440L, 
                                    reference.tint = tint(-4, 3), 
                                    twelfth = 2^(19/12),
                                    ...) standardGeneric('as.frequency'))

#' @name tonalInterval-write
#' @export
setMethod('as.frequency', signature = c(x = 'tonalInterval'),
          function(x, reference.freq, reference.tint, twelfth) {
            x <- x - reference.tint
            
            ratio <- as.ratio(x, twelfth = twelfth)
            attributes(ratio) <- NULL
            
            reference.freq * ratio
            
          })



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
          simpleints <- (abs(ints - sign(ints)) %% 7) # from 0
          fifths <- c(0, 2, 4, 6, 1, 3, 5)[simpleints + 1]
          ifelse(ints > 0, fifths, 7 - fifths) %% 7
          
}
lettername2fifth <- function(ln) match(toupper(ln), c('F', 'C', 'G', 'D', 'A', 'E', 'B')) - 2
accidental2fifth <- function(acc, sharp = '#', flat = '-') {
          sharps <- stringi::stri_count_fixed(acc, pattern = sharp)
          flats  <- stringi::stri_count_fixed(acc, pattern = flat)
          
          (7 * sharps) - (7 * flats)
}
solfa2fifth <- function(solfa) {
 base <- stringr::str_sub(solfa, start = 0L, end = 1L)        
 tail <- stringr::str_sub(solfa, start = 2L, end = 2L)        
 acc  <- stringr::str_sub(solfa, start = 3L, end = 3L)        
 
 basefifth <- match(base, c('f', 'd', 's', 'r', 'l', 'm', 't')) - 2L
 
 stackedbases <- solfatab[base, , drop = FALSE]
 
 tailfifth <- (apply(cbind(tail, stackedbases), 1, function(row) which(row[-1] == row[1])) - 2) * 7
 
 accfifth <- accidental2fifth(acc, sharp = '#', flat = '-')
 unname(basefifth + tailfifth + accfifth)
}

fifthNsciOct2tonalInterval <- function(fifth, sciOct) {
          tintWith0Octave <- tint(o = numeric(length(fifth)), f = fifth)
          octshift <- as.semits(tintWith0Octave %% tint(-11, 7)) %/% 12
          
          tint(sciOct - 4 - octshift, fifth)
}

######## From kern pitch

read.kernPitch2components <- function(str) {
          letters     <- stringi::stri_extract_first(str, regex = '([A-Ga-g])\\1*')
          
          accidentals <- stringi::stri_extract_first(str, regex = '([#-])\\1*')
          accidentals[is.na(accidentals)] <- ''
          
          nletters <- nchar(letters)
          upper    <- is.upper(letters)
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
          do.call('paste0', components)
}

#' @name tonalInterval-read
#' @export
read.kernPitch2tonalInterval <- function(str) {
          components <- read.kernPitch2components(str)
          
          fifth <- with(components, lettername2fifth(Letters) + accidental2fifth(Accidentals, flat = '-'))
 
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
          
          fifth <- lettername2fifth(letters) + accidental2fifth(accidentals, flat = 'b')
          fifthNsciOct2tonalInterval(fifth, sciOct)
          
}


#### From interval

#' @name tonalInterval-read
#' @export
read.interval2tonalInterval <- function(str) {
          direction <- stringi::stri_extract_first(str, regex = "^[-+]")
          quality   <- stringi::stri_extract_first(str, regex = "[MmP]|[Ad]+")
          generic   <- as.integer(stringi::stri_extract_first(str, regex = "[1-9][0-9]*"))
          
          
          ## Fifth
          genericfifth <- genericinterval2fifth(generic)
          genericfifth[genericfifth == 6] <- (-1) # because P4 is -1 not 6
          qualshift <- sapply(quality,
                              function(qual) {
                                        qual1 <- stringr::str_sub(qual, end = 1L)
                                        switch(qual1,  P = 0, M = 0, m = -7, d = -7, A = 7) *  nchar(qual)
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

#### From solfege

#' @name tonalInterval-read
#' @export
read.solfa2tonalInterval <- function(str) {
  fifths <- solfa2fifth(str)
  simpletint(fifths)
  
}




#############################################################################-
#### Translating pitch represenations ----
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
#' fundamental, \emph{lingua franca} representaiton of pitch.
#' 
#' @name humPitch
#' @export 
str2tonalInterval <- regexDispatch(inPlace = FALSE,
                                   'Kern Pitch' = read.kernPitch2tonalInterval,
                                   'Interval'  = read.interval2tonalInterval,
                                   'Scientific Pitch' = read.sciPitch2tonalInterval,
                                   'Solfege' = read.solfa2tonalInterval)
                                     
#' @name humPitch
#' @export
str2kernPitch <- as.kernPitch %.% str2tonalInterval

#' @name humPitch
#' @export
str2sciPitch <- as.sciPitch %.% str2tonalInterval

#' @name humPitch
#' @export
str2solfa <- as.solfa %.% str2tonalInterval


#' @name humPitch
#' @export
str2interval <- as.interval %.% str2tonalInterval



