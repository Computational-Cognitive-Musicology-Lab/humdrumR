######################################Tonal Intervals

fifth2genericinterval <- function(fifth) c(1, 5, 2, 6, 3, 7, 4)[1 + (fifth %% 7)]
fifth2lettername <- function(fifth) c('C', 'G', 'D', 'A', 'E', 'B', 'F')[1 + (fifth %% 7)]
fifth2qualityN <- function(fifth) (fifth + 1) %/% 7         

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
fifth2quality <- function(fifth, augment = 'A', diminish = 'd', major = 'M', minor = 'm') {
          qualityN <- fifth2qualityN(fifth)
          qualities <- rep('M', length(fifth))
          qualities <- IfElse(fifth < 2 & fifth > -2, rep('P', length(fifth)), qualities)
          qualities <- IfElse(fifth > 5,  strrep(augment,  abs(qualityN)), qualities)
          qualities <- IfElse(fifth > 5,  strrep(augment,  abs(qualityN)), qualities)
          qualities <- IfElse(fifth > -6 & fifth < -1, rep('m', length(fifth)), qualities)          
          qualities <- IfElse(fifth <= -6, strrep(diminish, abs(fifth2qualityN(fifth + 4))), qualities)          
          qualities
          
}
fifth2simpleInterval <- function(fifth, augment = 'A', diminish = 'd', major = 'M', minor = 'm') {
          generic <- fifth2genericinterval(fifth)
          qualities <- fifth2quality(fifth, augment = augment, diminish = diminish, major = major, minor = minor)
                  
          paste0(qualities, generic)
}

###############

#' Representation of tonal pitch information
#' 
#' This \code{S4} class is the core tonal pitch representation in \code{humdrumR}.
#' The object is used to represent tonal pitch names ("C", "G#", "Db", etc.), tonal intervals 
#' and scale degrees ("Major 3rd", "Diminished 5th", etc.), and solfege ("Do", "Re", "Fi", etc.).
#' Each pitch/interval is represented as two integers: one representing octave, the other
#' position on the circle of fifths. These values are held in the slots \code{@Octave} 
#' and \code{@Fifth}. 
#' The interval/pitch represented is the result of the Octaves and Perfect 5ths (actually, Perfect
#' *12ths*) in the slots added together.
#' 
#' This class has all the basic methods enabled for it function like a normal R vector. (Each slot can actually hold a vector of integers representing
#' Octaves/Fifths). This means you can apply normal vectorized commands to it.
#' It also has many arithmetic methods defined: for instance you can say X + Y, where
#' X and Y are each tonalIntervals. 
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
#' the resulting numbers represent *Pythagorean Tuning*.
#' \deqn{2^{-1} * 3^{1} = \frac{3^1}{2^1} = \frac{3}{2} = 1.5}
#' If we instead use as our fifth base \eqn{2^{\frac{19}{12}}} we 
#' will get equal temperement:
#' \deqn{2^{-1} * {2^{\frac{19}{12}}}^1 = 2^{\frac{7}{12}}}
#' 
#' 
#' @export
setClass('tonalInterval', slots = c(Fifth = 'integer', Octave = 'integer'))

#' @export
setValidity('tonalInterval', 
            function(object) {
              length(object@Fifth) == length(object@Octave) &&
                                is.integer(object@Fifth) &&
                                is.integer(object@Octave)
            })

#' @export
tint <- function(o, f) new('tonalInterval', Octave = as.integer(o), Fifth = as.integer(f))

getFifth <- function(tint) tint@Fifth
getOctave <- function(tint) tint@Octave


#' @export
setMethod('+', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(getOctave(e1) + getOctave(e2),
                         getFifth(e1)  + getFifth(e2))
          })

#' @export
setMethod('-', signature = c('tonalInterval', 'missing'),
          function(e1) {
                    tint(-getOctave(e1), -getFifth(e1))
          })

#' @export
setMethod('-', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    tint(getOctave(e1) - getOctave(e2),
                         getFifth(e1)  - getFifth(e2))
                    
          })



#' 
#' To take the modulo of a tonalInterval, it doesn't make sense 
#' to take the modulo of the Fifth and the Octave separately.
#' Rather, we take the modulo of the Fifth normally, but for the octave
#' we look at how many time the Fifth values was divided by the Fifth modulus
#' and subtract that number multiplied by the octave modulus.
#' That way the change applied to the Octave matches the one applied to the Fifth.
#' @export
setMethod('%%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    
            fifthNs <- getFifth(e1) %/% getFifth(e2)
            
            tint(getOctave(e1) - (getOctave(e2) * fifthNs),
                 getFifth(e1) %% getFifth(e2))
          })

#'
#' @export
setMethod('*', signature = c('tonalInterval', 'numeric'),
          function(e1, e2) {
                    tint(getOctave(e1) * e2,
                         getFifth(e1) * e2)
          })

#' @export
setMethod('==', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    getOctave(e1) == getOctave(e2) &  
                    getFifth(e1)  == getFifth(e2)
          })

#' @export
setMethod('!=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    getOctave(e1) != getOctave(e2) &  
                              getFifth(e1)  != getFifth(e2)
          })

#' @export
setMethod('>', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) > as.semits(e2)
          })

#' @export
setMethod('>=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) >= as.semits(e2)
          })

#' @export
setMethod('<', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) < as.semits(e2)
          })

#' @export
setMethod('<=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
                    as.semits(e1) <= as.semits(e2)
          })


setMethod('as.double', signature = c('tonalInterval'),
          function(x, fifthBase = 2^(19/12)) {
                    fifth <- getFifth(x)
                    oct   <- getOctave(x)
                    
                    (2 ^ oct) * (fifthBase ^ fifth)
          })


## a few methods required to make tonalIntervals act like vectors
setMethod('[', signature = c('tonalInterval'),
          function(x, i) {
            tint(getOctave(x)[i], getFifth(x)[i])        
          })

setMethod('length', signature = c('tonalInterval'),
          function(x) {
                    length(getOctave(x))
          })

setMethod('as.vector', signature = c('tonalInterval'),
          function(x) { x })
setMethod('is.vector', signature = c('tonalInterval'),
          function(x) { TRUE })

#' @export
setMethod('show', signature = c(object = 'tonalInterval'), function(object) { print(as.interval(object)) })



# semitss

#' @export
setGeneric('as.semits', function(x) standardGeneric('as.semits'))

#' @export
setMethod('as.semits', signature = c(x = 'tonalInterval'),
          function(x) {
                    ((getFifth(x) * 19) + (getOctave(x) * 12)) 
            })

#' @export
setGeneric('as.midi', function(x) standardGeneric('as.midi'))

#' @export
setMethod('as.midi', signature = c(x = 'tonalInterval'),
          function(x) {
                    as.semits(x) + 60L
          })


# Scientific pitch (i.e, C4)

sciOctave <- function(tint) {
          generic <- tint %% tint(-11, 7)
          (as.semits(generic) %/% 12) + 4L
}

#' @export
setGeneric('as.scipitch', function(x) standardGeneric('as.scipitch'))
#' @export
setMethod('as.scipitch', signature = c(x = 'tonalInterval'),
          function(x) {
                    octave <- sciOctave(x)
                    paste0(fifth2tonalname(getFifth(x), kernFlats = FALSE), octave)

            })


#' @export
setGeneric('as.kernpitch', function(x) standardGeneric('as.kernpitch'))
#' @export
setMethod('as.kernpitch', signature = c(x = 'tonalInterval'),
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


#' @export
setGeneric('as.interval', function(x, ...) standardGeneric('as.interval'))
#' @export
setMethod('as.interval', signature = c(x = 'tonalInterval'),
          function(x, augment = 'A', diminish = 'd', major = 'M', minor = 'm') {
                    octave <- sciOctave(x) - 4
                    fifth <- getFifth(x)
                    fifth <- IfElse(octave < 0,   fifth * sign(octave), fifth) # invert interval if direction is down
                    direction <- IfElse(fifth == 0, rep('', length(fifth)),  c('-', '+', '+')[sign(octave) + 2])
                    
                    generic <- fifth2genericinterval(fifth)
                    octave <- abs(IfElse(octave < 0, octave + 1, octave)) # problem here
                    
                    qualities <- fifth2quality(fifth, augment, diminish, major, minor)
                    
                    
                    paste0(direction, qualities, abs(generic + (7 * octave)))
          })





#### READING TO tonalInterval

lettername2fifth <- function(ln) match(toupper(ln), c('F', 'C', 'G', 'D', 'A', 'E', 'B')) - 2
accidental2fifth <- function(acc, sharp = '#', flat = '-') {
          sharps <- stringi::stri_count_fixed(acc, pattern = sharp)
          flats  <- stringi::stri_count_fixed(acc, pattern = flat)
          
          (7 * sharps) - (7 * flats)
}

# FROM kernpitch

from.kernpitch2components <- function(str) {
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

from.kernpitch2sciPitch <- function(str) {
          components <- from.kernpitch2components(str)
          
          components$Accidentals <- gsub('-', 'b', components$Accidentals)
          do.call('paste0', components)
}

from.kernpitch2tonalInterval <- function(str) {
          components <- from.kernpitch2components(str)
          
          fifth <- with(components, lettername2fifth(Letters) + accidental2fifth(Accidentals, flat = '-'))
 
          fifthNsciOct2tonalInterval(fifth, components$SciOctave)
         
}

fifthNsciOct2tonalInterval <- function(fifth, sciOct) {
          tintWith0Octave <- tint(o = numeric(length(fifth)), f = fifth)
          
          octshift <- as.semits(tintWith0Octave %% tint(-11, 7)) %/% 12
          
          tint(sciOct - 4 - octshift, fifth)
}


## Scientific Pitch

from.scipitch2tonalInterval <- function(str) {
          letters    <- stringi::stri_extract_first(str, regex = '[A-G]')
          accidentals <- stringi::stri_extract_first(str, regex = '([#b])\\1*')
          accidentals[is.na(accidentals)] <- ''
          
          sciOct      <- as.numeric(stringi::stri_extract_first(str, regex = '[-+]?[0-9]+'))
          if (all(is.na(sciOct))) sciOct <- rep(4, length(sciOct))
          
          fifth <- lettername2fifth(letters) + accidental2fifth(accidentals, flat = 'b')
          fifthNsciOct2tonalInterval(fifth, sciOct)
          
          
}

