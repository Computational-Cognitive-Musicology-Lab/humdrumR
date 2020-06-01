
#' Tonal Data Types
#' 
#' \code{\link[humdrumR:humdrumR]{humdrumR package}} contains a number of 
#' intertwined \code{S4} data types defined to represent tonal musical information
#' from the traditional Western persective (i.e., diatonicism).
#' The most important is the \code{\linkS4class{tonalInterval}}.
#' There are also the \code{\linkS4class{tonalCenter}} and the \code{\linkS4class{tonalHarmony}}.
#' 
#' @name humTonality
NULL


#####tonalInterval S4 class ####

####.class methods ####

###..definition, validity, initialization ####

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
#' \code{\linkS4class{struct}}.
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
         contains = 'struct',
         slots = c(Fifth  = 'integer', 
                   Octave = 'integer', 
                   Cent   = 'numeric')) -> tonalInterval

setValidity('tonalInterval', 
            function(object) {
              all(abs(object@Cent) <= 1200, na.rm = TRUE)
            })

setMethod("initialize", 
          "tonalInterval",
          function(.Object, Fifth = 0L, Octave = 0L, Cent = 0L) {
              .Object <- callNextMethod() # call the struct initialize
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


##...constructors ####

#' The basic constructor for \code{tonalInterval}s.
#' \code{tint} accepts integer values for octaves and LO5ths and numeric values for cent.
#' If the octave argument is missing a "simple" interval is constructed---i.e., an ascending interval less than one octave.
#' (When appropriate, we can think of these generically as an interval with no specific octave.)
#' @name tonalInterval
#' @export
tint <- function(octave, LO5th = 0L, cent = numeric(length(octave))) {
    if (missing(octave)) return(LO5thNsciOct2tint(LO5th, 4L))
  
    new('tonalInterval', 
        Octave = as.integer(octave), 
        Fifth  = as.integer(LO5th), 
        Cent   = as.numeric(cent))
}


setGeneric("LOF", function(x, sum = FALSE) standardGeneric("LOF"))
setMethod("LOF", "tonalInterval",
          function(x, sum = FALSE) {
            lof <- x@Fifth %dim% x
            
            if (hasdim(lof) && sum) rowSums(lof) else lof
            
          })


###..vector/core methods ####


#' @name tonalInterval
#' @export
is.tonalInterval <- function(x) inherits(x, 'tonalInterval')


###..formatting methods ####

#' @name tonalInterval
#' @export
setMethod('as.character', signature = c('tonalInterval'), 
          function(x) as.kernPitch(x))

#' @name tonalInterval
#' @export
setMethod('as.numeric', signature = c('tonalInterval'), 
          function(x) tint2decimal(x))



####.logic methods ####

###..order/relations methods ####

#' @export order.tonalInterval
#' @exportMethod > >= < <= Summary abs sign
 
order.tonalInterval <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
              
              x <- do.call('c', list(x, ...))
              order(tint2semit(x), 
                    na.last = na.last,
                    decreasing = decreasing,
                    method = method
              )
          }
          
setMethod('>', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
             tint2semit(e1) > tint2semit(e2)
          })

setMethod('>=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
              tint2semit(e1) >= tint2semit(e2)
          })

setMethod('Summary', signature = c('tonalInterval'),
          function(x) {
              semit2tint(callGeneric(tint2semit(x)))
          })

setMethod('abs', signature = c('tonalInterval'),
          function(x) {
              .ifelse(x < tint(0, 0), -x, x)
          })

setMethod('sign', signature = c('tonalInterval'),
          function(x) {
              sign(tint2semit(x))
          })



###..arithmetic methods ####

##...addition ####

#' @name tonalInterval
#' @exportMethod + - sum cumsum diff
 


setMethod('+', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- as.tonalInterval.character(e1, inPlace = TRUE)
              e3 <- stickyApply(`+`, e1, e2)
              
              re.place(re.as(e3))
              
          })

setMethod('+', signature = c('tonalInterval', 'character'),
          function(e1, e2) {
              e2 <- as.tonalInterval.character(e2, inPlace = TRUE)
              e3 <- stickyApply(`+`, e1, e2)
              
              re.place(re.as(e3))
              
          })



##...subtraction ####

setMethod('-', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- as.tonalInterval(e1)
              e3 <- stickyApply(`-`, e1, e2)
              
              re.place(re.as(e3))
          })


setMethod('-', signature = c('tonalInterval', 'character'),
          function(e1, e2) {
            e2 <- as.tonalInterval(e2)
            e3 <- stickyApply(`-`, e1, e2)
            
            re.place(re.as(e3))
          })


##...division/modulo  ####

#' @name tonalInterval
#' @exportMethod %% %/%
 
setMethod('%%', signature = c('tonalInterval', 'tonalInterval'),
          # To take the modulo of a tonalInterval, it doesn't make sense 
          # to take the modulo of the Fifth and the Octave separately.
          # Rather, we take the modulo of the Fifth normally, but for the octave
          # we look at how many time the Fifth values was divided by the Fifth modulus
          # and subtract that number multiplied by the octave modulus.
          # That way the change applied to the Octave matches the one applied to the Fifth.
          function(e1, e2) {
              if (length(e1) == 0L) return(e1)
              if (length(e2) == 0L) stop(call. = FALSE, "Can't take modulo (%%) with empty modulo.")
            
              recycledim(e1 = e1, e2 = e2, funccall = '%%')
              
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              
              LO5thDivs <- .ifelse(f2 == 0L, 0L, f1 %/% f2)
              LO5thMods <- .ifelse(f2 == 0L, f1, f1 %%  f2)
              
              tint <- tint(e1@Octave - (e2@Octave * LO5thDivs), LO5thMods)
              
              tint %dim% e1
              
          })

setMethod('%/%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%/%) by empty value.")
            recycledim(e1 = e1, e2 = e2, funccall = '%/%')
            
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              
              f3 <-  f1 %/% f2
              f3 %dim% e1
          })


setMethod('%%', signature = c('tonalInterval', 'integer'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%%) by empty value.")
            
            minusremain <- (e1 %/% e2) * e2
            
            e1 - minusremain
          })

setMethod('%/%', signature = c('tonalInterval', 'integer'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%/%) by empty value.")
            recycledim(e1 = e1, e2 = e2, funccall = '%/%')
            
            
            tint <- tint(e1@Octave %/% e2, e1@Fifth %/% e2)
            
            tint %dim% e1
          })


##### To/From line-of-fifths ####

###. line-of-fifths to x ####

genericFifth <- function(LO5th) ((LO5th + 1L) %% 7L) - 1L
LO5th2genericinterval <- function(LO5th) c(1L, 5L, 2L, 6L, 3L, 7L, 4L)[1 + (LO5th %% 7)]
LO5th2solfabase       <- function(LO5th) c('d', 's', 'r', 'l', 'm', 't', 'f')[1 + (LO5th %% 7)]
LO5th2lettername      <- function(LO5th) c('C', 'G', 'D', 'A', 'E', 'B', 'F')[1 + (LO5th %% 7)]
LO5th2qualityN        <- function(LO5th) as.integer((LO5th + 1) %/% 7)

LO5th2accidental <- function(LO5th, accidental.labels = c()) {
          setoptions(accidental.labels) <- c(sharp = '#', flat = '-')
          list2env(as.list(accidental.labels), envir = environment())
          
          accN <- LO5th2qualityN(LO5th)
          
          output <- character(length(LO5th)) # vector of ""
          isna <- is.na(LO5th)
          output[!isna & LO5th >  5] <- strrep(sharp,  abs(accN[!isna & LO5th > 5]))
          output[!isna & LO5th < -1] <- strrep(flat,   abs(accN[!isna & LO5th < -1]))
          output[isna] <- NA_character_
          output
}

LO5th2alteration <- function(LO5th, mode, cautionary = TRUE, alteration.labels = c()) {
    # LO5th and mode must be centered to 0 (C major)
    setoptions(alteration.labels) <- c(sharp = '#', flat = 'b', natural = 'n')
    list2env(as.list(alteration.labels), envir = environment())
    
    output <- alteration <- LO5th2accidental(LO5th - mode, alteration.labels[c('sharp', 'flat')])
    notna <- !is.na(alteration)
    alteration <- alteration[notna]
    LO5th <- LO5th[notna]
    
    accidental <- LO5th2accidental(LO5th, alteration.labels[c('sharp', 'flat')])
    
    alteration[alteration != '' & LO5th <= 5 & LO5th >= -1] <- 'n'
    alteration[LO5th >  5 & LO5th - mode >=  6] <- accidental[LO5th >  5 & LO5th - mode >= 6]
    alteration[LO5th < -1 & LO5th - mode <= -2] <- accidental[LO5th < -1 & LO5th - mode <= -2]
    
    alteration[ alteration %~% '#' & LO5th < -1] <- paste0('n', accidental[ alteration %~% '#' & LO5th < -1] )
    alteration[ alteration %~% 'b' & LO5th >  5] <- paste0('n', accidental[ alteration %~% 'b' & LO5th >  5] )
    
    if (cautionary) {
        alteration[LO5th >  5 & alteration == ""] <- paste0('(', accidental[LO5th >  5 & alteration == ""], ')')
        alteration[LO5th < -1 & alteration == ""] <- paste0('(', accidental[LO5th < -1 & alteration == ""], ')')
    }
    
    output[notna] <- alteration
    output
}

LO5th2tonalChroma <- function(LO5th, accidental.labels = c()) {
          letternames <- LO5th2lettername(LO5th)
          accidentals <- LO5th2accidental(LO5th, accidental.labels)
          .ifelse(is.na(letternames) | is.na(accidentals),
                  NA_character_, 
                  .paste(letternames, accidentals))
}

LO5th2quality <- function(LO5th, quality.labels = c()) {
          setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
          list2env(as.list(quality.labels), envir = environment()) 
          #puts major, minor, augment, diminish, perfect into environment
    
          qualityN <- abs(LO5th2qualityN(LO5th))
          qualities <- if (is.null(dim(LO5th))) rep(major, length(LO5th)) else array(major, dim = dim(LO5th))
          na <- is.na(LO5th)
          qualities[na] <- NA_character_
          
          qualities[] <- .ifelse(!na & LO5th < 2 & LO5th > -2, rep(perfect, length(LO5th)), qualities)
          qualities[] <- .ifelse(!na & LO5th > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- .ifelse(!na & LO5th > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- .ifelse(!na & LO5th > -6 & LO5th < -1, rep(minor, length(LO5th)), qualities)          
          qualities[] <- .ifelse(!na & LO5th <= -6, strrep(diminish, abs(LO5th2qualityN(LO5th + 4))), qualities)     
          qualities
          
}

LO5th2simpleInterval <- function(LO5th, quality.labels = c()) {
          setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
    
          generic <- LO5th2genericinterval(LO5th)
          qualities <- LO5th2quality(LO5th,  quality.labels)
          
          .paste(qualities, generic, na.if = any)
}

LO5th2scaleDegree <- function(LO5th, Key = dset(0L, 0L), cautionary = TRUE, quality.labels = c()) {
  # Key must be diatonicSet
  setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
  
  intervals <- LO5th2genericinterval(LO5th)
  
  needqual <- if (cautionary) seq_along(intervals) > 0L else LO5th != (LO5th %% Key)
  
  intervals[needqual] <- .paste(LO5th2quality(LO5th[needqual], quality.labels), intervals[needqual], na.if = any)
  
  intervals %dim% LO5th
  
}

solfatab <- rbind(d = c("e", "o", "i"),
                  r = c("a", "e", "i"),
                  m = c("e", "i", "y"),
                  f = c("e", "a", "i"),
                  s = c("e", "o", "i"),
                  l = c("e", "a", "i"),
                  t = c("e", "i", "y"))

LO5th2solfa <- function(LO5th, Key = dset(0L, 0L)) {
  # This is the function that does the real heavy lifting of the
  # as.solfa function
  # Key must be a diatonicSet
  
  bases <- LO5th2solfabase(LO5th)
  qualityN <- LO5th2qualityN(LO5th)
  qualSign <- sign(qualityN)    
  
  tails <- solfatab[cbind(LO5th2genericinterval(LO5th), 
                          qualSign + 2)]
  
  residualQualityN <- qualityN - qualSign
  
  accidentals <- strrep(c('-', '', '#')[qualSign + 2], abs(residualQualityN))
  
  .paste(bases, tails, accidentals) %dim% LO5th
  
}

## octave stuff

LO5thNedgeOct2tint <- function(LO5th, edgeOct) {
    tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
    octshift <- tint2semit(tintWith0Octave %% tint(-11L, 7L)) %/% 12L
    
    tint(edgeOct - octshift, LO5th)
}

LO5thNsciOct2tint <- function(LO5th, sciOct) LO5thNedgeOct2tint(LO5th, sciOct - 4L) 

LO5thNcentralOct2tint <- function(LO5th, centralOct) {
  tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
  octshift <- round(tint2semit(tintWith0Octave %% tint(-11L, 7L)) / 12L)
  
  tint(centralOct - octshift, LO5th)
}


###. x to line-of-fifths ####

genericinterval2LO5th   <- function(ints) {
  ints[ints ==  0L] <- NA
  ints[ints == -1L] <- 1
  simpleints <- (abs(ints - sign(ints)) %% 7L) # from 0
  LO5ths <- c(0L, 2L, 4L, 6L, 1L, 3L, 5L)[simpleints + 1L]
  LO5ths <- ifelse(ints > 0L | ints == -1L, LO5ths, 7L - LO5ths) %% 7L
  LO5ths[LO5ths == 6L] <- -1L
  LO5ths
  
}
lettername2LO5th <- function(ln) match(toupper(ln), c('F', 'C', 'G', 'D', 'A', 'E', 'B')) - 2L
accidental2LO5th <- function(acc, accidental.labels = c()) {
  setoptions(accidental.labels) <- c(sharp = '#', flat = '[-b]')
  sharps <- colSums(do.call('rbind', 
                            lapply(accidental.labels[names(accidental.labels) == 'sharp'], 
                                   stringi::stri_count_regex, str = acc)))
  flats <- colSums(do.call('rbind', 
                           lapply(accidental.labels[names(accidental.labels) == 'flat'], 
                                  stringi::stri_count_regex, str = acc)))
  as.integer((7 * sharps) - (7 * flats))
  
}
tonalChroma2LO5th <- function(tn) {
  LO5th <- lettername2LO5th(stringr::str_sub(tn, start = 0L, end = 1L))
  acc <- accidental2LO5th(stringr::str_sub(tn, start = 2L),
                          accidental.labels = c())
  
  LO5th + acc
}
solfa2LO5th <- function(solfa) {
  base <- stringr::str_sub(solfa, start = 0L, end = 1L)        
  tail <- stringr::str_sub(solfa, start = 2L, end = 2L)        
  acc  <- stringr::str_sub(solfa, start = 3L, end = 3L)        
  
  isna <- is.na(base)
  
  baseLO5th <- match(base, c('f', 'd', 's', 'r', 'l', 'm', 't')) - 2L
  stackedbases <- solfatab[base[!isna], , drop = FALSE]
  
  tailLO5th <- (apply(cbind(tail[!isna], stackedbases), 1, function(row) which(row[-1] == row[1])) - 2L) * 7L
  
  accLO5th <- accidental2LO5th(acc, c())
  LO5th <- baseLO5th + accLO5th
  LO5th[!isna] <- LO5th[!isna] + tailLO5th 
  
  unname(LO5th)
}



##### To/From octaves ####

####. tint to octave ####

tint2centralOctave <- function(x) {
  # centralOctave is octave surrounding unison (above and below, from -6semits to +6 semits)
  generic <- x %% dset(0L, 0L)
  (round(tint2semit(generic) / 12L)) %dim% x
}

tint2edgeOctave <- function(x, zero = TRUE) {
  # edgeOctave is octave from unison and above 
  generic <- x %% tint(-11L, 7L)
  octave <- tint2semit(generic) %/% 12L
  
  if (!zero) octave[octave >= 0L & generic@Fifth != 0L] <- octave[octave >= 0L &  generic@Fifth != 0L] + 1L
  
  octave %dim% x
}

tint2sciOctave <- function(tint) tint2edgeOctave(tint) + 4L

tint2octavemark <- function(x, tint2octave = tint2edgeOctave, octave.marks = c(), ...) {
  setoptions(octave.marks) <- c(Up = "^", Down = "v", Same = "")
  
  octave <- dropdim(tint2octave(x, ...))
  
  strrep(.ifelse(octave >= 0, octave.marks['Up'], octave.marks['Down']), abs(octave)) %dim% x
}

#..... octave marker functions

relativeContour <- function(str, tint, octave.before = TRUE, octave.marks = c()) {
  if (!all(ldim(str) == ldim(tint)))  .stop("Can't add octave info to string argument str if str is not same size as tint argument.")
  
  setoptions(octave.marks) <- c(Up = "+", Down = "-")
  
  tint <- delta(tint)
  label <- tint2octavemark(tint, tint2edgeOctave, octave.marks, zero = FALSE)
  
  .paste(if (octave.before) label, str, if (!octave.before) label)
}

relativeOctaver <- function(str, tint, octave.before = TRUE, octave.marks = c()) {
  if (!all(ldim(str) == ldim(tint)))  .stop("Can't add octave info to string argument str if str is not same size as tint argument.")
  
  tint <- delta(tint)
  label <- tint2octavemark(tint, tint2centralOctave, octave.marks)
  
  .paste(if (octave.before) label, str, if (!octave.before) label)
}

absoluteOctaver <- function(str, tint, octave.offset = 0L, octave.before = TRUE, octave.marks = NULL) {
  if (!all(ldim(str) == ldim(tint)))  .stop("Can't add octave info to string argument str if str is not same size as tint argument.")
  
  label <- if (is.null(octave.marks)) {
    tint2edgeOctave(tint) + octave.offset
  } else {
    tint2octavemark(tint, tint2edgeOctave, octave.marks)
  }
  
  .paste(if (octave.before) label, str, if (!octave.before) label)
}

kernOctaver <- function(str, tint) {
  if (!all(ldim(str) == ldim(tint)))  .stop("Can't add octave info to string argument str if str is not same size as tint argument.")
  
  n <- tint2edgeOctave(tint)
  
  char <- substr(str, 0L, 1L)
  char <- .ifelse(n >= 0L, tolower(char), toupper(char))
  
  rep <- .ifelse(n >= 0, n + 1, -n)
  
  .paste(strrep(char, rep), stringr::str_sub(str, start = 2L))
  
}

octaver <- function(str, tint, octave.style = 'sci', ...) {
  switch(octave.style %maybe% "",
         kern = kernOctaver(str, tint),
         sci      = absoluteOctaver(str, tint, octave.offset = 4L, octave.before = FALSE, octave.marks = NULL),
         lily_absolute = absoluteOctaver(str, tint, octave.before = FALSE, octave.marks = c(Up = "'", Down = ",")),
         lily_relative = relativeOctaver(str, tint, octave.before = FALSE, octave.marks = c(Up = "'", Down = ",")),
         absolute = absoluteOctaver(str, tint, ...),
         relative = relativeOctaver(str, tint, ...),
         contour  = relativeContour(str, tint, ...),
         str) # of octave.style is null, str passed unchanged
}


####. octave to tint ####


##### To/From tonal intervals ####
####. tint to x ####

###.. semitones

tint2semit <- function(x) {
        as.integer((((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L))) %dim% x
}

tint2midi <- function(x) tint2semit(x) + 60L

###.. tonal chroma names

tint2tonalChroma <- function(x, accidental.labels = c(), octave.style = NULL, ...) {
  tonalchroma <- LO5th2tonalChroma(x@Fifth, accidental.labels) %dim% x
  
  octaver(tonalchroma, x, octave.style, ...) %dim% x
  
}


tint2sciPitch <- function(x)  tint2tonalChroma(x, accidental.labels = c(flat = 'b'), octave = 'sci')

tint2kernPitch <- function(x) tint2tonalChroma(x, octave = 'kern')

tint2lilyPitch <- function(x, relative.octave = TRUE) {
  tolower(tint2tonalChroma(x, 
                           accidental.labels = c(sharp = 'is', flat = 'es'), 
                           octave = if (relative.octave) 'relative' else 'absolute', 
                           octave.marks = c(Up = "'", Down = ","),  octave.before = FALSE)) 
}

###.. intervals



tint2interval <- function(x, direction = TRUE, generic.part = TRUE, alteration.part = TRUE, quality.labels = c()) {
  
  setoptions(quality.labels) <- c(augment = 'A', diminish = 'd', major = 'M', minor = 'm', perfect = 'P')
  
  octave <- tint2sciOctave(x) - 4L
  x[octave < 0L] <- x[octave < 0L] * -1L
  
  generic <- alteration <- directed <- character(length(x))
  
  if (direction) {
    directed <- .ifelse(x == tint(0, 0), "", c('-', '+')[1 + (octave >= 0)])
  }
  if (alteration.part) {
    alteration <- LO5th2quality(x@Fifth, quality.labels) %dim% x
  } 
  if (generic.part) {
    genericInterval <- LO5th2genericinterval(x@Fifth) %dim% x
    octave[!is.na(octave) & octave < 0L & genericInterval != 1L] <- octave[!is.na(octave) & octave < 0L & genericInterval != 1L] + 1L  # can octave be NA?
    # generic <- empty(genericInterval, length(x), dim(x))
    generic<- as.character(abs(genericInterval + (7L * abs(octave))))
  } 
 
  
  .paste(directed, alteration, generic) %dim% x
  
}


##... scale degrees 

tint2scaleDegree <- function(x, Key = NULL, cautionary = FALSE, quality.labels = c(), octave.style = NULL, ...) {
    setoptions(quality.labels) <- c(perfect = 'n',  augment = '#',  diminish = 'b',  major = 'M',  minor = 'm')
    
    Key <- if (is.null(Key)) dset(0, 0) else as.diatonicSet(Key)
    
    x   <- x - Key
    Key <- Key - getRoot(Key)
    
    deg  <- LO5th2scaleDegree(x@Fifth, Key, cautionary, quality.labels = quality.labels)

    deg <- octaver(deg, x, octave.style, ...)

    deg %dim% x
}

# 


#....



tint2solfa <- function(x, Key = NULL, octave.style = NULL, ...) {
  Key <- if (is.null(Key)) dset(0, 0) else as.diatonicSet(Key)
  
  x <- x - Key
  Key <- Key - getRoot(Key)
  
  solfa <- LO5th2solfa(x@Fifth, Key)
    
  octaver(solfa, x, octave.style, ...)
}



###.. numbers



tint2rational <-  function(x, tonalHarmonic = 2^(19/12)) as.rational.numeric(as.numeric(x, tonalHarmonic = tonalHarmonic)) 

tint2fraction <- function(x, tonalHarmonic = 2^(19/12)) as.fraction.numeric(as.numeric(x, tonalHarmonic = tonalHarmonic)) 

tint2decimal <-  function(x, tonalHarmonic = 2^(19/12)) {
    LO5th <- x@Fifth
    oct   <- x@Octave
    cent  <- x@Cent
    
    .ifelse(is.na(LO5th), 
            NA_real_, 
            (2 ^ oct) * (tonalHarmonic ^ LO5th) * 2^(cent / 1200)) %dim% x
}

tint2frequency <- function(x, reference.freq = 440L, 
                           reference.tint = tint(-4L, 3L), 
                           tonalHarmonic = 2^(19/12)) {
    x <- x - reference.tint
    
    ratio <- tint2decimal(x, tonalHarmonic = tonalHarmonic)
    attributes(ratio) <- NULL
    
    reference.freq * ratio %dim% x
}



###.. contour

tint2contour <- function(x, threshold = tint( , 6),  contour.labels = c()) {
    setoptions(contour.labels) <- c(Down = '-', Same = '', Up = '+', Bound = "")
    
    contour <- character(length(x))
    
    threshold <- as.tonalInterval(threshold)
    targets <- abs(x) >= threshold & !is.na(x)
    
    contour[targets] <- contour.labels[c('Down', 'Same', 'Up')[sign(x[targets]) + 2L]]
    contour <- .ifelse(targets, 
                                   strrep(contour, 1 + (tint2semit(abs(x) - threshold) %/% 12L)), 
                                   contour)
    browser()
    contour[1] <- paste0(contour.labels['Bound'], contour[1])
    contour
    
}

addcontour <- function(strs, tint, contour.options = list()) {
    setoptions(contour.options) <- list(delta = TRUE, threshold = 0L, octave = TRUE, after = FALSE)
    contour.options <- nestoptions(contour.options, contour.labels = c("Up", "Down", "Same", "Bound"))
    
    after <- contour.options$after
    contour.options$after <- NULL
    
    contour <- do.call('as.contour.tonalInterval', c(list(tint), contour.options))
    if (after) .paste(strs, contour) else .paste(contour, strs)
    
}

####. x to tint ####



###.. semitones

semit2tint <- function(n, melodic = FALSE, Key = NULL) {
          wholen <- as.integer(c(n))
          
          pitchclass <- wholen %% 12L
          
          LO5ths <- .ifelse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- (wholen - (LO5ths * 19)) %/% 12
          tints <- tint(octaves, LO5ths)
          
          ##
          tints <- enharmonicpart(tints, 12L, Key %maybe% dset(0, 0))
          
          if (melodic) {
           chromatic <- LO5ths > 5 | LO5ths < -1
           ints <- c(diff(tints), tint(0, 0)) # tint(0,0) is just padding

           isA1 <- ints == tint(-11, 7)
           isD1 <- ints == tint(11, -7)
           
           tints[which(chromatic & isA1)] <- tints[which(chromatic & isA1)] + pythagorean.comma
           tints[which(chromatic & isD1)] <- tints[which(chromatic & isD1)] - pythagorean.comma
                    
          } 
          
          tints %dim% n
}

midi2tint <- function(n, melodic = FALSE, Key = NULL) semit2tint(n - 60L, melodic, Key)

###.. tonal chroma names

sciPitch2tint <- function(str) {
  
  letters     <- stringi::stri_extract_first(str, regex = '[A-G]')
  accidentals <- stringi::stri_extract_first(str, regex = '([#b])\\1*')
  accidentals[is.na(accidentals)] <- ''
  
  sciOct      <- as.numeric(stringi::stri_extract_first(str, regex = '[-+]?[0-9]+'))
  if (all(is.na(sciOct))) sciOct <- rep(4, length(sciOct))
  
  LO5th <- lettername2LO5th(letters) + accidental2LO5th(accidentals, accidental.labels = c(flat = 'b'))
  
  LO5thNsciOct2tint(LO5th, sciOct) %dim% str
  
}

#....

kernPitch2tint <- function(str) {
    components <- kernPitch2components(str)
    
    LO5th <- with(components, 
                  lettername2LO5th(Letters) + 
                      accidental2LO5th(Accidentals,   
                                       accidental.labels = c(flat = '-')))
    
    LO5thNsciOct2tint(LO5th, components$SciOctave) %dim% str
    
}

kernPitch2components <- function(str) {
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

kernPitch2sciPitch <- function(str) {
          components <- kernPitch2components(str)
          
          components$Accidentals <- gsub('-', 'b', components$Accidentals)
          do.call('.paste', components) 
}






###.. intervals



interval2tint <- function(str) {
          
          # parse string
          REparse(str, direction = "^[-+]?", quality = '^[PnMm]|^[A#bd]+', generic = '^[1-9][0-9]*', toEnv = TRUE)
  
          ## Fifth
          generic <- as.integer(generic)
          genericLO5th <- genericinterval2LO5th(generic)
          
          # how are degress shifted relative to MAJOR key
          ## Quality 
          shifts <- c(X =  NA_integer_, 
                      P =  0L, n   =  0L,  
                      M =  0L, m   = -7L,  
                      d = -7L, b   = -7L,
                      A =  7L, `#` = 7L) 
          
          qualtype <- substr(quality, 1L, 1L)
          qualshift <- shifts[qualtype] * nchar(quality)
          # "d" (diminish) on imperfect degrees gets one extra -7L, because it assumes implicit m (minor)
          qualshift[qualtype == 'd' & !genericLO5th %in% -1L:1L] <- qualshift[qualtype == 'd' & !genericLO5th %in% -1L:1L] - 7L
          
          ## together
          LO5th <- qualshift + genericLO5th
          
          ## Octave
          octaveshift <- (abs(generic) - 1) %/% 7
          tint <- LO5thNsciOct2tint(LO5th, octaveshift + 4L)                
          
          ## Direction
          
          sign <- (2 * ((is.na(direction) | direction != '-') - 0.5))
          
          (tint * sign) %dim% str
          
}

#....

scaleDegree2tint <- function(str, Key = NULL, octave.style = NULL) {
  
  Key <- if (is.null(Key)) dset(0L, 0L) else as.diatonicSet(Key)

  # parse string
  REparse(str, octave = "^[\\^v',]*", quality = '^[PnMm]|^[A#bd]*', generic ="^[1-7]", toEnv = TRUE)
  
  ## Fifths
  generic <- as.integer(generic)
  C5ths <- genericinterval2LO5th(generic) # defaults for Major
  key5ths <- C5ths %% (Key - getRoot(Key)) # default for Mode
  
  ## Quality 
  # how are degress shifted relative to MAJOR key
  quality[quality == ""] <- "X" # empty quality
  shifts <- c(X =  NA_integer_, 
              P =  0L, n   =  0L,  
              M =  0L, m   = -7L,  
              d = -7L, b   = -7L,
              A =  7L, `#` = 7L) 
  
  qualtype <- substr(quality, 1L, 1L)
  qualshift <- shifts[qualtype] * nchar(quality)
  # "d" (diminish) on imperfect degrees gets one extra -7L, because it assumes implicit m (minor)
  qualshift[qualtype == 'd' & !key5ths %in% -1L:1L] <- qualshift[qualtype == 'd' & !key5ths %in% -1L:1L] - 7L
  
  ## together
  LO5ths <- .ifelse(qualtype == 'X', key5ths, C5ths + qualshift)
  
  
  ## Octave
  tint <- if (is.null(octave.style)) {
    tint( , LO5ths)
  } else {
    
    if (!octave.style %in% c('relative', 'absolute')) .stop("Currently, can only read scale degree tokens with absolute or relative octaves (lily pond definitions)",
                                                            " using ^v or ' , symbols.")
    
    octaven <- stringi::stri_count_regex(octave, "[\\^']") - stringi::stri_count_regex(octave, "[v,]")
    if (octave.style == 'absolute') {
      LO5thNedgeOct2tint(LO5ths, octaven) 
    } else {
      sigma(LO5thNcentralOct2tint(delta(LO5ths), octaven))
    }
  }
  
  #
  (tint + Key) %dim% str
}

#....

solfa2tint <- function(str, Key = NULL, octave.style = NULL) {
  Key <- if (is.null(Key)) dset(0L, 0L) else as.diatonicSet(Key)
  
  REparse(str, octave = "^[\\^v',]*", solfa = '.+', toEnv = TRUE)
  
  LO5ths <- solfa2LO5th(solfa) 
  
  ## Octave
  tint <- if (is.null(octave.style)) {
    tint( , LO5ths)
  } else {
    
    if (!octave.style %in% c('relative', 'absolute')) .stop("Currently, can only read solfa tokens with absolute or relative octaves (lily pond definitions)",
                                                            " using ^ v or ' , symbols .")
    
    octaven <- stringi::stri_count_regex(octave, "[\\^']") - stringi::stri_count_regex(octave, "[v,]")
    if (octave.style == 'absolute') {
      LO5thNedgeOct2tint(LO5ths, octaven) 
    } else {
      sigma(LO5thNcentralOct2tint(delta(LO5ths), octaven))
    }
  }
  
  #

  (tint + as.diatonicSet(Key)) %dim% str 
}

###.. numbers

fraction2tint <- function(x, tonalHarmonic = 3) rational2tint(as.rational.character(x), tonalHarmonic) %dim% x

rational2tint <- function(x, tonalHarmonic = 3) {
  if (x$Numerator == 0 || (x$Numerator < 0 & x$Denominator > 0)) .stop('Rational values can only be interpreted as tonalIntervals if they are positive.')
  
  fracs <- do.call('cbind', x)
  
  # octaves
  octaves    <- log(fracs, base = 2)
  tonalRatios   <- log(fracs, base = tonalHarmonic)
  octaves [ , 2] <- -octaves[ , 2]
  tonalRatios[ , 2] <- -tonalRatios[ , 2] 
  is.octave  <- is.whole(octaves) & octaves != 0
  is.tonalRatio <- is.whole(tonalRatios) & tonalRatios != 0
  
  octs <- fifs <- numeric(nrow(fracs))
  # easy "pure" matches
  pure <- rowSums(is.whole(tonalRatios) | is.whole(octaves)) == 2
  pure12 <- rowSums(is.tonalRatio) > 0
  fifs[pure12] <- tonalRatios[pure12,][is.tonalRatio[pure12, ]]
  
  # 
  pure8 <- rowSums(is.octave) > 0
  octs[pure8] <- octaves[pure8,][is.octave[pure8, ]]
  
  # approximations
  # round to nearest LO5th value
  if (any(!pure)) {
    impure <- !is.tonalRatio & !is.octave & tonalRatios != 0
    tonalRatios <- tonalRatios + log(2^(octaves), base = tonalHarmonic)
    
    fifs[rowSums(impure) > 0] <- round(tonalRatios[impure])
  }
  
  tint(octs, fifs) %dim% str
}

decimal2tint <- function(x, tonalHarmonic = 3, centmargin = 10) {
    if (x <= 0) .stop('Decimal (numeric) values can only be interpreted as tonalIntervals if they are positive.')
  
    octrange <- attr(centmargin, 'octrange')
    if (is.null(octrange)) octrange <- 5L
    if (octrange > 150) stop(call. = FALSE,
                            "decimal2tint can't find a note corresponding exactly to this frequency/ratio. ",
                            "Try raising the centmargin.")
    
    #
    octs <- -octrange:octrange
    
    allocts <- do.call('cbind', lapply(2^octs, '*', x))
    logged <- log(allocts, tonalHarmonic)
    
    whole <- round(logged)
    remain <- logged - whole
    
    whichhit  <- applyrows(remain, function(row) {
        hitind <- which(abs(row) == min(abs(row)))
        hitind[which.min(abs(octs[hitind]))]
    })
    
    LO5th  <- whole[cbind(seq_along(x), whichhit)]
    remain <- remain[cbind(seq_along(x), whichhit)]
    octave <- round(log(x / tonalHarmonic ^ LO5th, 2))
    
    # cents
    cents <- log(tonalHarmonic^remain,2) * 1200
    
    accept <- abs(cents) < centmargin
    
    output <- tint(octave, LO5th, cent = cents)
    if (any(!accept)) output[!accept] <- Recall(x[!accept], tonalHarmonic, 
                                                data.table::setattr(centmargin, 'octrange', octrange + 5L))
    output %dim% x
}



frequency2tint <- function(float, reference.freq = 440L, 
                           reference.tint = tint(-4, 3), tonalHarmonic = 3,
                           centmargin = 10) {
    
   ( decimal2tint(float / reference.freq, tonalHarmonic, centmargin = 10) + reference.tint) %dim% float
}


###.. contour

contour2tint <- force

##### Tonal transforms ####

#' @export
tonalTransform <- function(x,  direction = TRUE, contour = FALSE, 
                           delta = FALSE, sigma = Exclusive %allin% c('mint'), 
                           generic = FALSE, simple = FALSE, simplifier = floor, enharmonic = FALSE, 
                           Key = NULL, Exclusive = NULL) {
    # Key
    
    # calculus
    if (!direction) x <- abs(x)
    
    if (delta)  x <- delta(x)
    if (sigma)  x <- sigma(x)
    
    # Generic/Specific
    if (generic) x <- genericpart.tonalInterval(x, Key %maybe% dset(0L, 0L))
    # ifif(generic.part, alteration.part, 
         # xor1 = x <- genericpart.tonalInterval(x, Key %maybe% dset(0L, 0L)),
         # xor2 = x <- alterationpart.tonalInterval(x, Key %maybe% dset(0L, 0L)),
         # .else = tint( , rep(0L, length(x))))
    
    # Simple/Complex
    if (simple) x <- simplepart.tonalInterval(x, simplifier)
    # ifif(octave.part, simple.part,
         # xor1 = x <- octavepart.tonalInterval(x, simplifier),
         # xor2 = x <- simplepart.tonalInterval(x, simplifier),
         # .else = tint( , rep(0L, length(x))))
    
    if (enharmonic) x <- enharmonicpart(x, Key = Key)
    a
    x
}



#' @name tonalInterval
#' @export 
invert <- function(tint, around, Key, ...) UseMethod('invert')
#' @export 
invert.tonalInterval <- function(tint, around = tint(0L, 0L), Key = NULL) {
  if (!is.tonalInterval(around)) around <- as.tonalInterval(around)
  
  output <- (around + around - tint) 
  if (!is.null(Key)) output <- output %% Key
  
  output
}


#' Transpose tonalIntervals
#' 
#' This function transposes tonalIntervals by other tonal intervals.
#' By default, does real transposition.
#' However, if a \code{key} argument is specified, tonal transposition
#' takes place in that (major) key.
#' @name humTranspose
#' @export
transposeBy <- function(x, by, Key, ...) UseMethod('transposeBy')
#' @export
transposeBy.tonalInterval <- function(x, by, altered.intervals = FALSE, Key = NULL) {
    if (!is.tonalInterval(by)) by <- as.tonalInterval(by)
    
    if (!altered.intervals) by <- by %% dset(0L, 0L)
    
    y <- x + by
    
    if (!is.null(Key)) y <- y %% Key
    
    if (!altered.intervals) {
      by <- (y - x)
      
      qual <- LO5th2quality(by@Fifth)
      
      by[qual %~% 'd'] <- by[qual %~% 'd'] + tint(-11,7) 
      by[qual %~% 'A'] <- by[qual %~% 'A'] - tint(-11,7) 
      
      y <- x + by
    }
      
    y
}

#' @name humTranspose
#' @export
transposeTo <- function(x, Key, ...) UseMethod('transposeTo')
#' @export
transposeTo.tonalInterval <- function(x, toKey, fromKey = dset(0L, 0L)) {
    by <- toKey - fromKey
 
    (x + by) %% toKey 

}



####. partitioning tonalIntervals ####


#' Tonal interval partitions
#' 
#' @name tonalIntervalparts
NULL



tintPartition <- function(tint, Key = dset(0,0), roundingMethod = floor) {
  mat <- tintPartition.octave_simple(tint, roundingMethod = roundingMethod)
  
  Key <- as.diatonicSet(Key)
  cbind(Key = getRootTint(Key), mat[, 'Octave'], tintPartition.generic_alteration(mat[ , 'Simple'], Key = Key))
  
}

###.. simple + octave = complex ####

#' @export octavepart simplepart  is.simple 
octavepart <- function(tint, roundingMethod) UseMethod('octavepart')
simplepart <- function(tint, roundingMethod) UseMethod('simplepart')
is.simple <- function(tint) UseMethod('is.simple')


#' @export
octavepart.tonalInterval <- function(tint, roundingMethod = floor) {
    generic <- genericpart.tonalInterval(tint)
    octshift <- as.integer(roundingMethod(tint2semit(generic) / 12))
    tint(octshift, 0L) %dim% tint
}

#' @export
simplepart.tonalInterval <- function(tint, roundingMethod = floor) {
    octavepart <- octavepart.tonalInterval(tint, roundingMethod)
    tint - octavepart
}

tintPartition.octave_simple <- function(tint, roundingMethod = floor) {
  if (hasdim(tint) && ncol(tint) > 1) .stop("Can't create a tonalInterval partition matrix if the tonalInterval is already a multi-column matrix.")
  octavepart <- octavepart.tonalInterval(tint, roundingMethod)
  
  cbind(Octave = octavepart, Simple = tint - octavepart)

}

#' @export
is.simple.tonalInterval <- function(tint) abs(tint2semit(tint)) < 12


###.. generic + alteration = specific ####

#' @name tonalIntervalparts
#' @export genericpart alterationpart
genericpart <- function(tint, Key) UseMethod('genericpart')
alterationpart <- function(tint, Key) UseMethod('alterationpart')

#' @export
genericpart.tonalInterval <- function(tint, Key = dset(0L, 0L)) {
    Key <- as.diatonicSet(Key)
    
    (tint %% Key) 
}

#' @export
alterationpart.tonalInterval <- function(tint, Key = dset(0L, 0L)) {
    Key <- as.diatonicSet(Key)
    gtint <- genericpart.tonalInterval(tint, Key)
    
    tint  - gtint
}
tintPartition.generic_alteration <- function(tint, Key = dset(0L, 0L)) {
  if (hasdim(tint) && ncol(tint) > 1) .stop("Can't create a tonalInterval partition matrix if the tonalInterval is already a multi-column matrix.")
  gtint<- genericpart.tonalInterval(tint, Key)

  cbind(Generic = gtint,  Alteration = tint - gtint)
  
}

###.. enharmonic + comma = harmonic ####

#' @name tonalIntervalparts
#' @export enharmonicpart commapart
enharmonicpart <- function(tint, enharmonicWrap, Key) UseMethod('enharmonicpart')
commapart      <- function(tint, enharmonicWrap, Key) UseMethod('commapart')

#' @export
enharmonicpart.tonalInterval <- function(tint, enharmonicWrap = 12, Key = dset(0L, 0L)) {
  Key <- as.diatonicSet(Key)
  
  modeoffset <- tint( , getMode(Key)) + M2 # because 2 fifths is the "center" of the diatonic set
  tint <- tint - modeoffset 

  enharmonicbound <- enharmonicWrap %/% 2
  sharpshift <-  tint( , enharmonicbound) # this makes it so an odd number (like 13) is biased towards sharp side
  flatshift  <- -tint( , enharmonicWrap - enharmonicbound)
  
  fs <- tint@Fifth
  sharp <-  fs > enharmonicbound
  flat  <- fs <= -(enharmonicWrap - enharmonicbound)
  
  tint[sharp] <- ((tint[sharp] + sharpshift) %%  pythagorean.comma) - sharpshift 
  tint[ flat] <- ((tint[ flat] + flatshift) %% -pythagorean.comma) - flatshift

  tint + modeoffset
  
}

#' @export
commapart.tonalInterval <- function(tint, enharmonicWrap = 12L, Key = dset(0L, 0L)) {
  tint - enharmonicpart.tonalInterval(tint, enharmonicWrap, Key)
    
}


tintPartition.enharmonic_comma <- function(tint, enharmonicWrap = 12L, Key = dset(0L, 0L)) {
  if (hasdim(tint) && ncol(tint) > 1) .stop("Can't create a tonalInterval partition matrix if the tonalInterval is already a multi-column matrix.")
  enharm <- enharmonicpart.tonalInterval(tint, enharmonicWrap, Key)
  
  cbind(Enharmonic = enharm, Comma = tint - enharm)
}


##### As x ####

####. generics ####

#' @export as.tonalInterval 
#' @export as.semit as.midi
#' @export as.tonalChroma as.sciPitch as.kernPitch as.lilyPitch
#' @export as.interval as.scaleDegree as.solfa
#' @export as.rational as.fraction as.decimal as.frequency
#' @export as.contour 
NULL

pitchgeneric <- function(pitchrep, secondargs = alist(), endargs = alist()) {
  rlang::new_function(c(alist(x = ),
                        secondargs,  
                        alist(direction = , contour = ,  delta = , sigma = ,
                            generic = , simple = , simplifier = ,  enharmonic.part = ,
                            Key = , Exclusive = , inPlace = , ... = ),
                        endargs),
                      rlang::expr(UseMethod(!!pitchrep)),
                      env = parent.frame())
}

as.tonalInterval <- pitchgeneric("as.tonalInterval")
as.semit         <- pitchgeneric("as.semit")
as.midi          <- pitchgeneric("as.midi")
as.tonalChroma   <- pitchgeneric("as.tonalChroma", endargs = alist(accidental.labels = ))
as.sciPitch      <- pitchgeneric("as.sciPitch",    endargs = alist(accidental.labels = ))
as.kernPitch     <- pitchgeneric("as.kernPitch",   endargs = alist(accidental.labels = ))
as.lilyPitch     <- pitchgeneric("as.lilyPitch")
as.interval      <- pitchgeneric("as.interval")
as.scaleDegree   <- pitchgeneric("as.scaleDegree")
as.solfa         <- pitchgeneric("as.solfa")
as.frequency     <- pitchgeneric("as.frequency", alist(reference.freq = , reference.tint = , tonalHarmonic = ))
as.contour       <- pitchgeneric("as.contour"  , endargs = alist(contour.labels = ))


### some generics get a few extra arguments


####. methods ####

###.. x as tint ####

#' Convert to tonalIntervals
#' 
#' Blah blah
#' @export
as.tonalInterval.tonalInterval <- tonalTransform

#' @export
as.tonalInterval.numeric <- tonalTransform %.% decimal2tint
#' @export
as.tonalInterval.integer <- tonalTransform %.% semit2tint

char2tint <- humdrumDispatch('kern: kernPitch' = kernPitch2tint,
                             'pitch: sciPitch' = sciPitch2tint,
                             'hint: interval'  = interval2tint,
                             'mint: interval'  = interval2tint,
                             'deg: scaleDegree'  = scaleDegree2tint,
                             'solfa: solfa' = solfa2tint,
                             'freq: decimal' = semit2tint,
                             'fraction: fraction' = fraction2tint)

#' @export
as.tonalInterval.character <- tonalTransform %.% char2tint
#' @export
as.tonalInterval.rational <- tonalTransform %.% rational2tint
#' @export
as.tonalInterval.fraction <- tonalTransform %.% fraction2tint

#....

#' @export
setAs('integer', 'tonalInterval', function(from) semit2tint(from))
#' @export
setAs('numeric', 'tonalInterval', function(from) decimal2tint(from))
#' @export
setAs('character', 'tonalInterval', function(from) as.tonalInterval.character(from))
#' @export
setAs('matrix', 'tonalInterval', function(from) as.tonalInterval(c(from)) %dim% from)


###.. tint as x ####

#' @export
as.semit.tonalInterval       <- tint2semit       %.% tonalTransform
#' @export
as.midi.tonalInterval        <- tint2midi        %.% tonalTransform
#' @export
as.tonalChroma.tonalInterval <- tint2tonalChroma %.% tonalTransform
#' @export
as.sciPitch.tonalInterval    <- tint2sciPitch    %.% tonalTransform
#' @export
as.kernPitch.tonalInterval   <- tint2kernPitch   %.% tonalTransform
#' @export
as.lilyPitch.tonalInterval   <- tint2lilyPitch   %.% tonalTransform
#' @export
as.interval.tonalInterval    <- tint2interval    %.% tonalTransform
#' @export
as.scaleDegree.tonalInterval <- tint2scaleDegree %.% tonalTransform
#' @export
as.solfa.tonalInterval       <- tint2solfa       %.% tonalTransform
#' @export
as.rational.tonalInterval    <- tint2rational    %.% tonalTransform
#' @export
as.fraction.tonalInterval    <- tint2fraction    %.% tonalTransform
#' @export
as.decimal.tonalInterval     <- tint2decimal     %.% tonalTransform
#' @export
as.frequency.tonalInterval   <- tint2frequency   %.% tonalTransform
#' @export
as.contour.tonalInterval     <- tint2contour     %.% tonalTransform


###.. x as y ####

#.... integer -> y ####

#' @export
as.semit.integer       <- force
#' @export
as.midi.integer        <- function(x) x + 60L
#' @export
as.tonalChroma.integer <- tint2tonalChroma %.% as.tonalInterval.integer
#' @export
as.sciPitch.integer    <- tint2sciPitch    %.% as.tonalInterval.integer
#' @export
as.kernPitch.integer   <- tint2kernPitch   %.% as.tonalInterval.integer
#' @export
as.lilyPitch.integer   <- tint2lilyPitch   %.% as.tonalInterval.integer
#' @export
as.interval.integer    <- tint2interval    %.% as.tonalInterval.integer
#' @export
as.scaleDegree.integer <- tint2scaleDegree %.% as.tonalInterval.integer
#' @export
as.solfa.integer       <- tint2solfa       %.% as.tonalInterval.integer
#' @export
as.frequency.integer   <- tint2frequency   %.% as.tonalInterval.integer
#' @export
as.contour.integer     <- tint2contour     %.% as.tonalInterval.integer

#.... numeric -> y ####

#' @export
as.semit.numeric       <- tint2semit       %.% as.tonalInterval.numeric
#' @export
as.midi.numeric        <- tint2midi        %.% as.tonalInterval.numeric
#' @export
as.tonalChroma.numeric <- tint2tonalChroma %.% as.tonalInterval.numeric
#' @export
as.sciPitch.numeric    <- tint2sciPitch    %.% as.tonalInterval.numeric
#' @export
as.kernPitch.numeric   <- tint2kernPitch   %.% as.tonalInterval.numeric
#' @export
as.lilyPitch.numeric   <- tint2lilyPitch   %.% as.tonalInterval.numeric
#' @export
as.interval.numeric    <- tint2interval    %.% as.tonalInterval.numeric
#' @export
as.scaleDegree.numeric <- tint2scaleDegree %.% as.tonalInterval.numeric
#' @export
as.solfa.numeric       <- tint2solfa       %.% as.tonalInterval.numeric
#' @export
as.frequency.numeric   <- tint2frequency   %.% as.tonalInterval.numeric
#' @export
as.contour.numeric     <- tint2contour     %.% as.tonalInterval.numeric

#.... character -> y ####

#' @export
as.semit.character       <- re.place %.% tint2semit       %.% as.tonalInterval.character
#' @export
as.midi.character        <- re.place %.% tint2midi        %.% as.tonalInterval.character
#' @export
as.tonalChroma.character <- re.place %.% tint2tonalChroma %.% as.tonalInterval.character
#' @export
as.sciPitch.character    <- re.place %.% tint2sciPitch    %.% as.tonalInterval.character
#' @export
as.kernPitch.character   <- re.place %.% tint2kernPitch   %.% as.tonalInterval.character
#' @export
as.lilyPitch.character   <- re.place %.% tint2lilyPitch   %.% as.tonalInterval.character
#' @export
as.interval.character    <- re.place %.% tint2interval    %.% as.tonalInterval.character
#' @export
as.scaleDegree.character <- re.place %.% tint2scaleDegree %.% as.tonalInterval.character
#' @export
as.solfa.character       <- re.place %.% tint2solfa       %.% as.tonalInterval.character
#' @export
as.frequency.character   <- re.place %.% tint2frequency   %.% as.tonalInterval.character
#' @export
as.contour.character     <- re.place %.% tint2contour     %.% as.tonalInterval.character



#### Tonal transform methods ----

#' @export
invert.character <- re.place %.% re.as %.% invert.tonalInterval %.% as.tonalInterval.character
#' @export
invert.numeric <- re.as %.% invert.tonalInterval %.% as.tonalInterval.numeric
#' @export
invert.integer <- re.as %.% invert.tonalInterval %.% as.tonalInterval.integer

#' @export
transposeBy.character <- re.place %.% re.as %.% transposeBy.tonalInterval %.% as.tonalInterval.character
#' @export
transposeBy.numeric <- re.place %.% re.as %.% transposeBy.tonalInterval %.% as.tonalInterval.numeric
#' @export
transposeBy.integer <- re.place %.% re.as %.% transposeBy.tonalInterval %.% as.tonalInterval.integer

#' @export
transposeTo.character <- re.place %.% re.as %.% transposeTo.tonalInterval %.% as.tonalInterval.character
#' @export
transposeTo.numeric <- re.place %.% re.as %.% transposeTo.tonalInterval %.% as.tonalInterval.numeric
#' @export
transposeTo.integer <- re.place %.% re.as %.% transposeTo.tonalInterval %.% as.tonalInterval.integer







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




############# Known tonalIntervals ####
#' @name tonalInterval
#' @export dd1 dd2 A2 P3 d4 d5 d6 AA6 M7 m8 dd9 A9 P10 d11 d12 d13 AA13 M14 P15
#' @export d1 d2 AA2 M3 P4 P5 m6 dd7 A7 P8 d9 AA9 M10 P11 P12 m13 dd14 A14 A15
#' @export P1 m2 dd3 A3 A4 A5 P6 d7 AA7 M8 m9 dd10 A10 A11 A12 P13 d14 AA14 AA15
#' @export A1 P2 d3 AA3 AA4 AA5 M6 m7 dd8 A8 P9 d10 AA10 AA11 AA12 M13 m14 dd15
#' @export AA1 M2 m3 dd4 dd5 dd6 A6 P7 d8 AA8 M9 m10 dd11 dd12 dd13 A13 P14 d15
#' @export Unison pythagorean.comma
allints <- outer(c('dd', 'd', 'm', 'P', 'M', 'A', 'AA'), 1:15, paste0)
allints[as.matrix(expand.grid(c(3,5), c(1,4,5,11,12,15)))] <- NA
allints <- c(allints)
allints <- allints[!is.na(allints)]
# cat(paste0("#' @export ", unlist(tapply(allints, rep(1:5, length.out = length(allints)), paste, collapse = ' '))), sep = '\n')
for (int in allints) {
  assign(int, interval2tint(int))
}
Unison <- P1
pythagorean.comma <- (-d2)
