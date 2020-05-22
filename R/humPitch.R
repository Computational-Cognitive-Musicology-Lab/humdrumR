
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
          function(x) as.interval(x))

#' @name tonalInterval
#' @export
as.double.tonalInterval <- function(x, ...) as.decimal(x, ...)



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
              e3 <- stickyApply(`+`, e2, e1)
              
              re.place(re.as(e3))
              
          })



##...subtraction ####

setMethod('-', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- as.tonalInterval(e1)
              e3 <- stickyApply(`-`, e1, e2)
              
              re.as(e3)
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
            if (length(e2) == 0L) stop(call. = FALSE, "Can't take divide (%/%) by empty value.")
            recycledim(e1 = e1, e2 = e2, funccall = '%/%')
            
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              
              f3 <-  f1 %/% f2
              f3 %dim% e1
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
          
          qualities[] <- IfElse(!na & LO5th < 2 & LO5th > -2, rep(perfect, length(LO5th)), qualities)
          qualities[] <- IfElse(!na & LO5th > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- IfElse(!na & LO5th > 5,  strrep(augment,  qualityN), qualities)
          qualities[] <- IfElse(!na & LO5th > -6 & LO5th < -1, rep(minor, length(LO5th)), qualities)          
          qualities[] <- IfElse(!na & LO5th <= -6, strrep(diminish, abs(LO5th2qualityN(LO5th + 4))), qualities)     
          qualities
          
}

LO5th2simpleInterval <- function(LO5th, quality.labels = c()) {
          setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
    
          generic <- LO5th2genericinterval(LO5th)
          qualities <- LO5th2quality(LO5th,  quality.labels)
          
          IfElse(is.na(generic) | is.na(qualities), NA_character_, .paste(qualities, generic))
}

LO5thNsciOct2tint <- function(LO5th, sciOct) {
    tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
    octshift <- tint2semit(tintWith0Octave %% tint(-11L, 7L)) %/% 12L
    
    tint(sciOct - 4L - octshift, LO5th)
}


###. x to line-of-fifths ####

genericinterval2LO5th   <- function(ints) {
  ints[ints ==  0] <- NA
  ints[ints == -1] <- 1
  simpleints <- (abs(ints - sign(ints)) %% 7L) # from 0
  LO5ths <- c(0, 2, 4, 6, 1, 3, 5)[simpleints + 1]
  ifelse(ints > 0, LO5ths, 7 - LO5ths) %% 7L
  
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


##### To/From tonal intervals ####

####. tint to x ####

###.. semitones

tint2semit <- function(x) {
        (((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L)) %dim% x
}

tint2midi <- function(x) tint2semit(x) + 60L

###.. tonal chroma names

tint2tonalChroma <- function(x, accidental.labels = c()) {
    LO5th2tonalChroma(x@Fifth, accidental.labels) %dim% x
}

#....

tint2sciOctave <- function(x) {
          generic <- x %% tint(-11L, 7L)
          (as.integer(tint2semit(generic) %/% 12L) + 4L) %dim% x
}

tint2sciPitch <- function(x) {
                    octave <- tint2sciOctave(x)
                    .paste(LO5th2tonalChroma(x@Fifth, c(flat = 'b')), octave) %dim% x
            }

#....

tint2kernPitch <- function(x) {
                    octaves <- c(tint2sciOctave(x) - 4L)
                    LO5ths <- x@Fifth
                    letternames <- LO5th2lettername(LO5ths)
                    accidentals <- LO5th2accidental(LO5ths)
                    
                    letternames <- .ifelse(octaves >= 0L, tolower(letternames), letternames)
                    repn <- IfElse(octaves >= 0, octaves + 1L, -octaves)
                    repn[repn == 0L] <- 1L
                    
                    .paste(strrep(letternames, abs(repn)), accidentals) %dim% x
}

#....

tint2lilyPitch <- force

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
    octave[is.na(octave) || octave < 0L & genericInterval != 1L] <- octave[is.na(octave) || octave < 0L & genericInterval != 1L] + 1L
    # generic <- empty(genericInterval, length(x), dim(x))
    generic<- as.character(abs(genericInterval + (7L * abs(octave))))
  } 
 
  
  .paste(directed, alteration, generic) %dim% x
  
}


##... scale degrees 

tint2scaleDegree <- function(x, Key = 0L, cautionary = TRUE, generic = FALSE, contour = FALSE, quality.labels = c(), ...) {
    deg  <- int2scaleDegree(x@Fifth, Key, cautionary, generic, quality.labels = quality.labels)
    
    if (logicalOption(contour)) {
        setoptions(contour) <- c(threshold = 6L)
        deg <- addcontour(deg, x, contour.options = contour)
    } 
    
    deg %dim% x
}

int2scaleDegree <- function(x, Key = 0L, cautionary = TRUE,
                            generic = FALSE, quality.labels = c()) {
    
    setoptions(quality.labels) <- c(perfect = 'n',  augment = '#',  diminish = 'b',  major = 'M',  minor = 'm')
    
    if (is.diatonicSet(Key)) {
        mode <- getMode(Key)
        key  <- getRoot(Key)
    } else {
        mode <- 0L
    }

    x <- x - Key
    
    if (generic) x <- genericFifth(x)
    
    intervals <- LO5th2simpleInterval(x, quality.labels = quality.labels)
    if (!cautionary) {
        x <- x - mode
        inkey <- !is.na(x) & x <= 5L & x >= -1L
        intervals[inkey] <-  stringi::stri_sub(intervals[inkey], from = 2L)
    }
    intervals %dim% x
    
}

#....

solfatab <- rbind(d = c("e", "o", "i"),
                  r = c("a", "e", "i"),
                  m = c("e", "i", "y"),
                  f = c("e", "a", "i"),
                  s = c("e", "o", "i"),
                  l = c("e", "a", "i"),
                  t = c("e", "i", "y"))

tint2solfa <- function(x, Key = 0L, generic = FALSE, contour = FALSE, ...) {
    solfa <- int2solfa(x@Fifth, Key, generic)
    
    if (logicalOption(contour)) {
        setoptions(contour) <- c(threshold = 6L)
        solfa <- addcontour(solfa, x, contour.options = contour)
    } 
    
    solfa 
}

int2solfa <- function(x, Key = 0L, generic = FALSE) {
    # This is the function that does the real heavy lifting of the
    # as.solfa function
    if (!is.numeric(Key)) Key <- as.tonalInterval(Key@Fifth)
    
    x <- x - Key
    if (generic) x <- genericFifth(x)
    
    bases <- LO5th2solfabase(x)
    qualityN <- LO5th2qualityN(x)
    qualSign <- sign(qualityN)    
    
    tails <- solfatab[cbind(LO5th2genericinterval(x), 
                            qualSign + 2)]
    
    residualQualityN <- qualityN - qualSign
    
    accidentals <- strrep(c('-', '', '#')[qualSign + 2], abs(residualQualityN))
    
    .paste(bases, tails, accidentals) %dim% x
    
}


###.. numbers

num2rational <- function(n, sep = '/') {
    frac <- numeric2fraction(n)
    .paste(frac$Numerator, frac$Denominator, sep = sep) %dim% n
}

tint2rational <-  function(x, tonalHarmonic = 2^(19/12), sep = '/') {
    frac <- numeric2fraction(as.decimal(x, tonalHarmonic = tonalHarmonic))
    
    .paste(frac$Numerator, frac$Denominator, sep = sep) %dim% x
    
}

tint2fraction <- tint2rational

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
    
    ratio <- as.decimal(x, tonalHarmonic = tonalHarmonic)
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
          
          LO5ths <- IfElse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- (wholen - (LO5ths * 19)) %/% 12
          tints <- tint(octaves, LO5ths)
          
          ##
          if (melodic) {
           chromatic <- LO5ths > 5 | LO5ths < -1
           ints <- c(diff(tints), tint(0, 0)) # tint(0,0) is just padding

           isA1 <- ints == tint(-11, 7)
           isD1 <- ints == tint(11, -7)
           
           tints[which(chromatic & isA1)] <- tints[which(chromatic & isA1)] + pythagorean.comma
           tints[which(chromatic & isD1)] <- tints[which(chromatic & isD1)] - pythagorean.comma
                    
          } else {
            mode <- if (is.null(Key)) 0L else getMode(as.diatonicSet(Key))
            LO5ths <- LO5ths - mode
            tints[LO5ths >  8 & !is.na(LO5ths)]  <- tints[LO5ths > 8 & !is.na(LO5ths)] - pythagorean.comma
            tints[LO5ths < -5 & !is.na(LO5ths)] <- tints[LO5ths < -5 & !is.na(LO5ths)] + pythagorean.comma
          }
          
          tints %dim% n
}

midi2tint <- function(n) semit2tint(n - 60L)

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
          direction <- stringi::stri_extract_first(str, regex = "^[-+]")
          quality   <- stringi::stri_extract_first(str, regex = "[MmP]|[AaDd]+")
          generic   <- as.integer(stringi::stri_extract_first(str, regex = "[1-9][0-9]*"))
          
          
          ## Fifth
          genericLO5th <- genericinterval2LO5th(generic)
          genericLO5th[genericLO5th == 6] <- (-1) # because P4 is -1 not 6
          qualshift <- sapply(quality,
                              function(qual) {
                                        qual1 <- stringr::str_sub(qual, end = 1L)
                                        switch(qual1,  P = 0, M = 0, m = -7, d = -7, D = -7, A = 7, a = 7) *  nchar(qual)
                              }) -  ifelse(grepl('^[dD]', quality) & genericLO5th > 1, 7, 0)
          
          LO5th <- qualshift + genericLO5th
          
          ## Octave
          octaveshift <- ((abs(generic) - 1) %/% 7)
          tints <- LO5thNsciOct2tint(LO5th, octaveshift + 4L)
          
          ## Direction
          
          direction <- 2 * ((is.na(direction) | direction == '+') - 0.5)
          
          (tints * direction) %dim% str
          
          
}

#....

scaleDegree2tint <- interval2tint

#....

solfa2tint <- function(str, Key = 0L) {
  LO5ths <- solfa2LO5th(str) + 0L
  tint( , LO5ths) %dim% str
}

###.. numbers

rational2decimal <- function(str) {
    split <- strsplit(str, split = '[/%]')
    split <- lapply(split, as.numeric)
    sapply(split, function(x) x[1] / x[2]) %dim% str
}

rational2tint <- function(str, tonalHarmonic = 3) {
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
            tonalRatios <- tonalRatios + log(2^(octaves), base = 3)
            
            fifs[rowSums(impure) > 0] <- round(tonalRatios[impure])
          }
          
          tint(octs, fifs) %dim% str
}


decimal2tint <- function(float, tonalHarmonic = 3, centmargin = 10) {
    octrange <- attr(centmargin, 'octrange')
    if (is.null(octrange)) octrange <- 5L
    if (octrange > 150) stop(call. = FALSE,
                            "decimal2tint can't find a note corresponding exactly to this frequency/ratio. ",
                            "Try raising the centmargin.")
    
    #
    octs <- -octrange:octrange
    
    allocts <- do.call('cbind', lapply(2^octs, '*', float))
    logged <- log(allocts, tonalHarmonic)
    
    whole <- round(logged)
    remain <- logged - whole
    
    whichhit  <- applyrows(remain, function(row) {
        hitind <- which(abs(row) == min(abs(row)))
        hitind[which.min(abs(octs[hitind]))]
    })
    
    LO5th  <- whole[cbind(seq_along(float), whichhit)]
    remain <- remain[cbind(seq_along(float), whichhit)]
    octave <- round(log(float / tonalHarmonic ^ LO5th, 2))
    
    # cents
    cents <- log(tonalHarmonic^remain,2) * 1200
    
    accept <- abs(cents) < centmargin
    
    output <- tint(octave, LO5th, cent = cents)
    if (any(!accept)) output[!accept] <- Recall(float[!accept], tonalHarmonic, 
                                                data.table::setattr(centmargin, 'octrange', octrange + 5L))
    output %dim% float
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
                           delta = FALSE, sigma = FALSE, 
                           generic.part = TRUE, alteration.part = TRUE, 
                           octave.part = TRUE, simple.part = TRUE, simplifier = floor,
                           enharmonic.part = TRUE, comma.part = TRUE, 
                           Key = NULL, Exclusive = NULL) {
    
    if (!direction) x <- abs(x)
    
    if (delta)  x <- delta(x)
    if (sigma)  x <- sigma(x)
    
    # Generic/Specific
    ifif(generic.part, alteration.part, 
         xor1 = x <- genericpart.tonalInterval(x, Key %maybe% dset(0, 0)),
         xor2 = x <- alterationpart.tonalInterval(x, Key %maybe% dset(0, 0)),
         .else = tint( , rep(0L, length(x))))
    
    # Simple/Complex
    ifif(octave.part, simple.part,
         xor1 = x <- octavepart.tonalInterval(x, simplifier),
         xor2 = x <- simplepart.tonalInterval(x, simplifier),
         .else = tint( , rep(0L, length(x))))
    
    
    x
}



#' @name tonalInterval
#' @export invert invert.tonalInterval invert.default
invert <- function(tint, around = Unison) {UseMethod('invert')}
invert.tonalInterval <- function(tint, around = tint(0L, 0L)) (around + around - tint)


#' Transpose tonalIntervals
#' 
#' This function transposes tonalIntervals by other tonal intervals.
#' By default, does real transposition.
#' However, if a \code{key} argument is specified, tonal transposition
#' takes place in that (major) key.
#' @name humTranspose
#' @export transpose transpose.tonalInterval
transpose <- function(x, interval = tint(0,0), generic = NULL, ...) {UseMethod('transpose')}

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



####. partitioning tonalIntervals ####


#' Tonal interval partitions
#' 
#' @name tonalIntervalparts
NULL



tintPartition <- function(tint, roundingMethod = floor) {
  octshift <- as.integer(roundingMethod(tint2semit(tint) / 12))
  octavepart <- tint(octshift, 0L)
  
  simplepart <- tint - octavepart
  
  cbind(Octave = octavepart, Simple = simplepart)
  
}

###.. simple + octave = complex ####

#' @export octavepart octavepart.tonalInterval octavepart.default
#' @export simplepart simplepart.tonalInterval simplepart.default
#' @export is.simple is.simple.tonalInterval is.simple.default
octavepart <- function(tint, roundingMethod) UseMethod('octavepart')
simplepart <- function(tint, roundingMethod) UseMethod('simplepart')
is.simple <- function(tint) UseMethod('is.simple')

octavepart.tonalInterval <- function(tint, roundingMethod = floor) {
    octshift <- as.integer(roundingMethod(tint2semit(tint) / 12))
    tint(octshift, 0L)
}

simplepart.tonalInterval <- function(tint, roundingMethod = floor) {
    octavepart <- octavepart.tonalInterval(tint, roundingMethod)
    tint - octavepart
}

is.simple.tonalInterval <- function(tint) {
    semit <- abs(tint2semit(tint))
    semit < 12
}


###.. generic + alteration = specific ####

#' @name tonalIntervalparts
#' @export genericpart genericpart.tonalInterval genericpart.default
#' @export alterationpart alterationpart.tonalInterval alterationpart.default
genericpart <- function(tint, Key) UseMethod('genericpart')
alterationpart <- function(tint, Key) UseMethod('alterationpart')

genericpart.tonalInterval <- function(tint, Key = dset(0L, 0L)) {
    Key <- as.diatonicSet(Key)
    mode <- tint( , Key@Mode - Key@Root)
    Key  <- tint( , Key@Root)
    
    gtint <- tint - Key 
    
    gtint <- ((gtint + P5 - mode ) %% tint(-11L, 7L)) - P5 + mode
    # offset by P5 because F (-1) is natural, not F# (6)
    
    gtint
}

alterationpart.tonalInterval <- function(tint, key = dset(0L, 0L)) {
    key <- as.diatonicSet(key)
    gtint <- genericpart.tonalInterval(tint, key)
    tint - gtint - tint( , key@Root)
}

###.. enharmonic + comma = harmonic ####

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
    
    if (is.null(wolf)) {
        commas <- (tint@Fifth %/% 12L) + sign(tint@Fifth)
        tint + pythagorean.comma * commas
    } else {
        wolf <- as.tonalInterval(wolf)
        wolf[wolf@Fifth < 0L] <- wolf[wolf@Fifth < 0L] + pythagorean.comma
        
        tint <- (wolf - P5 - (tint))
        
        (tint %/% pythagorean.comma) * -pythagorean.comma
    }
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
                            generic.part = , alteration.part = , octave.part = , simple.part = , simplifier = , 
                            enharmonic.part = , comma.part = , Key = , Exclusive = , inPlace = , ... = ),
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
as.rational      <- pitchgeneric("as.rational" , alist(tonalHarmonic = ))
as.fraction      <- pitchgeneric("as.fraction" , alist(tonalHarmonic = , sep = ))
as.decimal       <- pitchgeneric("as.decimal"  , alist(tonalHarmonic = ))
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
                             'mint,hint: interval'  = interval2tint,
                             'solfa: solfa' = solfa2tint,
                             'freq: decimal' = semit2tint)

#' @export
as.tonalInterval.character <- tonalTransform %.% char2tint

#....

#' @name as.tonalInterval
#' @export
setAs('integer', 'tonalInterval', function(from) as.tonalInterval.integer(from))
#' @name as.tonalInterval
#' @export
setAs('numeric', 'tonalInterval', function(from) as.tonalInterval.numeric(from))
#' @name as.tonalInterval
#' @export
setAs('character', 'tonalInterval', function(from) as.tonalInterval.character(from))

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
as.rational.integer    <- tint2rational    %.% as.tonalInterval.integer
#' @export
as.fraction.integer    <- tint2fraction    %.% as.tonalInterval.integer
#' @export
as.decimal.integer     <- tint2decimal     %.% as.tonalInterval.integer
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
as.rational.numeric    <- tint2rational    %.% as.tonalInterval.numeric
#' @export
as.fraction.numeric    <- tint2fraction    %.% as.tonalInterval.numeric
#' @export
as.decimal.numeric     <- tint2decimal     %.% as.tonalInterval.numeric
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
as.rational.character    <- re.place %.% tint2rational    %.% as.tonalInterval.character
#' @export
as.fraction.character    <- re.place %.% tint2fraction    %.% as.tonalInterval.character
#' @export
as.decimal.character     <- re.place %.% tint2decimal     %.% as.tonalInterval.character
#' @export
as.frequency.character   <- re.place %.% tint2frequency   %.% as.tonalInterval.character
#' @export
as.contour.character     <- re.place %.% tint2contour     %.% as.tonalInterval.character



####

# invert.default <- re.as %.% invert.tonalInterval %.% as.tonalInterval
# transpose.character <- re.as %.% transpose.tonalInterval %.% as.tonalInterval
# 
# is.simple.default <- is.simple %.% as.tonalInterval
# simplepart.default <- re.as %.% simplepart.tonalInterval %.% as.tonalInterval
# octavepart.default <- re.as %.% simplepart.tonalInterval %.% as.tonalInterval
# genericpart.default <- re.as %.% genericpart.tonalInterval %.% as.tonalInterval
# alterationpart.default <- re.as %.% alterationpart.tonalInterval %.% as.tonalInterval
# 
# 

invert.default <- force
traspose.default <- force
is.simple.default <- force
simplepart.default <- force
octavepart.default <- force
genericpart.default <- force
alterationpart.default <- force





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
