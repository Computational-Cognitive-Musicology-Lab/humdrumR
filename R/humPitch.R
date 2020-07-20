
#' humdrumR and pitch
#' 
#' [humdrumR][humdrumR::humdrumR] includes a number of intertwined data structures, and associated functions, for representing and manipulating musical pitch information.
#' 
#' @section Tonality:
#' 
#' There are four data types extensively used in `humdrumR` to encode/process [tonal](https://en.wikipedia.org/wiki/Tonality) musical information:
#' + [integers][base::integer] --- used to encode "[line-of-fifths]" tonal information
#' + [tonalInterval] --- embeds line-of-fifth tonal integers alongside [octave](https://en.wikipedia.org/wiki/Octave) and [cent]("https://en.wikipedia.org/wiki/Cent_(music)") information to encode most tonal pitch representations (solfege, intervals, letternames, etc.)
#' + [diatonicSet] --- combines line-of-fifth tonal integer representations to represent diatonic tonality, including alterations of basic diatonic scale(s).
#' + [tertianSet] --- an extension of `diatonicSet` used to encode  [tertian](https://en.wikipedia.org/wiki/Tertian) diatonic harmonies.
#' 
#' For a detailed explanation of the theory and specifics of `humdrumR`'s treatment of tonality, see the *Tonality in humdrumR* vignette.
#' 
#' @section Atonality:
#' 
#' **THIS SECTION IS INCOMPLETE**
#' 
#' In addition, there are xxx data types used to encode non-tonal (or [atonal](https://en.wikipedia.org/wiki/Atonality)) pitch information.
#' 
#' + [integers][base::integer] --- used to encode [semitones](https://en.wikipedia.org/wiki/Semitone) (as well as [MIDI](https://en.wikipedia.org/wiki/MIDI) numbers).
#' + [xxx][xxx] --- sets?
#' + [xxx][xxx] --- 12-tone rows?
#' 
#' @name humdrumPitch
NULL


#####tonalInterval S4 class ####

####.class methods ####

###..definition, validity, initialization ####

#' Representation of tonal pitch information
#' 
#' The `tonalInterval` is the core tonal pitch representation in [humdrumR][humdrumR::humdrumR].
#' A `tonalInterval` is an abstract representation of tonal pitch, which can be translated to/from all standard "concrete" pitch representations:
#' solfege, scientific pitch, semitones, frequencies, scale degrees, intervals, etc.
#' For the most part, users should not need to interact with `tonalInterval`s directly---rather, `tonalInterval`s work behind the scene in numerous `humdrumR` pitch functions.
#' See the [pitchRepresentations] and [tonalTransformations] documentation for details of usage and functionality or the *Tonality in humdrumR* vignette for 
#' a detailed explanation of the theory and specifics of `tonalInterval`s.
#'
#' @details
#' 
#' The `tonalInterval` is a [S4](http://adv-r.had.co.nz/S4.html) subclass of `humdrumR`'s virtual class [struct], from which it inherits a lot of useful "vector-like" behaviors/functionality.
#' 
#' The constructor function `tint` can be used to create `tonalIntervals` directly.
#' The three arguments corespond to the three slots: `octave`, `LO5th` (Fifth), and `cent`.
#' All inputs will be coerced to match in length.
#' What's more, the `octave` argument can be left blank, in which case the appropriate octave will automatically be computed
#' to place the interval in the octave above middle-C.
#' 
#' By default, the [as.character][base::character] method, and thus (via [struct]) the [show][methods::show] method, for `tonalInterval`s call [as.kernPitch()][pitchRepresentations].
#' Thus, if you return a `tonalInterval` on the command line (or call [print][base::print]) one one, you'll see the [kern pitch][pitchRepresentations] representation printed.
#' 
#' @slot Octave integers representing the octave offset.
#' @slot Fifth integers representing the "line-of-fifths' value.
#' @slot Cent numeric values representing cents (1200th of an octave).
#' 

#' 
#' @section Arithmetic:
#' 
#' Technically, `tonalInterval`s are examples of algebraic [modules over integers](https://en.wikipedia.org/wiki/Module_(mathematics)).
#' This means that certain arithmetic operations are defined for `tonalIntervals` and can be called using standard arithmetic operators (`+`, `-`, etc.):
#' 
#' + Addition: `tonalIntervals` can be added together, acting exactly as you'd expect (i.e., \eqn{M3 + m3 = P5}).
#' + Subtraction: `tonalIntervals` can be subtracted just as they are added. Also, they can be negated with a single `-`
#'   operator (like `-M3`).
#' + Multiplication: `tonalInterval`s can *not* be multiplied together.
#'   However, [scalar (integer) multiplication](https://en.wikipedia.org/wiki/Scalar_multiplication) is defined:
#'   thus, `tonalIntervals` can be multiplied by integers to create new `tonalInterval`s: e.g., \eqn{M2 * 3L = A4}.
#' + Division: as the natural inverse of scale multiplication, [Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division)
#'   is defined for `tonalIntervals`---i.e., division by/into whole (integer) pieces, often with leftover "remainders" (modulo).
#'   In R, Euclidean division is achieved with the [%/%][base::Arithmetic] operator---*not* `/`---, with the associated [%%][base::Arithmetic] used for the remainder/modulo.
#'   Two `tonalInterval`s can be divided to produced an integer; Conversely, a `tonalInterval` can be divided by an integer to produce a `tonalInterval`.
#'   
#'   Take note that the way `humdrumR` defines Euclidean division is based in *tonal space*---i.e., the line-of-fifths---not frequency or atonal-semitone space.
#'   For example, an augmented-fourth divided by a major-second *is* `3L`, but a diminished-fifth divided by a major-second is *not* 3L---`d5 %/% M2` equals `-3L` with a remainder of `P8` (plus an octave)!
#'   The division algorithm works by applying standard Euclidean division to the `@Fifth` slot (line-of-fifths tonal space), and shifting the `@Octave` value in
#'   the remainder to the match the appropriate octave.
#'   This definition has the useful properties that `specificinterval %% A1 = genericinterval` and `interval %% d2 = enharmonicinterval`.
#' 
#' 
#' 
#' @section Relational Operators:
#' 
#' `tonalInnterval`s can be compared using the standard [relational operations][base::Comparison]---`==`, `!=`, `>`, `>=`, etc.
#' Two `tonalInterval`s are equal (according to `==`) only if all their `Octave`, `Fifth`, and `Cent` slots
#' are exactly identical. 
#' Thus, enharmonic notes (like C# and Db) are *not* equal.
#' In contrast, ordinal comparisons (e.g., `>`, `<=`) between `tonalInterval`s are based on their semitone (equal temperament) size, so enharmonicity is irrelevant.
#' Thus, `m3 >= A2` and `A2 >= m3` are both `TRUE`, even though `m3 == A2` is not.
#' 
#' @section Coercion:
#' 
#' `humdrumR` knows how to [coerce](https://en.wikipedia.org/wiki/Type_conversion) several [base-R atomic types][base::vector] into `tonalIntervals`.
#' This can be done using the [as][methods::as] function---e.g., `as(3, "tonalInterval")`---or more intuitively using the function `as.tonalInterval()`.
#' Coercision methods are defined for 
#' 
#' + [integer][base::integer]: interpreted as semitones
#' + [numeric][base::numeric]: interpreted as frequency ratios, assuming a [Pythagorean tuning](https://en.wikipedia.org/wiki/Pythagorean_tuning).
#' + [character][base::character]: interpreted using `humdrumR`s [regular expression dispatch system][humdrumR::regexDispatch], as 
#'   explained fully [here][pitchRepresentations].
#'   
#' Since, coersion is defined, `tonalInterval` arithmatic can be applied when one of the two arguments is a `tonalInterval` and the other is a coercable atomic.
#' For instance, `M3 + 2L` will interpret `2L` as two semitones and add a major-second to the major-third!
#' The clever [dispatch system][humdrumR::regexDispatch] will even ignore character strings that are not recognized (see examples)!
#'
#' 
#' @section Predefined Intervals:
#' 
#' `humdrumR` automatically exports a bunch of `tonalInterval`s, named by their musical interval representation.
#' Every generic interval from 1 to 15 is combined with every interval quality `dd` (doubly diminished), `d` (diminished), `m` (minor), `M` (major), `A` (augumented)
#' `AA` (doubly augmented).
#' Thus, after loading `humdrumR`, you can type things like `M3 + M3` and get `A5`.
#' In addition, the variables `unison` (`= P1 = tint(0, 0)`),  `pythagorean.comma` (`= d2 = tint(-19,12)`), and `octave` (`tint(1, 0)`) are exported as well.
#' 
#' 
#' 
#' @examples 
#' 
#' M3 <- tint(   , 4L)
#' 
#' M2 <- tint(   , 2L)
#' M9 <- tint(-1L, 2L)
#' 
#' M9 - M2 
#' # = octave
#' M9 - 2L
#' # = octave
#' 
#' M3 %/% M2 
#' # = 2
#' 
#' ###
#' 
#' cMajor <- sort(tint( , -1:5))
#' eMajor <- cMajor + M3
#' eMajor + 2L 
#' # f# g# a# b cc# dd# ee#
#' 
#' eMajor[4:5] - octave 
#' # = A B
#' 
#' "4.ee" + P5 
#' # = "4.bb"
#' 
#' 
#' 
#' 
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
    if (missing(octave)) octave <- -LO5th2contourN(LO5th, contour.round = floor)
  
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

setMethod('as.character', signature = c('tonalInterval'), 
          function(x) as.kernPitch(x))

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

#' Line-of-Fifths
#' 
#' The fundamental tonal space.
#' 
#' @name line-of-fifths
NULL


genericFifth <- function(LO5th) ((LO5th + 1L) %% 7L) - 1L

###. line-of-fifths to x ####

##... line-of-fifths to tonalChroma ----

LO5th2scaleStep <- function(LO5th, step.labels = 1L:7L) {
  step.labels[c(1L, 5L, 2L, 6L, 3L, 7L, 4L)][1 + (LO5th %% 7)]
}


LO5th2alterationN        <- function(LO5th, Key = dset(0L, 0L)) (LO5th - (LO5th %% Key)) %/% 7L

alteration.conflicts <- function(LO5th) {
  # do two or more qualities of the same generic intervals appear in the input,
  # if so, which ones?
  
  uniq <- unique(LO5th)
  counts <- table(genericFifth(uniq))
  counts <- counts[counts > 1]

  genericFifth(LO5th) %in% names(counts)
}

alteration.memory <- function(LO5th) {
  # propogating though input vector, is each generic interval
  # a repetition of the same quality the last time it appeared
  # i.e., c('c', 'c', 'c#', 'a','c#','b','c') -> c(F, T,F,F,T,F,F)
  # first instances are FALSE in $sameasbefore, but $new indicates where they are
  
  
  generic <- genericFifth(LO5th)
  lapply(-1L:5L, # no need to consider intervals with no conflicts
         function(gen) {
           cur <- generic  == gen
           which(cur)[c(FALSE, diff(LO5th[cur]) == 0L)]
         }) %>% unlist -> hits
  
  list(sameasbefore = seq_along(LO5th) %in% hits,  
       new = seq_along(LO5th) %in% unlist(lapply(-1:5L, match, table = generic)))
  
}

alteration.inKey <- function(LO5th, Key) {
  if (is.null(Key)) Key <- dset(0, 0)
  
  LO5th2alterationN(LO5th, Key) == 0L
}

# data.frame(h, alteration.conflicts(h), alteration.memory(h)) -> g

alteration.filter <- function(LO5th, Key, cautionary, memory) {
  
  conflicted <- alteration.conflicts(LO5th)
  mem  <- alteration.memory(LO5th)
  inKey <- alteration.inKey(LO5th, Key) # if Key == NULL, inKey is now Cmajor
  
  if (!(memory || cautionary)) return(inKey) 
  
  if (is.null(Key)) {
    
    if ( cautionary & !memory) return(rep(FALSE, length(LO5th)))
    if (!cautionary &  memory) return((mem$sameasbefore & !mem$new) | (mem$new & inKey))
    if ( cautionary &  memory) return(mem$sameasbefore & !mem$new)
    
  } else {
   
    if ( cautionary & !memory) return(inKey & !conflicted)
    if (!cautionary &  memory) return(mem$sameasbefore & (!mem$new | (mem$new & inKey)))
    if ( cautionary & memory) return(mem$sameasbefore & !xor(inKey, mem$new)) 

  }

 
}


LO5th2accidental <- function(LO5th, Key = NULL, 
                             accidental.cautionary = FALSE, accidental.memory = FALSE, 
                             accidental.labels = c(),  
                             accidental.maximum = Inf, accidental.minimum = -accidental.maximum) {
          setoptions(accidental.labels) <- c(sharp = '#', flat = 'b', natural = 'n')
          
          dontlabel <- alteration.filter(LO5th, Key, accidental.cautionary, accidental.memory)
          
          # get "absolute" (c major) accidentals
          accidentalN <- pmaxmin(LO5th2alterationN(LO5th, dset(0L, 0L)), accidental.minimum, accidental.maximum) # to c major
          if (false(accidental.labels)) return(accidentalN)
          
          accidentals <- .ifelse(dontlabel, "", accidental.labels$natural)
          
          na <- is.na(LO5th)
          accidentals[na] <- NA_character_
          
          accidentals[!na & accidentalN > 0L & !dontlabel] <- strrep(accidental.labels$sharp, accidentalN[!na & accidentalN > 0L & !dontlabel])
          accidentals[!na & accidentalN < 0L & !dontlabel] <- strrep(accidental.labels$flat,  abs(accidentalN[!na & accidentalN < 0L & !dontlabel]))
            
          #
          if ('doublesharp' %in% names(accidental.labels)) accidentals <- stringi::stri_replace_all_fixed(accidentals, pattern = strrep(accidental.labels$sharp, 2L), accidental.labels$doublesharp)
          if ('doubleflat'  %in% names(accidental.labels)) accidentals <- stringi::stri_replace_all_fixed(accidentals, pattern = strrep(accidental.labels$flat , 2L), accidental.labels$doubleflat)
          
          accidentals
          
}

LO5th2quality <- function(LO5th, Key = NULL, 
                          quality.cautionary = FALSE,  quality.memory = FALSE, 
                          quality.labels = c(), 
                          quality.maximum = Inf, quality.minimum = -quality.maximum) {
  setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm')
  
  # if (!is.null(Key)) {
    # LO5th <- LO5th - getRoot(Key)
    # Key <- Key - getRoot(Key)
  # }
  
  dontlabel <- alteration.filter(LO5th, Key, quality.cautionary, quality.memory)
  
  # get qualities
  qualities <- .ifelse(dontlabel, "", quality.labels$major)
                       
  na <- is.na(LO5th)
  qualities[na] <- NA_character_
  
  qualities[!na & LO5th <  2 & LO5th > -2 & !dontlabel] <- quality.labels$perfect
  qualities[!na & LO5th > -6 & LO5th < -1 & !dontlabel] <- quality.labels$minor      
  
  qualities[!na & LO5th >   5 & !dontlabel] <- strrep(quality.labels$augment,  pmin(abs(LO5th2alterationN(LO5th[!na & LO5th  >  5 & !dontlabel]     )),  quality.maximum))
  qualities[!na & LO5th <= -6 & !dontlabel] <- strrep(quality.labels$diminish, pmin(abs(LO5th2alterationN(LO5th[!na & LO5th <= -6 & !dontlabel] + 4L)), -quality.minimum))    
  
  qualities
  
}







## octave stuff


LO5thNscaleOct2tint <- function(LO5th, scaleOct) {
    tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
    octshift <- tint2semit(tintWith0Octave %% tint(-11L, 7L)) %/% 12L
    
    tint(scaleOct - octshift, LO5th)
}

LO5thNsciOct2tint <- function(LO5th, sciOct) LO5thNscaleOct2tint(LO5th, sciOct - 4L) 

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
  LO5ths <- .ifelse(ints > 0L | ints == -1L, LO5ths, 7L - LO5ths) %% 7L
  LO5ths[LO5ths == 6L] <- -1L
  LO5ths
  
}
lettername2LO5th <- function(ln) match(toupper(ln), c('F', 'C', 'G', 'D', 'A', 'E', 'B')) - 2L



scaleStep2LO5th <- function(str, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B')) {
  step <- match(str, step.labels, nomatch = NA_integer_) 
  
  ifelse(is.na(step), NA_integer_, c(0L, 2L, 4L, -1L, 1L, 3L, 5L)[step])
}




updownN <- function(str, up = '#', down = 'b')  stringi::stri_count_fixed(str, up) - stringi::stri_count_fixed(str, down)

accidental2LO5th <- function(str, accidental.labels = c()) {
  setoptions(accidental.labels) <- c(sharp = '#', flat = 'b', natural = 'n')
  
  
  if ('doublesharp' %in% names(accidental.labels)) str <- gsub(accidental.labels['doublesharp'], strrep(accidental.labels['sharp'], 2), str)
  if ('doubleflat'  %in% names(accidental.labels)) str <- gsub(accidental.labels['doubleflat'],  strrep(accidental.labels['flat'], 2), str)
  
  n <- updownN(str, up = accidental.labels['sharp'], down = accidental.labels['flat'])
  
  names(n) <- names(accidental.labels)[match(str, accidental.labels)]
  
  n * 7L
  
}


quality2LO5th <- function(str, quality.labels = c()) {
  setoptions(quality.labels) <- c(perfect = 'P', augment = 'A', diminish = 'd', major = 'M', minor = 'm', natural = 'n')
  
  n <- c(perfect = 0L, major = 0L, minor = -7L, diminish = -7L, augment = +7L, natural = 0L)
  n <- n[names(quality.labels)[match(substr(str, 1, 1), quality.labels)]] * nchar(str)
  n[is.na(n)] <- 0L
  n
}






##### To/From octaves/contours ####

####. tint to contour ####

tint2centralOctave <- function(x) {
  # centralOctave is octave surrounding unison (above and below, from -6semits to +6 semits)
  generic <- x %% dset(0L, 0L)
  (round(tint2semit(generic) / 12L)) %dim% x
}

tint2scaleOctave <- function(x) {
  # scaleOctave is octave from unison and above 
  generic <- x %% tint(-11L, 7L)
  (tint2semit(generic)) %/% 12L %dim% x
}

tint2contour <- function(x, contour.labels = c(up = '^', down = 'v'), 
                        contour.offset = 0L, contour.maximum = Inf, contour.minimum = -Inf,
                        contour.delta = FALSE, contour.round = floor) {
  
  setoptions(contour.labels) <- c(up = "^", down = "v", same = "")
  
  if (contour.delta) x <- delta(x)
  #
  octn <- contour.offset + octavepart(x, contour.round)@Octave
  octn <- pmin(pmax(octn, contour.minimum), contour.maximum)
  
  # if contour.labels = FALSE, we just return the number
  if (false(contour.labels)) return(octn)
  
  out <- rep(NA_character_, length(octn))
  out[!is.na(octn)] <- strrep(.ifelse(octn[!is.na(octn)] >= 0L, contour.labels$up, contour.labels$down), abs(octn[!is.na(octn)]))
  out[octn == 0L] <- contour.labels$same

  out %dim% x
  
}


octave.kernstyle <- function(str, octn) {
  char <- substr(str, 0L, 1L)
  char <- .ifelse(octn >= 0L, tolower(char), toupper(char))
  
  octn[!is.na(octn) & octn >= 0L] <- octn[!is.na(octn) & octn >= 0L] + 1L # 0 -> 1
  
  .paste(strrep(char, abs(octn)), stringr::str_sub(str, start = 2L)) %dim% str
}



####. contour to tint ####

contour2tint <- function(str, simple, contour.labels = c(), contour.offset = 0L, contour.delta = FALSE, contour.round = floor) {
  setoptions(contour.labels) <- c(up = '^', down = 'v', same = '')
  
  n <- if (false(contour.labels)) {
    as.numeric(str)
  } else { 
    updownN(str, up = contour.labels['up'], down = contour.labels['down']) 
  }
  
  n <- n - contour.offset
  
  n <- if (contour.delta) {
    simplecontour <- LO5th2contourN(delta(simple), contour.round)
    
    sigma(n - simplecontour)
    
  } else {
    simplecontour <- LO5th2contourN(simple, contour.round)
    
    n - simplecontour
  }
  
  tint(n, 0L)

}

LO5th2contourN <- function(LO5th, contour.round = floor) { 
  octaves <- tint2semit(tint(0L, LO5th) %% tint(-11, 7)) / 12
  
  contour.round(octaves)
  
}

kernOctave2tint <- function(str) {
  nletters <- nchar(str)
  
  upper  <- str == toupper(str)
  
  tint(ifelse(upper, 0L - nletters, nletters - 1L), 0L)
  
}


##### To/From tonal intervals ####
####. tint to x ####


###.. semitones

tint2semit <- function(x) {
        as.integer((((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L))) %dim% x
}

tint2midi <- function(x) tint2semit(x) + 60L

###.. tonal chroma 


tint2tonalChroma <- function(x, parts = c('qualities', 'steps', 'contours'), sep = "", ...) {
  parts <- matched(parts, c('qualities', 'steps', 'contours', 'accidentals'))
  
  steps       <- if ('steps' %in% parts)        tint2scaleStep(x, ...)   %dots% (has.prefix('step.') %.% names)
  qualities   <- if ('qualities' %in% parts)    tint2quality(x, ...)     %dots% (has.prefix('quality.') %.% names)
  accidentals <- if ('accidentals' %in% parts)  tint2accidental(x, ...)  %dots% (has.prefix('accidental.') %.% names)
  contours    <- if ('contours' %in% parts)     tint2contour(x, ...)     %dots% (has.prefix('contour.') %.% names)

  tonalchroma <- pasteordered(parts, steps = steps, qualities = qualities, accidentals = accidentals, contours = contours, sep = sep)
  
  tonalchroma  %dim% x
  
}

tint2scaleStep  <- function(x, ...) LO5th2scaleStep(x@Fifth, ...)
tint2accidental <- function(x, ...) LO5th2accidental(x@Fifth, ...)
tint2quality    <- function(x, ...) LO5th2quality(x@Fifth, ...)




###.. specific ones

tint2sciPitch <- function(x, ...)  {
  overdot(tint2tonalChroma(x, 
                           step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                           contour.labels = FALSE, contour.offset = 4L, 
                           parts = c('steps', 'accidentals', 'contours'), ...))
}


tint2lilyPitch <- function(x, relative = TRUE, ...) {
  overdot(tint2tonalChroma(x, 
                           step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                           contour.labels = c(up = "'", down = ","), 
                           contour.delta = relative,
                           contour.round = if (relative) round else floor,
                           accidental.labels = c(sharp = 'is', flat = 'es'),
                           parts = c('steps', 'accidentals', 'contours'), ...))
} 

tint2helmholtz <- function(x, ...) {
  notes <- overdot(tint2tonalChroma(x, 
                                    step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                                    contour.labels = c(up = "'", down = ","), contour.offset = 1L,
                                    ...))
  
  octn <- tint2contour(x, contour.labels = FALSE, contour.offset = 1L)
  notes[octn < 0L] <- stringr::str_to_title(notes[octn < 0L]) 
  notes
}
                                                                
tint2kernPitch <- function(x, ...) {
  simple <- overdot(tint2tonalChroma(x, step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                                     accidental.labels = c(flat = '-'), 
                                     parts = c('steps', 'accidentals'), ...))
  
  octave      <- tint2contour(x, contour.labels = FALSE)
  
  octave.kernstyle(simple, octave) %dim% x
}


#.... intervals



tint2interval <- function(x) {
  
  octave <- tint2contour(x, contour.round = floor, contour.labels = FALSE)
  direction <- .ifelse(x == tint(0, 0), "", c('-', '+')[1 + (octave >= 0)])
  
  x[octave < 0L] <- x[octave < 0L] * -1L
  steps <- tint2scaleStep(x, step.labels = 1L:7L)
  
  octave[!is.na(octave) & octave < 0L & steps != 1L] <- octave[!is.na(octave) & octave < 0L & steps != 1L] + 1L
  steps <- steps + octave * 7
  
  qualities <- tint2quality(x, quality.cautionary = TRUE)
  
  
  .paste(direction, qualities, steps) %dim% x
  
}


#.... scale degrees 

tint2scaleDegree <- function(x, Key = Key, parts = c('qualities', 'steps'), ...) {
  Key <- if (is.null(Key)) dset(0, 0) else as.diatonicSet(Key)
  x <- x - Key
  Key <- Key - getRoot(Key)
  
  tint2tonalChroma(x, Key = Key,  contour = contour, parts = parts, ...)

}

# 


#....



tint2solfa <- function(x, Key = NULL,  parts = c('contours', 'accidentals', 'steps'), ...) {
  Key <- if (is.null(Key)) dset(0, 0) else as.diatonicSet(Key)
  x <- x - Key
  Key <- Key - getRoot(Key)
  
  steps <- tint2scaleStep(x, c('d', 'r', 'm', 'f', 's', 'l', 't'))
  
  ## tails
  solfatails <- rbind(d = c("e", "o", "i"),
                      r = c("a", "e", "i"),
                      m = c("e", "i", "y"),
                      f = c("e", "a", "i"),
                      s = c("e", "o", "i"),
                      l = c("e", "a", "i"),
                      t = c("e", "i", "y"))
  tailcol <- sign(tint2accidental(x, accidental.labels = FALSE))
  tails <- solfatails[cbind(match(steps, rownames(solfatails)),  tailcol + 2L)]
  
  
  accidentals <- if (accidental) stringr::str_sub(tint2accidental(x, ...) %dots% (has.prefix('accidentals.') %.% names),
                                                  start = 2L) # drop first accidental # will mess up with double sharps/flats
  contours <- if ('contours' %in% parts)  tint2contour(x, ...) %dots% (has.prefix('contour.') %.% names)
  
  pasteordered(parts, contour = contours, accidental = accidentals, step = .paste(steps, tails))
}  







###.. numbers



tint2rational <-  function(x, tonalHarmonic = 3L) {
  Fifth <- x@Fifth
  Octave <- x@Octave
  Cent <- x@Cent
  
  num <- .ifelse(Fifth >= 0L, tonalHarmonic ^  Fifth, 1L) * .ifelse(Octave >= 0L, 2L ^  Octave, 1L)
  den <- .ifelse(Fifth <  0L, tonalHarmonic ^ -Fifth, 1L) * .ifelse(Octave <  0L, 2L ^ -Octave, 1L)
  
  
  if (any(Cent != 0)) {
    floats <- as.rational.numeric(tint2decimal(x[Cent != 0], tonalHarmonic))
    num[Cent != 0] <- floats$Numerator
    den[Cent != 0] <- floats$Denominator
    
  } 
  list(Numerator = as.integer(num) %dim% x, Denominator = as.integer(den) %dim% x) %class% 'rational'
  
}

tint2fraction <- function(x, tonalHarmonic = 3) as.fraction.rational(tint2rational(x, tonalHarmonic = tonalHarmonic)) 

tint2decimal <-  function(x, tonalHarmonic = 2^(19/12)) {
    LO5th <- x@Fifth
    oct   <- x@Octave
    cent  <- x@Cent
    
    .ifelse(is.na(LO5th), 
            NA_real_, 
            (2 ^ oct) * (tonalHarmonic ^ LO5th) * 2^(cent / 1200)) %dim% x
}

tint2frequency <- function(x, frequency.reference = 440L, 
                           frequencyTint = tint(-4L, 3L), 
                           tonalHarmonic = 2^(19/12)) {
    x <- x - frequencyTint
    
    ratio <- tint2decimal(x, tonalHarmonic = tonalHarmonic)
    attributes(ratio) <- NULL
    
    frequency.reference * ratio %dim% x
}





####. x to tint ####



###.. semitones

semit2tint <- function(n, accidental.melodic = FALSE, Key = NULL) {
          wholen <- as.integer(c(n))
          
          pitchclass <- wholen %% 12L
          
          LO5ths <- .ifelse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- (wholen - (LO5ths * 19)) %/% 12
          tints <- tint(octaves, LO5ths)
          
          ##
          tints <- atonal2tint(tints, accidental.melodic, Key)
          
          tints %dim% n
}

atonal2tint <- function(tint, accidental.melodic, Key = NULL) {
  if (!is.null(Key)) tint <- enharmonicpart(tint, 12L, Key)
  
  if (accidental.melodic) {
    chromatic <- LO5ths > 5 | LO5ths < -1
    ints <- c(diff(tint), tint(0, 0)) # tint(0,0) is just padding
    
    isA1 <- ints == tint(-11, 7)
    isD1 <- ints == tint(11, -7)
    
    tint[which(chromatic & isA1)] <- tint[which(chromatic & isA1)] + pythagorean.comma
    tint[which(chromatic & isD1)] <- tint[which(chromatic & isD1)] - pythagorean.comma
    
  } 
  tint
}

midi2tint <- function(n, accidental.melodic = FALSE, Key = NULL) semit2tint(n - 60L, accidental.melodic, Key)

###.. tonal chroma 


tonalChroma2tint <- function(str, Key = NULL,
                             parts = c('steps', 'accidentals', 'contours'), sep = "", parse.exhaust = TRUE, ...) {
 
 parts <- matched(parts, c('steps', 'accidentals', 'qualities', 'contours'))
 
 if (sum(parts %in% c('qualities', 'accidentals')) > 1L) .stop("When reading a string as a tonal chroma, you can't read both qualities and accidentals at the same time.",
                                                               " The parts argument can only include one or the other (or neither).")
 
 if (all(c('accidentals', 'qualities') %in% parts)) .stop("You can't read a tonal chroma with accidentals AND qualities at the same time.")
 
 ############# parse string
 # regular expressions for each part
 REs <-  makeRE.tonalChroma(parts, collapse = FALSE, ...)
 
 REparse(str, REs, parse.exhaust = parse.exhaust, parse.strict = TRUE, toEnv = TRUE) ## save to enviroment!
 
 
 ## simple interval
 simple <- alterations <- integer(length(str))
 if ('steps' %in% parts) simple <- simple +  scaleStep2LO5th(steps, ...) %dots% (has.prefix('step.')  %.% names)
 
 if ('accidentals' %in% parts) alterations <- accidental2LO5th(accidentals, ...) %dots% (has.prefix('accidental.') %.% names) 
 if ('qualities' %in% parts)   alterations <- quality2LO5th(qualities,      ...) %dots% (has.prefix('quality.')    %.% names)
 
 simple <- simple + alterations
 if (!is.null(Key)) simple[is.na(names(alterations))] <- simple[is.na(names(alterations))] %% Key
 
 tint <- tint(0L, simple)
 
 # contours
 if (!'contours' %in% parts) contours <- character(length(str))
 tint <- tint + contour2tint(contours, simple, ...) %dots%  (has.prefix('contour.') %.% names)
 
 tint
 
 
}




sciPitch2tint <- function(str, ...) {
  overdot(tonalChroma2tint(str, parts = c('steps', 'accidentals', 'contours'), 
                           contour.labels = FALSE, contour.offset = 4L,
                           step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                           accidental.labels = c(sharp = '#', flat = 'b', natural = 'n'),
                           ...))
  
  
}




kernPitch2tint <- function(str, ...) {
  letter <- stringr::str_extract(str, '[A-Ga-g]')
  str_ <- stringr::str_replace(str, '([A-Ga-g])\\1*', toupper(letter)) # simple part
  
  simple <- overdot(tonalChroma2tint(str_, parts = c('steps', 'accidentals'), 
                   step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                   accidental.labels = c(sharp = '#', flat = '-', natural = 'n'),
                   ...))
  
  octave <- kernOctave2tint(stringr::str_extract(str, '([A-Ga-g])\\1*'))
  
  simple + octave
  
}








###.. intervals

interval2tint <- function(str, ...) {
  num <- as.integer(stringr::str_replace_all(str, '[^-+0-9]', ''))
  num_simple <- abs(num) %% 7L
  # num_simple[num < 0] <- 9L 
  
  str_ <- stringr::str_replace(str, '^[-+]', '')
  str_ <- stringr::str_replace(str_, as.character(abs(num)), as.character(num_simple))
  
  simple <- overdot(tonalChroma2tint(str_, parts = c('qualities', 'steps'), 
                                     step.labels = c('1', '2', '3', '4', '5', '6', '7'),
                                     ...))
  
  octave <- tint(abs(num) %/% 7L, 0L)
  
  (simple + octave) * sign(num)
  
}


scaleDegree2tint <- function(str, ...) {
  
  overdot(tonalChroma2tint(str, parts = c('qualities', 'steps'), 
                           step.labels = c('1', '2', '3', '4', '5', '6', '7'),
                           ...))
  
}


solfa2tint <- function(str, ...) {
  syl <- stringr::str_extract(str, '[fdsrlmt][aeioy]')
  
  base <- stringr::str_sub(syl, 1L, 1L)
  alt  <- stringr::str_sub(syl, 2L, 2L)
  
  
  alt.mat <- rbind(d = c(NA,  'b', '#', '', '##'),
                   r = c('b', '',  '#', NA, '##'),
                   m = c(NA,  'b',  '', NA, '#'),
                   f = c('',  'b', '#', NA, '##'),
                   s = c(NA,  'b', '#', '', '##'),
                   l = c('',  'b', '#', NA, '##'),
                   t = c(NA,  'b', '',  NA, '#'))
  colnames(alt.mat) <- c('a', 'e', 'i', 'o', 'y')
  
  sylalt <- alt.mat[cbind(base, alt)]
  
  str_ <- stringr::str_replace(str, alt, sylalt)
  
  overdot(tonalChroma2tint(str_, parts = c('steps', 'accidentals'),
                             step.labels = rownames(alt.mat),
                             ...))
  
  
  
}


###.. numbers

fraction2tint <- function(x, tonalHarmonic = 3) rational2tint(as.rational.character(x), tonalHarmonic) %dim% x

rational2tint <- function(x, tonalHarmonic = 3, accidental.melodic = FALSE, Key = NULL) {
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
  
  tint <- atonal2tint(tint, accidental.melodic, Key)
  
  tint(octs, fifs) %dim% str
}

decimal2tint <- function(x, tonalHarmonic = 3, centMargin = 10, accidental.melodic = FALSE, Key = NULL) {
    if (x <= 0) .stop('Decimal (numeric) values can only be interpreted as tonalIntervals if they are positive.')
  
    octrange <- attr(centMargin, 'octrange')
    if (is.null(octrange)) octrange <- 5L
    if (octrange > 150) stop(call. = FALSE,
                            "decimal2tint can't find a note corresponding exactly to this frequency/ratio. ",
                            "Try raising the centMargin.")
    
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
    
    accept <- abs(cents) < centMargin
    
    output <- tint(octave, LO5th, cent = cents)
    if (any(!accept)) output[!accept] <- Recall(x[!accept], tonalHarmonic, 
                                                data.table::setattr(centMargin, 'octrange', octrange + 5L))
    
    output <- atonal2tint(output, accidental.melodic, Key)
    
    output %dim% x
}



frequency2tint <- function(float, frequency.reference = 440L, 
                           frequencyTint = tint(-4, 3), tonalHarmonic = 3,
                           centMargin = 10) {
    
   ( decimal2tint(float / frequency.reference, tonalHarmonic, centMargin = centMargin) + frequencyTint) %dim% float
}



##### Tonal transforms ####

#' Tonal Transformations
#' 
#' Various transformations of pitch information
#' 
#' @name tonalTransformations
#' @export
tonalTransform <- function(x,  direction = TRUE, 
                           delta = FALSE, sigma = Exclusive %allin% c('mint'), 
                           generic = FALSE, simple = FALSE, roundContour = floor, enharmonic = FALSE, 
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
    if (simple) x <- simplepart.tonalInterval(x, roundContour)
    # ifif(octave.part, simple.part,
         # xor1 = x <- octavepart.tonalInterval(x, roundContour),
         # xor2 = x <- simplepart.tonalInterval(x, roundContour),
         # .else = tint( , rep(0L, length(x))))
    
    if (enharmonic) x <- enharmonicpart(x, Key = Key)
    
    x
}



#' @name tonalTransformations
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
#' @name tonalTransformations
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

#' @name tonalTransformations
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



tintPartition <- function(tint, Key = dset(0,0), roundContour = floor) {
  mat <- tintPartition.octave_simple(tint, roundContour = roundContour)
  
  Key <- as.diatonicSet(Key)
  cbind(Key = getRootTint(Key), mat[, 'Octave'], tintPartition.generic_alteration(mat[ , 'Simple'], Key = Key))
  
}

###.. simple + octave = complex ####

#' @export octavepart simplepart  is.simple 
octavepart <- function(tint, roundContour) UseMethod('octavepart')
simplepart <- function(tint, roundContour) UseMethod('simplepart')
is.simple <- function(tint) UseMethod('is.simple')


#' @export
octavepart.tonalInterval <- function(tint, roundContour = floor) {
    generic <- genericpart.tonalInterval(tint)
    octshift <- as.integer(roundContour(tint2semit(generic) / 12))
    tint(octshift, 0L) %dim% tint
}

#' @export
simplepart.tonalInterval <- function(tint, roundContour = floor) {
    octavepart <- octavepart.tonalInterval(tint, roundContour)
    tint - octavepart
}

tintPartition.octave_simple <- function(tint, roundContour = floor) {
  if (hasdim(tint) && ncol(tint) > 1) .stop("Can't create a tonalInterval partition matrix if the tonalInterval is already a multi-column matrix.")
  octavepart <- octavepart.tonalInterval(tint, roundContour)
  
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

#' Pitch representations and translations
#' 
#' Functions for creating and/or translating between numerous representations of pitch.
#' 
#' @details 
#' 
#' There are numerous ways that musicians and musicologists encode pitch information---solfege, scientific pitch,
#' intervals, scale degrees, frequencies, etc.---each with different purposes and uses.
#' Some of these representations are numeric, but most involve letters and other characters.
#' Any of these concrete representations can be translated to and from
#' abstract [tonalInterval] objects: `tonalInterval`s completely represent all/any of these standard representations,
#' so *cocrete pitch representation -> tonalInterval* is guaranteed to be lossless.
#' However, not all pitch representations encode complete tonal pitch information, so
#' *tonalInterval -> concrete pitch representation* may be lossy.
#' These caneats asside, `humdrumR` can, via the common `tonalInterval` representation, encode and translate *between* various 
#' representations: we can read in data in any reprsentation, manipulate it, and write it to the same or a different representation.
#' This documentation describes `humdrumR`'s pitch representation and translation features.
#' (`tonalIntervals` also give us easy ways to **manipulate** pitch information---see the [tonalTransform] documentation.)
#' 
#' `humdrumR` currently recognizes XXX "standard" pitch representations:
#' 
#' | Name        | Class       | Description                                                                                                                             |
#' | ----------- | ----------- | --------------------------------------------------------------------------------------------------------------------------------------: |
#' | semit       | `integer`   | [Semitones](https://en.wikipedia.org/wiki/Semitone)                                                                                     |
#' | midi        | `integer`   | [MIDI](https://en.wikipedia.org/wiki/MIDI) note number                                                                                  |
#' | kernPitch   | `character` | [Kern](https://www.humdrum.org/rep/kern/index.html) pitch representation                                                                |
#' | sciPitch    | `character` | [Scientific pitch](https://en.wikipedia.org/wiki/Scientific_pitch) representation                                                       |
#' | lilyPitch   | `character` | [LilyPond](https://lilypond.org/doc/v2.20/Documentation/notation/pitches) pitch representation                                          |
#' | interval    | `character` | Tonal [interval]("https://en.wikipedia.org/wiki/Interval_(music)#Interval_number_and_quality")                                          |
#' | scaleDegree | `character` | [Scale degree]("https://en.wikipedia.org/wiki/Degree_(music)")                                                                          |
#' | solfa       | `character` | Humdrum [solfa](https://www.humdrum.org/rep/solfa/) representation of solfege                                                           |
#' | decimal     | `numeric`   | [Frequency ratio](https://en.wikipedia.org/wiki/Interval_ratio) as R [numeric][base::numeric] (equal temperament)                       |
#' | frequency   | `numeric`   | [Sound frequency](https://en.wikipedia.org/wiki/Frequency#Sound) ([equal temperament](https://en.wikipedia.org/wiki/Equal_temperament)) |
#' | fraction    | `character` | Frequency ratio as fraction (Pythagorean tuning)                                                                                        |
#  | rational    | `list`      | Frequency ratio as integer numerators and denominators ([Pythagorean tuning](https://en.wikipedia.org/wiki/Pythagorean_tuning))         |
#' 
#' Below are equivalent examples of each of these standard pitch representation; all the representations in each column of the table are 
#' represented by the same [tonalInterval]---the final
#' two rows show the `tonalInterval` slot (`@Octave` and `@Fifth`) values associated with each interval as well (users should never really have to think about these).
#' 
#' | Name        |  Unison        | Minor second        | Descending major second  | Augmented fourth   | Perfect 5th    | Diminished 7th         | Minor 10th     |
#' | ----------- | -------------- | ------------------- | ------------------------ | ------------------ | -------------- | ---------------------- | -------------- |
#' | semit       | `0L`           | `1L`                |  `-2L`                   |  `6L`              |  `7L`          |  `9L`                  |  `15L`         |      
#' | midi        | `60L`          | `61L`               |  `58L`                   |  `66L`             |  `67L`         |  `69L`                 |  `75L`         | 
#' | kernPitch   | `"c"`          | `"d-"`              |  `"B-"`                  |  `"f#"`            |  `"g"`         |  `"b--"`               |  `"ee-"`       | 
#' | sciPitch    | `"C4"`         | `"Db4"`             |  `"Bb3"`                 |  `"F#4"`           |  `"G4"`        |  `"Bbb4"`              |  `"Eb5"`       | 
#' | lilyPitch   | `"c"`          | `"des"`             |  `"bes,"`                |  `"fis"`           |  `"g"`         |  `"beses"`             |  `"ees'"`      | 
#' | interval    | `"P1"`         | `"+m2"`             |  `"-M2"`                 |  `"+A4"`           |  `"+P5"`       |  `"+d7"`               |  `"+m10"`      | 
#' | scaleDegree | `"1"`          | `"m2"`              |  `"m7"`                  |  `"#4"`            |  `"5"`         |  `"b7"`                |  `"m3"`        | 
#' | solfa       | `"do"`         |  `"ra"`             |  `"te"`                  |  `"fi"`            |  `"so"`        |  `"te-"`               |  `"me"`        | 
#' | decimal     | `1`            | `1.059`             |  `0.891`                 |  `1.414`           |  `1.498`       |  `1.682`               |  `2.378`       | 
#' | frequency   | `261.625`      | `277.182`           | `233.0819`               | `369.994`          |  `391.995`     |  `440`                 |  `622.253`     | 
#' | fraction    | `"1/1"`        | `"256/243"`         | `"8/9"`                  | `"729/512"`        | `"3/2"`        | `"32768/19683"`        | `"64/27"`      |
#' | `@Fifth`    | `0L`           | `-5L`               | `-2L`                    | `6L`               | `1L`           | `-9L`                  | `-3L`          |
#' | `@Octave`   | `0L`           | `8L`                | `3L`                     | `-9L`              | `-1L`          | `15L`                  | `6L`           |
#' 
#' This table illustrates that some reprsentations are lossy, as they don't encode the full pitch information.
#' For instance, the *scaleDegree* representation drops octave information, while *semit* drops tonal information (i.e., that `C# != Db`).
#' 
#' @section Pitch Translation:
#' 
#' `humdrumR` exports functions to read and write all of the standard reprsentations above, as well as some custom non-standard representations (see `as.tonalChroma` below).
#' An input can be converted to any representation by calling the appropriate function of the form `as.xxx`: for example, `as.solfa`.
#' The complete list of `as.xxx` functions for pitch representations is:
#' 
#' + Atonal
#'     + `as.semit`
#'     + `as.midi`
#' + Tonal     
#'      + `as.tonalChroma`
#'           + `as.scaleStep`
#'           + `as.accidental`
#'           + `as.quality`
#'           + `as.contour`
#'      + `as.kernPitch`
#'      + `as.lilyPitch`
#'      + `as.sciPitch`
#'      + `as.interval`
#'      + `as.scaleDegree`
#'      + `as.solfa`
#' + Frequency
#'      + `as.decimal`
#'      + `as.frequency`
#'      + `as.fraction`
#' 
#' Note that none of the functions have a plural name, so it's not `as.semits` or `as.intervals` even if you are applying them to multiple inputs!
#' 
#' Each of these `as.xxx` functions does *three* things: 
#' 
#' 1. Read the input as a `tonalInterval` (details below).
#' 2. If desired, transform the `tonalInterval` (see [tonalTransform]).
#' 3. Write the `tonalInterval` to the output representation (details below).
#' 
#' Each of these three steps has numerous options that you can control via special arguments.
#' The details of the reading and writing stages, and all the associated arguments are described in subsequent sections.
#' All pitch translation functions *also* have arguments to perform various transformations on pitch data (i.e., inversion, transposition).
#' All the transformation arguments of the [tonalTransform][tonalTransformations] function can be applied to any pitch translation function.
#' For example, you can write `as.kernPitch(x, generic = TRUE, Key = "A")` to extract generic intervals in the key of A major and output them in `kernPitch` format.
#' These transformation arguments are listed as "common" arguments in the table below, and explained in more detail in the [tonalTransformations] documentation.

#' 
#' @section Reading Pitch Information:
#' 
#' The master function `as.tonalInterval` converts any recognized pitch input into a `tonalInterval` object.
#' All the main `as.xxx` functions implicitely call `as.tonalInterval`.
#' How `as.tonalInterval` interprets input as pitch information is determined by the base R [dispatch system][methods::Methods_Details] (interpreting the class of the input) and,
#' if the input is a character-string, humdrumR's [regular expression dispatch system][humdrumR::regexDispatch].
#' 
#' The process of reading an input as a `tonalInterval` follows two steps:
#' 
#' First, the base R dispatch system checks the [class][base::class] of the input. There are currently methods for three recognized classes:
#' + `integer`: integers are interpreted as semitones.
#' + `numeric`: numeric (floating point, not integer values) are interpreted as frequency ratios. 
#' + `character`: if the input is a character, humdrumRs dispatch system comes into play (see next paragraph).
#' 
#' Given an character-string input, the `humdrumR` [regular expression dispatch system][humdrumR::regexDispatch] determines the appropriate way to parse
#' pitch information from the string using two criteria: 
#' 1) matching known *regular expressions* in the string and 
#' 2) based on the `Exclusive` argument, which indicates a *exclusive interpretations* associated with it (see our *The Humdrum Syntax* vingette if you don't know
#' what an exclusive interpretation is).
#' For any input string, humdrumR searches for known regular expressions and decides to interpret the input based on whichever regular expression matches the *most* input strings.
#' For instance, if you call `as.kernPitch("A4")` (or any `as.xxx` pitch function), the dispatcher will see that the `"A4"` string matches the regular expression for 
#' scientific pitch notation (i.e., the A above middle C) and *also* the regular expression for intervals (i.e., an augmented fourth).
#' In this case, there is only one input string, so humdrumR will five scientific notation precedence and interpret the input as the A above middle C.
#' However, if we have an input with more information, like `as.kernPitch(c("A4", "P5"))`, humdrumR will see that **both** inputs match intervals, but only the first one matches
#' scientific pitch notation---therefore, humdrumR will interpret both strings as intervals (augmented fourth and perfect 5th).
#' Note that humdrumR will *not* use two different interpretions of the same string.
#' Also, any strings which match no known regular expressions, will result in `NA` (unless `inPlace = TRUE`; see below).
#' 
#' What if we want to interpret `as.kernPitch("A4")` as an interval? Well, we can use the `Exculusive` argument.
#' The `Exclusive` argument gives more information to the dispatcher, which will override the purely-regex based decisions.
#' For instance, if we specify `as.kernPitch("A4", Exclusive = 'interval')`, `humdrumR` will know that the exclusive interpretation "**interval" 
#' should be interpreted as an interval.
#' If we run `as.kernPitch(c("A4", "P5"), Exclusive = 'pitch')`, humdrumR will return `("a", NA)`, because the second input can't be interpreted as a scientfic pitch.
#' However, the `Exclusive` argument is vectorized, which gives us the option of using multiple input interpretations on different parts of the same input.
#' For instance, `as.kernPitch(c("A4", "A4"), Exclusive = c('pitch', 'int')` will return `("a", "f#")`---the `'pitch'` exclusive intepretation is 
#' matched with the first `"A4"` while the `'int'` interpretation is matched with the second one!
#' 
#' The following regular expressions and exclusive interpretations are associated with each representation in humdrumR:
#' 
#' ```{r, echo = FALSE, comment = NA}
#' 
#'   known <- read.delim("./inst/extdata/KnownInterpretations.tsv", fill = T, header=T, quote = "", as.is=T, comment.char = "")
#'   known <- subset(known, Type == "Exclusive" & !Name %in% c("sciChord", "kern"))
#'   
#'   #
#'   rownames(known) <- known$Name
#'   known$Type <-known$Name <- NULL
#'   colnames(known) <- gsub('Pretty', 'Example', colnames(known))
#'   known$Exclusive <- sapply(strsplit(known$Exclusive, split = ','), function(x) paste(paste0('**', x), collapse = ','))
#'   known$RE <- gsub('<<', '', gsub('>>', '', known$RE))
#'   
#'   print(known, row.names = TRUE, right = FALSE)
#'   
#' ```
#'
#' 
#' A final useful option is controlled using the logical `inPlace` argument.
#' If `inPlace = TRUE` any extra characters in the input string which are *not* part of the matching regular expression will be left unchanged:
#' for example, `as.kernPitch("4.A5xxxv", inPlace = TRUE)` will output `"4.aaxxxv`---the `"A5"` (scientific pitch) is translated to `"aa"` (kern pitch)
#' but the `"4."` and `"xxxv"` are not changed.
#' In contrast, `as.kernPitch("4.A5xxxv", inPlace = FALSE)` will just return `"aa"`, stripping away the `"4."` and `"xxxv"`.
#' (Thus, the `inPlace` argument can be used as a tool to extract the desired part of strings.)
#' Note that if `inPlace = TRUE`, any inputs which fail to match anything will just be left unchanged.
#' 
#'  
#'
#' 
#' @section Read/Writing Tonal Pitch Information:
#' 
#' Most of the pitch representations defined in `humdrumR` are based in Western diatonic *tonality*---for info on our atonal representations, see the *Read/Writing Atonal Pitch Information* section below.
#' The predefined `kernPitch`, `sciPitch`, `lilyPitch`, `helmholtz`, `interval`, `scaleDegree`, and `solfa` encodings are all tonal representations---in fact, they all represent *essentially* the same absract
#' information,  which is exactly what `tonalInterval`s encode.
#' Specifically, we condsider all of these representations to be different representations of what we call *tonal chroma*:
#' as such, the `as.tonalChroma` function is the master function used to construct all of these specific representations.
#' The following section(s) explain the abstract principles of pitch representation, how they function in standard representations, and how 
#' we, in humdrumR, we can use `as.tonalChroma` and its arguments to create custom tonal representations of pitch.
#' 
#' #### Tonal Pitch "Partitions"
#' 
#' As discussed in the *Tonality in humdrumR* vignette, tonal intervals can be "partitioned"---i.e., broken into constituent logical pieces.
#' There are two main "partitions" we frequently use:
#' 
#' 1. the partitioning of a *complex* interval into a *simple parts* and *octave* parts*.
#' 2. the partitioning of a *specific* interval into a *generic part* and a *alteration* part.
#' 
#' Simple intervals are abstract ([line-of-fifths](https://en.wikipedia.org/wiki/Circle_of_fifths)) information, without regards to a specific pitch in frequency space---i.e., actual "pitch height."
#' The combination of a simple interval with an octave part creates a complex interval, which *does* include information about specific pitch height.
#' Tonal "line-of-fifths space" can further be partitioned into [generic][tonalTransformations] (diatonic) information and *specific* interval information which describes the 
#' *quality* of intervals, including zero or more [alterations]("https://en.wikipedia.org/wiki/Accidental_(music)") of the diatonic set.
#' Thus, any tonal interval/pitch is composed of an **octave** part, a **generic** part, and a **quality** part.
#' 
#' Various pitch representation schemes (kern, solfege) represent the three different parts (octave/generic/quality) of `tonalInterval`s in various ways, though some representations are 
#' lossy/incomplete, as they don't fully encode all three partitions.
#' For example, scale degrees represent simple tonal information without complex octave information.
#' In some schemes, each tonal partition is represented distinctly from others, allowing us to mix and match different sub-representation to create various full reprsentations.
#' In other cases, representations blur partitions in ways that can not be broken apart.
#' For instance, solfege syllables do not neatly delineate generic and quality information---the "e" vowel means different qualities depending on which leading consonant it is paired with
#' (e.g., "re" vs "me").
#' Since the logic of interval representation does not always conform to the more abstract logic of `tonalInterval` partitions, we use a different set of terms to
#' refer to the *representation* of distinct partitions: scale-**step** for generic information,  **quality**/**alteration**/**accidental** for alteration information, and  **contour** for octave information,
#' Thus, the terms `step`, `contour`, `quality`, `alteration`, `accidental` all appear as or in various pitch representation/translation function arguments (details below).
#' 
#' In humdrumR, a *tonal chroma* is a representation that combines some combination of the three representation partions (scale, alteration, and/or contour).
#' The `as.tonalChroma` function is a master function for writing tonal chroma.
#' The functions`as.kernPitch`, `as.sciPitch`, `as.lilyPitch`, and `as.helmholtz` are all simply wrappers which call `as.tonalChroma` with various specific arguments.
#' `as.tonalChroma` itself calls four constituent functions: `as.scaleStep`, `as.quality`/`as.accidental`, and `as.contour`/`as.octave`.
#' The arguments to `as.tonalChroma`, derived from the constituent functions (details in the following sections) are:
#' 
#' + `as.tonalChroma`:
#'        + `parts`: a character string of length 1--4, indicating which parts of the tonalChroma to print, and in which order.
#'           Acceptable strings are `"steps"`, `"qualities"`, `"accidentals"`, or `"contours"`.
#'           ([Partial matches][base::pmatch] matches like `"ste"` or `"s"` for `step` will work too.)
#'           The output tonal chroma string will have its part concatinated in the order they appear in the `parts` argument.
#'           For instance, you could put `c('contour', 'step', 'accidental')` to put the contour before the simple interval,
#'           or put `c('step', 'accidental', 'contour')` to put it after.
#'             + `sep`: a character string which will be used to separate the elements (the default is `""`).
#' + from `as.scaleStep` (described below):
#'        + `step.labels`
#' + from `as.accidental` (described below):
#'        + `accidental.labels` 
#'        + `accidental.maximum` and `accidental.minimum`
#'        + `accidental.cautionary`
#'        + `accidental.memory`
#'        + `Key`
#' + from `as.quality` (described below):
#'        + `quality.labels` 
#'        + `quality.maximum` and `quality.minimum`
#'        + `quality.cautionary`
#'        + `quality.memory`
#'        + `Key` 
#' + from `as.contour` (described below):
#'        + `contour.labels`
#'        + `contour.maximum` and `contour.minimum`
#'        + `contour.offset` 
#'        + `contour.round` 
#'        + `contour.delta` 
#' 
#' 
#' ## Scale Steps
#' 
#' A particular area of confusion in pitch representation is in the relationship between tonal (line-of-fifths) space and contour (frequency) space.
#' The line-of-fifths is an abstract space unrelated to specific frequencies---e.g., G is not inherently "above" or "below" C.
#' Still, musicians do not play abstractions, so we must always realize our line-of-fifths space in concrete frequency space.
#' By convention, collections from the line-of-fifths are conceptualized as frequency-orded "**scales**": folding the line-of-fifths into a sequence of intervals in the octave "above" the unison.
#' In most theoretical contexts, the order/"height" of elements in the scale is actually completely irrelavant.
#' Specifically, the first seven (diatonic) elements of the line-of-fifths are mapped into generic **scale-steps** in the order [0, 2, 4, 6, 1, 3, 5].
#' Scale-steps are strictly limited to a this seven-step range of a single diatonic octave, and are always "*ascending*"/"*positive*"---as they are steps in a pseudo-octave "above" the unison.
#' This representation of the line-of-fiths in terms of scale-steps is the most basic approach to representing pitch, and is associated with arguments containing `step`;
#' The function `as.scaleStep` is the basic building block of scale-step representations in humdrumR.
#' 
#' If we apply the modulo-7 operation to the line-of-fifths space, all intervals are mapped to these seven generic steps.
#' Alterations of the basic generic steps are represented separately (see **Qualities and Alterations** section below).
#' However, we should note that scale order is defined entirely based on the generic interval: for example, C-flat is by convention positioned "below"
#' C-natural in the scale, even though it (is often) equivalent to B-natural, which is at the "top" of the scale.
#' 
#' Generic, pseudo-ordered, scale-steps are conventionally represented in three forms: as letters (A-G), numbers (1-7 or I-VII), or conventional syllables (do, re, mi, fa...).
#' HumdrumR predefines several representations which include letters (A-G) as their scale-step encoding: `kernPitch`, `lilyPitch`, `helmholtz`, and `sciPitch`.
#' There are also standard numeric (`scaleDegree`) and syllable-based (`solfa`) based representations predefined.
#' The more generic `as.scaleStep` function can be used to write other variations of the scale step representations.
#' `as.scaleStep` includes two arguments:
#' 
#' + `step.labels`: a vector of seven elements (default = c('C', 'D', 'E', 'F', 'G', 'A', 'B')) representing the desired names for the seven steps in scale order.
#' 

#' 
#' ## Quality and Alterations
#' 
#' Generically, a tonal interval's *quality* indicates its diatonic mode and relationship to that mode. 
#' Specifically, alterations/accidentals indicate alterations of the key/mode while the quality indicates what the mode is.
#' 
#' 
#' The diatonic set is primarily defined by the major mode, consisting of the line-of-fifth range -1 to +5.
#' These seven intervals are the "*natural*" intervals---by default, unless a major context is specifically counter-indicated, the absence of an accidental
#' indicates these natural steps.
#' Any intervals outside of this scale can be indicated as alterations of the natural steps using accidentals:
#' There are two primary accidentals, [sharp]("https://en.wikipedia.org/wiki/Sharp_(music)") and [flat]("https://en.wikipedia.org/wiki/Flat_(music)"),
#' though they can be added together to create multi-sharps and multi-flats. (Sharps and flats cannot be mixed in `humdrumR`.)
#' An alteration corresponds to adding or subtracting multiples of `7` from the generic values (e.g., to "sharpen" a note means to add 7 to the line-of-fifths value).
#' Finally, a [natural]("https://en.wikipedia.org/wiki/Natural_(music)") accidental is used when the default natural step needs to be explicitely labeled.
#' 
#' The encoding of *qualities* is subtly distinct from alterations.
#' Qualities encode information about the diatonic mode of intervals, as well as alterations of mode.
#' The five consonant diatonic [modes]("https://en.wikipedia.org/wiki/Mode_(music)")---phyrigian (-5--+1), minor (-4--+2), dorian (-3--+3), mixolydian (-2--+4), and major (-1--+5)---all 
#' share the same the "*perfect*" first, fourth, and  fifth scale-steps---corresponding to the central -1--+1 on the line-of-fifths.
#' However, these five modes encompass two varieties of each of the "*imperfect*" second, third, sixth, and seventh scale steps:
#' the **major** steps (+2--+5) and the **minor** steps (-2---5), which are (obviously) inverses on the line-of-fifths.
#' Each minor/major pair is separated on line-of-fifths interval of `7`, and thus one can be altered into the other.
#' Anything on the line-of-fifths outside of the -5--+5 range are represented as one or more alterations of the perfect/imperfect degree.
#' Postive alterations (`+7`) are  **augmented** while negative alterations (`-7`) are **diminished**---like sharps and flat, augmentations and diminutions can be multipled.
#' 
#' The following table illustrates the relationship between the line-of-fifths, scale-steps, accidentals, and qualities:
#' 
#' ```{r, echo = FALSE, comment = NA}
#' 
#' f <- -10:14
#' print(data.frame(`LO5th` = f, 
#'                  Step = LO5th2scaleStep(f, c('C', 'D', 'E', 'F', 'G', 'A', 'B')), 
#'                  Accidental = LO5th2accidental(f, accidental.cautionary = TRUE), 
#'                  Quality = LO5th2quality(f, quality.cautionary = TRUE)), 
#'       row.names = FALSE)
#' 
#' ```
#' 
#' Note that accidentals are symetrical around the dorian mode (D), while qualities are symetrical around the major mode.
#' As a result, though sharp accidentals and augmentation qualities always correspond, flat accidentals are *not* equivalent to diminished qualities.
#' 
#' 
#' 

#' HumdrumR's predefined tonal chroma representations (`kernPitch`, `lilyPitch`, `helmholtz`, and `sciPitch`) each include alteration information through three accidental 
#' indications (sharp, flat, natural).
#' The `scaleDegree` and `interval` representations encode more general qualities using five categories (major, minor, diminished, augmented, perfect).
#' The `solfa` representation also encodes quality through conventional (non-systematic) vowel modifications.
#' More generally, the `as.quality` and `as.accidental` functions can be used extract quality information in isolation.
#' These functions use some (or all) of the following arguments:

#' 
#' + `_.labels`:
#'      + `accidental.labels`: a [named][base::names()] character vector which controls the characters used to represent accidentals. 
#'         The characters must be named either `flat`, `sharp`, `natural`, `doublesharp`, or `doubleflat`.
#'         The defaults are `c(flat = "b", sharp = "#", natural = "n")`.
#'         By default, `doubleflat` and `doublesharp` are not defined, and doubles are simply represented by two of the normal accidental symbols.
#'         However, if either double accidental is defined, every pair of accidentals is collapsed to the double version.
#'         
#'         If `accidental.labels == FALSE`, returns an integer value counting the number of alterations (positive for sharps, negative for flats).
#'      + `quality.labels`: a [named][base::names()] character vector which controls the characters used to represent qualities.
#'         The characters must be named either `perfect`, `major`, `minor`, `augment`, or `diminish`, with defaults 
#'         `c(perfect = "P", major = "M", minor = "m", augment = "A", diminish = "d")`.
#'  + `_.maximum` and `_.minimum`
#'      + Single integer values---defaults are `Inf` (maximum) and `-Inf` (minimum). 
#'      These arguments define the maximum number of multi-accidentals permitted.
#'      For instance, if `accidental.maximum == 2L`, accidentals of at most `"##"` will be output; a triple sharp will be reduced to just a `"##"`.
#'      The`_.minimum` argument must be negative (-2 == two flats).
#'      By default, the minimum is the inverse of the maximum, so if you want them to be the same, just set the maximum.
#'  + `Key`: a [diatonicSet] object describing the key and mode. Default is `NULL`. If `Key` is specified, only alterations relative to that key are printed 
#'     (unless the `_.cautionary` or `_.memory` arguments override this).
#'  + `_.cautionary`: a single logical value. Causes *more* accidentals/qualities to print (details below). Default is `FALSE` for accidentals and `TRUE` for qualities.
#'  + `_.memory`: a single logical value (default is `FALSE`). Causes *fewer* accidentals/qualities to print (details below).
#'  
#'  The master `as.tonalChroma` calls these functions, and can thus pass these arguments to them.
#'  For example, you could write `as.tonalChroma(x, accidental.maximum = 1)`.
#'  
#'  ### Cautionary Alterations
#'  
#'  The interplay between the `Key`, `_.cautionary`, and `_.memory` arguments control which accidentals/qualities are returned, allowing us to achieve various useful representations.
#'  Generally, the `Key` argument---if not `NULL`---causes only accidentals/qualities that are outside of the specified key (i.e., alterations) to print.
#'  The `cautionary` argument causes accidentals that would otherwise be suppressed to print---always *adding* accidentals to the output.
#'  Finally, the `_.memory` argument implents a common practice in music notation where alterations are only printed when the quality of a generic note is different than
#'  the last time that note appeared (i.e., earlier in the input vector). For instance, given an input with two F#s, the second F# will not be printed unless a F natural is sounded between them, 
#'  in which case, both the natural and the two sharps will print.
#'  Combinations of the `Key`, `_.cautionary` and `_.memory` arguments achieve the following effects:
#'  
#'  In cases where `Key` is `NULL`:
#'  
#'  + `_.cautionary == FALSE & _.memory == FALSE`: only alterations of the C major set are printed. No naturals are shown.
#'  + `_.cautionary == TRUE  & _.memory == FALSE`: **all** accidentals/qualities are printed (including all naturals).
#'  + `_.cautionary == FALSE & _.memory == TRUE`: alterations of C major set are printed, unless the previous instance of the step was already altered. 
#'     (Natural notes occuring after a previous alteration are marked natural.)
#'  + `_.cautionary == TRUE  & _.memory == TRUE`: **all** accidentals/qualities print, unless the previous instance of note was already altered. 
#'  
#'  If, on the other hand, if a `Key` is specified:
#'  
#'  + `_.cautionary == FALSE & _.memory == FALSE`: only alterations of the key are printed. 
#'  + `_.cautionary == TRUE  & _.memory == FALSE`: any alterations of the key are printed as well as any instances of their corresponding in-key quality, so as to assure there is no ambiguity.
#'  + `_.cautionary == FALSE & _.memory == TRUE`: alterations of the key are printed, unless the previous instance of note was already altered. In-key accidentals *are* printed if the previous instance of the note
#'     was altered.
#'  + `_.cautionary == TRUE  & _.memory == TRUE`: any alterations of the key are printed as well as their corresponding in-key quality, except for in-key accidentals that appear before any corresponding alterations.
#'    In other words, the `cautionary` rule is applied for any generic note once an alteration is introduced, but not before.
#'  
#'  
#'  Some standard, useful represenations are:
#'  + `Key == NULL & _.cautionary == FALSE & _.memory == FALSE`: print all accidentals always, but no naturals (normal kern style).
#'  + `Key == NULL & quality.cautionary == TRUE & quality.memory == FALSE`: print all qualities always (normal humdrum style for intervals).
#'  + `Key == _ & _.cautionary == FALSE & _.memory == TRUE`: print out-of-key accidentals, unless previous note was the same alteration (normal style for music notation).
#'  
#'  
#'  The following tables illustrate the behaviors of the accidental/quality arguments in eight different conditions.
#'  In the first four conditions, the `Key` argument is NULL, while in the second group of four the key is Ab major.
#'  Each group of four columns represent the four possible combinations of the `_.cautionary` and `_.memory` arguments, in the pattern
#'  `c(_.cautionary = FALSE, _.memory = FALSE)`, `c(_.cautionary = FALSE, _.memory = TRUE)`, `c(_.cautionary = TRUE, _.memory = FALSE)`, `c(_.cautionary = TRUE, _.memory = TRUE)`.
#'  
#'  #### Accidentals:
#'  
#'  ```{r, echo=FALSE, comment = NA}
#'  
#'  kern <- c('a-', 'a', 'b-', 'a', 'b-', 'a-', 'e', 'f', 'f#', 'e-', 'g-', 'f', 'b', 'c', 'd-', 'b-', 'e-', 'b-', 'a-')
#'  tint <- as.tonalInterval(kern)
#'  
#'  rbind( 
#'  NFF = tint2accidental(tint, Key = NULL, accidental.cautionary = FALSE, accidental.memory = FALSE, accidental.labels = c(flat = '-')),
#'  NFT = tint2accidental(tint, Key = NULL, accidental.cautionary = FALSE, accidental.memory = TRUE , accidental.labels = c(flat = '-')),
#'  NTF = tint2accidental(tint, Key = NULL, accidental.cautionary = TRUE , accidental.memory = FALSE, accidental.labels = c(flat = '-')),
#'  NTT = tint2accidental(tint, Key = NULL, accidental.cautionary = TRUE , accidental.memory = TRUE , accidental.labels = c(flat = '-')),
#'  AbFF = tint2accidental(tint, Key = dset(-4, -4), accidental.cautionary = FALSE, accidental.memory = FALSE, accidental.labels = c(flat = '-')),
#'  AbFT = tint2accidental(tint, Key = dset(-4, -4), accidental.cautionary = FALSE, accidental.memory = TRUE , accidental.labels = c(flat = '-')),
#'  AbTF = tint2accidental(tint, Key = dset(-4, -4), accidental.cautionary = TRUE , accidental.memory = FALSE, accidental.labels = c(flat = '-')),
#'  AbTT = tint2accidental(tint, Key = dset(-4, -4), accidental.cautionary = TRUE , accidental.memory = TRUE , accidental.labels = c(flat = '-'))
#'        ) -> mat
#'        
#'        mat <- t(apply(mat, 1, function(row) paste0(tint2scaleStep(tint, c('c', 'd', 'e', 'f', 'g', 'a', 'b')), row)))
#'        
#'      colnames(mat) <- kern
#'      print(format(mat, width = 4), quote = FALSE)
#'  
#'  ```
#'  
#'  #### Qualities:
#'  
#'  ```{r, echo=FALSE, comment = NA}
#'  
#'  kern <- c('a-', 'a', 'b-', 'a', 'b-', 'a-', 'e', 'f', 'f#', 'e-', 'g-', 'f', 'b', 'c', 'd-', 'b-', 'e-', 'b-', 'a-')
#'  tint <- as.tonalInterval(kern)
#'  
#'  rbind( 
#'  NFF = tint2quality(tint, Key = NULL, quality.cautionary = FALSE, quality.memory = FALSE),
#'  NFT = tint2quality(tint, Key = NULL, quality.cautionary = FALSE, quality.memory = TRUE ),
#'  NTF = tint2quality(tint, Key = NULL, quality.cautionary = TRUE , quality.memory = FALSE),
#'  NTT = tint2quality(tint, Key = NULL, quality.cautionary = TRUE , quality.memory = TRUE ),
#'  AbFF = tint2quality(tint, Key = dset(-4, -4), quality.cautionary = FALSE, quality.memory = FALSE),
#'  AbFT = tint2quality(tint, Key = dset(-4, -4), quality.cautionary = FALSE, quality.memory = TRUE ),
#'  AbTF = tint2quality(tint, Key = dset(-4, -4), quality.cautionary = TRUE , quality.memory = FALSE),
#'  AbTT = tint2quality(tint, Key = dset(-4, -4), quality.cautionary = TRUE , quality.memory = TRUE )
#'        ) -> mat
#'        
#'        mat <- t(apply(mat, 1, function(row) paste0(row, tint2scaleStep(tint))))
#'        
#'      colnames(mat) <- kern
#'      print(format(mat, width = 4), quote = FALSE)
#'  
#'  ```
#'  
#'  
#'  
#' ## Contour (e.g., Octave)
#' 
#' 
#' When we *do* wish to represent concrete, frequency-ordered information about a pitch, we add additional *contour* information to the simple, line-of-fifth representation to 
#' creating *complex intervals*.
#' Since scale-step representations ostensibly encode frequency-space ordering already, the first step is to take the scale-ordering literally---e.g, G really is "above" D---, 
#' creating what we call a **simple contour**.
#' To fully represent a complex interval, we can append zero or more octaves to the simple contour, what we call the **octave contour**.
#' 
#' In humdrumR, the symbols `"+"` and `"-"` are used to indicate the direction of the simple contour, with various scale-step and quality/alterations used to indicate the simple interval.
#' (In many representations, these symbols are implicit, and are thus ommitted.)
#' In contrast, the *octave contour* of an interval is simply an integer value, and can be represented a variety of ways:
#' either directly as an integer, or using repetitions of symbol pairs, most commonly `"^"`/`"v"` and `"'"`/`","`.
#' Other approaches represent octave contour information by changing the case (upper or lower) or repeating parts of the simple-interval string.
#' By default, the "same" option (i.e., a unison) for both types of contours is left blank as an empty string `""`.
#' 
#' The relationship between the simple contour and the octave contour is determined by how the scale step is "rounded" to the octave (see the *Tonality in humdrumR* vignette), which
#' is controlled by the `contour.round` argument.
#' The standard approach, with the scale steps all ascending above the octave is achieved by the *floor* function: we call this a **scale contour**.
#' Another approach is to use the *round* function, which rounds the nearest octave below **or** above, centering the steps around the unison instead of above it: the result is a 
#' scale like [-P4, -m3, -m2, P1, M2, M3, P4], which we call a **central contour**.
#' The difference between central contours, scale contours, and other simple contour options, are illustrated further below.
#' 
#' 
#' #### Serial vs Fixed Octave Contour
#' 
#' #' As described in the *Tonality in humdrumR* vignette, there are two general approaches to representing intervals: fixed-reference and serial-reference.
#' Most pitch representations are *fixed*-reference, with each token representing an interval relative to a common reference (middle-C, tonic, unison, etc.).
#' In contrast, a *serial* representation represents each interval cummulatively, relative to the previous interval: the most common example being melodic intervals.
#' TonalIntervals can be encode either fixed- or serial-reference intervals, and either approach can be partitioned into simple intervals and octave offsets.

#' In the most common cases for representing pitch, the simple contour is treated as the "always ascending" scale contour, and the octave contour is simply kept fixed.
#' Common approaches to pairing octave information with this include:
#' 
#' + **sciPitch** style: the octave offset is simply printed as an integer, though offset by +4 so that middle-C (unison) is `"4"`.
#' + **kernPitch** style: the scale step is lowercase for octave contours `>= 0` and uppercase otherwise. In addition, the scale step is repeated
#'    the absolute value of the octave contour times (offset +1 for positive octaves).
#'    For example, `(CCC = -3, CC = -2, C = -1, c = 0, cc = 1, ccc = 2)`.
#' + **lilyAbsolute** absolute style: the charactes `"'"` (positive) or `","` are repeated the absolute value of the octave contour.
#' + **helmholtz** style: an intersection of `lilyPitch`-absolute and `kernPitch` style, the case of the scale step is changed as in kern-style,
#'   and the `"'"`/`","` characters are printed a la lilypond absolute style.
#'   
#' However, another particularly useful possibility is to encode the octave part in serial reference and the simple part in fixed reference.
#' If we compute a serial-octave contour, and set our `contour.round` argument to `round`, the contour labels are blank (the "same" marker) if the interval has moved to the *closest* neighbor,
#' with the octave contour indicating moves away to that "closest note."
#' For instance, we can say, "play the F# above the previous note."
#' This is known as "relative pitch" representation in [LilyPond](https://lilypond.org/doc/v2.20/Documentation/notation/pitches)]:
#' If `relative = TRUE`, `as.lilyPitch` will compute markers labeled in this way.
#' This approach is can be combined with with scale-contour (`contour.round = floor`) representation of pitch as *scale degrees*, resulting in a scale degree representation with lossless
#' contour information!
#' 
#' To achieve these serial contours, we use the `contour.delta` and `contour.round` arguments.
#' If `delta = TRUE`, the contour of the serial representation of the input is computed, and the octave-part
#' of the contour is computed using the `contour.round` rounding function.
#' `floor` will create the "always ascending" scale contour, while `round` will achieve a centered contour.
#' `expand` will label *all* contours as ascending/descending except unisons.
#'   
#' The following table illustrates the different `contour.round` arguments when `delta = TRUE`:
#'   
#' ```{r, echo=FALSE, comment = NA}
#' as.tonalInterval(c('c','A','A','c','d','G','G','A','B-','f','f','e','d','e','d','c','ff','ee','dd','cc')) -> x
#' 
#' print(row.names = FALSE, 
#'       data.table(#Kern = as.kernPitch(x), 
#'                  round  = tint2contour(x, contour.delta = TRUE, contour.round = round),
#'                  floor  = tint2contour(x, contour.delta = TRUE, contour.round = floor),
#'                  expand = tint2contour(x, contour.delta = TRUE, contour.round = expand)
#'                  ))
#' 
#' ```
#' 
#' The following table illustrates the same three rounding functions, but with `contour.delta = FALSE`.
#' 
#'  ```{r, echo=FALSE, comment = NA}
#' as.tonalInterval(c('c','A','A','c','d','G','G','A','B-','f','f','e','d','e','d','c','ff','ee','dd','cc')) -> x
#' 
#' print(row.names = FALSE, 
#'       data.table(#Kern = as.kernPitch(x), 
#'                  round  = tint2contour(x, contour.delta = FALSE, contour.round = round),
#'                  floor  = tint2contour(x, contour.delta = FALSE, contour.round = floor),
#'                  expand = tint2contour(x, contour.delta = FALSE, contour.round = expand)
#'                  ))
#' 
#' ```
#' 
#' 
#' 
#' 
#' #### Complex Steps
#' 
#' The final standard approach to indicating complex contours is through complex steps: i.e., 9ths, 11ths, etc.
#' This approach directly represents full contour information as scale steps, with `+7` added for every octave contour.
#' This representation can only be accessed via the `as.interval` function.
#' To limit intervals to simple values, use the [tonalTransform] arguments `octave` and/or `roundMethod`.
#' 
#' 
#' 
#' ### as.contour
#' 
#' 
#' The `as.contour` function can be used to directly generate representations of the octave/contour, or can be called indirectly via `as.tonalChroma` or one of the standard
#' pitch representation functions (`as.kernPitch`, `as.sciPitch`, etc.).
#' The `as.octave` command is a synonym for `as.contour`.
#' The following arguments are defined for as contour:
#' 
#' 
#' + `contour.labels`: a [named][base::names()] `character` vector which controls the characters used to represent octave shifts.
#'   The characters must be named either `up`, `down`, or `same`; the defaults are `c(up = "^", down = "v", same = "")`.
#'   If `contour.labels = FALSE`, the octave offset (integer) itself is returned. 
#'      + If `contour.labels == FALSE`, returns a integer value counting the octave offset number.
#' + `contour.maximum` and `contour.minimum`
#'      + Single integer values---defaults are `Inf` (maximum) and `-Inf` (minimum). 
#'      These arguments define the maximum number of octave offset labels permitted.
#'      If `maximum == 2L`, octave offsets of at most (`"^^"`) will be output; a triple octave mark will be reduced to just a `"^^`.
#'      The`minimum` arguments should be negative.
#'      By default, the minimum is the inverse of the maximum, so if you want them to be the same, just set the maximum.
#' + `contour.offset`: a single integer, indicating the "center" octave. The default is `0L`, but *scientific pitch*
#'   uses `4L` (because middle C is "C4").
#' + `contour.delta`: a single logical vale. If `TRUE`, the *serial* contour is calculated.
#' + `contour.round`: a function, either `floor`, `round`, `trunc`, `expand` (see *Tonality in humdrumR* vignette).
#'   The default is `floor`, which is standard when `delta = FALSE`. 
#' 
#' 
#' @section Reading/Writing Atonal Pitch Information:
#' 
#' 
#' Atonal pitch representations predefined in `humdrumR` include `semit`, `midi`, `frequency`, `ratio`, and `fraction`.
#' As mentioned above, `integer` inputs are interpreted as semitones while `numeric` are interpreted as frequency ratios.
#' 

#' 
#'   
#' #### Interpreting Frequencies/Ratios
#' 
#' 
#' Four additional arguments help `humdrumR` determing how to interpret frequencies or ratios as `tonalInterval`s.
#' 
#' + `tonalHarmonic`: This determines the assumed tuning system by specifying the "tonal frequency ratio" associated 
#'    with an interval of a perfect 12th.
#'    The default is `3`, corresponding to pythagorean tuning; change it to `2^(19/12)` to use equal temeperament.
#' + `centMargin`: this numeric argument determines how close `humdrumR` tries to get (in cents) to the input ratio (default is 10 cents).
#'   If the `centMargin` is low, `humdrumR` may give you a crazy accidental like `A----` to get very close to the input ratio.
#'   If the `centMargin` is higher, `humdrumR` will give the most reasonable (fewest accidentals) `tonalInterval` within that margin.
#'   For example, `as.tonalInterval(1.44, centMargin = 10)` will return E##, because a pythagorean E## is within 10 cents of the ratio `1.44`.
#'   However, `as.tonalInterval(1.44, centMargin = 20)` will return the more resonable `F#`.
#' + `frequency.reference` and `frequencyTint`: these arguments determing what the reference frequency and note is for 
#'   when reading frequencies. The standard A = 440Hz corresponds to `(frequency.reference = 440, frequencyTint = as.tonalInterval('a'))`.
#' 
#' #### Tonal Decisions
#' 
#' When interpreting an atonal reprsentation as a tonal one there are multiple possibilities (for instance, midi note 61 could
#' be C# or Db).
#' The process `humdrumR` uses to determine the tonal representation of atonal input is influenced by the `accidental.melodic` and
#' `Key` arguments:
#' By default (`Key == NULL & accidental.melodic == FALSE``), the line-of-fifths range -3--8 (E flat to G sharp) is used.
#' However, if a `Key` argument is specified, this line-of-fifths range will be shifted to match the corresponding key signature.
#' For instance, the key Bb minor would read accidentals in the range -8--3 (Fb flat to A natural).
#' If `accidental.melodic == TRUE`) the atonal notes are interpreted "melodically"---i.e., 
#' ascending chromatic steps as sharps and descending chromatic steps as flats.
#' When reading frequencies/ratios, a "pure" `tonalHarmonic` value---like the pure `3` of pythagorean tuning---will help determine
#' the appropriate accidental.
#' However, equal temperament will not provide useful information, so the `Key` and `accidental.melodic` arguments can be used instead.
#'   
#'   
#' 
#' 
#' @name pitchRepresentations
NULL

####. generics ####

#' @export as.tonalInterval 
#' @export as.semit as.midi
#' @export as.tonalChroma as.scaleStep as.accidental as.quality as.contour
#' @export as.sciPitch as.kernPitch as.lilyPitch as.helmholtz
#' @export as.interval as.scaleDegree as.solfa
#' @export as.rational as.fraction as.decimal as.frequency
NULL

pitchgeneric <- function(pitchrep, secondargs = alist(), endargs = alist()) {
  rlang::new_function(c(alist(x = ),
                        secondargs,  
                        alist(direction = , delta = , sigma = ,
                            generic = , simple = , roundContour = ,  enharmonic.part = ,
                            Key = , Exclusive = , inPlace = ),
                        endargs,
                        alist(... = )),
                      rlang::expr(UseMethod(!!pitchrep)),
                      env = parent.frame())
}

as.tonalInterval <- pitchgeneric("as.tonalInterval")
as.semit         <- pitchgeneric("as.semit")
as.midi          <- pitchgeneric("as.midi")
as.tonalChroma   <- pitchgeneric("as.tonalChroma")
as.scaleStep     <- pitchgeneric("as.scaleStep",  alist(step.labels = ))
as.accidental    <- pitchgeneric("as.accidental", alist(accidental.labels = , accidental.maximum =, accidental.minimum =, accidental.cautionary = , accidental.memory = ))
as.quality       <- pitchgeneric("as.quality",    alist(quality.labels = , quality.maximum =, quality.minimum =, quality.cautionary = , quality.memory = ))
as.contour       <- pitchgeneric("as.contour",    alist(contour.labels = , contour.maximum =, contour.minimum =, contour.offset = , contour.delta = , contour.round = ))
as.sciPitch      <- pitchgeneric("as.sciPitch")
as.kernPitch     <- pitchgeneric("as.kernPitch")
as.lilyPitch     <- pitchgeneric("as.lilyPitch")
as.helmholtz     <- pitchgeneric("as.helmholtz")
as.interval      <- pitchgeneric("as.interval")
as.scaleDegree   <- pitchgeneric("as.scaleDegree")
as.solfa         <- pitchgeneric("as.solfa")
as.frequency     <- pitchgeneric("as.frequency", alist(frequency.reference = , frequencyTint = , tonalHarmonic = ))
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

char2tint <- humdrumDispatch('kern: makeRE.kernPitch(...)' = kernPitch2tint,
                             'pitch: makeRE.sciPitch(...)' = sciPitch2tint,
                             'hint: makeRE.interval(...)'  = interval2tint,
                             'mint: makeRE.interval(...)'  = interval2tint,
                             'int: makeRE.interval(...)'  = interval2tint,
                             'deg: makeRE.scaleDegree(...)'  = scaleDegree2tint,
                             'solfa: makeRE.solfa(...)' = solfa2tint,
                             'freq: makeRE.decimal()' = semit2tint,
                             'fraction: makeRE.fraction(...)' = fraction2tint)

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
as.scaleStep.tonalInterval   <- tint2scaleStep   %.% tonalTransform
#' @export
as.accidental.tonalInterval  <- tint2accidental  %.% tonalTransform
#' @export
as.quality.tonalInterval     <- tint2quality     %.% tonalTransform
#' @export
as.contour.tonalInterval     <- tint2contour     %.% tonalTransform
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
as.scaleStep.integer   <- tint2scaleStep   %.% as.tonalInterval.integer
#' @export
as.accidental.integer  <- tint2accidental  %.% as.tonalInterval.integer
#' @export
as.quality.integer     <- tint2quality     %.% as.tonalInterval.integer
#' @export
as.contour.integer     <- tint2contour     %.% as.tonalInterval.integer
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
as.scaleStep.numeric   <- tint2scaleStep   %.% as.tonalInterval.numeric
#' @export
as.accidental.numeric  <- tint2accidental  %.% as.tonalInterval.numeric
#' @export
as.quality.numeric     <- tint2quality     %.% as.tonalInterval.numeric
#' @export
as.contour.numeric     <- tint2contour     %.% as.tonalInterval.numeric
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
as.scaleStep.character   <- re.place %.% tint2scaleStep   %.% as.tonalInterval.character
#' @export
as.accidental.character  <- re.place %.% tint2accidental  %.% as.tonalInterval.character
#' @export
as.quality.character     <- re.place %.% tint2quality     %.% as.tonalInterval.character
#' @export
as.contour.character     <- re.place %.% tint2contour     %.% as.tonalInterval.character
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











############# Known tonalIntervals ####
#' @name tonalInterval
#' @export dd1 dd2 A2 P3 d4 d5 d6 AA6 M7 m8 dd9 A9 P10 d11 d12 d13 AA13 M14 P15
#' @export d1 d2 AA2 M3 P4 P5 m6 dd7 A7 P8 d9 AA9 M10 P11 P12 m13 dd14 A14 A15
#' @export P1 m2 dd3 A3 A4 A5 P6 d7 AA7 M8 m9 dd10 A10 A11 A12 P13 d14 AA14 AA15
#' @export A1 P2 d3 AA3 AA4 AA5 M6 m7 dd8 A8 P9 d10 AA10 AA11 AA12 M13 m14 dd15
#' @export AA1 M2 m3 dd4 dd5 dd6 A6 P7 d8 AA8 M9 m10 dd11 dd12 dd13 A13 P14 d15
#' @export unison pythagorean.comma octave
NULL
allints <- outer(c('dd', 'd', 'm', 'P', 'M', 'A', 'AA'), 1:15, paste0)
allints[as.matrix(expand.grid(c(3,5), c(1,4,5,11,12,15)))] <- NA
allints <- c(allints)
allints <- allints[!is.na(allints)]
# cat(paste0("#' @export ", unlist(tapply(allints, rep(1:5, length.out = length(allints)), paste, collapse = ' '))), sep = '\n')
for (int in allints) {
  assign(int, interval2tint(int))
}
unison <- P1
pythagorean.comma <- (-d2)
octave <- P8
