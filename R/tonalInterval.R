################################## ###
# tonalInterval S4 class #############
################################## ###



## tonalIntervalS4 documentation ----


#' Representation of tonal pitch information
#' 
#' The `tonalInterval` is the core tonal pitch representation in [humdrumR][humdrumR::humdrumR].
#' A `tonalInterval` is an abstract representation of tonal pitch, which can be translated to/from all standard "concrete" pitch representations:
#' solfege, scientific pitch, semitones, frequencies, scale degrees, intervals, etc.
#' For the most part, users should not need to interact with `tonalInterval`s directly---rather, `tonalInterval`s work behind the scene in numerous `humdrumR` pitch functions.
#' See the [pitchRepresentations] and [tonalTransformations] documentation for details of usage and functionality or the *Tonality in humdrumR* vignette for 
#' a detailed explanation of the theory and specifics of `tonalInterval`s.
#'
#' 
#' The `tonalInterval` is a [S4](http://adv-r.had.co.nz/S4.html) subclass of `humdrumR`'s virtual class [struct], from which it inherits a lot of useful "vector-like" behaviors/functionality.
#' 
#' The constructor function `tint` can be used to create `tonalIntervals` directly.
#' The three arguments corespond to the three slots: `octave`, `LO5th` (Fifth), and `cent`.
#' All inputs will be coerced to match in length.
#' What's more, the `octave` argument can be left blank, in which case the appropriate octave will automatically be computed
#' to place the interval in the octave above middle-C.
#' 
#' By default, the [as.character][base::character] method, and thus (via [struct]) the [show][methods::show] method, for `tonalInterval`s call [kern()][pitchRepresentations].
#' Thus, if you return a `tonalInterval` on the command line (or call [print][base::print] one one) you'll see the [kern pitch][pitchRepresentations] representation printed.
#' 
#' @slot Octave integers representing the octave offset.
#' @slot Fifth integers representing the "line-of-fifths" value.
#' @slot Cent numeric values representing cents (1200th of an octave).
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
#' Take note that the way `humdrumR` defines Euclidean division is based in *tonal space*---i.e., the line-of-fifths---not frequency or atonal-semitone space.
#' For example, an augmented-fourth divided by a major-second *is* `3L`, but a diminished-fifth divided by a major-second is *not* 3L---`d5 %/% M2` equals `-3L` with a remainder of `P8` (plus an octave)!
#' The division algorithm works by applying standard Euclidean division to the `@Fifth` slot (line-of-fifths tonal space), and shifting the `@Octave` value in
#' the remainder to the match the appropriate octave.
#' This definition has the useful properties that `specificinterval %% A1 = genericinterval` and `interval %% d2 = enharmonicinterval`.
#' 
#' Since basic data types can be parsed into `tonalInterval` (see below), `humdrumR` will attempt to automatically [coerce](https://en.wikipedia.org/wiki/Type_conversion)
#' data to tonalIntervals when asked to perform arithmetic.
#' This means that arithmetic can be applied when one of the two arguments is a `tonalInterval` and the other is a coercable atomic.
#' For instance, `M3 + 2L` will interpret `2L` as two semitones and add a major-second to the major-third!
#' The clever [dispatch system][humdrumR::regexDispatch] will even ignore character strings that are not recognized (see examples)!
#' This is useful when combined with the "Predifined Intervals" (like `M3`), described below.
#' 
#' 
#' ## Relational Operators
#' 
#' `tonalInterval`s can be compared using the standard [relational operations][base::Comparison]---`==`, `!=`, `>`, `>=`, etc.
#' Two `tonalInterval`s are equal (according to `==`) only if all their slots (`Octave`, `Fifth`, and `Cent`)
#' are exactly identical. 
#' Thus, enharmonic notes (like C# and Db) are *not* equal.
#' In contrast, ordinal comparisons (e.g., `>`, `<=`) between `tonalInterval`s are based on their semitone (equal temperament) size, so enharmonicity is irrelevant.
#' Thus, `m3 >= A2` and `A2 >= m3` are both `TRUE`, even though `m3 == A2` is not.
#' 
#' 
#' ## Predefined Intervals:
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
#' @family {core pitch representation}
#' @seealso [tonalInterval]
#' @name tonalIntervalS4
NULL

## Definition, validity, initialization ####
#' @export 
setClass('tonalInterval', 
         contains = 'struct',
         slots = c(Fifth  = 'integer', 
                   Octave = 'integer', 
                   Cent   = 'numeric')) 



setValidity('tonalInterval', 
            \(object) {
              all(abs(object@Cent) <= 1200, na.rm = TRUE)
            })

setMethod("initialize", 
          "tonalInterval",
          \(.Object, Fifth = 0L, Octave = 0L, Cent = 0L) {
              .Object <- callNextMethod() # call the struct initialize
              Cent <- .Object@Cent
              if (any(abs(Cent) >= 1200L, na.rm = TRUE)) {
                  centOctaves <- .ifelse(Cent == 0L, 
                                         0L,
                                         as.integer((Cent %/% (sign(Cent) * 1200)) * sign(Cent)))
                  .Object@Octave <- .Object@Octave + centOctaves 
                  Cent   <- Cent  - (1200 * centOctaves)
                 .Object@Cent   <- Cent
              }
              .Object
          }) 


## Constructors ####

#' @rdname tonalIntervalS4
#' @export
tint <- function(octave, LO5th = 0L, cent = numeric(length(octave)), partition = FALSE, Key = NULL, roundContour = floor) {

    if (missing(octave) || is.null(octave)) {
      octave <- -floor(tint2semit(tint(integer(length(LO5th)), LO5th) %% tint(-11L, 7L)) / 12)
    }
    
  
    tint <- new('tonalInterval',  Octave = as.integer(octave),  Fifth  = as.integer(LO5th),  Cent   = as.numeric(cent)) 
    tint <- tint %<-matchdim% (if (size(tint) == size(LO5th)) LO5th else octave)
    if (partition) tintPartition(tint, Key = Key, roundContour = roundContour) else tint
}

## Accessors ####

#' Line of Fifths
#' 
#' The function `LO5th` is a S3-generic function with methods to extract
#' the "line-of-fifths" value from various pitch objects and representations.
#'
#' ## The Line of Fifths
#' 
#' Every interval in Western music is associated with a integer on the line of fifths:
#' 
#' + Bb = m7 = -2 
#' + F =  P4  = -1 
#' + C =  P1 = 0
#' + G =  P5 = 1
#' + D =  M2 = 2
#' + A =  M6 = 3
#' + E =  M3 = 4
#' + B =  M7 = 5
#' + F# = A4 = 6
#' + etc.
#' 
#' The natural notes of (C) major scale---which we also call the *generic intervals*---fall in the range `-1:5`.
#' In fact, any diatonic key is a block of seven consecutive numbers of the line-of-fifths: for example, Eb major is `-4:2`.
#' "Sharps" and "flats" represent `+7` or `-7` on the line-of-fifths respectively.
#' 
#' 
#' @family {core pitch representations}
#' @seealso `LO5th`s are a core component of the [tonalIntervalS4] representation, and are thus 
#'          an argument to the `tonalInterval` constructor: [tint(octave, LO5th)][tint()].
#'          Also, see the [tonalInterval] function, which is used by many `LO5th` methods. 
#' @return Returns an integer vector or array, matching the input.
#' @export
setGeneric("LO5th", function(x, generic = FALSE, ...) standardGeneric("LO5th"))
setMethod("LO5th", "tonalInterval",
          function(x, generic = FALSE) {
            LO5th <- x@Fifth
            if (generic) LO5th <- genericFifth(LO5th)
            LO5th %<-matchdim% x
          })
setMethod("LO5th", 'matrix',
          function(x, generic = FALSE) {
            lo5th <- LO5th(c(x))
            lo5th %<-matchdim% x
          })
setMethod('LO5th', 'ANY',
          function(x, generic = FALSE) {
            x <- as(x, 'tonalInterval')
            LO5th(x, generic = generic)
          })



getFifth  <- function(tint, generic = FALSE) LO5th(tint, generic = generic)
getOctave <- function(tint) tint@Octave %<-matchdim% tint

genericFifth <- function(LO5th) ((LO5th + 1L) %% 7L) - 1L
genericStep <- function(x) ((x - 1L) %% 7L) + 1L


## Formatting methods ####

setMethod('as.character', signature = c('tonalInterval'), 
          function(x) kern(x))

setMethod('as.numeric', signature = c('tonalInterval'), 
          function(x) tint2double(x))


## Logic methods ####

### is.methods ####


#' @name tonalIntervalS4
#' @export
is.tonalInterval <- function(x) inherits(x, 'tonalInterval')

#### Tonal is.methods ####

is.simple <- function(tint) UseMethod('is.simple')

#' @export
is.simple.tonalInterval <- function(tint) abs(tint2semit(tint)) < 12



## Order/relations methods ####

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
            neg <- x < tint(0L, 0L)
            x[neg] <- -x[neg]
            x
          })

setMethod('sign', signature = c('tonalInterval'),
          function(x) {
              sign(tint2semit(x))
          })



## Arithmetic methods ####

### Addition ####

setMethod('+', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
              e1 <- tonalInterval.character(e1, memoize = FALSE)
              
              e3 <- e1 + e2
              dispatch <- attr(e1, 'dispatch')
              
              rePlace(reParse(e3, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
              
          })

setMethod('+', signature = c('tonalInterval', 'character'),
          function(e1, e2) {
            e2 <- tonalInterval.character(e2)
            
            e3 <- e1 + e2
            dispatch <- attr(e2, 'dispatch')
            
            rePlace(reParse(e3, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
              
          })



### Subtraction ####

setMethod('-', signature = c('character', 'tonalInterval'),
          function(e1, e2) {
            e1 <- tonalInterval.character(e1)
            
            e3 <- e1 - e2
            dispatch <- attr(e1, 'dispatch')
            rePlace(reParse(e3, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
          })


setMethod('-', signature = c('tonalInterval', 'character'),
          function(e1, e2) {
            e2 <- tonalInterval.character(e2)
            
            e3 <- e1 - e2
            dispatch <- attr(e2, 'dispatch')
            rePlace(reParse(e3, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
          })


### Multiplication ####

# Inherits struct methods!

### Division/modulo  ####

 
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
              
              tint %<-matchdim% e1
              
          })

setMethod('%/%', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%/%) by empty value.")
            recycledim(e1 = e1, e2 = e2, funccall = '%/%')
            
              f1 <- e1@Fifth
              f2 <- e2@Fifth
              
              f3 <-  f1 %/% f2
              f3 %<-matchdim% e1
          })


setMethod('%%', signature = c('tonalInterval', 'integer'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%%) by empty value.")
            
            minusremain <- (e1 %/% e2) * e2
            
            (e1 - minusremain) %<-matchdim% e1
          })

setMethod('%/%', signature = c('tonalInterval', 'integer'),
          function(e1, e2) {
            if (length(e1) == 0L) return(e1)
            if (length(e2) == 0L) stop(call. = FALSE, "Can't divide (%/%) by empty value.")
            recycledim(e1 = e1, e2 = e2, funccall = '%/%')
            
            
            tint <- tint(e1@Octave %/% e2, e1@Fifth %/% e2)
            
            tint %<-matchdim% e1
          })







###################################################################### ###
# Deparsing Pitch Representations (tint2x) ###############################
###################################################################### ###

## Deparsing documentation ----

#' Deparsing pitch information
#' 
#' 

#' @seealso [tonalInterval], [tonalIntervalS4]
#' @name pitchDeparsing
NULL

## Pitch deparsers ####

### Octaves ####


tint2octave <- function(x,
                        octave.integer = TRUE,
                        up = '^', down = 'v', same = "",
                        octave.offset = 0L, octave.maximum = Inf, octave.minimum = -Inf,
                        octave.relative = FALSE, octave.round = floor, ...) {

  if (octave.relative) x <- delta(x)
  #
  octn <- octave.offset + tintPartition_complex(x, octave.round = octave.round)$Octave@Octave
  octn <- pmin(pmax(octn, octave.minimum), octave.maximum)
  
  if (octave.integer) return(as.integer(octn))
  
  out <- rep(NA_character_, length(octn))
  out[octn == 0L] <- same
  out[octn != 0L] <- strrep(ifelse(octn[octn != 0L] >= 0L, up, down), abs(octn[octn != 0L]))
  
  out 
  
}

tint2sign <- function(x, octave.offset = 0L, ...) {
 sign(tint2semit(x) + octave.offset * 12L)
}


octave.kernstyle <- function(str, octn, step.case = TRUE) {
  char <- substr(str, 0L, 1L)
  if (step.case) char <- .ifelse(octn >= 0L, tolower(char), toupper(char))
  
  octn[!is.na(octn) & octn >= 0L] <- octn[!is.na(octn) & octn >= 0L] + 1L # 0 -> 1
  
  .paste(strrep(char, abs(octn)), stringr::str_sub(str, start = 2L)) %<-matchdim% str
}


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





### Atonal ####

#### Semitones ####

tint2semit <- function(x, Key = NULL, specific = TRUE, complex = TRUE, ...) {

  
  if (!is.null(Key) && length(x) > 0L) x <- x + diatonicSet(Key)
  
  if (!specific) x <- tintPartition_specific(x, Key = Key, ...)$Generic
        
  semit <- as.integer((((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L)))
  
  if (!complex) semit <- semit %% 12L

  semit
  
}

tint2midi <- function(x, ...) {
  tint2semit(x, ...) + 60L
}


#### Frequency ####



tint2rational <-  function(x, tonalHarmonic = 3L) {
  Fifth <- x@Fifth
  Octave <- x@Octave
  Cent <- x@Cent
  
  num <- .ifelse(Fifth >= 0L, tonalHarmonic ^  Fifth, 1L) * .ifelse(Octave >= 0L, 2L ^  Octave, 1L)
  den <- .ifelse(Fifth <  0L, tonalHarmonic ^ -Fifth, 1L) * .ifelse(Octave <  0L, 2L ^ -Octave, 1L)
  
  
  if (any(Cent != 0)) {
    floats <- as.rational.numeric(tint2double(x[Cent != 0], tonalHarmonic))
    num[Cent != 0] <- floats@Numerator
    den[Cent != 0] <- floats@Denominator
    
  } 
  
  rational(as.integer(num), as.integer(den)) %<-matchdim% x
  
}

tint2fraction <- function(x, tonalHarmonic = 3) as.fraction(tint2rational(x, tonalHarmonic = tonalHarmonic)) 

tint2double <-  function(x, tonalHarmonic = 2^(19/12)) {
  LO5th <- x@Fifth
  oct   <- x@Octave
  cent  <- x@Cent
  
  .ifelse(is.na(LO5th), 
          NA_real_, 
          (2 ^ oct) * (tonalHarmonic ^ LO5th) * 2^(cent / 1200)) %<-matchdim% x
}

tint2frequency <- function(x, frequency.reference = 440L, 
                           frequencyTint = tint(-4L, 3L), 
                           tonalHarmonic = 2^(19/12)) {
  x <- x - frequencyTint
  
  ratio <- tint2double(x, tonalHarmonic = tonalHarmonic)
  attributes(ratio) <- NULL
  
  frequency.reference * ratio %<-matchdim% x
}



### Tonal ####

tint2step  <- function(x, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), ...) {
  
  step.labels[c(1L, 5L, 2L, 6L, 3L, 7L, 4L)][1 + (LO5th(x) %% 7)]
} 


###### Alteration stuff ###




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
         \(gen) {
           cur <- generic  == gen
           which(cur)[c(FALSE, diff(LO5th[cur]) == 0L)]
         }) |> unlist() -> hits
  
  list(sameasbefore = seq_along(LO5th) %in% hits,  
       new = seq_along(LO5th) %in% unlist(lapply(-1:5L, match, table = generic)))
  
}

alteration.inKey <- function(LO5th, Key) {
  LO5th2alterationN(LO5th, Key) == 0L
}



alteration.filter <- function(LO5th, Key, cautionary, memory, implicit, explicitNaturals) {
  # determines which notes need an specifier (FALSE) and which don't (TRUE)
  output <- logical(length(LO5th))
  
  if (!explicitNaturals) output[LO5th > -2L & LO5th < 6L] <- TRUE
  
  if (!(memory || cautionary || implicit)) return(output) 
  
  # implicit must be TRUE
  output <- alteration.inKey(LO5th, Key) 
  if (!(memory || cautionary)) return(output)
  
  
  conflicted <- alteration.conflicts(LO5th)
  mem  <- alteration.memory(LO5th)
  
    
  if ( cautionary & !memory) return(output & !conflicted)
  if (!cautionary &  memory) return((mem$sameasbefore & !mem$new) | (mem$new & output))
  if ( cautionary &  memory) return(mem$sameasbefore & !mem$new)
    
  
  
}


LO5th2alterationN        <- function(LO5th, Key = dset(0L, 0L)) ((LO5th - (LO5th %% Key)) %/% 7L)


tint2quality <- function(x, ...) tint2specifier(x, ..., qualities = TRUE)
tint2accidental <- function(x, ...) tint2specifier(x, ..., qualities = FALSE)

tint2specifier <- function(x, Key = NULL, ...,
                           qualities = FALSE,
                           cautionary = FALSE, memory = FALSE, parseWindows = NULL,
                           implicitSpecies = FALSE, absoluteSpecies = TRUE, explicitNaturals = FALSE,
                           sharp = '#', flat = '-', natural = 'n', 
                           doublesharp = FALSE, doubleflat = FALSE, 
                           perfect = 'P', major = 'M', minor = 'm', augment = 'A', diminish = 'd',
                           specifier.maximum = Inf, accidental.integer = FALSE) {
  LO5th <- LO5th(x)
  if (!absoluteSpecies) {
    LO5th <- LO5th - getSignature(Key)
    Key <- Key - getMode(Key)
  }
  
  
  dontlabel <- if (truthy(parseWindows) && length(parseWindows) == length(LO5th)) {
    tapply_inplace(LO5th, parseWindows,  \(x) alteration.filter(x, Key, cautionary, memory, implicitSpecies, explicitNaturals))
  } else {
    alteration.filter(LO5th, Key, cautionary, memory, implicitSpecies, explicitNaturals)
  }
  
  
  specifiers <- ifelse(dontlabel, "", if (qualities) major else natural)
  
  na <- is.na(LO5th)
  specifiers[na] <- NA_character_
  
  if (qualities) {
    specifiers[!na & LO5th <  2 & LO5th > -2 & !dontlabel] <- perfect
    specifiers[!na & LO5th > -6 & LO5th < -1 & !dontlabel] <- minor      
    
    specifiers[!na & LO5th >   5 & !dontlabel] <- strrep(augment,  pmin(abs(LO5th2alterationN(LO5th[!na & LO5th  >  5 & !dontlabel]     )),  specifier.maximum))
    specifiers[!na & LO5th <= -6 & !dontlabel] <- strrep(diminish, pmin(abs(LO5th2alterationN(LO5th[!na & LO5th <= -6 & !dontlabel] + 4L)),  specifier.maximum))    
    
  } else {
    # get "absolute" (c major) accidentals
    accidentalN <- pmaxmin(LO5th2alterationN(LO5th, dset(0L, 0L)), -specifier.maximum, specifier.maximum) # to c major
    
    if (accidental.integer) return(accidentalN)
    
    specifiers[!na & accidentalN > 0L & !dontlabel] <- strrep(sharp, accidentalN[!na & accidentalN > 0L & !dontlabel])
    specifiers[!na & accidentalN < 0L & !dontlabel] <- strrep(flat,  abs(accidentalN[!na & accidentalN < 0L & !dontlabel]))
    
    #
    if (!false(doublesharp)) specifiers <- stringi::stri_replace_all_fixed(specifiers, pattern = strrep(sharp, 2L), doublesharp)
    if (!false(doubleflat)) specifiers <- stringi::stri_replace_all_fixed(specifiers, pattern = strrep(flat , 2L), doubleflat)
  }
  
  
  specifiers
}

Nupdown <- function(n, up = '^', down = 'v') ifelse(n >= 0, strrep(up, abs(n)), strrep(down, abs(n)))

tint2tonalChroma <- function(x, 
                             parts = c("species", "step", "octave"), sep = "", 
                             step = TRUE, specific = TRUE, complex = TRUE,
                             keyed = FALSE, Key = NULL,
                             qualities = FALSE, collapse = TRUE, ...) {
  
  
  if (keyed && !is.null(Key)) {
    Key <- rep(Key, length.out = length(x))
    x[!is.na(Key)] <- x[!is.na(Key)] + diatonicSet(Key[!is.na(Key)])
  }
  
  parts <- matched(parts, c( "species", "step", "octave"))
  

  
  # simple part
  step     <- if (step)        tint2step(x, ...) 
  species  <- if (specific)    tint2specifier(x, qualities = qualities, Key = Key, ...)   
  
  
  # complex part
  octave  <- if (complex) {
    octave <- tint2octave(x, ...)
    if (is.integer(octave) && is.integer(step)) {
      step <- step + octave * 7L
      ""
    } else {
      octave
    }
    
  }

  # direction
  # directed <- if (directed) {
  #   sign <- tint2sign(x, ...)
  #   x <- abs(x * sign)
  #   c('-', '', '+')[sign + 2L]
  # }
  
  pasteordered(parts, step = step, species = species, octave = if (complex) octave, sep = sep, collapse = collapse)
    
  
}





tint2pitch <- partialApply(tint2tonalChroma,  
                           step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                           octave.offset = 4L, integer = TRUE,
                           flat = 'b', qualities = FALSE,
                           keyed = TRUE,
                           parts = c("step", "species", "octave"))





tint2kern <- function(x, complex = TRUE, Key = NULL, ...) {
  
  t2tC <- partialApply(tint2tonalChroma,
                       step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                       parts = c("step", "species"), qualities = FALSE, complex = FALSE,
                       keyed = TRUE)
  
  kern <- t2tC(x, Key = Key, ...)
  
  
  # if (directed) {
  #   direction <- stringr::str_extract(kern, '^[+-]?')
  #   kern <- tolower(stringr::str_remove(kern, '^[+-]'))
  # }  else {
  #   direction <- ""
  # }
  
  
  if (complex) {
    kern <- octave.kernstyle(kern, tint2octave(if (is.null(Key)) x else x + Key, octave.integer = TRUE), step.case = TRUE)
  }
  
  kern
  
}



tint2lilypond <- partialApply(tint2tonalChroma, 
                              step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                              up = "'", down = ",",
                              qualities = FALSE,
                              octave.relative = FALSE, octave.integer = FALSE,
                              octave.round = if (octave.relative) round else floor,
                              octave.offset = 1L, 
                              sharp = 'is', flat = 'es',
                              parts = c("step", 'species', "octave"))

tint2helmholtz <- function(x, ...) {
  
  octn <- tint2octave(x, octave.integer = TRUE, octave.offset = 1L)
  
  
  x[octn < 0L] <- x[octn < 0L] + tint(1L, 0L)
  
  t2tC <- partialApply(tint2tonalChroma,  
                       step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                       flat = 'b', parts = c('step', 'species', 'octave'),
                       up = "'", down = ",", octave.offset = 1L, octave.integer = FALSE)
  
  notes <- t2tC(x, ...)
  
  notes[octn < 0L] <- stringr::str_to_title(notes[octn < 0L]) 
  notes
}
                                                                

tint2romanRoot <- partialApply(tint2tonalChroma, 
                               step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'), 
                               octave.integer = TRUE, octave.offset = 4L, 
                               qualities = FALSE,
                               parts = c('species', "step"), Key = NULL)




tint2interval <- function(x, directed = TRUE, ...) {
  
  t2tC <- partialApply(tint2tonalChroma,
                       step.labels = 1L:7L,
                       parts = c("species", "step", "octave"),
                       complex = TRUE, keyed = FALSE, qualities = TRUE, 
                       octave.integer = TRUE, octave.relative = FALSE, explicitNaturals = TRUE,
                       octave.round = floor)
  
  direction <- if (directed) {
    c('-', '', '+')[sign(x) + 2L]
  } else {
    ""
  }
  x <- abs(x)
  
  interval <- t2tC(x, ...)
  
  paste0(direction, interval)
}




tint2degree <- partialApply(tint2tonalChroma, parts = c("octave", "species", "step"), 
                            complex = FALSE, keyed = FALSE, step.labels = 1L:7L, flat = 'b',
                            octave.integer = FALSE, octave.relative = TRUE, octave.round = round)


tint2bhatk <- function(x, ...) {
  t2tC <- partialApply(tint2tonalChroma,
                       step.labels = c('S', 'R', 'G', 'M', 'P', 'D', 'N'), 
                       octave.integer = FALSE, 
                       up = "'", down = ',')
  bhatk <- t2tC(x, ...)
  
  bhatk[grepl('[-#]', bhatk)] <- tolower(bhatk[grepl('[-#]', bhatk)])
  
  gsub('[-#]*', '', bhatk)
  
}



tint2solfa <- function(x, Key = NULL,  parts = c("octave", "step", 'species'), 
                       generic = FALSE, flat = '-', sharp = '#', factor = FALSE, ...) {
  
  
  t2tC <- partialApply(tint2tonalChroma, octave.integer = FALSE, octave.relative = FALSE,
                       step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't'), 
                       qualities = FALSE, accidental.integer = TRUE)
  
  solfa_parts <- t2tC(x, generic = generic, ..., collapse = FALSE) #
  
  # change species to syllable "tails"
  solfa_parts$tail <- if (!generic) {
    solfatails <- rbind(d = c("e", "o", "i"),
                        r = c("a", "e", "i"),
                        m = c("e", "i", "y"),
                        f = c("e", "a", "i"),
                        s = c("e", "o", "i"),
                        l = c("e", "a", "i"),
                        t = c("e", "i", "y"))
    accidental.integer <- solfa_parts$species
    
    solfa_parts$species <- Nupdown(accidental.integer - sign(accidental.integer), up = sharp, down = flat)
    
    solfatails[cbind(match(solfa_parts$step, rownames(solfatails)),  sign(accidental.integer) + 2L)]
    
  } else {
    c(d = 'o', r = 'e', m = 'i', f = 'a', s = 'o', l = 'a', t = 'i')[solfa_parts$step]
  }
  
  if ('step' %in% parts) parts <- append(parts, 'tail', after = which(parts == 'step'))
  
  solfa_parts <- solfa_parts[ , intersect(parts, colnames(solfa_parts))]
  
  do.call('.paste', solfa_parts)

}  





###################################################################### ### 
# Parsing Pitch Representations (x2tint) #################################
###################################################################### ### 


## Parsing (tonalInterval) documentation ----

#' Parsing pitch information
#' 
#' 
#' [humdrumR] includes a easy-to-use but powerful system for *parsing* pitch information:
#' various sorts of basic pitch representations (including numeric and character-string representations) can be "parsed"---read
#' and interpreted by `humdrumR`.
#' For the most part, parsing automatically happens "behind the scenes" whenever you use any humdrumR [pitch function][pitchFunctions], like [kern()].
#' 
#' @details 
#'
#' The underlying parser used by all `humdrumR` [pitch functions][pitchFunctions] can be called explicitly using the function `tonalInterval`.
#' The `tonalInterval` parser will attempt to parse any input information into a [tonalInterval][tonalIntervalS4] object---a back-end pitch representation that you probably don't need to care about!
#' When you use one of the main [pitch functions][pitchFunctions], like [kern()] or [semit()], the input is parsed into a [tonalInterval][tonalIntervalS4] object, then immediately [deparsed][pitchDeparsing]
#' to the representation you asked for (in this case, `kern` or `semit`).
#' 
#' The pitch parser (`tonalInterval`) is a generic function, meaning it can accept `numeric` or `character`-string input.
#' 
#' + [integer][base::integer] values are interpreted as semitones. Watch out! In R, you need to append an `L` to a number to make it an explicit integer:
#'   For example, `tonalInterval(3L)`.
#' + [numeric][base::numeric]/[double()] and [rational()] values are interpreted as frequency ratios, assuming a [Pythagorean tuning](https://en.wikipedia.org/wiki/Pythagorean_tuning).
#'   For example, the value `2.0` will be interpreted as an octave (two to one ratio.)
#' + [character][base::character] values are interpreted using a complex "regex/exclusive interpretation dispatch system"---details below!
#'   
#' (See the "Atonal parsing" section below to learn how atonal data (semitones or frequency/ratios) is interpreted as *tonal* data---i.e., how enharmonic spelling is decided.)
#' 
#' # Tonal Parsing (Character strings)
#' 
#' The most useful tool for humdrum data is parsing tonal pitch representations encoded as `character` strings.
#' (This includes character tokens with pitch information embedded alongside other information; Details below.)
#' The pitch parser (`tonalInterval`) uses either regular-expressions or exclusive interpretations to decide how to parse an input string.
#' There are eight regular-expression patterns for pitch that `tonalInterval` known how to parse automatically:
#' 
#' | Representation                                                                     | Exclusive                 | Example    |
#' | ---------------------------------------------------------------------------------- | ------------------------: | ---------: |
#' | [Kern][https://www.humdrum.org/rep/kern/index.html]                                | **kern                    | `ee-`      |
#' | [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch)                 | **pitch                   | `Eb5`      |
#' | [Helmholtz pitch](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                      | none                      | `eb'`      |
#' | [Lilypond pitch](https://lilypond.org/doc/v2.22/Documentation/notation/pitches)    | none                      | `ees'`     |
#' | [Interval](https://en.wikipedia.org/wiki/Interval_(music))                         | **hint/**mint/**int       | `+m3`      |
#' | [Scale degree](https://en.wikipedia.org/wiki/Degree_(music))                       | **deg                     | `b3`       |
#' | [Solfege]                                                                          | **solfa                   | `me`       |
#' | [Swara](https://en.wikipedia.org/wiki/Svara)                                       | **bhatk                   | `g`        |
#' 
#' If you call `tonalInterval` (or *any* [pitch function][pitchFunctions]) on a `character`-string vector, with a non-`NULL` `Exclusive` argument,
#' that `Exclusive` argument will be used to choose the input interpretation you want, based on the "Exclusive" column in the 
#' table above.
#' For example, `kern(x, Exclusive = 'solfa')` will force the parser to interpret `x` as `**solfa` data.
#' Similarly, `solfa(x, Exclusive = 'kern')` will force the parser to interpret `x` as `**kern` data.
#' If you use any [pitch function][pitchFunctions] within a special call to [withinHumdrum] (or using a [humdrumR pipe][humPipe], like `%hum>%`),
#' `humdrumR` will automatically pass the `Exclusive` field from the humdrum data to the function---this means, that in most cases, 
#' you don't need to explicitely do anything with the `Exclusive` argument!
#' 
#' If you call `tonalInterval` (or *any* [pitch function][pitchFunctions]) on a `character`-string vector, but the `Exclusive` argument is missing
#' or `NULL`, `humdrumR` will instead use regular-expression patterns to select a known interpretation.
#' For example, `pitch('so')` will automatically recognize that `'so'` is solfege, and will interpret the data accordingly (the output should be `r pitch('so')`).
#' If there are more than one matches, `humdrumR` will use the longest match, and if they tie, pick based on the order in the table above (topmost first).
#' 
#' If you call `tonalInterval` (or [any function that uses tonalInterval as a parser][pitch]) on a `character` string,

#' 
#' If there is no match, `tonalInterval` (and all other [pitch function][pitchFunctions]) return `NA` values.
#' Remember, if `Exclusive` is specified, it overrides the regex-based dispatch, which means that `pitch('so', Exclusive = 'kern')` will return `NA`, because
#' `'so'` can't be interpreted as a `**kern` value.
#' 
#' ## "In place" parsing (and deparsing)
#' 
#' In lots of humdrum data, character strings are encoded with multiple pieces of musical information right besides each other:
#' for example, `**kern` data might include tokens like `"4.ee-[`.
#' The `humdrumR` parser (`tonalInterval`) will automatically "pull out" pitch information from within strings, if it can find any 
#' using the appropriate known regular expressions.
#' Various [pitch parsing functions][pitchFunctions] have an option to keep the original "extra" data, using their `inPlace` argument.
#' 
#' # Advanced Parsing Options
#' 
#' The eight tonal representations listed above function through a common parsing interface.
#' By using "advanced" parsing arguments, you can tweak how parsing is done, so as to accommodate even more input representations!
#' Note that these arguments are identical to parallel [deparsing arguments][pitchDeparsing].
#' The following "advanced" parsing arguments are available:
#' 
#' + **Steps**
#'   + `step.labels`: What labels are used to mark the seven diatonic steps? 
#'      Examples include `c('A', 'B', 'C', 'D', 'E', 'F', 'G')` and `c('d', 'r', 'm', 'f', 's', 'l', 't')` (solfege).
#'      Must be a vector with a length that is a multiple of seven.
#' + **Species** (accidentals and/or qualities)
#'   + `qualities`: Should species be indicated using accidentals (flat, sharp, etc.) or qualities (major, minor, etc.)?
#'     `qualities = TRUE` means use qualities; `qualities = FALSE` means use accidentals.
#'   + `specifier.maximum`: For specifiers that can repeat, like `c##`, is there a maximum or minimum allowed?
#'     For example, you could force all triple sharps (`"###"`) or double sharps (`"##"`) to parse as just `"#"`, by specifying `specifier.maximum = 1L`.
#'   + *Accidentals*
#'     + `natural`, `flat`, `sharp`, `doubleflat`, `doublesharp`. These arguments choose how accidentals are represented in the input.
#'        For example, if the input strings look like `c("Eflat", "C")`, you could set the argument `flat = "flat"`.
#'        `doublesharp` and `doubleflat` are `NULL` by default, but can be set if a special symbol is used to represent two sharps or flats.
#'   + *Qualities*
#'     + `perfect`, `major`, `minor`, `augment`, `diminish`. #' These arguments choose how qualites are represented in the input.
#'        For example, if the input strings look like `c("min3", "dim5")`, you could set the arguments `minor = "min"` and `diminish = "dim"`.
#'   + *Implicit vs Explicit Species*
#'     + `cautionary`
#'     + `memory`, `parseWindows`
#'     + `implicitSpecies`, `absoluteSpecies`, `explicitNaturals`
#' + **Octave**
#'   + `octave.integer`
#'   + `up`, `down`, `same`
#'   + `octave.offset`
#'   + `octave.round`
#'   + `octave.relative`, `octave.absolute`
#' + **String parsing**
#'   + `parse.exhaust`
#'   + `sep`. If different pieces of information in the input string are separated by a delimeter, this can indicated here.
#'     For example, if the input string looks like `"E flat"`, you could specify `sep = " "` and `flat = "flat"`.
#'     
#' These "advanced" arguments can be used directly in *any* [pitch function][pitchFunctions], or in `tonalInterval` itself.
#' To use them with `tonalInterval` just specify them directly as arguments;
#' to use them with other [pitch functions][pitchFunctions], you must put them in the `parseArgs` argument, or use the "syntactic sugar" form (`parse(...)`).
#' 
#' Each Exclusive/Regex-dispatch combo (above) is associated with default parsing arguments.
#' For example, if you set `Exclusive = 'kern'` or just use data that *look* like `**kern`, the `flat` argument is set to `"-"`,
#' However, if you had, for example, input data that looked like `**kern` **except** it used a different flat symbol, like `"_"`, you could modify the parser:
#' `kern("EE_", parse(flat = "_"))`
#' This overrides the default value for `**kern`---notice, that it *also* updates the `**kern` regular expression accordingly, so it works exactly the same as the standard [kern()] function.
#' 
#' @return A [tonalInterval][tonalIntervalS4] object.
#' @name tonalInterval
NULL

## Pitch parsers ####

### Octaves ####




octave2tint <- function(str, simpletint, roottint,
                        octave.integer = TRUE,
                        up = '^', down = 'v', same = "",
                        octave.offset = 0L, octave.round = floor,
                        octave.relative = FALSE, ...) {
  
  n <- if (octave.integer) {
    as.integer(str)
  } else { 
    ifelse(str == same, 0L, updownN(str, up = up, down = down) )
  }
  
  #
  steps <- tint2step(simpletint, 0:6) + tint2step(roottint, 0:6)
  n <- if (octave.relative) {
    steps <- delta(steps)
    
    stepOct <- octave.round(steps / 7) 
    sigma(n - stepOct)
  } else {
    n - octave.round(steps / 7)
  }

  
  n <- n - octave.offset
  
  tint(n, 0L)
  
}


### Atonal ####

#### Semitones ####

atonal2tint <- function(tint, accidental.melodic, Key = NULL, ...) {
  if (!is.null(Key)) {
    Key <- diatonicSet(Key)
    tint <- tintPartition_harmonic(tint, 12L, Key)$Enharmonic
    tint <- tint - Key
  }
  LO5ths <- tint@Fifth
  
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

semit2tint <- function(n, accidental.melodic = FALSE, ...) {
          wholen <- as.integer(c(n))
          
          pitchclass <- wholen %% 12L
          
          LO5ths <- .ifelse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- as.integer((wholen - (LO5ths * 19)) %/% 12)
          tints <- tint(octaves, LO5ths)
          
          ##
          tints <- atonal2tint(tints, accidental.melodic, ...)
          
          tints
}



midi2tint <- function(n, accidental.melodic = FALSE, Key = NULL) {
  semit2tint(n - 60L, accidental.melodic, Key)
}


#### Frequency ####

fraction2tint <- function(x, tonalHarmonic = 3) rational2tint(as.rational(x), tonalHarmonic) %<-matchdim% x

rational2tint <- function(x, tonalHarmonic = 3, accidental.melodic = FALSE, ...) {
  if (x@Numerator == 0 || (x@Numerator < 0 & x@Denominator > 0)) .stop('Rational values can only be interpreted as tonalIntervals if they are positive.')
  
  fracs <- cbind(x@Numerator, x@Denominator)
  
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
  tint <- tint(octs, fifs)
  
  tint <- atonal2tint(tint, accidental.melodic, ...)
  
  tint(octs, fifs)
}

double2tint <- function(x, tonalHarmonic = 3, centMargin = 10, accidental.melodic = FALSE, ...) {
  if (x <= 0) .stop('Double (numeric) values can only be interpreted as tonalIntervals if they are positive.')
  
  octrange <- attr(centMargin, 'octrange')
  if (is.null(octrange)) octrange <- 5L
  if (octrange > 150) stop(call. = FALSE,
                           "double2tint can't find a note corresponding exactly to this frequency/ratio. ",
                           "Try raising the centMargin.")
  
  #
  octs <- -octrange:octrange
  
  allocts <- do.call('cbind', lapply(2^octs, '*', x))
  logged <- log(allocts, tonalHarmonic)
  
  whole <- round(logged)
  remain <- logged - whole
  
  whichhit  <- applyrows(remain, \(row) {
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
  
  output <- atonal2tint(output, accidental.melodic, ...)
  
  output 
}



frequency2tint <- function(float, frequency.reference = 440L, 
                           frequencyTint = tint(-4, 3), tonalHarmonic = 3,
                           centMargin = 10) {
  
  ( double2tint(float / frequency.reference, tonalHarmonic, centMargin = centMargin) + frequencyTint) %<-matchdim% float
}

### Tonal ####



step2tint <- function(str, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), steps.sign = FALSE, ...) {
  
  if (length(step.labels) %% 7L > 0) .stop('When parsing tonal pitches, the number of "step.labels" must be a multiple of 7.')
  
  
  
  step <- if (is.null(step.labels)) as.integer(str) else match(toupper(str), toupper(step.labels), nomatch = NA_integer_) 
  
  
  # generic
  genericstep <- genericStep(step)
  tint <- tint( , ifelse(is.na(genericstep), NA_integer_, c(0L, 2L, 4L, -1L, 1L, 3L, 5L)[genericstep]))
  
  # specific
  octave <- (step - 1L) %/% 7L
  
  if (steps.sign) octave[str == toupper(str)] <- (octave[str == toupper(str)] + 1L) * -1
  
  tint <- tint + tint(octave, 0L)
  
    
  tint
}




updownN <- function(str, up = '#', down = 'b')  stringi::stri_count_fixed(str, up) - stringi::stri_count_fixed(str, down)





specifier2tint <- function(str, step = NULL, Key = NULL, 
                           qualities = TRUE,
                           memory = FALSE, parseWindows = NULL,
                           implicitSpecies = FALSE, absoluteSpecies = TRUE, 
                           sharp = '#', flat = '-', natural = 'n', 
                           doublesharp = FALSE, doubleflat = FALSE, 
                           perfect = 'P', major = 'M', minor = 'm', augment = 'A', diminish = 'd',
                           specifier.maximum = Inf,
                           ...) {
  
  # step is lof = -1:5
  step <- if (is.null(step)) 0L else getFifth(step)
   
  if (qualities && !absoluteSpecies) .stop("When parsing tonal information, you can't use the absoluteSpecies == FALSE if the specifier type is 'quality'.")
    
  # use double sharp/flats?
  if (!false(doublesharp)) str <- gsub(doublesharp, strrep(sharp, 2), str)
  if (!false(doubleflat )) str <- gsub(doubleflat,  strrep(flat , 2), str)
  
  
  
  # incorporate memory?
  if (memory) {
    str <- if (truthy(parseWindows) && length(parseWindows) == length(str)) {
      match_size(str,
                 step = step, 
                 memory = memory, 
                 toEnv = TRUE)
      tapply_inplace(str, paste0(step, ':', parseWindows), fillForward, nonnull = \(x) x != '')
    } else {
      fillForward(str, nonnull = \(x) x != '')
    }
  
    
   
  } 
  
  # calculate lof
  
  natural <- stringi::stri_detect_fixed(str, natural)
  lof <- (if (qualities) {
    updownN(str, up = augment, down = diminish) -
      (substr(str, 1L, 1L) == diminish & step >= 3L) - # 3rd, 6th, and 7th diminish differently
      (str == 'm')
  } else {
    updownN(str, up = sharp, down = flat)
  } ) 

  lof <- pmaxmin(lof, -specifier.maximum, specifier.maximum) * 7L
  
  # incorporate key?
  if (!is.null(Key) && implicitSpecies) {
    keyalt <- ifelse(natural, 0L, -(step - (step %% Key)) )
    if (absoluteSpecies) {
      lof[str == ''] <- keyalt[str == '']
    } else {
      lof <- lof + keyalt
    }
    
  }
  # names(n) <- names(accidental.labels)[match(str, accidental.labels)]
  # names(n)[is.na(names(n))] <- ""
  
  tint( , lof)
  
}

CKey <- function(dset) if (!is.null(dset)) dset - getRootTint(dset) 


tonalChroma2tint <- function(str,  
                             parts = c("step", "species", "octave"), 
                             qualities = FALSE, 
                             parse.exhaust = TRUE, 
                             keyed = FALSE, Key = NULL, 
                             sep = NULL,
                             ...) {
 
  
  
 parts <- matched(parts, c("sign", "step", "species", "octave"))
 
 
 ############# parse string
 # regular expressions for each part
 REs <-  makeRE.tonalChroma(parts, collapse = FALSE, qualities = qualities, ...)
 REparse(str, REs, parse.exhaust = parse.exhaust, parse.strict = TRUE, sep = sep, toEnv = TRUE) ## save to environment!
 
 ## simple part
 step    <- if ("step" %in% parts)    step2tint(step, ...) 
 species <- if ("species" %in% parts) specifier2tint(species, qualities = qualities, 
                                                     Key = if (keyed) Key else CKey(Key), step = step, ...) 
 
 simpletint <- (step %||% tint( , 0L)) + (species %||%  tint( , 0L)) 
 
 # complex part
 tint <- if ("octave" %in% parts) octave2tint(octave, simpletint = simpletint, root = getRootTint(Key %||% dset(0L, 0L)), ...) + simpletint else simpletint
 
 if ("sign" %in% parts) tint[sign == '-'] <- tint[sign == '-'] * -1L
 
 
 if (keyed && !is.null(Key)) {
  Key <- rep(Key, length.out = length(tint))
  tint[!is.na(Key)] <- tint[!is.na(Key)] - diatonicSet(Key[!is.na(Key)])
  
 }
 
 tint

 
 
}




pitch2tint <- partialApply(tonalChroma2tint, parts = c("step", "species", "octave"), 
                           octave.offset = 4L, octave.integer = TRUE,
                           step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                           flat = 'b',
                           keyed = TRUE)

lilypond2tint <- partialApply(tonalChroma2tint, 
                              step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                              flat = 'es', sharp = 'is', 
                              octave.integer = FALSE, octave.offset = 1L, up = "'", down = ",")


helmholtz2tint <- partialApply(tonalChroma2tint,
                               step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                               steps.sign = TRUE,
                               up = "'", down = ",", flat = 'b', octave.integer = FALSE, octave.offset = 1L)

kern2tint <- function(str, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),  ...) {
  # letter <- stringr::str_extract(str, '[A-Ga-g]')
  # str_ <- stringr::str_replace(str, '([A-Ga-g])\\1*', toupper(letter)) # simple part
  step.labels <- unlist(lapply(1:50, strrep, x = step.labels))
  
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c("step", "species"), 
                       keyed = TRUE,  
                       qualities = FALSE,
                       steps.sign = TRUE)
  tint <- tC2t(str, step.labels = step.labels, ...)
  
  
}

interval2tint <- function(str, ...) {
  
  tC2t <- partialApply(tonalChroma2tint,
                              parts = c('sign', 'species', "step"), 
                              qualities = TRUE, step.labels = NULL)
  tC2t(str, ...)
  
}



degree2tint <- partialApply(tonalChroma2tint, parts = c("octave", "species", "step"), 
                           qualities = FALSE, 
                           keyed = FALSE, Key = NULL,
                           step.labels = c('1', '2', '3', '4', '5', '6', '7'),
                           octave.integer = FALSE, octave.relative = TRUE, octave.round = round,
                           flat = 'b')
  


solfa2tint <- function(str, ..., flat = '-', sharp = '#') {
  syl <- stringr::str_extract(str, '[fdsrlmt][aeio]')
  
  base <- stringr::str_sub(syl, 1L, 1L)
  alt  <- stringr::str_sub(syl, 2L, 2L)
  
  alt.mat <- rbind(d = c(NA,   flat, sharp, ''),
                   r = c(flat, '',   sharp, NA),
                   m = c(NA,   flat, '',    NA),
                   f = c('',   flat, sharp, NA),
                   s = c(NA,   flat, sharp, ''),
                   l = c('',   flat, sharp, NA),
                   t = c(NA,   flat, '',    NA))
  colnames(alt.mat) <- c('a', 'e', 'i', 'o')
  
  sylalt <- alt.mat[cbind(base, alt)]
  
  str_ <- stringr::str_replace(str, alt, sylalt)
 
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c("octave", "step", "species"),
                       qualities = FALSE,
                       keyed = FALSE,
                       octave.integer = FALSE, octave.relative = FALSE, octave.round = floor)
  
  tC2t(str_, step.labels = rownames(alt.mat), ..., flat = flat)
  
}

bhatk2tint <- function(str, ...) {
  
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c('step', 'octave'),
                       step.labels = c('S', 'R', 'G', 'M', 'P', 'D', 'N'),
                       octave.integer = FALSE,
                       up = "'", down = ',')
  
  tint <- tC2t(toupper(str))
  
  
  perfects <- abs(LO5th(tint)) <= 1L
  altered <- str == tolower(str)
  
  tint[altered] <- tint[altered] + tint( , ifelse(perfects[altered], 7L, -7L))
  
  tint
  
}



## Pitch Parsing Dispatch ######################################


### Parse 2tint generic and methods ####

#' @rdname tonalInterval
#' @export
tonalInterval <- function(...) UseMethod('tonalInterval')

#' @rdname tonalInterval
#' @export
tonalInterval.tonalInterval <- function(x, ...) x

#' @rdname tonalInterval
#' @export
tonalInterval.logical <- function(x, ...) vectorNA(length(x), 'tonalInterval')

#' @rdname tonalInterval
#' @export
tonalInterval.NULL <- function(x, ...) NULL

#### Numbers ####

#' @rdname tonalInterval
#' @export
tonalInterval.numeric  <- double2tint
#' @rdname tonalInterval
#' @export
tonalInterval.rational <- rational2tint
#' @rdname tonalInterval
#' @export
tonalInterval.fraction <- fraction2tint
#' @rdname tonalInterval
#' @export
tonalInterval.integer  <- semit2tint




#### Characters ####

#' @rdname tonalInterval
#' @export
tonalInterval.character <- makeHumdrumDispatcher(list('kern',                   makeRE.kern,        kern2tint),
                                                 list('pitch',                  makeRE.sciPitch,    pitch2tint),
                                                 list('lilypond' ,              makeRE.lilypond,    lilypond2tint),
                                                 list('helmholtz' ,             makeRE.helmholtz,   helmholtz2tint),
                                                 list(c('hint', 'mint', 'int'), makeRE.interval,    interval2tint),
                                                 list('deg',                    makeRE.scaleDegree, degree2tint),
                                                 list('solfa',                  makeRE.solfa,       solfa2tint),
                                                 list('bhatk',                  makeRE.bhatk,       bhatk2tint),
                                                 funcName = 'tonalInterval.character',
                                                 outputClass = 'tonalInterval')




#### setAs tonal interval ####

setAs('integer', 'tonalInterval', function(from) semit2tint(from))
setAs('numeric', 'tonalInterval', function(from) double2tint(from))
setAs('character', 'tonalInterval', function(from) tonalInterval.character(from))
setAs('matrix', 'tonalInterval', function(from) tonalInterval(c(from)) %<-matchdim% from)
setAs('logical', 'tonalInterval', function(from) tint(rep(NA, length(from))) %<-matchdim% from)

setMethod('as.rational', 'tonalInterval', tint2rational)
setMethod('as.double', 'tonalInterval', tint2double)
setMethod('as.integer', 'tonalInterval', tint2semit)




###################################################################### ### 
# Translating Pitch Representations (x2y) ################################
###################################################################### ### 

## Pitch function documentation ####



#' Translate between pitch representations.
#' 
#' These functions can be used to extract and "translate," or otherwise modify, data representing pitch information.
#' 
#' 
#' These pitch functions all work in similar ways, with similar arguments and functionality;
#' To read more details about each function, click on the links in the list below, 
#' or type `?func` in the R command line: for example, `?kern`.
#' The functions are:
#' 
#' + **Tonal pitch representations**
#'   + *Absolute pitch representations*
#'     + [kern]
#'     + [pitch]
#'     + [lilypond]
#'     + [helmholtz]
#'   + *Relative pitch representations*
#'     + [interval]
#'     + [solfa] (solfege)
#'     + [degree] (scale degrees)
#'     + [bhatk] (hindustani swara)
#' + **Atonal pitch representations**
#'   + *Musical (non-tonal) pitch representations*
#'     + [semit]
#'     + [midi]
#'   + *Physical pitch/interval representations*
#'     + [frequency]
#'     + [ratio]
#'     
#' @param x (atomic vector) The `x` argument can be any ([atomic][base::vector]) vector, or a [tonalInterval][tonalIntervalS4], or `NULL`.
#' @param ... These arguments are passed to the [pitch deparser][pitchDeparsing]. 
#'        There are also two hidden (advanced) argumens you can specify: `memoize` and `deparse` (see the details below).
#' @param generic (logical, length 1) If `generic = TRUE` the "specific" pitch information is discarded.
#'        For tonal representations, this means no accidentals/qualities or equivalent information is printed.
#'        For atonal representations, the generic pitch is returned.
#'        As an alternative, you can specify `specific` as an alternative (opposite) to the `generic` argument: `generic == !specific`.
#' @param simple (logical, length 1) If `simple = TRUE` the "complex" pitch information is discarded.
#'        This means that *octave* information is discarded, and the resulting output is confined within the default octave.
#'        For absolute pitch representations, this is the octave above middle C.
#'        For relative pitch representations, this is the octave above a unison.
#'        (The special `octave.round` argument can be used to modify this behavior).

#' @param parseArgs (list) `parseArgs` can be a list of arguments that are passed to the [pitch parser][tonalInterval].
#'        As a convenient syntactic sugar, instead of writing `parseArgs = list(a = x, b = y, etc.)`, you can write
#'        `parse(a = x, b = y, etc.)`.
#' @param transposeArgs (list) `transposeArgs` can be a list of arguments that are passed to a special call to [transpose].
#'        As a convenient syntactic sugar, instead of writing `transposeArgs = list(a = x, b = y, etc.)`, you can write
#'        `transpose(a = x, b = y, etc.)`.
#' @param inPlace (logical, length 1) This argument only has an effect if the input (the `x` argument) is `character` strings,
#'        *and* there is extral, non-pitch information in the strings "beside" the pitch information.
#'        If so, and `inPlace = TRUE`, the output will be placed into an output string beside the original non-pitch information.
#'        If `inPlace = FALSE`, only the pitch output information will be returned (details below).
#'  
#' 
#' 
#'
#' @section In-place parsing:
#' 
#' In humdrum data, character strings are often encoded with multiple pieces of musical information right besides each other:
#' for example, `**kern` data might include tokens like `"4.ee-[`.
#' The `humdrumR` parser (`tonalInterval`) will automatically "pull out" pitch information from within strings, if it can find any 
#' using the appropriate known regular expressions.
#' For example, `pitch('4.ee-[')` returns `r pitch('4.ee-[')`.
#' However, all the pitch functions (like [pitch()] and [kern()]) have an option to keep the "extra" information
#' and return the result "in place"---i.e., embedded right where it was found in the input string.
#' This is controlled with the `inPlace` argument, which is `FALSE` by default.
#' So, `pitch('4.ee-[', inPlace = TRUE)` will return `r pitch('4.ee-[', inPlace = TRUE)`---keeping the `"4."` and the `"["`.
#' (This obviously only works if the input is a string, not a numeric!)
#' Note that `inPlace = TRUE` will force functions like `semit`, which normally return numeric values, to return character strings
#' *if* their input is a character string.
#' 
#' @returns 
#' 
#' `NULL` inputs (`x` argument) return a `NULL` output.
#' Otherwise, returns a vector of the same length as `x`.
#' `NA` values in the input `x` are propagated to the output.
#'
#' @name pitchFunctions
#' @seealso tonalInterval
NULL

## Pitch transform maker ####

pitchArgCheck <- function(args,  callname) {
  argnames <- .names(args)
  
  if ('generic' %in% argnames) {
    if ('specific' %in% argnames && !xor(args$generic, args$specific)) .stop("In your call to {callname}, you've specified contradictory 'generic' and 'specific' arguments...it has to be one or the other!")
    args$specific <- !args$generic
  }
  
  if ('simple' %in% argnames) {
    if ('complex' %in% argnames && !xor(args$simple, args$complex)) .stop("In your call to {callname}, you've specified contradictory 'simple' and 'complex' arguments...it has to be one or the other!")
    args$complex <- !args$simple
  }
  
  if ('octave.absolute' %in% argnames) {
    if ('octave.relative' %in% argnames && !xor(args$octave.relative, args$octave.absolute)) .stop("In your call to {callname}, you've specified contradictory 'octave.relative' and 'octave.absolute' arguments...it has to be one or the other!")
    args$octave.relative <- !args$octave.absolute
  }
  
  checkTFs( args[intersect(argnames, c('generic', 'specific', 'complex', 'simple', 'octave.absolute', 'octave.relative'))], callname = callname)
    
  
  args 
  
}


makePitchTransformer <- function(deparser, callname, outputClass = 'character') {
  # this function will create various pitch transform functions
  exclusiveFunctions <<- c(exclusiveFunctions, callname)
  keyedFunctions <<- c(keyedFunctions, callname)
  
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- alist(x = , 
                ... = , # don't move this! Needs to come before other arguments, otherwise unnamed parse() argument won't work!
                generic = FALSE, simple = FALSE, octave.contour = FALSE, 
                parseArgs = list(), transposeArgs = list(), 
                inPlace = FALSE)
  
  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr( {
    # parse out args in ... and specified using the syntactic sugar parse() or transpose()
    
    checkVector(x, structs = 'tonalInterval', argname = 'x', callname = !!callname)
    
    c('args...', 'parseArgs', 'transposeArgs') %<-% specialArgs(rlang::enquos(...), 
                                                                parse = parseArgs, transpose = transposeArgs)
    formalArgs <- list(!!!fargcall)
    namedArgs <- formalArgs[.names(formalArgs) %in% .names(as.list(match.call())[-1])]
    
    # There are four kinds of arguments: 
    # ... arguments (now in args...), 
    # FORMAL arguments, if specified (now in namedArgs)
    # parseArgs
    # transposeArgs
    
    # Exclusive
    parseArgs$Exclusive <- parseArgs$Exclusive %||% args...$Exclusive 
    
    parseArgs   <- pitchArgCheck(parseArgs, !!callname)
    deparseArgs <- pitchArgCheck(c(args..., namedArgs), !!callname)
    
    # Key
    Key     <- diatonicSet(args...$Key %||% dset(0L, 0L))
    fromKey <- diatonicSet(transposeArgs$from %||% Key)
    toKey   <- diatonicSet(transposeArgs$to   %||% Key)
    
    parseArgs$Key   <- fromKey
    deparseArgs$Key <- toKey 
    
    transposeArgs$from <- CKey(fromKey)
    transposeArgs$to   <- CKey(toKey)
    
    # memoize % deparse
    memoize <- args...$memoize %||% TRUE
    deparse <- args...$deparse %||% TRUE
    
    
    # Parse
    parsedTint <- do(tonalInterval, c(list(x, memoize = memoize), parseArgs), memoize = memoize, outputClass = 'tonalInterval')
    if (length(transposeArgs) > 0L && is.tonalInterval(parsedTint)) {
      parsedTint <- do(transpose.tonalInterval, c(list(parsedTint), transposeArgs))
    }
    
    deparseArgs <- c(list(parsedTint), deparseArgs)
    output <- if (deparse && is.tonalInterval(parsedTint))  do(!!deparser, deparseArgs, 
                                                               memoize = memoize, 
                                                               outputClass = !!outputClass) else parsedTint
    if (deparse && inPlace) output <- rePlace(output, attr(parsedTint, 'dispatch'))
    
    output
    
  }))
}



### Pitch Transformers ####


#' Semitone pitch representation
#' 
#' This function translates pitch information into [semitones](https://en.wikipedia.org/wiki/Semitone) (integers).
#' Middle-C or unison values are `0`.
#' 
#' 
#' @family {atonal pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
semit <- makePitchTransformer(tint2semit, 'semit', 'integer')

#' MIDI pitch representation
#' 
#' This function translates pitch information into [MIDI pitch values](https://en.wikipedia.org/wiki/MIDI).
#' The `midi` function is identical to the [semit()] function, except offset by 60: `midi('c') = 60` while 
#' `semit('c') = 0`.
#' 
#' 
#' @family {atonal pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
midi  <- makePitchTransformer(tint2midi, 'midi', 'integer')

#' Scientific pitch representation
#' 
#' [Scientific pitch](https://en.wikipedia.org/wiki/Scientific_pitch) is the most standard
#' approach to representing pitch in traditional Western music. 
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
pitch <- makePitchTransformer(tint2pitch, 'pitch')

#' Kern pitch representation
#' 
#' Kern (`**kern`) is the most common humdrum interpretation for representing "notes" in the style of
#' traditional Western scores.
#' In [humdrumR], the `kern` function only relates to the *pitch* part of the `**kern` interpretation:
#' `**kern` *rhythms* are created using the [recip] function.
#' 
#' @details 
#' 
#' The pitch part of `**kern` tokens breakdown tonal pitch information as so:
#' 
#' + **Steps**
#'   + 1: `"C"` or `"c"`
#'   + 2: `"D"` or `"d"`
#'   + 3: `"E"` or `"e"`
#'   + 4: `"F"` or `"f"`
#'   + 5: `"G"` or `"g"`
#'   + 6: `"A"` or `"a"`
#'   + 7: `"B"` or `"b"`
#' + **Accidentals**
#'   + Flat: `"-"`
#'   + Sharp: `"#"`
#' + **Octave**
#'   + Octave is indicated through the case of the step characters, as well as *repetition* of the step character.
#'     Uppercase letters are used for octaves below middle-C; lowercase letters for the middle-C octave and higher.
#'     The middle-C octave, and the octave below it get one character each, with higher and lower octaves repeating that character.
#'     For example, using `C#` as the step value, and relative to the middle-C octave:
#'     + -3: `"CCC#"`
#'     + -2: `"CC#"`
#'     + -1: `"C#"`
#'     +  0: `"c#"`
#'     + +1: `"cc#"`
#'     + +2: `"ccc#"`
#'     + +3: `"cccc#"`
#'
#' Tokens are ordered `Step/Octave + Accidentals`, with no separator.
#' 
#' Like all `humdrumR` pitch functions, the ways that `kern` [parses][tonalInterval] and [deparses][pitchDeparsing] tokens
#' can be modified to accomodate variations of the standard `**kern` pitch representation.
#' 
#' ---
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
kern <- makePitchTransformer(tint2kern, 'kern') 

#' Lilypond pitch representation
#' 
#' This is the representation used to represent (Western tonal) pitches in the [Lilypond](https://lilypond.org/doc/v2.22/Documentation/notation/pitches) 
#' notation format.
#' In [humdrumR], the `lilypond` function only relates to the *pitch* part of Lilypond notation:
#' Lilypond-like *rhythms* can be creating using the [recip] function.
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
lilypond <- makePitchTransformer(tint2lilypond, 'lilypond')


#' Helmholtz pitch representation
#' 
#' [Helmholtz notation](https://en.wikipedia.org/wiki/Helmholtz_pitch_notation)
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
helmholtz <- makePitchTransformer(tint2helmholtz, 'helmholtz')

#' Tonal (pitch) interval representation
#' 
#' This returns the standard representations of [intervals](https://en.wikipedia.org/wiki/Interval_(music))
#' in Western music.
#' 
#' 
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions],
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
interval <- makePitchTransformer(tint2interval, 'interval')

#' Tonal [scale degree](https://en.wikipedia.org/wiki/Degree_(music)) representation
#' 
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
degree <- makePitchTransformer(tint2degree, 'degree')

#' [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge) representation
#' 
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
solfa <- makePitchTransformer(tint2solfa, 'solfa')

#' [Swara](https://en.wikipedia.org/wiki/Svara) representation
#' 
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][tonalInterval] and [deparsed][pitchDeparsing].
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchFunctions In-place parsing
#' @export 
bhatk <- makePitchTransformer(tint2bhatk, 'bhatk')














###################################################################### ###
# Manipulating tonal intervals ###########################################
###################################################################### ###

## Partitioning tonalIntervals ####


tintPartition <- function(tint, partitions = c('complex', 'harmonic', 'specific'),
                          roundContour = floor, Key = NULL, enharmonicWrap = 12L, ...) {
  
  partitions <- matched(partitions, c('complex', 'harmonic', 'specific'))
  
  Key <- diatonicSet(Key %||% dset(0, 0))
  match_size(tint = tint, Key = Key, toEnv = TRUE)
  
  octave <- if ('complex' %in% partitions) {
    complex <- tintPartition_complex(tint, roundContour)
    tint <- complex$Simple
    complex['Octave']
  }
  
  comma <- if ('harmonic' %in% partitions) {
    harmonic <- tintPartition_harmonic(tint, enharmonicWrap = enharmonicWrap, Key = Key)
    tint <- harmonic$Enharmonic
    harmonic['Comma']
  }
  
  specific <- if ('specific' %in% partitions) {
    specific <-  tintPartition_specific(tint, Key = Key)
    tint <- NULL
    specific
  }
  
  if (!is.null(tint) && !is.data.frame(tint)) tint <- as.data.frame(tint)
  
  .cbind(.data.frame(Key = getRootTint(Key)), octave, specific, tint, comma) %class% 'partition'
  
}

#### simple + octave = complex ####


tintPartition_complex <- function(tint, octave.round = floor, ...) {
  octshift <- as.integer(octave.round(tint2semit(tint %% dset(0, 0)) / 12))
  
  octavepart <- tint(octshift, 0L)
  simplepart <- tint - octavepart
  
  .data.frame(Octave = octavepart, Simple = simplepart)
  
}


#### enharmonic + comma = harmonic ####

tintPartition_harmonic <- function(tint, enharmonicWrap = 12L, Key = dset(0L, 0L), ...) {
  
  modeoffset <- tint( , getSignature(Key)) + tint(, 2) # because 2 fifths is the "center" of the diatonic set
  entint <- (tint - modeoffset) %<-matchdim% NULL
  
  enharmonicbound <- enharmonicWrap %/% 2
  sharpshift <-  tint( , enharmonicbound) # this makes it so an odd number (like 13) is biased towards sharp side
  flatshift  <- -tint( , enharmonicWrap - enharmonicbound)
  
  fs <- entint@Fifth
  sharp <-  fs > enharmonicbound
  flat  <- fs <= -(enharmonicWrap - enharmonicbound)
  
  entint[sharp] <- ((entint[sharp] + sharpshift) %%  pythagorean.comma) - sharpshift 
  entint[ flat] <- ((entint[ flat] + flatshift)  %% -pythagorean.comma) - flatshift
  
  enharmonicpart <- (entint + modeoffset) %<-matchdim% tint
  commapart <- tint - enharmonicpart
  
  .data.frame(Enharmonic = enharmonicpart,  Comma = commapart)
}


#### generic + alteration = specific ####

tintPartition_specific <- function(tint, Key = dset(0L, 0L), ...) {
  
  genericpart    <-  tint %% (Key %||% dset(0L, 0L)) 
  alterationpart <- tint - genericpart
  
  .data.frame(Generic = genericpart,  Alteration = alterationpart)
  
}





## Transposing tonal intervals ####




#' Transpose pitches and keys
#' 
#' This function [transposes][https://en.wikipedia.org/wiki/Transposition_(music)] pitches or keys 
#' by various intervals or to target keys.
#' Inside the box, inputs and transpositions take place as `tonalInterval`s or `diatonicSet`s,
#' but any numeric or character string representation of pitches can be transposed as well.
#' This function is incorporated directly into [tonalTransform], and thence, all [pitch translation][pitchRepresentations]
#' functions, so you probably won't call it directly very often.
#' 
#' There are two distinct types of transposition (real and tonal).
#' There are also two different approaches to *specifying* transpositions: "to" and "by".
#' "To" transpositions can also be either *parallel* or *relative*.
#' 
#' # Types of Transposition
#' 
#' There are two different types of transposition: **real** transposition and **tonal** transposition.
#' In *real* transposition, all inputs are transposed by the same *specific* interval.
#' For example, the pitches `{C D E F G}` could be transposed up a major second to `{C D E F# G}`.
#' In *tonal* transposition, inputs are transposed by *generic* intervals, within a key.
#' For example, the sequence `{C D E F G}`, in the key of C major, could be translated up a generic second
#' to `{D E F G A}`.
#' 
#' To choose between real and tonal transposition, use the `real` argument:
#' `real = TRUE` for real transposition, `real = FALSE` for tonal transposition.
#' 
#' ### Alterations
#' 
#' Tonal transposition is complicated by the presence of any alterations in the input pitches.
#' For instance, if we are given the pitches `{C F# G D# E}`` in the key of C major, how should they by tonally
#' transposed up a second, within C major?
#' There is not one obvious, correct answer answer, which can be easily identified.
#' The algorithm implemented by `humdrumR` is as follows:
#' 
#' 1. Alterations/accidentals in the input are identified. (In this case, F# and D#).
#' 2. The generic pitches are transposed within the key, resulting in `{D G A E F}`.
#' 3. Alterations in the input are added to the output *unless* the resulting pitches are interpreted as a comma
#'    by a call to [tintPartion], with a given enharmonic wrap value (the default is `12`).
#'    In this example, adding the first accidental results in `{G#}` which is not a comma.
#'    However, the second accidental results in `{E#}` which *is* a comma away from the natural `{F}`. 
#'    Thus, this accidental is not added to the output, resulting in `{E}`, not `{E#}`.
#'    The resulting output is `{D G# A E F}`.
#' 
#' The size of `enharmonicWrap` effectively determines how extreme accidentals are allowed.
#' The default value, `12`, assures that no output notes are enharmonically equivalent to notes in the key. 
#' To further illustrate, here is the sequence `{C F# G D# E, B- A A- G C# D, B D- C}` transposed
#' tonally within C major by all seven possible generic intervals, with `enharmonicWrap = 12`:
#' 
#' 
# #' | Interval  | Output                                                                                                                                                              |
# #' | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
# #' | Unison    | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = P1, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 2nd       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = M2, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 3rd       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = M3, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 4th       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = P4, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 5th       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = P5, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 6th       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = M6, real = FALSE), width=3), collapse = ''), '}')`  |
# #' | 7th       | `r paste0('{', paste(format(transpose(c('C', 'F#', 'G', 'D#','E','B-','A','A-','G','C#','D','B','D-','C'), by = M7, real = FALSE), width=3), collapse = ''), '}')`  |
#
#' 
#' # Specifying Transpositions
#' 
#' There are two approaches to specifying transpositions, the `by` and `to` arguments.
#' The `by` argument must be an interval, and the input is translated by that interval.
#' If the `by` interval is specific but `real = FALSE`, the input is treated as a generic interval,
#' and tranposition takes place within the key indicated by the `Key` argument.
#' 
#' 
#' The `to` argument translates an input *to* a desired key.
#' For example, if the input is in the key of E major but we want it transposed to G major, we could say `to = '*E:'`.
#' If `real = TRUE`, input is simply translated to the root of the `to` key, with all the exact same intervals.
#' If `real = FALSE`, the input is translated to the root of the new key, with its intervals changed to match the new key as well.
#' In either case, the result depends on what the input's key is, which is indicated by the [standard][tonalTransform] `Key` argument.
#' The `Key` arguments is like the "from" key.
#' If `Key = NULL`, the input key is interpreted as C major.
#' 
#' Consider the input notes `{D B C A# B, D C# D E D}` in the key of the G major.
#' If we specify `to = e:, real = TRUE`, the output will be `{B G# A F## G#, B A# B C# B}`.
#' (Notice that even though the `to` key is minor, the output is still clearly in E major).
#' If we specify `to = e:, real = FALSE`, the output will instead be `{B G A F# G, B A# B C B}`.
#' 
#' Building off the previous example, consider how the input *key* matters as well.
#' If we use the same input notes (`{D B C A# B, D C# D E D}`) but the input `Key` is C major, then:
#' If we specify `to = e:, real = TRUE`, the output will be `{F# D# E C## D#, F# E# F# G# F#}`.
#' If we specify `to = e:, real = FALSE`, the output will instead be `{F# D E C# D, F# E F# G F#}`.
#' 
#' If *both* `by` and `to` are specified, the `to` transposition is applied first, followed by the `by` transposition.
#' If `real = FALSE`, the `by` transposition happens within the `to` key, not the `Key` key.
#' 
#' ## Relative vs Parallel
#' 
#' When transposing to, we have diferent approaches about to determining the relationship between the
#' "from" key (`Key` argument) and the "to" key (`to` argument).
#' If we think of "parallel" relationships between keys, we match the roots of the keys regardless of modes.
#' For instance, C major and C minor are parallel keys.
#' If we instead think of "relative" relationships between keys, we match the modes of the keys, not the roots.
#' For instance, C major and A minor are relative keys.
#' This is similar to the distinction between "la-based minor" solfege (relative) vs "fixed-do" solfege (parallel).
#' 
#' When transposing using a `to` argument, if `relative = FALSE` the input key (`Key` argument) is transposed to match the *root*
#' of the `to` argument.
#' For example, if the input key is G minor and the `to`` key is C major, the output is transposed to G minor.
#' However, if `relative = TRUE` the input key is transposed to match the mode of the `to` key:
#' A G minor input with a C major `to` would be translated to A minor, the parallel minor of the `to` key.
#' If the `Key` (from key) and `to` (to key) arguments have the same mode, the parallel and relative transpositions
#' are the same.
#' 
#' 
#' # Special Operators +-
#' 
#' As a note, real transposition `by` and interval can be achieved more concisely using the `+` and `-` operators,
#' as long as at least one side of the operators is an actual `tonalInterval` object.
#' `humdrumR` preassigns all common tonalIntervals to objects in your global environment.
#' Thus, you can type commands like `"c#" + M2` to get `d#`, or `c("C4", "E4", "C5") - m6` to get `"E3" "G#3" "E4"`.
#' 
#' @param x The input pitch(es) to transpose. A `tonalInterval` or something intepretable as a `tonalInterval`. 
#' @param by A `tonalInterval` or something intepretable as a `tonalInterval`. 
#'        The input `x` is transposed by this interval.
#' @param Key A `diatonicSet` or something intepretable as a `diatonicSet`. For tonal and/or to transpositions,
#'        this is the "from" key. If this value is `NULL`, it defaults to C major.
#' @param to A `diatonicSet` or something intepretable as a `diatonicSet`. The input `x` is transposed
#'        to this key.
#' @param real A logical. If `TRUE` (the default), transposition is real. If `FALSE`, transposition is tonal.
#' @param relative A logical. If `TRUE` transposition is relative. If `FALSE` (the default), transposition is parallel.
#' 
#' @seealso tonalTransformations
#' @export
transpose <- function(x, by, Key, to, real, relative, ...) UseMethod('transpose')
#' @export
transpose.tonalInterval <- function(x, by = NULL, from = NULL, to = NULL, ...) {
  if (is.null(by) && is.null(to)) return(x)
  
  ## Prepare arguments
  args <- transposeArgCheck(list(...))
  real <- args$real
  relative <- args$relative
  
  ## Deal with keys
  from <- diatonicSet((from %||% dset(0, 0)))
  from[is.na(from)] <- dset(0, 0)
  
  
  if (!is.null(to)) {
    to <- diatonicSet(to)
    to[is.na(to)] <- dset(0, 0)
    
    if (relative) {
      sigdiff <- getSignature(to) - getSignature(from)
      to <- from + dset(sigdiff, sigdiff)
    }
    
    by <- (getRootTint(to) - getRootTint(from)) + (by %||% tint(0, 0))
    
  } else {
    to <- from
  }
  
  ## Do transposition!
  x <- if (real) {
    x + by
  } else {
    
    x <- tintPartition(x, Key = from, 'specific')
    x$Generic <- x$Generic + by
    x$Generic <- x$Generic %% to
    
    altered <- x$Alteration != tint(0, 0)
    if (any(altered)) {
      comma <- tintPartition(x$Generic[altered] + x$Alteration[altered], 'harmonic', Key = to, ...)$Comma
      x$Alteration[which(altered)[comma != tint(0,0)]] <- tint(0, 0)
    }
    
    
    x$Generic + x$Alteration
    
  }
  
  x
  
}



transposeArgCheck <- function(args) {
  argnames <- .names(args)
  
  
  doubleswitch('tonal' %in% argnames, 'real' %in% argnames,
               'neither' = {args$real <- TRUE},
               'notxor' =  {.stop("In your call to transpose, you've specified contradictory 'real' and 'tonal' arguments...it has to be one or the other!")},
               'first' = {args$real <- !args$tonal})
  
  doubleswitch('parallel' %in% argnames, 'relative' %in% argnames,
               'neither' = {args$relative <- FALSE},
               'notxor' =  {.stop("In your call to transpose, you've specified contradictory 'relative' and 'parallel' arguments...it has to be one or the other!")},
               'first' = {args$relative <- !args$parallel})
  
  
  args
}

### Transposition methods ####



## Inverting tonal intervals ####



#' Invert or transpose tonal intervals.
#'
#' @name tonalTransformations
#' @export 
invert <- function(tint, around, Key, ...) UseMethod('invert')
#' @export 
invert.tonalInterval <- function(tint, around = tint(0L, 0L), Key = NULL) {
  if (!is.tonalInterval(around)) around <- tonalInterval(around)
  
  output <- (around + around - tint) 
  if (!is.null(Key)) output <- output %% Key
  
  output
}

### Inversion methods ####


## Melodic Intervals ####

#' @export
mint <- function(x, ..., lag = 1, deparser = interval, initial = kern, bracket = TRUE, 
                 classify = FALSE,
                 parseArgs = list(), Exclusive = NULL, Key = NULL, File = NULL, Spine = NULL, Path = NULL) {
  
  lagged <- lag(x, lag, windows = list(File, Spine, Path))
  
  c('args', 'parseArgs') %<-% specialArgs(rlang::enquos(...), parse = parseArgs)
  
  minterval <- do(
    \(X, L, exclusive, key) {
      Xtint <- do.call('tonalInterval', c(list(X, Exclusive = exclusive, Key = key), parseArgs))
      Ltint <- do.call('tonalInterval', c(list(L, Exclusive = exclusive, Key = key), parseArgs))
      tint <- Xtint - Ltint
      
      output <- do(deparser, c(list(tint), args))
      
      singletons <- !is.na(Xtint) & is.na(Ltint)
      
      if (!is.null(initial) && any(singletons)) {
        
        if (is.function(initial)) output[singletons] <- paste0(if (bracket) '[', do(initial, c(list(Xtint[singletons]), args)), if (bracket) ']')
        if (is.atomic(initial)) output[singletons] <- initial
      }
      output
    }, 
    args = list(x, lagged, Exclusive, Key)) # Use do so memoize is invoked
  
  if (classify) minterval[!is.na(minterval)] <- mintClass(minterval[!is.na(minterval)], ...)
  minterval
  
}

mintClass <- function(x, directed = TRUE, skips = TRUE) {
  int <- interval(x, generic = TRUE)
  int <- as.integer(int)
  
  sign <- if (directed)  c('-', '', '+')[sign(int - 1L) + 2L] else ""
  int <- abs(int)
   
 
  
  intClass <- cut(int, breaks = c(0, 1, 2, 3, Inf), labels = c('Unison', 'Step', 'Skip', 'Leap'))
  
  
  paste0(sign, intClass)
}


###################################################################### ### 
# Predefined tonalIntervals ##############################################
###################################################################### ### 
#' @name tonalInterval
#' @export dd1 dd2 A2 P3 d4 d5 d6 AA6 M7 dd9 A9 P10 d11 d12 d13 AA13 M14 P15
#' @export d1 d2 AA2 M3 P4 P5 m6 dd7 A7 P8 d9 AA9 M10 P11 P12 m13 dd14 A14 A15
#' @export P1 m2 dd3 A3 A4 A5 P6 d7 AA7 m9 dd10 A10 A11 A12 P13 d14 AA14 AA15
#' @export A1 P2 d3 AA3 AA4 AA5 M6 m7 dd8 A8 P9 d10 AA10 AA11 AA12 M13 m14 dd15
#' @export AA1 M2 m3 dd4 dd5 dd6 A6 P7 d8 AA8 M9 m10 dd11 dd12 dd13 A13 P14 d15
#' @export unison pythagorean.comma octave
NULL
# 
allints <- outer(c('dd', 'd', 'm', 'P', 'M', 'A', 'AA'), 1:15, paste0)
allints[as.matrix(expand.grid(c(3,5), c(1,4,5,8, 11,12,15)))] <- NA
allints <- c(allints)
allints <- allints[!is.na(allints)]
cat(paste0("#' @export ", unlist(tapply(allints, rep(1:5, length.out = length(allints)), paste, collapse = ' '))), sep = '\n')
for (int in allints) {
  assign(int, interval2tint(int))
}
rm(allints)
unison <- P1
pythagorean.comma <- (-dd2)
octave <- P8
