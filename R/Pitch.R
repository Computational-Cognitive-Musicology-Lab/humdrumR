################################## ###
# tonalInterval S4 class #############
################################## ###



## tonalIntervalS4 documentation ----


#' Representation of tonal pitch information
#' 
#' The `tonalInterval` is the core tonal pitch representation in [humdrumR][humdrumR::humdrumR].
#' A `tonalInterval` is an abstract representation of tonal pitch, which can be translated to/from all standard 
#' "concrete" pitch representations:
#' solfege, scientific pitch, semitones, frequencies, scale degrees, intervals, etc.
#' For the most part, users should not need to interact with `tonalInterval`s directly---rather, `tonalInterval`s work 
#' behind the scene in numerous `humdrumR` pitch functions.
#' See the [pitch functions][pitchFunctions] and [pitch parsing][pitchParsing] documentation for 
#' details on how `tonalIntervals` are used by `humdrumR`.
#'
#' @slot Octave integers representing the octave offset.
#' @slot Fifth integers representing the "line-of-fifths" value.
#' @slot Cent numeric values representing cents (1200th of an octave).
#' 
#' The `tonalInterval` is a [S4](http://adv-r.had.co.nz/S4.html) subclass of `humdrumR`'s virtual class [struct], from which it inherits a lot of useful "vector-like" behaviors/functionality.
#' 
#' 
#' 
#' @section Creating tonal intervals:
#' 
#' Generally, `tonalIntervals` are created using the [tonalInterval()] function, and its
#' various methods.
#' The `tonalInterval` *function* is primarily a parser, [documented elsewhere][pitchParsing],
#'  which interprets various input representations
#' and generates `tonalInterval` S4 *objects* (documented here).
#' 
#' Alternatively, the constructor function `tint` can be used to directly create `tonalInterval` objects.
#' The three arguments to `tint` correspond to the three slots: `octave`, `LO5th` (Fifth), and `cent`.
#' All inputs will be coerced to match in length.
#' The `octave` argument can be left blank, in which case the appropriate octave will automatically be computed
#' to place the interval in the octave above .
#' 
#' By default, the [as.character][base::character] method, 
#' and thus (via [struct]) the [show][methods::show] method, for `tonalInterval`s call [interval()].
#' Thus, if you return a `tonalInterval` on the command line
#' you'll see the `**interval` representation printed.
#' 
#' ## Predefined Intervals:
#' 
#' `humdrumR` automatically exports a bunch of `tonalInterval`s, named by their musical interval representation.
#' Every generic interval from 1 to 15 is combined with every interval quality `dd` (doubly diminished), `d` (diminished), `m` (minor), `M` (major), `A` (augumented)
#' `AA` (doubly augmented).
#' Thus, after loading `humdrumR`, you can type things like `M3 + M3` and get `A5`.
#' In addition, the variables `unison` (`= P1 = tint(0, 0)`) and `pythagorean.comma` (`= d2 = tint(-19,12)`) are exported as well.
#' 
#' 
#' ## Arithmetic: 
#' 
#' Technically, `tonalInterval`s are examples of algebraic [modules over integers](https://en.wikipedia.org/wiki/Module_(mathematics)).
#' This means that certain arithmetic operations are defined for `tonalIntervals` and can be called 
#' using standard arithmetic operators (`+`, `-`, etc.):
#' 
#' + Addition: `tonalIntervals` can be added together, acting exactly as you'd expect (i.e., \eqn{M3 + m3 = P5}).
#' + Subtraction: `tonalIntervals` can be subtracted just as they are added. Also, they can be negated with a single `-`
#'   operator (like `-M3`).
#' + Multiplication: `tonalInterval`s can *not* be multiplied together.
#'   However, [scalar (integer) multiplication](https://en.wikipedia.org/wiki/Scalar_multiplication) is defined:
#'   thus, `tonalIntervals` can be multiplied by integers to create new `tonalInterval`s: e.g., \eqn{M2 * 3 = A4}.
#' + Division: as the natural inverse of scale multiplication, [Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division)
#'   is defined for `tonalIntervals`---i.e., division by/into whole (integer) pieces, often with leftover "remainders" (modulo).
#'   In R, Euclidean division is achieved with the [%/%][base::Arithmetic] operator---*not* `/`---, with the associated [%%][base::Arithmetic] used for the remainder/modulo.
#'   Two `tonalInterval`s can be divided to produced an integer; Conversely, a `tonalInterval` can be divided by an integer to produce a `tonalInterval`.
#'   
#' Take note that the way `humdrumR` defines Euclidean division is based in *tonal space*---i.e., the line-of-fifths---not 
#' frequency or atonal-semitone space.
#' For example, an augmented-fourth divided by a major-second *is* `3L`, but a diminished-fifth divided by 
#' a major-second is *not* 3L---`d5 %/% M2` equals `-3L` with a remainder of `P8` (plus an octave)!
#' The division algorithm works by applying standard Euclidean division to the `@Fifth` slot (line-of-fifths tonal space), and shifting the `@Octave` value in
#' the remainder to the match the appropriate octave.
#' 
#' If you attempt to do addition between a `tonalInterval` and non-`tonalInterval` atomic vector (e.g., `integer`, or `character`),
#' `humdrumR` will attempt to [coerce](https://en.wikipedia.org/wiki/Type_conversion)
#' the other input to a `tonalInterval`, using the [tonalInterval()] parser, do the math and then output the answer in
#' the original format (non-`tonalInterval`) format.
#' For instance, `M3 + 2` will interpret `2` as two semitones and add a major-second to the major-third, resulting in `6` semitones.
#' ["In-place"][pitchDeparsing] parsing/deparsing will be used, so "extra" characters in the input will be passed through.
#' For example, `M3 + 4.ee-` will return `4.gg`.
#' 
#' ## Relational Operators
#' 
#' `tonalInterval`s can be compared using the standard [relational operations][base::Comparison], like `==`, `!=`, `>`, and `>=`.
#' Two `tonalInterval`s are equal (according to `==`) only if all their slots (`Octave`, `Fifth`, and `Cent`)
#' are exactly identical. 
#' Thus, enharmonic notes (like C and Db) are *not* equal.
#' In contrast, ordinal comparisons (e.g., `>`, `<=`) between `tonalInterval`s are based on their semitone (equal temperament) size, so enharmonicity is irrelevant.
#' Thus, `m3 >= A2` and `A2 >= m3` are both `TRUE`, even though `m3 == A2` is not.
#' 
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
#' # = 12
#' 
#' M3 %/% M2 
#' # = 2
#' 
#' "cc#]" + M6
#' # = "aa#]"
#' 
#' ###
#' 
#' cMajor <- sort(tint( , -1:5))
#' eMajor <- cMajor + M3
#' eMajor + 2L 
#' # 6 8 10 11 13 15 17
#' 
#' eMajor[4:5] - P8
#' # = -m3 -m2
#' 
#' 
#' 
#' @family {core pitch representations}
#' @family {Tonal S4 classes}
#' @seealso The main way to create `tonalInterval` S4 objects is with the [tonalInterval()] pitch parser.
#' @name tonalIntervalS4
NULL

## Definition, validity, initialization ####

#' @rdname tonalIntervalS4
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
tint <- function(octave, LO5th = 0L, cent = numeric(length(octave)), partition = FALSE, Key = NULL) {

    if (missing(octave) || is.null(octave)) {
      octave <- -floor(round(tint2semits(tint(integer(length(LO5th)), LO5th) %% tint(-11L, 7L)) / 12, 10))
    }
    
  
    tint <- new('tonalInterval',  Octave = as.integer(octave),  Fifth  = as.integer(LO5th),  Cent   = as.numeric(cent)) 
    tint <- tint %<-matchdim% if (hasdim(LO5th) && size(tint) == size(LO5th)) LO5th 
    if (partition) tintPartition(tint, Key = Key, octave.round = octave.round) else tint
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



## Logic methods ####

### is.methods ####


#' @rdname tonalIntervalS4
#' @export
is.tonalInterval <- function(x) inherits(x, 'tonalInterval')

#### Tonal is.methods ####


#' Test the properties of tonal information
#' 
#' These functions test basic properties of pitch information.
#' `is.simple` returns `TRUE` if pitch information is constrained 
#' in one octave.
#' `is.generic` returns `TRUE` if pitch information is "natural" to
#' the key (`Key`) argument.
#' 
#' @details 
#' 
#' These functions can be called directly on [tonalIntervals][tonalIntervalS4];
#' If called on anything else, the functions first calls the [tonalInterval()]
#' parser. If any values fail to parse `NA` is returned.
#'
#' "Simple" intervals fall in a particular octave relative to middle-C/unison.
#' The `octave.floor` argument can be used to change how this works:
#' The default option, `floor`, interprets the (**kern) pitches  `c`, `d`, `e`, `f`, `g`, `a`, and `b` to be "simple."
#' The most common alternative, `round`, identifies `G`, `A`, `B`, `c`, `d`, `e`, and `f` as "simple."
#' See the [pitch deparsing docs][pitchDeparsing] for a more detailed explanation.
#' 
#' "Generic" intervals belong to a key.
#' 
#' @param x ***Pitch information.***
#' 
#' Must be something that can be [parsed as pitch information][pitchParsing].
#' 
#' @param octave.round ***The rounding function.***
#' 
#' Must be a [rouding function][expand()]. 
#' 
#' Controls how simple intervals are interpreted relative to C.
#' 
#' @param Key ***The diatonic key used to defined generic pitches.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be something that can be [parsed as a diatonic key][keyParsing]; must be either length `1` or `length(x)`.
#'
#' @param ... ***Parameters passed to [tonalInterval()].***
#' 
#' @family {Tonal feature functions}
#' @export
is.simple <- function(x, ...) UseMethod('is.simple')

#' @rdname is.simple
#' @export 
is.simple.tonalInterval <- function(x, octave.round = floor, ...) {
  semits <- tint2semits(x, specific = FALSE, ...)
  
  octave.round(semits / 12) == 0
}


#' @rdname is.simple
#' @export
is.simple.default <- function(x, ...) is.simple.tonalInterval(tonalInterval(x, ...), ...)

#' @rdname is.simple
#' @export 
is.generic <- function(x, Key, ...) UseMethod('is.generic')

#' @rdname is.simple
#' @export 
is.generic.tonalInterval <- function(x, Key = NULL) {
  Key <- diatonicSet(Key %||% dset(0L, 0L))
  match_size(x = x, Key = Key, toEnv = TRUE)
  
  keynotes <- LO5th(Key)
  
  rowSums(sweep(keynotes, 1, x@Fifth, `-`) == 0L) > 0L
  
  
}

is.generic.default <- function(x, Key = NULL, ...) is.generic.tonalInterval(tonalInterval(x, Key = NULL, ...), Key = Key)

## Order/relations methods ####

#' @export order.tonalInterval
#' @rdname tonalIntervalS4
#' @exportMethod > >= < <= Summary abs sign
order.tonalInterval <- function(x, ..., na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
              
              x <- do.call('c', list(x, ...))
              order(tint2step(x), tint2semits(x), 
                    na.last = na.last,
                    decreasing = decreasing,
                    method = method
              )
          }
          
setMethod('>', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
             tint2semits(e1) > tint2semits(e2)
          })

setMethod('>=', signature = c('tonalInterval', 'tonalInterval'),
          function(e1, e2) {
              tint2semits(e1) >= tint2semits(e2)
          })

setMethod('Summary', signature = c('tonalInterval'),
          function(x, na.rm = TRUE) {
              semits2tint(callGeneric(tint2semits(x), na.rm = na.rm))
          })

setMethod('abs', signature = c('tonalInterval'),
          function(x) {
            neg <- x < tint(0L, 0L)
            x[neg] <- -x[neg]
            x
          })

setMethod('sign', signature = c('tonalInterval'),
          function(x) {
              sign(tint2semits(x))
          })



## Arithmetic methods ####

### Addition ####

#### Numeric

setMethod('+', signature = c('numeric', 'tonalInterval'),
          function(e1, e2) {
            e1 <- semits2tint(as.integer(e1))
            
            e3 <- e1 + e2
            
            tint2semits(e3)
            
          })

setMethod('+', signature = c('tonalInterval', 'numeric'),
          function(e1, e2) {
            e2 <- semits2tint(as.integer(e2))
            
            e3 <- e1 + e2
            
            tint2semits(e3)
            
          })

#### Character

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

#### Numeric

setMethod('-', signature = c('numeric', 'tonalInterval'),
          function(e1, e2) {
            e1 <- semits2tint(as.integer(e1))
            
            e3 <- e1 - e2
            
            tint2semits(e3)
            
          })

setMethod('-', signature = c('tonalInterval', 'numeric'),
          function(e1, e2) {
            e2 <- semits2tint(as.integer(e2))
            
            e3 <- e1 - e2
            
            tint2semits(e3)
            
          })

#### Character

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

## Deparsing (tonalInterval) documentation ----

#' Generating ("deparsing") pitch representations
#' 
#' [humdrumR] includes a easy-to-use system for 
#' generating a variety of tonal (or atonal) pitch representations,
#' which can be flexibly modified by users.
#' "Under the hood" `humdrumR` represents all tonal pitch information using the [same underlying representation][tonalIntervalS4],
#' which is typically extracted from input data using the [pitch parser][pitchParsing].
#' This representation can then be "deparsed" into a variety of predefined output formats (like `**kern`), 
#' or into new formats that you create!
#' 
#' Deparsing is the second step in the [pitch function][pitchFunctions] processing pipeline:
#' 
#' + **Input** representation `|>` 
#'   + *Parsing* `|>`
#'     + **Intermediate** ([tonalInterval][tonalIntervalS4]) representation `|>`
#'     + **Transformation**  `|>`
#'   + *Deparsing* (DEPARSING ARGS GO HERE) `|>`
#' +  **Output** representation 
#' 
#' Various pitch representations like `**kern`, `**solfa`, and `**semits` can be generated using predefined [pitch functions][pitchFunctions] like [kern()]
#' [semits()], and [solfa()] respectively.
#' All of these functions use a common deparsing framework, and are specified using different combinations of arguments
#' to the deparser.a
#' By modifying these *"deparsing" arguments*, you can exercise 
#' fine control over how you want pitch information to be represented in your output.
#' *This* documentation talks about this deparsing step.
#' For an overview of the parsing process, look [here][pitchParsing].
#' 
#' @section Basic pitch arguments:
#' 
#' 
#' Each pitch function has a few standard arguments which control details of the output.
#' The most important are the `generic` and `simple` arguments, which allow you to control what type of pitch information
#' is returned.
#' 
#' ## Generic vs Specific
#' 
#' If `generic = TRUE`, [specific pitch information](https://en.wikipedia.org/wiki/Generic_and_specific_intervals)
#'  (accidentals or qualities) is omitted from the output.
#' As an alternative way of controlling the same functionality, you can use the `specific` argument, where `specific == !generic`.
#' 
#' In the case of atonal functions, the "generic" version of that pitch is output:
#' for example, `semits('c#', generic = TRUE)` will return `0`, because the "generic" version of *C#* is *C*, which corresponds to `0`.
#' However, note that the generic version of a pitch follows the key, so `semits('c#', generic = TRUE, Key = 'A:')` will return `1`!
#' 
#' ## Simple vs Compound
#' 
#' If `simple = TRUE`, [compound pitch information](https://en.wikipedia.org/wiki/Interval_(music)#Simple_and_compound)
#' (octave and contour) is omitted from the output.
#' As an alternative way of controlling the same functionality, you can use the `compound` argument ,where `compound == !simple`.
#'
#' There is actually more than one way you might want to divide compound intervals up into simple and octave parts.
#' For example, you might like to call an output `-M2` (descending major 2nd) *OR* `+m7` (ascending minor 7th in the octave below).
#' This functionality can be controlled with the `octave.round` argument: 
#' see the [pitch deparsing documentation][pitchDeparsing].
#' 
#' ## Key
#' 
#' The `Key` argument must be a [diatonicSet], or something that can be parsed into one.
#' The `Key` argument is passed to the [parser][pitchParsing], deparser, *and* transpose---*unless* 
#' an alternate `Key` is passed to `transposeArgs` or `parseArgs`.
#' Various deparsing options use the `Key` argument; for example, use of `implicitSpecies` (see advanced parsing section) is dependent on the `Key`.
#' 
#' If you use any [pitch function][pitchFunctions] within a special call to [withinHumdrum],
#' `humdrumR` will automatically pass the `Key` field from the humdrum data to the function---this means, that in most cases, 
#' you don't need to explicitly do anything with the `Key` argument!
#' (If you want this *not* to happen, you need to explicitly specify your own `Key` argument, or `Key = NULL`.)
#'
#' 
#' ## Parse arguments
#' 
#' The `parseArgs` argument must be a [list()] of (named) arguments which are passed to the input [parser][pitchParsing].
#' For example, if our input representation uses `"X"` to represent double sharps, we could specify `kern('CX5', parseArgs = list(doublesharp = 'X'))`
#' and get the correct result (`"cc##"`).
#' As a convenient shorthand, or "syntactic sugar," you can specify `parseArgs` in an alternate way:
#' Simply input `parse(args...)` as unnamed argument to any pitch function.
#' For example, we can get the exact same result as before by typing `kern('CX5', parse(doublesharp = 'X'))`.
#'
#' ## Transpose arguments
#' 
#' The `transposeArgs` argument must be a [list()] of (named) arguments which are passed to an internal call
#' to [transpose()], allowing us to easily transpose pitch information.
#' For example, we could type `kern(c('C', 'D', 'E'), transposeArgs = list(by = 'M9'))` can get the output `c('d', 'e', 'f#')`.
#' The possible transpose args are:
#' 
#' + `by` ([tonalInterval][tonalIntervalS4], `length == 1 | length == (x)`)
#' + `from` ([diatonicSet], `length == 1 | length == (x)`)
#' + `to`  ([diatonicSet], `length == 1 | length == (x)`)
#' + `real` (`logical`, `length == 1`) Should transposition be real or tonal?
#' + `relative` (`logical`, `length == 1`) Should key-wise transposition be based on relative or parallel keys?
#' 
#' As a convenient shorthand, or "syntactic sugar," you can specify `transposeArgs` in an alternate way:
#' Simply input `transpose(args...)` as unnamed argument to any pitch function.
#' For example, we can get the exact same result as before by typing `kern(c('C', 'D', 'E'), transpose(by = 'M9'))`.
#'
#' ### Transposing by interval
#'
#' As when calling [transpose()] directly, the `by` argument can be anything coercable to a [tonalInterval][tonalIntervalS4], and
#' the output will be transposed by that amount.
#' If `real = FALSE`, tonal transposition (within the `Key`) will be performed.
#' For more details on transposition behavior, check out the [transpose()] docs.
#' 
#' ### Transposing by key
#' 
#' Another way of transposing is by specifying an input ("from") key and an output ("to") key.
#' By default, the `Key` argument is passed to `transpose` as both `from` and `to`, so nothing actually happens.
#' Thus, if you specify either a `from` key or `to` key, transposition will happen to/from that key to `Key`.
#' Of course, if you specify `from` *and* `to` the transposition will happen between the keys you specify.
#' 
#' If you use any [pitch function][pitchFunctions] within a special call to [withinHumdrum],
#' `humdrumR` will automatically pass the `Key` field from the humdrum data to the function.
#' If you specify a `to` key, the `Key` field will be passed as the transpose `from` key, and as a result,
#' all the pitches in the input will be transposed from whatever keys they are in to your target (`to`) key!
#' 
#' The `real` and `relative` arguments give you special control of how key-wise transposition works, so
#' check out the [transpose()] docs for more details!
#' 
#' 
#' ## In-place parsing
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
#' Note that `inPlace = TRUE` will force functions like `semits`, which normally return `numeric` values, to return `character` strings
#' *if* their input is a character string.
#' 
#' @section Deparsing arguments:
#' 
#' The following "advanced" deparsing arguments are available (read all the details about them further down):
#' 
#' + **Steps**
#'   + `step.labels`
#'   + `step.signed`
#' + **Species** (accidentals or qualities) 
#'   + `qualities`
#'   + `specifier.maximum`
#'   + *Accidentals*
#'     + `natural`, `flat`, `sharp`, `doubleflat`, `doublesharp`
#'   + *Qualities*
#'     + `perfect`, `major`, `minor`, `augment`, `diminish`
#'   + *Implicit vs Explicit Species*
#'     + `implicitSpecies`
#'     + `absoluteSpecies`
#'     + `explicitNaturals`
#'     + `cautionary`
#'     + `memory`, `memoryWindows`
#' + **Octave**
#'   + `octave.integer`
#'   + `up`, `down`, `same`
#'   + `octave.offset`
#'   + `octave.round`
#'   + `octave.relative`, `octave.absolute`
#' + **String parsing**
#'   + `parts`
#'   + `sep`
#'   
#' Note that the deparsing arguments are similar (sometimes identical) to parallel [parsing arguments][pitchParsing].
#' These "advanced" arguments can be used directly in *any* [pitch function][pitchFunctions]: 
#' for example, `kern(x, qualities = TRUE)`.
#' 
#' Each of the `humdrumR` pitch functions is associated with default deparsing arguments.
#' For example, if you use [kern()], `flat` is set (by default) to `"-"`.
#' However, if you wanted to print `**kern`-like pitch data, **except** with a 
#' different flat symbol, like `"_"`, you could modify the deparser:
#' `kern('Eb5', flat = "_")`.
#' This overrides the default value for `**kern`, so the output would be `"ee_"` instead of `"ee-"`.
#' 
#' ## Steps
#' 
#' All representations of "tonal" pitch information include a representation of *diatonic steps*.
#' You can control how the deparser writes diatonic steps using the `step.labels` argument.
#' The `step.labels` argument must be an atomic vector of unique values, with a length which is a positive multiple of seven.
#' Examples of `step.labels` arguments that are currently used by `humdrumR` [pitch functions][pitchFunctions] include:
#' 
#' + `step.labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G')` 
#' + `step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')`
#' + `step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't')`
#' 
#' If `step.labels` is `NULL`, steps are assumed printed as integers, including negative integers representing downward steps.
#' 
#' 
#' 
#' There is also a `step.signed` (`logical`, `length == 1`) argument: if `step.signed = TRUE`, lowercase versions of `step.labels` are interpreted as negative (downward) steps and
#' uppercase versions of `step.labels` are interpreted as positive (upwards) steps.
#' This option is used, for example, by the default [kern()] and [helmholtz()] parsers.
#' 
#' ## Species
#' 
#' In tonal pitch representations, "*specific*" versions of tonal pitches---the tonal "species"---are indicated by "specifiers": 
#' either *accidentals* or *qualities*.
#' The `qualities` (`logical`, `length == 1`) argument indicates whether accidentals are used (`qualities = FALSE`) or qualities (`qualities = TRUE`).
#' Some specifiers can be repeated any number of times, like "triple sharps" or "doubly augmented";
#' The `specifier.maximum` (`integer`, `length == 1`) argument sets a maximum limit on the number of specifiers to write.
#' For example, you could force all triple sharps (`"###"`) or double sharps (`"##"`) to deparse as just `"#"`, by specifying `specifier.maximum = 1L`.
#' 
#' ### Accidentals 
#'   
#' If `qualities = FALSE` the deparser will print accidentals of three types: naturals, flats, and sharps.
#' The `natural`, `flat`, and/or `sharp` (`character`, `length == 1`) arguments can be used to indicate 
#' how accidentals are printed in the output.
#' For example, if set the `kern('Eb5', flat = 'flat')` you get the output `"eeflat"`.
#' 
#' Examples of accidental argument combinations that are currently used by `humdrumR` [pitch functions][pitchFunctions] include:
#' 
#' + `(flat = "b", sharp = "#")` ->  [pitch()]
#' + `(flat = "-", sharp = "#")` ->  [kern()]
#' + `(flat = "es", sharp = "is")` -> [lilypond()]
#' + `(flat = "-", sharp = "+")` -> [degree()]
#' 
#' 
#' 
#' 
#' The `doubleflat`, and `doublesharp` (`character`, `length == 1`) arguments are `NULL` by default, but can be set if a special symbol is wanted
#' to represent two sharps or flats. For example, you could modify [pitch()] to use a special double sharp symbol:
#' `pitch("f##", doublesharp = "x")` and the output will be `"Fx4"`.
#' 
#' The printing of naturals is controlled by the `natural` argument.
#' However, by default, the `humdrumR` deparsers don't both printing naturals.
#' You can force *all* naturals to print my setting the `explicitNaturals` (`logical`, `length == 1`)
#' argument to `TRUE`.
#' The exact behavior of `explicitNaturals` depends on the `implicitSpecies`, `absoluteSpecies`,
#' and `Key` argument (details below).
#' 
#' ### Qualities
#' 
#' If `qualities = TRUE` the deparser will print qualities, of five types: perfect, minor, major, augmented, and diminished.
#' The `perfect`, `major`, `minor`, `diminish`, and/or `augment` (`character`, `length == 1`) arguments 
#' can be used to indicate how qualities are printed in the output.
#' (Note: we are talking about interval/degree qualities here, not chord qualities!)
#' For example, you can write `interval(c("g-", "f#"), augment = 'aug', diminish = 'dim')`
#' and the output `c("+dim5", "+aug4")`.
#' Examples of quality argument combinations that are currently used by `humdrumR` [pitch functions][pitchFunctions] include:
#' 
#' + `parse(major = "M", minor = "m", perfect = "P", diminish = "d", augment = "A")`
#' + `parse(diminish = "o", augment = "+")`
#
#' ### Implicit vs Explicit Species
#' 
#' In some musical data, specifiers (e.g., accidentals or qualities) are not explicitly indicated; instead, 
#' you must infer the species of each pitch from the context---like the key signature!. 
#' 
#' #### From the Key
#' 
#' The most important argument here is `implicitSpecies` (`logical`, `length == 1`):
#' if `implicitSpecies = TRUE`, the species of input without an explicit species indicated is interpreted using the `Key`.
#' For example, 
#' 
#' + `kern('C', Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`
#'   + C is sharp in A major.
#' + `kern('C', Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as `"C"`
#'   + C is natural in A minor.
#' + `kern('C', Key = 'a-:', parse(implicitSpecies = TRUE))` is parsed as `"C-"`
#'   + C is flat in A-flat minor.
#'   
#' By default, if you input *already has* specifiers, they are interpreted absolutely---overriding the "implicit" `Key`---,
#' even if `implicitSpecies = TRUE`.
#' Thus, if we are in A major:
#' 
#' + `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`. 
#'   + The `"#"` is unnecessary. 
#' + `kern("Cn", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C"`. 
#'   + The `"n"` overrides the `Key`.
#' + `kern("C#", Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`.
#'   + The `"#"` overrides the `Key`.
#'   
#' However! You can also change this behavior by setting the `absoluteSpecies` (`logical`, `length == 1`) argument to `FALSE`.
#' If you do so, the specifiers in the input are interpreted "on top of" the key accidental:
#' 
#' + `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE, absoluteSpecies = FALSE))` is parsed as `"C##"`. 
#'   + The `"#"` from the input is added to the `"#"` from the `Key`, resulting in double sharp!
#'   
#' This is an unusual behavior, for absolute pitch representations like `**kern`.
#' However, for use with scale or chord degrees, `absoluteSpecies = FALSE` might be appropriate.
#' For example, if we are reading a [figured bass](https://en.wikipedia.org/wiki/Figured_bass) in the key of E minor,
#' a `"b7"` figure above an E in the bass should be interpreted as a *double flat* (diminished) 7th (Db over E)!
#' If this is how your data is encoded, use `absoluteSpecies = FALSE`.
#' 
#' #### Memory
#' 
#' In some musical data, it is assume that a accidental on a note "stays in effect" on that scale step until the next bar,
#' or until a different accidental replaces it.
#' Fortunately, the `humdrumR` parser (`tonalInterval()`) also knows how to parse data encoded with "memory" this way.
#' If `memory = TRUE`, the accidental (or quality) of each input note is "remembered" from previous appearances of that scale step.
#' For example,
#' 
#' + `kern(c("D#", "E", "D", "E", "Dn", "C", "D"), parse(memory = TRUE))` 
#'   + is parsed as `c("D#", "E", "D#", "E", "D", "C", "D")`
#'
#' If we want the "memory" to only last when specific time windows (like bars), we can also specify a 
#' `memoryWindows` argument. `memoryWindows` must be an atomic vector which is the same length as the input (`x` argument).
#' Each unique value within the `memoryWindows` vector is treated as a "window" within which `memory` operates.
#' The most common use case would be to pass the `Bar` field from a `humdrumR` dataset to `memoryWindows`!
#'
#' The `memory` and `memoryWindows` argument work whatever values of `implicitSpecies` or `absoluteSpecies` are specified!
#' Though all the examples here use accidentals, these arguments all have the same effect if parsing qualities (`qualities = TRUE`).
#' 
#' ## Octave
#' 
#' The final piece of information encoded in most (but not) all pitch representations is an indication of the "compound pitch"---
#' incorporating octave information.
#' In `humdrumR` octaves are *always* defined in terms of scale steps: so two notes with the same scale degree/letter name will always be the same octave.
#' This mainly comes up with regards to Cb and B#: Cb4 is a semitone below ; B#3 is enharmonically the same as middle-**C**.
#' 
#' ### Integer Octaves 
#' 
#' The simplest way octave information can be encoded is as an integer value, as in [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch).
#' If you need to parse integer-encoded octaves, set the `octave.integer` (`logical`, `length == 1`) argument to `TRUE`.
#' By default, `humdrumR` considers the "central" octave (`octave == 0`) to be the octave of , or equivalently, a unison.
#' However, if a different octave is used as the central octave, you can specify the `octave.offset` (`integer`, `length == 1`) argument.
#' 
#' To illustrate, the default [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch) parser used the arguments:
#' 
#' + `kern('C5', parse(octave.integer = TRUE, octave.offset = 4)`
#'   + Returns `"cc"` (the octave above middle C).
#'   
#' ### Non-integer Octave Markers
#' 
#' If `octave.integer = FALSE`, the `humdrumR` parser instead looks for three possible symbols to indicate octave information.
#' These symbols are controlled using the `up`, `down`, and `same` (`character`, `length == 1`) arguments.
#' A `same` symbol, or no symbol, is interpreted as the "central" octave; repeating strings of the `up` or `down` symbols indicate
#' increasing positive (`up`) or negative (`down`) octaves.
#' For example, in `lilypond` notation, `,` represents lower octaves, and `'` (single apostrophe) represents upper octaves.
#' So the default [lilypond()] parser uses these arguments:
#' 
#' + `pitch(c("c", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",", octave.offset = 1))`
#'   + Returns `c("C2", "C3", "C4")`.
#'   
#' (Note that lilypond makes the octave *below*  the central octave, using `octave.offset = 1`.)
#' 
#' 
#' ### Octave "Rounding"
#' 
#' In some situations, pitch data might interpret the "groupby" between octaves a little differently.
#' In most absolute pitch representations (e.g., [kern()], [pitch()]), the "boundary" between one octave and the next is 
#' between B (degree 7) and C (degree 1).
#' However, if for example, we are working with data representing intervals, we might think of an "octave" as spanning the range `-P4` (`G`) to `+P4` (`f`).
#' In this case, the "octave boundary" is *centered* around the unison (or ), rather than starting *at* middle-**C**/unison.
#' If our data was represented this way, we could use the `octave.round` argument; `octave.round` must be a rounding *function*, 
#' either [round, floor, ceiling, trunc][base::round], or [expand].
#' These functions indicate how we interpret simple pitches "rounding" to the nearest C/unison.
#' The default behavior for most pitch representations is `octave.round = floor`: each scale step is rounded downwards to the nearest C.
#' So B is associated with the C 7 steps below it.
#' If, on the other hand, `octave.round = round`, then scale-steps are "rounded" to the closest C, so B and A are associated with the closer C *above* them.
#' Indeed, `octave.round = round` gets us the `-P4` <-> `+P4` behavior we mentioned earlier!
#'
#' When working parsing [intervals][interval()], the `octave.round` option allows you to control how the "simple part" (less than an octave) of a compound interval is represented.
#' For example, we might think of a ascending major 12th as being an ascending octave *plus* a ascending perfect 5th: ** +P8 + P5**.
#' **Or** we could encode that same interval as *two* ascending octaves *minus* a perfect fourth: **+ P15 - P4**.
#' The following table illustrates how different `octave.round` arguments "partition" compound intervals into simple parts and octaves:
#'
#' ```{r, echo = FALSE, comment=NA, result = 'asis'}
#' 
#' 
#' tint <- tonalInterval(c('FF', 'GG','C', 'F','G', 'c', 'f','g','cc','ff','gg','ccc', 'fff', 'ggg'))
#' 
#' tintparts <- lapply(c(round, floor, ceiling, trunc, expand), \(f) tintPartition_compound(tint, octave.round = f))
#' 
#' intervals <- sapply(tintparts, \(tp) paste0(interval(tp$Octave), ' + ', interval(tp$Simple)))
#' intervals <- gsub('\\+ \\+', '+ ', intervals)
#' intervals <- gsub('\\+ -', '- ', intervals)
#' intervals <- gsub('^P', '+P', intervals)
#' intervals <- gsub('([81]) ', '\\1  ', intervals )
#' 
#' colnames(intervals) <- c('round', 'floor', 'ceiling', 'trunc', 'expand')
#' rownames(intervals) <- format(paste0(interval(tint), ': '), justify = 'right')
#' 
#' knitr::kable(intervals, "pipe")
#' 
#' ```
#' 
#' Notice that, if `octave.floor` is being used, all simple intervals are represented as ascending.
#' 
#' 
#' 
#' When parsing ["absolute" pitch][pitch()] representations, the `octave.round` option allows you to control which octave notes are associated with.
#' The following table illustrates:
#' 
#' ```{r, echo = FALSE, comment=NA, result = 'asis'}
#' 
#' 
#' tint <-  tonalInterval(c('FF', 'GG','C', 'F','G', 'c', 'f','g','cc','ff','gg','ccc', 'fff', 'ggg'))
#' 
#' pitches <- do.call('cbind', lapply(c(round, floor, ceiling, trunc, expand), \(f) pitch(tint, octave.round = f)))
#' 
#' 
#'
#' 
#' colnames(pitches) <- c('round', 'floor', 'ceiling', 'trunc', 'expand')
#' rownames(pitches) <- format(paste0(kern(tint), ': '), justify = 'right')
#' 
#' knitr::kable(pitches, 'pipe')
#' 
#' ```
#' 
#' ### Absolute or Relative (contour) Octave
#' 
#' In some notation encoding schemes, the "octave" of each note is interpreted *relative* the previous note, rather than any absolute reference.
#' The most prominent system is Lilypond's [relative octave entry](https://lilypond.org/doc/v2.22/Documentation/notation/writing-pitches#relative-octave-entry) style.
#' This style is often used in combination with scale degree representations---as in the [RS200](http://rockcorpus.midside.com/melodic_transcriptions.html) corpus.
#' For example, a data set might say `Do Re Mi vSo La Ti Do`, with the `"v"` indicating a jump down to `So`. 

#' To activate relative-octave parsing, set `octave.relative = TRUE`---alternatively, you can use `octave.absolute = FALSE`, which is equivalent.
#' 
#' In a relative-octave data, we assume that octave indications indicate a shift relative to the previous note.
#' This would usually be used in combination with octave markers like `"^"` (up) or `"v"` (down).
#' Different combinations of `octave.round` allow us to parse different behaviors:
#' 
#' + If `octave.round = round`, a `same` marker (or no marker) indicates that the note is the pitch *closest* to the previous pitch.
#'   Octave markers indicate alterations to this assumption.
#'   As always, this is based on scale steps, not semitones!
#'   Any fourth is "closer" than any fifth, regardless of their quality: So *C F#* is ascending and *C Gb* is descending!
#'   A ascending diminished 5th would be written `C ^Gb`---with `up = ^`. 
#' + If `octave.round = floor`, a `same` marker (or no marker) indicates that the note is in the octave above the previous pitch.
#'   Octave markers indicate alterations to this assumption.
#'   With this setting, going from *C* down to *B* always requires a `down` mark.
#'
#' ## String Parsing
#' 
#' In addition to the three types of *musical* parsing considerations reviewed above (steps, species, and octaves), there are also some general 
#' string-parsing issues that we can consider/control.
#' 
#' ### Parts and Order
#' 
#' So far (above) we've discussed various ways that tonal pitch information (step, species, and octave) can be encoded, and how
#' the `humdrumR` parser can be modified to handle different options.
#' However, there are two general parsing issues/options to consider: what information is encoded, and in *what order*?
#' The `parts` argument can be specifyied to indicate this.
#' The `parts` argument must be a `character` vector of length 1--3.
#' The characters in the must [partial match][base::pmatch] either `"step"`, `"species"`, or `"octave"`.
#' The presense of any of these strings in the `parts` vector indicate that that information should be parsed.
#' The *order* of the strings indicates what order the pieces of pitch information are encoded in input strings.
#' 
#' To illustrate, imagine that we had input data which was identical to a standard interval representation---e.g., `M2` and `P5`---except the 
#' quality appears *after* the step---e.g., `2M` and `5P`.
#' We could call `interval(c("2M", "5P"), parse(parts = c("step", "species")))` and sure enough we'd get the correct parse!
#' 
#' 
#' 
#' One final string-parsing argument is `sep`, which indicates if there is a character string separating the pitch information components:
#' The most common case would be a comma or space.
#' For example, we could use a parse command like this: `kern("E flat 5", parse(flat = "flat", sep = " "))`.
#' 
#' @section Pitch-Gamut Levels:
#' 
#' The [table()] will automatically generate factor levels 
#' for pitch data using the [gamut()] function.
#' This is makes sure tabulated data sorted in a logical order, and includes
#' missing pitches.
#' The `simple`/`complex` and  `generic`/`specific` arguments are automatically passed to [gamut()]; additional
#' arguments can be passed to gamut using `gamutArgs = list(...)`, or with the syntactic sugar `gamut(...)`.
#' (Read the [gamut()] docs for an explanation of gamut generation.)
#' This feature be used to control table layout of pitch data, as well as to assure
#' consistent tables when grouping data.
#' 
#' When `inPlace = TRUE` no special tabulation will occur.
#' 
#' 
#' @seealso All `humdrumR` [pitch functions][pitchFunctions] make use of the deparsing functionality.
#' @name pitchDeparsing
NULL

## Pitch deparsers ####

### Octaves ####


tint2octave <- function(x,
                        octave.integer = TRUE,
                        up = '^', down = 'v', same = "",
                        octave.offset = 0L, octave.maximum = Inf, octave.minimum = -Inf,
                        octave.relative = FALSE, octave.round = floor, ...) {

  if (octave.relative) x <- delta(x, init = tint(0L, 0L))
  #
  octn <- octave.offset + tintPartition_compound(x, octave.round = octave.round)$Octave@Octave
  octn <- pmin(pmax(octn, octave.minimum), octave.maximum)
  
  if (octave.integer) return(as.integer(octn))
  
  out <- rep(NA_character_, length(octn))
  out[octn == 0L] <- same
  out[octn != 0L] <- strrep(ifelse(octn[octn != 0L] >= 0L, up, down), abs(octn[octn != 0L]))
  
  out 
  
}

tint2sign <- function(x, octave.offset = 0L, ...) {
 sign(tint2semits(x) + octave.offset * 12L)
}


octave.kernstyle <- function(str, octn, step.case = TRUE) {
  char <- substr(str, 0L, 1L)
  if (step.case) char <- .ifelse(octn >= 0L, tolower(char), toupper(char))
  
  octn[!is.na(octn) & octn >= 0L] <- octn[!is.na(octn) & octn >= 0L] + 1L # 0 -> 1
  
  .paste(strrep(char, abs(octn)), stringr::str_sub(str, start = 2L)) %<-matchdim% str
}


LO5thNscaleOct2tint <- function(LO5th, scaleOct) {
  tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
  octshift <- tint2semits(tintWith0Octave %% tint(-11L, 7L)) %/% 12L
  
  tint(scaleOct - octshift, LO5th)
}

LO5thNsciOct2tint <- function(LO5th, sciOct) LO5thNscaleOct2tint(LO5th, sciOct - 4L) 

LO5thNcentralOct2tint <- function(LO5th, centralOct) {
  tintWith0Octave <- tint(integer(length(LO5th)), LO5th)
  octshift <- round(tint2semits(tintWith0Octave %% tint(-11L, 7L)) / 12L)
  
  tint(centralOct - octshift, LO5th)
}





### Atonal ####

#### Semitones ####

tint2semits <- function(x, Key = NULL, specific = TRUE, compound = TRUE, directed = TRUE, ...) {

  
  if (!is.null(Key) && length(x) > 0L) x <- x + diatonicSet(Key)
  
  if (!specific) x <- tintPartition_specific(x, Key = Key, ...)$Generic
        
  semits <- as.integer((((x@Fifth * 19L) + (x@Octave * 12L)) + (x@Cent / 100L)))
  
  if (!compound) semits <- semits %% 12L
  if (!directed) semits <- abs(semits)
  semits
  
}


tint2midi <- function(x, ...) {
  tint2semits(x, ...) + 60L
}

tint2cents <- function(x, ...) {
  double <- tint2double(x, ...)
  
  log(double, 2^(1/12)) * 100
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

tint2fraction <- function(tint, tonalHarmonic = 3) as.fraction(tint2rational(tint, tonalHarmonic = tonalHarmonic)) 

tint2double <-  function(x, tonalHarmonic = 2^(19/12), ...) {
  LO5th <- x@Fifth
  oct   <- x@Octave
  cent  <- x@Cent
  
    (2 ^ oct) * (tonalHarmonic ^ LO5th) * 2^(cent / 1200)
}

tint2freq <- function(x, frequency.reference = 440L, 
                      frequency.reference.note = tint(-4L, 3L), 
                      ...) {
  x <- x - tonalInterval(frequency.reference.note)
  
  ratio <- tint2double(x, ...)
  
  frequency.reference * ratio
}

tint2pc <- function(x, ten = 'A', eleven = 'B', ...) {
  str <- as.character(tint2semits(x, compound = FALSE))
  
  if (!is.null(ten)) str <- gsub('10', ten, str)
  if (!is.null(eleven)) str <- gsub('11', eleven, str)
  
  str
  
}


### Tonal ####

tint2step  <- function(tint, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), ...) {
  
  stepint <- 1L + (LO5th(tint) * 4L) %% 7L
  # stepint <- c(1L, 5L, 2L, 6L, 3L, 7L, 4L)[1 + (LO5th(x) %% 7L)]
  
  if (is.null(step.labels))  stepint else step.labels[stepint]

}


tint2third <- function(tint) LO5th2third(LO5th(tint))

LO5th2third <- function(lof) (lof * 2L) %% 7L
  

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



tint2specifier <- function(x, Key = NULL, ...,
                           qualities = FALSE,
                           cautionary = FALSE, memory = FALSE, memoryWindows = NULL,
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
  
  
  dontlabel <- if (truthy(memoryWindows) && length(memoryWindows) == length(LO5th)) {
    tapply_inplace(LO5th, memoryWindows,  \(x) alteration.filter(x, Key, cautionary, memory, implicitSpecies, explicitNaturals))
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
                             step = TRUE, specific = TRUE, compound = TRUE,
                             step.compound = FALSE,
                             keyed = FALSE, Key = NULL,
                             qualities = FALSE, collapse = TRUE, ...) {
  
  
  if ((compound || keyed) && !is.null(Key)) {
    Key <- rep(Key, length.out = length(x))
    Key <- diatonicSet(Key)
  }
  
  if (keyed && !is.null(Key))  x[!is.na(Key)] <- x[!is.na(Key)] + Key[!is.na(Key)]
  
  parts <- matched(parts, c( "species", "step", "octave"))
  

  
  # simple part
  step     <- if (step)        tint2step(x, ...) 
  species  <- if (specific)    tint2specifier(x, qualities = qualities, Key = Key, ...)   
  
  
  # compound part
  octave  <- if (compound) {
    if (!keyed && !is.null(Key)) x[!is.na(Key)] <- x[!is.na(Key)] + Key[!is.na(Key)]
    
    octave <- tint2octave(x, ...)
    if (is.integer(octave) && is.integer(step) && step.compound) {
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
  
  pasteordered(parts, step = step, species = species, octave = if (compound) octave, sep = sep, collapse = collapse)
    
  
}





tint2pitch <- partialApply(tint2tonalChroma,  
                           step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                           octave.offset = 4L, octave.integer = TRUE,
                           flat = 'b', qualities = FALSE,
                           keyed = TRUE,
                           parts = c("step", "species", "octave"))

tint2simplepitch <- partialApply(tint2tonalChroma,  
                                 step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                                 flat = 'b', qualities = FALSE,
                                 keyed = FALSE,
                                 parts = c("step", "species"))



tint2kern <- function(x, compound = TRUE, octave.round = floor, Key = NULL, ...) {
  
  t2tC <- partialApply(tint2tonalChroma,
                       step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                       parts = c("step", "species"), qualities = FALSE, compound = FALSE,
                       keyed = TRUE)
  
  kern <- t2tC(x, Key = Key, ...)
  
  
  # if (directed) {
  #   direction <- stringr::str_extract(kern, '^[+-]?')
  #   kern <- tolower(stringr::str_remove(kern, '^[+-]'))
  # }  else {
  #   direction <- ""
  # }
  
  
  if (compound) {
    kern <- octave.kernstyle(kern, 
                             tint2octave(if (is.null(Key)) x else x + Key, 
                                         octave.round = octave.round,
                                         octave.integer = TRUE), step.case = TRUE)
  }
  
  kern
  
}



tint2lilypond <- partialApply(tint2tonalChroma, 
                              step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                              up = "'", down = ",",
                              qualities = FALSE,
                              octave.relative = FALSE, octave.integer = FALSE,
                              octave.round = floor,
                              octave.offset = 1L, 
                              sharp = 'is', flat = 'es',
                              keyed = TRUE,
                              parts = c("step", 'species', "octave"))


tint2tonh <- function(x, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), flat = 'es', natural = 'n', S = TRUE, ...) {
  
  t2tC <- partialApply(tint2tonalChroma,
                       parts = c('step', 'species', 'octave'),
                       octave.integer = TRUE, octave.offset = 4L,
                       keyed = TRUE,
                       sharp = 'is')
  
  
  str <- t2tC(x, step.labels = step.labels, flat = flat, natural = natural, ...)
  
  
  seven <- step.labels[7L]
  
  str <- gsub(paste0(seven, natural), 'H', str)
  str <- gsub(paste0(seven, '(-?[0-9])'), 'H\\1', str)
  
  str[str == paste0(seven, flat)] <- 'B'
  
  if (S) {
    str <- gsub('([AE])es', '\\1s', str)
    str <- gsub(paste0(step.labels[3], flat), 'S', str)
  }
  
  str
}

tint2helmholtz <- function(x, ...) {
  
  octn <- tint2octave(x, octave.integer = TRUE, octave.offset = 1L)
  
  
  x[octn < 0L] <- x[octn < 0L] + tint(1L, 0L)
  
  t2tC <- partialApply(tint2tonalChroma,  
                       step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                       flat = 'b', parts = c('step', 'species', 'octave'),
                       keyed = TRUE,
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




tint2interval <- function(x, directed = TRUE, keyed = TRUE, Key = NULL, ...) {
  
  t2tC <- partialApply(tint2tonalChroma,
                       step.labels = 1L:7L,
                       parts = c("species", "step", "octave"),
                       compound = TRUE, qualities = TRUE, 
                       step.compound = TRUE,
                       octave.integer = TRUE, octave.relative = FALSE, explicitNaturals = TRUE,
                       octave.round = floor)
  
  if (keyed && !is.null(Key)) x <- x + Key
  
  direction <- if (directed) {
    c('-', '', '+')[sign(x) + 2L]
  } else {
    ""
  }
  x <- abs(x)
  
  
  interval <- t2tC(x, keyed = FALSE,  ...)
  
  paste0(direction, interval)
}




tint2degree <- partialApply(tint2tonalChroma, parts = c("step", "species", "octave"), 
                            compound = TRUE, keyed = FALSE, step.labels = 1L:7L, 
                            flat = '-', sharp = '+', sep = c("", "/"),
                            octave.integer = TRUE, octave.offset = 4L)

tint2deg <- partialApply(tint2tonalChroma, parts = c("octave", "step", "species"), 
                            compound = TRUE, keyed = FALSE, step.labels = 1L:7L, 
                            flat = '-', sharp = '+', 
                            octave.integer = FALSE, octave.relative = TRUE, octave.round = expand)


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
                       specific = TRUE, flat = '-', sharp = '#', factor = FALSE, ...) {
  
  
  t2tC <- partialApply(tint2tonalChroma, octave.integer = FALSE, octave.relative = FALSE,
                       step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't'), 
                       qualities = FALSE, accidental.integer = TRUE)
  
  solfa_parts <- t2tC(x, keyed = FALSE, specific = specific, Key = Key, ..., collapse = FALSE) #
  
  # change species to syllable "tails"
  solfa_parts$tail <- if (specific) {
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

tint2solfg <- partialApply(tint2tonalChroma, flat = '~b', doubleflat = '~bb', sharp = '~d', doublesharp = '~dd', natural = '~n',
                           step.labels = c('do', 're', 'mi', 'fa', 'so', 'la', 'ti'), 
                           parts = c('step', 'species', 'octave'), keyed = TRUE,
                           octave.integer = TRUE, octave.offset = 4L)





###################################################################### ### 
# Parsing Pitch Representations (x2tint) #################################
###################################################################### ### 


## Parsing (tonalInterval) documentation ----

#' Parsing pitch information
#' 
#' 
#' [humdrumR] includes a easy-to-use but powerful system for *parsing* pitch information:
#' various basic pitch representations (including `numeric` and `character`-string representations) can be "parsed"---read
#' and interpreted by `humdrumR`.
#' For the most part, parsing automatically happens "behind the scenes" whenever you use any humdrumR [pitch function][pitchFunctions], like [kern()]
#' [semit()], or [solfa()].
#' 
#' @details 
#'
#' The underlying parser used by all `humdrumR` [pitch functions][pitchFunctions] can be called explicitly using the function `tonalInterval()`.
#' The `tonalInterval` parser will attempt to parse any input information into a [tonalInterval][tonalIntervalS4] object---a back-end pitch representation 
#' that you probably don't need to care about!
#' When you use one of the main [pitch functions][pitchFunctions], like [kern()] or [semits()], 
#' the input is parsed into a [tonalInterval][tonalIntervalS4] object, then immediately [deparsed][pitchDeparsing]
#' to the representation you asked for (e.g., `**kern` or `**semits`).
#' Thus, the underlying pipeline for `humdrumR` [pitch functions][pitchFunctions] looks something like:
#' 
#' + **Input** representation (e.g., `**pitch` or `**semits`) `|>` 
#'   + *Parsing* (done by `tonalInterval()`) `|>`
#'     + **Intermediate** ([tonalInterval][tonalIntervalS4]) representation `|>`
#'   + *Deparsing* `|>`
#' +  **Output** representation (e.g. `**kern` or `**solfa`)
#' 
#' *This* documentation talks about the parsing step.
#' For an overview of the "deparsing" process, look [here][pitchDeparsing].
#' To learn about the "deparsing" of specific representations, [start here][pitchFunctions] or go straight to the docs for specific functions---
#' for example, call `?kern` to learn about [kern()].
#' 
#' 
#' # Dispatch
#' 
#' The pitch parser (`tonalInterval()`) is a generic function, meaning it accepts a variety of inputs 
#' and automatically "dispatches" the appropriate method for parsing ehe input.
#' R's standard `S3` system is used to dispatch for either `numeric` or `character`-string input:
#' Generally, `numeric` (or `integer`) inputs are interpreted as various *atonal* pitch representations while
#' `character` strings are interpreted as various *tonal* pitch representations.
#' Given either a `character` string or a number, `humdrumR` then uses either regular-expression matching or humdrum
#' exclusive interpretation matching to dispatch specific parsing methods.
#' 
#' # Tonal Parsing (`character`-string inputs)
#' 
#' Since humdrum data is inherently string-based, the most powerful part of the `humdrumR` pitch-parser
#' is its system for parsing pitch (mostly tonal) information from character strings.
#' (This includes character tokens with pitch information embedded alongside other information; Details below.)
#' The pitch parser (`tonalInterval()`) uses a combination of regular-expressions and exclusive interpretations to decide how to 
#' parse an input string.
#' There are twelve regular-expression patterns for pitch that `tonalInterval()` knows how to parse automatically:
#' 
#' | Representation                                                                     | Exclusive                 | Example          |
#' | ---------------------------------------------------------------------------------- | ------------------------: | ---------------: |
#' | [Kern](https://www.humdrum.org/rep/kern/index.html)                                | **kern                    | `ee-`            |
#' | [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch)                 | **pitch                   | `Eb5`            |
#' | [Helmholtz pitch](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                      | none                      | `eb'`            |
#' | [Lilypond pitch](https://lilypond.org/doc/v2.22/Documentation/notation/pitches)    | none                      | `ees'`           |
#' | [German pitch](https://www.humdrum.org/rep/Tonh/index.html) notation               | **Tonh                    | `S5`             |
#' | [Interval](https://en.wikipedia.org/wiki/Interval_(music))                         | **hint/**mint/**int       | `+m3`            |
#' | [Scale degree](https://en.wikipedia.org/wiki/Degree_(music))                       | **deg or **degree         | `^^3-` or `3-/5` |
#' | [Pitch Class](https://en.wikipedia.org/wiki/Pitch_class#Integer_notation)          | **pc                      | `3`              |
#' | Relative-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                  | **solfa                   | `^me`            |
#' | Fixed-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge)                     | **solfg                   | `mi~b5`          |
#' | [Swara](https://en.wikipedia.org/wiki/Svara)                                       | **bhatk                   | `g`              |
#' 
#' 
#' ## Exclusive Dispatch
#' 
#' If you call `tonalInterval()` (or *any* [pitch function][pitchFunctions]) on a `character`-string vector, with a non-`NULL` `Exclusive` argument,
#' that `Exclusive` argument will be used to choose the input interpretation you want, based on the "Exclusive" column in the 
#' table above.
#' For example, `kern(x, Exclusive = 'solfa')` will force the parser to interpret `x` as `**solfa` data.
#' Similarly, `solfa(x, Exclusive = 'kern')` will force the parser to interpret `x` as `**kern` data.
#' If you use any [pitch function][pitchFunctions] within a special call to [withinHumdrum],
#' `humdrumR` will automatically pass the `Exclusive` field from the humdrum data to the function---this means, that in most cases, 
#' you don't need to explicitly do anything with the `Exclusive` argument!
#' (If you want this *not* to happen, you need to explicitly specify your own `Exclusive` argument, or `Exclusive = NULL`.)
#' 
#' ## Regex Dispatch
#' 
#' If you call `tonalInterval()` (or *any* [pitch function][pitchFunctions]) on a `character`-string vector, but the `Exclusive` argument is missing
#' or `NULL`, `humdrumR` will instead use regular-expression patterns to select a known interpretation.
#' For example, `pitch('so')` will automatically recognize that `'so'` is solfege, and will interpret the data accordingly (the output should be `r pitch('so')`).
#' If there are more than one matches, `humdrumR` will use the longest match, and if they tie, pick based on the order in the table above (topmost first).
#' 
#' 
#' If there is no match, `tonalInterval` (and all other [pitch function][pitchFunctions]) return `NA` values.
#' Remember, if `Exclusive` is specified, it overrides the regex-based dispatch, which means that `pitch('so', Exclusive = 'kern')` will return `NA`, because
#' `'so'` can't be interpreted as a `**kern` value.
#' 
#' ### "In place" parsing
#' 
#' In lots of humdrum data, character strings are encoded with multiple pieces of musical information right besides each other:
#' for example, `**kern` data might include tokens like `"4.ee-[`.
#' The `humdrumR` pitch parser (`tonalInterval()`) will automatically "pull out" pitch information from within strings, if it can find any, 
#' using the appropriate known regular expressions.
#' Various [pitch parsing functions][pitchFunctions] have an option to keep the original "extra" data, using their `inPlace` argument.
#' 
#' ## Advanced Tonal Parsing Options
#' 
#' The eleven tonal representations listed above are parsed through a common intesrface.
#' By using "advanced" parsing arguments, you can tweak how this parsing is done, so as to accommodate even more input representations!
#' This means we are controlling the behavior of `tonalInterval()`, in the second step of our pipeline:
#' 
#' + **Input** representation `|>` 
#'   + *Parsing* (done by `tonalInterval(PARSE ARGS GO IN HERE!)`) `|>`
#'     + **Intermediate** ([tonalInterval][tonalIntervalS4]) representation `|>`
#'   + *Deparsing* `|>`
#' +  **Output** representation 
#' 
#' Note that these arguments are similar or identical to parallel "advanced" deparsing arguments used by various [pitch functions][pitchFunctions].
#' The following "advanced" parsing arguments are available (read all the details about them further down):
#' 
#' + **Steps**
#'   + `step.labels`
#'   + `step.signed`
#' + **Species** (accidentals or qualities) 
#'   + `qualities`
#'   + `specifier.maximum`
#'   + *Accidentals*
#'     + `natural`, `flat`, `sharp`, `doubleflat`, `doublesharp`
#'   + *Qualities*
#'     + `perfect`, `major`, `minor`, `augment`, `diminish`
#'   + *Implicit vs Explicit Species*
#'     + `implicitSpecies`
#'     + `absoluteSpecies`
#'     + `memory`, `memoryWindows`
#' + **Octave**
#'   + `octave.integer`
#'   + `up`, `down`, `same`
#'   + `octave.offset`
#'   + `octave.round`
#'   + `octave.relative`, `octave.absolute`
#' + **String parsing**
#'   + `parts`
#'   + `sep`
#'   
#' These "advanced" arguments can be used directly in *any* [pitch function][pitchFunctions], or in a call to `tonalInterval` itself.
#' To use them with `tonalInterval` just specify them directly as arguments: for example, `tonalInterval(x, qualities = TRUE)`.
#' To use them with other [pitch functions][pitchFunctions], you can either...
#' 
#' + Put them in the `parseArgs` argument:
#'   + `kern(x, parseArgs = list(qualities = TRUE))`
#' + Or use the "syntactic sugar" short-hand form:
#'   + `kern(x, parse(qualities = TRUE))`
#'   
#' 
#' 
#' Each of the known Exclusive/Regex-dispatch combo (see the table above) is associated with default parsing arguments.
#' For example, if you set `Exclusive = 'kern'` or just use data that *look* like `**kern`, the `flat` argument is set to `"-"`,
#' However, if you had, for example, input data that looked like `**kern` **except** it used a different flat symbol, like `"_"`, you could modify the parser:
#' `kern("EE_", parse(flat = "_"))`
#' This overrides the default value for `**kern`---notice, that it *also* updates the `**kern` regular expression accordingly, so it works exactly the same as the standard [kern()] function.
#' 
#' ## Steps
#' 
#' Any representation of "tonal" pitch information will include a representation of *diatonic steps*.
#' You can control how the parser reads diatonic steps from a pitch representation using the `step.labels` argument.
#' The `step.labels` argument must be an atomic vector of unique values, with a length which is a positive multiple of seven.
#' Examples of `step.labels` arguments that are currently used by preset `humdrumR` pitch parsers include:
#' 
#' + `parse(step.labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))` --- (`**Tonh`)
#' + `parse(step.labels = c('d', 'r', 'm', 'f', 's', 'l', 't'))` --- (`**solfa`)
#' + `parse(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'))` --- (roman numerals)
#' 
#' If `step.labels` is `NULL`, steps are assumed to be represented by integers, including negative integers representing downward steps.
#' 
#' 
#' 
#' There is also a `step.signed` (`logical`, `length == 1`) argument: if `step.signed = TRUE`, lowercase versions of `step.labels` are interpreted as negative (downward) steps and
#' uppercase versions of `step.labels` are interpreted as positive (upwards) steps.
#' This option is used, for example, by the default [kern()] and [helmholtz()] parsers.
#' 
#' ## Species
#' 
#' In tonal pitch representations, "*specific*" versions of tonal pitches---the tonal "species"---are indicated by "specifiers": 
#' either *accidentals* or *qualities*.
#' The `qualities` (`logical`, `length == 1`) argument indicates whether accidentals are used (`qualities = FALSE`) or qualities (`qualities = TRUE`).
#' Some specifiers can be repeated any number of times, like "triple sharps" or "doubly augmented";
#' The `specifier.maximum` (`integer`, `length == 1`) argument sets a maximum limit on the number of specifiers to read.
#' For example, you could force all triple sharps (`"###"`) or double sharps (`"##"`) to parse as just `"#"`, by specifying `specifier.maximum = 1L`.
#' 
#' ### Accidentals 
#'   
#' If `qualities = FALSE` the parser will look for accidentals in the input, recognizing three types: naturals, flats, and sharps.
#' The `natural`, `flat`, and/or `sharp` (`character`, `length == 1`) arguments can be used to indicate how accidentals are represented in the input.
#' For example, if the input strings look like `c("Eflat", "C")`, you could set the argument `flat = "flat"`.
#' 
#' Examples of accidental argument combinations that are currently used by preset `humdrumR` pitch parsers include:
#' 
#' + `parse(flat = "b", sharp = "#")` -> `**pitch`
#' + `parse(flat = "-", sharp = "#")` -> `**kern`
#' + `parse(flat = "-", sharp = "+")` -> `**degree`
#' 
#' 
#' 
#' 
#' The `doubleflat`, and `doublesharp` (`character`, `length == 1`) arguments are `NULL` by default, but can be set if a special symbol is used 
#' to represent two sharps or flats. For example, you might have an input which represents double sharps as `"x"`.
#' You could call `kern("Fx", parse(doublesharp = "x"))` and the output will be `"F##"`.
#' 
#' ### Qualities
#' 
#' If `qualities = TRUE` the parser will look for qualities in the input, recognizing five types: perfect, minor, major, augmented, and diminished.
#' The `perfect`, `major`, `minor`, `diminish`, and/or `augment` (`character`, `length == 1`) arguments can be used to indicate how qualities
#'  are represented in the input.
#' (Note: we are talking about interval/degree qualities here, not chord qualities!)
#' For example, if the input strings look like `c("maj3", "p4")`, you could set the arguments `major = "maj"` and `perfect = "p"`.
#' Examples of quality argument combinations that are currently used by `humdrumR` [pitch functions][pitchFunctions] include:
#' 
#' + `parse(major = "M", minor = "m", perfect = "P", diminish = "d", augment = "A")`
#' + `parse(diminish = "o", augment = "+")`

#' ### Implicit vs Explicit Species
#' 
#' In some musical data, specifiers (e.g., accidentals or qualities) are not explicitly indicated; instead, 
#' you must infer the species of each pitch from the context---like the key signature!. 
#' 
#' #### From the Key
#' 
#' The most important argument here is `implicitSpecies` (`logical`, `length == 1`):
#' if `implicitSpecies = TRUE`, the species of input without an explicit species indicated is interpreted using the `Key`.
#' For example, 
#' 
#' + `kern('C', Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`
#'   + C is sharp in A major.
#' + `kern('C', Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as `"C"`
#'   + C is natural in A minor.
#' + `kern('C', Key = 'a-:', parse(implicitSpecies = TRUE))` is parsed as `"C-"`
#'   + C is flat in A-flat minor.
#'   
#' By default, if you input *already has* specifiers, they are interpreted absolutely---overriding the "implicit" `Key`---,
#' even if `implicitSpecies = TRUE`.
#' Thus, if we are in A major:
#' 
#' + `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`. 
#'   + The `"#"` is unnecessary. 
#' + `kern("Cn", Key = 'A:', parse(implicitSpecies = TRUE))` is parsed as `"C"`. 
#'   + The `"n"` overrides the `Key`.
#' + `kern("C#", Key = 'a:', parse(implicitSpecies = TRUE))` is parsed as `"C#"`.
#'   + The `"#"` overrides the `Key`.
#'   
#' However! You can also change this behavior by setting the `absoluteSpecies` (`logical`, `length == 1`) argument to `FALSE`.
#' If you do so, the specifiers in the input are interpreted "on top of" the key accidental:
#' 
#' + `kern("C#", Key = 'A:', parse(implicitSpecies = TRUE, absoluteSpecies = FALSE))` is parsed as `"C##"`. 
#'   + The `"#"` from the input is added to the `"#"` from the `Key`, resulting in double sharp!
#'   
#' This is an unusual behavior, for absolute pitch representations like `**kern`.
#' However, for use with scale or chord degrees, `absoluteSpecies = FALSE` might be appropriate.
#' For example, if we are reading a [figured bass](https://en.wikipedia.org/wiki/Figured_bass) in the key of E minor,
#' a `"b7"` figure above an E in the bass should be interpreted as a *double flat* (diminished) 7th (Db over E)!
#' If this is how your data is encoded, use `absoluteSpecies = FALSE`.
#' 
#' #### Memory
#' 
#' In some musical data, it is assume that a accidental on a note "stays in effect" on that scale step until the next bar,
#' or until a different accidental replaces it.
#' Fortunately, the `humdrumR` parser (`tonalInterval()`) also knows how to parse data encoded with "memory" this way.
#' If `memory = TRUE`, the accidental (or quality) of each input note is "remembered" from previous appearances of that scale step.
#' For example,
#' 
#' + `kern(c("D#", "E", "D", "E", "Dn", "C", "D"), parse(memory = TRUE))` 
#'   + is parsed as `c("D#", "E", "D#", "E", "D", "C", "D")`
#'
#' If we want the "memory" to only last when specific time windows (like bars), we can also specify a 
#' `memoryWindows` argument. `memoryWindows` must be an atomic vector which is the same length as the input (`x` argument).
#' Each unique value within the `memoryWindows` vector is treated as a "window" within which `memory` operates.
#' The most common use case would be to pass the `Bar` field from a `humdrumR` dataset to `memoryWindows`!
#'
#' The `memory` and `memoryWindows` argument work whatever values of `implicitSpecies` or `absoluteSpecies` are specified!
#' Though all the examples here use accidentals, these arguments all have the same effect if parsing qualities (`qualities = TRUE`).
#' 
#' ## Octave
#' 
#' The final piece of information encoded in most (but not) all pitch representations is an indication of the "compound pitch"---
#' incorporating octave information.
#' In `humdrumR` octaves are *always* defined in terms of scale steps: so two notes with the same scale degree/letter name will always be the same octave.
#' This mainly comes up with regards to Cb and B#: Cb4 is a semitone below ; B#3 is enharmonically the same as middle-**C**.
#' 
#' ### Integer Octaves 
#' 
#' The simplest way octave information can be encoded is as an integer value, as in [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch).
#' If you need to parse integer-encoded octaves, set the `octave.integer` (`logical`, `length == 1`) argument to `TRUE`.
#' By default, `humdrumR` considers the "central" octave (`octave == 0`) to be the octave of , or equivalently, a unison.
#' However, if a different octave is used as the central octave, you can specify the `octave.offset` (`integer`, `length == 1`) argument.
#' 
#' To illustrate, the default [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch) parser used the arguments:
#' 
#' + `kern('C5', parse(octave.integer = TRUE, octave.offset = 4)`
#'   + Returns `"cc"` (the octave above middle C).
#'   
#' ### Non-integer Octave Markers
#' 
#' If `octave.integer = FALSE`, the `humdrumR` parser instead looks for three possible symbols to indicate octave information.
#' These symbols are controlled using the `up`, `down`, and `same` (`character`, `length == 1`) arguments.
#' A `same` symbol, or no symbol, is interpreted as the "central" octave; repeating strings of the `up` or `down` symbols indicate
#' increasing positive (`up`) or negative (`down`) octaves.
#' For example, in `lilypond` notation, `,` represents lower octaves, and `'` (single apostrophe) represents upper octaves.
#' So the default [lilypond()] parser uses these arguments:
#' 
#' + `pitch(c("c", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",", octave.offset = 1))`
#'   + Returns `c("C2", "C3", "C4")`.
#'   
#' (Note that lilypond makes the octave *below*  the central octave, using `octave.offset = 1`.)
#' 
#' 
#' ### Octave "Rounding"
#' 
#' In some situations, pitch data might interpret the "boundaries" between octaves a little differently.
#' In most absolute pitch representations (e.g., [kern()], [pitch()]), the "boundary" between one octave and the next is 
#' between B (degree 7) and C (degree 1).
#' However, if for example, we are working with data representing intervals, we might think of an "octave" as spanning the range `-P4` (`G`) to `+P4` (`f`).
#' In this case, the "octave boundary" is *centered* around the unison (or ), rather than starting *at* middle-**C**/unison.
#' If our data was represented this way, we could use the `octave.round` argument; `octave.round` must be a rounding *function*, 
#' either [round, floor, ceiling, trunc][base::round], or [expand].
#' These functions indicate how we interpret simple pitches "rounding" to the nearest C/unison.
#' The default behavior for most pitch representations is `octave.round = floor`: each scale step is rounded downwards to the nearest C.
#' So B is associated with the C 7 steps below it.
#' If, on the other hand, `octave.round = round`, then scale-steps are "rounded" to the closest C, so B and A are associated with the closer C *above* them.
#' Indeed, `octave.round = round` gets us the `-P4` <-> `+P4` behavior we mentioned earlier!
#'
#' When working parsing [intervals][interval()], the `octave.round` option allows you to control how the "simple part" (less than an octave) of a compound interval is represented.
#' For example, we might think of a ascending major 12th as being an ascending octave *plus* a ascending perfect 5th: ** +P8 + P5**.
#' **Or** we could encode that same interval as *two* ascending octaves *minus* a perfect fourth: **+ P15 - P4**.
#' The following table illustrates how different `octave.round` arguments "partition" compound intervals into simple parts and octaves:
#'
#' ```{r, echo = FALSE, comment=NA, result = 'asis'}
#' 
#' 
#' tint <- tonalInterval(c('FF', 'GG','C', 'F','G', 'c', 'f','g','cc','ff','gg','ccc', 'fff', 'ggg'))
#' 
#' tintparts <- lapply(c(round, floor, ceiling, trunc, expand), \(f) tintPartition_compound(tint, octave.round = f))
#' 
#' intervals <- sapply(tintparts, \(tp) paste0(interval(tp$Octave), ' + ', interval(tp$Simple)))
#' intervals <- gsub('\\+ \\+', '+ ', intervals)
#' intervals <- gsub('\\+ -', '- ', intervals)
#' intervals <- gsub('^P', '+P', intervals)
#' intervals <- gsub('([81]) ', '\\1  ', intervals )
#' 
#' colnames(intervals) <- c('round', 'floor', 'ceiling', 'trunc', 'expand')
#' rownames(intervals) <- format(paste0(interval(tint), ': '), justify = 'right')
#' 
#' knitr::kable(intervals, "pipe")
#' 
#' ```
#' 
#' Notice that, if `octave.floor` is being used, all simple intervals are represented as ascending.
#' 
#' 
#' 
#' When parsing ["absolute" pitch][pitch()] representations, the `octave.round` option allows you to control which octave notes are associated with.
#' The following table illustrates:
#' 
#' ```{r, echo = FALSE, comment=NA, result = 'asis'}
#' 
#' 
#' tint <-  tonalInterval(c('FF', 'GG','C', 'F','G', 'c', 'f','g','cc','ff','gg','ccc', 'fff', 'ggg'))
#' 
#' pitches <- do.call('cbind', lapply(c(round, floor, ceiling, trunc, expand), \(f) pitch(tint, octave.round = f)))
#' 
#' 
#'
#' 
#' colnames(pitches) <- c('round', 'floor', 'ceiling', 'trunc', 'expand')
#' rownames(pitches) <- format(paste0(kern(tint), ': '), justify = 'right')
#' 
#' knitr::kable(pitches, 'pipe')
#' 
#' ```
#' 
#' ### Absolute or Relative (contour) Octave
#' 
#' In some notation encoding schemes, the "octave" of each note is interpreted *relative* the previous note, rather than any absolute reference.
#' The most prominent system is Lilypond's [relative octave entry](https://lilypond.org/doc/v2.22/Documentation/notation/writing-pitches#relative-octave-entry) style.
#' This style is often used in combination with scale degree representations---as in the [RS200](http://rockcorpus.midside.com/melodic_transcriptions.html) corpus.
#' For example, a data set might say `Do Re Mi vSo La Ti Do`, with the `"v"` indicating a jump down to `So`. 

#' To activate relative-octave parsing, set `octave.relative = TRUE`---alternatively, you can use `octave.absolute = FALSE`, which is equivalent.
#' 
#' In a relative-octave data, we assume that octave indications indicate a shift relative to the previous note.
#' This would usually be used in combination with octave markers like `"^"` (up) or `"v"` (down).
#' Different combinations of `octave.round` allow us to parse different behaviors:
#' 
#' + If `octave.round = round`, a `same` marker (or no marker) indicates that the note is the pitch *closest* to the previous pitch.
#'   Octave markers indicate alterations to this assumption.
#'   As always, this is based on scale steps, not semitones!
#'   Any fourth is "closer" than any fifth, regardless of their quality: So *C F#* is ascending and *C Gb* is descending!
#'   A ascending diminished 5th would be written `C ^Gb`---with `up = ^`. 
#' + If `octave.round = floor`, a `same` marker (or no marker) indicates that the note is in the octave above the previous pitch.
#'   Octave markers indicate alterations to this assumption.
#'   With this setting, going from *C* down to *B* always requires a `down` mark.
#'
#' ## String Parsing
#' 
#' In addition to the three types of *musical* parsing considerations reviewed above (steps, species, and octaves), there are also some general 
#' string-parsing issues that we can consider/control.
#' 
#' ### Parts and Order
#' 
#' So far (above) we've discussed various ways that tonal pitch information (step, species, and octave) can be encoded, and how
#' the `humdrumR` parser can be modified to handle different options.
#' However, there are two general parsing issues/options to consider: what information is encoded, and in *what order*?
#' The `parts` argument can be specifyied to indicate this.
#' The `parts` argument must be a `character` vector of length 1--3.
#' The characters in the must [partial match][base::pmatch] either `"step"`, `"species"`, or `"octave"`.
#' The presense of any of these strings in the `parts` vector indicate that that information should be parsed.
#' The *order* of the strings indicates what order the pieces of pitch information are encoded in input strings.
#' 
#' To illustrate, imagine that we had input data which was identical to a standard interval representation---e.g., `M2` and `P5`---except the 
#' quality appears *after* the step---e.g., `2M` and `5P`.
#' We could call `interval(c("2M", "5P"), parse(parts = c("step", "species")))` and sure enough we'd get the correct parse!
#' 
#' 
#' 
#' One final string-parsing argument is `sep`, which indicates if there is a character string separating the pitch information components:
#' The most common case would be a comma or space.
#' For example, we could use a parse command like this: `kern("E flat 5", parse(flat = "flat", sep = " "))`.
#'     
#' # Atonal Parsing (`numeric` inputs) 
#' 
#' The `humdrumR` pitch parser (`tonalInterval()`) will interpret numeric inputs as atonal pitch information.
#' By default, numbers are interpreted as semitones.
#' However, parses for [midi()], [cents()],  and [frequencies][freq()] are also defined.
#' Dispatch to these different parsers is controlled by the `Exclusive` argument.
#' 
#' | Representation                                                                     | Exclusive                 | Example          |
#' | ---------------------------------------------------------------------------------- | ------------------------: | ---------------: |
#' | Semitones                                                                          | **semits (or `NULL`)      | `3` -> `e-`      |
#' | MIDI                                                                               | **midi                    | `63` -> `e-`     |
#' | Cents                                                                              | **cents                   | `300` -> `e-`    |
#' | Frequency (Hz)
#' 
#' ## Enharmonic Interpretation 
#' 
#' When converting from an atonal representation to a tonal one, we must decide how to interpret the tonality 
#' of the input---specifically, which [enharmonic spelling](https://en.wikipedia.org/wiki/Enharmonic) of notes to use.
#' The  `humdrumR` numeric parser interprets atonal pitches in a "enharmonic window" of 12 steps on the line-of-fifths.
#' The position of this window is set with the `enharmonic.center` (`integer`, `length == 1`) argument.
#' By default, `enharmonic.center = 0`, which creates a window from a `-5` (*b2*) to `+6`) (*#4*).
#' If you prefer *#1* instead of *b2*, set `enharmonic.center = 1`.
#' For all flats, set `enharmonic.center = -1`.
#' For all sharps, set `enharmonic.center = 4`.
#' 
#' | `enharmonic.center`      | 0   |   1 |   2 |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 |  11 |
#' |-------------------------:|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
#' | `-2`                     | 1   | b2  | 2   | b3  | 3   | 4   | b5  | 5   | b6  | 6   | b7  | b1  |
#' | `-1`                     | 1   | b2  | 2   | b3  | 3   | 4   | b5  | 5   | b6  | 6   | b7  | 7   |
#' | `0`                      | 1   | b2  | 2   | b3  | 3   | 4   | #4  | 5   | b6  | 6   | b7  | 7   |
#' | `1`                      | 1   | #1  | 2   | b3  | 3   | 4   | #4  | 5   | b6  | 6   | b7  | 7   |
#' | `2`                      | 1   | #1  | 2   | b3  | 3   | 4   | #4  | 5   | #5  | 6   | b7  | 7   |
#' | `3`                      | 1   | #1  | 2   | #2  | 3   | 4   | #4  | 5   | #5  | 6   | b7  | 7   |
#' | `4`                      | 1   | #1  | 2   | #2  | 3   | 4   | #4  | 5   | #5  | 6   | #6  | 7   |
#' 
#' The `enharmonic.center` argument will work the same when translating to any pitch representation, like [kern()].
#' However, we present the table above in terms of scale degrees because the *atonal -> enharmonic* calculation
#' is centered on a key.
#' So, if `Key` argument is specified, the "enharmonic window" is centered around that key.
#' So if you are translating to `kern` and the `Key = F#:`, the output will range from `Gn` to `B#`.
#' If you don't want this, set `Key = NULL`.
#' 
#' ### Melodic Interpretation of Chromatic Notes
#' 
#' It is very common for chromatic notes in melodic passages to be labeled based on their melodic contour:
#' i.e., ascending chromatic notes labeled sharp and descending chromatic notes labeled flat.
#' This behavior can be engaged by setting the `accidental.melodic` (`logical`, `length == 1`) argument.
#' When `accidental.melodic = TRUE`, the input is first centered in the enharmonic window (above), but then
#' any places where a chromatic alteration proceeds upwards to a non-chromatic note will be altered (if necessary) to a
#' sharp, and vice verse for a descending notes and flats.
#' For example, while `kern(0:2)` returns `c("c", "d-", "d")`, `kern(0:2, parse(accidental.melodic = TRUE))` returns `c("c", "c#", "d")`.
#' 
#' @param str ***The input vector.***
#' 
#' Must be either `character` or `numeric`.
#' 
#' @param Key ***The diatonic key used to interpret the pitch information.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be a `diatonicSet` or something coercable to `diatonicSet`; must be either length `1` or `length(x)`
#' 
#' For example, use of `implicitSpecies` (see advanced parsing section) is dependent on the `Key`.
#' The output `tonalInterval` is output *within the key*: thus, `tonalInterval('C#', Key = "A:")`
#' returns the tint representing a **Major 3rd**, because *C#* is the major third of A major.
#' 
#' @param Exclusive ***An exclusive interpretation to guide parsing of the input.***
#' 
#' Must be `character`; must be either length `1` or `length(x)`.
#'      
#' @examples 
#' 
#' tonalInterval('II#', step.labels =c('I', 'II', 'III','IV','V','VI','VII'))
#' 
#' kern('E x 5', parse(doublesharp = 'x', sep = ' '))
#' 
#' @returns 
#' 
#' `tonalInterval()` returns a [tonalInterval][tonalIntervalS4] object of the same
#' length and dimensions as `x`.
#' `NULL` inputs (`x` argument) return a `NULL` output.
#' `NA` values in the input `x` are propagated to the output.
#' 
#' @seealso All `humdrumR` [pitch functions][pitchFunctions] make use of the parsing functionality.
#' @name pitchParsing
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
  steps <- tint2step(simpletint, 0:6) # + tint2step(roottint, 0:6)
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

atonal2tint <- function(tint, accidental.melodic = FALSE, keyed = TRUE, Key = NULL, 
                        enharmonic.center = 0L, ...) {
  
  Key <- diatonicSet(Key %||% dset(0L, 0L))
  

  if (keyed) tint <- tint - Key   
  
  tint <- tintPartition_harmonic(tint, 
                                 enharmonic.minimum = enharmonic.center - 5L,
                                 enharmonic.maximum = enharmonic.center + 6L)$Enharmonic
  
  lof <- LO5th(tint)
  
  if (accidental.melodic) {
    chromatic <- lof > 5 | lof < -1
    ints <- c(diff(tint), tint(0, 0)) # tint(0,0) is just padding
    
    isA1 <- ints == tint(-11, 7)
    isD1 <- ints == tint(11, -7)
    
    tint[which(chromatic & isA1)] <- tint[which(chromatic & isA1)] + pythagorean.comma
    tint[which(chromatic & isD1)] <- tint[which(chromatic & isD1)] - pythagorean.comma
    
  } 
  
  tint
}

semits2tint <- function(x, ...) {
          wholen <- as.integer(c(x))
          
          pitchclass <- wholen %% 12L
          
          LO5ths <- .ifelse(pitchclass %% 2L == 0L, pitchclass, pitchclass - 6L)
          octaves <- as.integer((wholen - (LO5ths * 19)) %/% 12)
          tints <- tint(octaves, LO5ths)
          
          ##
          tints <- atonal2tint(tints, ...)
          
          tints
}

cents2tint <- function(x, ...) {
  roundedN <- round(x / 100)
  tint <- semits2tint(roundedN, ...)
  
  tint@Cent <- x - (roundedN * 100)
  
  tint
  
  
}


midi2tint <- function(x, ...) {
  semits2tint(x - 60L, ...)
}


#### Frequency ####

fraction2tint <- function(x, tonalHarmonic = 3) rational2tint(as.rational(x), tonalHarmonic) 


rational2tint <- function(x, tonalHarmonic = 3, ...) {
  ratio2tint(as.double(x), ...)
}

ratio2tint <- function(x, tonalHarmonic = 2^(19/12), centMargin = 25,  ...) {
  if (x <= 0) .stop('Numbers can only be interpreted as frequency ratios if they are non-zero and positive.')
  
  
  possibleLO5ths <- -12:12
  
  octaves <- log(outer(x, tonalHarmonic^possibleLO5ths, `/`), 2L)
  
  bestLO5th <- possibleLO5ths[apply(abs(octaves - round(octaves)), 1L,
                                    \(row) {
                                      hits <- which(row == min(row))
                                      hits[which.min(abs(possibleLO5ths[hits]))]
                                      })]
  
  x_removedLO5th <- x / 3^bestLO5th
  bestOctave <- round(log(x_removedLO5th, 2))

  
  approx <- 2^bestOctave * tonalHarmonic^bestLO5th
  
  error <- approx / x
  cents <- round(log(error, 2^(1/12)) * 100, 5)
  
  bestOctave[abs(cents) > centMargin] <- NA_integer_
  
  atonal2tint(tint(bestOctave, bestLO5th, cent = -cents), ...)
  
  
}



freq2tint <- function(x, frequency.reference = 440L, frequency.reference.note = tint(-4, 3), ...) {
  
  ratio <- x / frequency.reference
  
  tint <- ratio2tint(ratio, ...)
  
  ## add offset (defaults to A above )
  frequency.reference.note <- tonalInterval(frequency.reference.note)
  
  tint + frequency.reference.note
  
  
  
}

### Tonal ####

lof2tint <- function(x) tint(, x)

step2tint <- function(x, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), step.signed = FALSE, ...) {
  
  if (length(step.labels) %% 7L > 0) .stop('When parsing tonal pitches, the number of "step.labels" must be a multiple of 7.')
  
  
  
  step <- if (is.null(step.labels)) as.integer(x) else match(toupper(x), toupper(step.labels), nomatch = NA_integer_) 
  
  
  # generic
  genericstep <- genericStep(step)
  tint <- tint( , ifelse(is.na(genericstep), NA_integer_, c(0L, 2L, 4L, -1L, 1L, 3L, 5L)[genericstep]))
  
  # specific
  octave <- (step - 1L) %/% 7L
  
  if (step.signed) octave[x == toupper(x)] <- (octave[x == toupper(x)] + 1L) * -1
  
  tint <- tint + tint(octave, 0L)
  
    
  tint
}




updownN <- function(str, up = '#', down = 'b')  stringi::stri_count_fixed(str, up) - stringi::stri_count_fixed(str, down)





specifier2tint <- function(x, step = NULL, Key = NULL, 
                           qualities = TRUE,
                           memory = FALSE, memoryWindows = NULL,
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
  if (!false(doublesharp)) x <- gsub(doublesharp, strrep(sharp, 2), x)
  if (!false(doubleflat )) x <- gsub(doubleflat,  strrep(flat , 2), x)
  
  # incorporate memory?
  if (memory) {
    memoryWindows <- if (truthy(memoryWindows) && length(memoryWindows) == length(x)) {
      match_size(x,
                 step = step, 
                 memory = memory, 
                 toEnv = TRUE)
      paste0(step, ':', memoryWindows)
    } else {
      step
    }
  
    x <- tapply_inplace(x, memoryWindows, ditto.default, null = \(x) x == '', initial = '')
   
  } 
  
  # calculate lof
  
  natural <- stringi::stri_detect_fixed(x, natural)
  lof <- (if (qualities) {
    lof <- updownN(x, up = augment, down = diminish) -
      (substr(x, 1L, 1L) == diminish & step >= 3L)  - # 3rd, 6th, and 7th diminish differently
      (x == minor)
    
  } else {
    updownN(x, up = sharp, down = flat)
  } ) 

  lof <- pmaxmin(lof, -specifier.maximum, specifier.maximum) * 7L
  
  # incorporate key?
  if (!is.null(Key) && implicitSpecies) {
    keyalt <- ifelse(natural, 0L, -(step - (step %% Key)) )
    if (absoluteSpecies) {
      lof[x == ''] <- keyalt[x == '']
    } else {
      lof <- lof + keyalt
    }
    
  }
  # names(n) <- names(accidental.labels)[match(str, accidental.labels)]
  # names(n)[is.na(names(n))] <- ""
  
  tint( , lof)
  
}

CKey <- function(dset) if (!is.null(dset)) dset - getRootTint(dset) 


tonalChroma2tint <- function(x,  
                             parts = c("step", "species", "octave"), 
                             qualities = FALSE, 
                             parse.exhaust = TRUE, 
                             keyed = FALSE, Key = NULL, 
                             sep = NULL,
                             ...) {
 
  
 parts <- matched(parts, c("sign", "step", "species", "octave"))
 
 if (!is.null(Key)) Key <- diatonicSet(Key)
 
 ############# parse string
 # regular expressions for each part
 REs <-  makeRE.tonalChroma(parts, collapse = FALSE, qualities = qualities, ...)
 REparse(x, REs, parse.exhaust = parse.exhaust, parse.strict = TRUE, sep = sep, toEnv = TRUE) ## save to environment!
 
 ## simple part
 step    <- if ("step" %in% parts)    step2tint(step, ...) 
 species <- if ("species" %in% parts) specifier2tint(species, qualities = qualities, 
                                                     Key = if (keyed) Key else CKey(Key), step = step, ...) 
 
 simpletint <- (step %||% tint( , 0L)) + (species %||%  tint( , 0L)) 
 
 # compound part
 tint <- if ("octave" %in% parts) octave2tint(octave, simpletint = simpletint, root = getRootTint(Key %||% dset(0L, 0L)), ...) + simpletint else simpletint
 
 if ("sign" %in% parts) tint[sign == '-'] <- tint[sign == '-'] * -1L
 
 if (keyed && !is.null(Key)) {
  Key <- rep(Key, length.out = length(tint))
  tint[!is.na(Key)] <- tint[!is.na(Key)] - Key[!is.na(Key)]
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


tonh2tint <- function(x, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                      flat = 'es',   ...) {
  
  seven <- step.labels[7L]
  
  x <- gsub('([AE])s', '\\1es', x)
  x <- gsub('S', 'Ees', x)
  
  x <- gsub(paste0('H', flat, flat), paste0(seven, flat, flat), x) # Heses -> Beses
  x <- gsub(paste0(seven, '(-?[0-9])'), paste0(seven, flat, '\\1'), x)
  x <- gsub('H', seven, x)
   
  tC2t <- partialApply(tonalChroma2tint, 
                       parts = c('step', 'species', 'octave'),
                       step.sign = FALSE,
                       octave.integer = TRUE, octave.offset = 4L,
                       sharp = 'is')
  
  tC2t(x, flat = flat,  step.labels = step.labels, ...)
  
  
}

helmholtz2tint <- partialApply(tonalChroma2tint,
                               step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                               step.signed = TRUE,
                               up = "'", down = ",", flat = 'b', octave.integer = FALSE, octave.offset = 1L)

kern2tint <- function(x, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),  ...) {
  # letter <- stringr::str_extract(str, '[A-Ga-g]')
  # str_ <- stringr::str_replace(str, '([A-Ga-g])\\1*', toupper(letter)) # simple part
  step.labels <- unlist(lapply(1:50, strrep, x = step.labels))
  
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c("step", "species"), 
                       keyed = TRUE,  
                       qualities = FALSE,
                       step.signed = TRUE)
  tint <- tC2t(x, step.labels = step.labels, ...)
  
  
}

interval2tint <- function(x, ...) {
  
  tC2t <- partialApply(tonalChroma2tint,
                              parts = c('sign', 'species', "step"), 
                              qualities = TRUE, step.labels = NULL)
  tC2t(x, ...)
  
}


pc2tint <- function(x, ten = 'A', eleven = 'B', ...) {
  x <- gsub(ten, '10', x)
  x <- gsub(eleven, '11', x)
  
  x <- as.integer(x)
  
  semits2tint(x, ...)
  
}


degree2tint <- partialApply(tonalChroma2tint, parts = c("step", "species", "octave"), 
                           qualities = FALSE, 
                           keyed = FALSE, sep = c("", "/"),
                           step.labels = c('1', '2', '3', '4', '5', '6', '7'),
                           octave.integer = TRUE, octave.offset = 4L,
                           flat = '-', sharp = "+")
  
deg2tint <- partialApply(tonalChroma2tint, parts = c("octave", "step", "species"), 
                         qualities = FALSE, 
                         keyed = FALSE,
                         step.labels = c('1', '2', '3', '4', '5', '6', '7'),
                         octave.integer = FALSE, octave.relative = TRUE, octave.round = expand,
                         up = '^', down = 'v',
                         flat = '-', sharp = "+")

solfa2tint <- function(x, ..., flat = '-', sharp = '#') {
  syl <- stringr::str_extract(x, '[fdsrlmt][aeio]')
  
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
  
  str_ <- stringr::str_replace(x, alt, sylalt)
 
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c("octave", "step", "species"),
                       qualities = FALSE,
                       keyed = FALSE,
                       octave.integer = FALSE, octave.relative = FALSE, octave.round = floor)
  
  tC2t(str_, step.labels = rownames(alt.mat), ..., flat = flat)
  
}

solfg2tint <- partialApply(tonalChroma2tint, flat = '~b', doubleflat = '~bb', sharp = '~d', doublesharp = '~dd', natural = '~n',
                           parts = c('step', 'species', 'octave'), keyed = TRUE,
                           step.labels = c('do', 're', 'mi', 'fa', 'so', 'la', 'ti'), 
                           octave.integer = TRUE, octave.offset = 4L)
                           

bhatk2tint <- function(x, ...) {
  
  tC2t <- partialApply(tonalChroma2tint,
                       parts = c('step', 'octave'),
                       step.labels = c('S', 'R', 'G', 'M', 'P', 'D', 'N'),
                       octave.integer = FALSE,
                       up = "'", down = ',')
  
  tint <- tC2t(toupper(x))
  
  
  perfects <- abs(LO5th(tint)) <= 1L
  altered <- x == tolower(x)
  
  tint[altered] <- tint[altered] + tint( , ifelse(perfects[altered], 7L, -7L))
  
  tint
  
}



## Pitch Parsing Dispatch ######################################


### Parse 2tint generic and methods ####

#' @rdname pitchParsing
#' @export
tonalInterval <- function(...) UseMethod('tonalInterval')

#' @rdname pitchParsing
#' @export
tonalInterval.tonalInterval <- function(x, ...) x

#' @rdname pitchParsing
#' @export
tonalInterval.logical <- function(x, ...) vectorNA(length(x), 'tonalInterval')

#' @rdname pitchParsing
#' @export
tonalInterval.NULL <- function(x, ...) tint(c(), c())

#### Numbers ####


#' @rdname pitchParsing
#' @export
tonalInterval.numeric  <- makeHumdrumDispatcher(list('semits', NA, semits2tint),
                                                list('freq',  NA, freq2tint),
                                                list('cents', NA, cents2tint),
                                                list('midi',  NA, midi2tint),
                                                list('lof',  NA, lof2tint),
                                                funcName = 'tonalInterval.numeric',
                                                outputClass ='tonalInterval')


#### Characters ####

#' @rdname pitchParsing
#' @export
tonalInterval.character <- makeHumdrumDispatcher(list('kern',                   makeRE.kern,        kern2tint),
                                                 list('pitch',                  makeRE.sciPitch,    pitch2tint),
                                                 list('lilypond' ,              makeRE.lilypond,    lilypond2tint),
                                                 list('helmholtz' ,             makeRE.helmholtz,   helmholtz2tint),
                                                 list(c('hint', 'mint', 'int', 'interval'), makeRE.interval,    interval2tint),
                                                 list('degree',                 makeRE.degree,      degree2tint),
                                                 list('deg',                    makeRE.deg,         deg2tint),
                                                 list('solfa',                  makeRE.solfa,       solfa2tint),
                                                 list('solfg',                  makeRE.solfg,       solfg2tint),
                                                 list('bhatk',                  makeRE.bhatk,       bhatk2tint),
                                                 list('Tonh',                   makeRE.tonh,        tonh2tint),
                                                 list(nore = 'pc',                     makeRE.pc,          pc2tint),
                                                 funcName = 'tonalInterval.character',
                                                 outputClass = 'tonalInterval')


#' @rdname pitchParsing
#' @export
tonalInterval.factor <- function(x, Exclusive = NULL, ...) {
  levels <- levels(x)
  
  tints <- tonalInterval.character(levels, Exclusive = Exclusive, ...)
  
  c(tint(NA), tints)[ifelse(is.na(x), 1L, 1L + as.integer(x))]
}

#' @rdname pitchParsing
#' @export
tonalInterval.token <- function(x, Exclusive = NULL, ...) {
 tonalInterval.character(as.character(x@.Data), Exclusive = Exclusive %||% getExclusive(x), ...)
}


#### setAs tonal interval ####

setAs('integer', 'tonalInterval', \(from) semits2tint(from))
setAs('numeric', 'tonalInterval', \(from) semits2tint(as.integer(from)))
setAs('character', 'tonalInterval', \(from) {
  output <- tint(rep(NA, length(from)))
  if (any(!is.na(from))) output[!is.na(from)] <- tonalInterval.character(from[!is.na(from)])
  output
} )

setAs('matrix', 'tonalInterval', function(from) tonalInterval(c(from)) %<-matchdim% from)
setAs('logical', 'tonalInterval', function(from) tint(rep(NA, length(from))) %<-matchdim% from)

setMethod('as.rational',  'tonalInterval', tint2rational)
setMethod('as.double',    'tonalInterval', tint2double)
setMethod('as.integer',   'tonalInterval', tint2semits)
setMethod('as.character', 'tonalInterval', tint2interval)
setMethod('as.numeric',   'tonalInterval', tint2double)


###################################################################### ###
# Making pitch factor levels #############################################
###################################################################### ###



#' Make a pitch gamut
#' 
#' This function generates a [gamut](https://en.wikipedia.org/wiki/Gamut):
#' an ordered range of notes used in music.
#' It is used to generate [factor()] levels for [pitch functions][pitchFunctions].
#' The output format of the gamut is controlled by the `deparser` argument (a function) and any `deparseArgs`
#' that are passed to it, defaulting to [kern()].
#' 
#' @details 
#' 
#' A gamut is produced based on two criteria: what range on the line-of-fifths to include,
#' and what range of octaves to include?
#' These ranges can be controlled directly with the `min.octave`, `max.octave`, `min.lof`, and `max.lof` arguments,
#' with the corresponding ranges being `min.octave:max.octave` and `min.log:max.log` respectively.
#' If any of these arguments are missing (by default), the ranges default values based on the `simple/compound` and `generic/specific` arguments.
#' These default ranges are:
#' 
#' + **Line-of-fifths**: 
#'  + If `generic = TRUE`, `-1:5` (F to B)
#'  + If `generic = FALSE`, `-4:7` (Ab to C#)
#' + **Octaves**:
#'  + If `simple = TRUE`, `0:0` (one octave only).
#'  + If `simple = FALSE`, `-1:1`.
#'  
#' If a `tints` argument is provide (not `NULL`), `tints` is [parsed][tonalInterval()]
#' as pitch data and the line-of-fifth and octave ranges of this data is used to set the gamut ranges.
#' This assures that all values that appear in `tint` always make it into the gamut.
#' However, if `min.octave`, `max.octave`, `min.lof`, or `max.lof` are present, they override the ranges of `tint`;
#' this can be used to exclude values, even if they appear in `tint`.
#' 
#' 
#' @param generic ***Should the gamut include only generic intervals?
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#'
#' 
#' @param simple ***Should the gamut be constrained to one octave?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' 
#' @param reference ***An optional reference vector to base the gamut on.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be either `NULL`, or a [tonalInterval()], `integer`, or `character` vector.
#'
#' This vector is [parsed as pitch][pitchParsing]. 
#' If it can't be parsed as pitch, it will be ignored.
#' 
#' @param deparser ***A [pitch function][pitchFunctions] to format the output.***
#' 
#' Defaults to [kern()].
#' 
#' Must be a [pitch function][pitchFunctions].
#' 
#' @export
gamut <- function(generic = FALSE, simple = FALSE,
                  min.octave, max.octave, min.lof, max.lof,
                  reference = NULL,
                  deparser = tint2kern, deparseArgs = list(), ...) {
  
  checks(generic, xTF)
  checks(simple, xTF)
  checks(deparser, xinherits('function'))
  
  generic <- deparseArgs$generic %||% generic
  simple <- deparseArgs$simple %||% simple
  
  reference <- do.call('tonalInterval', c(list(reference), deparseArgs))
  
  
  deparseArgs <- local({
    deparseFormals <- formals(deparser)
    deparseFormals[names(deparseArgs)] <- deparseArgs
    deparseFormals$x <- deparseFormals$... <- NULL
    deparseFormals
    # lapply(deparseFormals, eval, envir = rlang::new_environment(deparseFormals, environment(deparser)))
  })
  
  # empty missing -> use default
  # empty notmissing -> use given
  # notempty missing -> use min(default, notempty)
  # notempty notmissing -> use given
  #
  if (length(reference)) {
    deparseOctave <- deparseArgs
    deparseOctave$octave.integer <- TRUE
    offset <- deparseArgs$octave.offset %||% 0L
    octaves <- do.call(tint2octave, c(list(reference), deparseOctave)) - offset
    
    
    lofs <- LO5th(reference)
    if (generic) lofs <- genericFifth(lofs)
    
  } else {
    octaves <- lofs <- NULL
  }
  
  if (missing(min.octave)) min.octave <- min(if (simple) 0L else -1L, octaves, na.rm = TRUE)
  if (missing(min.octave)) min.octave <- min(if (simple) 0L else -1L, octaves, na.rm = TRUE)
  if (missing(max.octave)) max.octave <- max(if (simple) 0L else 1L, octaves, na.rm = TRUE)
  if (missing(min.lof))    min.lof    <- min(if (generic) -1L else -4L, lofs, na.rm = TRUE)
  if (missing(max.lof))    max.lof    <- max(if (generic) 5L else 7L, lofs, na.rm = TRUE)
  


  
  
  if (!simple) {
    if (deparseArgs$octave.relative %||% FALSE) { 
      octave.round <- deparseArgs$octave.round %||% floor
      gamut <- .unlist(lapply(min.lof:max.lof,
             \(lof) {
               tint <- tint( , lof)
               tint + tint(unique(sort(c(octaves[lofs == lof], -1L:1L))), 0L)
             }))
      gamut <- gamut[order(tint2step(gamut, step.labels = NULL), LO5th(gamut), do.call(tint2octave, c(list(gamut))))]
    } else {
       gamut <- tint( , min.lof:max.lof)
       gamut <- do.call('c',lapply(min.octave:max.octave, \(o) gamut + tint(o, 0L)))
       gamut <- gamut[order(do.call(tint2octave, c(list(gamut))) * 7L + tint2step(gamut, step.labels = NULL))]
      
       # if (length(x)) gamut <- gamut[gamut >= min(x, na.rm = TRUE) & gamut <= max(x, na.rm = TRUE)]
    }
    
  } else {
    
       gamut <- tint( , min.lof:max.lof)
       gamut <- gamut[order(tint2step(gamut, step.labels = NULL), min.lof:max.lof)]
  }
  
  # x <- tint(, x@Fifth) + tint(octaves,0)
  
  deparseArgs$octave.relative <- FALSE
  deparseArgs$octave.round <- quote(floor)
  deparseArgs$Key <- NULL
  unique(if (!is.null(deparser)) do.call(deparser, c(list(gamut), deparseArgs)) else gamut)
}


set.gamut <- function(token) {
  deparseArgs <- token@Attributes$deparseArgs
  deparseArgs$Key <- NULL
  
  levels <- do.call(gamut, c(list(reference = token@.Data, 
                                  deparseArgs = deparseArgs, 
                                  deparser = token@Attributes$deparser), 
                             token@Attributes$gamutArgs))
  
  factor(token@.Data, levels = levels)
}


###################################################################### ### 
# Translating Pitch Representations (x2y) ################################
###################################################################### ### 

## Pitch function documentation ####


pitchFunctions <- list(Tonal = list(Absolute = c('kern', 'pitch', 'lilypond', 'helmholtz', 'tonh' = 'German-style notation'),
                                    Relative = c('interval', 
                                                 'solfa' = 'relative-do solfege', 
                                                 'solfg' = 'French-style fixed-do solfege', 
                                                 'degree' = 'absolute scale degrees', 'deg' = 'melodic scale degrees', 
                                                 'bhatk' = 'hindustani swara'),
                                    Partial  = c('step', 'accidental', 'quality', 'octave')),
                       Atonal = list(Musical = c('semits', 'midi', 'cents', 'pc' = 'pitch classes'),
                                     Physical = c('freq')))


#' Translate between pitch representations.
#' 
#' These functions are used to extract and translate between different representations
#' of pitch information.
#' The functions can also do things like transposing and simplifying pitches.
#' 
#' @details 
#' 
#' The full list of pitch functions is:
#' 
#' ```{r echo = FALSE, results = 'asis'}
#' 
#' pfs <- rapply(pitchFunctions, 
#'                 \(func) paste0('    + [', 
#'                                 ifelse(.names(func) == '', func, paste0(.names(func))), 
#'                                 '()]', ifelse(.names(func) == '', '', paste0(' (', func, ')'))), how = 'list')
#' 
#' pfs <- lapply(pfs, \(top) Map(\(name, pf) paste(c(paste0('  + *', name, ' pitch representations*'), pf), collapse = '\n'), names(top), top))
#' 
#' pfs <- Map(\(name, l) paste(c(paste0('+ **', name, ' pitch representations**'), unlist(l)), collapse ='\n'), names(pfs), pfs)
#' cat(unlist(pfs), sep = '\n')
#' 
#' 
#' ```
#' 
#' 
#' These pitch functions all work in similar ways, with similar arguments and functionality.
#' Each function takes an input pitch representation (which can be anything) and outputs
#' *its* own pitch representation. 
#' For example, [kern()] takes any input representation and outputs `**kern` (pitch) data.
#' Underneath the hood, the full processing of each function looks like this:
#' 
#' + **Input** representation (e.g., `**pitch` or `**semits`) `|>` 
#'   + *Parsing* (done by [tonalInterval()]) `|>`
#'     + **Intermediate** ([tonalInterval][tonalIntervalS4]) representation `|>`
#'     + **Transformation** (e.g., [transpose()]) `|>`
#'   + *Deparsing* `|>`
#' +  **Output** representation (e.g. `**kern` or `**solfa`) 
#' 
#' 
#' 
#' To read the details of the parsing step, read [this][pitchParsing].
#' To read the details of the "deparsing" step, read [this][pitchDeparsing].
#' To read more details about each specific function, click on the links in the list above, 
#' or type `?func` in the R command line: for example, `?kern`.
#'
#' The "partial" pitch functions [octave()], [step()], [accidental()], and [quality()] are so-called
#' because they each only return one part/aspect of pitch information, and only that part.
#' For example, `accidental()` only returns he accidentals (if any) of pitches.
#'     
#' @param x ***Input data to parse as pitch information.***
#' 
#' The `x` argument can be any ([atomic][base::vector]) vector, or a [tonalInterval][tonalIntervalS4], or `NULL`.
#' 
#' @param ... ***Arguments passed to the [pitch deparser][pitchDeparsing].***
#' 
#' There are also two hidden (advanced) arguments you can specify: `memoize` and `deparse` (see the details below).
#' 
#' @param generic ***Should "specific" pitch information (accidentals and qualites) be discarded?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param simple ***Should "compound" pitch information (octave/contour) be discarded?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param Key ***The input `Key` used by the parser, deparser, and transposer.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be a `diatonicSet` or something coercable to `diatonicSet`; must be either length `1` or `length(x)`
#' 
#' @param parseArgs ***An optional list of arguments passed to the [pitch parser][pitchParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [pitch parser][pitchParsing].
#' 
#' @param transposeArgs ***An optional list of arguments passed to a special [transpose()] call.***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to [transpose()].
#' 
#' @param inPlace ***Should non-pitch information be retained in the output string.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton  `logical` value: an on/off switch.
#' 
#' This argument only has an effect if the input (the `x` argument) is `character` strings,
#' *and* there is extra, non-pitch information in the input strings "besides" the pitch information.
#' If so, and `inPlace = TRUE`, the output will be placed into an output string beside the original non-pitch information.
#' If `inPlace = FALSE`, only the pitch output information will be returned (details below).
#' 
#' @returns 
#' 
#' `NULL` inputs (`x` argument) return a `NULL` output.
#' Otherwise, returns a vector/matrix of the same length/dimension as `x`.
#' `NA` values in the input `x` are propagated to the output.
#'
#' @name pitchFunctions
#' @seealso To better understand how these functions work, read about 
#' how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
NULL

## Pitch transform maker ####

pitchArgCheck <- function(args,  callname) {
  argnames <- .names(args)
  
  for (arg in intersect(argnames, c('generic', 'specific', 'compound', 'simple', 'accidental.melodic', 'octave.absolute', 'octave.relative'))) {
    checks(args[[arg]], argname = arg, xTF, seealso = '?pitchDeparsing')
  }
  scalarchar <- c('flat', 'sharp', 'doublesharp', 'doubleflat', 'natural',
                  'diminish', 'augment', 'major', 'minor', 'perfect',
                  'up', 'down', 'same')
  for (arg in intersect(argnames, scalarchar)) {
    checks(args[[arg]], xcharacter & xlen1, seealso = '?pitchDeparsing')
  }
  
  if ('generic' %in% argnames) {
    if ('specific' %in% argnames && !xor(args$generic, args$specific)) .stop("In your call to {callname}, you've specified contradictory 'generic' and 'specific' arguments...it has to be one or the other!")
    args$specific <- !args$generic
  }
  
  if ('simple' %in% argnames) {
    if ('compound' %in% argnames && !xor(args$simple, args$compound)) .stop("In your call to {callname}, you've specified contradictory 'simple' and 'compound' arguments...it has to be one or the other!")
    args$compound <- !args$simple
  }
  
  if ('octave.absolute' %in% argnames) {
    if ('octave.relative' %in% argnames && !xor(args$octave.relative, args$octave.absolute)) .stop("In your call to {callname}, you've specified contradictory 'octave.relative' and 'octave.absolute' arguments...it has to be one or the other!")
    args$octave.relative <- !args$octave.absolute
  }
  
  if ('octave.offset' %in% argnames) {
    checks(args$octave.offset, argname = 'octave.offset', xwholenum)
  }
  
  if ('octave.round' %in% argnames) {
    checks(args$octave.round, argname = 'octave.round', xrounding)
  }
  
  if ('parts' %in% argnames) {
    checks(args$parts, argname = 'parts', xcharacter & xplegal(c('step', 'species', 'octave')))
    
  }
  
    

  
  args 
  
}


# this function will create various pitch transform functions
makePitchTransformer <- function(deparser, callname, 
                                 outputClass = 'character', 
                                 keyed = TRUE,
                                 # removeArgs = NULL, 
                                 extraArgs = alist()) {
  autoArgTable <<- rbind(autoArgTable,
                         data.table(Argument = 'Exclusive', Type = 'Exclusive', Function = callname, Expression = list(quote(Exclusive))))
  autoArgTable <<- rbind(autoArgTable, 
                         data.table(Argument = 'Key', Type = 'Keyed', Function = callname, Expression = list(quote(Key))))
  
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- c(alist(x = , 
                  ... = , # don't move this! Needs to come before other arguments, otherwise unnamed parse() argument won't work!
                  generic = FALSE, simple = FALSE, octave.relative = FALSE, 
                  Key = NULL),
            extraArgs,
            alist(transposeArgs = list(),
                  parseArgs = list(), 
                  gamutArgs = list(),
                  inPlace = FALSE))

  # if (!is.null(removeArgs)) args <- args[!names(args) %in% removeArgs]

  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr( {
    
    checks(x, xatomic | xclass('tonalInterval'))
    checks(inPlace, xTF)
    checks(parseArgs, xclass('list'))
    checks(transposeArgs, xclass('list'))
    checks(gamutArgs, xclass('list'))
    
    # parse out args in ... and specified using the syntactic sugar parse() or transpose()
    c('args...', 'parseArgs', 
      'transposeArgs', 'gamutArgs') %<-% specialArgs(rlang::enquos(...), 
                                                      parse = parseArgs, 
                                                      transpose = transposeArgs,
                                                      gamut = gamutArgs)
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
    Key     <- diatonicSet(Key %||% dset(0L, 0L))
    fromKey <- diatonicSet(transposeArgs$from %||% Key)
    toKey   <- diatonicSet(transposeArgs$to   %||% Key)
    
    parseArgs$Key   <- fromKey
    deparseArgs$Key <- toKey 
    
    if (!is.null(transposeArgs$from)) transposeArgs$from <- CKey(fromKey)
    if (!is.null(transposeArgs$to))   transposeArgs$to    <- CKey(toKey)
    
    # memoize % deparse
    memoize <- args...$memoize %||% TRUE
    deparse <- args...$deparse %||% TRUE
    
    
    ############# #
    ### Parse 
    ############# #
    parsedTint <- do(tonalInterval, 
                     c(list(x), parseArgs), 
                     memoize = memoize, 
                     outputClass = 'tonalInterval')
    
    if (length(transposeArgs) > 0L && is.tonalInterval(parsedTint)) {
      parsedTint <- do(transpose.tonalInterval, c(list(parsedTint), transposeArgs))
    }
    
    deparseArgs <- c(list(parsedTint), deparseArgs)
    output <- if (deparse && is.tonalInterval(parsedTint))  do(!!deparser, 
                                                               deparseArgs, 
                                                               memoize = memoize, 
                                                               outputClass = !!outputClass) else parsedTint
    if (deparse && !is.null(output)) {
      output <- if (inPlace) {
        rePlace(output, attr(parsedTint, 'dispatch'))
      } else {
        token(output, Exclusive = callname,
              deparseArgs = deparseArgs[!names(deparseArgs) %in% c('x', 'Key', 'Exclusive')][-1],
              gamutArgs = gamutArgs,
              factorizer = set.gamut,
              parser = tonalInterval,
              deparser = !!deparser)
      }
    }
    
    output
    
  })) %class% 'pitchFunction'
}



### Pitch functions ####



#' Translate pitches to frequency (Hz)
#' 
#' @param frequency.reference ***The reference frequency.***
#' 
#' Defaults to `440`.
#' 
#' Must be a single number.
#' 
#' @param frequency.reference.note ***The note that the `reference.frequency` tuned to.***
#' 
#' Defaults to `"a"`.
#' 
#' Can be any [parsable pitch representation][pitchParsing]; must be length `1`.
#' 
#' @param tonalHarmonic ***The frequency of the "tonal harmonic" (perfect 12th).***
#' 
#' Defaults to `2^(19/12)`, the 12-tone-equal-temperament 12th.
#' 
#' Must be a single number.
#' 
#' For [Pythagorean tuning](https://en.wikipedia.org/wiki/Pythagorean_tuning), set `tonalHarmonic = 3`.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- freq(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], freq(Token))
#' results
#' 
#' @family {atonal pitch functions}
#' @family {frequency-based pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
freq  <- makePitchTransformer(tint2freq, 'freq', 'numeric',
                              extraArgs = alist(tonalHarmonic = 2^(19/12), 
                                                frequency.reference = 440,
                                                frequence.reference.note = 'a')) 
#' Atonal pitch representations
#' 
#' These function translates pitch information into basic atonal pitch values:
#' `midi` and `semits` map pitches to standard 12-tone-equal-temperament 
#' semitone (`integer`) values. For `semits` `0` (zero) is middle-C (or unison).
#' In contrast, the [MIDI pitch values](https://en.wikipedia.org/wiki/MIDI) output by 
#' `midi` place middle-C/unison at `60`.
#' `cents` returns [cents](https://en.wikipedia.org/wiki/Cent_(music)), one hundredth of a semitone.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- semits(exampleToken)
#' results
#' results <- midi(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], semits(Token))
#' results
#' results <- with(exampleHumdrum[[,3:4]], midi(Token))
#' results
#' 
#' @family {atonal pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
semits <- makePitchTransformer(tint2semits, 'semits', 'integer')


#' @rdname semits
#' @export 
midi  <- makePitchTransformer(tint2midi, 'midi', 'integer')

#' @section Cents:
#'
#' By default, the output of `cents` is simply the same as `semits(x) * 100`.
#' However, the `tonalHarmonic` value can be modified for `cents` to produce cent-values for alternate tunings.
#' For example, `cents('g', tonalHarmonic = 3)` returns `r cents('g', tonalHarmonic = 3)`, because the 
#' "pure" third harmonic (`3`) is `1.955` sharper than equal-temperment.
#' Thus, whereas `midi` and `semits` return [integers][base::integer], `cents` always returns real-number ([double][base::double]) values.
#' 
#' [TonalIntervals][tonalIntervalS4] parsed from [frequencies][freq()] might also have arbitrary cent deviations.
#' For example, `cents(440 * 10/9, Exclusive = 'freq')` returns `1082.404`---this would correspond to the 
#' "[minor tone](https://en.wikipedia.org/wiki/Major_second#Major_and_minor_tones)" above A=440.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- cents(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], cents(Token))
#' results
#' 
#' @inheritParams freq
#' @rdname semits
#' @export 
cents  <- makePitchTransformer(tint2cents, 'cents', 'numeric', extraArgs = alist(tonalHarmonic = 2^(19/12)))



#' Representation of atonal pitch classes
#' 
#' As encoded in the humdrum 
#' [`**pc`](https://www.humdrum.org/rep/pc/index.html) interpretation.
#' 
#' @param ten ***A shorthand-symbol to use for 10.***
#' 
#' Defaults to `"A"`.
#' 
#' Must be a single `character` string.
#'
#' If `NULL`, `"10"` is used with no shorthand.
#' 
#' @param eleven ***A shorthand-symbol to use for 11.***
#' 
#' Defaults to `"B"`.
#' 
#' Must be a single `character` string.
#'
#' If `NULL`, `"11"` is used with no shorthand.
#' 
#' @family {atonal pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- pc(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], pc(Token))
#' results
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
pc <- makePitchTransformer(tint2pc, 'pc', 'character')


#' Scientific pitch representation
#' 
#' [Scientific pitch](https://en.wikipedia.org/wiki/Scientific_pitch) is the most standard
#' approach to representing pitch in traditional Western music. 
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- pitch(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], pitch(Token))
#' results
#' 
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
pitch <- makePitchTransformer(tint2pitch, 'pitch')

#' Kern pitch representation
#' 
#' Kern (`**kern`) is the most common humdrum interpretation for representing "notes" in the style of
#' traditional Western scores.
#' However! In [humdrumR], the `kern` function outputs the *pitch* part of the `**kern` interpretation.
#' `**kern` *rhythms* are instead created using the [recip()] function.
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
#'     Uppercase letters are used for octaves below ; lowercase letters for the middle-**C** octave and higher.
#'     The  octave, and the octave below it get one character each, with higher and lower octaves repeating that character.
#'     For example, using `C#` as the step value, and relative to the  octave:
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
#' Like all `humdrumR` pitch functions, the ways that `kern` [parses][pitchParsing] and [deparses][pitchDeparsing] tokens
#' can be modified to accomodate variations of the standard `**kern` pitch representation.
#' 
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- kern(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], kern(Token))
#' results
#' 
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
kern <- makePitchTransformer(tint2kern, 'kern') 

#' Lilypond pitch representation
#' 
#' This is the representation used to represent (Western tonal) pitches in the [Lilypond](https://lilypond.org/doc/v2.22/Documentation/notation/pitches) 
#' notation format.
#' In [humdrumR], the `lilypond` function only relates to the *pitch* part of Lilypond notation:
#' Lilypond-like *rhythms* can be creating using the [recip] function.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- lilypond(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], lilypond(Token))
#' results
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
lilypond <- makePitchTransformer(tint2lilypond, 'lilypond')

#' German-style pitch notation. 
#' 
#' Based on the common German system of notating pitches, as encoded in the humdrum 
#' [`**Tonh`](https://www.humdrum.org/rep/Tonh/index.html) interpretation.
#' 
#'
#' @param S ***Should the special shorthand for Eb and Ab be used?.***
#' 
#' Defaults to `TRUE`.
#'
#' If `S = TRUE`, E-flat (`Ees`) will be output as `"S"` and A-flat (`Aes`) will be output `"As"`.
#'
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- tonh(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], tonh(Token))
#' results
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
tonh <- makePitchTransformer(tint2tonh, 'tonh')

#' Helmholtz pitch representation
#' 
#' [Helmholtz notation](https://en.wikipedia.org/wiki/Helmholtz_pitch_notation)
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- helmholtz(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], helmholtz(Token))
#' results
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
helmholtz <- makePitchTransformer(tint2helmholtz, 'helmholtz')

#' Tonal (pitch) interval representation
#' 
#' This returns the standard representations of [intervals](https://en.wikipedia.org/wiki/Interval_(music))
#' in Western music.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- interval(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], interval(Token))
#' results
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @family {Lagged pitch interval functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions],
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
interval <- makePitchTransformer(tint2interval, 'interval')

#' Tonal [scale degree](https://en.wikipedia.org/wiki/Degree_(music)) representation (absolute)
#' 
#' The humdrum [`**degree`](https://www.humdrum.org/rep/degree/index.html) and 
#' [`**deg`](https://www.humdrum.org/rep/deg/index.html) interpretations represent Western
#' "scale degrees" in two slightly different formats.
#' In the `**degree` representation, the octave of each pitch is represented "absolutely,"
#' in the same standard octave scheme as [scientific pitch][pitch()].
#' In the `**deg` representation, the octave of each pitch is indicated *relative to the previous pitch*---
#' a `"^"` indicates the pitch higher than the previous pitch while a `"v"` indicates a pitch lower than
#' the previous pitch.
#' 
#' @examples
#' 
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- degree(exampleToken)
#' results
#' results <- deg(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], degree(Token))
#' results
#' results <- with(exampleHumdrum[[,3:4]], deg(Token))
#' results
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
degree <- makePitchTransformer(tint2degree, 'degree', keyed = FALSE)


#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @rdname degree
#' @export 
deg <- makePitchTransformer(tint2deg, 'deg', keyed = FALSE)


#' Relative-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge) representation
#' 
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' 
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- solfa(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], solfa(Token))
#' results
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
solfa <- makePitchTransformer(tint2solfa, 'solfa', keyed = FALSE)


#' Fixed-do [Solfege](https://en.wikipedia.org/wiki/Solf%C3%A8ge) representation
#' 
#' Based on the common French system of notating pitches, as encoded in the humdrum 
#' [`**solfg`](https://www.humdrum.org/rep/solfg/index.html) interpretation.
#' 
#' @family {absolute pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- solfg(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], solfg(Token))
#' results
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
solfg <- makePitchTransformer(tint2solfg, 'solfg')

#' Swara representation
#' 
#' [Swara](https://en.wikipedia.org/wiki/Svara) are syllabes used to represent scale degrees
#' in hindustani music---like solfege.
#' 
#' @examples
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- bhatk(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], bhatk(Token))
#' results
#' 
#' @family {relative pitch functions}
#' @family {pitch functions}
#' @seealso To better understand how this function works, read about the [family of pitch functions][pitchFunctions], 
#' or how pitches are [parsed][pitchParsing] and [deparsed][pitchDeparsing].
#' 
#' @inheritParams pitchFunctions
#' @inheritSection pitchDeparsing Basic pitch arguments
#' @inheritSection pitchDeparsing Pitch-Gamut Levels
#' @export 
bhatk <- makePitchTransformer(tint2bhatk, 'bhatk', keyed = FALSE)


#### Partial pitch extractors ----

#' Extract scale step.
#' 
#' This is equivalent to using any [pitch function][pitchFunctions] with the
#' arguments `generic = TRUE`, `simple = TRUE`, and `step.labels = NULL`.
#' By default, `step()` will returns steps relative to the key---set `Key = NULL` if you don't want this.
#' 
#' @examples 
#' \dontrun{
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
#' 
#' within(chorales, step(Token))
#'
#' within(chorales, step(Token, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B')))
#' }
#' 
#' @inheritParams pitchFunctions
#' @family {pitch functions}
#' @family {partial pitch functions}
#' @export 
step <- makePitchTransformer(partialApply(tint2step, step.labels = NULL), 'step', 'integer')

#' Extract accidental from pitch.
#' 
#' Use this if you want to extract *only* the accidentals from pitch data,
#' discarding octave and step information.
#' Set `explicitNaturals = FALSE` if you don't want explicit naturals.
#' 
#' @examples 
#' \dontrun{
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
#' 
#' within(chorales, accidentals(Token))
#'
#' }
#' @inheritParams pitchFunctions
#' @family {pitch functions}
#' @family {partial pitch functions}
#' @export 
accidental <- makePitchTransformer(partialApply(tint2specifier, flat = 'b', qualities = FALSE, explicitNaturals = TRUE), 
                                   'accidental', 'character')

#' Extract quality from pitch
#' 
#' Use this if you want to extract *only* the tonal qualities from pitch data,
#' discarding octave and step information.
#' 
#' @examples 
#'
#' \dontrun{
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
#' 
#' within(chorales, quality(Token))
#' 
#' # Harmonic interval qualities:
#' 
#' within(chorales, hint(Token, deparser = quality))
#' with(chorales, hint(Token, deparser = quality, incomplete = NA, bracket = FALSE)) |> table()
#' 
#' }
#' @inheritParams pitchFunctions
#' @family {pitch functions}
#' @family {partial pitch functions}
quality <- makePitchTransformer(partialApply(tint2specifier, qualities = TRUE, explicitNaturals = TRUE), 
                                'quality', 'character')


#' Extract octave.
#' 
#' Returns which octave each pitch falls in.
#' By default, middle-C is the bottom of the zeroth-octave, but this can be changed with the `octave.offset`
#' argument.
#' Other octave labels (like [lilypond()]-style marks) can be used if you set `octave.integer = FALSE`.
#' 
#' 
#' \dontrun{
#' chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
#' 
#' within(chorales, octave(Token))
#' within(chorales, octave(Token, octave.offset = 4)) # traditional octaves
#' 
#' within(chorales, octave(Token, octave.integer = FALSE))
#' }
#' @inheritParams pitchFunctions
#' @family {pitch functions}
#' @family {partial pitch functions}
#' @export 
octave <- makePitchTransformer(tint2octave, 'octave', 'integer')





###################################################################### ###
# Manipulating tonal intervals ###########################################
###################################################################### ###

## Partitioning tonalIntervals ####


tintPartition <- function(tint, partitions = c('compound', 'harmonic', 'specific'),
                          octave.round = floor, Key = NULL, enharmonicWrap = 12L, ...) {
  
  partitions <- matched(partitions, c('compound', 'harmonic', 'specific'))
  
  Key <- diatonicSet(Key %||% dset(0, 0))
  match_size(tint = tint, Key = Key, toEnv = TRUE)
  
  octave <- if ('compound' %in% partitions) {
    compound <- tintPartition_compound(tint, octave.round)
    tint <- compound$Simple
    compound['Octave']
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

#### simple + octave = compound ####


tintPartition_compound <- function(tint, octave.round = floor, ...) {
  octshift <- as.integer(octave.round(tint2semits(tint %% dset(0, 0)) / 12))
  
  octavepart <- tint(octshift, 0L)
  simplepart <- tint - octavepart
  
  .data.frame(Octave = octavepart, Simple = simplepart)
  
}


#### enharmonic + comma = harmonic ####



tintPartition_harmonic <- function(tint, enharmonic.minimum = -5L, enharmonic.maximum = enharmonic.minimum + 11L, ...) {
  
  # modeoffset <- tint( , getSignature(Key)) + tint(, 2) # because 2 fifths is the "center" of the diatonic set
  # entint <- (tint - modeoffset) %<-dim% NULL
  
  # mode <- if (is.null(Key)) 0L else getMode(Key)
  lof <- LO5th(tint) #+ mode
  harmonic <- tint
  
  # flatside
  tooflat <- lof < enharmonic.minimum
  harmonic[tooflat] <- tint[tooflat] + (tint(-19L, 12L) * (1L + (lof[tooflat] - enharmonic.minimum) %/% -12L))
  
  # sharpside
  toosharp <- lof > enharmonic.maximum
  harmonic[toosharp] <- tint[toosharp] - (tint(-19L, 12L) * (1L + (lof[toosharp] - enharmonic.maximum) %/%  12L))

  
  

  .data.frame(Enharmonic = harmonic,  Comma = tint - harmonic)
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
#' @param x ***The input pitch(es) to transpose. *** 
#' 
#' Can be a `tonalInterval` or something [intepretable as a pitch information][pitchParsing].
#' 
#' @param by ***Transpose by this interval.***
#' 
#' Can be a `tonalInterval` or something intepretable as a `tonalInterval`. 
#'
#' @param Key ***Transpose *from* this key (to the `to` key).***
#' 
#' Can be a `diatonicSet` or something intepretable as a `diatonicSet`. 
#' 
#' For tonal and/or to transpositions, this is the "from" key. If this value is `NULL`, it defaults to C major.
#' 
#' @param to ***Transpose *to* this key (from the `Key` key).***
#' 
#' Can be a `diatonicSet` or something intepretable as a `diatonicSet`.
#' 
#' @param real ***Should transposition be real (or tonal)?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleon `logical` value: an on/off switch.
#' 
#' If `real == FALSE`, transposition is tonal.
#' 
#' @param relative ***Should transposition between keys be relative (or parallel)?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' Only relavent if using transposing between keys (`Key` and `to`) with different modes.
#' If `relative == FALSE`, transposition is parallel.
#' 
#' @family tonal transformations
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
      comma <- tintPartition(x$Generic[altered] + x$Alteration[altered], 'harmonic', Key = to)$Comma
      x$Alteration[which(altered)[comma != tint(0,0)]] <- tint(0, 0)
    }
    
    x$Generic + x$Alteration
  }
  
  x
}

#' @export
transpose.token <- function(x, by = NULL, from = NULL, to = NULL, ...) {
  x <- tonalInterval.character(as.character(x), ...)
  tints <- transpose.tonalInterval(x, by = by, from = from, to = to, ...)
  
  dispatch <- attr(x, 'dispatch')
  reParse(tints, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree'))
}

#' @export
transpose.factor <- transpose.token

#' @export
transpose.character <- function(x, by = NULL, from = NULL, to = NULL, ...) {
  x <- tonalInterval.character(x, ...)
  tints <- transpose.tonalInterval(x, by = by, from = from, to = to, ...)
  
  dispatch <- attr(x, 'dispatch')
  rePlace(reParse(tints, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
}

#' @export
transpose.numeric <- function(x, by = NULL, from = NULL, to = NULL, ...) {
  x <- tonalInterval.numeric(x, ...)
  tints <- transpose.tonalInterval(x, by = by, from = from, to = to, ...)
  
  
  dispatch <- attr(x, 'dispatch')
  y <- reParse(tints, dispatch, c('semits', 'freq', 'midi', 'cents', 'lof'))
  humdrumRattr(y) <- NULL
  y
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





## Inverting tonal intervals ####



#' Invert or transpose pitches.
#'
#' @family tonal transformations
#' @export 
invert <- function(tint, around, Key, ...) UseMethod('invert')
#' @export 
invert.tonalInterval <- function(tint, around = tint(0L, 0L), Key = NULL) {
  around <- tonalInterval(around)
  
  output <- (around + around - tint) 
  if (!is.null(Key)) output <- output %% diatonicSet(Key)
  
  output
}


#' @export
invert.character <- function(x, around = tint(0L, 0L), Key = NULL, ...) {
  x <- tonalInterval.character(x, ...)
  tints <- invert.tonalInterval(x, around = around, Key = Key)
  
  dispatch <- attr(x, 'dispatch')
  rePlace(reParse(tints, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
}

#' @export
invert.numeric <- function(x, around = tint(0L, 0L), Key = NULL, ...) {
  x <- tonalInterval.numeric(x, ...)
  tints <- invert.tonalInterval(x, around = around, Key = Key)
  
  y <- reParse(tints, dispatch, c('semits', 'freq', 'midi', 'cents', 'lof'))
  humdrumRattr(y) <- NULL
  y
}

#' @export
invert.token <- function(x, around = tint(0L, 0L) , Key = NULL, ...) {
  x <- tonalInterval.character(as.character(x), ...)
  tints <- invert.tonalInterval(x, around = around, Key = Key, ...)
  
  dispatch <- attr(x, 'dispatch')
  reParse(tints, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree'))
}

#' @export
invert.factor <- invert.token

### Inversion methods ####


## Melodic Intervals ####

#' Calculate intervals between pitches
#' 
#' These functions allow us to calculate intervals between pitches.
#' `int()` is the most basic form, calculating the interval(s) between two input vectors.
#' `mint()` and `hint()` are special forms for calculating intervals "melodically" or "harmonically," respectively.
#'
#' @details 
#' 
#' Input vectors `x` (and `from`) are [parsed as pitches][tonalInterval()] ([tonal interval objects][tonalIntervaS4]), if possible.
#' (Parsing arguments can be passed via the `parseArgs` list, or `parse(...)` sugar. 
#' `Key` and `Exclusive` arguments are also passed to the parser.)
#' Any inputs that fail to parse will show up as `NA` in the output.
#' 
#' Once parsed, the intervals between the pitches are calculated as `x - from`.
#' The resulting intervals are then "[deparsed][pitchDeparsing]" into a standard representation; by default, the [intervals()]
#' representation is used, but you can set the `deparser` argument to any [pitch function][pitchFunctions].
#' However, the only alternative deparser that would be *commonly* used (besides [intervals()]) would be [semits()].
#' If `deparser` is `NULL`, the raw [tonalIntervals][tonalIntervalS4] are returned.
#' 
#' @section Melodic and Harmonic intervals:
#'
#' `mint` and `hint` calculate "melodic" and "harmonic" intervals respectively.
#' In this context, "melodies" are sequences of notes within a **spine path**, while
#' "harmonies" are intervals between notes occurring in the same **record** (at the same time).
#' Outside of a [with(in)][withinHumdrum] call, `mint` or `hint` are exactly the same;
#' It is only when used in a call to [with(in)][withinHumdrum] that you will see them have different behaviors,
#' as [with(in)][withinHumdrum] will automatically apply them across spine paths (`mint()`) or records (`hint()`).
#' This is achieved by modifying the `groupby` and `orderby` arguments to [lag()]---you can manually achieve
#' the default behaviors, or other behaviors, by setting these arguments yourself.
#' 
#' When used in a [with(in)][withinHumdrum] expression, `mint()` will (by default) calculate the melodic interval *approaching* each note 
#' from the previous note:
#' for example, `mint('C4', 'D4', 'Eb4')` fill return `c('[c]', '+M2', '+m2')`, because D4 is approached by
#' ascending whole step *from* C4, and Eb4 is approached by ascending half step *from* D4.
#' Similarly, the default [with(in)][withinHumdrum] behavior of `hint()` is to calculate successive intervals in the same record 
#' (*across* spine paths), from left to right.
#' So the record `C3  G4  C5` will return values `[CC]  +P12  +P4`, because the G4 is a perfect 12th above C3, and
#' C5 is a perfect fourth above G4.
#' 
#' 
#' `mint()` and `hint()` work by passing [lagged][lag()] and/or [dittoed][ditto()]
#' versions of `x` as the `from` argument to `int()`.
#' Basically, `mint()` is equivalent to `int(x, lag(x, lag = lag, groupby = list(Piece, Spine, Path)), ...)`
#' and `hint()` is equivalent to `int(x, lag(x, lag = lag, groupby = list(Piece, Record), orderby = list(Piece, Record, Spine, Path)), ...)`.
#' In either case, the parsed pitch vector is copied and lagged using [lag()], with pairs crossing outside `groupby` groups ignored.
#' The `lag` argument controls how far apart in the melody intervals are calculated.
#' For instance, a lag of `2` will calculate intervals between *every other* note in the vector.
#' Positive lags (the default) will calculate **approaching** intervals: each token represents the interval between the current note
#' and the *previous* note.
#' Negative lags will calculate **departing** intervals: each token represents the interval 
#' between the current note and the *next* note.
#' Note that, by passing `directed = FALSE` through the the [deparser][pitchDeparsing], the undirected (absolute value)
#' of the melodic intervals can be returned.
#' 
#' @section Incomplete value padding:
#' 
#' By default, `int` will return `NA` anywhere where `x` **or** `from` is `NA`.
#' However, if `from` is `NA` but `x` is *not* `NA`, we can ask for different output for these "incomplete" pairs.
#' using the `incomplete` argument.
#' If `incomplete` is an atomic value, incomplete outputs indices are willed with this value.
#' If the incomplete argument is a [pitch function][pitchFunctions] (like the `deparser` argument),
#' this function is used to (re)parse the values of `x` where `from` is missing.
#' If `bracket == TRUE`, incomplete output values are surrounded with `[]`, so they are easier to distinguish from the
#' actual intervals.
#'
#' The main use of the `incomplete` argument is in `mint()` and `hint()`.
#' The lagged `from` arguments used in `mint()`/`hint()` (see previous section) are necessarily padded by `abs(lag)` `NA`
#' values at the beginning (positive lag) or end (negative lag).
#' These are thus "incomplete" pairs passed to `int()`, and can controlled using the `incomplete` argument.
#' By default, both `mint()` and `hint()` set `incomplete = kern(), bracket = TRUE` which cause these
#' notes to show up as bracketed kern, like `[ee-]` or `[C#]`.
#' If `incomplete` is `NULL`, the incomplete values are simply padded with `NA`.
#' 
#' 
#' @section Interval classification:
#' 
#' If the `classify` argument is set to `TRUE`, intervals are classified as either `"Unison"`,
#' `"Step"`, `"Skip"`, or `"Leap"`.
#' Alternatively, skips can be interpreted as leaps by setting `skips = FALSE`.
#' (`classify = TRUE` overrides the `deparser` argument.)
#'
#' By default, intervals are categorized tonally, meaning that the interval in tonal *steps*
#' is used as the basis of classification.
#' For example, an augmented 2nd is a step, and a diminished 3rd is a skip/leap.
#' This means that augmented and diminished unisons are marked `"Unison"` as well!
#' However, if `directed = TRUE`, augmented/diminished unisons will be marked with `+` or `-`
#' to indicate direction, whereas perfect unisons are never marked with `+`/`-`.
#' 
#' Alternatively, you may choose to categorize intervals *atonally* by setting `atonal = TRUE`.
#' If so, intervals are categorized based only on semitone (enharmonic) intervals:
#' D# and Eb are classified the same.
#' 
#' @section Logical (ditto) lags:
#' 
#' For calls to `hint()` and `mint()` the default behavior is a `numeric` `lag` argument passed to [lag()].
#' An alternate option is to specify the `lag` argument as  `logical` vector the same length as the input (`x` argument).
#' Rather than calculating the interval between a pitch and another pitch separated by a regular lag,
#' a `logical` `lag` argument "lags" each pitch back to the previous value where `lag == TRUE`.
#' This means that more than one interval can be calculated from those same `TRUE` indices.
#' 
#' The canonic use of this "logical lag" feature is to calculate harmonic intervals relative to the same voice, like the bass voice.
#' For example, consider this file:
#' 
#' ```
#'  **kern        **kern        **kern        **kern
#' *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
#'       C             e             g            cc
#'       G             d             f             b
#'       C             c             e            cc             
#'      *-            *-            *-            *-
#' ```
#' 
#' If we [read][readHumdrum()] this file and applied `hint()` to the `Token` field (with default arguments)
#' the result would be:
#' 
#' ```
#'  **kern        **kern        **kern        **kern
#' *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
#'     [C]          +M10           +m3           +P4
#'     [G]           +P5           +m3           +A4
#'     [C]           +P8           +M3           +m6             
#'      *-            *-            *-            *-
#' ```
#' 
#' In each record, we see the intervals as lagged (`lag == 1`) from left right:
#' we see the intervals between the bass and the tenoir, the tenor and the alto, and the alto
#' and the soprano.
#' What if we wanted to see all the intervals with the bass?
#' Well, we can use a `logical` `lag` argument, where we would specify that `Spine == 1`:
#' `with(humData, hint(Token, lag = Spine == 1)`.
#' This means that all `from` values are "lagged" back to the previous value where `Spine == 1`.
#' The result would be:
#'
#' ```
#'  **kern        **kern        **kern        **kern
#' *I"Bass      *I"Tenor       *I"Alto    *I"Soprano
#'     [C]          +M10          +P12          +P14
#'     [G]           +P5           +m7          +M10
#'     [C]           +P8          +M10          +P14             
#'      *-            *-            *-            *-
#' ```
#' 
#' Now we see all the intervals relative to the bass.
#' 
#' The `logical` `lag` only takes place within the `groupby` groups.
#' However, note that any values *before* the first index where `lag == TRUE`
#' are calculated relative to that first value.
#' 
#' @param x ***Input pitch information.***
#' 
#' Can be any ([atomic][base::vector]) vector, or a [tonalInterval][tonalIntervalS4], or `NULL`.
#' Must be [parsable as pitch information][pitchParsing].
#'
#' 
#' @param from ***Pitches to calculate the intervals from.***
#' 
#' Defaults to middle C / unison.
#' 
#' Can be any ([atomic][base::vector]) vector, or a [tonalInterval][tonalIntervalS4], or `NULL`.
#' Must be [parsable as pitch information][pitchParsing].
#' 
#' @param lag ***The [lag()] to calculate harmonic/melodic intervals between.***
#' 
#' Defaults to `1`, which means intervals between immediate successors in `x`.
#' 
#' Must be either a single number, or a `logical` of `length(x)` (see "Logical lags" section in manual).
#' 
#' @param deparser ***What output representation do you want?***
#' 
#' Defaults to [interval][interval()].
#'
#' Must be a [pitch function][pitchFunctions], like `kern()`.
#' 
#' @param incomplete ***How to pad incomplete intervals (e.g., the start/end of input).***
#' 
#' Defaults to `NULL` for `int()`, `kern` for `mint()` and `hint()`.
#' 
#' Must `NULL`, a [pitch function][pitchFunctions], or an atomic value of `length(incomplete) == abs(lag)`.
#' 
#' @param bracket ***Whether to print brackets around `incomplete` output.***
#' 
#' Defaults to `TRUE`, unless `incomplete` is `NA`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, square brackets (`"[]"`) are printed around `incomplete` observations.
#' 
#' @param classify ***Should intervals be classified as step/skip/leaps?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, the `deparser` is ignored and the output is classified as `Unison`, `Step`, `Skip`, or `Leap`.
#'
#' @param parseArgs ***An optional list of arguments passed to the [pitch parser][pitchParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [pitch parser][pitchParsing].
#' 
#' @param groupby ***A `list` of vectors to group `x`.***
#' 
#' Defaults to `list()`.
#' 
#' Must be a `list`; every element of the list must be length `length(x)`.
#' 
#' @param orderby ***A `list` of vectors to group `x`.***
#' 
#' Defaults to `list()`.
#' 
#' Must be a `list`; every element of the list must be length `length(x)`.
#' 
#' Used to interpret the order of elements in `x`. Lagged computations are done in the indicated
#' order, but the output is returned in the original order.
#'
#' @family {relative pitch functions}
#' @family {Lagged vector functions}
#'
#' @examples 
#' exampleToken <- c('4GG', '4G', '4E', '4F#', '4G', '4D', '4E')
#' results <- mint(exampleToken)
#' results
#' results <- hint(exampleToken)
#' results
#' 
#' exampleHumdrum <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' results <- with(exampleHumdrum[[,3:4]], mint(Token))
#' results
#' results <- with(exampleHumdrum[[,3:4]], hint(Token))
#' results
#'
#' @seealso {`mint` uses [lag()] to "lag" the input pitches, and also makes use of [pitch parsers][tonalInterval()] and [pitch functions][pitchFunctions].}
#' @inheritSection sigma Grouping
#;
#' @name int
#' @export
int <- function(x, from = tint(0L, 0L), deparser = interval, incomplete = NULL, bracket = is.null(incomplete) || !is.na(incomplete),
                classify = FALSE, 
                ..., Exclusive = NULL, Key = NULL, parseArgs = list()) {
  
  
  checks(deparser, xnull | xclass('pitchFunction'))
  
  checks(classify, xTF)
  checks(bracket, xTF)
  
  from <- rep(from, length.out = length(x))
  
  if (classify) deparser <- mintClass
 
  x    <- do.call('tonalInterval', c(list(x,    Exclusive = Exclusive, Key = Key), parseArgs))
  from <- do.call('tonalInterval', c(list(from, Exclusive = Exclusive, Key = Key), parseArgs))
  
  interval <- x - from
  
  if (is.null(deparser)) return(interval)
  
  output <- deparser(interval, ...) 
  
  missing <- !is.na(x) & is.na(output)
  
  if (!is.null(incomplete) && any(missing)) {
    
    if (is.function(incomplete) && inherits(incomplete, 'pitchFunction')) {
      
       incomplete <- as.character(incomplete(x[missing], Key = Key[missing], Exclusive = Exclusive[missing], ...)) # need Exclusive right?
    } 
    
    if (bracket) incomplete <- paste0('[', incomplete, ']')
    
    
    if (is.factor(output)) {
      levs <- c(unique(as.character(incomplete)), levels(output))
      class <- class(output)
      output <- as.character(output)
      output[missing] <- incomplete
      output <- factor(output, levels = levs)
      class(output) <- class
    } else {
      
      output[missing] <- incomplete
    }
    
  }
  
 output
}


#' @rdname int
#' @export 
mint <- function(x, lag = 1, deparser = interval, incomplete = kern, bracket = is.null(incomplete) || !is.na(incomplete),
                         classify = FALSE, ..., 
                         parseArgs = list(), Exclusive = NULL, Key = NULL, groupby = list(), orderby = list()) {
  
  checks(lag, (xlogical & xmatch(x)) | (xwholenum & xlen1 & xnotzero))
  checks(deparser, xclass('pitchFunction'))
  checks(incomplete, xnull | xclass('pitchFunction') | (xatomic & xminlength(1) & 
           argCheck(\(arg) length(arg) <= abs(lag), 
                    "must be as short or shorter than the absolute lag",  
                    \(arg) paste0(.mismatch(length)(arg), ' and lag == ', lag))))
  checks(bracket, xTF)
  checks(classify, xTF)
  
  
  if (is.numeric(lag)) {
    if (lag >= 0L) {
      from <- lag(x, lag, groupby = groupby, orderby = orderby)
     
    } else {
      from <- x
      x <- lag(from, lag, groupby = groupby, orderby = orderby)
      na <- is.na(x) & !is.na(from)
      x[na] <- from[na]
      from[na] <- NA
    }
    
    
  } else {
    from <- ditto.default(x,    null = !lag, groupby = groupby, orderby = orderby)
    from <- ditto.default(from, null = !lag & (is.na(from) & !is.na(x)), groupby = groupby, orderby = orderby, reverse = TRUE)
    from[lag] <- NA
  }
  
  int(x, from, deparser = deparser, parseArgs = parseArgs, 
      Exclusive = Exclusive, Key = Key, 
      incomplete = incomplete, bracket = bracket, 
      classify = classify, ...)
 
  
}


.mint <- function(X, L, lag, deparser, initial, bracket, parseArgs, Exclusive, Key, ...) {
  Xtint <- do.call('tonalInterval', c(list(X, Exclusive = Exclusive, Key = Key), parseArgs))
  Ltint <- do.call('tonalInterval', c(list(L, Exclusive = Exclusive, Key = Key), parseArgs))
  tint <- if (lag >= 0) Xtint - Ltint else Ltint - Xtint
  
  output <- do(deparser, list(tint, ...))
  
  singletons <- !is.na(Xtint) & is.na(Ltint)
  
  if (!is.null(initial) && any(singletons)) {
    
    if (is.function(initial)) output[singletons] <- paste0(if (bracket) '[', 
                                                           do(initial, c(list(Xtint[singletons], 
                                                                              Exclusive = Exclusive, 
                                                                              Key = Key), 
                                                                         ...)), 
                                                           if (bracket) ']')
    if (is.atomic(initial)) output[singletons] <- initial
  }
  output
}

mintClass <- function(x, directed = TRUE, skips = TRUE, atonal = FALSE) {
  
  int <- rep(NA_integer_, length(x))
  if (atonal) {
    int[!is.na(x)] <- tint2semits(x[!is.na(x)])
    sign <- c('-', '', '+')[sign(int) + 2L]
    breaks <- c(-Inf, 0, 2, 4, Inf)

  } else {
    int[!is.na(x)] <- tint2interval(x[!is.na(x)], step.labels = NULL, specific = FALSE, compound = FALSE)
    sign <- stringr::str_extract(int, '^[+-]?')
    int <- as.numeric(int)
    
    
    breaks <- c(0, 1, 2, 3, Inf)
  }
  
  int <- abs(int)
   
 
  intClass <- as.character(cut(int, breaks = breaks, labels = c('Unison', 'Step', if (skips) 'Skip' else 'Leap', 'Leap')))
  
  
  .paste(if (directed) sign, intClass)
}



## Harmonic Intervals ####

#' @rdname int
#' @export
hint <- function(x, lag = 1, deparser = interval, incomplete = kern, bracket = is.null(incomplete) || !is.na(incomplete),
                 ...,
                 parseArgs = list(), Exclusive = NULL, Key = NULL, groupby = list(), orderby = list()) {

  # all checks are conducted by mint, so don't need to repeat them
  #checks(x, xatomic & xminlength(1))
  #checks(lag, xwholenumber & xlen1 & xnotzero)
  #checks(deparser, xclass('function'))
  #checks(incomplete, xatomic & xminlength(1) & 
  #        argCheck(\(arg) length(arg) <= abs(lag), 
  #                 "must be as short or shorter than the absolute lag",  
  #                 \(arg) paste0(.mismatch(length)(arg), ' and lag == ', lag)))
  #checks(bracket, xTF)
  
  mint(x, lag = lag, deparser = deparser, incomplete = incomplete, bracket = bracket,
       parseArgs = parseArgs, 
       Exclusive = Exclusive, Key = Key, 
       groupby = groupby, orderby = orderby, ...)
  
  
 
}

###################################################################### ### 
# Predefined tonalIntervals ##############################################
###################################################################### ### 
#' @name tonalIntervalS4
#' @export dd1 dd2 A2 P3 d4 d5 d6 AA6 M7 dd9 A9 P10 d11 d12 d13 AA13 M14 P15
#' @export d1 d2 AA2 M3 P4 P5 m6 dd7 A7 P8 d9 AA9 M10 P11 P12 m13 dd14 A14 A15
#' @export P1 m2 dd3 A3 A4 A5 P6 d7 AA7 m9 dd10 A10 A11 A12 P13 d14 AA14 AA15
#' @export A1 P2 d3 AA3 AA4 AA5 M6 m7 dd8 A8 P9 d10 AA10 AA11 AA12 M13 m14 dd15
#' @export AA1 M2 m3 dd4 dd5 dd6 A6 P7 d8 AA8 M9 m10 dd11 dd12 dd13 A13 P14 d15
#' @export unison pythagorean.comma octave

allints <- outer(c('dd', 'd', 'm', 'P', 'M', 'A', 'AA'), 1:15, paste0)
allints[as.matrix(expand.grid(c(3,5), c(1,4,5,8, 11,12,15)))] <- NA
allints <- c(allints)
allints <- allints[!is.na(allints)]
cat(paste0("#' @export ", unlist(tapply(allints, rep(1:5, length.out = length(allints)), paste, collapse = ' '))), sep = '\n')
for (i in allints) {
  val <- interval2tint(i)
  assign(i, val)
}
rm(allints)
unison <- P1
pythagorean.comma <- (-dd2)





