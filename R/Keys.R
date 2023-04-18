################################## ###
# diatonicSet S4 class ###############
################################## ###

## diatoniocSetS4 documentation ----

#' Tonal (diatonic) sets
#' 
#' 
#' `diatonicSet` is one of [humdrumR]'s 
#' types of tonal data, representing Western diatonic keys.
#' For the most part, users should not need to interact with diatonicSets directly---rather, diatonicSets work behind the scene in numerous `humdrumR` pitch functions.
#' See the [keyRepresentations] and [keyTransformations] documentation for details of usage and functionality or the *Tonality in humdrumR* vignette for 
#' a detailed explanation of the theory and specifics of diatonicSets.
#' 
#' @details
#' 
#' `diatonicSet` is a [S4](https://adv-r.had.co.nz/S4.html) subclass of `humdrumR`'s virtual class [struct], 
#' from which it inherits a lot of useful "vector-like" behaviors/functionality.
#' 
#' 
#' The constructor function `dset` can be used to create `diatonicSets` directly.
#' The three arguments corespond to the three slots: `root`, `mode`, and `alteration`.
#' All inputs will be coerced to match in length.
#' The `root` argument will attempt to coerce character strings to [tonalIntervals][tonalInterval], and use their `LO5th` value as the root.
#' 
#' By default, the [as.character][base::character] method, and thus (via [struct]) the [show][methods::show] method,
#'  for diatonicSets call [key()][diatonicRepresentations].
#' Thus, if you return a `diatonicSet` on the command line (or call [print][base::print] one one), 
#' you'll see the [key interpretation][diatonicRepresentations] representation printed.
#' 
#' @slot Root integers representing the root of the key on the line-of-fifths
#' @slot Signature integers representing the signature (number of accidentals) of the key. 
#' 
#' A key is represented by two integers, `Root` and `Signature`.
#' Root is simply the tonic note of the key on the circle of fifths.
#' Signature is a value on the circle of fifths, indicating the diatonic mode.
#' You can think of the `Signature` value as indicating the number of accidentals, with negative numbers
#' for flats and positive numbers for sharps.
#' You can also think of the signature as indicating how much the "natural key" (C major) is
#' slid up and down the line-of-fifths.
#' The [traditional diatonic modes](https://en.wikipedia.org/wiki/Mode_(music)) of Western music occur wherever `Signature - Tonic` is in the range `-5:1`:
#' 
#' * Signature - Tonic = +1 => Lydian
#' * Signature - Tonic = +0 => Major (Ionian)
#' * Signature - Tonic = -1 => Mixolydian
#' * Signature - Tonic = -2 => Dorian
#' * Signature - Tonic = -3 => Minor (Aeolian)
#' * Signature - Tonic = -5 => Locrian
#' * Signature - Tonic = -4 => Phyrgian
#' 
#' *Note that you can make diatonicSets where the `Root` is outside the `Key`. This is unusual, and may result in sets you wouldn't predict.*
#' 
#' @slot Alteration integers representing alterations of the diatonic set (details below).
#' 
#' @section Alterations:
#' 
#' The `Alteration` slots (also integer) can be used to represent various 
#' "altered" scales. 
#' The integer values are interpreted as a seven-trit [balanced ternary](https://en.wikipedia.org/wiki/Balanced_ternary) string.
#' ("trits" are the ternary equivalent of binary "bits.")
#' Balanced ternary allows for three digits, `0` (unaltered degree), `1` (sharpened degree), and `-1` (flattened degree).
#' The seven trits correspond to the seven scale degrees on the line-of-fifth indicated by the *signature*---i.e., ordered from 
#' lowest to hightest on the line-of-fifths, not relative to the root.
#' (For instance, when `Signature == 0`, the degrees are `c(-1, 0, 1, 2, 3, 4, 5)`.)
#' 
#' The ternary arrangement maps powers of three to each scale degree, as so that in the `Alteration` integer:
#' 
#' + ± 1: raise or flatten the **7th** scale degree.
#' + ± 3: raise or flatten the **3rd** scale degree.
#' + ± 9: raise or flatten the **6th** scale degree.
#' + ± 27: raise or flatten the **2nd** scale degree.
#' + ± 81: raise or flatten the **5th** scale degree.
#' + ± 243: raise or flatten the **1st** scale degree.
#' + ± 749: raise or flatten the **4th** scale degree.
#' 
#' For example, consider `Alteration == 26`:
#' In a balanced ternary representation, the decimal integer 26 is represented as `1 0 0 1 0 -1 0`.
#' (In other words 1 in the "27s  place" and -1 in the "ones place"---i.e., 27 - 1).
#' This represents a raised 2nd (the 27) and a lowered 7th (the -1).
#' 
#' The `Alteration` integer allows us to concisely represent all the 2,187 possible combinations of raised and lowered diatonic scale degrees!
#' However, combined with the `Signature` slot, there is some redundancy in scale representation.
#' For example, a melodic minor scale can be represented as a major scale (`Signature - Root == 0`) with a lowered third degree (`Alteration == -3`) *or* as 
#' minor scale (`Signature - Root == -3`) with raised 6ths and 7ths (`Alteration == 10`).
#' However, though these two representations result in the same set on the line-of-fifths, some might consider them to be
#' conceptually different in some contexts, so we consider the redundancy acceptable.
#' Another case of encoding redundancy *is* that `Alteration - 1` (flatten the 7th) is exactly equivalent to `Signature - 1`.
#' Similarly, `Alteration + 749` (raise the 4th) is exactly equivalent to `Signature + 1`.
#' 
#' 
#' Double-flat and double-sharp degrees are **not** encodable in `diatonicSet`.
#' However, in combination with the `Signature` slot, sets with double-flat/sharps (like doubly-diminished 7ths) can be encoded.
#' 
#' 
#' @section Arithmetic:
#' 
#' Arithmetic between diatonicSets is not defined.
#' However, a number of useful arithmetic operations between diatonicSets and other data types *are* defined:
#' 
#' XXXX Elaborate
#' XXXX Need to implement special logic for adding Alterations! (Taking into account Signature addition.)
#' 
#' 
#' @section Relational Operators:
#' 
#' diatonicSets can be compared using the standard [relational operations][base::Comparison] `==`, and `!=`.
#' Two diatonicSets are equal (according to `==`) only if all their slots (`Root`, `Signature`, and `Alteration`)
#' are exactly identical. 
#' Ordinal comparisons (e.g., `>`, `<=`) between diatonicSets are on their `Signature` only.
#' 
#' 
#' @section Coercion:
#' 
#' `humdrumR` knows how to [coerce](https://en.wikipedia.org/wiki/Type_conversion) several [base-R atomic types][base::vector] into diatonicSets.
#' This can be done using the [as][methods::as] function---e.g., `as(3, "diatonicSet")`---or more intuitively using the function `diatonicSet()`.
#' Coercision methods are defined for 
#' 
#' + [integer][base::integer]: interpreted as root of major key
#' + [numeric][base::numeric]: rounded to nearest integer and intepreted as root of major key
#' + [character][base::character]: interpreted using `humdrumR`s [regular expression dispatch system][humdrumR::regexDispatch], as 
#'   explained fully [here][diatonicRepresentations].
#'   
#' @seealso The main way to create `diatonicSet` S4 objects is with the [diatonicSet()] pitch parser.
#' @family {Tonal S4 classes}
#' @name diatonicSetS4
NULL

## Definition, validity, initialization ####




#' @rdname diatonicSetS4
#' @export 
setClass('diatonicSet', 
         contains = 'struct',
         slots = c(Root = 'integer', 
                   Signature = 'integer', 
                   Alteration = 'integer')) -> diatonicSet




## Constructors ####

#' The basic constructor for `diatonicSet`s.
#' The root argument can accept either an integer (line-of-fifths), a [tonalInterval()], 
#' or a character string which will be coerced to a `tonalInterval`.
#' @rdname diatonicSetS4
#' @export
dset <- function(root = 0L, signature = root, alterations = 0L) {
           if (is.character(root)) root <- tonalInterval.character(root)
           if (is.tonalInterval(root)) root <- root@Fifth
           
           
           if (length(root) == 0L && length(signature) == 0L) alterations <- integer()
           match_size(root = root, signature = signature, alterations = alterations, toEnv = TRUE)
           
           new('diatonicSet', 
               Root = as.integer(root), 
               Signature = as.integer(signature), 
               Alteration = as.integer(alterations)) 
}



## Accessors ####


getRoot <- function(dset){
  checks(dset, xinherits('diatonicSet'))
  dset@Root %<-matchdim% dset
} 

getRootTint <- function(dset) {
    checks(dset, xinherits('diatonicSet'))
    root <- getRoot(dset)
    
    tint( , c(root)) %<-matchdim% dset
    
}

getSignature <- function(dset){
    checks(dset, xinherits('diatonicSet'))
    dset@Signature %<-matchdim% dset
}  

getMode <- function(dset) {
    checks(dset, xinherits('diatonicSet'))
    # mode is sign - root (the signature RELATIVE to the root)
    root <- getRoot(dset)
    sign <- getSignature(dset)
    sign - root
}

getAlterations <- function(dset) {
    # colnames represent the MAJOR degrees
    alterations <- dset@Alteration %<-matchdim% dset

    
    output <- ints2baltern(alterations, 7L) * 7L
    rownames(output) <- NULL
    colnames(output) <- c('4th', 'Root', '5th', '2nd', '6th', '3rd', '7th')
    
    output
    
    
}


## Logic methods ####

### is.methods ####

#' @rdname diatonicSetS4
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

#### Tonal is.methods ####

#' Test the major/minor modality of a set
#' 
#' These functions test the majorness/minorness of a 
#' tertian or diatonic set, a logical `TRUE`/`FALSE`.
#' These functions are not testing whether a chord is strictly
#' a major or minor chord, but rather a "broad" major/minorness:
#' gnerally, the presence of a minor third degree
#' makes a set "minor"; thus, a diminished chord is "minor"
#' and the lydian key is "major."
#'
#' @details 
#' 
#' Either function can be called directly on [tertian][tertianSetS4] or [diatonic][diatonicSetS4] sets.
#' If called on anything else, the functions first call the [tertianSet()]
#' parser. If any values fail to parse (returning `NA`), the [diatonicSet()]
#' parser is called on them.
#' 
#' @param x ***Input data, interpreted as diatonic keys or chords.***
#' 
#' Must be a `diatonicSet` or `tertianSet` or something that can be parsed as one.
#' 
#' @param ... ***Parameters passed to the parsers ([tertianSet()] and [diatonicSet()]).***
#' 
#' @family {Tonal feature functions}
#' @name is.major
#' @export 
is.major <- function(x, ...) UseMethod('is.major')
#' @rdname is.major
#' @export
is.minor <- function(x, ...) UseMethod('is.minor')



#' @rdname is.major
#' @export
is.major.diatonicSet <- function(x) getMode(x) >= -1L
#' @rdname is.major
#' @export
is.minor.diatonicSet <- function(x) getMode(x) < -1L




## Order/relations methods ####

#' `diatonicSets` methods for [order][base::order()] and 
#' [sort][base::sort()] order/sort along the circle of LO5ths.
#' Signatures are sorted secondarily from fewest flats to most sharps.
#' If `parallel = TRUE` all modes are grouped by shared tonics, so
#' C minor and C major will appear besides each other.
#' If `parallel = FALSE` modes/keys are sorted together by number of accidentals,
#' so C minor and Eb major will be sorted next to each other.
#' 
#' @rdname diatonicSetS4
#' @export
order.diatonicSet <- function(x, ..., parallel = TRUE, na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
                    x <- do.call('c', list(x, ...))
                    if (parallel) {
                      order(x@Root, -x@Signature,
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)
                    } else {
                      order(x@Root - x@Signature, -x@Signature,
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)

                    }
          }

#' @rdname diatonicSetS4
#' @export
setMethod('==', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
             checkSame(e1, e2, "==")
             f1 <- LO5th(e1)
             f2 <- LO5th(e2)
              
             rowSums(f1 == f2, na.rm = TRUE) == 7L
          })




setMethod('abs', signature = c('diatonicSet'),
          \(x) {
              .ifelse(x@Root < 0, -x, x)
          })

#' @rdname diatonicSetS4
#' @export
setMethod('Compare', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
              checkSame(e1, e2, "relational comparison")
              callGeneric(getSignature(e1), getSignature(e2))
          })



## Arithmetic methods ####

### Addition ####


### Division/modulo  ####

#' @export
setMethod('%%', signature = c('integer', 'diatonicSet'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
              alter <- e2@Alteration
              signature <- getSignature(e2)
              
              output <- integer(length(e1))
              
              hits <- !is.na(alter) & alter == 0L
              output[hits] <- (((e1[hits] + 1L) - signature[hits]) %% 7L) - 1 + signature[hits]
              if (any(!is.na(alter) & alter != 0L)) {
                  output[!is.na(alter) & alter != 0L] <- {
                      lof <- t(LO5th(e2[!is.na(alter) & alter != 0L]))
                      lof[sweep(lof %% 7L, 2, e1[!is.na(alter) & alter != 0L] %% 7L, `==`)]
                  }
              }
              
    
              as.integer(output) 
              
          })


#' @export
setMethod('%%', signature = c('tonalInterval', 'diatonicSet'),
          function(e1, e2) {
              indim <- dim(e1)
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              fifth <- getFifth(e1)
              
              simple <-  fifth %% e2
              
              octdiff <- ((simple - fifth) * 19) / 12
              
              tint(round(e1@Octave - octdiff), simple) %<-matchdim% e1
              
              
          })

#' @export
setMethod('%%', signature = c('character', 'diatonicSet'),
          function(e1, e2) {
            e1 <- tonalInterval.character(c(e1), inPlace = TRUE)
            
            e3 <- e1 %% e2
            dispatch <- attr(e1, 'dispatch')
            
            rePlace(reParse(e3, dispatch, c('kern', 'pitch', 'solfa', 'interval', 'degree')),  dispatch)
          })


#' @export
setMethod('%%', signature = c('matrix', 'diatonicSet'),
          function(e1, e2) {
            (c(e1) %% c(e2)) %<-matchdim% e1
            
            
          })


#' @export
setMethod('-', signature = c('tonalInterval', 'diatonicSet'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              e1 - getRootTint(e2)
              
          })


#' @export
setMethod('+', signature = c('tonalInterval', 'diatonicSet'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              e1 + getRootTint(e2)
              
          })

#' @export
setMethod('+', signature = c('diatonicSet', 'tonalInterval'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              lof <- e2@Fifth
              
              dset(getRoot(e1) + lof, getSignature(e1) + lof, e1@Alteration)
              
          })

#' @export
setMethod('+', signature = c('diatonicSet', 'integer'),
          function(e1, e2) {
            match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            e1@Root <- e1@Root + e2
            e1@Signature <- e1@Signature + e2
            e1
          })


#' @export
setMethod('-', signature = c('diatonicSet', 'integer'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              e1@Root <- e1@Root - e2
              e1@Signature <- e1@Signature - e2
              e1
          })


#' @export
setMethod('+', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
            match_size(e1 = e1, e2 = e2, toEnv = TRUE)
            
            e1@Root <- e1@Root + e2@Root
            e1@Signature <- e1@Signature + e2@Signature
            e1
            
          })

###################################################################### ###
# Deparsing Key Representations (dset2x) #################################
###################################################################### ###


## Deparsing (diatonicSet) documentation ----

#' Generating ("deparsing") key representations
#' 
#' [humdrumR] includes a easy-to-use system for 
#' generating a variety of diatonic key representations,
#' which can be flexibly modified by users.
#' "Under the hood" `humdrumR` represents all tonal chord information using the [same underlying representation][diatonicSetS4],
#' which is typically extracted from input data using the [key parser][keyParsing].
#' This representation can then be "deparsed" into a variety of predefined output formats, 
#' or into new formats that you create!
#' 
#' Deparsing is the second step in the [key function][keyFunctions] processing pipeline:
#' 
#' + **Input** representation `|>` 
#'   + *Parsing* `|>`
#'     + **Intermediate** ([diatonicSet][diatonicSetS4]) representation `|>`
#'     + **Transformation**  `|>`
#'   + *Deparsing* (DEPARSING ARGS GO HERE) `|>`
#' +  **Output** representation 
#' 
#' Various pitch representations can be generated using predefined [key functions][keyFunctions] like [key()]
#' [signature()], and [romanKey()].
#' All of these functions use a common deparsing framework, and are specified using different combinations of arguments
#' to the deparser.
#' By modifying these *"deparsing" arguments*, you can exercise 
#' fine control over how you want pitch information to be represented in your output.
#' 
#' @seealso All `humdrumR` [key functions][keyFunctions] make use of the deparsing functionality.
#' @name keyDeparsing
NULL

## Key deparsers ####

### Key representations ####  




dset2alterations <- function(dset, augment = '#', diminish = 'b', ...) {

    mode <- getMode(dset)
    
    altered <- !is.na(dset) & dset@Alteration != 0L & mode > -7L & mode < 2L
    
    alterations <- getAlterations(dset)[altered, , drop = FALSE]
    alterations[] <- c(augment, diminish, "")[match(alterations, c(7, -7, 0))]

    order <- lapply(mode[altered] %% 7L, \(m) ((0L:6L + m) %% 7L) + 1 )
        
    labs <- do.call('rbind', lapply(order, \(ord) c('4', '1', '5', '2', '6', '3', '7')[ord]))
    labs[alterations == ''] <- ''

    alterations[] <- paste0(alterations, labs)
    
    alterations <- apply(alterations, 1, paste, collapse = '')
    output <- character(length(mode))
    output[altered] <- alterations
    output


}

dset2modelabel <- function(dset, ...) {
    mode <- getMode(dset)
    .ifelse(mode == 0L | mode == -3L,
            "",
            LO5th2mode(mode, short = TRUE))
    
}

LO5th2mode <- function(LO5th, short = FALSE) {
  
  known <- LO5th > -7L & LO5th < 2L & !is.na(LO5th)
  
  LO5th <- LO5th %% 7L
  
  fullname <- rep('?', length(LO5th))
  modes <- c('major', 'lydian',  'locrian', 'phyrgian', 'minor', 'dorian', 'mixolydian')
  fullname[known] <- modes[LO5th[known] + 1]
  
  if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}



dset2signature <- function(x, Key = NULL, ...) {
    if (!is.null(Key)) x <- x + diatonicSet(Key %||% dset(0, 0))
  
    LO5ths <- LO5th(x)
    LO5ths[] <- t(apply(LO5ths, 1, \(row) row[order(sign(row), (abs(row) + ifelse(row > 1, 1L, -2L)) %% 7L)])) 
    # this puts accidentals in absolute ascending order, but putting double flats/sharps in the right place
    tints <- tint( , LO5ths) %<-matchdim% NULL
    
    notes <- tint2tonalChroma(tints, parts = c('step', 'species'),
                              flat = '-', qualities = FALSE, Key = x(0, 0),
                              step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b')) %<-matchdim% LO5ths
    
    notes[LO5ths <= 5L & LO5ths >= -1L] <- ""
    
    notes <- apply(notes, 1, paste, collapse = '')
        
    .paste("k[", notes, ']')
}




dset2key <- function(x, Key = NULL, ...) {
    if (!is.null(Key)) x <- x + getRootTint(Key)
    
    root <- tint2kern(tint( , getRoot(x)))
    mode <- getMode(x)
    root[!is.na(mode) & mode %in% c(1L, 0L, 6L, -1L)] <- toupper(root[!is.na(mode) & mode %in% c(1L, 0L, 6L, -1L)])
    
    modelab <- dset2modelabel(x) 
    
    alterations <- dset2alterations(x, ...)
    
    key <- .paste("*", root, ":", modelab, alterations) 
    
    #
    key
}



#' Roman Numeral
#' 
#' Roman numerals can be calculated for diatonicSets (keys) and 
#' for tertian sets (chords).
#' The later case is the standard meaning of "roman numeral."
#' However, the former case is used as well, for instance
#'  to represent modulation schemes in 
#' analyses of classical music. For instance, modulate from I-V,
#' the to vi/V.
#' More importantly, many "roman numerals" in harmonic analyses
#' implicitely combine tertian and diatonic roman numerals:
#' in "applied" roman numerals.
#' Given a roman numeral like "V65/V", the "V65" represents a
#' chord while the "/V" represents a key.
#'
#' @name romanNumerals
NULL

dset2romanNumeral <- function(x, flat = 'b', Key = NULL, ...) {
  
    tint <- getRootTint(x)
    
    numeral <- tint2romanRoot(tint, flat = flat, ...)
    
    mode <- getMode(x)
    numeral[mode <= -2L] <- tolower(numeral[mode <= -2L])
    
    modelab <- dset2modelabel(x) 
    
    alterations <- dset2alterations(x)
    
    out <- .paste(numeral, modelab, alterations)

    out
   
}



###################################################################### ###
# Parsing Key Representations (x2dest) ###################################
###################################################################### ###


## Parsing (diatonicSet) documentation ----


#' Parsing key information
#' 
#' [humdrumR] includes a easy-to-use but powerful system for *parsing* diatonic key information:
#' various basic key representations (including `numeric` and `character`-string representations) can be "parsed"---read
#' and interpreted by `humdrumR`.
#' For the most part, parsing automatically happens "behind the scenes" whenever you use any humdrumR [key function][keyFunctions], like [key()]
#' or [signature()].
#' 
#' @seealso All `humdrumR` [key functions][keyFunctions] make use of the deparsing functionality.
#' @name keyParsing
NULL


## Key parsers ####

### Key representations ####  

qualities2dset <-  function(x, steporder = 2L, allow_partial = FALSE, 
                            major = 'M', minor = 'm', augment = '+', diminish = 'o', perfect = 'P', ...) {
    
    
    # modes are the 7 13th-chord/modes in L05th order
    modes <- list(c(perfect, perfect, major, major, major, major, augment),
                  c(perfect, perfect, major, major, major, major, perfect),
                  c(perfect, perfect, major, major, major, minor, perfect),
                  c(perfect, perfect, major, minor, major, minor, perfect),
                  c(perfect, perfect, major, minor, minor, minor, perfect),
                  c(perfect, perfect, minor, minor, minor, minor, perfect),
                  c(perfect, diminish, minor, minor, minor, minor, perfect))

    modes_int <- 1L:-5L
    names(modes) <- names(modes_int) <- sapply(modes, paste, collapse = '')
    
    modes_int <- modes_int[c(2,3,5,4,1,6,7)] # reorder to prefer mixo > major >  minor, etc.     
    modes <- modes[names(modes_int)]
    
    ####
    if (steporder != 1L) {
      x <- strsplit(x, split = '')
      ord <- order(seq(0, by = steporder, length.out = 7L) %% 7L)
      x <- sapply(x, \(s) paste(s[ord], collapse = ''))
    }
    
    mode <- modes_int[x]
    if (allow_partial) {
      mode[is.na(mode)] <- sapply(paste0('^', x[is.na(mode)]), 
                                  \(x) modes_int[which(stringr::str_detect(names(modes_int), escape(x)))[1]], USE.NAMES = FALSE)
    } 
    alterations <- integer(length(x))
    if (any(is.na(mode))) {
      altered <- is.na(mode)
      quality.labels <- escape(c(diminish, minor, perfect, major, augment)) # reorder for rank
      modes <- do.call('cbind', modes)
      
      mode_alterations <- lapply(strsplit(x[altered], split = ''),
                                 \(qualities) {
                                   qualities <- escape(qualities)
                                   hits <- qualities == modes[1L:length(qualities), ]
                                   
                                   # only want to alter 1 5 or 3 as last resort
                                   if (any(hits[1, ])) hits[ , !hits[1, ]] <- FALSE
                                   if (any(hits[2, ])) hits[ , !hits[2, ]] <- FALSE
                                   if (any(hits[5, ])) hits[ , !hits[5, ]] <- FALSE
                                   
                                   # which is closest mode
                                   pick <- which.max(colSums(hits))
                                   mode <- modes_int[pick] 
                                   #
                                   altered <- !hits[, pick] & (!allow_partial | qualities != '.')
                                   supposedtobe <- modes[altered, pick]
                                   actual <- qualities[altered]
                                     
                                   # what direction are they altered?
                                   change <- ifelse(which(altered) %in% c(1L, 2L, 7L), # Perfects
                                                    match(actual, quality.labels[-c(2, 4)]) - match(supposedtobe, quality.labels[-c(2, 4)]), # no M or m
                                                    match(actual, quality.labels[-3]) - match(supposedtobe, quality.labels[-3])) # no P
                                   if (any(abs(change) > 1L)) change <- sign(change)
                                   
                                   altermat <- matrix(0L, nrow = 1, ncol = 7)
                                   altermat[((which(altered) - mode) %% 7L) + 1L] <- change
                                     
                                   alterint <- baltern2int(altermat)
                                   c(mode = mode, altered = alterint)
                                 }) |> do.call(what = 'rbind')
      mode[altered] <- mode_alterations[ , 1]
      alterations[altered] <- mode_alterations[ , 2]
      
    }
    
    dset(root = 0, signature = mode, alterations = alterations )
    
}


alteration2trit <- function(x, mode = integer(length(x)), sharp = '#', flat = 'b', ...) {
    
    accidentalRE <- captureUniq(c(sharp, flat), zero = TRUE)
    
    x <- stringr::str_replace(x, '13', '6')
    x <- stringr::str_replace(x, '11', '4')
    x <- stringr::str_replace(x, '10', '3')
    x <- stringr::str_replace(x,  '9', '2')
    
    hits <- x != ''
    # degrees
    degrees <- stringr::str_extract_all(x[hits],   paste0(accidentalRE, '[1234567]'))
    
    acc <- lapply(degrees, stringr::str_extract, accidentalRE)
    acc <- lapply(acc, \(acc) specifier2tint(acc, qualities = FALSE, 
                                                    sharp = sharp, flat = flat, ...)@Fifth) 
    degrees <- lapply(degrees, stringr::str_remove, accidentalRE)
    
    alterations <- matrix(0, nrow = length(x), ncol = 7)
    
    degrees <- data.frame(Accidentals = unlist(acc), 
                          Degrees = unlist(degrees),
                          Row = rep(seq_len(sum(hits)), lengths(acc)))

    alterations[cbind(which(hits)[degrees$Row], match(degrees$Degrees, c(4, 1, 5, 2, 6, 3, 7, 4)))] <- degrees$Accidentals 
    alterations[] <- alterations %/% 7L
    
    ## rotate to appropriate mode
    order <- lapply(-mode %% 7L, \(m) ((0L:6L + m) %% 7L) + 1 )
    threes <- do.call('rbind', lapply(order, \(ord) (3^(6:0))[ord]))
    
    rowSums(threes * alterations)
    
    
}


##... from key signature

signature2dset <- function(x, Key = NULL, signature.mode = 0L, ...) {
    if (!is.null(Key)) dset <- dset + Key
  
    x <- gsub('^\\*', '', x)
    signotes <- stringr::str_extract_all(x, '[a-g]([#-n])\\1*')
    
    sigs <- integer(length(x))
    
    empty <- lengths(signotes) == 0L
    
    lof <- lapply(signotes[!empty], 
                  \(notes) {
                      lof <- kern2tint(notes)@Fifth
                      lof[lof < -1L | lof > 5L]
                      })
    empty[!empty] <- lengths(lof) == 0L
    lof <- lof[lengths(lof) > 0L]
    
    
    sharp <- sapply(lof, mean) > 0
    
    altered <- unlist(Map(\(fs, bound)  any(diff(sort(c(bound, fs))) > 1), 
                          lof, 
                          c(-1, 5L)[sharp + 1L]))
    
    
    ranges <- sapply(lof, range)
    
    sigs[!empty] <- .ifelse(sharp, ranges[2, ] - 5, ranges[1, ] + 1)
    
    dsets <- dset(sigs - signature.mode, sigs)
    
    
    #
    if (any(altered)) {
        
        alterations <- do.call('rbind',
                               Map( 
                                   \(fth, sig) {
                                       alt <- unalt <- -1L:5L + sig
                                       natural <- alt > -2L & alt < 6L
                                       alt[!natural & !alt %in% fth] <-  alt[!natural & !alt %in% fth] + 7L
                                       (alt - unalt) %/% 7L
                                   }, 
                                   lof[altered], sigs[altered]))
        
        
        # if root is altered
        rootqual <- alterations[cbind(1:nrow(alterations), 2 - signature.mode)]
        
        alterations <- as.integer(rowSums(sweep(alterations, 2, 3L^(6L:0L), `*`)))
        
        if (any(rootqual != 0L)) {
           signature.mode[rootqual != 0L] <- signature.mode[rootqual != 0L] - 1L
        }
        dsets <- dset(sigs - signature.mode, sigs, alterations)
        
        # dsets@Alteration[altered] <- alterations
    }
    dsets
    
}


##... From key interpretation


key2dset <- function(x, parts = c('step', 'species', 'mode', 'alterations'), 
                     step.labels = c('C', 'D','E','F','G','A','B'),
                     Key = NULL, keyed = TRUE,
                     ...) {
    
   
  
    # str <- stringr::str_remove(str, '^\\*')
    if (!is.null(Key)) Key <- diatonicSet(Key)
  
    REs <- makeRE.key(..., parts = parts, step.labels = step.labels, collapse = FALSE)
    REparse(x, REs, parse.strict = FALSE, parse.exhaust = FALSE, toEnv = TRUE)
    
    # Root
    root <- local( {
      generic <- step2tint(toupper(step), step.labels = toupper(step.labels))
      specifier <- specifier2tint(species, step = generic, useKey = TRUE, Key = CKey(Key), ..., qualities = FALSE)
      LO5th(generic + specifier)
      })
    
    # Signature
    minor <- stringi::stri_detect_regex(step, '[a-g]|[iv]') * -3L
    mode <- .ifelse(mode == "", 0 , c(dor = +1, mix = -1, lyd = +1, phr = -1, loc = -2)[mode])
    signature <- root + mode + minor
    
    ## Alterations
    alterations <- alteration2trit(alterations, mode + minor) %|% 0
    
    dset <- dset(root, signature, alterations)
    
    if (keyed && !is.null(Key)) dset <- dset - getRootTint(Key)
    
    # if (!is.null(of) && Key != dset(0, 0)) {
    #   of <- CKey(diatonicSet(Key))
    #   alter <- dset@Root - (dset@Root %% Key)
    #   dset@Root <- dset@Root - alter
    #   dset@Signature <- dset@Signature - alter
    # }
    
    
    dset
    
}


##... From roman numeral



romanNumeral2dset <- function(x, Key = NULL, flat = '-', sep = '/', ...) {
  REparse(x, makeRE.romanKey(..., sep = sep, collapse = FALSE), toEnv = TRUE) # makes head rest and base
  
  parts <- strPartition(paste0(head, rest), split = sep)
  
  Key <- if (is.null(Key)) dset(integer(length(x)), 0L) else rep(diatonicSet(Key), length.out = length(x))
  
  parts[] <- head(Reduce(\(x, y) {
    na <- is.na(x)
    y[!na] <- key2dset(x[!na], c('species', 'step', 'mode', 'alterations'), 
                       step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                       flat = flat, keyed = FALSE,
                       Key = y[!na], ...)
    y
    
    }, right = TRUE, init = Key, parts, accumulate = TRUE), -1) 
  
  dset <- parts$base
  if (length(parts) > 1L) {
    of <- Reduce('+', lapply(parts[ , colnames(parts) == 'of', drop = FALSE], getRoot))
    dset + dset(of, of, 0L)
  }  else {
    dset
  }
}



##... Numbers

integer2dset <- \(x) dset(x, x)


## Key Parsing Dispatch ######################################

### Parse 2dset generic and methods ####

#' @rdname keyParsing
#' @export
diatonicSet <- function(...) UseMethod('diatonicSet')

#' @rdname keyParsing
#' @export
diatonicSet.diatonicSet <- function(x, ...) x

#' @rdname keyParsing
#' @export 
diatonicSet.logical <- function(x, ...) vectorNA(length(x), 'diatonicSet')

#' @rdname keyParsing
#' @export
diatonicSet.NULL <- function(x, ...) dset(c(), c())


#### Numbers ####

#' @rdname keyParsing
#' @export
diatonicSet.numeric <- \(x) integer2dset(as.integer(x))

#' @rdname keyParsing
#' @export
diatonicSet.integer <- integer2dset

#### Characters ####





#' @rdname keyParsing
#' @export
diatonicSet.character <- makeHumdrumDispatcher(list('any', makeRE.romanKey,  romanNumeral2dset),
                                               list('any', makeRE.key,       key2dset),
                                               list('any', makeRE.signature, signature2dset),
                                               funcName = 'diatonicSet.character',
                                               outputClass = 'diatonicSet')





#### setAs diatonic set ####

setAs('integer', 'diatonicSet', function(from) integer2dset(from))
setAs('numeric', 'diatonicSet', function(from) integer2dset(from))
setAs('character', 'diatonicSet', function(from) {
  output <- dset(rep(NA, length(from)))
  if (any(!is.na(from))) output[!is.na(from)] <- diatonicSet.character(from[!is.na(from)])
  output
})
setAs('matrix', 'diatonicSet', function(from) diatonicSet(c(from)) %<-matchdim% from)
setAs('logical', 'diatonicSet', function(from) dset(rep(NA, length(from))) %<-matchdim% from)


###################################################################### ### 
# Translating Key Representations (x2y) ##################################
###################################################################### ### 

## Key function documentation ####


#' Parsing and deparsing key information
#' 
#' These functions can be used to extract and "translate," or otherwise modify, data representing diatonic key information.
#' The functions are:
#' 
#' + [key()]
#' + [romanKey()]
#' + [signature()]
#' 
#' @seealso To better understand how these functions work, read about how diatonic keys are 
#' [represented][diatonicSet], [parsed][keyParsing], and [deparsed][keyDeparsing].
#' @name keyFunctions
NULL

## Key transform maker ####



makeKeyTransformer <- function(deparser, callname, outputClass = 'character') {
  # this function will create various pitch transform functions
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  args <- alist(x = , 
                ... = , 
                Key = NULL, 
                parseArgs = list())
  
  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr({
    
    checks(x, xatomic | xclass('diatonicSet'))
    
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    c('args...', 'parseArgs') %<-% specialArgs(rlang::enquos(...), 
                                               parse = parseArgs)
    formalArgs <- list(!!!fargcall)
    namedArgs <- formalArgs[.names(formalArgs) %in% .names(as.list(match.call())[-1])]
    # There are four kinds of arguments: 
    # ... arguments (now in args...), 
    # FORMAL arguments, if specified (now in namedArgs)
    # parseArgs
    # transposeArgs
    
    # Exclusive
    parseArgs$Exclusive <- parseArgs$Exclusive %||% args...$Exclusive 
    
    # parseArgs   <- pitchArgCheck(parseArgs, !!callname)
    # deparseArgs <- pitchArgCheck(c(args..., namedArgs), !!callname)
    deparseArgs <- c(args..., namedArgs)
    
    # Key
    Key     <- diatonicSet(Key %||% dset(0L, 0L))
    deparseArgs$Key <- Key
    parseArgs$Key <- Key
    
    # memoize % deparse
    memoize <- args...$memoize %||% TRUE
    deparse <- args...$deparse %||% TRUE
    
    # Parse
    parsedDset <- do(diatonicSet, c(list(x, memoize = memoize), parseArgs), memoize = memoize, outputClass = 'diatonicSet')
    
    deparseArgs <- c(list(parsedDset), deparseArgs)
    
    output <- if (deparse && is.diatonicSet(parsedDset))  do(!!deparser, deparseArgs, 
                                                             memoize = memoize, 
                                                             outputClass = !!outputClass) else parsedDset
    
    output
  }))
}

### Key functions ####

#' @param x ***Input data, interpreted as diatonic keys.***
#' 
#' Must be an `atomic` vector.
#' 
#' @param Key ***The key used by the parser, deparser, and transposer.***
#' 
#' Defaults to `NULL`.
#' 
#' Must be a `diatonicSet` or something coercable to `diatonicSet`; must be either length `1` or `length(x)`
#' 
#' @param parseArgs ***An optional list of arguments passed to the [key parser][keyParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [key parser][keyParsing].
#' 
#' @name keyFunctions
NULL

#' Humdrum key interpretation
#' 
#' @examples
#' 
#' key(c('I', 'ii', 'ii:dor', 'v', '-vi', 'V/V', 'ii/V'))
#' 
#' 
#' @inheritParams keyFunctions
#' @export
key <- makeKeyTransformer(dset2key, 'key')

#' Humdrum key signature
#' 
#' @examples
#' 
#' signature(c('I', 'ii', 'ii:dor', 'v', '-vi', 'V/V', 'ii/V'))
#' 
#' signature(c('C:', 'c:', 'A-:', 'a-:', 'E:'))
#' 
#' @inheritParams keyFunctions
#' @export
signature <- makeKeyTransformer(dset2signature, 'signature')

#' Roman numeral key areas
#' 
#' @examples
#'
#' romanKey(c('C:', 'd:dor', 'G:', 'g:'))
#' 
#' @inheritParams keyFunctions
#' @export
romanKey <- makeKeyTransformer(dset2romanNumeral, 'romanKey')

###################################################################### ### 
# Manipulating diatonic sets #############################################
###################################################################### ### 


## Extracting pitches ----


### Line of Fifths ####

#' @export
setMethod('LO5th', 'diatonicSet',
          function(x, steporder = 2L, inversion = 0L) {
            # the steporder argument controls the order the LO5ths are output
            # steporder = 2L means every two LO5ths (which is generic steps)
            # steporder = 4L means thirds, which makes tertian harmonies
            dset <- x
            root <- getRoot(dset)
            sign <- getSignature(dset)
            alter <- getAlterations(dset)
            
            notna <- !is.na(sign) & !is.na(root)
            inversion <- rep(inversion, length.out = length(x))
            
            scale <- ((0:6) * steporder ) %% 7L
            scales <- outer(root, scale, '+')
            
            scales <- sweep((sweep(scales, 1, sign - 1L, '-')) %% 7L, 1, sign - 1L, '+') + alter[ , match(scale + 1L, c(7L, 1L:6L)), drop = FALSE]
            scales[ , 1L] <- root_inkey <- ((root - sign + 1L) %% 7L) + sign - 1L
            
            if(any(inversion != 0L, na.rm = TRUE)) scales[] <- scales[cbind(c(row(scales)), c(1 + (sweep(col(scales) - 1L, 1, inversion, '+') %% 7L)))]
            
            rownames(scales) <- dset2key(dset)
            
            scales
            ### get line-of-fifths values
            # LO5ths <- split(outer(sign, -1L:5L, '+') + alter, f = seq_along(sign))
            
            ### reorder (root/inversion/steporder)
            # root_inkey <- ((root - sign + 1L) %% 7L) + sign - 1L # normalize into signature (in case root is outside signature)
            
            # order <- lapply(lapply(root_inkey[notna] + steporder * inversion[notna], seq, by = steporder, length.out = 7L), `%%`, e2 = 7L)
            # LO5ths[notna] <- Map(\(lo5th, ord) lo5th[match(ord, lo5th %% 7)], LO5ths[notna], order)
            
            # LO5ths <- do.call('rbind', Map(\(r,i, inv) LO5ths[i, match(seq(r + steporder * inv, by = steporder, length.out = 7L) %% 7L, LO5ths[i, ] %% 7L, )], 
            # root %% 7L, 1:nrow(LO5ths), inversion))
            # LO5ths <- do.call('rbind', LO5ths)
            # LO5ths[cbind((1:nrow(LO5ths))[notna], 1L + ((7L - inversion[notna]) %% 7L))] <- root[notna]
            
            # colnames(LO5ths) <- c('Root', nthfix(c(5, 2, 6, 3, 7, 4)))[(seq(0L, by = as.integer(steporder), length.out = 7L) %% 7L) + 1L]
            # 
            # LO5ths
          })


### Tonal intervals ####    



dset2pitcher <- function(pitch.func) {
  pitch.func <- rlang::enexpr(pitch.func)
  body <- rlang::expr({
    LO5ths <- LO5th(x)
    
    tints <- tint( , LO5ths)
    (!!pitch.func)(tints, ...)
    
  })
  
  rlang::new_function(alist(x = , ... = ), body, parent.env(environment()))
}

dset2tonalChroma <- dset2pitcher(tint2tonalChroma)



###################################################################### ### 
# Predefined diatonicSets ################################################
###################################################################### ### 

#' @export Eflatmajor Cminor Asharpminor Fdorian Dsharpdorian Bflatmixolydian Gsharpmixolydian Eflatlydian Cphrygian Asharpphrygian Flocrian Dsharplocrian 
#' @export Amajor Fflatmajor Dminor Bsharpminor Gdorian Esharpdorian Cflatmixolydian Alydian Fflatlydian Dphrygian Bsharpphrygian Glocrian Esharplocrian 
#' @export Bmajor Gflatmajor Eminor Csharpminor Aflatdorian Fsharpdorian Dflatmixolydian Blydian Gflatlydian Ephrygian Csharpphrygian Aflatlocrian Fsharplocrian 
#' @export Cmajor Asharpmajor Fminor Dsharpminor Bflatdorian Gsharpdorian Eflatmixolydian Clydian Asharplydian Fphrygian Dsharpphrygian Bflatlocrian Gsharplocrian 
#' @export Dmajor Bsharpmajor Gminor Esharpminor Cflatdorian Amixolydian Fflatmixolydian Dlydian Bsharplydian Gphrygian Esharpphrygian Cflatlocrian 
#' @export Emajor Csharpmajor Aflatminor Fsharpminor Dflatdorian Bmixolydian Gflatmixolydian Elydian Csharplydian Aflatphrygian Fsharpphrygian Dflatlocrian 
#' @export Fmajor Dsharpmajor Bflatminor Gsharpminor Eflatdorian Cmixolydian Asharpmixolydian Flydian Dsharplydian Bflatphrygian Gsharpphrygian Eflatlocrian 
#' @export Gmajor Esharpmajor Cflatminor Adorian Fflatdorian Dmixolydian Bsharpmixolydian Glydian Esharplydian Cflatphrygian Alocrian Fflatlocrian 
#' @export Aflatmajor Fsharpmajor Dflatminor Bdorian Gflatdorian Emixolydian Csharpmixolydian Aflatlydian Fsharplydian Dflatphrygian Blocrian Gflatlocrian 
#' @export Bflatmajor Gsharpmajor Eflatminor Cdorian Asharpdorian Fmixolydian Dsharpmixolydian Bflatlydian Gsharplydian Eflatphrygian Clocrian Asharplocrian 
#' @export Cflatmajor Aminor Fflatminor Ddorian Bsharpdorian Gmixolydian Esharpmixolydian Cflatlydian Aphrygian Fflatphrygian Dlocrian Bsharplocrian 
#' @export Dflatmajor Bminor Gflatminor Edorian Csharpdorian Aflatmixolydian Fsharpmixolydian Dflatlydian Bphrygian Gflatphrygian Elocrian Csharplocrian 
# 
# allkeys <- expand.grid(Step = LETTERS[1:7], stringsAsFactors = FALSE,
#                        Accidental = c('', 'flat', 'sharp'),
#                        Mode = c('major', 'minor', 'dorian', 'mixolydian', 'lydian', 'phrygian', 'locrian'))
# allkeys <- within(allkeys, Handle <- paste0(Step, Accidental, Mode))
# allkeys$Step <- ifelse(allkeys$Mode %in% c('minor', 'dorian', 'phrygian', 'locrian'), tolower(allkeys$Step), allkeys$Step)
# allkeys$Mode <- ifelse(allkeys$Mode %in% c('major', 'minor') , '', substr(allkeys$Mode, 1, 3))
#                       
# 
# allkeys$Accidental <- c('', '-', '#')[match(allkeys$Accidental, c('', 'flat', 'sharp'))]
#                       
# allkeys <- within(allkeys, String <- paste0('*', Step, Accidental, ':', Mode))
# 
# 
# for (i in 1:nrow(allkeys)) {
#     assign(allkeys$Handle[i], key2dset(allkeys$String[i]))
# }
