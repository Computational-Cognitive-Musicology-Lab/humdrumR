##################################
###### diatonicSet S4 class   ####
##################################

##### class methods ####

####. definition, validity, initialization ####


#' Tonal (diatonic) sets
#' 
#' 
#' `diatonicSet` is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' types of tonal data, representing Western diatonic keys.
#' For the most part, users should not need to interact with `diatonicSet`s directly---rather, `diatonicSet`s work behind the scene in numerous `humdrumR` pitch functions.
#' See the [keyRepresentations] and [keyTransformations] documentation for details of usage and functionality or the *Tonality in humdrumR* vignette for 
#' a detailed explanation of the theory and specifics of `diatonicSet`s.
#' 
#' @details
#' 
#' `diatonicSet` is a [S4](http://adv-r.had.co.nz/S4.html) subclass of `humdrumR`'s virtual class [struct], 
#' from which it inherits a lot of useful "vector-like" behaviors/functionality.
#' 
#' 
#' The constructor function `dset` can be used to create `diatonicSets` directly.
#' The three arguments corespond to the three slots: `root`, `mode`, and `alteration`.
#' All inputs will be coerced to match in length.
#' The `root` argument will attempt to coerce character strings to [tonalIntervals][tonalInterval], and use their `LO5th` value as the root.
#' 
#' By default, the [as.character][base::character] method, and thus (via [struct]) the [show][methods::show] method,
#'  for `diatonicSet`s call [as.keyI()][diatonicRepresentations].
#' Thus, if you return a `diatonicSet` on the command line (or call [print][base::print] one one), 
#' you'll see the [key interpretation][diatonicRepresentations] representation printed.
#' 
#' @slot Root integers representing the root of the key on the line-of-fifths
#' @slot Signature integers representing the signature (number of accidentals) of the key. 
#' @slot Alteration integers representing alterations of the diatonic set
#' 
#' A key is represented by two integers, \code{Root} and \code{Signature}.
#' Root is simply the tonic note of the key on the circle of fifths.
#' Signature is a value on the circle of fifths, indicating the diatonic mode.
#' You can think of the Signature value as indicating the number of accidentals, with negative numbers
#' for flats and positive numbers for sharps.
#' You can also think of the signature as indicating how much the "natural key" (C major) is
#' slid up and down the line-of-fifths.
#' The standard diatonic modes occur if the \code{Signature - Tonic} is in the range -5:1:
#' 
#' + +1 = Lydian
#' + +0 = Major (Ionian)
#' + -1 = Mixolydian
#' + -2 = Dorian
#' + -3 = Minor (Aeolian)
#' + -4 = Phyrgian
#' + -5 = Locrian
#' 
#' > Note that you can make diatonicSets where the `Root` is outside the `Key`.
#' > This is unusual, and may resort in sets you wouldn't predict.
#' 
#' @section Alterations:
#' 
#' The `Alteration` slot (also integers) can be used to represent various 
#' "altered" scales. To understand how the `Alteration` integer works, first consider how the `Signature` (key-signature)
#' integer works.
#' Think of it like this, we start with a natural diatonic set consisting of the numbers `[-1 0 1 2 3 4 5]` (C major) on the line of fifths:
#' If the `Signature` integer is `+1`, everything is shifted up one to be `[0 1 2 3 4 5 6]` (C Lydian).
#' You *can* think of this as `+1` being added to each number, but instead, think of it as the following operation:
#' remove the lowest (leftmost number) from the vector, add 7 to that number, then append it on the rightmost side.
#' If we follow this operation, we take 0 off the left, and add 7 to the end, getting `[0 1 2 3 4 5 6]`.
#' If `Signature` is greater than one, we repeat this operation however many times, and if `Signature` is negative, we just reverse the procedure.
#' This way of thinking about the `Signature` value is convoluted, but it helps us understand the `Alteration` operation.
#' 
#' The `Alteration` integer does the same operation on a key as `Signature`, except we take the *second*-most left/right value and add/subtract 7.
#' So if `Alteration == -1`, start with `[-1 0 1 2 3 4 5]`, take the `4`, subtract `7`, and append it to the left side to get `[-3 -1 0 1 2 3 5]` (C melodic minor);
#' If `Alteration == -2`, `[-1 0 1 2 3 4 5]` becomes `[-4 -3 -1 0 1 2 5]` (C harmonic minor); and so on.
#' `Alteration == -1` results in all diatonic sets which are modes of the *melodic minor* scale, and 
#' `Alteration == -2` results in diatonic sets which are modes of the *harmonic minor* scale.
#' Other `Alteration` values get us increasingly exotic scales!
#' 
#' 
#' 
#' @section Arithmatic:
#' 
#' Arithmetic between `diatonicSet`s is not defined.
#' However, a number of useful arithmetic operations between `diatonicSet`s and other data types *are* defined:
#' 
#' XXXX Elaborate
#' 
#' 
#' @section Relational Operators:
#' 
#' `diatonicSet`s can be compared using the standard [relational operations][base::Comparison] `==`, and `!=`.
#' Two `diatonicSet`s are equal (according to `==`) only if all their slots (`Root`, `Signature`, and `Alteration`)
#' are exactly identical. 
#' Ordinal comparisons (e.g., `>`, `<=`) between `diatonicSet`s are on their `Signature` only.
#' 
#' 
#' @section Coercion:
#' 
#' `humdrumR` knows how to [coerce](https://en.wikipedia.org/wiki/Type_conversion) several [base-R atomic types][base::vector] into `diatonicSet`s.
#' This can be done using the [as][methods::as] function---e.g., `as(3, "diatonicSet")`---or more intuitively using the function `as.diatonicSet()`.
#' Coercision methods are defined for 
#' 
#' + [integer][base::integer]: interpreted as root of major key
#' + [numeric][base::numeric]: rounded to nearest integer and intepreted as root of major key
#' + [character][base::character]: interpreted using `humdrumR`s [regular expression dispatch system][humdrumR::regexDispatch], as 
#'   explained fully [here][diatonicRepresentations].
#'   
#'
#' 
NULL


#' @export 
setClass('diatonicSet', 
         contains = 'struct',
         slots = c(Root = 'integer', 
                   Signature = 'integer', 
                   Alteration = 'integer')) -> diatonicSet




##...constructors ####

#' The basic constructor for \code{diatonicSet}s.
#' The root argument can accept either an integer (line-of-fifths), a \code{\link[humdrumR:tonalInterval]{tonalInterval}}, 
#' or a character string which will be coerced to a `tonalInterval`.
#' @name diatonicSet
#' @export
dset <- function(root = 0L, signature = root, alterations = 0L) {
           if (is.character(root)) root <- as.tonalInterval.character(root)
           if (is.tonalInterval(root)) root <- root@Fifth
           
           new('diatonicSet', 
               Root = as.integer(root), 
               Signature = as.integer(signature), 
               Alteration = as.integer(alterations)) 
}



##...accessors ####


getRoot <- function(dset, sum = TRUE) {
    root <- dset@Root %dim% dset
    
    if (hasdim(root) && sum) rowSums(root) else root
}

getRootTint <- function(dset, sum = TRUE) {
    root <- getRoot(dset, sum)
    
    out <- LO5thNcentralOct2tint(root, 0L)
    if (sum) out else out %dim% dset
    
}

getSignature <- function(dset, sum = TRUE) {
    signature <- dset@Signature %dim% dset
    
    if (hasdim(signature) && sum) rowSums(signature) else signature
    
}

getMode <- function(dset, sum = TRUE) {
    # mode is sign - root (the signature RELATIVE to the root)
    root <- getRoot(dset, sum = sum)
    sign <- getSignature(dset, sum = sum)
    sign - root
}

getAlterations <- function(dset, sum = TRUE) {
    alter <- dset@Alteration %dim% dset
    
    if (hasdim(alter) && sum) rowSums(alter) else alter
    
}

####. vector/core methods ####
    

#' @name humDiatonic
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

###.. formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('diatonicSet'), function(x) dset2keyI(x))

####. logic methods ####

###.. order/relations methods ####

#' \code{diatonicSets} methods for \code{\link[base]{order}} and 
#' \code{\link[base]{sort}} order/sort along the circle of LO5ths.
#' Signatures are sorted secondarily from fewest flats to most sharps.
#' If \code{parallel = TRUE} all modes are grouped by shared tonics, so
#' C minor and C major will appear besides each other.
#' If \code{parallel = FALSE} modes/keys are sorted together by number of accidentals,
#' so C minor and Eb major will be sorted next to each other.
#' @name diatonicSet
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

#' @name diatonicSet
#' @export
setMethod('==', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
             checkSame(e1, e2, "==")
             f1 <- dset2LO5ths(e1)
             f2 <- dset2LO5ths(e2)
              
             same <- f1 == f2 
             rowSums(same, na.rm = TRUE) == 7L
          })




setMethod('abs', signature = c('diatonicSet'),
          function(x) {
              .ifelse(x@Root < 0, -x, x)
          })

#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
              checkSame(e1, e2, "relational comparison")
              callGeneric(getSignature(e1), getSignature(e2))
          })

###..arithmetic methods ####

##...addition ####



#' @export
setMethod('%%', signature = c('integer', 'diatonicSet'),
          function(e1, e2) {
              signature <- getSignature(e2)
              alter <- getAlterations(e2)
              
              match_size(e1 = e1, signature = signature, alter = alter, toEnv = TRUE)
              generic <- ((e1 + 1L) - signature) %% 7
              
              if (any(alter != 0)) {
                  alter <- alter + sign(alter)
                  toalter <- alter != 0 & generic != .ifelse(alter > 0, 0, 6)
                  
                  generic[toalter] <- ((generic[toalter] - alter[toalter]) %% 7 ) + alter[toalter]
              }
              generic - 1 + signature
          })


#' @export
setMethod('%%', signature = c('tonalInterval', 'diatonicSet'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              fifth <- e1@Fifth
              
              generic <- fifth %% e2
              octdiff <- ((generic - fifth) * 19) / 12
              
              tint(round(e1@Octave - octdiff), generic)
              
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
setMethod('-', signature = c('diatonicSet', 'integer'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              e1@Root <- e1@Root - e2
              e1@Signature <- e1@Signature - e2
              e1
          })



##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


LO5th2mode <- function(LO5th, short = FALSE) {
    
    fullname <- rep("?", length(LO5th))
    fullname[LO5th >= -5 & LO5th <= 1] <-  c('locrian', 'phrygian', 
                                           'minor', 'dorian', 'mixolydian', 
                                           'major', 'lydian')[LO5th[LO5th >= -5 & LO5th <= 1] + 6L]
                      
    
    if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}



###. x to line-of-fifths ####


setMethod('LO5th', 'diatonicSet',
          function(x, steporder = 2L ) {
    # the steporder argument controls the order the LO5ths are output
    # steporder = 2L means every two LO5ths (which is generic steps)
    # steporder = 4L means thirds, which makes tertian harmonies
    dset <- x
    root <- getRoot(dset, sum = FALSE)
    sign <- getSignature(dset, sum = FALSE)
    
    notna <- !is.na(sign) & !is.na(root)
    
    ## Generate scale structure
    sq <- seq(0L, by = as.integer(steporder), length.out = 7L)
    LO5ths <- matrix(sq, nrow = length(root), ncol = 7L, byrow = TRUE)
    LO5ths[!notna, ] <- NA_integer_
    LO5ths <- sweep(LO5ths, 1L, root, `+`)
    LO5ths <- sweep(LO5ths, 1L, sign,
                    function(row, m) {
                      (row + 1L - m) %% 7L - 1L + m  # + 1L and - 1L because F is -1
                    })
    
    # Force root to be root, regardless of mode
    LO5ths[ , 1] <- root
    
    #
    LO5ths[] <- alterLO5ths(LO5ths, dset@Alteration)
    
    rownames(LO5ths) <- dset2keyI(dset)
    colnames(LO5ths) <- c('Root', nth(c(5, 2, 6, 3, 7, 4)))[(sq %% 7L) + 1L]
    
    LO5ths
})


alterLO5ths <- function(LO5ths, alt) {
    alt[is.na(alt)] <- 0L
    if (all(alt == 0L)) return(LO5ths)
    
    roots <- LO5ths[ , 1]
    
    ord <- applyrows(LO5ths, rank)
    
    altmat <- matrix(alt, nrow = length(alt), ncol = 7L)
    LO5ths[ord == 2L & altmat > 0L] <- LO5ths[ord == 2L & alt > 0L] + 7L
    LO5ths[ord == 6L & altmat < 0L] <- LO5ths[ord == 6L & alt < 0L] - 7L
    
    
    # recurse if necessary
    alt <- alt - sign(alt)
    done <- alt == 0L 
    if (any(!done)) {
        LO5ths[!done, ] <- Recall(LO5ths[!done, , drop = FALSE], alt[!done])
    }
    
    LO5ths[, 1] <- roots
    LO5ths
    
}


##### To/From pitch representations ####    

###. dset to pitches ####

dset2pitcher <- function(pitch.func) {
    pitch.func <- rlang::enexpr(pitch.func)
    body <- rlang::expr({
    	LO5ths <- LO5th(x)

        LO5ths <- apply(LO5ths, 2, tint, octave = octave - 4L)
    
	pitches <- sapply(LO5ths, !!pitch.func)
    
    	if (collapse) {
        	apply(pitches, 1, .paste, na.rm = TRUE)
    	} else {
     	    	pitches
	}
     })

    rlang::new_function(alist(x = , collapse = FALSE, octave = 4L),
		        body, parent.env(environment()))
}

dset2semit      <- dset2pitcher(tint2semit)
dset2midi       <- dset2pitcher(tint2midi)
dset2kernPitch  <- dset2pitcher(tint2kernPitch)

dset2tints <- function(dset, steporder = 2L) {
    LO5ths <- LO5th(dset, steporder = steporder)
    tints <- tint( , LO5ths)
    tints %dim% LO5ths
}

##### To/From diatonic sets ####  

####. dset to x ####

###.. key signatures

dset2signature <- function(dset) {
    LO5ths <- LO5th(dset)
    LO5ths[] <- t(apply(LO5ths, 1, sort))
    tints <- tint( , LO5ths) %dim% LO5ths
    
    notes <- as.tonalChroma(tints, parts = c('steps', 'accidentals'),
                   accidental.labels = c(flat = '-'),
                   step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'))
    
    notes[LO5ths <= 5L & LO5ths >= -1L] <- ""
    
    notes <- apply(notes, 1, paste, collapse = '')
        
    .paste("*k[", notes, ']')
}


###.. key indications


dset2keyI <- function(dset, alteration.labels = c(), sum = FALSE) {
    ## As kern key interpretation (i.e., *G:, *e-:)
    
    root <- tint2kernPitch(tint( , getRoot(dset, sum = sum)))
    mode <- getMode(dset, sum = TRUE)
    root[mode > -2L] <- toupper(root[mode > -2L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      LO5th2mode(mode, short = TRUE))
    
    #
    setoptions(alteration.labels) <- c(augment = '+', diminish = '-')
    alterations <- getAlterations(dset, sum = TRUE)
    alterations <- .ifelse(alterations > 0,
                           strrep(alteration.labels['augment'] , abs(alterations)),
                           strrep(alteration.labels['diminish'], abs(alterations)))
    
    
    out <- .paste("*", root, ":", modelab, alterations) 
    if (!sum) out <- out %dim% dset
    out
}


###.. roman numerals

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
#' @name diatonicSet-write
NULL

dset2romanNumeral <- function(dset, ..., sum = FALSE) {
    tint <- getRootTint(dset, sum = sum)
    
    if (hasdim(tint)) {
        numeral <- array("", dim = dim(tint))
        numeral[ , 1L] <- tint2romanRoot(tint[ , 1L], ...)
        if (ncol(tint) > 1L) {
            for (j in 2:ncol(tint)) {
               numeral[ , j] <- tint2romanRoot(tint[ , j], Key = dset[ , j - 1L], ...)
            }
        }
    } else {
        numeral <- tint2romanRoot(tint, ...)
        
    }
    
    mode <- getMode(dset, sum = sum)
    numeral[mode <= -2L] <- tolower(numeral[mode <= -2L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      LO5th2mode(mode, short = TRUE))
    
    out <- .paste(numeral, modelab)
    if (!sum) out <- out %dim% dset
    out
   
}


####. x to dset ####



##... From key interpretation


keyI2dset <- function(keyI) {
    keyI <- stringr::str_remove(keyI, '^\\*')
    
    
    #
    tonalChroma <- stringi::stri_extract_first_regex(keyI, '[A-Ga-g][#-]*')
    root <- tonalChroma2tint(toupper(tonalChroma), accidental.labels = c(flat = '-'))@Fifth
    
    
    #
    minor <- stringi::stri_detect_charclass(tonalChroma, '[a-g]') * -3L
    
    mode <- stringi::stri_extract_first_regex(keyI, 'dor$|mix$|phr$|lyd$|loc$')
    mode <- .ifelse(is.na(mode), 0 , c(dor = +1, mix = -1, phr = -1, loc = -2)[mode])
    
    dset(root, root + mode + minor)
    
    
}

##... From roman numerals
# 
# read.romanNumeral2diatonicSet <- function(rn) {
#     parseRN <- REparser(DegreeAcc = "^[b#]?", 
#                         Numeral = "[IViv]{1,2}[iI]?", 
#                         TriadQuality= '[o+]?', 
#                         Seventh = '([nb#]?7)?',
#                         Ninth   = '([nb#]?9)?',
#                         Eleventh = '([nb#]?11)?',
#                         Thirteenth = '([nb#]?13)?',
#                         Inversion = '[abcdefg]?')
#     return(parseRN(rn))
#     preacc <- stringr::str_extract(rn, '^[b#]')
#     numeral <- stringr::str_extract(rn, '[VIvi][VIvi]?[Ii]?')
#     isminor <- numeral == tolower(numeral)
#     numeral <- toupper(numeral)
#     
#     accf <- numeric(length(preacc))
#     accf[!is.na(preacc)] <- IfElse(preacc[!is.na(preacc)] == "#", 7, -7)
#     numeralf <- c(IV = -1, I = 0, V = 1, II = 2, VI = 3, III = 4, VII = 5)[toupper(numeral)]
#     isminorf <- isminor * -3
#     tset(numeralf + accf, numeralf + isminorf)
#     
#     # extensions
#     stringr::str_extract_all(rn, '[b#]?[79]|[b#]?11|[b#]?13')
# }
# 


#' @name diatonicSet
#' @export as.diatonicSet as.diatonicSet.diatonicSet
as.diatonicSet <- function(...) UseMethod('as.diatonicSet')

as.diatonicSet.diatonicSet <- force

#' @name diatonicSet
#' @export
as.diatonicSet.character <- regexDispatch( '[A-Ga-g][#b-]*:' = keyI2dset,
                                          '.' = force)
#' @export
as.diatonicSet.integer <- function(x) dset(x, x)


##### Diatonic transforms ####




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
#' @name diatonicSet
#' @export
# as.romanNumeral.character <- as.romanNumeral.tertianSet %.% as.tertianSet

##### As x ####

####. generics ####

####. methods ####

##### Predefined diatonicSets ####
