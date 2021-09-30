##################################
###### diatonicSet S4 class   ####
##################################

##### class methods ####

####. definition, validity, initialization ####


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
#' @slot Alteration integers representing alterations of the diatonic set
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
#' + \eqn{Signature - Tonic = +1 \rightarrow} Lydian
#' + \eqn{Signature - Tonic = +0 \rightarrow} Major (Ionian)
#' + \eqn{Signature - Tonic = -1 \rightarrow} Mixolydian
#' + \eqn{Signature - Tonic = -2 \rightarrow} Dorian
#' + \eqn{Signature - Tonic = -3 \rightarrow} Minor (Aeolian)
#' + \eqn{Signature - Tonic = -5 \rightarrow} Locrian
#' + \eqn{Signature - Tonic = -4 \rightarrow} Phyrgian
#' 
#' *Note that you can make diatonicSets where the `Root` is outside the `Key`. This is unusual, and may result in sets you wouldn't predict.*
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
#' + \eqn{\pm 1}: raise or flatten the **7th** scale degree.
#' + \eqn{\pm 3}: raise or flatten the **3rd** scale degree.
#' + \eqn{\pm 9}: raise or flatten the **6th** scale degree.
#' + \eqn{\pm 27}: raise or flatten the **2nd** scale degree.
#' + \eqn{\pm 81}: raise or flatten the **5th** scale degree.
#' + \eqn{\pm 243}: raise or flatten the **1st** scale degree.
#' + \eqn{\pm 749}: raise or flatten the **4th** scale degree.
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
#' @section Arithmatic:
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
#'
#' @name diatonicSet
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


getRoot <- function(dset) dset@Root %dim% dset

getRootTint <- function(dset) {
    root <- getRoot(dset)
    
    out <- LO5thNcentralOct2tint(root, 0L)
    out %dim% dset
    
}

getSignature <- function(dset)  dset@Signature %dim% dset


getMode <- function(dset) {
    # mode is sign - root (the signature RELATIVE to the root)
    root <- getRoot(dset)
    sign <- getSignature(dset)
    sign - root
}

getAlterations <- function(dset) {
    # colnames represent the MAJOR degrees
    alterations <- dset@Alteration %dim% dset
    
    output <- ints2baltern(alterations, 7L) * 7L
    rownames(output) <- NULL
    colnames(output) <- c('4th', 'Root', '5th', '2nd', '6th', '3rd', '7th')
    
    output
}

####. vector/core methods ####
    

#' @name diatonicSet
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

###.. formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('diatonicSet'), function(x) dset2key(x))

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
             f1 <- LO5th(e1)
             f2 <- LO5th(e2)
              
             rowSums(f1 == f2, na.rm = TRUE) == 7L
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
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              alter <- e2@Alteration
              signature <- getSignature(e2)
              
              output <- integer(length(e1))
              
              output[alter == 0L] <- (((e1[alter == 0L] + 1L) - signature) %% 7L) - 1 + signature
              if (any(alter != 0L)) {
                  output[alter != 0L] <- {
                      lof <- LO5th(e2[alter != 0L])
                      lof[sweep(lof %% 7L, 1, e1[alter != 0L] %% 7L, `==`)]
                  }
              }
              
    
              output
      
          })


#' @export
setMethod('%%', signature = c('tonalInterval', 'diatonicSet'),
          function(e1, e2) {
              match_size(e1 = e1, e2 = e2, toEnv = TRUE)
              
              fifth <- getFifth(e1)
              
              simple <-  fifth %% e2
              
              octdiff <- ((simple - fifth) * 19) / 12
              
              tint(round(e1@Octave - octdiff), simple) %dim% e1
              
              
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


##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


LO5th2mode <- function(LO5th, short = FALSE) {
    
    known <- LO5th > -7L & LO5th < 2L & !is.na(LO5th)
    
    LO5th <- LO5th %% 7L
    
    fullname <- rep('?', length(LO5th))
    modes <- c('major', 'lydian',  'locrian', 'phyrgian', 'minor', 'dorian', 'mixolydian')
    fullname[known] <- modes[LO5th[known] + 1]
    
    if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}



###. x to line-of-fifths ####

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
    
    # notna <- !is.na(sign) & !is.na(root)
    inversion <- rep(inversion, length.out = length(x))
    
    ### get line-of-fifths values
    LO5ths <- split(outer(sign, -1L:5L, '+') + alter, f = seq_along(sign))

    ### reorder (root/inversion/steporder)
    root_inkey <- ((root - sign + 1L) %% 7L) + sign - 1L # normalize into signature (in case root is outside signature)
    
    order <- lapply(lapply(root_inkey + steporder * inversion, seq, by = steporder, length.out = 7L), `%%`, e2 = 7L)
    
    LO5ths <- Map(function(lo5th, ord) lo5th[match(ord, lo5th %% 7)], LO5ths, order)
    
    # LO5ths <- do.call('rbind', Map(function(r,i, inv) LO5ths[i, match(seq(r + steporder * inv, by = steporder, length.out = 7L) %% 7L, LO5ths[i, ] %% 7L, )], 
                                   # root %% 7L, 1:nrow(LO5ths), inversion))
    LO5ths <- do.call('rbind', LO5ths)
    LO5ths[cbind(1:nrow(LO5ths), 1L + ((7L - inversion) %% 7L))] <- root
    

    rownames(LO5ths) <- dset2key(dset)
    colnames(LO5ths) <- c('Root', nth(c(5, 2, 6, 3, 7, 4)))[(seq(0L, by = as.integer(steporder), length.out = 7L) %% 7L) + 1L]
    # 
    LO5ths
})


##### To/From pitch representations ####    

###. dset to pitches ####



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



##### To/From diatonic sets ####  

####. dset to x ####


dset2alterations <- function(dset, alteration.labels = c()) {
    setoptions(alteration.labels) <- c(augment = '#', diminish = 'b')

    mode <- getMode(dset)
    
    altered <- dset@Alteration != 0L & mode > -7L & mode < 2L
    
    alterations <- getAlterations(dset)[altered, , drop = FALSE]
    alterations[] <- c(alteration.labels$augment, alteration.labels$diminish, "")[match(alterations, c(7, -7, 0))]

    order <- lapply(mode[altered] %% 7L, function(m) ((0L:6L + m) %% 7L) + 1 )
        
    labs <- do.call('rbind', lapply(order, function(ord) c('4', '1', '5', '2', '6', '3', '7')[ord]))
    labs[alterations == ''] <- ''

    alterations[] <- paste0(alterations, labs)
    
    alterations <- apply(alterations, 1, paste, collapse='')
    output <- character(length(mode))
    output[altered] <- alterations
    output


}

dset2modelabel <- function(dset) {
    mode <- getMode(dset)
    .ifelse(mode == 0L | mode == -3L,
            "",
            LO5th2mode(mode, short = TRUE))
    
}

###.. key signatures

dset2signature <- function(dset) {
    LO5ths <- LO5th(dset)
    LO5ths[] <- t(apply(LO5ths, 1, sort))
    tints <- tint( , LO5ths) %dim% LO5ths
    
    notes <- tonalChroma(tints, parts = c('steps', 'accidentals'),
                   accidental.labels = c(flat = '-'),
                   step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'))
    
    notes[LO5ths <= 5L & LO5ths >= -1L] <- ""
    
    notes <- apply(notes, 1, paste, collapse = '')
        
    .paste("*k[", notes, ']')
}


###.. key indications


dset2key <- function(dset, alteration.labels = c()) {
    ## As kern key interpretation (i.e., *G:, *e-:)
    
    root <- tint2kern(tint( , getRoot(dset)))
    mode <- getMode(dset)
    root[!is.na(mode) & mode %in% c(1L, 0L, 6L, -1L)] <- toupper(root[!is.na(mode) & mode %in% c(1L, 0L, 6L, -1L)])
    
    modelab <- dset2modelabel(dset) 
    
    alterations <- dset2alterations(dset, alteration.labels)
    
    key <- .paste("*", root, ":", modelab, alterations) 
    
    #
    key
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
#' @name romanNumerals
NULL

dset2romanNumeral <- function(dset, ...) {
    tint <- getRootTint(dset)
    
    numeral <- tint2romanRoot(tint, ...)
    
    mode <- getMode(dset)
    numeral[mode <= -2L] <- tolower(numeral[mode <= -2L])
    
    modelab <- dset2modelabel(dset) 
    
    alterations <- dset2alterations(dset)
    
    out <- .paste(numeral, modelab, alterations)

    out
   
}



####. x to dset ####


alteration2trit <- function(str, mode = integer(length(str)), alteration.labels = c()) {
    setoptions(alteration.labels) <- c(augment = '#', diminish = 'b')
    
    accidentalRE <- captureUniq(alteration.labels, zero = TRUE)
    
    str <- stringr::str_replace(str, '13', '6')
    str <- stringr::str_replace(str, '11', '4')
    str <- stringr::str_replace(str, '10', '3')
    str <- stringr::str_replace(str,  '9', '2')
    
    # degrees
    degrees <- stringr::str_extract_all(str,   paste0(accidentalRE, '[1234567]'))
    
    acc <- lapply(degrees, stringr::str_extract, accidentalRE)
    acc <- lapply(acc, accidental2LO5th, accidental.labels = alteration.labels) 
    degrees <- lapply(degrees, stringr::str_remove, accidentalRE)
    
    alterations <- matrix(0, nrow = length(str), ncol = 7)
    
    degrees <- data.frame(Accidentals = unlist(acc), 
                          Degrees = unlist(degrees),
                          Row = rep(seq_along(str), lengths(acc)))

    alterations[cbind(degrees$Row, match(degrees$Degrees, c(4, 1, 5, 2, 6, 3, 7, 4)))] <- degrees$Accidentals 
    alterations[] <- alterations %/% 7L
    
    ## rotate to appropriate mode
    order <- lapply(-mode %% 7L, function(m) ((0L:6L + m) %% 7L) + 1 )
    threes <- do.call('rbind', lapply(order, function(ord) (3^(6:0))[ord]))
    
    rowSums(threes * alterations)
    
    
}


##... from key signature

signature2dset <- function(str, mode = 0L) {
    signotes <- stringr::str_extract_all(str, '[a-g]([#-n])\\1*')
    
    sigs <- integer(length(str))
    
    empty <- lengths(signotes) == 0L
    
    lof <- lapply(signotes[!empty], 
                  function(notes) {
                      lof <- kern2tint(notes)@Fifth
                      lof[lof < -1L | lof > 5L]
                      })
    empty[!empty] <- lengths(lof) == 0L
    lof <- lof[lengths(lof) > 0L]
    
    
    sharp <- sapply(lof, mean) > 0
    
    altered <- unlist(Map(function(fs, bound)  any(diff(sort(c(bound, fs))) > 1), 
                          lof, 
                          c(-1, 5L)[sharp + 1L]))
    
    
    ranges <- sapply(lof, range)
    
    sigs[!empty] <- .ifelse(sharp, ranges[2, ] - 5, ranges[1, ] + 1)
    
    dsets <- dset(sigs - mode, sigs)
    
    
    #
    if (any(altered)) {
        
        alterations <- do.call('rbind',
                               Map( 
                                   function(fth, sig) {
                                       alt <- unalt <- -1L:5L + sig
                                       natural <- alt > -2L & alt < 6L
                                       alt[!natural & !alt %in% fth] <-  alt[!natural & !alt %in% fth] + 7L
                                       (alt - unalt) %/% 7L
                                   }, 
                                   lof[altered], sigs[altered]))
        
        
        # if root is altered
        rootqual <- alterations[cbind(1:nrow(alterations), 2 - mode)]
        
        alterations <- as.integer(rowSums(sweep(alterations, 2, 3L^(6L:0L), `*`)))
        
        if (any(rootqual != 0L)) {
           mode[rootqual != 0L] <- mode[rootqual != 0L] - 1L
        }
        dsets <- dset(sigs - mode, sigs, alterations)
        
        # dsets@Alteration[altered] <- alterations
    }
    dsets
    
}


##... From key interpretation


key2dset <- function(str, parts = c('steps', 'accidentals'), 
                     step.labels = c('C', 'c', 'D', 'd', 'E', 'e', 'F', 'f', 'G', 'g', 'A', 'a', 'B', 'b'), 
                     alteration.labels = c(), accidental.labels = c(), ...) {
    
    setoptions(alteration.labels) <- c(augment = '#', diminish = 'b')
    setoptions(accidental.labels) <- c(sharp = '#', flat = '-')
    
    str <- stringr::str_remove(str, '^\\*')
    
    tonalChromaRE <- makeRE.tonalChroma(parts,  
                                        accidental.labels = accidental.labels,
                                        step.labels = step.labels,
                                        ...)
    
    alterationRE <- makeRE.alterations(alteration.labels)
    
    REparse(str, 
            parse.strict = FALSE, parse.exhaust = FALSE, 
            toEnv = TRUE,
            list(tonalChroma = tonalChromaRE,
                 mode = captureRE(c('dor', 'mix', 'phr', 'lyd', 'loc')),
                 alterations = alterationRE))
    
    # Root
    root <- tonalChroma2tint(chartr('A-GIV', 'a-giv', tonalChroma), accidental.labels = accidental.labels, ...,
                             parts = parts,
                             step.labels = step.labels[step.labels == tolower(step.labels)])@Fifth
    
    
    # Signature
    minor <- stringi::stri_detect_regex(str, '[a-g].*:|[iv]') * -3L
    mode <- .ifelse(is.na(mode), 0 , c(dor = +1, mix = -1, lyd = +1, phr = -1, loc = -2)[mode])
    signature <- root + mode + minor
    
    ## Alterations
    alterations <- .ifelse(is.na(alterations), 0, alteration2trit(alterations, mode + minor))
    
    dset(root, signature, alterations)
    
}


##... From roman numeral

romanNumeral2dset <- function(str, alteration.labels = c(), accidental.labels = c(), of = dset(0, 0)) {
    setoptions(alteration.labels) <- c(augment = '#', diminish = 'b')
    setoptions(accidental.labels) <- c(sharp = '#', flat = 'b')
    
    
    of <- dset(0, getMode(of), of@Alteration)
    dset <- key2dset(str, c('accidentals', 'steps'), 
             alteration.labels = alteration.labels, accidental.labels = accidental.labels,
             step.labels = c('I', 'i', 'II', 'ii', 'III', 'iii', 'IV', 'iv', 'V', 'v', 'VI', 'vi', 'VII', 'vii'),
             Key = of)
    
    dset # + getRoot(of)
    
    
    
    
}



##... Numbers

integer2dset <- function(x) dset(x, x)




##### Tonals transforms ####


#' Pitch translations
#' 
#' These functions translate various pitch representations
#' 
#' 
#' Under the hood, these functions use the \code{\link{humdrumR}} 
#' \code{\link[humdrumR:tonalInterval]{tonalInterval}} \code{S4} class as the 
#' fundamental, \emph{lingua franca} representation of pitch.
#' 
#' @name diatonicSet
#' @export
# romanKey.character <- romanKey.tertianSet %.% as.tertianSet

mapPartition <- function(func, split = '/') {
    function(str) {
        parts <- strPartition(str, split = split)
        
        # parts[] <- lapply(parts, func)
        parts[] <- head(Reduce(function(x, y) func(x, of = y), right = TRUE, init = dset(0, 0), parts, accumulate = TRUE), -1) 
        parts %class% "partition"
        
    }
}

sum_diatonicPartition <- function(part) {
    of <- Reduce('+', lapply(part[ , colnames(part) == 'of', drop = FALSE], getRoot))
    
    dset <- part$base
    dset + dset(of, of, 0L)
    
}

##### As x ####


#' Diatonic set representations
#' 
#' Diatonic sets can be read/wrote in various ways.
#' 
#' @name diatonicRepresentations
NULL

####. generics ####



#' @name diatonicSet
#' @export diatonicSet key signature romanKey
diatonicSet  <- function(x, ...) UseMethod('diatonicSet')
key          <- function(x, ...) UseMethod('key')
signature    <- function(x, ...) UseMethod('signature')
romanKey     <- function(x, ...) UseMethod('romanKey')


####. methods ####

###.. x as dset ####

#' @export
diatonicSet.diatonicSet <- force

#' @export
diatonicSet.numeric <- integer2dset %.% as.integer



char2dset <- humdrumDispatch(doExclusiveDispatch = FALSE,
                             'key: makeRE.key(...)' = key2dset,
                             'romanKey: makeRE.romanKey(...)' = romanNumeral2dset,
                             'signature: makeRE.signature(...)' = signature2dset)


char2dset_partition <- humdrumDispatch(doExclusiveDispatch = FALSE,
                              'keyof: makeRE.diatonicPartition(...)' = mapPartition(char2dset),
                              'key: makeRE.key(...)' = key2dset,
                              'romanNumeral: makeRE.romanKey(...)' = romanNumeral2dset,          
                              'signature: makeRE.signature(...)' = signature2dset)

#' @export
diatonicSet.character <- char2dset_partition


#.... set as

#' @export
setAs('integer', 'diatonicSet', function(from) integer2dset(from))
#' @export
setAs('numeric', 'diatonicSet', function(from) integer2dset(as.integer(from)))
#' @export
setAs('character', 'diatonicSet', function(from) char2dset(from))
#' @export
setAs('matrix', 'diatonicSet', function(from) diatonicSet(c(from)) %dim% from)


###.. dset as x ####

#' @export
key.diatonicSet          <- dset2key
#' @export
signature.diatonicSet    <- dset2signature
#' @export
romanKey.diatonicSet     <- dset2romanNumeral
#' @export
kern.diatonicSet         <- dset2pitcher(tint2kern)

###. x as y ####

#.... numeric -> y ####


#' @export
key.numeric <- dset2key %.% diatonicSet.numeric
#' @export
signature.numeric <- dset2key %.% diatonicSet.numeric
#' @export
romanKey.numeric <- dset2key %.% diatonicSet.numeric

#.... character -> y ####

#' @export
key.character          <- re.place %.% dset2key %.% diatonicSet.character
#' @export
signature.character    <- re.place %.% dset2signature %.% diatonicSet.character
#' @export
romanKey.character     <- re.place %.% dset2romanNumeral %.% diatonicSet.character


##### Tonal transform methods ####


#' @export
is.major <- function(x) UseMethod('is.major')
#' @export
is.minor <- function(x) UseMethod('is.minor')

#' @export
is.major.diatonicSet <- function(x) getMode(x) >= 1L
#' @export
is.minor.diatonicSet <- function(x) getMode(x) < -1L

#' @export
is.minor.diatonicSet <- function(x) getMode(x) == -3L
#' @export
is.minor.character <- is.minor.diatonicSet %.% char2dset



##### Predefined diatonicSets ####
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
#' 
allkeys <- expand.grid(Step = LETTERS[1:7], stringsAsFactors = FALSE,
                       Accidental = c('', 'flat', 'sharp'),
                       Mode = c('major', 'minor', 'dorian', 'mixolydian', 'lydian', 'phrygian', 'locrian'))
allkeys <- within(allkeys, Handle <- paste0(Step, Accidental, Mode))
allkeys$Step <- ifelse(allkeys$Mode %in% c('minor', 'dorian', 'phrygian', 'locrian'), tolower(allkeys$Step), allkeys$Step)
allkeys$Mode <- ifelse(allkeys$Mode %in% c('major', 'minor') , '', substr(allkeys$Mode, 1, 3))
                      

allkeys$Accidental <- c('', '-', '#')[match(allkeys$Accidental, c('', 'flat', 'sharp'))]
                      
allkeys <- within(allkeys, String <- paste0('*', Step, Accidental, ':', Mode))


for (i in 1:nrow(allkeys)) {
    assign(allkeys$Handle[i], diatonicSet(allkeys$String[i]))
}
