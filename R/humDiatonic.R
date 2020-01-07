#######################################-
##################diatonicSet S4 class ####
#######################################-

#' Tonal (diatonic) sets
#' 
#' 
#' \code{diatonicSet} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western diatonic keys.
#' A key is represented by two integers, \code{Root} and \code{Mode}.
#' Root is simply the tonic note of the key on the circle of fifths.
#' Mode is a value on the circle of fifths, indicating the diatonic mode.
#' You can think of the Mode value as indicating the number of accidentals, with negative numbers
#' for flats and positive numbers for sharps.
#' The standard diatonic modes occur if the \code{Mode - Tonic} is in the range -5:1:
#' \itemize{
#' \item{+1 = Lydian}
#' \item{+0 = Major (Ionian)}
#' \item{-1 = Mixolydian}
#' \item{-2 = Dorian}
#' \item{-3 = Minor (Aeolian)}
#' \item{-4 = Phyrgian}
#' \item{-5 = Locrian}
#' }
#' 
#' @seealso humTonality
#' @name humDiatonic
NULL

setClassUnion('maybe_dSet', members = 'NULL')

#' @name humDiatonic
#' @export 
setClass('diatonicSet', contains = 'humdrumVector',
         slots = c(Root = 'integer', 
                   Mode = 'integer', 
                   Alterations = 'integer', 
                   Of = 'maybe_dSet')) -> diatonicSet

setIs('diatonicSet', 'maybe_dSet')

setValidity('diatonicSet', 
            function(object) {
                !class(object@Of) == "tertianSet"
            })

#' Tertian set
#' 
#' \code{tertianSet} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western tertian harmonies.
#' \code{tertianSet} is a subclass of \code{diatonicSet}.
#' 
#' 
#' 
#' @name humDiatonic
#' @seealso humTonality
#' @export 
setClass('tertianSet', contains = 'diatonicSet',
         slots = c(Thirds = 'integer'))



######diatonicSet constructors and accessors ####
## Accessors

#' @name humDiatonic
#' @export
getRoot <- function(dset, recurse = TRUE, sum = TRUE) {
    # If recurse is FALSE, only returns the "local" root
    # If recurse is TRUE, includes any diatonic sets in the @Of field
    # If sum is TRUE, all keys (X/X/X/X etc) are summed to their final root
    # if sum is FALSE, a matrix of roots is returned
    roots <- cbind(dset@Root, 
                   if (recurse && !is.null(dset@Of)) Recall(dset@Of, recurse = recurse, sum = sum))
    
    if (sum) rowSums(roots) else roots
    
    
}

`setRoot<-` <- function(x, value) {
    x@Root <- Repeat(value, length.out = length(x))
    as.integer(x)
}

#' @name humDiatonic
#' @export
getMode <- function(dset, recurse = TRUE, sum = TRUE) {
    # If recurse is FALSE, only returns the "local" mode
    # If recurse is TRUE, includes any diatonic sets in the @Of field
    # If sum is TRUE, all keys (X/X/X/X etc) are summed to their final mode
    # if sum is FALSE, a matrix of modes is returned
    modes <- cbind(dset@Mode, 
                   if (recurse && !is.null(dset@Of)) Recall(dset@Of, recurse = recurse, sum = sum))
    
    as.integer(if (sum) rowSums(modes) else modes)
}

`setMode<-` <- function(x, value) {
    x@Mode <- Repeat(value, length.out = length(x))
    x
}

#' @name humDiatonic
#' @export
getCardinality <- function(dset) dset@Cardinality

`setCardinality<-` <- function(x, value) {
    x@Cardinality <- Repeat(value, length.out = length(x))
    x
}


#' @name humDiatonic
#' @export
getAlterations <- function(dset) dset@Alterations

`setAlterations<-` <- function(x, value) {
    x@Alterations <- Repeat(value, length.out = length(x))
    x
}

#' @name humDiatonic
#' @export
getThirds <- function(tset) cbind(TRUE, ints2bits(tset@Thirds))

`setThirds<-` <- function(x, value) {
    x@Thirds <- Repeat(value, length.out = length(x))
    x
}

ints2bits <- function(n, nbits = 6) {
    mat <- t(sapply(n, function(x) as.logical(intToBits(x))))[ , 1:nbits, drop = FALSE]
    
    rownames(mat) <- n
    colnames(mat) <- 2 ^ (0:(nbits - 1))
    mat
}

bits2ints <- function(x) as.integer(rowSums(sweep(x, 2, 2L ^ (0L:(ncol(x) - 1L)), `*`)))

    
    
## Constructors

#' The basic constructor for \code{diatonicSet}s.
#' Accepts either a integer (fifth) or a \code{\link[humdrumR:tonalInterval]{tonalInterval}}.
#' @name humDiatonic
#' @export
dset <- function(root = 0L, mode = root, alterations = 0L, of = NULL) {
           if (is.tonalInterval(root)) root <- getFifth(root)
           new('diatonicSet', 
               Root = as.integer(root), 
               Mode = as.integer(mode), 
               Alterations = as.integer(alterations),
               Of = of) -> x
           x
}

#' @name humDiatonic
#' @export
tset <- function(root = 0L, mode = 1L, alterations = 0L, cardinality = 3L, of = NULL) {
    if (is.tonalInterval(root)) root <- getFifth(root)
    
    root <- IfElse(cardinality == 0L, NA_integer_, root)
    extensions <- c(0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L)[cardinality + 1L]
    
    new('tertianSet', 
        Root = as.integer(root), 
        Mode = as.integer(mode), 
        Alterations = as.integer(alterations), 
        Thirds = extensions,
        Of = of)
}


######diatonicSet vector (and other core) methods ####
####Is/As ####

#' @name humDiatonic
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

#' @name humDiatonic
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


#' @name humdrumVector
#' @export
setMethod('is.numeric', signature = c('diatonicSet'),
          function(x) { FALSE })



######diatonicSet order/relations methods ####


#' \code{diatonicSets} methods for \code{\link[base]{order}} and 
#' \code{\link[base]{sort}} order/sort along the circle of fifths.
#' Modes are sorted secondarily from fewest flats to most sharps.
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
                      order(getRoot(x), -getMode(x),
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)
                    } else {
                      order(getRoot(x) - getMode(x), -getMode(x),
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
             f1 <- getFifths(e1)
             f2 <- getFifths(e2)
              
             same <- f1 == f2 
             rowSums(x, na.rm = TRUE) == 7L
          })

#' @name diatonicSet
#' @export
setMethod('==', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "==")
              f1 <- getFifths(e1)
              f2 <- getFifths(e2)
              
              same <- f1 == f2 | (is.na(f1) & is.na(f2))
              
              rowSums(x, na.rm = TRUE) == 7L
          })

#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
              checkSame(e1, e2, "Compare")
              callGeneric("Compare", getRoot(e1), getRoot(e2))
          })


#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "Compare")
              
              callGeneric("Compare", getRoot(e1), getRoot(e2))
          })




######diatonicSet formatting methods ####

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('diatonicSet'), function(x) as.keyI(x))

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) as.chordSymbol(x))



######Special methods

    


# triad = 3, 7th = 7, 9th = 15, 11th = 31, 13th = 63

############################################-
####### Writing diatonic representations ----
############################################-
#' Writing \code{\link[humdrumR:humDiatonic]{diatonicSets}} to various representations.
#' 
#' These functions all translate \code{\link[humdrumR:humDiatonic]{diatonicSets}} to 
#' various pitch representations. Diatonic sets can be diatonic keys, or harmonies (the
#' more specific \code{\link[humdrumR:humDiatonic]{tertianSet}} class).
#' 
#' @name diatonicSet-write
NULL

###Writing from fifths (integers) to X
##To start, we need to be able to translate the fifths
##part of every diaonicKey into various things.

fifth2mode <- function(fifth, short = FALSE) {
    
    fullname <- rep("?", length(fifth))
    fullname[fifth >= -5 & fifth <= 1] <-  c('locrian', 'phrygian', 
                                           'minor', 'dorian', 'mixolydian', 
                                           'major', 'lydian')[fifth[fifth >= -5 & fifth <= 1] + 6L]
                      
    
    if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}

fifth2romanroot <- function(fifth, mode) {
    # calculates the roman numeral for the root, including
    # any root alteration relative to a mode
    # doesn't change quality
    # fifth and mode should be centered relative to C major (0,0)
    numer <- c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')[fifth2genericinterval(fifth)]
    
    alteration <- fifth2alteration(fifth, mode)
    
    paste0(alteration, numer)
}

#' @export
getFifths <- function(dset, step = 2L) UseMethod("getFifths")

#' @export
getFifths.diatonicSet <- function(dset, step = 2L) {
    # the step argument controls the order the fifths are output
    # step = 2L means every two fifths (which is generic steps)
    # step = 4L means thirds, which makes tertian harmonies
    root <- getRoot(dset)
    mode <- getMode(dset)
    
    notna <- !is.na(mode) & !is.na(root)
    
    ## Generate scale structure
    sq <- seq(0L, by = as.integer(step), length.out = 7L)
    fifths <- matrix(sq, nrow = length(root), ncol = 7L, byrow = TRUE)
    fifths[!notna, ] <- NA_integer_
    fifths <- sweep(fifths, 1L, root, `+`)
    fifths <- sweep(fifths, 1L, mode,
                    function(row, m) {
                      (row + 1L - m) %% 7L - 1L + m  # + 1L and - 1L because F is -1
                    })
    
    # Force root to be root, regardless of mode
    fifths[ , 1] <- root
    
    #
    fifths[] <- alterFifths(fifths, getAlterations(dset))
    
    rownames(fifths) <- as.keyI(dset)
    colnames(fifths) <- c('Root', nth(c(5, 2, 6, 3, 7, 4)))[(sq %% 7L) + 1L]
    
    fifths
}

#' @export
getFifths.tertianSet <- function(tset) {
    alterations <- getAlterations(tset)
    # setAlterations(tset) <- 0L
    
    fifths <- getFifths.diatonicSet(tset, step = 4L)
    thirds <- getThirds(tset)
    
    
    fifths <- fifths * thirds
    fifths[!thirds] <- NA_integer_
    
    # if (any(alterations != 0L)) fifths <- sweep(fifths, 1, alterations, alterFifthSet)
    
    colnames(fifths)[5:7] <- nth(c(9,11,13))
    rownames(fifths) <- fifth2lettername(getRoot(tset))
    
    fifths
}


alterFifths <- function(fifths, alt) {
    alt[is.na(alt)] <- 0L
    if (all(alt == 0L)) return(fifths)
    
    roots <- fifths[ , 1]
    
    ord <- applyrows(fifths, rank)
    
    altmat <- matrix(alt, nrow = length(alt), ncol = 7L)
    fifths[ord == 2L & altmat > 0L] <- fifths[ord == 2L & alt > 0L] + 7L
    fifths[ord == 6L & altmat < 0L] <- fifths[ord == 6L & alt < 0L] - 7L

    
    # recurse if necessary
    alt <- alt - sign(alt)
    done <- alt == 0L 
    if (any(!done)) {
        fifths[!done, ] <- Recall(fifths[!done, , drop = FALSE], alt[!done])
    }
    
    fifths[, 1] <- roots
    fifths

}

    

##### Breakout diatonic sets into individual pitches


as.pitches <- function(x, asStops = FALSE, outclass= 'character', pitch.func) {
    fifths <- getFifths(x)
    
    pitches <- array(as(NA, Class = outclass), 
                     dim = dim(fifths), dimnames = dimnames(fifths))
    pitches[] <- pitch.func(fifths)
    
    #
    if (asStops) {
        apply(pitches, 1,  
              function(row) paste(row[!is.na(row)], collapse = ' '))
    } else {
        pitches
    } 
    
}
#' @name diatonicSet
#' @export
as.kernPitch.diatonicSet <- function(x, asStops = FALSE) {
    as.pitches(x, asStops, 'character', fifth2tonalname)
}


#' @name diatonicSet
#' @export
as.semit.diatonicSet <- function(x, asStops = FALSE) {
    as.pitches(x, asStops, 'integer',
               function(fifths) {
                   (fifths * 7L) %% 12L
               })
}

##### As key signature interpretation (i.e., *k[f#], *k[b-e-a-d-g-])

#' @name diatonicSet-write
#' @export
as.keysignatureI <- function(dset) {
    fifths <- getFifths(dset)
    
    notes <- apply(fifths, 1, 
                   function(f) {
                       f <- f[!is.na(f)]
                       flats  <- f[f < -1]
                       sharps <- f[f > 5]
                       accidentals <- c(sort(flats,  decreasing = TRUE),
                                        sort(sharps, decreasing = TRUE))
                       paste(tolower(fifth2tonalname(accidentals)), collapse = "")
                       })
        
    .paste("*k[", notes, ']')
}


#' @name diatonicSet
#' @export
as.romanNumeral <- function(x, ...) UseMethod('as.romanNumeral')



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
#' @export
as.romanNumeral.diatonicSet <- function(dset) {
    root <- getRoot(dset, sum = FALSE)
    mode <- getMode(dset, sum = FALSE) - root
    
    cummode <- applyrows(mode, rev %.% cumsum %.% rev)
    numeral <- modelab <- array("", dim = dim(mode))
    numeral[] <- fifth2romanroot(root, cbind(mode[ , -1, drop = FALSE], 0))
    
    
    numeral[mode < -1] <- tolower(numeral[mode < -1L])
    modelab[] <- IfElse(mode == 0L | mode == -3L,
                      "",
                     paste0(":", fifth2mode(mode, short = TRUE)))
    
    numeral[] <- paste0(numeral, modelab)
    
    if (ncol(numeral) > 1L) apply(numeral, 1, paste, collapse = "/") else numeral
}


##### As kern key interpretation (i.e., *G:, *eb-:)

#' @name diatonicSet-write
#' @export
as.keyI <- function(dset, alteration.labels = c()) {
    root <- fifth2tonalname(getRoot(dset))
    mode <- getMode(dset) - getRoot(dset)
        
    root[mode < -1] <- tolower(root[mode < -1L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      fifth2mode(mode, short = TRUE))
    
    #
    setoptions(alteration.labels) <- c(augment = '+', diminish = '-')
    alterations <- getAlterations(dset)
    alterations <- IfElse(alterations > 0,
                          strrep(alteration.labels['augment'] , abs(alterations)),
                          strrep(alteration.labels['diminish'], abs(alterations)))
    
    
    .paste("*", root, ":", modelab, alterations)
}

##### As tonal name (i.e., "Eb")

#' @name diatonicSet-write
#' @export
as.tonalname.diatonicSet <- function(x, accidental.labels = c(flat = 'b')) {
    fifth2tonalname(getRoot(x), accidental.labels)
}

##### As "scientific chord label" (i.e., "Cmm" or "EbMm")

getSciQuality <- function(tset, collapse.triad = TRUE, thirds = 1:6, collapse = TRUE) {
   
    fifths <- getFifths(tset)
    fifths <- sweep(fifths, 1, fifths[ , 1], `-`)[ , -1, drop = FALSE] # center on 0 then remove root
    
    qualities <- fifth2quality(fifths,
                               quality.labels = list(major = 'M', minor = 'm',
                                                     diminish = 'o', augment = '+',
                                                     perfect = 'P'))
    
    qualities[is.na(qualities)] <- ""
    qualities[nchar(qualities) > 1L] <- .paste('(',  qualities[nchar(qualities) > 1L], ')')
    if (collapse.triad) {
        thirds <- thirds[thirds != 1] - 1
        triads <- sapply(apply(qualities[ , 1:2, drop = FALSE], 1, .paste, collapse = ''),
                        function(row) {
                            switch(row,
                                   MP = "M",
                                   mP = "m",
                                   mo = 'o',
                                   `M+` = "+",
                                   .paste('{', row, '}'))
                            
                        })
        qualities[ , 2] <- triads
        qualities <- qualities[ , -1, drop = FALSE]
    }
    
    qualities <- qualities[ , thirds, drop = FALSE]
    
    if (collapse)  apply(qualities, 1, .paste, collapse = '') else qualities
    
}

#' @name diatonicSet
#' @export
as.sciChord <- function(tharm) {
    root <- getRoot(tharm)
    tonalname <- fifth2tonalname(root, accidental.labels = c(flat = 'b'))
   
    quality <- getSciQuality(tharm)
    
    IfElse(!is.na(root) & !is.na(tonalname), 
           .paste(tonalname, quality), 
           NA_character_)
}


#' @name diatonicSet
#' @export
as.chordSymbol <- function(tharm, sep = '') {
    scichord <- as.sciChord(tharm)
    root <- stringr::str_extract(scichord, '^[A-G][b#]*')
    qual <- stringr::str_remove(scichord, '^[A-G][b#]*')
    
    c(M = "",
      m = "min",
      o = "dim",
      `+` = "aug",
      MM = "maj7",
      MMM = 'maj9',
      MMMP = 'maj11',
      `MMM+` = 'maj9(#11)',
      `MMM+M` = 'maj13(#11)',
      Mm = "7",
      Mmm = "b9",
      MmM = "9",
      `Mm+` = "7(#9)",
      `MmM+` = "#11",
      MmMPM = '13',
      MmMPm = 'b13',
      mm = "min7",
      mmM = "min9",
      mmm = "min7(b9)",
      mmMP = "min11",
      MmMP = "11",
      om = "min7(b5)",
      omm = "min7(b5b9)",
      oo = "dim7",
      `+m` = "aug7",
      `+M` = "aug(maj7)",
      `_` = 'dim(b3)',
      `_m` = 'dim7(b5b3)',
      `_o` = 'dim7(b3)'
      )[qual] -> qual
    
    IfElse(!is.na(root) & !is.na(qual), .paste(root, sep, qual), NA_character_)
    
}

### As roman numeral (I, V, viio, etc.)    


#' @name diatonicSet
#' @export
as.romanNumeral.tertianSet <- function(tset, cautionary = FALSE) {
 ofkey <- if (is.null(tset@Of)) "" else paste0('/', as.romanNumeral(tset@Of))
 
 
 root <- getRoot(tset, recurse = FALSE)
 mode <- getMode(tset, recurse = TRUE) - 
     getMode(tset, recurse = FALSE) -
     getRoot(tset, recurse = TRUE) 
 
 numeral <- fifth2romanroot(root, mode + root)
 
 
 ### triad quality
 # o or + indicate diminished or augmented fifths
 # not going to work for weird, double altered
 triadqual <- getSciQuality(tset, thirds = 1:2)
 
 # case indicates major/minor third
 numeral <- IfElse(grepl('[mo]', triadqual), tolower(numeral), numeral)
 triadqual[triadqual %in% c('M', 'm')] <- ""
 
 ### extensions and alterations
 
 fifths <- getFifths(tset)[ , -1L:-3L, drop = FALSE]
 extension <- array(NA_character_, dim = dim(fifths))
 

 extension[] <- fifth2alteration(fifths, getMode(tset), cautionary = FALSE)
 highest <- applyrows(extension, function(row) seq_len(ncol(extension)) == max(which(!is.na(row))))
 extension[] <- sweep(extension, 2, c('7', '9', '11', '13'), paste0)
 extension[!(highest | (!is.na(extension) & extension == ''))] <- ""
 extension <- applyrows(extension, paste, collapse = "")
 
 IfElse(!is.na(root), paste0(numeral, triadqual, extension), NA_character_)
 
}



#####################################-
#### Reading diatonic representations ----
#######################################-
#' Reading  from various representations
#' 
#' These functions all translate other pitch representations
#' into 
#' 
#' These functions all assume that thheir string input is a well-formed
#' example of the target pitch representation, with no extra strings.
#' (The \code{\link[humdrumR:regexDispatch]{regex dispatch}} functions can be 
#' used to clean/filter inputs into these functions.
#' 
#' @name diatonicSet
NULL


### From key interpretation
#' @name diatonicSet
#' @export
read.keyI2diatonicSet <- function(keyI) {
    tonalnames <- stringi::stri_extract_first_regex(keyI, '[A-Ga-g][#b-]*')
    
    fifths <- tonalname2fifth(tonalnames)
    
    dset(fifths, mode = c(-3L, 0L)[(keyI == toupper(keyI)) + 1L])
    
}


####From scientific chord labels (i.e., GMm)

#' @name diatonicSet
#' @export
read.sciChord2tertianSet <- function(csym) {
    tonalname <- stringi::stri_extract_first_regex(csym, '^[A-Ga-g][#b-]*')
    fifth <- tonalname2fifth(tonalname)
    
    quality <- stringr::str_remove(csym, tonalname)
    quality7 <- substr(quality, start = 0L, stop = 3L)
    
    mode <- c(m  = -3, M = 0, A = 3, d = -5,
              mm = -3, Mm = -1, MM = 0, dm = -5, dd = -8, AM = 3, Am = -1,
              mmm = -4, mmM = -3, MmM = -1)[quality7]
    
    cardinality <- c(3, 4, 5, 6, 7)[nchar(quality)]
    
    alterations <- numeric(length(fifth))
    alterations[quality7 == 'Am'] <- 3
    
    tset(root = fifth, mode = mode, cardinality = cardinality, alterations = alterations )
    
}

####From roman numerals

read.romanNumeral2diatonicSet <- function(rn) {
    parseRN <- REparser(DegreeAcc = "^[b#]?", 
                        Numeral = "[IViv]{1,2}[iI]?", 
                        TriadQuality= '[o+]?', 
                        Seventh = '([nb#]?7)?',
                        Ninth   = '([nb#]?9)?',
                        Eleventh = '([nb#]?11)?',
                        Thirteenth = '([nb#]?13)?',
                        Inversion = '[abcdefg]?')
    return(parseRN(rn))
    preacc <- stringr::str_extract(rn, '^[b#]')
    numeral <- stringr::str_extract(rn, '[VIvi][VIvi]?[Ii]?')
    isminor <- numeral == tolower(numeral)
    numeral <- toupper(numeral)
    
    accf <- numeric(length(preacc))
    accf[!is.na(preacc)] <- IfElse(preacc[!is.na(preacc)] == "#", 7, -7)
    numeralf <- c(IV = -1, I = 0, V = 1, II = 2, VI = 3, III = 4, VII = 5)[toupper(numeral)]
    isminorf <- isminor * -3
    tset(numeralf + accf, numeralf + isminorf)
    
    # extensions
    stringr::str_extract_all(rn, '[b#]?[79]|[b#]?11|[b#]?13')
}

#### From anything!


#' @name diatonicSet
#' @export
as.diatonicSet <- function(...) UseMethod('as.diatonicSet')


#' @name diatonicSet
#' @export
as.diatonicSet.character <- regexDispatch( '[A-Ga-g][#b-]*:' = read.keyI2diatonicSet,
                                          '.' = force)



#' @name diatonicSet
#' @export
setAs('numeric', 'tertianSet', function(from) as.tertianSet.numeric(from))
#' @name diatonicSet
#' @export
setAs('character', 'tertianSet', function(from) as.tertianSet.character(from))

#' @name diatonicSet
#' @export
as.tertianSet <- function(...) UseMethod('as.tertianSet')


#' @name diatonicSet
#' @export
as.tertianSet.character <- regexDispatch( 'Scientific Chord' = read.sciChord2tertianSet,
                                          '.' = force)




#############################################################################-
#### Translating pitch representations ----
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
#' fundamental, \emph{lingua franca} representation of pitch.
#' 
#' @name diatonicSet
#' @export
as.romanNumeral.character <- as.romanNumeral.tertianSet %.% as.tertianSet



#################################################


#' @name diatonicSet
#' @export
as.triad <- function(x, ...) UseMethod('as.triad')

#' @name diatonicSet
#' @export
as.triad.tertianSet <- function(x) {
    setThirds(x) <- 3L
    x
}

#' @name diatonicSet
#' @export
as.triad.character <- as.triad.tertianSet %.% as.tertianSet

