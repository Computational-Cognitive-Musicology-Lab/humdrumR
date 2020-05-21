#####diatonicSet and tertianSet S4 classes ####

####.class methods ####

###..definition, validity, initialization ####


#' Tonal (diatonic) sets
#' 
#' 
#' `diatonicSet`` is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western diatonic keys.
#' `diatonicSet` is a subeclass of (inherits from) [humdrumR::struct], so they act like vectors/matrices.
#' 
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
#' In addition there is an `Alteration` slot (also integers), which can be used to create various 
#' "altered" scales. To understand how the `Alteration` integer works, first consider how the `Mode` (key-signature)
#' integer works.
#' Think of it like this, we start with a natural diatonic set consisting of the numbers `[-1 0 1 2 3 4 5]` (C major) on the line of fifths:
#' If the `Mode` integer is `+1`, everyathing is shifted up one to be `[0 1 2 3 4 5 6]` (C Lydian).
#' You can think of this as `+1` being added to each number, but instead, think of it has the following operation:
#' remove the lowest (leftmost number) from the vector, add 7 to that number, then append it on the rightmost side.
#' If we follow this operation, we take 0 off the left, and add 7 to the end, getting `[0 1 2 3 4 5 6]`.
#' If `Mode` is greater than one, we repeat this operation however many times, and if `Mode` is negative, we just reverse the procedure.
#' This way of thinking about the `Mode` value is convoluted, but it helps us under stand the `Alteration` operation.
#' The `Alteration` integer does the same operation on a key as `Mode`, except we take the *second*-most left/right value and add/subtract 7.
#' So if `Alteration == -1`, `[-1 0 1 2 3 4 5]` becomes `[-3 -1 0 1 2 3 5]` (C melodic minor);
#' If `Alteration == -2`, `[-1 0 1 2 3 4 5]` becomes `[-4 -3 -1 0 1 2 5]` (C harmonic minor); and so on.
#' In fact, `Alteration == -1` results in all diatonic sets which are modes of the *melodic minor* scale, and 
#' `Alteration == -2` results in diatonic sets which are modes of the *harmonic minor* scale.
#' Other `Alteration` values get us increasingly exotic scales!
#' 
#' 
#' 
#' @seealso humTonality
#' @name humDiatonic
NULL




#' @name humDiatonic
#' @export 
setClass('diatonicSet', 
         contains = 'struct',
         slots = c(Root = 'integer', 
                   Mode = 'integer', 
                   Alteration = 'integer')) -> diatonicSet





#' Tertian set
#' 
#' \code{tertianSet} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western tertian harmonies.
#' \code{tertianSet} is a subclass of \code{diatonicSet} (and thus, `struct`).
#' 
#' 
#' 
#' @name humDiatonic
#' @seealso humTonality
#' @export 
setClass('tertianSet', 
         contains = 'diatonicSet',
         slots = c(Thirds = 'integer'))



##...constructors ####

#' The basic constructor for \code{diatonicSet}s.
#' Accepts either a integer (LO5th) or a \code{\link[humdrumR:tonalInterval]{tonalInterval}}.
#' @name humDiatonic
#' @export
dset <- function(root = 0L, mode = root, alterations = 0L) {
           if (is.tonalInterval(root)) root <- root@Fifth
           new('diatonicSet', 
               Root = as.integer(root), 
               Mode = as.integer(mode), 
               Alteration = as.integer(alterations)) 
}



#' @name humDiatonic
#' @export
tset <- function(root = 0L, mode = 1L, alterations = 0L, cardinality = 3L, of = NULL) {
    if (is.tonalInterval(root)) root <- root@Fifth
    
    root <- .ifelse(cardinality == 0L, NA_integer_, root)
    extensions <- c(0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L)[cardinality + 1L]
    
    new('tertianSet', 
        Root = as.integer(root), 
        Mode = as.integer(mode), 
        Alteration = as.integer(alterations), 
        Thirds = extensions)
}

##...accessors ####

setMethod("LOF", "diatonicSet",
          function(x, sum = TRUE) {
              lof <- x@Root
              lof %<-dim% x
              
              if (hasdim(lof) && sum) rowSums(lof) else lof
              
          })

getMODE <- function(dset, sum = TRUE) {
    mode <- dset@Mode
    mode %<-dim% dset
    
    if (hasdim(mode) && sum) rowSums(mode) else mode
    
}

getThirds <- function(tset) cbind(TRUE, ints2bits(tset@Thirds))

# thirds coded as ints:

ints2bits <- function(n, nbits = 6) {
    mat <- t(sapply(n, function(x) as.logical(intToBits(x))))[ , 1:nbits, drop = FALSE]
    
    rownames(mat) <- n
    colnames(mat) <- 2 ^ (0:(nbits - 1))
    mat
}

bits2ints <- function(x) as.integer(rowSums(sweep(x, 2, 2L ^ (0L:(ncol(x) - 1L)), `*`)))

###..vector/core methods ####
    

#' @name humDiatonic
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

#' @name humDiatonic
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


###..formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('diatonicSet'), function(x) as.keyI(x))

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) as.chordSymbol(x))

####.logic methods ####

###..order/relations methods ####

#' \code{diatonicSets} methods for \code{\link[base]{order}} and 
#' \code{\link[base]{sort}} order/sort along the circle of LO5ths.
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
                      order(x@Root, -x@Mode,
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)
                    } else {
                      order(x@Root - x@Mode, -x@Mode,
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

#' @name diatonicSet
#' @export
setMethod('==', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "==")
              f1 <- dset2LO5ths(e1)
              f2 <- dset2LO5ths(e2)
              
              same <- f1 == f2 | (is.na(f1) & is.na(f2))
              
              rowSums(same, na.rm = TRUE) == 7L
          })

#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
              checkSame(e1, e2, "Compare")
              callGeneric("Compare", e1@Root, e2@Root)
          })


#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "Compare")
              
              callGeneric("Compare", e1@Root, e2@Root)
          })

setMethod('abs', signature = c('diatonicSet'),
          function(x) {
              .ifelse(x@Root < 0, -x, x)
          })

###..arithmetic methods ####

##...addition ####


##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


LO5th2mode <- function(LO5th, short = FALSE) {
    
    fullname <- rep("?", length(LO5th))
    fullname[LO5th >= -5 & LO5th <= 1] <-  c('locrian', 'phrygian', 
                                           'minor', 'dorian', 'mixolydian', 
                                           'major', 'lydian')[LO5th[LO5th >= -5 & LO5th <= 1] + 6L]
                      
    
    if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}

LO5th2romanroot <- function(LO5th, mode) {
    # calculates the roman numeral for the root, including
    # any root alteration relative to a mode
    # doesn't change quality
    # LO5th and mode should be centered relative to C major (0,0)
    numer <- c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')[LO5th2genericinterval(LO5th)]
    
    alteration <- LO5th2alteration(LO5th, mode)
    
    paste0(alteration, numer)
}

###. x to line-of-fifths ####

dset2LO5ths <- function(dset, step = 2L) UseMethod("dset2LO5ths")

dset2LO5ths.diatonicSet <- function(dset, step = 2L) {
    # the step argument controls the order the LO5ths are output
    # step = 2L means every two LO5ths (which is generic steps)
    # step = 4L means thirds, which makes tertian harmonies
    root <- dset@Root
    mode <- dset@Mode
    
    notna <- !is.na(mode) & !is.na(root)
    
    ## Generate scale structure
    sq <- seq(0L, by = as.integer(step), length.out = 7L)
    LO5ths <- matrix(sq, nrow = length(root), ncol = 7L, byrow = TRUE)
    LO5ths[!notna, ] <- NA_integer_
    LO5ths <- sweep(LO5ths, 1L, root, `+`)
    LO5ths <- sweep(LO5ths, 1L, mode,
                    function(row, m) {
                      (row + 1L - m) %% 7L - 1L + m  # + 1L and - 1L because F is -1
                    })
    
    # Force root to be root, regardless of mode
    LO5ths[ , 1] <- root
    
    #
    LO5ths[] <- alterLO5ths(LO5ths, dset@Alteration)
    
    rownames(LO5ths) <- as.keyI(dset)
    colnames(LO5ths) <- c('Root', nth(c(5, 2, 6, 3, 7, 4)))[(sq %% 7L) + 1L]
    
    LO5ths
}

dset2LO5ths.tertianSet <- function(tset) {
    alterations <- tset@Alteration
    
    LO5ths <- dset2LO5ths.diatonicSet(tset, step = 4L)
    thirds <- getThirds(tset)
    
    
    LO5ths <- LO5ths * thirds
    LO5ths[!thirds] <- NA_integer_
    
    # if (any(alterations != 0L)) LO5ths <- sweep(LO5ths, 1, alterations, alterFifthSet)
    
    colnames(LO5ths)[5:7] <- nth(c(9,11,13))
    rownames(LO5ths) <- LO5th2lettername(tset@Root)
    
    LO5ths
}


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

    
###. dset to pitches ####



dset2pitcher <- function(pitch.func) {
    pitch.func <- rlang::enexpr(pitch.func)
    body <- rlang::expr({
    	LO5ths <- dset2LO5ths(x)

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



####. dset to x ####

dset2signature <- function(dset) {
    LO5ths <- dset2LO5ths(dset)
    
    notes <- apply(LO5ths, 1, 
                   function(f) {
                       f <- f[!is.na(f)]
                       flats  <- f[f < -1]
                       sharps <- f[f > 5]
                       accidentals <- c(sort(flats,  decreasing = TRUE),
                                        sort(sharps, decreasing = TRUE))
                       paste(tolower(LO5th2tonalChroma(accidentals)), collapse = "")
                       })
        
    .paste("*k[", notes, ']')
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
#' @name diatonicSet-write
NULL

dset2romanNumeral <- function(dset) {
    root <- getRoot(dset, sum = FALSE)
    mode <- getMode(dset, sum = FALSE) - root
    
    cummode <- applyrows(mode, rev %.% cumsum %.% rev)
    numeral <- modelab <- array("", dim = dim(mode))
    numeral[] <- LO5th2romanroot(root, cbind(mode[ , -1, drop = FALSE], 0))
    
    
    numeral[mode < -1] <- tolower(numeral[mode < -1L])
    modelab[] <- IfElse(mode == 0L | mode == -3L,
                      "",
                     paste0(":", LO5th2mode(mode, short = TRUE)))
    
    numeral[] <- paste0(numeral, modelab)
    
    if (ncol(numeral) > 1L) apply(numeral, 1, paste, collapse = "/") else numeral
}

tset2romanNumeral <- function(tset, cautionary = FALSE) {
 ofkey <- if (is.null(tset@Of)) "" else paste0('/', as.romanNumeral(tset@Of))
 
 
 root <- getRoot(tset, recurse = FALSE)
 mode <- getMode(tset, recurse = TRUE) - 
     getMode(tset, recurse = FALSE) -
     getRoot(tset, recurse = TRUE) 
 
 numeral <- LO5th2romanroot(root, mode + root)
 
 
 ### triad quality
 # o or + indicate diminished or augmented LO5ths
 # not going to work for weird, double altered
 triadqual <- getSciQuality(tset, thirds = 1:2)
 
 # case indicates major/minor third
 numeral <- IfElse(grepl('[mo]', triadqual), tolower(numeral), numeral)
 triadqual[triadqual %in% c('M', 'm')] <- ""
 
 ### extensions and alterations
 
 LO5ths <- dset2LO5ths(tset)[ , -1L:-3L, drop = FALSE]
 extension <- array(NA_character_, dim = dim(LO5ths))
 

 extension[] <- LO5th2alteration(LO5ths, tset@Mode, cautionary = FALSE)
 highest <- applyrows(extension, function(row) seq_len(ncol(extension)) == max(which(!is.na(row))))
 extension[] <- sweep(extension, 2, c('7', '9', '11', '13'), paste0)
 extension[!(highest | (!is.na(extension) & extension == ''))] <- ""
 extension <- applyrows(extension, paste, collapse = "")
 
 IfElse(!is.na(root), paste0(numeral, triadqual, extension), NA_character_)
 
}


##### As kern key interpretation (i.e., *G:, *eb-:)

#' @name diatonicSet-write
#' @export
as.keyI <- function(dset, alteration.labels = c()) {
    root <- LO5th2tonalChroma(dset@Root)
    mode <- dset@Mode - dset@Root
        
    root[mode < -1] <- tolower(root[mode < -1L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      LO5th2mode(mode, short = TRUE))
    
    #
    setoptions(alteration.labels) <- c(augment = '+', diminish = '-')
    alterations <- dset@Alteration
    alterations <- IfElse(alterations > 0,
                          strrep(alteration.labels['augment'] , abs(alterations)),
                          strrep(alteration.labels['diminish'], abs(alterations)))
    
    
    .paste("*", root, ":", modelab, alterations)
}

##### As tonal name (i.e., "Eb")

#' @name diatonicSet-write
#' @export
as.tonalChroma.diatonicSet <- function(x, accidental.labels = c(flat = 'b')) {
    LO5th2tonalChroma(x@Root, accidental.labels)
}

##### As "scientific chord label" (i.e., "Cmm" or "EbMm")

getSciQuality <- function(tset, collapse.triad = TRUE, thirds = 1:6, collapse = TRUE) {
   
    LO5ths <- dset2LO5ths(tset)
    LO5ths <- sweep(LO5ths, 1, LO5ths[ , 1], `-`)[ , -1, drop = FALSE] # center on 0 then remove root
    
    qualities <- LO5th2quality(LO5ths,
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
    root <- tharm@Root
    tonalChroma <- LO5th2tonalChroma(root, accidental.labels = c(flat = 'b'))
   
    quality <- getSciQuality(tharm)
    
    IfElse(!is.na(root) & !is.na(tonalChroma), 
           .paste(tonalChroma, quality), 
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
    tonalChromas <- stringi::stri_extract_first_regex(keyI, '[A-Ga-g][#b-]*')
    
    LO5ths <- tonalChroma2LO5th(tonalChromas)
    
    dset(LO5ths, mode = c(-3L, 0L)[(keyI == toupper(keyI)) + 1L])
    
}


####From scientific chord labels (i.e., GMm)

#' @name diatonicSet
#' @export
read.sciChord2tertianSet <- function(csym) {
    tonalChroma <- stringi::stri_extract_first_regex(csym, '^[A-Ga-g][#b-]*')
    LO5th <- tonalChroma2LO5th(tonalChroma)
    
    quality <- stringr::str_remove(csym, tonalChroma)
    quality7 <- substr(quality, start = 0L, stop = 3L)
    
    mode <- c(m  = -3, M = 0, A = 3, d = -5,
              mm = -3, Mm = -1, MM = 0, dm = -5, dd = -8, AM = 3, Am = -1,
              mmm = -4, mmM = -3, MmM = -1)[quality7]
    
    cardinality <- c(3, 4, 5, 6, 7)[nchar(quality)]
    
    alterations <- numeric(length(LO5th))
    alterations[quality7 == 'Am'] <- 3
    
    tset(root = LO5th, mode = mode, cardinality = cardinality, alterations = alterations )
    
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
#' @export as.diatonicSet as.diatonicSet.diatonicSet
as.diatonicSet <- function(...) UseMethod('as.diatonicSet')

as.diatonicSet.diatonicSet <- force

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
# as.romanNumeral.character <- as.romanNumeral.tertianSet %.% as.tertianSet



#################################################


#' @name diatonicSet
#' @export
as.triad <- function(x, ...) UseMethod('as.triad')

#' @name diatonicSet
#' @export
as.triad.tertianSet <- function(x) {
    x@Thirds <- 3L
    x
}

#' @name diatonicSet
#' @export
as.triad.character <- as.triad.tertianSet %.% as.tertianSet

