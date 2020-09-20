##################################
###### tertianSet S4 class    ####
##################################

##### class methods ####

####. definition, validity, initialization ####


#' Tertian set
#' 
#' \code{tertianSet} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' types of tonal data, representing Western tertian harmonies.
#' \code{tertianSet} is a subclass of \code{diatonicSet} (and thence, `struct`).
#' 
#' The only structural addition, compared to `diatonicSet`, is the `Thirds` slot.
#' This slot indicates which tertian chord members are active in the chord.
#' Since the root is always assumed, there are six other possible chord members: 
#' the third, fifth, seventh, ninth, eleventh, and thirteenth.
#' Every possible combination of these six degrees is represented by a single integer, corresponding
#' to the 6-bit representation of on/offs on the six degrees in reverse order (13, 11, 9, 7, 5, 3).
#' For example, the integer `7` corresponds to a seventh chord: in binary, 7 is `000111`.
#' The initial three zeros indicate that the 13th, 11th, and 9th are *not* part of the harmony, while the three ones
#' indicate that the third fifth and seventh *are* part of the harmony.
#' Ultimately, adding or removing a chord degree from a harmony can be achieved by adding the power of
#' two associated with that degree: 
#' 
#' + **Third**: $\pm 1$
#' + **Fifth**: $\pm 2$
#' + **Seventh**: $\pm 4$
#' + **Ninth**: $\pm 8$
#' + **Eleventh**: $\pm 16$
#' + **Thirteenth**: $\pm 32$
#' 
#' `tertianSet` has many specific methods defined for reading/writing harmonic information.
#' 
#' 
#' @seealso diatonicSet humTonality
#' @export 
setClass('tertianSet', 
         contains = 'diatonicSet',
         slots = c(Thirds = 'integer'))

setValidity('tertianSet', 
            function(object) {
                all(object@Thirds <= 2^6)
            })

#' @name humDiatonic
#' @export
tset <- function(root = 0L, signature = 0L, alterations = 0L, cardinality = 3L) {
    if (is.tonalInterval(root)) root <- root@Fifth
    
    root <- .ifelse(cardinality == 0L, NA_integer_, root)
    extensions <- c(0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L)[cardinality + 1L]
    
    new('tertianSet', 
        Root = as.integer(root), 
        Signature = as.integer(signature), 
        Alteration = as.integer(alterations), 
        Thirds = extensions)
}

##...accessors ####

getThirds <- function(tset) {
    if (hasdim(tset)) tset <- tset[ , ncol(tset)]
    
    cbind(TRUE, ints2bits(tset@Thirds))
}

# how to code/decode chord degrees as integers:

ints2bits <- function(n, nbits = 6) {
    mat <- t(sapply(n, function(x) as.logical(intToBits(x))))[ , 1:nbits, drop = FALSE]
    
    rownames(mat) <- n
    colnames(mat) <- 2 ^ (0:(nbits - 1))
    mat
}

bits2ints <- function(x) as.integer(rowSums(sweep(x, 2, 2L ^ (0L:(ncol(x) - 1L)), `*`)))


####. vector/core methods ####
    

#' @name humDiatonic
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


###.. formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) as.chordSymbol(x))

####. logic methods ####

###.. order/relations methods ####

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


###.. arithmetic methods ####

##... addition ####



##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


###. x to line-of-fifths ####


setMethod('LO5th', 'tertianSet',
         function(x) {
    tset <- x
    # alterations <- getAlterations(tset, sum = TRUE)
    
    LO5ths <- callNextMethod(tset, step = 4L)
    thirds <- getThirds(tset)
    
    
    LO5ths <- LO5ths * thirds
    LO5ths[!thirds] <- NA_integer_
    
    # if (any(alterations != 0L)) LO5ths <- sweep(LO5ths, 1, alterations, alterFifthSet)
    
    colnames(LO5ths)[5:7] <- nth(c(9,11,13))
    rownames(LO5ths) <- tint2tonalChroma(tint( , getRoot(tset, sum = TRUE)), 
                                         step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                                         parts = c('steps', 'accidentals'))
    
    LO5ths
})


##### To/From tertianSets ####    

###. tset to pitches ####


####. tset to x ####


tset2romanNumeral <- function(tset, cautionary = FALSE) {
 ofkey <- if (is.null(tset@Of)) "" else paste0('/', as.romanNumeral(tset@Of))
 
 
 root <- getRoot(tset, recurse = FALSE)
 mode <- getSignature(tset, recurse = TRUE) - 
     getSignature(tset, recurse = FALSE) -
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
 

 extension[] <- LO5th2accidental(LO5ths, tset@Signature, cautionary = FALSE)
 highest <- applyrows(extension, function(row) seq_len(ncol(extension)) == max(which(!is.na(row))))
 extension[] <- sweep(extension, 2, c('7', '9', '11', '13'), paste0)
 extension[!(highest | (!is.na(extension) & extension == ''))] <- ""
 extension <- applyrows(extension, paste, collapse = "")
 
 IfElse(!is.na(root), paste0(numeral, triadqual, extension), NA_character_)
 
}



# As "scientific chord label" (i.e., "Cmm" or "EbMm")

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
    tonalChroma <- LO5th2scaleStep(root, accidental.labels = c(flat = 'b'))
   
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

# As roman numeral (I, V, viio, etc.)    




# From scientific chord labels (i.e., GMm)

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




##### Tertian transforms ####

##### As x ####

####. generics ####

####. methods ####



##### Predefined tertianSets ####
