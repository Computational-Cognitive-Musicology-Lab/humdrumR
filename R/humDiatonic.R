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
#' @export 
setClass('diatonicSet', contains = 'humdrumVector',
         slots = c(Root = 'integer', Mode = 'integer', Alterations = 'integer')) -> diatonicSet


#' @name humDiatonic
#' 
#' \code{tertianSet} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western tertian harmonies.
#' \code{tertianSet} is a subclass of \code{diatonicSet}.
#' 
#' 
#' 
#' @seealso humTonality
#' @export 
setClass('tertianSet', contains = 'diatonicSet',
         slots = c(Thirds = 'integer'))



######diatonicSet constructors and accessors ####
## Accessors

#' @name humDiatonic
#' @export
getRoot <- function(dset) dset@Root

`setRoot<-` <- function(x, value) {
    x@Root <- Repeat(value, length.out = length(x))
    x
}

#' @name humDiatonic
#' @export
getMode <- function(dset) dset@Mode

`setMode<-` <- function(x, value) {
    x@Mode <- Repeat(value, length.out = length(x))
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
dset <- function(root = 0L, mode = 0L, alterations = 0L) {
           if (is.tonalInterval(root)) root <- getFifth(root)
           new('diatonicSet', 
               Root = as.integer(root), 
               Mode = as.integer(mode), 
               Alterations = as.integer(alterations))
}

#' @name humDiatonic
#' @export
tset <- function(root = 0L, mode = 1L, alterations = 0L, cardinality = 3L) {
    if (is.tonalInterval(root)) root <- getFifth(root)
    
    root <- IfElse(cardinality == 0L, NA_integer_, root)
    extensions <- c(0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L)[cardinality + 1L]
    
    new('tertianSet', 
        Root = as.integer(root), 
        Mode = as.integer(mode), 
        Alterations = as.integer(alterations), 
        Thirds = extensions)
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
setMethod('Compare', signature = c('diatonicSet', 'diatonicSet'),
          function(e1, e2) {
             checkSame(e1, e2, "Compare")
             callGeneric(accidentals(e1), accidentals(e2))
          })

#' @name diatonicSet
#' @export
setMethod('Compare', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "Compare")
              callGeneric(getRoot(e1), get(e2))
          })




######diatonicSet formatting methods ####

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('diatonicSet'), function(x) as.keyI(x))

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) as.chordSymbol(x))



######Special methods

# How many accidentals does key have?
accidentals <- function(dset) getMode(dset)


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



#' @export
getFifths <- function(dset, step = 2L) UseMethod("getFifths")

#' @export
getFifths.diatonicSet <- function(dset, step = 2L) {
    # the step argument controls the order the fifths are output
    # step = 2L means every two fifths (which is generic steps)
    # step = 4L means thirds, which makes tertian harmonies
    root <- getRoot(dset)
    mode <- getMode(dset) - root
    
    notna <- !is.na(mode) & !is.na(root)
    
    fifths <- matrix(NA_integer_, nrow = length(root), ncol = 7)
    if (any(notna)) {
        fifths[notna, ] <- t(mapply(function(m, alt) {
            fs <- rotate(-1:5L, m - 1L, wrap = TRUE)
            alterFifthSet(fs, alt)
        },
        mode[notna], getAlterations(dset)[notna]))
    }
    
    fifths <- sweep(fifths, 1, root + mode, `+`)
    fifths[ , 1] <- root
    
    fifths <- orderScale(fifths, step = step)
    rownames(fifths) <- fifth2tonalname(fifths[ , 1])
    
    fifths
}

#' @export
getFifths.tertianSet <- function(tset) {
    alterations <- getAlterations(tset)
    setAlterations(tset) <- 0L
    
    fifths <- getFifths.diatonicSet(tset, step = 4L)
    thirds <- getThirds(tset)
    
    
    fifths <- fifths * thirds
    fifths[!thirds] <- NA_integer_
    
    if (any(alterations != 0L)) fifths <- t(apply(fifths, 1, alterFifthSet, alterations))
    
    colnames(fifths)[5:7] <- nth(c(9,11,13))
    
    
    fifths
}


alterFifthSet <- function(f, n = 0L, position = 2L) {
    if (n == 0L) return(f)
    
    which.alter <- order(f, decreasing = n < 0L)[position]
    f[which.alter] <- f[which.alter] + 7L * sign(n)
    
    Recall(f, n - sign(n), position = position)
    
}


orderScale <- function(fs, step = 2L) {
    if (is.null(dim(fs))) fs <- matrix(fs, nrow = 1L)
    
    sq <- (seq(0, by = step, length.out = ncol(fs)) %% 7L) + 1L
    fs <- fs[ , sq, drop = FALSE]
    
    colnames(fs) <- c("Root", nth(c(1,5,2,6,3,7,4)[sq[-1]]))
    
    fs
    
}


##### As key signature interpretation (i.e., *k[f#], *k[b-e-a-d-g-])

#' @name diatonicSet-write
#' @export
as.keysignatureI <- function(dset) {
    fifth <- getMode(dset) 
    
    notzero <- fifth != 0L
    notes <- character(length(fifth))
    
    fifth <- fifth[notzero]
    if (any(notzero)) {
        to   <- fifth + ifelse(fifth > 0L, 5, -1)
        from <- c(ifelse(min(fifth) > -7, -2, min(fifth) + 5), 
                  ifelse(min(fifth) <  7, 6, max(fifth) - 5))[(fifth > 0L) + 1L]
        notef <- do.call(`:`, as.list(range(unique(c(from, to)))))
        notenames <- as.kernPitch(simpletint(notef))
        
        notes[notzero] <- unlist(Map(function(from, to) .paste(collapse = '', notenames[match(from:to, notef)]), 
                                     from, to))
    }
        
    .paste("*k[", notes, ']')
}


##### As kern key interpretation (i.e., *G:, *eb-:)

#' @name diatonicSet-write
#' @export
as.keyI <- function(dset) {
    root <- fifth2tonalname(getRoot(dset))
    
    mode <- getMode(dset) - getRoot(dset)
    
    root[mode < -1] <- tolower(root[mode < -1L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      fifth2mode(mode, short = TRUE))
    
    .paste("*", root, ":", modelab)
}

##### As tonal name (i.e., "Eb")

#' @name diatonicSet-write
#' @export
as.tonalname.diatonicSet <- function(x, accidental.labels = c(flat = 'b')) {
    fifth2tonalname(getRoot(x), accidental.labels)
}

##### As "scientific chord label" (i.e., "Cmm" or "EbMm")

getSciQuality <- function(tset, collapse.triad = TRUE) {
   
    fifths <- getFifths(tset)
    fifths <- sweep(fifths, 1, fifths[ , 1], `-`)[ , -1, drop = FALSE] # center on 0 then remove root
    
    qualities <- fifth2quality(fifths,
                               quality.labels = list(major = 'M', minor = 'm',
                                                     diminish = 'o', augment = '+',
                                                     perfect = 'P'))
    
    qualities[is.na(qualities)] <- ""
    qualities[nchar(qualities) > 1L] <- .paste('(',  qualities[nchar(qualities) > 1L], ')')
    if (collapse.triad) {
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
    
    apply(qualities, 1, .paste, collapse = '')
    
}

#' @name diatonicSet
#' @export
as.sciChord <- function(tharm) {
    root <- getRoot(tharm)
    tonalname <- fifth2tonalname(root, accidental.labels = c(flat = 'b'))
   
    quality <- getSciQuality(tharm)
    
    IfElse(!is.na(root) & !is.na(tonalname), .paste(tonalname, quality), NA_character_)
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
      mmm = "min(b9)",
      mmMP = "min11",
      MmMP = "11",
      om = "min7(b5)",
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
as.romanNumeral <- function(x, ...) UseMethod('as.romanNumeral')

#' @name diatonicSet
#' @export
as.romanNumeral.tertianSet <- function(tset, key = dset(0L, 0L), cautionary = FALSE) {
 fifths <- getFifths(tset)
 
 if (!is.diatonicSet(key)) key <- as.diatonicSet(key)
 
 degrees <- array(NA_character_, dim = dim(fifths))
 degrees[] <- apply(fifths, 2, as.scaleDegree, cautionary = FALSE, key = key)
 
 ### Root
 roots    <- stringi::stri_extract_first_regex(degrees[ , 1], '[1-7]')
 numerals <- c(`1` = 'I', `2` = 'II', `3` = 'III', `4` = 'IV', `5` = 'V', `6` = 'VI', `7` = 'VII')[roots]
 
 ### triad quality
 mode <- getMode(tset)
 numeral <- IfElse(mode < -1L, tolower(numerals), numerals)
 triadalt <- character(length(mode))
 triadalt[mode < -4L] <- 'o'
 triadalt[mode >  1L] <- '+'
 
 ### extensions and alterations
 accidental   <- degrees
 accidental[] <- apply(degrees, 2, stringi::stri_extract_first_regex, pattern = '^[mnb#M]*')
 accidental[accidental == 'M'] = 'n'
 accidental[accidental == 'm'] = 'b'
 
 rootaccidental <- accidental[ , 1L]
 accidental <- accidental[ , -1L, drop = FALSE]
 
 chordtones <- c('3', '5', '7', '9', '11', '13')
 
 if (!cautionary) {
     chordtones <- chordtones[-1:-2]
     accidental <- accidental[ , -1:-2, drop = FALSE]
 }
 
 n <- length(chordtones)
 extensions <- apply(accidental, 1,
       function(row) {
           notna <- !is.na(row)
           acc <- row != ''
           last <- logical(length(row))
           if (any(notna)) last[ max(which(notna))] <- TRUE
           hits <- notna & (acc | last)
           .paste(row[hits], chordtones[hits], sep = '', collapse = '')
           
       })
 
 
 ##
 IfElse(!is.na(roots), .paste(rootaccidental, numeral, triadalt, extensions), NA_character_)
 
}

#' @name diatonicSet
#' @export
as.kernPitch.diatonicSet <- function(x, asStops = FALSE) {
    fifths <- getFifths(x)
    
    tonalnames <- array(NA_character_, dim = dim(fifths))
    tonalnames[] <- fifth2tonalname(fifths)
    
    #
    if (asStops) {
        apply(tonalnames, 1, 
                       function(row) paste(row[!is.na(row)], collapse = ' '))
        } else {
            tonalnames
        } 
    
    
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

