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
#' Mode is a value between 0 and 6 on the circle of fifths, indicating the diatonic mode:
#' \itemize{
#' \item 0 = Lydian
#' \item 1 = Major (Ionian)
#' \item 2 = Mixolydian
#' \item 3 = Dorian
#' \item 4 = Minor (Aeolian)
#' \item 5 = Phyrgian
#' \item 6 = Locrian
#'}
#' 
#' @name humDiatonic
#' @seealso humTonality
#' @export 
setClass('diatonicSet', contains = 'humdrumVector',
         slots = c(Root = 'integer', Mode = 'integer'))


#' @name humDiatonic
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
    x@Root <- rep(value, length.out = x@Root)
    x
}
#' @name humDiatonic
#' @export
getMode <- function(dset) dset@Mode

#' @name humDiatonic
#' @export
getThirds <- function(tset, as.bits = TRUE) if (as.bits) ints2bits(tset@Thirds) else tset@Thirds

ints2bits <- function(n, nbits = 6) t(sapply(n, function(x) c(TRUE, head(as.logical(intToBits(x)), nbits))))
bits2ints <- function(x) as.integer(rowSums(sweep(x[ , -1, drop = FALSE], 2, 2L ^ (0L:(ncol(x) - 2L)), `*`)))

# triad = 3, 7th = 7, 9th = 15, 11th = 31, 13th = 63


## Constructors

#' The basic constructor for \code{diatonicSet}s.
#' Accepts either a integer (fifth) or a \code{\link[humdrumR:tonalInterval]{tonalInterval}}.
#' @name humDiatonic
#' @export
dset <- function(root = 0L, mode = 1L) {
           if (is.tonalInterval(root)) root <- getFifth(root)
           new('diatonicSet', Root = as.integer(root), Mode = as.integer(mode))
}

tset <- function(root = 0L, mode = 1L, cardinality = 3L) {
    if (is.tonalInterval(root)) root <- getFifth(root)
    
    root <- IfElse(cardinality == 0L, NA_integer_, root)
    exten <- c(0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L)[cardinality + 1L]
    
    new('tertianSet', Root = as.integer(root), Mode = as.integer(mode), Thirds = exten)
}


######diatonicSet vector (and other core) methods ####
####Is/As ####

#' @name humDiatonic
#' @export
is.diatonicSet <- function(x) inherits(x, 'diatonicSet')

#' @name humDiatonic
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


#' @name diatonicSet-asvector
#' @export
setMethod('is.numeric', signature = c('diatonicSet'),
          function(x) { FALSE })



######diatonicSet order/relations methods ####


#' \code{diatonicSets} methods for \code{\link[base]{order}} and 
#' \code{\link[base]{sort}} order/sort along the circle of fifths.
#' Modes are sorted secondarily from fewest flats to most sharps.
#' If \code{parallel = TRUE} all modes are grouped by shared tonics, so
#' C minor and C major will appear besides each other.
#' If \code{parallel = FALSE} modes\keys are sorted together by number of accidentals,
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
accidentals <- function(dset) getRoot(dset) - getMode(dset) - 1L


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
    fullname[fifth > -5 & fifth <= 1] <-  c('locrian', 'phrygian', 
                                           'minor', 'dorian', 'mixolydian', 
                                           'major', 'lydian')[fifth[fifth > -5 & fifth <= 1] + 6L]
                      
    
    if (short) stringi::stri_sub(fullname, 1L, 3L) else fullname
}


getFifths <- function(dset, ...) UseMethod("getFifths")

getFifths.diatonicSet <- function(dset, step = 2L) {
    base <- -1L:5L
    t(mapply(function(m, r) {
           fs <- rotate(base + r + m, -1L + m, wrap = TRUE)[(seq(0, by = step, length.out = 7L) %% 7L) + 1L]
           fs[1] <- r
           fs
        },
        getMode(dset), getRoot(dset)))
}

getFifths.tertianSet <- function(tset) {
    fifths <- getFifths.diatonicSet(tset, step = 4L)
    
    thirds <- getThirds(tset, as.bits = TRUE)
    
    fifths[!thirds] <- NA_integer_
    fifths
}


##### As key signature interpretation (i.e., *k[f#], *k[b-e-a-d-g-])

#' @name diatonicSet-write
#' @export
as.keysignatureI <- function(dset) {
    fifth <- getRoot(dset) + getMode(dset) 
    
    notzero <- fifth != 0L
    notes <- character(length(fifth))
    
    fifth <- fifth[notzero]
    if (any(notzero)) {
        to   <- fifth + ifelse(fifth > 0L, 5, -1)
        from <- c(-2, 6)[(fifth > 0L) + 1L]
        notef <- do.call(`:`, as.list(range(unique(c(from, to)))))
        notenames <- as.kernPitch(simpletint(notef))
        
        notes[notzero] <- unlist(Map(function(from, to) paste(collapse = '', notenames[match(from:to, notef)]), 
                                     from, to))
    }
        
    paste0("*k[", notes, ']')
}


##### As kern key interpretation (i.e., *G:, *eb-:)

#' @name diatonicSet-write
#' @export
as.keyI <- function(dset) {
    root <- fifth2tonalname(getRoot(dset))
    
    mode <- getMode(dset)
    
    root[mode < -1] <- tolower(root[mode < -1L])
    
    modelab <- ifelse(mode == 0L | mode == -3L,
                      "",
                      fifth2mode(mode, short = TRUE))
    
    paste0("*", root, ":", modelab)
}

##### As tonal name (i.e., "Eb")

#' @name diatonicSet-write
#' @export
as.tonalname.diatonicSet <- function(x, kernFlats = FALSE) {
    fifth2tonalname(getRoot(x), kernFlats)
}



##### As "scientific chord label" (i.e., "Cmm" or "EbMm")


getSciQuality <- function(tset, collapse.triads = TRUE) {
    roots <- getRoot(tset)
    
    
    fifths <- getFifths(tset)
    fifths[] <- sweep(fifths, 1, fifths[1], `-`)
    fifths <- fifths[ , -1, drop = FALSE] # remove root because it doesn't bear on the quality
    
    qualities <- fifths
    qualities[!is.na(fifths)] <- fifth2quality(fifths[!is.na(fifths)],
                                               quality.labels = list(perfect = "P", major = "M", minor = "m",
                                                                     augment = "+", diminish = "o"))
    qualities[is.na(qualities)] <- '_'
    qualities[nchar(qualities) > 1L] <- paste0('(', qualities[nchar(qualities) > 1L], ')')
    
    # if (length(qualification) > 4L) qualification[5L] <- c(o = "o", m = 'P', M = '+')[qualification[5L]]
    if (collapse.triads) {
        triads <- apply(qualities[, 1:2, drop = FALSE], 1, 
                         function(`35`) {
                             `35` <- paste(`35`, collapse = '')
                             switch(`35`, 
                                    MP   = "M", 
                                    `M+` = "+", 
                                    mP   = "m", 
                                    `mo` = 'o', 
                                    paste0('{', `35`, '}'))
                             
                         })
        qualities[ , 2] <- triads
        qualities <- qualities[ , -1, drop = FALSE]
    }
    
    qualities <- apply(qualities, 1, paste, collapse = '') 
    qualities[] <- stringi::stri_trim(qualities, side = 'right', pattern = '[^_]')
    qualities
}


#' @name humHarmony
#' @export
as.sciChord <- function(tset, collapse.triads = TRUE) {
    roots <- getRoot(tset)
    tonalnames <- fifth2tonalname(roots, kernFlats = FALSE)
    
   
    qualities <- getSciQuality(tset, collapse.triads = collapse.triads)
    
    paste0(tonalnames, qualities)
}

# 
# as.jazz <- function(tset) {
#     roots  <- fifth2tonalname(getRoot(tset))
#     thirds <- lapply(getThirds(tset), tail, n = -1L)
#     fifths <- lapply(getFifths(tset), function(f) f[-1] - f[1])
#     
#     qualMatrix <- t(mapply(function(f, th) {th[th] <- th[th] * f ; th}, fifths, thirds))
#     qualMatrix[] <- fifth2quality(qualMatrix)
#     qualMatrix[!do.call('rbind', thirds)] <- NA_character_
#     qualMatrix[qualMatrix == 'P'] <- ''
#     colnames(qualMatrix) <- c('3rd', '5th', '7th', '9th', '11th', '13th')
# 
#     qualities <- character(length(tset))
#     
#     #
#     transformer <- vector('list', length(roots))
#     thirdMissing <- is.na(qualMatrix[ , '3rd'])
#     transformer[thirdMissing] lapply() <- apply(qualMatrix[thirdMissing, , drop = FALSE], 1,
#           function(row) {
#               fix <- ''
#               
#               `119` <- row[c('11th', '9th')]
#               if (all(is.na(`119`)) && !is.na(row['5th'])) {
#                   fix <- paste0(row['5th'], '5')
#                   
#               } else {
#                   fix <- paste('sus', c('4', '2')[!is.na(`119`)])
#               }
#               function(x) paste0(x, fix)
#               
#           }) 
#     
#     browser()
#     
# }

#' @name humHarmony
#' @export
as.chordSymbol <- function(tharm, sep = '') {
    scichord <- as.sciChord(tharm, collapse.triads = FALSE)
    
    root <- stringr::str_extract(scichord, '^[A-G][b#]*')
    qual <- stringr::str_remove(scichord, '^[A-G][b#]*')
    
    c(
    ##Diads
         #Thirds
       o   = "(b3)",
       m   = "(m3)",
      `M` = '(M3)',
      `+`  = "(#3)",
         #Fifths
      `_o` = "(b5)",
      `_P` = "5",
      `_+` = "(#5)",
         #Sevenths
      `__o` = '(d7)',
      `__m` = '(m7)',
      `__M` = '(M7)',
      `__+` = '(#7)',
         #Ninths
      `___o` = '(d9)',
      `___m` = '(b9)',
      `___M` = '(9)',
      `___+` = '(#9)',
         #Fourths
      `____o` = '(b4)',
      `____P` = '(4)',
      `____+` = '(#4)',
         #Sixths
      `_____o` = '(b6)',
      `_____m` = '(m6)',
      `_____M` = '(M6)',
      `_____+` = '(#6)',
    #Tertian
         # Triads
      oo = "dim(b3)",
      mo = "dim",
      mP = "min",
      Mo = "maj(b5)",
      MP = "",
      `M+` = "aug",
      `m+` = 'min(aug)',
         # 7ths
      ooo = 'dim7(b3)',
      moo = 'dim7',
      mom = 'min7(b5)',
      mPm = 'min7',
      MPm = '7',
      MPM = 'maj7',
      `M+m` = '7(#5)',
      `M+M` = 'maj7(#5)',
         # 9ths
      moom = "dim7(b9)",
      momm = 'min(b9,b5)',
      mPmm = 'min(b9)',
      mPmM = 'min9',
      MPmM = "9",
      MPMM = "maj9",
        # Susx (missing third, but 11 or 9 present)
      `_P__o` = 'sus4(b5)',
      `_P__P` = 'sus4',
      `_P__+` = 'sus4(#5)',
    
      )[qual] -> qual
    
    paste0(root, sep, qual)
    
}

    
#' @name humHarmony
#' @export
as.romanNumeral <- function(tharm) {
 root <- getRoot(tharm)
 mode <- getMode(tharm)
 
 generic <- fifth2genericinterval(root)
 rootrel <- root + mode - 1L
 
 
 accidental <- fifth2accidental(rootrel, # the 1L==major is built into fifth2accidental, so must be offset with -1L
                                sharp = "#",
                                flat  = 'b')
 
 naturalize <- root > -2L & root < 6L & accidental != ''
 accidental[naturalize] <- "n"
    
 roman <- c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')[generic]
 
 roman[(root + mode - 1L) > 1L] <- tolower(roman[(root + mode - 1L) > 1L])
 
 ##
 quality <- character(length(root))
 quality[rootrel >=  5L] <- 'o'
 quality[rootrel <= -3L] <- '+'
 
 paste0(accidental, roman, quality)
 
}
