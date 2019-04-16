
#######################################-
##################tonalHarmonies S4 class ####
#######################################-

#' Tonal Harmonies
#' 
#' 
#' (\code{tonalHarmony} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}.)
#' 
#' @name humHarmony
#' @seealso humTonality
#' @export 
setClass('tonalHarmony', slots = c(Root = 'integer', Quality = 'integer', 
                                   Extensions = 'matrix', 
                                   Bass = 'integer'))


setValidity('tonalHarmony', 
            function(object) {
                extens <- object@Extensions
                root <- object@Root
                quality <- object@Quality
                bass <- object@Bass
                
                
                is.logical(extens) &&
                    ncol(extens) == 7L &&
                    length(root) == nrow(extens) &&
                    length(root) == length(quality) &&
                    length(root) == length(bass) 
                    
            })


#' @name humHarmony
#' @export
setMethod('show', signature = c(object = 'tonalHarmony'), function(object) { cat(as.sciChord(object)) })


#' @name humHarmony
#' @export
getRoot    <- function(tharm) tharm@Root
#' @name humHarmony
#' @export
getQuality <- function(tharm) tharm@Quality
#' @name humHarmony
#' @export
getExtensions <- function(tharm) tharm@Extensions
#' @name humHarmony
#' @export
getBass <- function(tharm) tharm@Bass


#' @name humHarmony
#' @export
tharm <- function(root = 0L, card = 3L, quality = 1L, bass = root) {
 extensions <- lower.tri(matrix(ncol = 7, nrow = 7), diag = FALSE)[card + 1L, , drop = FALSE]
    
 new('tonalHarmony', 
     Root = as.integer(root), 
     Quality = as.integer(quality),
     Bass = as.integer(bass),
     Extensions = extensions)   
}

#' @export
getFifths <- function(tharm) {
    fifths <-  sweep(getExtensions(tharm), 2, seq(0, by = 4 , length.out = 7), FUN = `*`)
    
    quality <- getQuality(tharm)
    fifths <- ((fifths + quality) %% 7) - quality 
    
    
    fifths + getRoot(tharm)
}


getSciQuality <- function(tharm, collapse = TRUE) {
    root <- getRoot(tharm)
    
    actualfifths   <- getFifths(tharm)[-1]
    
    majorversion   <- getFifths(setMode(tharm, -root + 1L))[-1]
    
    qualification <- (actualfifths - majorversion) %/% 7
    qualification <- c('+', 'M', 'm', 'o')[2 - qualification]
    if (length(qualification) > 4L) qualification[5L] <- c(o = "o", m = 'P', M = '+')[qualification[5L]]
    
    triad <- switch(paste(qualification[1:2], collapse = ''),
                    "++" = "?",
                    "M+" = "+",
                    MM = "M",
                    mM = "m",
                    mm = 'o',
                    om = "_",
                    "?")
    
    complete <- c(triad, qualification[-1:-2])
    
    if (collapse) paste(complete, collapse = '') else complete
}

#' @name humHarmony
#' @export
as.sciChord <- function(tharm) {
    root <- getRoot(tharm)
    tonalname <- fifth2tonalname(root, kernFlats = FALSE)
    
   
    quality <- getSciQuality(tharm)
    
    paste0(tonalname, quality)
}


#' @name humHarmony
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
