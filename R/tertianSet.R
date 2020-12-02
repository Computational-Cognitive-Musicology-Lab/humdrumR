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
#' The only structural addition, compared to `diatonicSet`, is the `Extensions` slot.
#' This slot indicates which tertian chord members are active in the chord.
#' There are seven possible chord members: 
#' the root, third, fifth, seventh, ninth, eleventh, and thirteenth.
#' Every possible combination of these seven degrees is represented by a single integer, corresponding
#' to the 7-bit representation of on/offs on the seven degrees in reverse order (13, 11, 9, 7, 5, 3, 1).
#' For example, the integer `15` corresponds to a seventh chord: in binary, 15 is `0001111`.
#' The initial three zeros indicate that the 13th, 11th, and 9th are *not* part of the harmony, while the four ones
#' indicate that the root, third, fifth, and seventh *are* part of the harmony.
#' Ultimately, adding or removing a chord degree from a harmony can be achieved by adding the power of
#' two associated with that degree: 
#' 
#' + **Root**: $\pm 1$
#' + **Third**: $\pm 2$
#' + **Fifth**: $\pm 4$
#' + **Seventh**: $\pm 8$
#' + **Ninth**: $\pm 16$
#' + **Eleventh**: $\pm 32$
#' + **Thirteenth**: $\pm 64$
#' 
#' `tertianSet` has many specific methods defined for reading/writing harmonic information.
#' 
#' 
#' @seealso diatonicSet humTonality
#' @export 
setClass('tertianSet', 
         contains = 'diatonicSet',
         slots = c(Extensions = 'integer',
                   Inversion = 'integer'))

setValidity('tertianSet', 
            function(object) {
                all(object@Extensions >= 0L &
                      object@Extensions <= 2^7 &
                      object@Inversion >= 0 &
                      object@Inversion < 7)
            })

#' @name humDiatonic
#' @export
tset <- function(root = 0L, signature = 0L, alterations = 0L, cardinality = 3L, extension = NULL, inversion = 0L) {
    if (is.tonalInterval(root)) root <- root@Fifth
    
    match_size(root = root, signature = signature, alterations = alterations,
               cardinality = cardinality, extension = extension, inversion = inversion, 
               toEnv = TRUE)
    
    if (is.null(extension)) {
      root <- .ifelse(cardinality == 0L, NA_integer_, root)
      extension <- c(0L, 1L, 3L, 7L, 15L, 31L, 63L, 127L)[cardinality + 1L]
    } else {
      root[extension == 0L] <- NA_integer_
    }
   
    
    new('tertianSet', 
        Root = as.integer(root), 
        Signature = as.integer(signature), 
        Alteration = as.integer(alterations), 
        Extensions = extension,
        Inversion = as.integer(inversion))
}

##...accessors ####

getExtensions <- function(tset) {
    if (hasdim(tset)) tset <- tset[ , ncol(tset)]
    
   rootpos <- tset@Extensions
   inverted <- bitwRotateR(rootpos,  getInversion(tset), nbits = 7L)

   inverted <- inverted + ((inverted - 1) %% 2) # make sure root position is there always 

   as.logical(ints2bits(inverted, nbits = 7L))
   
}




getInversion <- function(tset) {
  if (hasdim(tset)) tset <- tset[ , ncol(tset)]
  
  tset@Inversion
}


####. vector/core methods ####
    

#' @name humDiatonic
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


###.. formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) tset2chordSymbol(x))

####. logic methods ####

###.. order/relations methods ####

#' @name diatonicSet
#' @export
setMethod('==', signature = c('tertianSet', 'tertianSet'),
          function(e1, e2) {
              checkSame(e1, e2, "==")
              f1 <- LO5th(e1)
              f2 <- LO5th(e2)
              
              same <- f1 == f2 | (is.na(f1) & is.na(f2))
              
              rowSums(same, na.rm = TRUE) == 7L
          })


###.. arithmetic methods ####

##... addition ####



##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


###. x to line-of-fifths ####

#' @export
setMethod('LO5th', 'tertianSet',
         function(x) {
    tset <- x
    # alterations <- getAlterations(tset, sum = TRUE)
    
    LO5ths <- callNextMethod(tset, steporder = 4L, inversion = getInversion(x))
    thirds <- getExtensions(tset)
    
    LO5ths <- LO5ths * thirds
    LO5ths[!thirds] <- NA_integer_
    
    # if (any(alterations != 0L)) LO5ths <- sweep(LO5ths, 1, alterations, alterFifthSet)
    
    colnames(LO5ths) <- c('root', nth(c(3,5, 7, 9,11,13)))
    rownames(LO5ths) <- tint2tonalChroma(tint( , getRoot(tset, sum = TRUE)), 
                                         step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                                         parts = c('steps', 'accidentals'))
    
    LO5ths
})


##### To/From extensions ####    

###. extensions to x ####

extension2bit <- function(str) {
 
  extensions <- stringr::str_extract_all(str, captureRE(c('7', '9', '11', '13', 'sus4', 'add6', 'add2')))
  
  bit <- 7L # triad
  
  sapply(extensions,
         function(exten) {
           if (any(exten %in% c('9', '11', '13')) & !any(exten == '7')) bit <- bit + 8L
           
           if (any(stringr::str_detect(exten, 'sus'))) bit <- bit - 2L
           
           
           
           exten <- stringr::str_replace(exten, captureRE(c('65', '43', '42')), '7')
           exten <- stringr::str_replace(exten, 'add2', '9')
           exten <- stringr::str_replace(exten, 'add9', '9')
           exten <- stringr::str_replace(exten, 'sus4', '11')
           exten <- stringr::str_replace(exten, 'add6', '13')
           
           bit + sum(c(`7` = 8L, `9` = 16L, `11` = 32L, `13` = 64L)[exten])
         })
  
  
  
}



##### To/From tertianSets ####    

###.. tset to pitches ####


####. tset to x ####


# As "scientific chord label" (i.e., "Cmm" or "EbMm")

getSciQuality <- function(tset, collapse.triad = TRUE, collapse = TRUE, quality.labels = c()) {
  setoptions(quality.labels) <- c(major = 'M', minor = 'm', 
                                  diminish = 'o', augment = '+', 
                                  perfect = 'P')
   
    LO5ths <- LO5th(tset)
    LO5ths <- sweep(LO5ths, 1, LO5ths[ , 1], `-`)[ , -1, drop = FALSE] # center on 0 then remove root
    
    qualities <- array("", dim = dim(LO5ths))
    qualities[!is.na(LO5ths)] <- LO5th2quality(c(LO5ths[!is.na(LO5ths)]),
                                               quality.labels = quality.labels,
                                               quality.cautionary = TRUE,
                                               quality.memory = FALSE)
    
    qualities[nchar(qualities) > 1L] <- .paste('(',  qualities[nchar(qualities) > 1L], ')')
    if (collapse.triad) {
        triads <- sapply(apply(qualities[ , 1:2, drop = FALSE], 1, paste, collapse = ''),
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
    
    colnames(qualities) <- nth(c(3, 5, 7, 9, 11, 13))
    
    if (collapse)  apply(qualities, 1, paste, collapse = '') else qualities
    
}


tset2sciChord <- function(tset) {
    root <- tset@Root
    tonalChroma <- tint2tonalChroma(tint( , root), parts = c('steps', 'accidentals'),
                                    accidental.labels = c(flat = 'b'), step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'))
   
    quality <- getSciQuality(tset)
    
    .ifelse(!is.na(root) & !is.na(tonalChroma), 
           .paste(tonalChroma, quality), 
           NA_character_)
}


tset2chordSymbol <- function(tset, sep = '') {
  # print highest extension
  # if 7th is major, print "maj"
  
  root <- tset@Root
  tonalChroma <- tint2tonalChroma(tint( , root), parts = c('steps', 'accidentals'),
                                  accidental.labels = c(flat = 'b'), step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'))
  
  qualities <- getSciQuality(tset, collapse = FALSE, collapse.triad = FALSE,
                             quality.labels = c(major = 'M', minor = 'b', perfect = 'P', augment = '#', diminish = 'o'))
  present <- qualities != ""
  

  
  # triad qual + 
  triad <- character(length(tset))
  triad[qualities[ , '3rd'] == 'b' ] <- 'min'
  triad[qualities[ , '3rd'] == 'M' & qualities[ , '5th'] == '#'] <- '+'
  
  sus <- ifelse(!present[ , '3rd'] & qualities[ , '11th'] == 'P', 'sus4', '')
  sus[!present[ , '3rd'] & qualities[ , '9th'] == 'M'] <- 'sus2'
  qualities[sus == 'sus4', '11th'] <- ""
  qualities[sus == 'sus2',  '9th'] <- ""
  
  
  extra5   <- ifelse(qualities[ , '5th'] == 'o', 'b5', '')
  extra5[qualities[ , '5th'] == '+' & qualities[ , '3rd'] == 'm'] <- '#5'
  
  qualities <- qualities[ , -1:-2, drop = FALSE]
  
  # extensions
  extensions <- matrix(c(7, 9, 11, 13), nrow = length(tset), ncol = 4, byrow = TRUE)
  extensions[!present[ , '7th'], 4] <- 6
  extensions[qualities == ""] <- ""
  
  topmost <- which(present, arr.ind = TRUE)
  topmost <- tapply(topmost[ , 'col'], topmost[ , 'row'], max) - 2L
  nottopmost <- sweep(col(qualities), 1, topmost, '<')
  
  altered <- sweep(qualities, 2, c('b', 'M', 'P', 'M'), `!=`) & qualities != ""
  
  maj7 <- ifelse(qualities[ , '7th'] == 'M', 'maj', '')
  altered[qualities[ , '7th'] == 'M' & !apply(altered[, -1], 1, any), 1] <- FALSE
  qualities[qualities[ , '7th'] == 'M', 1] <- ""
  
  
  extensions[nottopmost & !altered] <- ""
  qualities[!altered] <- ""
  
  extensions[] <- paste0(qualities, extensions)
  extensions <- apply(extensions, 1, paste, collapse = '')
  
  
  paste0(tonalChroma, triad, maj7, extensions, sus, extra5)
  # 
    # scichord <- tset2sciChord(tharm)
    # root <- stringr::str_extract(scichord, '^[A-G][b#]*')
    # qual <- stringr::str_remove(scichord, '^[A-G][b#]*')
    # 
    # c(M = "",
    #   m = "min",
    #   o = "dim",
    #   `+` = "aug",
    #   MM = "maj7",
    #   MMM = 'maj9',
    #   MMMP = 'maj11',
    #   `MMM+` = 'maj9(#11)',
    #   `MMM+M` = 'maj13(#11)',
    #   Mm = "7",
    #   Mmm = "b9",
    #   MmM = "9",
    #   `Mm+` = "7(#9)",
    #   `MmM+` = "#11",
    #   MmMPM = '13',
    #   MmMPm = 'b13',
    #   mm = "min7",
    #   mmM = "min9",
    #   mmm = "min7(b9)",
    #   mmMP = "min11",
    #   MmMP = "11",
    #   om = "min7(b5)",
    #   omm = "min7(b5b9)",
    #   oo = "dim7",
    #   `+m` = "aug7",
    #   `+M` = "aug(maj7)",
    #   `_` = 'dim(b3)',
    #   `_m` = 'dim7(b5b3)',
    #   `_o` = 'dim7(b3)'
    #   )[qual] -> qual
    # 
    # IfElse(!is.na(root) & !is.na(qual), .paste(root, sep, qual), NA_character_)
    
}


####. x to text ####



romanNumeral2tset <- function(str, accidental.labels = c()) {
  setoptions(accidental.labels) <-  c(natural = 'n', flat = 'b', sharp = '#')
  
  accidentalRE <- captureUniq(accidental.labels, zero = TRUE)
  
  Inversion <- stringr::str_extract(str, captureRE(c('6', '63', '64', '65', '43', '42', '2'), '*'))
  str <- stringr::str_replace(str, '65|43|42', '7')
  
  REparse(str,
          list(Accidental = accidentalRE,
               Numeral = "(vii|VII|iii|III|vi|VI|iv|IV|ii|II|v|V|i|I)", 
               TriadQuality = '[o+]?', 
               Extensions = paste0('(', accidentalRE,
                                   captureRE(c('7', '9', '11', '13')), 
                                   '|sus[42]|add[692])*')),
          toEnv = TRUE) -> parsed
  
  bit <- extension2bit(stringr::str_remove_all(Extensions, '[^0-9]*'))
  
  
  
  root <- tonalChroma2tint(paste0(Accidental, toupper(Numeral)), parts = c('accidentals', 'steps'), 
                           accidental.labels = accidental.labels, 
                           step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'))@Fifth
  
  mode <- extension2mode(Extensions, accidental.labels, root, Numeral == tolower(Numeral), TriadQuality)
  
  return(tset(root,mode$Mode, alterations = mode$Alteration,extension = bit))
  
  
  
  
}


sciChord2tertianSet <- function(csym) {
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
