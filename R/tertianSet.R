################################## ###
# tertianSet S4 class ################
################################## ###

## Definition, validity, initialization ####


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
#' + **Root**: \eqn{\pm 1}
#' + **Third**: \eqn{\pm 2}
#' + **Fifth**: \eqn{\pm 4}
#' + **Seventh**: \eqn{\pm 8}
#' + **Ninth**: \eqn{\pm 16}
#' + **Eleventh**: \eqn{\pm 32}
#' + **Thirteenth**: \eqn{\pm 64}
#' 
#' `tertianSet` has many specific methods defined for reading/writing harmonic information.
#' 
#' @name tertianSet
#' @seealso diatonicSet humTonality
#' @export 
setClass('tertianSet', 
         contains = 'diatonicSet',
         slots = c(Extensions = 'integer',
                   Inversion = 'integer')) -> tertianSet

setValidity('tertianSet', 
            function(object) {
                all(object@Extensions >= 0L &
                      object@Extensions <= 2^7 &
                      object@Inversion >= 0 &
                      object@Inversion < 7, na.rm = TRUE)
            })

## Constructors ####

#' @name tertianSet
#' @export
tset <- function(root = 0L, signature = 0L, alterations = 0L, cardinality = 3L, extension = NULL, inversion = 0L) {
    if (is.tonalInterval(root)) root <- root@Fifth
    
    match_size(root = root, signature = signature, alterations = alterations,
               cardinality = cardinality, extension = extension, inversion = inversion, 
               toEnv = TRUE)
    
    if (is.null(extension)) {
      # root <- .ifelse(cardinality == 0L, NA_integer_, root)
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

## Accessors ####

#' @export
getBass <- function(tset) LO5th(tset)[ , 1L]

#' @export
getBassTint <- function(tset) tint( , getBass(tset))

getExtensions <- function(tset) {
    
   rootpos <- tset@Extensions
   inverted <- bitwRotateR(rootpos,  getInversion(tset), nbits = 7L)

   inverted <- inverted + ((inverted - 1) %% 2) # make sure root position is there always 

   bits <- ints2bits(inverted, nbits = 7L) == 1L
   colnames(bits) <- c('Root', nth(c(3, 5, 7, 9, 11, 13)))
   rownames(bits) <- NULL
   bits
   
}


getInversion <- function(tset, inversion.labels = NULL) {
  
  inversion <- tset@Inversion
  if (!is.null(inversion.labels)) inversion <- inversion.labels[1L + (inversion %% length(inversion.labels))]
  inversion
}

#' @export
rootposition <- function(tset) {
  tset@Inversion <- rep(0L, length(tset))
  tset
}


## Formatting methods ####

#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) tset2chordSymbol(x))

## Logic methods ####

### is.methods #####

#' @name tertianSet
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')




## Order/relations methods ####

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

## Arithmetic methods ###


### Addition ###


###################################################################### ###
# Deparsing chord information (tset2x) ###################################
###################################################################### ###

## Chord deparsers ####


### Extracting Pitches ####

#### Line of Fifths ####

#' @export
setMethod('LO5th', 'tertianSet',
         function(x) {
    tset <- x
    
    # if (is.null(Key)) Key <- dset(0, 0)
    # tset <- tset + tset(getRoot(Key), getSignature(Key), cardinality = 0L)
    LO5ths <- callNextMethod(tset, steporder = 4L, inversion = getInversion(x))
    thirds <- getExtensions(tset)
    LO5ths <- LO5ths * thirds
    LO5ths[!thirds] <- NA_integer_
    
    rownames(LO5ths) <- tint2tonalChroma(tint( , getRoot(x)), qualities = FALSE,
                                        step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                                        parts = c('step', 'species'))
    
    LO5ths

})

#### Tonal intervals ####   


### Chord representations ####



tset2alterations <- function(tset, qualities = TRUE, inversion = TRUE, Key = dset(0,0), useKey = FALSE, accidental.naturals = TRUE, ...) {
  # this produces either accidentals or qualities, depending on the parts argument
  
  if (!inversion) tset <- rootposition(tset)
  bass <- getBass(tset)
  
  if (!useKey) {
    tset <- tset - getRoot(tset)
  }
  if (!accidental.naturals) {
    tset <- tset - getRoot(Key)
    Key <- Key - getRoot(Key)
  }
  
  LO5ths <- LO5th(tset)
  tints <- tint( , c(LO5ths))
  figures <- tint2tonalChroma(tints,  Key = Key, qualities = qualities, complex = FALSE, useKey = useKey, ...)
  
  # colnames(figures) <- extensions
  # rownames(figures) <- tint2simplepitch(tint( , bass), Key = dset(0, 0), quality.cautionary = TRUE)
  figures %dim% LO5ths
  
}


tset2extensions <- function(tset, extension.simple = FALSE, inversion = TRUE, ...) {
  extensions <- c(1L, 3L, 5L, 7L, 9L, 11L, 13L)
  if (extension.simple) extensions <- ((extensions - 1L) %% 7L) + 1L
  
  if (!inversion) tset <- rootposition(tset)
  
  extensions <- matrix(extensions, byrow = TRUE, ncol = 7L, nrow = length(tset))
  colnames(extensions) <- c('Root', nth(c(3, 5, 7, 9, 11, 13)))
  
  extensions[!getExtensions(tset)] <- NA_integer_
  
  extensions
}



triadQualify.Roman <- function(root, triad, triad.labels = c(), triad.lowercase = c('diminish', 'minor'), triad.show = c('diminish', 'augment')) {
  setoptions(triad.labels) <- c(major = 'M', minor = 'm', diminish = 'o', augment = '+')
  triad.labels <- unlist(triad.labels)
  
  root[triad %in% triad.labels[triad.lowercase]] <- tolower(root[triad %in% triad.labels[triad.lowercase]])
  triad[!triad %in% triad.labels[triad.show]] <- ""
  
  paste0(root, triad)
  
}

tset2triadLabel <- function(tset, 
                            major = 'M', minor = 'm', diminish = 'o', augment = '+', perfect = 'P',
                            ...) {
  
  qualities <- tset2alterations(tset, qualities = TRUE, inversion = FALSE,  step = FALSE, ...,
                                major = major, minor = minor, diminish = diminish, augment = augment, perfect = perfect)
  qualities[!is.na(qualities) & nchar(qualities) > 1L] <- .paste('(',  qualities[!is.na(qualities) & nchar(qualities) > 1L], ')')
  # rownames(qualities) <- root
  
  
  triadnotes <- qualities[ , c('3rd', '5th'), drop = FALSE]
  triad <- character(nrow(triadnotes))
  
  # incomplete triads
  incompletetriad <- rowSums(is.na(triadnotes)) > 0
  if (any(incompletetriad)) {
    inc <- which(!is.na(triadnotes), arr.ind = TRUE)
    triad[inc[ , 'row']] <- paste0('(', triadnotes[inc], c(3,5)[inc[ , 'col']], ')')
  }
  
  ##
  reductions <- matrix(ncol = 3, nrow = 4, dimnames = list(c(diminish, minor, major, augment), c(diminish, perfect, augment)))
  reductions[] <- paste0('(', outer(rownames(reductions), colnames(reductions), paste0), ')')
  
  reductions <- local({
                       reductions[minor, diminish] <- diminish
                       reductions[minor, perfect] <- minor
                       reductions[major, perfect] <- major
                       reductions[major, augment] <- augment
                       reductions[major, diminish] <- '(b5)'
                       reductions
                     }
  )
  # BAD FIX:
  # triadnotes[triadnotes[ , '5th'] == '','5th'] <- 'P'
  known <- triadnotes[ , '3rd'] %in% rownames(reductions) & triadnotes[ , '5th'] %in% colnames(reductions)
  triad[!incompletetriad & known] <- reductions[triadnotes[!incompletetriad & known, , drop = FALSE]]
  triadnotes[is.na(triadnotes)] <- 'no'
  triad[!incompletetriad & !known] <- paste0('(', triadnotes[!incompletetriad & !known , '3rd'], 
                                             triadnotes[!incompletetriad & !known , '5th'], ')')
  
  triad
  
  
}


reduceFigures <- function(alterations, extensions, inverted, 
                          extension.which = c(3, 5, 7, 2, 4, 6), extension.shorthand = TRUE, 
                          extension.add = TRUE, extension.sus = TRUE, 
                          extension.sep = '', ...) {
  if (is.null(extensions)) extensions <- array("", dim = dim(alterations))
  if (is.null(alterations)) alterations <- array("", dim = dim(extensions))
  
  
  present <- !is.na(alterations) 
  tags <- array("", dim = dim(alterations))
  
  if (any(!inverted) && extension.sus) {
    
    nines.elevens <- col(extensions) %in% 5:6 & present
    sus <- sweep(nines.elevens, 1, !present[ , '3rd'] & !inverted, `&`)
    extensions[sus] <- ((extensions[sus] - 1L) %% 7L) + 1L
    tags[sus] <- 'sus'
  }
  
  if (any(!inverted) && extension.add) {
    
    nines.11s.13s <- col(extensions) %in% 5:7 & present
    adds <- sweep(nines.11s.13s, 1, !present[ , '7th'] & !inverted, `&`) 
    
    if (extension.sus) adds <- adds & !sus
    extensions[adds] <- ((extensions[adds] - 1L) %% 7L) + 1L
    tags[adds] <- 'add'
  }
  
  #
  if (extension.shorthand) {
    topmost <- do.call('pmax', as.data.frame(col(present) * present))
    nottopmost <- sweep(col(extensions), 1, topmost, '<') | col(extensions) <= 3L 
    
    extensions[nottopmost & alterations == ""] <- NA_integer_
    
  }
  
  alterations[] <- .paste(tags, alterations, extensions, fill = "")
  figures <- alterations[ , nth(extension.which), drop = FALSE]
  
  #
  figures[figures != ""] <- .paste(extension.sep[1], figures[figures != ""], extension.sep[2], na.if = all, sep = '')
  Reduce(paste0, as.data.frame(figures))
  
}


tset2tonalHarmony <- function(tset,
                              parts = c('root', 'quality', 'figuration'), 
                              root = TRUE, quality = TRUE, figuration = TRUE, inversion = TRUE, bass = FALSE, 
                              root_func = tint2romanRoot, bass_func = root_func, quality_func = tset2triadLabel,
                              qualifyRoot = triadQualify.Roman, 
                              keyed = FALSE, of = NULL, useKey = FALSE, 
                              inversion.labels = letters,
                              sep = '', ...) {
  parts <- matched(parts, c('root', 'quality', 'figuration', 'inversion', 'bass'))
  
  
  root      <- if (root) root_func(getRootTint(tset), Key = of, useKey = useKey, ...) 
  bass      <- if (bass) .ifelse(getInversion(tset) > 0, bass_func(getBassTint(tset), Key = of, useKey = useKey, ...), "")
  
  
  quality  <- if (quality) {
    quality <- quality_func(tset, Key = of, ...)
    if (!is.null(qualifyRoot)) root <- qualifyRoot(root, quality) 
    quality
  }
  
  figuration <- if (figuration) {
    extensions <- tset2extensions(tset, inversion = inversion, ...)
    alterations <- tset2alterations(tset, qualities = FALSE, Key = of, useKey = useKey, step = FALSE, inversion = inversion, flat = 'b', ...) 
    reduceFigures(alterations, extensions, getInversion(tset) > 0L, ...)
  }
  
  inversion <- if (inversion) getInversion(tset, inversion.labels = inversion.labels)
  
  tonalharmony <- pasteordered(parts, root = root, figuration = figuration, inversion = inversion, bass = 'bass', sep = sep)
  
  tonalharmony  
}



tset2figuredBass <- function(tset, extension.shorthand = TRUE, ...) {
  overdot(tset2tonalHarmony(tset, parts = c('inversion','accidentals', 'extensions'), qualifyTriad = NULL, inversion.labels = tint2simplepitch,
                            steps = tint2simplepitch,
                            extension.shorthand = FALSE, extension.which = c(7,6,5,4,3,2), extension.simple=TRUE,
                            extension.sus = FALSE, extension.add = FALSE,
                            inversion = TRUE, figure.Key = TRUE, Key = dset(0, 0), 
                            sep = '', ...)) -> figures
  
  if (extension.shorthand) {
    figures <- stringr::str_replace(figures,'([^913])753|^753', '\\17')
    figures <- stringr::str_replace(figures, '([^9713])63|^63', '\\16')
    figures <- stringr::str_replace(figures, '([^9713])653|^653', '\\165')
    figures <- stringr::str_replace(figures, '([^9713])643|^643', '\\143')
    figures <- stringr::str_replace(figures, '([^9713])642|^642', '\\142')
  }
  
  figures
  
  
}


tset2romanNumeral <- function(tset,  ...) {
  overdot(tset2tonalHarmony(tset, parts = c('root', 'quality', 'figuration', 'inversion'), 
                            root_func = tint2romanRoot, 
                            qualifyTriad = triadQualify.Roman, 
                            inversion.labels = letters,
                            extension.shorthand = TRUE, extension.which = c(7,2,4,6), extension.simple=TRUE,
                            extension.sus = TRUE, extension.add = TRUE,
                            inversion = FALSE, useKey = FALSE, of = dset(0,0), ...))
  
}

tset2sciChord <- function(tset,  ...) {
  overdot(tset2tonalHarmony(tset, parts = c('root', 'quality'), 
                            steps = tint2simplepitch, quality.labels =c(diminish = 'o', augment = '+'),
                            qualifyTriad = paste0, quality.cautionary = TRUE,
                            extension.shorthand = FALSE, extension.which = c(7,2,4,6), extension.simple=FALSE,
                            accidental.naturals = TRUE,
                            extension.add=FALSE, extension.sus = FALSE,
                            inversion = FALSE, figure.Key = FALSE, Key = NULL, ...)) 
}


tset2chordSymbol <- function(tset,  ...) {
  overdot(tset2tonalHarmony(tset, parts = c('root', 'accidentals', 'extensions', 'inversion'), 
                            steps = tint2simplepitch, accidental.labels =c(flat = 'b', natural = 'maj', doubleflat = 'o', doublesharp = '+'),
                            qualifyTriad = paste0,
                            inversion.labels = function(x, ...) paste0('/', tint2simplepitch(x)),
                            extension.shorthand = TRUE, extension.which = c(7, 2, 4, 6), extension.simple=FALSE,
                            extension.sus = TRUE, extension.add = TRUE,
                            triad.labels = c(major = ''),
                            accidental.naturals = FALSE,
                            inversion = FALSE, figure.Key = FALSE, Key = dset(getRoot(tset), getRoot(tset) - 1L), ...)) -> chords
  
  stringr::str_replace(chords, 'maj7([139]{1,2})', 'maj\\1')
  
}



  
###################################################################### ###
# Parsing chord information (x2tset) #####################################
###################################################################### ###

## Chord parsers ####


### Extensions/Figuration ####

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




triad2sciQuality <- function(triad, extensionQualities, 
                             major = 'M', minor = 'm', perfect = 'P', diminish = 'o', augment = '+') {
  
  
  triadQualities <- local({
                           quals <- cbind(perfect, 
                                          c(major, minor, minor, major), 
                                          c(perfect, perfect, diminish, augment))
                           rownames(quals) <- c(major, minor, diminish, augment)
                           cbind(quals[triad, , drop = FALSE ], '.', '.', '.', '.')
                         })
  extensionQualities[col(extensionQualities) <= 3L & extensionQualities == '.'] <- triadQualities[col(extensionQualities) <= 3L & extensionQualities == '.']
  extensionQualities[ , 2L:3L] <- triadQualities[ , 2L:3L]
  
  apply(extensionQualities, 1L, paste, collapse = '')
  
}



extensions2qualities <- function(root, figurations, triadalts, Key = NULL, ...) {
  
  
  mode <- if(is.null(Key)) 0L else getMode(Key)
  
  dots <- rep('.', 7L)
  Map(function(r, deg, acc, m) {
    redundantroot <- deg == 1 & acc == ""
    deg <- deg[!redundantroot]
    acc <- acc[!redundantroot]
    if (length(deg) == 0L) return(dots)
    step <- step2tint(deg, step.labels = 1L:14L)
    alterations <- specifier2tint(acc, step, qualities = FALSE,  ...)
    
    qualities <- tint2specifier((step %% dset(-r, m - r)) + alterations, qualities = TRUE, ...)
    
    dots[1L + ((deg - 1L) %/% 2L)] <- qualities
    dots
  }, root, figurations$Degrees, figurations$Accidentals, rep(mode, length(root))) |> do.call(what = 'rbind')
  
  
  
}

parseFiguration <- function(str, figureFill = TRUE, flat = 'b', ...) {
  
  # str[str == ''] <- '35'
  figures <- stringr::str_extract_all(str, 
                                      overdot(makeRE.tonalChroma(step.labels = 13:1, 
                                                                 parts = c('species', 'step'), qualities = FALSE,
                                                                 flat = flat,
                                                                 collapse = TRUE,
                                                                 ...)),
                                      simplify = FALSE)
  
  figures <- lapply(figures, REparse, res =  overdot(makeRE.tonalChroma(step.labels = 13:1, 
                                                                        parts = c('species', 'step'), qualities = FALSE,
                                                                        flat = flat, 
                                                                        collapse = FALSE,
                                                                        ...)))
  lapply(figures, 
         function(parsedfig) {
           parsedfig <- if(!is.null(parsedfig)) as.data.table(parsedfig) else data.table(species = character(2L), step = c('5', '3'))
           parsedfig[ , Explicit := TRUE]
           
           if (!any(parsedfig$step == '1')) parsedfig <- rbind(parsedfig, data.table(species = '', step = '1', Explicit = FALSE)) 
           
           
           ## 
           parsedfig[ , step := as.integer(step)]
           parsedfig[ , third := ifelse(step %% 2L == 0L, step - 7L, step)]
           parsedfig[ , third := as.integer((third - min(third)) %/% 2L)] # translates steps -> thirds
           #
           parsedfig <- parsedfig[!duplicated(third)]
           inversion <- parsedfig[step %in% c(1L, 8L, 15L), third[1]]
           
           # extensions
           setorder(parsedfig, step)
           
           parsedfig <- parsedfig[  , {
             newthird <- if (length(third) == 0L) {
               0L:2L 
             } else {
               newthird <- if (all(step <= 3L)) 0:max(third) else 0L:max(2L, max(third))
               
               if (all(step == 5L) && species[1] == "") newthird <- setdiff(newthird, 1L) 
               gaps <- diff(step[Explicit])
               skips <- gaps > 2L & head(species[Explicit] == '', -1L)
               if (any(skips)) for (g in gaps[gaps > 2L]) newthird <- setdiff(newthird, third[Explicit][which(skips & gaps == g)] + (1L:((g - 2L) / 2L)))
               newthird
               
             }
             
             newaccidentals <- species[match(newthird, third)]
             newaccidentals[is.na(newaccidentals)] <- ""
             
             data.table(species = newaccidentals, step = newthird * 2L + 1L, third = newthird, Explicit = FALSE)
           }]
           
           
           extensionInt <- parsedfig[ , as.integer(sum(2^third))]
           #
           data.table(Inversion   = inversion, 
                      Extension   = extensionInt, 
                      Degrees     = list(parsedfig$step),
                      Accidentals = list(parsedfig$species))
           
           
         }) |> do.call(what = 'rbind')
}


### Chord representations ####  

romanNumeral2tset <- function(str, Key = NULL, of = dset(0,0), diminish = 'd', augment = 'A', ...) {

  
  of <- dset(0, getMode(of), of@Alteration)
  
  REparse(str,
          makeRE.romanChord(..., diminish = diminish, augment = augment, collapse = FALSE),
          parse.exhaust = FALSE, parse.strict = FALSE,
          toEnv = TRUE)  # adds accidental numeral triadalt figurations to the environment
  
  root <- tonalChroma2tint(paste0(accidental, toupper(numeral)), useKey = TRUE,
                           parts = c('species', 'step'), qualities = FALSE,
                           step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                           Key = of, ...)@Fifth
  
  figurations <- parseFiguration(figurations)
  
  ### quality of degress
  # extension qualities
  qualities <- extensions2qualities(root, figurations, triadalt, Key = of, flat = 'b', diminish = diminish, augment = augment, ...)
  # incorporate quality of triad
  qualities <- local({
    triad <- rep('M', length(numeral))
    triad[numeral == tolower(numeral)] <- 'm'
    triad[triadalt == diminish] <- diminish
    triad[triadalt == augment]  <- augment
    
    triad2sciQuality(triad, qualities, ...)
  })

  qualitytset <-  sciQualities2tset(qualities, ..., diminish = diminish, augment = augment)
  
  # if 1 is altered!
  root <- root + setNames(c(-7L, 7L, 0L), c(diminish, augment, 'P'))[stringr::str_sub(qualities, 1L, 1L)]
    
  ###
 
  return(tset(root, root + getMode(qualitytset),
              alterations = qualitytset@Alteration,
              extension = figurations$Extension,  
              inversion = figurations$Inversion))
  
}



sciQualities2tset <- function(str, ...) {
  
  chord <- stringr::str_pad(str, width = 7L, side = 'right', pad = '.')
  
  dset <- qualities2dset(chord, steporder = 4L, allow_partial = TRUE, ...)
  
  
  cardinalities <- sapply(stringr::str_locate_all(str, '[^.]'), function(x) x[nrow(x), 1L])
  
  tset(dset@Root, dset@Signature, dset@Alteration, cardinalities)
  
}

sciChord2tset <- function(str, ...) {

   
    REparse(str,
            makeRE.sciChord(..., collapse = FALSE),
            toEnv = TRUE) -> parsed
  
    root <- tonalChroma2tint(paste0(step, species), parts = c('step', 'species'), qualities = FALSE, ...)@Fifth
    
    # qualities
    quality <- local({
      quality <- stringr::str_pad(quality, width = 5L, side = 'right', pad = '.')
      quality <- do.call('rbind', strsplit(quality, split = ""))
      triad <- quality[ , 1]
      extensions <- cbind('.', '.', '.', quality[ , -1L, drop = FALSE])
      
      triad2sciQuality(triad, extensions, ...)
    })
    
    sciQualities2tset(quality,  ...) + tset(root, root)
    
}

##... Numbers

integer2tset <- function(x) tset(x, x)

## Chord Parsing Dispatch ######################################


### Parse 2tset generic and methods ####

#' @name tertianSet
#' @export tertianSet figuredBass sciChord chordSymbol romanChord
tertianSet   <- function(x, ...) UseMethod('tertianSet')
figuredBass  <- function(x, ...) UseMethod('figuredBass')
sciChord     <- function(x, ...) UseMethod('sciChord')
chordSymbol  <- function(x, ...) UseMethod('chordSymbol')
romanChord   <- function(x, ...) UseMethod('romanChord')


#### Numbers ####

#### Characters ####

#### setAs tertianSet ####



#' @export
setAs('integer', 'tertianSet', function(from) integer2tset(from))
#' @export
setAs('numeric', 'tertianSet', function(from) integer2tset(as.integer(from)))
#' @export
setAs('character', 'tertianSet', function(from) char2tset(from))
#' @export
setAs('matrix', 'tertianSet', function(from) tertianSet(c(from)) %dim% from)





 
###################################################################### ### 
# Translating Chord Representations (x2y) ################################
###################################################################### ### 

## Chord transform documentation ####


#' Tertian set representations
#' 
#' Tertian sets can be read/wrote in various ways.
#' 
#' @name tertianRepresentations
NULL

## Chord transform maker ####



### Chord transformers ####

###################################################################### ### 
# Manipulating tertian sets ##############################################
###################################################################### ### 

#' @export
tertianSet.tertianSet <- force

#' @export
tertianSet.numeric <- integer2tset %.% as.integer

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


char2tset           <- humdrumDispatch(doExclusiveDispatch = FALSE,
                                       'romanChord: makeRE.romanChord(...)' = romanNumeral2tset,
                                       'sciChord: makeRE.sciChord(...)' = sciChord2tset)

char2tset_partition <- humdrumDispatch(doExclusiveDispatch = FALSE,
                                       'keyof: makeRE.tertianPartition(...)' = mapPartition(char2tset),
                                       'romanChord: makeRE.romanChord(...)' = romanNumeral2tset,
                                       'sciChord: makeRE.sciChord(...)' = sciChord2tset)

#' @export
tertianSet.character <- force %.% char2tset_partition

#.... set as


###.. tset as x ####

#' @export
romanChord.tertianSet <- tset2romanNumeral
#' @export
sciChord.tertianSet <- force %.% tset2sciChord

###. x as y ####

#.... numeric -> y ####

#' @export
romanChord.numeric <- tset2romanNumeral %.% tertianSet.numeric
#' @export
sciChord.numeric <- tset2sciChord %.% tertianSet.numeric

#.... character -> y ####

#' @export
romanChord.character <- re.place %.% tset2romanNumeral %.% tertianSet.character
#' @export
sciChord.character <- re.place %.% tset2sciChord %.% tertianSet.character



##### Tonal transform methods ####
