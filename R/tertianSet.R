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
rootPosition <- function(tset) {
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



tset2alterations <- function(tset, Key = dset(0,0), qualities = FALSE, inversion = TRUE, absoluteSpecies = TRUE, 
                             implicitSpecies = FALSE, explicitNaturals = FALSE, ...) {
  # this produces either accidentals or qualities, depending on the parts argument
  
  if (!inversion) tset <- rootPosition(tset)
  bass <- getBass(tset)
  
  if (absoluteSpecies) {
    tset <- tset - getRoot(tset)
    Key <- dset(0, -1L)
  }
  if (explicitNaturals) {
    tset <- tset - getRoot(Key)
    Key <- Key - getRoot(Key)
  }
  
  LO5ths <- LO5th(tset)
  tints <- tint( , c(LO5ths))
  figures <- tint2tonalChroma(tints,  Key = Key, qualities = qualities, complex = FALSE, 
                              implicitSpecies = implicitSpecies, explicitNaturals = explicitNaturals, ...)
  
  # colnames(figures) <- extensions
  # rownames(figures) <- tint2simplepitch(tint( , bass), Key = dset(0, 0), quality.cautionary = TRUE)
  figures %dim% LO5ths
  
}


tset2extensions <- function(tset, extension.simple = FALSE, inversion = TRUE, ...) {
  extensions <- c(1L, 3L, 5L, 7L, 9L, 11L, 13L)
  if (extension.simple) extensions <- genericStep(extensions)
  
  if (!inversion) tset <- rootPosition(tset)
  
  extensions <- matrix(extensions, byrow = TRUE, ncol = 7L, nrow = length(tset))
  colnames(extensions) <- c('Root', nth(c(3, 5, 7, 9, 11, 13)))
  
  extensions[!getExtensions(tset)] <- NA_integer_
  
  extensions
}



# triadQualify.Roman <- function(root, triad, triad.labels = c(), triad.lowercase = c('diminish', 'minor'), triad.show = c('diminish', 'augment')) {
#   setoptions(triad.labels) <- c(major = 'M', minor = 'm', diminish = 'o', augment = '+')
#   triad.labels <- unlist(triad.labels)
#   
#   root[triad %in% triad.labels[triad.lowercase]] <- tolower(root[triad %in% triad.labels[triad.lowercase]])
#   triad[!triad %in% triad.labels[triad.show]] <- ""
#   
#   paste0(root, triad)
#   
# }

tset2triadLabel <- function(tset, root, root.case = TRUE, 
                            major = 'M', minor = 'm', diminish = 'o', augment = '+', ...) {
  
  perfect <- 'P'
  
  qualities <- tset2alterations(tset, qualities = TRUE, inversion = FALSE,  step = FALSE,
                                explicitNaturals = TRUE, implicitSpecies = FALSE,
                                major = major, minor = minor, diminish = diminish, augment = augment, perfect = perfect)
  
  qualities <- qualities[ , c('3rd', '5th'), drop = FALSE]
  thirds <- qualities[ , '3rd', drop = FALSE]
  fifths <- qualities[ , '5th', drop = FALSE]
  
  #
  triadQuality <- rep('?', nrow(qualities))
  

  
  ## prepare labels for known combinations of third and fifth qualities
  reductions <- matrix('?', ncol = 3, nrow = 4, dimnames = list(c(diminish, minor, major, augment), c(diminish, perfect, augment)))
  # reductions[] <- paste0('(', outer(rownames(reductions), colnames(reductions), paste0), ')')
  
  reductions <- local({
                       reductions[minor, diminish] <- diminish
                       reductions[minor, perfect] <- minor
                       reductions[major, perfect] <- major
                       reductions[major, augment] <- augment
                       reductions
                     }
  )
  
  ## get labels
  known <- thirds %in% rownames(reductions) & fifths %in% colnames(reductions)
  
  triadQuality[known] <- reductions[cbind(thirds[known], fifths[known])]
  
  
  if (!is.null(root) && root.case)  {
    root[substr(thirds, 0, 1) %in% c(minor, diminish)] <- tolower(root[substr(thirds, 0, 1) %in% c(minor, diminish)])
    triadQuality[triadQuality %in% c(major, minor)] <- ""
  }
  
  
  list(triadQuality = triadQuality, root = root)
  
  
}


reduceFigures <- function(alterations, extensions, 
                          triadQuality, root.case = FALSE,
                          inversion, step = TRUE,
                          extension.shorthand = TRUE, extension.simple = TRUE,
                          extension.add = TRUE, extension.sus = TRUE, 
                          extension.decreasing = TRUE, 
                          extension.sep = '', flat = '-', minor = 'm', diminish = 'o', ...) {
  if (is.null(extensions)) extensions <- array("", dim = dim(alterations))
  if (is.null(alterations)) alterations <- array("", dim = dim(extensions))
  
  inverted <- inversion > 0L
  
  present <- !is.na(alterations) 
  tags <- array("", dim = dim(alterations))
  
  
  # get rid of alterations that are already taken care of by the quality!
  if (!is.null(triadQuality)) {
    if (root.case) {
      alterations[col(alterations) == 2L & alterations %in% c(flat, minor)] <- ""
      alterations[col(alterations) == 3L & alterations == diminish] <- ""
    }
    alterations[triadQuality != '?', 1:3] <- ""
    
    alterations[col(alterations) <= 3L & alterations == 'n'] <- ""
  }

  
  
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
    # if (!extension.simple && any(inverted)) {
      # extensions[inverted, ] <- genericstep(extensions[inverted, ])
    # }
    chorddegree <- sweep(extensions, 1, 2L * inversion, '+')
    chorddegree[chorddegree %in% c(8L, 10L, 12L)] <- chorddegree[chorddegree %in% c(8L, 10L, 12L)] - 7L
    chorddegree[which(chorddegree > 13L, arr.ind = TRUE)] <- chorddegree[which(chorddegree > 13L, arr.ind = TRUE)] - 14L
    
    hide <- sweep(col(chorddegree), 1, apply(chorddegree, 1, \(row) max(4L, which.max(row))), '<') 
    
    extensions[(hide & alterations == "") | extensions == 1L] <- NA_integer_
    
  }
  
  
  # order
  orders <- apply(extensions, 1, order, decreasing = extension.decreasing, na.last = NA, simplify = FALSE)
  
  alterations[] <- .paste(tags, alterations, if (step) extensions, fill = "", na.if = all)
  figures <- Map(\(i,j) alterations[i,j], 1:nrow(alterations), orders)
  
  sapply(figures, \(f) paste(.paste(extension.sep[1], f, extension.sep[2], na.if = all, sep = ''), collapse = ''))
  
  #
  
}


tset2tonalHarmony <- function(tset,
                              parts = c('root', 'quality', 'figuration'), 
                              root = TRUE, quality = TRUE, figuration = TRUE, inversion = TRUE, bass = FALSE, 
                              figurationArgs = list(),
                              root_func = tint2romanRoot, bass_func = root_func, bass.sep = '/',
                              root.case = TRUE,
                              Key = NULL, 
                              inversion.labels = NULL,
                              sep = '', ...) {
  
  parts <- matched(parts, c('root', 'quality', 'figuration', 'inversion', 'bass'))
  
  
  bass      <- if (bass) ifelse(!root | (getInversion(tset) > 0), paste0(bass.sep, bass_func(getBassTint(tset) - tint(1L, 0L), Key = Key, ...)), "")
  root      <- if (root) root_func(getRootTint(tset), Key = Key, ...) 
  
  quality   <- if (quality) {
    {quality; root} %<-% tset2triadLabel(tset, root, root.case, ...)
    quality
  }
 
  
  figuration <- if (figuration) {
    extensions  <- do.call('tset2extensions', c(list(tset, inversion = inversion), figurationArgs))
    alterations <- do.call('tset2alterations', c(list(tset, Key = Key, inversion = inversion, step = FALSE), figurationArgs[names(figurationArgs) != 'step']))
    
    figuration <- do.call('reduceFigures', c(list(alterations, extensions, quality, root.case, if (inversion) getInversion(tset) else 0L), figurationArgs))
    quality[quality == '?'] <- ""
    figuration
    
  }
  
  
  
  inversion.label <- if (!is.null(inversion.labels)) getInversion(tset, inversion.labels = inversion.labels)
  
  tonalharmony <- pasteordered(parts, root = root, quality = quality, figuration = figuration, inversion = inversion.label, bass = bass, sep = sep)
  
  tonalharmony  
}



tset2figuredBass <- function(tset, figurationArgs = list(),  ...) {
  setoptions(figurationArgs) <- list(implicitSpecies = TRUE, flat = 'b', qualities = FALSE,
                                     absoluteSpecies = FALSE, extension.decreasing = TRUE,
                                     extension.simple = TRUE)
  
  overdot(tset2tonalHarmony(tset, parts = c('bass','figuration'), 
                            figurationArgs = figurationArgs,  root.case = FALSE,
                            root = FALSE, bass = TRUE, bass_func = tint2kern,
                            figuration = TRUE, quality = FALSE,
                            extension.shorthand = TRUE, extension.simple=TRUE,
                            extension.sus = FALSE, extension.add = FALSE,
                            inversion = TRUE,
                            sep = ' ', bass.sep = '', ...)) -> figures
  
  # if (extension.shorthand) {
  #   figures <- stringr::str_replace(figures,'([^913])753|^753', '\\17')
  #   figures <- stringr::str_replace(figures, '([^9713])63|^63', '\\16')
  #   figures <- stringr::str_replace(figures, '([^9713])653|^653', '\\165')
  #   figures <- stringr::str_replace(figures, '([^9713])643|^643', '\\143')
  #   figures <- stringr::str_replace(figures, '([^9713])642|^642', '\\142')
  # }
  # 
  figures
  
  
}


tset2romanNumeral <- function(tset,  Key = dset(0, 0), figurationArgs = c(), ...) {
  
  setoptions(figurationArgs) <- list(implicitSpecies = TRUE, flat = 'b', qualities = FALSE)
  
  overdot(tset2tonalHarmony(tset, parts = c('root', 'quality', 'figuration', 'inversion'), 
                            root_func = tint2romanRoot, Key = Key,
                            figurationArgs = figurationArgs, 
                            implicitSpecies = TRUE,
                            rootCase = TRUE,
                            inversion.labels = NULL,
                            extension.shorthand = TRUE, extension.simple=TRUE,
                            extension.sus = TRUE, extension.add = TRUE,
                            inversion = TRUE, ...))
  
}

tset2sciChord <- function(tset,  figurationArgs = c(), ...) {
  setoptions(figurationArgs) <- list(implicitSpecies = FALSE, explicitNaturals = TRUE, absoluteSpecies = TRUE, qualities = TRUE, step = FALSE)
  
  
  overdot(tset2tonalHarmony(tset, parts = c('root', 'quality', 'figuration', 'bass'), 
                            root_func = tint2simplepitch, figurationArgs = figurationArgs,
                            root.case = FALSE,
                            root = TRUE, quality = TRUE, figuration = TRUE, inversion = FALSE, bass = FALSE,
                            implicitSpecies = FALSE,
                            extension.shorthand = TRUE, extension.simple = FALSE,
                            extension.add = TRUE, extension.sus = TRUE,
                             ...)) 
}


tset2chordSymbol <- function(tset, figurationArgs = c(), major = NULL, ...) {
  setoptions(figurationArgs) <- list(absoluteSpecies = TRUE, implicitSpecies = TRUE, extension.decreasing = FALSE,
                                     flat = 'b', qualities = FALSE, natural = 'maj')
  
  
  chords <- overdot(tset2tonalHarmony(tset, parts = c('root', 'quality', 'figuration', 'bass'), 
                            root_func = tint2simplepitch, figurationArgs = figurationArgs,
                            major = major %maybe% "MAJOR", minor = 'min', diminish = 'dim',
                            root = TRUE, quality = TRUE, figuration = TRUE, inversion = FALSE, bass = TRUE,
                            implicitSpecies = FALSE, root.case=FALSE,
                            extension.shorthand = TRUE, extension.simple = FALSE,
                            extension.add = TRUE, extension.sus = TRUE,
                            ...)) 
  
  if (is.null(major)) chords <- stringr::str_replace(chords, "MAJOR", '')
  
  stringr::str_replace(chords, 'maj7([139]{1,2})', 'maj\\1')
  
}



  
###################################################################### ###
# Parsing chord information (x2tset) #####################################
###################################################################### ###

## Chord parsers ####


### Numeric

integer2tset <- function(int) tset(int, 0)

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
                             major = 'M', minor = 'm', perfect = 'P', diminish = 'o', augment = '+',
                             ...) {
  
  
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
  }, root, figurations$Degrees, figurations$Accidentals, rep(mode, length.out = length(root))) |> do.call(what = 'rbind')
  
  
  
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

romanNumeral2tset <- function(str, Key = dset(0,0), diminish = 'd', augment = 'A', ...) {

  
  Key <- CKey(Key)
  
  REparse(str,
          makeRE.romanChord(..., diminish = diminish, augment = augment, collapse = FALSE),
          parse.exhaust = FALSE, parse.strict = FALSE,
          toEnv = TRUE)  # adds accidental numeral triadalt figurations to the environment
  
  root <- tonalChroma2tint(paste0(accidental, toupper(numeral)), useKey = TRUE,
                           parts = c('species', 'step'), qualities = FALSE,
                           step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                           Key = Key, ...)@Fifth
  
  figurations <- parseFiguration(figurations)
  
  ### quality of degress
  # extension qualities
  qualities <- extensions2qualities(root, figurations, triadalt, Key = Key, flat = 'b', diminish = diminish, augment = augment, ...)
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

sciChord2tset <- function(str, Key = dset(0, 0), ...) {

   
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
    
    (sciQualities2tset(quality,  ...) + tset(root, root)) - getRoot(Key)
    
}

##... Numbers

integer2tset <- function(x) tset(x, x)

## Chord Parsing Dispatch ######################################


### Parse 2tset generic and methods ####


tertianSet_dispatch <- function(...) UseMethod('tertianSet_dispatch')

tertianSet_dispatch.tertianSet <- function(x, ...) x


#### Numbers ####

tertianSet_dispatch.integer <- integer2tset

#### Characters ####

char2tset <- humdrumDispatch(doExclusiveDispatch = FALSE,
                             'romanChord: makeRE.romanChord(...)' = romanNumeral2tset,
                             'sciChord: makeRE.sciChord(...)' = sciChord2tset)


mapoftset <- function(str, ..., split = '/') {
  
  parts <- strPartition(str, split = split)
  
  Keys <- parts[-1]
  Keys[] <- head(Reduce(function(x, y) {
    y[!is.na(x)] <- char2dset(x[!is.na(x)], y[!is.na(x)], ...)
    y
    }, right = TRUE, init = dset(integer(length(str)), 0), Keys, accumulate = TRUE), -1L) 
  
  Mode <- CKey(Keys[[1]])
  root <- Reduce('+', lapply(Keys, getRoot))
  Key <- Mode + dset(root, root)
  
  tset <- char2tset(parts$base, Key = Key)
  tset + dset(root, root, 0L)
}

tertianSet_dispatch.character <- humdrumDispatch(doExclusiveDispatch = FALSE,
                                                      'chordof: makeRE.tertianPartition(...)' = mapoftset,
                                                      'romanChord: makeRE.romanChord(...)' = romanNumeral2tset,
                                                      'sciChord: makeRE.sciChord(...)' = sciChord2tset)

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


makeChordTransformer <- function(deparser, callname, outputclass = 'character') {
  # this function will create various pitch transform functions
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  parse <- function(...) list(...) %class% 'parseArgs'
  transpose <- function(...) list(...) %class% 'transposeArgs'
  figuration <- function(...) list(...) %class% 'figurationArgs'
  
  args <- alist(x = , ... = , Key = NULL, Exclusive = NULL, 
                inPlace = FALSE, dropNA = FALSE, parseArgs = list(), transposeArgs = list(), figurationArgs = list(), memoize = TRUE)
  
  if (is.null(deparser)) deparse <- FALSE else args <- c(args, alist(deparse = TRUE))
  
  rlang::new_function(args,
                      rlang::expr( {
                        redim <- dimParse(x)
                        
                        
                        # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
                        args <- lapply(rlang::enexprs(...), eval, envir = environment()) # this evals in the makePitchTransformer closure!
                        do.call('checkTFs', c(args[names(args) %in% c('implicitSpecies', 'absoluteSpecies', 'explicitNaturals')],
                                              memoize = memoize, inPlace = inPlace, dropNA = dropNA,
                                              list(callname = callname)))
                        classes <- sapply(args, \(arg) class(arg)[1]) 
                        
                        transposeArgs <- c(transposeArgs, unlist(args[classes == 'transposeArgs'], recursive = FALSE))
                        figurationArgs <- c(figurationArgs, unlist(args[classes == 'figurationArgs'], recursive = FALSE))
                        
                        parseArgs   <- c(list(Exclusive = Exclusive), parseArgs, unlist(args[classes == 'parseArgs'], recursive = FALSE))
                        deparseArgs <- c(args[!grepl('Args$', classes)], list(figurationArgs = figurationArgs))
                        
                        # Keys
                        Key <- if (is.null(Key)) dset(0, 0) else diatonicSet(Key)
                        from <- if (is.null(transposeArgs$from)) Key else diatonicSet(transposeArgs$from)
                        to   <- if (is.null(transposeArgs$to)) Key else diatonicSet(transposeArgs$to)
                        
                        # automatically remove NA values
                        putNAback <- predicateParse(Negate(is.na), all = FALSE,
                                                    x = x, Key = Key, Exlusive = Exclusive,
                                                    from = from, to = to)
                        
                        if (length(x) == 0L) return(putNAback(vectorNA(outputclass, 0L)))
                        
                        rebuild <-  memoizeParse(x = x, Key = Key, Exclusive = Exclusive, from  = from, to = to, memoize = memoize)
                        parseArgs$Key <- from
                        deparseArgs$Key <- to 
                        
                        transposeArgs$from <- CKey(from)
                        transposeArgs$to <- CKey(to)
                        
                        result <- {
                          #
                          parsedTset <- do.call(tertianSet_dispatch, c(list(x, inPlace = inPlace, memoize = FALSE), parseArgs))
                          
                          # if (length(transposeArgs) > 0L) {
                            # parsedTint <- do.call('transpose.diatonicSet', c(list(parsedTint), transposeArgs))
                          # }
                          output <- if (deparse) do.call(!!deparser, c(list(parsedTset), deparseArgs)) else parsedTset
                          
                          if (inPlace) output <- re.place(output, parsedTset)
                          
                          
                          output
                        }
                        
                        redim(if (dropNA) result else putNAback(rebuild(result)))
                        
                      }))
  
  
}




### Chord transformers ####

##
#' @name chordTransformer
#' @export figuredBass romanNumeral 
#' @export sciChord chordSymbol
figuredBass <- makeChordTransformer(tset2figuredBass, 'figuredBass')
romanNumeral <- makeChordTransformer(tset2romanNumeral, 'romanNumeral')
sciChord <- makeChordTransformer(tset2sciChord, 'sciChord')
chordSymbol <- makeChordTransformer(tset2chordSymbol, 'chordSymbol')

###################################################################### ### 
# Manipulating tertian sets ##############################################
###################################################################### ### 




##### Tonal transform methods ####
