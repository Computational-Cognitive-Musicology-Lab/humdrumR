################################## ###
# tertianSet S4 class ################
################################## ###

## tertianSetS4 documentation ----

#' Tertian set
#' 
#' `tertianSet` is one of [humdrumR's][humdrumR] 
#' types of tonal data, representing Western tertian harmonies.
#' `tertianSet` is a subclass of `diatonicSet` (and thence, `struct`).
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
#' @seealso The main way to create `tertianSet` S4 objects is with the [tertianSet()] pitch parser.
#' @family {Tonal S4 classes}
#' @name tertianSetS4
NULL

## Definition, validity, initialization ####

#' @rdname tertianSetS4
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
                      object@Inversion < 7, na.rm = TRUE)
            })

## Constructors ####

#' @rdname tertianSetS4
#' @export
tset <- function(root = 0L, signature = 0L, alterations = 0L, cardinality = 3L, extension = NULL, inversion = 0L) {
    if (is.tonalInterval(root)) root <- root@Fifth
    
    if (length(root) == 0L && length(signature) == 0L) {
      return(new('tertianSet', 
                   Root = integer(), 
                   Signature = integer(), 
                   Alteration = integer(), 
                   Extensions = integer(),
                   Inversion = integer()))
    }
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


getBass <- function(tset){
    is.tertianSet(tset)
    LO5th(tset)[ , 1L]
}

getBassTint <- function(tset){
    is.tertianSet(tset)
    tint( , getBass(tset)) 
} 

getExtensions <- function(tset) {
    
   rootpos <- tset@Extensions
   inverted <- bitwRotateR(rootpos,  getInversion(tset), nbits = 7L)

   inverted <- inverted + ((inverted - 1) %% 2) # make sure root position is there always 

   bits <- ints2bits(inverted, nbits = 7L) == 1L
   colnames(bits) <- c('Root', nthfix(c(3, 5, 7, 9, 11, 13)))
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
  is.tertianSet(tset)
  tset@Inversion <- rep(0L, length(tset))
  tset
}




## Logic methods ####

### is.methods #####

#' @rdname tertianSetS4
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


#### Tonal is.methods ####

#' @rdname is.major
#' @export
is.major.default <- function(x, ...) {
   parsed <- tertianSet(x, ...)
   if (any(is.na(parsed))) {
     keys <- diatonicSet(x, ...)
     if (all(!is.na(keys)[!is.na(parsed)]) && any(!is.na(keys)[is.na(parsed)])) parsed <- keys
   }
  
   is.major.diatonicSet(parsed) 
   
}
#' @rdname is.major
#' @export
is.minor.default <- function(x, ...) {
  parsed <- tertianSet(x, ...)
  if (any(is.na(parsed))) {
    keys <- diatonicSet(x, ...)
    if (all(!is.na(keys)[!is.na(parsed)]) && any(!is.na(keys)[is.na(parsed)])) parsed <- keys
  }
  
  is.minor.diatonicSet(parsed) 
  
}

## Order/relations methods ####

#' @rdname diatonicSetS4
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




###################################################################### ###
# Deparsing Chord Representations (tset2x) ###############################
###################################################################### ###


## Deparsing (tertianSet) documentation ----

#' Generating ("deparsing") chord representations
#' 
#' [humdrumR] includes a easy-to-use system for 
#' generating a variety of tertian harmony (chord) representations,
#' which can be flexibly modified by users.
#' "Under the hood" `humdrumR` represents all tonal chord information using the [same underlying representation][tertianSetS4],
#' which is typically extracted from input data using the [chord parser][chordParsing].
#' This representation can then be "deparsed" into a variety of predefined output formats (like `**harm`), 
#' or into new formats that you create!
#' 
#' Deparsing is the second step in the [chord function][chordFunctions] processing pipeline:
#' 
#' + **Input** representation `|>` 
#'   + *Parsing* `|>`
#'     + **Intermediate** ([tertianSet][tertianSetS4]) representation `|>`
#'     + **Transformation**  `|>`
#'   + *Deparsing* (DEPARSING ARGS GO HERE) `|>`
#' +  **Output** representation 
#' 
#' Various pitch representations can be generated using predefined [chord functions][chordFunctions] like [chord()]
#' [tertian()], and [roman()].
#' All of these functions use a common deparsing framework, and are specified using different combinations of arguments
#' to the deparser.
#' By modifying these *"deparsing" arguments*, you can exercise 
#' fine control over how you want pitch information to be represented in your output.
#' 
#' @seealso All `humdrumR` [chord functions][chordFunctions] make use of the deparsing functionality.
#' @name chordDeparsing
NULL


## Chord deparsers ####




### Chord representations ####



tset2alterations <- function(x, Key = dset(0,0), 
                             qualities = FALSE, 
                             inversion = TRUE, 
                             absoluteSpecies = TRUE,  implicitSpecies = FALSE, 
                             dominantSpecies = FALSE,
                             explicitNaturals = FALSE, ...) {
  # this produces either accidentals or qualities, depending on the parts argument
  
  if (!inversion) x <- rootPosition(x)
  bass <- getBass(x)
  
  if (absoluteSpecies) {
    roots <- getRoot(x)
    x <- x - getRoot(x)
    Key <- if (dominantSpecies) dset(0, -1L) else dset(0L, getMode(Key) - roots)
  }
  if (explicitNaturals) {
    x <- x - getRoot(Key)
    Key <- Key - getRoot(Key)
  }
  
  LO5ths <- LO5th(x)
  tints <- tint( , c(LO5ths))
  figures <- tint2tonalChroma(tints,  Key = Key, qualities = qualities, complex = FALSE, 
                              parts = 'species',
                              implicitSpecies = implicitSpecies, explicitNaturals = explicitNaturals, ...)
  
  # colnames(figures) <- extensions
  # rownames(figures) <- tint2simplepitch(tint( , bass), Key = dset(0, 0), quality.cautionary = TRUE)
  figures %<-matchdim% LO5ths
  
}


tset2extensions <- function(x, extension.simple = FALSE, inversion = TRUE, inverted, ...) {
  extensions <- c(1L, 3L, 5L, 7L, 9L, 11L, 13L)
  
  if (!inversion) x <- rootPosition(x)
  
  extensions <- matrix(extensions, byrow = TRUE, ncol = 7L, nrow = length(x))
  colnames(extensions) <- c('Root', nthfix(c(3, 5, 7, 9, 11, 13)))
  
  if (extension.simple) extensions[inverted, ] <- genericStep(extensions[inverted, ])
  extensions[!getExtensions(x)] <- NA_integer_
  
  extensions
}




tset2triadLabel <- function(x, root, root.case = TRUE, 
                            major = 'M', minor = 'm', diminish = 'o', augment = '+', ...) {
  
  perfect <- 'P'
  
  qualities <- tset2alterations(x, qualities = TRUE, inversion = FALSE,  step = FALSE,
                                explicitNaturals = TRUE, implicitSpecies = FALSE,
                                major = major, minor = minor, diminish = diminish, augment = augment, perfect = perfect)
  
  qualities <- qualities[ , c('3rd', '5th'), drop = FALSE]
  thirds <- qualities[ , '3rd', drop = FALSE]
  fifths <- qualities[ , '5th', drop = FALSE]
  thirds[is.na(thirds)] <- '.'
  fifths[is.na(fifths)] <- '.'
  
  #
  triadQuality <- rep('?', nrow(qualities))
  

  
  ## prepare labels for known combinations of third and fifth qualities
  reductions <- matrix('?', ncol = 4, nrow = 5, dimnames = list(c(diminish, minor, major, augment, '.'), c(diminish, perfect, augment, '.')))
  # reductions[] <- paste0('(', outer(rownames(reductions), colnames(reductions), paste0), ')')
  
  reductions <- local({
                       reductions[minor, diminish] <- diminish
                       reductions[minor, perfect] <- minor
                       reductions[major, perfect] <- major
                       reductions[major, augment] <- augment
                       reductions[major, '.'] <- paste0('3', major)
                       reductions[minor, '.'] <- paste0('3', minor)
                       reductions['.', perfect] <- paste0('5', major)
                       reductions['.', augment] <- paste0('5', augment)
                       reductions['.', diminish] <- paste0('5',diminish) 
                       reductions['.', '.'] <- paste0('1', major) 
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
  tags <- array(NA_character_, dim = dim(alterations))
  

  roots  <- sweep(col(extensions), 1, ((1L - inversion - 1L) %% 7L) + 1L, '==')
  thirds <- sweep(col(extensions), 1, ((2L - inversion - 1L) %% 7L) + 1L, '==')
  fifths <- sweep(col(extensions), 1, ((3L - inversion - 1L) %% 7L) + 1L, '==')
  
  # get rid of alterations that are already taken care of by the quality!
  if (!is.null(triadQuality)) {
   
    
    if (root.case) {
      alterations[thirds & alterations %in% c(flat, minor)] <- '' 
      alterations[fifths & alterations  ==     diminish   ] <- ''
    }
    alterations[!is.na(alterations) & 
                  (roots | thirds | fifths) & 
                  ((row(alterations) %in% which(triadQuality != '?')) | (alterations == 'n'))] <- ""
    
  }

  
  
  if (any(!inverted) && extension.sus) {
    
    nines.elevens <- col(extensions) %in% 5:6 & present
    sus <- sweep(nines.elevens, 1, !present[ , '3rd'] & !inverted, `&`) & alterations == ''
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
  
  # missing triad tones
  tags[!row(extensions) %in% which(inverted) & is.na(extensions) & col(tags) %in% 2L:3L & is.na(tags)] <- 'no'
  extensions[which(tags == 'no')] <- ((col(extensions)[which(tags == 'no')] - 1L) * 2L) + 1L
  #
  if (extension.shorthand) {
    # if (extension.simple && any(inverted)) {
      # extensions[inverted, ] <- genericstep(extensions[inverted, ])
    # }
    chorddegree <- sweep(extensions, 1, 2L * inversion, '+')
    chorddegree[chorddegree %in% c(8L, 10L, 12L)] <- chorddegree[chorddegree %in% c(8L, 10L, 12L)] - 7L
    chorddegree[which(chorddegree > 13L, arr.ind = TRUE)] <- chorddegree[which(chorddegree > 13L, arr.ind = TRUE)] - 14L
    
    hide <- sweep(col(chorddegree), 1, apply(chorddegree, 1, \(row) max(4L, which.max(row))), '<') 
    if (any(inverted)) hide <- hide & !sweep(chorddegree > 5, 1, inverted, '&')
    
    
    extensions[(hide & alterations == "" & is.na(tags)) | extensions == 1L] <- NA_integer_
    
  }
  
 
  # order
  
  alterations[] <- .paste(tags, alterations, if (step) extensions, fill = ".", na.if = all)
  
  figures <- if (!is.null(extension.decreasing)) {
    Map(\(i,j) alterations[i,j], 1:nrow(alterations),
                   apply(extensions, 1, order, 
                         decreasing = extension.decreasing, 
                         na.last = NA, simplify = FALSE))
  } else {
    lapply(1L:nrow(alterations), \(i) alterations[i, ])
  }
  
  output <- sapply(figures, \(f) paste(.paste(extension.sep[1], f, extension.sep[2], na.if = all, sep = ''), collapse = ''))
  
  gsub('\\.+$', '', output)
  
  #
  
}


tset2tonalHarmony <- function(x,
                              parts = c('root', 'quality', 'figuration'), 
                              root = TRUE, quality = TRUE, figuration = TRUE, inversion = TRUE, bass = FALSE, 
                              figurationArgs = list(),
                              root_func = tint2romanRoot, bass_func = root_func, bass.sep = '/',
                              root.case = TRUE,
                              Key = NULL, keyed = FALSE,
                              inversion.labels = NULL,
                              sep = '', ...) {
  Key <- diatonicSet(Key)
  
  if (keyed && !is.null(Key)) {
    Key <- rep(Key, length.out = length(x))
    x[!is.na(Key)] <- x[!is.na(Key)] + getRoot(Key[!is.na(Key)])
  }
  Key <- CKey(Key)
  
  parts <- matched(parts, c('root', 'quality', 'figuration', 'inversion', 'bass'))
  
  
  bass      <- if (bass) ifelse(!root | (getInversion(x) > 0), 
                                paste0(bass.sep, bass_func(getBassTint(x) - tint(1, 0), Key = Key, ...)), 
                                "")
  root      <- if (root) root_func(getRootTint(x), Key = Key, ...) 
  
  quality   <- if (quality) {
    {quality; root} %<-% tset2triadLabel(x, root, root.case, ...)
    quality
  }
 
  
  figuration <- if (figuration) {
    extensions  <- do.call('tset2extensions', c(list(x, inversion = inversion, inverted = getInversion(x) > 0L), figurationArgs))
    alterations <- do.call('tset2alterations', c(list(x, Key = Key, inversion = inversion, step = FALSE), figurationArgs[names(figurationArgs) != 'step']))
    
    figuration <- do.call('reduceFigures', c(list(alterations, extensions, ...,
                                                  quality, root.case, if (inversion) getInversion(x) else 0L), figurationArgs))
    quality[quality == '?'] <- ""
    figuration
    
  }
  
  
  
  inversion.label <- if (!is.null(inversion.labels)) getInversion(x, inversion.labels = inversion.labels)
  
  tonalharmony <- pasteordered(parts, root = root, quality = quality, figuration = figuration, inversion = inversion.label, bass = bass, sep = sep)
  
  tonalharmony  
}



tset2figuredBass <- function(x, figurationArgs = list(),  ...) {
  figArgs <- list(implicitSpecies = TRUE, flat = 'b', qualities = FALSE,
                  absoluteSpecies = FALSE, extension.decreasing = TRUE,
                  extension.simple = TRUE)

  figArgs[names(figurationArgs)] <- figurationArgs
  
  t2tH <- partialApply(tset2tonalHarmony, keyed = TRUE,
                       parts = c('bass','figuration'),
                       root.case = FALSE,
                       root = FALSE, bass = TRUE, bass_func = tint2kern,
                       figuration = TRUE, quality = FALSE,
                       extension.shorthand = TRUE, #extension.simple = TRUE,
                       extension.sus = FALSE, extension.add = FALSE,
                       inversion = TRUE,
                       sep = ' ', bass.sep = '')
  figures <- t2tH(x, figurationArgs = figArgs, ...)
  
  
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


tset2roman <- function(x,  Key = dset(0, 0), figurationArgs = c(), ...) {
  
  figArgs <- list(implicitSpecies = TRUE, flat = 'b', qualities = FALSE, 
                  extension.shorthand = TRUE, extension.simple = TRUE, absoluteSpecies = FALSE,
                  extension.sus = TRUE, extension.add = TRUE)
  figArgs[names(figurationArgs)] <- figurationArgs
  
  t2tH <- partialApply(tset2tonalHarmony, 
                       parts = c('root', 'quality', 'figuration', 'inversion'), 
                       root_func = tint2romanRoot, 
                       implicitSpecies = TRUE,
                       rootCase = TRUE,
                       inversion.labels = NULL,
                       inversion = TRUE)
  
  t2tH(x, figurationArgs = figArgs, Key = Key, ...)
  
}


tset2harm <- function(x,  Key = dset(0, 0), figurationArgs = c(), ...) {
  figArgs <- list(implicitSpecies = TRUE, qualities = TRUE, absoluteSpecies = TRUE,
                  diminish = 'D', augment = 'A', 
                  extension.shorthand = TRUE, extension.simple = FALSE, extension.decreasing = FALSE,
                  extension.sus = TRUE, extension.add = TRUE)
  figArgs[names(figurationArgs)] <- figurationArgs
  
  t2tH <- partialApply(tset2tonalHarmony, 
                       parts = c('root', 'quality', 'figuration', 'inversion'), 
                       root_func = tint2romanRoot, 
                       implicitSpecies = TRUE,
                       rootCase = TRUE,
                       inversion.labels = c('', 'b', 'c', 'd', 'e', 'f', 'g'),
                       inversion = FALSE)
  
  t2tH(x, figurationArgs = figArgs, Key = Key, ...)
  
}

tset2tertian <- function(x,  figurationArgs = c(), ...) {
  figArgs <- list(implicitSpecies = FALSE, explicitNaturals = TRUE, diminish = 'o', augment = '+',
                  absoluteSpecies = TRUE, qualities = TRUE, step = FALSE)
  
  figArgs[names(figurationArgs)] <- figurationArgs
  
  
  t2tH <- partialApply(tset2tonalHarmony, keyed = TRUE,
                       parts = c('root', 'quality', 'figuration', 'inversion'), 
                       root_func = tint2simplepitch, 
                       root.case = FALSE,
                       root = TRUE, quality = TRUE, figuration = TRUE, 
                       inversion = FALSE, bass = TRUE,
                       implicitSpecies = FALSE, inversion.labels = c('', '/3', '/5', '/7', '/2', '/4', '/6'),
                       extension.shorthand = TRUE, extension.simple = FALSE,
                       extension.decreasing = NULL,
                       extension.add = FALSE, extension.sus = FALSE)
  t2tH(x, figurationArgs = figArgs, ...)
}


tset2chord <- function(x, figurationArgs = c(), major = NULL, ...) {
  figArgs <- list(absoluteSpecies = TRUE, implicitSpecies = TRUE, extension.decreasing = FALSE, dominantSpecies = TRUE,
                  flat = 'b', qualities = FALSE, natural = 'maj')
  figArgs[names(figurationArgs)] <- figurationArgs
  
  t2tH <- partialApply(tset2tonalHarmony, keyed = TRUE,
                       parts = c('root', 'quality', 'figuration', 'bass'), 
                       root_func = tint2simplepitch, 
                       minor = 'min', diminish = 'dim',
                       root = TRUE, quality = TRUE, figuration = TRUE, inversion = FALSE, bass = TRUE,
                       implicitSpecies = FALSE, root.case=FALSE,
                       extension.shorthand = TRUE, extension.simple = FALSE,
                       extension.add = TRUE, extension.sus = TRUE)
  
  chords <- t2tH(x, figurationArgs = figArgs, major = major %||% "MAJOR", ...)
  
  if (is.null(major)) chords <- stringr::str_replace(chords, "MAJOR", '')
  
  stringr::str_replace(chords, 'maj7([139]{1,2})', 'maj\\1')
  
}



  
###################################################################### ###
# Parsing Chord Representations (x2tset) #################################
###################################################################### ###


## Parsing (tertianSet) documentation ----

#' Parsing chord information
#' 
#' [humdrumR] includes a easy-to-use but powerful system for *parsing* tertian harmony information:
#' various basic chord representations (including `numeric` and `character`-string representations) can be "parsed"---read
#' and interpreted by `humdrumR`.
#' For the most part, parsing automatically happens "behind the scenes" whenever you use any humdrumR [chord function][chordFunctions], like [harm()]
#' [roman()], or [chord()].
#' 
#' @seealso All `humdrumR` [chord functions][chordFunctions] make use of the deparsing functionality.
#' @name chordParsing
NULL

## Chord parsers ####


### Numeric

integer2tset <- function(int) tset(int, 0)

### Extensions/Figuration ####

extension2bit <- function(str) {
  
  extensions <- stringr::str_extract_all(str, captureRE(c('7', '9', '11', '13', 'sus4', 'add6', 'add2')))
  
  bit <- 7L # triad
  
  sapply(extensions,
         \(exten) {
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




triad2sciQuality <- function(triad, extensionQualities, incomplete,
                             major = 'M', minor = 'm', perfect = 'P', diminish = 'o', augment = '+',
                             ...) {
  
  
  triadQualities <- local({
                           quals <- cbind(perfect, 
                                          c(major, minor, minor, major), 
                                          c(perfect, perfect, diminish, augment))
                           rownames(quals) <- c(major, minor, diminish, augment)
                           quals <- cbind(quals[triad, , drop = FALSE ], '.', '.', '.', '.')
                           
                           quals[incomplete == '1', 2L] <- '.'
                           quals[incomplete == '1', 3L] <- '.'
                           quals[incomplete == '3', 3L] <- '.'
                           quals[incomplete == '5', 2L] <- '.'
                           quals
                           
                         })
  
  extensionQualities[col(extensionQualities) <= 3L & extensionQualities == '.'] <- triadQualities[col(extensionQualities) <= 3L & extensionQualities == '.']
  extensionQualities[ , 2L:3L] <- triadQualities[ , 2L:3L]
  
  apply(extensionQualities, 1L, paste, collapse = '')
  
}



extensions2qualities <- function(root, figurations, triadalts, Key = NULL, qualities = FALSE, ...) {
  
  mode <- if(is.null(Key)) 0L else getMode(Key)
  
  dots <- rep('.', 7L)
  Map(function(r, deg, acc, m) {
    redundantroot <- deg == 1 & acc == ""
    deg <- deg[!redundantroot]
    acc <- acc[!redundantroot]
    if (length(deg) == 0L) return(dots)
    step <- step2tint(deg, step.labels = 1L:14L)
    
    alterations <- specifier2tint(acc, step, qualities = qualities,  
                                  Key = dset(0L, m - r), implicitSpecies = TRUE, ...)
    
    qualities <- tint2specifier(step + alterations, qualities = TRUE, ...)
    
    dots[1L + ((deg - 1L) %/% 2L)] <- qualities
    dots
  }, root, figurations$Degrees, figurations$Accidentals, rep(mode, length.out = length(root))) |> do.call(what = 'rbind')
  
  
  
}

parseFiguration <- function(str, figureFill = TRUE, flat = 'b', qualities = FALSE, ...) {
  
  # str[str == ''] <- '35'
  
  makeRE <- partialApply(makeRE.tonalChroma, step.labels = 13:1, 
                         parts = c('species', 'step'),
                         collapse = TRUE)
  
  figures <- stringr::str_extract_all(str, makeRE(..., collapse = TRUE, flat = flat, qualities = qualities)[[1]], simplify = FALSE)
 
  figures <- lapply(figures, REparse, res = makeRE(..., collapse = FALSE, flat = flat, qualities = qualities))
  
  lapply(figures, 
         \(parsedfig) {
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

roman2tset <- function(x, Key = dset(0,0), augment = '+', diminish = 'o', implicitSpecies = FALSE, ...) {
  Key <- CKey(Key)
  REparse(x,
          makeRE.roman(..., diminish = diminish, augment = augment, collapse = FALSE),
          parse.exhaust = FALSE, parse.strict = FALSE,
          toEnv = TRUE)  # adds accidental numeral triadalt figurations to the environment
  root <- tonalChroma2tint(paste0(accidental, toupper(numeral)), useKey = TRUE,
                           parts = c('species', 'step'), qualities = FALSE,
                           implicitSpecies = implicitSpecies,
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
    
    triad2sciQuality(triad, qualities, incomplete = '', diminish = diminish, augment = augment, ...)
  })
  
  qualitytset <-  sciQualities2tset(qualities, ..., diminish = diminish, augment = augment)
  
  # if 1 is altered!
  root <- root + setNames(c(-7L, 7L, 0L), c(diminish, augment, 'P'))[stringr::str_sub(qualities, 1L, 1L)]
    
  ###
 
  output <- tset(root, 
                 root + getMode(qualitytset),
                 alterations = qualitytset@Alteration,
                 extension = figurations$Extension,  
                 inversion = figurations$Inversion)

  # if (implicitSpecies) output <- output + Key
  output
  
}


harm2tset <- function(x, Key = dset(0,0), 
                      figurationArgs = list(),
                      augment = '+', diminish = 'o', implicitSpecies = TRUE, ...) {
  Key <- CKey(Key)
  REparse(x,
          makeRE.harm(..., collapse = FALSE),
          parse.exhaust = FALSE, parse.strict = FALSE,
          toEnv = TRUE)  # adds accidental numeral triadalt figurations and inversion to the environment
  
  Key <- romanNumeral2dset(stringr::str_sub(of, start = 2L), Key = Key, implicitSpecies = implicitSpecies, ...)
  Key[is.na(Key)] <- dset(0L, 0L)
  root <- tonalChroma2tint(paste0(accidental, toupper(numeral)), useKey = TRUE,
                           parts = c('species', 'step'), qualities = FALSE,
                           implicitSpecies = implicitSpecies,
                           step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                           Key = Key, ...)@Fifth
  
  # 
  figArgs <- list(diminish = 'D', augment = 'A', qualities = TRUE)
  figArgs[names(figurationArgs)] <- figurationArgs
  
  figurations <- do.call('parseFiguration', c(list(figurations), figArgs))
  ### quality of degress
  # extension qualities
  qualities <- do.call('extensions2qualities',
                       c(list(root, figurations, triadalt, Key = Key), figArgs)) 
  
  # incorporate quality of triad
  qualities <- local({
    triad <- rep('M', length(numeral))
    triad[numeral == tolower(numeral)] <- 'm'
    triad[triadalt == diminish] <- figArgs$diminish
    triad[triadalt == augment]  <- figArgs$augment
    
    triad2sciQuality(triad, qualities, incomplete = '', diminish = 'D', augment = 'A', ...)
  })
  
  qualitytset <-  sciQualities2tset(qualities, ..., diminish = 'D', augment = 'A')
  
  # if 1 is altered!
  root <- root + setNames(c(-7L, 7L, 0L), c(diminish, augment, 'P'))[stringr::str_sub(qualities, 1L, 1L)]
  root <- root + getRoot(Key)
  
  ###
  output <- tset(root, 
                 root + getMode(qualitytset),
                 alterations = qualitytset@Alteration,
                 extension = figurations$Extension, 
                 inversion = ifelse(inversion == '', 0L, match(inversion, letters) - 1L))
  
  # if (implicitSpecies) output <- output + Key
  output
  
}

sciQualities2tset <- function(str, inversion = 0L, ...) {
  
  chord <- stringr::str_pad(str, width = 7L, side = 'right', pad = '.')
  
  dset <- qualities2dset(chord, steporder = 4L, allow_partial = TRUE, ...)
  
  
  extension <- sapply(stringr::str_locate_all(str, '[^.]'), \(x) sum(as.integer(2L^(x[,  'start'] - 1L))))
  
  tset(dset@Root, dset@Signature, dset@Alteration, extension = extension, inversion = inversion)
  
}

tertian2tset <- function(x, Key = dset(0, 0), ...) {
  
    REparse(x,
            makeRE.tertian(..., collapse = FALSE), # makes root, quality, inversion, and incomplete
            toEnv = TRUE) -> parsed
  
    Key <- diatonicSet(Key)
    
    root <- tonalChroma2tint(tonalChroma, parts = c('step', 'species'), qualities = FALSE, ...)@Fifth
    
    # qualities
    quality <- local({
      quality <- stringr::str_pad(quality, width = 5L, side = 'right', pad = '.')
      quality <- do.call('rbind', strsplit(quality, split = ""))
      triad <- quality[ , 1]
      extensions <- cbind('.', '.', '.', quality[ , -1L, drop = FALSE])
      
      triad2sciQuality(triad, extensions, incomplete, ...)
    })
    inversion <- ifelse(inversion == '', 0L, match(gsub('^/', '', inversion), c(1, 3, 5, 7, 2, 4, 6)) - 1L)
    
    (sciQualities2tset(quality,  inversion = inversion, ...) + tset(root, root, inversion = inversion)) - getRoot(Key)
    
}


chord2tset <- function(x, ..., major = 'maj', minor = 'min', augment = 'aug', diminish = 'dim', flat = 'b') {
  # preprocessing
  x <- stringr::str_replace(x, '[Mm]aj7', 'majn7')
  x <- stringr::str_replace(x, 'maj([91])', 'majn7\\1')
  
  #
  REparse(x,
          makeRE.chord(..., major = major, minor = minor, augment = augment, diminish = diminish,
                       flat = flat, collapse = FALSE), # makes tonalChroma, bass, quality, figurations
          toEnv = TRUE) -> parsed
  
  quality[quality == ''] <- major
  quality <- setNames(c('M', 'm', 'A', 'd'), c(major, minor, augment, diminish))[quality]
  makeRE.figs <- partialApply(makeRE.tonalChroma, step.labels = 13:1, 
                         parts = c('species', 'step'), qualities = FALSE,
                         collapse = TRUE)
  
  figurations <- stringr::str_extract_all(figurations, makeRE.figs(..., collapse = TRUE, flat = flat)[[1]], simplify = FALSE)
  
  figurations <- lapply(figurations, REparse, res = makeRE.figs(..., collapse = FALSE, flat = flat))
  sciQualities <- do.call('rbind',
                       lapply(figurations,
                              \(fig) {
                                step <- fig[ , 'step']
                                
                                if (is.null(step)) return(c('.', '.', '.', '.'))
                                step <- ifelse(step %in% c('9', '11', '13'), c('2', '4', '6')[match(step, c('9', '11', '13'))], step)
                                fig <- paste0(fig[ , 'species'], step)
                                
                                quals <-  tint2specifier(deg2tint(fig, flat = flat, parts = c('species', 'step'),
                                                                  Key = dset(0L, -1L), implicitSpecies = TRUE), qualities = TRUE, explicitNaturals = TRUE)
                                
                                
                                extensions <- c('7' = '.', '2' = '.', '4' = '.', '6' = '.')
                                extensions[step] <- quals
                                if (extensions['7'] == '.' && any(step == c('2', '4', '6'))) extensions['7'] <- 'm'
                                extensions
                                
                              }))
  sciQualities <- do.call('paste0', as.data.frame(sciQualities))
  
  tset <- tertian2tset(paste0(tonalChroma, quality, sciQualities), flat = flat, ...)
  
  if (any(bass != '')) {
    hasbass <- bass != ''
    bassint <- integer(sum(hasbass))
    
    bassint <- getFifth(kern2tint(stringr::str_sub(bass[hasbass], start = 2L))) - getFifth(kern2tint(tonalChroma[hasbass]))
    tset@Inversion[hasbass] <- c(0L, 2L, 4L, 6L, 1L, 3L, 5L)[bassint %% 7L + 1L]
  }
  
  
  tset
  
  
  
}
##... Numbers

integer2tset <- function(x) tset(x, x)

## Chord Parsing Dispatch ######################################


### Parse 2tset generic and methods ####

#' @rdname chordParsing
#' @export
tertianSet <- function(...) UseMethod('tertianSet')

#' @rdname chordParsing
#' @export
tertianSet.tertianSet <- function(x, ...) x

#' @rdname chordParsing
#' @export
tertianSet.logical <- function(x, ...) vectorNA(length(x), 'tertianSet')

#' @rdname chordParsing
#' @export
tertianSet.NULL <- function(x, ...) tset(c(), c())

#### Numbers ####


#' @rdname chordParsing
#' @export
tertianSet.numeric <- \(x) integer2tset(as.integer(x))

#' @rdname chordParsing
#' @export
tertianSet.integer <- integer2tset


#### Characters ####


# 
# 
# 
# mapoftset <- function(str, Key = NULL, ..., split = '/') {
#   Key <- Key %||% dset(0L, 0L)
#   Key <- rep(Key, length.out = length(str))
#   
#   parts <- strPartition(str, split = split)
#   Keys <- parts[-1]
#   if (length(Keys) > 0L) {
#     Keys[] <- head(Reduce(\(x, y) {
#       y[!is.na(x)] <- char2dset(x[!is.na(x)], Key = y[!is.na(x)], ...)
#       y
#     }, right = TRUE, 
#     init = dset(integer(length(str)), 0L), 
#     Keys, 
#     accumulate = TRUE), -1L) 
#     
#   } else {
#     Keys <- list(dset(integer(length(str)), 0))
#   }
#   
#   ofMode <- CKey(Keys[[1]])
#   root <- Reduce('+', lapply(Keys, getRoot))
#   ofKey <- ofMode + dset(root, root)
#   
#   tset <- char2tset(parts$base, Key = Key + ofKey, ...)
#   tset + dset(root, root, 0L)
# }

#' @rdname chordParsing
#' @export
tertianSet.character <- makeHumdrumDispatcher(list('harm', makeRE.harm,     harm2tset),
                                              list('any',  makeRE.roman,    roman2tset),
                                              list('any',  makeRE.tertian,  tertian2tset),
                                              list('any',  makeRE.chord,    chord2tset),
                                              funcName = 'tertianSet.character',
                                              outputClass = 'tertianSet')
  

#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) tset2tertian(x))

#### setAs tertianSet ####


setAs('integer', 'tertianSet', function(from) integer2tset(from))
setAs('numeric', 'tertianSet', function(from) integer2tset(as.integer(from)))
setAs('character', 'tertianSet', function(from) {
  output <- tset(rep(NA, length(from)))
  if (any(!is.na(from))) output[!is.na(from)] <- tertianSet.character(from[!is.na(from)])
  output
  })
setAs('matrix', 'tertianSet', function(from) tertianSet(c(from)) %<-matchdim% from)
setAs('logical', 'tertianSet', function(from) tset(rep(NA, length(from))) %<-matchdim% from)

setAs('diatonicSet', 'tertianSet', function(from) dset(from@Root, from@Signature, from@Alterations) %<-matchdim% from)
setAs('tertianSet', 'diatonicSet', function(from) tset(from@Root, from@Signature, from@Alterations) %<-matchdim% from)




 
###################################################################### ### 
# Translating Chord Representations (x2y) ################################
###################################################################### ### 


## Chord function documentation ####



#' Parsing and deparsing chord information
#' 
#' These functions can be used to extract and "translate," or otherwise modify, data representing tertian harmony information.
#' The functions are:
#' 
#' + [chord()]
#' + [figuredBass()]
#' + [harm()]
#' + [roman()]
#' + [tertian()]
#' 
#' @seealso To better understand how these functions work, read about how tertian harmonies are 
#' [parsed][chordParsing] and [deparsed][chordDeparsing].
#' 
#' @name chordFunctions
NULL


## Chord transform maker ####


makeChordTransformer <- function(deparser, callname, outputClass = 'character', removeArgs = NULL, extraArgs = alist()) {
  # this function will create various pitch transform functions
  
  withinFields$Key <<- c(withinFields$Key, callname)
  
  deparser <- rlang::enexpr(deparser)
  callname <- rlang::enexpr(callname)
  
  
  args <- c(alist(x = , 
                  ... = , # don't move this! Needs to come before other arguments, otherwise unnamed parse() argument won't work!
                  Key = NULL,
                  transposeArgs = list(),
                  parseArgs = list(), 
                  inPlace = FALSE),
            extraArgs)
  if (!is.null(removeArgs)) args <- args[!names(args) %in% removeArgs]
  
  fargcall <- setNames(rlang::syms(names(args[-1:-2])), names(args[-1:-2]))
  
  rlang::new_function(args, rlang::expr( {
    # parse out args in ... and specified using the syntactic sugar parse() or tranpose()
    # parse out args in ... and specified using the syntactic sugar parse() or transpose()
    c('args...', 'parseArgs', 'transposeArgs') %<-% specialArgs(rlang::enquos(...), 
                                                                parse = parseArgs, transpose = transposeArgs)
    formalArgs <- list(!!!fargcall)
    namedArgs <- formalArgs[.names(formalArgs) %in% .names(as.list(match.call())[-1])]
    # There are four kinds of arguments: 
    # ... arguments (now in args...), 
    # FORMAL arguments, if specified (now in namedArgs)
    # parseArgs
    # transposeArgs
    
    # Exclusive
    parseArgs$Exclusive <- parseArgs$Exclusive %||% args...$Exclusive 
    
    parseArgs   <- pitchArgCheck(parseArgs, !!callname)
    deparseArgs <- pitchArgCheck(c(args..., namedArgs), !!callname)
    
    # Key
    Key     <- diatonicSet(Key %||% dset(0L, 0L))
    fromKey <- diatonicSet(transposeArgs$from %||% Key)
    toKey   <- diatonicSet(transposeArgs$to   %||% Key)
    
    parseArgs$Key   <- fromKey
    deparseArgs$Key <- toKey 
    
    if (!is.null(transposeArgs$from)) transposeArgs$from <- CKey(fromKey)
    if (!is.null(transposeArgs$to))   transposeArgs$to   <- CKey(toKey)
    
    # memoize % deparse
    memoize <- args...$memoize %||% TRUE
    deparse <- args...$deparse %||% TRUE
    
    # Parse
    parsedTset <- do(tertianSet, c(list(x, memoize = memoize), parseArgs), memoize = memoize, outputClass = 'tertianSet')
    if (length(transposeArgs) > 0L && is.tertianSet(parsedTset)) {
      parsedTset <- do(transpose.tertianSet, c(list(parsedTset), transposeArgs))
    }
    
    deparseArgs <- c(list(parsedTset), deparseArgs)
    output <- if (deparse && is.tertianSet(parsedTset))  do(!!deparser, deparseArgs, 
                                                            memoize = memoize, 
                                                            outputClass = !!outputClass) else parsedTset
    if (deparse && !is.null(output)) {
      dispatch <- attr(parsedTset, 'dispatch')
      if (inPlace) output <- rePlace(output, attr(parsedTset, 'dispatch'))
      
      if (!is.null(parseArgs$Exclusive)) humdrumRattr(output) <- list(Exclusive = makeExcluder(dispatch$Exclusives, !!callname))
    }
    
    output
    
   
  }))
  
  
}





### Chord functions ####

#' @param x ***An `atomic` vector.***
#' 
#' The `x` argument can be any ([atomic][base::vector]) vectors
#' 
#' @param Key ***The diatonic key used by the parser, deparser, and transposer.***
#' 
#' Defaults to `NULL`, which is interpreted as C major.
#' 
#' Must be a `diatonicSet` or something coercable to `diatonicSet`; must be either length `1` or `length(x)`.
#' 
#' @param parseArgs ***An optional list of arguments to the [chord parser][chordParsing].***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to the [chord parser][chordParsing].
#' 
#' @param transposeArgs ***An optional list of arguments passed to a [transpose()] call.***
#' 
#' Defaults to an empty `list()`.
#' 
#' Must be a `list` of named arguments to [transpose()].
#' 
#' @param inPlace ***Should non-chord information be retained in the output string.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @name chordFunctions
NULL

#' "Pop/Jazz" chord symbols
#' 
#' This function outputs a generic "jazz" chord symbol representation of a tonal harmony.
#' 
#' @examples
#' romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')
#' 
#' chord(romanNumerals)
#' chord(romanNumerals, Key = 'A:')
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' with(B075[[ , 2]], chord(Token))
#' } 
#' 
#' @inheritParams chordFunctions
#' @export 
chord <- makeChordTransformer(tset2chord, 'chord')

#' Figured bass representation of harmony
#' 
#' This function outputs a [figured bass](https://en.wikipedia.org/wiki/Figured_bass)
#' representation of a tertian harmony.
#' 
#' @examples
#' romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')
#' 
#' figuredBass(romanNumerals)
#' 
#' tertian <- c('CM', 'CMm/3', 'FM', 'Fm', 'D-MM', 'GMmm')
#' 
#' figuredBass(tertian)
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' with(B075[[ , 2]], figuredBass(Token))
#' }
#' 
#' @inheritParams chordFunctions
#' @export 
figuredBass <- makeChordTransformer(tset2figuredBass, 'figuredBass')

#' Roman numeral representations of harmony
#' 
#' These functions output [roman numeral](https://en.wikipedia.org/wiki/Roman_numeral_analysis)
#' representations of a tertian harmony.
#' The `**harm` representation is the most widely used standard for roman numeral notation in humdrum data.
#' Unlike traditional roman numerals, `**harm` does not indicate inversions with figuration, using lowercase letters
#' (`a`, `b`, `c`, etc.) instead.
#' The `roman` function however does output (relatively) traditional figures.
#' 
#' @examples
#' tertian <- c('AM', 'AMm/3', 'DM', 'Dm', 'B-MM', 'AM/5', 'EMmm')
#'
#' harm(tertian, Key = 'A:')
#' roman(tertian, Key = 'A:')
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#'
#' with(B075[[ , 2]], harm(Token))
#' with(B075[[ , 2]], roman(Token))
#' }
#' 
#' @inheritParams chordFunctions
#' @export 
harm <- makeChordTransformer(tset2harm, 'harm')



#' Roman numeral representation of harmony
#' 
#' The output format of `roman()` is very similar to `**harm`.
#' The main difference is that inversions are indicated using traditional figures
#' , like `653`, instead of `**harm`'s simpler system (using letters).
#' So, for example, if we take the input `E7/B` in the key of A major, we'll get:
#'
#' + `harm('E7/B', Key = 'A:')` => `"V7c"`
#' + `roman('E7/B', Key = 'A:')` => `"V643"`
#' 
#' @rdname harm
#' @export 
roman <- makeChordTransformer(tset2roman, 'roman')


#' Tertian quality chord representation
#' 
#' This functions a generic form of tertian harmony representation, commonly used in music theory.
#' In this representation, the root of a chord is indicated as **kern, followed by one or more 
#' quality indicators, like `"CMM"` (C major seventh).
#' 
#' @details 
#' 
#' The first quality after the root indicates the quality of the triad.
#' Subsequent qualities, if present, indicate the quality of the 7th, 9th, 11th, and 13th respectively.
#' Some examples:
#' 
#' + `M`: major triad
#' + `Mm`: dominant-seventh chord
#' + `MM`: major-seventh chord
#' + `Mmm`: dominant-seventh-with-flat-9 chord.
#' + `oo`: fully-diminished-seventh chord.
#' 
#' Missing extensions can be indicated in their position using `.`.
#' For example, `E-Mm.P` indicates a E-flat dominant-11th chord with no 9th.
#' Missing members of the triad can be indicated by specifying either `5` or `3` immediately after the root, but before any
#' quality indicators.
#' For example, `C5M` indicates a C major chord with no 3rd, while `G3mm` indicates a G-minor-seventh chord with missing 5th.
#' 
#' The default quality indicators are `P` (perfect), `M` (major), `m` (minor), `o` (diminished), or `+` (augmented), but these
#' can be overridden by calls to their respective arguments: for example, `tertian('Cdim', diminish = 'd')`.
#' 
#' @section Inversions:
#' 
#' Inversions are indicated with slash notation, with the scale degree to the right of the slash.
#' For example, a first-inversion A major chord would be `AM/3`.
#' 
#' @examples
#' romanNumerals <- c('2I', '2IV7', '1V', '2vi', '2-VI', '2iio7', '2Vb9')
#' 
#' tertian(romanNumerals)
#' tertian(romanNumerals, Key = 'A:')
#' 
#' \dontrun{
#' B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
#' with(B075[[,2]], tertian(Token))
#' results
#' }
#' 
#' @inheritParams chordFunctions
#' @export 
tertian <- makeChordTransformer(tset2tertian, 'tertian')



###################################################################### ### 
# Manipulating tertian sets ##############################################
###################################################################### ### 



## Extracting pitches #####



### Line of Fifths ####

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

### Tonal intervals ####   


## Tonal transform methods ####
