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

##...accessors ####

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

# setAs('diatonicSet', 'tertianSet', function(from) tset(getRoot(from), getSignature(from), alterations = from@Alteration, cardinality = 3, inversion = 0L))

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

####. vector/core methods ####
    

#' @name tertianSet
#' @export
is.tertianSet <- function(x) inherits(x, 'tertianSet')


###.. formatting methods ####


#' @name diatonicSet
#' @export
setMethod('as.character', signature = c('tertianSet'), function(x) .ifelse(is.na(x), NA, tset2chordSymbol(x)))

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

# #' @export
# setMethod('Arith', c('diatonicSet', 'tertianSet'),
#           function(e1, e2) {
#             callGeneric(as(e1, 'tertianSet'), e2)
#           })

# #' @export
# setMethod('Arith', c('tertianSet', 'diatonicSet'),
#           function(e1, e2) {
#             callGeneric(e1, as(e2, 'tertianSet'))
#           })

##... addition ####



##### To/From line-of-fifths ####
    
###. line-of-fifths to x ####


###. x to line-of-fifths ####


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
    
    rownames(LO5ths) <- tint2tonalChroma(tint( , getRoot(x)),
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


# romanNumeral2mode <- function(root, Key, TriadQuality) {
  # mode <- getMode(Key)
  # mode + root - mode + TriadQuality
# }


triad2sciQuality <- function(triad, extensionQualities, triad.labels) {
  

  triadQualities <- with(triad.labels, 
                     {
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
    LO5th <- step2LO5th(deg, step.labels = 1L:14L) #%% dset(0, m)
    alterations <- accidental2LO5th(acc, ...)
    
    qualities <- LO5th2quality(as.integer((LO5th %% dset(-r, m - r)) + alterations), ..., quality.cautionary = TRUE)
    
    dots[1L + ((deg - 1L) %/% 2L)] <- qualities
    dots
  }, root, figurations$Degrees, figurations$Accidentals, rep(mode, length(root))) %>% do.call('rbind', .)
  
  
  
}

parseFiguration <- function(str, figureFill = TRUE, ...) {
  
  # str[str == ''] <- '35'
  
  figures <- stringr::str_extract_all(str, 
                                      overdot(makeRE.tonalChroma(step.labels = 13:1, 
                                                                 parts = c('accidentals', 'steps'), 
                                                                 collapse = TRUE,
                                                                 ...)),
                                      simplify = FALSE)
  
  figures <- lapply(figures, REparse, res =  overdot(makeRE.tonalChroma(step.labels = 13:1, 
                                                                        parts = c('accidentals', 'steps'), 
                                                                        collapse = FALSE,
                                                                        ...)))
  lapply(figures, 
         function(parsedfig) {
           parsedfig <- if(!is.null(parsedfig)) as.data.table(parsedfig) else data.table(accidentals = character(0L), steps = character(0L))
           parsedfig[ , Explicit := TRUE]
           
           if (!any(parsedfig$steps == '1')) parsedfig <- rbind(parsedfig, data.table(accidentals = '', steps = '1', Explicit = FALSE)) 
            
           
           ## 
           parsedfig[ , steps := as.integer(steps)]
           parsedfig[ , thirds := ifelse(steps %% 2L == 0L, steps - 7L, steps)]
           parsedfig[ , thirds := as.integer((thirds - min(thirds)) %/% 2L)] # translates steps -> thirds
           #
           parsedfig <- parsedfig[!duplicated(thirds)]
           inversion <- parsedfig[steps %in% c(1L, 8L, 15L), thirds[1]]
           
           # extensions
           setorder(parsedfig, steps)
 
           parsedfig <- parsedfig[  , {
             newthirds <- if (length(thirds) == 0L) {
               0L:2L 
             } else {
               newthirds <- if (all(steps <= 3L)) 0:max(thirds) else 0L:max(2L, max(thirds))
               
               if (all(steps == 5L) && accidentals[1] == "") newthirds <- setdiff(newthirds, 1L) 
               gaps <- diff(steps[Explicit])
               skips <- gaps > 2L & head(accidentals[Explicit] == '', -1L)
               if (any(skips)) for (g in gaps[gaps > 2L]) newthirds <- setdiff(newthirds, thirds[Explicit][which(skips & gaps == g)] + (1L:((g - 2L) / 2L)))
               newthirds
               
             }
             
             newaccidentals <- accidentals[match(newthirds, thirds)]
             newaccidentals[is.na(newaccidentals)] <- ""
             
             data.table(accidentals = newaccidentals, steps = newthirds * 2L + 1L, thirds = newthirds, Explicit = FALSE)
           }]
           
             
           extensionInt <- parsedfig[ , as.integer(sum(2^thirds))]
           #
           data.table(Inversion   = inversion, 
                      Extension   = extensionInt, 
                      Degrees     = list(parsedfig$steps),
                      Accidentals = list(parsedfig$accidentals))
           
           
         }) %>% do.call('rbind', .)
}

##### To/From tertianSets ####    

###.. tset to pitches ####


####. tset to x ####

tset2alterations <- function(tset, parts = 'qualities', inversion = TRUE, Key = dset(0,0), accidental.naturals = TRUE, ...) {
  # this produces either accidentals or qualities, depending on the parts argument
  
  if (!inversion) tset <- rootposition(tset)
  bass <- getBass(tset)
  
  if (is.null(Key)) {
    # Key <- dset(0, -1)
    tset <- tset - getRoot(tset)
  }
  if (!accidental.naturals) {
    tset <- tset - getRoot(Key)
    Key <- Key - getRoot(Key)
  }
  
  LO5ths <- LO5th(tset)
  tints <- tint( , LO5ths)
  figures <- tint2tonalChroma(tints,  Key = Key, parts = parts, ...)
  
  # colnames(figures) <- extensions
  rownames(figures) <- tint2simplepitch(tint( , bass), Key = dset(0, 0), quality.cautionary = TRUE)
  figures
  
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
# As "scientific chord label" (i.e., "Cmm" or "EbMm")



## figurations/extensions

  
  

# (53) 6(3) 64
# 7(53) 65(3) (6)43 (64)2
# 9(753) 7653 6543 6432 7642
# 11(9753) 76532 76543 65432 76432

#t  ..5.3.  ......
#7  7.5.3.  7.....
#9  7.5.32  ....32
#11 7.5432  7.54..
#13 765432

#t  .6..3.  .6....
#7  .65.3.  .65...
#9  765.3.  76....
#11 765.32
#13 765432

#t  .6.4..  .6.4..
#7  .6.43.  ...43.
#9  .6543.  .654..
#11 76543.
#13 765432

#7  .6.4.2  ...4.2
#9  .6.432  ...432
#11 .65432
#13 765432

#9  76.4.2  76.4..
#11 76.432
#13 765432

#11 7654.2
#13 765432

#13 765432

triadQualify.Roman <- function(root, triad, triad.labels = c(), triad.lowercase = c('diminish', 'minor'), triad.show = c('diminish', 'augment')) {
  setoptions(triad.labels) <- c(major = 'M', minor = 'm', diminish = 'o', augment = '+')
  triad.labels <- unlist(triad.labels)
  
  root[triad %in% triad.labels[triad.lowercase]] <- tolower(root[triad %in% triad.labels[triad.lowercase]])
  triad[!triad %in% triad.labels[triad.show]] <- ""
  
  paste0(root, triad)
  
}

tset2triadLabel <- function(tset, quality.labels = c(), triad.labels = c(), ...) {
  setoptions(quality.labels) <- c(major = 'M', minor = 'm', 
                                  diminish = 'o', augment = '+', 
                                  perfect = 'P')
  
  setoptions(triad.labels) <- c(major = 'M', minor = 'm', diminish = 'o', augment = '+')
  
  qualities <- tset2alterations(tset, parts = 'qualities', inversion = FALSE, Key = NULL, quality.labels = quality.labels, quality.cautionary = TRUE, quality.memory = FALSE)
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
  reductions <- with(quality.labels, matrix(ncol = 3, nrow = 4, dimnames = list(c(diminish, minor, major, augment), c(diminish, perfect, augment))))
  reductions[] <- paste0('(', outer(rownames(reductions), colnames(reductions), paste0), ')')
  
  reductions <- with(quality.labels,
                     {
                       reductions[minor, diminish] <- triad.labels$diminish
                       reductions[minor, perfect] <- triad.labels$minor
                       reductions[major, perfect] <- triad.labels$major
                       reductions[major, augment] <- triad.labels$augment
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


tset2tonalHarmony <- function(x, parts = c('root', 'accidentals', 'extensions'), steps = tint2romanRoot, Key = NULL, 
                              inversion = TRUE, inversion.labels = letters,
                              qualifyTriad = triadQualify.Roman, figure.Key = TRUE, accidental.naturals = FALSE, sep = '', ...) {
  parts <- matched(parts, c('root', 'qualities', 'accidentals', 'extensions', 'inversion'))
  
  qualoracc <- parts[parts %in% c('qualities', 'accidentals')]
  
  
  root        <- if ('root' %in% parts)      steps(getRootTint(x), Key = if (figure.Key) Key, ...) 
  alterations <- if (length(qualoracc) > 0L) tset2alterations(x, parts = qualoracc[1], Key = Key, inversion = inversion,
                                                              accidental.naturals = accidental.naturals, ...) 
  extensions  <- if ('extensions' %in% parts) tset2extensions(x, inversion = inversion, ...)  %dots% (has.prefix('extension.') %.% names)
  
  inversion   <- if ('inversion' %in% parts) {
    if (is.function(inversion.labels)) {
      ifelse(inversion | getInversion(x) > 0, inversion.labels(tint( , getBass(x)), Key = NULL), "")
    } else {
      getInversion(x, inversion.labels = inversion.labels)
    }
  }
  
  triad.quality <- tset2triadLabel(x, Key = NULL, ...) %dots% (has.prefix('^qualities.|^triad.') %.% names)
  if (!is.null(qualifyTriad)) root <- qualifyTriad(root, triad.quality) 
  
  figures <- if (any(c('extensions', 'qualities') %in% parts)) {
    parts[parts == qualoracc[1]] <- 'figures'
    reduceFigures(alterations, extensions, getInversion(x) > 0L, ...) %dots% (has.prefix('extension.') %.% names)
  }
  
  tonalharmony <- pasteordered(parts, root = root, figures = figures, inversion = inversion, sep = sep)
  
  tonalharmony  %dim% x
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
  overdot(tset2tonalHarmony(tset, parts = c('root', 'accidentals', 'extensions', 'inversion'), qualifyTriad = triadQualify.Roman, 
                            inversion.labels = letters,
                            extension.shorthand = TRUE, extension.which = c(7,2,4,6), extension.simple=TRUE,
                            extension.sus = TRUE, extension.add = TRUE,
                            inversion = FALSE, figure.Key = TRUE, Key = dset(0,0), ...))
  
}

tset2sciChord <- function(tset,  ...) {
  overdot(tset2tonalHarmony(tset, parts = c('root', 'qualities'), 
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


####. x to text ####



romanNumeral2tset <- function(str, Key = NULL, triad.labels = c(), of = dset(0,0), ...) {
  setoptions(triad.labels) <- c(diminish = 'o', augment = '+')
  
  
  of <- dset(0, getMode(of), of@Alteration)
  
  REparse(str,
          makeRE.romanChord(triad.labels = triad.labels, ..., collapse = FALSE),
          parse.exhaust = FALSE, parse.strict = FALSE,
          toEnv = TRUE)  # adds accidentals numerals triadalts figurations to the enviroment
  
  root <- tonalChroma2tint(paste0(accidentals, toupper(numerals)), 
                           parts = c('accidentals', 'steps'), 
                           step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                           Key = of, ...)@Fifth
  
  figurations <- parseFiguration(figurations)
  
  ### quality of degress
  # extension qualities
  quality.labels <- c(major = 'M', minor = 'm', perfect = 'P', triad.labels)
  qualities <- extensions2qualities(root, figurations, triadalts,
                                    Key = of, quality.labels = quality.labels)
  # incorporate quality of
  qualities <- local({
    triad <- rep(quality.labels$major, length(numerals))
    triad[numerals == tolower(numerals)] <- quality.labels$minor
    triad[triadalts == triad.labels$diminish] <- triad.labels$diminish
    triad[triadalts == triad.labels$augment]  <- triad.labels$augment
    
    triad2sciQuality(triad, qualities, triad.labels = quality.labels)
  })

  qualitytset <-  sciQualities2tset(qualities, quality.labels = quality.labels)
  
  # if 1 is altered!
  root <- root + with(quality.labels, 
                      setNames(c(-7L, 7L, 0L), c(diminish, augment, perfect))[stringr::str_sub(qualities, 1L, 1L)])
    
  ###
 
  return(tset(root, root + getMode(qualitytset),
              alterations = qualitytset@Alteration,
              extension = figurations$Extension,  
              inversion = figurations$Inversion))
  
}



sciQualities2tset <- function(str, quality.labels = c(), ...) {
  setoptions(quality.labels) <- c(major = 'M', minor = 'm', augment = 'A', diminish = 'd', perfect = 'P')
  
  
  chord <- stringr::str_pad(str, width = 7L, side = 'right', pad = '.')
  
  dset <- qualities2dset(chord, steporder = 4L, allow_partial = TRUE, quality.labels = quality.labels)
  
  
  cardinalities <- sapply(stringr::str_locate_all(str, '[^.]'), function(x) x[nrow(x), 1L])
  
  tset(dset@Root, dset@Signature, dset@Alteration, cardinalities)
  
}

sciChord2tset <- function(str, quality.labels = c(),  ...) {
   setoptions(quality.labels) <- c(major = 'M', minor = 'm', augment = 'A', diminish = 'd', perfect = 'P')

   
    REparse(str,
            makeRE.sciChord(..., quality.labels = quality.labels, collapse = FALSE),
            toEnv = TRUE) -> parsed
  
    root <- tonalChroma2tint(paste0(steps, accidentals), ...)@Fifth
    
    # qualities
    qualities <- local({
      qualities <- stringr::str_pad(qualities, width = 5L, side = 'right', pad = '.')
      qualities <- do.call('rbind', strsplit(qualities, split = ""))
      triad <- qualities[ , 1]
      extensions <- cbind('.', '.', '.', qualities[ , -1L, drop = FALSE])
      
      triad2sciQuality(triad, extensions, triad.labels = quality.labels)
    })
    
    sciQualities2tset(qualities, quality.labels = quality.labels, ...) + tset(root, root)
    
}

##... Numbers

integer2tset <- function(x) tset(x, x)

##### Tertian transforms ####

##### As x ####

#' Tertian set representations
#' 
#' Tertian sets can be read/wrote in various ways.
#' 
#' @name tertianRepresentations
NULL

####. generics ####

#' @name tertianSet
#' @export tertianSet figuredBass sciChord chordSymbol romanChord
tertianSet   <- function(x, ...) UseMethod('tertianSet')
figuredBass  <- function(x, ...) UseMethod('figuredBass')
sciChord     <- function(x, ...) UseMethod('sciChord')
chordSymbol  <- function(x, ...) UseMethod('chordSymbol')
romanChord   <- function(x, ...) UseMethod('romanChord')


####. methods ####  
 
###.. x as tset ####

#' @export
tertianSet.tertianSet <- force

#' @export
tertianSet.numeric <- integer2tset %.% as.integer


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

#' @export
setAs('integer', 'tertianSet', function(from) integer2tset(from))
#' @export
setAs('numeric', 'tertianSet', function(from) integer2tset(as.integer(from)))
#' @export
setAs('character', 'tertianSet', function(from) char2tset(from))
#' @export
setAs('matrix', 'tertianSet', function(from) tertianSet(c(from)) %dim% from)



###.. tset as x ####

#' @export
romanChord.tertianSet <- force %.% tset2romanNumeral
#' @export
sciChord.teritianSet <- force %.% tset2sciChord


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
