#######################################-
##################diatonicKey S4 class ####
#######################################-

#' Tonal (diatonic) keys
#' 
#' 
#' \code{diatonicKey} is one of \code{\link[humdrumR:humdrumR]{humdrumR}}'s 
#' \code{\link[humdrumR:humTonality]{types of tonal data}}, representing Western diatonic keys.
#' A key is represented by two integers, \code{Tonic} and \code{Mode}.
#' Tonic is simply the tonic note of the key on the circle of fifths.
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
#' @name humKey
#' @seealso humTonality
#' @export 
setClass('diatonicKey', slots = c(Tonic = 'integer', Mode = 'integer'))

setValidity('diatonicKey', 
            function(object) {
                object@Mode %% 7L == object@Mode &&
                    length(object@Tonic) == length(object@Mode)
            })

setMethod('initialize', 
          'diatonicKey',
          function(.Object, Tonic = 0L, Mode = 0L) {
              match_size(Tonic = Tonic, Mode = Mode, toEnv = TRUE)
              
              .Object@Tonic <- Tonic
              .Object@Mode <- Mode %% 7L
              .Object
          })

######diatonicKey constructors and accessors ####

## Constructors

#' The basic constructor for \code{diatonicKey}s.
#' Accepts either a integer (fifth) or a \code{\link[humdrumR:tonalInterval]{tonalInterval}}.
#' @name humKey
#' @export
tkey <- function(tonic = 0L, mode = 1L) {
           if (is.tonalInterval(tonic)) tonic <- getFifth(tonic)

           new('diatonicKey', Tonic = as.integer(tonic), Mode = as.integer(mode))
}

## Accessors

#' @name humKey
#' @export
getTonic <- function(tkey) tkey@Tonic

#' @name humTonality
#' @export
getMode <- function(tkey) tkey@Mode


######diatonicKey vector (and other core) methods ####

#' Methods required to make diatonicKeys act like vectors
#'
#' These methods allow us to treat 
#' \code{\link[humdrumR:humKey]{diatonicKeys}}
#' a lot like base \code{R} 
#' \code{\link[base:vector]{atomic vectors}}.
#' @name diatonicKey-asvector
NULL

####Indexing ####

#' @name diatonicKey-asvector
#' @export
setMethod('[', signature = c('diatonicKey'),
          function(x, i) {
            if (missing(i)) return(x)
            tkey(getTonic(x)[i], getMode(x)[i])
          })

#' @name diatonicKey-asvector
#' @export
setMethod('[<-', signature = c(x = 'diatonicKey', i = 'ANY', j = 'missing', value = 'diatonicKey'),
          function(x, i, value) {
                    
                    if (missing(i)) return(x)
                    x@Tonic[i] <- getTonic(value)
                    x@Mode[i]  <- getMode(value)
                    
                    x
          })


####Shape  ####

#' @name diatonicKey-asvector
#' @export
setMethod('c', signature = c('diatonicKey'),
          function(x, ...) {
                    tkeys <- list(x, ...)
                    tonics <- unlist(sapply(tkeys, getTonic))
                    modes  <- unlist(sapply(tkeys, getMode))
                    
                    tkey(tonics, modes)
          })

#' @name diatonicKey-asvector
#' @export
setMethod('length', signature = c('diatonicKey'),
          function(x) {
                    length(getTonic(x))
          })

#' @name diatonicKey-asvector
#' @export
setMethod('dim', signature = 'diatonicKey',
          function(x) NULL)

####Is/As ####

#' @name diatonicKey
#' @export
is.diatonicKey <- function(x) inherits(x, 'diatonicKey')

#' @name diatonicKey-asvector
#' @export
as.data.frame.diatonicKey <- function(x, row.names = NULL, optional = FALSE, ...) {
          if (is.null(row.names)) row.names <- 1:length(x)
          
          value <- list(x)
          attr(value, 'row.names') <- row.names
          attr(value, 'names') <- 'diatonicKey'
          class(value) <- c('data.frame')
          value
}

#' @name diatonicKey-asvector
#' @export
setMethod('as.vector', signature = c('diatonicKey'),
          function(x) { x })

#' @name diatonicKey-asvector
#' @export
setMethod('as.list', signature = c('diatonicKey'),
          function(x, ...) {
            x <- list(x, ...)
            x <- do.call('c', x)
            
            lapply(seq_along(x), function(i) x[i])
          })

#' @name diatonicKey-asvector
#' @export
setMethod('is.vector', signature = c('diatonicKey'),
          function(x) { TRUE })

#' @name diatonicKey-asvector
#' @export
setMethod('is.numeric', signature = c('diatonicKey'),
          function(x) { FALSE })

#' @name diatonicKey-asvector
#' @export
is.atomic.diatonicKey <- function(x) TRUE



#' @name diatonicKey-asvector
#' @export
rep.diatonicKey <- function(x, ...) {
 tkey(rep(getTonic(x), ...), rep(getMode(x), ...))       
}


######diatonicKey order/relations methods ####


#' \code{diatonicKeys} methods for \code{\link[base]{order}} and 
#' \code{\link[base]{sort}} order/sort along the circle of fifths.
#' Modes are sorted secondarily from fewest flats to most sharps.
#' If \code{parallel = TRUE} all modes are grouped by shared tonics, so
#' C minor and C major will appear besides each other.
#' If \code{parallel = FALSE} modes\keys are sorted together by number of accidentals,
#' so C minor and Eb major will be sorted next to each other.
#' @name diatonicKey-asvector
#' @export
order.diatonicKey <- function(x, parallel = TRUE, na.last = TRUE, decreasing = FALSE,
                   method = c("auto", "shell", "radix"), ...) {
                    x <- do.call('c', list(x, ...))
                    if (parallel) {
                      order(getTonic(x), -getMode(x),
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)
                    } else {
                      order(getTonic(x) - getMode(x), -getMode(x),
                          na.last = na.last,
                          decreasing = decreasing,
                          method = method)

                    }
          }

#' @name diatonicKey-asvector
#' @export
setMethod('sort', signature = c(x = 'diatonicKey'),
          function(x, parallel = TRUE, decreasing = FALSE) {
                    x[order.diatonicKey(x, parallel = parallel, decreasing = decreasing)]
          })

#' @name diatonicKey
#' @export
setMethod('==', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    getTonic(e1) == getTonic(e2) &  
                    getMode(e1)  == getMode(e2)
          })

#' @name diatonicKey
#' @export
setMethod('!=', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    getTonic(e1) != getTonic(e2) |
                    getMode(e1)  != getMode(e2)
          })


#' Numeric comparisons (e.g., \code{>}) compare the number of accidentals between two keys.
#'
#' @name diatonicKey
#' @export
setMethod('>', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    accidentals(e1) > accidentals(e2)
          })

#' @name diatonicKey
#' @export
setMethod('>=', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    accidentals(e1) >= accidentals(e2)
          })

#' @name diatonicKey
#' @export
setMethod('<', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    accidentals(e1) < accidentals(e2)
          })

#' @name diatonicKey
#' @export
setMethod('<=', signature = c('diatonicKey', 'diatonicKey'),
          function(e1, e2) {
                    accidentals(e1) <= accidentals(e2)
          })


######diatonicKey formatting methods ####

#' @name diatonicKey
#' @export
setMethod('show', signature = c(object = 'diatonicKey'), function(object) { cat(as.keyI(object)) })


#' @name diatonicKey-asvector
#' @export
format.diatonicKey <- function(x, ...) {
          as.keyI(x)         
}


#' @name diatonicKey
#' @export
setMethod('as.character', signature = c('diatonicKey'), function(x) as.keyI(x))



######Special methods

# How many accidentals does key have?
accidentals <- function(tkey) getTonic(tkey) - getMode(tkey) - 1L

############################################-
####### Writing key representations ----
############################################-
#' Writing \code{\link[humdrumR:humKey]{diatonicKeys}} to various representations
#' 
#' These functions all translate \code{\link[humdrumR:humKey]{diatonicKeys}} to 
#' various pitch representations. 
#' 
#' @name diatonicKey-write
NULL

###Writing from fifths (integers) to X
##To start, we need to be able to translate the fifths
##part of every diaonicKey into various things.

fifth2mode <- function(fifth, short = FALSE) {
    fullname <- c('lydian', 'major', 'mixolydian', 'dorian', 'minor', 'phyriggian', 'locrian')[(fifth %% 7L ) + 1L]
    
    if (short) stringr::str_sub(fullname, 1L, 3L) else fullname
}

##### As key signature interpretation (i.e., *k[f#], *k[b-e-a-d-g-])

#' @name diatonicKey-write
#' @export
as.keysignatureI <- function(tkey) {
    fifth <- getTonic(tkey) - getMode(tkey) + 1L
    
    base  <- "*k["
    notes <- ""
    end   <- "]"
    if (fifth > 0L) { 
        notes <- paste(as.kernPitch.tonalInterval(simpletint(6 : (fifth + 5))), collapse = '')
    } 
    if (fifth < 0L) {
        notes <- paste(as.kernPitch.tonalInterval(simpletint(-2 : (fifth - 1))), collapse = '')
    }
        
    paste0(base, notes, end)
}


##### As kern key interpretation (i.e., *G:, *eb-:)

#' @name diatonicKey-write
#' @export
as.keyI <- function(tkey) {
    root <- fifth2tonalname(getTonic(tkey))
    
    mode <- getMode(tkey)
    
    root[mode > 2L] <- tolower(root[mode > 2L])
    
    modelab <- ifelse(mode ==1L | mode == 4L,
                      "",
                      fifth2mode(mode, short = TRUE))
    
    paste0("*", root, ":", modelab)
    
    
}

##### As tonal name (i.e., "Eb")

#' @name diatonicKey-write
#' @export
as.tonalname.diatonicKey <- function(x, kernFlats = FALSE) {
    fifth2tonalname(getTonic(x), kernFlats)
}
