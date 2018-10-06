#######################################Pitch class
## inherits most of it's methods from integer for free.
#' @export
setClass('tint', contains = 'integer') -> tint

#' @export
"tint"

tint2lettername <- function(int) c('F', 'C', 'G', 'D', 'A', 'E', 'B')[1 + (int %% 7)]


#' @export
setMethod('as.character', signature = c(x = 'tint'),
          function(x) {
                    tint <- x@.Data
                    
                    ln <- int2lettername(tint)
                    
                    acc <- character(length(tint))
                    acc <- ifelse(tint > 6,  strrep('#', abs(tint) %/% 7), acc)
                    acc <- ifelse(tint < 0,  strrep('-', (abs(tint) + 6) %/% 7), acc)
                    
                    glue::glue('{ln}{acc}')
          })


setMethod('show', signature = c(object = 'tint'), function(object) { print(as.character(object)) })


###############

#' @export
setClass('pint', slots = c(Tint = 'tint', Octave = 'integer'))

pint <- function(t, o) new('pint', Tint = tint(t), Octave = as.integer(o))

#' @export
setValidity('pint', 
            function(object) {
              t  <- object@Tint
              o  <- object@Octave
              
              length(t) == length(o) 
            }
)

#' @export
setMethod('+', signature = c('pint', 'pint'),
          function(e1, e2) {
            pint(e1@Tint + e2@Tint, e1@Octave + e2@Octave)
                            
          })

#' @export
setMethod('-', signature = c('pint', 'missing'),
          function(e1) {
                    pint(-e1@Tint, -e1@Octave)
                    
          })

#' @export
setMethod('-', signature = c('pint', 'pint'),
          function(e1, e2) {
                    pint(e1@Tint - e2@Tint, e1@Octave - e2@Octave)
                    
          })

#' @export
setMethod('*', signature = c('pint', 'integer'),
          function(e1, e2) {
                    pint(e1@Tint * e2, e1@Octave * e2)
          })


as.ratio <- function(pint, tbase = 2^(19/12)) {
          tint <- pint@Tint@.Data
          oct  <- pint@Octave
          
          tbase ^ tint * 2 ^ oct
          
}

setMethod('show', signature = c(object = 'pint'), function(object) { print(as.pitch(object)) })

as.pitch <- function(x) {
                    tint <- as.character(x@Tint)
                    o    <- x@Octave
                    
                    paste0(tint, o)
                    
          }



setGeneric('as.semitone', function(x) standardGeneric('as.semitone'))



###
#' @export
setMethod('as.semitone', signature = c(x = 'pint'),
          function(x) {
            base <- (x@tint * 7) %% 12
            octshift <- (x@Octave - 4) * 12
            return(base + octshift)
            
            })
