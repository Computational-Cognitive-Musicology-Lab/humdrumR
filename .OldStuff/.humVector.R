####

#' @export
setClass('humVector', slots = c(Data = 'vector', Null = 'logical', Type = 'character'))



newhumVector <- function(x) {
  
            if (is.character(x)) {
                x[x == ''] <- '.'
                null <- x %in% c('*', '!', '.') | is.na(x)
                type <- parseTokenType(x)
            } else {
                null <- is.na(x) 
                type <- rep('Data', length(x))
            }
            new('humVector', Data = x, Null = null, Type = type)
}

#' @export
is.humVector <- function(x) inherits(x, 'humVector')

#' @export
setMethod('[', 'humVector',
          function(x, i) {
            initialize(x, Data = x[i], x@Null[i], x@Type[i])
          })




#' @export
setMethod('length', signature =c(x='humVector'), function(x) length(x@Null))

#' @export
setMethod('as.character', 'humVector',
          function(x) {
            toks <- as.character(x@Data)
            null <- getNull(x)
            if (any(null)) { toks[null] <- na2null(x@Type[null]) }
            toks
            
          })

#' @export
setMethod('is.vector', 'humVector',
          function(x) { TRUE })

#' @export
setMethod('c', 'humVector',
          function(x, ...) {
            vecs <- list(x, ...)
            data <- unlist(lapply, vecs, function(x) x@Data)
            null <- unlist(lapply, vecs, function(x) x@Null)
            type <- unlist(lapply, vecs, function(x) x@Type)
            
            new('humVector', data, Null = null, Type = type)
            
          })


#' @export
setMethod('show', 'humVector',
          function(object) { show(as.character(object))})


na2null <- Vectorize(function(type) switch(type, 'Interpretation' = '*', 'Comment' = '!', 'Barline' = '=', '.'), USE.NAMES = FALSE)




#' @export
setMethod('[<-', signature = c(x = 'humVector', i = 'logical', value = 'humVector'),
          function(x, i, value) {
            x@Data[i] = value@.Data
            x@Type[i] = value@Type
            x@Null[i] = value@Null
            x
          })


#' @export
setMethod('[<-', signature = c(x = 'humVector', value = 'vector'),
          function(x, i, value) {
            x@Data[i] <- value
            x
          })


getNull <- function(x) { x@Null }


applyhumVec <- function(.func, ..., d = FALSE) {
  # vecs must all be same length
  vecs <- list(...)
  
  nnotnull <- sapply(vecs, function(vec) sum(!vec@Null))
  
  if (!d) vecs <- lapply(vecs, removeNull)
  
  output <- do.call('.func', vecs)
  
  if (is.object(output)) return(output)
  
  if (!d) {
    if (is.list(output)) {
      if (length(output) == length(vecs) && all(lengths(output) == nnotnull)) { return (Map(function(vec, out) { vec[!vec@Null] <- out ; vec } , vecs, output)) } 
      return(output)
    }
    
    if (length(output) == nnotnull[1]) {
      vec <- vecs[[1]]
      vec[!vec@Null] <- newhumVector(output)
      return(vec)
    }
  } else {
    if (is.list(output)) {
      if (length(output) == length(vecs) && all(lengths(output) == lengths(vec))) { return (lapply(newhumVector, output)) } 
      return(output)
    }
    
    if (length(output) == length(vecs[[1]])) { return(newhumVector(output)) }
  }
  
  
}

removeNull <- function(humvec) { humvec[!humvec@Null] }
