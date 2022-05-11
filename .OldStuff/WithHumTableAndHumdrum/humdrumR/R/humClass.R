###########
##Method for Humdrum class
########
#' @importFrom pander evals
NULL

# ##' @importFrom lazyeval expr_text f_text
#NULL
#'

#' HumdrumR clas
#' @export
setClass('humdrumR', 
         slots = c(Data = 'ANY',
                   Files = 'list',
                   Apply = 'formula',
                   LoadTime = 'POSIXct'
                   ))

#' @export
setMethod('show', signature = c(object = 'humdrumR'),
          function(object) print(object@Data))


####
###Indexing humdrumR objects
###


#########################################
## Applying functions to humdrumR objects
#########################################

#' This controls which humdrumR data are affected by piped.
#' @export
setMethod('$', signature = c(x = 'humdrumR'),
          function(x, name) {
            x@Apply[[2]] <- as.name(name)
            x
          })





#' @export
setMethod('|', signature = c(e1 = 'humdrumR', e2 = 'function'),
          function(e1, e2) {
            func <- match.fun(e2)
            
            data   <- e1@Data
            
            
            data <- if (inherits(data, 'humdrum.table')) humdrum.table_apply(data, func, active = e1@Active, partition = e1@Partition ) else func(data)
            
            e1@Data <- data
            
            e1
            
          })


#' @export
setMethod('|', signature = c(e1 = 'humdrumR', e2 = 'formula'),
          function(e1, e2) {
          
            e1@Data <- data 
            
            c('Parse', 'RestOfPipe') %<-% parseFunctionFormula(e2)
            # browser()
            # if (!is.null(RestOfPipe)) `|`(e1, as.formula(RestOfPipe)) else e1
            Parse
            
          })



#' @export
setMethod('[',  signature = c(x = 'humdrumR', i = 'missing', j = 'missing'), force)

#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'),
          function(x, i) {
            x@Data <- x@Data[i]
            x
          })

#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'),
          function(x, j) {
            data <- x@Data
            x@Data <- if (is.humdrum.table(data)) data[j] else data[ , j]
            x
          })

#' @export
setMethod('[',
          signature = c(x = 'humdrumR', i = 'numeric', j = 'numeric'),
          function(x, i, j) {
            data <- x@Data
            x@Data <- if (is.humdrum.table(data)) data[i] else data[i , j]
            x
          })



##################################################[[

#' @export
setMethod('[[',  signature = c(x = 'humdrumR', i = 'missing', j = 'missing'), force)

#' @export
setMethod('[[',  
          signature = c(x = 'humdrumR', i = 'logical'), 
          function(x, i) {
            x@Data <- x@Data[[i]]
            x
          })

#' @export
setMethod('[[',  
          signature = c(x = 'humdrumR', i = 'numeric', j = 'missing'), 
          function(x, i) {
            x@Data <- x@Data[[i]]
            x
          })

#' @export
setMethod('[[',  
          signature = c(x = 'humdrumR', i = 'missing', j = 'numeric'), 
          function(x, j) {
            x@Data <- x@Data[[ , j]]
            x
          })

#' @export
setMethod('[[',  
          signature = c(x = 'humdrumR', i = 'numeric', j = 'numeric'), 
          function(x, i, j) {
            x@Data <- x@Data[[i, j]]
            x
          })


#' @export:
setMethod('[[',  
          signature = c(x = 'humdrumR', i = 'formula'), 
          function(x, i , j, ...) {
            exprs <- list(...)
            if (!missing(j)) exprs <- c(j, exprs)
            if (!missing(i)) exprs <- c(i, exprs)
            
            
            evaled  <- lapply(exprs, lazyeval::f_eval_rhs, data = x@Data@D)
            
            logical <- Reduce(`&`, Filter(is.logical, evaled))
            numeric <- Reduce(union, Filter(is.numeric, evaled))
            
            if (!is.null(logical)) x <- x[[logical]]
            if (!is.null(numeric)) x <- x[[numeric]]
            
            x
          })


pre_eval <- function(exprs) {
  exprs
}

post_eval <- function(evaled) {
 c('logics', 'factors') %<-% partition(is.logical, evaled)
 
 logic <- Reduce(`&`, logics)
 
 list(Logical = logic, Factors = factors)
}

######humdrumR manipulation functions
### NEED TO: add methods for > < >= <= == != ^

getactive <- function(humdrumR) {
 if (is.humdrum.table(humdrumR@Data)) {
    form <- humdrumR@Apply[[2]]
    
    if (len1(form)) call('list', form) else form
    
   } else {
     NULL
   }
}


#' @export
setMethod('-', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            data <- e1@Data
            subfunc <- function(x) if (is.character(x)) x %str-% e2 else x - e2
            
            if (is.humdrum.table(data)) {
              
              unHum(data)
              D <- copy(D)
              
              active <- actchar <- getactive(e1)
              
              actassign <- sapply(as.list(actchar[-1]), deparse) 
              actassign <- c('c', actassign) %splat|% call
              
              
              D[ , eval(actassign) := lapply(eval(active), subfunc)]
          
              data@D <- D
              
            } else {
              data   <- subfunc(data)
            }
            
            e1@Data <- data
            e1
          })

#' @export
setMethod('-', signature = c(e1 = 'humdrumR', e2 = 'numeric'),
          function(e1, e2) {
            data <- e1@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e1@Active]]
              active <- if (is.character(active)) active %str>% -e2 else active - e2
              D[[e1@Active]] <- active
              data@D <- D
              
            } else {
              data   <- if (is.numeric(data)) data - e2 
            }
            
            e1@Data <- data
            e1
          })

#' @export
setMethod('%%', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            data <- e1@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e1@Active]]
              active <- if (is.character(active)) active %strkeep% e2 else active %% e2
              D[[e1@Active]] <- active
              
              data@D <- D
              
            } else {
              data   <- if (is.character(data)) data %strkeep% e2 else data %% e2
            }
            
            e1@Data <- data
            e1
          })

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'character'),
          function(e1, e2) {
            data <- e1@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e1@Active]]
              active <- active %str+% e2
              
              D[[e1@Active]] <- active
              
              data@D <- D
              
            } else {
              data   <- data %str+% e2 
            }
            
            e1@Data <- data
            e1
          })


#' @export
setMethod('+', signature = c(e1 = 'character', e2 = 'humdrumR'),
          function(e1, e2) {
            data <- e2@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e2@Active]]
              active <- e1 %str+% active
              
              D[[e2@Active]] <- active
              
              data@D <- D
              
            } else {
              data   <-  e1 %str+% data
            }
            
            e2@Data <- data
            e2
          })

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'numeric'),
          function(e1, e2) {
            data <- e1@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e1@Active]]
              if (is.numeric(active)) active <- active + e2
              D[[e1@Active]] <- active
              
              data@D <- D
              
            } else {
              if (is.numeric(data)) data <- data + e2
            }
            
            e1@Data <- data
            e1
          })

#' @export
setMethod('+', signature = c(e1 = 'humdrumR', e2 = 'function'),
          function(e1, e2) {
            data <- e1@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e1@Active]]
              
              add    <- e2(active)
              
              active <- if (is.character(active) || is.character(add)) active %str+% add else active + add
              D[[e1@Active]] <- active
              
              data@D <- D
              
            } else {
              add  <- e2(active)
              data <- if (is.character(data) || is.character(add)) data %str+% add else data + add
            }
            
            e1@Data <- data
            e1
          })

#' @export
setMethod('+', signature = c(e1 = 'function', e2 = 'humdrumR'),
          function(e1, e2) {
            data <- e2@Data
            
            if (is.humdrum.table(data)) {
              unHum(data)
              
              active <- D[[e2@Active]]
              
              add    <- e1(active)
              
              active <- if (is.character(active) || is.character(add)) add %str+% active else add + active
              D[[e2@Active]] <- active
              
              data@D <- D
              
            } else {
              add  <- e1(active)
              data <- if (is.character(data) || is.character(add)) add %str+% data else add + data
            }
            
            e2@Data <- data
            e2
          })

