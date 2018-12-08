
#### Piping humdrumR ----

#' Piping humdrumR data
#' 
#' These infix operators make it possible to use
#' the \code{\link[humdrumR:with-in-Humdrum]{with(in)Humdrum}} 
#' functions in a piping style, similar to the \code{|} (pipe)
#' in unix-style terminals, or the \code{\link[magrittr:\%>\%]{pipe operator}}
#' from the R package \href{https://cran.r-project.org/web/packages/magrittr/index.html}{magrittr}.
#' 
#' The key is that the function \code{\link{withinHumdrum}} always returns a new
#' \code{\linkS4class{humdrumR}} data object. Thus, you can always send the output
#' of \code{\link{withinHumdrum}} back in to a new call of \code{\link{withinHumdrum}}.
#' This is exactly what the \code{\%hum>\%} is for: on the left-hand side,
#' input a \code{\linkS4class{humdrumR}} object, on the right-hand side suitable
#' arguments to \code{\link{withinHumdrum}} (i.e., a formula, a function, or list of formulae,
#' functions, and named arguments). You can then chain this call with another call to
#' \code{\%hum>\%} and more \code{\link{withinHumdrum}} arguments.
#' 
#' \code{\%hum<\%} acts the same way as \code{\%hum>\%} except it calls \code{\link{withHumdrum}}.
#' Since \code{\link{withHumdrum}} does \emph{not} return a \code{\linkS4class{humdrumR}},
#' the output can't be piped any further (using \code{\%hum>\%} or \code{\%hum<\%}).
#' Thus, \code{\%hum<\%} should only be used as the last step in a pipe---you would do this
#' if you want to extract the last step in your pipe from the data's \code{\link[humdrumR:humtable]{Humdrum Table}} into
#' a normal vector or list of R data.
#' 
#' @name humPipe
NULL

#' @name humPipe
#' @examples 
#' humdata <- readHumdrum('path*.krn')
#' 
#' humdata %hum>% ~table(.)
#' 
#' humdata %hum>%
#'      c(by~Spine, do ~ table(.)) %hum>%
#'      c(mfcol ~ c(2,2), doplot~barplot(.))
#'      
#' humdata %hum>%
#'      c(by ~ Spine, do ~ table(.)) %hum<%
#'      (do ~ sort(.))    
#' 
#' @export
`%hum>%` <- function(humdrumR, formula) {
          if (class(humdrumR) != 'humdrumR') stop("%hum>% pipe operator can only be called with humdrumR data on left side." )
          
          if (is.formula(formula) && is.null(lazyeval::f_lhs(formula))) {
           splitpipe <- splitPipe(formula)
           formula <- splitpipe$Current
           rest    <- splitpipe$Rest
          } else {
           rest <- NULL
                    
          }
                    
          output <- do.call('withinHumdrum', c(humdrumR, formula))
          
          if (!is.null(rest)) {
                    nextpipe <- call(splitpipe$Infix, quote(output), rest)
                    
                    eval(nextpipe)
          } else {
           output         
          }
}

#' @name humPipe
#' @export
`%hum<%` <- function(humdrumR, formula) {
          if (class(humdrumR) != 'humdrumR') stop("%hum<% pipe operator can only be called with humdrumR data on left side. \n
                                                  You will get this error if you put %hum<% anywhere but the last step of a humdrum pipe." )
          
          if (is.formula(formula) && is.null(lazyeval::f_lhs(formula))) {
           splitpipe <- splitPipe(formula)
           formula <- splitpipe$Current
           rest    <- splitpipe$Rest
          } else {
           rest <- NULL
                    
          }
                    
          output <- do.call('withHumdrum', c(humdrumR, formula))
          
          if (!is.null(rest)) {
                   stop("%hum<% pipe operator can only be called with humdrumR data on left side. 
                                                  You will get this error if you put %hum<% anywhere but the last step of a humdrum pipe." )
          } else {
           output         
          }
}



splitPipe <- function(formula) {
          # this function takes a right-sided formula and splits
          # of any part of it after a %hum><% within it.
          # This is because ~blah blah %hum>% blah blah will all get captured
          expr <- lazyeval::f_rhs(formula)
          expr <- paste(collapse = '', deparse(expr))
          
          exprs <- strsplit(expr, split = '%hum>%|%hum<%')[[1]]
          
          if (length(exprs) == 1) return(list(Current = formula, Rest = NULL, Infix = NA_character_))
          
          infix <- stringr::str_match(expr, '%hum<%|%hum>%')[1] 
          
          lazyeval::f_rhs(formula) <- parse(text = exprs[[1]])[[1]]
          
          list(Current = formula, 
               Rest = parse(text = do.call('paste', list(collapse = ' %hum>% ', exprs[-1])))[[1]],
               Infix = infix)
          
}

splitExpression <- function(expr, on = '|') {
  #' This function takes an expression and
  #' and breaks it into separate expressions based on
  #' top level calls to a infix function.
  if (!is.call(expr) || !deparse(expr[[1]]) %in% on) return(expr)
          
          ls <- Recall(expr[[2]], on)
          rs <- Recall(expr[[3]], on)
          c(ls, rs)
          
}



removeParentheses <- function(expr) {
 if (!is.call(expr)) return(expr)
 if (deparse(expr[[1]]) == '(') return(Recall(expr[[2]]))
          
 for (i in 2:length(expr)) {
  expr[[i]] <- Recall(expr[[i]])         
 }
 expr
                    
          
}