


splitExpression <- function(form, on = '|') {
  #' This function takes a formula
  #' looks for any top level | operators
  #' in the right hand side
  #' and splits the expression into pieces at these points
  expr <- lazyeval::f_rhs(form)
  
  exprstr <- deparse(expr)
  if (length(expr) > 1 && deparse(expr[[1]]) %in% on) {
            
    rest <- lazyeval::f_new(expr[[3]], env = lazyeval::f_env(form))
    head <- lazyeval::f_new(expr[[2]], env = lazyeval::f_env(form))
    
    head <- Recall(head)
    
  } else { 
    head <- form
    rest <- NULL
  }
  
  c(head, rest)
}


#### Applying to humdrumR ----

#' humApply as an infix
#' 
#' This infix operators makes it possible to use
#' humApply in a pipe-like scenario.
#' The clearest, long form, is \code{%humApply>%} but there
#' is also the equivalent short form \code{%hum>%} if you want
#' to save some keystrokes.
#' @name humApply-infix
NULL

#' @name humApply-infix
#' @examples 
#' humdata <- readHumdrum('path*.krn')
#' 
#' humdata %humApply>% ~table(.)
#' 
#' humdata %hum>%
#'      (list(by~Spine)~table(.)) %hum>%
#'      ~barplot(.)
#' @export
`%humApply>%` <- function(humdrumR, formula) {
          
          if (is.formula(formula)) {
           formulas <- splitExpression(formula, c('%humApply>%', '%hum>%'))
                    browser()
          }
         
          humApply(humdrumR, formula) 
}


#' @name humApply-infix
#' @export
`%hum>%` <- `%humApply>%`

