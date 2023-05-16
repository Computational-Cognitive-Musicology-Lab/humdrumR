#############################################################
##### Methods for dplyr "verbs" #####################-----
##############################################################



### mutate ----

#' @export
mutate.humdrumR <- function(.data, ..., dataTypes = 'D', alignLeft = TRUE, expandPaths = FALSE) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- NULL
  
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!exprs, 
                                              dataTypes = !!dataTypes,
                                              alignLeft = !!alignLeft,
                                              expandPaths = !!expandPaths)))
  
}

### summarize ----

#' @export
summarize.humdrumR <- function(.data, ..., dataTypes = 'D', expandPaths = FALSE, drop = FALSE) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- NULL
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!exprs, 
                                            dataTypes = !!dataTypes,
                                            drop = !!drop)))
  
}

### pull ----

#' @export
pull.humdrumR <- function(.data, var, ..., dataTypes = 'D') {
  field <- rlang::enquo(var)
  
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!field, dataTypes = !!dataTypes)))
  
}


### filter ----
#' @export
filter.humdrumR <- function(.data, ...) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- NULL
  
  rlang::eval_tidy(rlang::quo(subset.humdrumR(.data, !!!exprs))) 
}