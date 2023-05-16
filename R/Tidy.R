#############################################################
##### Methods for dplyr "verbs" #####################-----
##############################################################



### mutate ----

#' @export
mutate.humdrumR <- function(.data, ...) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- NULL
  
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!exprs)))
  
}

### summarize ----

#' @export
summarize.humdrumR <- function(.data, ...) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- NULL
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!exprs, drop = FALSE)))
  
}

### pull ----

#' @export
pull.humdrumR <- function(.data, var, ...) {
  field <- rlang::enquo(var)
  
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!field, ...)))
  
}
