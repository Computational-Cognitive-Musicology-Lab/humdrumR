#############################################################
##### Methods for dplyr "verbs" #####################-----
##############################################################



### mutate ----

#' HumdrumR using Tidyverse "verbs"
#' 
#' These methods for [dplyr] verbs are all shorthand calls for [with/within/subset.humdrumR()][withHumdrum].
#' 
#' @name tidyHumdrum
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
  
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!exprs, 
                                              dataTypes = !!dataTypes,
                                              alignLeft = !!alignLeft,
                                              expandPaths = !!expandPaths)))
  
}

### summarize ----

#' @rdname tidyHumdrum
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
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!exprs, 
                                            dataTypes = !!dataTypes,
                                            drop = !!drop)))
  
}

### pull ----

#' @rdname tidyHumdrum
#' @export
pull.humdrumR <- function(.data, var, ..., dataTypes = 'D') {
  field <- rlang::enquo(var)
  
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!field, dataTypes = !!dataTypes)))
  
}


### filter ----

#' @rdname tidyHumdrum
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
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(subset.humdrumR(.data, !!!exprs))) 
}


### group_by ----

#' @rdname tidyHumdrum
#' @export
group_by.humdrumR <- function(.data, ..., .add = FALSE) {
  
  exprs <- as.list(rlang::enquos(...))
  names(exprs) <- 'by'
  
  .data@Groupby <- if (.add) {
    c(.data@Groupby, exprs)
  } else {
    exprs
  }
  
  .data
  
}

#' @rdname tidyHumdrum
#' @export
ungroup.humdrumR <- function(x, ...) {
  x@Groupby <- list()
  x
}

#############################################################
##### Methods for ggplot2 #####################-----
##############################################################


#' @rdname tidyHumdrum
#' @export
ggplot.humdrumR <- function(data = NULL, mapping = aes(), ..., dataTypes = 'D') {
  humtab <- getHumtab(data, dataTypes = dataTypes)
  
  ggplot(humtab, mapping = mapping, ...)
}

## Theme


