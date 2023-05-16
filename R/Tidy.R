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
  
  exprs <- rlang::enquos(...)
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
  
  ggplot(humtab, mapping = mapping, ...) + theme_humdrum()
}

## Theme

### Colors ----
scale_color_humdrum <- scale_fill_manual(values = flatly)
# scale_color_continuous(type = colorRamp(flatly[2:3]))

options(ggplot2.continuous.fill = scale_color_gradientn(colors = flatly_continuous(100)))
options(ggplot2.continuous.color = scale_color_gradientn(colours = flatly_continuous(100)))
options(ggplot2.continuous.colour = scale_color_gradientn(colours = flatly_continuous(100)))

# options(ggplot2.continuous.colour = 'humdrum')

### theme ----
theme_humdrum <- function() {
  theme(panel.background = element_blank(), axis.ticks = element_blank(),
        strip.background = element_blank(), 
        # panel.border = element_rect(linetype = 'dashed', fill = NA),
        legend.key = element_rect(fill = NA),
        title = element_text(family = 'Lato', color = flatly[5], size = 16),
        plot.title.position = 'plot', plot.title = element_text(hjust = .5),
        line = element_line(color = flatly[1]),
        rect = element_rect(color = flatly[2]),
        text = element_text(family = 'Lato', color = flatly[4]),
        axis.text = element_text(color = flatly[5], size = 7),
        axis.title = element_text(color = flatly[4], size = 11)
        )
}

update_geom_defaults("point", list(size = .5, color = flatly[1], fill = flatly[2]))
update_geom_defaults("line", list(size = .5, color = flatly[4], fill = flatly[3]))
update_geom_defaults("rect", list(fill = flatly[1]))

