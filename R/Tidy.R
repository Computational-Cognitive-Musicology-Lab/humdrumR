#############################################################-
# Methods for dplyr "verbs" #####################-----
##############################################################-



### mutate ----

#' HumdrumR using Tidyverse "verbs"
#' 
#' These methods for [dplyr] verbs are all shorthand calls for [with/within/subset.humdrumR()][withHumdrum].
#' 
#' @name tidyHumdrum
#' @export
mutate.humdrumR <- function(.data, ..., dataTypes = 'D', fill = TRUE, alignLeft = TRUE, expandPaths = FALSE) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
                            f = \(expr, name) {
                              name <- rlang::sym(name)
                              
                              rlang::quo(!!name <- !!expr)
                            })
  names(exprs) <- if (fill) 'fill' else NULL
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!exprs, 
                                              dataTypes = !!dataTypes,
                                              alignLeft = !!alignLeft,
                                              expandPaths = !!expandPaths)))
  
}

### summarize ----

#' @rdname tidyHumdrum
#' @export
summarise.humdrumR <- function(.data, ..., dataTypes = 'D', expandPaths = FALSE, drop = FALSE) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs <- Map(exprs, names,
                            f = \(expr, name) {
                              expr <- rlang::quo({
                                result <- {!!expr}
                                if (length(result) != 1) .stop("The summarize() function only works when the computed values are length 1 (per group).",
                                                               "In your case, we've found (at least one) result which is length {length(result)}.",
                                                               "\nTry using reframe() or within() instead, as these functions allow results of any length.")
                                result
                              })
                              
                              if (name != '') {
                                name <- rlang::sym(name)
                                expr <-  rlang::quo(!!name <- !!expr)
                              }
                              expr
                              
                            })
  names(exprs) <- NULL
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!exprs, 
                                            dataTypes = !!dataTypes,
                                            drop = !!drop)))
  
}


### summarize ----

#' @rdname tidyHumdrum
#' @export
reframe.humdrumR <- function(.data, ..., dataTypes = 'D', expandPaths = FALSE) {
  exprs <- rlang::enquos(...)
  
  names <- .names(exprs)
  exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
               f = \(expr, name) {
                 
                   name <- rlang::sym(name)
                   expr <-  rlang::quo(!!name <- !!expr)
               })
  names(exprs) <- NULL
  exprs <- c(exprs, .data@Groupby)
  
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!exprs, 
                                              dataTypes = !!dataTypes)))
  
}


### pull ----

#' @rdname tidyHumdrum
#' @export
pull.humdrumR <- function(.data, var, ..., dataTypes = 'D') {
  if (missing(var)) return(evalActive(.data, dataTypes = dataTypes, ...))
  
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
  
  # if (all(sapply(exprs, rlang::quo_is_symbol))) exprs <- rlang::quos(list(!!!exprs))
  names(exprs) <- rep('by', length(exprs))
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


### select ----

#' @rdname tidyHumdrum
#' @export
select.humdrumR <- function(.data, ...) {
    fields <- rlang::ensyms(...)
    
    fields <- fieldMatch(.data, sapply(fields, as.character), callfun = 'select')
    
    active <- if (length(fields) == 1L) rlang::quo(!!(rlang::sym(fields))) else rlang::quo(list(!!!(rlang::syms(fields))))
    
    .data@Active <- active
    .data
  
}

#############################################################-
# ggplot2 stuff #############################################-----
##############################################################-

## Methods ----

#' @rdname tidyHumdrum
#' @export
ggplot.humdrumR <- function(data = NULL, mapping = aes(), ..., dataTypes = 'D') {
  humtab <- getHumtab(data, dataTypes = dataTypes)
  
  ggplot(humtab, mapping = mapping, ...) + theme_humdrum()
}

#' @rdname tidyHumdrum
#' @export
ggplot.humdrum.table <- function(data = NULL, mapping = aes(), ...) {
  
  ggplot(as.data.frame(tab), mapping = mapping, ...) + theme_humdrum()
}


### Treatment of token ----

#' @export
scale_type.token <- function(x) if (class(x@.Data) %in% c('integer', 'numeric', 'integer64')) 'continuous' else 'discrete'


#' @export
scale_x_token <- function(..., expand = waiver(), guide = waiver(), position = "bottom") {
  sc <- ggplot2::discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                                # limits = c("c", "c#", "d-", "d", "d#", "e-", "e", "e#", "f", "f#", "f##", "g-", "g", "g#", "a-", "a", "a#", "b-", "b", "b#"),
                                expand = expand, guide = guide, position = position, super = ScaleDiscretePosition)
  
  sc$range_c <- scales::ContinuousRange$new()
  sc
}







## Colors ----

scale_color_humdrum <- ggplot2::scale_fill_manual(values = flatly)
# scale_color_continuous(type = colorRamp(flatly[2:3]))

options(ggplot2.continuous.fill = ggplot2::scale_color_gradientn(colors = flatly_continuous(100)))
options(ggplot2.continuous.color = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))
options(ggplot2.continuous.colour = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))

# options(ggplot2.continuous.colour = 'humdrum')

## Theme ----

theme_humdrum <- function() {
  ggplot2::update_geom_defaults("point", list(size = .5, color = flatly[1], fill = flatly[2]))
  ggplot2::update_geom_defaults("line", list(size = .5, color = flatly[4], fill = flatly[3]))
  ggplot2::update_geom_defaults("rect", list(fill = flatly[1]))
  
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



