#############################################################-
# dyplr stuff #####################-----
##############################################################-

## Hhelpers ----



## Methods for dplyr "verbs" ----

### mutate ----

#' HumdrumR using Tidyverse "verbs"
#' 
#' These methods for [dplyr] verbs are all shorthand calls for [with/within/subset.humdrumR()][withHumdrum].
#' 
#' @rdname tidyHumdrum
#' @export
mutate.humdrumR <- function(.data, ..., dataTypes = 'D', recycle = c('scalar', 'never'), alignLeft = TRUE, expandPaths = FALSE, .by = NULL) {
  quosures <- rlang::enquos(...)
  
  recycle <- match.arg(recycle)
  
  #names
  names <- unlist(Map(quosures, .names(quosures), f = \(quo, name) if (name == '')  rlang::as_label(quo) else name))
  if (any(duplicated(names))) .stop("You can't run mutate.humdrumR() and give {num2word(max(table(names)))} new fields the same name!")
  quosures <- Map(quosures, names, f = \(quo, name) rlang::quo(!!(rlang::sym(name)) <- !!quo))
  
  # eval
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!quosures, recycle = !!recycle,
                                              dataTypes = !!dataTypes,
                                              alignLeft = !!alignLeft,
                                              expandPaths = !!expandPaths,
                                              .by = !!.by)))
  
}

### summarize ----

#' @rdname tidyHumdrum
#' @export
summarise.humdrumR <- function(.data, ..., dataTypes = 'D', expandPaths = FALSE, drop = FALSE, .by = NULL) {
  quosures <- rlang::enquos(...)
  
  # names
  names <- unlist(Map(quosures, .names(quosures), f = \(quo, name) if (name == '')  rlang::as_label(quo) else name))
  if (any(duplicated(names))) .stop("You can't run summarize.humdrumR() and give {num2word(max(table(names)))} columns the same name!")
  quosures <- Map(quosures, names, f = \(quo, name) rlang::quo(!!(rlang::sym(name)) <- !!quo))
  
  # eval
  rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!quosures, recycle = 'summarize',
                                            dataTypes = !!dataTypes,
                                            drop = !!drop,
                                            .by = !!.by)))
  
}


### summarize ----

#' @rdname tidyHumdrum
#' @export
reframe.humdrumR <- function(.data, ..., dataTypes = 'D', alignLeft = TRUE, expandPaths = FALSE, .by = NULL) {
  quosures <- rlang::enquos(...)
  
  # names
  names <- unlist(Map(quosures, .names(quosures), f = \(quo, name) if (name == '')  rlang::as_label(quo) else name))
  if (any(duplicated(names))) .stop("You can't run mutate.humdrumR() and give {num2word(max(table(names)))} new fields the same name!")
  quosures <- Map(quosures, names, f = \(quo, name) rlang::quo(!!(rlang::sym(name)) <- !!quo))
  
  # eval
  rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!quosures, recycle = 'pad',
                                              dataTypes = !!dataTypes,
                                              alignLeft = !!alignLeft,
                                              expandPaths = !!expandPaths,
                                              .by = !!.by)))
  
}


### pull ----

#' @rdname tidyHumdrum
#' @export
pull.humdrumR <- function(.data, var, ..., dataTypes = 'D', null = 'asis', drop = TRUE) {
  if (missing(var)) {
    pulled <- pullSelectedFields(.data, dataTypes = dataTypes, null = null)
  } else {
    fields <- rlang::enquos(var, ...)
    pulled <- rlang::eval_tidy(rlang::quo(with.humdrumR(.data, !!!fields, dataTypes = !!dataTypes, drop = FALSE)))
    
  }
  if (drop) pulled[[1]] else pulled
  
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
  
  rlang::eval_tidy(rlang::quo(subset.humdrumR(.data, !!!exprs))) 
}


### group_by ----


#' @rdname tidyHumdrum
#' @export
group_by.humdrumR <- function(.data, ..., .add = FALSE) {
  if (!.add) .data <- ungroup(.data)
  
  exprs <- rlang::enquos(...)
  calls <- sapply(exprs, rlang::quo_is_call)
  
  groupFields <- sapply(exprs[!calls], rlang::as_name)
  if (length(groupFields)) groupFields <- fieldMatch(.data, groupFields, 'group_by')
  
  
  fields <- fields(.data)
  groupn <- max(fields$GroupedBy)
  
  if (any(calls)) {
    oldfields <- fields$Name
    
    .data <- rlang::eval_tidy(rlang::quo(within.humdrumR(.data, !!!(exprs[calls]))))
    fields <- fields(.data)
    
    newfields <- fields[ , !Name %in% oldfields]
    fields$Type[newfields] <- 'Grouping'
    groupFields <- c(fields$Name[newfields], groupFields)
  }
  
  fields[ , GroupedBy := Name %in% groupFields | GroupedBy]
  .data@Fields <- fields
  
  .data
  
  
  
}

#' @rdname tidyHumdrum
#' @export
ungroup.humdrumR <- function(x, ...) {
  fields <- fields(x)
  fields[ , GroupedBy := FALSE]
  
  remove <- fields[Type == 'Grouping', Name]
  if (length(remove)) {
    fields <- fields[Type != 'Grouping']
    x@Fields <- fields
    
    x@Humtable[, remove := NULL]
  }
  
  x
  
}



### select ----

#' @rdname selectedFields
#' @export
select.humdrumR <- function(.data, ...) {
    fields <- sapply(rlang::ensyms(...), as.character)
    if (length(fields) == 0L) fields <- 'Token'
    
    fieldTypes <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference')
    if (any(fields %in% fieldTypes)) {
     fields <- unique(c(fields, fields(.data)[Type %in% fields]$Name))
     fields <- setdiff(fields, fieldTypes)
    }
    
    fields <- fieldMatch(.data, fields, callfun = 'select')
    
    selectFields(.data, fields)
    
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
  
  ggplot(as.data.frame(data), mapping = mapping, ...) + theme_humdrum()
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


# Indexing in pipes ----


#' @rdname tidyHumdrum
#' @export
index <- function(x, i, j, drop = TRUE) {
  
  pat <- paste0(missing(i), missing(j))
  
  switch(pat,
         'TRUETRUE' = x,
         'TRUEFALSE' = x[  , j, drop],
         'FALSETRUE' = x[i ,  , drop],
         'FALSEFALSE' = x[i, j, drop])
}
#' @rdname tidyHumdrum
#' @export
index2 <- function(x, i, j, drop = TRUE) {
  
  pat <- paste0(missing(i), missing(j))
  
  if (!is.humdrumR(x)) return(x[[i]])
  switch(pat,
         'TRUETRUE' = x,
         'TRUEFALSE' = x[[  , j, drop]],
         'FALSETRUE' = x[[i ,  , drop]],
         'FALSEFALSE' = x[[i, j, drop]])
}

# Humdrum-style application ----

humdrumRmethods <- function(name) {
  # prexisting method becomes .default
  default <- match.fun(name)
  envir <- rlang::fn_env(default)
  
  args <- formals(default)
  assign(paste0(name, '.default'), default, parent.frame())
  
  # humdrumR method
  subcall <- rlang::sym(paste0(name, '.default'))
  firstArg <- rlang::sym(names(args)[1])
  
  subargs <- args[-1]
  ldots <- names(subargs) == '...'
  if (any(ldots)) {
    subargs[[which(ldots)]] <- quote(...)
    names(subargs)[which(ldots)] <- ''
  }
  subargs[!ldots] <- rlang::syms(names(subargs)[!ldots])
  
  #### insert "auto args"
  autoArgs <- autoArgTable[Function == name]
  subargs[autoArgs$Argument] <- autoArgs$Expression
  
  Name <- rlang::sym(stringr::str_to_title(name))
  
  humdrumR <- rlang::new_function(args[!names(args) %in% autoArgs$Argument], env = envir,
                                  rlang::expr(within.humdrumR(!!firstArg, !!Name <- (!!subcall)(., !!!subargs))))
  
  assign(paste0(name, '.humdrumR'), humdrumR, parent.frame())
  
  
  # generic function
  generic <- rlang::new_function(args,
                                 rlang::expr(UseMethod(!!name)),
                                 env = envir)
  class(generic) <- class(default)
  
  assign(name, generic, parent.frame())
  
}

## Pitch functions ----

#' @exportS3Method kern default
#' @exportS3Method kern humdrumR
humdrumRmethods('kern')
#' @exportS3Method solfa default
#' @exportS3Method solfa humdrumR
humdrumRmethods('solfa')


### Interval functions ----

#' @exportS3Method mint default
#' @exportS3Method mint humdrumR
humdrumRmethods('mint')
#' @exportS3Method hint default
#' @exportS3Method hint humdrumR
humdrumRmethods('hint')

## Rhythm functions ----

#' @exportS3Method recip default
#' @exportS3Method recip humdrumR
humdrumRmethods('recip')

#' @exportS3Method duration default
#' @exportS3Method duration humdrumR
humdrumRmethods('duration')