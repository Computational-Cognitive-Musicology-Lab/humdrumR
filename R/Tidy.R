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
  
  # names <- .names(exprs)
  # exprs[names != ''] <- Map(exprs[names != ''], names[names != ''],
  #                           f = \(expr, name) {
  #                             name <- rlang::sym(name)
  #                             
  #                             rlang::quo(!!name <- !!expr)
  #                           })
  # names(exprs) <- NULL
  
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

test <- function(func) {
  args <- formals(func)
   firstArg <- names(args)[1]
  func <- rlang::enexpr(func)
  firstArg <- rlang::sym(names(args)[1])
  bquote({
    
    quos <- rlang::enquos(...)
    
    if (!any(.names(quos) %in% c(firstArg, ''))) quos <- c(rlang::quo(.), quos)
    browser()
    rlang::eval_tidy(rlang::expr(within(.(rlang::sym(firstArg)), kern.default(!!!quos))))
    
  }) -> expr
  
  rlang::new_function(alist(x=, ... =), expr)
}

humdrumRmethods <- function(name) {
  # prexisting method becomes .default
  default <- match.fun(name)
  envir <- rlang::fn_env(default)
  
  args <- formals(default)
  assign(paste0(name, '.default'), default, parent.frame())
  
  # humdrumR method
  .default <- rlang::sym(paste0(name, '.default'))
  firstArg <- names(args)[1]
  Name <- rlang::sym(stringr::str_to_title(name))
  
  body <- bquote({
    quos <- rlang::enquos(...)
    
    if (!any(.names(quos) %in% c(.(firstArg), ''))) quos <- c(rlang::quo(.), quos)
    rlang::eval_tidy(rlang::expr(within(.(rlang::sym(firstArg)), .(Name) <- .(.default)(!!!quos))))
  })
  
  # subargs <- args[-1]
  # ldots <- names(subargs) == '...'
  # if (any(ldots)) {
    # subargs[[which(ldots)]] <- quote(...)
    # names(subargs)[which(ldots)] <- ''
  # }
  # subargs[!ldots] <- rlang::syms(names(subargs)[!ldots])
  
  #### insert "auto args"?
  # autoArgs <- autoArgTable[Function == name]
  # subargs[autoArgs$Argument] <- autoArgs$Expression
  
  
  humdrumR <- rlang::new_function(setNames(alist(x = , ... = ), c(firstArg, '...')), env = envir,
                                  body) #rlang::expr(within.humdrumR(!!firstArg, !!Name <- (!!.default)(., !!!subargs))))
  
  assign(paste0(name, '.humdrumR'), humdrumR, parent.frame())
  
  
  # generic function
  generic <- rlang::new_function(args,
                                 rlang::expr(UseMethod(!!name)),
                                 env = envir)
  class(generic) <- class(default)
  
  assign(name, generic, parent.frame())
  
}

## Pitch functions ----

# lapply(rapply(humdrumR:::pitchFunctions, 
#               \(x) ifelse(humdrumR:::.names(x) == '', x, names(x))),
#        \(func) {
#          paste0(sep = '',
#              "#' @exportS3Method ", func, " default\n",
#              "#' @exportS3Method ", func, " humdrumR\n",
#              "humdrumRmethods('", func, "')\n")
#        }) |> do.call(what = 'paste0') |> clipr::write_clip()

#' @exportS3Method kern default
#' @exportS3Method kern humdrumR
humdrumRmethods('kern')
#' @exportS3Method pitch default
#' @exportS3Method pitch humdrumR
humdrumRmethods('pitch')
#' @exportS3Method lilypond default
#' @exportS3Method lilypond humdrumR
humdrumRmethods('lilypond')
#' @exportS3Method helmholtz default
#' @exportS3Method helmholtz humdrumR
humdrumRmethods('helmholtz')
#' @exportS3Method tonh default
#' @exportS3Method tonh humdrumR
humdrumRmethods('tonh')
#' @exportS3Method interval default
#' @exportS3Method interval humdrumR
humdrumRmethods('interval')
#' @exportS3Method solfa default
#' @exportS3Method solfa humdrumR
humdrumRmethods('solfa')
#' @exportS3Method solfg default
#' @exportS3Method solfg humdrumR
humdrumRmethods('solfg')
#' @exportS3Method degree default
#' @exportS3Method degree humdrumR
humdrumRmethods('degree')
#' @exportS3Method deg default
#' @exportS3Method deg humdrumR
humdrumRmethods('deg')
#' @exportS3Method bhatk default
#' @exportS3Method bhatk humdrumR
humdrumRmethods('bhatk')
#' @exportS3Method step default
#' @exportS3Method step humdrumR
humdrumRmethods('step')
#' @exportS3Method accidental default
#' @exportS3Method accidental humdrumR
humdrumRmethods('accidental')
#' @exportS3Method quality default
#' @exportS3Method quality humdrumR
humdrumRmethods('quality')
#' @exportS3Method octave default
#' @exportS3Method octave humdrumR
humdrumRmethods('octave')
#' @exportS3Method semits default
#' @exportS3Method semits humdrumR
humdrumRmethods('semits')
#' @exportS3Method midi default
#' @exportS3Method midi humdrumR
humdrumRmethods('midi')
#' @exportS3Method cents default
#' @exportS3Method cents humdrumR
humdrumRmethods('cents')
#' @exportS3Method pc default
#' @exportS3Method pc humdrumR
humdrumRmethods('pc')
#' @exportS3Method freq default
#' @exportS3Method freq humdrumR
humdrumRmethods('freq')



### Interval functions ----

#' @exportS3Method mint default
#' @exportS3Method mint humdrumR
humdrumRmethods('mint')
#' @exportS3Method hint default
#' @exportS3Method hint humdrumR
humdrumRmethods('hint')

## Rhythm functions ----

# lapply(rapply(humdrumR:::rhythmFunctions,
#               \(x) ifelse(humdrumR:::.names(x) == '', x, names(x))),
#        \(func) {
#          paste0(sep = '',
#              "#' @exportS3Method ", func, " default\n",
#              "#' @exportS3Method ", func, " humdrumR\n",
#              "humdrumRmethods('", func, "')\n")
#        }) |> do.call(what = 'paste0') |> clipr::write_clip()

#' @exportS3Method recip default
#' @exportS3Method recip humdrumR
humdrumRmethods('recip')
#' @exportS3Method notehead default
#' @exportS3Method notehead humdrumR
humdrumRmethods('notehead')
#' @exportS3Method duration default
#' @exportS3Method duration humdrumR
humdrumRmethods('duration')
#' @exportS3Method quarters default
#' @exportS3Method quarters humdrumR
humdrumRmethods('quarters')
#' @exportS3Method dur default
#' @exportS3Method dur humdrumR
humdrumRmethods('dur')
#' @exportS3Method seconds default
#' @exportS3Method seconds humdrumR
humdrumRmethods('seconds')
#' @exportS3Method ms default
#' @exportS3Method ms humdrumR
humdrumRmethods('ms')
