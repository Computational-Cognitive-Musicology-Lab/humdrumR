#############################################################-
# dyplr stuff #####################-----
##############################################################-

## Hhelpers ----


tidyNamer <- function(quosures) {
  names <- .names(quosures)
  
  assigned <- sapply(quosures, 
                        \(quo) {
                          exprA <- analyzeExpr(quo)
                          if (exprA$Head == '<-')  rlang::as_label(exprA$Args[[1]]) else  '' 
                        })
  
  doubleAssign <- names != '' & assigned != ''
  if (any(doubleAssign)) {
    sameName <- names[doubleAssign] == assigned[doubleAssign]
    if (all(sameName)) {
      example <- names[doubleAssign][which(sameName)[1]]
      .warn("You are using '=' AND '<-' at the same time; you only need one or the other.",
            "For example, '{example} = {example} <-' could just be '{example} ='.")
    } else {
      bad <- paste0(names[doubleAssign][!sameName], ' = ', assigned[doubleAssign][!sameName], ' <- ...')
      .stop("You are using '=' AND '<-' to assign contradictory field names.",
            "For example, '{bad[1]}' is contradictory.")
    }
  }
  
  names <- ifelse(names == '', assigned, names)
  names[names == ''] <- sapply(quosures[names == ''], rlang::as_label)
  # if (any(duplicated(names))) .stop("You can't run summarize.humdrumR() and give {num2word(max(table(names)))} columns the same name!")
  
  names <- rlang::syms(names)
  
  Map(quosures, names, f = \(quo, name) rlang::quo(!!name <- !!quo))
  
}


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
  
  quosures <- tidyNamer(quosures)
  
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
  
  quosures <- tidyNamer(quosures)
  
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
  
  quosures <- tidyNamer(quosures)
  
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
pull.humdrumR <- function(.data, ..., dataTypes = 'D', fieldTypes = "any", null = 'asis', drop = TRUE) {
  
  exprs <- rlang::enexprs(...)
  
  fields <- if (length(exprs)) {
    tidyselect_humdrumRfields(.data, exprs, fieldTypes, 'pull.humdrumR()')
  } else {
    selectedFields(.data)
  }
 
  pulled <- pullFields(.data, fields = fields, dataTypes = dataTypes, null = null)

  if (drop) pulled[[1]] else pulled
  
}


### filter ----

#' @rdname tidyHumdrum
#' @export
filter.humdrumR <- function(.data, ...) {
  exprs <- rlang::enquos(...)
  .data <- uncontextMessage(.data, 'filter')
  
  rlang::eval_tidy(rlang::quo(subset.humdrumR(.data, !!!exprs))) 
}


### group_by ----


#' @rdname tidyHumdrum
#' @export
group_by.humdrumR <- function(.data, ..., .add = FALSE) {
  .data <- uncontextMessage(.data, 'group_by')
  
  if (!.add) .data <- ungroup(.data)
  
  selectedFields <- selectedFields(.data)
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
  
  selectFields(.data, selectedFields)

  
  
  
}

#' @rdname tidyHumdrum
#' @export
ungroup.humdrumR <- function(x, ...) {
  fields <- fields(x)
  fields[ , GroupedBy := FALSE]
  remove <- fields[Type == 'Grouping', Name]
  if (length(remove)) {
    fields <- fields[Type != 'Grouping']
    for (field in remove) x@Humtable[[field]] <- NULL
  }
  
  x@Fields <- fields
  x
  
}



### select ----

#' @rdname selectedFields
#' @export
select.humdrumR <- function(.data, ..., fieldTypes = "any") {
 
    exprs <- rlang::enexprs(...)
    fields <- if (length(exprs) == 0L) {
      'Token'
    } else {
      tidyselect_humdrumRfields(.data, exprs, fieldTypes, 'select.humdrumR')
    }
    selectFields(.data, fields)
}

tidyselect_humdrumRfields <- function(humdrumR, exprs, fieldTypes, callname) {
  fields <- fields(humdrumR)
  
  types <- c('Data', 'Structure', 'Interpretation', 'Formal', 'Reference', 'Grouping')
  
  fieldTypes <- if ('any' %in% tolower(fieldTypes)) {
    types
  } else {
    fieldTypes <- checkFieldTypes(fieldTypes, 'fieldTypes', callname, includeSelected = FALSE)
  }
  
  # select by field type (character string only, partially matched)
  typeSelections <- lapply(exprs, 
                           \(expr) {
                             if (is.character(expr)) {
                               type <- types[pmatch(expr, types, nomatch = 0L)]
                               if (length(type)) fields[Type == type, Name]
                             }
                           })
  
  exprs <- exprs[lengths(typeSelections) == 0L]
  
  # select by field names
  fields <- fields[order(!Type %in% fieldTypes)]

  options <- fields$Name
  expr <- rlang::expr(c(!!!exprs))
  fieldSelections <- options[tidyselect::eval_select(expr, setNames(options, options), strict = FALSE)]
  ##
  selections <- union(fieldSelections, unlist(typeSelections))
  
  
  if (length(selections) == 0L) {
    exprs <- do.call('harvard', c(lapply(rlang::enexprs(...), rlang::as_label), conjunction = '', quote = TRUE))
    .stop("The <expressions {exprs} don't|expression {exprs} doesn't> match any {fieldTypes} fields in your humdrumR data.",
          ifelse = length(exprs) > 1)
  }
  
  selections
}

### pivot ----

#' @rdname tidyHumdrum
#' @export
pivot_wider.humdrumR <- function(data, names_from = 'Spine', value_from = selectedFields(data)[1], fold, onto) {
  if (missing(fold)) fold <- 2:ncol(data)
  if (missing(onto)) onto <- 1
  foldHumdrum(data, fold, onto, what = names_from[1], fromField = value_from, newFieldNames = paste0(names_from, fold))
}

#' @rdname tidyHumdrum
#' @export
pivot_longer.humdrumR <- function(data, cols, fieldTypes = 'D') {
  fields <- tidyselect_humdrumRfields(data, list(rlang::enexpr(cols)), fieldTypes = fieldTypes, callname = 'pivot_longer.humdrumR()')
  
  rend(data, fields)
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

## Other functions ----


#' @rdname ditto
#' @export
ditto.humdrumR <- function(x, ..., initial = NA, reverse = FALSE) {
  
  quosures <- rlang::enquos(...)
  
  
  if (length(quosures) == 0L) {
    selected <- selectedFields(x)
    quosures <- setNames(rlang::syms(selected), selected)
  }
  
  quosures <- tidyNamer(quosures) # this makes all expressions of form name <- quo
  
  quosures <- lapply(quosures,
                     \(quo) {
                       exprA <- analyzeExpr(quo)
                       
                       if (rlang::as_label(exprA$Args[[1]]) %in% fields(x)$Name) {
                         exprA$Args[[1]] <- paste0("ditto(", exprA$Args[[1]], ")")
                       } 
                       
                       expr <- exprA$Args[[2]]
                       expr <- rlang::quo(ditto(!!expr, initial = !!initial, reverse = !!reverse))
                       
                       exprA$Args[[2]] <- expr
                       unanalyzeExpr(exprA)
                     })
 
  rlang::eval_tidy(rlang::quo(within(x, !!!quosures, dataTypes = 'Dd')))
  
}
