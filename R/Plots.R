
# Plotting ----




## Plotting functions ----

### draw() ----

#' Visualize data
#' 
#' The `draw()` function is humdrumR's goto plotting function.
#' `draw()` can make a variety of graphs, depending on the type of data you give it.
#' For the most part, `draw()` is simply a stylish, easy to use wrapper around
#' the base-R graphics functions [plot()], [barplot()], and [hist()].
#' 
#' 
#' @details
#' 
#' `draw()` is a generic function, which does different plots depending on the data you pass to its
#' `x` and `y` arguments.
#' 
#' + `x` and `y` both numeric: scatter plot.
#' + `x` numeric by itself: histogram.
#' + `y` numeric by itself: quantile plot.
#' + `x` is a [table][count()]: barplot.
#' + `y` is numeric, `x` is `character` or `factor`: a violin plot.
#' 
#' All the standard arguments to base-R plots can be used to customize plots.
#' See [par()] for a full list.
#' 
#' 
#' @export
draw <- function(x, y, facet = list(), ..., 
                 xlab = NULL, ylab = NULL, 
                 axes = 1:4,
                 main = '', sub = '', 
                 par = list(family = 'Helvetica', col = 4, col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2, cex.axis =.7, pch = 16)) {
  oldpalette <- palette(flatly)
  oldpar <- par(par)
  
  on.exit({par(oldpar) ; palette(oldpalette)})
  
  # xlab and ylab
  xexpr <- deparse1(substitute(x)) 
  yexpr <- deparse1(substitute(y)) 
  if (xexpr == '') xexpr <- 'x'
  if (yexpr == '') yexpr <- 'y'
  
  output <- if (length(facet)) {
    if (!is.list(facet)) facet <- list(facet)
    par(mar = c(1, 1, 1, 1), oma = c(5, 5, 5, 5))
    draw_facets(facet, xlab = '', ylab = '', ...)
  } else {
    .draw(x = if (!missing(x)) x, y = if (!missing(y)) y, ...)
  }
  
  title(main = main, sub = sub)
  output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
  output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  
  if (length(output$axes)) humaxes(output$axes[side %in% axes])
  
  Map(output$axisNames, 1:4, f = \(label, side) if (!is.null(label)) mtext(label, side, las = if (nchar(label) > 3 && side %% 2 == 0) 3 else 1))
  
  # if (!is.null(attr(col, 'levels'))) legend('topleft', horiz = TRUE, xpd = TRUE, pch = 16, cex = .8, bty = 'n',
                                            # col = sort(unique(col)), legend = attr(col, 'levels'))
  
}
  
setGeneric('.draw', def =  \(x, y,  ...) standardGeneric('.draw'))

#### .draw numeric ----

setMethod('.draw', c('numeric', 'numeric'), 
          \(x, y, col = 3, log = '', jitter = '', 
            xlim = NULL, ylim = NULL, cex = prep_cex(x), quantiles = c(), ...) {
            
            
         
            
            
            output <- canvas(x = x, xlim = xlim, 
                             y = y, ylim = ylim,
                             log = log)
            
            draw_quantiles(1, x, quantiles)
            draw_quantiles(2, y, quantiles)
            
            
            if (grepl('x', jitter)) x <- smartjitter(x)
            if (grepl('y', jitter)) y <- smartjitter(y)
            
            points(x, y, col = col, cex = cex, ...)
            
            
            output
            
          })

setMethod('.draw', c('numeric', 'NULL'), 
          \(x, y, col = 3, log = '', jitter = '', xlim = NULL, ylim = NULL,
            breaks = 'Sturges', cex = prep_cex(x) * .75, quantiles = c(), ...) {
            
            histogram <- hist.default(x, breaks = breaks, plot = FALSE)
            prob <- histogram$density * diff(histogram$breaks)
            
            
            ylim <- ylim %||% c(0, 2^(ceiling(log( max(prob), 2)) + 1)) # 1, .5, .25, .125, etc.
            output <- canvas(x = x, xlim = xlim %||%range(histogram$breaks), 
                             y = prob[prob > 0], ylim = ylim, 
                             log = log)
            
            draw_quantiles(1, x, quantiles)
            
            # actual plot ov polygons
            ymin <- min(output$window$ylim[[1]])
            Map(head(histogram$breaks, -1), tail(histogram$breaks, -1), prob,
                f = \(x0, x1, p) {
                  polygon(c(x0, x0, x1, x1), c(ymin, p, p, ymin), col = setalpha(col, .2), border = NA)
                  
                  graphics::segments(x0, p, x1, p, col = col)
                })
            
            # lines between polygons:
            graphics::segments(histogram$breaks, 0, histogram$breaks, pmax(c(prob, 0), c(0, prob)),
                               col = setalpha(col, .3))
            
            # prepare ticks
            
            ## x
            x.ticks <- histogram$breaks
            while(length(x.ticks) > 20L) {
              x.ticks <- x.ticks[seq(1, length(x.ticks), by = 2)]
            }
            output$axes[side == 1, ticks := x.ticks]
            
            ## y
            output$axes[side == 2, ticks := setNames(ticks[[1]], format(paste0(ticks[[1]] * 100, '%')))]
              
              
            ## counts (side 4)
            count.ticks <- unique(round(pretty(c(0, sum(histogram$counts) * output$axes[side == 2, ticks[[1]]]), n = 10L, min.n = 5L)))
            count.ticks <- structure(count.ticks / sum(histogram$counts), names = count.ticks)
            output$axes <- rbind(output$axes,
                                 data.table(side = 4, ticks = list(count.ticks), line = 0))
        
            
            output$axisNames[c(2,4)] <- list('Proportion', 'Count')
            output
            
          })

setMethod('.draw', c('NULL', 'numeric'),
          function(x, y, log = '', xlim = NULL, ylim = NULL, ..., quantiles = c(.25, .5, .75)) {
            
            
            output <- canvas(x = c(0, 1), xlim = xlim, 
                             y = y, ylim = ylim , 
                             log = gsub('x', '', log))
            
            output$axisNames[[1]] <- 'Quantile'
            
            draw_quantiles(2, y, quantiles = quantiles)
            
            
            y <- sort(y)
            x <- seq(0, 1, length.out = length(y))
            
            points(x = x, y = y, ...)
            
            
            output
          })


#### .draw discrete ----


setMethod('.draw', c('table', NULL),
          function(x, y, log = '', ylim = NULL, col = NULL,  ..., beside = TRUE) {
            if (length(dim(x)) == 1L) x <- as.table(rbind(x))
            if (is.null(col)) col <- flatly_continuous(nrow(x))
            dimnames(x) <- lapply(dimnames(x), \(dn) ifelse(is.na(dn), "NA", dn))
            
            space <- if (beside) c(0, 0) else 1
            barx <- barplot(x, col = col, log = gsub('x', '', log), space = space,
                            names.arg = logical(length(x)),
                            ylab = '', xlab = '',
                            beside = beside, axes = FALSE, 
                            ylim = ylim ,
                            border = rgb(.2,.2,.2,.2), ...)
            
            
            if (length(dim(x)) > 1) {
              legend('right', legend = rownames(x), fill = col, 
                     border = NA, bty = 'n', xpd = TRUE, cex = .6)
            }
            list(x.ticks = colMeans(barx), x.labels = colnames(x),
                 ylab = if (is.integer(x)) 'Counts' else 'N')
          })



setMethod('.draw', c('discrete', 'discrete'),
          function(x, y, ...){ 
            .draw(count(x, y), ...)
            })

setMethod('.draw', c('discrete', 'missing'),
          function(x, y, ...){ 
            .draw(count(x), ..., xlab = '')
          })

setMethod('.draw', c('token', 'missing'),
          function(x, y, ...){ 
            x <- if (is.numeric(x)) untoken(x) else factorize(x)
            .draw(x = x, ..., xlab = '')
          })

setMethod('draw', c('missing', 'token'),
          function(x, y, ...){ 
            y <- if (is.numeric(y)) untoken(y) else factorize(y)
            .draw( , y = y, ..., xlab = NA)
          })

setMethod('.draw', c(x = 'token', y = 'token'),
          function(x, y, ...){ 
            x <- if (is.numeric(x)) untoken(x) else factorize(x)
            y <- if (is.numeric(y)) untoken(y) else factorize(y)
            .draw(x, y, ..., xlab = '')
          })

setMethod('draw', c('missing', 'discrete'),
         function(x, y, ...){
           output <- draw(count(y), ...)
         })


setMethod('.draw', 'count',
          function(x, ...) {
            draw(as.table.distribution(x), ...)
          })


setMethod('.draw', 'humdrumR.table',
          function(x, ...) {
            class(x) <- class(x)[-1]
            .draw(x, ...)
          })

setMethod('.draw', 'probability',
          function(x, y, col = 1:nrow(x), log = '', ..., yat = NULL, beside = TRUE) {
            y.ticks <- sort(unique(c(0, auto_ticks(c(x), log = grepl('y', log), at = yat))))
            if (grepl('y', log)) y.ticks <- y.ticks[y.ticks > 0]
            if (inherits(x, 'count.frame')) x <- S3Part(x)
            names(x)[is.na(names(x))] <- 'NA'
          
            barx <- barplot(x, col = col, beside = beside, axes = FALSE, 
                            ylim = c(0, 1),
                            border = NA, ...)
            
            y.ticks <- seq(0, 1, .1)
            humaxis(2, at = y.ticks, labels = c('0.0', seq(.1, .9, .1), '1.0'))
            
            if (length(dim(x)) > 1) {
              legend(x = max(barx), y = max(y.ticks), legend = colnames(x), fill = col, 
                     border = NA, bty='n', xpd = TRUE, cex = .6)
            }
            
            list(ylab = 'Probability')
          })



setMethod('.draw', c('discrete', 'numeric'),
          function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
            .draw(list(1, factor(x)), y, col = col, log = log, breaks = breaks, ..., yat = yat)
            list(xlab = NULL, ylab = NULL)
          })

setMethod('.draw', c('list', 'numeric'),
          function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
            
            layout <- prep_layout(x)
            oldpar <- par(oma = par('mar'), mar = c(0, 0, 0, 0))
            on.exit({
              layout(cbind(1)) 
              par(oldpar)
              
            })
            
            y.ticks <- auto_ticks(y, log = grepl('y', log), at = yat)
            ylim <- range(y.ticks)
            y <- split(y, f = x)
            
            x.ticks <- seq(0, 1, .1)
            x.labels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
            
            xuniq <- unique(as.data.frame(x))
            xuniq <- xuniq[sapply(xuniq, \(val) length(unique(val)) > 1L)]
            grouplabels <- do.call('paste', xuniq)
            for (k in c(layout)) {
              ytick <- if (k %in% layout[, 1]) y.ticks 
              if (k %in% layout[nrow(layout), ]) {
                xtick <- x.ticks 
                xlabel <- x.labels
              } else {
                xtick <- xlabel <- NULL
              }
              
              canvas(log = gsub('x', '', log), 
                     xlim = c(0, 1), xat = xtick, x.labels = xlabel,
                     ylim = ylim, yat = ytick)
              
              if (length(layout) > 1L) text(0.2, ylim[1] + (diff(ylim) * .75), grouplabels[k])
              draw_violin(y[[k]], breaks = breaks)
            }
            
            
            list(oma = TRUE, xlab = if (length(layout) == 1L) 'Proportion' else "", ylab = "")
          })


setMethod('.draw', c('formula'),
          function(x, y, col = 2, xlab = NULL, ylab = NULL, data = NULL, ...) {
            
            vars <- model.frame(x, data = data)
            
            if (ncol(vars) == 1L) {
              .draw(vars[[1]], col = col, ..., xlab = xlab %||% names(vars), ylab = ylab)
            } else {
              
              if (ncol(vars) > 2) {
                
              }
              
              .draw(vars[[2]], vars[[1]], col = col, ...,
                   xlab = xlab %||% names(vars)[2],
                   ylab = ylab %||% names(vars)[1])
            } 
            
            
            list(xlab = '')
            
          })

setMethod('.draw', c('humdrumR'),
          function(x, facet = NULL, ...) {
            selected <- pullSelectedField(x, null = 'asis')
            fields <- fields(x)
            groupFields <- if (length(facet)) {
              fieldMatch(x, unlist(facet), callfun = 'draw')
            } else {
              fields[GroupedBy == TRUE]$Name 
            }
            if (length(groupFields)) {
              facet <- pullFields(x, groupFields)
            }
            .draw(selected, facet = facet, ...)
            
          })

## draw()'s helpers ----



smartjitter <- function(x) {
  .x <- x[!is.na(x)]
  
  ord <- order(.x)
  sorted <- .x[ord]
  
  range <- if (length(unique(sorted)) == 1) 1 else diff(range(sorted))
  diff <- c(range, diff(sorted))
  
  close <- diff == 0 
  if (!any(close)) return(x)
  smallest <- min(diff[!close], range / 10)
  
  shift <- (rbeta(sum(close), 3, 3) - .5) * smallest * .5
  
  
  sorted[close] <- sorted[close] + shift
  
  .x <- sorted[match(seq_along(sorted), ord)] # back to original order
  
  x[!is.na(x)] <- .x
  x
}


prep_cex <- function(x) {
  l <- length(x)
  
  pmax(1 - log(l, 1000000), .1 )
}

prep_col <- function(col, alpha = 1) {
  if ('prepped' %in% class(col)) return(col)
  
  if (is.numeric(col)) {
    if ( length(unique(col)) > 10) {
      col <- col - min(col, na.rm = TRUE)
      col <- col / max(col, na.rm = TRUE)
      
      uniqcols <- sort(unique(col))
      
      col <- flatly_continuous(col, alpha = alpha)
    } else {
      col <- match(col, unique(col))
    }
  } else {
    if (is.logical(col)) {
      col <- ifelse(col, 2, 5)
      attr(col, 'levels') <- c('TRUE', 'FALSE')
    }
    
    if (is.character(col) && !any(isColor(col))) {
      col <- factor(col)
    }
    
    if (is.factor(col)) {
      levels <- levels(col)
      col <- as.integer(col)
      attr(col, 'levels') <- levels
      
    } 
  }
  
  
  col %class% 'prepped'
  
}

prep_layout <- function(facets) {
  
  if (length(facets) > 2) {
    facets[[2]] <- squashGroupby(facets[-1])
    facets <- facets[1:2]
  }
  facets <- unique(as.data.frame(facets))
  
  
  mat <- matrix(1:nrow(facets), nrow = length(unique(facets[[1]])))
  
  layout(mat)
  
  mat
}



setalpha <- function(col, alpha = 1) {
  rgba <- col2rgb(col, alpha = TRUE) / 255
  
  rgb(rgba['red', ], rgba['green', ], rgba['blue', ], alpha)
}



logcheck <- function(log, x = '', y = '') {
  badx <- grepl('x', log, fixed = TRUE) && any(x <= 0)
  bady <- grepl('y', log, fixed = TRUE) && any(y <= 0)
  
  if (badx || bady) {
    bad <- .paste(if (badx) 'x', if (bady) 'y', sep = ' and ')
    .stop("You've specified draw(..., log = '{log}') but your {bad} numbers include zero or negative numbers.",
          "These can't be drawn on a log scale.")
    
  }
  
}

humaxes <- function(axesframe) {
  do.call('Map', c(list(humaxis), axesframe))
}

humaxis <- function(side, ticks, line = 0, lab = 0, cex = par('cex.axis')) {
  # this function attempts to draw axis labels that always fit on the screen
  # but never overlap
  las <- 1
  sides <- side %% 2 == 0L
  
  labels <- names(ticks) %||% ticks
  
  dist <- local({
    if (par('ylog') && sides || (par('xlog') && !sides)) {
      ticks <- ticks
      10 ^ pmin(c(Inf, diff(ticks)), 
                c(diff(ticks), Inf))
    } else {
      pmin(c(Inf, diff(ticks)), 
           c(diff(ticks), Inf))
    } })
  if (!sides) {
    widths <- strwidth(labels, cex = cex)
    toowide <- widths > dist
    if (any(toowide))las <- (las + 2) %% 4
  } else {
    toowide <- FALSE
  }

  if (sides || any(toowide)) {
    heights <- strheight(labels, cex = cex)
    tootall <- heights*1.5 > dist
    if (any(tootall) && cex > .3) return(Recall(side, ticks, line, cex = cex * .9))
  }

  
  axis(side, ticks, labels, line = line, las = las, tick = FALSE, cex.axis = cex, gap.axis = .1)
  
  
 
  
  
}

canvas <- function(x, xlim = NULL, y, ylim = NULL, log = '') {
  logcheck(log, x, y)

  xlim <- xlim %||% range(x) 
  ylim <- ylim %||% range(y) 
  
  if (grepl('x', log, fixed = TRUE) && xlim[1] <= 0) xlim[1] <- min(x) / 2
  if (grepl('y', log, fixed = TRUE) && ylim[1] <= 0) ylim[1] <- min(y) / 2
  
  plot.new()
  plot.window(xlim = xlim, ylim = ylim, log = log)
  
  axes <- data.table(side = 1:2,
                     ticks = list(axTicks(1, log = grepl('x', log)),
                                  axTicks(2, log = grepl('y', log))),
                     line = 0L)
  
  window <- data.table(Screen = as.integer(screen()),
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  list(window = window, axes = axes, axisNames = vector('list', 4))
}

### draw_x ----


draw_facets <- function(facets, ..., xlim = NULL, ylim = NULL, x.ticks = NULL, xat = NULL, yat = NULL, log = '') {
  layout <- prep_layout(facets)
  on.exit(layout(1))
  # 
  # auto_ticks()
  args <- list(...)
  if (is.null(args$x)) args$x <- NULL
  if (is.null(args$y)) args$y <- NULL
  
  args$xlim <- xlim %||% if (!is.null(x.ticks)) range(x.ticks)
  
  x.ticks <- if (is.numeric(args$x)) auto_ticks(args$x, log = grepl('x', log), at = xat)
  y.ticks <- if (is.numeric(args$y)) auto_ticks(args$y, log = grepl('y', log), at = yat)
  args$ylim <- ylim %||% if (!is.null(y.ticks)) range(y.ticks)
  args$xlim <- args$xlim %||% if (!is.null(x.ticks)) range(x.ticks)
  
  
  args$xat <- args$yat <- NA
  
  facetLabels <- unique(as.data.frame(facets))
  facetLabels <- facetLabels[sapply(facetLabels, \(val) length(unique(val)) > 1L)]
  facetLabels <- paste(colnames(facetLabels), do.call('paste', facetLabels))
  
  facets <- squashGroupby(facets)
  args <- lapply(args, \(x) if (length(x) == length(facets)) split(x, f = facets) else rep(list(x), length(layout)))
  args <- lapply(1:length(layout), \(i) lapply(args, '[[', i = i))
  

  # y.ticks <- auto_ticks(y, 
  # ylim <- range(y.ticks)
  # y <- split(y, f = x)
  # 
  # x.ticks <- seq(0, 1, .1)
  # x.labels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
  # 

  
  for (k in c(layout)) {
   
    # if (k %in% layout[nrow(layout), ]) {
    # xtick <- x.ticks
    # xlabel <- x.labels
    # } else {
    # xtick <- xlabel <- NULL
    # }
    
    # canvas(log = gsub('x', '', log), 
    #        xlim = c(0, 1), xat = xtick, x.labels = xlabel,
    #        ylim = ylim, yat = ytick)
    do.call('draw', args[[k]])
    if (k %in% layout[, 1]) {
      if (!is.null(y.ticks)) humaxis(2, at = y.ticks)
    }
    if (k %in% layout[nrow(layout), ]) {
      if (!is.null(x.ticks)) humaxis(1, at = x.ticks)
    }
    
    if (length(layout) > 1L) mtext(facetLabels[k], 3, line = -1)
    # draw_violin(y[[k]], breaks = breaks)
  }
  layout(1)
  plot.window(c(0, 1), c(0, 1))
  par(oma = c(0,0,0,0))
  # if (nrow(layout) > 1) {
  #   abline(h = head(seq(0, 1, length.out = nrow(layout) + 1)[-1], -1),
  #          lty = 'dashed', col = setalpha(flatly[5], .3))
  # }
  # if (ncol(layout) > 1) {
  #   abline(v = head(seq(0, 1, length.out = ncol(layout) + 1)[-1], -1),
  #          lty = 'dashed', col = setalpha(flatly[5], .3))
  # }
  # list(oma = TRUE, xlab = if (length(layout) == 1L) 'Proportion' else "", ylab = "")
}

draw_quantiles <- function(side, var, quantiles = c(.025, .25, .5, .75, .975), ...) {
  
  if (length(quantiles)) {
    checks(quantiles, xnumeric & xrange(0, 1))
    
    sides <- side %% 2 == 0
    quants <- quantile(var, prob = quantiles)
    
    
    lineArgs <- list(quants, lty = 'dashed', lwd = .3, col = rgb(0, 0, 0, .5))
    names(lineArgs)[1] <- if (sides) 'h' else 'v'
    do.call('abline', lineArgs)
    
    annotes <- lapply(quantiles * 100, 
                      \(q) {
                        arrow <- switch(paste0(q > 50, sides),
                                        TRUETRUE  = quote(`%up%`),
                                        TRUEFALSE = quote(`%->%`),
                                        FALSETRUE = quote(`%down%`),
                                        FALSEFALSE = quote(`%<-%`))
                        
                        if (q > 50) q <- 100 - q
                        q <- paste0(round(q, 1), '%')
                        
                        bquote(.(arrow)(.(q), ''))
                          
                        })
    
    axis(side, quants, labels = as.expression(annotes), cex.axis = .4, xpd = TRUE, line = -.5, tick = FALSE, las = 1)
  }

}

draw_violin <- function(var, breaks = 'Sturges', col = 1, ...) {
  var <- var[!is.na(var)]
  breaks <- hist.default(var, breaks = breaks, plot = FALSE)
  
  
  prob <- breaks$density
  prob <- .5 * prob / sum(prob)
  
  Map(head(breaks$breaks, -1), tail(breaks$breaks, -1), prob, 
      f = \(y0, y1, d) {
        polygon(x = .5 + c(-d, -d , d, d),
                y = c(y0, y1, y1, y0),
                border = NA, col = setalpha(col, .8), ...)
      } 
  )
  
  p <- prob[findInterval(var, breaks$breaks, rightmost.closed = TRUE)]
  othercoor <- runif(length(var), .5 - p, .5 + p)
  points(x = othercoor, y = smartjitter(var),  cex = .25, col = rgb(1,0,0, .1), pch = 16)
  
  list(xlab = 'Proportion')
}


# Notation viewer ----

toHNP <- function(lines, message) {
  output <- paste(lines, collapse = '\n')
  
  randomID <- paste0(sample(letters, 100, replace = TRUE), collapse = '')
  message <- gsub("PLUGIN", '<a href="https://plugin.humdrum.org/">humdrum notation plugin</a>', message)
  
  html <- .glue(.open = '[[', .close = ']]',
  '<!DOCTYPE html>
    <html lang="en">
    <head>
    <script src="https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"></script>
    <script>displayHumdrum({source: "[[randomID]]", autoResize: "true"});</script>
    </head>
    <body>
    <h1>HumdrumR viewer</h1>
    <p>[[message]]</p>
    <script id="[[randomID]]" type="text/x-humdrum">[[output]]</script>
    </body>
    </html>')
  
  
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, 'index.html')
  
  writeLines(strsplit(html, split = '\n')[[1]],  htmlFile)
  
  getOption('viewer', default = utils::browseURL)(htmlFile)
  
}

#' @export
viewKernTable <- function(table) {
  df <- as.data.frame(table)
  df <- df[order(df[[length(df)]], decreasing = TRUE), ]
  
  df <- subset(df, df[[length(df)]] > 0)
  
  
  
  kern <- lapply(as.list(df[1:(ncol(df) -1)]), as.character)
  # if (length(kern) > 1) {
    # kern[[1]] <- paste0('(', kern[[1]])
    # kern[[length(kern)]] <- paste0(kern[[length(kern)]], ')')
  # }
  N    <- num2str(df[[length(df)]])
  
  kernspine <- do.call('rbind', c(kern, list('=||')))
  kernspine <- c('**kern', kernspine, '*-')
  
  Nspine <- c(do.call('rbind', c(list(N), 
                                 replicate(length(kern) - 1, list('.'), simplify = T), 
                                 list('=||'))))
  Nspine <- c('**cdata', Nspine, '*-')
  
  lines <- paste(kernspine, Nspine, sep = '\t')
  
  toHNP(lines, "Tabulating kern data and viewing using the PLUGIN.")
}

# pattern finding ----


findrep <- function(x, func = `==`) {
  x <- outer(x, x, func)
  
  
  x
  
}

getDiagonals <- function(mat, upper = TRUE, min.n = 4, max.lag = 100) {
  grid <- as.data.table(expand.grid(Row = seq_len(nrow(mat)), Col = seq_len(ncol(mat))))
  
  grid[ , Lag := Col - Row]
  setorder(grid, Lag)
  if (upper) grid <- grid[Lag > 0]
  
  grid <- grid[(nrow(mat) - Lag) >= min.n & Lag <= max.lag]
  
  grid[, list(Sequence = list(rle(mat[cbind(Row, Col)]))), by = Lag]
}

findstretches <- function(rle, lag , min.n = 4) {
  
  rle$values[rle$lengths < min.n] <- FALSE
  hits <- cumsum(c(1, head(rle$lengths, n = -1L)))[rle$values]
  cbind(Antecedent = hits, Consequent = hits + lag, Length = rle$lengths[rle$values])
}

findrepeats <- function(x, min.n = 4, max.lag = 400, func = `==`) {
  findrep(x, func = func) |> getDiagonals(min.n = min.n, max.lag = max.lag) -> sequences
  
  sequences[ , Hits := Map(\(s, l) findstretches(s, l, min.n = min.n), Sequence, Lag)]
  sequences[lengths(Hits) > 1L, Hits] |> do.call(what = 'rbind') |> as.data.table() -> sequences
  if (nrow(sequences) == 0) return(data.table(Antecedent = integer(0), Consequent = integer(0)))
  setorder(sequences, Antecedent)
  sequences[ , Lag := Consequent - Antecedent]
  sequences[]
  
}

# ggplot2 ----



#' @rdname withinHumdrum
#' @export
ggplot.humdrumR <- function(data = NULL, mapping = aes(), ..., dataTypes = 'D') {
  humtab <- getHumtab(data, dataTypes = dataTypes)
  
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



### humdrumR plot style ----

#### Colors ----

scale_color_humdrum <- ggplot2::scale_fill_manual(values = flatly)
# scale_color_continuous(type = colorRamp(flatly[2:3]))

options(ggplot2.continuous.fill = ggplot2::scale_color_gradientn(colors = flatly_continuous(100)))
options(ggplot2.continuous.color = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))
options(ggplot2.continuous.colour = ggplot2::scale_color_gradientn(colours = flatly_continuous(100)))

# options(ggplot2.continuous.colour = 'humdrum')

#### Theme ----


theme_humdrum <- function() {
  ggplot2::update_geom_defaults("point", list(size = .5, color = flatly[1], fill = flatly[2]))
  ggplot2::update_geom_defaults("line", list(size = .5, color = flatly[4], fill = flatly[3]))
  ggplot2::update_geom_defaults("rect", list(fill = flatly[1]))
  
  theme(panel.background = element_blank(), axis.ticks = element_blank(),
        strip.background = element_blank(), 
        # panel.border = element_rect(linetype = 'dashed', fill = NA),
        legend.key = element_rect(fill = NA),
        title = element_text(family = 'Helvetica', color = flatly[5], size = 16),
        plot.title.position = 'plot', plot.title = element_text(hjust = .5),
        line = element_line(color = flatly[1]),
        rect = element_rect(color = flatly[2]),
        text = element_text(family = 'Helvetica', color = flatly[4]),
        axis.text = element_text(color = flatly[5], size = 7),
        axis.title = element_text(color = flatly[4], size = 11)
        )
}


  


 

          
