
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
setGeneric('draw', \(x, y, 
                     col = 2, facet = list(), 
                     main = '', sub = '',
                     xlab = NULL, ylab = NULL, ...) {
  oldpalette <- palette(flatly)
  oldpar <- par(family = 'Helvetica', 
                col = 4, col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2,
                cex.axis = .7, pch = 16)
  
  on.exit({par(oldpar) ; palette(oldpalette)})
  
  # xlab and ylab
  xexpr <- deparse1(substitute(x)) 
  yexpr <- deparse1(substitute(y)) 
  if (xexpr == 'missing') xexpr <- 'x'
  if (yexpr == 'missing') yexpr <- 'y'
  
  col <- prep_col(col)
  if (length(facet)) {
    if (!is.list(facet)) facet <- list(facet)
    par(mar = c(1, 1, 1, 1), oma = c(5, 5, 5, 5))
    draw_facets(facet, x = if (!missing(x)) x, y = if (!missing(y)) y, col = col, xlab = '', ylab = '', ...)
  } else {
    output <- standardGeneric('draw')
    # plot.window(c(0,1), c(0, 1))
    title(main = main, sub = sub)
    
    outer <- output$outer %||% FALSE
    xlab <- xlab %||% (output$xlab %||% xexpr)
    ylab <- ylab %||% (output$ylab %||% yexpr)
    if (sys.nframe() < 2)  mtext(xlab, 1, line = 2.5, outer = outer)
   
    mtext(ylab, 2, line = 3, outer = outer, las = if (nchar(ylab) > 3) 3 else 1)
    
    if (!is.null(attr(col, 'levels'))) legend('topleft', horiz = TRUE, xpd = TRUE, pch = 16, cex = .8, bty = 'n',
                                              col = sort(unique(col)), legend = attr(col, 'levels'))
  }
 
})

#' @rdname draw
#' @export
setMethod('draw', c('numeric', 'numeric'), 
          \(x, y, col = 3, log = '', jitter = 'xy', 
            xlim = NULL, ylim = NULL, xat = NULL, yat = NULL, cex = prep_cex(x),  ...) {
            
            
            if (grepl('x', jitter)) x <- smartjitter(x)
            if (grepl('y', jitter)) y <- smartjitter(y)
            
            xat <- prep_ticks(xlim %||% x, log = grepl('x', log), at = xat)
            yat <- prep_ticks(ylim %||% y, log = grepl('y', log), at = yat)
            
            canvas(log = log, 
                   xlim = xlim %||% range(x),
                   ylim = ylim %||% range(y),
                   xat = xat, yat = yat, ...)
            points(x, y, col = col, cex = cex, ...)
            
          })

#' @rdname draw
#' @export
setMethod('draw', c('numeric', 'missing'), 
          \(x, y, col = 3, breaks = 'Sturges', jitter = '', ..., cex = prep_cex(x) * .75, xlim = NULL, ylim = NULL) {
            
            breaks <- hist.default(x, breaks = breaks, plot = FALSE)
            
            xat <- breaks$breaks
            while(length(xat) > 20) {xat <- xat[seq(1, length(xat), by = 2)]}
            
            ylim <- ylim %||%  c(0, 1)
            yat <- pretty(ylim)
            canvas(log = '', 
                   xlim = xlim %||% range(breaks$breaks), 
                   ylim = ylim, xat = xat,
                   yat = yat, ylabels = format(paste(yat * 100, '%')))
            
            
            countaxis <- unique(round(pretty(c(0, sum(breaks$counts) * max(ylim)), n = 10L, min.n = 5L)))
            # humaxis(4, at = countaxis / sum(breaks$counts), labels = num2str(countaxis))
            # mtext('Counts', 4, las = 3, line = 2)
            
            prob <- breaks$density
            prob <- prob / sum(prob)
            Map(head(breaks$breaks, -1), tail(breaks$breaks, -1), prob,
                f = \(x0, x1, p) {
                  polygon(c(x0, x0, x1, x1), c(0, p, p, 0), col = setalpha(col, .2), border = NA)
                  
                  graphics::segments(x0, p, x1, p, col = col)
                })
            
            graphics::segments(breaks$breaks, 0, breaks$breaks, pmax(c(prob, 0), c(0, prob)), 
                               col = setalpha(col, .4))
            
            
            if (length(x) > 1e5) x <- sample(x, 1e5)
            if (grepl('x', jitter)) x <- smartjitter(x)
            # points(x, rnorm(length(x), mean(ylim), diff(range(ylim)) / 20), 
                   # cex = cex , col = rgb(1,0,0, .1), pch = 16, xpd = TRUE)
            
            
            list(ylab = 'Proportion')
          })

#' @rdname draw
#' @export
setMethod('draw', c('missing', 'numeric'),
          function(x, y, col = 3, log = '', jitter = '', ..., cex = prep_cex(y), yat = NULL, quantiles = c(.025, .25, .5, .75, .975)) {
            
            
            yat <- prep_ticks(y, log = grepl('y', log), at = yat)
            
            canvas(log = gsub('x', '', log), xlim = c(0, 1), ylim = range(yat), 
                   xat = seq(0, 1, .1), xlabels = c(('0.0'), seq(.1, .9, .1), '1.0'),
                   yat = yat)
            
            
            
            draw_quantile(y, ymin = min(yat), jitter = grepl('y', jitter), quantiles = quantiles,
                          ..., col = col, cex = cex)
            
            list(xlab = 'Quantile')
          })


#' @rdname draw
#' @export
setMethod('draw', c('discrete', 'discrete'),
          function(x, y, ...){ 
            draw(count(x, y), ...)
            })

#' @rdname draw
#' @export
setMethod('draw', c('discrete', 'missing'),
          function(x, y, ...){ 
            draw(count(x), ..., xlab = '')
          })

#' @rdname draw
#' @export
setMethod('draw', c('token', 'missing'),
          function(x, y, ...){ 
            x <- if (is.numeric(x)) untoken(x) else factorize(x)
            draw(x = x, ..., xlab = '')
          })

#' @rdname draw
#' @export
setMethod('draw', c('missing', 'token'),
          function(x, y, ...){ 
            y <- if (is.numeric(y)) untoken(y) else factorize(y)
            draw( , y = y, ..., xlab = NA)
          })

#' @rdname draw
#' @export
setMethod('draw', c(x = 'token', y = 'token'),
          function(x, y, ...){ 
            x <- if (is.numeric(x)) untoken(x) else factorize(x)
            y <- if (is.numeric(y)) untoken(y) else factorize(y)
            draw(x, y, ..., xlab = '')
          })

# #' @rdname draw
# #' @export
#setMethod('draw', c('missing', 'discrete'),
#          function(x, y, ...){ 
#            output <- draw(count(y), ...)
#          }) ################ THis can work except the labels are reversed...need to figure that your


#' @rdname draw
#' @export
setMethod('draw', 'count',
          function(x, ...) {
            draw(as.table.distribution(x), ...)
          })

#' @rdname draw
#' @export
setMethod('draw', 'table',
          function(x, y, col = 1:nrow(x), log = '', ..., ylim = NULL, yat = NULL, beside = TRUE) {
            yticks <- sort(unique(prep_ticks(ylim %||% c(0, x), log = grepl('y', log), at = yat)))
            if (grepl('y', log)) yticks <- yticks[yticks > 0]
            if (inherits(x, 'count.frame')) x <- S3Part(x)
            names(x)[is.na(names(x))] <- 'NA'
            
            barx <- barplot(x, col = col, log = gsub('x', '', log), beside = beside, axes = FALSE, 
                            ylim = ylim %||% range(yticks),
                            border = NA, ...)
            
            humaxis(2, at = yticks)
            
            if (length(dim(x)) > 1) {
              legend(x = max(barx), y = max(yticks), legend = rownames(x), fill = col, 
                     border = NA, bty='n', xpd = TRUE, cex = .6)
            }
            
            list(ylab = if (is.integer(x)) 'Counts' else 'N')
          })


#' @rdname draw
#' @export
setMethod('draw', 'humdrumR.table',
          function(x, ...) {
            class(x) <- class(x)[-1]
            draw(x, ...)
          })

#' @rdname draw
#' @export
setMethod('draw', 'probability',
          function(x, y, col = 1:nrow(x), log = '', ..., yat = NULL, beside = TRUE) {
            yticks <- sort(unique(c(0, prep_ticks(c(x), log = grepl('y', log), at = yat))))
            if (grepl('y', log)) yticks <- yticks[yticks > 0]
            if (inherits(x, 'count.frame')) x <- S3Part(x)
            names(x)[is.na(names(x))] <- 'NA'
          
            barx <- barplot(x, col = col, beside = beside, axes = FALSE, 
                            ylim = c(0, 1),
                            border = NA, ...)
            
            yticks <- seq(0, 1, .1)
            humaxis(2, at = yticks, labels = c('0.0', seq(.1, .9, .1), '1.0'))
            
            if (length(dim(x)) > 1) {
              legend(x = max(barx), y = max(yticks), legend = colnames(x), fill = col, 
                     border = NA, bty='n', xpd = TRUE, cex = .6)
            }
            
            list(ylab = 'Probability')
          })



#' @rdname draw
#' @export
setMethod('draw', c('discrete', 'numeric'),
          function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
            draw(list(1, factor(x)), y, col = col, log = log, breaks = breaks, ..., yat = yat)
            list(xlab = NULL, ylab = NULL)
          })

#' @rdname draw
#' @export
setMethod('draw', c('list', 'numeric'),
          function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
            
            layout <- prep_layout(x)
            oldpar <- par(oma = par('mar'), mar = c(0, 0, 0, 0))
            on.exit({
              layout(cbind(1)) 
              par(oldpar)
              
            })
            
            yticks <- prep_ticks(y, log = grepl('y', log), at = yat)
            ylim <- range(yticks)
            y <- split(y, f = x)
            
            xticks <- seq(0, 1, .1)
            xlabels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
            
            xuniq <- unique(as.data.frame(x))
            xuniq <- xuniq[sapply(xuniq, \(val) length(unique(val)) > 1L)]
            grouplabels <- do.call('paste', xuniq)
            for (k in c(layout)) {
              ytick <- if (k %in% layout[, 1]) yticks 
              if (k %in% layout[nrow(layout), ]) {
                xtick <- xticks 
                xlabel <- xlabels
              } else {
                xtick <- xlabel <- NULL
              }
              
              canvas(log = gsub('x', '', log), 
                     xlim = c(0, 1), xat = xtick, xlabels = xlabel,
                     ylim = ylim, yat = ytick)
              
              if (length(layout) > 1L) text(0.2, ylim[1] + (diff(ylim) * .75), grouplabels[k])
              draw_violin(y[[k]], breaks = breaks)
            }
            
            
            list(oma = TRUE, xlab = if (length(layout) == 1L) 'Proportion' else "", ylab = "")
          })


#' @rdname draw
#' @export
setMethod('draw', c('formula'),
          function(x, y, col = 2, xlab = NULL, ylab = NULL, data = NULL, ...) {
            
            vars <- model.frame(x, data = data)
            
            if (ncol(vars) == 1L) {
              draw(vars[[1]], col = col, ..., xlab = xlab %||% names(vars), ylab = ylab)
            } else {
              
              if (ncol(vars) > 2) {
                
              }
              
              draw(vars[[2]], vars[[1]], col = col, ...,
                   xlab = xlab %||% names(vars)[2],
                   ylab = ylab %||% names(vars)[1])
            } 
            
            
            list(xlab = '')
            
          })

#' @rdname draw
#' @export
setMethod('draw', c('humdrumR'),
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
            draw(selected, facet = facet, ...)
            
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
      col <- flatlyramp(col, alpha = alpha)
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

prep_ticks <- function(x, log = TRUE, at = NULL) {
  if (any(is.na(at))) return(NULL)
  if (is.null(x)) x <- seq(0, 1, .1)
  
  if (log && is.null(at)) {
    if (any(x <= 0)) .stop("You can't draw a variable on a logarithmic scale",
                           "if it includes negative numbers or zeros.")
    
    ticks <- pretty(log10(x), n = 10L, min.n = 5L) 
    labels <- 10^ticks
    scale <- floor(ticks)
    scale <- ifelse(scale >= 2, scale - 1, scale)
    scale <- 10^scale
    ticks <- unique(round(labels / scale) * scale)
    
    
  } else {
    ticks <- at %||% pretty(x, n = 10L, min.n = 5L)
  }
}

setalpha <- function(col, alpha = 1) {
  rgba <- col2rgb(col, alpha = TRUE) / 255
  
  rgb(rgba['red', ], rgba['green', ], rgba['blue', ], alpha)
}


humaxis <- function(side, tick = FALSE, las = 1, ...) axis(side, tick = FALSE, las = 1, ...)

canvas <- function(log = '', xlim = NULL, ylim = NULL, xat = NULL, yat = NULL,
                   xlabels = num2str(xat), ylabels = num2str(yat),
                   ...) {
  plot.new()
  plot.window(xlim = xlim %||% (xat %||% c(0, 1)), 
              ylim = ylim %||% (yat %||% c(0, 1)), log = log)
  
  if (!is.null(xat)) humaxis(1, at = xat, labels = xlabels, line = -1)
  if (!is.null(yat)) humaxis(2, at = yat, labels = ylabels)
}

### draw_x ----


draw_facets <- function(facets, ..., xlim = NULL, ylim = NULL, xticks = NULL, xat = NULL, yat = NULL, log = '') {
  layout <- prep_layout(facets)
  on.exit(layout(1))
  # 
  # prep_ticks()
  args <- list(...)
  if (is.null(args$x)) args$x <- NULL
  if (is.null(args$y)) args$y <- NULL
  
  args$xlim <- xlim %||% if (!is.null(xticks)) range(xticks)
  
  xticks <- if (is.numeric(args$x)) prep_ticks(args$x, log = grepl('x', log), at = xat)
  yticks <- if (is.numeric(args$y)) prep_ticks(args$y, log = grepl('y', log), at = yat)
  args$ylim <- ylim %||% if (!is.null(yticks)) range(yticks)
  args$xlim <- args$xlim %||% if (!is.null(xticks)) range(xticks)
  
  
  args$xat <- args$yat <- NA
  
  facetLabels <- unique(as.data.frame(facets))
  facetLabels <- facetLabels[sapply(facetLabels, \(val) length(unique(val)) > 1L)]
  facetLabels <- paste(colnames(facetLabels), do.call('paste', facetLabels))
  
  facets <- squashGroupby(facets)
  args <- lapply(args, \(x) if (length(x) == length(facets)) split(x, f = facets) else rep(list(x), length(layout)))
  args <- lapply(1:length(layout), \(i) lapply(args, '[[', i = i))
  

  # yticks <- prep_ticks(y, 
  # ylim <- range(yticks)
  # y <- split(y, f = x)
  # 
  # xticks <- seq(0, 1, .1)
  # xlabels <- c(seq(1,.2,-.2), '0.0', seq(.2, 1, .2))
  # 

  
  for (k in c(layout)) {
   
    # if (k %in% layout[nrow(layout), ]) {
    # xtick <- xticks
    # xlabel <- xlabels
    # } else {
    # xtick <- xlabel <- NULL
    # }
    
    # canvas(log = gsub('x', '', log), 
    #        xlim = c(0, 1), xat = xtick, xlabels = xlabel,
    #        ylim = ylim, yat = ytick)
    do.call('draw', args[[k]])
    if (k %in% layout[, 1]) {
      if (!is.null(yticks)) humaxis(2, at = yticks)
    }
    if (k %in% layout[nrow(layout), ]) {
      if (!is.null(xticks)) humaxis(1, at = xticks)
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

draw_quantile <- function(var, ymin, col = 1, jitter = FALSE,
                          quantiles = c(.025, .25, .5, .75, .975), na.rm = FALSE, ...) {
  if (length(col) == length(var)) col <- col[order(var)]
  
  coor <- sort(var)
  
  if (jitter) coor <- smartjitter(coor)
  othercoor <- seq(0, 1, length.out = length(coor))
  
  
  quants <- quantile(coor, prob = quantiles)
  
  mean <- mean(var)
  polygon(x = c(0, 0, 1, 1), y = c(ymin,  mean, mean, ymin), col = setalpha(col, alpha = .2), border = NA)
  
  graphics::segments(x0 = 0, y0 = quants, x1 = quantiles, y1 = quants, lty = 'dashed', lwd = .5)
  
  annotes <- lapply(quantiles * 100, 
                    \(q) {
                      if (q > 50) {
                        q <- 100 - q
                        bquote({frac(.(q), 100)} %up% "")
                      } else {
                        bquote({frac(.(q), 100)} %down% "" )
                        
                      }})
                          
                          
  text(x = 0.02, y = quants, labels = as.expression(annotes), #paste0(quantiles*100, '%'), 
       cex = .4, pos = 2, xpd = TRUE)
  points(x = othercoor, y = coor, col = col, ...)
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


  


 

          
