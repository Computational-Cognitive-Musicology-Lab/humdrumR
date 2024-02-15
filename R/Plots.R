



# draw() ----


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
draw <- function(x, y, facets = list(), ..., 
                 xlab = NULL, ylab = NULL, 
                 axes = 1:4, legend = TRUE,
                 main = '', sub = '') {
  
  
  # this sets default par(...) values for for draw(), but these defaults can be overrode by ...
  oldpar <- par(family = 'Helvetica', col = 1, pch = 16,  col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2, 
                cex.axis =.7, mar = c(5, 5, 5, 5))
  do.call('par', list(...)[pmatch(names(list(...)), names(par()), nomatch = 0L) > 0]) 
  oldpalette <- palette(flatly)
  on.exit({par(oldpar) ; palette(oldpalette)})
  
  
  checks(legend, xTF)
  checks(axes, xwholenum & xmaxlength(4L) & xmax(4) & xmin(1))
  checks(main, xatomic & xlen1)
  checks(sub, xatomic & xlen1)
  

  # xlab and ylab
  xexpr <- deparse1(substitute(x)) 
  yexpr <- deparse1(substitute(y)) 
  if (xexpr == '') xexpr <- 'x'
  if (yexpr == '') yexpr <- 'y'
  
  # change missing to NULL
  x <- if (!missing(x)) x
  y <- if (!missing(y)) y
  
  if (rlang::is_formula(x)) {
    formula <- xy_formula(x)
    x <- formula$x
    y <- formula$y
    xlab <- xlab %||% formula$xlab
    ylab <- ylab %||% formula$ylab
  } 
   
  if (length(facets)) {
    if (!is.list(facets)) facets <- list(facets)
    par(mar = c(2, 3, 1, 1), oma = c(5, 5, 5, 5))
    return(draw_facets(x, y, facets, xlab = xlab, ylab = ylab, ..., 
                       main = main, sub = sub, 
                       axes = axes, legend = legend,
                       xexpr = xexpr, yexpr = yexpr))
  } 
  output <- .draw(x, y, ...)
  title(main = main, sub = sub)
  output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
  output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  
  humaxes(output$axes, output$axisNames, axes)
  
 
  if (legend) {
    if (!is.null(output$col$legend)) output$col$legend()
    if (!is.null(output$cex$legend)) output$cex$legend()
  } 
  
  return(invisible(output))
}
  
## the .draw() function ----

setGeneric('.draw', def =  \(x, y,  ...) standardGeneric('.draw'))


### draw() numeric ----

setMethod('.draw', c('numeric', 'numeric'), 
          \(x, y, log = '', jitter = '', 
            quantiles = c(), lm = FALSE,
            xlim = NULL, ylim = NULL, 
            col = 1, alpha = .5, cex = NULL, ...) {
            
            if (length(x) != 1L && length(x) != length(y) && length(y) != 1L) {
              .stop("You can't draw two numeric vectors if they are different lengths.",
                    "In your call, length(x) = {length(x)} and length(y) = {length(y)}.")
            }
            
            match_size(x = x, y = y, toEnv = TRUE)
            
            output <- canvas(x = x, xlim = xlim, 
                             y = y, ylim = ylim,
                             log = log)
            
            output$col <- prep_col(col, y, alpha = alpha, ...)
            output$cex <- prep_cex(x, y, cex = cex, col = output$col$col, ...)
            draw_quantiles(1, x, quantiles)
            draw_quantiles(2, y, quantiles)
            
            if (grepl('x', jitter)) x <- smartjitter(x)
            if (grepl('y', jitter)) y <- smartjitter(y)
            points(x, y, col = output$col$col, cex = output$cex$cex, ...)
            
            if (lm) {
              fit <- stats::lm(y ~ x)
              
              xseq <-  seq(output$window$xlim[[1]][1], 
                           output$window$xlim[[1]][2], length.out = 300L)
              conf <- predict(fit,  newdata = data.frame(x = xseq), interval = 'confidence', ...)
              
              points(xseq, conf[ , 1], type = 'l', lwd = .5, col = 'black')
              points(xseq, conf[ , 2], type = 'l', lwd = .5, lty = 'dashed', col = 'black')
              points(xseq, conf[ , 3], type = 'l', lwd = .5, lty = 'dashed', col = 'black')
              
              legend('topleft', bty = 'n', lwd = .5, col = 'black', text.col = 'black', cex = .8,
                     legend = bquote(list(a == .(format(coef(fit)[1], big.mark = ',', digits = 3)),
                                          b == .(format(coef(fit)[2], big.mark = ',', digits = 3)))))
            }
            

            output
            
          })

setMethod('.draw', c('numeric', 'NULL'), 
          \(x, y, log = '', jitter = '', 
            breaks = 'Sturges', quantiles = c(),
            conditional = FALSE,
            xlim = NULL, ylim = NULL,
            col = 3, alpha = .2, cex = .7, ...) {
            
            if (length(breaks) == 1L && pmatch(breaks, 'quantiles', 0) == 1 && length(quantiles)) {
              breaks <- quantile(x, sort(unique(c(0, quantiles, 1))))
              names(breaks) <- format(breaks, digits = 3)
            }  
            
              
            histogram <- hist.default(x, breaks = breaks, plot = FALSE)
            
            prob <- histogram$density
            if (histogram$equidist) prob <- prob * diff(histogram$breaks) # height is proportion (same as area)
            
            
            ylim <- ylim %||% c(0, 2^(ceiling(log( max(prob), 2)) + 1)) # 1, .5, .25, .125, etc.
            output <- canvas(x = x, xlim = xlim %||%range(histogram$breaks), 
                             y = prob[prob > 0] , ylim = ylim, 
                             log = log)
            
            draw_quantiles(1, x, quantiles)
            
            
            # actual plot of polygons
            cols <- prep_col(col, x, alpha = alpha, ...)
            ymin <- min(output$window$ylim[[1]])
            probs <- if (length(cols$col) == length(x)) {
              hists <- tapply(x, col, hist, breaks = histogram$breaks, plot = FALSE, simplify = FALSE)
              cols$col <- unique(cols$col)
              proportions <- proportions(table(col))
              
              Map(\(h, p) h$density * diff(h$breaks)[1] * if (conditional) 1 else p, hists, proportions)
              
            } else {
               list(prob)
            }
            Map(\(p, c) {
              Map(head(histogram$breaks, -1), tail(histogram$breaks, -1), p,
                  f = \(x0, x1, p) {
                    polygon(c(x0, x0, x1, x1), c(ymin, p, p, ymin), col = c, border = NA)
                    
                    graphics::segments(x0, p, x1, p, col = c)
                  })
              
              # lines between polygons:
              graphics::segments(histogram$breaks, 0, histogram$breaks, pmax(c(p, 0), c(0, p)),
                                 col = c)
            }, probs, cols$col)
          
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
        
            
            output$axisNames[c(2,4)] <- list(if (histogram$equidist) 'Proportion' else 'Density', 
                                             'Count')
            
            output$col <- cols
            output
            
          })

setMethod('.draw', c('NULL', 'numeric'),
          function(x, y, log = '', 
                   violin = FALSE, showNormal = FALSE,
                   quantiles = c(.25, .5, .75),
                   xlim = NULL, ylim = NULL, 
                   col = 1, alpha = .8, cex = NULL, pch = 16, ...) {
            
            checks(violin, xTF)
            output <- canvas(x = if (violin) c(.5, 1.5) else c(0, 1), 
                             xlim = xlim, 
                             y = y, ylim = ylim , 
                             log = gsub('x', '', log))
            
            if (violin) {
              
              output$col <- prep_col(col, 1, alpha = alpha, ...)
              draw_violins(list(y), horiz = FALSE, ..., col = output$col$col, quantiles = quantiles)
              output$axisNames[[1]] <- 'Density'
              output$axes <- output$axes[side == 2L]
              
            } else {
              
              output$col <- prep_col(col, y, ..., alpha = alpha, pch = pch)
              output$cex <- prep_cex(x, y, cex = cex, col = output$col$col, ...)
              if (length(output$col$col) == length(y)) output$col$col <- output$col$col[order(y)]
               
              draw_quantiles(2, y, quantiles = quantiles)
              y <- sort(y)
              x <- seq(0, 1, length.out = length(y))
              points(x = x, y = y, col = output$col$col, cex = output$cex$cex, ...)
              
              if (showNormal) {
                points(x, qnorm(x, mean(y), sd(y)), type = 'l', col = 'black',
                                     lwd = .5, lty = 'dashed', xpd = TRUE)
                
                legend('topleft', bty = 'n', lty = 'dashed', lwd = .5, 
                       col = 'black', text.col = 'black', cex = .8, 
                       legend = quote(N(mu[y], sigma[y])) )
              }
              
              output$axisNames[[1]] <- 'Quantile'
            }
           
            output
          })



### draw() discrete ----



setMethod('.draw', c('table', 'NULL'),
          function(x, y, log = '', 
                   beside = NULL, heat = length(dim(x) == 2L) && length(x) > 80L,
                   ylim = NULL, 
                   col = NULL,  alpha = .9, ...) { 
            if (!is.numeric(c(x))) .stop("No draw() method for a matrix/table of class '{class(x[1, 1])}.'")
            dimnames(x) <- lapply(dimnames(x), \(dn) ifelse(is.na(dn), "NA", dn))
            
            # if table is one dimensional, add col dimension
            if (length(dim(x)) == 1L)  {
              dn <- dimnames(x)
              dim(x) <- c(dim(x), 1L)
              
              dimnames(x) <- c(dn, list(''))
            }
            
            
            if (heat) return(draw_heat(x, log = log, ...))
              
            if (dim(x)[1] == 1L) x <- t(x)
            
            type <- if (is.null(beside)) 'both' else { if (beside) 'beside' else 'stacked'}
            space <- if (type == 'stacked') .5 else c(0, 1 + nrow(x) %/% 8) 
            
            ylim <- ylim %||% c(0, if (type == 'beside') max(x) else max(colSums(x)))
            
            col <- prep_col_categories(col %||% rownames(x), rownames(x), alpha = alpha, ...)
            
            barx <- barplot(x, col = if (type == 'stacked' ) rev(col$col) else col$col, log = gsub('x', '', log), space = space,
                            axisnames = FALSE,
                            ylab = '', xlab = '',
                            beside = type != 'stacked', axes = FALSE, 
                            ylim = ylim,
                            border = rgb(.2,.2,.2,.2), ...)
            
            if (type == 'both') {
              barplot(x[nrow(x):1, ], col = setalpha(rev(col$col), alpha / 4), border = rgb(.2,.2,.2, alpha / 3),
                      names.arg = logical(ncol(x)), axes = FALSE,
                      add = TRUE, beside = FALSE, space = nrow(x) + space[2] - 1)
            }
            
            legend_col_discrete(rownames(x), col$col, ..., pch = 15)
            
            # axes
            proportions <- pretty(ylim / sum(x), n = 10L, min.n = 5L)
            proportions <- setNames(proportions * sum(x), proportions)
            
            axes <- data.table(side = c(2, 4),
                               ticks = list(proportions,
                                            unique(round(axTicks(2, log = grepl('y', log, fixed = TRUE))))),
                               line = 1L)
            
            if (ncol(x) > 1) axes <- rbind(axes,
                                           data.table(side = 1,
                                                      ticks = list(setNames(if (type == 'stacked') barx else colMeans(barx), colnames(x))),
                                                      line = 1 + as.integer(type != 'stacked')))
            if (type != 'stacked' && nrow(x) > 1L && length(x) < 100) axes <- rbind(axes,
                                                              data.table(side = c(1),
                                                                         ticks = list(setNames(c(barx), rownames(x)[row(barx)])),
                                                                         line = 1))
            
            
            window <- data.table(Screen = as.integer(screen()),
                                 xlim = list(c(0, ceiling(max(barx)))), 
                                 ylim = list(ylim),
                                 log = log)
            
            
            axisNames <- vector('list', 4L)
            axisNames[c(2,4)] <- c('Proportion', if (is.integer(x)) 'Count' else 'N')
            
            list(axes = axes, window = window, axisNames = axisNames, col = col)
          })



setMethod('.draw', c('count', 'NULL'),
          function(x, y, ...) {
            .draw(as.table(x), NULL, ...)
          })


setMethod('.draw', c('discrete', 'NULL'),
          function(x, y, ...){ 
            .draw(table(x, deparse.level = 2L), NULL, ...)
            })


setMethod('.draw', c('NULL', 'discrete'),
          function(x, y, ...){ 
            .draw(table(y, deparse.level = 2L) |> t(), NULL, ...)
          })
setMethod('.draw', c('discrete', 'discrete'),
          function(x, y, ...){ 
            .draw(table(x, y, deparse.level = 2L), NULL, ...)
          })





# setMethod('.draw', c('discrete', 'numeric'),
#           function(x, y, col = 3, log = '', breaks = 'Sturges', ..., yat = NULL) {
#             .draw(list(1, factor(x)), y, col = col, log = log, breaks = breaks, ..., yat = yat)
#             list(xlab = NULL, ylab = NULL)
#           })

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


### draw() numeric ~ discrete ----


setMethod('.draw', c('discrete', 'numeric'),
          function(x, y, log = '', 
                   smooth = TRUE, conditional = FALSE,
                   mean = TRUE, quantiles = c(.25, .75), global_quantiles = FALSE, 
                   xlim = NULL, ylim = NULL, ...,
                   col = NULL) {
            
            categories <- sort(unique(x))
            if (is.integer(x) && length(categories) > 25L) {
              return(.draw(as.numeric(x), y, log = log, 
                           xlim = xlim, ylim = ylim, ..., quantiles = quantiles))
            }
            
            xlim <- xlim %||% c(.5, length(categories) + .5)
            output <- canvas(x = seq_along(categories), xlim = xlim, 
                             y = y, ylim = ylim , 
                             log = gsub('y', '', log))
            
            output$col <- prep_col_categories(col %||% categories, categories, ...)
            
            draw_violins(tapply(y, x, list), smooth = smooth, conditional = conditional, col = output$col$col, ...,
                         mean = mean, quantiles = quantiles, global_quantiles = global_quantiles)
            
            # output$axisNames[[1]] <- 'Density'
            output$axes[side == 1, ticks := list(setNames(seq_along(categories), categories))]
            output
            
          })



### draw() discrete ~ numeric ----


setMethod('.draw', c('numeric', 'discrete'),
          function(x, y, log = '', 
                   center = TRUE, conditional = FALSE, breaks = 40,
                   mean = TRUE, quantiles = c(.25, .75),
                   xlim = NULL, ylim = NULL, 
                   col = NULL, alpha = .7, ...) {
            
            categories <- sort(unique(y), decreasing = TRUE)
            
            breaks <- hist.default(x, breaks = breaks, ...)$breaks #seq(min(x), max(x), length.out = n)
            xcuts <- cut(x, breaks = breaks, include.lowest = TRUE)
            tab <- table(xcuts, factor(y, levels = categories))
            tab <- cbind(0, tab)
            
            if (conditional) tab <- proportions(tab, margin = 1)
            tab[is.na(tab)] <- 0
            tab <- do.call('cbind', Reduce('+', accumulate = TRUE, lapply(1:ncol(tab), \(j) tab[, j])))
            if (center) tab <- sweep(tab, 1, rowMeans(tab), '-')
            
            output <- canvas(x, xlim, range(tab), ylim, log = gsub('y', '', log))
            output$col <- prep_col_categories(col %||% categories, categories, alpha = alpha, ...)
            
            if (center) output$axes[ , ticks := lapply(ticks, \(t) {names(t) <- abs(t) ; t})]
            
            for (j in 1:(ncol(tab) - 1L)) {
              polygon(c(breaks, rev(breaks)), 
                      c(0, tab[ , j], rev(tab[ , j + 1]), 0), 
                      col = output$col$col[j],
                      border = FALSE)
            }
            
            
            output
            
          })

## draw_facets ----


draw_facets <- function(x = NULL, y = NULL, facets,  ..., xexpr = '', yexpr = '', 
                        xlab = NULL, ylab = NULL,
                        axes = 1:4, legend = TRUE,
                        col = 1, cex = NULL,
                        main = '', sub = '') {
  
  if (length(facets) > 2L) .stop("The draw() functon can't handle more than two faceting variables.",
                                 "You have provided {num2print(length(facet))}.")
  
  vecsize <- max(length(x), length(y))
  if (!all(lengths(facets) == vecsize)) {
    .stop('Facets variables must be vectors of the same length as the x/y plotting variables.')
  }
  
  oldpar <- par(oma = c(2, 3, 2, 3), mar = c(0, 0, 0, 0))
  on.exit({
    layout(1)
    title(main, sub, outer = TRUE, line = 0)
    par(oldpar)
    })
  
  
  # determine overall xlim ylim etc (output)
  output <- .draw(x, y, ..., col = col, cex = cex)
  
  output$axisNames[[1]] <- xlab %||% (output$axisNames[[1]] %||% xexpr)
  output$axisNames[[2]] <- ylab %||% (output$axisNames[[2]] %||% yexpr)
  
  args <- list(x = x, y = y, ..., col = output$col$col, cex = output$cex$cex,
               xlim = output$window$xlim[[1]], ylim = output$window$ylim[[1]],
               log = output$window$log)
  
  
  # Determine layout
  table <- do.call('table', facets)
  lay <- array(seq_along(table), dim = dim(table))
  layout(lay)
  
  
  if (length(facets) == 1L) {
    left.side   <- right.side <- table > 0L
    top.side    <- seq_along(table) == 1L
    bottom.side <- seq_along(table) == length(table)
  } else {
    .table <- table > 0
    left.side   <- leftmost(.table)
    right.side  <- rightmost(.table)
    top.side    <- topmost(.table)
    bottom.side <- bottommost(.table)
    
  }
               
  if (length(facets) == 1L) {
    left.mar   <- right.mar <- table >= 0
    top.mar    <- seq_along(table) == 1L
    bottom.mar <- seq_along(table) == length(table)
  } else {
    left.mar   <- col(table) == 1L
    right.mar  <- col(table) == ncol(table)
    top.mar    <- row(table) == 1L
    bottom.mar <- row(table) == nrow(table)
    
  }
  
  mar <- c(outside = 5, inside = .5)
  # plot each screen
  for (n in lay) {
      cur <- lay == n
      curlevels <- Map('[', dimnames(table), which(cur, arr.ind = TRUE))
      
      # set margins
      margin <- c(bottom.mar[cur], left.mar[cur], top.mar[cur], right.mar[cur])
      margin <- ifelse(margin, mar['outside'], mar['inside'])
      par(mar = margin)
      
      
      # prepare args and draw
      if (table[cur] > 0) {
        facet_ind <- Reduce('&', Map('==', curlevels, facets))
        curargs <- lapply(args, \(arg) if (length(arg) == vecsize) arg[facet_ind] else arg)
        
        do.call('.draw', curargs) # actual draw of plot
        
        sides <- c(bottom.side[cur], left.side[cur], top.side[cur], right.side[cur])
        lapply(which(!sides), border)
        
        # # axes 
        curaxes <- intersect(which(sides), axes)
        humaxes(output$axes, 
                ifelse(1:4 %in% curaxes, output$axisNames, vector('list', 4L)),
                curaxes)
      } else {
        plot.new() # only needed for writing facet levels in margins
      }
      
      # facet levels
      if (left.mar[cur]) mtext(curlevels[[1]], side = 2, font = 2,
                               outer = FALSE, col = 'black',
                               line = 5, las = 1)
      if (length(curlevels) > 1L && bottom.mar[cur]) mtext(curlevels[[2]], side = 1, font = 2,
                                                           outer = FALSE, col = 'black',
                                                           line = 5, las = 1)
      
      
  }
  layout(1)
  if (legend) {
    if (!is.null(output$col$legend)) output$col$legend()
    if (!is.null(output$cex$legend)) output$cex$legend()
  }
}

## draw_x ----


draw_quantiles <- function(side, var, quantiles = c(.025, .25, .5, .75, .975), limits = NULL, ...) {
  
  if (length(quantiles)) {
    checks(quantiles, xnumeric & xrange(0, 1))
    
    sides <- side %% 2 == 0
    quants <- quantile(var, prob = quantiles)
    
    
    if (is.null(limits)) {
      usr <- par('usr')
      limits <- if (sides) usr[1:2] else usr[3:4]
    }
    lineArgs <- list(limits[1], limits[2], quants, quants, lty = 'dashed', lwd = .3, col = rgb(0, 0, 0, .5))
    names(lineArgs)[1:4] <- if (sides) {
      c('x0', 'x1', 'y0', 'y1')
    } else {
      c('y0', 'y1', 'x0', 'x1')
    }
    do.call(graphics::segments, lineArgs)
    
    
   q <- paste0(round(quantiles       * 100, 1), '%')
   p <- paste0(round((1 - quantiles) * 100, 1), '%')
    
   if (sides) {
     text(limits[1], quants, as.expression(lapply(q, \(q) bquote('' %down% .(q)))), 
          cex = .4, xpd = TRUE, adj = c(1, .5))
     text(limits[2], quants, as.expression(lapply(p, \(q) bquote(.(q) %up% ''))),  
          cex = .4, xpd = TRUE, adj = c(0, 5))
   } else {
     text(quants, limits[1], as.expression(lapply(q, \(q) bquote('' %<-% .(q)))), 
          cex = .4, xpd = TRUE, adj = c(.5, 1))
     text(quants, limits[2], as.expression(lapply(p, \(q) bquote(.(q) %->% ''))), 
          cex = .4, xpd = TRUE, adj = c(.5, 0))
   }
    
    
  }
  
}




draw_violins <- function(vars, smooth = TRUE, conditional = FALSE, 
                         mean = TRUE, quantiles = c(), global_quantiles = FALSE, 
                         breaks = "Sturges", kernel = 'gaussian', bw = 'nrd0', ...,
                         col = 1) {
  vars <- lapply(vars, \(v) v[!is.na(v)])
  
  if (length(breaks) == 1L && pmatch(breaks, 'quantiles', 0) == 1 && length(quantiles)) {
    breaks <- quantile(unlist(vars), sort(unique(c(0, quantiles, 1))))
    names(breaks) <- format(breaks, digits = 3)
  }  
  
  if (!smooth) x <- hist.default(unlist(vars), breaks = breaks, plot = FALSE)$breaks
  
  
  lapply(seq_along(vars), 
         \(i) {
           var <- vars[[i]]
           if (smooth) {
             dens <- density(var, bw = bw, kernel = kernel)
             x <- dens$x
             y <- dens$y
             dx <- mean(diff(x))
             y <- y * dx
             
             x <- c(x[1], x,x[length(x)])
             y <- c(0, y, 0)
           } else {
             hist <- hist.default(var, breaks = x, plot = FALSE)
             y <- hist$density
             dx <- mean(diff(x))
             y <- y * dx
             
             x <- c(x[1], x[1], rep(x[-1], each = 2))
             y <- c(0, rep(y, each = 2), 0)
             # x <- c(x[1], x, x[length(x)])
             # y <- c(0, y, 0)
           }
           
           data.table(I = i, X = x, Y = y)
           
         }) |> data.table::rbindlist() -> Coor
  
  varprop <- proportions(lengths(vars))
  
  if (conditional) {
    Coor[, Yscale := 2^(ceiling(log(max(Y), 2))), by = I]
    
  } else {
    Coor[ , Y := Y * varprop[I]]
    Coor[ , Yscale := 2^(ceiling(log(max(Y), 2)))]
  }
  
  Coor[ , Y := Y / Yscale]
  
  for (i in seq_along(vars)) {
    Coor[I == i, {

      polygon(I + Y * .5, X, border = NA, col = col[i])
      polygon(I - Y * .5, X, border = NA, col = col[i])

      if (mean) points(i, mean(vars[[i]]), pch = 3, cex = 1.4, lwd = 1.5, col = 'black')
      
      minx <- par('usr')[3]
      arrows(i - .5, x1 = i + .5, minx, minx, 
             code = 3, length = .1, angle = 90, lty = 'dashed')
      text(i, minx, unique(Yscale), pos = 3, cex = .5, xpd = TRUE, col = 'black')

      
    }]
    if (!global_quantiles) draw_quantiles(2, vars[[i]], quantiles, limits = c(i - .5, i + .5))
  }
      
  if (global_quantiles) draw_quantiles(2, unlist(vars), quantiles)
  
  
}


draw_heat <- function(tab, log = '', ...) {
  xlim <- c(0L, ncol(tab))
  ylim <- c(0L, nrow(tab))
  
  plot.new()
  plot.window(xlim, ylim, log = log)
  
  col <- prep_col(c(tab), c(tab), ..., pch = NULL)
  colarray <- array(col$col, dim = dim(tab))
  
  Map(\(i, j, c) {
    polygon(c(i, i, i - 1, i - 1), 
            c(j, j - 1, j - 1, j), 
            col = c,
            border = rgb(.1, .1, .1, .1), lwd = .3)
    
  }, col(tab), nrow(tab) + 1L - row(tab), colarray)
  
  axes <- data.table(side = 1:2,
                     ticks = list(setNames(1:ncol(tab) - .5, colnames(tab)),
                                  setNames(1:nrow(tab) - .5, rev(rownames(tab)))),
                     line = -1)
  
  window <- data.table(Screen = as.integer(screen()),
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  axisNames <-  vector('list', 4L)
  if (names(dimnames(tab))[1] != '') axisNames[[1]] <- names(dimnames(tab))[1]
  if (names(dimnames(tab))[2] != '') axisNames[[2]] <- names(dimnames(tab))[2]
  list(window = window, axes = axes, axisNames = axisNames, col = col)
}



## draw()'s helpers ----

shrinklim <- function(lim, scale = .8) {
  ((lim - mean(lim)) * scale) + mean(lim)
}

border <- function(side, scale = .8) {
  coor <- par('usr')
  
  
  x <- switch(as.character(side),
              "1" = , "3" = shrinklim(coor[1:2], scale),
              "2" = coor[c(1, 1)],
              "4" = coor[c(2, 2)])
  
  y <- switch(as.character(side),
              "1" = coor[c(3, 3)],
              "3" = coor[c(4, 4)],
              "2" = , "4" = shrinklim(coor[3:4], scale))
  
  graphics::segments(x0 = x[1], x1 = x[2],
                     y0 = y[1], y1 = y[2], col = setalpha('grey50', .35),
                     lwd = .5, lty = 'longdash')
  
}

xy_formula <- function(form) {
  lhs <- rlang::f_lhs(form)
  rhs <- rlang::f_rhs(form)
  env <- rlang::f_env(form)
  y <- rlang::eval_tidy(lhs, env = env)
  x <- rlang::eval_tidy(rhs, env = env)
  
  list(x = x, y = y, xlab = rlang::as_label(rhs), ylab = rlang::as_label(lhs))
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

humaxes <- function(axesframe, axisNames, axes = 1:4) {
  if (length(axesframe)) do.call('Map', c(list(humaxis), axesframe[side %in% axes]))
  
  Map(axisNames, 1:4, f = \(label, side) {
    if (!is.null(label)) {
      mtext(label,  side,  line = 3,
            las = if (is.character(label) && nchar(label) > 3 && side %% 2 == 0) 3  else  1)
    } })
  
  
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
    tootall <- heights * 1.5 > dist
    if (any(tootall) && cex > .3) return(Recall(side, ticks, line, cex = cex * .9))
  }
  
  
  axis(side, ticks, labels, line = line - 1, las = las, tick = FALSE, cex.axis = cex, gap.axis = .1)
  
  
  
  
  
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
                     line = 1L)
  
  window <- data.table(Screen = as.integer(screen()),
                       xlim = list(xlim), ylim = list(ylim),
                       log = log)
  
  list(window = window, axes = axes, axisNames = vector('list', 4L))
}

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


# lines
# 0 -> quantile labels
# 1 -> axis labels
# 2 -> second axis ticks
# 3 -> xaxis labels
# 4 -> legend / sub title
# 5 -> title

axis.lines <- function() {
  cexs <- par(c('cex.axis', 'cex.lab', 'cex.sub'))
  
  lines <- cumsum(c(0.5, unlist(cexs))) 
  names(lines) <- c(names(lines)[-1], 'mar')
  lines <- as.list(lines)
  
  par(mar = rep(lines$mar, 4))
  plot(1:10, type='n', axes= FALSE, xlab='', ylab='')
  box()
  mtext(1:10, 1, at = 1:10, line = lines$cex.axis, cex = cexs$cex.axis, padj = 1)
  mtext(1:10, 2, at = 1:10, line = lines$cex.axis, cex = cexs$cex.axis)
  mtext('Y', side = 2, line = lines$cex.lab, cex = cexs$cex.lab)
  mtext('X', side = 1, line = lines$cex.lab, cex = cexs$cex.lab, padj = 1)
  
  mtext('Main', line = lines$cex.sub, cex = cexs$cex.sub)
  mtext('Sub', side = 1, line = lines$cex.sub, cex = cexs$cex.sub, padj=1)
lines
}

### argument preppers ----


#### prep_col ----


prep_col_categories <- function(col, categories, pch = 16, alpha = 1, contrast = FALSE, ...) {
  checks(col, xlen1 | xmatch(categories))
  checks(contrast, xTF, seealso = c('?draw'))
  checks(alpha, xlen1 & xnumber & xrange(0, 1), seealso = c('?draw'))
  col <- if (all(isColor(col))) {
    setalpha(col, alpha)
  } else {
    if (is.integer(col)) {
      flatly_scale(max(col), alpha = alpha, contrast = contrast)[col]
    } else {
      flatly_scale(length(categories), alpha = alpha, contrast = contrast)
    }
  }
  
  list(col = col,
       legend = \(pos = 'right') legend_col_discrete(categories, col, pch, pos = pos))
}

setGeneric('prep_col', 
           useAsDefault = function(col, var, pch, alpha, contrast, ncontinuous, ...) rep(col, length.out = n), # if there is no method
           function(col, var, pch = 16, alpha = 1, contrast = FALSE, ncontinuous = 100, ...) { 
             if (is.list(col) && names(col)[1] == 'col') col <- col$col
             
             checks(col, xlen1 | xmatch(var), seealso = c('?draw'))
             checks(contrast, xTF, seealso = c('?draw'))
             checks(alpha, xlen1 & xnumber & xrange(0, 1), seealso = c('?draw'))
             checks(ncontinuous, xlen1 & xnatural & xmin(50), seealso = c('?draw')) 
             
             if (length(col) == 1L || any(isColor(as.character(col)))) return(list(col = setalpha(col, alpha)))
             
             standardGeneric('prep_col')
             
           })


setMethod('prep_col', c('discrete'),
          function(col, var, pch = 16, alpha = 1, contrast = FALSE, ...) {
            categories <- sort(unique(col))
            if (is.integer(col) && length(categories) > 10L) return(prep_col(as.numeric(col), var, 
                                                                             alpha = alpha, pch, contrast = FALSE))
            
            palette <- flatly_scale(length(categories), alpha = alpha, contrast = contrast)
            col <- palette[match(col, categories)]
            
            list(col = col,
                 legend = \(pos = 'right')  legend_col_discrete(categories, palette, pch, pos = pos))
          })

setMethod('prep_col', c('numeric'),
          function(col, var, pch = NULL, alpha = 1, ncontinuous = 100L, ...) {
            if (length(unique(col)) < 6) return(prep_col(factor(col),
                                                         var, alpha = alpha, contrast = FALSE))
            breaks <- seq(min(col), max(col), length.out = ncontinuous)
            
            palette <- flatly_scale(ncontinuous, alpha = alpha)
            cols <- palette[as.integer(cut(col, breaks = breaks, include.lowest = TRUE))]
            
            
            list(col = cols,
                 legend = \(pos) legend_col_continuous(col, palette, pch = pch))
          })



legend_col_discrete <- function(categories, palette, pch, pos = 'right') {
  legend(pos, TRUE, legend = categories, col = palette, 
         inset = -.2, xpd = TRUE,
         pch = pch[1], bty = 'n')
}

legend_col_continuous <- function(var, palette, pch = NULL) {
  
  yrange <- par('usr')[3:4]
  yrange <- ((yrange - mean(yrange)) * .9) + mean(yrange)
  
  ticks <- pretty(var, min.n = 5, n = 10)
  yticks <- seq(yrange[1], yrange[2], length.out = length(ticks))
  
  y <- seq(yrange[1], yrange[2], length.out = length(palette))
  ydiff <- diff(y)[1] / 2
  
  xpos <- par('usr')[1:2]
  xpos <- xpos[1] + diff(xpos) * c(1.2, 1.3)
  
  if (is.null(pch)) {
    for(i in seq_along(y)) {
      polygon(c(xpos[1], xpos[2], xpos[2], xpos[1]),
              y[i] + c(ydiff, ydiff, -ydiff, -ydiff),
              col = palette[i], xpd = TRUE, border = NA)
    } 
  } else {
    points(rep(xpos[1], length(y)), y, pch = pch, col = palette, xpd = TRUE)
  }
 
  text(xpos[if (is.null(pch)) 2 else 1], yticks, ticks, pos = 4, cex = .4, xpd = TRUE)
  
  
}


#### prep_cex ----



prep_cex <- function(x, y, cex = NULL, col, ...) {
  size <- max(length(x), length(y))
  checks(cex, xnull | (xpositive & (xlen1 | xlength(size))), seealso = '?draw')
 
  
  output <- list(cex = cex)
  if (is.null(cex)) {
    output$cex <- cex_density(x, y)
    
  } else {
    if (length(cex) == size) {
      val_legend <- 2^seq(log(min(cex), 2), log(max(cex), 2), length.out = 9)
      
      cex <- sqrt(cex) 
      
      maxPowers <- 6
      logrange <- diff(range(log(cex)))
      power <- max(2, ceiling(exp(logrange / maxPowers)))
      
      if (power > 2) {
        .message("In draw(cex = ), your largest cex value is {round(exp(logrange)^2)} times greater than the smallest value.",
                 "To plot this, we must understate the differences between points.",
                 "When comparing the point in this plot, a doubling of area corresponds to multiplying the value",
                 "by {num2print(power)}.")
        cex <- 2^log(cex, power)
      }
      
      
      # scale to center on .5

      scale <- exp(mean(log(cex))) 
      cex_legend <- sqrt(val_legend) / scale
      cex <- cex / scale
      
        
      output$cex <- cex 
      output$legend <- \(pos = 'right') legend(pos, inset = -.1, col = col[1], pch = 16, 
                                               legend = format(val_legend, big.mark = ',', digits = 2), 
                                               pt.cex = cex_legend,
                                               text.col = 'black', xpd = TRUE, bty = 'n')
    } 
  }
  
  output
  
}

cex_density <- function(x, y) {
  vars <- Filter(length, list(x, y))
  
  cuts <- lapply(vars, cut, breaks = if (length(vars) == 1L) 100 else 10)
  
  # forced to be between 1 and 250
  maxdensity <- min(floor(log10(max(do.call('table', cuts)))), 4)

  1 - (maxdensity * .225)
  
  
}

#### prep_layout

prep_layout <- function(facets) {
  browser()
  if (length(facets) > 2) {
    facets[[2]] <- squashGroupby(facets[-1])
    facets <- facets[1:2]
  }
  facets <- unique(as.data.frame(facets))
  
  
  mat <- matrix(1:nrow(facets), nrow = length(unique(facets[[1]])))
  
  layout(mat)
  
  mat
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

options(ggplot2.continuous.fill = ggplot2::scale_color_gradientn(colors = flatly_scale(100)))
options(ggplot2.continuous.color = ggplot2::scale_color_gradientn(colours = flatly_scale(100)))
options(ggplot2.continuous.colour = ggplot2::scale_color_gradientn(colours = flatly_scale(100)))

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


  


 

          
