# Tallies ----

setClass('text', contains = 'character')

setClass('humdrum.table', contains = 'table')

#' Tabulate and/or cross-tabulate data
#' 
#' The `tally()` function is exactly like R's fundamental [table()][base::table] function,
#' except that 1) will give special treatment to humdrumR [token()] data 2)
#' has more intuitive/simple argument names 3) makes it easier to combine/manipulate
#' disparate output tables.
#' 
#' @details
#' 
#' The `tally()` function is essentially a wrapper
#' around [base::table()][base::table] function.
#' However, any [token()] class arguments are treated like [factors()],
#' calling generating their own levels.
#' This assures that, for example, pitch data is tabulated in order of pitch height,
#' and "missing" pitches are counted as zero.
#' 
#' `tally()` will, by default, count `NA` values if they are present---if you don't want
#' to count `NA`s, specify `na.rm = TRUE`.
#' You can also tell `tally()` to exclude (not count) any other arbitrary values you
#' provide as a vector to the `exclude` argument.
#' 
#' 
#' `tally()` will always give names to the dimensions of the table it creates.
#' You can specify these names directly as argument names, like `tally(Kern = kern(Token))`;
#' if you don't specify a name, `tally()` will make up a name(s) based on expression(s) it is tallying.
#' (Note that `tally()` does not copy [base::table()]'s obtusely-named `dnn` or `deparse.level` arguments.)

#' @section Manipulating humdrum tables:
#' 
#' The output of `tally()` is a special form of R `table`, a `humdrum.table`.
#' Given two or more `humdrum.table`s, if you apply basic R operators 
#' (e.g., arithmetic, comparisons) or row/column binding (`cbind`/`rbind`) 
#' `humdrumR` will align the tables by their dimension-names before
#' doing the operation.
#' This means, that if you have two tables of pitch data, but one table includes specific pitch and other doesn't,
#' you can still add them together or bind them into a matrix.
#'  See the examples!
#'
#' @examples 
#' 
#' generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b')
#' complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a')
#' 
#' genericTable   <- tally(generic)
#' complexTable <- tally(complex)
#' 
#' genericTable
#' complexTable
#' 
#' genericTable + complexTable
#' 
#' cbind(genericTable, complexTable)
#' 
#' @export
tally <- function(..., na.rm, exclude) UseMethod('tally')
#' @export
tally.default <- function(..., 
                          na.rm = FALSE,
                          exclude = NULL) {
  
  # exprs <- rlang::enexprs(...)
  exprs <- as.list(substitute(list(...)))[-1L]
  
  args <- list(...)
  dimnames <- .names(args)
  if (any(dimnames == '')) dimnames[dimnames == ''] <- deparse.unique(exprs[dimnames == ''])
  
  
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) factorize(arg) else arg
                 })
  tab <- do.call(base::table,
                 c(args, list(exclude = c(exclude, (if (na.rm) c(NA, NaN))), 
                              useNA = if (na.rm) 'no' else 'ifany', 
                              deparse.level = 0)))
  
  # dimnames(tab) <- lapply(dimnames(tab), \(dn) ifelse(is.na(dn), 'NA', dn))
  names(dimnames(tab)) <- dimnames
  
  
  new('humdrum.table', tab)
  
  
}

#' @export
tally.humdrumR <- function(x, na.rm = FALSE, exclude = NULL) {
  fields <- pullSelectedFields(x)
  
  do.call('tally', as.list(fields))
  
}


#' @rdname tally
#' @export
setMethod('Ops', c('humdrum.table', 'humdrum.table'),
          \(e1, e2) {
            
            tables <- alignTables(list(e1, e2))
            e3 <- callNextMethod(tables[[1]], tables[[2]])
            
            if (inherits(e3, 'table')) new('humdrum.table',  e3) else e3
            
          })

#' @rdname tally
#' @export
cbind.humdrum.table <- function(...) {
  tables <- list(...)
  tables <- Filter(Negate(is.null), tables)
  
  tables <- alignTables(tables, 'cbind') 
  
  do.call('cbind', tables)
  
}

#' @rdname tally
#' @export
rbind.humdrum.table <- function(...) {
  tables <- list(...)
  tables <- Filter(Negate(is.null), tables)
  
  tables <- alignTables(tables, 'rbind') 
  
  do.call('rbind', tables)
  
}

#' @rdname tally
#' @export
as.data.frame.humdrum.table <- function(x, ...) {
  tab <- as.data.frame(S3Part(x), ...)
  names(tab)[names(tab) == 'Freq'] <- 'Tally'
  tab
}

alignTables <- function(tables, funcname = '') {
  tables <- lapply(tables, 
                   \(tab) {
                     dimnames(tab) <- lapply(dimnames(tab), \(names)ifelse(is.na(names), '_<NA>_', names) ) 
                     tab})
  dimnames <- lapply(tables, dimnames)
  dimensions <- Filter(\(dn) any(dn != ''), lapply(dimnames, names))
  
  if (length(unique(lengths(dimnames))) > 1L) .stop("If using {funcname} on humdrum.tables, they must all have the same number of dimensions.")
  
  dimnames <- as.data.frame(do.call('rbind', dimnames))
  dimnames <- lapply(dimnames, \(dim) Reduce('union', dim))
  
  
  allindices <- as.data.frame(do.call('expand.grid', dimnames))
  
  empty <- do.call('table', allindices) - 1L
  
  dimensions <- Reduce(\(a, b) paste(a, b, sep = '/'), dimensions)
  names(dimnames(empty)) <- dimensions
  
  lapply(tables, 
         \(tab) {
           
           indices <- as.matrix(allindices[Reduce('&', Map(`%in%`, allindices, dimnames(tab))), ])
           empty[indices] <- tab[indices]
           
           dimnames(empty) <- lapply(dimnames(empty), \(names) ifelse(names == '_<NA>_', NA_character_, names))
           empty
         })
  
  
  
  
}


# Probabilities ----

## probabilityDistribution ----



setClass('probabilityDistribution', contains = 'humdrum.table', slots = c(N = 'integer', margin = 'integer'))

#' @rdname p
#' @export
setMethod('%*%', c('probabilityDistribution', 'probabilityDistribution'),
         function(x, y) {
           new('probabilityDistribution', as.table(outer(S3Part(x), S3Part(y), '*')), N = x@N, margin = integer(0L))
         })


unmargin <- function(pd) {
  if (length(pd@margin) == 0L) return(pd)
 
  marginal <- proportions(pd@N)
  
  pd <- sweep(pd, pd@margin, marginal, '*')
  pd@margin <- integer(0L)
  pd@N <- sum(pd@N)
  pd
  
}
#' @rdname p
#' @export
setMethod('show', 'probabilityDistribution',
          function(object) {
            digits <- 4L
            x <- S3Part(object, strictS3 = TRUE)
            zeros <- x == 0
            
            x[] <- format(x, scientific = FALSE)
            n <- nchar(x) - 2
            long <- n > digits & !is.na(x)
            x[long] <- paste0(substr(x[long], start = 1, stop = digits + 2), '_')
            
            # dimension names
            header <- pdist.name(x, object@margin)
            
            # names(dimnames(x)) <- NULL
            x[zeros] <- '.'
            x[] <- gsub('^0', '', x)
            cat('\t\t', header, '\n', sep = '')
            print(x, quote = FALSE, na.print = '.NA')
          })


pdist.name <- function(ptab, margin = NULL, func = 'P') {
  dimnames <- names(dimnames(ptab))
  
  args <- if (length(margin)) {
    independent <- dimnames[margin]
    dependent <- dimnames[-margin]
    
    paste0(paste(dependent, collapse = ', '), 
           ' | ', 
           paste(independent, collapse = ', '))
    
    
  } else {
    paste(dimnames, collapse = ', ') 
  }
  
  paste0(func, '(', args, ')')
  
}

#' @rdname tally
#' @export
as.data.frame.probabilityDistribution <- function(x, ...) {
  tab <- as.data.frame(S3Part(x), ...)
  names(tab)[names(tab) == 'Freq'] <- 'p'
  tab
}

## p() -----

#' Tabulate and cross proportions
#' 
#' 
#' @export
setGeneric('p', function(x, ...) standardGeneric('p'))



#' @rdname p
#' @export
setMethod('p', c('table'),
          function(x, margin = NULL, na.rm = FALSE) {
            
            
            if (length(dim(x)) == 1L) margin <- NULL
            
            if (na.rm) {
              notna <- unname(lapply(dimnames(x), \(dim) !is.na(dim)))
              x <- do.call('[', c(list(x), notna))
            }
            
            ptab <- proportions(x, margin = margin) 
            
            
            n <- marginSums(x, margin = margin)
            
            new('probabilityDistribution', ptab, N = as.integer(n), margin =  as.integer(margin))
          })



setClassUnion("discrete", members = c('character', 'integer', 'logical', 'token', 'factor'))

#' @rdname p
#' @export
setMethod('p', 'discrete',
          function(x, ..., distribution = NULL, margin = NULL, na.rm = TRUE) {
            checks(distribution, xnull | xclass('probabilityDistribution'))
            
            if (is.null(distribution)) distribution <- p(tally(x, ..., na.rm = FALSE), margin = margin, na.rm = na.rm)
                
            
            
            ind <- if (na.rm) {
              cbind(x, ...)
            } else {
              dimnames(distribution) <- lapply(dimnames(distribution), \(dim) ifelse(is.na(dim), '<NA>', dim))
              ind <- cbind(x, ...)
              ind[is.na(ind)] <- '<NA>'
              ind
            }
            c(unclass(distribution)[ind])
          })


#' @rdname p
#' @export
setMethod('p', 'numeric',
          function(x, density = NULL, na.rm = FALSE, ..., bw = 'SJ', adjust = 1.5) {
            checks(density, xnull | xclass('density'))
            
            if (!na.rm && any(is.na(x)))  {
              return(rep_len(NA_real_, length(x)))
            } else {
              .x <- x[!is.na(x)]
            }
            
            nuniq <- length(unique(.x))
            if ((all(is.whole(.x)) && nuniq < 50L)) return(p(as.character(x), margin = margin, na.rm = na.rm))
            
            if (is.null(density)) density <- density(.x, ..., bw = bw, adjust = adjust)
            
            
            x[!is.na(x)] <- density$y[closest(.x, density$x, value = FALSE)]
            
            x
            
          })

#' @rdname p
#' @export
setMethod('p', 'missing',
          function(x, ..., margin = NULL, na.rm = TRUE) {
            args <- list(..., margin = margin, na.rm = na.rm)
            origname <- names(args)[1]
            names(args)[1] <- 'x'
            
            result <- do.call('p', args)
            
            if (is.table(result)) names(dimnames(result))[1] <- origname
            result
          })





# Information theory ----



#' Calculate Entropy or Information Content of variables 
#'
#' Information content and entropy are fundamental concepts in [information theory](https://en.wikipedia.org/wiki/Information_theory),
#' which quantify the amount of information (or "surprise") in a random variable.
#' Both concepts are closely related the probability density/mass of events: improbable events have higher information content.
#' The probability of *each* observation maps to the [information content](https://en.wikipedia.org/wiki/Information_content);
#' The average information content of a variable is the [entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)).
#' Information content/entropy can be calculated for discrete probabilities or continuous probabilities,
#' and humdrumR defines methods for calculating both.
#' 
#' @details 
#' 
#' To calculate information content or entropy, we must assume (or estimate) a probability distribution.
#' HumdrumR uses R's standard [table()] and [density()] functions to estimate discrte and continuous probability
#' distributions respectively.
#' 
#' Entropy is the average information content of a variable.
#' The `entropy()` function can accept either a [table()] object (for discrete variables), 
#' or a [density()] object (for continuous variables).
#' If `entropy()` is passed an [atomic][base::vector()] vector,
#' the values of the vector are treated as observations or a random variable:
#' for `numeric` vectors, the [stats::density()] function is used to estimate the probability distribution
#' of the random (continuous) variable, then entropy is computed for the density.
#' For other atomic vectors, [table()] is called to tabulate the discrete probability mass for each
#' observed level, and entropy is then computed for the table.
#'
#' The `ic()` function only accepts atomic vectors as its main (`x`) argument, but must also
#' be provided a `distribution` argument.
#' By default, the `distribution` argument is estimated using [density()] (`numeric` input) or [table()] (other input).
#' 
#' 
#' @family {Information theory functions} 
#' @export
setGeneric('entropy', function(x, base = 2, ...) standardGeneric('entropy'))
#' @rdname entropy
#' @export
setMethod('entropy', 'table',
          function(x, base = 2, margin = NULL, na.rm = FALSE) {
            
            joint <- p(x, margin = NULL, na.rm = na.rm)
            
            other <- p(x, margin = margin, na.rm = na.rm)
            other <- ifelse(x == 0L, 0, log(other, base = base)) 
            
            equation <- pdist.name(joint, margin, 'H')
            setNames(-sum(joint * other), equation)
          })

  
#' @rdname entropy
#' @export
setMethod('entropy', 'probabilityDistribution',
          function(x, base = 2) {
            
            joint <- unmargin(x)
            
            other <- x
            other <- ifelse(x == 0L, 0, log(other, base = base)) 
            
            equation <- pdist.name(joint, x@margin, 'H')
            
            setNames(-sum(joint * other), equation)
          })



  
#' @rdname entropy
#' @export
setMethod('entropy', 'density',
          function(x, base = 2, na.rm = TRUE) {
            label <- rlang::expr_name(rlang::enexpr(x))
            if (any(is.na(x))) return(NA_real_)
            
            dx <- diff(x$x[1:2])
            
            equation <- paste0('H(', label, ')')
            
            setNames(-sum(log(x$y, base = base) * x$y * dx), equation)
            
            
          })

#' @rdname entropy
#' @export
setGeneric('ic', function(x, ...) standardGeneric('ic'))
#' @rdname entropy
#' @export
setMethod('ic', 'discrete',
          function(x, ..., base = 2, distribution = NULL, margin = NULL, na.rm = TRUE) {
            
            p <- p(x, distribution = distribution, ..., margin = margin, na.rm = na.rm)
            
            -ifelse(p == 0, NA, log(p, base = base))

            
          })

#' @rdname entropy
#' @export
setMethod('ic', 'numeric',
          function(x, base = 2, density = NULL, ..., na.rm = TRUE) {
            
            p <- p(x, density = density, ..., na.rm = na.rm)
            
            -ifelse(p == 0, NA, log(p, base = base))
            
            
          })

#' @rdname entropy
#' @export
setMethod('ic', 'missing',
          function(x, base = 2, ..., margin = NULL, na.rm = TRUE) {
            args <- list(..., base = base, margin = margin, na.rm = na.rm)
            names(args)[1] <- 'x'
            do.call('ic', args)
            
            
          })





#' Calculate Mutual Information of variables
#' 
#' @family {Information theory functions} 
#' @export
setGeneric('mutualInfo', function(x, ...) standardGeneric('mutualInfo'))


#' @rdname mutualInfo
#' @export
setMethod('mutualInfo', 'probabilityDistribution',
          function(x, ..., base = 2) {
            if (length(dim(x)) < 2L) return(entropy(x, base = base))
            
            joint <- S3Part(unmargin(x))
            
            independentJoint <- outer(rowSums(x), colSums(x), '*')
            
            logjoint <- ifelse(joint == 0, 0, log(joint, base))
            logindependent <- ifelse(independentJoint == 0, 0, log(independentJoint, base))
            
            ratio <- logjoint - logindependent
            
            equation <- pdist.name(joint, x@margin, 'I')
            setNames(sum(joint * ratio), equation)
            
          })


#' @rdname mutualInfo
#' @export
setMethod('mutualInfo', 'table',
          function(x, base = 2, margin = NULL, na.rm = FALSE) {
            
            ptab <- p(x, margin = margin, na.rm = na.rm)
            mutualInfo(ptab, base = base)
            
          })

   
#' @rdname mutualInfo
#' @export
setMethod('mutualInfo', 'discrete',
          function(x, ..., base = 2, na.rm = FALSE) {
            args <- list(x, ...)
            marginals <- lapply(args, \(arg) p(tally(arg)))
            
            joint <- Reduce('%*%', marginals)
            
            p_observed <- p(x, ..., margin = NULL, na.rm = na.rm)
            p_joint <- p(x, ..., distribution = joint, margin = NULL, na.rm = na.rm)
            
            p_observed <- ifelse(p_observed == 0, NA_real_, log(p_observed, base = base))
            p_joint <- ifelse(p_joint == 0, NA_real_, log(p_joint, base = base))
            
            p_observed - p_joint
            
            
          
            
          })
  


#' Calculate cross entropy between two distributions
#' 
#' TBA
#' @family {Information theory functions}
#' @export
setGeneric('crossEntropy', function(distribution1, distribution2, ...) standardGeneric('crossEntropy'))

#' @rdname crossEntropy
#' @export
setMethod('crossEntropy', c('probabilityDistribution', 'probabilityDistribution'),
          function(distribution1, distribution2, base = 2) {
            
            distribution2 <- ifelse(distribution2 == 0L, 0, log(distribution2, base = base))
            
            
            equation <- paste0('H(', 
                               pdist.name(distribution1, func = ''), ', ',
                               pdist.name(distribution2, func = ''))
            setNames(-sum(distribution1 * distribution2), equation)
            
          })




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
#' + `x` is a [table][tally()]: barplot.
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
  oldpar <- par(family = 'Lato', col = 4, col.main = 5, col.axis = 5, col.sub = 5, col.lab = 2,
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
    
    mtext(xlab, 1, line = 2.5, outer = outer)
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
            points(x, rnorm(length(x), mean(ylim), diff(range(ylim)) / 20), 
                   cex = cex , col = rgb(1,0,0, .1), pch = 16, xpd = TRUE)
            
            
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
            draw(tally(x, y), ...)
            })

#' @rdname draw
#' @export
setMethod('draw', c('discrete', 'missing'),
          function(x, y, ...){ 
            draw(tally(x), ..., xlab = '')
          })

# #' @rdname draw
# #' @export
#setMethod('draw', c('missing', 'discrete'),
#          function(x, y, ...){ 
#            output <- draw(tally(y), ...)
#          }) ################ THis can work except the labels are reversed...need to figure that your

#' @rdname draw
#' @export
setMethod('draw', 'table',
          function(x, y, col = 1:nrow(x), log = '', ..., ylim = NULL, yat = NULL, beside = TRUE) {
            yticks <- sort(unique(prep_ticks(ylim %||% c(0, x), log = grepl('y', log), at = yat)))
            if (grepl('y', log)) yticks <- yticks[yticks > 0]
            if (inherits(x, 'humdrum.table')) x <- S3Part(x)
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
setMethod('draw', 'probabilityDistribution',
          function(x, y, col = 1:nrow(x), log = '', ..., yat = NULL, beside = TRUE) {
            yticks <- sort(unique(c(0, prep_ticks(c(x), log = grepl('y', log), at = yat))))
            if (grepl('y', log)) yticks <- yticks[yticks > 0]
            if (inherits(x, 'humdrum.table')) x <- S3Part(x)
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
  
  args$xat <- args$yat <- NA
  
  facetLabels <- unique(as.data.frame(facets))
  facetLabels <- facetLabels[sapply(facetLabels, \(val) length(unique(val)) > 1L)]
  facetLabels <- do.call('paste', facetLabels)
  
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
  if (nrow(layout) > 1) {
    abline(h = head(seq(0, 1, length.out = nrow(layout) + 1)[-1], -1),
           lty = 'dashed', col = setalpha(flatly[5], .3))
  }
  if (ncol(layout) > 1) {
    abline(v = head(seq(0, 1, length.out = ncol(layout) + 1)[-1], -1),
           lty = 'dashed', col = setalpha(flatly[5], .3))
  }
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


