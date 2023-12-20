


# S4 Distributions ----

## Definitions ----





#' Distributions
#' 
#' HUmdrumR has ways to...
#' 
#' @name distributions
NULL


setClass('distribution', contains = 'data.frame', slots = c(Sort = 'integer'))
setClass('count', contains = 'distribution')
setClass('probability', contains = 'distribution', slots = c(N = 'integer', Condition = 'maybecharacter'))



### Constructors ----

distribution <- function(x, type, ...) {
  df <- as.data.frame(x)
  
  new(if (type == 'n') 'count' else 'probability', 
      new('distribution', df, Sort = 0L), 
      ...)
}



### Accessors ----


dist_type <- function(dist) intersect(colnames(dist), c('n', 'p'))
getValues <- function(dist) as.data.frame(dist)[ , dist_type(dist)]
getFactors <- function(dist) as.data.frame(dist)[ , dimnames(dist), drop = FALSE]

#' @export
dimnames.distribution <- function(x) setdiff(colnames(x), c('n', 'p'))

### print() ----

#' @export
#' @rdname distributions
setMethod('show', 'distribution', \(object) print.distribution(object))

#' @export
#' @rdname distributions
print.distribution <- function(dist, digits = if (inherits(dist, 'probability')) 3 else 1,
                               syntaxHighlight = humdrumRoption('syntaxHighlight'),
                               zeros = '.') {
  
  type <- dist_type(dist)
  message <- paste0('humdrumR ', 
                    if (type == 'p') paste0('probability distribution ', Pequation(dist)) else 'count distribution')
  
  dimnames <- dimnames(dist)
  
  if (nrow(dist) == 0) {
    message <- paste0(message, ' (empty)')
    if (syntaxHighlight) message <- syntaxHighlight(message, 'G')
    cat(message, '\n')
    return(invisible(dist))
  }
  sort <- dist@Sort
  
  X <- getValues(dist)
  
  X <- if (type == 'p') prettyP(X, digits = digits, zeros = zeros) else prettyN(X, digits = digits, zeros = zeros)
  
  # do we scale or round?
  scale <- attr(X, 'scale')
  
  scale <- if (length(scale) == 1L) {
    if (scale > 0) paste0(' (', c('thousands', 'millions', 'billions', 'trillions')[scale], ')')
  }
  
  if (attr(X, 'negative') %||% FALSE) scale <- paste0(scale, ' (∃N < 0)')
  if (attr(X, 'approx')) scale <- paste0(scale, ', ~rounded')
  message <- paste0(message, scale)
  
  dist <- getFactors(dist)
  dist[[type]] <- X
  
  # check if we can widen
  iswide <- FALSE
  printmat <- (if(sort == -0L && length(dimnames) >= 2L) {
    
    wide <- as.matrix(dcast(as.data.table(dist), rlang::new_formula(quote(...), rlang::sym(dimnames[2])), fill = attr(X, 'zerofill'), value.var = type))
    
    factorcols <- colnames(wide) %in% dimnames
    toprow    <- ifelse( factorcols, colnames(wide), '')
    toprow[length(dimnames)] <- dimnames[2]
    secondrow <- ifelse(!factorcols, colnames(wide), '')
    
    wide <- rbind(toprow, secondrow, wide, secondrow, toprow)
    wide <- apply(wide, 2, format, justify = 'right')
    wide <- wide[ , order(match(toprow, colnames(dist), nomatch = 2))]
    
    width <- sum(nchar(wide[1, ])) + ncol(wide) * 2
    
    iswide <- width < (options('width')$width - 10L)
    if (iswide) wide
  })  %||%  apply(rbind(colnames(dist), 
                        as.matrix(dist), 
                        colnames(dist)), 
                  2, format, justify = 'right')

  if (sort != 0L) {
    rank <- format(c('Rank', seq_len(nrow(printmat) - 2L), 'Rank'), 
                   justify = 'left')
    if (sort == 1) rank <- rev(rank)
    printmat <- cbind(Rank = rank, printmat)
    
  }
  

  
  
  if (syntaxHighlight) {
    
    syntax <- array('D', dim = dim(printmat))
    
    syntax[c(1, nrow(printmat)), ] <- 'N'
    
    dataCols <- !colnames(printmat) %in% c(dimnames, 'Rank')
    rankCol <- colnames(printmat) == 'Rank'
    
    if (iswide) {
      syntax[c(2, nrow(printmat) - 1L), ] <- 'I' 
      dataRows <- 3:(nrow(printmat) - 2L)
    } else {
      dataRows <- 2:(nrow(printmat) - 1L)
    }
    
    syntax[dataRows, !dataCols & !rankCol] <- 'I'
    syntax[dataRows, rankCol] <- 'E'
    
    printmat[] <- syntaxHighlight(printmat, syntax)
    
    
    message <- syntaxHighlight(message, 'G')
  }


  cat(message, '\n')
  cat(apply(printmat, 1, paste, collapse = '  '), sep = '\n')
  cat(message, '\n')
  invisible(dist)
}


Pequation <- function(dist, f = 'P', collapse = ',') {
  dimnames <- dimnames(dist)
  
  condition <- dist@Condition 
  
  eq <- paste(setdiff(dimnames, condition), collapse = collapse)
  if (!is.null(condition)) {
    condition <- paste(condition, collapse = collapse)
    eq <- paste0(eq, ' | ', condition)
  }
  
  paste0(f, '(', eq, ')')
}

prettyN <- function(N, digits = 1L, zeros = '.') {
  tens <- ifelse(N == 0, 0, log10(abs(N)) |> floor())
  
  thousands <- pmin(tens %/% 3, 4L)
  thousands[thousands < 0L] <- thousands[thousands < 0L] + 1L
  
  # scaling (powers of 10^3)
  scale <- c('', 'k', 'M', 'G', 'P')[1L + abs(thousands)]
  scale[thousands < 0] <- paste0('/', scale[thousands < 0])
  N <- N / 1000^thousands

  globalscale <- if (length(unique(scale[N != 0])) == 1L) {
    globalscale <- unique(thousands[N != 0])
    scale <- NULL
    globalscale
  }
  
  # round and format
  Nround <- round(N, digits = digits)
  approx <- N != Nround
  
  Nprint <- format(Nround, scientific = FALSE)
  Nprint[N != 0] <- gsub('^( *)0\\.', '\\1 .', Nprint[N != 0])
  Nprint <- gsub('\\.0+$', '', Nprint)
  Nprint <- gsub('^( *)0( *)$', paste0('\\1', zeros, '\\2'), Nprint)

  
  output <- paste0(Nprint, scale, ifelse(approx, '~', ''))
  output <- paste0(output, strrep(' ', max(nchar(output)) - nchar(output))) 
  
  attr(output, 'zerofill') <- zeros
  attr(output, 'scale') <- globalscale
  attr(output, 'approx') <- any(approx, na.rm = TRUE)
  attr(output, 'negative') <- any(N < 0, na.rm = TRUE)
  
  output
  
}


prettyP <- function(P, digits = 3, zeros = '.') {
  tens <- ifelse(P == 0, 0, log10(P) |> round())
  
  thousands <- (tens - 1) %/% -3
  scale <- min(thousands)
  
  P <- P * 1000^-scale
  
  Pround <- round(P, digits = digits)
  approx <- P != Pround
  
  Pprint <- format(Pround, scientific = FALSE)
  Pprint <- gsub('^( *)0\\.', '\\1 .', Pprint)
  Pprint <- gsub('\\0*$', '', Pprint)
  Pprint[P == 0] <- paste0(' ', zeros)

  
  output <- paste0(Pprint, ifelse(approx, '~', ''))
  maxnchar <- max(nchar(output))
  output <- paste0(output, strrep(' ', maxnchar - nchar(output))) 
  # output <- gsub(paste0('\\.', strrep('0', digits), '~'), paste0('.~', strrep(' ', digits)), output)
  
  attr(output, 'zerofill') <- paste0(' ', zeros, strrep(' ', maxnchar - 2L))
  attr(output, 'scale') <- if (scale != 0L) paste0('10^(', -scale * 3, ')')
  attr(output, 'approx') <- any(approx)
  
  output
  
}


prettyBins <- function(x, maxN = 20, quantiles = 0, right = TRUE, ...) {
  levels <- sort(unique(x))
  if (is.integer(x) || all(is.whole(levels))) {
    range <- diff(range(levels))
    
    if (range <= maxN) {
      return(factor(x, levels = min(levels) : max(levels)))
    }
    if (length(levels) <= maxN) return(factor(x, levels = levels))
  } else {
    if (length(levels) <= (maxN / 2)) return(factor(x, levels = levels))
  }
  
  if (quantiles) {
    if (is.logical(quantiles)) quantiles <- 10L
    
    breaks <- quantile(x, probs = seq(0, 1, length.out = quantiles + 1L))
  } else {
    breaks <- hist(x, plot = FALSE, ...)$breaks
  }
  
  cut(x, breaks, include.lowest = TRUE, right = right, dig.lab = 3)
  
}

### indexing ----

#' @export
setMethod('[', 'distribution',
          function(x, i, ...) {
            
            if (is.matrix(i) && ncol(i) == 1L && names(dimnames(i))[[1]] == paste(dimnames(x), collapse = '.')) {
              i <- i[do.call('paste', c(getFactors(x), list(sep = '.'))), , drop = FALSE]
            }
            
            type <- dist_type(x)
            if (type == 'p') {
              distribution(as.data.frame(x)[i , ], type = type, Condition = x@Condition, N = x@N)
            } else {
              distribution(as.data.frame(x)[i , ], type = type)
            }
            
             
          })

#' @export
setMethod('[', c('probability', 'missing', 'character'),
          function(x, i, j, ...) {
            
            N <- x@N
            x <- unmargin(x)
            x <- as.data.table(x)
            
            x <- as.data.frame(x[ , list(p = sum(p)), by = j])
            
            distribution(x, 'p', Condition = NULL, N = N)
            
          })


### coercion ----

#' @export
as.matrix.distribution <- function(x, wide = TRUE, ...) {
  dimnames <- dimnames(x)
  type <- dist_type(x)
  
  if (length(dimnames) >= 2L && wide && x@Sort == -0L) {
    mat <- as.matrix(dcast(as.data.table(x), 
                    rlang::new_formula(quote(...), rlang::sym(dimnames[2])), 
                    fill = 0, value.var = type))
    
    talldim <- colnames(mat) %in% dimnames 
    
    rownames(mat) <- do.call('paste', c(as.data.frame(mat[ , talldim, drop = FALSE]), list(sep = '.')))
    mat <- mat[ , !talldim, drop = FALSE]
    names(dimnames(mat)) <- c(paste(dimnames[talldim], collapse = '.'), dimnames[2])
    mat <- array(as.numeric(mat), dim = dim(mat), dimnames = dimnames(mat))
    
    
  } else {
    mat <- matrix(getValues(x), ncol = 1)
    rownames(mat) <- do.call('paste', c(getFactors(x), list(sep = '.')))
    names(dimnames(mat)) <- c(paste(dimnames, collapse = '.'), '')
    colnames(mat) <- type
  }
  
  mat
 
  mat
}

#' @export
setMethod('as.data.frame', 'distribution',
          function(x) {
  setNames(as.data.frame(x@.Data), colnames(x))
})

#' @export
as.data.table.distribution <- function(x) {
  as.data.table(as.data.frame(x))
}


### sorting ----

#' @rdname distributions
#' @export
setMethod('sort', 'distribution',
          function(x, decreasing = TRUE) {
  X <- getValues(x)
  
  x <- x[order(X, decreasing = decreasing), ]
  
  x@Sort <- if (decreasing) -1L else 1L
  x
})


### aligning ----

alignDistributions <- function(..., funcname = '') {
  
  dists <- list(...)
  dimnames <- lapply(dists, dimnames)
  if (length(Reduce(\(x, y) if (setequal(x, y)) x, dimnames)) == 0L) .stop("If using {funcname} on distributions, they must all have the same dimension names.")
  dimnames <- dimnames[[1]]
  
  
  levels <- setNames(lapply(dimnames, 
                            \(dn) do.call('mergeLevels', lapply(dists, '[[', dn))), 
                     dimnames)
  levels <- do.call('expand.grid', levels)

  
  aligned <- lapply(dists, 
                  \(dist) {
                    df <- merge(levels, as.data.frame(dist), all = TRUE)
                    
                    type <- dist_type(dist)
                    df[[type]] <-  ifelse(is.na(df[[type]]), 0, df[[type]])
                    
                    df
                    
                   
                    
                  })
  
  levels <- as.data.frame(aligned[[1]])[, dimnames, drop = FALSE] # not sure why, but the order has changed
  aligned <- lapply(aligned, getValues)
  names(aligned) <- sapply(dists, dist_type)
  
  list(Levels = levels, X = aligned)
}
  
mergeLevels <- function(...) {
  args <- list(...)
  newlev <- Reduce(union, args)
  
  mini <- do.call('pmin', c(lapply(args, 
                                   \(arg) {
                                     match(newlev, arg)
                                   }), 
                            list(na.rm = TRUE)))
  ord <- order(mini, newlev)
  newlev[ord]
}


#' @export
cbind.distribution <- function(...) {
  args <- list(...)
  dists <- sapply(args, inherits, what = 'distribution')
  
  aligned <- do.call('alignDistributions', args[dists])
  args[dists] <- aligned$X
  
  mat <- do.call('cbind', args)
  colnames(mat)[dists][colnames(mat)[dists] == ''] <- names(aligned$X)[colnames(mat)[dists] == '']

  rownames(mat) <- do.call('paste', c(aligned$Levels, list(sep = '.')))
  names(dimnames(mat)) <- c(paste(colnames(aligned$Levels), collapse = '.'), "")
  mat
}




### arithmetic ----

distmat <- function(factors, result, type = 'n') {
  if (inherits(factors, 'distribution')) factors <- getFactors(factors)
  mat <- matrix(result, ncol = 1)
  rownames(mat) <- do.call('paste', c(factors, list(sep = '.')))
  colnames(mat) <- type
  names(dimnames(mat)) <- c(paste(colnames(factors), collapse = '.'), '')
  mat
}

#' @export
setMethod('+', c('count', 'count'),
          function(e1, e2) {
            aligned <- alignDistributions(e1, e2, funcname = '+')
            
            df <- aligned$Levels
            df$N <- callGeneric(aligned$X[[1]], aligned$X[[2]])
            
            distribution(df, 'n')
          })

#' @export
setMethod('+', c('count', 'integer'),
          function(e1, e2) {
            df <- getFactors(e1)
            df$N <- callGeneric(getValues(e1), e2)
            
            distribution(df, 'n')
          })

#' @export
setMethod('Ops', c('distribution', 'distribution'),
          function(e1, e2) {
            aligned <- alignDistributions(e1, e2, funcname = .Generic)

            
            result <- callGeneric(aligned$X[[1]], aligned$X[[2]])
            distmat(aligned$Levels, result, dist_type(e1))
            
          })


#' @export
setMethod('Ops', c('distribution', 'numeric'),
          function(e1, e2) {
            
            result <- callGeneric(getValues(e1), e2)
            distmat(e1, result)

            
          })

#' @export
setMethod('Math', 'distribution',
          \(x) {
            distmat(x, callGeneric(getValues(x)))
          })

#' @export
setMethod('Summary', 'distribution',
          \(x, ..., na.rm = FALSE) {
            setNames(callGeneric(getValues(x), ..., na.rm), paste(dimnames(x), collapse = '.'))
          })


#' @export
setMethod('mean', 'distribution',
          \(x, ..., na.rm = FALSE) {
            
            setNames(mean(getValues(x), ..., na.rm), paste(dimnames(x), collapse = '.'))
          })

#' @export
setMethod('*', c('probability', 'probability'),
          \(e1, e2) {
            
            dimnames1 <- dimnames(e1)
            dimnames2 <- dimnames(e2)
            
            if (length(intersect(dimnames1, dimnames2))) return(callNextMethod(e1, e2))
            
            if (length(dimnames1) > 1L || length(dimnames2) > 1L) .stop("Can't cross product probability distributions with more than one dimenion yet.")
            
            p1 <- setNames(e1$p, getFactors(e1)[[1]])
            p2 <- setNames(e2$p, getFactors(e2)[[1]])
            
            jointp <- outer(p1, p2, '*')
            
            df <- as.data.frame(as.table(jointp))
            colnames(df) <- c(dimnames1, dimnames2, 'p')
            
            distribution(df, 'p', Condition = NULL, N = sum(e1@N, e2@N))
            
            
          })












## User Functions ----


### count() ----


#' Tabulate and/or cross-tabulate data
#' 
#' The `count()` function is exactly like R's fundamental [table()][base::table] function,
#' except that 1) will give special treatment to humdrumR [token()] data 2)
#' has more intuitive/simple argument names 3) makes it easier to combine/manipulate
#' disparate output tables.
#' 
#' @details
#' 
#' The `count()` function is essentially a wrapper
#' around [base::table()][base::table] function.
#' However, any [token()] class arguments are treated like [factors()],
#' calling generating their own levels.
#' This assures that, for example, pitch data is tabulated in order of pitch height,
#' and "missing" pitches are counted as zero.
#' 
#' `count()` will, by default, count `NA` values if they are present---if you don't want
#' to count `NA`s, specify `na.rm = TRUE`.
#' You can also tell `count()` to exclude (not count) any other arbitrary values you
#' provide as a vector to the `exclude` argument.
#' 
#' 
#' `count()` will always give names to the dimensions of the table it creates.
#' You can specify these names directly as argument names, like `count(Kern = kern(Token))`;
#' if you don't specify a name, `count()` will make up a name(s) based on expression(s) it is tallying.
#' (Note that `count()` does not copy [base::table()]'s obtusely-named `dnn` or `deparse.level` arguments.)

#' @section Manipulating humdrum tables:
#' 
#' The output of `count()` is a special form of R `table`, a `humdrumR.table`.
#' Given two or more `humdrumR.table`s, if you apply basic R operators 
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
#' genericTable   <- count(generic)
#' complexTable <- count(complex)
#' 
#' genericTable
#' complexTable
#' 
#' genericTable + complexTable
#' 
#' cbind(genericTable, complexTable)
#' 
#' @name distributions
#' @export 
count.default <- function(..., sort = FALSE, na.rm = FALSE,
                          .drop = FALSE, binArgs = list()) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  
  args <- list(...)
  
  # get the appropriate (dim)names for each argument
  exprs <- as.list(substitute(list(...)))[-1L]
  
  dimnames <- .names(args)
  if (any(dimnames == '')) dimnames[dimnames == ''] <- vapply(exprs[dimnames == ''], deparse, nlines = 1L, '')
  
  if (length(unique(lengths(args))) > 1L) .stop("Can't cross-tabulate these vectors ({harvard(dimnames, 'and')}), because they are different lengths.")
  
  # factorize arguments as needed
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) arg <-  factorize(arg) 
                   if (is.numeric(arg)) do.call('prettyBins', c(list(arg), binArgs)) else arg
                 })
  
  
  argdf <- as.data.frame(args)
  colnames(argdf) <- dimnames
  
  result <- rlang::eval_tidy(rlang::expr(count(argdf, !!!(rlang::syms(dimnames)), name = 'n', .drop = !!.drop)))
  
  if (na.rm) result <- result[Reduce('&', lapply(result[ , dimnames, drop = FALSE], \(col) !is.na(col))), , drop = FALSE]
  
  dist <- distribution(result, 'n')
  
  if (sort) sort(dist, decreasing = sort > 0L) else dist
  
  
  
}

#' @rdname distributions
#' @export
count.humdrumR <- function(x, ..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  quos <- rlang::enquos(...)
  counts <- if (length(quos)) {
    names(quos) <- sapply(quos, rlang::as_label)
    quo <- rlang::quo(with(x, count.default(!!!quos, sort = !!sort, na.rm = !!na.rm, .drop = !!.drop, binArgs = binArgs)))
    rlang::eval_tidy(quo)
    
  } else {
    fields <- pullFields(x, union(selectedFields(x), getGroupingFields(x)), null = 'asis')
    do.call('count.default', c(as.list(fields), list(sort = sort, na.rm = na.rm, .drop = .drop, binArgs = binArgs)))
  }
  
  counts
}



#' @rdname distributions
#' @export
count.table <- function(..., sort = FALSE,
                        na.rm = FALSE,
                        .drop = FALSE) {
  tab <- list(...)[[1]]
  type <- if (any(tab < 1 & tab > 0)) 'p' else 'n'
  df <- as.data.frame(tab, responseName = type)
  
  dist <- if (type == 'p') {
    distribution(df, type, N = sum(tab), Condition = NULL)
  } else {
    distribution(df, type)
  }
  
  
  if (sort) dist <- sort.distribution(dist, decreasing = sort > 0L)
  
  dist
  
}




### pdist() -----



#' Tabulate and cross proportions
#' 
#' 
#' @export
pdist <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  
  UseMethod('pdist')
}

#' @rdname distributions
#' @export
pdist.count <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  
  if (na.rm) x <- x[!Reduce('|', lapply(getFactors(x), is.na)), ]
  if (sort) x <- sort(x, sort > 0L)
  
  x <- as.data.frame(x)
  
  exprs <- rlang::enexprs(...)
  if (length(exprs)) condition <- pexprs(exprs, colnames(x), condition)$Condition %||% condition
  
  x$p <- if (is.null(condition)) {
    n <- sum(x$n, na.rm = TRUE)
    x$n / n
  } else {
    conditionvec <- do.call('paste', c(x[condition], list(sep = '.')))
    n <- c(tapply(x$n, conditionvec, \(x) as.integer(sum(x))))
    n <- n[unique(conditionvec)] # to match original order
    
    ifelse(n[conditionvec] == 0, 0, x$n / n[conditionvec])
  }
  
  x$n <- NULL
  
  distribution(x, 'p', N = n, Condition = condition)
}

#' @rdname distributions
#' @export
pdist.default <-  function(..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  exprs <- as.list(substitute(list(...)))[-1]
  args <- list(...)
  
  # get the appropriate (dim)names for each argument
  
  dimnames <- .names(args)
  if (any(dimnames == '')) dimnames[dimnames == ''] <- vapply(exprs[dimnames == ''], deparse, nlines = 1L, '')
  names(args) <- dimnames
  
  # if condition is given as separate vector
  conditionName <- deparse(substitute(condition), nlines = 1L)
  if (!is.null(condition) && is.atomic(condition) && length(condition) == length(args[[1]])) {
    args[[conditionName]] <- condition
    condition <- conditionName
  }
  
  
  count <- do.call('count.default', c(args, list(na.rm = na.rm, sort = sort, .drop = .drop, binArgs = binArgs)))
  
  pdist(count, condition = condition)
  
}

#' @rdname distributions
#' @export
pdist.data.frame <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
            exprs <- rlang::enexprs(...)
            
            if (length(exprs) == 0L) {
              args <- as.list(x)
            } else {
              parsed <- pexprs(exprs, colnames(x), condition)
              condition <- parsed$Condition 
              
              args <- rlang::eval_tidy(rlang::quo(list(!!!(parsed$Exprs))), data = x)
              names(args) <- names(parsed$Exprs)
            }
            
            
            do.call('pdist', c(args, list(condition = condition, na.rm = na.rm, sort = sort, .drop = .drop, binArgs = binArgs)))
            
          }


pexprs <- function(exprs, colnames, condition) {
  
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    
    exprs[[i]] <-  switch(class(expr),
           integer = ,
           numeric = rlang::sym(colnames[expr]),
           character = rlang::sym(expr),
           formula = ,
           call = {
             if (as.character(expr[[1]]) %in% c('|', '~')) {
               
               if (rlang::is_symbol(expr[[3]])) condition <- as.character(expr[[3]]) 
               
               exprs[[length(exprs) + 1L]] <- expr[[3]]
               expr[[2]] 
               
             } else {
               expr
             }
           },
           expr)
   
  }
  
  names(exprs) <- sapply(exprs, rlang::as_label)
  exprs <- exprs[!duplicated(names(exprs))]
  
  list(Exprs = exprs, Condition = condition)
}




#' @rdname distributions
#' @export 
pdist.humdrumR <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
            
            exprs <- rlang::enexprs(...)
            
            humtab <- getHumtab(x, 'D')
            if (length(exprs)) {
              
              rlang::eval_tidy(rlang::expr(pdist(humtab, !!!exprs, condition = !!condition, na.rm = !!na.rm, sort = !!sort, .drop = !!.drop, binArgs = !!binArgs)))
            } else {
              selectedFields <- selectedFields(x)
              names(selectedFields) <- selectedFields
              
              do.call('pdist', 
                      c(list(humtab), 
                        as.list(selectedFields),
                        list(condition = condition, na.rm = na.rm, sort = sort, .drop = .drop, binArgs = binArgs)))
              
            }
}




#' @rdname distributions
#' @export
pdist.table <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, binArgs = list()) {
            
            if (length(dim(x)) == 1L) condition <- NULL
            
            if (na.rm) {
              notna <- unname(lapply(dimnames(x), \(dim) !is.na(dim)))
              x <- do.call('[', c(list(x), notna))
            }
            
            if (is.character(condition)) {
              condition <- pmatch(condition, dimnames(x), duplicates.ok = FALSE)
              condition <- condition[!is.na(condition)]
              if (length(condition) == 0L) condition <- NULL
            }
            
            ptab <- proportions(x, margin = condition) 
            
            
            N <- marginSums(x, margin = condition)
            
            distribution(N, 'p')
            # new('probability.frame', ptab, N = as.integer(n), margin =  as.integer(condition))
          }



            
unmargin <- function(dist) {
  if (is.null(dist@Condition)) return(dist)
  
  
  N <- sum(dist@N)
  margins <- dist@N / N
  
  dist$p <- dist$p * margins[paste(dist[[dist@Condition]])] # paste to deal with NA levels
  
  
  dist@Condition <- NULL
  dist@N <- N
  dist
 
  
}






# Information Theory ----

## Likelihoods ----

###likelihood() ----

#' @export
setGeneric('likely', function(x, log, ...) standardGeneric('likely'))

#' @rdname entropy
#' @export
setGeneric('ic', function(x, ...) standardGeneric('ic'))

## Distributional ----


### entropy() ----

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
entropy <- function(..., base = 2) {
  checks(base, xnumber & xpositive)
  
  UseMethod('entropy')
}


#' @rdname entropy
#' @export
H <- entropy
  
#' @rdname entropy
#' @export
entropy.probability <-  function(x, base = 2) {
            
            joint <- unmargin(x)$p
            
            other <- ifelse(x$p > 0L, log(x$p, base = base), 0) 
            
            equation <- Pequation(x, 'H')
            
            setNames(-sum(joint * other), equation)
          }


#' @rdname entropy
#' @export
entropy.numeric <- function(x, base = 2, na.rm = TRUE) {
  entropy(density(x, ...), base = base, na.rm = na.rm)
}


#' @rdname entropy
#' @export
entropy.density <- function(x, base = 2, na.rm = TRUE) {
            label <- rlang::expr_name(rlang::enexpr(x))
            if (any(is.na(x))) return(NA_real_)
            
            dx <- diff(x$x[1:2])
            
            equation <- paste0('H(', label, ')')
            
            setNames(-sum(log(x$y, base = base) * x$y * dx), equation)
            
            
          }


#' @rdname entropy
#' @export
entropy.default <- function(..., base = 2) {
            entropy(pdist(...), base = base)
          }


entropies <- function(..., base = 2, conditional = FALSE) {
  args <- list(...)
  
  names <- .names(args)
  names <- ifelse(names == '', sapply(substitute(list(...))[-1], deparse, nlines = 1L), names)
  names(args) <- names
  
  mat <- matrix(NA_real_, nrow = length(args), ncol = length(args))
  
  indices <- expand.grid(i = seq_along(args), j = seq_along(args))
  if (!conditional) indices <- subset(indices, j >= i)
  
  
  for (k in 1:nrow(indices)) {
    i <- indices$i[k]
    j <- indices$j[k]
    

    
    mat[i, j] <- if (i == j) {
      entropy(args[[i]], base = base)
    } else {
      do.call('entropy', c(args[c(i, j)], list(condition = if (conditional) names[j])))
    }
  }
  
  colnames(mat) <- rownames(mat) <- names
  mat
}


### mutualInfo() ----

#' Calculate Entropy or Information Content of variables 
#'
#' 
#' @family {Information theory functions} 
#' @rdname entropy
#' @export
mutualInfo <- function(..., base = 2) {
  checks(base, xnumber & xpositive)
  
  UseMethod('mutualInfo')
}



#' @rdname entropy
#' @export
mutualInfo.probability <-  function(x, base = 2) {
  dimnames <- dimnames(x)
  if (length(dimnames) != 2L) .stop("Can't calculate mutual information of a single variable.")
  
  x <- unmargin(x)
  
  independentjoint <- (x[ , dimnames[1]] * x[ , dimnames[2]])
  
  ratio <- x / independentjoint
  logratio <- ifelse(ratio == 0 | ratio == Inf, 0, log(ratio, base = base))
  
  joint <- setNames(x$p, do.call('paste', c(getFactors(x), list(sep = '.'))))
  joint <- joint[rownames(logratio)]
  
  equation <- Pequation(x, 'I', ';')
  
  setNames(sum(joint * logratio, na.rm = TRUE), equation)
  
}




#' @rdname entropy
#' @export
mutualInfo.default <- function(..., base = 2) {
  mutualInfo(pdist(...), base = base)
}





  


#' Calculate cross entropy between two distributions
#' 
#' TBA
#' @family {Information theory functions}
#' @export
setGeneric('crossEntropy', function(distribution1, distribution2, ...) standardGeneric('crossEntropy'))

#' @rdname crossEntropy
#' @export
setMethod('crossEntropy', c('probability.frame', 'probability.frame'),
          function(distribution1, distribution2, base = 2) {
            
            distribution2 <- ifelse(distribution2 == 0L, 0, log(distribution2, base = base))
            
            
            equation <- paste0('H(', 
                               pdist.name(distribution1, func = ''), ', ',
                               pdist.name(distribution2, func = ''))
            setNames(-sum(distribution1 * distribution2), equation)
            
          })


###################################################
# table() extensions for humdrumR ---- ###########
##################################################



humdrumR.table <- function(tab) {
  class(tab) <- unique(c('humdrumR.table', 'table', class(tab)))
  tab
}


#' @export
cbind.humdrumR.table <- function(...) {
  args <- list(...)
  classes <- unique(lapply(args, class))
  
  table.i <- which(sapply(args, is.table))
  tables <- args[table.i]
  tables <- lapply(tables, \(tab) if (length(dim(tab)) == 1) t(t(tab)) else tab)
  colnames <- unlist(lapply(tables, \(tab) if (is.null(colnames(tab))) rep('', ncol(tab)) else colnames(tab)))
  tables <- lapply(tables, `colnames<-`, value = NULL)
  tables <- if (length(tables) > 1L) alignTables(tables, 'cbind', margin = 1) else lapply(tables, unclass)
  
  args[table.i] <- tables
  newtab <- do.call('cbind', args)
  
  argnames <- rep(.names(args), sapply(args, ncol))
  colnames(newtab) <- ifelse(argnames == '', colnames, argnames)
  
  class(newtab) <- if (length(classes) == 1L) classes[[1]] else c('humdrumR.table', 'table')
  newtab
}

#' @export
rbind.humdrumR.table <- function(...) {
  args <- list(...)
  table.i <- which(sapply(args, is.table))
  tables <- args[table.i]
  
  tables <- lapply(tables, \(tab) if (length(dim(tab)) == 1) t(tab) else tab)
  rownames <- unlist(lapply(tables, \(tab) if (is.null(rownames(tab))) rep('', nrow(tab)) else rownames(tab)))
  tables <- lapply(tables, `rownames<-`, value = NULL)
  tables <- alignTables(tables, 'rbind', margin = 2) 
  
  args[table.i] <- tables
  newtab <- do.call('rbind', args)
  
  argnames <- rep(.names(args), sapply(args, \(arg) if (length(dim(arg)) == 1L) 1 else nrow(arg)))
  rownames(newtab) <- ifelse(argnames == '', rownames, argnames)
  
  class(newtab) <- 'table'
  newtab
  
  
}



#' @export
as.table.distribution <- function(x) {
  levels <- lapply(getFactors(x), unique)
  dimnames <- dimnames(x)
  type <- dist_type(x)
  
  x <- as.data.frame(x)
  x[dimnames] <- lapply(x[dimnames], paste) # forces NA to be strings
  
  tab <- array(dim = lengths(levels), dimnames = lapply(levels, paste))
  tab[do.call('cbind', x[dimnames])] <- x[[type]]
  dimnames(tab) <- levels
  
  humdrumR.table(tab)
  
}

alignTables <- function(tables, funcname = '', margin = NULL) {
  tables <- lapply(tables, 
                   \(tab) {
                     dn <- dimnames(tab)
                     null <- sapply(dn, is.null)
                     dn[null] <- lapply(dim(tab)[null], \(n) paste0(seq_len(n), '___'))
                     dimnames(tab) <- lapply(dn, \(names) ifelse(is.na(names), '_<NA>_', names) ) 
                     tab})
  olddimnames <- lapply(tables, dimnames)
  dimensions <- lapply(olddimnames, .names)
  dimensions <- Reduce(\(a, b) ifelse(a == b, a, paste(a, b, sep = if (nchar(funcname) == 1) funcname else '/')), dimensions)
  if (length(unique(lengths(olddimnames))) > 1L) .stop("If using {funcname} on tables, they must all have the same number of dimensions.")
  
  dimnames <- as.list(as.data.frame(do.call('rbind', olddimnames)))
  if (is.null(margin)) margin <- seq_along(dimnames)
  
  # dimnames[ margin] <- lapply(dimnames[ margin], \(dim) rep(list(stringr::str_sort(Reduce('union', dim), numeric = TRUE)), length(dim)))
  dimnames[ margin] <- lapply(dimnames[margin], 
                              \(dim) {
                                uniondim <- Reduce('union', dim)
                                
                                # get order
                                uniondim <- uniondim[order(do.call('pmin', c(list(na.rm = TRUE), lapply(dim, \(d) match(uniondim, d)))),
                                                           uniondim)]
                                
                                rep(list(uniondim), length(dim))
                                
                              })
  
  
  dimnames <- as.data.frame(do.call('rbind', dimnames))
  rownames(dimnames) <- dimensions
  
  empties <- lapply(dimnames, \(dn) array(data = 0L, dim = lengths(dn), dimnames = dn))
  
  Map(tables, empties,
      f = \(tab, empty) {
        
        # indices <- as.matrix(do.call('expand.grid', c(dimnames(tab), list(stringsAsFactors = FALSE))))
        indices <- rep(list(rlang::missing_arg()), length(dim(tab)))
        indices[margin] <- dimnames(tab)[margin]
        empty[] <- do.call('[<-', c(list(empty), indices, list(value = tab)))
        # empty[indices] <- tab[indices]
        
        dimnames(empty) <- lapply(dimnames(empty), 
                                  \(names) {
                                    if (!any(grepl('[0-9]___', names))) ifelse(names == '_<NA>_', NA_character_, names)
                                  })
        empty
      }) 
  
}


### table() -----

deparse.names <- function(exprs, deparse.level = 1L) {
  # exprs <- as.list(substitute(list(...)))[-1L]
  # if (length(exprs) == 1L && is.list(..1) && !is.null(nm <- names(..1))) 
  # return(nm)
  vapply(exprs, \(x) switch(deparse.level + 1, 
                            "", 
                            if (is.symbol(x)) as.character(x) else "",
                            deparse(x, nlines = 1)[1L]),
         "")
}

#' @rdname distributions
#' @export
setGeneric('table', signature = 'x',
           def = function(x, ..., exclude = if (useNA == 'no') c(NA, NaN), useNA = 'no', dnn = NULL, deparse.level = 1) {
             # this is an approach to making base::table generic, but dispatched on a single x argument.
             # this is necessary to make it so we can dispatch on x=humdrumR with non-standard evaluation of ... 
             # the hope is that this function behaves exactly like base::table (for non humdrumR classes), but its tricky to achieve.
             # table(x = ..., x=...) will cause an error that doesn't appear in base::table.
             args <- list(...)
             
             if (missing(x)) {
               if (is.null(dnn)) dnn <- character(length(args))
               dnn <- ifelse(dnn == '', names(args), dnn)
               names(args)[1] <- 'x'
               return(do.call('table', 
                              c(args, 
                                list(exclude = exclude, useNA = useNA, dnn = dnn, deparse.level = deparse.level))))
             }
             exprs <- sys.call()[-1]
             exprs <- exprs[!.names(exprs) %in% c('exclude', 'useNA', 'dnn', 'deparse.level', '.drop', 'na.rm')]
             names <- .names(exprs)
             if (!any(names == 'x')) names[which(names == '')[1]] <- 'x'
             
             
             args <- append(args, list(x = x), which(names == 'x')[1] - 1) # put x into right position (argument order)
             
             
             if (is.null(dnn)) dnn <- character(length(args))
             dnn <- ifelse(dnn == '', names(args), dnn)
             dnn <- ifelse(dnn == '' | (dnn == 'x' & .names(exprs) != 'x'), deparse.names(exprs, deparse.level), dnn)
             
             token <- any(sapply(args, inherits, what = 'token'))
             args <- lapply(args, factorize)
             
             tab <- do.call(base::table, c(args, list(exclude = exclude, useNA = useNA, dnn = dnn, deparse.level = deparse.level)))
             
             tab
           }) 




#' @export
#' @rdname distributions
setMethod('table', 'humdrumR', 
          function(x, ..., exclude = if (useNA == 'no') c(NA, NaN),
                   useNA = 'no', dnn = NULL,
                   deparse.level = 1) {
            
            quos <- rlang::enquos(...)
            tab <- if (length(quos)) {
              dnn <- .names(quos)
              dnn[dnn == ''] <- deparse.names(quos[dnn == ''], deparse.level = deparse.level)
              quo <- rlang::quo(with(x, table(!!!quos, exclude = exclude, useNA = !!useNA, dnn = dnn)))
              rlang::eval_tidy(quo)
              
              
            } else {
              fields <- pullFields(x, union(selectedFields(x), getGroupingFields(x)))
              
              do.call('table', c(as.list(fields), list(exclude = exclude, useNA = useNA, dnn = dnn, deparse.level = deparse.level)))
            }
            
            humdrumR.table(tab)
            
          })

#' @export
#' @rdname distributions
setMethod('table', 'distribution',
          function(x) {
            as.table(x)
          })
