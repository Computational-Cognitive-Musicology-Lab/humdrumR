
setClassUnion('discrete', c('character', 'factor', 'logical', 'integer', 'token'))

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

distribution <- function(x, type, Sort = 0L, N = 0L, Condition = NULL) {
  df <- as.data.frame(x)
  args <- list(new('distribution', df, Sort = Sort))
  
  if (class(type) != 'character') {
    
    if (class(type)[1] == 'probability') {
      N <- type@N
      Condition <- type@Condition
      type <- 'p'
    }  else {
      type <- class(type)[1]
    }
   
  }
    
  if (type == 'p') {
    new('probability', df, N = N, Condition = Condition, Sort = Sort)
  } else {
    new('count', df, Sort = Sort)
  }

}
  



### Accessors ----


dist_type <- function(dist) intersect(colnames(dist), c('n', 'p'))
getValues <- function(dist) as.data.frame(dist)[ , dist_type(dist)]
getLevels <- function(dist) as.data.frame(dist)[ , varnames(dist), drop = FALSE]


#' @export
varnames <- function(x) setdiff(colnames(x), c('n', 'p'))


#' @export
levels.distribution <- function(x) getLevels(x)

### print() ----

#' @export
#' @rdname distributions
setMethod('show', 'distribution', \(object) print.distribution(object))

#' @export
#' @rdname distributions
print.distribution <- function(dist, digits = if (inherits(dist, 'probability')) 3 else 1,
                               syntaxHighlight = humdrumRoption('syntaxHighlight'),
                               wide = TRUE,
                               printZeros = TRUE,
                               zeros = '.') {
  
  type <- dist_type(dist)
  message <- paste0('humdrumR ', 
                    if (type == 'p') paste0('probability distribution ', Pequation(dist)) else 'count distribution')
  
  varnames <- varnames(dist)
  
  if (nrow(dist) == 0) {
    message <- paste0(message, ' (empty)')
    if (syntaxHighlight) message <- syntaxHighlight(message, 'G')
    cat(message, '\n')
    return(invisible(dist))
  }
  sort <- dist@Sort
  
  X <- getValues(dist)
  zero <- X == 0
  
  X <- if (type == 'p') prettyP(X, digits = digits, zeros = zeros) else prettyN(X, digits = digits, zeros = zeros)
  
  # do we scale or round?
  scale <- attr(X, 'scale')
  
  scale <- if (length(scale) == 1L) {
    if (scale > 0) paste0(' (', c('thousands', 'millions', 'billions', 'trillions')[scale], ')')
  }
  
  if (attr(X, 'negative') %||% FALSE) scale <- paste0(scale, ' (∃N < 0)')
  if (attr(X, 'approx')) scale <- paste0(scale, ', ~rounded')
  message <- paste0(message, scale)
  
  dist <- getLevels(dist)
  dist[[type]] <- X
  
  # check if we can widen
  iswide <- FALSE
  printmat <- (if(wide && sort == 0L && length(varnames) >= 2L) {
    
    wide <- as.matrix(dcast(as.data.table(dist), rlang::new_formula(quote(...), rlang::sym(varnames[2])), fill = attr(X, 'zerofill'), value.var = type))
    
    factorcols <- colnames(wide) %in% varnames
    toprow    <- ifelse( factorcols, colnames(wide), '')
    toprow[length(varnames)] <- varnames[2]
    secondrow <- ifelse(!factorcols, colnames(wide), '')
    
    wide <- rbind(toprow, secondrow, wide, secondrow, toprow)
    wide <- apply(wide, 2, format, justify = 'right')
    wide <- wide[ , order(match(toprow, colnames(dist), nomatch = 2))]
    
    width <- sum(nchar(wide[1, ])) + ncol(wide) * 2
    
    iswide <- width < (options('width')$width - 10L)
    if (iswide) wide
  })  %||%  apply(rbind(colnames(dist), 
                        as.matrix(if (printZeros) dist else dist[!zero, , drop = FALSE]), 
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
    
    dataCols <- !colnames(printmat) %in% c(varnames, 'Rank')
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
  varnames <- varnames(dist)
  
  condition <- dist@Condition 
  
  eq <- paste(setdiff(varnames, condition), collapse = collapse)
  if (!is.null(condition)) {
    condition <- paste(condition, collapse = collapse)
    eq <- paste0(eq, '|', condition)
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
  
  attr(output, 'zerofill') <- paste0(' ', zeros, strrep(' ', max(0, maxnchar - 2L)))
  attr(output, 'scale') <- if (scale != 0L) paste0('10^(', -scale * 3, ')')
  attr(output, 'approx') <- any(approx)
  
  output
  
}


prettyBins <- function(x, maxN = 20, quantiles = 0, right = TRUE, ...) {
  checks(maxN, xlen1 & xpositive & xwholenum, argname = 'binArgs(maxN = )', seealso = '?count')
  checks(quantiles, xlen1 & xpositiveorzero, argname = 'binArgs(quantiles = )', seealso = '?count')
  checks(right, xTF, argname = 'binArgs(right = )', seealso = '?count')
  
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

#### distribution[] ----

# 
setMethod('[', c('distribution', 'atomic', 'missing'),
          function(x, i, drop = FALSE) {

            
            df <- as.data.frame(x)[i, ]
            
            if (drop) df else distribution(df, x)
            
          })

# 
setMethod('[', c('distribution', 'missing', 'atomic'),
          function(x, i, j, drop = FALSE) {
            

            # prepare j
            if (is.logical(j)) j <- which(j)
            if (is.numeric(j)) j <- varnames[j] 
            
            if (length(setdiff(j, varnames)) || any(is.na(j)))  .stop("{harvard(setdiff(j ,varnames), 'and', quote = TRUE)} <is not a name|are not names> of",
                                                                      '<any dimension|dimensions> in this distribution.', ifelse = length(setdiff(j, varnames)) == 1)
            j <- intersect(j, varnames)
            
            # do indexing
            type <- dist_type(x)
            dt <- as.data.table(x)[ , setNames(list(sum(get(type))), type), by = j]
            
            if (drop) as.data.frame(dt) else distribution(dt, x)
            

          })

setMethod('[', c('distribution', 'ANY', 'ANY'),
          function(x, i, j, drop = FALSE) {
            x[ , c(i, j), drop = drop]
          })

#### distribution[[]] ----

setMethod('[[', c('distribution', 'matrix'),
          function(x, i, j, cartesian = TRUE, drop = FALSE) {
            
            i <- as.data.frame(i)
            names(i) <- NULL
            
            do.call('[[', c(list(x, cartesian = cartesian, drop = drop), i))
            
          })

#' @export
setMethod('[[', 'distribution',
          function(x, i, j, ..., cartesian = FALSE, drop = FALSE) {
            varnames <- varnames(x)
            args <- setNames(vector('list', length(varnames)), varnames)
            
            # prep args
            ldots <- list(...)
            
            named <- ldots[.names(ldots) != '']
            if (any(duplicated(names(named)))) {
              dups <- names(named)[duplicated(names(named))]
              .stop("You have named multiple indexes for the {harvard(dups, 'and', quote = TRUE)} variable<|s>.", ifelse = length(dups) > 1)
              }
            if (length(setdiff(names(named), varnames))) .stop("{harvard(setdiff(names(named) ,varnames), 'and', quote = TRUE)} <is not the name|are not names> of",
                                                               '<any dimension|dimensions> in this distribution.', ifelse = length(setdiff(names(named), varnames)) == 1)
            
            unnamed <- c(list(if (!missing(i)) i), list(if (!missing(j)) j), ldots[.names(ldots) == ''])
            unnamed <- Filter(Negate(is.null), unnamed)
            if ((length(named) + length(unnamed)) > length(args)) .stop("This distribution only has {num2print(length(varnames))} dimensions to index.",
                                                                        "You have provided {num2print((length(named) + length(unnamed)))}.")
            
            args[names(named)] <- named
            rest <- head(which(!varnames %in% names(named)), length(unnamed))
            args[rest] <- unnamed
            
            missing <- sapply(args, is.null)
            
            # do indexing
            levels <- getLevels(x)
            
            
            args[!missing] <- Map(
              \(arg, lev) {
                if (is.numeric(arg)) unique(lev)[arg] else arg
              } ,
              args[!missing], levels[!missing])
            
            i <- if (cartesian) {
              matches(args[!missing], levels[!missing])
              
            } else {
              Reduce('&', Map(`%in%`, levels[!missing], args[!missing]))
            }

            df <- as.data.frame(x)[i , , drop = FALSE]
            
            if (drop) df else distribution(df, x) 
              
              # output <- distribution(df, x)
              # output@names <- ifelse(output@names %in% varnames(x)[!missing] & !grepl('\\[\\]$', output@names), 
                                     # paste0(output@names, '[]'), output@names)
              # output


          })

#### probability[] ----

setMethod('[', c('probability', 'atomic', 'missing'),
          function(x, i, drop = FALSE) {
            
            
            output <- callNextMethod()
            
            output <- recomputeP(output, x)
            
            if (drop) as.data.frame(output) else output
            
          })


#' @export
setMethod('[', c('probability', 'missing', 'atomic'),
          function(x, i, j, ..., cartesian = FALSE, drop = FALSE) {

            varnames <- varnames(x)
            # prepare j
            if (is.logical(j)) j <- which(j)
            if (is.numeric(j)) j <- varnames[j] 
            
            if (length(setdiff(j, varnames)) || any(is.na(j)))  .stop("{harvard(setdiff(j ,varnames), 'and', quote = TRUE)} <is not a name|are not names> of",
                                                                      '<any dimension|dimensions> in this distribution.', ifelse = length(setdiff(j, varnames)) == 1)
            j <- intersect(j, varnames)
            
            # do indexing
            condition <- intersect(x@Condition, j)
            x <- unconditional(x)
            
            dt <- as.data.table(x)[ , list(p = sum(p)), by = j]

            output <- distribution(as.data.frame(dt), x)
            
            if (length(condition)) conditional(output, condition) else output

          })

#### probability[[]] ----

#' @export
setMethod('[[', 'probability',
          function(x, i, j, ..., cartesian = FALSE, drop = FALSE) {
            
            output <- callNextMethod(x, i, j, ..., cartesian = cartesian, drop = FALSE)
           
            if (drop) as.data.frame(output) else recomputeP(output, x)
          })


setMethod('[[', c('probability', 'matrix'),
          function(x, i, j, cartesian = TRUE, drop = FALSE) {
            
            i <- as.data.frame(i)
            names(i) <- NULL
            
            do.call('[[', c(list(x, cartesian = cartesian, drop = drop), i))
            
          })

recomputeP <- function(newx, x) {
  
  if (is.null(x@Condition)) {
    newx@N <- as.integer(round(x@N * sum(newx)))
    newx$p <- newx$p / sum(newx$p)
    
  } else {
    newN <- as.integer(round(newx@N * tapply(newx$p, as.data.frame(newx)[, x@Condition, drop = FALSE], sum)[names(newx@N)]))
    newx@N <- setNames(ifelse(is.na(newN), 0L, newN), names(newx@N))
    
    newx$p <- tapply_inplace(newx$p, as.data.frame(newx)[, x@Condition, drop = FALSE], \(x) if (all(x == 0)) x else x / sum(x, na.rm = TRUE))
    
    newx
  }
  
  newx
}

#### filter() ----

#' @export
filter.distribution <- function(.data, ..., drop = FALSE) {
  exprs <- rlang::enquos(...)
  .data[rlang::eval_tidy(exprs[[1]], data = as.data.frame(.data)), , drop = drop]
}

### coercion ----

#' @export
as.matrix.distribution <- function(x, wide = TRUE, ...) {
  varnames <- varnames(x)
  type <- dist_type(x)
  
  if (length(varnames) >= 2L && wide && x@Sort == -0L) {
    mat <- as.matrix(dcast(as.data.table(x), 
                    rlang::new_formula(quote(...), rlang::sym(varnames[2])), 
                    fill = 0, value.var = type))
    
    talldim <- colnames(mat) %in% varnames 
    
    rownames(mat) <- do.call('paste', c(as.data.frame(mat[ , talldim, drop = FALSE]), list(sep = '.')))
    mat <- mat[ , !talldim, drop = FALSE]
    names(dimnames(mat)) <- c(paste(varnames[talldim], collapse = '.'), varnames[2])
    mat <- array(as.numeric(mat), dim = dim(mat), dimnames = dimnames(mat))
    
    
  } else {
    mat <- matrix(getValues(x), ncol = 1)
    rownames(mat) <- do.call('paste', c(getLevels(x), list(sep = '.')))
    names(dimnames(mat)) <- c(paste(varnames, collapse = '.'), '')
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
  df <- as.data.frame(x)[order(X, decreasing = decreasing), ]
  
  distribution(df, x, Sort = if (decreasing) -1L else 1L)
  
})


### aligning ----

alignDistributions <- function(..., funcname = '') {
  
  dists <- list(...)
  varnames <- lapply(dists, varnames)
  if (length(Reduce(\(x, y) if (setequal(x, y)) x, varnames)) == 0L) .stop("If using {funcname} on distributions, they must all have the same dimension names.")
  varnames <- varnames[[1]]
  
  
  levels <- setNames(lapply(varnames, 
                            \(dn) do.call('mergeLevels', lapply(dists, \(dist) as.data.frame(dist)[[dn]]))), 
                     varnames)
  
  levels <- do.call('expand.grid', c(levels, list(KEEP.OUT.ATTRS = FALSE)))
  
  aligned <- lapply(dists, 
                  \(dist) {
                    df <- merge(levels, as.data.frame(dist), all = TRUE)
                    
                    df <- df[matches(levels, df[,varnames, drop = FALSE]), , drop = FALSE] # put in right order
                    
                    type <- dist_type(dist)
                    ifelse(is.na(df[[type]]), 0, df[[type]])
                    
                  })
  
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
  if (inherits(factors, 'distribution')) factors <- getLevels(factors)
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
            df$n <- callGeneric(aligned$X[[1]], aligned$X[[2]])
            distribution(df, 'n')
          })

#' @export
setMethod('+', c('count', 'integer'),
          function(e1, e2) {
            df <- getLevels(e1)
            df$n <- callGeneric(getValues(e1), e2)
            
            distribution(df, 'n')
          })

#' @export
setMethod('Ops', c('distribution', 'distribution'),
          function(e1, e2) {
            aligned <- alignDistributions(e1, e2, funcname = .Generic)

            
            callGeneric(aligned$X[[1]], aligned$X[[2]])
            # distmat(aligned$Levels, result, dist_type(e1))
            
          })


#' @export
setMethod('Ops', c('distribution', 'numeric'),
          function(e1, e2) {
            
            callGeneric(getValues(e1), e2)
            # distmat(e1, result)

            
          })

#' @export
setMethod('Math', 'distribution',
          \(x) {
            distmat(x, callGeneric(getValues(x)))
          })

#' @export
setMethod('Summary', 'distribution',
          \(x, ..., na.rm = FALSE) {
            setNames(callGeneric(getValues(x), ..., na.rm), paste(varnames(x), collapse = '.'))
          })


#' @export
setMethod('mean', 'distribution',
          \(x, ..., na.rm = FALSE) {
            setNames(mean(getValues(x), ..., na.rm = na.rm), paste(varnames(x), collapse = '.'))
          })

#' @export
setMethod('median', 'distribution',
          \(x, na.rm = FALSE, ...) {
            setNames(median(getValues(x), na.rm = na.rm, ...), paste(varnames(x), collapse = '.'))
          })

#' @export
setMethod('rowSums', 'distribution',
          \(x, na.rm = FALSE, ...) {

            x[ , 1]

          })

#' @export
setMethod('colSums', 'distribution',
          \(x, na.rm = FALSE, ...) {
            x[, 2]
            
          })


#' @export
setMethod('*', c('probability', 'probability'),
          \(e1, e2) {
            
            varnames1 <- varnames(e1)
            varnames2 <- varnames(e2)
            
            if (length(intersect(varnames1, varnames2))) return(callNextMethod(e1, e2))
            
            if (length(varnames1) > 1L || length(varnames2) > 1L) .stop("Can't cross product probability distributions with more than one dimenion yet.")
            
            p1 <- setNames(e1$p, getLevels(e1)[[1]])
            p2 <- setNames(e2$p, getLevels(e2)[[1]])
            
            jointp <- outer(p1, p2, '*')
            
            df <- as.data.frame(as.table(jointp))
            colnames(df) <- c(varnames1, varnames2, 'p')
            
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
#'
#' @section Counting numeric values:
#' 
#' For numeric values, if there are many unique numbers to count we often want to count ranges of numbers in bins,
#' like in a histrogram.
#' By default, if you pass a vector of numbers to `count()` which has more than `20` unique values,
#' `count()` will bin the values using the same algorithm as [graphics::hist()].
#' This process can be controlled using the `binArgs` argument, which is itself a list of control arguments.
#' `binArgs = list(maxN = N)` controls the number of unique numbers needed before binning occurs,
#' and `binArgs = list(right = FALSE)` (default is `TRUE`) can be used to make bins that are closed on the right instead of the left.
#' Finally, any arguments to [graphics::hist()] can be passed via `binArgs`, controlling how binning occurs: notably,
#' you can use the `binArgs = list(breaks = _)` to control exactly where boundaries should occur, or the number of bins you want.
#' For example, `binArgs = list(breaks = 10)` will make `count()` bin the input numbers into twelve bins (see [hist()] 
#' for details).
#'
#' Alternatively, you can tell `count()` to divy up (bin) the input numbers into quantiles by 
#' passing `binArgs = list(quantiles = N)`.
#' For example, `binArgs = list(quantiles = 4)` will divide the data into four equal quantiles (0%-25%, 25%-50%, 50%-75%, 75%-100%).
#' 
#'
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
  checks(binArgs, xclass('list'))
  
  args <- list(...)
  
  # get the appropriate (dim)names for each argument
  exprs <- as.list(substitute(list(...)))[-1L]
  
  varnames <- .names(args)
  if (any(varnames == '')) varnames[varnames == ''] <- vapply(exprs[varnames == ''], deparse, nlines = 1L, '')
  
  if (length(unique(lengths(args))) > 1L) .stop("Can't cross-tabulate these vectors ({harvard(varnames, 'and')}), because they are different lengths.")
  
  # factorize arguments as needed
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) arg <-  factorize(arg) 
                   if (is.numeric(arg)) do.call('prettyBins', c(list(arg), binArgs)) else arg
                 })
  
  
  argdf <- as.data.frame(args)
  colnames(argdf) <- varnames
  
  if (.drop) argdf[sapply(argdf, is.factor)] <- lapply(argdf[sapply(argdf, is.factor)], factor) # drops unused levels
  result <- do.call('table', c(list(useNA = if (na.rm) 'no' else 'ifany'), 
                               argdf[ , varnames, drop = FALSE])) |> as.data.frame(responseName = 'n')
  
  # if (na.rm) result <- result[Reduce('&', lapply(result[ , varnames, drop = FALSE], \(col) !is.na(col))), , drop = FALSE]
  
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
count.table <- function(..., sort = FALSE, na.rm = FALSE, .drop = FALSE) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  
  tab <- list(...)[[1]]
  if (any(tab != round(tab))) {
    tab <- tab *  10^-floor(log10(min(tab[tab > 0])))
  }
  
  
  
  df <- as.data.frame(tab, responseName = 'n')
  df$n <- as.integer(df$n)
  
  if (.drop) df <- df[dist$n > 0, , drop = FALSE]
  
  dist <- distribution(df, 'n')

  if (na.rm) dist <- dist[Reduce('&', lapply(getLevels(dist), \(col) !is.na(col))), ]
  if (sort) dist <- sort(dist, decreasing = sort > 0L)
  
  dist
  
}


#' @rdname distributions
#' @export
count.pdist <- function(x, ..., sort = FALSE,
                        na.rm = FALSE,
                        .drop = FALSE) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  
  
  df <- as.data.frame(x)
  
  if (length(x@Condition)) {
    vars <- do.call('paste', c(df[ , x@Condition, drop = FALSE], list(sep = '.')))
    df$n <- round(df$p * df@N[vars])
    
  } else {
    df$n <- round(df$p * x@N)
  }
  df$p <- NULL
  
  dist <- distribution(df, 'n')
  
  if (na.rm) dist <- dist[Reduce('&', lapply(getLevels(dist), \(col) !is.na(col))), ]
  if (sort) dist <- sort(dist, decreasing = sort > 0L)
  
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
  checks(binArgs, xclass('list'))
  
  UseMethod('pdist')
}

#' @rdname distributions
#' @export
pdist.count <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  
  if (na.rm) x <- x[!Reduce('|', lapply(getLevels(x), is.na)), ]
  if (sort) x <- sort(x, sort > 0L)
  
  df <- as.data.frame(x)
  
  exprs <- rlang::enexprs(...)
  if (length(exprs)) condition <- pexprs(exprs, colnames(df), condition)$Condition %||% condition
  
  n <- sum(df$n, na.rm = TRUE)
  
  df$p <- df$n / n
  df$n <- NULL
  
  dist <- distribution(df, 'p', N = n)
  if (!is.null(condition)) dist <- conditional(dist, condition = condition)
  
  if (na.rm) dist <- dist[Reduce('&', lapply(getLevels(dist), \(col) !is.na(col))), ]
  if (sort) dist <- sort(dist, decreasing = sort > 0L)
  
  dist
  
}

#' @rdname distributions
#' @export
pdist.probability <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  exprs <- rlang::enexprs(...)
  if (length(exprs)) condition <- pexprs(exprs, colnames(x), condition)$Condition %||% condition
  
  if (!is.null(condition)) conditional(x, condition) else x
  
}


#' @rdname distributions
#' @export
pdist.default <-  function(..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  exprs <- as.list(substitute(list(...)))[-1]
  args <- list(...)
  
  # get the appropriate (dim)names for each argument
  
  varnames <- .names(args)
  if (any(varnames == '')) varnames[varnames == ''] <- vapply(exprs[varnames == ''], deparse, nlines = 1L, '')
  names(args) <- varnames
  
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
pdist.table <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE) {
  
  if (all(x == round(x))) return(pdist(count(x, ..., na.rm = na.rm, sort = sort, binArgs = binArgs), condition = condition))
  

  
  if (sum(x) != 1) x <- x / sum(x)
  
  df <- as.data.frame(x, responseName = 'p')
  N <- as.integer(10 ^ -floor(log10(min(df$p[df$p > 0]))))
  
  dist <- distribution(df, 'p', N = N, Condition = condition)
  
  if (na.rm) dist <- dist[Reduce('&', lapply(getLevels(dist), \(col) !is.na(col))), ]
  if (sort) dist <- sort(dist, decreasing = sort > 0L)
  
  dist
  
}



conditional <- function(pdist, condition) {
  varnames <- varnames(pdist)
  checks(condition, xcharnotempty | (xwholenum & xpositive & xmax(length(varnames))), seealso = '?pdist()')
  
  if (is.numeric(condition)) condition <- varnames[condition]
  if (any(!condition %in% varnames)) .stop("We can only calculate a conditional probability across an existing dimension/factor.",
                                           "The <conditions|condition> {harvard(setdiff(condition, varnames), 'and', quote = TRUE)} <are not dimensions|is not a dimension> of the given",
                                           "distribution (the dimensions are named {harvard(varnames, 'and', quote = TRUE)}).", ifelse = length(setdiff(condition, varnames)) > 1)
  
  if (!is.null(pdist@Condition)) {
    if (setequal(condition, pdist@Condition)) return(pdist)
    pdist <- unconditional(pdist)
  }
  
  conditionvec <- do.call('paste', c(as.list(pdist)[condition], list(sep = '.')))
  
  margin <- c(tapply(pdist$p, conditionvec, sum))
  margin <- margin[unique(conditionvec)] # to match original order
  
  p <- ifelse(margin[conditionvec] == 0, 0, pdist$p / margin[conditionvec])
  pdist$p <- tapply_inplace(p, conditionvec, \(x) if (any(x > 0)) x / sum(x) else x)
  
  pdist@Condition <- condition
  
  pdist@N <- setNames(as.integer(round(pdist@N * margin)), names(margin))
  pdist
  
}
            
unconditional <- function(dist) {
  if (!inherits(dist, 'probability') || is.null(dist@Condition)) return(dist)
  
  N <- sum(dist@N)
  margins <- dist@N / N
  
  dist$p <- dist$p * margins[do.call('paste', c(as.data.frame(dist)[, dist@Condition, drop = FALSE], list(sep = '.')))] # paste to deal with NA levels
  
  dist@Condition <- NULL
  dist@N <- N
  dist
 
  
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






# Information Theory ----

#' Information theory
#' 
#' Many computational musicology analyses rely on probabilistic modeling and [information theory](https://en.wikipedia.org/wiki/Information_theory).
#' HumdrumR includes functions to make these sorts of analyses quick and easy.
#' These functions are closely connected to our [distributions] functions, which can be used to calculate/estimate the probability of data observations.
#'
#' @details
#' 
#' 
#' The most fundamental tools of information theory are statistics that characterize probability *distributions*.
#' Thus, they are descriptive statistics, which describe a distribution (usually, the distribution of values in your data)
#' using a single number.
#' Such information-theoretic descriptive statistics can be computed using the [entropy()] 
#' (joint or conditional entropy), [xentropy()] (cross entropy), [kld()] (Kullback–Leibler divergence), and  [mutual()] (mutual information) functions.
#' In contrast, other information theory metrics are calculated "point-wise": one value for each data observation.
#' Our point-wise information theory functions are [like()] (likelihood),
#' [info()] (information content), `pentropy()` (pointwise conditional entropy), and `pmutual()` (pointwise mutual information).
#'
#' Note that all of these functions calculate or utilize *empirical* statistics---i.e., they describe *your data*.
#' They are not (necessarily) representative of the "true" information content in real music.
#' They may be used as *estimates* of the "true" entropy of music we study, but this assumes that our sample is 
#' representative and that our probabilistic models make sense (i.e., make valid assumptions).
#' 
#' @name information
NULL

## Distributional ----


### entropy() ----

#' Calculate Entropy or Information Content of variables 
#'
#' Information content and entropy are fundamental concepts in [information theory](https://en.wikipedia.org/wiki/Information_theory),
#' quantifying the amount of information in samples from a random variable; they are often
#' characterized as measures of how "expected" (low information) or "surprising" (high information) data is.
#' Both concepts are closely related the probability density/mass of events: improbable events have higher information content.
#' The probability of *each* (point-wise) observation maps to the [information content](https://en.wikipedia.org/wiki/Information_content);
#' The average information content of a variable is the [entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)).
#' Information content/entropy can be calculated for discrete probabilities or continuous probabilities,
#' and humdrumR defines methods for calculating both.
#' 
#' @details 
#' 
#' To calculate information content or entropy, we must assume (or more often, estimate) a probability distribution.
#' HumdrumR's [count()] and [pdist()] methods (or R's standard [table()] function) can be used calculate empirical
#' distributions of atomic data.
#' For numeric, data we can also use R's standard [stats::density()] function to estimate the continuous probability density.
#' 
#' The `entropy()` function takes an object representing a probability distribution---ideally a humdrumR [distribution] object,
#' base-R [table], or a [density()] object (for continuous variables)---and returns the entropy, defaulting to base-2 entropy ("bits").
#' However, if you are lazy, you can pass `entropy()` our atomic data vectors directly, and it will automatically pass them to the [pdist()]
#' function for you; for example, if you want to calculate the joint entropy of variables`x` and `y` (which must be the same length),
#'  you can call `entropy(pdist(x, y))` 
#' or just `entropy(x, y)`.
#' Other arguments can be provided to `pdist()` as well; notably, if you want to calculate the *conditional* entropy,
#' you can, for example, say `entropy(x, y, condition = 'y')`.
#'
#' The `info()` function is used similarly to the calling `entropy()` directly on data vectors:
#' anywhere where you can call `entropy(x, y)`, you can call `info(x, y)` instead.
#' The difference is that `info()` will return a vector of numbers, the same length as the representing the information content of each input observation.
#' By definition, entropy of the data distribution is the average of all these point-wise information values: thus, `mean(info(x, y)) == entropy(x, y)`.
#' 
#' @section Cross entropy:
#' 
#' In many cases, we simply use entropy/information content to describe a set of data.
#' In this case, the data we observe and the probability model (distribution) are the same---the probability model is the distribution of the data itself.
#' However, we can also use a *different model*---in this case, a different probability distribution---to describe data.
#' We thus get a measure of how well the model fits the data; this is called the [cross entropy](https://en.wikipedia.org/wiki/Cross-entropy).
#' The minimum cross entropy occurs when the data matches the model exactly, and that minimum is the normal "self" entropy of the model.
#' If a data matches the model well, the cross entropy will be a bit higher than the self entropy; if the data matches the model poorly,
#' the cross entropy can be much higher.
#' The difference between the cross entropy and the self entropy is always positive (or zero), and is called the 
#' [Kullback-Leibler Divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence).
#' 
#' To calculate cross entropy, use the `xentropy()` command.
#' (The Kullback-Leibler Divergence can be calculated in the same way using the `kld()` function.)
#' The `xentropy()` command works just like the entropy command, except you need to provide it a `model` argument, which must be 
#' *another* probability distribution.
#' Note that that data and the model have to have the **exact** same variable names, or `humdrumR` will throw an error!
#' Name your arguments to avoid this (this is illustrated in the example below, where we name everything `X`).
#' To illustrate, lets create three sets of data, two of which are similar, and one which is very different:
#' 
#' ```
#' dorian <- c('A', 'B', 'C', 'D', 'E', 'F#', 'G')
#' N <- 1000
#' 
#' sample1 <- sample(dorian, N, replace = TRUE, prob = 7:1)
#' sample2 <- sample(dorian, N, replace = TRUE, prob = 7:1)
#' sample3 <- sample(dorian, N, replace = TRUE, prob = 1:7)
#' 
#' 
#' ## first the self entropy
#' entropy(X = sample1)
#' entropy(X = sample2)
#' entropy(X = sample3)
#' 
#' ## now the cross entropy
#' 
#' xentropy(X = sample1, model = pdist(X = sample2))
#' xentropy(X = sample2, model = pdist(X = sample2))
#' xentropy(X = sample3, model = pdist(X = sample2))
#' 
#' ```
#' 
#' `sample1` and `sample2` have very similar distributions, so when we use `sample2` as a model for `sample1`,
#' the cross entropy is only slightly higher than the self entropy of `sample1`.
#' However, when we use `sample2` as the model for `sample3` (which is distributed very differently)
#' the entropy is quite a lot higher than the entropy of sample 3.
#' 
#' The `info()` command can also be passed a `model` argument.
#' As always, `mean(info(x, model)) == xentropy(x, model)`.
#' There is no standard name for this "cross information content."
#' However, cross entropy/information-content are closely related to the more general concept of *likelihood* (see the next section).
#'
#' @section Likelihood:
#' 
#' The output of `info()` is identical to the log (base 2 by default) of the modeled [likelihood](https://en.wikipedia.org/wiki/Likelihood_function)
#' of each data point, which can be computed using the [like()] function.
#' Literally, `info(x, base) == log(like(x), base)`.
#' The [like()] function works just like `info()`, computing pointwise probabilities for each data point based on the 
#' probability distribution in `model`.
#' However, we can use it to, for example, calculate the total *log likelihood* of data using `sum(log(like(...)))`.
#' This value divided by N is the cross entropy (make sure to use the right log base!): `-sum(log(like(...), base = 2)) == xentropy(...)`.
#' 
#' 
#' 
#' 
#' @family {Information theory functions} 
#' @seealso The HumdrumR [information theory][information] overview.
#' @export
entropy <- function(..., model, base = 2) {
  checks(base, xlen1 & xnumber & xpositive)
  if (!missing(model)) checks(model, xinherits('probability'))
  
  UseMethod('entropy')
}



#' In equations, entropy is traditionally represented as *H(x)*, so we provide the `H()` function as a synonym for `entropy()`.
#' @rdname entropy
#' @export
H <- entropy
  
#' @rdname entropy
#' @export
entropy.probability <-  function(pdist, model, condition = NULL, base = 2) {
            if (!is.null(condition)) pdist <- conditional(pdist, condition)
  
            
            if (missing(model)) {
              expected <- pdist$p
              observed <- unconditional(pdist)$p
              equation <- Pequation(pdist, 'H')
            } else {
              # cross entropy of q and p
              aligned <- alignDistributions(pdist, model, funcname = 'xentropy')
              observed <- aligned$X[[1]]
              expected <- aligned$X[[2]]
              equation <- 'H(p, q)'
          }

          expected <- ifelse(expected > 0L, log(expected, base = base), 0) 
            
          setNames(-sum(observed * expected), equation)
}


#' @rdname entropy
#' @export
entropy.numeric <- function(x, model, base = 2, na.rm = TRUE, ...) {
  if (missing(model)) {
    entropy(density(x, ...), base = base, na.rm = na.rm)
  } else {
    entropy(pdist(...), model = model, base = base)
  }
  
}


#' @rdname entropy
#' @export
entropy.density <- function(x, model, base = 2, na.rm = TRUE) {
            label <- rlang::expr_label(rlang::enexpr(x))
            if (any(is.na(x))) return(NA_real_)
            
            dx <- diff(x$x[1:2])
            
            equation <- if (nchar(label) < 10) paste0('H(', label, ')')
            
            
            setNames(-sum(log(x$y, base = base) * x$y * dx), equation)
            
            
          }


#' @rdname entropy
#' @export
entropy.default <- function(..., model, base = 2) {
            entropy(pdist(...), model = model, base = base)
          }

#### entropy_by() ----

#' Calculate point-wise or contextual entropy
#' 
#' 
#' @family {Information theory functions} 
#' @seealso The HumdrumR [information theory][information] overview.
#' @export
entropy_by <- function(..., by, independent = TRUE, base = 2) {
  checks(base, xlen1 & xnumber & xpositive)
  checks(by, xcharnotempty)

  UseMethod('entropy_by')
}


#' @rdname entropy_by
#' @export
entropy_by.probability <-  function(pdist, by, independent = TRUE, base = 2) {
  
  varnames <- varnames(pdist)
  
  if (!is.null(pdist@Condition) && any(base %in% pdist@Condition)) {
    pdist <- conditional(unconditional(pdist), setdiff(pdist@Condition, base))
  }
  
  if (length(setdiff(by, varnames))) .stop("We can't group this probability distribution by non-existent variable<s|>",
                                           "{harvard(setdiff(by, varnames), 'or', TRUE)}.", ifelse = length(setdiff(by, varnames)) > 1)
  
  grouping <- as.data.frame(pdist)[ , by , drop = FALSE]
  
  expected <- if (!independent) unconditional(pdist)$p else tapply_inplace(unconditional(pdist)$p, as.data.frame(pdist)[ , by , drop = FALSE], \(x) x / sum(x))
  observed <- tapply_inplace(unconditional(pdist)$p, as.data.frame(pdist)[ , c(pdist@Condition, by) , drop = FALSE], \(x) log(x / sum(x), base = base))
# 
#   Hs <- tapply(pdist$p, ,
#                \(ps) {
#                  browser()
#                  if (independent) ps <- ps / sum(ps)
#                  -sum(ps * log(ps, base = base), na.rm = TRUE)
#                })
  
  Hs <- -tapply(expected * observed, as.data.frame(pdist)[ , by , drop = FALSE], sum, na.rm = TRUE)
  
  equation <- Pequation(pdist[ , setdiff(varnames, by)], 'H')
  equation <- gsub('\\)$', '', equation)
  equations <- paste0(equation, ';', 
                      paste(by, collapse = '.'), '=',
                      names(Hs), ')')
  names(Hs) <- equations
  Hs
  
}

#' @rdname entropy_by
#' @export
entropy_by.default <- function(..., by, independent = TRUE, base = 2) {
  entropy_by(pdist(...), by = by, independent = independent, base = base)
}


#### xentropy() ----

#' @rdname entropy
#' @export
xentropy <- function(..., model, base = 2) {
  if (missing(model)) .stop("The xentropy() function requires a probability distribution passed to the 'model' argument.",
                                   "If you want to estimate the model from the data, just use entropy().")
  
  if (!inherits(model, 'probability')) model <- pdist(model)
  
  entropy(..., model = model, base = base)

}



#### kld() ----

#' @rdname entropy
#' @export
kld <- function(..., model, base = 2) {
  checks(base, xnumber & xpositive)
  
  if (missing(model)) .stop("The lkd() function requires a probability distribution passed to the 'model' argument.")
  
  UseMethod('kld')
}




#' @rdname entropy
#' @export
kld.probability <-  function(pdist, model, condition = NULL, base = 2) {
  xentropy(pdist, model = model, base = base, condition = condition) - 
    entropy(pdist, base = base, condition = condition)
}


#' @rdname entropy
#' @export
kld.default <- function(..., model, base = 2) {
  kld(pdist(...), model = model, base = base)
}


### mutual() ----

#' Calculate Entropy or Information Content of variables 
#'
#' @seealso The HumdrumR [information theory][information] overview.
#' @family {Information theory functions} 
#' @export
mutual <- function(..., base = 2) {
  checks(base, xnumber & xpositive)
  
  UseMethod('mutual')
}



#' @rdname mutual
#' @export
mutual.probability <-  function(x, base = 2) {
  varnames <- varnames(x)
  if (length(varnames) < 2L) .stop("Can't calculate mutual information of a single variable.")
  
  x <- unconditional(x)
  
  observed <- setNames(x$p, do.call('paste', c(getLevels(x), list(sep = '.'))))
  
  independent <- Reduce('*', lapply(varnames, \(j) x[ , j]))
  # expected <- (x[ , 1] * x[ , 2])
  independent <- setNames(independent$p, do.call('paste', c(getLevels(independent), list(sep = '.'))))
  
  independent <- independent[names(observed)]
  
  ratio <- observed / independent
  logratio <- ifelse(ratio == 0 | ratio == Inf, 0, log(ratio, base = base))
  
  equation <- Pequation(x, 'I', ';')
  
  setNames(sum(observed * logratio, na.rm = TRUE), equation)
  
}




#' @rdname mutual
#' @export
mutual.default <- function(..., base = 2) {
  mutual.probability(pdist(...), base = base)
}







#' @rdname mutual
#' @export
mutual.probability <-  function(x, base = 2) {
  varnames <- varnames(x)
  if (length(varnames) < 2L) .stop("Can't calculate mutual information of a single variable.")
  
  x <- unconditional(x)
  
  observed <- setNames(x$p, do.call('paste', c(getLevels(x), list(sep = '.'))))
  
  independent <- Reduce('*', lapply(varnames, \(j) x[ , j]))
  # expected <- (x[ , 1] * x[ , 2])
  independent <- setNames(independent$p, do.call('paste', c(getLevels(independent), list(sep = '.'))))
  
  independent <- independent[names(observed)]
  
  ratio <- observed / independent
  logratio <- ifelse(ratio == 0 | ratio == Inf, 0, log(ratio, base = base))
  
  equation <- Pequation(x, 'I', ';')
  
  setNames(sum(observed * logratio, na.rm = TRUE), equation)
  
}




#' @rdname mutual
#' @export
mutual.default <- function(..., base = 2) {
  mutual.probability(pdist(...), base = base)
}


## Point-wise ----

### like() ----


#' @export
like <- function(..., model) {
  if (!missing(model)) checks(model, xinherits('probability') | xinherits('lm'))
  
  UseMethod('like')
}

#' @rdname entropy
#' @export
info <- function(..., model, base = 2, condition = NULL, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  -log(like(..., model = model, condition = condition, na.rm = na.rm, .drop = .drop, binArgs = binArgs), 
       base = base)
}

#' @export
like.default <- function(..., model = NULL, condition = NULL, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  like.data.frame(data.frame(...), model = model, condition = condition, na.rm = na.rm, .drop = .drop, binArgs = binArgs)
}

#' @export
like.data.frame <- function(df, ..., model) {
  if (missing(model) || is.null(model))  {
    model <- if (is.numeric(df[[1]])) lm(df[,ncol(df):1]) else model <- do.call('pdist', list(df, ...))
    
  }
  
  if (inherits(model, 'probability')) {
    colnames <- colnames(df)
    varnames <- varnames(model)
    
    if (!setequal(colnames, varnames)) .stop("To calculate likelihoods, the expected distribution must have the same variables as the observed variables.")
    
    model[[as.matrix(df), , drop = TRUE]]$p
    
  } else {
    dnorm(predict(model, newdata = df, type = 'response'), 0, summary(model)$sigma)
  }
  
  
}


### pentropy() ----


#' @rdname entropy_by
#' @export
pentropy <- function(..., model, base = 2, condition = NULL, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  df <- data.frame(...)
  
  if (missing(model)) {
    model <- pdist(..., condition = NULL, na.rm = na.rm, .drop = .drop, binArgs = binArgs)
    df <- df[1]
  }
  
  conditions <- intersect(varnames(model), names(df))
  
  model <- as.data.frame(model)
  entropymat <- tapply(model$p, model[ , conditions, drop = FALSE], 
                       \(P) {
                         P <- P / sum(P)
                         -sum(P * log(P, base = base), na.rm = TRUE)
                       })
  
  
  entropymat[do.call('cbind', df)]
  
}


### pmutual() ----

#' @rdname mutual
#' @export
pmutual <- function(..., model, base = 2, condition = NULL, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  df <- data.frame(...)
  
  if (missing(model)) model <- pdist(..., condition = condition, na.rm = na.rm, .drop = .drop, binArgs = binArgs)
  
  independent <- Reduce('*', lapply(varnames(model), \(j) model[ , j]))
  
  ic_observed <- info(df, model = model, base = base)
  ic_independent <- info(df, model = independent, base = base)
  
  ic_independent - ic_observed
  
}



##################################################-
# table() extensions for humdrumR ---- ###########
##################################################-



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
  levels <- lapply(getLevels(x), unique)
  varnames <- varnames(x)
  type <- dist_type(x)
  
  x <- as.data.frame(x)
  x[varnames] <- lapply(x[varnames], paste) # forces NA to be strings
  
  tab <- array(dim = lengths(levels), dimnames = lapply(levels, paste))
  tab[do.call('cbind', x[varnames])] <- x[[type]]
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
