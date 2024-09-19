
setClassUnion('discrete', c('character', 'factor', 'logical', 'integer', 'token'))

# S4 Distributions ----

## Definitions ----





#' Distributions
#' 
#' HumdrumR represents data distributions in a special `distribution` class, which
#' is a simple extension of a standard `data.frame`.
#' Distributions may have one or more dimensions.
#' There are two subtypes of distributions: count distributions (created by [count()]) and 
#' probability distributions (created by [pdist()]).
#' 
#' @details
#' 
#' The `distribution` class is essentially just a normal `data.frame` except with
#' a special methods for printing, indexing, and combining distributions.
#' These features all make the object look and behave more intuitively
#' like a like a distribution of data.
#' You can always use `as.data.frame()`, `as.data.table()`, or `as_tibble()` (if you've attached [tibble]) to 
#' get rid of the humdrumR features.
#' 
#' Each `distribution` frame has \eqn{k + 1} columns, where \eqn{k} is the number of dimensions (variables).
#' In a `distribution` data.frame, each row represents a level, or combination of levels, in the distribution.
#' The first $k$ columns indicate the levels of each variable in the distribution (each dimension).
#' Each of these $k$ dimensions always has a unique (non-empty) name, which can
#'  be accessed with the `varnames()` command, or modified with `varnames(myDist) <- newnames`.
#' These columns can be accessed directly using the `levels()` command.
#' 
#' The last (rightmost) column contains the count or proportion associated with each level.
#' In the case of count distributions, this last column is named `n`; in probability distributions, it is named `p`.
#' The `n` (count) column is always an `integer` vector, where are \eqn{\forall n, n \geq 0}.
#' The `p` (proportion) column is always a `numeric` vector, where \eqn{\forall p, 1 \geq p \geq 0}.
#' 
#' We define `as.matrix()` and `as.array()` methods for `distribution` objects.
#' The `as.array()` method results in an [array()] with the same dimensionality
#' as the distribution.
#' In contrast, the `as.matrix()` function will force distributions with more than two dimensions
#' into two dimensions, by collapsing combining all dimensions greater than two with the first dimension:
#' each combination of levels will appear as a row (with `.` in between the names of levels).
#'
#' @section Printing distributions:
#' 
#' The [print()] method for distribution objects has some nice features, including syntax highlighting
#' (which can be turned off with the `syntaxHighlight = FALSE` argument to `print()`, or by setting the 
#' global humdrumR option with `humdrumR(syntaxHighlight = FALSE)`.
#' 
#' HumdrumR distributions will print rounded versions of numbers (marked with `~`), and use appropriate 
#' [SI prefixes](https://en.wikipedia.org/wiki/Metric_prefix) for large and small numbers. 
#' Specifically, count distributions will affix large numbers with the characters
#'  `k` (kilo), `M` (mega), `G` (giga), or `P` (peta), while probability distributions will use
#'  `m` (milli), `𝜇` (micro), `n` (nano), and `p` (pico).
#' For example, humdrumR will print `12321` as `~12.3k`.
#' The number of digits printed can be controlled with the `digits` argument to `print()`, defaulting
#' to `digits = 3`.
#' For example, if `digits = 4`, `12321` would print as `~12.32k`.
#' If *all* numbers in a distribution are of the same order of magnitude,
#' the scale is printed with the name of the distribution instead of adding
#' prefixes to all the numbers. 
#' For example, it might print "humdrumR count distribution (thousands)."
#' 
#' The `distribution` print method also includes an argument `printZeros`, to control whether
#' zero values are printed, and the argument `zeros` to control *how* zeros are printed.
#' By default, `zeros = '.'`, so zeros are printed as `.`, which makes tables easier to read.
#' Note that the `printZeros` function only determines if zeros are printed; the `count()`
#' and `pdist()` functions also have the `.drop` argument to actually *remove* those levels
#' from a distribution.
#' 
#' A final printing feature is controlled by the `wide` print argument.
#' When `wide = TRUE` (which is the default), the second dimension (if present) of
#' the a distribution is printed "wide", with each level in its own column.
#' This creates a contingency table like visual, which is easier to read.
#' This wide format is only used when the resulting printout will fit on your screen.
#' 
#' Note that the wide printing does not change the structure of the underlying `data.frame`,
#' which is still a "tall" data.frame, with each levels of dimension two represented in different rows.
#' This may make indexing unintuitive, i.e., if you expect the (apparent) two-dimensional table to
#' be indexed like a 2d matrix/table would be in R.
#' It won't work that way!
#' This is why we provide the `wide = FALSE` option, so you can see what the table
#' "really" looks like if you need to.
#' 
#' @section Indexing distributions:
#' 
#' Subsets of humdrumR `distribution` objects can be extracted using indexing (`[]` or `[[]]`)
#' or the (equivalent) `filter()`/`subset()` methods.
#' In either case, a `distribution` of the same class (either `count` or `probability`)
#' if returned unless the `drop` argument is set to `TRUE`, which will cause a `data.frame`
#' to return.
#' Distributions cannot be empty, so any attempt to index that returns nothing will
#' result in an error (unless `drop = TRUE`).
#' You can use `myDist[drop = TRUE]` to transform a `distribution` into a normal `data.frame`, without indexing it.
#' 
#' In calls to `subset()`/`filter()`, you can refer to either the distribution's variable names,
#' or to the `n` (count) or `p` (proportion) column.
#' For example, you can find all variables where the count is greater than one hundred 
#' with `myDist |> filter(n > 100)`.
#' 
#'
#' #### Single-bracket [i , ]
#' 
#' With single-bracket indexing, the `i` argument is matched to rows of the underlying data.table.
#' (As mentioned above, this may conflict visually with the `wide` printing option, 
#' for distributions with two or more dimensions.)
#' If `i` is either `logical` or `numeric`, indexing is exactly like conventional `data.frames`,
#' except there are more strict checks for valid indexes---for example, non-whole values of `i` are not allowed,
#' and an error will throw if `any(i > nrow(myDist))`,
#' 
#' If `i` is `character`, the strings are matched against the level names of *all* the distributions' dimensions.
#' An exact match with a level in any dimension will result in that level being returned.
#' 
#' #### Single-bracket [ , j]
#' 
#' With single-bracket indexing, the `j` argument is used to index the dimensions of the distribution.
#' If `j` is either `logical` or `numeric`, indexing is exactly like conventional `data.frames`,
#' except 1) there are more strict checks for valid indexes---for example, non-whole values of `j` are not allowed,
#' and an error will throw if `any(j > length(varnames(myDist)))`; and 2) the `n`/`p` is not counted
#' as a column to index---this column is always retained.
#' If `j` is `character`, the strings are matched exactly against the distribution names.
#'  
#' #### Double-bracket [i, j, ...]
#'  
#' Double-bracket indexing can be used to index specific combinations of levels, across more than one distribution.
#' Named index arguments are matched (exactly) to dimension names; unnamed arguments are matched
#' in order to any dimensions not given a named argument.
#' Each index argument can be a `logical` (same length as number of levels for corresponding dimension),
#' whole numbers (indexing dimension levels in order), or `character` strings (exactly) matching level names.
#' 
#' Normally, each dimension is indexed separately based on any indexes applied to it.
#' However, if `cartesian = TRUE`, specific combinations of levels are indexed
#' based on which values in each of the index vectors align with each other.
#' (This approach cannot be used with `logical` indices.)
#' For example, `myDist[[c(1, 1, 2, 2), c(1, 2, 3, 4)]]` will return only the counts/proportions
#' from the level-pairs (1,1), (1,2), (2,3), and (2,4).
#' The count for (1,3)--for example---would not be returned.
#' This behavior is similar to indexing a [base::matrix()] using matrix---indeed,
#' you can get the same behavior by indexing with a matrix.
#' For example, `myDist[[cbind(c(1,1,2,2), c(1,2,3,4))]]`.
#' 
#' ### Probability sums
#'   
#' When indexing/filtering a `probability` distribution, the probabilities of any remaining levels
#' after filtering/indexing are recomputed so as to sum to 1.
#' If you index out dimensions, the levels of the removed dimensions are summed across
#' the levels of the remaining dimensions.
#' If you want to keep the values as is (not summing to 1), use `drop = TRUE`---the output will no longer
#' be a probability distribution.
#' 
#' @section Conditional probability:
#' 
#' 
#' `probability` `distribution` objects, by default, represent the joint probability
#' of all their dimensions.
#' (This means that the whole distribution sums to one).
#' However, they may also have their probabilities
#' conditioned on one or more of their variables, so that the probabilities with each level
#' (or combinations of levels) of the conditions sum to one.
#' These conditions can be set when a distribution is created, or modified, by the `pdist()`
#' function, using the `condition` argument.
#' When a `condition` is not specified, the joint probability is computed.
#' Thus, you can remove conditions from any `probability` distribution by passing it to
#' `pdist()` with no `condition` argument---or can also change the condition to other variables.
#' For example:
#' 
#' ```
#' pdist(X = x, Y = y, condition = 'X') -> condition_X
#' 
#' condition_X |> pdist() -> joint
#' condition_X |> pdist(condition = 'Y') -> condition_y
#' 
#' ```
#' 
#' When indexing levels, conditional probabilities are resummed as usual.
#' If you are indexing out variables, any conditions remaining in the distribution (after indexing)
#' will be kept (and recomputed) if it makes sense.
#' However, if you index out all the conditions, or index out all variables that weren't conditioned on,
#' the conditions will be removed during indexing.
#' 
#' 
#' @section Combining distributions:
#' 
#' HumdrumR's `distribution` objects can be concatenatated (combined) together to form new distributions.
#' Only `distribution` objects with *identical* dimension names can be combined (an error will be thrown, otherwise).
#' (If you need to change dimension names to make the match, use `varnames()<-`.
#' When combined, the shared levels of each dimension are aligned.
#' Levels that are not shared are simply copied from their origin `distribution`.
#' This means that the levels of each dimension of the resulting `distribution` will be the
#' union of the levels of that dimension between the two originating distributions.
#' 
#' The `c()` function can be used to combine two or more `count` distributions which have identical dimension names.
#' The resulting distribution has a new variable added, indexing the original distribution source.
#' This new variable/dimension is named `"concat"` by default, but can be changed to another name using
#' the `varname` argument.
#' 
#' 
#' @section Distribution math:
#' 
#' HumdrumR defines some mathematical operations for `distribution` objects, including arithmetic and comparison operations 
#' between two distributions; operations between a distributions and positive, whole-number scalar values, and
#' mathematical summaries of distributions.
#' 
#' When doing arithmetic or comparison with `distribution` objects, some operations make sense as "closed" operations: meaning
#' that the result of the operation is still a `distribution` of the same type.
#' In contrast, other arithmetic operations only make sense if we view the result as "just" numbers,
#' not a new distribution.
#' In these cases, humdrumR simply returns an `atomic` vector corresponding to the `p`/`n` column of the distribution, 
#' with names corresponding to the levels of the distribution.
#' These vectors can be used to, for example, index the original distribution; for example: `myCounts[myCounts > 50]`.
#'
#' ### Arithmetic with count distributions
#' 
#' Only addition (`+`) between two `count` distributions results in new distribution: all other operations *between*
#' distributions result in an vector return value.
#' (Subtraction is not allowed, so as to avoid negative counts.)
#' However, `count` distributions can be scaled by positive whole numbers using either `*` (multiplication) or `%/%` (Euclidean division),
#' while remaining a count distribution.
#' The scaling value must either be length 1, or be the same length as the number of levels in the distribution (`nrow(levels(myDist))`).
#' All other arithmetic with count distributions will result in vector output.
#'
#' ### Arithmetic with probability distributions
#' 
#' For `probability` distributions, all arithmetic involving distributions inevitably destroys their structure
#'  (e.g., make it so the total probability no longer sums to 1), so if
#' arithmetic is done between `probability` distributions (which share dimension names),
#' or between `probability` distributions and scalar values (length 1, or same length as the number of levels in the distribution),
#' the result is always always a vector.
#' 
#' There is one other special arithmetic operation between `probability` distributions which *don't*
#' share dimensions.
#' The outer product function `%o%` can be used to produce the empirical,
#' independent joint probability between two probability distributions.
#' The joint probability of all levels is calculated, assuming all dimensions/variables
#' are independent---in other words, the joint product of each condition is just the product of
#' all the levels.
#' This outer-product operation (`%o%`) will strip any/all conditions from the `probability`
#' distributions.
#' 
#' ### Mathematical summaries of distributions
#' 
#' The R "group generic functions" [Math] and [Summary] are defined for
#' `distribution` objects. This includes, functions like `log()`,
#' `round()`, `min()`, `range()`, and `sum()`.
#' Methods for [mean()] and [median()] are also defined.
#' All of these functions, when applied to a `distribution`, return an atomic vector.
#' 
#' For more general operations with `distribution` values,
#' use [mutate()] or [summarize()] (or [with/within][base::within]), and refer to
#' the `n` (count) or `p` (probability) field.
#' For example: `myDist |> filter(n > 70)`.
#' 
#' 
#' @param syntaxHighight ***Should syntax highlighting be used (in Rstudio)?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param wide ***Should the second dimension in distributions be printed "wide"?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' 
#' @param printZeros ***Should zero counts/probabilities be printed?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param zeros ***How should zeros be represented in tables?***
#' 
#' Defaults to `"."`.
#' 
#' Must be a single atomic value, which will be coerced to a `character`.
#' 
#' @seealso Use the [count()][count.default] and [pdist()] functions to create distribution objects.
#' @name distribution
NULL


setClass('distribution', contains = 'data.frame', slots = c(Sort = 'integer'))
setClass('count', contains = 'distribution')
setClass('probability', contains = 'distribution', slots = c(N = 'integer', Condition = 'maybecharacter'))

setValidity('distribution',
            function(object) {
              names <- names(object)
              
              errors <- c(
                if (ncol(object) <= 1L) 'distribution must have at least one dimension',
                if (is.null(names)) {
                  c('distributions must have named columns')
                } else {
                  c(if (any(duplicated(names))) 'distribution dimension names cannot be duplicated',
                    if (any(names == '')) 'distribution dimension names cannot be empty strings')
                } ,
                if (any(head(sapply(object, class), -1) != 'factor')) 'Each level column of variables in a distribution object must be of class "factor"')
                
              if (length(errors)) errors else TRUE
            
            })

setValidity('count',
            function(object) {
              ncol <- ncol(object)
              errors <- c(
                if (names(object)[ncol] != 'n') 'The last column of a count distribution object must be named "n"',
                if (class(object@.Data[[ncol]]) != 'integer') 'The "count "n" column of a count distribution object must of class "integer"',
                if (any(object@.Data[[ncol]] < 0L)) 'Count distributions cannot hold negative values'
              )
              if (length(errors)) errors else TRUE
            })

setValidity('probability',
            function(object) {
              ncol <- ncol(object)
              vals <- object@.Data[[ncol]]
              errors <- c(
                if (names(object)[ncol] != 'p') 'The last column of a probability distribution object must be named "p"',
                if (class(object@.Data[[ncol]]) != 'numeric') 'The "p" column of a probability distribution object must of class "numeric"',
                if (any(vals < 0 | vals > 1)) 'Probability distributions cannot hold negative values, or values greater than 1'
              )
              if (length(errors)) errors else TRUE
            })

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
getLevelString <- function(dist, sep = '.') do.call('paste', c(getLevels(dist), list(sep = sep)))
 
#' @rdname distribution
#' @export
varnames <- function(x) setdiff(colnames(x), c('n', 'p'))

#' @rdname distribution
#' @export
`varnames<-` <- function(x, value) {
  checks(value, xlen && xmaxlength(length(x) - 1L))
  x@names[1:(min(length(value), length(x) - 1))] <- value
  x
}

#' @export
levels.distribution <- function(x) getLevels(x)

### print() ----

#' @export
setMethod('show', 'distribution', \(object) print.distribution(object))

#' @export
#' @rdname distribution
print.distribution <- function(dist, digits = 3,
                               syntaxHighlight = humdrumRoption('syntaxHighlight'),
                               wide = TRUE,
                               printZeros = TRUE,
                               zeros = '.') {
  
  checks(syntaxHighlight, xTF)
  checks(wide, xTF)
  checks(printZeros, xTF)
  checks(zeros, xatomic & xlen1)
  
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
  scale <- if (length(scale)) paste0(' (', scale, ')')
  
  
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

prettyN <- function(N, digits = 3L, zeros = '.') {
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
    c('thousands', 'millions', 'billions', 'trillions')[globalscale]
  }
  # round and format
  Nround <- signif(N, digits = digits)
  approx <- N != Nround
  
  
  Nprint <- if (is.null(scale)) {
     format(Nround, scientific = FALSE, digits = digits, justify = 'left') 
  }  else {
    tapply_inplace(Nround, scale, format, scientific = FALSE, digits = digits, justify = 'left')
  }
  # Nprint[N != 0] <- gsub('^( *)0\\.', '\\1 .', Nprint[N != 0]) # pad left
  
  output <- paste0(Nprint, scale)
  output[approx] <- gsub('^( *) ?', '\\1~', output[approx])
  output <- stringr::str_pad(output, width = max(nchar(output)), 'left')
  output[N == 0] <- zeros
  # output <- paste0(ifelse(approx, '~', ''), Nprint, scale)
  # output <- paste0(output, strrep(' ', max(nchar(output)) - nchar(output))) 
  # 
  attr(output, 'zerofill') <- zeros
  attr(output, 'scale') <- globalscale
  attr(output, 'approx') <- any(approx, na.rm = TRUE)
  attr(output, 'negative') <- any(N < 0, na.rm = TRUE)
  
  output
  
}


prettyP <- function(P, digits = 3L, zeros = '.') {
  tens <- ifelse(P == 0, 0, log10(P) |> floor())
  
  thousands <- ifelse(P == 1, 0L, (tens + 1L) %/% -3)
  # scale <- min(thousands)
  
  # scaling (powers of 10^3)
  scale <- c('', 'm', '𝜇', 'n', 'p')[1L + abs(thousands)]
  
  globalscale <- if (length(unique(scale[P != 0])) == 1L && unique(thousands[P != 0]) != 0) {
    globalscale <- paste0('10^(', -unique(thousands[P != 0]) * 3, ')')
    scale <- NULL
    globalscale
  }
  
  P <- P * 1000^thousands
  
  Pround <- signif(P, digits = digits)
  approx <- P != Pround
  
  Pprint <- if (is.null(scale)) {
     format(Pround, scientific = FALSE, digits = digits, justify = 'left')
  } else {
     tapply_inplace(Pround, scale, format, scientific = FALSE, digits = digits, justify = 'left')
    
  }
  # Pprint <- format(Pround, scientific = FALSE, digits = digits)
  Pprint <- gsub('^( *)0\\.', '\\1.', Pprint)

  
  output <- paste0(ifelse(approx, '~', ' '), Pprint, scale)
  output[P == 0] <- zeros
  maxnchar <- max(nchar(output))
  
  
  attr(output, 'zerofill') <- paste0(' ', zeros, strrep(' ', max(0, maxnchar - 2L)))
  attr(output, 'scale') <- globalscale 
  attr(output, 'approx') <- any(approx)
  
  output
  
}


prettyBins <- function(x, maxUnique = 20, quantiles = 0, right = TRUE, ...) {
  checks(maxUnique, xlen1 & xpositive & xwholenum, argname = 'binArgs(maxUnique = )', seealso = '?count')
  checks(quantiles, xlen1 & xpositiveorzero, argname = 'binArgs(quantiles = )', seealso = '?count')
  checks(right, xTF, argname = 'binArgs(right = )', seealso = '?count')
  
  levels <- sort(unique(x))
  if (is.integer(x) || all(is.whole(levels))) {
    range <- diff(range(levels))
    
    if (range <= maxUnique) {
      return(factor(x, levels = min(levels) : max(levels)))
    }
    if (length(levels) <= maxUnique) return(factor(x, levels = levels))
  } else {
    if (length(levels) <= (maxUnique / 2)) return(factor(x, levels = levels))
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

            checks(i, xlen & ((xnodups & (xcharacter | (xwholenum & xnotzero & xposORneg))) | (xlogical & xmatchnrow(x))))
            if (is.numeric(i) && any(abs(i) > nrow(x))) .stop("Can't index this distribution with values beyond {nrow(x)}.")
            
            if (is.character(i)) {
              levels <- levels.distribution(x)
              bad <- !i %in% unlist(levels)
              if (any(bad)) .stop("The name<|s> {harvard(i[bad], 'and', quote = TRUE)} do<es|> not match any levels in this distribution.", ifelse = sum(bad) == 1L)
              i <- Reduce('|', lapply(levels, \(lev) lev %in% i))
            }
            
            df <- as.data.frame(x)[i, ]
            
            if (!drop && nrow(df) == 0) .stop("You have removed all values from the distribution.")
            
            if (drop) df else distribution(df, x)
            
          })

# 
setMethod('[', c('distribution', 'missing', 'atomic'),
          function(x, i, j, drop = FALSE) {
            
            varnames <- varnames(x)

            checks(j, xlen & (((xcharacter | (xwholenum & xnotzero & xposORneg)) & xnodups) | xlogical))
            
            # prepare j
            if (is.logical(j)) {
              if (length(j) != length(varnames)) .stop('If indexing a distribution using a logical j argument, the logical vector must by the',
                                                       'same length as the number of dimensions in the distribution.',
                                                       'You have provided a length {num2word(length(j))} logical, but the distribution has {num2word(length(varnames))} dimensions.')
              j <- which(j)
            }
            if (is.numeric(j)) {
              if (any(abs(j) > length(varnames))) .stop("This distribution has only {num2word(length(varnames))} dimensions, but",
                                                   "you have provided <the |>j (column) <index|indices> {harvard(j[abs(j) > length(varnames)], 'and')}.", ifelse = sum(abs(j) > length(varnames)) == 1L)
              j <- varnames[j] 
              if (length(j) == 0) .stop('You have removed all dimensions from the distribution.')
            }
            
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
          function(x, i, j, drop = FALSE) {
            
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
              matches(args[!missing], levels[!missing], multi = TRUE)
              
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
          function(x, i, ..., drop = FALSE) {
            
            
            output <- callNextMethod(x = x, i = i, drop = FALSE)
           
            
            if (drop) {
              as.data.frame(output) 
              } else {
                recomputeP(output, x)
              }
            
          })


#' @export
setMethod('[', c('probability', 'missing', 'atomic'),
          function(x, i, j, ..., drop = FALSE) {

            conditions <- x@Condition
            x <- unconditional(x)
            # do indexing
            output <- callNextMethod(x = x, j = j, drop = FALSE)
            
            varnames <- varnames(output)
            if (any(!varnames %in% conditions)) {
              newConditions <- intersect(conditions, varnames)
              if (length(newConditions)) output <- conditional(output, newConditions)
            }
            
            
            if (drop) as.data.frame(output) else output

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

#' @export
subset.distribution <- function(x, ..., drop = FALSE) {
  exprs <- rlang::enquos(...)
  x[rlang::eval_tidy(exprs[[1]], data = as.data.frame(x)), , drop = drop]
}

### coercion ----

#' @export
as.array.distribution <- function(x) {
   output <- as.array(as.table(x))
   if (length(dim(output)) == 1L) {
     # dimnames <- setNames(list(names(output)), varnames(x))
     output <- cbind(output)
     colnames(output) <- varnames(x)
     output
     }
   else {
     output
   }
}

#' @export
as.matrix.distribution <- function(x, ...) {
  varnames <- varnames(x)
  type <- dist_type(x)
  
  if (length(varnames) >= 2L) {
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

### with/within() ----

##### with() and summarize() already work

#' @export
within.distribution <- function(data, ...) {
  within(as.data.frame(data), ...)
}

#' @export 
mutate.distribution <- function(.data, ...) {
  mutate(as.data.frame(.data), ...)
}


### sorting ----

#' @rdname distribution
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
setMethod('c', 'count',
          function(x, ..., varname = 'concat') {
            
            checks(varname, xatomic & xlen1 & xcharnotempty, seealso = '?distribution > Combining distributions')
            
            ldots <- list(x, ...)
            if (!all(sapply(ldots, inherits, what = 'count'))) return(cbind(...))
            
            varnames <- lapply(ldots, varnames)
            if (varname %in% unlist(varnames)) .stop("You can't concatinate these count distributions across a new",
                                                       "dimension called '{varname}', because that dimenion already exists",
                                                       "in at least one of the distributions.")
            if (!Reduce('identical', varnames)) .stop("You can't concatinate these count distributions, because they",
                                                      "don't have identical dimensions names.")
            
            dfs <- lapply(ldots, as.data.frame)
            dfs <- Map(\(df, n) {
              df[[varname]] <- n
              df[ , order(colnames(df) == 'n')]
            }, dfs, factor(seq_along(dfs)))
            
            df <- do.call('rbind', dfs)
            
            distribution(df, 'n')
            
            
          })

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
            df$n <- as.integer(callGeneric(aligned$X[[1]], aligned$X[[2]]))
            distribution(df, 'n')
          })

#' @export
setMethod('+', c('count', 'integer'),
          function(e1, e2) {
            
            checks(e2, xpositiveorzero & (xlen1 | xmatchnrow(e1)), seealso = '?distribution > Combining distributions')
            df <- getLevels(e1)
            df$n <- callGeneric(getValues(e1), e2)
            
            distribution(df, 'n')
          })

#' @export
setMethod('+', c('count', 'numeric'),
          function(e1, e2) {
            checks(e2, xwholenum, seealso = '?distribution > Combining distributions')
            e1 + as.integer(e2)
          })



#' @export
setMethod('*', c('count', 'integer'),
          function(e1, e2) {
            checks(e2, xpositiveorzero & (xlen1 | xmatchnrow(e1)), seealso = '?distribution > Combining distributions')
            
            df <- getLevels(e1)
            df$n <- callGeneric(getValues(e1), e2)
            
            distribution(df, 'n')
          })


#' @export
setMethod('*', c('count', 'numeric'),
          function(e1, e2) {
            checks(e2, xwholenum, seealso = '?distribution > Combining distributions')
            
            e2 <- as.integer(e2)
            df <- getLevels(e1)
            df$n <- as.integer(callGeneric(getValues(e1), e2))
            
            distribution(df, 'n')
          })


#' @export
setMethod('%/%', c('count', 'integer'),
          function(e1, e2) {
            checks(e2, xpositive & (xlen1 | xmatchnrow(e1)), seealso = '?distribution > Combining distributions')
            
            df <- getLevels(e1)
            df$n <- callGeneric(getValues(e1), e2)
            
            distribution(df, 'n')
          })


#' @export
setMethod('%/%', c('count', 'numeric'),
          function(e1, e2) {
            checks(e2, xwholenum, seealso = '?distribution > Combining distributions')
            
            e2 <- as.integer(e2)
            df <- getLevels(e1)
            df$n <- as.integer(callGeneric(getValues(e1), e2))
            
            distribution(df, 'n')
          })



#' @export
setMethod('Ops', c('distribution', 'distribution'),
          function(e1, e2) {
            aligned <- alignDistributions(e1, e2, funcname = .Generic)

            setNames(callGeneric(aligned$X[[1]], aligned$X[[2]]), 
                     do.call('paste', c(aligned$Levels, list(sep = '.'))))
            
            
          })


#' @export
setMethod('Ops', c('distribution', 'numeric'),
          function(e1, e2) {
            setNames(callGeneric(getValues(e1), e2), getLevelString(e1))
          })


#' @export
setMethod('Ops', c('distribution', 'integer'),
          function(e1, e2) {
            setNames(callGeneric(getValues(e1), e2), getLevelString(e1))
            
          })




#' @export
setGeneric('%o%')
#' @export
setMethod('%o%', c('probability', 'probability'),
          \(X, Y) {
            
            Xexpr <- rlang::as_label(substitute(X))
            Yexpr <- rlang::as_label(substitute(Y))
            
            X <- unconditional(X)
            Y <- unconditional(Y)
            
            
            varnamesX <- varnames(X)
            varnamesY <- varnames(Y)
            newvarnames <- if (any(duplicated(c(varnamesX, varnamesY)))) {
              exprs <- make.unique(c(Xexpr, Yexpr), sep = '')
              c(paste0(exprs[1], '{', varnamesX, '}'),
                paste0(exprs[2], '{', varnamesY, '}'))
            } else {
              c(varnamesX, varnamesY)
            }
            
            # if (length(varnames1) > 1L || length(varnames2) > 1L) .stop("Can't cross product probability distributions with more than one dimenion yet.")
            
            Xlevels <- lapply(getLevels(X), unique)
            Ylevels <- lapply(getLevels(Y), unique)
            pX <- array(X$p, dim = sapply(Xlevels, length), dimnames = Xlevels)
            pY <- array(Y$p, dim = sapply(Ylevels, length), dimnames = Ylevels)
            
            pXY <- outer(pX, pY, '*')
            
            df <- as.data.frame(as.table(pXY))
            colnames(df) <- c(newvarnames, 'p')
            
            distribution(df, 'p', Condition = NULL, N = sum(X@N, Y@N))
            
            
          })


### other math ----

#' @export
setMethod('Math', 'distribution',
          \(x) {
            # distmat(x, callGeneric(getValues(x)))
            setNames(callGeneric(getValues(x)), getLevelString(x))
          })

#' @export
setMethod('Summary', 'distribution',
          \(x, ..., na.rm = FALSE) {
            setNames(callGeneric(getValues(x), ..., na.rm = na.rm), paste(varnames(x), collapse = '.'))
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













## User Functions ----


### count() ----


#' Tabulate and/or cross-tabulate data
#' 
#' The `count()` function can be used to tabulate unique values in a vector, or cross-tabulate
#' combinations of values across multiple vectors of the same length.
#' This is similar to R's fundamental [table()][base::table] function,
#' except it returns a specialized `data.frame` ([distribution] object) instead of an `array`.
#' HumdrumR `count()` methods also give special treatment to [token()] data
#' and to numeric data.
#' 
#' 
#' @details
#' 
#' The `count()` function is defined in the [dplyr][dplyr::count] package, but only 
#' for working with [tibbles][tibble()].
#' In `humdrumR`, we extend the `count()` function to work with atomic data (like [base::table()])
#' as well as [humdrumR data][humdrumRclass].
#' When `humdrumR` is attached, [dplyr::count()] will only be called for [tibble()] objects specifically.
#' Note that [dplyr::count()] doesn't have all the same arguments or behaviors as `humdrumR`'s `count()`
#' methods, which are described in *this* documentation.
#' 
#' HumdrumR `count()` and `pdist()` methods return special [distribution] objects, with each input vector
#' creating one dimension in the distribution.
#' When applied to atomic vectors, `humdrumR::count()` will use the deparsed expression(s)
#' provided to it as the name for each vector/dimension (similar to `base::table(..., dnn = 2)`).
#' You can override this by specifying dimension names directly as argument names, like `count(Kern = kern(Token))`;
#' if you don't specify a name, `count()` will make up a name(s) based on expression(s) it is tallying.
#'
#' The `sort` argument can be used to sort the output distribution, just like passing it to the `sort()` function.
#' Thus, `count(x) |> sort()` is identical to `count(x, sort = TRUE)`.
#' If you want to sort the output in reverse (ascending), specify `sort = -1`.
#' Thus, `count(x) |> sort(decreasing = FALSE)` is also identical to `count(x, sort = -1)`.
#'
#' If `count()` or `pdist()` are applied directly to a [humdrumR data object][humdrumRclass],
#' you can specify any fields in the data as arguments.
#' If you don't specify any fields, the [selected][selectedFields] field(s) will be passed and tabulated.
#' 
#'
#' @section NAs and zeros:
#' 
#' HumdrumR `count()` and `pdist()` methods will, by default, count `NA` values if they are present---if you don't want
#' to count `NA`s, specify `na.rm = TRUE`.
#' 
#' By default, `count()` and `pdist()` will include all known levels of input variables, even if those levels don't occur 
#' (they are counted as zero).
#' This can happen if the input includes [factors][base::factor] or [tokens][token()], which have their known levels attached to
#' them.
#' Zeros can also occur for any atomic type when cross-tabulating multiple vectors, if certain combinations of values
#' never occur.
#' To drop zeros from the output distribution, specify .drop = TRUE`.
#' (Note that `dplyr::count()` drops levels by default, but `r hm` functions do not.=)
#' 
#' 
#' 
#' @section Tabulating numeric values:
#' 
#' For numeric values, especially real numbers, it is often the case that there are few (or no) exact values that
#' occur more than once, so tabulating unique values is pointless.
#' In these cases, we might prefer to count numbers into corresponding to ranges of numbers, like in a histogram,
#' and this is exactly what `count()`/`pdist()` (can) do.
#' 
#' By default, if you pass a vector of numbers which has more than `20` unique values,
#' the numbers will be binned using the same algorithm as [graphics::hist()].
#' This process can be controlled using the `binArgs` argument, which is itself a list of control arguments.
#' `binArgs = list(maxUnique = N)` controls the number of unique numbers needed before binning occurs (`20` by default).
#' `binArgs = list(right = FALSE)` (default is `TRUE`) controls whether are closed on the right (larger numbers) instead of the left.
#' Finally, additional arguments to [graphics::hist()] can be passed via `binArgs`, controlling how binning occurs: notably,
#' you can use the `binArgs = list(breaks = _)` to control exactly where boundaries should occur, or the number of bins you want.
#' For example, `binArgs = list(breaks = 10)` will make `count()` bin the input numbers into twelve bins (see [hist()] 
#' for details).
#'
#' Alternatively, you can tell `count()` to divy up (bin) the input numbers into quantiles by
#' passing `binArgs = list(quantiles = N)`.
#' For example, `binArgs = list(quantiles = 4)` will divide the data into four equal quantiles (0%-25%, 25%-50%, 50%-75%, 75%-100%).
#' In the resulting tables, all the counts/proportions will be the same, but you can see what the quantile ranges would be.
#' 
#' Note that this binning process will also be applied to integer values, if there are more than `maxUnique` unique integers.
#' If you ever want to force `count()`/`pdist()` to (not) do numeric binning, coerce your input to `character`.
#' For example, `count(as.character(myNumbers))`.
#'
#' @section Coersion/conversion:
#' 
#' Count and probability distributions, as well as base R [tables][base::table()]
#' can be freely converted between using the `count()`, `pdist()`, and `table()` functions.
#' What this means is that, for example:
#' 
#' + `count(x) |> table()` is the same as `table(x)`
#' + `table(x) |> count()` is the same as `count(x, na.rm = TRUE)`
#' + `count(x) |> pdist()` is the same as `pdist(x)`
#' + `pdist(x) |> count() |> table()` is the same as `count(x) |> table()`
#' + etc.
#' 
#'
#' @examples 
#' 
#' generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b', NA)
#' complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a', 'a##')
#' 
#' count(generic)
#' count(generic, na.rm = TRUE)
#' count(complex)
#' 
#' 
#' 
#' count(generic, complex)
#' count(generic, complex, sort = TRUE)
#' count(generic, complex, sort = -1)
#' count(generic, complex, sort = -1, na.rm = TRUE)
#' count(generic, complex, sort = -1, .drop = TRUE)
#' 
#' # Dimension names
#' count(Generic = generic, X = complex)
#' 
#' # HumdrumR data
#' \dontrun{
#'   humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")
#' 
#'   humData |> kern() |> count()
#'   humData |> mutate(Kern = kern(Token), Recip = recip(Token)) |> count()
#'   humData |> mutate(Kern = kern(Token),  Recip = recip(Token)) |> count(Recip, sort = TRUE)
#'   humData |> mutate(Kern = kern(Token),  Recip = recip(Token)) |> select(Recip) |> count()
#' }
#' 
#' # Numeric values
#' 
#' real <- rnorm(1000)
#' 
#' count(real)
#' count(real, binArgs = list(breaks = 40))
#' count(real, binArgs = list(breaks = 40, right = FALSE))
#' count(real, binArgs = list(quantiles = 4))
#' 
#' int <- sample(100, 30, replace = TRUE)
#' 
#' count(int)
#' count(int, binArgs = list(maxUnique = 50))
#' count(int, binArgs = list(maxUnique = 5))
#' 
#' @param ... ***Values to count.***
#' 
#' Either one or more vectors of equal length, a [humdrumR object][humdrumRclass],
#' or a [table][table()].
#' 
#' @param sort ***Should the output table be sorted?***
#' 
#' Defaults to `FALSE`.
#' 
#' Either a single `logical` value (on or off), or a single numeric value.
#' 
#' Positive values (or `TRUE`) lead to decreasing sort (top to bottom);
#' Negative values lead to increasing sort;
#' Zero or `FALSE` lead to no sort.
#' 
#' @param na.rm ***Should `NA` values be removed (not counted)?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, `NA` values are not counted.
#' 
#' @param .drop ***Should missing levels be dropped (not counted as zeros)?***
#' 
#' Defaults to `FALSE`. (This is opposite of [dplyr's][dplyr::count] default.)
#' 
#' Must be singleton `logical` value: an on/off switch.
#' 
#' If `TRUE`, missing factor levels and/or missing combinations of values
#' are *not* counted in the output table.
#' If `FALSE`, these values are included in the output table (as zeros).
#'
#' @param binArgs ***List of arguments to pass to numeric binning algorithm.***
#' 
#' Defaults to empty `list()`.
#'
#' Possible list arguments include any arguments to [hist()], as well as:
#' 
#' + `maxUnique` (single whole number), defaulting to `20`.
#' + `right` (single `logical`), defaulting to `TRUE`, 
#' + `quantiles` (single whole number), defaulting to `0` (no quantiles).
#' 
#' Note that the `binArgs` argument has no effect if the input (`...`) are not numbers.
#'
#' @seealso These functions create [distribution] objects.
#' @name count
#' @export 
count.default <- function(..., sort = FALSE, na.rm = FALSE,
                          .drop = FALSE, binArgs = list()) {
  checks(sort, xTF | (xnumber & xlen1))
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
  
  colnames(result)[seq_along(argdf)] <- colnames(argdf)
  # if (na.rm) result <- result[Reduce('&', lapply(result[ , varnames, drop = FALSE], \(col) !is.na(col))), , drop = FALSE]
  
  dist <- distribution(result, 'n')
  
  if (.drop) dist <- dist |> filter(n > 0L)
  
  if (sort) sort(dist, decreasing = sort > 0L) else dist
  
  
  
}

#' @rdname count
#' @export
count.humdrumR <- function(x, ..., sort = FALSE, na.rm = FALSE, .drop = FALSE, binArgs = list()) {
  quos <- rlang::enquos(...)
  
  counts <- if (length(quos)) {
    names(quos) <- ifelse(.names(quos) == '', sapply(quos, rlang::as_label), .names(quos))
    quo <- rlang::quo(with(x, count.default(!!!quos, sort = !!sort, na.rm = !!na.rm, .drop = !!.drop, binArgs = binArgs)))
    rlang::eval_tidy(quo)
    
  } else {
    fields <- pullFields(x, union(selectedFields(x), getGroupingFields(x)), null = 'asis')
    do.call('count.default', c(as.list(fields), list(sort = sort, na.rm = na.rm, .drop = .drop, binArgs = binArgs)))
  }
  
  counts
}



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


#' @export
count.probability <- function(x, ..., sort = FALSE,
                        na.rm = FALSE,
                        .drop = FALSE) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  
  
  df <- as.data.frame(x)
  
  if (length(x@Condition)) {
    vars <- do.call('paste', c(df[ , x@Condition, drop = FALSE], list(sep = '.')))
    df$n <- as.integer(round(df$p * df@N[vars]))
    
  } else {
    df$n <- as.integer(round(df$p * x@N))
  }
  df$p <- NULL
  
  dist <- distribution(df, 'n')
  
  if (na.rm) dist <- dist[Reduce('&', lapply(getLevels(dist), \(col) !is.na(col))), ]
  if (sort) dist <- sort(dist, decreasing = sort > 0L)
  
  dist 
}

### pdist() -----



#' Tabulate and/or cross-tabulate proportions of data
#' 
#' The `pdist()` function is identical to `count()`, except it produces (empirical) probability [distributions][distribution], 
#' simply by dividing by the sum of all counts or, if a `condition` is indicated, the marginal sums across one or more dimensions.
#' The later case produces a conditional probability distribution.
#' The function of `pdist()` is similar to using [base::prop.table()].
#' 
#' @section Conditional probability:
#' 
#' By default, `pdist(x)` produces a table which is essentially identical to `count(x) / length(x)`, or
#' `count(x, y, ...) / length(x)` for multi-dimensional arrays. 
#' This means the default is the [marginal probability](https://en.wikipedia.org/wiki/Marginal_distribution) (for one variable) 
#' or the [joint probability](https://en.wikipedia.org/wiki/Joint_probability_distribution) (for more than one variables).
#' 
#' If more than variables are present, `pdist()` can also the 
#' [conditional probabilities](https://en.wikipedia.org/wiki/Conditional_probability_distribution), conditioned
#' on one or more of the variable.
#' (There can be `K - 1` conditions, where `K` is the total number of variables.)
#' Conditions can be expressed as either natural numbers (indicating which variable(s) to condition on
#'  in their input order) or as character strings exactly matching dimension names.
#' Thus, if we call something like `pdist(X = x, Y = y)`, we could condition on the `y` variable *either*
#' by saying `pdist(X = x, Y = y, condition = 2)` or `pdist(X = x, Y = y, condition = "Y")`.
#' 
#' @param condition ***Compute conditional entropy/information, conditioned on this variable.***
#' 
#' Defaults to `NULL` (no condition), so the joint entropy is calculated.
#' 
#' Must be a non-empty `character` string, which matches the name of one or of the named variables
#' in the distribution, or a positive whole number which indexes the variables.
#'
#' @examples
#' 
#' chord <- c('I', 'I', 'I', 'I', 'V', 'V', 'V', 'V', 'IV', 'IV', 'IV', 'IV')
#' note  <- c('1', '3', '5', '5', '5', '7', '4', '2', '6',  '1',   '4',  '4')
#' 
#' entropy(chord, note) # joint entropy
#' entropy(chord, note, condition = 'note') # conditional entropy
#' 
#' entropy_by(chord, note, condition = 'note') # conditional entropy, by condition
#' 
#' 
#' @rdname count
#' @export
pdist <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(.drop, xTF)
  checks(binArgs, xclass('list'))
  
  UseMethod('pdist')
}

#' @export
pdist.count <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE) {
  # 
  # if (na.rm) x <- x[!Reduce('|', lapply(getLevels(x), is.na)), ]
  # if (sort) x <- sort(x, sort > 0L)
  checks(condition, xnull | (xcharacter & xlegal(varnames(x))) | (xnatural & xmax(length(varnames(x)))))
  
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

#' @export
pdist.probability <-  function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
  checks(condition, xnull | (xcharacter & xlegal(varnames(x))) | (xnatural & xmax(length(varnames(x)))))
  
  exprs <- rlang::enexprs(...)
  if (length(exprs)) condition <- pexprs(exprs, colnames(x), condition)$Condition %||% condition
  
  x <- if (!is.null(condition)) conditional(x, condition) else unconditional(x)
  
  if (na.rm) x <- x[Reduce('&', lapply(getLevels(x), \(col) !is.na(col))), ]
  if (sort) x <- sort(x, decreasing = sort > 0L)
  
  x
  
}


#' @rdname distribution
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

#' @rdname distribution
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






#' @rdname distribution
#' @export 
pdist.humdrumR <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE, .drop = FALSE, binArgs = list()) {
            
            quos <- rlang::enexprs(...)
            
            humtab <- getHumtab(x, 'D')
            if (length(quos)) {
              names(quos) <- ifelse(.names(quos) == '', sapply(quos, rlang::as_label), .names(quos))
              
              rlang::eval_tidy(rlang::expr(pdist(humtab, !!!quos, condition = !!condition, na.rm = !!na.rm, sort = !!sort, .drop = !!.drop, binArgs = !!binArgs)))
            } else {
              selectedFields <- selectedFields(x)
              names(selectedFields) <- selectedFields
              
              do.call('pdist', 
                      c(list(humtab), 
                        as.list(selectedFields),
                        list(condition = condition, na.rm = na.rm, sort = sort, .drop = .drop, binArgs = binArgs)))
              
            }
}




#' @export
pdist.table <- function(x, ..., condition = NULL, na.rm = FALSE, sort = FALSE) {
  
  if (all(x == round(x))) return(pdist(count(x, ..., na.rm = na.rm, sort = sort), condition = condition))
  

  
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
#' These functions are closely connected to our [distribution] functions, which can be used to calculate/estimate the probability of data observations.
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
#' HumdrumR's [count()] and [pdist()] methods can be used calculate empirical
#' distributions from atomic data.
#' For numeric data we can also use R's standard [stats::density()] function to estimate the continuous probability density.
#' 
#' The `entropy()` function takes an object representing a probability distribution---ideally a humdrumR [distribution] object,
#' base-R [table], or a [density()] object (for continuous variables)---and returns the entropy, defaulting to base-2 entropy ("bits").
#' However, if you are lazy, you can pass `entropy()` atomic data vectors directly and it will automatically pass them to the [pdist()] or [stats::density()]
#' functions for you; for example, if you want to calculate the joint entropy of variables `x` and `y` (which must be the same length),
#'  you can either call `entropy(pdist(x, y))` 
#' or just `entropy(x, y)`.
#' Other arguments can be provided to [pdist()] as well; notably, if you want to calculate the *conditional* entropy,
#' you can, for example, say `entropy(x, y, condition = 'y')`.
#'
#' Using the `info()` function is similar to calling `entropy()` directly on data vectors:
#' anywhere where you can call `entropy(x, y)`, you can call `info(x, y)` instead.
#' The difference is that `info()` will return a vector of numbers representing the information content of each input observation.
#' By definition, the entropy of the data distribution is the average of all these point-wise information values: thus, `mean(info(x, y)) == entropy(x, y)`.

#' 
#' @section Cross entropy:
#' 
#' In many cases, we simply use entropy/information content to describe a set of data.
#' In this case, the data we observe and the probability model (distribution) are the
#' same---i.e., the probability model is the ("empirical") distribution of the data itself.
#' However, we can also use a *different model*---in this case, a different probability distribution---to describe data.
#' This is called the [cross entropy](https://en.wikipedia.org/wiki/Cross-entropy), and can be interpreted as a measure
#' of how well the model fits the data.
#' The cross entropy is lowest when the model exactly matches the data, when it will be the same as the
#' normal ("self") entropy.
#' If the model doesn't exactly match the data, the cross entropy will be higher then the self entropy.
#' If the data matches the model well, the cross entropy will be a little bit higher than the self entropy; if the data matches the model poorly,
#' the cross entropy can be much higher.
#' The difference between the cross entropy and the self entropy is always positive (or zero), and is called the 
#' [Kullback-Leibler Divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence) (KLD).
#' 
#' To calculate cross entropy, use the `xentropy()` command.
#' (The Kullback-Leibler Divergence can be calculated in the same way using the `kld()` function.)
#' The `xentropy()` command works just like the entropy command, except you need to provide it a `model` argument, which must be 
#' *another* probability distribution.
#' Note that the data and the model have to have the **exact** same variable names or `humdrumR` will throw an error!
#' Name your arguments to avoid this (this is illustrated in the example below, where we name everything `X`).
#' 
#' To illustrate the propertise of cross entropy, lets create three sets of data, two of which are similar,
#' and one which is very different:
#' 
#' ```
#' set.seed(1)
#' dorian <- c('A', 'B', 'C', 'D', 'E', 'F#', 'G')
#' N <- 1000
#' 
#' sample1 <- sample(dorian, N, replace = TRUE, prob = 7:1)
#' sample2 <- sample(dorian, N, replace = TRUE, prob = 7:1) 
#' sample3 <- sample(dorian, N, replace = TRUE, prob = 1:7) 
#' 
#' 
#' ## first the self entropy
#' entropy(X = sample1) # 2.607
#' entropy(X = sample2) # 2.597
#' entropy(X = sample3) # 2.592
#' 
#' ## now the cross entropy
#' 
#' xentropy(X = sample1, model = pdist(X = sample2)) # 2.619
#' xentropy(X = sample2, model = pdist(X = sample2)) # 2.597 (same as self entropy above)
#' xentropy(X = sample3, model = pdist(X = sample2)) # 3.538
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
#' The output of `info()` is identical to the log of the modeled 
#' [likelihood](https://en.wikipedia.org/wiki/Likelihood_function) of each data point, 
#' which can be computed using the [like()] function.
#' Literally, `info(x, base) == log(like(x), base)`.
#' The [like()] function works just like `info()`, computing pointwise probabilities for each data point based on the 
#' probability distribution in `model`.
#' However, we can use it to, for example, calculate the total *log likelihood* of data using `sum(log(like(...)))`.
#' This value divided by N is the cross entropy (make sure to use the right log base!): `-sum(log(like(...), base = 2)) == xentropy(...)`.
#' 
#' 
#' @param ... ***Distribution (or atomic vectors) to compute entropy/information of.***
#' 
#' Must either be a distribution object (created by [table()], [density()], [count()], or [pdist()]), or one
#' or more atomic vectors of equal length.
#'
#' If atomic vectors are provided, and `model` is missing, the atomic vectors are passed to
#' [pdist()] in order to calculate the `model`.
#' 
#' @param model ***The expected probability model.***
#' 
#' Must either be omitted (not allowed in calls to `xentropy()`) or must be a probability distribution created by [pdist()].
#' 
#' In calls to `entropy()` or `info()`, if `model` is missing, `...` arguments are used to generate the `model`.
#' 
#' @param base ***The logarithmic base.***
#' 
#' Defaults to `2`, so information is measured in units "bits."
#' 
#' Must be a single, non-zero positive number.
#'
#' Use `base = exp(1)` for natural-log "nats," or `base = 10` for Hartley/"dits".
#' 
#' @param condition ***Compute conditional entropy/information, conditioned on this variable.***
#' 
#' Defaults to `NULL` (no condition), so the joint entropy is calculated.
#' 
#' Must be a non-empty `character` string, which matches the name of one or of the named variables
#' in the distribution, or a positive whole number which indexes the variables.
#' 
#' This argument is simply passed to [pdist()].
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
#' These functions model [conditional entropy](https://en.wikipedia.org/wiki/Conditional_entropy)
#' as a dynamic process.
#' The `entropy_by()` function returns the entropy of outcome variables, grouped
#' by conditioning variables, with a separate (isolated) entropy values for each condition.
#' The `pentropy()` function computes and returns these same values in a vectorized form,
#' with the entropy of each condition returned at each point in the input vectors.
#' This "pointwise-entropy" is often used as a model of dynamic "uncertainty" in music.
#' 
#' @details 
#'
#' The formal information theoretic notion of[conditional entropy](https://en.wikipedia.org/wiki/Conditional_entropy)
#' is a description of how much entropy is observed in explained variables when conditioned on other variables,
#' but *averaged over all possible conditions and outcomes* in the proportions they appear in the distribution.
#' This "proper" conditional entropy is always a single value, and can be computed using
#' humdrumR's [entropy(..., condition = 'var')][entropy()] command.
#' 
#' Consider the following sequence: A, B, A, B, A, B, A, B, C, A, B.
#' If we compute the conditional probability of each letter, conditioned on the previous letter,
#' we get the conditional entropy
#' 
#' ```
#' seq <- c('A', 'B', 'A', 'B', 'A', 'B', 'A', 'B', 'C', 'A', 'B', 'C', 'B')
#' 
#' entropy(From = lag(seq,-1), To = seq, condition = 'From', na.rm = TRUE) 
#' 
#' ```
#' 
#' The result is `0.5954373` bits of entropy, which is pretty low.
#' However, if we analyze the conditions in the this sequence closely we can observe a few things:
#' 
#' + When the previous value is A, the next value is *always* B---so the entropy is `0`.
#' + When the previous value is B, the next value is A three times, and C once---so the entropy of *that*
#' distribution would be `entropy(c('A', 'A','A', 'C')) = .8112781`.
#' + Finally, when the previous value is C, the next values is always A---so again, the entropy is `0`.
#' 
#' However, these three outcomes don't occur equally often. 
#' If we walked through this sequence, we'd see five As (`0` each), four Bs (`.81` each), and one C (`0` zero).
#' If we average six `0` and four `.8112781` we get...`0.3245112`. That's the conditional entropy which we computed above;
#' The *average* conditional entropy over the sequence.
#' In music research practice, sometimes we don't just want to know the overall conditional entropy.
#' Rather, we want to keep track of the dynamic changes in the conditional entropy, as we did above.
#' This is the purpose of the `entropy_by()` and `pentropy()` functions.
#' 
#' The `entropy_by()` and `pentropy()` functions can be use just like the [entropy()] and [info()] functions respectively,
#' except 1) there must be at least two dimensions and 2) a `condition` *must* be supplied,
#' as a `character` string matching dimension names or whole number indices of dimensions.
#' Unlike [entropy()], `entropy_by()` will return a vector of entropy values,
#' corresponding to each combination of levels in the `condition` arguments.
#' Unlike [info()], the output of `pentropy()` represents the entropy
#' at each index, conditioned on all the `condition` variables, 
#' not the information content of each observed data point.
#' 
#' @param ... ***Distribution (or atomic vectors) to compute entropy of.***
#' 
#' Must either be a distribution object (created by [table()], [density()], [count()], or [pdist()]), or one
#' or more atomic vectors of equal length.
#'
#' 
#' @param base ***The logarithmic base.***
#' 
#' Defaults to `2`, so information is measured in units "bits."
#' 
#' Must be a single, non-zero positive number.
#'
#' Use `base = exp(1)` for natural-log "nats," or `base = 10` for Hartley/"dits".
#' 
#' @param condition ***Compute conditional entropy, conditioned on this variable.***
#' 
#' Must be a non-empty `character` string, which matches the name of one or of the named variables
#' in the distribution, or a positive whole number which indexes the variables.
#' 
#' 
#' 
#' @family {Information theory functions} 
#' @seealso The HumdrumR [information theory][information] overview.
#' @export
entropy_by <- function(..., condition, independent = TRUE, base = 2) {
  checks(base, xlen1 & xnumber & xpositive)

  UseMethod('entropy_by')
}


#' @rdname entropy_by
#' @export
entropy_by.probability <-  function(pdist, condition, independent = TRUE, base = 2) {
  varnames <- varnames(pdist)
  checks(condition, xlen & ((xcharacter & xlegal(varnames)) | (xnatural & xmax(length(varnames)))))
  
  if (!is.null(pdist@Condition) && any(base %in% pdist@Condition)) {
    pdist <- conditional(unconditional(pdist), setdiff(pdist@Condition, base))
  }
  
  # if (length(setdiff(condition, varnames))) .stop("We can't group this probability distribution by non-existent variable<s|>",
                                                 # "{harvard(setdiff(by, varnames), 'or', TRUE)}.", ifelse = length(setdiff(condition, varnames)) > 1)
  
  grouping <- as.data.frame(pdist)[ , condition , drop = FALSE]
  
  expected <- if (!independent) unconditional(pdist)$p else tapply_inplace(unconditional(pdist)$p, as.data.frame(pdist)[ , condition , drop = FALSE], \(x) x / sum(x))
  observed <- tapply_inplace(unconditional(pdist)$p, as.data.frame(pdist)[ , c(pdist@Condition, condition) , drop = FALSE], \(x) log(x / sum(x), base = base))
# 
#   Hs <- tapply(pdist$p, ,
#                \(ps) {
#                  browser()
#                  if (independent) ps <- ps / sum(ps)
#                  -sum(ps * log(ps, base = base), na.rm = TRUE)
#                })
  
  Hs <- -tapply(expected * observed, as.data.frame(pdist)[ , condition , drop = FALSE], sum, na.rm = TRUE)
  
  equation <- Pequation(pdist[ , setdiff(varnames, condition)], 'H')
  equation <- gsub('\\)$', '', equation)
  equations <- paste0(equation, ';', 
                      paste(condition, collapse = '.'), '=',
                      names(Hs), ')')
  names(Hs) <- equations
  Hs
  
}

#' @rdname entropy_by
#' @export
entropy_by.default <- function(..., condition, independent = TRUE, base = 2) {
  entropy_by(pdist(...), condition = condition, independent = independent, base = base)
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

#' Calculate mutual information between variables 
#'
#' The [mutual information](https://en.wikipedia.org/wiki/Mutual_information) is a measure of how statistically
#' dependent two variables are: in information theory terms, how much information about one variable
#' is learned from observing the other variable(s).
#' The overall mutual information can be calculated using `mutual()` (analogous to [entropy()]),
#' while the point-wise mutual information can be calculated using `pmutual()` (analogous to [info()]).
#'
#' @details
#' 
#' Mutual information is a property of probability distributions over two or more variables.
#' HumdrumR's [count()] and [pdist()] methods can be used to calculate empirical
#' distributions over atomic data, and we can then calculate their mutual information.
#'
#' The `mutual()` and `pmutual()` functions are called just like [entropy()] and [info()].
#' `mutual()` can be provided a probability [distribution] (made with [pdist()]), or can 
#' be directly provided two or more 
#' atomic vectors, which are simply passed to `pdist()`; in other words, `mutual(x, y) == mutual(pdist(x, y))`.
#' `pmutual()`, like [info()], can only be passed raw atomic vectors, like `pmutual(x, y)`.
#' Note that, unlike the entropy functions, the mutual information functions will throw an error if you only
#' provide them a single variable.
#'
#' @section Further explanation:
#' 
#' If two (or more) variables are statistically independent, their joint entropy will be the sum of their
#' independent entropies.
#' 
#' \deqn{
#' H(X, Y) = H(X) + H(Y)
#' }
#' 
#' However, *if they are not independent*, their joint entropy will be less than the summed independent entropies.
#' The mutual information is the difference between the summed independent entropies and their actual observed
#' joint entropy.
#' 
#' \deqn{
#' I(X,Y) = (H(X) + H(Y)) - H(X,Y)
#' }
#' 
#' 
#' For the point-wise mutual information, we get a single value for each data observation.
#' The value represents the difference between the observed joint likelihood of each observation
#' and the value we'd expect if the variable were independent.
#' For example, consider the binary variables "person likes heavy metal" (\eqn{P(metal)}) and 
#' "person plays electric guitar" (\eqn{P(guitar)}).
#' Imagine that \eqn{P(metal) = 0.05} and \eqn{P(guitar) = 0.1}.
#' If these two variables are independent, we'd expect that the joint probability of liking heavy metal
#' *and* playing guitar would be \eqn{\bar{P(metal, guitar)} = .05 * 0.1 = 0.005} (one out of 200 people).
#' However, on measuring some data, we might find that actually one in fifty people 
#' like metal *and* play guitar (\eqn{P(metal, guitar) = 0.02}).
#' This means that the combination of liking metal and playing guitar is \eqn{\frac{0.02}{0.005} = 4}
#' times more likely than we'd expect if they were independent.
#' This would translate to a point-wise mutual information of (using default base-2 "bits") \eqn{\\log_2(4) = +2}.
#' The overall mutual information is the average over all the point-wise values 
#' (including other combinations, like heavy metal fans who don't play guitar).
#' 
#' 
#'
#' @examples
#' 
#' guitar <- c(T, T, T, T, T, T, T, T, F, F, F, F, F, F, F, F)
#' metal <- c(T, T, T, T,T,T,F,F,T,T,F,F,F,F,F,F)
#' 
#' mutual(pdist(guitar, metal))
#' mutual(guitar, metal)
#' 
#' pmutual(guitar, metal)
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
  if (length(varnames) < 2L) .stop("Can't calculate the mutual information of a single variable.")
  
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
    if (is.null(condition)) condition <- names(df)[-1]
    model <- pdist(..., condition = condition, na.rm = na.rm, .drop = .drop, binArgs = binArgs)
  }
  
  if (is.null(model@Condition)) .stop("pentropy() requires a conditional probability model. The model provided has no condition variables.")
  
  conditions <- model@Condition
  model <- as.data.frame(model)
  entropymat <- tapply(model$p, model[ , conditions, drop = FALSE], 
                       \(P) {
                         P <- P / sum(P)
                         -sum(P * log(P, base = base), na.rm = TRUE)
                       })
  
  entropymat[as.matrix(df[,conditions, drop=FALSE])]
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
  names(dim(tab)) <- NULL
  dimnames(tab) <- levels
  
  class(tab) <- 'table'
  tab
  
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

#' @rdname distribution
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
#' @rdname distribution
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
#' @rdname distribution
setMethod('table', 'distribution',
          function(x) {
            humdrumR.table(as.table(x))
          })
