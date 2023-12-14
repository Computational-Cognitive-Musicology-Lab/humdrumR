# S4 Distributions ----


## Definitions ----


# setClass('text', contains = 'character')



#' Distributions
#' 
#' HUmdrumR has ways to...
#' 
#' @name distributions
NULL



### Constructors ----

distribution <- function(x, ...) {
  df <- as.data.frame(x, ...)
  class(df) <- c(if ('P' %in% colnames(df)) 'probability' else 'count', 
                 'distribution', 
                 class(df))
  
  attr(df, 'sorted') <- attr(df, 'sorted') %||% 0L
  
  df
}


humdrumR.table <- function(tab) {
  class(tab) <- unique(c('humdrumR.table', class(tab)))
  tab
}

### Accessors ----



dist_type <- function(dist) c(probability = 'P', count = 'N')[class(dist)[1]]
getNP <- function(dist) as.data.frame(dist)[ , grepl('^[NP][12]*$', colnames(dist)), drop = FALSE]
getFactors <- function(dist) as.data.frame(dist)[ , !grepl('^[NP][12]*$', colnames(dist)), drop = FALSE]

dimnames.distribution <- function(x) {
  grep('^[NP][12]*$', colnames(x), value = TRUE, invert = TRUE)
}

### print() ----


#' @export
#' @rdname distributions
print.distribution <- function(dist, digits = if (inherits(dist, 'probability')) 3 else 1,
                               syntaxHighlight = humdrumRoption('syntaxHighlight')) {
  
  type <- dist_type(dist)
  dimnames <- dimnames(dist)
  sort <- attr(dist, 'sort') 
  prettier <- if (type == 'N') prettyN else prettyP
  Ns <- lapply(getNP(dist), prettier, digits = digits)
  
  # do we scale or round?
  scale <- unique(lapply(Ns, attr, which = 'scale') |> unlist())
  approx <- Reduce('|', sapply(Ns, attr, which = 'approx'))
  
  scale <- if (length(scale) == 1L) {
    if (scale > 0) paste0(' (', c('thousands', 'millions', 'billions', 'trillions')[scale], ')')
  }
  if (approx) scale <- paste0(scale, ', ~rounded')
  
  dist <- cbind(as.data.table(getFactors(dist)), as.data.table(Ns))
  
  iswide <- FALSE
  printmat <- (if(sort == -0L && length(dimnames) >= 2L) {
    # check if we can widen
    wide <- as.matrix(dcast(dist, rlang::new_formula(quote(...), rlang::sym(dimnames[2])), fill = 0, value.var = colnames(Ns)))
    
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
  
  message <- paste0('humdrumR ',
                    if (type == 'N') 'counts' else 'probabilities',
                    scale,
                    '\n')
  
  
  if (syntaxHighlight) {
    dataCols <- !colnames(printmat) %in% c(dimnames, 'Rank')
    rankCol <- colnames(printmat) == 'Rank'
    dataRows <- !(1:nrow(printmat)) %in% c(1, nrow(printmat))
    if (iswide) dataRows <- dataRows &  !(1:nrow(printmat)) %in% c(2, nrow(printmat) - 1L)
    
    
    printmat[dataRows, dataCols] <- syntaxHighlight(printmat[dataRows, dataCols], 'D')
    printmat[dataRows, !dataCols & !rankCol] <- syntaxHighlight(printmat[dataRows, !dataCols & !rankCol], 'I')
    printmat[dataRows, rankCol] <- syntaxHighlight(printmat[dataRows, rankCol], 'E')
    printmat[!dataRows, ] <- syntaxHighlight(printmat[!dataRows, ], 'N')
    
    message <- syntaxHighlight(message, 'G')
  }


  cat(message)
  cat(apply(printmat, 1, paste, collapse = '  '), sep = '\n')
  cat(message)
  invisible(dist)
}




prettyN <- function(N, digits = 1L) {
  tens <- ifelse(N == 0, 0, log10(N) |> floor())
  
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
  Nprint <- gsub('^( *)0\\.', '\\1 .', Nprint)
  Nprint <- gsub('\\.0*$', '', Nprint)
  
  output <- paste0(Nprint, scale, ifelse(approx, '~', ''))
  output <- paste0(output, strrep(' ', max(nchar(output)) - nchar(output))) 
  
  attr(output, 'scale') <- globalscale
  attr(output, 'approx') <- any(approx)
  
  output
  
}


prettyP <- function(P, digits = 3) {
  tens <- ifelse(P == 0, 0, log10(P) |> round())
  
  thousands <- (tens + 1) %/% -3
  scale <- min(thousands)
  
  P <- P * 1000^scale
  
  Pround <- round(P, digits = digits)
  approx <- P != Pround
  
  Pprint <- format(Pround, scientific = FALSE)
  Pprint <- gsub('^( *)0\\.', '\\1 .', Pprint)
  Pprint <- gsub('\\0*$', '', Pprint)

  
  output <- paste0(Pprint, ifelse(approx, '~', ''))
  output <- paste0(output, strrep(' ', max(nchar(output)) - nchar(output))) 
  
  attr(output, 'scale') <- if (scale != 0L) paste0('10^(', -scale * 3, ')')
  attr(output, 'approx') <- any(approx)
  
  output
  
}

### indexing ----

#' @rdname distributions
#' @export
`[.distribution` <- function(x, ...) {
  result <- NextMethod()
  class(result) <- class(x)
  result
}

### coercion ----
as.data.frame.distribution <- function(x) {
  class(x) <- c('data.frame')
  x
}

as.data.table.distribution <- function(x) {
  as.data.table(as.data.frame(x))
}

as.table.distribution <- function(x) {
  x <- as.data.frame(x)
  
  
  dimnames <- setdiff(colnames(dist), c('P', 'N'))
  type <- setdiff(colnames(dist), dimnames)
  
  levels <- lapply(x[ , dimnames], unique)
  
  
  tab <- array(dim = lengths(dimnames), dimnames = levels)
  tab[as.matrix(x[ , dimnames])] <- x[[type]]
  
  humdrumR.table(tab)
  
}

### sorting ----

#' @rdname distributions
#' @export
sort.distribution <- function(x, decreasing = TRUE) {
  type <- dist_type(x)
  
  
  
  ord <- do.call('order', c(as.list(x[colnames(x) == type]), 
                            list(decreasing = decreasing)))
  
  x <- x[ord, ]
  
  attr(x, 'sort') <- if (decreasing) -1 else 1
  x
}


### aligning ----

alignDistributions <- function(d1, d2, funcname = '') {
  
  
  dimnames1 <- dimnames(d1)
  dimnames2 <- dimnames(d2)
  
  if (!setequal(dimnames1, dimnames2)) .stop("If using {funcname} on distributions, they must all have the same dimensions.")
  
  d3 <- merge(d1, d2, by = dimnames1, all = TRUE, suffixes = c('1', '2'))
  d3[!colnames(d3) %in% union(colnames(d1), colnames(d2))] <- lapply(d3[!colnames(d3) %in% union(colnames(d1), colnames(d2))], 
                                                                    \(np) ifelse(is.na(np), 0, np))
  
  
  levels <- lapply(getFactors(d1), unique)
  
  
  lapply(dimnames1,
         \(dim) {
           list(pmin(match(d3[[dim]], d1[[dim]]), 
                     match(d3[[dim]], d2[[dim]]), 
                     na.rm = TRUE),
                d3[[dim]])
         }) |> unlist(recursive = FALSE) -> orderby
  
  d3 <- d3[do.call('order', orderby), order(!colnames(d3) %in% dimnames1)]
  
  distribution(d3)
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
cbind.distribution <- function(...) {
  args <- list(...)
  dists <- sapply(args, inherits, what = 'distribution')
  
}

### arithmetic ----


setMethod('+', c('count', 'count'),
          function(e1, e2) {
            
          })

setMethod('*', c('count', 'numeric'),
          function(e1, e2) {
            Ns <- grepl('^N[1-9]?[0-9]*$', colnames(e1)) 
            e1[Ns] <- lapply(e1[Ns], `+`, e2 = e2)
            e1
            
          })

#' @export
Ops.distribution <- function(e1, e2) {
  if (!inherits(e1, 'distribution')) return(do.call(.Generic, list(e2, e1)))
  
    browser()
  NP1 <- getNP(e1)
  if (!inherits(e2, 'distribution')) {
    cbind(getFactors(e1), as.data.frame(lapply(NP1, .Generic, e2 = e2)))
    
  } else {
    
    NP2 <- getNP(e2)
    if (length(NP1) != length(NP2)) .stop("Can't do math between distributions when there are multiple distributions")
    
    frame <- alignDistributions(e1, e2, .Generic)
    frame[ , N := do.call(.Generic, list(N.x, N.y))]
    frame[ , c('N.x', 'N.y') := NULL]
    
  }
 
}





#' @rdname distributions
#' @export
setMethod('+', c('count.frame', 'count.frame'),
          \(e1, e2) {
            
            e3 <- data.table::rbindlist(list(e1, e2), fill = TRUE)
            e3 <- e3[ , list(Count = sum(Count)), by = setdiff(colnames(e3), 'Count')]
            
            new('count.frame', e3)
            
          })

#' @rdname distributions
#' @export
setMethod('-', c('count.frame', 'count.frame'),
          \(e1, e2) {
            
            e3 <- merge.count.frame(e1, e2)
            e3 <- as.data.table.count.frame(e3)
            e3$Count <- e3$Count.x - e3$Count.y
            
            e3[ , c('Count.x', 'Count.y') := NULL]
            new('count.frame', e3)
            
          })

#' @rdname distributions
#' @export
setMethod('Ops', c('count.frame', 'count.frame'),
          \(e1, e2) {
            
            e3 <- merge.count.frame(e1, e2)
            e3 <- as.data.table.count.frame(e3)
            e3$Count <- callGeneric(e3$Count.x, e3$Count.y)
            
            e3[ , c('Count.x', 'Count.y') := NULL]
            e3[]
          })







#' @rdname P
#' @export
setMethod('%*%', c('probability.frame', 'probability.frame'),
          function(x, y) {
            new('probability.frame', as.table(outer(S3Part(x), S3Part(y), '*')), N = x@N, margin = integer(0L))
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
#' The output of `count()` is a special form of R `table`, a `count.frame`.
#' Given two or more `count.frame`s, if you apply basic R operators 
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
count.humdrumR <- function(x, ..., sort = FALSE, na.rm = FALSE, exclude = NULL, .drop = FALSE) {
  quos <- rlang::enquos(...)
  
  counts <- if (length(quos)) {
    names(quos) <- sapply(quos, rlang::as_name)
    quo <- rlang::quo(with(x, count.default(!!!quos, sort = !!sort, na.rm = !!na.rm, exclude = !!exclude, .drop = !!.drop)))
    rlang::eval_tidy(quo)
    
  } else {
    fields <- pullFields(x, union(selectedFields(x), getGroupingFields(x)))
    
    do.call('count', c(as.list(fields), list(sort = sort, na.rm = na.rm, exclude = exclude, .drop = .drop)))
  }
  
  counts
}

#' @rdname distributions
#' @export
count.default <- function(..., sort = FALSE,
                          na.rm = FALSE,
                          exclude = NULL, .drop = FALSE) {
  
  checks(sort, xTF | (xwholenum & xlen1))
  checks(na.rm, xTF)
  checks(exclude, xnull | xvector)
  checks(.drop, xTF)
  
  # exprs <- rlang::enexprs(...)
  exprs <- as.list(substitute(list(...)))[-1L]
  
  args <- list(...)
  dimnames <- .names(args)
  if (any(dimnames == '')) dimnames[dimnames == ''] <- vapply(exprs[dimnames == ''], deparse, nlines = 1L, '')
  
  
  args <- lapply(args,
                 \(arg) {
                   if (inherits(arg, 'token')) factorize(arg) else arg
                 })
  
  if (length(unique(lengths(args))) > 1L) .stop("Can't cross-tabulate these vectors ({harvard(dimnames, 'and')}), because they are different lengths.")
  
  tab <- as.data.table(args)
  colnames(tab) <- dimnames
  
  if (na.rm) exclude <- union(exclude, c(NA, NaN))
  if (!is.null(exclude)) tab <- tab[Reduce('&', lapply(tab, \(col) !col %in% exclude))]
  
  
  
  result <- rlang::eval_tidy(rlang::expr(tab |> count(!!!(rlang::syms(dimnames)), sort = !!sort, name = 'N', .drop = !!.drop)))
  
  
  if (is.numeric(sort) && sort < 0) result <- result[nrow(result):1]
  
  distribution(result)
  
}

#' @rdname distributions
#' @export
count.table <- function(..., sort = FALSE,
                        na.rm = FALSE,
                        exclude = NULL, .drop = FALSE) {
  tab <- list(...)[[1]]
  probs <- any(tab < 1 & tab > 0)
  dist <- distribution(tab, responseName = if (probs) 'P' else 'N')
  
  if (sort) dist <- sort.distribution(count.frame, decreasing = sort != -1)
  
  dist
  
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
             
             if (token) makecounts(tab) else tab
  }) 


# #' @rdname distributions
# #' @export
# setMethod('table', 'token', 
          # function(x, ..., exclude = if (useNA == 'no') c(NA, NaN),
                   # useNA = 'no', dnn = names(list(...)),
                   # deparse.level = 1) {
            
            # args <- list(x, ...)
            # args <- lapply(args, factorize)
            # tab <-  do.call('table', c(args, 
                                       # list(exclude = exclude, useNA = useNA, dnn = dnn,
                                        # deparse.level = deparse.level)))
            # new('count.table', tab)
          # })



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
            
            makecounts(tab)
            
          })

#' @export
#' @rdname distributions
setMethod('table', 'count.frame',
          function(x) {
            tab <- xtabs(N ~ . , data = as.data.frame(x))
            makecounts(tab)
          })


#' @rdname distributions
#' @export
as.table.count.frame <- function(x) table(x)


### P() -----

#' Tabulate and cross proportions
#' 
#' 
#' @export
setGeneric('P', signature = 'x', function(x, ...) standardGeneric('P'))


setClassUnion("discrete", members = c('character', 'integer', 'logical', 'factor', 'token'))

#' @rdname P
#' @export
setMethod('P', 'formula', 
          function(x, ..., condition = NULL, na.rm = TRUE) {
            
            distribution <- P(count.default(x, ..., na.rm = na.rm))
            
            makeprobs(distribution)
          })

#' @rdname P
#' @export
setMethod('P', 'discrete',
          function(x, ..., condition = NULL, na.rm = TRUE) {
            
            distribution <- P(count.default(x, ..., na.rm = na.rm))
      
           makeprobs(distribution)
          })


#' @rdname P
#' @export
setMethod('P', 'numeric',
          function(x, na.rm = FALSE, ..., bw = 'SJ', adjust = 1.5) {
            
            if (!na.rm && any(is.na(x)))  {
              return(NULL)
            } else {
              x <- x[!is.na(x)]
            }
            
            density <- density(x, ..., bw = bw, adjust = adjust)
            
            frame <- data.table(X = density$x, P = density$y)
            makeprobs(frame)
            
          })

setMethod('P', 'missing',
          function(x, ..., margin = NULL, na.rm = TRUE) {
            args <- list(..., margin = margin, na.rm = na.rm)
            origname <- names(args)[1]
            names(args)[1] <- 'x'

            result <- do.call('P', args)
             
            if (is.table(result)) names(dimnames(result))[1] <- origname
            result
          })


#' @rdname distributions
#' @export 
setMethod('P', 'humdrumR',
          function(x, ..., sort = FALSE, na.rm = FALSE, exclude = NULL, .drop = FALSE) {
  quos <- rlang::enquos(...)
   probs <- if (length(quos)) {
    quo <- rlang::quo(with(x, P(!!!quos, sort = !!sort, na.rm = !!na.rm, exclude = !!exclude, .drop = !!.drop)))
    rlang::eval_tidy(quo)
    
  } else {
    fields <- pullFields(x, union(selectedFields(x), getGroupingFields(x)))
    do.call('P', c(as.list(fields), list(sort = sort, na.rm = na.rm, exclude = exclude, .drop = .drop)))
  }
   
   makeprobs(probs)
})


#' @rdname distributions
#' @export
setMethod('P', c('count.frame'),
          function(x, ..., .by = NULL, na.rm = FALSE) {
            if (na.rm) x <- x[!Reduce('|', lapply(x, is.na))]
            
            x$P <- if (is.null(.by)) {
              x[ , N / sum(N, na.rm = TRUE)]
            } else {
              x[ , N / sum(N, na.rm = TRUE), by = .by]$V1
            }
           
            N <- sum(x$N)
            x[ , N := NULL]
            
            makeprobs(x, N = N)
          })


#' @rdname P
#' @export
setMethod('P', c('table'),
          function(x, condition = NULL, na.rm = FALSE) {
            
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
            
            makeprobs(ptab, N = N)
            # new('probability.frame', ptab, N = as.integer(n), margin =  as.integer(condition))
          })



unmargin <- function(pd) {
  if (length(pd@margin) == 0L) return(pd)
 
  marginal <- proportions(pd@N)
  
  pd <- sweep(pd, pd@margin, marginal, '*')
  pd@margin <- integer(0L)
  pd@N <- sum(pd@N)
  pd
  
}






# Information Theory ----

## Likelihoods ----

###likelihood() ----

#' @export
setGeneric('likely', function(x, log, ...) standardGeneric('likely'))


## Entropy etc. ----


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
setGeneric('entropy', function(x, base = 2, ...) standardGeneric('entropy'))
#' @rdname entropy
#' @export
setMethod('entropy', 'table',
          function(x, base = 2, margin = NULL, na.rm = FALSE) {
            
            joint <- P(x, margin = NULL, na.rm = na.rm)
            
            other <- P(x, margin = margin, na.rm = na.rm)
            other <- ifelse(x == 0L, 0, log(other, base = base)) 
            
            equation <- pdist.name(joint, margin, 'H')
            setNames(-sum(joint * other), equation)
          })

  
#' @rdname entropy
#' @export
setMethod('entropy', 'probability.frame',
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
            
            p <- P(x, distribution = distribution, ..., margin = margin, na.rm = na.rm)
            
            -ifelse(p == 0, NA, log(p, base = base))

            
          })

#' @rdname entropy
#' @export
setMethod('ic', 'numeric',
          function(x, base = 2, density = NULL, ..., na.rm = TRUE) {
            
            p <- P(x, density = density, ..., na.rm = na.rm)
            
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
setMethod('mutualInfo', 'probability.frame',
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
            
            ptab <- P(x, margin = margin, na.rm = na.rm)
            mutualInfo(ptab, base = base)
            
          })

   
#' @rdname mutualInfo
#' @export
setMethod('mutualInfo', 'discrete',
          function(x, ..., base = 2, na.rm = FALSE) {
            args <- list(x, ...)
            marginals <- lapply(args, \(arg) P(count(arg)))
            
            joint <- Reduce('%*%', marginals)
            
            p_observed <- P(x, ..., margin = NULL, na.rm = na.rm)
            p_joint <- P(x, ..., distribution = joint, margin = NULL, na.rm = na.rm)
            
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
setMethod('crossEntropy', c('probability.frame', 'probability.frame'),
          function(distribution1, distribution2, base = 2) {
            
            distribution2 <- ifelse(distribution2 == 0L, 0, log(distribution2, base = base))
            
            
            equation <- paste0('H(', 
                               pdist.name(distribution1, func = ''), ', ',
                               pdist.name(distribution2, func = ''))
            setNames(-sum(distribution1 * distribution2), equation)
            
          })



