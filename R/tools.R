#
allnamed <- function(x) { !is.null(names(x)) && !any(names(x) == '')}


#' @export
compose <- function(...) {
  funcs <- rev(list(...))


  forms <- fargs(funcs[[1]])

  bod <- call('Reduce',
              quote(function(f, arg) f(arg)),
              quote(funcs),
              init = quote(temp),
              right = quote(TRUE))
  bod[names(bod) == 'init'] <- list(init = as.name(names(forms)[1]))

  newfunc <- function() {  }
  body(newfunc) <- bod
  formals(newfunc) <- forms[1]

  newfunc
  }

#' @export
`%.%` <- function(e1, e2) { compose(e2, e1) }

allsame <- function(x) length(unique(x)) == 1L

`%class%` <- function(object, newclass){
  class(object) = append(newclass, class(object))
  object
}

popclass <- function(object) `class<-`(object, class(object)[-1])


#' @export
curriedfunction <- function(args, expr) {

  func <- function() {}
  formals(func) <- args

  isstring <- tryCatch(is.character(expr), error = function(e) FALSE)
  body(func) <- if (isstring) string2expr(expr) else substitute(expr)

  func <- prependExpression('if (any(misses)) {
                              formals(.self)[!misses] <- mget(names(.selfargs)[!misses])
                              formals(.self) <- formals(.self)[order(misses, decreasing = TRUE)]
                              environment(.self) <- environment()
                              if (sum(misses) == 1) {
                                  body(.self) <- parse(text = deparse(body(.self))[-2:-17])
                              } else {
                                  .self <- .self %class% "curried"
                              }
                              return(.self)}', func)
  # alternative approach -> formals(func) <- formals()[misses]
  func <- prependExpression('.self <- sys.function()
                            .selfargs <- formals(.self)
                            misses <- unlist(lapply(mget(names(.selfargs)), function(form) all(deparse(form) == "")))', func)
  func %class% 'curried'
}

#' @export
EQ <- function(pat) {
  func <- function(x) {
    c('pat', 'x') %<-% match_size(pat,x)
    ifelse(is.na(pat), is.na(x), x == pat)
  }

  attr(func, 'FuncNames') <- glue('=={deparse(pat)}')

  new('predicate.function', func, string = glue::glue('x == {deparse(pat)}'))
}

#' @export
GT <- function(n) {
  func <- function(x) x > n
  attr(func, 'FuncNames') <- glue('>{deparse(n)}')
  new('predicate.function', func, string = glue::glue('x > {deparse(n)}'))
}

#' @export
RE <- function(pat) {
  func <- function(x)  if (!hasdim(x)) grepl(pat, x) else greplmat(pat, x)
  attr(func, 'FuncNames') <- glue('~{deparse(pat)}')
  new('predicate.function', func, string = glue::glue('x ~ {deparse(pat)}'))
}

#' @export
LT <- function(n) {
  func <- function(x) x < n
  attr(func, 'FuncNames') <- glue('<{deparse(n)}')
  new('predicate.function', func, string = glue::glue('x < {deparse(n)}'))
}
#' @export
grepls <- function(patterns, string, combine = any) {
  if (len1(patterns)) return(grepl(patterns, string))

  matches <- matrix(sapply(patterns, grepl, x = string), ncol = length(patterns))
  apply(matches, 1, combine)

}


#' Lazy version of base::ifelse

#' This function is exactly like \code{\link{base::ifelse}}, except it is lazy.
#' \code{\link{base::ifelse}} applies the x and y cases to the whole vector,
#' regardless of the condition. IfElse only computes the output y where actually 
#' asked to.
#' @export
IfElse <- function(true, yes, no) {
  out <- no
  if (any(true)) out[true] <- yes[true]
  out
}


#' @export
init <- function(x, n = 1) {
  if (len0(x)) return(vector(mode(x)))

  if (!hasdim(x)) x[1 : (length(x) - n)] else x[1 : (nrow(x) - n), , drop = FALSE]
}


`%len==%` <- function (x, y) 
{
    xl <- if (!hasdim(x)) {
        length(x)
    }
    else {
        nrow(x)
    }
    yl <- if (is.integer(y)) {
        y
    }
    else {
        if (is.null(dim(y))) {
            length(y)
        }
        else {
            nrow(y)
        }
    }
    xl == yl
}

`%len>%` <- function (x, y) 
{
    xl <- if (!hasdim(x)) {
        length(x)
    }
    else {
        nrow(x)
    }
    yl <- if (is.integer(y)) {
        y
    }
    else {
        if (is.null(dim(y))) {
            length(y)
        }
        else {
            nrow(y)
        }
    }
    xl > yl
}

len1 <- function(x) {if (!hasdim(x)) length(x) else nrow(x)} == 1L
len0 <- function(x) {if (!hasdim(x)) length(x) else nrow(x)} == 0L
lennot0 <- function(x) {if (!hasdim(x)) length(x) else nrow(x)} != 0L

match_size <- function(..., size.out = max, margin = 1, toEnv = FALSE) {
  stuff <- list(...)

  if (is.function(size.out)) {
    sizes <- sapply(stuff,
                    function(thing) {
                      dim <- dim(thing)
                      if (is.null(dim)) length(thing) else dim[margin]
                    })

    size.out <- size.out(sizes)
  }

  output <- lapply(stuff, Repeat, length.out = size.out, margin = margin)

  if (toEnv) list2env(output[names(output != '')], envir = parent.frame(1))

  if (toEnv) invisible(output) else output

}

#' @export
num2str <- function(n, pad = FALSE) format(n, digits = 3, trim = !pad, zero.print = T, big.mark = ',', justify = 'right')

#' @export
num2word <- function(num) {
  words = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
            'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen')
  tens = c('', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')


  out = num
  out[num < 101] = unlist(lapply(num[num < 101],
                                 function(n) {
                                   if(n == 100) return('one-hundred')
                                   if(n < 20) { words[n + 1]  } else {
                                    gsub('-zero$', '', paste0(tens[1 + floor(n / 10)], '-', words[n %% 10 + 1]))
                                   }
                                   }
                                 )
                          )
  out
}

padder <- function(strs, sizes = max(nchar(strs)) + 1) {unlist(Map(stri_pad_left, strs, sizes))}

##Rotate

#' @export
rotate <- function(obj, rotation = 1, wrap = FALSE, pad = NA, ...) UseMethod('rotate')



#' @export
rotate.data.frame <- function(df, rotation = 1, margin = 1, wrap = FALSE, pad = NA) {
         df[] <- rotate.matrix(as.matrix(df), rotation = rotation, margin = margin, wrap = wrap, pad = pad)
         Map(rotation, margin,
             f = function(r, m) {
                              newnames <- rotate(dimnames(df)[[m]], r, wrap = wrap, pad = '')
                              dimnames(df)[[m]] <<- make.names(newnames, TRUE)
                       }
             )
         df
}
#' @export
rotate.default <- function(obj, rotation = 1, wrap = FALSE, pad = NA) {
          rotation <- rotation[1]

          size <- length(obj)
          rotation = sign(rotation) * (abs(rotation) %% size) #if rotation is greater than size, or negative, modulo
          if (rotation == 0) return(obj)

          ind <- seq_len(size) - rotation

          if (wrap) ind <- ind %mod% size else ind[ind > size | ind < 1] <- NA

          output <- obj[ind]

          if (!is.na(pad)) output[which(is.na(ind))] <- pad

          output
}

#' @export
rotate.matrix <- function(mat, rotation = 1, margin = 1, wrap = FALSE, pad = NA) {
          if ( margin %len>% 1L ) {
                    rest.mar <- margin[-1]
                    margin   <- margin[1]

                    rest.rot <- if (rotation %len>% 1L ) rotation[-1] else rotation

                    on.exit(return(Recall(output, rotation = rest.rot, margin = rest.mar, wrap = wrap))        )
          }
          rotation <- rotation[1]

          size <- dim(mat)[margin]
          rotation = sign(rotation) * (abs(rotation) %% size) #if rotation is greater than size, or negative, modulo
          if (rotation == 0) return(mat)

          ind <- seq_len(size) - rotation

          if (wrap) ind <- ind %mod% size else ind[ind > size | ind < 1] <- NA

          calls <- alist(mat, i = , j = )
          calls[[margin + 1]] <- ind

          output <- do.call('[', calls)

          if (!is.na(pad)) {
                    calls[[margin + 1]] <- which(is.na(ind))
                    calls$value <- pad
                    calls[[1]] <- output
                    output <- do.call('[<-', calls)

          }

          output
}


`%splat|%` <- function(obj, func) {
  if (is.atomic(obj)) obj <- as.list(obj)
  do.call(func, obj)
}

trimLongString <- function(strs, n = 20L) {
  strs[str_length(strs) > n] <- paste0(stri_trim_both(str_sub(strs[str_length(strs) > n], end = n)), '...')
  strs
}
