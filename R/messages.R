
# Checking arguments ----

## Check patterns ----


## argCheck class ----
setClass('argCheck', slots = c(Function = 'function', Logic = 'character', Rule = 'character', Explanation = 'character', ProblemDescription = 'function')) 


argCheck <- function(func, rule, problem, logic = 'force', explanation = character(0L)) new('argCheck',  
                                                                                               Function = func, 
                                                                                               Logic = logic, 
                                                                                               Explanation = explanation,
                                                                                               Rule = rule, 
                                                                                               ProblemDescription = problem)


setMethod('|', c('argCheck', 'argCheck') ,
          \(e1, e2) {
            argCheck(\(arg) list(e1, e2),
                     logic = '||',
                     c(e1@Rule, e2@Rule),
                     explanation = c(e1@Explanation, e2@Explanation),
                     \(arg) c(e1@ProblemDescription(arg), e2@ProblemDescription(arg)))
            
          })


setMethod('&', c('argCheck', 'argCheck') ,
          \(e1, e2) {
            argCheck(\(arg) list(e1 , e2),
                     logic = '&&',
                     c(e1@Rule, e2@Rule),
                     explanation = c(e1@Explanation, e2@Explanation),
                     \(arg) c(e1@ProblemDescription(arg), e2@ProblemDescription(arg)))
            
          })





setMethod('+', c('argCheck', 'character'),
          \(e1, e2) {
            e1@Explanation <- paste(c(e1@Explanation, e2), collapse = '; ')
            e1
          })

## Checking functionality ----

checks <- function(arg, argcheck, argname, seealso = c()) {
  
  callstack <- sys.calls()
  if (length(callstack) > 20L) return(arg) 
  
  if (missing(argname)) argname <- rlang::expr_name(rlang::enexpr(arg))
  callname <- rlang::expr_name(callstack[[1]][[1]])
  
  seealso <- c(paste0('?', callname), seealso)
  
  messages <- docheck(argcheck, arg, argname)
 
  
  if (length(messages) == 0L) return(arg)
  
  
  argprint <- if (length(arg) == 0L) {
    paste0(class(arg), '(0)')
  } else {
    paste0(if (length(arg) > 1L) 'c(', 
           harvard(head(arg, 6), quote = is.character(arg)),
           if (length(arg) > 6L) ', ...',
           if (length(arg) > 1L) ')')
    
  }
  
  # premessage <- glue::glue("\nThere is a problem with the call {callname}(..., {argname} = {argprint}): ", trim = FALSE)
  alert <- glue::glue("There is a problem with the call {rlang::expr_text(callstack[[1]])}.", trim = FALSE)

  
  ## see also
  seealso <- paste0('> See ', harvard(seealso, 'or'), ' for ', if (length(argcheck@Explanation)) 'further ' else '', 'explanation. <')
  padn <- options('width')$width - nchar(seealso)
  seealso <- paste0('\n', strrep('-', padn %/% 2), seealso, strrep('-', padn %/% 2))
            
  # message()
  # stop(call.=FALSE)
  .showstack()
  stop(call. = FALSE, paste0(alert, '\n', messages, seealso))
  # .stop(paste0(premessage, '\n', messages, seealso))
}



docheck <- function(argcheck, arg, argname) {
  result <- docheck_recurse(argcheck, arg)
  if (!result$Good)  {
    leftpad <- 16L
    
    headers <- c("Requirement", "Problem", "Explanation")
    
    headers <- paste0(strrep('!', leftpad - nchar(headers) - 1), ' ', headers, ':')
    
    pad <- paste0(strrep('!', leftpad), ': ')
    
    rule <- glue::glue(pad, "The 'argname' argument {result$Rule}", '.', trim = FALSE)
    problem <- glue::glue(pad, 'In your call, ', harvard(unique(unlist(result$Problem)), 'and'), '.', trim = FALSE)
    
    message <- c(headers[1], rule, headers[2], problem)
    
    
    if (length(result$Explanation)) {
      explanation <- paste0(pad, paste(result$Explanation, collapse = '; '), '.')
      message <- c(message, headers[3], explanation)
    }
    
    gsub('argname', argname, paste(message, collapse = '\n'))
  }
  
 
}

docheck_recurse <- function(argcheck, arg) {
  good <- argcheck@Function(arg)
  
  if (is.list(good)) {
    logic <- argcheck@Logic
    
    result1 <- docheck_recurse(good[[1]], arg)
    
    result2 <- if (logic == '&&' && !result1$Good) {
       list(Good = FALSE, Rule = c(), Problem = c(), Depth = 0L, Explanation = character(0L))
    } else {
       docheck_recurse(good[[2]], arg)
    }
    results <- list(result1, result2)
    
    goods <- sapply(results, '[[', 'Good')
    rules <- sapply(results, '[[', 'Rule')
    
    descriptions <- sapply(results, '[[', 'Problem') # functions
    explanations <- lapply(results, '[[', 'Explanation') # vectors
    
    
    # success?
    good <- match.fun(argcheck@Logic)(goods[1], goods[2])

    # How deep are we?
    depths <- sapply(results, '[[', 'Depth')
    depth <- sum(depths)
    
    # which messages etc. do we pass up?
    targets <- if (logic == '&&') {
      if (!goods[1]) {
        1
        } else {
          depth <- 1 + sum(depths)
          2
        }
    } else {
      if (!any(good) && sum(depths > 0) == 1) which.max(depths) else 1:2
    }
    
    
    rule <- paste0(rules[targets], collapse =  if (logic == '&&') ' and ' else ' or ')
    description <- unique(descriptions[targets])
    explanation <- unique(unlist(explanations[targets]))
    
    
  } else {
    depth <- 0L
    explanation <- argcheck@Explanation
    rule <- argcheck@Rule
    description <- argcheck@ProblemDescription(arg)
  }
  
  list(Good = good, Rule = rule, Depth = depth, Problem = description, Explanation = explanation)

}

dochecks <- function(arg, ...) {
  unlist(lapply(list(...), docheck, arg = arg))
}


## Common Messages ----


.show_values <- function(bad, n = 6) {
  
  uniq <- unique(bad)
  
  message <- if (length(bad) == 0L) {
    "your 'argname' is empty: {class(bad)[1]}(0)."
  } else {
    if (length(uniq) == 1L) {
      "your 'argname' includes the value {if (is.character(bad)) quotemark(bad) else bad}" 
    } else {
      "your 'argname' includes the values {harvard(head(uniq, n), 'and', is.character(bad))}"
    }
  }
  
  glue::glue(message)
}

.values <- function(arg, n = 6, conj = 'and') {
  arg <- unique(arg)
  if (is.character(arg)) arg <- quotemark(arg)
  if (length(arg) > n) {
    arg <- c(arg[1:n], glue::glue('{length(arg) - n} more values.'))
  }
  harvard(arg, conj, FALSE)
}
.show_vector <- function(arg, n = 6) {
  if (length(arg) == 0L) {
    glue::glue("{class(arg)[1]}(0).")
  } else {
    if (length(arg) == 1L) {
      if (is.character(arg)) quotemark(arg) else arg
    } else {
      arg <- if (length(arg) > n) c(head(arg, n), '...') else head(arg, n)
      glue::glue("c({paste(arg, collapse = ', ')})")
    }
    
  }
  
}



.mismatch <- function(func) {
  funcname <- rlang::expr_name(rlang::enexpr(func))
  function(arg, argname = 'argname') {
    glue::glue("{funcname}({argname}) == {.show_vector(func(arg))}")
  }
}


## Common Check functions ----

### Class ----

xnull <- argCheck(\(arg) is.null(arg), 'must be NULL', .mismatch(is.null))
xna <- argCheck(\(arg) all(is.na(arg)), 'must be NA', .mismatch(is.na))

xclass <- function(classes) {
  if ('number' %in% classes) classes <- c('numeric', 'integer', 'integer64', classes)
  
  argCheck(\(arg) class(arg)[1] %in% classes,
           # glue::glue("a {harvard(classes, 'or', quote = TRUE)} object"),
           glue::glue("must be a {paste(quotemark(classes), collapse = '|')} object"),
           .mismatch(class))
  
}

xinherits <- function(parentclass) {
  
  argCheck(\(arg) inherits(arg, parentclass),
           # glue::glue("a {harvard(classes, 'or', quote = TRUE)} object"),
           glue::glue("must be or inherit from the class {parentclass}"),
           .mismatch(class))
  
}



xhumdrumR <- xclass('humdrumR')
xcharacter <- xclass('character')
xlogical <- xclass('logical')
xinteger <- xclass('integer')
xnumeric <- xclass('numeric')

.numericClasses <- c('integer', 'integer64', 'numeric', 'rational')
xnumber  <- argCheck(\(arg) class(arg)[1] %in% .numericClasses, "must be numeric", .mismatch(class))

xatomic <- argCheck(\(arg) is.atomic(arg), "must be an atomic vector (integer, numeric, character, or logical)", .mismatch(class))
xvector <- argCheck(\(arg) is.vector(arg), "must be a vector; i.e., an atomic (integer, numeric, character, logical) vector or a list().", .mismatch(class))

xcharnotempty <-  xcharacter & argCheck(\(arg) all(nchar(arg) > 0L), 'cannot be an empty string ("")', \(arg) glue::glue("includes {sum(nchar(arg) == 0)} empty strings"))


# ...atomic <- function(arg) {
#   if (!is.atomic(arg)) {
#     glue::glue("The 'argname' argument must be an 'atomic' value; i.e., a basic R vector (either logical, numeric, integer, or character).",
#                "You have provided a {class(arg)} value, which is not atomic.")
#   }
#   
# }

### Numbers ----



xmax <- function(n) xnumber & argCheck(\(arg) all(arg <= n), glue::glue("must be {n} or less"), \(arg) .show_values(arg[arg > n]))
xmin <- function(n) xnumber & argCheck(\(arg) all(arg >= n), glue::glue("must be {n} or more"), \(arg) .show_values(arg[arg < n]))

xrange <- function(min, max) xnumber & argCheck(\(arg) all(arg >= min & arg <= max), 
                                                glue::glue("must be between {min} and {max} (inclusive)"),
                                                \(arg) .show_values(arg[arg > max | arg < min]))

xpositive       <- xnumber & argCheck(\(arg) all(arg > 0), "must be greater than zero", \(arg) .show_values(arg[arg <= 0]))
xpositiveorzero <- xnumber & argCheck(\(arg) all(arg >= 0), "must be zero or more", \(arg) .show_values(arg[arg < 0]))


xnegative       <- xnumber & argCheck(\(arg) all(arg > 0), "must be less than zero", \(arg) .show_values(arg[arg <= 0]))
xnegativeorzero <- xnumber & argCheck(\(arg) all(arg >= 0), "must be zero or less", \(arg) .show_values(arg[arg < 0]))


xnotzero <- argCheck(\(arg) all(arg != 0),
                        "must not be zero",
                        \(arg) "'argname' includes {sum(arg == 0)} {plural(sum(arg == 0), 'zeroes', 'zero')}")


xwholenum <- xnumber & argCheck(\(arg) all(arg == round(arg)), 'must be whole number(s)', \(arg) .show_values(arg[arg != round(arg)]))

xnatural <- xnumber & xmin(0) & xwholenum
xpnatural <- xnumber & xmin(1) & xwholenum

### Length -----


xmaxlength <- function(n = 1) {
  argCheck(\(arg) length(arg) <= n,
           glue::glue("can be at most {n} long"),
           .mismatch(length))
}
xminlength <- function(n = 1) {
  argCheck(\(arg) length(arg) >= n,
           glue::glue("must be at least {n} long"),
           .mismatch(length))
}

xlen1 <- argCheck(\(arg) length(arg) == 1L, "must be a single value", .mismatch(length))


xlen <- argCheck(\(arg) length(arg) > 0L, "must not be empty", .mismatch(length))

  
## Matching another argument ----

xnrowmatch <- function(match) {
  matchname <- rlang::expr_name(rlang::enexpr(match))
  rule <- glue::glue("must have one row for each index in the '{matchname}' argument")  
  nrower <- .mismatch(nrow)

  describe <- \(arg) glue::glue("{nrower(arg)} and", .mismatch(length)(match, matchname))
  argCheck(\(arg) nrow(arg) == length(match),
           rule, 
           describe)
}

xmatch <- function(match) {
  matchname <- rlang::expr_name(rlang::enexpr(match))
  
  dims <- hasdim(match)
  rule <- glue::glue("must be the same ", if (dims) "dimensions as the '{matchname}' argument" else "length as the '{matchname}' argument")  
  
  matcher <- if (dims) .mismatch(dim) else .mismatch(length)  # if must happen outside .mismatch
  describe <- \(arg) glue::glue("{matcher(arg)} and ", matcher(match, matchname))
  argCheck(\(arg) if (dims) all(ldim(arg) == ldim(match)) else length(arg) == length(match),
           rule,
           describe)

}

xmatchclass <- function(match) {
  
  matchname <- rlang::expr_name(rlang::enexpr(match))
  targetclass <- class(match)[1]
  
  argCheck(\(arg) class(arg) == targetclass,
           glue::glue("must be the same class as the '{matchname}' argument"),
           \(arg) glue::glue(.mismatch(class)(arg), 'class({matchname}) == {targetclass}'))
}





## Specific valid values ----

xnotna <- argCheck(\(arg) all(!is.na(arg)), "must not include NA values", \(arg) "'argname' includes {sum(is.na(arg))} {plural(sum(is.na(arg)), 'NAs', 'NA')}")

xTF <- argCheck(\(arg) is.logical(arg) && length(arg) == 1L,
                  "is an on/off switch: It must be a single TRUE or FALSE value",
                  \(arg) c(if (!is.logical(arg)) .mismatch(class)(arg), if (length(arg) != 1L) .mismatch(length)(arg))) & xnotna


xlegal <- function(values) {
  argCheck(\(arg) all(arg %in% values), 
           glue::glue("contains invalid values; valid values are {.values(values)}."),
           \(arg) .show_values(arg[!arg %in% values]))
}

xplegal <- function(values) {
  xatomic & argCheck(\(arg) all(!is.na(pmatch(arg, values))), glue::glue("must partial match {.values(values, conj = 'or')}"),
                     \(arg) .show_values(arg[is.na(pmatch(arg, values))]))
}

xrounding <- argCheck(\(arg) any(sapply(list(round, floor, ceiling, trunc, expand), identical,  y = arg)),
                      "must be a rounding function: round(), floor(), ceiling(), trunc(), or expand()",
                      \(arg) "is not one of these functions")



xcharclass <- function(chars, single = TRUE) {
  charclass <- paste0('^[', chars, ']+$')
  chars <- strsplit(chars, split = '')
  xcharacter & argCheck(\(arg) all(grepl(charclass, arg)),
                        glue::glue('must be made up solely of the characters {.values(chars, n = 12)}'),
                        \(arg) {
                          arg <- unique(unlist(strsplit(arg, split = '')))
                          bad <- !arg %in% chars
                          .show_values(arg[bad])
                        })
  
}

xrecordtypes <- xcharacter & xminlength(1) & xmaxlength(6) & argCheck(\(arg) all(unique(unlist(strsplit(arg, split = '')) %in% c('G', 'L', 'I', 'M', 'D', 'd'))), 
                                              glue::glue("must be a string of characters representing humdrum's six record types: {.values(c('G', 'L', 'I', 'M', 'D','d'), conj = 'or')}"),
                                              \(arg) glue::glue("'argname' includes the character(s) {.values(setdiff(unlist(strsplit(arg, split = '')), c('G', 'L', 'I', 'M', 'D', 'd')))}"))





checkTypes <- function(dataTypes, callname, argname = 'dataTypes') {
  # checks if datatypes (e.g. GLIMDd) are valid, but also 
  # returns dataTypes as a vector of single-characters
  # which is what many functions want to work with
  
  checks(dataTypes, xrecordtypes)
  unique(unlist(strsplit(dataTypes, split = '')))
}


##


## Common predicates ----

is.length1 <- function(x) length(x) == 1L

is.scalar <- function(x) is.atomic(x) && length(x) == 1L

is.zero <- function(x) if (is.numeric(x)) x == 0 else logical(length(x))

is.whole <- function(x) x %% 1 == 0

is.positive <- function(x, strict = FALSE) if (is.numeric(x)) (if (strict) x > 0 else x >= 0) else logical(length(x))
is.negative <- function(x, strict = TRUE) if (is.numeric(x)) (if (strict) x < 0 else x <= 0) else logical(length(x))

# Error messages ----


.showstack <- function() {
  stack <- lapply(head(sys.calls(), -1L), rlang::expr_deparse)
  stack <- sapply(stack, paste, collapse = '\n')
  
  stack <- stack[!grepl('^check|\\.stop\\(', stack)]
 
  cut <- 15
  stack[-1] <- paste0(' -> ', stack[-1])
  stack[nchar(stack) > cut] <- paste0(stack[nchar(stack) > cut], '\n\t')
  # 
  message('humdrumR error in:')
  message('\t', stack, sep = '')
   
}

.stop <- function(..., ifelse = TRUE, sep = ' ') {
  stack <- lapply(head(sys.calls(), -1), rlang::expr_deparse)
  stack <- sapply(stack, paste, collapse = '\n')
  
  stack <- stack[!grepl('^check|\\.stop\\(', stack)]
  # stack <- paste0('  ', strrep(' ', 1:length(stack) * 2), stack)
  
  cut <- 15
  stack[-1] <- paste0(' -> ', stack[-1])
  stack[nchar(stack) > cut] <- paste0(stack[nchar(stack) > cut], '\n\t')
  # 
  message('humdrumR error in:')
  message('\t', stack, sep = '')
  
  message <- .glue(..., ifelse = ifelse, sep = sep, envir = parent.frame(1), trim = FALSE)
  
  stop(call. = FALSE, message)
}


.warn <- function(...,  ifelse = TRUE, sep = ' ', immediate. = FALSE) {
  stack <- lapply(head(sys.calls(), -1), rlang::expr_deparse)
  stack <- sapply(stack, paste, collapse = '\n')
  
  call <- sys.calls()[[1]]
  
  warning('In your call ', rlang::expr_deparse(call), ': ',
          .glue(..., ifelse = ifelse, sep = sep, envir = parent.frame(1)),
          call. = FALSE,
          immediate. = immediate.)
}


