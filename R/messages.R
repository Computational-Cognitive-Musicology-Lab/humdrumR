
# Checking arguments ----

## argCheck class ----
setClass('argCheck', slots = c(Function = 'function', Rule = 'character', ProblemDescription = 'function')) 


argCheck <- function(func, rule, problem) new('argCheck', Function = func, Rule = rule, ProblemDescription = problem)
  

setMethod('|', c('argCheck', 'argCheck') ,
          \(e1, e2) {
            argCheck(\(arg) e1@Function(arg) || e2@Function(arg),
                     paste0('[', e1@Rule, ' OR ', e2@Rule, ']'),
                     \(arg) harvard(unique(c(e1@ProblemDescription(arg), e2@ProblemDescription(arg))), 'AND'))
            
          })


setMethod('&', c('argCheck', 'argCheck') ,
          \(e1, e2) {
            argCheck(\(arg) e1@Function(arg) && e2@Function(arg),
                     paste0('[', e1@Rule, ' AND ', e2@Rule, ']'),
                     \(arg) harvard(unique(c(e1@ProblemDescription(arg), e2@ProblemDescription(arg))), 'AND'))
            
          })

setMethod('+', c('argCheck', 'character'),
          \(e1, e2) {
            e1@Rule <- paste0(e1@Rule, '; ', e2)
            e1
          })

## Checking functionality ----

checks <- function(arg, argcheck) {
  
  sys <- sys.calls()
  if (length(sys) > 10L) return(arg) 
  argname <- rlang::expr_name(rlang::enexpr(arg))
  callname <- rlang::expr_name(sys[[1]][[1]])
  
  messages <- docheck(argcheck, arg)
  messages <- gsub("argname", argname, messages)
 
  
  if (length(messages) == 0L) return(arg)
  
  
  argprint <- if (length(arg) == 0L) {
    paste0(class(arg), '(0)')
  } else {
    paste0(if (length(arg) > 1L) 'c(', 
           harvard(head(arg, 6), quote = is.character(arg)),
           if (length(arg) > 6L) ', ...',
           if (length(arg) > 1L) ')')
    
  }
  
  premessage <- glue::glue("There is a problem with the call {callname}(..., {argname} = {argprint}): ")
  
  
  .stop(paste(c(premessage, messages), collapse = '\n\t'))
}




docheck <- function(argcheck, arg) {
  bad <- !argcheck@Function(arg)
  if (any(bad)) {
    glue::glue("The 'argname' argument must be ",
               argcheck@Rule,
               "; In your call, ",
               harvard(unique(argcheck@ProblemDescription(arg)), 'AND'), '.')
  }
}

dochecks <- function(arg, ...) {
  unlist(lapply(list(...), docheck, arg = arg))
}


## Common Messages ----


.show_values <- function(bad, n = 6) {
  
  uniq <- unique(bad)
  
  message <- if (length(bad) == 0L) {
    "Your 'argname' is empty: {class(bad)[1]}(0)."
  } else {
    if (length(uniq) == 1L) {
      "Your 'argname' has the value {bad}" 
    } else {
      "Your 'argname' includes the values {harvard(head(uniq, n), 'and', is.character(bad))}"
    }
  }
  
  glue::glue(message)
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
  function(arg) {
    glue::glue("{funcname}(argname) == {.show_vector(func(arg))}")
  }
}


## Common Check functions ----

### Class ----

...class <- function(classes) {
  if ('number' %in% classes) classes <- c('numeric', 'integer', 'integer64', classes)
  
  argCheck(\(arg) class(arg)[1] %in% classes,
           # glue::glue("a {harvard(classes, 'or', quote = TRUE)} object"),
           glue::glue("a {paste(quotemark(classes), collapse = '|')} object"),
           .mismatch(class))
  
}

...character <- ...class('character')
...logical <- ...class('logical')
...integer <- ...class('integer')
...numeric <- ...class('numeric')
...number  <- ...class('number')

...atomic <- function(arg) {
  if (!is.atomic(arg)) {
    glue::glue("The 'argname' argument must be an 'atomic' value; i.e., a basic R vector (either logical, numeric, integer, or character).",
               "You have provided a {class(arg)} value, which is not atomic.")
  }
  
}

### Number stuff ----

...natural <- function(arg) {
  numb <- ...number(arg)
  if (length(numb) == 0L && arg != round(arg)) glue::glue("The 'argname' argument must be",
                                                          if (length(arg) > 1) "whole numbers." else "a whole number.", .sep = ' ',
                                                          "Your 'argname' includes",
                                                          if (sum( arg != round(arg) ) > 1L) "the values" else "the value",
                                                          "{arg[arg != round(arg)]}.") else numb

}



...max <- function(n) \(arg) if (any(arg > n)) glue::glue("The 'argname' argument cannot by more than {n}. ", 
                                                          .arg_function(arg, max, 'max'))
...min <- function(n) \(arg) if (any(arg < n)) glue::glue("The 'argname' argument cannot by less than {n}. ", 
                                                          .arg_function(arg, min, 'min'))

...min1 <- ...min(1)
...max1 <- ...max(1)

...positive <- function(arg, strict = TRUE) {
  bad <- if (strict) arg <= 0 else arg < 0
  if (any(bad)) glue::glue("The 'argname' argument must be ", if (strict) "strictly (no zeros) " else "", "positive. ",
                           .arg_values(arg[bad]),
                           if (sum(bad) > 6L) ", as well as {sum(bad) - 6} other negative values." else ".")
} 

...notzero <- argCheck(\(arg) arg != 0,
                        "not zero",
                        \(arg) "'argname' includes {sum(arg == 0)} {plural(sum(arg == 0), 'zero', 'zeroes')}")

### Length -----

...maxlength <- function(n = 1) {
  argCheck(\(arg) length(arg) <= n,
           glue::glue("at most {n} long"),
           .mismatch(length))
}
...minlength <- function(n = 1) {
  argCheck(\(arg) length(arg) >= n,
           glue::glue("at least {n} long"),
           .mismatch(length))
}



...scalar <- function(arg) {
  c(...atomic(arg),
    if (length(arg) != 1L) glue::glue("The 'argname' argument must be a single scalar value (e.g., a vector of length  == 1).",
                                      .arg_function(arg, length, 'length')))
}
  


  


...match <- function(match, matchname, orscalar = TRUE) {
  
  argCheck(\(arg) length(arg) == length(match),
           "must be the same length as the 'match' argument",
           \(arg) glue::glue("length(argname) == {length(arg)}; length({matchname}) == {length(match)}"))
  # \(arg) {
  #   if (any(ldim(arg) != ldim(match)) && (orscalar && length(arg) != 1L)) {
  #     glue::glue("The 'argname' argument must {if (orscalar) 'either' else ''} be the same size as the '{matchname}' argument", .sep = ' ', 
  #                if (orscalar) 'or be length == 1.' else '.',
  #                "However, the 'argname' argument you have provided",
  #                if (hasdim(arg)) "has dimensions {paste(dim(arg), collapse = ' x ')}" else "is length {length(arg)}",
  #                "while the '{matchname}' argument",
  #                if (hasdim(match)) "has dimensions {paste(dim(match), collapse = ' x ')}" else "is length {length(match)}.")
  #     
  #   }
  # }
}



...TF <- function(arg) {
  if (!is.logical(arg) || length(arg) != 1L) glue::glue("The 'argname' argument is a On/Off switch argument: It must be a single TRUE or FALSE value. ",
                                                        if (!is.logical(arg)) .arg_function(arg, class, 'class') else .arg_function(arg, length, 'length'))
}


##

checkArg <- function(arg,  argname, callname = NULL, 
                     atomic = FALSE,
                     valid, validoptions = NULL, min.length = 1L, max.length = Inf, alt.length = NULL, classes = NULL) {
    # arg a argument to check
    # 
    if (length(sys.calls()) > 10L) return(arg) 
    
    argNames <- if (length(arg) > 1L) paste0('c(', harvard(argname, quote = TRUE), ')') else quotemark(argname)
    if (length(argNames) == 0) argNames <- paste0(class(argNames), '(0)')
    callname <- if (is.null(callname)) '' else glue::glue("In the call humdrumR::{callname}({argname} = {argNames}): ")
    
    if (atomic && !is.atomic(arg)) .stop(callname, "The {argname} argument must be an 'atomic' vector.")
    
    if (!(!is.null(alt.length) && length(arg) == alt.length)) {
      if (length(arg) <  min.length) .stop(callname, 
                                           "The length of the '{argname}' argument must be at least {min.length}.",
                                           "In your call, length({argname}) == {length(arg)}.")
      if (length(arg) > max.length) .stop(callname, 
                                          "The length of the '{argname}' argument must be at most {max.length}.",
                                          "In your call, length({argname}) == {length(arg)}.")
    }
   
    
    if (!is.null(classes) && !any(sapply(classes, inherits, x = arg))) {
        classNames <- harvard(classes, 'or', quote = TRUE)
        .stop(callname, "The '{argname}' argument must inherit the class <{classNames}>, but you have provided a <{class(arg)}> argument.")
    }
    
    if (missing(valid) && !is.null(validoptions)) valid <- \(x) x %in% validoptions
    if (!missing(valid) && !is.null(valid)) {
        ill <- !valid(arg)
        
        if (any(ill)) {
            if (is.null(validoptions)) {
                .stop(callname, "{arg} is not a valid value for the {argname} argument.")
            } else {
                case <- glue::glue(plural(sum(ill), " are not valid {argname} values. ", "is not a valid value for the '{argname}' argument. "))
                illNames <- harvard(arg[ill], 'and', quote = TRUE)
                legalNames <-  paste0(harvard(validoptions, quote = TRUE), '.')
                
                message <- list(callname, illNames, case, 'Valid options are ', legalNames)
                
                .stop(message)
            }
            
        }
        
        arg[!ill]
    } else {
        arg
    }
    
    
    
}

checkVector <- function(x, argname, callname = NULL, structs = NULL, null = TRUE, matrix = FALSE, min.length = 0L, ...) {
  if (matrix && is.matrix(x)) x <- c(x)
  
  checkArg(x, argname = argname, callname = callname, ..., 
           valid = NULL,
           classes = c('numeric', 'integer', 'character', 'logical', structs, 
                       if (null) 'NULL'),
           min.length = min.length)
}


checkNumeric <- function(x, argname, callname = NULL, minval = -Inf, maxval = Inf, ...) {
    checkArg(x, argname = argname, callname = callname,
             classes = c('numeric', 'integer'), 
             valid = \(arg) is.na(arg) | arg >= minval & arg <= maxval,
             ...)
}
checkLooseInteger <- function(x, argname, callname = NULL, minval = -Inf, maxval = Inf, ...) {
  checkArg(x, argname = argname, callname = callname,
           classes = c('numeric', 'integer'), 
           valid = \(arg) arg >= minval & arg <= maxval & arg == round(arg),
           ...)
}

checkInteger <- function(x, argname, callname = NULL, minval = -Inf, maxval = Inf, ...) {
    checkArg(x, argname = argname, callname = callname,
             classes = c('integer'), 
             valid = \(arg) arg >= minval & arg <= maxval & !is.double(x),
             ...)
}
checkCharacter <- function(x, argname, callname = NULL, allowEmpty = TRUE, ...) {
    checkArg(x, argname = argname, callname = callname, classes = c('character'),
             valid = if (!allowEmpty) \(arg) arg != "", ...)
}
checkLogical <- function(x, argname, callname = NULL, ...) {
    checkArg(x, argname = argname, callname = callname, classes = c('logical'), ...)
}

checkTF <- function(x, argname, callname) checkArg(x, valid = \(arg) !is.na(arg), 
                                                   validoptions = c(TRUE, FALSE), argname, callname, max.length = 1L, classes = 'logical')
checkTFs <- function(args = list(), ..., callname = NULL) {
    args <- c(args, list(...))
    mapply(checkTF, args, names(args), MoreArgs = list(callname = callname))
}

checkhumdrumR <- function(x, callname, argname = 'humdrumR') {
    if (!is.humdrumR((x))) .stop("In the call {callname}({argname} = _), the argument {argname} must be a humdrumR object.")      
}

checkFunction <- function(x, argname, callname) {
  label <- rlang::as_label(rlang::enexpr(x))
  
  message <- "In the call {callname}({argname} = {label}), the argument {argname}"
  
  if (!is.function(x)) .stop(message, " must be a function!")
  
}

checkRoundingFunction <- function(x, argname, callname) {
  label <- rlang::as_label(rlang::enexpr(x))
  
  if (!any(sapply(list(round, floor, ceiling, trunc, expand), identical,  y = x))) {
    .stop(message, 
          " must be one of the five 'rounding' functions: round, floor, celing, trunc, or expand.")
  }
}

checkTypes <- function(dataTypes, callname, argname = 'dataTypes') {
    dataTypes <- unique(unlist(strsplit(dataTypes, split = '')))
    checkArg(dataTypes,
             valid = \(arg) arg %in% c('G', 'L', 'I', 'M', 'D', 'd'),
              validoptions = c('G', 'L', 'I', 'M', 'D', 'd'),
              argname, callname,
              min.length = 1L, max.length = 7L,
              classes = "character")
}


# Error messages ----

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
  
  message <- .glue(..., ifelse = ifelse, sep = sep, envir = parent.frame(1))
  
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

