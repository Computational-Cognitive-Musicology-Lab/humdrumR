####### Regex Parsing

#' @name REparser
#' @export
REparser <- function(res, parse.strict = TRUE, parse.exhaust = TRUE, parse.lead = FALSE, parse.rest = FALSE, toEnv = FALSE) {
    # makes a parser which strictly, exhaustively parses a string
    # into a sequence of regexes
    
    rlang::new_function(args = alist(str = ),
                        body = rlang::expr(REparse(str, !!!res, !!parse.strict, !!parse.exhaust, !!parse.lead, !!parse.rest, !!toEnv)))
}


#' Parse String Using Regular expressions
#' 
#' Takes an input string and parses it into a sequence of regular expressions.
#' 
#' If `exhaustive` is TRUE, the string must be exhaustively broken up by the matching regular expressions.
#' Superfluous (non-match) characters at the begginning, end, or in bettween matches, will result in 
#' all `NA` being returned.
#' 
#' @name REparser
#' @export
REparse <- function(str, res, parse.strict = TRUE, parse.exhaust = TRUE, parse.lead = FALSE, parse.rest = FALSE, reverse = FALSE, toEnv = FALSE) {
    res <- res[lengths(res) > 0]
    
    if (any(.names(res) == "")) .stop("In call to REparse, all arguments must be named.")
    
    ##
    matches <- list()
    complete <- !logical(length(str))
    lead <- NULL
    rest <- str
    
    if (reverse) res <- rev(res)
    
    for (re in names(res)) {
        locs <- stringr::str_locate(rest, res[[re]])
        
        
        
        hits <- !is.na(locs[ , 1]) 
        
        if (parse.exhaust) {
            hits <- if (!reverse) {
                hits & (locs[ , 1] == 1)
            } else {
                hits & (locs[ , 2] == nchar(rest))
            }
        }
        
        complete <- complete & hits
        # 
        if (!parse.exhaust && parse.lead && is.null(lead)) {
            # should only ever happen in first iteration
            lead <- if (!reverse) {
                stringr::str_sub(rest, 0, pmax(locs[ , 'start'] - 1L, 0L))
            } else {
                stringr::str_sub(rest, locs[ , 'end'] + 1L)
            }
        }
        
        
        matches[[re]] <- .ifelse(hits, stringr::str_sub(rest, locs[ , 'start'], locs[ , 'end']), NA_character_)
        
        rest[hits] <- if (!reverse) {
            stringr::str_sub(rest[hits], start = locs[hits, 'end'] + 1L) 
        } else {
            stringr::str_sub(rest[hits], end = locs[hits, 'start'] - 1L)
        }
    }
    
    if (parse.lead) matches <- c(list(Lead = lead), matches)
    if (parse.rest) matches <- c(matches, list(Rest = rest))
    if (parse.strict) matches <- lapply(matches, `[<-`, i = !complete, value = NA_character_)
    
    if (reverse) matches <- rev(matches)
    
    if (toEnv) list2env(matches, parent.frame())
    
    output <- do.call('cbind', matches)
    # if (length(str) > 0L) rownames(output) <- str
    
    if (toEnv) invisible(output) else output
    
}

#' @export
popRE <- function(str, regex) {
    var <- rlang::enexpr(str)
    
    loc <- stringi::stri_locate_first_regex(str, regex)
    hits <- !is.na(loc[ , 1])
    # match if any
    match <- character(length(str))
    match[hits] <- stringi::stri_sub(c(str)[hits], loc[hits, 'start'], loc[hits , 'end'])
    match <- match %dim% str
    
    # rest if any
    str[hits] <- stringi::stri_sub(str[hits], loc[hits , 'end'] + 1L)
    str <- str %dim% match
    
    if (length(var) == 1L && !is.atomic(var)) eval(rlang::expr(!!var <- !!str), envir = parent.frame())
    
    match
    
}



####### Regex dispatch ----

#' Regular expression method dispatch and function application
#' 
#' The \code{\link{humdrumR}} \strong{regular-expression method dispatch}
#' system is a simple system for making new functions which can by smartly
#' applied to complex character strings.
#' 
#' The function \code{do2RE} accepts and arbitrary function
#' and a \href{https://en.wikipedia.org/wiki/Regular_expression}{regular expression} (regex)
#' and makes a new function that applies the original function only to
#' any part of a string which matches the regex.
#' 
#' The function \code{regexDispatch} accepts a list
#' of functions, each with a matching regular expression,
#' and creates a new function which applies whichever function
#' based on which regexs it finds in its input.
#' @name regexDispatch
NULL







#' @name regexDispatch
#' @export
regexDispatch <- function(...) {
          funcs <- Filter(is.function, list(...))
          if (length(funcs) == 0L) stop("Can't regexDispatch on zero functions.")
          
          regexes <- getRE(names(funcs))
          funcsArgs <- lapply(funcs, function(rf) formals(args(rf))[-1])
          
          genericFunc <- function() {
              if (!is.character(str)) stop(call. = FALSE,
                                           "The regex-dispatch function you've called requires a character argument.")
              
              dispatch <- regexFindMethod(str, regexes)  
              if (dispatch == 0L) return(if (inPlace) str else vectorNA(length(str), 'character'))
              dispatchFunc <- funcs[[dispatch]]
              dispatchRE   <- regexes[[dispatch]]
              dispatchArgs <- funcsArgs[[dispatch]]
              # ... args
              elips <- names(dispatchArgs) == '...'
              not_elips <- names(dispatchArgs)[!elips]
              #
              dispatchArgs <- setNames(lapply(not_elips, get, envir = environment()), not_elips)
              dispatchArgs <- c(x = list(str), regex = dispatchRE, .func = dispatchFunc, inPlace = inPlace,
                                dispatchArgs, if (any(elips)) list(...))
              result <- do.call('.REapply', dispatchArgs)
              
              result %re.as% names(dispatch)
          }
          
          # Assemble the new function's arguments
          genericArgs <- do.call('c', c(funcsArgs, use.names = FALSE))
          genericArgs <- genericArgs[!duplicated(names(genericArgs))]
          formals(genericFunc) <- c(alist(str = ), genericArgs, alist(inPlace = FALSE))
          
          genericFunc
}

# 
regexFindMethod <- function(str, regexes) {
    # this takes a str of character values and a string of REs
    # and returns a single interger value representing the 
    # index (of regexes) to dispatch.
    
    regexes <- getRE(regexes)
    
    Nmatches <- sapply(regexes, function(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
    if (!any(Nmatches > 0L)) return(0L)
    
    #which function to dispatch
    Ncharmatches <- sapply(regexes[Nmatches > 0],
                           function(re) {
                               nchars <- nchar(stringi::stri_extract_first_regex(str, re))
                               nchars[is.na(nchars)] <- 0L
                               sum(nchars)
                           })
    which(Nmatches > 0L)[which.max(Ncharmatches)]
}


#' @name regexDispatch
#' @export
REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    if (!is.character(x)) stop(call. = FALSE,
                               "Sorry, REapply can only apply to an x argument that is a character vector.")
    result <- .REapply(x, getRE(regex), .func, inPlace = inPlace, ...)
    
    as.re(result, regex) %dim% x
}

#' 
.REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    # accepts a regex (whereas REapply can take a unparsed regex name
    # like "Recip").
    matches <- stringi::stri_extract_first(str = x, regex = regex)
    result <- local({
        hits <- !is.na(matches)
        hits_result <- do.call(.func, c(list(matches[hits]), list(...)))
        
        result <- vectorNA(length(x), class(hits_result))
        result[hits] <- hits_result
        result
    })
    
    associatedExclusive <- getREexclusive(regex)
    if (!is.null(associatedExclusive)) stickyAttrs(result) <- list(Exclusive = associatedExclusive)
    
    if (inPlace) inPlace(result, x, regex) else result

}







normalizeBody <- function(fname, func = NULL, removeElips = TRUE) {
    # this takes the name of a function (as a string)
    # and creates a usable function body
    # including jumping through hoops to for
    # primitives and generics
    
    if (is.null(func)) func <- match.fun(fname)
    fname <- rlang::sym(fname)
    ftext <- rlang::quo_text(func)
    
    
    # args
    argnames <- names(fargs(func))
    if (argnames[[1]] == '...' && removeElips) argnames[[1]] <- 'tmp'
    argnames <- rlang::syms(argnames)
    
    namedfuncexpr <- rlang::expr((!!fname)(!!!argnames))
    
    if (is.primitive(func) | 
        grepl('\\.Internal|\\.Primitive', ftext) |
        grepl('UseMethod|standardGeneric', ftext)) {
        namedfuncexpr
        
    } else {
        
        body <- rlang::fn_body(func)
        if (sum(stringi::stri_count_fixed(as.character(body), '\n')) > 1 ||
            !grepl('function\\(', as.character(body))) {
            namedfuncexpr
        } else {
            body
        }
    }
}







###################  User Regex tools ----


#' Match strings against regular expression
#' 
#' These infix functions are simply syntactic sugar for
#' existing `R` regular expression matching functions.
#' If the a vector of regexes is given as the right argument, matches to *any* of the regexes are returned.
#' 
#' + `\%~l\%`: Matches `pattern` in `x` and returns `logical`. Shorthand for [base::grepl].
#' + `\%~\%`: The "default"---same as `\%~l\%`.
#' + `\%~i\%`: Matches `pattern` in `x` and returns `integer` indices. Shorthand for [base::grep].
#' + `\%~n\%`: Matches `pattern` in `x` and returns `integer` counts (can be greater than one if more 
#'   than one match occurs in the same token). Shorthand for [stringi::stri_count_regex].
#' @export
#' @name RegexFind
`%~l%` <- function(x, pattern) Reduce('|', lapply(pattern, grepl, x = x))
#' @export
#' @name RegexFind
`%~i%` <- function(x, pattern) which(x %~l% pattern)
#' @export
#' @name RegexFind
`%~n%` <- function(x, pattern) Reduce('+', lapply(pattern, stringi::stri_count_regex, str = x))
#' @name RegexFind
#' @export
`%~%` <- `%~l%`


# regexParse <- function(str, ..., toEnv = TRUE) {
#     regexes <- list(...)
#     matches <- setNames(vector('list', length(regexes)), .names(regexes))
# 
#     for (i in seq_along(regexes)) { # loop because regexPop change str in place
#         matches[[i]] <- regexPop(str, regexes[[i]])
#     }
# 
#     if (toEnv) list2env(matches[.names(matches) != ""], envir = parent.frame())
# 
#     return(invisible(do.call('cbind', matches)))
# 
# 
# }


#################### Making Regexes ----



#' Making Regular Expressions
#' 
#' `humdrumR` includes some helpful functions for creating new regular expressions which work with the
#' [stringr] package.
#' 
#' `captureRE` will take a character vector and collapse it to a "capture group."
#' The `n` argument can be used to append a number tag, for instance `'*'` (zero or more) to the group.
#' I.e., `captureRE(c("a", "b", "c"), '*')` will output `"[abc]*"`.
#' 
#' `captureUniq` will make a similar capture group to `captureRE`, but with an expression
#' that makes sure that only 1 or more *of the same character* repeats.
#' For instance, `captureUniq(c('a', 'b','c'))` will return `"([abc])\\1*"`---this expression will match
#' `"aaa"` or `"bb"` but not `"aabb"`.
#' 
#' @name regexConstruction
#' @export
captureRE <- function(strs, n = '') {
    strs <- strs[order(nchar(strs), decreasing = TRUE)]
    
    if (length(strs) > 1) {
        multi <- nchar(strs) > 1L & !grepl('-', strs)
        strs <- paste(c(if (any(multi)) paste0('(', paste(collapse = '|', strs[multi]), ')'),
                        if (any(!multi)) paste0('[', paste(collapse = '', strs[!multi]), ']')),
                      collapse = '|')
        
        if (any(multi)) strs <- paste0('(', strs, ')')
    }
    
    escaper(paste0(strs, n))
}

#' @name regexConstruction
#' @export
captureUniq <- function(strs, zero = TRUE) paste0('(', captureRE(strs), if (zero) "?", ')\\1*') 
# takes a RE capture group and makes it so it will only match one or more of the same character

escaper <- function(str) {
    stringr::str_replace_all(str, '\\[\\^', '[\\\\^')
    
}




cREs <- function(REs, parse.exhaust = TRUE) {
    if (length(REs) == 0) return('')
    REs <- unlist(paste0('(?:', REs, ')'))
    
    ## if the regexes use capture groups, we must increment groups in later expressions
    # for instance, if the first regex has (x)\\1 and the second has (.*)\\1,
    # we need to change the latter to (.*)\\2
    hasCapture <- stringr::str_detect(REs, '\\\\[1-9]')
    if (any(hasCapture)) {
        captures <-  stringr::str_extract_all(REs[hasCapture], '\\\\[1-9]')
        noCaptures <- stringr::str_split(REs[hasCapture], '\\\\[1-9]')
        captures <- lapply(captures, function(cap) as.integer(factor(cap)))
        
        shifts <- head(Reduce(function(a, b) a + max(b), captures, init = 0, accumulate = TRUE), length(captures))
        captures <- Map('+', captures, shifts)
        
        REs[hasCapture] <- Map(function(nocap, cap) {paste(c(rbind(nocap, c(paste0('\\', cap),''))), collapse = '')}, noCaptures, captures)
        
    }
    
    paste(REs, collapse = if (parse.exhaust) '' else '.*') 
    
}

####. REs for tonalIntervals ####

makeRE.steps <- function(step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), ..., step.sign = TRUE)  paste0(if (step.sign) '[-+]?', captureRE(step.labels))

makeRE.accidentals <- function(accidental.labels = c(), ...) {
    setoptions(accidental.labels) <- c(sharp = '#', flat = 'b', natural = 'n')
    
    paste0(accidental.labels['natural'], '|', captureUniq(accidental.labels[names(accidental.labels) != 'natural']))
}

makeRE.qualities <- function(quality.labels = c(), ...) {
    setoptions(quality.labels) <-  c(major = 'M', minor = 'm', perfect = 'P', augment = 'A', diminish = 'd', natural = 'n')
    paste0(captureRE(quality.labels[c('perfect', 'major', 'minor')], ''), '|', captureUniq(quality.labels[c('diminish', 'augment')]))
}

makeRE.contours <- function(contour.labels = c(), ...) {
    setoptions(contour.labels) <- c(up = '^', down = 'v', same = '')
    if (false(contour.labels)) '-?[0-9]+' else captureUniq(contour.labels)
}

makeRE.tonalChroma <- function(parts = c('steps', 'accidentals', 'contours'), collapse = TRUE, ...){
    REs <-  list(steps       = if ('steps' %in% parts)       makeRE.steps(...),
                 accidentals = if ('accidentals' %in% parts) makeRE.accidentals(...),
                 qualities   = if ('qualities' %in% parts)   makeRE.qualities(...),
                 contours    = if ('contours' %in% parts)    makeRE.contours(...)
                 )[parts]
    
    if (collapse) setNames(cREs(REs), 'tonalChroma') else REs
    
}

makeRE.kern <- function(parts = c('steps', 'accidentals'), collapse = TRUE, step.labels = c(), accidental.labels = c(), ...) {
    setoptions(step.labels) <- letters[1:7]
    setoptions(accidental.labels) <- c(sharp = '#', flat = '-', natural = 'n')
    
    REs <- makeRE.tonalChroma(parts[parts != 'steps'], collapse = FALSE, accidental.labels = accidental.labels, ...)
    
    if ('steps' %in% parts) {
        REs$steps <- captureUniq(c(tolower(step.labels), toupper(step.labels)), zero = FALSE)
        REs <- REs[parts]
    }
    
    if (collapse) setNames(cREs(REs), 'kern') else REs
}

makeRE.sciPitch <- function(parts = c('steps', 'accidentals', 'contours'), collapse = TRUE, contour.offset = 4L, contour.labels = FALSE, ...) {
   setNames(makeRE.tonalChroma(parts, collapse  = collapse, contour.offset = contour.offset, contour.labels = contour.labels, ...), 'pitch')
}

makeRE.interval <- function(parts = c('qualities', 'steps'), collapse = TRUE, ...) {
    setNames(makeRE.tonalChroma(parts, collapse  = collapse, step.labels = 1:19, ...), 'interval')
}

makeRE.scaleDegree <- function(parts = c('qualities', 'steps'), collapse = TRUE, ...) {
    setNames(makeRE.tonalChroma(parts, collapse  = collapse, step.labels = 1:7, ...), 'scaleDegree')
}

makeRE.solfa <- function(parts = c('steps', 'accidentals'), ..., collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts[parts != 'steps'], ..., collapse = FALSE)
    
    if ('steps' %in% parts) {
        REs$steps <- "[sd][eoi]|[fl][eai]|[mt][eiy]|r[aei]"
        REs <- REs[parts]
    }
    
    if (collapse) setNames(cREs(REs), 'solfa') else REs
}


####

makeRE.decimal <- function() c(decimal = "[+-]?[0-9]+(\\.[0-9]+)?" )
makeRE.fraction <- function(sep = '/', ...) paste0("[1-9][0-9]*", sep, "[1-9][0-9]*")


####. REs for diatonic sets ####

makeRE.alterations <- function(alteration.labels, ...) {
    # names(alteration.labels) <- gsub('augment', 'sharp', names(alteration.labels))
    # names(alteration.labels) <- gsub('diminish', 'flat', names(alteration.labels))

    
    paste0('(', 
           makeRE.tonalChroma(c('accidentals', 'steps'), ...,
                              step.sign = FALSE,
                              step.labels = c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13)),
           ')*')
}

makeRE.key <- function(..., accidental.labels = c(), alteration.labels = c(), collapse = TRUE) {
    setoptions(accidental.labels) <- c(flat = '-')
    
    REs <- makeRE.tonalChroma(parts = c('steps', 'accidentals'),
                              step.labels = '[A-Ga-g]',
                              accidental.labels = accidental.labels,
                              step.sign = FALSE, collapse = FALSE)
    
    REs['colon'] <- ':'
    REs['star'] <- '\\*?'
    
    REs['mode'] <- captureRE(c('dor', 'phy', 'mix', 'loc', 'lyd'), n = '?')
    REs['alterations'] <- makeRE.alterations(alteration.labels)
    
    REs <- REs[c('star', 'steps', 'accidentals', 'colon', 'mode', 'alterations')]
    
    if (collapse) setNames(cREs(REs), 'key') else REs
}

makeRE.romanKey <- function(..., accidental.labels = c(), alteration.labels = c(), collapse = TRUE) {
    setoptions(alteration.labels) <- c(augment = '#', diminish = 'b')
    setoptions(accidental.labels) <- c(sharp   = '#', flat     = 'b')
    
    REs <- makeRE.tonalChroma(parts = c('steps', 'accidentals'),
                              step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                              accidental.labels = accidental.labels,
                              ...,
                              step.sign = FALSE, collapse = FALSE)
    
    res <- makeRE.tonalChroma(parts = c('steps', 'accidentals'),
                              step.labels = c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii'),
                              accidental.labels = accidental.labels,
                              ...,
                              step.sign = FALSE, collapse = FALSE)
    
    
    REs['mode'] <- captureRE(c('mix', 'lyd', 'ion'), n = '?')
    res['mode'] <- captureRE(c('phy', 'aeo', 'loc', 'dor'), n = '?')
    
    REs$steps <- paste0('(', cREs(REs[c('steps', 'mode')]), '|', cREs(res[c('steps', 'mode')]), ')')
    REs$mode <- NULL
    
    REs['alterations'] <- makeRE.alterations(alteration.labels)
    REs <- REs[c('accidentals', 'steps', 'alterations')]
    
    
    if (collapse) setNames(cREs(REs), 'key') else REs
}



makeRE.signature <- function(accidental.labels = c(), ...) {
    setoptions(accidental.labels) <- c(flat = '-')
    
    RE <- cREs(list(steps = '[A-Ga-g]',  accidentals = makeRE.accidentals(accidental.labels = accidental.labels)))
    paste0('^\\*k\\[(', RE, ')*\\]')
}

makeRE.diatonicPartition <- function(..., split = '/', mustPartition = TRUE) {
    
    key <- makeRE.key(...)
    romanNumeral <- makeRE.romanKey(...)
    
    re <- paste0('(', key, '|', romanNumeral, ')')
    
    paste0('((', re, ')', split, ')', if (mustPartition) '+' else '?', re)
}


####. REs for tertian sets ####

makeRE.sciChord <- function(..., quality.labels = c(), collapse = TRUE) {
    setoptions(quality.labels) <- c(major = 'M', minor = 'm', augment = 'A', diminish = 'd', perfect = 'P')
    
    REs <- makeRE.tonalChroma(parts = c('steps', 'accidentals'),
                              step.labels = '[A-G]',
                              step.sign = FALSE, collapse = FALSE, ...)
    
    qualityRE <- captureRE(quality.labels[c('major', 'minor', 'augment', 'diminish')])
    REs['qualities'] <-  paste0('(', 
                                qualityRE, '{3}',  
                                captureRE(quality.labels[c('perfect', 'augment', 'diminish')]), 
                                qualityRE, 
                                '?)|(', 
                                qualityRE, '{1,3})')
   
    REs <- REs[c('steps', 'accidentals', 'qualities')]
    
    if (collapse) setNames(cREs(REs), 'sciChord') else REs
}

makeRE.romanChord <- function(..., triad.labels = c(), collapse = TRUE) {
    setoptions(triad.labels)      <- c(diminish = 'o', augment  = '+')
    triad.labels$augment <- paste0('[', triad.labels$augment, ']') # because "+" is a special character!
    
    REs <- list()
    REs$accidentals <- makeRE.accidentals(...)
    
    upper <- paste0('(?=[IV]+', triad.labels$augment,  '?)', captureRE(c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')))
    lower <- paste0('(?=[iv]+', triad.labels$diminish, '?)', captureRE(c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii')))
    REs$numerals <- paste0('(', upper, '|', lower, ')')
    
    
    
    # REs['triadalts'] <- paste0('(', 
    #                           '(?<=[IV])', triad.labels['augment'],
    #                           '|',
    #                           '(?<=[iv])', triad.labels['diminish'],
    #                           ')')
    REs$triadalts <- captureRE(triad.labels, n = '?')

    
    REs['figurations'] <- makeRE.alterations(...)
    REs <- REs[c('accidentals', 'numerals', 'triadalts', 'figurations')]
    
    
    if (collapse) setNames(cREs(REs), 'chord') else REs
}



makeRE.tertianPartition <- function(..., split = '/') {
    
    romanChord <- makeRE.romanChord(...)
    
    re <- paste0('(', romanChord, ')')
    
    key <- makeRE.diatonicPartition(..., split = split, mustPartition = FALSE)
    
    paste0('((', re, ')', split, ')+', key)
}



####. REs for durations ####

makeRE.recip <- function() getRE('recip')
