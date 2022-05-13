# Regex Parsing ----

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
#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' @name REparser
#' @export
REparse <- function(str, res, parse.strict = TRUE, parse.exhaust = TRUE,
                    parse.lead = FALSE, parse.rest = FALSE, reverse = FALSE, 
                    sep = NULL,
                    toEnv = FALSE) {
    res <- res[lengths(res) > 0]
    
    if (any(.names(res) == "")) .stop("In call to REparse, all arguments must be named.")
    
    ##
    matches <- list()
    complete <- !logical(length(str))
    lead <- NULL
    rest <- str
    
    if (reverse) res <- rev(res)
    
    for (re in names(res)) {
        if (!is.null(sep)) rest <- stringr::str_remove(rest, pattern = paste0('^', sep))
        
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
    match <- match %<-matchdim% str
    
    # rest if any
    str[hits] <- stringi::stri_sub(str[hits], loc[hits , 'end'] + 1L)
    str <- str %<-matchdim% match
    
    if (length(var) == 1L && !is.atomic(var)) eval(rlang::expr(!!var <- !!str), envir = parent.frame())
    
    match
    
}



# Regex dispatch ----








regexFindMethod <- function(str, regexes) {
    # this takes a str of character values and a string of REs
    # and returns a single interger value representing the 
    # index (of regexes) to dispatch.
    
    regexes <- getRE(regexes)
    
    Nmatches <- sapply(regexes, \(regex) sum(stringi::stri_detect_regex(str, regex), na.rm = TRUE))
    if (!any(Nmatches > 0L)) return(0L)
    
    #which function to dispatch
    Ncharmatches <- sapply(regexes[Nmatches > 0],
                           \(re) {
                               nchars <- nchar(stringi::stri_extract_first_regex(str, re))
                               nchars[is.na(nchars)] <- 0L
                               sum(nchars)
                           })
    which(Nmatches > 0L)[which.max(Ncharmatches)]
}


REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    if (!is.character(x)) stop(call. = FALSE,
                               "Sorry, REapply can only apply to an x argument that is a character vector.")
    result <- .REapply(x, getRE(regex), .func, inPlace = inPlace, ...)
    
    if (inherits(result, 'partition')) {
        result <- lapply(result,
                         \(res) { res %<-matchdim% x})
    }
    as.re(result, regex) 
}

#' 
.REapply <- function(x, regex, .func, inPlace = TRUE, ...) {
    # accepts a regex (whereas REapply can take a unparsed regex name
    # like "Recip").
    matches <- stringi::stri_extract_first(str = x, regex = regex)
    result <- local({
        hits <- !is.na(matches)
        hits_result <- do.call(.func, c(list(matches[hits]), list(...)))
        
        if (inherits(hits_result, 'partition')) {
            result <- hits_result
            result <- lapply(hits_result,
                             \(hresult) {
                                 result <- vectorNA(length(x), class(hresult))
                                 result[hits] <- hresult
                                 result 
                             })
        } else {
            result <- vectorNA(length(x), class(hits_result))
            result[hits] <- hits_result
        }
        result
    })
    
    associatedExclusive <- getREexclusive(regex)
    if (!is.null(associatedExclusive)) stickyAttrs(result) <- list(Exclusive = associatedExclusive)
    
    if (inPlace) inPlace(result, x, regex) else result

}












## %% Regex tools ----


#' Match strings against regular expression
#' 
#' These infix functions are simply syntactic sugar for
#' existing `R` regular expression matching functions.
#' If the a vector of regexes is given as the right argument, matches to *any* of the regexes are returned.
#' 
#' + `%grepl%`: Matches `pattern` in `x` and returns `logical`. Shorthand for [base::grepl()].
#' + `%grep%`: The "default"---same as `%grepl%`.
#' + `%grepi%`: Matches `pattern` in `x` and returns `integer` indices. Shorthand for [base::grep()].
#' + `%grepn%`: Matches `pattern` in `x` and returns `integer` counts (can be greater than one if more 
#'   than one match occurs in the same token). Shorthand for [stringi::stri_count_regex()].
#' + `%grepm%`: Matches `pattern` in `x` and returns matching strings (or NA if no match). Shorthand for [stringi::stri_extract_first_regex()]
#'
#' Each regex infix has an "lapply" version, called `%lgrepx%`.
#' 
#' @param x A vector to search in
#' @param list A list of vectors (all the same length) to search in
#' @param pattern One or more regular expression
#'
#' @export
#' @name RegexFind
`%grepl%` <- function(x, pattern) Reduce('|', lapply(pattern, grepl, x = x))
#' @export
#' @rdname RegexFind
`%grepi%` <- function(x, pattern) which(x %grepl% pattern)
#' @export
#' @rdname RegexFind
`%grepn%` <- function(x, pattern) Reduce('+', lapply(pattern, stringi::stri_count_regex, str = x))
#' @export
#' @rdname RegexFind
`%grepm%` <- function(x, pattern) Reduce('paste0', lapply(pattern, stringi::stri_extract_first_regex, str = x))
#' @rdname RegexFind
#' @export
`%grep%` <- `%grepl%`
#' @export
#' @rdname RegexFind
`%lgrepl%` <- function(list, pattern) Reduce(`|`, lapply(list, `%grepl%`, pattern = pattern)) 
#' @export
#' @rdname RegexFind
`%lgrepi%` <- function(list, pattern) which(list %lgrepl% pattern)
#' @export
#' @rdname RegexFind
`%lgrepn%` <- function(list, pattern) Reduce('+', lapply(list, `%grepn%`, pattern = pattern))



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
        strs <- orRE(orRE(strs[multi], 
                     if (any(!multi)) paste0('[', paste(collapse = '', strs[!multi]), ']')))
        
        # if (any(multi)) strs <- paste0('(?:', strs, ')')
    }
    
    escaper(paste0(strs, n))
}

#' @name regexConstruction
#' @export
captureUniq <- function(strs, zero = TRUE) {
# takes a RE capture group and makes it so it will only match one or more of the SAME character
    
    strs <- paste0(strs, '+')
    
    strs <- paste(strs, collapse = '|')
    
    paste0('(', strs, ')', if (zero) '?')
    
}

#' @name regexConstruction
#' @export
orRE <- function(...) {
    res <- unlist(list(...))
    res <- res[lengths(res) > 0L]
    if (length(res) == 0L) return(NULL)
    if (length(res) == 1L) return(res[[1]])
    
    paste0('(', do.call('paste', c(as.list(res), sep = '|')), ')')
    
}

# captureUniq <- function(strs, zero = TRUE) paste0('(', captureRE(strs), if (zero) "?", ')\\1*') 

escaper <- function(str) {
    stringr::str_replace_all(str, '\\[\\^', '[\\\\^')
    
}




cREs <- function(REs, parse.exhaust = TRUE, sep = NULL) {
    if (length(REs) == 0) return('')
    # REs <- unlist(paste0('(?:', REs, ')'))
    REs <- unlist(REs)
    
    ## if the regexes use capture groups, we must increment groups in later expressions
    # for instance, if the first regex has (x)\\1 and the second has (.*)\\1,
    # we need to change the latter to (.*)\\2
    hasCapture <- stringr::str_detect(REs, '\\\\[1-9]')
    if (any(hasCapture)) {
        captures <-  stringr::str_extract_all(REs[hasCapture], '\\\\[1-9]')
        noCaptures <- stringr::str_split(REs[hasCapture], '\\\\[1-9]')
        captures <- lapply(captures, \(cap) as.integer(factor(cap)))
        
        shifts <- head(Reduce(function(a, b) a + max(b), captures, init = 0, accumulate = TRUE), length(captures))
        captures <- Map('+', captures, shifts)
        
        REs[hasCapture] <- Map(function(nocap, cap) {paste(c(rbind(nocap, c(paste0('\\', cap),''))), collapse = '')}, noCaptures, captures)
        
    }
    
    paste(REs, collapse = if (is.null(sep)) {if (parse.exhaust) '' else '.*'} else {sep}) 
    
}

####. REs for tonalIntervals ####

makeRE.steps <- function(step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), steps.sign = FALSE, ...)  {
    if (is.null(step.labels)) return('[1-9][0-9]*')
    
    if (steps.sign) step.labels <- c(tolower(step.labels), toupper(step.labels))
    
    
        # captureUniq(step.labels, zero = FALSE)

    captureRE(step.labels) 

    

}

makeRE.accidentals <- function(sharp = '#', flat = '-', natural = 'n', ...) {
    
    paste0(natural,'?', captureUniq(c(sharp, flat)))
}

makeRE.qualities <- function(major = 'M', minor = 'm', perfect = 'P', augment = 'A', diminish = 'd', ...) {
    paste0(captureRE(c(perfect, major, minor), ''), '|', captureUniq(c(diminish, augment)))
}

makeRE.contours <- function(octave.integer = TRUE, up = '\\^', down = 'v', ...) {
    if (octave.integer) '-?[0-9]+' else captureUniq(c(up, down))
}

makeRE.tonalChroma <- function(parts = c("step", "species", "octave"), qualities = FALSE, collapse = TRUE, sep = NULL, ..., regexname = 'tonalChroma'){
    REs <-  list(sign    = if ('sign' %in% parts)       '[-+]?',
                 step    = if ("step" %in% parts)       makeRE.steps(...),
                 species = if ("species" %in% parts) {if (qualities) makeRE.qualities(...) else makeRE.accidentals(...)},
                 octave  = if ("octave" %in% parts)    makeRE.contours(...)
                 )[parts]
    
    if (collapse) setNames(cREs(REs, sep = sep), regexname) else REs
    
}

makeRE.kern <- function(parts = c("step", "species"), qualities = FALSE, ...) {
    
    step.labels <- unlist(lapply(1:50, strrep, x = c('C', 'D', 'E', 'F', 'G', 'A', 'B')))
    makeRE.tonalChroma(parts, step.labels = step.labels, steps.sign = TRUE, 
                       qualities = qualities,
                       octave.integer = FALSE, ..., regexname = 'kern')
    
}

makeRE.sciPitch <- function(parts = c("step", "species", "octave"), qualities = FALSE, 
                            collapse = TRUE, octave.offset = 4L, octave.integer = TRUE,
                            flat = 'b', ...) {
   makeRE.tonalChroma(parts, collapse  = collapse, 
                      octave.offset = octave.offset, octave.integer = octave.integer, 
                      qualities = qualities, flat = flat, ..., regexname = 'pitch')
}

makeRE.interval <- function(parts = c("species", "step"), collapse = TRUE, qualities = TRUE, ...) {
    makeRE.tonalChroma(parts, collapse  = collapse, qualities =qualities, step.labels = 1:99,
                       flat = 'b', ..., regexname = 'interval')
}

makeRE.scaleDegree <- function(parts = c("octave", "species", "step"), qualities = FALSE, collapse = TRUE, ...) {
    makeRE.tonalChroma(parts, collapse  = collapse, qualities = qualities, step.labels = 1:7, octave.integer = FALSE, 
                       flat = 'b', ..., regexname = 'scaleDegree')
}

makeRE.solfa <- function(parts = c("octave", "step", "species"), ..., collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts[parts != "step"], octave.integer = FALSE, ..., collapse = FALSE)
    
    if ("step" %in% parts) {
        REs$step <- "[sd][eoi]|[fl][eai]|[mt][eiy]|r[aei]"
        REs <- REs[parts]
    }
    
    if (collapse) setNames(cREs(REs), 'solfa') else REs
}


#### REs for numbers ----


makeRE.double <- function(...) c(double = "[+-]?[0-9]+(\\.[0-9]+)?" )
makeRE.fraction <- function(sep = '/', ...) paste0("[1-9][0-9]*", sep, "[1-9][0-9]*")


####. REs for diatonic sets ####

makeRE.alterations <- function(..., qualities = FALSE) {
    # names(alteration.labels) <- gsub('augment', 'sharp', names(alteration.labels))
    # names(alteration.labels) <- gsub('diminish', 'flat', names(alteration.labels))

    
    paste0('(', 
           overdot(makeRE.tonalChroma(parts = c("species", "step"), ...,
                              steps.sign = FALSE, qualities = qualities, flat = 'b',
                              step.labels = c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13),
                              regexname = 'alterations')),
           ')*')
}

makeRE.key <- function(..., parts = c("step", "species", "mode", "alterations"),
                       step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                       collapse = TRUE) {
    
    parts <- c("star", parts[parts %in% c("step", "species")], "colon", parts[parts %in% c("mode", "alterations")])
    if (collapse && "mode" %in% parts) {
        REs <- makeRE.tonalChroma(parts = parts[parts %in% c("step", "species")],
                                  step.labels = toupper(step.labels), qualities = FALSE,
                                  steps.sign = FALSE, collapse = FALSE, 
                                  ...)
        res <- makeRE.tonalChroma(parts = parts[parts %in% c("step", "species")],
                                  step.labels = tolower(step.labels), qualities = FALSE,
                                  steps.sign = FALSE, collapse = FALSE,
                                  ...)
        
        REs['colon'] <- res['colon'] <- ':?'
        REs['star'] <- res['star'] <- '\\*?'
        
        REs['mode'] <- captureRE(c('mix', 'lyd', 'ion'), n = '?')
        res['mode'] <- captureRE(c('phr', 'aeo', 'loc', 'dor'), n = '?')
        
        majors <- cREs(REs[parts[parts %in% names(REs)]])
        minors <- cREs(res[parts[parts %in% names(REs)]])
        
        REs <- c(step =  cREs(c(majors, minors), sep = '|'))
    } else {
        REs <- makeRE.tonalChroma(parts = parts[parts %in% c("step", "species")],
                                  step.labels = step.labels, qualities = FALSE,
                                  steps.sign = TRUE, collapse = FALSE,
                                  ...)
        REs['colon'] <-  ':?'
        REs['star']  <- '\\*?'
        REs['mode'] <- captureRE(c('mix', 'lyd', 'ion', 'phr', 'aeo', 'loc', 'dor'), n = '?')
    }
    
    
    if ("alterations" %in% parts) {
        REs["alterations"] <- makeRE.alterations(...)
    }
    
    
    REs <- REs[parts[parts %in% names(REs)]]
    
    if (collapse) setNames(cREs(REs), 'key') else REs
}

makeRE.romanKey <- function(..., flat = 'b') {

    makeRE.key(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
               parts = c('species', 'step', 'mode', 'alterations'),
               flat = flat,
               ...)
    
}



makeRE.signature <- function(flat = '-', ...) {
    
    RE <- cREs(list(steps = '[A-Ga-g]',  accidentals = makeRE.accidentals(flat = flat, ...)))
    paste0('^\\*?k\\[(', RE, ')*\\]')
}

makeRE.diatonicPartition <- function(..., split = '/', mustPartition = FALSE) {
    
    key <- makeRE.key(...)
    romanNumeral <- makeRE.romanKey(...)
    
    re <- orRE(key, romanNumeral)
    
    paste0(re, '(', split, re, ')', if (mustPartition) '+' else '*')
}


####. REs for tertian sets ####

makeRE.sciChord <- function(..., major = 'M', minor = 'm', augment = '+', diminish = 'o', perfect = 'P', collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts = c("step", 'species'),
                              step.labels = '[A-G]', qualities = FALSE,
                              step.sign = FALSE, collapse = FALSE, ...)
    
    qualityRE <- captureRE(c(major, minor, augment, diminish))
    REs['quality'] <-  paste0('(', 
                              qualityRE, '{3}',  
                              captureRE(c(perfect, augment, diminish)), 
                              qualityRE, 
                              '?)|(', 
                              qualityRE, '{1,3})')
   
    REs <- REs[c("step", "species", "quality")]
    
    if (collapse) setNames(cREs(REs), 'sciChord') else REs
}

makeRE.romanChord <- function(..., diminish = 'o', augment = '+', collapse = TRUE) {
    augment <- paste0('[', augment, ']') # because "+" is a special character!
    
    REs <- list()
    REs$accidental <- makeRE.accidentals(...)
    
    upper <- paste0('(?=[IV]+', augment,  '?)', captureRE(c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')))
    lower <- paste0('(?=[iv]+', diminish, '?)', captureRE(c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii')))
    REs$numeral <- orRE(upper, lower)
    
    
    REs$triadalt <- captureRE(c(diminish, augment), n = '?')

    
    REs['figurations'] <- makeRE.alterations(...)
    REs <- REs[c('accidental', 'numeral', 'triadalt', 'figurations')]
    
    
    if (collapse) setNames(cREs(REs), 'chord') else REs
}



makeRE.tertianPartition <- function(..., split = '/', mustPartition = FALSE) {
    
    romanChord <- makeRE.romanChord(...)
    
    
    key <- makeRE.diatonicPartition(..., split = split, mustPartition = FALSE)
    
    paste0(romanChord, '(', split, key, ')', if (!mustPartition) '?')
}



####. REs for durations ####

makeRE.recip <- function(...) getRE('recip')


makeRE.timeSignature <- function(sep = '/', collapse = TRUE, ...) {
    REs <- list(star = '\\*?',
         em   = 'M?',
         numerator = '[1-9][0-9]*',
         sep = sep,
         denominator = '[1-9][0-9]*'
         )
    
    if (collapse) setNames(cREs(REs), 'timeSignature') else REs
    
}
