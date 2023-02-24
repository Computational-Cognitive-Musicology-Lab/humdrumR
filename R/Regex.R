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
    
    sep <- setNames(if (is.null(sep)) character(length(res)) else lag(rep(sep, length.out = length(res)), fill = ''), names(res))
    
    if (reverse) res <- rev(res)
    
    for (re in names(res)) {
         rest <- stringr::str_remove(rest, pattern = paste0('^', sep[re]))
        
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
    
    matches <- lapply(matches, `names<-`, value = NULL)
    
    if (toEnv) list2env(matches, parent.frame())

    output <- do.call('cbind', matches)
    
    if (toEnv) invisible(output) else output
    
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


REapply <- function(str, regex, .func, inPlace = TRUE, ..., args = list(), outputClass = 'character') {
    if (!is.character(str)) .stop(call. = FALSE,
                                "Sorry, REapply can only apply to an x argument that is a character vector.")
    checks(str, xcharacter)
    checks(regex, xcharacter)
    checks(inPlace, xTF)
    
  
    regex <- getRE(regex)
    matches <- stringi::stri_extract_first_regex(str = str, pattern = regex)
    
    #
    hits <- !is.na(matches)
    
    args <- local({
      args <- c(list(matches), args, list(...))
      firstArgLength <- length(str)
      args[lengths(args) == firstArgLength] <- lapply(args[lengths(args) == firstArgLength], '[', i = hits)
      args
    })
    
    result <- vectorNA(length(str), outputClass)
    if (any(hits)) result[hits] <- do(.func, args)
    
    if (inPlace && outputClass == 'character') result <- stringi::stri_replace_first_fixed(str, matches, result)
    
    result

}












## %% Regex tools ----


#' Match strings against regular expression
#' 
#' The functions give you a concise way to search for regular expressions in `character` vectors.
#' They are "infix" functions, meaning you write the function between its two arguments:
#' `myvector %~% regex`.
#'  
#' 
#' @details 
#' 
#' Each version of the function returns a different type of information about regex matches (if any)
#' in the input vector:
#' 
#' + `%~l%`: returns `logical` (`TRUE`/`FALSE`) indicating where in `x` there are matches.
#' + `%~i%`: returns `integer` indicating the indices of matches in `x`.
#' + `%~n%`: returns `integer` indicating the number (count) of matches in each string.
#' + `%~m%`: returns `character` string of the matched string itself. Returns `NA`
#'   where there is no match.
#' 
#' The basic function (`%~%`) is the same as `%~l%`.
#' There is also a negative versions of the `l` and `i` functions: giving all
#' strings that *don't* match the given regular expression.
#' These are `%!~%`, `%!~l%`, and `%!~i%`.
#'
#' These functions are simply syntactic sugar for
#' existing `R` regular expression matching functions:
#' 
#' + `%~l%`: [base::grepl()]
#' + `%~i%`: [base::grep()]
#' + `%~n%`: [stringi::stri_count_regex()]
#' + `%~m%`: [stringi::stri_extract_first_regex()]
#' 
#' @section Multiple regexes:
#' 
#' If more than one regex is supplied,
#' `%~l%` and `%~i%` return the indices where *any* of the regexes match.
#' In the case of `%~n%`, each matching regex is counted separately, and they are all summed.
#' In the case of `%~m%`, all matches (if any) are pasted together, 
#' including multiple matches of the same string.
#' 
#' 
#' @param x A `character` vector to search in.
#' @param regex One or more regular expressions. If more than one regex is supplied,
#'  matches to *any* of the regexes are returned. (See "Multiple regexes" section.)
#'
#' @export
#' @name RegexFind
`%~l%` <- function(x, regex) {
  checks(x, xatomic)
  checks(regex, xcharacter & xminlength(1))
  Reduce('|', lapply(regex, grepl, x = x))
}
#' @export
#' @rdname RegexFind
`%~i%` <- function(x, regex) {
  checks(x, xatomic)
  checks(regex, xcharacter & xminlength(1))
  Reduce('union', lapply(regex, grep, x = x))
}
#' @export
#' @rdname RegexFind
`%~n%` <- function(x, regex) {
  checks(x, xatomic)
  checks(regex, xcharacter & xminlength(1))
  Reduce('+', lapply(regex, stringi::stri_count_regex, str = x))
}
#' @export
#' @rdname RegexFind
`%~m%` <- function(x, regex) {
  checks(x, xatomic)
  checks(regex, xcharacter & xminlength(1))
  
  do.call('.paste', c(list(na.if = all), lapply(regex, stringi::stri_extract_first_regex, str = x)))
}
#' @rdname RegexFind
#' @export
`%~%` <- `%~l%`
#' @rdname RegexFind
#' @export
`%!~%` <- Negate(`%~l%`)
#' @rdname RegexFind
#' @export
`%!~l%` <- Negate(`%~l%`)
#' @rdname RegexFind
#' @export
`%!~i%` <- function(x, pattern) which(!x %~l% pattern)



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
# takes a RE capture group and makes it so it will only match one or more of the SAME string
    
    strs[nchar(strs) > 1L] <- paste0('(', strs[nchar(strs) > 1L], ')')
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


escape <- function(str) {
  stringr::str_replace_all(str, "([+*?])", "\\\\\\1")
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
    
    output <- if (!parse.exhaust && length(REs) > 1L) {
      Reduce(\(head, last) paste0(head, sep,  '(.*(', last, '))'), REs, right = TRUE )
    } else {
      do.call('.paste', c(as.list(unname(REs)), list(sep = if (is.null(sep)) "" else sep)))
      # paste(REs, collapse = if (is.null(sep)) "" else sep)
    }
    
    list(output)
    
}

#### REs for tonalIntervals ----

makeRE.steps <- function(step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), step.signed = FALSE, ...)  {
    if (is.null(step.labels)) return('[1-9][0-9]*')
    
    if (step.signed) step.labels <- c(tolower(step.labels), toupper(step.labels))
    
    
        # captureUniq(step.labels, zero = FALSE)

    captureRE(step.labels) 


}

makeRE.accidentals <- function(sharp = '#', flat = '-', natural = 'n', doublesharp = NULL, doubleflat = NULL, ...) {
  
 if (flat %in% c('+', '?')) flat <- paste0('\\', flat)  
 if (sharp %in% c('+', '?')) sharp <- paste0('\\', sharp)  
  
 if (nchar(natural) > 1L)  natural <-  paste0('(', natural, ')') 
  
  paste0(natural, '?', captureUniq(c(sharp, flat, doublesharp, doubleflat)))
}

makeRE.qualities <- function(major = 'M', minor = 'm', perfect = 'P', augment = 'A', diminish = 'd', ...) {
    paste0('(', captureRE(c(perfect, major, minor), ''), '|', captureUniq(c(diminish, augment)), ')')
}

makeRE.contours <- function(octave.integer = TRUE, up = '\\^', down = 'v', ...) {
    if (octave.integer) '-?[0-9]+' else captureUniq(c(up, down))
}

makeRE.tonalChroma <- function(parts = c("step", "species", "octave"), qualities = FALSE, collapse = TRUE, 
                               parse.exhaust = TRUE,
                               sep = NULL, ..., regexname = 'tonalChroma'){
    REs <-  list(sign    = if ('sign' %in% parts)       '[-+]?',
                 step    = if ("step" %in% parts)       makeRE.steps(...),
                 species = if ("species" %in% parts) {if (qualities) makeRE.qualities(...) else makeRE.accidentals(...)},
                 octave  = if ("octave" %in% parts)    makeRE.contours(...)
                 )[parts]
    
    
    if (collapse) setNames(cREs(REs, sep = sep, parse.exhaust = parse.exhaust), regexname) else REs
    
    
}

makeRE.kern <- function(parts = c("step", "species"), 
                        step.labels = unlist(lapply(1:50, strrep, x = c('C', 'D', 'E', 'F', 'G', 'A', 'B'))),
                        step.signed = TRUE,
                        qualities = FALSE, 
                        octave.integer = FALSE,
                        
                        ...) {
    
    makeRE.tonalChroma(parts, step.labels = step.labels, 
                       step.signed = step.signed, 
                       qualities = qualities,
                       octave.integer = octave.integer, ..., regexname = 'kern')
    
}


makeRE.lilypond <- partialApply(makeRE.tonalChroma, step.labels = c('c', 'd', 'e', 'f', 'g', 'a', 'b'),
                                sharp = 'is', flat = 'es', regexname = 'lilypond', 
                                octave.integer = FALSE, up = "'", down = ",")

makeRE.helmholtz <- partialApply(makeRE.tonalChroma, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'),
                                 step.signed = TRUE, regexname = 'helmholtz', 
                                 flat = 'b', octave.integer = FALSE, up = "'", down = ",")

makeRE.tonh <- function(parts = c('step', 'species', 'octave'), flat = 'es',
                        step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), ...) {
  
  REs <- makeRE.tonalChroma(parts = parts, flat = flat, step.labels = step.labels, octave.integer = TRUE, collapse = FALSE)
  
  REs$step <- paste0('(', REs$step, '|[HS])')
  REs$species <- paste0('(', REs$species, '|s)')
  
  setNames(cREs(REs), 'tonh')
  
}

makeRE.sciPitch <- partialApply(makeRE.tonalChroma, octave.offset = 4L, octave.integer = TRUE, flat = 'b')

makeRE.interval <- function(parts = c("direction", "species", "step"), step.labels = 1:99, collapse = TRUE, qualities = TRUE, flat = 'b', ...) {
    REs <- makeRE.tonalChroma(parts[parts != 'direction'], collapse  = FALSE, qualities = qualities, step.labels = step.labels,
                              flat = flat, ..., regexname = 'interval')
    
    if ('direction' %in% parts) REs$direction <- '[+-]?'
    REs <- REs[parts]
    
    if (collapse) setNames(cREs(REs), 'solfa') else REs
}


makeRE.degree <- partialApply(makeRE.tonalChroma, parts = c("step", "species", "octave"), step.labels = 1:7, octave.integer = TRUE,
                              flat = '-', sharp = '\\+', qualities = FALSE, regexname = 'degree', sep = c("", "/"))

makeRE.deg <- partialApply(makeRE.tonalChroma, parts = c( "octave", "step", "species"), step.labels = 1:7, 
                           octave.integer = FALSE, 
                           flat = '-', sharp = '\\+', qualities = FALSE, regexname = 'deg')

makeRE.solfa <- function(parts = c("octave", "step", "species"), octave.integer = FALSE, flat = '-', ..., collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts[parts != "step"], octave.integer = octave.integer, ..., flat = flat, collapse = FALSE)
    
    if ("step" %in% parts) {
        REs$step <- "([sd][eoi]|[fl][eai]|[mt][eiy]|r[aei])"
        REs <- REs[parts]
    }
    
    if (collapse) setNames(cREs(REs), 'solfa') else REs
}

makeRE.solfg <- partialApply(makeRE.tonalChroma, parts = c('step', 'species', 'octave'),
                             step.labels = c('do', 're', 'mi', 'fa', 'so', 'la', 'ti'),
                             flat = '~b', doubleflat = '~bb', natural = '~n', sharp = '~d', doublesharp = '~dd',
                             octave.integer = TRUE, regexname = 'solfg')

makeRE.bhatk <- function(parts = c("step", "octave"), up = "'", down = ',', 
                         step.labels = c('S', 'r', 'R', 'G', 'g', 'M', 'm', 'P', 'D', 'd', 'N', 'n'),
                         octave.integer = FALSE, ..., collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts = parts, step.labels = step.labels,
                              up = up, down = down,
                              octave.integer = octave.integer, 
                              ..., collapse = FALSE)
    

    if (collapse) setNames(cREs(REs), 'bhatk') else REs
}
#### REs for numbers ----


makeRE.double <- function(...) c(double = "[+-]?[0-9]+(\\.[0-9]+)?" )
makeRE.fraction <- function(sep = '/', ...) paste0("[1-9][0-9]*", sep, "[1-9][0-9]*")


makeRE.pc <- function(ten = 'A', eleven = 'B', ...) {
  captureRE(c(0:11, ten, eleven))
}

#### REs for diatonic sets ####

makeRE.alterations <- function(..., qualities = FALSE) {
    # names(alteration.labels) <- gsub('augment', 'sharp', names(alteration.labels))
    # names(alteration.labels) <- gsub('diminish', 'flat', names(alteration.labels))

    
    makeRE <- partialApply(makeRE.tonalChroma,
                           parts = c("species", "step"), step.signed = FALSE, flat = 'b',
                           step.labels = c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13),
                           regexname = 'alterations')
    
    paste0('(', makeRE(..., qualities = qualities), ')*')
}

makeRE.key <- function(..., parts = c("step", "species", "mode", "alterations"),
                       step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B'), 
                       collapse = TRUE) {
    
    parts <- c("star", parts[parts %in% c("step", "species")], "colon", parts[parts %in% c("mode", "alterations")])
    if (collapse && "mode" %in% parts) {
        REs <- makeRE.tonalChroma(parts = parts[parts %in% c("step", "species")],
                                  step.labels = toupper(step.labels), qualities = FALSE,
                                  step.signed = FALSE, collapse = FALSE, 
                                  ...)
        res <- makeRE.tonalChroma(parts = parts[parts %in% c("step", "species")],
                                  step.labels = tolower(step.labels), qualities = FALSE,
                                  step.signed = FALSE, collapse = FALSE,
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
                                  step.signed = TRUE, collapse = FALSE,
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

makeRE.romanKey <- function(..., collapse = TRUE, sep = '/') {

    numeral <- makeRE.key(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                          parts = c('species', 'step', 'mode', 'alterations'),
                          collapse = TRUE, 
                          ...)
    
    
    REs <- list(head = paste0('(', numeral, ')'),
                rest = paste0('(', sep, '(', numeral, '))*'))
    
    if (collapse) setNames(cREs(REs), 'romanKey') else REs
    
}



makeRE.signature <- function(flat = '-', ...) {
    
    RE <- cREs(list(steps = '[A-Ga-g]',  accidentals = makeRE.accidentals(flat = flat, ...)))
    paste0('^\\*?k\\[(', RE, ')*\\]')
}

makeRE.diatonicPartition <- function(..., split = '/', mustPartition = FALSE) {
    
    key <- makeRE.key(...)
    romanNumeral <- makeRE.romanKey(...)
    
    re <- orRE(key, romanNumeral)
    
    paste0(romanNumeral, '(', split, re, ')', if (mustPartition) '+' else '*')
}


#### REs for tertian sets ####

makeRE.tertian <- function(..., major = 'M', minor = 'm', augment = '+', diminish = 'o', perfect = 'P', collapse = TRUE) {
    
    REs <- makeRE.tonalChroma(parts = c("step", "species"),
                              step.labels = '[A-G]', qualities = FALSE,
                              step.sign = FALSE, ...)
    
    REs$incomplete <- '[135]?'
    
    qualityRE <- captureRE(c(major, minor, augment, diminish, '.'))
    REs$quality <-  paste0('((', 
                           qualityRE, '{3}',  
                           captureRE(c(perfect, augment, diminish, '.')), 
                           qualityRE, 
                           '?)|(', 
                           qualityRE, '{1,3}))')
    
    REs$inversion <- '(/[1-7])?'
   
    
    REs <- REs[c("tonalChroma", "incomplete", "quality", "inversion")]
    
    if (collapse) setNames(cREs(REs), 'tertian') else REs
}

makeRE.chord <-  function(..., major = 'maj', minor = 'min', augment = 'aug', diminish = 'dim', 
                                bass.sep = '/', flat = 'b', 
                                collapse = TRUE) {
  REs <- makeRE.tonalChroma(parts = c("step", "species"), flat = flat,
                            step.labels = '[A-G]', qualities = FALSE,
                            step.sign = FALSE, ...)
  
  REs$quality <- captureRE(c(major, minor, augment, diminish), '?')
  
  REs$figurations <- makeRE.alterations(...)
  
  REs$bass <- paste0('(', bass.sep, REs$tonalChroma, ')?')
  
  REs <- REs[c("tonalChroma", "quality", "figurations", "bass")]
  
  
  
  if (collapse) setNames(cREs(REs), 'chord') else REs
  
}

makeRE.roman <- function(..., diminish = 'o', augment = '+', sep = '/', collapse = TRUE) {
    augment <- paste0('[', augment, ']') # because "+" is a special character!
    
    REs <- list()
    REs$accidental <- makeRE.accidentals(...)
    
    upper <- paste0('(?=[IV]+', augment,  '?)', captureRE(c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')))
    lower <- paste0('(?=[iv]+', diminish, '?)', captureRE(c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii')))
    REs$numeral <- orRE(upper, lower)
    
    
    REs$triadalt <- captureRE(c(diminish, augment), n = '?')

    
    REs$figurations <- makeRE.alterations(...)
    REs <- REs[c('accidental', 'numeral', 'triadalt', 'figurations')]
    
    
    key <- makeRE.key(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                           parts = c('species', 'step', 'mode', 'alterations'),
                           collapse = TRUE, 
                           ...)
    
    REs$of <- paste0('(', sep, '(', key, '))*')
    
    if (collapse) setNames(cREs(REs), 'roman') else REs
}

makeRE.harm <- function(..., diminish = 'o', augment = '+', sep = '/', collapse = TRUE) {
  augment <- paste0('[', augment, ']') # because "+" is a special character!
  
  REs <- list()
  REs$accidental <- makeRE.accidentals(...)
  
  upper <- paste0('(?=[IV]+', augment,  '?)', captureRE(c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII')))
  lower <- paste0('(?=[iv]+', diminish, '?)', captureRE(c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii')))
  REs$numeral <- orRE(upper, lower)
  
  
  REs$triadalt <- captureRE(c(diminish, augment), n = '?')
  
  
  REs$figurations <- makeRE.alterations(diminish = 'D', augment = 'A', qualities = TRUE, ...)
  REs$inversion <- '[a-g]?'
  REs <- REs[c('accidental', 'numeral', 'triadalt', 'figurations', 'inversion')]
  
  
  key <- makeRE.key(step.labels = c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'),
                    parts = c('species', 'step', 'mode', 'alterations'),
                    collapse = TRUE, 
                    ...)
  
  REs$of <- paste0('(', sep, '(', key, '))*')
  
  if (collapse) setNames(cREs(REs), 'harm') else REs
}



makeRE.tertianPartition <- function(..., split = '/', mustPartition = FALSE) {
    
    roman <- makeRE.roman(...)
    
    
    key <- makeRE.diatonicPartition(..., split = split, mustPartition = FALSE)
    
    paste0(roman, '(', split, key, ')', if (!mustPartition) '?')
}



#### REs for durations ####

makeRE.dur <- function(..., sep.time = ':', sep.date = '/', sep.decimal = '\\.', collapse = TRUE) {
  number <- '[0-9]*'
  
  REs <- list(prefix = captureRE(c('~', '>', '<'), n = '?'),
              datetime = paste0('(', number, sep.date, '){0,3}',
                                '(', number, sep.time, '){0,2}',
                                number),
              decimal = paste0('(', sep.decimal, '[0-9]+)?'))
  
  if (collapse) setNames(cREs(REs), 'dur') else REs
  
  
}

makeRE.recip <- function(collapse = TRUE, fractions = TRUE, sep = '%', ...) {
  REs <- list(graceMark1 = '([Qq][^0-9q]*)?',
              recip = paste0('(', if (fractions) paste0('([1-9][0-9]*', sep, ')?'),
                             '[1-9][0-9]*\\.*|0{1,2}\\.*)'),
              graceMark2 = '([^qQ]*[Qq])?')
  if (collapse) setNames(cREs(REs), 'recip') else REs
}


makeRE.timeSignature <- function(..., sep = '/', collapse = TRUE) {
  REs <- list(star = '\\*?',
              em   = 'M',
              numerator = '[1-9][0-9]*([+][1-9][0-9]*)*',
              sep = sep,
              denominator = '[1-9][0-9]*'
  )
  
    if (collapse) setNames(cREs(REs), 'timeSignature') else REs
    
}

makeRE.grid <- function(..., sep = '', on = 'X', off = 'O') {
  RE <- paste0(on, '(', sep, off, ')*')
  
  setNames(RE, 'grid')
}

notehead.unicode <- data.frame(stringsAsFactors = FALSE,
                                Unicode = c('\U1D15C', '\U1D15D', '\U1D15E', '\U1D15F', 
                                            '\U1D160', '\U1D161', '\U1D162', '\U1D163', '\U1D164'),
                                Recip = c('0', '1', '2', '4', '8', '16', '32', '64', '128'))

makeRE.notehead <- function(..., sep = " \U2215", collapse = TRUE) {
  
  REs <- list(multiplies = '([1-9][0-9]*)?',
              value = captureRE(notehead.unicode$Unicode),
              divides = paste0('(', sep, '[1-9][0-9]*)?'),
              space = ' ',
              dots = paste0('(',  '\U1D16D\U2009',')*'))
  
  if (collapse) setNames(cREs(REs), 'notehead') else REs
}

