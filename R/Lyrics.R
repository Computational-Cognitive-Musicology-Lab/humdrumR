silbe2wort <- function(x, sep = '-', keep.sep = TRUE, number.syllables = FALSE, groupby = list()) {
  checks(sep, xlen1 & xcharnotempty, seealso = 'wort')
  checks(keep.sep, xTF, seealso = 'wort')
  
  open  <- stringr::str_detect(x, paste0('^[^', sep, ']+', sep, '$'))
  close <- stringr::str_detect(x, paste0('^', sep, '[^', sep, ']+$'))
  
  if (!any(open) || !any(close)) return(x)
  
  if (sum(open) != sum(close)) {
    .warn("The silbe data you have provided has improperly connected words.")
  }
  windowFrame <- findWindows(x, which(open), which(close), overlap = 'none', groupby = groupby)
  
  expr <- if (number.syllables) {
    rlang::expr(paste0(paste(.x., collapse = !!sep), 
                       '[', seq_along(.x.), ']'))
  } else {
    rlang::expr(paste(.x., collapse = !!sep))
    
  }
  
  
  wort <- .applyWindows(data.table(.x. = x), windowFrame, expr, field = '.x.')
 
  
  wort <- if (keep.sep) {
    stringr::str_replace_all(wort, paste0(sep, '+'), sep)
  } else {
    stringr::str_replace_all(wort, paste0(sep, '[_', sep,']*'), '')
  }
  
  
  wort[is.na(wort) & !is.na(x) & x != '.'] <- '_'
  
  wort
}

#' Paste sylllables together into words 
#'
#' Most humdrum datasets which include lyrics, include them in a `**silbe` spine, representing each syllable from the lyrics on
#' one so they line up with notes in the music.
#' Syllables from multi-syllabic words are connected with `-` markers at the end of the first syllable, beginning of the last syllable
#' and both beginning and end of medial syllables.
#' The `wort()` command translates this syllable representation to words, simply collapsing them together.
#' The resulting word is aligned with the first syllable of each word in `**silbe`.
#' 
#' @details
#' 
#' If a non-null `Exclusive` argument is provided, `wort()` will only apply where `Exclusive == "silbe"`.
#' When used in a [withinHumdrum()] call, `wort()` will by automally passed the `Exclusive` field from the humdrum
#' data, as well as `groupby = list(Piece, Spine, Path)`, so that words are not collapsed across pieces/spines/paths.

#' The output of `wort()` is always the same length as the input.
#' Any collapsed syllables are replaced by the `**silbe` melisma marker, `"_"`.
#' If `number.syllables = TRUE`, the whole word is repeated for each syllable, but with each numbered 
#' in square brackets: e.g., `c("yesterday[1], "yesterday[2]", "yesterday[3]", "yesterday[4]")`. This
#' format is seen a lot in computational linguistics.
#'  
#' By default, the syllable separators are retained in the collapsed output: this makes it possible to recreate the syllables if 
#' necessary.
#' Any mid-word melismas (indicated by `"_"`) are kept collapsed as well, for the same reason.
#' However, if `keep.sep = TRUE`, seperators (and mid-word melismas) are removed, making the function non invertible (you can't easily get
#' back to the syllables).
#' 
#' 
#' @param x **A silbe data vector.***
#' 
#' Must be `character`.
#' 
#' @param x ***A vector of exclusive intepretations to control dispatch.***
#'
#' Defaults to `NULL`.
#' 
#' Must be `NULL`, or a `character` vector which is either length `1` or `length(x)`.
#' 
#' @param sep **What separator is in input and/or output.**
#' 
#' Defaults to `"-"`.
#' 
#' Must be a single, non-empty `character` string.
#' 
#' @param keep.sep **Should syllable separators be kept in output?**
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param number.syllables ***Should output show words with numbered syllables?***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be a singleton `logical` value: an on/off switch.
#' 
#' @param groupby **Optional vectors to group words within.**
#' 
#' Defaults to `list()`.
#' 
#' Must be a `list`; every element of the list must be length `length(x)`.
#' 
#' 
#' @examples 
#' 
#' wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'))
#'
#' wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'), 
#'      keep.sep = FALSE)
#'
#' wort(c('now', 'let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west'),
#'      keep.sep = FALSE, number.syllables = TRUE)
#' 
#' @name wort
#' @export
wort.character <- makeHumdrumDispatcher(list('silbe', NA, silbe2wort), 
                                        funcName = 'wort', outputClass = 'character', 
                                        args = alist(groupby = list()),
                                        memoize = FALSE, dispatch.attr = FALSE)

#' Apply to humdrumR data
#' 
#' If `wort()` is applied to a [humdrumR data class][humdrumRclass]
#' you may use the data's [fields][fields()] as arguments.
#' If no field names are specified, the first [selectedField] is used as `x`.
#'
#' @usage 
#' humData |> select(Token) |> wort() 
#' humData |> wort(Token)
#' 
#' @rdname  wort
#' @export
wort.humdrumR <- humdrumRmethod(wort.character)
#' @rdname wort
#' @export
wort <- humdrumRgeneric(wort.character)




