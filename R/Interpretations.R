### This file contains info about "known" humdrum interpretations,
### meaning standardized exclusive and tandem interpretations like
### **kern and *C:
### For exclusive interpretations, we get a regular expression
### describing the kinds of tokens we expect to see in that interpretation.
### For tandem interpretations, we get a regular expression that indicates
### different versions of the same tandem interpretation information:
### for instance "*clefG2" and "*clefF4" are both examples of the same type
### of tandem information.

#' @format 
#' @rdname readHumdrum
#' @export
knownInterpretations <- data.table::fread(system.file('extdata', 'KnownInterpretations.tsv', package = 'humdrumR'))
# Preprocess self-referential {}s in the file KnownInterperations.tsv
for (i in seq_along(knownInterpretations$RE)) {
          knownREs <- list2env(as.list(setNames(knownInterpretations$RE, knownInterpretations$Name)))
          knownInterpretations$RE[i] <- glue::glue(knownInterpretations$RE[i], 
                                                   .envir = knownREs, 
                                                   .open = '<<', .close = '>>')         
}

#' @export
getRE <- function(pattern = NULL, types = c('Tandem', 'Exclusive', 'Atomic'), strict = FALSE) {
 known <- knownInterpretations[knownInterpretations$Type %in% types, ]          
 if (is.null(pattern)) return(known$Name)
 
 interpPattern <- tolower(pattern)
 hits <- sapply(interpPattern,
                \(pat) {
                  hits <- which(pat == tolower(known$Name))
                  if (length(hits) == 0) hits <- pmatch(pat, tolower(known$Name))
                  hits
                })
 res <- known$RE[hits]
 
 
 if (strict) {
           ressplit <- strsplit(unlist(res), split = '\\|')
           
           res <- sapply(ressplit, \(re) paste(paste0('^(', re, ')$'), collapse = '|'))
 }
 names(res) <- pattern # interpPattern
 
 res[is.na(res)] <- pattern[is.na(res)]
 res
 
}

#' @export
getREexclusive <- function(pattern) {
    checks(pattern, xcharacter)
    exclusive <- knownInterpretations[knownInterpretations$RE == getRE(pattern), ]$Exclusive  
    if (length(exclusive) == 0L || exclusive == "") NULL else exclusive
}
                                     

matchKnownExclusive <- function(strs) {
  #' @export
  checks(strs, xcharacter)
  sapply(knownInterpretations[knownInterpretations$Type == 'Exclusive', ], stringr::str_detect, string = strs)
}

matchKnownTandem <- function(strs) {
  #' @export
  checks(strs, xcharacter)
  knownTand <- knownInterpretations[knownInterpretations$Type == 'Tandem', ]
  output <- sapply(knownTand$RE, stringr::str_detect, string = strs, simplify = TRUE)
  dim(output) <- c(length(strs), nrow(knownTand))
  colnames(output) <- knownTand$Name
  rownames(output) <- strs
  output
}

isKnownTandem <- function(strs) {
  #' @export
  checks(strs, xcharacter)
  rowSums(matchKnownTandem(strs)) > 0L
}

generalizeTandem <- function(strs) {
  checks(strs, xcharacter)
  hits <- matchKnownTandem(strs)
  misses <- rowSums(hits) == 0
  output <- setNames(strs, strs)
  
  hit.arr.ind <- which(hits[!misses, , drop = FALSE], arr.ind = TRUE)
  hit.arr.ind <- hit.arr.ind[order(hit.arr.ind[ , 'row']), , drop = FALSE ]
  
  output <- knownInterpretations[knownInterpretations$Type == 'Tandem' , ]$RE[hit.arr.ind[ , 'col']]
  setNames(output, strs)
  
}

#' @export
idTandem <- function(strs) {
  checks(strs, xcharacter)
  hits <- matchKnownTandem(strs)
  misses <- rowSums(hits) == 0
  output <- setNames(strs, strs)
  
  hit.arr.ind <- which(hits[!misses, , drop = FALSE], arr.ind = TRUE)
  hit.arr.ind <- hit.arr.ind[order(hit.arr.ind[ , 'row']), , drop = FALSE ]
  
  output[!misses] <- knownInterpretations[knownInterpretations$Type == 'Tandem', ]$Name[hit.arr.ind[ , 'col']]
  output
}







