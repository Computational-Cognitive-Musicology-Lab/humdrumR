### This file contains info about "known" humdrum interpretations,
### meaning standardized exclusive and tandem interpretations like
### **kern and *C:
### For exclusive interpretations, we get a regular expression
### describing the kinds of tokens we expect to see in that interpretation.
### For tandem interpretations, we get a regular expression that indicates
### different versions of the same tandem interpretation information:
### for instance "*clefG2" and "*clefF4" are both examples of the same type
### of tandem information.

knownInterpretations <- data.table::fread(system.file('extdata', 'KnownInterpretations.tsv', package = 'humdrumR'))
# Preprocess self-referential {}s in the file KnownInterperations.tsv
for (i in seq_along(knownInterpretations$RE)) {
          knownREs <- list2env(as.list(setNames(knownInterpretations$RE, knownInterpretations$Name)))
          knownInterpretations$RE[i] <- glue::glue(knownInterpretations$RE[i], .envir = knownREs)         
}

#' @export
getRE <- function(names, types = c('Tandem', 'Exclusive'), strict = FALSE) {
 known <- knownInterpretations[Type %in% types]          
          
 names <- tolower(names)
 hits <- which(names == tolower(known$Name))
 if (length(hits) == 0) hits <- pmatch(names, tolower(known$Name), nomatch = NULL)

 res <- known$RE[hits]
 
 if (strict) {
           ressplit <- strsplit(unlist(res), split = '\\|')
           
           res <- sapply(ressplit, function(re) paste(paste0('^', REpar(re), '$'), collapse = '|'))
 }
 names(res) <- names
 res
 
}


               
                                     

knownExclusive <- function(strs) {
  #' @export
  sapply(knownExclusiveInterpretations, stringr::str_detect, string = strs)
}

knownTandem <- function(strs) {
  #' @export
  output <- sapply(knownTandemInterpretations$RE, stringr::str_detect, string = strs, simplify = TRUE)
  dim(output) <- c(length(strs), nrow(knownTandemInterpretations))
  colnames(output) <- knownTandemInterpretations$Name
  rownames(output) <- strs
  output
}

isKnownTandem <- function(strs) {
  #' @export
  rowSums(knownTandem(strs)) > 0 
}

generalizeTandem <- function(strs) {
  #' @export
  hits <- knownTandem(strs)
  misses <- rowSums(hits) == 0
  output <- setNames(strs, strs)
  
  hit.arr.ind <- which(hits[!misses, , drop = FALSE], arr.ind = TRUE)
  hit.arr.ind <- hit.arr.ind[order(hit.arr.ind[ , 'row']), , drop = FALSE ]
  
  output <- knownTandemInterpretations$RE[hit.arr.ind[ , 'col']]
  setNames(output, strs)
  
}

idTandem <- function(strs) {
  #' @export
  hits <- knownTandem(strs)
  misses <- rowSums(hits) == 0
  output <- setNames(strs, strs)
  
  hit.arr.ind <- which(hits[!misses, , drop = FALSE], arr.ind = TRUE)
  hit.arr.ind <- hit.arr.ind[order(hit.arr.ind[ , 'row']), , drop = FALSE ]
  
  output[!misses] <- knownTandemInterpretations$Name[hit.arr.ind[ , 'col']]
  output
}







