
tandemTable <- function(tandems) {
          uniqueTandem <- unique(unlist(stringr::str_split(unique(tandems), ',')))
          uniqueTandem <- uniqueTandem[!is.na(uniqueTandem) & uniqueTandem != '']
          
          areKnownTandems <- isKnownTandem(uniqueTandem)
          knownTandems   <- uniqueTandem[areKnownTandems]
          # unknownTandems <- uniqueTandem[!areKnownTandems]
          
          tandemPatterns <- unique(generalizeTandem(knownTandems))
          tandemMat <- lapply(tandemPatterns,
                              function(tan) stringr::str_match(tandems, tan)[ ,1])
          
          # for (t in knownTandems) tandems <- stringi::stri_replace_all_fixed(tandems, pattern = t, '')
          # tandems <- stringi::stri_replace_all(tandems, regex = ',,*', ',')
          # tandems[tandems %in% c(',', '')] <- NA_character_
          
          names(tandemMat) <- unique(idTandem(knownTandems))
          
          # tandemMat$Tandem <- tandems
          
          as.data.table(tandemMat)
}


#' @export
knownExclusiveInterpretations = c(
  kernpitch = '(?<=^|[^A-Ga-g#-])([A-Ga-g])\\1*((#)*\\2*|(-)*\\3*)(?=$|[^A-Ga-g#-])',
  recip = '[1-9][0-9]*%?[1-9]?[0-9]*[.]*'
)

#' @export 
knownTandemInterpretations <- data.frame(stringsAsFactors = FALSE,
                                         Name      = c('Key', "KeySignature", "Clef", 
                                                       "Time Signature", "Mensuration", "BPM", "Timebase", 
                                                       "Instrument", "InstrumentClass", "TransposingInstrument"),
                                         RE        = c('\\*[A-Ga-g][-#b]*:', '\\*k\\[([a-g][#-]* *)*\\]',  '\\*clef[A-G]v*[1-5]*', 
                                                       '\\*M[1-9][0-9]*/((16)|(32)|[1248])', '\\*met\\([^)]*\\)', '\\*MM[0-9]+', '\\*tb[1-9][0-9]*',
                                                       '\\*I[^C,]+', '\\*IC[^,]*', '\\*ITr[^,]*'),
                                         Globish   = c("_:", "k[_]", "clef_",
                                                       "M_/_", "met(_)", "MM_", "tb_",
                                                       "I_", "IC_", "ITr_"))
rownames(knownTandemInterpretations) <- knownTandemInterpretations$Name

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







