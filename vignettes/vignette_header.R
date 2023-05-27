
knitr::opts_chunk$set(

  collapse = TRUE, message = FALSE,
  comment = ">\t"
)

hm <- "humdrum$_{\\mathbb{R}}$"
Hm <- "Humdrum$_{\\mathbb{R}}$"

library(humdrumR)
humdrumR(syntaxHighlight = FALSE, maxRecordsPerFile = 30L)

knitr::opts_knit$set(root.dir = humdrumRroot)

htmlColors <- function(humdrumR) {
  lines <- capture.output(show(humdrumR))
  browser()
  recs <- grepl('^  *[1-9][0-9]*:', lines)
  
  recNum <- stringr::str_extract(lines[recs], '^  *[1-9][0-9]*:  *')
  recContent <- stringr::str_remove(lines[recs], '^  *[1-9][0-9]*:  *')
  
  colors <- rep('yellow', sum(recs))
  colors[grepl('^!', recContent)] <- 'turquoise'
  colors[grepl('^\\*', recContent)] <- 'violet'
  colors[grepl('^\\*\\*', recContent)] <- 'red'
  colors[grepl('^=', recContent)] <- 'green'
  
  recContent <- paste0("<span style='color:", colors, ";'>", gsub(' ', '&nbsp;', recContent), "</span>")
  recNum <- gsub(' ', '&nbsp;', recNum)
  
  
  data <- paste0(recNum, recContent, '<br>')
  lines[recs] <- data
  
  cat(lines, sep = '\n')
}
# 
# local({
#   hook_old <- knitr::knit_hooks$get("output")  # save the old hook
#   knitr::knit_hooks$set(output = function(x, options) {
#     # now do whatever you want to do with x, and pass
#     # the new x to the old hook
#     x <- strsplit(x, split = '\n')[[1]]
#     
#     refl <- which(grepl('!!!', x))
#     ref <- x[refl[1]]
#     ref <- paste(ref, "FUCK THIS")
#     x[refl[1]] <- ref
#     x <- paste(x, collapse = '\n')
#     hook_old(x, options)
#   })
# })