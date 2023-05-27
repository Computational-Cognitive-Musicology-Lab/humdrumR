
knitr::opts_chunk$set(

  collapse = TRUE, message = FALSE,
  comment = ">\t"
)

hm <- "humdrum$_{\\mathbb{R}}$"
Hm <- "Humdrum$_{\\mathbb{R}}$"

library(humdrumR)
humdrumR(syntaxHighlight = FALSE, maxRecordsPerFile = 30L)

knitr::opts_knit$set(root.dir = humdrumRroot)
