#!/usr/bin/env Rscript

setwd("~/Bridge/Coding/R/Packages/humdrumR")
library(data.table)

  manfiles <- dir('man', pattern = '\\.Rd$')

do <- function(file) {
  
  lines <- readLines(paste0('man/', file))
  Source <- grep('Please edit documentation', lines, value = TRUE) |> gsub(pat = '^%.*R\\/', rep = '')
  
  N <- sum(nchar(lines))
  Exported <- paste(grep('alias\\{', lines, value = TRUE) |> gsub(pat = '.*\\{', rep = '') |> gsub(pat = '\\}$', rep = ''), collapse = ', ')
  
  
  Family <- paste(grep('concept\\{', lines, value = TRUE) |> gsub(pat = '.*\\{', rep = '') |> gsub(pat = '\\}*$', rep = ''), collapse = ', ')

  data.table(Rd = file, Source = Source, Nchar = N, Family = Family, Exported =Exported)
  

  
  
}

mans <- lapply(manfiles, do) |> rbindlist()


setorder(mans, Source, Rd)

existingSheet <- fread('documentation.tsv', header = TRUE)

mans <- cbind(mans, existingSheet[mans, on = c('Rd')][ , setdiff(colnames(existingSheet), colnames(mans)), with = FALSE])


setcolorder(mans, c("Source", "Rd", "Family", "Vignette", "Nchar", "Accurate", "Complete", "Exported"))

fwrite(mans, file = 'documentation.tsv', sep = '\t')


