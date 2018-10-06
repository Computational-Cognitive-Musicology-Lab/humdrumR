

grepl_list <- function(pattern, ls) sapply(ls, function(el) any(grepl(pattern, el, useBytes = TRUE), na.rm = TRUE)) 


