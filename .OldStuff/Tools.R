applyx = function(X, MARGIN = 2, FUN, ...) {
  output = apply(X, MARGIN, FUN, ...)

  if(MARGIN == 1) output = t(output)

  if(!is.null(dim(output)) && dim(output) == dim(X) && is.data.frame(X)){
    output = as.data.frame(output, stringsAsFactors = FALSE)
    names(output) = names(X)
    rownames(output) = rownames(X)
  }

  output
}



grepl2 = function(pattern, x){
  apply(x, 2, grepl, pattern = pattern)
}

`%in%` = function(x, table) UseMethod('%in%')
`%in%.default` = function(x, table) match(x, table, nomatch = 0L) > 0L

`%in%.function` = function(what, where) {
 any(sapply(where, identical, y = what))
}
