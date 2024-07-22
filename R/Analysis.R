

# pattern finding ----


findrep <- function(x, func = `==`) {
  x <- outer(x, x, func)
  
  
  x
  
}

getDiagonals <- function(mat, upper = TRUE, min.n = 4, max.lag = 100) {
  grid <- as.data.table(expand.grid(Row = seq_len(nrow(mat)), Col = seq_len(ncol(mat))))
  
  grid[ , Lag := Col - Row]
  setorder(grid, Lag)
  if (upper) grid <- grid[Lag > 0]
  
  grid <- grid[(nrow(mat) - Lag) >= min.n & Lag <= max.lag]
  
  grid[, list(Sequence = list(rle(mat[cbind(Row, Col)]))), by = Lag]
}

findstretches <- function(rle, lag , min.n = 4) {
  
  rle$values[rle$lengths < min.n] <- FALSE
  hits <- cumsum(c(1, head(rle$lengths, n = -1L)))[rle$values]
  cbind(Antecedent = hits, Consequent = hits + lag, Length = rle$lengths[rle$values])
}

findrepeats <- function(x, min.n = 4, max.lag = 400, func = `==`) {
  findrep(x, func = func) |> getDiagonals(min.n = min.n, max.lag = max.lag) -> sequences
  
  sequences[ , Hits := Map(\(s, l) findstretches(s, l, min.n = min.n), Sequence, Lag)]
  sequences[lengths(Hits) > 1L, Hits] |> do.call(what = 'rbind') |> as.data.table() -> sequences
  if (nrow(sequences) == 0) return(data.table(Antecedent = integer(0), Consequent = integer(0)))
  setorder(sequences, Antecedent)
  sequences[ , Lag := Consequent - Antecedent]
  sequences[]
  
}
