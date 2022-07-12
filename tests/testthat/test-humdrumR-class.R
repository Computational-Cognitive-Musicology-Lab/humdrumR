
# Fold ----

test_that('Spine folding works properly', {
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  
  folded <- getHumtab(foldHumdrum(spine, c(2, 4), 1, newFieldNames =  'silbe'))
  
  expect_equal(dim(folded), c(24, 24))
  if (expect_true(all(c('Token', 'silbe1', 'silbe2') %in% colnames(folded)))) {
    expect_equal(folded[silbe1 == 'the' & silbe2 == 'a', Token], 'c#')
  }
  
})


test_that("Exclusive (spine) folding works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  # parallel (two silbe onto two kern)
  foldedSilbe <- getHumtab(foldExclusive(spine, 'silbe', 'kern'))
  
  expect_equal(dim(foldedSilbe), c(24, 23))
  if (expect_true(all(c('Token', 'silbe') %in% colnames(foldedSilbe)))) {
    expect_equal(foldedSilbe[ , table(Token, silbe)]['a', 'These'], 2L)
  }
  
  # spreading: one harm onto twokern
  foldedHarm <- getHumtab(foldExclusive(spine, 'harm', 'kern'))
  
  expect_equal(dim(foldedHarm), c(32, 23))
  if (expect_true(all(c('Token', 'harm') %in% colnames(foldedHarm)))) {
    expect_equal(foldedHarm[ , table(Token, harm)]['c#', 'AM'], 2L)
  }
  
})

test_that('Path folding works properly', {
  
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')

  foldedPath <- getHumtab(foldPaths(path))
  
  
  expect_equal(dim(foldedPath), c(25, 23))
  if (expect_true(all(c('Token', 'Token_Path') %in% colnames(foldedPath)))) {
    expect_equal(foldedPath[Stop == 1L, table(Token, Token_Path)]['c', 'a'], 2L)
  }
  
  
  
})

test_that('Stop folding works properly', {
  
  stop <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  foldedStop <- getHumtab(foldStops(stop))
  
  
  expect_equal(dim(foldedStop), c(18, 25))
  if (expect_true(all(c('Token', 'Token_Stop1', 'Token_Stop2') %in% colnames(foldedStop)))) {
    expect_equal(foldedStop[ , table(Token, Token_Stop1, Token_Stop2)]['b', 'd', 'g#'], 2L)
  }
})

test_that('Stop and path folding work together', {
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')
  
  foldedPS <- getHumtab(foldStops(foldPaths(path))$Token, 'D')
  foldedSP <- getHumtab(foldPaths(foldStops(path))$Token, 'D')
  
  expect_equal(foldedPS[, table(Token, Token_Path)], 
               foldedSP[, table(Token, Token_Path)])
  expect_equal(foldedPS[, table(Token, Token_Stop)], 
               foldedSP[, table(Token, Token_Stop)])
  
})

# Reshaping vignette

test_that('Examples from Reshaping vignette work', {
  reshaping <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
  
  reshaped <- getHumtab(foldHumdrum(reshaping, 2, 1), 'D')
  
  expect_equal(dim(reshaped), c(6, 26))
  if (expect_true(all(c('Token', 'Result1') %in% colnames(reshaped)))) {
    expect_equal(reshaped[ , table(Token, Result1)]['4e', 'an'], 1L)
  }
})

