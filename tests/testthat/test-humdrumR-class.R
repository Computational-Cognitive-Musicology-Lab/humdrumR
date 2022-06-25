context('')


# Fold ----

test_that('Spine folding works properly', {
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  
  folded <- getHumtab(foldHumdrum(spine, c(2, 4), 1, newFieldNames =  'silbe'))
  
  expect_equal(dim(folded), c(21, 23))
  if (expect_true(all(c('Token', 'silbe1', 'silbe2') %in% colnames(folded)))) {
    expect_equal(folded[silbe1 == 'the' & silbe2 == 'a', Token], 'c#')
  }
  
})


test_that("Exclusive folding works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  # parallel (two silbe onto two kern)
  foldedSilbe <- getHumtab(foldExclusive(spine, 'silbe', 'kern'))
  
  expect_equal(dim(foldedSilbe), c(21, 22))
  if (expect_true(all(c('Token', 'silbe') %in% colnames(foldedSilbe)))) {
    expect_equal(foldedSilbe[ , table(Token, silbe)]['a', 'These'], 1L)
  }
  
  # spreading: one harm onto twokern
  foldedHarm <- getHumtab(foldExclusive(spine, 'harm', 'kern'))
  
  expect_equal(dim(foldedHarm), c(28, 22))
  if (expect_true(all(c('Token', 'harm') %in% colnames(foldedHarm)))) {
    expect_equal(foldedHarm[ , table(Token, harm)]['c#', 'AM'], 2L)
  }
  
})

test_that('Path folding works properly', {
  
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')

  foldedPath <- getHumtab(foldPaths(path))
  
  
  expect_equal(dim(foldedPath), c(23, 22))
  if (expect_true(all(c('Token', 'Token_Path') %in% colnames(foldedPath)))) {
    expect_equal(foldedPath[ , table(Token, Token_Path)]['c', 'a'], 1L)
  }
})

test_that('Stop folding works properly', {
  
  stop <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  foldedStop <- getHumtab(foldStops(stop))
  
  
  expect_equal(dim(foldedStop), c(12, 23))
  if (expect_true(all(c('Token', 'Token_Stop1', 'Token_Stop2') %in% colnames(foldedStop)))) {
    expect_equal(foldedStop[ , table(Token, Token_Stop1, Token_Stop2)]['b', 'd', 'g#'], 1L)
  }
})