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
