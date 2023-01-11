
# Shape ----

test_that('Shaping and coercion functions match', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  expect_equal(dim(chorales), dim(as.matrix(chorales)))
  
})

# Fold ----

test_that('Spine folding works properly', {
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  
  folded <- getHumtab(foldHumdrum(spine, c(2, 4), 1, newFieldNames =  'silbe'))
  
  expect_equal(dim(folded), c(24, 24))
  if (expect_true(all(c('Token', 'silbe1', 'silbe2') %in% colnames(folded)))) {
    expect_equal(folded[silbe1 == 'the' & silbe2 == 'a', Token], 'c#')
  }
  
})

test_that('Spine folding works properly when stops are present, and vice versa', {
  
  spinesWithStops <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  tab1 <- getHumtab(foldHumdrum(spinesWithStops, 2, 1), dataTypes = 'Dd')
  tab2 <- getHumtab(foldHumdrum(spinesWithStops, 1, 2), dataTypes = 'Dd')
   #the exclusive interpretations get changed by update_Exclusive, so only look at Dd
  
  if (expect_true(identical(dim(tab1), dim(tab2)))) {
    expect_true(all(tab1$Result1 == tab2$Token, na.rm = TRUE))
    expect_true(all(tab2$Result1 == tab1$Token))
  }
  
  #
  tab1 <- getHumtab(foldStops(foldHumdrum(spinesWithStops, 2, 1), fromField = 'Token'), dataTypes = 'Dd')
  tab2 <- getHumtab(foldStops(foldHumdrum(spinesWithStops, 1, 2), fromField = 'Result1'), dataTypes = 'Dd')
  if (expect_true(identical(dim(tab1), dim(tab2)))) {
    expect_true(all(tab1$Token == tab2$Result1))
    expect_true(all(tab1$Result1 == tab2$Token))
    expect_true(all(tab1$Token_Stop3 == tab2$Result1_Stop3, na.rm = TRUE))
  }
  
})

test_that("Exclusive (spine) folding works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  # parallel (two silbe onto two kern)
  foldedSilbe <- getHumtab(foldExclusive(spine, 'silbe', 'kern'))
  
  expect_equal(dim(foldedSilbe), c(24, 23))
  if (expect_true(all(c('Token', 'Silbe') %in% colnames(foldedSilbe)))) {
    expect_equal(foldedSilbe[ , table(Token, Silbe)]['a', 'These'], 2L)
  }
  
  
  # spreading: one harm onto twokern
  foldedHarm <- getHumtab(foldExclusive(spine, 'harm', 'kern'))
  
  expect_equal(dim(foldedHarm), c(32, 23))
  if (expect_true(all(c('Token', 'Harm') %in% colnames(foldedHarm)))) {
    expect_equal(foldedHarm[ , table(Token, Harm)]['c#', 'AM'], 2L)
  }
  
  # two parallel moves AND ond spreading move
  foldedDouble <- getHumtab(foldExclusive(spine ,c('harm', 'silbe'), 'kern'))
  expect_equal(dim(foldedDouble), c(16, 24))
  if (expect_true(all(c('Token', 'Harm', 'Silbe') %in% colnames(foldedDouble)))) {
    expect_equal(foldedDouble[ , table(Token, Harm, Silbe)]['c#', 'AM', 'These'],1L)
  }
  
  # all moved onto one, including a self move (kern -> kern) 
  foldedAll <- getHumtab(foldExclusive(spine ,c('harm', 'silbe', 'kern'), 'kern'))
  expect_equal(dim(foldedAll), c(8, 26))
  if (expect_true(all(c('Token', 'Harm', 'Kern', 'Silbe1', 'Silbe2') %in% colnames(foldedAll)))) {
    expect_equal(foldedAll[ , paste(Token, Harm, Kern, Silbe1,Silbe2)[6]] , 'd DM f# lyr- cat')
  }
})

test_that('Path folding works properly', {
  
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')

  foldedPath <- getHumtab(foldPaths(path))
  
  
  expect_equal(dim(foldedPath), c(28, 23))
  if (expect_true(all(c('Token', 'Token_Path1') %in% colnames(foldedPath)))) {
    expect_equal(foldedPath[Stop == 1L, table(Token, Token_Path1)]['c', 'a'], 2L)
  }
  
  
  
})

test_that('Stop folding works properly', {
  
  stop <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  foldedStop <- getHumtab(foldStops(stop))
  
  
  expect_equal(dim(foldedStop), c(18, 25))
  if (expect_true(all(c('Token', 'Token_Stop2', 'Token_Stop3') %in% colnames(foldedStop)))) {
    expect_equal(foldedStop[ , table(Token, Token_Stop2, Token_Stop3)]['b', 'd', 'g#'], 2L)
  }
})

test_that('Stop and path folding work together', {
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')
  
  foldedPS <- getHumtab(foldStops(foldPaths(path), fromField = 'Token')$Token, 'D')
  foldedSP <- getHumtab(foldPaths(foldStops(path), fromField = 'Token')$Token, 'D')
  
  expect_equal(foldedPS[, table(Token, Token_Path1)], 
               foldedSP[, table(Token, Token_Path1)])
  expect_equal(foldedPS[, table(Token, Token_Stop2)], 
               foldedSP[, table(Token, Token_Stop2)])
  
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



test_that("Exclusive (spine) folding works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  # parallel (two silbe onto two kern)
  foldedSilbe <- getHumtab(foldExclusive(spine, 'silbe', 'kern'))
  
  expect_equal(dim(foldedSilbe), c(24, 23))
  if (expect_true(all(c('Token', 'Silbe') %in% colnames(foldedSilbe)))) {
    expect_equal(foldedSilbe[ , table(Token, Silbe)]['a', 'These'], 2L)
  }
  
  
  # spreading: one harm onto twokern
  foldedHarm <- getHumtab(foldExclusive(spine, 'harm', 'kern'))
  
  expect_equal(dim(foldedHarm), c(32, 23))
  if (expect_true(all(c('Token', 'Harm') %in% colnames(foldedHarm)))) {
    expect_equal(foldedHarm[ , table(Token, Harm)]['c#', 'AM'], 2L)
  }
  
  # two parallel moves AND ond spreading move
  foldedDouble <- getHumtab(foldExclusive(spine ,c('harm', 'silbe'), 'kern'))
  expect_equal(dim(foldedDouble), c(16, 24))
  if (expect_true(all(c('Token', 'Harm', 'Silbe') %in% colnames(foldedDouble)))) {
    expect_equal(foldedDouble[ , table(Token, Harm, Silbe)]['c#', 'AM', 'These'],1L)
  }
  
  # all moved onto one, including a self move (kern -> kern) 
  foldedAll <- getHumtab(foldExclusive(spine ,c('harm', 'silbe', 'kern'), 'kern'))
  expect_equal(dim(foldedAll), c(8, 26))
  if (expect_true(all(c('Token', 'Harm', 'Kern', 'Silbe1', 'Silbe2') %in% colnames(foldedAll)))) {
    expect_equal(foldedAll[ , paste(Token, Harm, Kern, Silbe1,Silbe2)[6]] , 'd DM f# lyr- cat')
  }
})


# Update Exclusive

test_that("Exclusive is updated by update_Exclusive", {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  chorales <- within(chorales,
                     Semits <- semits(Token),
                     Pitch <- pitch(Token),
                     Nchar <- nchar(Token))
  
  expect_equal(with(chorales$Semits, Token[Token %~% '\\*\\*'], dataTypes = 'I')[1], '**semits')
  expect_equal(with(chorales$Nchar, Token[Token %~% '\\*\\*'], dataTypes = 'I')[1], '**kern')
  # (semits and Nchar update Token because they are not character)
  expect_equal(with(chorales$Pitch, .[Token %~% '\\*\\*'], dataTypes = 'I')[1], '**pitch')
  expect_equal(with(chorales$Token, .[Token %~% '\\*\\*'], dataTypes = 'I')[1], '**kern')
  
  })



