# cleave ----

test_that('Spine folding works properly', {
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  
  folded <- getHumtab(cleave(spine, Spine = c(1, 2), Spine = c(1, 4), newFields =  'silbe'))
  
  expect_equal(dim(folded), c(24, 25))
  if (expect_true(all(c('Token', 'silbe', 'silbe1') %in% colnames(folded)))) {
    expect_equal(folded[silbe == 'the' & silbe1 == 'a', Token], 'c#')
  }
  
})

test_that('Spine folding works properly when stops are present, and vice versa', {
  
  spinesWithStops <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  tab1 <- getHumtab(cleave(spinesWithStops, 2:1))
  tab2 <- getHumtab(cleave(spinesWithStops, 1:2))
   #the exclusive interpretations get changed by update_Exclusive, so only look at Dd
  
  if (expect_true(identical(dim(tab1), dim(tab2)))) {
    expect_true(all(tab1$Spine1 == tab2$Token, na.rm = TRUE))
    expect_true(all(tab2$Spine1 == tab1$Token))
  }
  
  #
  tab1 <- getHumtab(cleaveStops(cleave(spinesWithStops, 2:1), field = 'Spine1'), dataTypes = 'Dd')
  tab2 <- getHumtab(cleaveStops(cleave(spinesWithStops, 1:2), field = 'Token'), dataTypes = 'Dd')
  if (expect_true(identical(dim(tab1), dim(tab2)))) {
    expect_true(all(tab1$Token == tab2$Result1))
    expect_true(all(tab1$Result1 == tab2$Token))
    expect_true(all(tab1$Token_Stop3 == tab2$Result1_Stop3, na.rm = TRUE))
  }
  
})

test_that("Exclusive (spine) folding works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_spines.hum')
  
  # parallel (two silbe onto two kern)
  foldedSilbe <- getHumtab(cleave(spine, c('kern', 'silbe')))
  
  expect_equal(dim(foldedSilbe), c(24, 23))
  if (expect_true(all(c('Token', 'Silbe') %in% colnames(foldedSilbe)))) {
    expect_equal(foldedSilbe[ , table(Token, Silbe)]['a', 'These'], 2L)
  }
  
  
  # spreading: one harm onto twokern
  foldedHarm <- getHumtab(cleave(spine, c('kern', 'harm')))
  
  expect_equal(dim(foldedHarm), c(32, 23))
  if (expect_true(all(c('Token', 'Harm') %in% colnames(foldedHarm)))) {
    expect_equal(foldedHarm[ , table(Token, Harm)]['c#', 'AM'], 2L)
  }
  
  # two parallel moves AND ond spreading move
  foldedDouble <- getHumtab(cleave(spine , c('kern', 'harm', 'silbe')))
  expect_equal(dim(foldedDouble), c(16, 25))
  if (expect_true(all(c('Token', 'Harm', 'Silbe') %in% colnames(foldedDouble)))) {
    expect_equal(foldedDouble[ , table(Token, Harm, Silbe)]['c#', 'AM', 'These'],1L)
  }
  
  # all moved onto one, including a self move (kern -> kern) 
  foldedAll <- getHumtab(cleave(spine , 1:5, newFields = c('Silbe1', 'Kern', 'Silbe2', 'Harm')))
  expect_equal(dim(foldedAll), c(8, 29))
  if (expect_true(all(c('Token', 'Harm', 'Kern', 'Silbe1', 'Silbe2') %in% colnames(foldedAll)))) {
    expect_equal(foldedAll[ , paste(Token, Harm, Kern, Silbe1,Silbe2)[6]] , 'd DM f# lyr- cat')
  }
})

test_that('Path folding works properly', {
  
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')

  foldedPath <- getHumtab(cleavePaths(path))
  
  
  expect_equal(dim(foldedPath), c(28, 23))
  if (expect_true(all(c('Token', 'Path1') %in% colnames(foldedPath)))) {
    expect_equal(foldedPath[Stop == 1L, table(Token, Path1)]['c', 'a'], 2L)
  }
  
  
})

test_that('Stop folding works properly', {
  
  stop <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_stops.hum')
  
  foldedStop <- getHumtab(cleaveStops(stop))
  
  
  expect_equal(dim(foldedStop), c(18, 26))
  if (expect_true(all(c('Token', 'Stop2', 'Stop3') %in% colnames(foldedStop)))) {
    expect_equal(foldedStop[ , table(Token, Stop2, Stop3)]['b', 'd', 'g#'], 2L)
  }
})

test_that('Stop and path folding work together', {
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/fold_paths.hum')
  
  foldedPS <- getHumtab(humdrumR:::selectFields(cleaveStops(cleavePaths(path), field = 'Token'), 'Token'), 'D')
  foldedSP <- getHumtab(humdrumR:::selectFields(cleavePaths(cleaveStops(path), field = 'Token'), 'Token'), 'D')
  
  expect_equal(foldedPS[, table(Token, Path1)], 
               foldedSP[, table(Token, Path1)])
  expect_equal(foldedPS[, table(Token, Stop2)], 
               foldedSP[, table(Token, Stop2)])
  
})

# Reshaping vignette

test_that('Examples from Reshaping vignette work', {
  reshaping <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
  
  reshaped <- getHumtab(cleave(reshaping, 1:2), 'D')
  
  expect_equal(dim(reshaped), c(24, 26))
  if (expect_true(all(c('Token', 'Spine2') %in% colnames(reshaped)))) {
    expect_equal(reshaped[ , table(Token, Spine2)]['4e', 'an'], 1L)
  }
})





# Update Exclusive

test_that("Exclusive is updated by update_Exclusive", {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor00[5-7].*.krn')
  
  chorales <- within(chorales,
                     Semits <- semits(Token),
                     Pitch <- pitch(Token),
                     Nchar <- nchar(Token))
  
  # Actual Exclusive field is updated (for analysis)
  expect_equal(chorales |> select(Token) |> pull(Exclusive) |> unique(), 'kern')
  expect_equal(chorales |> select(Nchar) |> pull(Exclusive) |> unique(), 'kern')
  expect_equal(chorales |> select(Semits) |> pull(Exclusive) |> unique(), 'semits')
  expect_equal(chorales |> select(Pitch) |> pull(Exclusive) |> unique(), 'pitch')
  
  # Printout of ** tokens are updated (for printing)
  expect_equal(chorales |> pullPrintable('Token', 'ED') |> pull(1) |> index(1), '**kern')
  expect_equal(chorales |> pullPrintable('Nchar', 'ED') |> pull(1) |> index(1), '**kern')
  expect_equal(chorales |> pullPrintable('Semits', 'ED') |> pull(1) |> index(1), '**semits')
  expect_equal(chorales |> pullPrintable('Pitch', 'ED') |> pull(1) |> index(1), '**pitch')
  
  })



