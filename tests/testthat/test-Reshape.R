# cleave ----

test_that('Spine cleaving works properly', {
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_spines.hum')
  
  
  cleaved <- getHumtab(cleave(spine, Spine = c(1, 2), Spine = c(1, 4), newFields =  'silbe'))
  
  expect_equal(dim(cleaved), c(24, 25))
  if (expect_true(all(c('Token', 'silbe', 'silbe1') %in% colnames(cleaved)))) {
    expect_equal(cleaved[silbe == 'the' & silbe1 == 'a', Token], 'c#')
  }
  
})

test_that('Spine cleaving works properly when stops are present, and vice versa', {
  
  spinesWithStops <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_stops.hum')
  
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

test_that("Exclusive (spine) cleaving works properly", {
  
  spine <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_spines.hum')
  
  # parallel (two silbe onto two kern)
  cleavedSilbe <- getHumtab(cleave(spine, c('kern', 'silbe')))
  
  expect_equal(dim(cleavedSilbe), c(24, 23))
  if (expect_true(all(c('Token', 'Silbe') %in% colnames(cleavedSilbe)))) {
    expect_equal(cleavedSilbe[ , table(Token, Silbe)]['a', 'These'], 2L)
  }
  
  
  # spreading: one harm onto twokern
  cleavedHarm <- getHumtab(cleave(spine, c('kern', 'harm')))
  
  expect_equal(dim(cleavedHarm), c(32, 23))
  if (expect_true(all(c('Token', 'Harm') %in% colnames(cleavedHarm)))) {
    expect_equal(cleavedHarm[ , table(Token, Harm)]['c#', 'AM'], 2L)
  }
  
  # two parallel moves AND ond spreading move
  cleavedDouble <- getHumtab(cleave(spine , c('kern', 'harm', 'silbe')))
  expect_equal(dim(cleavedDouble), c(16, 25))
  if (expect_true(all(c('Token', 'Harm', 'Silbe') %in% colnames(cleavedDouble)))) {
    expect_equal(cleavedDouble[ , table(Token, Harm, Silbe)]['c#', 'AM', 'These'],1L)
  }
  
  # all moved onto one, including a self move (kern -> kern) 
  cleavedAll <- getHumtab(cleave(spine , 1:5, newFields = c('Silbe1', 'Kern', 'Silbe2', 'Harm')))
  expect_equal(dim(cleavedAll), c(8, 29))
  if (expect_true(all(c('Token', 'Harm', 'Kern', 'Silbe1', 'Silbe2') %in% colnames(cleavedAll)))) {
    expect_equal(cleavedAll[ , paste(Token, Harm, Kern, Silbe1,Silbe2)[6]] , 'd DM f# lyr- cat')
  }
})

test_that('Path cleaving works properly', {
  
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_paths.hum')

  cleavedPath <- getHumtab(cleavePaths(path))
  
  
  expect_equal(dim(cleavedPath), c(28, 23))
  if (expect_true(all(c('Token', 'Path1') %in% colnames(cleavedPath)))) {
    expect_equal(cleavedPath[Stop == 1L, table(Token, Path1)]['c', 'a'], 2L)
  }
  
  
})

test_that('Stop cleaving works properly', {
  
  stop <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_stops.hum')
  
  cleavedStop <- getHumtab(cleaveStops(stop))
  
  
  expect_equal(dim(cleavedStop), c(18, 26))
  if (expect_true(all(c('Token', 'Stop2', 'Stop3') %in% colnames(cleavedStop)))) {
    expect_equal(cleavedStop[ , table(Token, Stop2, Stop3)]['b', 'd', 'g#'], 2L)
  }
})

test_that('Stop and path cleaving work together', {
  path <- readHumdrum(humdrumRroot, 'extdata/testfiles/cleave_paths.hum')
  
  cleavedPS <- getHumtab(humdrumR:::selectFields(cleaveStops(cleavePaths(path), field = 'Token'), 'Token'), 'D')
  cleavedSP <- getHumtab(humdrumR:::selectFields(cleavePaths(cleaveStops(path), field = 'Token'), 'Token'), 'D')
  
  expect_equal(cleavedPS[, table(Token, Path1)], 
               cleavedSP[, table(Token, Path1)])
  expect_equal(cleavedPS[, table(Token, Stop2)], 
               cleavedSP[, table(Token, Stop2)])
  
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

# rend ----

test_that("Test that rend words" , {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor00[5-7].*.krn')
  
  chorales <- within(chorales, Recip <- recip(Token))
  
  expect_equal(chorales |> rend(Token, Recip) |> ncol(), 8)
  
  
  chorales |> rend(Token, Recip) |> getHumtab() -> tokrec
  chorales |> rend(Recip, Token) |> getHumtab() -> rectok
  
  
  expect_equal(tokrec[Spine %in% c(1, 3, 5, 7), Token.Recip], getHumtab(chorales)[!is.na(Spine), Token])
  expect_equal(rectok[Spine %in% c(2, 4, 6, 8), Recip.Token], getHumtab(chorales)[!is.na(Spine), Token])
  
  expect_equal(tokrec[Spine %in% c(2, 4, 6, 8), Token.Recip], getHumtab(chorales)[!is.na(Spine), as.character(Recip)])
  expect_equal(rectok[Spine %in% c(1, 3, 5, 7), Recip.Token], getHumtab(chorales)[!is.na(Spine), as.character(Recip)])
  
  expect_equal(tokrec[Spine %in% c(1, 3, 5, 7), Token.Recip], rectok[Spine %in% c(2, 4, 6, 8), Recip.Token])
  
  ##
  
  chorales <-  chorales |> select(Token) |> within(Pitch <- pitch(Token))
  
  
  triplerend <- chorales |> rend(Token, Recip, Pitch) 
  
  expect_equal(triplerend |> ncol(), 12)
  
  expect_equal(getHumtab(triplerend)[Spine %in% c(1, 4, 7, 10), Token.Recip.Pitch], 
               getHumtab(chorales)[!is.na(Spine), Token])
  
  
  expect_equal(getHumtab(chorales |> rend(Token, Recip, Pitch, fieldName = 'Rended'))[Spine %in% c(2, 5, 8, 11), Rended], 
               getHumtab(chorales)[!is.na(Spine), as.character(Recip)])
})
