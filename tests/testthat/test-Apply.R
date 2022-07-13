# WorkingWithData vignette


# 
test_that('Grouping and where are consistent', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')


  chorales <- within(chorales, Token <- factor(Token))

  alltab <- with(chorales, table(Token))

  # grouping
  spines <- with(chorales, table(Token), by = Spine)
  expect_true(all(Reduce('+', spines) == alltab))

  # where
  spine1 <- with(chorales, table(Token), where = Spine == 1)
  expect_true(all(spines[[1]] == spine1))

})

test_that('Examples from Working With Data vignette work', {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  ## With
  alltab <- with(chorales, table(Token))
  
  if (expect_true(class(alltab) == 'table')) {
    expect_length(alltab, 379)
    expect_equal(unname(alltab['8f#J']), 26)
  }

  #
  kerntab <- with(chorales, 
                  kern(Token) |> 
                    table())
  
  if (expect_true(class(kerntab) == 'table')) {
    expect_length(kerntab, 56)
    expect_equal(unname(kerntab['dd#']), 4)
  }

  #
  barplot1 <- withVisible(with(chorales, 
                   kern(Token) |> 
                     table() |> 
                     sort() |> 
                     tail(n = 10) |> 
                     barplot()))
  
  if (expect_false(barplot1$visible)) {
    expect_equal(dim(barplot1$value), c(10, 1))
  }
  
  # where 
  spine1tab <- with(chorales, table(kern(Token)), where = Spine == 1)
  expect_equal(unname(spine1tab['F#']), 44)
  
  evenbartab <- with(chorales, 
       kern(Token) |> table() |> sort() |> tail(n = 10),
       where = Bar %% 2 == 0)
  
  expect_equal(unname(evenbartab['d']), 63)
  
  # elsedo
  
  within(chorales,
         Pitch <- pitch(Token, simple = TRUE),
         where = Spine == 1) -> chorales2
  
  expect_equal(with(chorales2, table(Pitch, Spine)) |> ncol(), 1)

  
  within(chorales,
         Pitch <- pitch(Token, simple = TRUE),
         where = Spine == 1,
         elsedo = Token)  -> chorales2
  
  spinetab <- with(chorales2, table(Pitch, Spine))
  expect_equal(ncol(spinetab), 4)
  expect_equal(spinetab['2a', 3], 7)
  expect_equal(spinetab['A', 1], 80)
  
  # group by
  spinetab <- with(chorales, 
       kern(Token) |> table() |> sort() |> tail(n = 10),
       by = Spine)
  
  if (expect_true(class(spinetab) == 'list'))  expect_length(spinetab, 4)
  if (expect_true(unique(sapply(spinetab, class)) == 'table')) expect_equal(unname(spinetab[[3]]['a']), 67)
  
  
  #
  na <- with(chorales, 
       semits(Token) |> mean(),
       by = Spine)
  
  expect_true(all(is.na(na)))
  
  notna <- with(chorales, 
       semits(Token) |> mean(na.rm = TRUE),
       by = Spine)
  
  expect_equal(notna, c(-9.6692308, -0.1143317, 5.6065041, 10.5672192))
  
  #
  hist <- withVisible(with(chorales, 
       semits(Token) |> hist(xlim = c(-24, 24), main = Instrument[1]),
       by = Spine) )
  
  expect_false(hist$visible)
  if(expect_true(class(hist$value[[1]]) == 'histogram')) {
    expect_equal(hist$value[[4]]$counts[2], 18)
  }
  
  #
  hist2 <- withVisible(with(chorales, 
       semits(Token) |> hist(xlim = c(-24, 24), main = paste(unique(Instrument), sep = ' and ')),
       by = Spine < 3))
  
  expect_false(hist2$visible)
  expect_true(class(hist2$value) == 'list')
  expect_length(hist2$value, 2)
  
  ## Within
  
  pitches <- with(chorales, pitch(Token))
  
  expect_length(pitches, 2433)
  expect_equal(pitches[567], 'G#4')
  
  #
  chorales <- within(chorales, Pitch <- pitch(Token))
  
  if (expect_true(class(chorales) == 'humdrumR')) {
    expect_equal(fields(chorales, 'D')$Name, c('Token', 'Pitch'))
    expect_equal(with(chorales, sum(Token == '4dd-' & Pitch == 'Db5')), 7)
    
  }
  
  
  ## Dofill
  
  within(chorales,
         Semits <- semits(Token)) -> chorales
  
  within(chorales, 
         dofill = BarBassNote <- min(Semits),
         by = list(File, Bar)) -> chorales
  
  expect_true(round(with(chorales, mean(Semits - BarBassNote)), 1) == 14.1)
})


