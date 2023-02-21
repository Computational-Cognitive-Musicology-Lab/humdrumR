# WorkingWithData vignette


# 
test_that('Grouping and subset are consistent', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')


  chorales <- within(chorales, TokenF <- factor(Token))

  alltab <- with(chorales, table(TokenF))

  # grouping
  spines <- with(chorales, table(TokenF), by = Spine)
  expect_true(all(Reduce('+', spines) == alltab))

  # subset
  spine1 <- with(chorales, table(TokenF), subset = Spine == 1)
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
    expect_length(kerntab, 72)
    expect_equal(unname(kerntab['dd#']), 4)
    expect_equal(unname(kerntab['DD#']), 0)
  }

  #
  barplot1 <- withVisible(with(chorales, 
                   kern(Token) |> 
                     table() |> 
                     sort() |> 
                     tail(n = 10) |> 
                     barplot()))
  
  if (expect_false(barplot1$visible)) {
    expect_equal(length(barplot1$value), c(10))
  }
  
  # subset 
  spine1tab <- with(chorales, table(kern(Token)), subset = Spine == 1)
  expect_equal(unname(spine1tab['F#']), 44)
  
  evenbartab <- with(chorales, 
       kern(Token) |> table() |> sort() |> tail(n = 10),
       subset = Bar %% 2 == 0)
  
  expect_equal(unname(evenbartab['d']), 63)
  
  # elsedo
  
  within(chorales,
         Pitch <- pitch(Token, simple = TRUE),
         subset = Spine == 1) -> chorales2
  
  expect_equal(with(chorales2, table(Pitch, Spine)) |> ncol(), 1)

  
  within(chorales,
         Pitch <- pitch(Token, simple = TRUE),
         subset = Spine == 1,
         complement = Token)  -> chorales2
  
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
  
  expect_equal(round(notna, 2), setNames(c(-9.67, -0.11, 5.61, 10.57), 1:4))
  
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
  expect_equalchar(pitches[567], 'G#4')
  
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
         fill = BarBassNote <- min(Semits),
         by = list(File, Bar)) -> chorales
  
  expect_true(round(with(chorales, mean(Semits - BarBassNote)), 1) == 14.1)
})



test_that("Examples from withinHumdrum docs work", {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  chorales <- within(chorales, Semits <- semits(Token))
  humtab <- getHumtab(chorales)[!is.na(Spine) & !is.na(Semits)]
  grandMean <- humtab[ , mean(Semits)]
  
  #
  Count1 <- sort(humtab[ , list(sum(Semits > grandMean), Spine), by = Spine][ , setNames(V1, paste0(Spine, ';', TRUE))])
  Count2 <- sort(with(chorales, length(Semits), subset = Semits > mean(Semits),  by = Spine))
  
  expect_equal(Count1, Count2)
  
  #
  Count3 <- sort(humtab[, sum(Semits > mean(Semits)), by = Spine][ , setNames(V1, paste0(TRUE, ';', Spine))])
  Count4 <- sort(with(chorales, length(Semits), by = Spine, subset = Semits > mean(Semits)))
  
  expect_equal(Count3, Count4)
  
  expect_false(any(Count1 == Count3))
  expect_false(any(Count2 == Count4))
  
  
  #
  expect_identical(with(chorales, lag(Token, 1)), with(chorales, Token[lag = 1]))
  expect_identical(with(chorales, table(lag(Token, 1), lag(Token, 2))), 
                   with(chorales, table(Token[lag = 1:2])))
  
})

test_that("Assignment and multiple do expressions work correctly in with.humdrumR", {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  # factors

    
  # with, single argument
  ### drop = TRUE
  A <- with(chorales, nchar(Token))
  B <- with(chorales, X <- nchar(Token))
  
  expect_null(dim(A))
  expect_identical(A, B)
  
  ### drop = FALSE
  
  A <- with(chorales, nchar(Token), drop = FALSE)
  B <- with(chorales, X <- nchar(Token), drop = FALSE)
  
  expect_equal(colnames(A), 'Result1')
  expect_equal(colnames(B), 'X')
  
  # with, two arguments
  ### drop = TRUE
  A <- with(chorales, nchar(Token)^2)
  B <- with(chorales, nchar(Token), .^2)
  C <- with(chorales, substr(Token, 0,1), nchar(Token)^2)
  D <- with(chorales, nchar(Token), .^2)
  E <- with(chorales, NChar <- nchar(Token), NChar^2)
  
  expect_null(dim(A))
  expect_identical(A, B)
  expect_identical(B, C)
  expect_identical(C, D)
  expect_identical(D, E)
  
  ### drop = FALSE
  
  A <- with(chorales, drop = FALSE, nchar(Token)^2)
  B <- with(chorales, drop = FALSE, nchar(Token), .^2)
  C <- with(chorales, drop = FALSE, substr(Token, 0,1), nchar(Token)^2)
  D <- with(chorales, drop = FALSE, nchar(Token), .^2)
  
  expect_equal(colnames(A), 'Result1')
  expect_identical(A, B)
  expect_identical(B, C)
  expect_identical(C, D)
  
  A <- with(chorales, drop = FALSE, NChar <- nchar(Token), NChar^2)
  B <- with(chorales, drop = FALSE, NChar <- nchar(Token), Squared <- NChar^2)
  expect_identical(colnames(A), c('NChar', 'Result1'))
  expect_identical(colnames(B), c('NChar', 'Squared'))

  expect_identical(A$NChar, B$NChar)
  expect_identical(A$Result1, B$Squared)
  
  

  
  
  ## with fx
  ### drop = TRUE
  A <- with(chorales, nchar(Token), side = .^2)
  B <- with(chorales, nchar(Token))

  expect_identical(A, B)
  
  A <- with(chorales, nchar(Token), side = .^2, .^3)
  B <- with(chorales, nchar(Token), .^3)
  C <- with(chorales, nchar(Token)^3)
  expect_identical(A, B)
  expect_identical(B, C)
  
   #### # all fx
  A <- with(chorales, side = nchar(Token))
  B <- with(chorales, side = nchar(Token), side = Token == '4ee-')
  expect_null(A)
  expect_null(B)
  
  ### drop = FALSE
  A <- with(chorales, drop = FALSE, nchar(Token), side = .^2)
  B <- with(chorales, drop = FALSE, nchar(Token))
  
  expect_identical(A, B)
  
  A <- with(chorales, drop = FALSE, nchar(Token), side = .^2, .^3)
  B <- with(chorales, drop = FALSE, nchar(Token), .^3)
  C <- with(chorales, drop = FALSE, nchar(Token)^3)
  expect_equal(ncol(A), 1)
  expect_identical(A, B)
  expect_identical(B, C)
  
  A <- with(chorales, drop = FALSE, Nchar <- nchar(Token), side = .^2, .^3)
  B <- with(chorales, drop = FALSE, Nchar <- nchar(Token), side = .^2, Nchar^3)
  expect_equal(ncol(A), 2)
  expect_identical(A, B)
  
  #### # all fx
  A <- with(chorales, drop = FALSE, side = nchar(Token))
  B <- with(chorales, drop = FALSE, side = nchar(Token), side = Token == '4ee-')
  expect_identical(A, B)
  expect_equal(nrow(A), 0L)
  
  
})

  
