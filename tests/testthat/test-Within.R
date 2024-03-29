# WorkingWithData vignette


# 
test_that('Grouping and subset are consistent', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')


  chorales <- within(chorales, TokenF <- factor(Token))

  alltab <- with(chorales, table(TokenF))

  # grouping
  spines <- with(chorales, table(TokenF), .by = 'Spine')
  expect_true(all(Reduce('+', spines) == alltab))

  # subset
  spine1 <- with(subset(chorales, Spine == 1), table(TokenF))
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
                    table(useNA = 'always'))
  
  if (expect_true(class(kerntab) == 'table')) {
    expect_length(kerntab, 73)
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
  spine1tab <- with(chorales |> subset(Spine == 1L), table(kern(Token)))
  expect_equal(unname(spine1tab['F#']), 44)
  
  evenbartab <- with(chorales |> subset(Bar %% 2 == 0), 
       kern(Token) |> table() |> sort() |> tail(n = 10))
  
  expect_equal(unname(evenbartab['d']), 63)
  
  
  # group by
  spinetab <- with(chorales, 
       kern(Token) |> table() |> sort() |> tail(n = 10),
       .by = 'Spine')
  
  if (expect_true(class(spinetab) == 'list'))  expect_length(spinetab, 4)
  if (expect_true(unique(sapply(spinetab, class)) == 'table')) expect_equal(unname(spinetab[[3]]['a']), 67)
  
  
  spinetab <- with(chorales |> group_by(Spine), 
                   kern(Token) |> table() |> sort() |> tail(n = 10))
  
  if (expect_true(class(spinetab) == 'list'))  expect_length(spinetab, 4)
  if (expect_true(unique(sapply(spinetab, class)) == 'table')) expect_equal(unname(spinetab[[3]]['a']), 67)
  
  
  #
  na <- with(chorales, 
             semits(Token) |> mean(),
             .by = 'Spine')
  
  expect_true(all(is.na(na)))
  
  notna <- with(chorales |> group_by(Spine), semits(Token) |> mean(na.rm = TRUE))
  
  expect_equal(round(notna, 2), setNames(c(-9.67, -0.11, 5.61, 10.57), paste0('Spine', 1:4)))
  
  #
  hist <- withVisible(with(chorales, 
                           semits(Token) |> hist(xlim = c(-24, 24), main = Instrument[1]),
                           .by = 'Spine') )
  
  expect_false(hist$visible)
  if(expect_true(class(hist$value[[1]]) == 'histogram')) {
    expect_equal(hist$value[[4]]$counts[2], 18)
  }
  
  #
  hist2 <- withVisible(with(chorales |> group_by(Spine < 3), 
                            semits(Token) |> hist(xlim = c(-24, 24), main = paste(unique(Instrument), sep = ' and '))))
  
  expect_false(hist2$visible)
  expect_true(class(hist2$value) == 'list')
  expect_length(hist2$value, 2)
  
  ## Within
  
  pitches <- with(chorales, pitch(Token))
  
  expect_length(pitches, 2433)
  expect_equal(as.character(pitches[567]), 'G#4')
  
  #
  chorales <- within(chorales, Pitch <- pitch(Token))
  
  if (expect_true(class(chorales) == 'humdrumR')) {
    expect_equal(fields(chorales, 'D')$Name, c('Pitch', 'Token'))
    expect_equal(with(chorales, sum(Token == '4dd-' & Pitch == 'Db5')), 7)
    
  }
  
  
  ## recycling
  
  within(chorales |> select(Token), Semits <- semits(Token)) -> chorales
  
  chorales <- within(chorales |> group_by(File, Bar), recycle = 'ifscalar',
         BarBassNote <- min(Semits))  |> ungroup()
  
  expect_true(round(with(chorales, mean(Semits - BarBassNote)), 1) == 14.1)
})



test_that("Examples from withinHumdrum docs work", {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  chorales <- within(chorales, Semits <- semits(Token))
  humtab <- getHumtab(chorales)[!is.na(Spine) & !is.na(Semits)]
  grandMean <- humtab[ , mean(Semits)]
  
  #
  Count1 <- sort(humtab[ , list(sum(Semits > grandMean), Spine), by = Spine]$V1)
  Count2 <- sort(with(chorales |> subset(Semits > mean(Semits)), length(Semits), .by = 'Spine'))
  
  expect_equal(Count1, unname(Count2))
  
  #
  Count3 <- sort(humtab[, sum(Semits > mean(Semits)), by = Spine]$V1)
  Count4 <- sort(with(chorales |> group_by(Spine) |> subset(Semits > mean(Semits)), length(Semits)))
  
  expect_equal(Count3, unname(Count4))
  
  expect_false(any(Count1 == Count3))
  expect_false(any(Count2 == Count4))
  
  
  #
  expect_identical(with(chorales, lag(Token, 1)), with(chorales, Token[lag = 1]))
  expect_identical(with(chorales, table(lag(Token, 1), lag(Token, 2))), 
                   with(chorales, table(Token[lag = 1:2])))
  
  
  expect_identical(with(chorales, 
                        Kern <- kern(Token),
                        Kern2 <- paste0(Kern, nchar(Kern))),
                   with(chorales,
                        Kern2 <- paste0(kern(Token), nchar(kern(Token)))))
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
  
  expect_equal(colnames(A), 'nchar(Token)')
  expect_equal(colnames(B), 'X')
  
  # with, two arguments
  ### drop = TRUE
  A <- with(chorales, nchar(Token)^2)
  B <- with(chorales, NChar <- nchar(Token), NChar^2)
  C <- with(chorales, substr(Token, 0,1), nchar(Token)^2)
  
  expect_null(dim(A))
  expect_identical(A, B)
  expect_identical(B, C)
  
  ### drop = FALSE
  
  A <- with(chorales, drop = FALSE, nchar(Token)^2)
  B <- with(chorales, drop = FALSE, nchar(Token), nchar(Token)^2)
  C <- with(chorales, drop = FALSE, substr(Token, 0,1), nchar(Token)^2)
  D <- with(chorales, drop = FALSE, Nchar <- nchar(Token), Nchar^2)
  
  expect_equal(colnames(A), 'nchar(Token)^2')
  expect_equal(colnames(B), c('nchar(Token)', 'nchar(Token)^2'))
  expect_equal(colnames(C), c('substr(Token, 0, 1)', 'nchar(Token)^2'))
  expect_equal(colnames(D), c('Nchar', 'Nchar^2'))
  expect_identical(A[ , 1], B[ , 2])
  expect_identical(A[ , 1], C[ , 2])
  expect_identical(C[[2]], D[[2]])
  
  A <- with(chorales, drop = FALSE, NChar <- nchar(Token), NChar^2)
  B <- with(chorales, drop = FALSE, NChar <- nchar(Token), Squared <- NChar^2)
  expect_identical(colnames(A), c('NChar', 'NChar^2'))
  expect_identical(colnames(B), c('NChar', 'Squared'))

  expect_identical(A$NChar, B$NChar)
  expect_identical(A$`NChar^2`, B$Squared)
  
  
  expect_equal(names(with(chorales, mean(nchar(Token)), .by = 'Spine')), paste0('Spine', 1:4))
  
})

  

test_that("recycle are works correctly", {
  chor <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor00[12].krn')
  chorg <- group_by(chor, Spine)
  
  expect_equal(length(with(chorg, mean(nchar(Token)))), 4)
  
  fullLength <- chor |> with(length(Token)) 
  
  #### First for with()
  
  # 'yes'
  expect_equal(fullLength, length(with(chorg, mean(nchar(Token)), recycle = 'yes')))
  
  # 'no'
  expect_equal(length(with(chorg, mean(nchar(Token)), recycle = 'no')), 4)
  expect_equal(length(with(chorg, range(nchar(Token)), recycle = 'no')), 8)
  
  # 'pad'
  padded <-  chorg |> with(mean(nchar(Token)), recycle='pad') 
  expect_equal(fullLength, length(padded))
  expect_equal(sum(is.na(padded)), fullLength - 4L) # four spines
  
  padded2 <-  chorg |> with(range(nchar(Token)), recycle='pad') 
  expect_equal(fullLength, length(padded2))
  expect_equal(sum(is.na(padded2)), fullLength - 8L) # four spines times 2
  
  # 'ifscalar'
  expect_equal(with(chorg, mean(nchar(Token)), recycle = 'yes'),
               with(chorg, mean(nchar(Token)), recycle = 'ifscalar'))
  expect_error(with(chorg, range(nchar(Token)), recycle = 'ifscalar'))
  
  # 'ifeven'
  expect_equal(with(chor, range(nchar(Token)), recycle = 'yes'),
               with(chor, range(nchar(Token)), recycle = 'ifeven'))
  expect_error(with(chor, c(range(nchar(Token)), mean(nchar(Token))), recycle = 'ifeven')) # 3 doesn't divide
  
  # 'never' 
  expect_error(with(chor, mean(nchar(Token)), recycle = 'never'))
  expect_equal(length(with(chor, nchar(Token), recycle = 'never')), fullLength)
  
  # 'summarize'
  expect_error(with(chor, nchar(Token), recycle = 'summarize'))
  expect_equal(with(chorg, mean(nchar(Token)), recycle = 'no'),
               with(chorg, mean(nchar(Token)), recycle = 'summarize'))
  
  ### within()
  
  # 'yes'
  expect_equal(fullLength, 
               within(chorg, mean(nchar(Token)), recycle = 'yes') |> pull() |> length())
  
  # 'no'
  # expect_error(within(chorg, mean(nchar(Token)), recycle = 'no'))
  
  # 'pad'
  expect_equal(within(chorg, mean(nchar(Token)), recycle = 'pad') |> pull() |> length(),
               4) # 4 spines
  
  # 'ifscalar'
  expect_identical(within(chorg, mean(nchar(Token)), recycle = 'yes'),
                   within(chorg, mean(nchar(Token)), recycle = 'ifscalar'))
  expect_error(within(chorg, range(nchar(Token)), recycle = 'ifscalar'))
  
  # 'ifeven'
  expect_identical(within(chor, range(nchar(Token)), recycle = 'yes'),
                   within(chor, range(nchar(Token)), recycle = 'ifeven'))
  expect_error(within(chor, c(range(nchar(Token)), mean(nchar(Token))), recycle = 'ifeven')) # 3 doesn't divide
  
  # 'never' 
  expect_error(within(chor, mean(nchar(Token)), recycle = 'never'))
  expect_equal(within(chor, nchar(Token), recycle = 'never') |> pull() |> length(), fullLength)
  
  # 'summarize'
  expect_error(within(chor, nchar(Token), recycle = 'summarize'))
  
  
               
})


test_that('lag/lead sugar works', {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  chorales |> mutate(Kern = kern(Token, simple = TRUE)) -> chorales
  
  chorales |> with(table(Kern[lag = 0:1])) -> lag
  chorales |> with(table(Kern[lead = 0:1])) -> lead
  dimnames(lag) <- dimnames(lead) <- NULL
  expect_equal(lag, t(lead))
  
})


test_that("Quosures thread environment variables correctly", {
  chor <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')[[ , 1]]
  chor |> mutate(N = nchar(Token)) -> chor
  
  # inside
  expect_equal(with(chor, 
                    {
                      a <- 'mean'
                      paste(median(N), a)
                    }), 
               '3 mean')
  
  # outside
  a <- 'mean'
  expect_equal(with(chor, paste(median(N), a)),  '3 mean')
  
  # inside trumps outside
  a <- 'mean'
  expect_equal(with(chor, 
                    {
                      a <- 'median'
                      paste(median(N), a)
                    }), 
               '3 median')
  
  # for loop
  result <- c()
  for (i in 1:5) result <- c(result, with(chor, median(N) * i)) 
  expect_equal(result, 1:5 * 3)
  
  # sapply
  expect_equal(sapply(1:5, \(i) with(chor, median(N) * i)), 1:5 * 3)
  
  # function argument
  
  (function(x) {
    paste0(with(chor, paste(median(N), x)))
  }) -> f
  expect_equal(f('median'), '3 median')
  
  # lag
  i <- 1
  expect_true(all(with(chor, table(N[lag = i])) == c(17, 34, 11)))
  
  i <- 1:3
  outsidelag <- with(chor, table(N[lag = i]))
  expect_equal(dim(outsidelag), c(3, 3, 3))
  
  expect_identical(outsidelag, with(chor, table(N[lag = 1:3])))
  expect_identical(outsidelag,
                   with(chor, {
                     i <- 1:3
                     table(N[lag = i])
                   }))
  
  
  
})

test_that("Quoted expressions and formulae work", {
  chor <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
  
  # NULL or no args
  expect_error(chor |> with())
  expect_error(chor |> with(NULL))
  
  #
  result1 <- with(chor, mean(nchar(Token)) + 1)
  result2 <- with(chor, ~mean(nchar(Token)) + 1)
  result3 <- with(chor, quote(mean(nchar(Token)) + 1))
  
  form <- ~mean(nchar(Token)) + 1
  quoted <- quote(mean(nchar(Token)) + 1)
  
  result4 <- with(chor, form)
  result5 <- with(chor, quoted)
  
  expect_identical(result1, result2)
  expect_identical(result2, result3)
  expect_identical(result3, result4)
  expect_identical(result4, result5)
  
  
})
