

test_that('Basic rhythm functions work', {
  reci <- c('4a', '8b', '8.c', '4.d', NA, 'x', '16e', '16.f', '3g', '6h', '0i', '00KK', '0.J', '1LL', '2.X')
  durs <- c( 0.25, 0.125, 0.1875, 0.375, NA, NA, 0.0625, 0.09375, 0.333333333333333, 0.166666666666667, 2, 4, 2, 1, 0.75 )
  
  
  expect_equal(recip(reci), c( '4', '8', '8.', '4.', NA, NA, '16', '16.', '3', '6', '0', '00', '0', '1', '2.' ))
  expect_equal(recip(reci, inPlace = TRUE), reci)
  
  expect_equal(duration(reci), durs)
  expect_equal(duration(reci, inPlace = TRUE), 
              c( '0.25a', '0.125b', '0.1875c', '0.375d', NA, 'x', '0.0625e', '0.09375f', '0.333333333333333g',
                 '0.166666666666667h', '2i', '4KK', '2.J', '1LL', '0.75X' ))
  
  expect_equal(recip(durs), recip(reci))
  expect_equal(duration(reci), durs)
  
})


test_that("ioi and untie work correctly", {
  
  # ioi()
  test <- c('4a', '4a', '4b', '4r', '8c', '16r', '8.c~',  '8G', '8A', '4.r')
  
  expect_equal(ioi(test), c( '4a', '4a', '2b', '.', '8.c', '.', '8.c~', '8G', NA, '.' ))
  expect_equal(ioi(test, inPlace = FALSE), c( '4', '4', '2', '.', '8.', '.', '8.', '8', NA, '.' ))
  expect_equal(ioi(test, finalOnset = TRUE), c( '4a', '4a', '2b', '.', '8.c', '.', '8.c~', '8G', '2A', '.' ))
  expect_equal(ioi(test, finalOnset = TRUE, inPlace = FALSE), c( '4', '4', '2', '.', '8.', '.', '8.', '8', '2', '.' ))
  
  
  expect_equal(ioi(c('4a','4r','4a','4.a','8r','4a'), groupby = list(c(1,1,1,2,2,2))),
               c("2a", ".", NA, "2a", ".", NA)) 
  # 
  
  mc <- readHumdrum(humdrumRroot, 'HumdrumData/RapFlow/.*rap')
  
  mc <- foldHumdrum(mc[[ , c(1, 6)]], 2, 1, newFieldNames = 'IPA')
  mc <- within(mc, IOI <- ioi(Token, onsets = IPA != 'R', finalOnset = TRUE))
  
  pairs <- with(mc$IOI,  data.frame(IOI, Token))
  expect_true(all(Reduce('>=', lapply(pairs, duration))))
  
  # untie()
  
  test <- c('4a', '[4a',']8a', NA, '8g','8G','[8a','_2a','4a]','4G')
  expect_equal(untie(test), c( '4a', '4.a', '.', NA, '8g', '8G', '2..a', '.', '.', '4G' ))
  expect_equal(untie(test, inPlace = FALSE), c( '4', '4.', '.', NA, '8', '8', '2..', '.', '.', '4' ))
  
  # both
  test <- c('4a', '[4a',']8a','8g','8r','[8a','_2a','4a]','4r')
  expect_equal(ioi(untie(test), finalOnset = TRUE), 
               untie(ioi(test, finalOnset = TRUE)))
})

test_that('Examples from rhythm man are correct', {
  
  expect_equal(untie(c('[4a', '4a]', '2g')), 
               c('2a', '.', '2g'))
  
  expect_equal(ioi(c('4.a','8r', '4.a','8r','2a', '2r')),
               c("2a", ".",  "2a", ".", NA, "." ))
  
  expect_equal(ioi(c('4.a','8r', '4.a','8r','2a', '2r'), finalOnset = TRUE),
               c("2a", ".",  "2a", ".", '1a', "." ))
  
  
})
