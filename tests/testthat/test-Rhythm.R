expect_equalchar <- function(e1, e2) expect_equal(as.character(e1), as.character(e2))
expect_equalnum <- function(e1, e2) expect_equal(as.numeric(e1), as.numeric(e2))

test_that('Basic rhythm functions work', {
  reci <- c('4a', '8b', '8.c', '4.d', NA, 'x', '16e', '16.f', '3g', '6h', '0i', '00KK', '0.J', '1LL', '2.X')
  durs <- c( 0.25, 0.125, 0.1875, 0.375, NA, NA, 0.0625, 0.09375, 0.333333333333333, 0.166666666666667, 2, 4, 3, 1, 0.75 )
  

  
  expect_equalchar(recip(reci), c( '4', '8', '8.', '4.', NA, NA, '16', '16.', '3', '6', '0', '00', '0.', '1', '2.' ))
  expect_equal(recip(reci, inPlace = TRUE), reci)

  
  expect_equalchar(recip(durs), recip(reci))

  
  ## scale argument
  expect_equalnum(duration(reci, scale = 2), duration(reci) * 2)
  expect_equalnum(duration(reci, unit = 4), duration(reci) / 4)
  expect_equalnum(duration(reci, unit = 4, scale = 4), duration(reci))
  
})


test_that("ioi and untie work correctly", {
  
  # ioi()
  test <- c('4a', '4a', '4b', '4r', '8c', '16r', '8.c~',  '8G', '8A', '4.r')
  
  expect_equal(ioi(test), c( '4a', '4a', '2b', '.', '8.c', '.', '8.c~', '8G', NA, '.' ))
  expect_equalchar(ioi(test, inPlace = FALSE), c( '4', '4', '2', NA, '8.', NA, '8.', '8', NA, NA ))
  expect_equal(ioi(test, finalOnset = TRUE), c( '4a', '4a', '2b', '.', '8.c', '.', '8.c~', '8G', '2A', '.' ))
  expect_equalchar(ioi(test, finalOnset = TRUE, inPlace = FALSE), c( '4', '4', '2', NA, '8.', NA, '8.', '8', '2', NA))
  
  
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
  expect_equalchar(untie(test, inPlace = FALSE), c( '4', '4.', NA, NA, '8', '8', '2..', NA, NA, '4' ))
  
  # both
  test <- c('4a', '[4a',']8a','8g','8r','[8a','_2a','4a]','4r')
  expect_equal(ioi(untie(test), finalOnset = TRUE), 
               untie(ioi(test, finalOnset = TRUE)))
})

test_that('Examples from rhythm man are correct', {
  expect_equalnum(duration('4.ee-['), 0.375)
  expect_equal(duration('4.ee-[', inPlace = TRUE), '0.375ee-[')
  
  expect_equalnum(seconds('4.'), 1.5)
  expect_equalchar(recip('4.', Exclusive = 'notevalue'), NA_character_)
  
  expect_equalchar(recip('2', scale = 1/16), '32')
  
  
  expect_equalchar(recip('4%5', sep ='/'), '4/5')
  
  expect_equalchar(untie(c('[4a', '4a]', '2g')), 
               c('2a', '.', '2g'))
  
  expect_equal(ioi(c('4.a','8r', '4.a','8r','2a', '2r')),
               c("2a", ".",  "2a", ".", NA, "." ))
  
  expect_equal(ioi(c('4.a','8r', '4.a','8r','2a', '2r'), finalOnset = TRUE),
               c("2a", ".",  "2a", ".", '1a', "." ))
  
  ##
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/Chorales/.*krn')
  
  x <- with(chorales, table(notehead(Token)))
  
  expect_equalchar(x["𝅗𝅥 "], 222)
  
  samp <- c('4', '4.', '4%5')
  expect_equalnum(seconds(samp) * 1000, ms(samp))
  
  
})


test_that("timeline() and timestamp() work correctly", {
  x <- c('4', '4', '8', '8', '4', '8.', '16','8','8','2')
  
  expect_equal(timeline(x), c(0, 0.25, 0.5, 0.625, 0.75, 1, 1.1875, 1.25, 1.375, 1.5))
  expect_equal(timeline(x, start = 1), 1 +  c(0, 0.25, 0.5, 0.625, 0.75, 1, 1.1875, 1.25, 1.375, 1.5))
  expect_equal(timeline(x, pickup = seq_along(x) < 3), c(0, 0.25, 0.5, 0.625, 0.75, 1, 1.1875, 1.25, 1.375, 1.5) - .5)
  
  expect_equal(timestamp(x), c(":0", ":1", ":2", ":2.500", ":3", ":4", ":4.750", ":5", ":5.500", ":6"))
  expect_equal(timestamp(x, BPM = 120), c(":0", ":0.500", ":1", ":1.250", ":1.500", ":2", ":2.375", ":2.500", ":2.750", ":3"))
  
  
})

test_that("Grid functions work", {
  
  rhythm <- c('8.', '8.', '8', '8.', '8', '16', '8')
  
  expect_equalchar(grid(rhythm), c('XOO', 'XOO', 'XO', 'XOO', 'XO', "X", 'XO'))
  expect_equalchar(grid(rhythm, tick = '32'), c('XOOOOO', 'XOOOOO', 'XOOO', 'XOOOOO', 'XOOO', "XO", 'XOOO'))
  expect_equalchar(grid(duration(rhythm)), c('XOO', 'XOO', 'XO', 'XOO', 'XO', "X", 'XO'))
  
  expect_equal(togrid(rhythm), paste(grid(rhythm), collapse = ''))
  
  expect_equal(togrid(rhythm, collapse = FALSE, on = '1', off = '0'), rbind(c('1', '0', '0', '1', '0', '0', '1', '0', '1', '0', '0', '1', '0', '1', '1', '0')))
  
  expect_equalchar(rhythm, fromgrid(togrid(rhythm)))
  
})
  

test_that('Factors work correctly',{
  expect_equal(tally(recip(c('16', '4')))['8'] |> unname(), 0)
  
})

