# General expectations ----
# General expectations ----
test_that("wort() works correctly", {
  silbe <- c('let', 'me', 'wel-', '-come', 'ev-', '-ery-', '-bo-','-dy', 'to', 'the', 'wild', 'wild', 'west')

  wort <- c('let', 'me', 'wel-come', '_', 'ev-ery-bo-dy', '_', '_', '_', 'to', 'the', 'wild', 'wild', 'west')

  expect_identical(wort, wort(silbe))
  
  
  expect_identical(gsub('-', '', wort),
                   wort(silbe, keep.sep = FALSE))
  
  expect_identical(c('let', 'me', 'welcome[1]', 'welcome[2]', 'everybody[1]', 'everybody[2]', 'everybody[3]', 'everybody[4]',
                     'to', 'the', 'wild', 'wild', 'west'),
                   wort(silbe, keep.sep = FALSE, number.syllables = TRUE))
  
  
  expect_identical(wort(c('e-', 'b-', 'G', 'ba-', '-na-', '-na'),
                        Exclusive = c('kern', 'kern', 'kern', 'silbe', 'silbe', 'silbe')),
                   c(NA, NA, NA, 'ba-na-na', '_', '_'))
})
