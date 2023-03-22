
test_that('hop() man page examples work',{
  
  # from the man page
  expect_equal(hop(1:13, by = c(2,3)), c(1,3, 6, 8, 11, 13))
  
  expect_equal(hop(letters, by = 3), seq(1, 26, by = 3))
  expect_equal(hop(letters, by = 2, from = 4), seq(4, 26, by = 2))
  expect_equal(hop(letters, by = 2, from = 'e', to = 'x'), seq(5, 24, by = 2))
  expect_equal(hop(letters, by = c(-1, 2), from = 'e', to = 'w', value = TRUE),
               c('e', 'd', 'f', 'e', 'g', 'f', 'h', 'g', 'i', 'h', 'j', 'i', 'k', 'j', 
                 'l', 'k', 'm', 'l', 'n', 'm', 'o', 'n', 'p', 'o', 'q', 'p', 'r', 'q', 
                 's', 'r', 't', 's', 'u', 't', 'v', 'u', 'w'))
  expect_equal(hop(letters, by = -1, from = 'z', to = 3), 26:3)
               
})

test_that('findWindows works', {
  testL <- c('(', '(', ')', '(', '(', ')')
  testR <- c('(', ')', ')', '(', ')', ')')
  nest <- c('(', '(', '(', ')', ')', '(', ')', ')')
  nestdup <-  c('e', 'e', '(e', 'd', '((b', 'c', 'b)', 'd', 'e));', 'f', '(g', 'h)', '((g', 'h)', 'a);')
  # nestdup includes multiple opens in one token
  
  # testL
  expect_equal(do.call('paste', 
                       findWindows(testL, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 3', '2 6'))
  expect_equal(do.call('paste', 
                       findWindows(testL, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 3', '2 3', '4 6', '5 6'))
  expect_equal(do.call('paste', 
                       findWindows(testL, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 3',  '4 6'))
  expect_equal(do.call('paste', 
                       findWindows(testL, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('2 3', '5 6'))
  
  # testR
  expect_equal(do.call('paste', 
                       findWindows(testR, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 2', '4 5'))
  expect_equal(do.call('paste', 
                       findWindows(testR, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 2', '4 5'))
  expect_equal(do.call('paste', 
                       findWindows(testR, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 2',  '4 5'))
  expect_equal(do.call('paste', 
                       findWindows(testR, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('1 2',  '4 5'))
  
  # nest
  expect_equal(do.call('paste', 
                       findWindows(nest, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 4', '2 5', '3 7', '6 8'))
  expect_equal(do.call('paste', 
                       findWindows(nest, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 4', '2 4', '3 4', '6 7'))
  expect_equal(do.call('paste', 
                       findWindows(nest, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 4',  '6 7'))
  expect_equal(do.call('paste', 
                       findWindows(nest, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('1 8', '2 5', '3 4', '6 7'))
  
  # nestdup
  expect_equal(do.call('paste', 
                       findWindows(nestdup, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('3 7', '5 9', '5 9', '11 12', '13 14', '13 15'))
  expect_equal(do.call('paste', 
                       findWindows(nestdup, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('3 7', '5 7', '5 7', '11 12', '13 14', '13 14'))
  expect_equal(do.call('paste', 
                       findWindows(nestdup, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('3 7',  '11 12', '13 14'))
  expect_equal(do.call('paste', 
                       findWindows(nestdup, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('3 9', '5 7', '5 9', '11 12', '13 14', '13 15'))
  
  
  
})
  