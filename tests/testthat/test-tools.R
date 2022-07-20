
test_that('Delta and sigma work right', {
  
  
  
  for (x in list(rep(NA_real_, 10),
                 sample(c(NA,1:10), 100,rep=T),
                 integer(0))) {
    a
    len <- length(x)
    
    for (lag in c(-5:-1, 1:5)) {
      right <- delta(x, right = TRUE, lag = lag)
      left  <- delta(x, right = FALSE, lag = lag)
      
      expect_length(right, len)
      expect_length(left, len)
      
      expect_equal(right[!is.na(right)], left[!is.na(left)])
      
      expect_equal(delta(x, lag = lag), delta(x, lag = -lag) * -1)
      
      expect_equal(which(is.na(x)), which(is.na(delta(x, init = 0, lag = lag))))
      expect_equal(which(is.na(x)), which(is.na(delta(x, init = 0, lag = lag, right = TRUE))))
  }

    
  }
  

  
 
  
  ### from docs
  
  
  x <- c(1, 3, 2, 2, 5)
  expect_equal(x, delta(sigma(x, lag = 2), lag = 2, init = 0))
  expect_equal(delta(c(5, 7, 5, 6)), c(NA, 2, -2, 1))
  expect_equal(sigma(delta(c(5,7,5,6))), c(0, 2, 0, 1))
  expect_equal(delta(c(5, 7, 5, 6), init = 0), c(5, 2, -2, 1))
  expect_equal(sigma(c(NA, 2, -2, 1)), c(0, 2, 0, 1))
  expect_equal(sigma(delta(c(5, 7, 5, 6), init = 0)), c(5, 7,5,6))
  expect_equal(sigma(delta(c(5, 7, 5, 6)), init = 5), c(5, 7,5,6))
  
})
