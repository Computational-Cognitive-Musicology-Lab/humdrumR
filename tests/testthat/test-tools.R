
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
  

  
 
  
  ###

  
})
