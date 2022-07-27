


test_that('Filtering subsets should add up to total', {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
  
  Total <- with(chorales, length(Token))
  
  for (i in 1:length(chorales)) {
    this <-  with(chorales[i], length(Token))
    rest <- with(chorales[-i], length(Token))
    
    expect_equal(Total, this + rest)
  }
  
  expect_equal(with(chorales[c(1, 3, 5, 7, 9)], length(Token)),
               Total - with(chorales[c(2, 4, 6, 8, 10)], length(Token)))

  
  for (j in 1:4) {
    this <-  with(chorales[[ , j]], length(Token))
    rest <- with(chorales[[ , -j]], length(Token))
    
    expect_equal(Total, this + rest)
  }
  
  
  for (i in list(1:50, 21:40, seq(1, 1000, by = 2))) {
    this <-  with(chorales[[i, ]], length(Token))
    rest <- with(chorales[[-i, ]], length(Token))
    
    expect_equal(Total, this + rest)
  }
  chorales[[-1:-20, ]]
  
})

test_that('Multiple filters works as it should', {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
  
  
  expect_identical(chorales[6:10][2], chorales[7])
  expect_identical(chorales[[ , 3:4]][[, 2]], chorales[[, 4]])
  
})

test_that('Filtering vignette examples work', {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
  
  # [numeric]
  
  expect_length(chorales[5], 1)
  expect_length(chorales[c(1,3,5)], 3)
  expect_length(chorales[6:10], 5)
  
  expect_equal(with(chorales[5], length(Token)), 333)
  
  # [character]
  
  expect_length(chorales['-'], 5)
  expect_true(all(with(chorales['-'], any(grepl('-', Token)))))
  
  # [[numeric]]
  
  expect_equal(with(chorales[[1:50,]], length(Token)), 772)
  expect_equal(with(chorales[[, 2]], length(Token)), 622)
  
  expect_length(chorales[[150:200, ]], 3)
  
  # [[character]]
  
  expect_length(chorales[['-', ]], 5)
  expect_length(chorales[[, '-' ]], 5)
  expect_equal(with(chorales[[ , '-']][3], max(Spine)), 4)
  expect_equal(with(chorales[[ , '-']][5], max(Spine)), 1)
  expect_true(with(chorales[[ , '-']][1], all(Instrument == 'I"Tenor')))
  expect_true(with(chorales[[ , '-']][5], all(Instrument == 'I"Bass')))
})