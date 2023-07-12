

test_that('Shaping and coercion functions match', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  expect_equal(dim(chorales), dim(as.matrix(chorales)))
  
})
