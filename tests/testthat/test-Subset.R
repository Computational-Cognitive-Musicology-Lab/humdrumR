

# test_that("Remove empty works right", {
  # chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
  # spines
  # for (j in 1:4) {
  #   x <- chorales[[ , j, removeEmpty = FALSE]]
  #   y <- subset.humdrumR(chorales, Spine == j | is.na(Spine))
  #   expect_identical(x, y)
  # }
  # # 
  # for (j in 1:4) {
  #   x <- chorales[[ , j]]
  #   
  #   y <- removeEmptySpines(subset(chorales, Spine == j | is.na(Spine)))
  #   expect_identical(x,y)
  # }
  
  # records
  # x <- chorales[[1:70, , removeEmpty = FALSE]]
  # y <- subset(chorales, Record %in% 1:70 | Token %in% c('*-', '*v', '*^') | grepl('\\*\\*', Token),
  #             recordtypes = 'GLIMDdP')
  # expect_identical(x,y)
  
  ## removeNull needs to be changed so this can work:
  # x <- chorales[[1:70]]
  # y <- removeEmptyRecords(y)
  # expect_identical(x,y)
  # 
# })

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
  
  expect_equal(with(chorales[[1:50,]], length(Token)), 776)
  expect_equal(with(chorales[[, 2]], length(Token)), 622)
  
  expect_length(chorales[[150:200, ]], 3)
  
  # [[character]]
  
  expect_length(chorales[['-', ]], 5)
  expect_length(chorales[[, '-' ]], 5)
  expect_equal(with(chorales[[ , '-']][3], max(Spine)), 4)
  expect_equal(with(chorales[[ , '-']][5], max(Spine)), 1)
  expect_true(with(chorales[[ , '-']][1], all(Instrument == 'I"Tenor')))
  expect_true(with(chorales[[ , '-']][5], all(Instrument == 'I"Bass')))
  
  # general subsetting

  expect_length(subset(chorales, (Record %% 2 == 0) == (Spine %% 2 == 0) ), length(chorales))
  expect_equal(with(subset(chorales, (Record %% 2 == 0) == (Spine %% 2 == 0)), length(Token)), 1213)
  
  
  expect_length(subset(chorales, Token %~% '-'), 5)
  expect_equal(with(subset(chorales, Token %~% '-'), length(Token)), 
               with(chorales, sum(grepl('-', Token))))
  
  # subsetting by
  barsub <- subset(chorales, fill = any(Token %~% '-'), by = list(File, Bar)) 
  expect_equal(with(barsub, nrow(unique(cbind(File,Record)))), 171)
  expect_equal(with(barsub, nrow(unique(cbind(Bar,Record)))), 144)
  
  barsub2 <- subset(chorales, fill = any(Token %~% '-'), by = list(File, floor(Bar  / 2)))
  
  expect_equal(with(barsub2, nrow(unique(cbind(Bar,Record)))), 162)
  expect_equal(with(barsub2, nrow(unique(cbind(File,Record)))), 194)
  
})


test_that("Unfiltering works", {
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
  
  orig <- getHumtab(chorales)
  cleared <- getHumtab(clearFilter(subset(chorales, Spine == 1)))
  expect_true(all(orig == cleared, na.rm = TRUE))
  
})
