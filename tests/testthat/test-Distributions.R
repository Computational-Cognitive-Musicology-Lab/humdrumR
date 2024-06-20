
test_that('distribution stuff, 1D',{
    
  # categorical        
  set.seed(1)
  N <- 1000
  cat <- sample(letters, N, replace = TRUE, prob = 1:26)
  
  cat_count <- count(cat)
  cat_count_named <- count(varname = cat)
  
  ## logicals
  expect_equal(which(cat_count == 72), 21)
  expect_equal(sum(cat_count > 20), 19)
  
  expect_equal(cat_count |> filter(grepl('[aeiou]', cat)) |> sum(), setNames(147, 'cat'))
  expect_equal(cat_count_named |> filter(grepl('[aeiou]', varname)) |> sum(), setNames(147, 'varname'))
  expect_equal(sum((cat_count |> filter(grepl('[aeiou]', cat))) > 20), 2)
  
  expect_equal(mean(cat_count), setNames(38.46154 - .00000154, 'cat'))
  expect_equal(mean(cat_count_named), setNames(38.46154 - .00000154, 'varname'))
  expect_equal(median(cat_count), setNames(36.5, 'cat'))
  
  ## indexing
  expect_identical(cat_count, cat_count[,'cat'])
  expect_identical(cat_count |> filter(grepl('[aeiou]', cat)), cat_count[c('a', 'e', 'i', 'o', 'u')])
  
  expect_equal(sum(cat_count[1:10,]), setNames(158, 'cat'))
  
  ## coercion
  expect_equal(as.data.frame(cat_count) |> ncol(), 2)
  expect_equal(as.matrix(cat_count) |> ncol(), 1)
  expect_equal(as.data.frame(cat_count_named)$varname, rownames(as.matrix(cat_count)))
  
          
  # numeric
  set.seed(1)
  num <- rchisq(N, 2)
  num_count <- count(x = num)
  
  expect_equal(sum(num_count > 5), 9)
  expect_equal(num_count[c('(5,6]','(6,7]')] |> sum(), setNames(61, 'x'))
  
  ## controlling binning
  expect_equal(nrow(num_count), 16)
  expect_equal(count(num, binArgs = list(breaks = 20)) |> nrow(), 16)
   
  expect_equal(count(range = num, binArgs=list(right = TRUE))$range[2] |> as.character(), '(1,2]')
  expect_equal(count(range = num, binArgs=list(right = FALSE))$range[2] |> as.character(), '[1,2)')
  
  for (n in c(5, 10, 20)) expect_true(all(count(num, binArgs = list(quantiles = n)) == (N / n)))
  
  expect_error(count(num, binArgs = list(quantiles = c(3, 6))))
  expect_error(count(num, binArgs = list(right = list(3))))
  expect_error(count(num, binArgs = list(maxN = 'apple')))
  
  # NA
  cat[sample(length(cat), 20)] <- NA
  num[sample(length(num), 20)] <- NA
  
  expect_equal(sum(count(cat)), sum(count(cat, na.rm = TRUE)) + 20)
  expect_equal(sum(count(num)), sum(count(num, na.rm = TRUE)) + 20)
  
  # sort
  
  expect_equal(count(cat) |> sort(), count(cat, sort = TRUE))
  expect_equal(count(cat) |> sort(decreasing = FALSE), count(cat, sort = -1))
  expect_equal(count(num) |> sort(decreasing = TRUE), count(num, sort = 1))
  
  # .drop
  expect_equal(nrow(count(num, .drop = FALSE)), 13)
  expect_equal(nrow(count(num, .drop = TRUE)), 11)
  
})



test_that('distribution stuff, 2+D',{
  
  set.seed(1)
  N <- 10000
  num <-  rchisq(N, 2)
  cat <- sample(letters, N, replace = TRUE, prob = 1:26)
  cat2 <- sample(LETTERS[1:5], N, replace = TRUE, prob = c(10,2,10,2,5))
  
  
  counts <- count(num, cat, na.rm = TRUE)
  counts_named <- count(x = num, y = cat)
  
  expect_equal(sum(counts), setNames(N, 'num.cat'))
  expect_equal(sum(counts_named), setNames(N, 'x.y'))
  
  expect_equal(sum(counts[counts > 50]), setNames(8697, 'num.cat'))
  expect_equal(sum(counts[2:30, ]), setNames(6341, 'num.cat'))
  
  
  # indexing collapses
  expect_equal(count(cat), counts[,'cat'])
  expect_equal(count(num), counts[,'num'])
  expect_equal(count(cat), count(cat, num, cat2)[ , 'cat'])
  expect_equal(count(x = cat, z = cat2), count(x = cat, y = num, z = cat2)[ , c('x', 'z')])
  
  expect_equal(pdist(x = cat, z = cat2), pdist(x = cat, y = num, z = cat2)[ , c('x', 'z')])
  
  # pdist sums
  expect_equal(pdist(x = cat, z = cat2) |> sum(), setNames(1, 'x.z'))
  expect_equal(pdist(x = cat, y = num, z = cat2)[ , c('x', 'z')] |> sum(),  setNames(1, 'x.z'))
})


expect_that('Can create distributions from other things', {
  set.seed(2)
  N <- 10000
  num <-  rchisq(N, 2)
  cat <- sample(letters, N, replace = TRUE, prob = 1:26)
  cat2 <- sample(LETTERS[1:5], N, replace = TRUE, prob = c(10,2,10,2,5))
  
  # using table first should be identical for CATEGORICAL variables
  expect_equal(count(table(cat)), count(cat))
  expect_equal(pdist(table(cat)), pdist(cat))
  
  expect_equal(count(table(cat, cat2)), count(cat, cat2))
  expect_equal(pdist(table(cat, cat2), condition = 'cat'), pdist(cat, cat2, condition = 'cat'))
  
  
  # na
  expect_equal(count(table(c(NA, cat), useNA = 'always', dnn = 'cat')), count(cat = c(NA, cat)))
  expect_equal(count(table(c(NA, cat), useNA = 'no', dnn = 'cat')), count(cat = c(NA, cat), na.rm = TRUE))
  
  
  
})

expect_that('Distribution arithmetic works', {
  set.seed(2)
  N <- 100
  cat1 <- sample(letters, N, replace = TRUE, prob = 1:26)
  cat2 <- sample(letters, N, replace = TRUE, prob = 1:26)
  
  
  expect_true(all(count(X = c(cat1, cat2)) == (count(X = cat1) + count(X = cat2))))
})

expect_that('pdist and count are consistent', {
  set.seed(1)
  N <- 10000
  num <-  rchisq(N, 2)
  cat <- sample(letters, N, replace = TRUE, prob = 1:26)
  cat2 <- sample(LETTERS[1:5], N, replace = TRUE, prob = c(10,2,10,2,5))
  
  
  expect_equal(pdist(count(num)), pdist(num))
  expect_equal(pdist(count(x = cat)), pdist(x = cat))
  expect_equal(pdist(count(x = cat, y = num)), pdist(x = cat, y = num))
  
  expect_equal(count(pdist(x = cat)), count(x = cat))
  expect_equal(count(pdist(x = cat, y = num)), count(x = cat, y = num))
  
  
  expect_equal(pdist(count(x = cat, y = num), condition = 'x'), pdist(x = cat, y = num, condition = 'x'))
  expect_equal(pdist(count(cat, num), condition = 'num'), pdist(cat, num, condition = 'num'))
  expect_equal(pdist(count(Cat = cat, num), condition = 'Cat'), pdist(Cat = cat, num, condition = 'Cat'))
  
  expect_equal(pdist(cat, num, condition = 'cat'), pdist(cat, num, condition = 1))
  expect_equal(pdist(cat, num, condition = 'num') |> unconditional(), 
               pdist(cat, num))
  
  
  
  expect_equal(pdist(cat, num, condition = 'cat'), 
               pdist(pdist(cat, num), condition = 'cat'))
  
  
  
  
})