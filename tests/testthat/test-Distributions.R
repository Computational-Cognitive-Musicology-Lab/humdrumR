
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
  
  # categorical        
  set.seed(1)
  N <- 10000
  num <-  rchisq(N, 2)
  cat <- sample(letters, N, replace = TRUE, prob = 1:26)
  
  
  counts <- count(num, cat, na.rm = TRUE)
  counts_named <- count(x = num, y = cat)
  
  expect_equal(sum(counts), setNames(N, 'num.cat'))
  expect_equal(sum(counts_named), setNames(N, 'x.y'))
  
  expect_equal(sum(counts[counts > 50]), setNames(8697, 'num.cat'))
  expect_equal(sum(counts[2:30, ]), setNames(6341, 'num.cat'))
  
  
  # indexing collapses
  expect_equal(count(cat), counts[,'cat'])
  expect_equal(count(num), counts[,'num'])
  
})