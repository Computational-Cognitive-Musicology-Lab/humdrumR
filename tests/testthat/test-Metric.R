

test_that('count() and subpos() work correctly', {
  
  ## basics
  fourfour <- c('4', '8', '8', '4', '8', '8', '8','4','8','2')
  
  # count()
  expect_identical(count(fourfour), c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_identical(count(fourfour, phase = '4'), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L))
  expect_identical(count(fourfour, beat = '4'), c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 5L, 6L, 7L))
  expect_identical(count(fourfour, beat = '4', offBeats = FALSE), c(1L, 2L, NA, 3L, 4L, NA, 5L, NA, NA, 7L))
  expect_identical(count(fourfour, start = 2L), 1L + c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_identical(count(fourfour, beat = '2', start = -2L),  c(-2L, -2L, -2L, -1L, -1L, -1L, 1L, 1L, 1L, 2L))
  expect_identical(count(fourfour, beat = '4', pickup = seq_along(fourfour) < 2L),  c(-1L, 1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L, 6L))
  
  # subpos()
  expect_identical(subpos(fourfour), c(0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375, 0.5))
  expect_identical(subpos(fourfour, phase = '4'), c(0, 0.25, 0.375, 0.5, -0.250, -0.125, 0, 0.125, 0.375, 0.5))
  expect_identical(subpos(fourfour, beat = '4'), c(0, 0, 0.125, 0, 0, 0.125, 0, 0.125, 0.125, 0))
  expect_identical(subpos(fourfour, beat = '4', deparser = recip), c("1%0", "1%0", "8", "1%0", "1%0", "8", "1%0", "8", "8", "1%0"))
  
  
  ## irregular
  
  fourtwo <- rep(head(fourfour, -1), 2)
  met <- rep(c('M4/4', 'M2/4', 'M4/4', 'M2/4'), c(6, 3, 6, 3))
  expect_identical(count(fourtwo, beat = met), c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L) )
  expect_identical(subpos(fourtwo, beat = met), c(0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375, 0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375) )
  
  
  seven <- c('4', '4', '4.', '4', '4', '4', '8', '8', '16', '16', '4', '4.')
  expect_identical(count(seven, beat = list(c('4', '4', '4.'))), c(1L, 2L, 3L, 4L, 5L, 6L, 6L, 7L, 7L, 7L, 8L, 9L))
  expect_identical(count(seven, beat = list(c('4', '4', '4.')), phase = '8'), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 7L, 8L, 8L, 8L, 9L))
  expect_identical(subpos(seven, beat = list(c('4', '4', '4.'))), c(0, 0, 0, 0, 0, 0, 0.25, 0, 0.125, .1875, 0, 0))
  expect_identical(subpos(seven, beat = list(c('4', '4', '4.')), phase = '8'), c(0, 0, 0, 0, 0, 0, -0.125, 0, -0.125, -0.0625, 0, 0))
  
  # subpos()
  
  
})


test_that('metlev family works correctly', {
  
  triplets <- c('6','6','6','8','8','12','12','12')
  
  expect_equal(metsubpos(triplets), c(0, 1/6, 1/3, 0, 0, 0, 1/12, 1/6))
  expect_equal(metsubpos(triplets, remainderSubdivides = FALSE), c(0, 1/24, 1/48, 0, 0, 0, 1/48, 1/24))
  expect_equal(metsubpos(triplets, meter = meter('M4/4', '6')), c(0, 0, 0, 0, 0, 0, 0, 1/12 ))
  expect_equal(metsubpos(triplets, meter = meter('M4/4', '12')), c(0, 0, 0, 0, 0, 0, 0, 0 ))
  
})

test_that('Examples from Metric man work', {
  
  ## count() man
  rhythm <- rep('8', 14)
  expect_identical(count(rhythm, beat = '8'), 1L:14L)
  expect_identical(count(rhythm, beat = list(c('4','4', '4.'))), as.integer(c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6)))
  
  
  expect_equal(duration('M3/4'), 0.75)
  expect_identical(count(rep('8', 8), beat = '1', phase = 3/8), as.integer(rep(c(1,2), c(5, 3))))
  
})


test_that("'Meter extraction tools' work", {

  # tatum  
  expect_equal(tatum(c('1', '4', '8'), deparser = duration), 1/8)
  expect_equal(tatum(c('1', '4', '12')), '12')
  expect_equal(tatum(c('1', '4', '8', '12')), '24')
  expect_equal(tatum(c('4', '8', '12', '16')), '48')
  expect_equal(tatum(c('M4/4', '6')), '48')
  
  expect_equal(tatum(c(1, .25, 1/8, 1/12)), 1/24)
  
  
  # tactus and measure
  meters <- c('M4/4', 'M3/4', 'M12/8', 'M6/8', 'M7/8', 'M2+2+3/8')
  expect_equal(tactus(meters), c('4', '4', '4.', '4.', '8', '4+4+4.'))
  expect_equal(tactus(meters, deparser = duration), c('0.25', '0.25', '0.375', '0.375', '0.125', '0.25+0.25+0.375'))
  expect_equal(measure(meters), c('1', '2.', '1.', '2.', '2..', '2..'))
  expect_equal(measure(meters, deparser = duration), c(1, .75, 1.5, .75, .875, .875))
  
})