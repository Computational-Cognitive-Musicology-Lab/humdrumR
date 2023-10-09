expect_equalchar <- function(e1, e2) expect_equal(as.character(e1), as.character(e2))
expect_equalnum <- function(e1, e2) expect_equal(as.numeric(e1), as.numeric(e2))


test_that('timecount() and subpos() work correctly', {
  
  ## basics
  fourfour <- c('4', '8', '8', '4', '8', '8', '8','4','8','2')
  
  # timecount()
  expect_identical(timecount(fourfour), c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_identical(timecount(fourfour, phase = '4'), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L))
  expect_identical(timecount(fourfour, unit = '4'), c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 5L, 6L, 7L))
  expect_identical(timecount(fourfour, unit = '4', offBeats = FALSE), c(1L, 2L, NA, 3L, 4L, NA, 5L, NA, NA, 7L))
  expect_identical(timecount(fourfour, start = 2L), 1L + c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_identical(timecount(fourfour, unit = '2', start = -2L),  c(-2L, -2L, -2L, -1L, -1L, -1L, 1L, 1L, 1L, 2L))
  expect_identical(timecount(fourfour, unit = '4', pickup = seq_along(fourfour) < 2L),  c(-1L, 1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L, 6L))
  
  # subpos()
  expect_equalnum(subpos(fourfour), c(0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375, 0.5))
  expect_equalnum(subpos(fourfour, phase = '4'), c(0, 0.25, 0.375, 0.5, -0.250, -0.125, 0, 0.125, 0.375, 0.5))
  expect_equalnum(subpos(fourfour, unit = '4'), c(0, 0, 0.125, 0, 0, 0.125, 0, 0.125, 0.125, 0))
  expect_equalchar(subpos(fourfour, unit = '4', deparser = recip), c("1%0", "1%0", "8", "1%0", "1%0", "8", "1%0", "8", "8", "1%0"))
  
  
  ## irregular
  
  fourtwo <- rep(head(fourfour, -1), 2)
  met <- rep(c('M4/4', 'M2/4', 'M4/4', 'M2/4'), c(6, 3, 6, 3))
  expect_identical(timecount(fourtwo, unit = met), c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L) )
  expect_equalnum(subpos(fourtwo, unit = met), c(0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375, 0, 0.25, 0.375, 0.5, 0.75, 0.875, 0, 0.125, 0.375) )
  
  
  seven <- c('4', '4', '4.', '4', '4', '4', '8', '8', '16', '16', '4', '4.')
  expect_identical(timecount(seven, unit = list(c('4', '4', '4.'))), c(1L, 2L, 3L, 4L, 5L, 6L, 6L, 7L, 7L, 7L, 8L, 9L))
  expect_identical(timecount(seven, unit = list(c('4', '4', '4.')), phase = '8'), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 7L, 8L, 8L, 8L, 9L))
  expect_equalnum(subpos(seven, unit = list(c('4', '4', '4.'))), c(0, 0, 0, 0, 0, 0, 0.25, 0, 0.125, .1875, 0, 0))
  expect_equalnum(subpos(seven, unit = list(c('4', '4', '4.')), phase = '8'), c(0, 0, 0, 0, 0, 0, -0.125, 0, -0.125, -0.0625, 0, 0))
  
  # subpos()
  
  
})


test_that('metlev family works correctly', {
  
  triplets <- c('6','6','6','8','8','12','12','12')
  
  expect_equalnum(metsubpos(triplets), c(0, 1/6, 1/3, 0, 0, 0, 1/12, 1/6))
  expect_equalnum(metsubpos(triplets, remainderSubdivides = FALSE), c(0, 1/24, 1/48, 0, 0, 0, 1/48, 1/24))
  expect_equalnum(metsubpos(triplets, meter = meter('M4/4', '6')), c(0, 0, 0, 0, 0, 0, 0, 1/12 ))
  expect_equalnum(metsubpos(triplets, meter = meter('M4/4', '12')), c(0, 0, 0, 0, 0, 0, 0, 0 ))
  
})

test_that('Examples from Metric man work', {
  
  ## timecount() man
  rhythm <- rep('8', 14)
  expect_identical(timecount(rhythm, unit = '8'), 1L:14L)
  expect_identical(timecount(rhythm, unit = list(c('4','4', '4.'))), as.integer(c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6)))
  
  
  expect_equalnum(duration('M3/4'), 0.75)
  expect_identical(timecount(rep('8', 8), unit = '1', phase = 3/8), as.integer(rep(c(1,2), c(5, 3))))
  
})


test_that("'Meter extraction tools' work", {

  # tatum  
  expect_equalnum(tatum(c('1', '4', '8'), deparser = duration), 1/8)
  expect_equalchar(tatum(c('1', '4', '12')), '12')
  expect_equalchar(tatum(c('1', '4', '8', '12')), '24')
  expect_equalchar(tatum(c('4', '8', '12', '16')), '48')
  expect_equalchar(tatum(c('M4/4', '6')), '48')
  
  expect_equalnum(tatum(c(1, .25, 1/8, 1/12)), 1/24)
  
  
  # tactus and measure
  meters <- c('M4/4', 'M3/4', 'M12/8', 'M6/8', 'M7/8', 'M2+2+3/8')
  expect_equal(tactus(meters), c('4', '4', '4.', '4.', '8', '4+4+4.'))
  expect_equal(tactus(meters, deparser = duration), c('0.25', '0.25', '0.375', '0.375', '0.125', '0.25+0.25+0.375'))
  expect_equalchar(measure(meters), c('1', '2.', '1.', '2.', '2..', '2..'))
  expect_equalnum(measure(meters, deparser = duration), c(1, .75, 1.5, .75, .875, .875))
  
})

test_that("Syncopation works correctly", {
  
  rhythm <- c('8.', '8.', '8.', '8.', '2.')
  
  expect_equal(syncopation(rhythm), c(FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(syncopation(rhythm, levels = '8'), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(syncopation(rhythm, levels = c('16', '4')), c(FALSE, TRUE, FALSE, TRUE, TRUE))
  
})
