context("Humdrum pitch function tests.")


test_that("as.kernPitch works", {
  expect_equal(as.kernPitch(c('g',  'dd', 'B',   'BB-', 'D--',  'f##')),  
               as.kernPitch(c('G4', 'D5', 'B3',  'Bb2', 'Dbb3', 'F##4')),
               as.kernPitch(c('P5', 'M9', '-m2', '-M9', '-A7',  'AA4')),
               c(             'g',  'dd', 'B',   'BB-', 'D--',  'f##'))}
  )
