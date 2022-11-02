


test_that("Chord functions return same output, regardless of input.", {
  inputs <- list(harm = c('I', 'Ic', 'V/V', 'V7c/V', 'iio', 'iio7'
                 pitch = c("Ab3", "Eb4", "Bb5", "F4", "C4", "G4", "D6", "A1", "E4", "Bbb4", "F#4", "C#4"),
                 interval = c("-M3", "+m3", "+m14", "+P4", "P1", "+P5", "+M16", "-m17", "+M3", "+d7", "+A4", "+A1"),
                 solfa = c("vle", "me", "^te", "fa", "do", "so", "^^re", "vvvla", "mi", "te-", "fi", "di"))
  
  expect_allequal <- function(f, inputs) {
    vals <- lapply(inputs, f)
    vals <- lapply(vals, \(v) {attr(v, 'Exclusive') <- NULL ; v})
    
    Reduce('expect_equal', vals)
  }
  
  
  expect_allequal(kern, inputs)
  expect_allequal(pitch, inputs)
  
})



test_that('Examples used in Chords.R mans work', {
  # harm/roman man
  expect_equal(harm('E7/B', Key = 'A:'), 'V7c')
  expect_equal(roman('E7/B', Key = 'A:'), 'V643')
  
})

