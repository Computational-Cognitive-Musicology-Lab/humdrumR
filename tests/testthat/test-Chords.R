


test_that("Chord functions return same output, regardless of input.", {
  inputs <- list(tertian = c('CM', 'CMM', 'CMm', 'Cmm', 'FMm', 'FMM', 'Fmm', 'Fm', 'GMM', 'GMm', 'Ao', 'Aom', 'Bom', 'Boo', 'AM/3', 'Amm/3', 'E-MM'),
                 harm = c("I", "I7", "Im7", "im7", "IVm7", "IV7", "ivm7", "iv", "VM7", "V7", "vio", "vio7", "viio7", "viioD7", "VIb", 'vi7b', '-III7'),
                 roman = c("I", "I7", "Ib7", "ib7", "IVb7", "IV7", "ivb7", "iv", "Vn7", "V7", "vio", "vio7", "viio7", "viiobb7", "VI6", "vi65", '-III7'))
                 # figuredBass = c("C ", "C 7", "C b7", "C b7b3", "F b7", "F 7", "F b7b3", "F b3", "G #7", "G 7", "A b5", "A 7b5", "B 7", "B ", "C# 6", "C 65"),
                 # chord = c("C", "Cmaj7", "C7", "Cmin7", "F7", "Fmaj7", "Fmin7", "Fmin", "Gmaj7", "G7", "Adim", "Adim7", "Bdim7", "Bdim", "A/C#", "Amin7/C"))
  
  expect_allequal <- function(f, inputs) {
    vals <- lapply(inputs, f)
    vals <- lapply(vals, \(v) {attr(v, 'Exclusive') <- NULL ; v})
    
    Reduce('expect_equal', vals)
  }
  
  
  expect_allequal(tertian, inputs)
  expect_allequal(harm, inputs)
  expect_allequal(roman, inputs)
  # expect_allequal(figuredBass, inputs)
  # expect_allequal(chord, inputs)
  
})



test_that('Examples used in Chords.R mans work', {
  # harm/roman man
  expect_equal(harm('E7/B', Key = 'A:'), 'V7c')
  expect_equal(roman('E7/B', Key = 'A:'), 'V643')
  
})

