


test_that("Chord functions return same output, regardless of input.", {
  inputs <- list(tertian = c('CM', 'CMM', 'CMm', 'Cmm', 'FMm', 'FMM', 'Fmm', 'Fm', 'GMM', 'GMm', 'Ao', 'Aom', 'Bom', 
                             'Boo', 'AM/3', 'Amm/3', 'E-MM', 'A+', 'G+m'),
                 harm = c("I", "I7", "Im7", "im7", "IVm7", "IV7", "ivm7", "iv", "VM7", "V7", "vio", "vio7", "viio7", 
                          "viioD7", "VIb", 'vi7b', '-III7', 'VI+', 'V+7'),
                 roman = c("I", "I7", "Ib7", "ib7", "IVb7", "IV7", "ivb7", "iv", "Vn7", "V7", "vio", "vio7", "viio7", 
                           "viiobb7", "VI6", "vi65", '-III7', 'VI+', 'V+7'),
                 harte = c('C:maj', 'C:maj7', 'C:7', 'C:min7', 'F:7', 'F:maj7', 'F:min7', 'F:min', 'G:maj7', 'G:7', 'A:dim', 'A:hdim7', 'B:hdim7', 
                           'B:dim7', 'A:maj/3', 'A:min7/b3', 'E-:maj7', 'A:aug', 'G:(1,3,#5,b7)'))
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
  expect_allequal(harte, inputs)
  # expect_allequal(figuredBass, inputs)
  # expect_allequal(chord, inputs)
  
})


test_that('Common chord function arguments work', {
  harte <- c('C:maj7/3', 'G#:min9/7', 'A:maj/5', 'D:7/5', 'D:maj7/3', 'E-:9', 'B-:maj')
  
  # convert to harm
  expect_equal(harm(harte), c('I7b', '#vm7M9d', 'VIc', 'II7c', 'IIM7b', '-IIIm79', '-VII'))
  expect_equal(harm(harte, inversion = FALSE), c('I7', '#vm7M9', 'VI', 'II7', 'IIM7', '-IIIm79', '-VII'))
  expect_equal(harm(harte, inversion = FALSE , figuration = FALSE), c('I', '#v', 'VI', 'II', 'II', '-III', '-VII'))
  expect_equal(harm(harte, inversion = TRUE , figuration = FALSE), c('Ib', '#vd', 'VIc', 'IIc', 'IIb', '-III', '-VII'))
  
  # convert to roman
  expect_equal(roman(harte), c('I65', '#v64M32m1', 'VI64', 'II643', 'II6M5', '-III9m7', '-VII'))
  expect_equal(roman(harte, inversion = FALSE), c('I7', '#vM9m7', 'VI', 'II7', 'IIM7', '-III9m7', '-VII'))
  expect_equal(roman(harte, inversion = FALSE , figuration = FALSE), c('I', '#v', 'VI', 'II', 'II', '-III', '-VII'))
  expect_equal(roman(harte, inversion = TRUE , figuration = FALSE), c('I', '#v', 'VI', 'II', 'II', '-III', '-VII'))
  
  
  # convert to figured bass
  expect_equal(figuredBass(harte), c('E|65', 'F#|#64#3#2', 'E|#64', 'A|#643', 'F#|6#5', 'E-|9b7b5', 'B-|'))
  expect_equal(figuredBass(harte, inversion = FALSE), c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 'E-|9b7b5', 'B-|'))
  expect_equal(figuredBass(harte, inversion = FALSE , figuration = FALSE),  c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 'E-', 'B-'))
  expect_equal(figuredBass(harte, inversion = TRUE , figuration = FALSE), c('E', 'F#', 'E', 'A', 'F#', 'E-', 'B-'))
  
  
})

test_that('Figuration and keys cooperate', {
  
  harte <- c('C:maj7/3', 'G#:min9/7', 'A:maj/5', 'D:7/5', 'D:maj7/3', 'E-:9', 'B-:maj')
  
  # already done with Key = C in previous tests!
  expect_equal(harm(harte, Key = 'D'), c('-VII7b', '#ivM9d', 'Vc', 'Im7c', 'I7b', '-IIm7M9', '-VI'))
  expect_equal(roman(harte, Key = 'D'), c('-VII65', '#iv64M321', 'V64', 'I64m3', 'I65', '-IIM9m7', '-VI'))
  expect_equal(figuredBass(harte, Key = 'D'),  c('E|b65', 'F#|#64#3#2', 'E|64', 'A|64b3', 'F#|65', 'E-|b9b7b5', 'B-|b5'))
  expect_equal(figuredBass(harte, Key = 'E'),  c('E|b65b3', 'F#|64#32', 'E|64', 'A|6b4b3', 'F#|b65', 'E-|b9bb7b5b3', 'B-|b5b3'))
  
})

test_that('Examples used in Chords.R mans work', {
  # harm/roman man
  expect_equal(harm('E7/B', Key = 'A:'), 'V7c')
  expect_equal(roman('E7/B', Key = 'A:'), 'V643')
  
})


test_that("sonority() works properly", {
  
  expect_equal(sonority(c('G','b','d')), 'G')
  expect_equal(sonority(c('E','BB','G#')), 'E/B')
  expect_equal(sonority(c('E-','g','b-', 'DD-')), 'Eb7/Db')
  expect_equal(sonority(c('b-','dd','ff','C')), 'Bbadd2/C')
  
  expect_equal(sonority(c('BB-','E--','f' ), enharmonic = TRUE), 'Bb')
  expect_equal(sonority(c('BB-','d','f', 'b' ), enharmonic = FALSE), 'Bb')
  expect_equal(sonority(c('BB-','d','f', 'b' ), enharmonic = TRUE), 'Bbaddb2')
  
  expect_equal(sonority(c('B','e-','f#','E-','g','a#','EE','g','cc','b','f#','e-','b-','B-','e-','f#'), 
               groupby=list(c(1,1,1,2,2,2,3,3,3,4,4,4,4,5,5,5)), enharmonic = TRUE),
               c("B", "B", "B", "Eb", "Eb", "Eb", "C/E", "C/E", "C/E", "Ebmin", "Ebmin", "Ebmin", "Ebmin", "Ebmin/Bb", "Ebmin/Bb", "Ebmin/Bb"))
           
  
  expect_equal(sonority(c('B','e-','f#','E-','g','a#','EE','g','cc','b','f#','e-','b-','B-','e-','f#'), 
                        groupby=list(c(1,1,1,2,2,2,3,3,3,4,4,4,4,5,5,5)), enharmonic = TRUE, fill = FALSE),
               c("B", NA, NA, "Eb", NA, NA, "C/E", NA, NA, "Ebmin", NA, NA, NA, "Ebmin/Bb", NA, NA))
  
  expect_equal(sonority(c('B','e-','f#','E-','g','a#','EE','g','cc','b','f#','e-','b-','B-','e-','f#'), 
                        groupby=list(c(1,1,1,2,2,2,3,3,3,4,4,4,4,5,5,5)), enharmonic = TRUE, inPlace = FALSE),
               c("B", "Eb", "C/E", "Ebmin", "Ebmin/Bb"))
  
  expect_equal(sonority(c('x','y','a','c','e'), groupby =list(c(1,1,2,2,2))),
               c(NA, NA, "Amin/C", "Amin/C", "Amin/C"))

  expect_equal(sonority(c('c','e',NA,'g#'), groupby=list(c(1,1,1,1))), c("C+", "C+", "C+", "C+"))
  
  expect_equal(sonority(c('c','f','g','A','b','e', 'G','b','d'), groupby=list(c(1,1,1,2,2,2,3,3,3)), inversions = FALSE, inPlace = FALSE),
               c("Csus4", "Asus2", "G"))
  
  
  expect_equal(sonority(c('D#','FF#','a','c#')), 'D#dim7/F#')
  expect_equal(sonority(c('c','e','g#','b-', 'd')), 'C+9')
  expect_equal(sonority(c('c','e','g', 'd','f#','b-')), 'C#11')
  expect_equal(sonority(c('c','e-','g','b-','d','a-'), inversions = FALSE), 'Cminb13')
  expect_equal(sonority(c('c','e-','g','b-','d','a-')), 'Abmaj7#11/C')
  expect_equal(sonority(c('c','g','b-')), 'C57')

  
})

