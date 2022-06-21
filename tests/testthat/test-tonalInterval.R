# General expectations ----





# generate examples ----

# scale <- sort(tint( , -12L:12L))
# scale <- c(scale - octave*2, scale - octave, scale, scale + octave)
# 
# correct <- data.frame(kern = tint2kern(scale),
#                       interval = tint2interval(scale),
#                       solfa = tint2solfa(scale),
#                       semits = tint2semits(scale),
#                       lilypond = tint2lilypond(scale),
#                       helmholtz = tint2helmholtz(scale),
#                       degree = tint2degree(scale)
# )




test_that("Pitch function Input -> Output maintains structure", {

    
    expect_throughput <- function(func, x) {
        y <- do.call(func, list(x))
        
        expect_equal(length(x), length(y))
        expect_equal(dim(x), dim(y))
        expect_equal(dimnames(x), dimnames(y))
        expect_equal(names(x), names(y))
        expect_equal(is.na(x), is.na(y))
        expect_equal(is.null(x), is.null(x))
        
    }
    
   
    
    
    
    funcs <- c('semits', 'midi', 'pitch', 'kern', 'lilypond', 'interval',
               'degree', 'solfa', 'bhatk')
    inputs <- list(c('c', A='d', 'e', 'f', 'g'),
                   c('c', 'd', 'e', 'f', 'g'),
                   NULL,
                   rep(NA, 5),
                   rep(NA_character_, 5),
                   NA,
                   c('c', NA),
                   c('g'),
                   character(0))
    
    for (func in funcs) {
        for (input in inputs) {
            eval(bquote(expect_throughput(.(func), .(input))))
        }
    }
    

})

test_that("Pitch functions return same output, regardless of input.", {
    inputs <- list(kern = c('A-', 'e-', 'bb-', 'f', 'c', 'g', 'ddd', 'AAA', 'e', 'b--', 'f#', 'c#'),
                   pitch = c("Ab3", "Eb4", "Bb5", "F4", "C4", "G4", "D6", "A1", "E4", "Bbb4", "F#4", "C#4"),
                   interval = c("-M3", "+m3", "+m14", "+P4", "P1", "+P5", "+M16", "-m17", "+M3", "+d7", "+A4", "+A1"),
                   solfa = c("vle", "me", "^te", "fa", "do", "so", "^^re", "vvvla", "mi", "te-", "fi", "di"))
    
    expect_allequal <- function(f, inputs) {
        vals <- lapply(inputs, f)
        
        Reduce('expect_equal', vals)
    }
    
    
    expect_allequal(kern, inputs)
    expect_allequal(pitch, inputs)
    expect_allequal(interval, inputs)
    expect_allequal(semits, inputs)
    expect_allequal(solfa, inputs)
    
})
 
test_that("Functions are invertible", {
    expect_invertible <-function(func1, func2, x)  expect_equal(func2(func1(x)), x)
    
    inputs <- list(tint = tint(c(6, 5, 5, 2, 0, -1, -1, -7, -6, 15, -9, -11), c(-4, -3, -2, -1, 0, 1, 2, 3, 4, -9, 6, 7)), 
                   kern = c('A-', 'e-', 'bb-', 'f', 'c', 'g', 'ddd', 'AAA', 'e', 'b--', 'f#', 'c#'),
                   pitch = c("Ab3", "Eb4", "Bb5", "F4", "C4", "G4", "D6", "A1", "E4", "Bbb4", "F#4", "C#4"),
                   interval = c("-M3", "+m3", "+m14", "+P4", "P1", "+P5", "+M16", "-m17", "+M3", "+d7", "+A4", "+A1"),
                   solfa = c("vle", "me", "^te", "fa", "do", "so", "^^re", "vvvla", "mi", "te-", "fi", "di"),
                   helmholtz = c("ab", "eb'", "bb''", "f'", "c'", "g'", "d'''", "A,", "e'", "bbb'", "f#'", "c#'"),
                   lilypond = c("aes", "ees'", "bes''", "f'", "c'", "g'", "d'''", "a,,", "e'", "beses'", "fis'", "cis'"))
    
    #
    
    expect_invertible(tint2pitch,  pitch2tint,  inputs$tint)
    expect_invertible(tint2kern, kern2tint, inputs$tint)
    expect_invertible(tint2interval,  interval2tint,  inputs$tint)

    expect_invertible(kern2tint, tint2kern, inputs$kern)
    expect_invertible(pitch2tint, tint2pitch, inputs$pitch)
    expect_invertible(interval2tint, tint2interval, inputs$interval)
    expect_invertible(solfa2tint, tint2solfa, inputs$solfa)
    
    # exported functions
    expect_invertible(kern, kern, inputs$kern)
    expect_invertible(kern, pitch, inputs$pitch)
    expect_invertible(kern, solfa, inputs$solfa)
    expect_invertible(kern, interval, inputs$interval)
    expect_invertible(kern, helmholtz, inputs$helmholtz)
    expect_invertible(kern, lilypond, inputs$lilypond)
    
    expect_invertible(pitch, kern, inputs$kern)
    expect_invertible(pitch, pitch, inputs$pitch)
    expect_invertible(pitch, solfa, inputs$solfa)
    expect_invertible(pitch, interval, inputs$interval)
    expect_invertible(pitch, helmholtz, inputs$helmholtz)
    expect_invertible(pitch, lilypond, inputs$lilypond)
    
    expect_invertible(interval, kern, inputs$kern)
    expect_invertible(interval, pitch, inputs$pitch)
    expect_invertible(interval, solfa, inputs$solfa)
    expect_invertible(interval, interval, inputs$interval)
    expect_invertible(interval, helmholtz, inputs$helmholtz)
    expect_invertible(interval, lilypond, inputs$lilypond)
    
    expect_invertible(solfa, kern, inputs$kern)
    expect_invertible(solfa, pitch, inputs$pitch)
    expect_invertible(solfa, solfa, inputs$solfa)
    expect_invertible(solfa, interval, inputs$interval)
    expect_invertible(solfa, helmholtz, inputs$helmholtz)
    expect_invertible(solfa, lilypond, inputs$lilypond)
    
    expect_invertible(helmholtz, kern, inputs$kern)
    expect_invertible(helmholtz, pitch, inputs$pitch)
    expect_invertible(helmholtz, solfa, inputs$solfa)
    expect_invertible(helmholtz, interval, inputs$interval)
    expect_invertible(helmholtz, helmholtz, inputs$helmholtz)
    expect_invertible(helmholtz, lilypond, inputs$lilypond)
    
    expect_invertible(lilypond, kern, inputs$kern)
    expect_invertible(lilypond, pitch, inputs$pitch)
    expect_invertible(lilypond, solfa, inputs$solfa)
    expect_invertible(lilypond, interval, inputs$interval)
    expect_invertible(lilypond, helmholtz, inputs$helmholtz)
    expect_invertible(lilypond, lilypond, inputs$lilypond)
})


test_that('Tonal args work correctly', {
    test <- c('Eb5', 'G4')
    
    
    
    expect_equal(kern(test, generic = TRUE), kern(test, specific = FALSE))
    expect_equal(kern(test, generic = FALSE), kern(test, specific = TRUE))
    expect_equal(kern(test, generic = FALSE, specific = TRUE), kern(test, specific = TRUE))
    expect_error(kern(test, generic = TRUE, specific = TRUE))
    expect_error(kern(test, generic = FALSE, specific = FALSE))
    
})


test_that('Pitch arguments return correct values!', {
    # These are used in documentation!:
    expect_equal(pitch('so'), 
                 'G4')
    expect_equal(pitch('4.ee-['), 
                 'Eb5')
    expect_equal(pitch('4.ee-[', inPlace = TRUE), 
                 '4.Eb5[')
    
    expect_equal(kern('Eflatflat', parse(flat = 'flat')), 
                 'E--')
    expect_equal(kern('aa_', parse(flat = "_")), 
                 "aa-")
    expect_equal(kern('4.aa_JJ', parse(flat= "_"), inPlace = TRUE), 
                 '4.aa-JJ')
    expect_equal(kern('G flat', parse(flat = 'flat', sep = ' ')), 
                 'G-')
    
    expect_equal(kern('Fx', parse(doublesharp = 'x')), 
                 "F##")
    expect_equal(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'A:'), 
                 'C#')
    expect_equal(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'a:'), 
                 'C')
    expect_equal(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'a-:'), 
                 'C-')
    expect_equal(kern(c('C','C','C'), parse(implicitSpecies = TRUE), Key = c('A:', 'a:', 'a-:')), 
                 c('C#', 'C', 'C-'))
    
    expect_equal(kern(c("D#", "E", "D", "E", "Dn", "C", "D"), parse(memory = TRUE)),
                 c("D#", "E", "D#", "E", "D", "C", "D"))
    expect_equal(kern('C5', parse(octave.integer = TRUE, octave.shift = 4)),
                 'cc')
    
    expect_equal(pitch(c("c,", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",")),
                 c('C2', 'C3', 'C4'))
    
    expect_equal(interval(c("2M", "5P"), parse(parts = c("step", "species"))), c('+M2', "+P5"))
    
    expect_equal(kern("E flat 5", parse(flat = "flat", sep = " ")), 'ee-')
    
    expect_equal(`humdrumRattr<-`(tonalInterval('II#', step.labels =c('I', 'II', 'III','IV','V','VI','VII')), list(dispatch = NULL)), tint(-15L, 9L))
    
    expect_equal(kern('E x 5', parse(doublesharp = 'x', sep = ' ')), 'ee##')
    
    expect_equal(kern(0:2), c('c', 'd-', 'd'))
    expect_equal(kern(0:2, parseArgs=list(accidental.melodic = TRUE)), c('c', 'c#', 'd'))
    
    expect_equal(cents('g', tonalHarmonic = 3), 701.955)
    expect_equal(cents(440 * 10/9, Exclusive = 'freq'), 1082.404 )
    
    expect_equal(semits('c#', generic = TRUE), 0)
    expect_equal(semits('c#', generic = TRUE, Key ='A:'), 1)
    
    expect_equal(kern(c('CX5','C4','Cb5'), parse(doublesharp = 'X')),
                 kern(c('CX5','C4','Cb5'), parseArgs = list(doublesharp = 'X')))
    
    expect_equal(tonalInterval('c#', Key = "A:"), tint(-8, 4))
    
    expect_equal( kern('Eb5', flat = "_"), 'ee_')
    expect_equal(kern('Eb5', flat = 'flat'), 'eeflat')
    
    expect_equal(pitch("f##", doublesharp = "x"), 'Fx4')
    
    expect_equal( kern('C####5', specifier.maximum=2), 'cc##')
    expect_equal(interval(c("g-", "f#"), augment = 'aug', diminish = 'dim'),
                 c("+dim5", "+aug4"))
    
    expect_equal(kern(c('e-','e','f','f#','f'), explicitNaturals = FALSE),
                 c("e-", "e",  "f",  "f#", "f" ))
    expect_equal(kern(c('e-','e','f','f#','f'), explicitNaturals = TRUE),
                 c("e-", "en",  "fn",  "f#", "fn" ))
})
