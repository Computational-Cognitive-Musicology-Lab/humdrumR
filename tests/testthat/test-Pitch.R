# General expectations ----
expect_equalchar <- function(e1, e2) expect_equal(as.character(e1), as.character(e2))
expect_equalnum <- function(e1, e2) expect_equal(as.numeric(e1), as.numeric(e2))





# generate examples ----

# scale <- sort(tint( , -12L:12L))
# scale <- c(scale - octave*2, scale - octave, scale, scale + octave)
# 
# correct <- data.frame(kern = humdrumR:::tint2kern(scale),
#                       interval = humdrumR:::tint2interval(scale),
#                       solfa = humdrumR:::tint2solfa(scale),
#                       semits = humdrumR:::tint2semits(scale),
#                       lilypond = humdrumR:::tint2lilypond(scale),
#                       helmholtz = humdrumR:::tint2helmholtz(scale),
#                       degree = humdrumR:::tint2degree(scale)
# )

test_that('tonalIntervalS4 math works', {
    
    expect_true((M3 + M3) == A5)
    expect_true((m3 - m3) == unison)
    expect_true((-M3 + M3) == unison)
    expect_true((P4 - P5) == -M2)
    expect_true((-M3 - m3) == -P5)
    
    expect_true((A5 %/% M3) == 2)
    
    expect_true((P4 + 'FF') == 'BB-')
    expect_true((P4 + 'FF]') == 'BB-]')
    
})

test_that('Examples from tonalIntervalS4 doc work', {
    
    expect_true((M3 + m3) == P5)

    
    expect_true((M2 * 3) == A4)
    expect_true((M3 + 2) == 6)
    expect_true((M3 + '4.ee-') == '4.gg')
    expect_true((M3 + M3) == A5)
    
    expect_true((A4 %/% M2) == 3)
    expect_true((A4 %%  M2) == unison)
    expect_true((d5 %/% M2) == -3)
    expect_true((d5 %%  M2) == P8)
    
    expect_false(A1 == m2)
    expect_true((m3 >= A2) && (m3 <= A2) && m3 != A2)
    
    
    expect_true((M9 - M2) == P8)
    expect_true((M9 - 2) == 12)
    expect_true((M3 %/% M2) == 2)
    expect_true((M3 %/% 2L) == M2)
    expect_true(("cc#]" + M6) == 'aa#]')
    
    
    cMajor <- sort(tint( , -1:5))
    eMajor <- cMajor + M3
    expect_equal(eMajor + 2L, c(6,  8, 10, 11, 13, 15, 17))

    expect_equal(eMajor[4:5] - P8, c(-m3, -m2 ))
})

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
               'degree', 'solfa', 'bhatk', 'deg', 'solfg', 'tonh')
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
        vals <- lapply(vals, humdrumR:::untoken)
        Reduce('expect_equal', vals)
    }
    
    
    expect_allequal(kern, inputs)
    expect_allequal(pitch, inputs)
    expect_allequal(interval, inputs)
    expect_allequal(semits, inputs)
    expect_allequal(solfa, inputs)
    expect_allequal(degree, inputs)
    expect_allequal(deg, inputs)
    
})
 
test_that("Functions are invertible", {
    expect_invertible <-function(func1, func2, x) expect_equal(func2(func1(x)), x)
 
    
    inputs <- list(tint = tint(c(6, 5, 5, 2, 0, -1, -1, -7, -6, 15, -9, -11), c(-4, -3, -2, -1, 0, 1, 2, 3, 4, -9, 6, 7)), 
                   kern = c('A-', 'e-', 'bb-', 'f', 'c', 'g', 'ddd', 'AAA', 'e', 'b--', 'f#', 'c#', 'B'),
                   pitch = c("Ab3", "Eb4", "Bb5", "F4", "C4", "G4", "D6", "A1", "E4", "Bbb4", "F#4", "C#4", 'B3'),
                   interval = c("-M3", "+m3", "+m14", "+P4", "P1", "+P5", "+M16", "-m17", "+M3", "+d7", "+A4", "+A1", '-m2'),
                   solfa = c("vle", "me", "^te", "fa", "do", "so", "^^re", "vvvla", "mi", "te-", "fi", "di", 'vti'),
                   helmholtz = c("ab", "eb'", "bb''", "f'", "c'", "g'", "d'''", "A,", "e'", "bbb'", "f#'", "c#'", 'b'),
                   tonh = c("As3", "Es4", "Bes5", "F4", "C4", "G4", "D6", "A1", "E4", "Beses4", "Fis4", "Cis4", 'H3'),
                   lilypond = c("aes", "ees'", "bes''", "f'", "c'", "g'", "d'''", "a,,", "e'", "beses'", "fis'", "cis'", 'b'))
    
    #
    
    # expect_invertible(humdrumR:::tint2pitch,  humdrumR:::pitch2tint,  inputs$tint)
    expect_invertible(humdrumR:::tint2kern, humdrumR:::kern2tint, inputs$tint)
    expect_invertible(humdrumR:::tint2interval,  humdrumR:::interval2tint,  inputs$tint)

    expect_invertible(humdrumR:::kern2tint, humdrumR:::tint2kern, inputs$kern)
    expect_invertible(humdrumR:::pitch2tint, humdrumR:::tint2pitch, inputs$pitch)
    expect_invertible(humdrumR:::interval2tint, humdrumR:::tint2interval, inputs$interval)
    expect_invertible(humdrumR:::solfa2tint, humdrumR:::tint2solfa, inputs$solfa)
    
    expect_invertible_factor <-function(func1, func2, x) expect_equal(c(unclass(func2(func1(x)))), x)
    # exported functions
    expect_invertible_factor(kern, kern, inputs$kern)
    expect_invertible_factor(kern, pitch, inputs$pitch)
    expect_invertible_factor(kern, solfa, inputs$solfa)
    expect_invertible_factor(kern, interval, inputs$interval)
    expect_invertible_factor(kern, helmholtz, inputs$helmholtz)
    expect_invertible_factor(kern, lilypond, inputs$lilypond)
    
    expect_invertible_factor(pitch, kern, inputs$kern)
    expect_invertible_factor(pitch, pitch, inputs$pitch)
    expect_invertible_factor(pitch, solfa, inputs$solfa)
    expect_invertible_factor(pitch, interval, inputs$interval)
    expect_invertible_factor(pitch, helmholtz, inputs$helmholtz)
    expect_invertible_factor(pitch, lilypond, inputs$lilypond)
    
    expect_invertible_factor(interval, kern, inputs$kern)
    expect_invertible_factor(interval, pitch, inputs$pitch)
    expect_invertible_factor(interval, solfa, inputs$solfa)
    expect_invertible_factor(interval, interval, inputs$interval)
    expect_invertible_factor(interval, helmholtz, inputs$helmholtz)
    expect_invertible_factor(interval, lilypond, inputs$lilypond)
    
    expect_invertible_factor(solfa, kern, inputs$kern)
    expect_invertible_factor(solfa, pitch, inputs$pitch)
    expect_invertible_factor(solfa, solfa, inputs$solfa)
    expect_invertible_factor(solfa, interval, inputs$interval)
    expect_invertible_factor(solfa, helmholtz, inputs$helmholtz)
    expect_invertible_factor(solfa, lilypond, inputs$lilypond)
    
    expect_invertible_factor(helmholtz, kern, inputs$kern)
    expect_invertible_factor(helmholtz, pitch, inputs$pitch)
    expect_invertible_factor(helmholtz, solfa, inputs$solfa)
    expect_invertible_factor(helmholtz, interval, inputs$interval)
    expect_invertible_factor(helmholtz, helmholtz, inputs$helmholtz)
    expect_invertible_factor(helmholtz, lilypond, inputs$lilypond)
    
    expect_invertible_factor(lilypond, kern, inputs$kern)
    expect_invertible_factor(lilypond, pitch, inputs$pitch)
    expect_invertible_factor(lilypond, solfa, inputs$solfa)
    expect_invertible_factor(lilypond, interval, inputs$interval)
    expect_invertible_factor(lilypond, helmholtz, inputs$helmholtz)
    expect_invertible_factor(lilypond, lilypond, inputs$lilypond)
})


test_that('Tonal args work correctly', {
    test <- c('Eb5', 'G4')
    
    expect_equalchar(kern(test, generic = TRUE), kern(test, specific = FALSE))
    expect_equalchar(kern(test, generic = FALSE), kern(test, specific = TRUE))
    expect_equalchar(kern(test, generic = FALSE, specific = TRUE), kern(test, specific = TRUE))
    expect_error(kern(test, generic = TRUE, specific = TRUE))
    expect_error(kern(test, generic = FALSE, specific = FALSE))
    
})


test_that('Pitch arguments return correct values!', {
    # These are used in documentation!:
    
    expect_equalchar(pitch('so'),  'G4')
    expect_equalchar(pitch('4.ee-['),  'Eb5')
    expect_equalchar(pitch('4.ee-[', inPlace = TRUE),  '4.Eb5[')
    
    expect_equalchar(kern('Eflatflat', parse(flat = 'flat')),  'E--')
    expect_equalchar(kern('aa_', parse(flat = "_")),  "aa-")
    expect_equalchar(kern('4.aa_JJ', parse(flat= "_"), inPlace = TRUE),  '4.aa-JJ')
    expect_equalchar(kern('G flat', parse(flat = 'flat', sep = ' ')),  'G-')
    
    expect_equalchar(kern('Fx', parse(doublesharp = 'x')),  "F##")
    expect_equalchar(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'A:'),  'C#')
    expect_equalchar(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'a:'),  'C')
    expect_equalchar(kern(c('C'), parse(implicitSpecies = TRUE), Key = 'a-:'),  'C-')
    expect_equalchar(kern(c('C','C','C'), parse(implicitSpecies = TRUE), Key = c('A:', 'a:', 'a-:')),  c('C#', 'C', 'C-'))
    
    expect_equalchar(kern(c("D#", "E", "D", "E", "Dn", "C", "D"), parse(memory = TRUE)), c("D#", "E", "D#", "E", "D", "C", "D"))
    expect_equalchar(kern('C5', parse(octave.integer = TRUE, octave.shift = 4)), 'cc')
    
    expect_equalchar(pitch(c("c,", "c", "c'"), parse(octave.integer = FALSE, up = "'", down = ",")), c('C2', 'C3', 'C4'))
    
    expect_equalchar(interval(c("2M", "5P"), parse(parts = c("step", "species"))), c('+M2', "+P5")) 
    expect_equalchar(kern("E flat 5", parse(flat = "flat", sep = " ")), 'ee-') 
    #
    step <- tonalInterval('II#', step.labels =c('I', 'II', 'III','IV','V','VI','VII'))
    attr(step, 'dispatch') <- NULL
    expect_equal(step, tint(-15L, 9L))
    
    expect_equalchar(kern('E x 5', parse(doublesharp = 'x', sep = ' ')), 'ee##')
    
    expect_equalchar(kern(0:2), c('c', 'd-', 'd'))
    expect_equalchar(kern(0:2, parseArgs=list(accidental.melodic = TRUE)), c('c', 'c#', 'd'))
    
    expect_equalnum(cents('g', tonalHarmonic = 3), 701.955)
    expect_true(round(cents(440 * 10/9, Exclusive = 'freq'), 3) ==  1082.404)
    
    expect_equalnum(semits('c#', generic = TRUE), 0)
    expect_equalnum(semits('c#', generic = TRUE, Key ='A:'), 1)
    
    expect_equalchar(kern(c('CX5','C4','Cb5'), parse(doublesharp = 'X')),
                     kern(c('CX5','C4','Cb5'), parseArgs = list(doublesharp = 'X')))
    
    expect_true(tonalInterval('c#', Key = "A:") == tint(-7, 4))
    
    expect_equalchar( kern('Eb5', flat = "_"), 'ee_')
    expect_equalchar(kern('Eb5', flat = 'flat'), 'eeflat')
    
    expect_equalchar(pitch("f##", doublesharp = "x"), 'Fx4')
    
    expect_equalchar( kern('C####5', specifier.maximum=2), 'cc##')
    expect_equalchar(interval(c("g-", "f#"), augment = 'aug', diminish = 'dim'),
                 c("+dim5", "+aug4"))
    
    expect_equalchar(kern(c('e-','e','f','f#','f'), explicitNaturals = FALSE),
                 c("e-", "e",  "f",  "f#", "f" ))
    expect_equalchar(kern(c('e-','e','f','f#','f'), explicitNaturals = TRUE),
                 c("e-", "en",  "fn",  "f#", "fn" ))
})


test_that("int, mint, and hint work", {
    inputs <- list(kern = c('A-', 'e-', 'bb-', 'f', 'c', 'g', 'ddd', 'AAA', 'e', 'b--', 'f#', 'c#'),
                   pitch = c("Ab3", "Eb4", "Bb5", "F4", "C4", "G4", "D6", "A1", "E4", "Bbb4", "F#4", "C#4"),
                   interval = c("-M3", "+m3", "+m14", "+P4", "P1", "+P5", "+M16", "-m17", "+M3", "+d7", "+A4", "+A1"),
                   solfa = c("vle", "me", "^te", "fa", "do", "so", "^^re", "vvvla", "mi", "te-", "fi", "di"))
    
    expect_equal(nrow(unique(do.call('rbind', lapply(inputs, mint)))), 1L)
    expect_equal(nrow(unique(do.call('rbind', lapply(inputs, hint)))), 1L)
    
    expect_equal(mint(inputs$kern, incomplete = NULL) |> as.character(),
                 c(NA, "+P5", "+P12", "-P11", "-P4", "+P5", "+P12", "-P32", "+P19", "+dd5", "-dd4", "-P4"))
    expect_equal(mint(inputs$kern, directed = FALSE, incomplete = pitch, bracket = FALSE) |> as.character(),
                 c('Ab3', "P5", "P12", "P11", "P4", "P5", "P12", "P32", "P19", "dd5", "dd4", "P4"))
    
    classif <- c('c', 'd', 'f', 'g','g','c','b', 'c','c#', 'd','d#','e','g--')
    expect_equal(mint(classif, bracket = TRUE, classify = TRUE),
                 c("[c]", "+Step", "+Skip", "+Step", "Unison", "-Leap", "+Leap", "-Leap", "+Unison", "+Step", "+Unison", "+Step", "+Skip"))
    expect_equal(mint(classif, classify = TRUE, atonal = TRUE, skips = FALSE),
                 c("[c]", "+Step", "+Leap", "+Step", "Unison", "-Leap", "+Leap", "-Leap", "+Step", "+Step", "+Step", "+Step", "+Step"))
    
    ##
    chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001*.krn')
    
    chorale <- within(chorale,
                      Lag <- hint(Token),
                      Bass <- hint(Token, lag = Spine == 1)
                      )
    
    expect_true(with(chorale, subset = Spine < 3, all(as.character(Bass) == as.character(Lag))))
    
    expect_equal(with(chorale, tally(mint(Token))['+M2']), 45)
    expect_equal(with(chorale, tally(hint(Token, lag = Spine == 4))['-M17']), 7)
    
})


test_that("transpose and invert work", {
    x <- c('A4','B4','C#5', 'D5', 'E5', 'F#5', 'G#5', 'A5')
    
    ## transpose
    expect_equal(transpose(x, by = M9), c("B5", "C#6", "D#6", "E6", "F#6", "G#6", "A#6", "B6"))
    expect_equal(transpose(x, by = M3, tonal = TRUE, from = 'A:'), transpose(x, by = M3, tonal = TRUE, from = 'A:'))
    expect_equal(transpose(x, from = 'A:', to = 'C:'), c("C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"))
    expect_equal(transpose(x, from = 'a:', to = 'G:', relative = TRUE), c("E4", "F#4", "G#4", "A4", "B4", "C#5", "D#5", "E5"))
    expect_equal(transpose(x, from = 'A:', to = 'g:', tonal = TRUE), c("G4", "A4", "Bb4", "C5", "D5", "Eb5", "F5", "G5"))
    
    
    # arithmetic style
    expect_equal((x + P8) - P8, x)
    expect_equal(x + M2 + m7, x + P5 + P4)
    expect_equal(x - m3 + M6, x + P8 - d5)
    
    
    ## invert
    expect_equal(invert(x), c("Eb3", "Db3", "Cb3", "Bb2", "Ab2", "Gb2", "Fb2", "Eb2"))
    expect_equal(invert(x, around = x[1]), c("A4", "G4", "F4", "E4", "D4", "C4", "Bb3", "A3"))
    expect_equal(invert(x, around = M9, Key = 'A:'), c("G#5", "F#5", "E5", "D5", "C#5", "B4", "A4", "G#4"))
    
    
    #
    
    chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
    
    with(chorales, kern(Token, simple = TRUE, transposeArgs = list(to = 'C:'))) |> tally() -> kerntab
    with(chorales, solfa(Token, simple = TRUE)) |> tally() -> solfatab
    
    expect_true(all(S3Part(kerntab) == S3Part(solfatab)))
    
})

test_that('Factors constructed correctly', {
    
    chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
    
    args <- expand.grid(generic = c(FALSE, TRUE), simple = c(FALSE, TRUE), min.lof = c(-10, 0L))
    for (i in 1:nrow(args)) {
        generic <- args$generic[i]
        simple <- args$simple[i]
        min.lof <- args$min.lof[i]
        
        kerntab <- with(chorales, tally(kern(Token, generic = generic, simple = simple, gamutArgs = list(min.lof = min.lof))))
        pitchtab <- with(chorales, tally(pitch(Token, generic = generic, simple = simple, gamutArgs = list(min.lof = min.lof))))
        lilytab <- with(chorales, tally(lilypond(Token, generic = generic, simple = simple, gamutArgs = list(min.lof = min.lof))))
        
        expect_true(all(S3Part(kerntab) == S3Part(pitchtab)))
        expect_true(all(S3Part(kerntab) == S3Part(lilytab)))
        
        solfatab <- with(chorales, tally(solfa(Token, generic = generic, simple = simple, gamutArgs = list(min.lof = min.lof))))
        degreetab <- with(chorales, tally(degree(Token, generic = generic, simple = simple, gamutArgs = list(min.lof = min.lof))))
        
        expect_true(all(S3Part(solfatab) == S3Part(degreetab)))
        
    }
    
    kerntab <- with(chorales, tally(kern(Token, generic = TRUE, simple = TRUE))) |> S3Part()
    lilytab <- with(chorales, tally(lilypond(Token, generic = TRUE, simple = TRUE))) |> S3Part()
    
    names(dimnames(kerntab)) <- names(dimnames(lilytab)) <- NULL
    
    expect_identical(kerntab, lilytab)
    
    
})
