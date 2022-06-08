# General expectations ----





# generate examples ----

# scale <- sort(tint( , -12L:12L))
# scale <- c(scale - octave*2, scale - octave, scale, scale + octave)
# 
# correct <- data.frame(kern = tint2kern(scale),
#                       interval = tint2interval(scale),
#                       solfa = tint2solfa(scale),
#                       semit = tint2semit(scale),
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
    
   
    
    
    
    funcs <- c('semit', 'midi', 'pitch', 'kern', 'lilypond', 'interval',
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
    expect_allequal(semit, inputs)
    expect_allequal(solfa, inputs)
    
})
 
test_that("Functions are invertible", {
    expect_invertible <-function(func1, func2, x)  expect_equal(func2(func1(x)), x)
    
    
    #
    tint_test <- tint( , -4:7)
    kern_test <- c('a-', 'e-', 'b-', 'f', 'c', 'g', 'd', 'a', 'e', 'b', 'f#', 'c#')
    pitch_test <- c("Ab4", "Eb4", "Bb4", "F4", "C4", "G4", "D4", "A4", "E4", "B4", "F#4", "C#4")
    
    expect_invertible(tint2pitch,  pitch2tint,  tint_test)
    expect_invertible(tint2kern, kern2tint, tint_test)
    expect_invertible(tint2interval,  interval2tint,  tint_test)

    expect_invertible(kern2tint, tint2kern, kern_test)
    expect_invertible(pitch2tint, tint2pitch, pitch_test)
    
    # exported functions
    expect_invertible(kern, pitch, pitch_test)
    expect_invertible(pitch, kern, kern_test)
    
    
})
