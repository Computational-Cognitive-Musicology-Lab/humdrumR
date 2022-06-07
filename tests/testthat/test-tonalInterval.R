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
# 
# test_that("Functions are invertible", {
#     expect_invertible <-function(func1, func2, x, y) {
#         expect_equal(func2(func1(x)), x)
#         expect_equal(func1(func2(y)), y)
#     }
#     expect_invertible(tint2sciPitch,  sciPitch2tint,  ex$tint, ex$sciPitch)
#     expect_invertible(tint2kernPitch, kernPitch2tint, ex$tint, ex$kernPitch)
#     expect_invertible(tint2interval,  interval2tint,  ex$tint, ex$interval)
# })
