# generate examples ----


scale <- sort(tint( , -12L:12L))
scale <- c(scale - octave*2, scale - octave, scale, scale + octave)

correct <- data.frame(kern = tint2kern(scale),
                      interval = tint2interval(scale),
                      solfa = tint2solfa(scale),
                      semit = tint2semit(scale),
                      lilypond = tint2lilypond(scale),
                      helmholtz = tint2helmholtz(scale),
                      degree = tint2degree(scale)
                      )

test_that("Input -> Output maintains struture", {
    expect_throughput <-function(func, x) {
        y <- func(x)
        
        expect_equal(length(x), length(y))
        expect_equal(dim(x), dim(y))
        expect_equal(dimnames(x), dimnames(y))
        expect_equal(names(x), names(y))
        expect_equal(is.na(x), is.na(y))
    }
    expect_throughput(semit2tint,     ex$semit)
    expect_throughput(sciPitch2tint,  ex$sciPitch)
    expect_throughput(kernPitch2tint, ex$kernPitch)
    expect_throughput(interval2tint,  ex$interval)
    expect_throughput(invert,  ex$interval)
})

test_that("Functions are invertible", {
    expect_invertible <-function(func1, func2, x, y) {
        expect_equal(func2(func1(x)), x)
        expect_equal(func1(func2(y)), y)
    }
    expect_invertible(tint2sciPitch,  sciPitch2tint,  ex$tint, ex$sciPitch)
    expect_invertible(tint2kernPitch, kernPitch2tint, ex$tint, ex$kernPitch)
    expect_invertible(tint2interval,  interval2tint,  ex$tint, ex$interval)
})
