ex <- list(
    semit = -30L:30L,
    kernPitch = c('c','cc','ccc','c#','d-','DD-','A-','G#','GGG##','B-','b--','bbb-','bb','f-','F-'),
    sciPitch = c('C4', 'C5', 'C6','C#4', 'Db4','Db2','Ab3','G#3','G##1','Bb3','Bbb4','Bb6','B5','Fb4','Fb3'),
    tonalChroma = c('C','C#','Db','Ab','G#','G##','Bb','B','Fb'),
    interval = c('P1', 'P8', 'P15', 'A1', 'm2', '-M14', '-M3', '-d4', '-d18', '-M2', '+d7', 'm21', 'M14', 'd4', '-A5')
)

test_that("Input -> Output maintains struture", {
    expect_throughput <-function(func, x) {
        y <- func(x)
        
        expect_equal(length(x), length(y))
        expect_equal(dim(x), dim(y))
        expect_equal(dimnames(x), dimnames(y))
        expect_equal(names(x), names(y))
    }
    expect_throughput(semit2tint,     ex$semit)
    expect_throughput(sciPitch2tint,  ex$sciPitch)
    expect_throughput(kernPitch2tint, ex$kernPitch)
    expect_throughput(interval2tint,  ex$interval)
})

test_that("Functions are invertible", {
    expect_invertible <-function(func1, func2, x, y) {
        expect_equal(func2(func1(x)), x)
        expect_equal(func1(func2(y)), y)
    }
    expect_invertible(tint2semit,     semit2tint,     ex$tint, ex$semit)
    expect_invertible(tint2sciPitch,  sciPitch2tint,  ex$tint, ex$sciPitch)
    expect_invertible(tint2kernPitch, kernPitch2tint, ex$tint, ex$kernPitch)
    expect_invertible(tint2interval,  interval2tint,  ex$tint, ex$interval)
})