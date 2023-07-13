
test_that('hop() man page examples work',{
  
  # from the man page
  expect_equal(hop(1:13, by = c(2,3)), c(1,3, 6, 8, 11, 13))
  
  expect_equal(hop(letters, by = 3), seq(1, 26, by = 3))
  expect_equal(hop(letters, by = 2, from = 4), seq(4, 26, by = 2))
  expect_equal(hop(letters, by = 2, from = 'e', to = 'x'), seq(5, 24, by = 2))
  expect_equal(hop(letters, by = c(-1, 2), from = 'e', to = 'w', value = TRUE),
               c('e', 'd', 'f', 'e', 'g', 'f', 'h', 'g', 'i', 'h', 'j', 'i', 'k', 'j', 
                 'l', 'k', 'm', 'l', 'n', 'm', 'o', 'n', 'p', 'o', 'q', 'p', 'r', 'q', 
                 's', 'r', 't', 's', 'u', 't', 'v', 'u', 'w'))
  expect_equal(hop(letters, by = -1, from = 'z', to = 3), 26:3)
               
})

test_that('findWindows works', {
  testL <- c('(', '(', ')', '(', '(', ')')
  testR <- c('(', ')', ')', '(', ')', ')')
  nest <- c('(', '(', '(', ')', ')', '(', ')', ')')
  nestdup <-  c('e', 'e', '(e', 'd', '((b', 'c', 'b)', 'd', 'e));', 'f', '(g', 'h)', '((g', 'h)', 'a);')
  # nestdup includes multiple opens in one token
  
  # testL
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testL, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 3', '2 6'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testL, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 3', '2 3', '4 6', '5 6'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testL, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 3',  '4 6'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testL, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('2 3', '5 6'))
  
  # testR
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testR, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 2', '4 5'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testR, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 2', '4 5'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testR, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 2',  '4 5'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(testR, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('1 2',  '4 5'))
  
  # nest
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nest, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('1 4', '2 5', '3 7', '6 8'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nest, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('1 4', '2 4', '3 4', '6 7'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nest, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('1 4',  '6 7'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nest, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('1 8', '2 5', '3 4', '6 7'))
  
  # nestdup
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nestdup, open = '(', close = ')', overlap = 'paired')[ , 1:2]),
               c('3 7', '5 9', '5 9', '11 12', '13 14', '13 15'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nestdup, open = '(', close = ')', overlap = 'edge')[ , 1:2]),
               c('3 7', '5 7', '5 7', '11 12', '13 14', '13 14'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nestdup, open = '(', close = ')', overlap = 'none')[ , 1:2]),
               c('3 7',  '11 12', '13 14'))
  expect_equal(do.call('paste', 
                       humdrumR:::findWindows(nestdup, open = '(', close = ')', overlap = 'nested')[ , 1:2]),
               c('3 9', '5 9', '5 7', '11 12', '13 15', '13 14'))
  
  
})
  

test_that('context() man examples work', {
   
   expect_equal(humdrumR::context(letters, open = c(4, 11), close = c(15, 24)),
                c("d,e,f,g,h,i,j,k,l,m,n,o", "k,l,m,n,o,p,q,r,s,t,u,v,w,x"))
  
   expect_equal(humdrumR::context(letters, open = hop(2), close = open + 3),
                c("a,b,c,d", "c,d,e,f", "e,f,g,h", "g,h,i,j", 
                  "i,j,k,l", "k,l,m,n", "m,n,o,p", "o,p,q,r", 
                  "q,r,s,t", "s,t,u,v", "u,v,w,x", "w,x,y,z"))
   
   expect_equal(humdrumR::context(letters, open = letters %in% c('e', 'j', 'l'), close = open + 2),
                c('e,f,g', 'j,k,l', 'l,m,n'))
   


   expect_equal(humdrumR::context(letters, open = '[aeiou]', close = open + 4),
                c("a,b,c,d,e", "e,f,g,h,i", "i,j,k,l,m", "o,p,q,r,s", "u,v,w,x,y"))
   
   expect_equal(humdrumR::context(letters, open = close - 4, close = '[aeiou]', alignToOpen = FALSE),
                c("a,b,c,d,e", "e,f,g,h,i", "k,l,m,n,o", "q,r,s,t,u"))
   
   expect_equal(humdrumR::context(letters, open = '[aeiou]', close = nextopen - 1L),
                c("a,b,c,d", "e,f,g,h", "i,j,k,l,m,n", "o,p,q,r,s,t"))
   
   expect_equal(humdrumR::context(letters, open = prevclose + 1, close = '[aeiou]', alignToOpen = FALSE),
                c("b,c,d,e", "f,g,h,i", "j,k,l,m,n,o", "p,q,r,s,t,u"))
   
   expect_equal(humdrumR::context(letters, open = '[aeiou]', close = nextopen - 1L | 26),
                c("a,b,c,d", "e,f,g,h", "i,j,k,l,m,n", "o,p,q,r,s,t", "u,v,w,x,y,z"))
   
   expect_equal(humdrumR::context(letters, open = '[aeiou]', close = nextopen - 1L | end),
                c("a,b,c,d", "e,f,g,h", "i,j,k,l,m,n", "o,p,q,r,s,t", "u,v,w,x,y,z"))
   
   expect_equal(humdrumR::context(letters, open = '[aeiou]', close = nextopen - 1L | 26) |> toupper(),
                humdrumR::context(LETTERS, reference = letters, open = '[aeiou]', close = nextopen - 1L | 26))
  
   expect_equal(humdrumR::context(letters,
                                  reference = data.frame(Threes = rep(1:3, length.out = 26),
                                                         Fours = rep(4:1, length.out = 26)),
                                  open = Threes == Fours, close = Fours == 1),
                c("d,e,f,g,h", "f,g,h,i,j,k,l", "p,q,r,s,t", "r,s,t,u,v,w,x"))
   
   ####
   humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-3].*.krn")
   
   expect_equal((humData)@Context |> nrow(), 0)
   cont <- (humData |> humdrumR::context(open = hop(), open + 3))@Context
   expect_equal(cont |> nrow(), 620)
   expect_equal(unique(cont[, Close - Open]), 3)
   
   
   
   ####
   
   nesting1 <- c('(a', 'b)', '(c', 'd', 'e)', '(d', 'e', 'f)', '(e', 'f', 'f#', 'g', 'g#', 'a)')
   
   expect_equal(humdrumR::context(nesting1, open = '(', close = ')'),
                c("(a,b)", "(c,d,e)", "(d,e,f)", "(e,f,f#,g,g#,a)"))
   
   nesting2 <- c('(a', 'b)', '(c', '(d', 'e)',  '(d', 'e)', 'f)', '(e', '(f', '(f#', 'g)', 'g#)', 'a)')
   
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')'),
                c("(a,b)", "(c,(d,e)", "(d,e),(d,e)", "(d,e),f)",  "(e,(f,(f#,g)", "(f,(f#,g),g#)", "(f#,g),g#),a)"))
   
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')', overlap = 'nested'),
                c("(a,b)", "(c,(d,e),(d,e),f)", "(d,e)", "(d,e)", 
                  "(e,(f,(f#,g),g#),a)", "(f,(f#,g),g#)", "(f#,g)"))
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 1),
                c("(a,b)", "(c,(d,e),(d,e),f)", "(e,(f,(f#,g),g#),a)"))
   
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2),
                c("(d,e)", "(d,e)", "(f,(f#,g),g#)"))
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')', overlap = 'nested', depth = 2:3),
                c("(d,e)", "(d,e)", "(f,(f#,g),g#)", "(f#,g)"))
   expect_equal(humdrumR::context(nesting2, open = '(', close = ')', overlap = 'nested', depth = -1),
                c("(f#,g)"))
   
   melody <- c('so', 'la', 'ti', 'do', 'so', 'fi', 'so', 'la', 'ti', 're', 'do', 'so', 'la', 're', 'do')
   
   expect_equal(humdrumR::context(melody, open = 'so', close = 'do'),
                c("so,la,ti,do", "so,fi,so,la,ti,re,do", "so,la,ti,re,do,so,la,re,do"))
   expect_equal(humdrumR::context(melody, open = 'so', close = 'do', overlap = 'edge'),
                c("so,la,ti,do", "so,fi,so,la,ti,re,do", "so,la,ti,re,do", "so,la,re,do"))
   expect_equal(humdrumR::context(melody, open = 'so', close = 'do', overlap = 'none'),
                c("so,la,ti,do", "so,fi,so,la,ti,re,do", "so,la,re,do"))
   
   nesting3 <- c('(a', 'b)', '((c', 'd', 'e)',  '(d', 'e', 'f))', '(e', 'f', '((f#', 'g)', 'g#)', 'a)')
   
   expect_equal(humdrumR::context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 1),
                c("(a,b)", "((c,d,e),(d,e,f))", "(e,f,((f#,g),g#),a)"))
   expect_equal(humdrumR::context(nesting3, open = '(', close = ')', overlap = 'nested', depth = 2),
                c("((c,d,e)", "(d,e,f))", "((f#,g),g#)"))
  
   # from @examples
   expect_equal(context(letters, open = hop(4), close = open + 3),
                c('a,b,c,d', 'e,f,g,h', 'i,j,k,l', 'm,n,o,p', 'q,r,s,t', 'u,v,w,x'))
                
   expect_equal(context(letters, open = "[aeiou]", close = nextopen - 1 | end),
                c('a,b,c,d', 'e,f,g,h', 'i,j,k,l,m,n', 'o,p,q,r,s,t', 'u,v,w,x,y,z'))
   
   expect_equal(context(letters, open = "[aeiou]", close = nextopen - 1 | end, inPlace = TRUE),
                c('a,b,c,d', NA, NA, NA, 'e,f,g,h', NA, NA, NA,
                  'i,j,k,l,m,n', NA, NA, NA, NA, NA, 'o,p,q,r,s,t', 
                  NA, NA, NA, NA, NA, 'u,v,w,x,y,z', NA, NA, NA, NA, NA))
   
   expect_equal(context(letters, open = "[aeiou]", close = nextopen - 1 | end, collapse = FALSE),
                list(c("a", "b", "c", "d"), c("e", "f", "g", "h"), 
                     c("i", "j", "k", "l", "m", "n"),
                     c("o", "p", "q", "r", "s", "t"), c("u", "v", "w", "x", "y", "z")))
  
   # within.humdrumR
   chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")
   
   # 4-grams
   expect_equal(chorales |>
                   context(open = hop(), open + 3) |>
                   within(paste(Token, collapse = ',')) |>
                   as.lines() |> 
                  index(50),
                setNames("2GG;,4GG,4GG,4AA\t2B;,4d,4d,4c\t2d;,[4g,8gL],8f#J\t2g;,4b,4b,4cc", '1.50') )
   
   # phrases leading to fermatas
   expect_equal(chorales |>
                  context(open = 1 | prevclose + 1, close = ';', overlap = 'none') |>
                  within(paste(Token, collapse = ','), alignLeft = FALSE) |>
                  pull() |> index(20),
                "4b,2dd,4cc,4b,2a,2g;")
   
})

test_that("context() output options work right", {
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2),
               c("a,b,c", "e,f,g", "i,j,k", "m,n,o", "q,r,s", "u,v,w"))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, complement = TRUE, sep = '-'),
               c("a-b-c", "d", "e-f-g", "h", "i-j-k", "l", "m-n-o", "p", "q-r-s", "t", "u-v-w", "x", "y", "z"))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, inPlace = TRUE),
               c("a,b,c", NA, NA, NA, "e,f,g", NA, NA, NA, "i,j,k", 
                 NA, NA, NA, "m,n,o", NA, NA, NA, "q,r,s", NA, NA, NA, "u,v,w", NA, NA, NA, NA, NA))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, inPlace = TRUE, complement = TRUE),
               c("a,b,c", NA, NA, "d", "e,f,g", NA, NA, "h", "i,j,k", NA, NA, "l", "m,n,o", 
                 NA, NA, "p", "q,r,s", NA, NA, "t", "u,v,w", NA, NA, "x", "y", "z"))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, inPlace = TRUE, alignToOpen = FALSE),
               c(NA, NA, "a,b,c", NA, NA, NA, "e,f,g", NA, NA, NA, 
                 "i,j,k", NA, NA, NA, "m,n,o", NA, NA, NA, "q,r,s", NA, NA, NA, "u,v,w", NA, NA, NA))
  
  # not collapsed
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, collapse = FALSE),
               list(c("a", "b", "c"), c("e", "f", "g"), c("i", "j", "k"), c("m", "n", "o"), 
                    c("q", "r", "s"), c("u", "v", "w"))
               )
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, complement = TRUE, collapse = FALSE),
               list(c("a", "b", "c"), "d", c("e", "f", "g"), "h", c("i", "j", "k"), 
                    "l", c("m", "n", "o"), "p", c("q", "r", "s"), "t", c("u", "v", "w"), "x", "y", "z"))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, inPlace = TRUE, collapse = FALSE),
               list(c("a", "b", "c"), NA, NA, NA, c("e", "f", "g"), NA, NA, NA, c("i", "j", "k"), 
                 NA, NA, NA, c("m", "n", "o"), NA, NA, NA, c("q", "r" ,"s"), NA, NA, NA, c("u", "v" ,"w"), 
                 NA, NA, NA, NA, NA))
  
  expect_equal(humdrumR::context(letters, open = hop(4), open + 2, inPlace = TRUE, complement = TRUE, collapse = FALSE),
               list(c("a", "b", "c"), NA, NA, c("d"), c("e", "f", "g"), NA, NA, c("h"),
                    c("i", "j", "k"), NA, NA, c("l"), c("m", "n", "o"), 
                    NA, NA, c("p"), c("q", "r", "s"), NA, NA, c("t"), c("u", "v","w"), NA, NA, 
                    c("x"), c("y"), c("z")))
  
  
  # examples
  chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/.*.krn")
  expect_equal(chorales |> 
                 humdrumR::context(hop(), open + 3) |> 
                 within(Pasted <- paste(Token, collapse = '->')) |> 
                 uncontext() |> 
                 with(sum(nchar(Pasted))), 
               40301)
         
  # phrases leading to fermatas
  phrases <- chorales |>
                humdrumR::context(open = 1 | prevclose + 1, close = ';', overlap = 'none') |>
                with(paste(Token, collapse = ','))
  expect_length(phrases, 256)
  expect_equal(unname(phrases[34]), "4e,8f#,4B,8A#,4B,4c#,2.d;")
  
  
  
  
})


test_that('Context vignette examples work', 
          {
            
            beethoven <- readHumdrum(humdrumRroot, 'HumdrumData/BeethovenVariations/.*krn')
            
            beethoven <- subset(beethoven, Exclusive == 'kern' & Stop == 1) |> removeEmptySpines() |> removeEmptyStops()
            result <- beethoven |>
                        humdrumR::context("(", ")") |>
                        with(if (length(Token) == 13) paste0(File, ',', Record))
            expect_equal(unname(c(result)), c("9,46", "9,47", "9,48", "9,49", "9,50", "9,52", "9,53", "9,54", "9,55", "9,56", "9,57", "9,58", "9,59"))
            
          })

