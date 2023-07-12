
test_that("Predicates work", {
  
  mozart <- readHumdrum(humdrumRroot, "HumdrumData/MozartVariations/.*.krn")
  
  expect_true(anyPaths(mozart))
  
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  expect_false(anyPaths(chorales))
})
test_that('Coercion functions match', {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  expect_equal(dim(chorales), dim(as.matrix(chorales)))
  
})



# Update Exclusive ----

test_that("Exclusive is updated by update_Exclusive", {
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor00[5-7].*.krn')
  
  chorales <- within(chorales,
                     Semits <- semits(Token),
                     Pitch <- pitch(Token),
                     Nchar <- nchar(Token))
  
  # Actual Exclusive field is updated (for analysis)
  expect_equal(chorales |> select(Token) |> pull(Exclusive) |> unique(), 'kern')
  expect_equal(chorales |> select(Nchar) |> pull(Exclusive) |> unique(), 'kern')
  expect_equal(chorales |> select(Semits) |> pull(Exclusive) |> unique(), 'semits')
  expect_equal(chorales |> select(Pitch) |> pull(Exclusive) |> unique(), 'pitch')
  
  # Printout of ** tokens are updated (for printing)
  expect_equal(chorales |> pullPrintable('Token', 'ED') |> pull(1) |> index(1), '**kern')
  expect_equal(chorales |> pullPrintable('Nchar', 'ED') |> pull(1) |> index(1), '**kern')
  expect_equal(chorales |> pullPrintable('Semits', 'ED') |> pull(1) |> index(1), '**semits')
  expect_equal(chorales |> pullPrintable('Pitch', 'ED') |> pull(1) |> index(1), '**pitch')
  
})