

test_that('Reading writing are inverses', {
  oldfiles <- dir(paste0(humdrumRroot, '/HumdrumData/BachChorales/'), pattern = '^chor', full.names = TRUE)
  chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor.*.krn')
  
  writeHumdrum(chorales, EMD = NULL)
  
  newfiles <- dir(paste0(humdrumRroot, '/HumdrumData/BachChorales/'), pattern = '^humdrumR_chor', full.names = TRUE)
  
  if (expect_equal(length(newfiles), length(oldfiles))) {
    for (i in 1:length(newfiles)) {
     expect_equal(readLines(oldfiles[i]), readLines(newfiles[i]))
    }
  }
  
  for (file in newfiles) file.remove(file)
  
  
  
  
} )
