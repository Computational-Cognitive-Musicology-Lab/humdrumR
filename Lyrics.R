#' text
#' 
#' Transform humdrumR's syllabic form of lyrics into complete words, where each row will contain either one word or a null data token.
#' 
#' @param data The data to be transformed (a character vector)
#' 
#' @param nullTokens Boolean expression which determines whether null tokens will replace empty spaces where syllables have moved to combine with others to make a word. Default is TRUE
#' 
#' @return the transformed data
#' 
#' @export
#' 
#' @example Spine of syllabic form transformed into word/text form with nullTokens = TRUE
#'  # lyrics
#'  #   Now
#'  #   let
#'  #    me
#'  #  wel-
#'  # -come
#'  #    e-
#'  # -very-
#'  #   -bo-
#'  #    -dy
#'  #     to
#'  #    the
#'  #   wild
#'  #   wild
#'  #  west.
#' 
#' text(data)
#' 
#'  # lyrics
#'  #       Now
#'  #       let
#'  #        me
#'  #   welcome
#'  #         .
#'  # everybody
#'  #         .
#'  #         .
#'  #         .
#'  #        to
#'  #       the
#'  #      wild
#'  #      wild
#'  #     west.
#'  @example Spine of syllabic form transformed into word/text form with nullTokens = FALSE
#'  Same input as above.
#'  
#'  text(data, nullTokens = FALSE)
#'  
#'  # lyrics
#'  #       Now
#'  #       let
#'  #        me
#'  #   welcome 
#'  # everybody 
#'  #        to
#'  #       the
#'  #      wild
#'  #      wild
#'  #     west. 
text <- function(data, nullTokens = TRUE){
  if(nullTokens == FALSE){
    data <- as.data.frame(data)
    data <- toString(data[,1])
    data <- str_replace_all(data, "-, -", "")
    data <- str_replace_all(data, ",", "")
    data <- as.list(strsplit(data, '\\s+')[[1]])
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.character(transpose2)
  }
  else{
    wordAddSpace <- function(value){
      if(substr(value,1,1) == "-"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    replaceWithNullToken <- function(booleanValue){
      if(booleanValue == TRUE){
        return(".")
      }
      else{
        return("word")
      }
    }
    saveData <- data
    data <- as.data.frame(data)
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    save2 <- as.data.frame(save2)
    saveWords <- text(saveData, nullTokens = FALSE)
    saveWords <- as.data.frame(saveWords)
    
    newFunction <- function(dataValue, rowValue){
      rowValueToString <- toString(rowValue)
      dataValue[rowValue,1] <- paste(dataValue[rowValue,1], rowValueToString, sep = "")
      return(dataValue[rowValue,1])
    }
    newFunction2 <- function(findRowValues, iteration){
      getRowValueFinal <- sub("word*", "", findRowValues[iteration,1])
      return(getRowValueFinal)
    }
    newFunction4 <- function(iterate, final, wordsArray){
      iterateToString <- toString(iterate)
      if(iterateToString %in% final){
        return(wordsArray[match(iterate,final),1])
      }
      else{
        return(".")
      }
    }
    numbers <- 1:nrow(save2)
    numbers <- as.data.frame(numbers)
    saveNew <- apply(numbers, 1, function(x){newFunction(save2,x)})
    saveNew <- as.data.frame(saveNew)
    saveNew <- saveNew[!grepl(".", saveNew$saveNew, fixed = TRUE),]
    finalData <- numbers
    finalWordsLength <- 1:nrow(saveWords)
    finalWordsLength <- as.data.frame(finalWordsLength)
    saveNewDataFrame <- as.data.frame(saveNew)
    finalData <- apply(finalWordsLength, 1, function(x){newFunction2(saveNewDataFrame, x)})
    finalDataComplete <- apply(numbers, 1, function(x){newFunction4(x, finalData, saveWords)})
    data <- (unlist(finalDataComplete))
  }
  return(data)
}
#' silbeFormat
#' 
#' Check that the formatting of the lyrics is correct, with -'s in the right places (i.e., to denote the start or end of a syllable)
#' 
#' @param data The data to be checked for improper formatting (for now, please read in your spine as a dataframe with 1 column)
#' 
#' @return "Formatted properly." if the lyrics are formatted properly, else print error message with corrections.
#' 
#' @export
#' 
#' @example Spine with syllable labelling errors, with resulting error message
#' # lyrics
#' #  ya'll
#' #    act
#' #   like
#' # you've
#' #    ne-
#' #    ver
#' #   seen
#' #      a
#' #  white
#' #   per-
#' #    son
#' #    be-
#' #  fore
#' 
#' silbeFormat(data)
#' 
#' # error, improperly formatted **silbe: ne- ver should be ne- - ver and per- son should be per- - son and be- fore should be be- - fore
#' 
silbeFormat <- function(data){
  save_initials <- list()
  print_initial <- list()
  save_corrected <- list()
  print_corrected <- list()
  counter <- 0
  for(i in 1:nrow(data)){
    splitString <- strsplit(data[i, 1], "")[[1]]
    splitStringNext <- strsplit(data[i+1,1], "")[[1]]
    if(splitString[length(splitString)] == '-' && splitStringNext[1] != '-'){
      counter <- counter + 1
      print_initial <- append(data[i,1], data[i+1,1])
      save_initials[counter] <- list(print_initial)
      print_corrected <- append(data[i,1], '-')
      print_corrected <- append(print_corrected, data[i+1, 1])
      save_corrected[counter] <- list(print_corrected)
    }
  }
  if(counter == 0){
    return("Formatted properly.")
  }
  else{
    cat("error, improperly formatted **silbe:", save_initials[1][[1]], "should be", save_corrected[1][[1]])
    if(counter > 1){
      for(i in 2:counter){
        cat(" and", save_initials[i][[1]], "should be", save_corrected[i][[1]])
      }
    }
  }
}
## Tests

# test 1
values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
new <- text(values)
save <- text(values, nullTokens = FALSE)
silbeFormat(dummyData)

data <- as.data.frame(values)
data <- toString(data[,1])
data <- str_replace_all(data, "-, -", "")
data <- str_replace_all(data, ",", "")
data <- as.list(strsplit(data, '\\s+')[[1]])
transpose1 <- t(data)
transpose2 <- t(transpose1)
data <- as.character(transpose2)
data <- as.data.frame(data)

# test 2
values <- c("ya'll", 'act', 'like', "you've", 'ne-', 'ver', 'seen', 'a', 'white', 'per-', 'son', 'be-', 'fore')
dummyData <- data.frame(values)
text(dummyData)
text(dummyData, nullTokens = FALSE)
silbeFormat(dummyData)

# text keep silbe
library(stringr)


values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
word_count <- str_count(dummyData, '\\w+')
compare <- text(values)
compare2 <- toString(compare)
compare2 <- str_replace_all(compare2, ",", "")
word_count_2 <- str_count(compare2, '\\w+')
library(stringi)
values2 <- toString(values)
values2 <- str_replace_all(values2, ",", "")
stringi::stri_sub(compare2, 3, 2) <- 1
# use the above to insert dashes at specific indices

textKeepSilbe <- function(data, nullTokens = TRUE){
  dummyData <- data.frame(data)
  dummyData <- toString(dummyData[,1])
  dummyData <- str_replace_all(dummyData, "-, -", "-")
  dummyData <- str_replace_all(dummyData, ",", "")
  indices <- str_locate_all(dummyData, "-")
  save_length <- length(indices[[1]])/2
  save_indices <- indices[[1]][1:save_length]
  if(nullTokens == FALSE){
    data <- as.data.frame(data)
    data <- toString(data[,1])
    data <- str_replace_all(data, "-, -", "")
    data <- str_replace_all(data, ",", "")
    data <- as.list(strsplit(data, '\\s+')[[1]])
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.character(transpose2)
  }
  else{
    wordAddSpace <- function(value){
      if(substr(value,1,1) == "-"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    replaceWithNullToken <- function(booleanValue){
      if(booleanValue == TRUE){
        return(".")
      }
      else{
        return("word")
      }
    }
    saveData <- data
    data <- as.data.frame(data)
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    save2 <- as.data.frame(save2)
    saveWords <- text(saveData, nullTokens = FALSE)
    saveWords <- as.data.frame(saveWords)
    
    newFunction <- function(dataValue, rowValue){
      rowValueToString <- toString(rowValue)
      dataValue[rowValue,1] <- paste(dataValue[rowValue,1], rowValueToString, sep = "")
      return(dataValue[rowValue,1])
    }
    newFunction2 <- function(findRowValues, iteration){
      getRowValueFinal <- sub("word*", "", findRowValues[iteration,1])
      return(getRowValueFinal)
    }
    newFunction4 <- function(iterate, final, wordsArray){
      iterateToString <- toString(iterate)
      if(iterateToString %in% final){
        return(wordsArray[match(iterate,final),1])
      }
      else{
        return(".")
      }
    }
    numbers <- 1:nrow(save2)
    numbers <- as.data.frame(numbers)
    saveNew <- apply(numbers, 1, function(x){newFunction(save2,x)})
    saveNew <- as.data.frame(saveNew)
    saveNew <- saveNew[!grepl(".", saveNew$saveNew, fixed = TRUE),]
    finalData <- numbers
    finalWordsLength <- 1:nrow(saveWords)
    finalWordsLength <- as.data.frame(finalWordsLength)
    saveNewDataFrame <- as.data.frame(saveNew)
    finalData <- apply(finalWordsLength, 1, function(x){newFunction2(saveNewDataFrame, x)})
    finalDataComplete <- apply(numbers, 1, function(x){newFunction4(x, finalData, saveWords)})
    data <- (unlist(finalDataComplete))
  }
  return(list(data,save_indices))
}

# keepSilbeFinal <- function(vector, index){

values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
keepSilbeExample <- textKeepSilbe(values)
save_index <- keepSilbeExample[[2]][2]
values <- toString(values)
values2 <- str_replace_all(values, "-, -", "")
values3 <- str_replace_all(values2, ",", "")
insertSilbes <- function(wordVector, index){
  stringi::stri_sub(wordVector, index, index-1) <- "-"
  return(wordVector)
}
iterations <- 1:length(keepSilbeExample[[2]])
iterations <- as.data.frame(iterations)
save_indices <- keepSilbeExample[[2]][1:length(keepSilbeExample[[2]])]
dataIndices <- as.data.frame(save_indices)
characterVectorWithSyllables <- apply(iterations, 1, function(x){insertSilbes(values3, dataIndices[x,])})
# }