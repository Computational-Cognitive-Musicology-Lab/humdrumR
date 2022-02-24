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
  print(silbeFormat(as.data.frame(data)))
  if(nullTokens == FALSE){
    # if the user does not want null tokens to replace instances of syllables occurring after the first syllable of a multi-syllable word
    data <- as.data.frame(data)
    # transform character vector to data frame
    data <- toString(data[,1])
    # transform character vector to string *seems as though we might not need to transform to data frame above then*
    data <- stringr::str_replace_all(data, "-, -", "")
    # remove all instances of -, -, which represents a space between two syllables which when combined form a word
    data <- stringr::str_replace_all(data, ",", "")
    # remove all instances of , which occur after every word except the last one
    data <- as.list(stringr::strsplit(data, '\\s+')[[1]])
    # get all of the words as a list
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.character(transpose2)
    # transform the data further to get desired character vector
  }
  else{
    # if the user does want null tokens to replace instances of syllables occurring after the first syllable of a multi-syllable word
    wordAddSpace <- function(value){
      # create function to be used later which will either print TRUE or FALSE, depending on if there is a - at the beginning of a syllable
      # when we split up syllables from multi-syllable words, some will either have - at the beginning or - at the end.
      if(substr(value,1,1) == "-"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    replaceWithNullToken <- function(booleanValue){
      # create a function for replacing cells with null tokens, to be used in an apply function later
      if(booleanValue == TRUE){
        return(".")
      }
      else{
        return("word")
        # return "word" for identification/logic purposes later
      }
    }
    saveData <- data
    # save current character vector ("data") in a new variable to be used later
    data <- as.data.frame(data)
    # transform character vector ("data") into a data frame
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    # for each row value in the data frame (which in this case corresponds to a syllable) determine if this is a row that needs to be deleted in the future by returning TRUE for that
    # index
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    # save a vector with null tokens in the correct spots, and the spots that will be filled with words are each labeled "word"
    save2 <- as.data.frame(save2)
    # transform to data frame
    saveWords <- text(saveData, nullTokens = FALSE)
    # run this text function with null tokens = false to get the full words of the character vector
    saveWords <- as.data.frame(saveWords)
    # transform these words into a data frame
    newFunction <- function(dataValue, rowValue){
      rowValueToString <- toString(rowValue)
      # save given row value as a string to be used below
      dataValue[rowValue,1] <- paste(dataValue[rowValue,1], rowValueToString, sep = "")
      # paste the row value to each word for parsing and reading into final data frame/character vector later
      return(dataValue[rowValue,1])
    }
    newFunction2 <- function(findRowValues, iteration){
      getRowValueFinal <- sub("word*", "", findRowValues[iteration,1])
      # based on the row values of each word, replace each "word" with the row value corresponding to a word
      return(getRowValueFinal)
    }
    newFunction4 <- function(iterate, final, wordsArray){
      iterateToString <- toString(iterate)
      # save iteration value as a string for comparison to what is in the data frame/character vector
      if(iterateToString %in% final){
        # comparison for each cell
        return(wordsArray[match(iterate,final),1])
        # return corresponding word
      }
      else{
        return(".")
        # return null token if no match
      }
    }
    numbers <- 1:nrow(save2)
    # number of rows to go through in apply function
    numbers <- as.data.frame(numbers)
    # save as data frame so the apply function can work properly
    saveNew <- apply(numbers, 1, function(x){newFunction(save2,x)})
    # apply function to paste row values to each word
    saveNew <- as.data.frame(saveNew)
    # save as data frame for parsing below
    saveNew <- saveNew[!grepl(".", saveNew$saveNew, fixed = TRUE),]
    # save values that are not null tokens
    finalData <- numbers
    # final data will have size nrows
    finalWordsLength <- 1:nrow(saveWords)
    # final words length will have size of total words
    finalWordsLength <- as.data.frame(finalWordsLength)
    # save as data frame for use in apply function
    saveNewDataFrame <- as.data.frame(saveNew)
    # save as data frame for use in apply function
    finalData <- apply(finalWordsLength, 1, function(x){newFunction2(saveNewDataFrame, x)})
    # apply function to replace words with row values
    finalDataComplete <- apply(numbers, 1, function(x){newFunction4(x, finalData, saveWords)})
    # apply function to match row numbers of words with corresponding words
    data <- (unlist(finalDataComplete))
    # unlist to save as character vector
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
  for(i in 1:length(data)){
    splitString <- strsplit(data[i], "")[[1]]
    splitStringNext <- strsplit(data[i+1], "")[[1]]
    if(splitString[length(splitString)] == '-' && splitStringNext[1] != '-'){
      counter <- counter + 1
      print_initial <- append(data[i], data[i+1])
      save_initials[counter] <- list(print_initial)
      print_corrected <- append(data[i], '-')
      print_corrected <- append(print_corrected, data[i+1])
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
# silbe format vectorized
data <- c('Now', 'let', 'me', 'wel-', 'come', 'e-', 'very', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
# save_initials <- list()
# print_initial <- list()
# save_corrected <- list()
# print_corrected <- list()
# counter <- 0
iteration1 <- 1:length(data)
iteration1 <- cbind(iteration1)
splitString <- apply(iteration1, 1, function(x){return(strsplit(data[x], "")[[1]])})
library(spelling)
indicesWithErrors <- apply(iteration1, 1, function(x){
  if(x < length(iteration1)){
    if(splitString[[x]][length(splitString[[x]])] == '-' && splitString[[x+1]][1] != "-"){
      return(x+1)
    }
  }
  if(x > 1){
    if(splitString[[x]][1] == '-' && splitString[[x-1]][length(splitString[[x-1]])] != "-"){
      return(x-1)
    }
  }
  if(splitString[[x]][1] != '-' && splitString[[x]][length(splitString[[x]])] != "-"){
    if(length(spell_check_text(data[x])$word) == 1 && nchar(spell_check_text(data[x])$word) > 1){
      # if a value is not a word and does not have any dashes, print this index as having an error.
      return(x)
    }
  }
})
indicesWithErrorsSave <- unlist(indicesWithErrors)
dup <- duplicated(indicesWithErrorsSave)
removeDuplicated <- indicesWithErrorsSave[-which(dup == TRUE)]
iteration <- 1:length(removeDuplicated)
iteration <- as.data.frame(iteration)
printErrors1 <- function(iteration, df1, length1){
  if(iteration == 1){
    return(NULL)
  }
  else{
    splitString <- apply(iteration1, 1, function(x){return(strsplit(df1[x], "")[[1]])})
    # this function will assume that the first value in the character vector is input properly (need an assumption to implement such a function)
    if(splitString[[iteration]][1] == "-" && splitString[[iteration]][length(splitString[[iteration]])] == "-"
       || (splitString[[iteration]][1] != "-" && splitString[[iteration]][length(splitString[[iteration]])] == "-")){
      if(iteration > 1 && iteration < length1){
        if(splitString[[iteration+1]][1] != "-"){
          cat("error, improperly formatted **silbe: ", df1[iteration+1], " should be -",df1[iteration+1], sep = "")
          value <- paste("-", df1[iteration+1], sep = "")
          return(value)
        }
      }
    }
  }
  # return(df1[iteration])
}
saveNew <- apply(iteration1, 1, function(x){
  printErrors1(x, data, length(data))
})
saveNew <- append(list(NULL), saveNew)
newData <- apply(iteration1, 1, function(x){
    if(!is.null(saveNew[[x]])){
      value <- saveNew[[x]]
      return(value)
    }
    else{
      return(data[x])
    }
})

whichIndices <- apply(iteration1, 1, function(x){
  if(!is.null(saveNew[[x]])){
    return(x)
  }
})
whichIndices <- unlist(whichIndices)

# then check below again?


  # if(splitString[[x]][1] != "-" && splitString[[x]][length(splitString[[x]])] == "-"){
  #   if(x > 1 && x < length(iteration)){
  #     if(splitString[[x+1]][1] != "-"){
  #       cat("error, improperly formatted **silbe: ", data[x], " should be ",substr(data[x],1,nchar(data[x])-1), sep = "")
  #     }
  #   }
  # }
  # if(splitString[[x]][1] != "-" && splitString[[x]][length(splitString[[x]])] != "-"){
  #   if(x > 1 && x < length(iteration)){
  #     if(splitString[[x-1]][length(splitString[[x-1]])] != "-"){
  #       cat("error, improperly formatted **silbe: ", data[x], " should be ",substring(data[x],2), sep = "")
  #     }
  #   }
  # }

printErrors <- apply(iteration, 1, function(x){
  # split into 4 main cases, each case has 16 possible nodes
  if(splitString[[x]][1] == '-' && splitString[[x]][length(splitString[[x]])] != "-"){
    if(x > 1 && x < length(iteration)){
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
    }
  }
  if(splitString[[x]][1] != '-' && splitString[[x]][length(splitString[[x]])] == "-"){
    if(x > 1 && x < length(iteration)){
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
    }
  }
  if(splitString[[x]][1] != '-' && splitString[[x]][length(splitString[[x]])] != "-"){
    if(x>1 && x < length(iteration)){
      if(splitString[[x-1]][length(splitString[[x-1]])] == "-" && splitString[[x+1]][1] == "-"){
        if(length(spell_check_text(data[x])$word) == 1 && nchar(spell_check_text(data[x])$word) > 1){
          # if a value is not a word and does not have any dashes, print this index as having an error.
          cat("error, improperly formatted **silbe: ", data[x], " should be -",data[x], "-", sep = "")
        }
      }
      if(splitString[[x-1]][length(splitString[[x-1]])] != "-" && splitString[[x+1]][1] == "-"){
        if(length(spell_check_text(data[x])$word) == 1 && nchar(spell_check_text(data[x])$word) > 1){
          # if a value is not a word and does not have any dashes, print this index as having an error.
          cat("error, improperly formatted **silbe: ", data[x], " should be ",data[x], "-", sep = "")
        }
      }
    }
  }
  if(splitString[[x]][1] == '-' && splitString[[x]][length(splitString[[x]])] == "-"){
    if(x > 1 && x < length(iteration)){
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] == "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] == '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
      if(splitString[[x-1]][1] != '-' && splitString[[x]][length(splitString[[x-1]])] != "-"){
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] == "-"){
          
        }
        if(splitString[[x+1]][1] == '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
        if(splitString[[x+1]][1] != '-' && splitString[[x]][length(splitString[[x+1]])] != "-"){
          
        }
      }
    }
  }
})



## Tests

# test 1
values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
new <- text(values)
save <- text(values, nullTokens = FALSE)
silbeFormat(values)

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


# values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
# word_count <- str_count(dummyData, '\\w+')
# compare <- text(values)
# compare2 <- toString(compare)
# compare2 <- str_replace_all(compare2, ",", "")
# word_count_2 <- str_count(compare2, '\\w+')
# library(stringi)
# values2 <- toString(values)
# values2 <- str_replace_all(values2, ",", "")
# stringi::stri_sub(compare2, 3, 2) <- 1
# # use the above to insert dashes at specific indices

#'
#'
#' @export
textKeepSilbe <- function(data, nullTokens = TRUE){
  # same function as text but it returns a list with the first element being the character vector of words and the second item being the indices at which to insert -'s.
  dummyData <- data.frame(data)
  dummyData <- toString(dummyData[,1])
  dummyData <- str_replace_all(dummyData, "-, -", "-")
  dummyData <- str_replace_all(dummyData, ",", "")
  indices <- str_locate_all(dummyData, "-")
  getIndices <- function(index, iteration){
    # get index where you should insert a - to indicate splitting into syllables.
    getIndex <- index - iteration + 1
  }
  save_length <- length(indices[[1]])/2
  # length of indices will be the above length divided by 2 because it prints each index twice.
  iterations <- 1:save_length
  # iterations for apply function will equal length of indices
  iterations <- as.data.frame(iterations)
  # save as data frame so it can be read into apply function properly.
  save_indices <- apply(iterations, 1, function(x){getIndices(indices[[1]][x], x)})
  # save indices in a vector
  # all code below is the same as in the original text function
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
    # define the above functions in the same way as in the original text function
    saveData <- data
    data <- as.data.frame(data)
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    save2 <- as.data.frame(save2)
    saveWords <- text(saveData, nullTokens = FALSE)
    saveWords <- as.data.frame(saveWords)
    # the logic below is the same as in the original text function
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
values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
keepSilbeExample <- textKeepSilbe(values, nullTokens = FALSE)
# run the above function on an example for input into the print silbe format function
#'
#'
#' @export
printSilbeFormat <- function(keepSilbeOutput){
  reverse <- function(string, index, replacement){
    stringi::stri_sub_replace_all(string, from = index, to = index-1, replacement = replacement)
  }
  # the above function is where the bijection occurs
  save_index <- as.vector(keepSilbeOutput[[2]])
  # save indices based on input
  values <- toString(keepSilbeOutput[[1]])
  # convert character vector to string for stringr usage
  values2 <- str_replace_all(values, "-, -", "")
  values3 <- str_replace_all(values2, ",", "")
  # get just words with spaces in between each
  reverseSave <- reverse(values3, save_index, "- -")
  # use reverse function to input dashes in correct spots
  word_count <- str_count(reverseSave, '\\w+')
  # count number of words based on spaces
  saveWords <- head(strsplit(reverseSave, split = "\ "), word_count)
  # save words in list
  saveWords2 <- unlist(saveWords)
  return(saveWords2)
}
printSilbeFormat(keepSilbeExample)