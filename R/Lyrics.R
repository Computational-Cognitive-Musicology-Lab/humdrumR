#' text
#' 
#' Transform humdrumR's syllabic form of lyrics into complete words, where each row will contain either one word or a null data token.
#' 
#' @param data The data to be transformed (for now, please read in your spine as a dataframe with 1 column)
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
    data <- toString(data[,1])
    data <- stringr::str_replace_all(data, "-, -", "")
    data <- stringr::str_replace_all(data, ",", "")
    data <- as.list(strsplit(data, '\\s+')[[1]])
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.data.frame(transpose2)
    colnames(data) <- c('Lyrics')
  } else {
    wordAddSpace <- function(value){
      if(substr(value,1,1) == "-"){
        return(TRUE)
      } else{
        return(FALSE)
      }
    }
    replaceWithNullToken <- function(booleanValue){
      if(booleanValue == TRUE){
        return(".")
      } else{
        return("word")
      }
    }
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    save2 <- as.data.frame(save2)
    saveWords <- text(data, nullTokens = FALSE)
    
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
    data <- as.data.frame(unlist(finalDataComplete))
    colnames(data) <- c('Lyrics')
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
dummyData <- data.frame(values)
text(dummyData)
text(dummyData, nullTokens = FALSE)
silbeFormat(dummyData)

# test 2
values <- c("ya'll", 'act', 'like', "you've", 'ne-', '-ver', 'seen', 'a', 'white', 'per-', 'son', 'be-', 'fore')
dummyData <- data.frame(values)
text(dummyData)
text(dummyData, nullTokens = FALSE)
silbeFormat(dummyData)




# c('ne-','-ver', 'seen','a','white','per-','-son', 'be-','-fore')
# c('never','.','seen','a','white','person','.','before')
# c('never[1]','never[2]', 'seen','a','white','person[1]',person[2]',
#   'before[1]','before[2]')
# c('never[2]', '.', 'seen','a','white','person[2]','.','before[2]')

