#' text
#' 
#' Transform humdrumR's syllabic form of lyrics into complete words, where each row will contain either one word or a null data token.
#' 
#' @param data The data to be transformed (for now, please read in your spine as a dataframe with 1 column)
#' 
#' @return the transformed data
#' 
#' @export
#' 
#' @example Spine of syllabic form transformed into word/text form
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
text <- function(data){
  save_initial_row <- 0
  save_word <- ""
  save_value <- c()
  iteration <- 1
  for (i in iteration:nrow(data)){
    splitString <- strsplit(data[i, 1], "")[[1]]
    if(splitString[length(splitString)] == '-' ||  splitString[1] == '-'){
      if(splitString[length(splitString)] == '-' && splitString[1] != '-') {
        save_initial_row = i
        save_value <- append(save_value, splitString)
        splitStringBelow <- strsplit(data[i+1, 1], "")[[1]]
        save_value <- append(save_value, splitStringBelow)
        if(splitStringBelow[length(splitStringBelow)] != '-'){
          save_value <- save_value[save_value != "-"]
          save_word <- paste(save_value, collapse = '')
          data[save_initial_row,] <- save_word
          data[save_initial_row+1,] <- "."
          iteration = i + 1
          save_value <- c()
        }
        else{
          iteration <- i+1
        }
      }
      if(splitString[length(splitString)] == '-' &&  splitString[1] == '-'){
        save_value <- append(save_value, strsplit(data[i+1, 1], "")[[1]])
        data[i,] = "."
      }
      if(splitString[length(splitString)] != '-' && splitString[1] == '-') {
        data[i,] = "."
        save_value <- save_value[save_value != "-"]
        save_word <- paste(save_value, collapse = '')
        data[save_initial_row,] = save_word
        iteration = i + 1
        save_value <- c()
      }
    }
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

# test
# values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
# dummyData <- data.frame(values)
# text(dummyData)
# silbeFormat(dummyData)
# 
# values <- c("ya'll", 'act', 'like', "you've", 'ne-', 'ver', 'seen', 'a', 'white', 'per-', 'son', 'be-', 'fore')
# dummyData <- data.frame(values)
# text(dummyData)
# silbeFormat(dummyData)
