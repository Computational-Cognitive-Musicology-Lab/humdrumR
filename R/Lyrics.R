#' Extract words from syllables
#' 
#' Transform humdrumR's syllabic form of lyrics into complete words, where each row will contain either one word or a null data token.
#' 
#' @param data ***The data to be transformed***
#' 
#' Must be `character`.
#' 
#' @param nullTokens ***Whether null tokens will replace empty spaces where syllables have moved to combine with others to make a word. ***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be `logical`; must be length `1`.
#' 
#' @param keepSilbe ***Whether the user wants to save silbe format in the output in case needed for back translation later or other uses.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be `logical`; must be length `1`.
#' 
#' @param indices ***Whether the linguistics version of the word will be printed.***
#' 
#' Defaults to `FALSE`.
#' 
#' Must be `logical`; must be length `1`.
#' 
#' @return the transformed data
#' 
#' @export
#' 
#' @example Spine of syllabic form transformed into word form with nullTokens = TRUE
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
#' wort(silbe)
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
#'  @example Spine of syllabic form transformed into word form with nullTokens = FALSE
#'  Same input as above.
#'  
#'  wort(silbe, nullTokens = FALSE)
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
#'  @example Spine of syllabic form transformed into word with nullTokens = FALSE and keepSilbe = TRUE
#'  
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
#'  wort(silbe, nullTokens = FALSE, keepSilbe = TRUE)
#'  
#' # [[1]]
#' # [1] "Now"       "let"       "me"        "welcome"   "everybody" "to"        "the"       "wild"     
#' # [9] "wild"      "west."    
#' 
#' # [[2]]
#' # [1] 15 21 25 27
#' 
#' @example Spine of syllabic form transformed into word with nullTokens = FALSE and indices = TRUE
#' 
#' #  lyrics
#' #     op-
#' #      _
#' #   -por-
#' #    -tu-
#' #    -ni-
#' #     -ty
#' #  knocks
#' #    once
#' #       _
#' #      in
#' #       a
#' #   life-
#' #   -time
#'    
#' wort(silbe, nullTokens = FALSE, indices = TRUE) 
#' 
#' # [1] "opportunity[1]" "_"              "opportunity[2]" "opportunity[3]" "opportunity[4]"
#' # [6] "opportunity[5]" "knocks"         "once"           "_"              "in"            
#' # [11] "a"              "lifetime[1]"    "lifetime[2]" 
#' 
#' 
#' 
wort <- function(silbe, nullTokens = TRUE, keepSilbe = FALSE, indices = FALSE){
  print(silbeFormat(silbe))
  if(indices){
      open <- grepl('-$', silbe)
      close <- grepl('^-', silbe)
      continue <- open & close
      single <- !open & !close
      
      underscore <- grepl("_", silbe)
      underscore_print <- ifelse(underscore, "_", "")
      
      single_words <- ifelse(single, silbe, "")
      single_words_boolean <- ifelse(single_words != "", TRUE, FALSE)
      
      silbe_grouped <- ifelse(!single_words_boolean, silbe, "")
      
      remove_empty_space <- silbe_grouped[-which(silbe_grouped == "")]
      
      open2 <- grepl("-$", remove_empty_space)
      close2 <- grepl('^-', remove_empty_space)
      continue2 <- open2 & close2
      
      silbe_grouped2 <- tapply(remove_empty_space, cumsum(open2 & !close2), list)
      
      words <- sapply(silbe_grouped2, \(x) gsub('-*', '', paste(x, collapse = '')))
      
      numbers <- lapply(silbe_grouped2, seq_along)
      words_with_dashes_numbers <- Map(\(w,n) paste(w, '[', n, ']', sep = ''), words, numbers)
      words_with_dashes_numbers_2_unlisted <- unlist(words_with_dashes_numbers)
      
      single_words_df <- as.data.frame(single_words)
      
      words_with_dashes_numbers_2_unlisted_df <- as.data.frame(words_with_dashes_numbers_2_unlisted)
      
      is.na(single_words_df) <- single_words_df == ""
      
      indx <- is.na(single_words_df)
      
      single_words_df[which(indx == TRUE),] <- words_with_dashes_numbers_2_unlisted_df
      
      return(as.vector(single_words_df[,1]))
  }
  else if(keepSilbe){
      dummyData <- data.frame(silbe)
      dummyData <- toString(dummyData[,1])
      dummyData <- stringr::str_replace_all(dummyData, "-, -", "-")
      dummyData <- stringr::str_replace_all(dummyData, ",", "")
      indices <- stringr::str_locate_all(dummyData, "-")
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
          silbe <- as.data.frame(silbe)
          silbe <- toString(silbe[,1])
          silbe <- str_replace_all(silbe, "-, -", "")
          silbe <- str_replace_all(silbe, ",", "")
          silbe <- as.list(strsplit(silbe, '\\s+')[[1]])
          transpose1 <- t(silbe)
          transpose2 <- t(transpose1)
          silbe <- as.character(transpose2)
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
          saveData <- silbe
          silbe <- as.data.frame(silbe)
          save <- apply(silbe, 1, function(x){wordAddSpace(x)})
          save <- as.data.frame(save)
          # go through and if true then add space below
          save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
          save2 <- as.data.frame(save2)
          saveWords <- wort(saveData, nullTokens = FALSE)
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
          silbe <- (unlist(finalDataComplete))
      }
      return(list(silbe,save_indices))
  }
  else if(nullTokens == FALSE){
    # if the user does not want null tokens to replace instances of syllables occurring after the first syllable of a multi-syllable word
    silbe <- as.data.frame(silbe)
    # transform character vector to data frame
    silbe <- toString(silbe[,1])
    # transform character vector to string *seems as though we might not need to transform to data frame above then*
    silbe <- stringr::str_replace_all(silbe, "-, -", "")
    # remove all instances of -, -, which represents a space between two syllables which when combined form a word
    silbe <- stringr::str_replace_all(silbe, ",", "")
    # remove all instances of , which occur after every word except the last one
    silbe <- as.list(strsplit(silbe, '\\s+')[[1]])
    # get all of the words as a list
    transpose1 <- t(silbe)
    transpose2 <- t(transpose1)
    silbe <- as.character(transpose2)
    # transform the data further to get desired character vector
    return(silbe)
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
    saveData <- silbe
    # save current character vector ("silbe") in a new variable to be used later
    silbe <- as.data.frame(silbe)
    # transform character vector ("data") into a data frame
    save <- apply(silbe, 1, function(x){wordAddSpace(x)})
    # for each row value in the data frame (which in this case corresponds to a syllable) determine if this is a row that needs to be deleted in the future by returning TRUE for that
    # index
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    # save a vector with null tokens in the correct spots, and the spots that will be filled with words are each labeled "word"
    save2 <- as.data.frame(save2)
    # transform to data frame
    saveWords <- wort(saveData, nullTokens = FALSE)
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
    return(data)
  }
}

#' silbeFormat
#' 
#' Check that the formatting of the lyrics is correct, with -'s in the right places (i.e., to denote the start or end of a syllable)
#' 
#' @param cVector ***The data to be checked for improper formatting.***
#' 
#' Must be `data.frame`.
#' 
#' For now, please read in your spine as a dataframe with 1 column.
#' 
#' @return "Formatted properly." if the lyrics are formatted properly, else print error message with corrections.
#' 
#' @note This function might detect multiple inconsistencies/errors in a given value at a particular index, which could help the user determine the exact issue(s) with their transcription. 
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
#' silbeFormat(cVector)
#' 
#' error: ver should be -ver. index 
#' 6 
#' error: son should be -son. index 
#' 11 
#' error: fore should be -fore. index 
#' 13
#' 
silbeFormat <- function(cVector){
  index <- 1:length(cVector)
  index <- cbind(index)
  index <- as.data.frame(index)
  splitString <- apply(index, 1, function(x){return(strsplit(cVector[x], "")[[1]])})
  checkIsCharacter <- function(cVector){
    value <- FALSE
    booleanValues <- apply(index, 1, function(x){
      if(!is.character(cVector[x])){
        stop(c(cVector[x], " is not a character. Please input a character vector."))
      }
      })
  }
  checkLength <- function(cVector){
    booleanValues <- apply(index, 1, function(x){
      if(length(cVector) <= 1){
        stop("Your input must be a character vector of length greater than 1. Please input a character vector of length greater than 1.")
      }
    })
  }
  checkNumberOfDashes <- function(cVector){
    booleanValues <- apply(index, 1, function(x){
      if(str_count(cVector[x], "-") > 2){
        stop(c(cVector[x], " has more than 2 -'s", "Each character cannot have more than 2 -'s. Please adjust your input accordingly."))
      }
      if(str_count(cVector[x], "-") == 2){
        iteration1 <- 1:length(splitString[[x]])
        iteration1 <- as.data.frame(iteration1)
        checkIfRepeating <- apply(iteration1, 1, function(y){
          if(y < nrow(iteration1)){
            if(splitString[[x]][y] == "-" && splitString[[x]][y+1] == "-"){
              stop(c(cVector[x], "has repeating -'s"," you cannot have repeating -'s. "))
            }
          }
        })
      }
    })
  }
  if(!is.null(c(checkIsCharacter(cVector), checkLength(cVector), checkNumberOfDashes(cVector)))){
    return(c(checkIsCharacter(cVector), checkLength(cVector), checkNumberOfDashes(cVector)))
  }
  printErrors <- apply(index, 1, function(x){
    if(x == 1){
      if(splitString[[x]][1] == '-'){
        stop(c(cVector[x], " has a - in the front. The first character input should not have - in the front. Please adjust your input accordingly. "))
      }
    }
    if(x < nrow(index)){
      if(splitString[[x]][length(splitString[[x]])] == '-' && splitString[[x+1]][1] != "-"){
        stop(c(cVector[x+1], " should be -", cVector[x+1], ". ", sep = ""))
      }
    }
    if(x > 1){
      if(splitString[[x]][1] == '-' && splitString[[x-1]][length(splitString[[x-1]])] != "-"){
        stop(c(cVector[x-1], " should be ", cVector[x-1], "-. ", sep = ""))
      }
    }
    if(splitString[[x]][1] != "-" && splitString[[x]][length(splitString[[x]])] != "-"){
      if(length(spell_check_text(cVector[x])$word) != 0){
        stop(c("You might want to double check the transcription of", cVector[x], "because it was detected as not being a word. It is only possible for words to not have -'s."))
      }
    }
  })
}

#'
#'
#' @export
printSilbeFormat <- function(keepSilbeOutput){
    #checkArg(keepSilbeOutput[[1]], classes = c('character'))
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
