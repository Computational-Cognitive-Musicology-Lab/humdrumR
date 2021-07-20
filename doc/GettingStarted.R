## ---- include = FALSE----------------------------------------------------
knitr::opts_knit$set(root.dir = humdrumR::humdrumRroot)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ------------------------------------------------------------------------
library(humdrumR)

?humdrumR


## ------------------------------------------------------------------------
library(humdrumR)

setwd(humdrumRroot)

dir('HumdrumData')


## ------------------------------------------------------------------------

dir('HumdrumData/BachChorales')


## ------------------------------------------------------------------------

readHumdrum('HumdrumData/BachChorales/chor001.krn') -> chor1


## ------------------------------------------------------------------------

chor1


## ------------------------------------------------------------------------
readHumdrum('HumdrumData/BachChorales/chor0') -> chorales

## ------------------------------------------------------------------------

chorales


## ----echo=FALSE, results='none'------------------------------------------
ex1 <- readLines('examples/BasicExample.krn')

rest <- ex1[-1]
tokens <- c(ex1[1], unlist(strsplit(rest, split = '\t| ')))

##
ex1df <- as.data.frame(t(stringi::stri_list2matrix(strsplit(rest, '\t'))), stringsAsFactors = FALSE)

cat(' ', sep = '', ex1[1], '\n')
apply(format.data.frame(ex1df, justify = 'left', width = 30), 1, 
      function(x) cat(' ', x, sep = '', '\n')) 



## ----echo = FALSE--------------------------------------------------------

printquoted <- function(ex) {
quoted <- ex
    quoted[] <- lapply(quoted,
                   function(col) {
                       col <- strsplit(col, split = ' ')
                       col <- lapply(col,  function(x) paste0('<', x, '>'))
                       sapply(col, paste, collapse = ' ')
                       
                   })


cat('<', ex1[1], '>', '\n', sep ='')
apply(format.data.frame(quoted, justify = 'left', width = 30), 1, 
      function(x) cat(x, sep = '', '\n'))
    invisible(NULL)
}

printquoted(ex1df)


## ------------------------------------------------------------------------
setwd(humdrumRroot)

example1 <- readHumdrum('examples/BasicExample.krn')

example1

## ------------------------------------------------------------------------

humtab <- getHumtab(example1)

humtab


## ------------------------------------------------------------------------
colnames(humtab)


## ------------------------------------------------------------------------
fields(example1)

## ------------------------------------------------------------------------
fields(chorales)


## ------------------------------------------------------------------------

example1$Spine

# example1$Record

example1$Type

example1$Filename


## ------------------------------------------------------------------------
stops <- readHumdrum('examples/Stops.krn')

stops


## ------------------------------------------------------------------------
getHumtab(stops)


## ------------------------------------------------------------------------

paths1 <- readHumdrum('examples/Paths.krn')

paths1

paths1$Path

getHumtab(paths1)


## ------------------------------------------------------------------------

paths2 <- readHumdrum('examples/Paths2.krn')

paths2

paths2$Path

getHumtab(paths2)




## ------------------------------------------------------------------------

~ Spine + File

~ nchar(Token)

~ paste0(Token, Record)


## ------------------------------------------------------------------------

chorales %hum>% ~ Spine + File

chorales %hum>% ~ nchar(Token)

chorales %hum>% ~ paste0(Token, Record)


## ------------------------------------------------------------------------

chorales %hum>% ~ as.semit(Token)


## ------------------------------------------------------------------------

chorales %hum>% ~ as.semit(Token) -> chorales$Semits


## ------------------------------------------------------------------------

chorales$Token

chorales$Semits


## ------------------------------------------------------------------------

chorales %hum>% ~ paste0(Token, ' = ', Semits)



