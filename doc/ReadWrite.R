## ---- include = FALSE----------------------------------------------------
knitr::opts_knit$set(root.dir = humdrumR::humdrumRroot)
knitr::opts_chunk$set(message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(humdrumR)

?readHumdrum
?writeHumdrum


## ------------------------------------------------------------------------
library(humdrumR)
setwd(humdrumRroot)


## ------------------------------------------------------------------------

dir('HumdrumData', recursive = TRUE)


## ------------------------------------------------------------------------
library(humdrumR)
setwd(humdrumRroot)

readHumdrum('HumdrumData/BachChorales/chor.*.krn')


## ------------------------------------------------------------------------
readHumdrum('HumdrumData/.*Variations/.*.krn')

## ------------------------------------------------------------------------

readHumdrum('HumdrumData/.*/.*')


## ----eval = FALSE--------------------------------------------------------
#  readHumdrum("HumdrumData/BachChorales/chor.*.krn")
#  
#  readHumdrum("HumdrumData", "BachChorales", "chor.*.krn")

## ------------------------------------------------------------------------
readHumdrum(c('HumdrumData/BeethovenVariations/.*.krn', 'HumdrumData/MozartVariations/.*.krn'))

## ------------------------------------------------------------------------
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn')

## ----eval = FALSE--------------------------------------------------------
#  "HumdrumData/BeethovenVariations/.*.krn"

## ----eval = FALSE--------------------------------------------------------
#  "HumdrumData/MozartVariations/.*.krn"

## ------------------------------------------------------------------------
readHumdrum('HumdrumData',
            c(Rap  = 'Rap/.*',
              Rock = 'RollingStone/.*')) -> rapAndRock

## ------------------------------------------------------------------------
readHumdrum('HumdrumData', 'BachChorales', 'chor.*.krn',
            contains = '\\*[a-g][-b#]*:') 

## ------------------------------------------------------------------------
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn',
            reference = 'COM')

## ------------------------------------------------------------------------
readHumdrum('HumdrumData',
            c('BeethovenVariations', 'MozartVariations'),
            '.*.krn',
            reference = c(Composer = 'COM'))

## ------------------------------------------------------------------------
knownInterpretations[Type == 'Tandem'] 

