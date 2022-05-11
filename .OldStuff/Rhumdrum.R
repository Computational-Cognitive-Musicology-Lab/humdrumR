library(stringr)
library(MASS)
setwd("~/Bridge/Research/Programming/Rhumdrum")
source('~/Bridge/Research/Programming/Rhumdrum/hum_score_containslist.R')
source('~/Bridge/Research/Programming/Rhumdrum/hum_func.R')
source('~/Bridge/Research/Programming/Rhumdrum/BaseFunctions.R')
source('~/Bridge/Research/Programming/Rhumdrum/Tools.R')


# ----- define S4 class, the HUMDRUM object, which is a closure containing a hum_score object ----- #


# -------------------------

hum_recipfractions = function(frac) eval(parse(text=str_replace(frac,'%','/')))

# --------------------------Humdrum interpretation knowledge---------------------------------------------------------#



hum_knownExclusiveInterpretations = list(
  kernpitch = '(?<=^|[^A-Ga-g#-])([A-Ga-g])\\1*((#)*\\2*|(-)*\\3*)(?=$|[^A-Ga-g#-])',
  recip = '[1-9][0-9]*%?[1-9]?[0-9]*[.]*'
)

hum_knownTandemInterpretations=list(
  pitch=c(
    key='^\\*[A-Ga-g][-#b]*:',
    keysignature='^\\*k\\[([a-g][#-]* *)*\\]'
  ),
  notation=c(
    clef='^\\*clef[A-G][v]*[1-5]',
    metersignature='^\\*M[1-9][0-9]*/[1248(16)(32)]'
  ),
  rhythm=c(
    tempo='^\\*MM[0-9]+',
    timebase='^\\*tb[1-9][0-9]*'
  ),
  timbre=c(
    instrument='^\\*I[^C]+',
    instrumentclass='\\*IC.*',
    transposinginstrument='^\\*ITr.*'
  )
)



##----------------------------------




test = hum_file2hum_score('Data/test.krn')





