


# -------------------------

# --------------------------Humdrum interpretation knowledge---------------------------------------------------------#

lookupInterpretation = function(interp, type = 'Exclusive'){
  knowns = unlist(Interpretations[[type]])
  Names = gsub('.*\\.', '', names(knowns))

  regexes = knowns[Names == gsub('\\*\\*', '', interp)]
  if(any(grepl('\\*\\*', regexes))) {
    subregexes = strsplit(split = 'BREAK', gsub('\\*\\*', 'BREAK**', regexes))[[1]][-1]
    regexes = knowns[Names %in% gsub('\\*\\*', '', subregexes)]
    regexes = regexes[order(gsub('.*\\.', '', names(regexes)))][order(subregexes)]
  }

  names(regexes) = gsub('.*\\.', '**', names(regexes))

  regexes
}

knownExclusiveTokens = list(
    meta = c(kern = c('**recip**kernpitch**kernbeams')),
    pitch = c(kernpitch = '(?<=^|[^A-Ga-g#-])([A-Ga-g])\\1*((#)*\\2*|(-)*\\3*)(?=$|[^A-Ga-g#-])'),
    notation = c(kernbeams = '[LJ][\\/]*'),
    rhythm = c( recip = '\\[?[1-9][0-9]*\\.*(%[1-9]?[0-9]*\\.*)?|\\]'), #need to improve this to catch ends of ties?
    timbre = c()
)

knownInterpretations = list(
  Exclusive = c('^\\*\\*'),
  Tandem = list(
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
      instrument='^\\*I[^C"]+',
      instrumentx='^\\*I"[^C]+',
      instrumentclass='\\*IC.*',
      transposinginstrument='^\\*ITr.*'
      )
    )
)



##----------------------------------







