################Documentation for whole package.

#' MCFlow: the Musical Corpus of Flow
#'
#' The MCFlow package includes the entire Musical Corpus of Flow,
#' a corpus of digital transcriptions of racThe current version of
#' MCFlow contains 124 songs, by 47 artists.
#'
#' @section MCFlow datasets:
#' Upon loading this package, three data objects are lazily loaded:
#' \enumerate{
#' \item The object \code{\link{FlowData}} is a list object containing each
#' of MCFlow's current 124 songs, each encoded as an object of class Rap.
#' The names(FlowData) correspond to each Artist_Song in the corpus
#' in CamelCase with no spaces, and no apostrophes.
#' Thus, Ja Rule's "Livin' It Up'" can be accessed with the command
#' FlowData[['JaRule_LivinItUp']].
#' \item The object \code{\link{FlowCorpus}} is a single data frame with
#' all the verses of all songs simply stacked on top of each other.
#' This is ideal for analyzing or generalizing across the whole corpus.
#' \item The object \code{\link{CorpusData}} is a data.frame object with
#' information about each of the 124 songs in the current MCFlow.
#' }
#' @section MCFlow functions:
#' A useful function in the MCFlow package is the function \code{\link{Corpus_Subset}}.
#' Corpus_Subset can be used to quickly find subsets of the \code{\link{FlowCorpus}} object
#' based on a variety of criteria. In addition, the MCFlow package includes
#' five visualization functions: \code{\link{plot.Rap}} (which makes flow diagrams of single
#' verses), \code{\link{plot_Speed}} (which plots histrograms of rapped syllable speeds),
#' \code{\link{plot_Metric}} (which plots the metric distribution of various syllable layers),
#' \code{\link{plot_Phrase}} (which plots the metric position of phrases in the meter), and
#' \code{\link{plot_Rhyme}} (which plots the 1st-order relationships between the metric positions
#' of antecedent/consequent rhymes).
#'
#' @docType package
#' @name MCFlow
NULL


######Documentation for Rap class.
#' The Rap S3 class
#'
#' The class of object of each MCFlow transcription.
#' A list of data.frames. Each data.frame represents one verse in the rap song.
#' The \code{\link{FlowData}} object contains all the current MCFlow Rap objects.
#'
#' @section Format:
#' Each verse's data.frame includes a row for each syllable (and rest) in the verse, with
#' 22 variables. The first nine variables are direct representations of the nine spines in each
#' MCFlow Humdrum-syntax text file. The next seven columns represent data that is stored in
#' Humdrum inerpretaton and reference records. The next six columns are various useful transformations
#' of the data in the first column.
#' \describe{
#'   \item{recip}{The MCFlow Humdrum-syntax **recipx spine. Tokens representing the duration of each event.}
#'   \item{stress}{The MCFlow Humdrum-syntax **stress spine. Tokens representing syllables' stress, as either 1 stressed, or 0 unstressed.}
#'   \item{tone}{The MCFlow Humdrum-syntax **tone spine. Tokens prosodic accents and boundary tones.}
#'   \item{breaks}{The MCFlow Humdrum-syntax **break spine. Tokens represent prosodic boundaries, from weakest 1 to strongest 4.}
#'   \item{melody}{The MCFlow Humdrum-syntax **melody spine. Tokens represnting prosodic (especially pitch) paralellism in flow.}
#'   \item{rhyme}{The MCFlow Humdrum-syntax **rhyme spine. Tokens "two-dimmesional" rhyme annotations, with roman letters marking syllable parallelism and parenthesis grouping rhymed syllables together.}
#'   \item{ipa}{The MCFlow Humdrum-syntax **ipa spine. Tokens represent rests (R) or International Phonetic Alphabet spellings of syllables' pronounciation.}
#'   \item{lyrics}{The MCFlow Humdrum-syntax **lyrics spine. Tokens rap's lyrics, with multisyllable words connected by - (dash) and acronyms and noun phrases connected by _ (underscore).}
#'   . (period) , (comma) ; (semicolon) and | (pipe) represent various syntactic boundaries. : (colon) and {} indicate semantic relationshins.
#'   \item{hype}{The MCFlow Humdrum-syntax **hype spine. Tokens hype (backing rap) in the song. recipx ipa and stress data jammed into each token.}
#'   \item{Year}{From MCFlow Humdrum-syntax reference records: the year (including decimal date) the song peaked on Billboard's \emph{Hot100} (or the year it was released if it never appeared on Billboard).}
#'   \item{Song}{From MCFlow Humdrum-syntax reference records: the song's name.}
#'   \item{Artist}{From MCFlow Humdrum-syntax reference records: the commcercially releasing artist.}
#'   \item{Emcee}{From MCFlow Humdrum-syntax comment data: The emcee rapping at any given time.}
#'   \item{BPM}{From MCFlow Humdrum-syntax interpretation data: The tempo in Beats Per Minute at each momment.}
#'   \item{Measure}{From MCFlow Humdrum-syntax barline data: The measure number (within verses) of each event. Measure 0 is a pickup measure.}
#'   \item{Section}{From MCFlow Humdrum-syntax interpretation data: The current section within the song (Verse1, Verse2 etc).}
#'   \item{dur.16}{Numeric. The event duration in 16th-notes.}
#'   \item{dur.ms}{Numeric. The event duration in milliseconds.}
#'   \item{ioi.16}{Numeric. The syllable's inter-onset-interval in 16th-notes.}
#'   \item{ioi.ms}{Numeric. The syllable's inter-onset-interval in milliseconds.}
#'   \item{syl.per.sec}{Numeric. The number of syllables rapped in the one-second window beginning with the current syllable.}
#'   \item{beat}{Numeric. The metric position (in 16th-notes) within the 4/4 measure.}
#' }
#'
#' @docType class
#' @name Rap
NULL


###LOAD DATA (and document it)

load('data/CorpusData.RData')
#' Data about each song in MCFlow.
#'
#' General information about each song in the current Musical Corpus of Flow.
#'
#' @name CorpusData
#' @docType data
#' @format A data frame with 124 rows and 8 variables:
#' \describe{
#'   \item{Artist}{The name of the song's commercially releasing artist.}
#'   \item{Song}{The name of the song.}
#'   \item{ReleaseData}{The date the song was released.}
#'   \item{BillboardPeak}{The song's peak position on Billboard's \emph{Hot100}.}
#'   \item{BillboardPeakData}{The date the song reached it's Billboard peak.}
#'   \item{Emcees}{A character string representing the emcees who rap in each verse of the song, with each verse separated by a comma.}
#'   \item{N.Verses}{The number of verses in the song.}
#'   \item{BPM}{The beats per minute tempo of each verse in the song, in a character string separated by commas.}
#' }
"CorpusData"


load('data/FlowCorpus.RData')
#' MCFlow data in single object.
#'
#' A single data frame containing all MCFlow data stacked on top of each other.
#'
#' @name FlowCorpus
#' @docType data
#' @format A data frame with 72,801 rows and 22 variables. The 22 variables are the same
#' as those found in an object of class \code{\link{Rap}}.
"FlowCorpus"

load('data/FlowData.RData')
#' MCFlow data, by song.
#'
#' A list of objects of class \code{\link{Rap}}. Each object contains the complete transcription of
#' a song in MCFlow.
#'
#' @name FlowData
#' @docType data
#' @format A list of objects of class Rap. Each object contains the complete transcription of
#' a song in MCFlow. The names(FlowData) are the artists' name and song name, seperated by a _
#' (underscore), with no spaces, commas, or apostrophes, in CamelCase. For instance: JaRule_LivinItUp.
"FlowData"




Corpus_Subset=function(
  artists=NA,
  emcees=NA,
  songs=NA,
  verses=1:10,
  bpms=c(60,150),
  years=1980:2014,
  remove.hype=T,
  remove.rests=F){
  #' Subsets of the MCFlow.
  #'
  #' This function extracts a subset from the globally defined FlowCorpus data object.
  #' @param artists A vector of regex character strings, each representing an artist,
  #'  meaning the official commercial artist of the recording, to include in the subset.
  #'   Defaults to all. Information about what artists are in the current corpus can be
  #'   found in the \code{\link{CorpusData}} object.
  #' @param emcees A vector of regex character strings, each representing an emcee to include
  #' in the subset. Defaults to all. Information about what emcees are in the current corpus can be
  #'   found in the \code{\link{CorpusData}} object.
  #' @param songs A vector of regex character strings, each representing an song to include in
  #' the subset. Defaults to all. The list of songs in the current corpus can be
  #'   found in the \code{\link{CorpusData}} object.
  #' @param years A numeric vector of years to include in the sample. Defaults to all 1980-2014.
  #' @param verses A vector of integers, specifying which verses to include in the subset. Defaults to all, i.e. 1-10.
  #' @param bpms A 2-length numeric vector indicating the range of accompaniment bpms to include in the susbet. Defaults to all, which is captured by the range (60, 150)
  #' @param remove.hype A single logical value. If TRUE, all purely hype data records are removed from output.
  #' @param remove.rests A single logical value. If TRUE, all rests are removed from output.
  #' @return Returns a subset of the \code{\link{FlowCorpus}} data object, which is a data.frame with
  #' the same variables as any object of class \code{\link{Rap}}.
  #' @export Corpus_Subset
  #' @examples To get the data from Eminem rapped after 2005:
  #' Corpus_Subset(emcees='Eminem',years=2005:2014)

  #years
  year.ind=which(floor(FlowCorpus$Year) %in% years)

  #bpms
  bpm.ind=which(FlowCorpus$BPM >= bpms[1] & FlowCorpus$BPM <= bpms[2])

  #emcees
  if(any(is.na(emcees))) emcee.ind=1:nrow(FlowCorpus) else { ###Get indexes of target emcees
    emcee.ind=c()
    for(i in 1:length(emcees)){ emcee.ind=c(emcee.ind,grep(emcees[i],FlowCorpus$Emcee)) }
    emcee.ind=unique(emcee.ind)
  }

  #artists
  if(any(is.na(artists))) artist.ind=1:nrow(FlowCorpus) else { ###Get indexes of target artists
    artist.ind=c()
    for(i in 1:length(artists)){ artist.ind=c(artist.ind,grep(artists[i],FlowCorpus$Artist)) }
    artist.ind=unique(artist.ind)
  }

  #songs
  if(any(is.na(songs))) song.ind=1:nrow(FlowCorpus) else { ###Get indexes of target artists
    song.ind=c()
    for(i in 1:length(artists)){ song.ind=c(song.ind,grep(songs[i],FlowCorpus$Song)) }
    song.ind=unique(song.ind)
  }

  #verses
  verse.ind=which(FlowCorpus$Section %in% paste('Verse',verses,sep=''))


  #####Find the intersection of all inds, and do the subset

  indexes=intersect(year.ind,intersect(bpm.ind,intersect(emcee.ind,intersect(artist.ind,intersect(song.ind,verse.ind)))))

  output=FlowCorpus[indexes,]
  if(any(remove.hype)) output=subset(output,ipa!='.')
  if(any(remove.rests)) output=subset(output,ipa!='R')

  return(output)
}


###################################################################################################
###########################################Plot Functions##########################################
###################################################################################################



plot.Rap = function(
	Rap,
	which.verse=1,
	line.range=NULL,
	rhythmic.layers='all',
	color.rhymes=T,
	metric.alignment=32,
	plot.by.phrase=F,
	phrase.break=c('2','3','4'),
	IPA=F,
	cex=.8
	){
  #' Flow diagramer
  #'
  #' This function makes a flow diagram of a given rap verse.
  #' @param Rap Object of class \code{\link{Rap}} (a list of data.frames).
  #' The desired rap song; The data object \code{\link{FlowData}} contains \code{\link{Rap}} objects.
  #' @param which.verse Numeric, length 1. Which verse of the rap to plot.
  #' @param line.range Numeric vector of length two, specifying the first and last lines,
  #' whether measures or phrases, to plot. If left NULL, then entire verse is plotted.
  #' @param rhythmic.layers Character. Determines which rhythmic layers should be plotted.
  #' Options are either 'all' 'stress' or 'rhymes'
  #' @param color.rhymes Logical. Should rhymes be colored?
  #' @param metric.alignment Metric distance at which successive lines are aligned in 32nd-note
  #' units. 32 = one measure, 16 two beats, etc..
  #' @param plot.by.phrase Logical. Plot by phrases instead of by measure?
  #' @param phrase.break Vector of character strings. If plot.by.phrase==T, which break
  #' annotations should be used to determine phrase boundaries? Options are '2', '3', or '4'.
  #' First letters of phrases are also capitalized, whether or not plot.by.phrase==T.
  #' @param IPA Print in the International Phonetic Alphabet instead of English orthography?
  #' @param cex Size of text.
  #' @keywords Flow Diagram Rhyme
  #' @return none
  #' @examples
  #' To plot the second verse of 2pac's "California Love"
  #' plot(FlowData[['2pac_CaliforniaLove]],which.verse=2,plot.by.phrase=T)
  #' @export plot.Rap


  if(class(Rap)!='Rap'){
    return('')
  } # If the input object is not of class Rap, return an error.

  par(family='DejaVu Serif') # Serifs are necessary for reading IPA symbols.

  Ref=attributes(Rap)[['Reference']] # Reference records...IE song metadata

# which.verse

  which.verse=floor(which.verse) # Coerce which.verse to a whole number, just in case a non-whole number is inputed.
  if(which.verse<1) which.verse=1 # if which.verse is below 1 (impossible) default to plotting the first verse.
  if(which.verse>length(Rap)) which.verse=length(Rap) # if the verse number is greater than the actual number of verses in the rap, default to the last verse of the song.

  tran=Rap[[which.verse]] # tran is the data.frame representing the desired verse.

# phrase.break
  if(!any(phrase.break %in% c('2','3','4'))) stop('Invalid phrase.break argument.')

# IPA
  if(!is.logical(IPA)) stop('Invalid IPA argument')

  Text=unlist(ifelse(IPA,list(tran$ipa),list(tran$lyrics))) # Grab either English orthography data or IPA data (unlist(list()) is necesarry because ifelse can't return vectors)
  Text[tran$breaks %in% phrase.break] = unlist(lapply(strsplit(Text[tran$breaks %in% phrase.break],''), function(w) {w[1]=toupper(w[1]) ; paste(w,collapse='')})) # Capitalize first letter in each phrase.

# rhymic.layers
  if(!rhythmic.layers %in% c('all','stress','rhymes')) stop('Invalid rhythmic.layers argument.')

  if(rhythmic.layers=='all') Font=suppressWarnings(as.numeric(tran$stress))+1 else Font=rep(2,nrow(tran)) # If rhythmic.layers=='all', Font vector will put stressed syllables in bold font.
  if(rhythmic.layers=='stress') Text[tran$stress=='0']='' # If rhythmic.layers=='stress' get rid of unstressed syllables.
  if(rhythmic.layers=='rhymes') Text[tran$rhyme=='.']='' # If rhythmic.layers=='rhymes' get rid of unrhymes syllables.


# cex
  if(!is.numeric(cex) | cex <= 0 | cex > 5) stop('Invalid cex argument.')

  Cex.vec=rep(cex,length(Text))
  Cex.vec[tran$Syl.stress=='0']=cex*.9

# color.rhymes
  if(!is.logical(color.rhymes)) stop('Invalid color.rhymes argument.')

  tran$rhyme=gsub(']','',gsub('[)\\({}]','',gsub('\\[','',tran$rhyme))) # Get rid of rhyme-unit grouping () and [].

  rhyme.colors=sample(rainbow(length(unique(tran$rhyme)),start=.1,end=1,v=1,s=1)) #Get a pallete of rhymes with a unique color for each rhyme.

  cols=rep('black',nrow(tran)) #Initialize cols vector. This will be a vector of colors to plot.
  if(color.rhymes){
  	uniq.rhymes=unique(tolower(tran$rhyme[tran$rhyme !='.'])) # Rhymes with same letter name get same color (because of tolower)
  	cols[tran$rhyme !='.']=rhyme.colors[.mod(match(tolower(tran$rhyme[tran$rhyme !='.']),uniq.rhymes),length(rhyme.colors))]
  }

# plot.by.phrase
  if(!is.logical(plot.by.phrase)) stop('Invalid plot.by.phrase argument.')

  ### Get emcee data. #This forms an emcee spine which indicates the rapping emcee at the side of the plot.
  if(plot.by.phrase) {emcee=tran$Emcee[tran$breaks%in%phrase.break]} else {emcee=tran$Emcee[seq(1,nrow(tran),32)]}
  emcee[which(head(emcee,-1)==tail(emcee,-1))+1]='' # Only keep data where emcee changes

# line.range
  if(!is.null(line.range)) if(length(line.range)>2 | length(line.range)<2 | any(!is.numeric(line.range)) | any(diff(line.range) <= 0)) stop('Invalid line.range argument')

  #######
  #####Need to get X and Y coordinates for each syllable, as well as XLIM and YLIM values
  #######
  tran$beat=((tran$beat-1)*2)+1 # This is because $beat is in 16th-note units, but the flow diagram works in 32nd-note units

  if(!plot.by.phrase) { # Plot by fixed metric duration.

    ####X coordinates
      X=.mod(1+(2*(head(cumsum(c(1,tran$dur.16)),-1)-1)),metric.alignment)
      XLIM=c(0,metric.alignment+1)

    ####Y coordinates
      Y=cumsum(c(1,as.numeric(sign(diff(X))==-1)))
      if(0%in%tran$Measure) Y=Y-1

    	if(is.null(line.range)) line.range=c(min(Y),max(Y)) #default to whole range
    	if(line.range[2]>max(Y)) line.range[2]=max(Y)
    	YLIM=rev(line.range)

  } else { # Plot by phrases.

    ####Y coordinates
      Y=rep(0,nrow(tran)); Y[tran$breaks %in% phrase.break]=1; Y=cumsum(Y) #Get Y from phrases this time
      Y[Y==0]=1 #no phrase zero

    	if(is.null(line.range)) line.range=c(min(Y),max(Y))#default to whole range
    	if(line.range[2]>max(Y)) line.range[2]=max(Y)
    	YLIM=rev(line.range)

    ####X coordinates
      X=1+(2*(head(cumsum(c(1,tran$dur.16)),-1)-1))

    	X[!tran$ipa %in% c('R','.')]=.find_best_phrase_alignment(X[!tran$ipa %in% c('R','.')],
    	                                                        Y[!tran$ipa %in% c('R','.')],metric.alignment)
    	XLIM=range(X[!tran$ipa %in%c('R','.')])
  }

########
#####Now we plot!
########

par(mar=c(5,7,5,0))

#setup plot

plot(-1,-1,xlim=XLIM,ylim=YLIM,type='n', cex.main=cex,
     axes=F,xlab='',ylab='',
     #,
     main=paste(c("'",Ref$OTL,"' by ",Ref$COC,'\n',
                  'MC(s): ',unique(tran$Emcee),
                  ' (',Ref$RRD,')'),collapse='')
     )

###Grid
par(xpd=F)
bottom=par('usr')[3]
top=par('usr')[4]

downbeats=c(-64,-31,1,33,65) ; downbeats=downbeats[downbeats>XLIM[1] & downbeats <XLIM[2]]
if(length(downbeats)>0) segments(x0=downbeats,x1=downbeats,col='grey80',lty='dotted',lwd=2, y0=bottom,y1=top,xpd=T)

beatsthree=c(-48,-15,17,49) ; beatsthree=beatsthree[beatsthree>XLIM[1] & beatsthree <XLIM[2]]
if(length(beatsthree)>0) segments(x0=beatsthree,x1=beatsthree,col='grey80',lty='dotted',lwd=1.2, y0=bottom,y1=top,xpd=T)

twonfour=seq(-63+8,65-8,16) ; twonfour=twonfour[twonfour>XLIM[1] & twonfour <XLIM[2]]
if(length(twonfour)>0) segments(x0=twonfour,x1=twonfour,col='grey80',lty='dotted',lwd=1, y0=bottom,y1=top,xpd=T)


##X axis
par(xpd=T)
  mtext('Metric position',1,line=3,cex=2)

  xaxis.lab=rep(unlist(strsplit(paste(1:4,' e & a ',sep=''),split='')),3)
  axis(1,XLIM[1]:XLIM[2],xaxis.lab[.mod(XLIM[1]:XLIM[2],metric.alignment)],tick = F,cex.axis=cex*.8,font=3)
  axis(3,XLIM[1]:XLIM[2],xaxis.lab[.mod(XLIM[1]:XLIM[2],metric.alignment)],tick = F,cex.axis=cex*.8,font=3)


##Y axis

left=par('usr')[1]
text(labels=ifelse(plot.by.phrase,'Phrase','Measure'),y = top+((top-bottom)/length(Y)),
     x=left-2,cex=.6)

Ylabels=rev(unlist(ifelse(plot.by.phrase,
                          list(c(line.range[1]:line.range[2])),
                          list(as.numeric(tran$Measure[c(T,(sign(diff(tran$beat))==-1))])))))#this gets measure numbers from file

text(left,line.range[2]:line.range[1],adj=c(1,0),cex=1,#phrase labes on y axis
     labels =Ylabels) #this is because I can't get axis to allign right with the text

##emcee labels
Y.emcee=tran$Emcee[as.logical(c(1,diff(Y)))][unique(Y)>=line.range[1] & unique(Y)<= line.range[2]] #emcee at beginning of each measure within the line.range
Y.emcee[1+which(head(Y.emcee,-1)==tail(Y.emcee,-1))]=''

mtext(at = top+((top-bottom)/length(Y)),text='Emcee',adj=1,side=2,line=3,cex=1,las=2)
mtext(at = unique(Y)[unique(Y)>=line.range[1] & unique(Y)<= line.range[2]],
      text = Y.emcee,side = 2,las=2,line=2.5,padj = 0,cex=1)
par(xpd=F)


###finally get rid of stuff we don't want to plot
Text[Text=='.']='' # Get rid of null data tokens
Text[Text=='R']='' # Get rid of rests.
##Now finally plot the lyrics!
points(X[Text!=''],-.05+Y[Text!=''],pch=15,
col='white',
cex=Cex.vec[Text!='']*3)#this paints over the grid lines
text(X,Y,
     adj=c(.5,0),cex=Cex.vec,
     col=cols,
     labels=Text,font=Font)

} # End of plot.Rap function

####The following functions are need specifically by plot.Rap

.find_best_phrase_alignment = function(beat,Y,metric){
  #' Finds most compact alignment of phrases in diagram.
  #'
  #' This function returns the X coordinates for each syllable that result in the most compact plot.by.phrase=T flow diagram.
  #' @param tran The cumulative X coordinates.
  #' @param Y The Y coordinates.
  #' @param The metric alignment number.
  #' @return A vector of X coordinates.

  Xs=tapply(beat,Y,function(x) x-(floor(x[1]/metric)*metric))

  Xs=lapply(Xs,function(x) {
    Rskew=max(x)-metric+1##this is the ammount the phrase currently sticks to the right
    Lskew=1-(min(x)-metric) ##this is the ammount the phrase would stick to the left, IF we subtracted metric

    if(Lskew>Rskew) x=x+metric
    x})
  unlist(Xs)
}

#######################################################################################

plot_Speed=function(
  Data=FlowCorpus,
  unit='SyllablesPerSecond',
  exclude.tails=c(0,ifelse(unit=='SyllablesPerSecond',1,.97)),
  interquantile=95,
  freq=F){
  #' Distribution of syllable by speed
  #'
  #' This plots a histogram of given Corpus subset's syllable speed.
  #' @param Data The \code{\link{FlowCorpus}} data.frame or a subset of it.
  #' @param unit Single character string, What kind of durations measures to plot. Options are SyllablesPerSecond, msIOIs, 16thIOIs
  #' @param freq Logical. If freq=T, total counts are shown, else proportion of total are shown.
  #' @param exclude.tails A 2-length numeric vector. This is used to screen out outliers. The first value in the vector is the lower (left) quantile, below which to exlude. The second value is the upper (right), above which to exclude.
  #' @param interquantile A single number between 0 and 100. This marks the middle quantiles% of data under the plot.
  #' @export plot_Speed
  #' @return none
  #' @examples
  #' #To see the histogram of syllables per second delivered by Big Boi.
  #' plot_Speed(Corpus_Subset(emcees='Big Boi'))
  #'
  #'
##################
###ARGUMENT CHECKS
##################

  if(nrow(Data)==0) stop('Data is empty.')

#unit
  if(!unit %in% c('SyllablesPerSecond','msDurations','msIOIs')) stop("Invalid unit argument. Must be SyllablesPerSecond, msDurations, or msIOIs")

#quantile
  if(interquantile <0 | interquantile >100) stop('Invalid quantile argument. Must be between 0 and 100')

####################
## PREPARE DATA
#####################

  data=Data[[switch(unit,
                    'SyllablesPerSecond'='syl.per.sec',
                    'msDurations'='dur.ms',
                    'msIOIs'='ioi.ms')]]
  data=data[!Data$ipa%in%c('.','R')]

#exclude data that is too extreme, if indicated by exclude.tails
  quants=quantile(data,probs = exclude.tails,na.rm=T)
  data=data[data>quants[1] & data<quants[2]]

Breaks=switch(unit,
                       'SyllablesPerSecond'=seq(0,length.out=200,by=1),
                       'msDurations'=seq(0,length.out=200,by=50),
                       'msIOIs'=seq(0,length.out=200,by=50))

Breaks=Breaks[1:(which(Breaks>max(data,na.rm=T))[1])]

H=hist(data,plot=F,breaks=Breaks)


#Y axis info
YMAX=max(H[[ifelse(freq,'counts','density')]],na.rm = T)
clean.maxes=c(0,.005,.01,.012,.015,.02,.03,.04,.05,.06,.075,.08,.1,.15,.2,.25,.3,.35,.4,.5,.6,.75,.8,1,
              2,5,10, 15,20,25,30 ,35,40, 50,60,75,100,120,125,
              200,250,300,400,500,600,750,800,1000,1200,1250,1500,2000,3000,2500,3000,3500,4000,5000,6000,7500,8000,10000,
              12000,15000,20000,25000,30000,50000,60000,75000,80000,100000,150000,200000)


difs=clean.maxes-(YMAX) # differences between "clean.maxes" and actual maximum in data
YAXES.MAX=clean.maxes[difs>0][which.min(difs[difs>0])] #find the closest clean.maxes which is greater thant YMAX

##################
#####NOW WE PLOT##
##################

hist(data,col = 'grey70',border = F,axes=F,ylim=c(0,YAXES.MAX),breaks=Breaks,main='Distribution of Syllables \n by Speed',ylab=ifelse(freq,'Number of Syllables','Proportion of Syllables'),
       freq=freq,
       xlab=switch(unit,##X label depends on unit
                   'SyllablesPerSecond'='Syllables per Second',
                   'msDurations'='Durations (ms)',
                   '16thDurations'='Durations (16th-notes)',
                   'msIOIs'='Inter Onset Intervals (ms)',
                   '16thIOIs'='Inter Onset Intervals (16th-notes)'))

axis(2,seq(0,YAXES.MAX,length.out=6),labels = unlist(ifelse(freq,
                                                            list(seq(0,YAXES.MAX,length.out=6)),
                                                            list(paste(round(seq(0,YAXES.MAX,length.out=6),4)*100,'%',sep='')))),
     las=2,cex.axis=1.6,tick=F,line=-3)


axis(1,Breaks,tick=F)

#quantile arrows under plot
if(unit=='SyllablesPerSecond' & interquantile>=1){

  quantiles=c(.5-interquantile/200,.5+interquantile/200) #translate the interquantile range to symetrical quantiles
  quants=quantile(data,probs = quantiles,na.rm=T)

  median=quantile(data,probs=.5,na.rm=T)

  arrows(x1=quants[1],x0=quants[2],par('usr')[3],par('usr')[3],angle=50,length = .1,lwd=3,xpd=T,code=3,col='red')

  ##PLOT middle interquantile% on line, or above it?
  if(diff(quants)>1.2) {
    segments(y0=par('usr')[3],y1=par('usr')[3],lwd=4,col='white',x0=median-.5,x1=median+.5,xpd=T)#this whites out the middle portion of the arrow
    shift.median.text=0
  } else shift.median.text=par('usr')[3]*.65

  text(median,par('usr')[3]-shift.median.text,paste(diff(quantiles)*100,'%',sep=''),xpd=T,col='red',cex=.8)
  text(quants[1],par('usr')[3],round(quants[1],1),xpd=T,adj = 1.3,cex=.7,col='red')
  text(quants[2],par('usr')[3],round(quants[2],1),xpd=T,adj = -.3,cex=.7,col='red')

  }#end of if unit==SyllablePerSecond
}#end of plot_Speed function


#######################################################################################

plot_Metric=function(
                    Data=FlowCorpus,
                    rhythmic.layers=c('unstressed','stressed','rhymed'),
                    metric=16,
                    measures=1,
                    freq=F){
  #' Plot syllables' metric distributions.
  #'
  #' This plots the relative proportion of syllables to land in various metric positions.
  #' @param Data The \code{\link{FlowCorpus}} data.frame or a subset of it.
  #' @param rhythmic.layers Character vector, of length 1-3. Options are unstressed
  #' stressed or rhymed...any combination of these three is acceptable
  #' @param metric Numeric, the durational value to which all durations are rounded.
  #' Defaults to 16th note. Other possibilites are 32 and 8 and 4.
  #' @param measures The number of measures to show distributions across. Acceptable
  #' options are .5, 1, and 2
  #' @param freq Logical. If freq=T, total counts are shown, else proportion of total are shown.
  #' @export plot_Metric
  #' @return none
  #' @examples
  #' #To see the metric disitribution of rhymed syllables Young M.C.'s first verses.
  #' plot_Metric(Corpus_Subset(artists='Young',verses=1),rhythmic.layers='rhymed')
  #'

##################
##ARGUMENT CHECKS
#################


  if(nrow(Data)==0) stop('Data is empty.')

#measures
  if(!measures %in% c(.5,1,2)) stop('Invalid measures argument.')

#metric
  if(!metric %in% c(16,32,8,4)) stop('Invalid measures argument.')
  metrat=metric/16
  Data$beat=floor(((Data$beat-1)*metrat)+1)


#rhythmic.layers
  if(length(rhythmic.layers)<1 | length(rhythmic.layers)>3) stop('Invalid rhythmic.layers argument')
  if(any(!rhythmic.layers %in% c('stressed','unstressed','rhymed'))) stop('Invalid rhythmic.layers argument')


#################
##PREPARE DATA
#################
  if(measures==2){##need to redo beat with a larger window
     Data=do.call('rbind',by(Data,paste(Data$Song,Data$Section),function(Sec){
      durs=Sec$dur.16
      durs[is.na(durs)]=0
      newbeat=.mod(head(cumsum(c(1,durs)),-1),32)

      newbeat[is.na(Sec$dur.16)]=NA
      Sec$beat=newbeat
      Sec
    }))
  }

  if(measures==.5){## simply need to mod beat
    Data$beat=.mod(Data$beat,8)
  }



  inds=c()
  if(any(rhythmic.layers=='stressed')) inds=c(inds,which(Data$stress=='1'))
  if(any(rhythmic.layers=='unstressed')) inds=c(inds,which(Data$stress %in% c('0','1')))
  if(all(c('rhymed','stressed') %in% rhythmic.layers) | all(rhythmic.layers=='rhymed')) inds=c(inds,which(Data$stress=='1' & Data$rhyme!='.')) # is stressed is included or if rhyme is alone, only look at stressed rhymes
  if('rhymed' %in% rhythmic.layers & !'stressed' %in% rhythmic.layers) inds=c(inds,which(Data$rhyme!='.')) # if rhyme and unstressed are requested, but not stressed, look at all rhymed syllables (inlcuding unstressed)

  Data=Data[unique(inds),]

Data=subset(Data,!ipa %in% c('R','.'))


#######Make tables

possible.positions=unique(floor((((1:(metric*measures))-1)))+1) ##need to include all possible positions in tables, because they might be missing from data

unstressed.tab=stressed.tab=rhymed.tab=NA # initialize

if('unstressed' %in% rhythmic.layers){
  unstressed.tab=(table(c(Data$beat,possible.positions))-1)
}

if('stressed' %in% rhythmic.layers){
  stressed.tab=(table(c(Data$beat[Data$stress=='1'],possible.positions))-1)
}

if('rhymed' %in% rhythmic.layers){
  rhymed.tab=(table(c(Data$beat[Data$rhyme!='.'],possible.positions))-1)
}

######STUFF REGARDING Y AXES AND YLIM

denominator=ifelse(freq,1,nrow(Data)) # needed if freq=F
YMAX=max(c(unstressed.tab,stressed.tab,rhymed.tab),na.rm = T)
clean.maxes=c(0,.01,.02,.03,.04,.05,.06,.075,.08,.1,.15,.2,.25,.3,.35,.4,.5,.6,.75,.8,1,
              2,5,10, 15,20,25,30 ,35,40, 50,60,75,100,
              200,300,400,500,600,750,800,1000,1500,2000,3000,2500,3000,3500,4000,5000,6000,7500,8000,10000,
              20000,30000,50000,60000,75000,80000,100000,150000,200000)


difs=clean.maxes-(YMAX/denominator) # differences between "clean.maxes" and actual maximum in data
YAXES.MAX=clean.maxes[difs>0][which.min(difs[difs>0])] #find the closest clean.maxes which is greater thant YMAX

#######################
########NOW WE PLOT
#######################

#blankplot
xcoor=barplot(table(possible.positions)-1,xlab='Metric Position',ylab=ifelse(freq,'Number of Syllables','Proportion of Syllables'),
              ylim=c(0,YAXES.MAX),axes=F,names.arg = F)

#X-axis stuff
metric.positions=switch(as.character(metric),
                        '16'=unlist(strsplit(paste(1:4,c('e&a'),sep=''),split='')),
                        '32'=unlist(strsplit(paste(1:4,c('+e+&+a+'),sep=''),split='')),
                        '8'=unlist(strsplit(paste(1:4,c('&'),sep=''),split='')),
                        '4'=c('1','2','3','4'))


if(measures==2) metric.positions=rep(metric.positions,2)
if(measures==.5) metric.positions=metric.positions[1:(length(metric.positions)/2)]

axis(1,xcoor,metric.positions,tick=F)

#Y-axis stuff

roundto=ifelse(freq,0,3)
yat=round(seq(0,YAXES.MAX,length.out=6),roundto)
ylabs=yat
if(!freq) ylabs=paste(ylabs*100,'%',sep='')
axis(2,yat,ylabs,las=2,tick=F,line=-1)


#plot actual data

if(all(!is.na(unstressed.tab))) barplot(unstressed.tab/denominator,add=T,names.arg=F,axes=F,border=F)
if(all(!is.na(stressed.tab))) barplot(stressed.tab/denominator,add=T,names.arg=F,axes=F,
                                      border=F,density=15,lwd=1,col='black')
if(all(!is.na(rhymed.tab))){
  if(all(rhythmic.layers=='rhymed')){ #plot barplot if only rhymes
    barplot(rhymed.tab/denominator,add=T,names.arg=F,axes=F,
            border=F,lwd=1,col='red')
  } else{#plot as lines if rhymes are only part
    points(xcoor,rhymed.tab/denominator,type='h',col='red',lwd=5)
  }
}

legend('topright',xpd=T,inset=c(0,-.1),legend = c('Surface','Stressed','Rhymed'),border=T,bty='n',cex=1.2,
       fill= c('grey70','black','red'),horiz = T,density=c(100,20,100))

}#end of plot_Metric



plot_Rhyme=function(
                    Data=FlowCorpus,
                    conditional=F,
                    within.chain=T,
                    rhyme.distance=1,
                    freq=F){
  #' Plot first-order metric position of rhymes.
  #'
  #' This plots the relative probability (either joint or conditional) of consequent rhymes appearing on a given beat given an antecedent rhymes on a given beat.
  #' @param Data The \code{\link{FlowCorpus}} data.frame or a subset of it.
  #' @param conditional Single logical value. If true, conditional probabilities (given
  #' antecedent rhyme) are shown. If false, the joint probability is shown.
  #' @param within.chain Logical, length 1. If true, only rhyme relationships within a rhyme
  #' chain (sounds that actually rhyme with each other) are counted.
  #' Else, relationships between consecutive first-stressed-syllables in any rhyme are counted.
  #' @param rhyme.distance Vector of numbers. Controls how close together in chain rhymes
  #' must be. For instance, rhyme.distance==1 means that only adjacent rhymes in chain are
  #' shown, while 2 means that everyother rhyme in chain is compared. Can be any or all values
  #'  in the range 1:10.
  #' @param freq Logical. If freq=T, total counts are shown else proportion of total are
  #'  shown (this only affects plot key). Conditional plots can't be freq (currently).
  #' @return none
  #' @export plot_Rhyme
  #' @examples
  #' To plot 1st-rhyme patterns in songs released by Kurtis Blow: plot_Rhyme(Corpus_Subset(artists="Kurtis"))
  #'

##################
###ARGUMENT CHECKS
##################

  if(nrow(Data)==0) return('')


  #conditional
  if(length(conditional)>1 | any(!is.logical(conditional))) conditional=F #coerce to logical

#rhyme.distance
  if(any(!is.numeric(rhyme.distance))) rhyme.distance=1:length(rhyme.distance) #coerce to numeric
  rhyme.distance=round(rhyme.distance) #coerce to whole numbers
  rhyme.distance[rhyme.distance<0]=1 # coerce to positive non-zero
  rhyme.distance[rhyme.distance>10]=1 # coerce to less than 11
  rhyme.distance=sort(unique(rhyme.distance)) #get rid of redundant


#freq
  if(length(freq)>1 | any(!is.logical(freq))) freq=F #coerce to logical
  if(conditional) freq=F #can't have freq in conditional plot

#
Rdata=subset(Data,rhyme!='.' & !ipa%in%c('R','.') & stress=='1' ) #exclude unstressed syllables

#################
#####Prepare Data
#################

#first separate 'two-dimmensions' of rhyme annotation.


Rdata$Rhymeletters=gsub('[^A-Za-z][^A-Za-z]*','',Rdata$rhyme) #one dimension is the letter names
Rdata$Rhymestarts=grepl('\\(',Rdata$rhyme) #the dimensions are the parenthesis which connect syllables into multi-syllable rhyme links
Rdata=subset(Rdata,!grepl('[a-z]',Rdata$Rhymeletters)) #get rid of weird unstressed rhyme letters (unusual cases...not sure what would happen if they're included)
Rdata=subset(Rdata,Rhymeletters!='') # these are places where a ( marks the beginning of a rhyme unit which doesn't actually rhyme, as in the 2Pac "mash out" example in my dissertation.

Rdata=do.call('rbind',by(Rdata,INDICES = paste(Rdata$Song,Rdata$Section), function(Verse) {###need to apply only within verses
  #should segments only be counted within emcees? Sometimes rhymes might cross emcees...?
  .RhymeContext(Verse,rhyme.distance,within.chain) #this function does a lot of the work
}))
Rdata=floor(Rdata) #floor to nearest 16th-note

#data table
Rtab=table(rbind(
            data.frame(Antecedent=rep(1:16,each=16),Consequent=rep(1:16,16)),#this is to make sure that all possibilities are in the table, even if there are 0 in data
            Rdata))-1

denominator=unlist(ifelse(conditional,list(rowSums(Rtab)),list(sum(Rtab)))) #needed for calculating frequencies later

Rtab=Rtab*(1/denominator)
Rtab[is.na(Rtab)]=0
###################
######now plot
###################

#empty plot
par(mar=c(5,5,5,5),pty='s')
plot(1:16,type='n',axes=F,xlab='Consequent Metric Position',
     ylab='Antecedent Metric Position',
     main=paste(ifelse(conditional,'Conditional','Joint'),'Probability of Rhyme-Couplet\n Metric Positions',sep=' '))
for(i in c(0,-8,8)) abline(i,1,lwd=2,lty='dashed',col='grey80')

axis(2,1:16,unlist(strsplit(paste(1:4,c('e&a'),sep=''),split='')),tick=F,las=2)
axis(1,1:16,unlist(strsplit(paste(1:4,c('e&a'),sep=''),split='')),tick=F)

#plot data

scaler=(3/max(sqrt(Rtab)))^2 # this scales the table so the biggest squared probability is cex 3. Change the 3 is the scale is wrong for you.

ptab=sqrt(Rtab*scaler) #take the square root so that the area, not the height, of circles represents proportion of data.

for(i in 1:16) points(1:16,rep(i,16),cex=(ptab[i,]),pch=16) #actually plots data

##Size key -- freq only affects this part
MAX=max(Rtab)
keys=round(MAX*c(.05,.1,.25,.5,1),4)

key.labs=unlist(ifelse(freq,
                       list(paste('=',round(keys*denominator))),
                       list(paste('= ',100*keys,'%',sep=''),cex=2)))

points(rep(18,5),c(4,6,8,10,12),cex=sqrt(keys*scaler),pch=16,xpd=T)
text(rep(18.5,5),c(4,6,8,10,12),adj = 0,xpd=T,label=key.labs)
} #end plot_Rhyme

####Function used by plot_Rhyme

.RhymeContext=function(tran,rhyme.distance,within.chain) {
  #' Extracts antecedent-consuequent rhyme pairs.
  #'
  #' This extracts antecedent-consuequent pairs from a rhyme data.frame, and is used by plot_Rhyme.
  #' @param tran data.frame. The rhyme-only data.frame passed from plot_Rhyme.
  #' @param rhyme.distance Numeric vector. The desired distance(s) between rhymes. 1=adjacent only. (passed from plot_Rhyme)
  #' @param within.chain Logical. Whether to look at rhyme pairs within rhymechains, or at all rhymes. (passed from plot_Rhyme)
  #' @return A data.frame representing metric-rhyme-pairs in two columns, Antecedent and Consequent.

  contexter=function(rhyme,rhyme.distance){# this function grabs the 2-gram pairs, with the requested rhyme.distances
    if(any(nrow(rhyme)<(rhyme.distance+1))) return(data.frame(Antecedent=NA,Consequent=NA)) #Protects against errors

    rhyme.pairs=list()
    for(i in 1:length(rhyme.distance)){#loop through rhyme distances
      rhyme.pairs[[i]]=data.frame(Antecedent=head(rhyme$beat,-rhyme.distance[i]),Consequent=tail(rhyme$beat,-rhyme.distance[i]))
    }#close for loop
    return(do.call('rbind',rhyme.pairs))
  }#close contexter function

  if(within.chain){ output=do.call('rbind',by(tran,tran$Rhymeletters,contexter,rhyme.distance=rhyme.distance))
 } else{output=contexter(tran[tran$Rhymestarts,],rhyme.distance=rhyme.distance)#only apply to first accented syllable in each rhyme links, so that we don't get within rhyme pairs
  } #close else if

  return(output)
}

#################################################################################################################################

plot_Phrase=function(
  Data=FlowCorpus,
  Wrap.beat=1
  ){
  #' Plots metric position of phrases
  #'
  #' This plots the metric position of phrases as a scatter of arrows across a metric grid.
  #' @param Data The \code{\link{FlowCorpus}} data.frame or a subset of it.
  #' @param Wrap.beat single numeric, between 1:16. Controls where the metric position of the
  #' phrases at the bottom of the plot.
  #' @export plot_Phrase
  #' @return none
  #' @examples
  #' To see the metric distribution of all phrased in MCFlow rapped between 1992 and
  #' 1997:
  #' plot_Phrase(Corpus_Subset(years=1992:1997))
  #'

###################
####ARGUMENT CHECK
##################

if(nrow(Data)==0) stop('Data is empty.')

#Wrap.beat
  if(!is.numeric(Wrap.beat)) Wrap.beat=1 # coerce to number
  Wrap.beat=round(Wrap.beat) # coerce to whole number
  if(Wrap.beat[1]<1 | Wrap.beat[1]>16 ) Wrap.beat=.mod(Wrap.beat,16)


#####################
#####Prepare data
####################

Data$beat=floor(Data$beat) #round to 16th-note
Data$PhraseN=cumsum(as.numeric(Data$breaks %in% c('3','4','5','0')))


phrases=do.call('rbind',by(Data,Data$PhraseN,function(phrase){
  if(nrow(phrase)<3) return(data.frame(Start=NA,End=NA))
  if(count(!phrase$ipa %in% c('.','R') )<2) return(data.frame(Start=NA,End=NA))

  out=data.frame(Start=phrase$beat[1])

  last.syl=max(which(!phrase$ipa %in% c('.','R')))

  out$End=out$Start+sum(phrase$dur.16[1:(last.syl-1)])

  out
}))

##Change wrapping
if(Wrap.beat!=1){
phrases[phrases[,1]<Wrap.beat & !is.na(phrases[,1]),2]=phrases[phrases[,1]<Wrap.beat & !is.na(phrases[,1]),2]+16
phrases[phrases[,1]<Wrap.beat & !is.na(phrases[,1]),1]=phrases[phrases[,1]<Wrap.beat & !is.na(phrases[,1]),1]+16
}

###now plot

#blank plot
plot(1,1,type='n',ylim=c(Wrap.beat,Wrap.beat+15),xlim=c(Wrap.beat,max(phrases[,2],na.rm = T)),
     main='Metric Position of Phrases',xlab='Metric Position',ylab='',axes=F)
axis(1,seq(1,64,4),rep(1:4,4))

#plot data

 ALPHA=1-((nrow(phrases)/10000))
 for(i in 0:15+Wrap.beat){
  #axis
  text(i-1,i,labels = unlist(strsplit(split='',paste(1:4,'e&a',sep='')))[.mod(i,16)])

  cur=subset(phrases,phrases[,1]==i)
  Ys=jitter(rep(i,nrow(cur)),amount = .3)
  arrows(x0=jitter(cur[,1]),
         x1=jitter(cur[,2]),
         y0=Ys, y1=Ys,
         col=rgb(.5,.5,.5,alpha=ALPHA),angle=60,length=.1)
 }#end for loop
} #end plot_Phrase

####Helper functiojns


.mod=function(n,m) {
  #' Musical modulo function
  #'
  #' This performs a form of modulo math where the reset only happens aftern the modul number, so there are no zeros
  #' @param n The input number or numbers
  #' @param m The modulo number

  ((n-1)%%m)+1
}







