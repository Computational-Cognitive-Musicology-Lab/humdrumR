library(humdrumR)


# Reading and Writing -----------------------------------------------------
setwd(humdrumRroot)
getwd()
readHumdrum()

# Summarizing -----------------------------------------------------
chorales <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/*.krn")
summary(chorales)

reference('ENC')

# Index Humdrum ------------------------------------------------------------------
humData <- readHumdrum(humdrumRroot, "HumdrumData/RollingStoneCorpus/*.hum")
humData[['b3', removeEmpty = TRUE]]
humData |> index(1:3) |> index2(20:30)


# Octave ------------------------------------------------------------------

chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
chorale[[20:30,]]

within(chorales[[20:30,]], octave(Token))
within(chorales[[20:30,]], octave(Token, octave.offset = 4)) # traditional octaves

within(chorales[[20:30,]], octave(Token, octave.integer = FALSE))

# Steps ----------------------------
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
chorale[[20:30,]]

within(chorale[[20:30,]], step(Token))

within(chorale[[20:30,]], step(Token, step.labels = c('C', 'D', 'E', 'F', 'G', 'A', 'B')))

# accidentals --------------
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
chorale[[100:110, ]]
within(chorale[[100:110, ]], accidental(Token))

# quality ------
chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')

within(chorales, quality(Token))
within(chorales, hint(Token, deparser = quality))
with(chorales, hint(Token, deparser = quality, incomplete = NA, bracket = FALSE)) |> table()

# transpose -------
transpose(c("D", "C#", "D", "E", "D"), Key='C', to='e:', real=FALSE)

# key ---
key(c('I', 'ii', 'ii:dor', 'v', '-vi', 'V/V', 'ii/V'))
signature(c('I', 'ii', 'ii:dor', 'v', '-vi', 'V/V', 'ii/V'))

# harm ---
B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
with(B075[[ , 2]], harm(Token))
with(B075[[ , 2]], roman(Token))
with(B075[[ , 2]], chord(Token))
with(B075[[,2]], tertian(Token))
with(B075[[ , 2]], figuredBass(Token))

# sonority ---
sonority(c('C', 'e', 'g'), inPlace = TRUE, fill=FALSE)
chorale <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor001.krn')
chorale <- within(chorales, dataTypes = 'Dd', ditto(Token) -> Token_dittoed) 
chorale[[20:30,]]
within(chorale[[20:30,]], sonority(Token_dittoed))
within(chorale[[20:30,]], sonority(Token_dittoed, deparser = harm))

# rhythms ---
B075 <- readHumdrum(humdrumRroot, "HumdrumData/BeethovenVariations/B075_00_05_a.krn")
within(B075, subset = Spine > 2,  recip(Token))
with(B075, subset = Spine > 2, recip(Token)) |> table()
with(B075, subset = Spine > 2, duration(Token)) 
with(B075, subset = Spine > 2, quarters(Token)) 
with(B075[[,3:4]], notehead(Token)) |> table()
with(B075[[,3:4]], seconds(Token))
within(B075, timeline(Token))

chorales <- readHumdrum(humdrumRroot, 'HumdrumData/Chorales/.*krn')
with(chorales, barplot(table(notehead(Token)), cex.names = 2))

# meter ---
tatum("M4/4")
tatum(c('M4/4', '6'))


# selectedFields ---
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-3].krn")
humData |> select(Spine, Record) |> selectedFields()
humData |> select(fieldTypes = 'Structure') |> selectedFields()

humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/*.krn")
cleave(humData, Path = c("1" = 0:1, "5" = 0:2))
cleave(humData, 1:2, 3:4)

humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-3].krn")
humData
humData |> 
  mutate(Recip = recip(Token), 
         Solfa = solfa(Token, simple = TRUE)) -> humData
humData
humData |> rend(c('Recip', 'Solfa'))


# count ---
generic <- c('c', 'c', 'e', 'g', 'a', 'b', 'b', 'b')
complex <- c('c', 'c#', 'e', 'f', 'g','g#', 'g#', 'a')

genericTable   <- count(generic)
complexTable <- count(complex)
genericTable
complexTable
genericTable + complexTable
cbind(genericTable, complexTable)

