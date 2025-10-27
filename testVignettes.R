# Getting Started ---------------------------------------------------------
library(humdrumR)

readHumdrum('inst/HumdrumData/BachChorales/chor001.krn') -> chor1

readHumdrum('inst/HumdrumData/BachChorales/chor0') -> chorales

chorales |>
  pitch(simple = TRUE) |>
  count() |>
  draw(xlab='pitch(simple=True)', ylab='count')

chorales |>
  notehead() |>
  count() |>
  draw()

chorales |>
  pitch(simple=TRUE) |>
  filter(Spine==4) |>
  count()

chorales |>
  filter(Key == 'G:') |>
  pitch(simple = TRUE) |>
  count()

# DataFields
example1 <- readHumdrum(humdrumRroot, 'examples/BasicExample.krn')
fields(example1)

example1 |>
  pull(Key)

example1 |>
  pull_data.frame(Token, Key)

chorales |>
  pitch(simple=TRUE) -> chorales

chorales |>
  select() |>
  duration() -> chorales

chorales |>
  select(Pitch, Duration) |>
  count()

chorales |>
  mutate(mint(Token))

chorales |>
  mutate(ifelse(Duration > (1/8), 
                mint(Token), 
                "NA"))

chorales |> 
  summarize(Token |> nchar() |> mean())

chorales |>
  mutate(MelodicIntervals = ifelse(Duration > (1/8), 
                                   mint(Token), 
                                   "NA"),
         MetricPosition = metsubpos(Token))

chorales |>
  mutate(ditto(Token))
chorales |> ditto(Token)

chorales |> mutate(lag(Token))

# R/Background ------------------------------------------------------------
myfunc <- \(x) x + 1
myfunc(1:9)

# Data prep ---------------------------------------------------------------
setwd(humdrumRroot)
dir('HumdrumData', recursive=TRUE)

fields(readHumdrum('HumdrumData',
                   c('BeethovenVariations', 'MozartVariations'),
                   '.*.krn',
                   reference = c(Composer = 'COM')))

paths1 <- readHumdrum(humdrumRroot, "examples/Paths.krn")
paths1 |> print(view="humdrum")
paths2 <- readHumdrum('examples/Paths2.krn')
paths2 |> print(view="table")
paths1 |> 
  filter(Path == 0) |>
  removeEmptyPaths()
paths2
paths2 |> expandPaths(asSpines = TRUE)

stops <- readHumdrum(humdrumRroot, 'examples/Stops.krn')
stops |> print(view = 'humdrum')
stops |> print(view = 'table')
stops |> 
  filter(Stop == 1) |>
  removeEmptyStops()


example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example
example[[ , c('**kern', '**harm')]]
example |>
  mutate(Rhythm = recip(Token),
         Pitch = kern(Token)) -> example
example
example |>
  filter(Rhythm == '4') |>
  count(Pitch)
example |>
  rend(Rhythm, Pitch)

example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example |>
  cleave(1:2, 3:4, newFields = 'Silbe') -> example
example |> select(Token)
example |> select(Silbe)

example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example |>
  cleave(c('kern', 'silbe'))

example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example |>
  index2( , '**kern') |>
  kern(simple = TRUE) |>
  cleave(c(1, 2)) |>
  count()

example <- readHumdrum(humdrumRroot, 'examples/Reshaping_example.hum')
example
      ed     
example_stops <- readHumdrum(humdrumRroot, 'examples/Reshaping_example2_stops.hum')
example_stops
example_stops |>
  cleave(Stop = 1:3)

example_paths <- readHumdrum(humdrumRroot, 'examples/Reshaping_example3_paths.hum')
example_paths
example_paths |>
  cleave(Path = 0:1)

chorales <- readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn')
# chorales[5]
chorales[[, 2]]
# chorales[[5, ]]
chorales[[1:50, ]]
chorales[[1, ]]
chorales[[50, 2]]


chorales
chorales |>
  filter(Spine == 1)

chorales |>
  filter(Spine ==1) |>
  mutate(Transposed = transpose(Token, by = '12')) |>
  unfilter(complement = 'Token')


# Analysis ----------------------------------------------------------------
setwd(humdrumRroot)
readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/chor0.*.krn') -> chorales
summary(chorales)
chorales |> census()
census(chorales, dataTypes = 'D')

readHumdrum(humdrumRroot, 'HumdrumData/BachChorales/.*krn') -> chorales
chorales |>
  mutate(Semits = semits(Token),
         Kern = kern(Token)) -> chorales
chorales |>
  group_by(Piece, Bar) |>
  with(Kern[which.min(Semits)]) |>
  count()

chorales |> select(Token) |>
  mutate(Beat = timecount(Token, unit = tactus(TimeSignature))) -> chorales
chorales |>
  group_by(Piece, Beat) |> 
  with(diff(range(Semits))) |>
  draw(xlab = 'Pitch range within beat')

chorales |>
  solfa(Token, simple = TRUE) -> chorales
chorales

beethoven <- readHumdrum(humdrumRroot, 'HumdrumData/BeethovenVariations/.*krn')
beethoven |>
  filter(Exclusive == 'kern' & Stop == 1) |> 
  removeEmptySpines() |> 
  removeEmptyStops() -> beethoven
beethoven


nested <- readHumdrum(humdrumRroot, 'examples/Phrases.krn')
nested

nested |>
  context('(', ')', overlap = 'nested') |>
  with(paste(Token, collapse = '|'))

nested |>
  context('(', ')', overlap = 'paired') |>
  with(paste(Token, collapse = '|'))
