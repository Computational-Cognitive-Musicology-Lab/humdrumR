
midifiles <- system('find ~/Bridge/Research/Data/. -name "*mid"', intern = TRUE)

samp <- sample(midifiles, 1e3, prob = 1 + 100 * !grepl('Polyfonisk', midifiles) )
test <- lapply(samp, \(i) {print(i) ; result <- try(parseChunks(readBin(i, what = 'raw', 10e6)), silent = TRUE) ; if (class(result) != 'try-error') result})

testdt <- lapply(test, \(x) if (is.null(x)) data.table(Format = NA, Ntracks = NA, Division = NA) else as.data.table(x$Header)) |> rbindlist(fill=TRUE)
testdt$Filename <- samp

# Parsing MIDI binary ----

parseNumber <- function(raw, nBytes = 4L) {
  int <- as.integer(raw)
  scale <- 2 ^ (8L * ((nBytes - 1L) : 0L))
  
  scale <- rep(scale, length.out = length(int))
  
  int <- int * scale
  
  tapply(int, cumsum(((seq_along(int) - 1L) %% nBytes) == 0), sum)
  
}

`%i%` <- function(i, n = 1L)  i : (i + n - 1L)

parseChunks <- function(raw) {
  i <- 1L
  
  chunkType <- chunkType(raw, i)
  
  if (chunkType == 'MThd') {
    header <- parseHeader(raw, i)
  } 
  
  list(Header = header, Tracks = getTracks(raw, 15L))
  
}

getTracks <- function(raw, i) {
  chunkType <- chunkType(raw, i)
  
  if (chunkType != 'MTrk') return(NULL)
  
  len <- parseNumber(raw[(i + 4L) %i% 4L], 4L)
  data <- (i + 8L):(i + 8L + len - 1)
  
  events <- list(raw[data])
  
  nex <- i + 8L + len 
  if (nex > length(raw)) return(events)
  
  c(events, getTracks(raw, nex))
  
}

parseHeader <- function(raw, i) {
  cur <- raw[i %i% 14L]
  
  if (parseNumber(cur[5:8], 4L) != 6L) .stop("MIDI header has wrong length indication")
  
  format <- parseNumber(cur[9:10], 2L)
  format <- switch(format + 1, 'Single', 'Parallel', 'Sequential', .stop('MIDI header has wrong format.') )
  
  ntracks <- parseNumber(cur[11:12], 2L)
  
  division <- parseDivision(cur[13:14])
  
  list(Format = format, Ntracks = ntracks, Division = division)
}

parseDivision <- function(raw) {
  if (rawToBits(raw)[16] == 0L) {
    parseNumber(raw, 2L)
  } else {
    NA
  }
  
 
}

chunkType <- function(raw, i) {
  cur <- raw[i %i% 4L]
  
  result <- try(rawToChar(cur))
  if (class(result)[1] == 'try-error' || !result %in% c('MThd', 'MTrk')) "NA" else result
  
}

parseTracks <- function(chunks) {
  if (is.na(chunks$Header$Division) {
    .stop("Don't know how to parse time-code-based divisions yet")
    
  } 

  lapply(chunks$Tracks, parseTrack, division = chunks$Header$Division)
  
}

parseTrack <- function(track, division) {
  int <- as.integer(track)
  status <- int >= 128
  
  on <- int %in% 144L:159L
  off <- int %in% 128:143L
  
  groups <- cumsum(status)
  noteon <- unlist(tapply(on, groups, \(x) rep(any(x), length(x))))
  
  triple <- seq_len(sum(noteon)) %% 3L
  
  time <- int[noteon][triple == 1]
  pitch <- int[noteon][triple == 2]
  velocity <- int[noteon][triple == 0]
  
  cbind(time, pitch, velocity)
  
  
  
  
}

# MIDI to humdrumR ----



