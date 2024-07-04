
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
  
  
  data <- data.table(Int = int, Channel = integer(length(int)), Type = character(length(int)), Code = integer(length(int)), Delta = 0L, Data = vector('list', length(int)))
  
  data <- parseFF(data)

  
  data[, Status := cumsum(Int >= 128L)]
  
  data <- data[, parseStatus(.SD), by = Status]

  # try to find deltas!
  data[Status == 0, Type := 'Delta']
  data[is.na(Type), Type := {
    Type[1] <- 'Delta'
    Type
    
  }, by = Status]
  # data[Type == 'Delta', Delta :=]
  browser()
  # data[is.na(Type) & Int < 128, 
  
  
  
}

parseStatus <- function(data) {
  if (!is.na(data$Type[1]) && data$Type[1] == 'FF') return(data)
  if (nrow(data) == 1L) {
    data$Type == 'Delta'
    return(data)
  } 
  
  int <- data$Int[1] - 128L
  channel <- int %% 16L
  type <- (int - channel) / 16L
  
  type <- c('Note off', 'Note on', 'Polyphonic aftertouch', 'Control change', 'Program change', 'Channel aftertouch', 'Pitch wheel range', 'System')[type  + 1L]
  
  
  datalen <- c('Note off' = 2L, "Note on" = 2L,
               'Polyphonic aftertouch' = 2L, 'Control change' = 2L,
               'Program change' = 1L, 'Channel pressure' = 1L,
               'Pitch wheel range' = 2L, 'System' = NA_integer_)[type]
  
  dataint <- head(data$Int[-1], datalen)
  
  stuff <- data.table(Int = data$Int[1], Channel = channel, Type = type, Code = NA_integer_,  Delta = 0L, Data = list(dataint))
  rest <- tail(data, -(datalen + 1L))
  rbind(stuff, rest)
}

parseFF <- function(data) {
  data[, FF := cumsum(Int == 255L)]
  
  data <- data[, {
    if (FF == 0) {
      .SD
    } else {
      len <- parseVarLen(Int[-1:-2])
      
      rest <-  tail(Int, -(2 + len$Length + length(len$Rest)))
      data.table(Int = c(255L, rest),
                 Channel = c(NA_integer_, integer(length(rest))),
                 Type = c('FF', rep(NA_character_, length(rest))),
                 Code = c(Int[2], rep(NA_integer_, length(rest))), 
                 Delta =0L,
                 Data = c(list(len$Rest), vector('list', length(rest))))
    }
  }, by = FF]
  
  data[ , FF := NULL]
  
  data


}

parseVarLen <- function(x) {
  
  bytes <- x[1:which(x < 128)[1]]
  
  bytes <- bytes %% 128L
  n <- (length(bytes) - 1L) : 0L
  
  result <- sum(bytes * (2 ^ (7 * n)))
  list(Number = result, Length = length(bytes), Rest = head(tail(x, -length(bytes)), result))
  
}

# MIDI to humdrumR ----



