
midifiles <- system('find ~/Bridge/Research/Data/. -name "*mid"', intern = TRUE)

samp <- sample(midifiles, 1e3, prob = 1 + 100 * !grepl('Polyfonisk', midifiles) )
test <- lapply(samp, \(i) {print(i) ; result <- try(parseChunks(readBin(i, what = 'raw', 10e6)), silent = TRUE) ; if (class(result) != 'try-error') result})

testdt <- lapply(test, \(x) if (is.null(x)) data.table(Format = NA, Ntracks = NA, Division = NA) else as.data.table(x$Header)) |> rbindlist(fill=TRUE)
testdt$Filename <- samp

# Parsing MIDI binary ----

parseMIDI <- function(midifile) {
  binary <- readBin(midifile, 'raw', 10e6)
  
  chunks <- parseChunks(binary)
  parseTracks(chunks)
  
}

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
  division <-if (is.na(chunks$Header$Division)) {
    # warning("Don't know how to parse time-code-based divisions yet")
    120L
  }  else {
    chunks$Header$Division
  }

  lapply(chunks$Tracks, parseTrack, division = division)
  
}

parseTrack <- function(track, division) {
  int <- as.integer(track)
  status <- int >= 128
  
  data <- data.table(Int = int, Channel = integer(length(int)), Type = character(length(int)), Code = integer(length(int)), Duration = 0L, 
                     Data = vector('list', length(int)), Par1 = rep(NA_integer_, length(int)), Par2 = rep(NA_integer_, length(int)))
  
  initialDelta <- {
    int1 <- data$Int[1]
    if (int1 >= 128L) {
      delta <- parseVarLen(data$Int)
      data <- data[-1:-(delta$Length)]
      delta <- delta$Number
      
    } else {
      delta <- int1
      data <- data[-1]
    }
    delta
  }
  
  #
  data <- parseVarLenDelta(data) #should find and delta values that are longer than two bytes
  data <- parseFF(data) # parse system messages (needs expansion)
  data <- parseMessages(data) # parse everything else
  
  data[, Time := head(cumsum(c(initialDelta, Duration)),-1)]
  browser()

  
  
  
}


parseSpans <- function(data) {
  # finds distance ("span") between candidate status bytes
  data[ , Span := c(sum(Count), rep(0L, length(Count) - 1)), by = cumsum(Candi)]
  data[cumsum(Candi) == 0, Span := 0L]
  
  data
}


parseMessages <- function(data) {
  
  # FIND WHERE MESSAGES BEGIN (excluding var length delta bytes)
  messages <- parseStatusByte(data)
  # these may be wrong, because parseStatusByte can't distinguish true status bytes from 
  # multi-value delta tokens
  
  
  messages[ , Span := diff(c(Index, nrow(data) + 1L))]
  messages[ , Valid := validLength(Length, Span)]
  
  while(any(!messages$Valid, na.rm = TRUE)) {
    cur <- which(!messages$Valid)[1]
    val <- messages[cur:nrow(messages),
                    Index[validLength(Length[1], cumsum(c(Span[1], Span[-1] - 1L)))]]
    
    if (length(val) == 0L) stop("Something wrong with messages lengths.")
    
    block <- cur:which(messages$Index == val[1])
    
    messages$Span[cur] <- sum(messages$Span[cur], messages$Span[block[-1]])
    messages$Valid[cur] <- TRUE
    messages <- messages[-block[-1]]
    
  }
    
    
  data$Type[messages$Index] <- messages$Type
  data$Length[messages$Index] <- messages$Length
  data$Channel[messages$Index] <- messages$Channel
  
  #  PARSEDATA
  
  data <-  data[, {
    if (nrow(.SD) == 1L) {
      data.table(Int = Int, Channel = NA_integer_, Type = Type, Code = Code,
                 Duration = Duration, Data = Data, Par1 = NA_integer_, Par2 = NA_integer_, Length = NA_integer_)
    } else {
      datalen <- Length[1]
      sq <- c(-1, cumsum(Int[-1] < 128L) %% (datalen + 1L))
      
      twobyte <- c(FALSE, Int[-1] >= 128L) # at this point, only first Int should be "status" byte greater than 127, otherwise is var len delta
      sq[twobyte] <- 0L
      deltas <- sq == 0
      
      
      ## two-byte deltas
      deltaval <- if (any(twobyte)) {
        secondbyte <- which(twobyte) + 1L
        Int[twobyte] <- ((Int[twobyte] %% 128L) * 128L) + Int[secondbyte]
        # Int[!seq_along(deltas) %in% secondbyte] <- 0L
        Int[setdiff(which(deltas), secondbyte)]
      } else {
        Int[deltas]
      }
      
      
      data.table(Int = Int[1], Channel = Channel[1], Type = Type[1], Code = NA_integer_,
                 Duration = deltaval, 
                 Data = vector('list', sum(sq == 1)),
                 Par1 = Int[sq == 1], Par2 = if (datalen > 1) Int[sq == 2] else NA_integer_,
                 Length = datalen)
    }
  }, by = cumsum(!is.na(Type))]
  
  data[Type == 'Note on' & Par2 == 0L, Type := 'Note off']
  data
}

parseStatusByte <- function(data) {
  index <- which(data$Int >= 128L)
  data <- data[Int >= 128L]
  
  type <- data$Type
  
  
  
  int <- data$Int - 128L
  channel <- int %% 16L
  
  typen <- (int - channel) / 16L
  type <- ifelse(is.na(type),
                 c('Note off', 'Note on', 'Polyphonic aftertouch', 'Control change', 'Program change', 'Channel aftertouch', 'Pitch wheel range', 'System')[typen + 1L],
                 type)
  
  datalen <- c('Note off' = 2L, "Note on" = 2L,
               'Polyphonic aftertouch' = 2L, 'Control change' = 2L,
               'Program change' = 1L, 'Channel pressure' = 1L,
               'Pitch wheel range' = 2L, 'System' = NA_integer_)[type]
  
  data.table(Index = index, Int = data$Int, Type = type, Channel = channel, Length = datalen)
  
}

validLength <- function(length, span) {
  length <- rep(length, length.out = length(span))
  ifelse(length == 2L,
         ((span - 1L) %% 3L) == 0L,
         ((span - 1L) %% 2L) == 0L)
}

parseStatus2 <- function(data) {
  # finds which candidate status bytes are real status bytes, as opposed to leading var len delta bytes


  data[ , CompleteData := {
    comp <- logical(length(Int))
    
    comp[DataLen == 2L & ((Span - 1L) %% 3L) == 0L] <- TRUE
    comp[DataLen == 1L & ((Span - 1L) %% 2L) == 0L]
    comp[Type == 'FF'] <- TRUE
    comp
  }]
  
  incomplete <- data[ , any(Candi & !CompleteData)]
  candi <- which(data$Candi)
  
  while (incomplete) {
    
   
    
    bad <- data[ , which(Candi & !CompleteData)]
    if (length(bad)) {
      browser()
     
      nex <- candi[candi > bad[1]][1]
      data$Candi[nex] <- FALSE
      data$Type[nex] <- 'Delta'
      data$Channel[nex] <- NA_integer_
      data$DataLen[nex] <- NA_integer_
      data$Count[nex + 1L] <- 0L
      data <- parseSpans(data)
    d} else {
      incomplete <- FALSE
    }
  }
  data[, CompleteData := NULL]
  data
}

parseStatus <- function(data) {
  target <- data[, is.na(Type) & Int >= 128L ]
  
  int <- data[target == TRUE, Int]

  int <- int - 128L
  channel <- int %% 16L
  type <- (int - channel) / 16L
  
 
  type <- c('Note off', 'Note on', 'Polyphonic aftertouch', 'Control change', 'Program change', 'Channel aftertouch', 'Pitch wheel range', 'System')[type + 1L]

  datalen <- c('Note off' = 2L, "Note on" = 2L,
               'Polyphonic aftertouch' = 2L, 'Control change' = 2L,
               'Program change' = 1L, 'Channel pressure' = 1L,
               'Pitch wheel range' = 2L, 'System' = NA_integer_)[type]
  
  
  data[target == TRUE, Type := type]
  data[target == TRUE, Channel := channel]
  data[target == TRUE, DataLen := datalen]
  data
}

parseVarLenDelta <- function(data) {
  # collapse any obvious var len delta right away.
  # this means multilple bytes greater than 128 in a row, which should be very rare.
  runs <- data[, rle(Int >= 128L)]
  
  varlen <- runs$values & runs$lengths > 1L
  if (!any(varlen)) return(data)
  
  runs$values <- cumsum(varlen) * varlen
  runs$lengths[varlen] <-  runs$lengths[varlen] + 1L
  
  after <- which(varlen) + 1L
  
  runs$lengths[after] <- runs$lengths[after] - 1L
  
  data[ , Blocks :=  inverse.rle(runs)]
  
  data[Blocks > 0L, Scale := 2 ^ (7 * ((length(Int) - 1L) : 0)), by = Blocks]
  
  data[Blocks > 0L, Duration := c(sum((Int %% 128L) * Scale), rep(NA_integer_, length(Int) - 1L)), by = Blocks]
  data[Blocks > 0L, Data := list(list(Int[-1])), by = Blocks]
  data[Blocks > 0L, Type := 'Delta']
  data <- data[!is.na(Duration) | Blocks == 0L]
  
  data[, Scale := NULL]
  data[ , Blocks := NULL]
  data[]
  
  
  
}

parseFF <- function(data) {
  
  data[, FFblocks := cumsum(Int == 255L)]
  data <- data[, {
    if (FFblocks == 0) {
      .SD
    } else {
      len <- parseVarLen(Int[-1:-2])
      
      rest <-  tail(Int, -(2 + len$Length + length(len$Rest)))
      data.table(Int = c(255L, rest),
                 Channel = c(NA_integer_, integer(length(rest))),
                 Type = c('FF', rep(NA_character_, length(rest))),
                 Code = c(Int[2], rep(NA_integer_, length(rest))), 
                 Duration = 0L,
                 Data = c(list(len$Rest), vector('list', length(rest))),
                 Par1 = NA_integer_, Par2 = NA_integer_)
    }
  }, by = FFblocks]
  data[ , FFblocks := NULL]
  
  # grab deltas (unless they were already parsed )
  data[, nex := c(FALSE, head(Int, -1L) == 255L)]
  data[nex == TRUE, Duration := Int]
  data <- data[nex != TRUE]
  data[ , nex := NULL]
  
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



