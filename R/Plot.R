#### humdrum plot function

setAlpha = function(col, alpha) { do.call(rgb, c(as.list(col2rgb(col)[,1] / 255), alpha = alpha)) }

showHumdrum = function(humdata, scale = 1, svg = TRUE, file = paste0(tempdir(), '/humdrumDisplay.svg')) {
  #' @export
  #only plot one file
  humdata = humdata[1]
  if (nrow(humdata) == 0) invisible(NULL)
  
  #load records
  records = as.RecordVectors(humdata, global = TRUE)
  
  recordns   = records$RecordNumber
  recordvecs = records$Tokens 
  
  # recor types
  recordtypes = records$Type
  recordtypes[recordtypes == 'Comment' & records$Global] = 'Global Comment'
  recordtypes[recordtypes == 'Interpretation' & unlist(lapply(recordvecs, function(rec) grepl('^\\*\\*', rec[1])))] = 'Exclusive Interpretation'
  
  #spine numbers
  spinens  = names(records$Tokens[[which.max(lengths(recordvecs))]])
  sspinens = gsub('[0-9][0-9]*\\.', '', spinens)
  spinens  = gsub('\\..*', '', spinens)
  spinens[duplicated(spinens)] = ''

  # set up plot
  top = length(recordvecs)
  right = sum(spinens != '') + 1
  
  scale = scale / (right / 5)
  
  ################
  # Do plot
  ##############
  
  if (svg) {
    svg(file, width = right * 2 * scale * 1.25, height = top / 5 * scale * 1.25)
  }
    plot.new()
    par(mar = c(5, 0, 8, 0), bg = '#F1F1D4')
    plot.window(xlim = c(1, right),  ylim = c(top, 1))

    for (j in seq(1, right - 1, 2)) { # alternating column backgrounds
      polygon(x = c(j, j , j + 1, j + 1), 
              y = c(1, top + 1, top + 1, 1),
              col = 'grey95', border = FALSE)
    }

    # print title (filename)
    mtext(side = 3, text = unique(gsub('^.*/', '', records$FileName)), line = 2, cex = scale * 1.5)
  
    #
    X = unlist(lapply(rle(cumsum(spinens != ''))$lengths, function(n) head(seq(from = 0, to = 1, length.out = n + 1), -1))) + cumsum(spinens != '')
    
    # print spine numbers
    X_ = c(X, ceiling(max(X) + .01))
    X_ = (X + tail(X_, -1)) / 2
    text(X_, -1, spinens , xpd = T, cex = scale * 1.4, pos = 4) #spines
    text(X_, top + 2, spinens , xpd = T, cex = scale * 1.4, pos = 4) #spines
    text(X_,  0, sspinens, xpd = T, cex = scale * 1.2, pos = 4) #subspines
    text(X_, top + 3, sspinens, xpd = T, cex = scale * 1.2, pos = 4) #subspines
    
  
    linecolors = c('blue', 'green', 'darkgreen', 
                   'black', 'red', 'darkred', 'white')[match(recordtypes,  
                                                  c('Barline', 'Global Comment', 'Comment', 
                                                    'Data', 'Interpretation', 'Exclusive Interpretation', 'Reference'))]
    
    lapply(top:1, # loop for each record
           function(i) {
             if (recordtypes[i] != 'Data') { # add background color to record
               polycol = if (recordtypes[i] %in% c('Reference', 'Global Comment')) 'grey40' else setAlpha(linecolors[i], .3)
               polygon(x = c(1, 1, right + 1, right + 1),
                       y = i + c(-.5, .5, .5, -.5),
                       border = FALSE,
                       col = polycol)
               
             }
             
             # drawrecord number
             text(1, i, paste('[', recordns[i], ']', sep = ''), cex = .5 * scale, pos = 2, col = 'grey50')
             
             # draw tokens
             tokens = recordvecs[[i]]
             tokens[is.na(tokens)] = ''
             # cutoff tokens that are too long:
             tokens = trimTokens(tokens, 20L)
             
             text(x = X[seq_along(tokens)], 
                  y = i,
                  labels = tokens,
                  family = 'monospace',
                  pos = 4, col = linecolors[i],
                  cex = scale)
           }
    ) #close lapply

    if (svg) {
      dev.off() #close svg
      browseURL(file, browser = 'google-chrome')
    }
    
    invisible(NULL)
}

