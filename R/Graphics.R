#### plotting music notation symbols in R graphics!




makeStaff <- function(y, xlim = c(0, 1), nlines = 5, cex = 1) {
    corners <- par('usr')
    yrange <- diff(corners[3:4])
    
    linesep <- cex * (yrange / 20)
    
    ystaffbase <- (yrange * y) + corners[3] - (nlines / 2) * linesep
    
    
    ylines <- seq(ystaffbase, by = linesep, length.out = nlines)
    
    ##
    xlim <- (diff(corners[1:2])* xlim) + corners[1]
    #
    sapply(ylines, \(y) lines(xlim, c(y, y)))
    
   
    list(X = xlim, 
         Y = list(Pos = seq(ylines[1], by =  (linesep / 2), length.out = 101),
                  Neg = seq(ylines[1], by = -(linesep / 2), length.out = 101)),
         N = nlines)
    
}


staff.xycoords <- function(staff, xprop, yinteger) {
    
    match_size(xprop = xprop, yinteger = yinteger, toEnv = TRUE)
    
    xlim <- staff$X
    xrange <- diff(xlim)
    x <- xlim[1] + xprop * xrange
    y <- ifelse(yinteger >= 0, staff$Y$Pos, staff$Y$Neg)[abs(yinteger) + 1]
    
    list(x = x, y = y)
    
}

#### Plotting notes

plotNotes <- function(rhythm, pitch, cex = 1, staffy = .5, staffx = c(0, 1)) {
    par(family = 'Times')
    xpos <- cumsum(c(0, as.decimal(rhythm)))
    xpos <- xpos / max(xpos)
    rhythm <- as.notevalue(rhythm)
    
    makeStaff(staffy, xlim = staffx, cex = cex) -> staff
    
    staff.xycoords(staff, xpos, 2) -> xy
    
    text(xy$x, xy$y + cex * .045, rhythm, cex = cex * 2)
    
}

#### Plotting Rhythm Symbols

plotRhythm <- function(x, y, notes, ...) UseMethod('plotRhythm')

plotRhythm.default <- function(x, y, notes, ...){
    checkArg(x)
    
    checkArg(y)
    
    checkArg(notes)
    
    plotRhythm.rhythmInterval(x, y, notes, ...)
} 

plotRhythm.rhythmInterval <- function(x, y, notes, ...) {
    checkArg(x)
    
    checkArg(y)
    
    checkArg(notes)
    
    symbols <- as.notevalue(notes)
    
    text(x, y, labels = symbols, ...)
    
    invisible(symbols)
    
}
