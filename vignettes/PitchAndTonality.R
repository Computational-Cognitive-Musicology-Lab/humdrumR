## ---- fig.cap = "Scientific Pitch representation in tonalIntervals", result = 'hide', echo = FALSE----
arc <- function(x, y, sigma = 3, ...) {
    if (length(x) <= 1) return(NULL)
    
    for (i in 2:length(x)) {
        xs <- seq(x[i - 1], x[i], length.out = 1000)
        
        scaled <- xs - mean(xs)
        scaled <- (scaled / max(scaled)) * sigma
        
        ys <- plogis(scaled)
        ys <- ys - min(ys)
        ys <- ys / max(ys)
        ys <- (ys * (y[i] - y[i - 1])) + y[i - 1]
        
        points(xs, ys, type = 'l', ...)
        
    }
    

}

par(mar = c(6,6,6,6))
plot.new()
plot.window(xlim = c(-8,8), ylim = c(8,-8))

segments(0, -15, 0, 15, lwd = 20, col = 'grey90')
segments(-15,0, 15, 0,   lwd = 20, col = 'grey90')

coor <- expand.grid(X = -8:8, Y = -8:8)

text(x = coor$X, y = coor$Y, labels = as.sciPitch(tint(coor$Y, coor$X)), cex=.5)


axis(2, seq(-14,14, 2), tick = FALSE, cex.axis = .8, las = 1)
mtext('Octave', 2, line = 3, las = 1, xpd = NA)
mtext(quote(2^y), 2, line = 2, las = 1)

axis(1, seq(-14,14, 2), las = 1, tick = FALSE, cex.axis = .8, line = -1)
mtext('Fifth', 1, line = 2, las = 1, xpd = NA)
mtext(quote(3^x),  1, line = 1, las = 1)


cols <- col2rgb(rainbow(5))

cols <- apply(cols, 2, function(cur) rgb(cur[1], cur[2], cur[3], alpha = 5, maxColorValue = 255))


scale <- c(P1, M2, M3, P4, P5, M6, M7)

for (j in -7:7) {
	jscale <- tint(,-8:8) + j * P8
	jscale <- jscale[order(jscale@Fifth, jscale@Octave)]

	arc(jscale@Fifth , jscale@Octave + .5, lwd=.3)

#	for (i in 1:length(scale)) {
#		x <- jscale[i]@Fifth
#		y <- jscale[i]@Octave
#		polygon(x + c(-.5 , -.5, .5, .5), 
#			y + c(-.5, .5, .5, -.5), 
#		 		col = cols[j],
#				border= NA)
#	}
}



## ---- fig.cap = "Interval Representation of tonalIntervals", result = 'hide', echo = FALSE----

par(mar = c(6,6,6,6))
plot.new()
plot.window(xlim = c(-8,8), ylim = c(8,-8))

segments(0, -15, 0, 15, lwd = 20, col = 'grey90')
segments(-15,0, 15, 0,   lwd = 20, col = 'grey90')

coor <- expand.grid(X = -8:8, Y = -8:8)

text(x = coor$X, y = coor$Y, labels = as.interval(tint(coor$Y, coor$X), contour = c(octave = FALSE, threshold = P1)), cex=.5)


axis(2, seq(-14,14, 2), tick = FALSE, cex.axis = .8, las = 1)
mtext('Octave', 2, line = 3, las = 1, xpd = NA)
mtext(quote(2^y), 2, line = 2, las = 1)

axis(1, seq(-14,14, 2), las = 1, tick = FALSE, cex.axis = .8, line = -1)
mtext('Fifth', 1, line = 2, las = 1, xpd = NA)
mtext(quote(3^x),  1, line = 1, las = 1)


scale <- c(P1, M2, M3, P4, P5, M6, M7)

for (i in 1:length(scale)) {
	x <- scale[i]@Fifth
	y <- scale[i]@Octave
	polygon(x + c(-.5 , -.5, .5, .5), 
		y + c(-.5, .5, .5, -.5), 
		 	col = rgb(0, 0, 255, 100, max = 255), border= NA)
}


