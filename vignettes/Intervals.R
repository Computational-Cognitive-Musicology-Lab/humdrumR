## -----------------------------------------------------------------------------

referenceMajor   <- c(0, 2, 4, 5, 7, 9, 11, 12)

referenceTagline <- c(0, 3, 6, 9, 12, 14, 16)


## -----------------------------------------------------------------------------

serialMajor   <- c(0, 2, 2, 1, 2, 2, 2, 1)

serialTagline <- c(0, 3, 3, 3, 3, 2, 2) 


## -----------------------------------------------------------------------------

identical(cumsum(serialMajor), referenceMajor)

identical(cumsum(serialTagline), referenceTagline)

identical(c(referenceMajor[1], diff(referenceMajor)), serialMajor)

identical(c(referenceTagline[1], diff(referenceTagline)), serialTagline)



## -----------------------------------------------------------------------------
library(humdrumR)

identical(referenceMajor, sigma(delta(referenceMajor)))

identical(referenceMajor, delta(sigma(referenceMajor)))



## ---- fig.cap = "Derivative/Integral relationship in Major Scale", echo = FALSE, results = "hide"----
library(humdrumR)

plot(c(0, referenceMajor[-1] - .25), type = 'h', xlab = 'Index', ylab = 'Semitones',
	col = 'blue', main = "")
text(1:8, referenceMajor, labels = referenceMajor, col = 'blue', cex=.7)

points(referenceMajor, type = 'c', col = 'red')
text((2:8) - .5, referenceMajor[-1] - (serialMajor[-1]/2), labels = paste0(c('-', '', '+')[2 + sign(serialMajor[-1])], serialMajor[-1]), col = 'red', cex=.5, pos=1)

legend('topleft', fill = c('blue', 'red'), legend = c('Reference', 'Serial'), bty = "n")


## ---- fig.cap = "Derivative/Integral relationship in Tagline Rhythm", echo = FALSE, results = "hide"----
library(humdrumR)

plot(c(0, referenceTagline[-1] - .25), type = 'h', xlab = 'Index', ylab = 'Ticks',
	col = 'blue', main = "")
text(1:8, referenceTagline, labels = referenceTagline, col = 'blue', cex=.7)

points(referenceTagline, type = 'c', col = 'red')
text((2:8) - .5, referenceTagline[-1] - (serialTagline[-1]/2), labels = paste0(c('-', '', '+')[2 + sign(serialTagline[-1])], serialTagline[-1]), col = 'red', cex=.5, pos=1)

legend('topleft', fill = c('blue', 'red'), legend = c('Reference', 'Serial'), bty = "n")


## ---- fig.cap = "Figured Bass", echo = FALSE, results = "hide"----------------

library(humdrumR)

# In semitones:

bassline_reference <- c(0, 7, 4, 5, 2, 4, 5, 7, 7)

harmonicintervals <- c(16, 7, 8, 4, 15, 12, 9, 5, 4)

plot(c(0, bassline_reference[-1] - .25), type = 'h', xlab = 'Index', ylab = 'Pitch', ylim = c(0, 19), axes = FALSE,
	col = 'blue', main = "")
text(seq_along(bassline_reference), bassline_reference, labels = bassline_reference, col = 'blue', cex=.7)

ax <- c(0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19)
# axis(2, ax, as.tonalChroma(as.character(ax)), tick = FALSE, las = 1)

i <- seq_along(bassline_reference)

	segments(x0 = i, x1 = i, y0 = bassline_reference + .25, y1 = bassline_reference + harmonicintervals - .25, col = 'red')
	text(i, bassline_reference + harmonicintervals, labels = harmonicintervals, col = 'red', cex = .7)


legend('topleft', fill = c('blue', 'red'), legend = c('Bass Interval', 'Harmonic Interval'), bty = "n")


