## Bodo Winter
## April 22, 2015
## Hand continuum matrix graph (Figure 3) for paper

## Load in jpeg library for interacting with jpeg file:

library(jpeg)

## Load in image file:

setwd('/Users/teeniematlock/Desktop/research/gesture_categorization/')
jj <- readJPEG('continuum_matrix.jpg')

## The actual plot:

quartz('', 8, 6)
par(mai = c(1, 1.25, 0.25, 0.25))
plot(0:1, 0:1, type = 'n', ann = FALSE,
	axes = FALSE, yaxs = 'i', xlim = c(0.02, 0.98))
rasterImage(jj, 0, 0, 1, 1)
box(lwd = 4)
axis(side = 2, seq(0.1, 0.9, length.out = 6), labels = 1:6,
	lwd = 4, font = 2, cex.axis = 1.5)
axis(side = 1, seq(0.05, 0.95, length.out = 9), labels = 1:9,
	lwd = 4, font = 2, cex.axis = 1.5)
mtext(side = 1, text = 'Pinkie Curl', cex = 1.75, line = 3, font = 2)
mtext(side = 2, text = 'Index Curve', cex = 1.75, line = 3, font = 2)
abline(v = seq(0.10625, 0.89375, length.out = 8), lty = 2)
abline(h = seq(0.18, 0.82, length.out = 5), lty = 2)

