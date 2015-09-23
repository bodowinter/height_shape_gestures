## Bodo Winter
## Created September 1, 2014; Edited September 23, 2015
## Analysis of study #2, web experiment

##------------------------------------------------------------------
## Define functions:
##------------------------------------------------------------------

## Function for batch-loading in .txt files from web experiments:

load_files <- function (filenames, new = F, show = T, ncol = 6) {
	xdata <- c()
	for (i in 1:length(filenames)) {
				if(show) print(filenames[i])
				xtemp <- read.table(filenames[i], sep = ';', stringsAsFactors = F)
				if(ncol(xtemp) == ncol) xdata <- rbind(xdata, xtemp)
				}
	return(xdata)
	}

## Empty plot function:

emptyplot <- function(...){
	plot(1, 1, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', type = 'n', main = '', ...)
	box(lwd = 3)
	}



##------------------------------------------------------------------
## Preliminaries:
##------------------------------------------------------------------

## Load in jpeg library for Figure 4:

library(jpeg)

## Set working directory:

setwd("/Users/teeniematlock/Desktop/research/gesture_categorization/all_analyses_gesture2015/web_experiment_raw_data/")

## Load in data:

ju <- load_files(list.files())

## Get rid of wrong row:

ju <- ju[-c(265),]

## Rename columns:

colnames(ju) <- c('index_curve', 'curl', 'question_order',
	'choice', 'confidence', 'seen_picture')

## The curl variable is reversed in coding, reverse it back:

ju$pinkie_curl <- numeric(nrow(ju))
ju[ju$curl == 1,]$pinkie_curl <- 9
ju[ju$curl == 2,]$pinkie_curl <- 8
ju[ju$curl == 3,]$pinkie_curl <- 7
ju[ju$curl == 4,]$pinkie_curl <- 6
ju[ju$curl == 5,]$pinkie_curl <- 5
ju[ju$curl == 6,]$pinkie_curl <- 4
ju[ju$curl == 7,]$pinkie_curl <- 3
ju[ju$curl == 8,]$pinkie_curl <- 2
ju[ju$curl == 9,]$pinkie_curl <- 1

## Check whether everybody has seen the picture:

table(ju$seen_picture)		# all yes, so we can safely exclude this

## Add two to index curve to make the index curve scale go from 1 to 6:

ju$index_curve <- ju$index_curve + 2

## Resort columns in data frame:

ju <- ju[, c('index_curve', 'pinkie_curl', 'question_order', 'confidence', 'choice')]




##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Analysis of confidence:

t.test(ju$confidence, mu = 5)

## Look at overall distributions descriptively:

table(ju$index_curve)
table(ju$pinkie_curl)
table(ju$pinkie_curl,ju$index_curve)
table(ju$choice) / sum(table(ju$choice))

## Center continuous predictors:

ju$index_curve_c <- ju$index_curve - mean(ju$index_curve)
ju$pinkie_curl_c <- ju$pinkie_curl - mean(ju$pinkie_curl)

## Make question order into deviation coding, so that the intercept represents the grand mean:

ju$question_order <- as.factor(ju$question_order)
contrasts(ju$question_order) <- contr.sum(2)/2

## Create quadratic predictors for curvature and curl:

ju$pinkie_curl_c2 <- ju$pinkie_curl_c^2
ju$index_curve_c2 <- ju$index_curve_c^2

## Make shape the reference level (for consistency with plots below):

ju$choice <- as.factor(ju$choice)
ju$choice <- relevel(ju$choice, ref = 'shape')

## Run the main model:

summary(xmdl <- glm(choice ~ index_curve_c + pinkie_curl_c +
	pinkie_curl_c:index_curve_c +
	pinkie_curl_c2 + index_curve_c2 +
	question_order, ju, family = 'binomial'))
drop1(xmdl, test = 'Chisq')		# this is what we report in 4.1.3. for the non-significant effects

## Deriving a minimally optimal model:

summary(xmdl <- glm(choice ~ index_curve_c + pinkie_curl_c +
	pinkie_curl_c2 + index_curve_c2, ju, family = 'binomial'))
drop1(xmdl, test = 'Chisq')			# all significant (minimally optimal model)

## For reporting odds:

exp(coef(xmdl))
exp(coef(xmdl) * -1)



##------------------------------------------------------------------
## Extract model predictions for plotting:
##------------------------------------------------------------------

## Make a data frame for index curve predictions:

index_newdata <- data.frame(index_curve_c = sort(unique(ju$index_curve_c)))
index_newdata$index_curve_c2 <- index_newdata$index_curve_c ^ 2
index_newdata$pinkie_curl_c <- 0.1521036			# these are the middle pinkie values
index_newdata$pinkie_curl_c2 <- 4.63154973

## Make a data frame for pinkie curl predictions:

pinkie_newdata <- data.frame(pinkie_curl_c = sort(unique(ju$pinkie_curl_c)))
pinkie_newdata$pinkie_curl_c2 <- pinkie_newdata$pinkie_curl_c ^ 2
pinkie_newdata$index_curve_c <- 0	# 0 works because there is an even numbered sequence of values
pinkie_newdata$index_curve_c2 <- 0

## Calculate predictions:

index_preds <- predict.glm(xmdl, newdata = index_newdata, type = 'response', se.fit = T)
pinkie_preds <- predict.glm(xmdl, newdata = pinkie_newdata, type = 'response', se.fit = T)

## Make predictions into nice dataframe:

index_preds <- as.data.frame(index_preds[1:2])
pinkie_preds <- as.data.frame(pinkie_preds[1:2])

## Get upper and lower bounds (confidence intervals):

index_preds$UB <- index_preds$fit + (1.96 * index_preds$se.fit)
index_preds$LB <- index_preds$fit - (1.96 * index_preds$se.fit)
pinkie_preds$UB <- pinkie_preds$fit + (1.96 * pinkie_preds$se.fit)
pinkie_preds$LB <- pinkie_preds$fit - (1.96 * pinkie_preds$se.fit)



##------------------------------------------------------------------
## Publication-ready plot (Figure 4 in paper):
##------------------------------------------------------------------

## Load in images:

setwd("/Users/teeniematlock/Desktop/research/gesture_categorization/all_analyses_gesture2015/")

curve_min <- readJPEG('figure4_curve_minimum.jpg')
curve_max <- readJPEG('figure4_curve_maximum.jpg')
curl_min <- readJPEG('figure4_curl_mimimum.jpg')
curl_max <- readJPEG('figure4_curl_maximum.jpg')

## The plot:

quartz('', 15, 6)
par(mai = c(1, 0.2, 1, 0.2),
	omi = c(0.5, 1.5, 0, 0.25),
	mfrow = c(1, 2))
# Plot 1, Curvature:
emptyplot(xlim = c(0.5,6.5), ylim = c(0, 1))
axis(side = 2, seq(0, 1, 0.25),
	lwd.ticks = 3, lwd = 3, las = 2,
	font = 2, cex.axis = 1.25)
axis(side = 1, 1:6, lwd.ticks = 3, lwd = 3,
	font = 2, cex.axis = 1.45)
abline(h = 0.5, lwd = 2, lty = 2)
mtext('Proportion of\n\"height\" responses',
	side = 2, cex = 1.5, line = 4.5, font = 2)
mtext('(a) Index Curve', side = 3, cex = 2, line = 1.5, font = 2)
mtext('Curvature continuum', side = 1, cex = 2.1, line = 3.5, font = 2)
points(1:6, index_preds$fit, type = 'b', lwd = 3, pch = 19)
arrows(1:6, y0 = index_preds$LB, y1 = index_preds$UB,
	angle = 90, code = 3, length = 0.1 , lwd = 2)
rasterImage(curve_min,
	xleft = 0.6, xright = 1.4,
	ybottom = -0.43, ytop = -0.18, xpd = NA)
rasterImage(curve_max,
	xleft = 5.6, xright = 6.4,
	ybottom = -0.43, ytop = -0.18, xpd = NA)
# Plot 2, Curvature
emptyplot(xlim = c(0.5,9.5), ylim = c(0, 1))
axis(side = 1, 1:9, lwd.ticks = 3, lwd = 3,
	font = 2, cex.axis = 1.45)
abline(h = 0.5, lwd = 2, lty = 2)
mtext('(b) Pinkie Curl', side = 3, cex = 2, line = 1.5, font = 2)
mtext('Curl continuum', side = 1, cex = 2.1, line = 3.5, font = 2)
points(1:9, pinkie_preds$fit, type = 'b', lwd = 3, pch = 19)
arrows(1:9, y0 = pinkie_preds$LB, y1 = pinkie_preds$UB,
	angle = 90, code = 3, length = 0.1 , lwd = 2)
rasterImage(curl_min,
	xleft = 0.44, xright = 1.56,
	ybottom = -0.43, ytop = -0.18, xpd = NA)
rasterImage(curl_max,
	xleft = 8.44, xright = 9.56,
	ybottom = -0.43, ytop = -0.18, xpd = NA)


